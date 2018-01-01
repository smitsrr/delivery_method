#create a web app to explore prevalence of C-sections versus vaginal deliveries

    # initialize libraries
library(shiny)
library(plyr)
library(dplyr)
library(ggplot2)
library(rgdal)    # for readOGR(...)
library(maptools)
library(data.table)
library(mapproj)
library(RColorBrewer) # for brewer.pal(...)
library(plotly)
library(stringr)  # for str_detect(...)
library(ggthemes)


setwd("C:/Users/smits/Documents/GitHub/delivery_method")
    #import some data
    # Note, you have to delete the comments at the end of the text files. 
    # due to supression, the totals likely won't be able to be calculated when
    # you include segments. 

    # age and race - use this for the pop-up next to the map. 
natality_race_age<- read.delim("wonder_data_extracts/Natality_12_15_race_age_v2.txt",
                  header = TRUE, colClasses = "character")

race_age<- natality_race_age %>%
  mutate(Births = as.numeric(Births)) %>%
  group_by(County.Code, Race.Code, Age.of.Mother.9.Code) %>%
  mutate(cesarean_rate = Births/sum(Births)) %>%
  filter(Delivery.Method == "Cesarean")

    # validate against aggregated age/race totals
race_age_totals<- read.delim("wonder_data_extracts/Natality_12_2015_race_age_totals_v2.txt",
                             header = TRUE, colClasses = "character") %>%
  mutate(Births = as.numeric(Births)) %>%
         # race_recode = case_when(.$Race.Code == '1002-5' ~ "Other", 
         #                         .$Race.Code == 'A-PI' ~ "Other",
         #                         TRUE ~ .$Race))%>%
  group_by(Race.Code, Age.of.Mother.9.Code) %>%
  mutate(cesarean_rate = Births/sum(Births)) %>%
  filter(Delivery.Method == "Cesarean")

ggplot(race_age_totals,aes(x=Age.of.Mother.9, y=cesarean_rate, color = Race))+
  geom_point()

    # county totals, all births
natality<- read.delim("wonder_data_extracts/Natality_12_15_county_totals_all_births_v2.txt", 
                               header = TRUE, colClasses = "character") %>%
  filter(Births != "Suppressed") %>%
  mutate(Births = as.numeric(Births)) %>%
  group_by(County.Code, County) %>%
  mutate(cesarean_rate_all = round(Births/sum(Births),4)*100,
         births_all = sum(Births)) %>%
  filter(Delivery.Method == "Cesarean") %>%
  select(-Notes, -Births, -Delivery.Method, -Delivery.Method.Code)

    # population totals for counties
    # https://www.census.gov/data/datasets/2016/demo/popest/counties-total.html#ds
setwd("C:/Users/smits/Documents/GitHub/delivery_method/")
county.pop <- read.csv("co-est2016-alldata.csv", colClasses = "character") %>%
  mutate(fips = paste0(STATE, COUNTY),
         state = tolower(STNAME), 
         county = tolower(gsub(" .*$", "", CTYNAME)))%>%
  select(fips, POPESTIMATE2012, state, county)

  #join in unidentified counties
unidentified<- filter(natality, str_detect(County, "Unidentified")) %>%
 mutate(state_code = substr(County.Code, 1,2))

counties_3<- county.pop %>%
  left_join(natality, by=c("fips" = "County.Code")) %>%
  mutate(fips_state = substr(fips,1,2)) %>%
  left_join(unidentified, by = c("fips_state" = "state_code")) %>%
  mutate(County_all_pop = ifelse(is.na(County.x), County.y, County.x),
         cesarean_rate_all_pop = ifelse(is.na(cesarean_rate_all.x), cesarean_rate_all.y, cesarean_rate_all.x),
         births_all_pop = ifelse(is.na(births_all.x), births_all.y, births_all.x),
         population = as.numeric(POPESTIMATE2012)) %>%
  group_by(County_all_pop) %>%
  mutate(population_all_pop = sum(population))

    # if you join in and expand the unidentified counties:
    ##take either county.x or county.y
    ##cesarean_rate.x or cesarean_rate.y
    ##births.x or births.y
    ## depending on whether x is populated. 

    # check that we have cesarean rates for all counties with >100,000 2013 pop
#unidentified_check<- filter(counties_3, population<100000)

    ## setup mapping data
map.county <- map_data('county')
counties   <- unique(map.county[,5:6])
state_map <- map_data("state")

    #join natality with county.pop
birth_map<- left_join(counties_3, county.pop, by=c("County.Code" = "fips"))
map.county <- data.table(map_data('county'))
setkey(map.county,region,subregion)
birth_map <- data.table(birth_map)
setkey(birth_map,state.x,county.x)
map.df      <- map.county[birth_map]

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Cesarean section rate"),
   hr(),
   h3("Rate as a factor of geographic location", align = "center"),
   div(
     style = "position:relative",
     plotOutput("county_map", 
                hover = hoverOpts("plot_hover", delay = 100, delayType = "debounce")),
     uiOutput("hover_info")
   ),
   h4("Grey counties have population < 100,000, for which the CDC combines all state counties. 
      White counties have had population changes and therefore data are not available. "),
   hr(),
   h3("Rate as a factor of race and age of mother", align = "center"),
   plotOutput("race_age_plot"),
   h4("Each data point is a county. Only counties with population >100,000 are shown.
      Horizontal line is the median, top and bottom of boxes are the 25th and 75th percentile,
      respectively. The lines extend to ____, with data points indicating outliers. ")

)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
  output$county_map <- renderPlot({
      # plot the birth rate minus unidentified counties. 
      p<- ggplot(map.df, aes(x=long, y=lat, group = group)) +
        geom_polygon(colour = "grey" , aes(fill = cesarean_rate_all.x )) +
        scale_fill_gradientn("",colours=brewer.pal(9,"YlGnBu"))+
       # coord_map() +
        coord_map("polyconic" ) + #causes the tooltips to not work as well
        geom_path(data = state_map, colour="black")+
        theme_void() + 
        theme(legend.position = c(.15,.2))

      # unfilled is unavailable due to population changes. 
      # grey/NA is an 'unidentified' county
    p
   })
  
  output$hover_info<-renderUI({
    hover <- input$plot_hover
    point <- nearPoints(map.df, hover, threshold = 5, maxpoints = 1, addDist = TRUE)
    if (nrow(point) == 0) return(NULL)
    
    # calculate point position INSIDE the image as percent of total dimensions
    # from left (horizontal) and from top (vertical)
    left_pct <- (hover$x - hover$domain$left) / (hover$domain$right - hover$domain$left)
    top_pct <- (hover$domain$top - hover$y) / (hover$domain$top - hover$domain$bottom)
    
    # calculate distance from left and bottom side of the picture in pixels
    left_px <- hover$range$left + left_pct * (hover$range$right - hover$range$left)
    top_px <- hover$range$top + top_pct * (hover$range$bottom - hover$range$top)
    
    # create style property fot tooltip
    # background color is set so tooltip is a bit transparent
    # z-index is set so we are sure are tooltip will be on top
    style <- paste0("position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.85); ",
                    "left:", left_px + 2, "px; top:", top_px + 2, "px;")
    
    # actual tooltip created as wellPanel
    wellPanel(
      style = style,
      p(HTML(paste0("<b></b>", point$County_all_pop, "<br/>",
                    "<b> Region Population</b>", point$population_all_pop, "<br/>",
                    "<b> County Population: </b>", point$population, "<br/>",
                    "<b> Region Cesarean Rate: </b>", point$cesarean_rate_all_pop, "<br/>",
                    "<b> County Cesarean Rate: </b>", point$cesarean_rate_all.x, "<br/>",
                    "<b> Distance from left: </b>", left_px, 
                    "<b>, from top: </b>", top_px)))
    )
  })
  
  output$race_age_plot <- renderPlot({
    ggplot(race_age, aes(x=Age.of.Mother.9.Code, y=cesarean_rate, fill = Race)) + 
      geom_boxplot()+ 
      theme_few()+
      theme(legend.position="top", legend.title = element_blank(), 
            axis.title = element_text(color = "black"), 
            text = element_text(color = "black"), 
            axis.text = element_text(color = "black")) + 
      ylab("Percent of Births Delivered via Cesarean") + 
      xlab("Age of Mother") + 
      scale_y_continuous(labels = scales::percent)+
      scale_fill_brewer()
  })
  
}

# Run the application 
runApp(list(ui = ui, server = server))

## TO DO
# tooltips to ggmap/plot object - 'nearpoint' is definitely OFF

# Since i'm not going to use plotly, think about having a zoom feature for states? 

# write methods

#formatting stuff in shiny - make the map bigger, move legend? 

# maybe add a table to show multiple counties data at once? I like the
#  ggiraph example using on_click, and then have to have a 'reset' button. 

## NOTES
# I don't know if I'm a huge fan of having the county name for unidentified counties. 
# it feels a little deceptive. 

# after comparing, it's pretty clear that first birth rates are not different from all
# I think I could just present all. 


# plot hospital layer?
# https://hifld-dhs-gii.opendata.arcgis.com/datasets/5eafb083e43a457b9810c36b2414d3d3_0
# setwd("C:/Users/smits/Documents/GitHub/delivery_method/wonder_data_extracts/Hospitals")
#     # US.counties <- readOGR(dsn=".",layer="gz_2010_us_050_00_5m")
# hospitals = readOGR(dsn=".", layer="Hospitals")
# hospitals.df<- as.data.frame(hospitals) %>%
#   filter(STATE != "AK",
#          STATE != "HI",
#          STATE!= "PW", 
#          STATE != "GU",     # Exclude guam
#          COUNTRY == "USA"
#          )