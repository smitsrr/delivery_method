
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
library(shinyWidgets)

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
   
  # Make this height = 0 : div class="ggiraph-toolbar" 
  tags$div(class = "ggiraph-toolbar", 
           height = "0px"),
  
   # Application title
   titlePanel("Cesarean section rate"),
   hr(),
   h3("Rate as a factor of geographic location", align = "center"),
   pickerInput(inputId = "select_state",
              label = "Display: ",
              choices = sort(unique(counties$region)),
              multiple = TRUE,
              options = list(`actions-box` = TRUE)), 
  fluidRow(
     column(width = 7, offset=3,
            div(style="width:800px;height:600px;", ggiraphOutput("county_map")))),
   h4("All counties in a state with population 
      <100,000 are presented together (grey). 
      White counties have had population changes and therefore data are not available. "),
   hr(),
   h3("Rate as a factor of race and age of mother", align = "center"),
   plotOutput("race_age_plot"),
   h4("Each data point is a county. Only counties with population >100,000 are shown.
      Horizontal line is the median, top and bottom of boxes are the 25th and 75th percentile,
      respectively. The lines extend to ____, with data points indicating outliers. ")

)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
   
  output$county_map <- renderggiraph({
      p<- ggplot(map.df, aes(x=long, y=lat, group = group)) +
        geom_polygon(colour = "grey" , aes(fill = cesarean_rate_all.x )) +
        coord_map("polyconic",
                  xlim = c(-120, -70),ylim = c(24.9, 49.9))  +
        geom_path(data = state_map, colour="black")+
        theme_void() + 
        theme(legend.position = c(.9,.25)) + 
        geom_polygon_interactive(aes(tooltip = paste0(County_all_pop, 
                                                      "<br>Population: ", format(population_all_pop,big.mark=","),
                                                      "<br>C-section Rate: ", cesarean_rate_all_pop, "%")
                                     , fill = cesarean_rate_all.x))+
        scale_fill_gradientn("",colours=brewer.pal(9,"YlGnBu"))

      # unfilled is unavailable due to population changes. 
      # grey/NA is an 'unidentified' county
    ggiraph(code = print(p), selection_type = "multiple") #takes a super long time to render...
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
# add tooltip to age/race data to see those outlier counties. add # births

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