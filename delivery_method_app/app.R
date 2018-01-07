
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
library(stringr)      # for str_detect(...)
library(ggthemes)
library(shinyWidgets)
library(ggiraph)      # for Geom_polygon_interactive(...)
library(tools)        # for toTitleCase(...)

setwd("C:/Users/smits/Documents/GitHub/delivery_method")
    #import some data
    # Note, you have to delete the comments at the end of the text files. 
    # due to supression, the totals likely won't be able to be calculated when
    # you include segments. 

    # age and race - use this for the analysis of race and age. 
natality_race_age<- read.delim("wonder_data_extracts/Natality_12_15_race_age_v2.txt",
                  header = TRUE, colClasses = "character")

race_age<- natality_race_age %>%
  mutate(Births = as.numeric(Births)) %>%
  group_by(County.Code, Race.Code, Age.of.Mother.9.Code) %>%
  mutate(cesarean_rate = Births/sum(Births),
         Race = factor(Race, levels=c("American Indian or Alaska Native",
                                      "Asian or Pacific Islander",
                                      "White", 
                                      "Black or African American"),
                              ordered = TRUE)) %>%
  filter(Delivery.Method == "Cesarean")

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
         state = toTitleCase(STNAME), 
         county = gsub(" county", "", tolower(CTYNAME)))%>%
  mutate(county=gsub(" parish", "", tolower(county))) %>% # because louisiana is weird
  mutate(county=gsub("[.]", "", county)) %>%  # in the ggplot map data, they don't have periods
  select(fips, POPESTIMATE2012, state, county) %>%
  filter(substr(fips,3,5) != '000')  #this was super exagerating the population!

  #join in unidentified counties so taht we can join by state
unidentified<- filter(natality, str_detect(County, "Unidentified")) %>%
 mutate(state_code = substr(County.Code, 1,2))

natality.2<- county.pop %>%
  left_join(natality, by=c("fips" = "County.Code")) %>%
  mutate(fips_state = substr(fips,1,2)) %>%
  left_join(unidentified, by = c("fips_state" = "state_code")) %>%
  mutate(county_display = ifelse(is.na(County.x), County.y, County.x),
         cesarean_rate = ifelse(is.na(cesarean_rate_all.x), cesarean_rate_all.y, cesarean_rate_all.x),
         births = ifelse(is.na(births_all.x), births_all.y, births_all.x),
         population = as.numeric(POPESTIMATE2012)) %>%
  group_by(county_display) %>%
  mutate(population_display = sum(population) 
         # ,fips = ifelse(fips == "46102", "46113", fips)
         ) %>% #Damn county in SD
  select(fips,                     #take only the variables I need going forward
         county_display, 
         cesarean_rate,
         births,
         population,
         population_display,
         state,
         county)

    # check that we have cesarean rates for all counties with >100,000 2013 pop
#unidentified_check<- filter(counties_3, population<100000)

    # check population sums
#unidentified_check<- filter(counties_3, state == 'Alabama')

    # Get Census shape file information
    #https://www2.census.gov/geo/tiger/GENZ2016/shp/cb_2016_us_county_5m.zip
exclude_states<- c("02", "15", "78", "60", "66", "69", "72")
    #excludes Alaska, american samoa (60), commonwealth of the northern mariana islands (69), 
    # guam (66). need to exclude the virgin islands
setwd("C:/Users/smits/Documents/GitHub/delivery_method/wonder_data_extracts")
us.counties <- readOGR(dsn=".",layer="cb_2016_us_county_5m")
us.counties<- us.counties[!us.counties$STATEFP %in% exclude_states,]

county.data <- us.counties@data
county.data <- cbind(id=rownames(county.data),county.data)
county.data <- data.table(county.data)
county.data[,FIPS:=paste0(STATEFP,COUNTYFP)] # this is the state + county FIPS code
setkey(county.data,FIPS)  

    # join the natality data with the county data.
natality.3 <- data.table(natality.2)
setkey(natality.3,fips)
county.data <- county.data[natality.3]  # should be joined on fips
setkey(county.data, id)

    # make the map layer
map.df<-data.table(fortify(us.counties))
setkey(map.df,id)
    # add in interesting data
map.df<- map.df[county.data]  # should be joined on ID

    # use the TIGER dataset from the census to draw the state outlines
    # http://www2.census.gov/geo/tiger/GENZ2016/shp/cb_2016_us_state_5m.zip
US.states <- readOGR(dsn=".",layer="cb_2016_us_state_5m")
exclude_states<- c("02", "15", "78", "60", "66", "69", "72")
US.states<- US.states[!US.states$GEOID %in% exclude_states,]
us_states<- merge(fortify(US.states), as.data.frame(US.states), by.x="id", by.y=0)

    #testing:
# ggplot(map.df, aes(x=long, y=lat, group = group)) +
#   geom_polygon( aes( fill = cesarean_rate)) +
#   coord_quickmap()+
#   theme_void()+
#   geom_polygon(data = us_states, aes(x=long, y=lat, group = group), color = "black", fill = NA)
#   scale_fill_gradientn("",colours=brewer.pal(9,"YlGnBu"))

input<-NULL
input$select_state <- c("Alabama", "Georgia")

##################
# SHINY
#################
ui <- fluidPage(
   
   # Application title
  titlePanel("Cesarean section rate"),
  tabsetPanel(type="tabs",
    tabPanel(title = "Geography",
           hr(),
           h3("Percent of Births Delivered Via Cesarean as a Factor of Geographic Location", align = "center"),
           pickerInput(inputId = "select_state",
                       label = "Display: ",
                       choices = sort(unique(map.df$state)),
                       multiple = TRUE,
                       options = list(`actions-box` = TRUE), 
                       selected = unique(map.df$state)), 
           fluidRow(
                ggiraphOutput("county_map")
           )
     ),
    tabPanel(title = "Age and Race", 
             h3("Median Percent of Births Delivered via Cesarean as a Factor of Mother Age and Race", align = "center"),
             fluidRow(
               column(width = 6, offset = 3, 
                      plotOutput("race_age_plot")))), 
    tabPanel(title = "Technical Details",
           fluidRow(
             column(width = 8, offset=2,
                    br(),
                    tags$div(
                      tags$p("All data were obtained through the CDC's Wonder data query tool between
                             December 20 and December 31, 2017. Queries had the following restrictions:"),
                      tags$li("Single live births between 20012 and 2015"),  
                      tags$li("Gestational age between 37-41 weeks"),
                      tags$li("Birth occured in a hospital")),
                    tags$div(
                      tags$p("County data were grouped only by County and Delivery Method in the Wonder query. 
                             Age and race data were grouped by County, Race, Age of Mother 9, and
                             Delivery Method in the Wonder Query. Counties and race categories with 
                             fewer than 20 births were excluded from the calculation. ")),
                    hr(),
                    tags$div(
                      tags$p("Suggested Citation: United States Department of Health and Human Services (US DHHS), Centers for Disease Control and Prevention
                             (CDC), National Center for Health Statistics (NCHS), Division of Vital Statistics, Natality public-use data 2007-2015, on CDC
                             WONDER Online Database, February 2017. Accessed at http://wonder.cdc.gov/natality-current.html on Jan 1, 2018 9:42:30 AM"))
           ))))
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
   
  
  
  county_data<- reactive({
    req(input$select_state)
    map.df[map.df$state %in% input$select_state,]
  })
  
  state_data<-reactive({
    req(input$select_state)  
    us_states[us_states$NAME %in% input$select_state, ]
   
  })
  

  output$county_map <- renderggiraph({
        # take the filtered data from the reactive portion above
    p<- ggplot(county_data(),  aes(x=long, y=lat, group = group)) +
        geom_polygon( aes( fill = cesarean_rate)) +
        coord_quickmap()+
        theme_void()+
        geom_polygon_interactive(aes(tooltip = paste0(gsub("'", "", county_display),
                                                    "<br>Population: ", format(population,big.mark=","),
                                                    "<br>Births: ", format(births, big.mark=","),
                                                    "<br>C-section Rate: ", cesarean_rate, "%"), 
                                   fill = cesarean_rate))+ 
        scale_fill_gradientn("",colours=brewer.pal(9,"YlGnBu"))+
        geom_polygon(data = state_data(), aes(x=long, y=lat, group = group), color = "black", fill = NA)

    
    ggiraph(code = print(p)) #takes a super long time to render...
    
   })
  
  output$race_age_plot <- renderPlot({
    ggplot(race_age[race_age$Births>20,], aes(x=Age.of.Mother.9.Code, y=cesarean_rate, fill = Race)) + 
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
# For some reason the PickerInput is not selecting all/deselecting all appropriately. 

# contingency for no state selected. Seems to not be freaking out right now.  

# center state selecter. 

# add tooltips to the age/race plot! It should contain N's and summary stats. 
# SHIT. nearpoints doesn't work super well for polygons!!! 
# might be going back to ggiraph after all. 

# write methods

# after comparing, it's pretty clear that first birth rates are not different from all
# I think I could just present all. 