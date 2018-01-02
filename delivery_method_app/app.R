
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

    # validate against aggregated age/race totals
# race_age_totals<- read.delim("wonder_data_extracts/Natality_12_2015_race_age_totals_v2.txt",
#                              header = TRUE, colClasses = "character") %>%
#   mutate(Births = as.numeric(Births)) %>%
#          # race_recode = case_when(.$Race.Code == '1002-5' ~ "Other", 
#          #                         .$Race.Code == 'A-PI' ~ "Other",
#          #                         TRUE ~ .$Race))%>%
#   group_by(Race.Code, Age.of.Mother.9.Code) %>%
#   mutate(cesarean_rate = Births/sum(Births)) %>%
#   filter(Delivery.Method == "Cesarean",
#          Births >= 100)
# 
# ggplot(race_age_totals,aes(x=Age.of.Mother.9.Code, y=cesarean_rate, color = Race, group = Race))+
#   geom_point() + 
#   geom_path()

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
  mutate(county=gsub(".", "", county)) %>%  # in the ggplot map data, they don't have periods
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
  mutate(population_display = sum(population)) %>%
  select(fips,
         county_display, 
         cesarean_rate,
         births,
         population,
         population_display,
         state,
         county)
    #take only the variables I need going forward

    # check that we have cesarean rates for all counties with >100,000 2013 pop
#unidentified_check<- filter(counties_3, population<100000)

    # check population sums
#unidentified_check<- filter(counties_3, state == 'Alabama')

    ## setup mapping data
map.county <- map_data('county') %>%
  mutate(region = toTitleCase(region))
map.county <- data.table(map.county)
setkey(map.county,region,subregion)
state_map <- map_data("state") %>%
  mutate(region = toTitleCase(region))

    #join natality with county.pop
natality.3 <- data.table(natality.2)
setkey(natality.3,state,county)

    # join the natality data with the county data. 
map.df      <- map.county[natality.3]

    # troubleshooting
selected_states<- c("Utah", "Colorado", "Idaho")
# [map.df$region %in% selected_states,]
# [state_map$region %in% selected_states,]
p<- ggplot(map.df, aes(x=long, y=lat, group = group)) +
  geom_polygon(colour = "grey" , aes(fill = cesarean_rate)) +
  coord_map("polyconic")+
  geom_path(data = state_map, colour="black")+
  theme_void() +
  geom_polygon_interactive(aes(tooltip = paste0(county_display,
                                                "<br>Population: ", format(population,big.mark=","),
                                                "<br>C-section Rate: ", cesarean_rate, "%")
                               , fill = cesarean_rate))+
  scale_fill_gradientn("",colours=brewer.pal(9,"YlGnBu"))

ggiraph(code = print(p)) #takes a super long time to render...

ui <- fluidPage(
   
   # Application title
  titlePanel("Cesarean section rate"),
  tabsetPanel(type="tabs",
    tabPanel(title = "Background", 
             fluidRow(
               column(width = 6, offset = 3,
                      tags$div(
                         tags$p(""))
             ))),
    tabPanel(title = "Visualizations",
           hr(),
           h3("Percent of Births Delivered Via Cesarean as a Factor of Geographic Location", align = "center"),
           pickerInput(inputId = "select_state",
                       label = "Display: ",
                       choices = sort(unique(counties$region)),
                       multiple = TRUE,
                       options = list(`actions-box` = TRUE), 
                       selected = unique(counties$region)), 
           fluidRow(
             column(width = 7, offset=3,
                    div(style="width:900px;height:700px;", 
                        ggiraphOutput("county_map")))),
           h4("Grey counties have population < 100,000. CDC aggregates all counties in a state
              with population < 100,000.    
               White counties have had population changes and therefore data are not available. "),
           hr(),
           h3("Median Percent of Births Delivered via Cesarean as a Factor of Mother Age and Race", align = "center"),
           fluidRow(
             column(width = 6, offset = 3, 
                    plotOutput("race_age_plot")))
     ),
     tabPanel(title = "Technical Details",
           fluidRow(
             column(width = 8, offset=2,
                    br(),
                    tags$div(
                      tags$p("All data were obtained through the CDC's Wonder data query tool between
                             December 20 and December 31, 2017. Queries had the following restrictions:")),
                    tags$li(
                      tags$li("Births 2012-2015"), 
                      tags$li("Single live births"), 
                      tags$li("In hospitals"), 
                      tags$li("gestational age between 37-41 weeks"),
                      tags$li("In hospitals")),
                    tags$div(
                      tags$p("County data were grouped only by County and Delivery Method. 
                             Age and race data were grouped by County, Race, Age of Mother 9, and
                             Delivery Method. Counties and race categories with fewer than 20 births
                             were excluded from the calculation. ")),
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
    map.df[map.df$region %in% input$select_state,]
  })

  output$county_map <- renderggiraph({
        # take the filtered data from the reactive portion above
    p<- ggplot(county_data(), aes(x=long, y=lat, group = group)) +
      geom_polygon(colour = "grey" , aes(fill = cesarean_rate_all.x )) +
      coord_map("polyconic")+
      geom_path(data = state_map[state_map$region %in% input$select_state,], colour="black")+
      theme_void() + 
      geom_polygon_interactive(aes(tooltip = paste0(County_all_pop, 
                                                    "<br>Population: ", format(population_all_pop,big.mark=","),
                                                    "<br>C-section Rate: ", cesarean_rate_all_pop, "%")
                                   , fill = cesarean_rate_all.x))+
      scale_fill_gradientn("",colours=brewer.pal(9,"YlGnBu"))
    
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
# damn map size. individual states look great, but the whole US is WAY too tiny. 

# fill in the data for 'white' counties = 'county name', 'data not available'
# write methods

# maybe add a table to show multiple counties data at once? I like the
#  ggiraph example using on_click, and then have to have a 'reset' button. 

## NOTES
# I don't know if I'm a huge fan of having the county name for unidentified counties. 
# it feels a little deceptive. 

# after comparing, it's pretty clear that first birth rates are not different from all
# I think I could just present all. 