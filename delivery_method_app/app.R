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


setwd("C:/Users/smits/Documents/GitHub/delivery_method")
    #import some data
    # Note, you have to delete the comments at the end of the text files. 
    # due to supression, the totals likely won't be able to be calculated when
    # you include segments. 

    # age and race - use this for the pop-up next to the map. 
# natality<- read.delim("wonder_data_extracts/Natality_12_15_race_age_v2.txt", 
#                    header = TRUE, colClasses = "character")



    #total county rates - first born children
natality_counties<- read.delim("wonder_data_extracts/Natality_12_15_county_totals_v2.txt", 
                      header = TRUE, colClasses = "character") %>%
  filter(Births != "Suppressed") %>%
  mutate(Births = as.numeric(Births)) %>%
  group_by(County.Code, County) %>%
  mutate(cesarean_rate_first = round(Births/sum(Births),4)*100,
         births_first = sum(Births)) %>%
  filter(Delivery.Method == "Cesarean") %>%
  select(-Notes, -Births, -Delivery.Method, -Delivery.Method.Code)

    # county totals, all births
natality_counties_all<- read.delim("wonder_data_extracts/Natality_12_15_county_totals_all_births_v2.txt", 
                               header = TRUE, colClasses = "character") %>%
  filter(Births != "Suppressed") %>%
  mutate(Births = as.numeric(Births)) %>%
  group_by(County.Code, County) %>%
  mutate(cesarean_rate_all = round(Births/sum(Births),4)*100,
         births_all = sum(Births)) %>%
  filter(Delivery.Method == "Cesarean") %>%
  select(-Notes, -Births, -Delivery.Method, -Delivery.Method.Code)

    # join the counts/rates for first births v. all births
natality<- left_join(natality_counties, natality_counties_all)

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
   
  plotOutput("county_map")
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
  output$county_map <- renderPlot({
      # plot the birth rate minus unidentified counties. 
      p<- ggplot(map.df, aes(x=long, y=lat, group = group)) +
        geom_polygon( colour = "grey" , aes( fill = cesarean_rate_all.x )) +
        scale_fill_gradientn("",colours=brewer.pal(9,"YlGnBu"))+
        coord_map("polyconic" ) +
        geom_path(data = state_map, colour="black")+
        theme_void()

      # unfilled is unavailable due to population changes. 
      # grey/NA is an 'unidentified' county
    p
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

## TO DO
# add state lines
# add faint lines between counties. 
# click on a county, get rates by ethnicity/age
# age of mom = color of point, x-axis = race, y-axis = cesarean rate
# if age is too cluttered, we could facet by race (there are only 4 )
# resize the map - figure out where the other graphs are going to go

## NOTES
# I don't know if I'm a huge fan of having the county name for unidentified counties. 
# it feels a little deceptive. 

# after comparing, it's pretty clear that first birth rates are not different from all
# I think I could just present all. 


# NOTES
# choose age of mother
# [extension, choose medical risk factor]
# Choose month prenatal care began
# display US map of c-section rate (using bi-directional color scale)
# compare only first-births to 'all births'
# show/remove hospital layer. 



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