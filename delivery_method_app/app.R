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
natality<- read.delim("wonder_data_extracts/Natality_12_15_race_age_v2.txt", 
                   header = TRUE, colClasses = "character")

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


    # county and FIPS codes from https://www.census.gov/geo/reference/codes/cou.html
counties<- read.delim("national_county.txt", header = FALSE, sep = ",",
                      colClasses = "character")
counties_2<- counties %>%
  rename_("state_code" = "V1", 
          "fips_state" = "V2", 
          "fips_county" = "V3",
          "county_name" = "V4",
          "fips_class" = "V5") %>%
  mutate(fips = paste0(fips_state, fips_county))

    # population totals for counties
    # https://www.census.gov/data/datasets/2016/demo/popest/counties-total.html#ds
setwd("C:/Users/smits/Documents/GitHub/delivery_method/")
county.pop <- read.csv("co-est2016-alldata.csv", colClasses = "character") %>%
  mutate(fips = paste0(STATE, COUNTY)) %>%
  select(fips, POPESTIMATE2012)

  #join in unidentified counties
# unidentified<- filter(natality_counties, str_detect(County, "Unidentified")) %>%
#  mutate(state_code = substr(County.Code, 1,2)) 

counties_3<- left_join(counties_2, county.pop) %>%
  left_join(natality, by=c("fips" = "County.Code")) 
  # left_join(unidentified, by = c("fips_state" = "state_code")) %>%
  # mutate(County.x = ifelse(is.na(County.x), County.y, County.x),
  #        cesarean_rate.x = ifelse(is.na(cesarean_rate.x), cesarean_rate.y, cesarean_rate.x),
  #        births.x = ifelse(is.na(births.x), births.y, births.x),
  #        population = as.numeric(POPESTIMATE2012)) %>%
  # select(-County.Code, -County.y, -cesarean_rate.y, -births.y, -POPESTIMATE2012)

    # if you join in and expand the unidentified counties:
    ##take either county.x or county.y
    ##cesarean_rate.x or cesarean_rate.y
    ##births.x or births.y
    ## depending on whether x is populated. 

    # check that we have cesarean rates for all counties with >100,000 2013 pop
#unidentified_check<- filter(counties_3, population<100000)

    # following stack overflow example
setwd("C:/Users/smits/Documents/GitHub/delivery_method/counties_shapes/gz_2010_us_050_00_5m")
US.counties <- readOGR(dsn=".",layer="gz_2010_us_050_00_5m")
    #leave out AK, HI, and PR (state FIPS: 02, 15, and 72)
US.counties <- US.counties[!(US.counties$STATE %in% c("02","15","72")),]  
county.data <- US.counties@data
county.data <- cbind(id=rownames(county.data),county.data)
county.data <- data.table(county.data)
county.data[,FIPS:=paste0(STATE,COUNTY)] # this is the state + county FIPS code
setkey(county.data,FIPS)   

natality.data <- data.table(counties_3)
setkey(natality.data,fips)

map.df <- data.table(fortify(US.counties))
setkey(map.df,id)

county.data2 <- county.data %>%
  left_join(counties_3, by=c("FIPS"="fips")) %>%
  data.table()
setkey(county.data2,id)

map.df[county.data2,cesarean_rate_first:=cesarean_rate_first]
map.df[county.data2,births_first:=births_first]
map.df[county.data2,cesarean_rate_all:=cesarean_rate_all]
map.df[county.data2,births_all:=births_all]
map.df[county.data2,population:=POPESTIMATE2012]
map.df[county.data2,county:=NAME]
# map.df[county.data2,county2:=County.x]
map.df[county.data2,FIPS:=FIPS]


    # plot hospital layer?
    # https://hifld-dhs-gii.opendata.arcgis.com/datasets/5eafb083e43a457b9810c36b2414d3d3_0
setwd("C:/Users/smits/Documents/GitHub/delivery_method/wonder_data_extracts/Hospitals")
    # US.counties <- readOGR(dsn=".",layer="gz_2010_us_050_00_5m")
hospitals = readOGR(dsn=".", layer="Hospitals")
hospitals.df<- as.data.frame(hospitals) %>%
  filter(STATE != "AK",
         STATE != "HI",
         STATE!= "PW", 
         STATE != "GU",     # Exclude guam
         COUNTRY == "USA"
         )
    # need to exclude hospitals outside of the contiguous US

# p<-ggplot() +
#   geom_polygon(data=map.df, aes(x=long, y=lat, fill=cesarean_rate_all, 
#                            text = paste0(county, " County",
#                                          "<br>Pop: ", format(population,big.mark = ","), 
#                                          "<br>", substr(county2, 1,12),
#                                          "<br>Cesarean Rate: ", cesarean_rate, "%",
#                                          "<br>Births: ", format(births, big.mark=","))))+
#   coord_map()+
#   # geom_point(data=hospitals.df, aes(x=LONGITUDE, y=LATITUDE, 
#   #                                   alpha=.8  # size parameter isn't working. 
#   #                                   ))+
#   scale_fill_gradientn("",colours=brewer.pal(9,"YlOrRd"))+
#   labs(title="2012-2015 Cesarean Section Rate by Country, percent",x="",y="")+
#   theme_bw()
# ggplotly(p)




    # NOTES
# choose age of mother
# [extension, choose medical risk factor]
# Choose month prenatal care began
# display US map of c-section rate (using bi-directional color scale)
# compare only first-births to 'all births'
# show/remove hospital layer. 


# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Cesarean section rate"),

   checkboxInput(inputId="first_births",
                 label = "Show First Births Only", 
                 value = FALSE),
   
  plotlyOutput("county_map")
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
  output$county_map <- renderPlotly({
    if(input$first_births == FALSE) {
      p<-ggplot(map.df, aes(x=long, y=lat, group=group, fill=cesarean_rate_all, 
                            text = paste0(county, " County",
                                          "<br>Pop: ", format(population,big.mark = ","),
                                          "<br>Cesarean Rate: ", cesarean_rate_all, "%",
                                          "<br>Births: ", births_all))) +
        scale_fill_gradientn("",colours=brewer.pal(9,"YlOrRd"))+
        geom_polygon()+coord_map()+
        labs(title="2012-2015 Cesarean Section Rate by Country, percent",x="",y="")+
        theme_bw()
    } else {
      p<-ggplot(map.df, aes(x=long, y=lat, group=group, fill=cesarean_rate_first, 
                            text = paste0(county, " County",
                                          "<br>Pop: ", format(population,big.mark = ","),
                                          "<br>Cesarean Rate: ", cesarean_rate_first, "%",
                                          "<br>First Births: ", births_first,
                                          "<br>All Births: ", births_all))) +
        scale_fill_gradientn("",colours=brewer.pal(9,"YlOrRd"))+
        geom_polygon()+coord_map()+
        labs(title="2012-2015 Cesarean Section Rate by Country, percent",x="",y="")+
        theme_bw()
    }
    ggplotly(p)

   })
}

# Run the application 
shinyApp(ui = ui, server = server)

## NOTES
# I don't know if I'm a huge fan of having the county name for unidentified counties. 
# it feels a little deceptive. 
