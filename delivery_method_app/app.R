#create a web app to explore prevalence of C-sections versus vaginal deliveries

    # initialize libraries
library(shiny)
library(plyr)
library(dplyr)
library(ggplot2)
library(rgdal)    # for readOGR(...)
library(data.table)
library(mapproj)


    # move up one directory
setwd("..")
     #import some data
    # Note, you have to delete the comments at the end of the text files. 
asian<- read.delim("wonder_data_extracts/Natality_2015_asian.txt", 
                             sep = "/t", header = TRUE, colClasses = "character")
american_indian<- read.delim("wonder_data_extracts/Natality_2015_american_indian.txt", 
                             sep = "/t", header = TRUE, colClasses = "character")
black<- read.delim("wonder_data_extracts/Natality_2015_black.txt", 
                   sep = "/t", header = TRUE, colClasses = "character")
white_1<- read.delim("wonder_data_extracts/Natality_2015_white_1.txt", 
                     sep = "/t", header = TRUE, colClasses = "character")
white_2<- read.delim("wonder_data_extracts/Natality_2015_white_2.txt", 
                     sep = "/t", header = TRUE, colClasses = "character")
white_3<- read.delim("wonder_data_extracts/Natality_2015_white_3.txt", 
                     sep = "/t", header = TRUE, colClasses = "character")
    # append all of the data
natality<- rbind(asian
                 ,american_indian
                 ,black
                 ,white_1
                 ,white_2
                 ,white_3) %>%
  select(-Notes)

#####################
# Race and medical risk factors
#####################
setwd("C:/Users/smits/Documents/GitHub/delivery_method/wonder_data_extracts/race_risk_factors")

wh_diabetes<- read.delim("Natality_2015_white_diabetes_v2.txt",
                          header = TRUE, colClasses = "character") %>%
  mutate(risk_factor = ifelse(Diabetes == "Yes", "diabetes", "none"))%>%
  select(-Diabetes, -Diabetes.Code)
wh_eclampsia<- read.delim("Natality_2015_white_eclampsia_v2.txt",
                          header = TRUE, colClasses = "character")%>%
  mutate(risk_factor = ifelse(Eclampsia == "Yes", "eclampsia", "none"))%>%
  select(-Eclampsia, -Eclampsia.Code)
wh_hypertension<- read.delim("Natality_2015_white_hypertension_v2.txt",
                          header = TRUE, colClasses = "character") %>%
  mutate(risk_factor = ifelse(Chronic.Hypertension == "Yes", "chronic.hypertension", "none"))%>%
  select(-Chronic.Hypertension, -Chronic.Hypertension.Code)
wh_preg_hypertension<- read.delim("Natality_2015_white_pregnancy_hypertension_v2.txt",
                          header = TRUE, colClasses = "character")%>%
  mutate(risk_factor = ifelse(Pregnancy.associated.Hypertension == "Yes", "preg_hypertension", "none"))%>%
  select(-Pregnancy.associated.Hypertension, -Pregnancy.associated.Hypertension.Code )
wh_tobacco<- read.delim("Natality_2015_white_tobacco_v2.txt",
                        header = TRUE, colClasses = "character")%>%
  mutate(risk_factor = ifelse(Tobacco.Use.Code == "Yes", "tobacco", "none")) %>%
  select(-Tobacco.Use.Code, -Tobacco.Use)

# append all of the data
natality<- rbind(wh_diabetes
                 ,wh_eclampsia
                 ,wh_hypertension
                 ,wh_preg_hypertension
                 ,wh_tobacco) %>%
  mutate(Births = as.numeric(Births))

county_births_check <- natality %>%
  group_by(County,County.Code, Delivery.Method) %>%
  summarize(births = sum(Births))


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
    # following stack overflow example
setwd("./counties_shapes/gz_2010_us_050_00_5m")
US.counties <- readOGR(dsn=".",layer="gz_2010_us_050_00_5m")
#leave out AK, HI, and PR (state FIPS: 02, 15, and 72)
US.counties <- US.counties[!(US.counties$STATE %in% c("02","15","72")),]  
county.data <- US.counties@data
county.data <- cbind(id=rownames(county.data),county.data)
county.data <- data.table(county.data)
county.data[,FIPS:=paste0(STATE,COUNTY)] # this is the state + county FIPS code
setkey(county.data,FIPS)      
natality.data <- data.table(natality)
setkey(natality.data,County.Code)
#county.data[obesity.data,obesity:=PCT_OBESE_ADULTS10]

map.df <- data.table(fortify(US.counties))
setkey(map.df,id)
setkey(county.data,id)
#map.df[county.data,obesity:=obesity]

#it's a start! 
ggplot(map.df, aes(x=long, y=lat, group=group, fill = "white")) +
  geom_polygon()+coord_map()+
  labs(title="2010 Adult Obesity by Country, percent",x="",y="")+
  theme_bw()

ggplot(map.df, aes(x=long, y=lat, group=group, fill=obesity)) +
  scale_fill_gradientn("",colours=brewer.pal(9,"YlOrRd"))+
  geom_polygon()+coord_map()+
  labs(title="2010 Adult Obesity by Country, percent",x="",y="")+
  theme_bw()

    # NOTES
# choose age of mother
# [extension, choose medical risk factor]
# Choose month prenatal care began
# display US map of c-section rate (using bi-directional color scale)


# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Old Faithful Geyser Data"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         sliderInput("bins",
                     "Number of bins:",
                     min = 1,
                     max = 50,
                     value = 30)
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("distPlot")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$distPlot <- renderPlot({
      # generate bins based on input$bins from ui.R
      x    <- faithful[, 2] 
      bins <- seq(min(x), max(x), length.out = input$bins + 1)
      
      # draw the histogram with the specified number of bins
      hist(x, breaks = bins, col = 'darkgray', border = 'white')
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

