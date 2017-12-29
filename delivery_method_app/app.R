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
                             sep = "\t", header = TRUE, colClasses = "character")
american_indian<- read.delim("wonder_data_extracts/Natality_2015_american_indian.txt", 
                             sep = "\t", header = TRUE, colClasses = "character")
black<- read.delim("wonder_data_extracts/Natality_2015_black.txt", 
                   sep = "\t", header = TRUE, colClasses = "character")
white_1<- read.delim("wonder_data_extracts/Natality_2015_white_1.txt", 
                     sep = "\t", header = TRUE, colClasses = "character")
white_2<- read.delim("wonder_data_extracts/Natality_2015_white_2.txt", 
                     sep = "\t", header = TRUE, colClasses = "character")
white_3<- read.delim("wonder_data_extracts/Natality_2015_white_3.txt", 
                     sep = "\t", header = TRUE, colClasses = "character")
    # append all of the data
natality<- rbind(asian
                 ,american_indian
                 ,black
                 ,white_1
                 ,white_2
                 ,white_3) %>%
  select(-Notes)

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

