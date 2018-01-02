library(ggplot2)
library(mapdata)
library(shiny)
library(data.table)
library(ggiraph)

map.county <- map_data('county')
counties<- data.table(map.county)

ui <- fluidPage(
  
  # Application title
  titlePanel("NearPoints using a map"),
fluidRow(column(12,
                ggiraph::ggiraphOutput("county_map")))
)

server <- function(input, output) {
  
  output$county_map<- renderPlot({
   p<- ggplot(counties, aes(x=long, y=lat, group = group)) +
      geom_polygon(colour = "grey") +
      coord_map("polyconic" ) +
      geom_polygon_interactive(aes(tooltip = subregion))
    
    ggiraph(code = {print(p)})
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)