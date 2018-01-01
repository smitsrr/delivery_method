library(ggplot2)
library(mapdata)

map.county <- map_data('county')
counties<- data.table(map.county)

library(shiny)

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
      geom_polygon_interactive(aes(tooltip = county))
    
    ggiraph(code = print(p), 
            hover_css = "fill:#FF3333;stroke:black;cursor:pointer;")
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)