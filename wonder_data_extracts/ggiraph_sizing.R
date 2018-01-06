library(ggplot2)
library(mapdata)
library(shiny)
library(data.table)
library(ggiraph)

map.county <- map_data('county')
counties<- data.table(map.county)

ui <- fluidPage(
  # Application title
  titlePanel("ggiraph plot margins"),
  fluidRow(column(6,
                  ggiraphOutput("county_map")), 
          column(6,
                 ggiraphOutput("county_map_all"))
  )
)

server <- function(input, output) {
  output$county_map<- renderggiraph({
    p<- ggplot(counties, aes(x=long, y=lat, group = group)) +
      # coord_map("polyconic" ) +  # comment or uncomment this line to see the changes
      theme(plot.margin = unit(c(.1,.1,.1,.1), "cm")) + 
      geom_polygon_interactive(aes(tooltip = subregion))
    ggiraph(code = {print(p)},width = 1)
  })
  
  output$county_map_all<-renderggiraph({
    # just two states
    p<- ggplot(counties[counties$region %in% c("alabama", "georgia"),], aes(x=long, y=lat, group = group)) +
      # coord_map("polyconic" ) +  # comment or uncomment this line to see the changes
      theme(plot.margin = unit(c(.1,.1,.1,.1), "cm")) + 
      geom_polygon_interactive(aes(tooltip = subregion))
    ggiraph(code = {print(p)},width = 1)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)