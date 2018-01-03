library(ggplot2)
library(mapdata)

map.county <- map_data('county')
counties<- data.table(map.county)

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("NearPoints using a map"),
  div(
    style = "position:relative",
    plotOutput("county_map", 
               hover = hoverOpts("plot_hover", delay = 100, delayType = "debounce")),
    uiOutput("hover_info")
  )
)

server <- function(input, output) {
  
  output$county_map<- renderPlot({
    ggplot(counties, aes(x=long, y=lat, group = group)) +
      geom_polygon(colour = "grey") 
      # coord_map("polyconic" )  #causes the tooltips to be even more off
  })
  
  output$hover_info<-renderUI({
    hover <- input$plot_hover
    point <- nearPoints(counties, hover, threshold = 5, maxpoints = 1, addDist = TRUE)
    if (nrow(point) == 0) return(NULL)
    
    # calculate point position INSIDE the image as percent of total dimensions
    # from left (horizontal) and from top (vertical)
    left_pct <- (hover$x - hover$domain$left) / (hover$domain$right - hover$domain$left)
    top_pct <- (hover$domain$top - hover$y) / (hover$domain$top - hover$domain$bottom)
    
    # calculate distance from left and bottom side of the picture in pixels
    left_px <- hover$range$left + left_pct * (hover$range$right - hover$range$left)
    top_px <- hover$range$top + top_pct * (hover$range$bottom - hover$range$top)
    
    # create style property fot tooltip
    # background color is set so tooltip is a bit transparent
    # z-index is set so we are sure are tooltip will be on top
    style <- paste0("position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.85); ",
                    "left:", left_px + 2, "px; top:", top_px + 2, "px;")
    
    # actual tooltip created as wellPanel
    wellPanel(
      style = style,
      p(HTML(paste0("<b></b>", point$region, "<br/>",
                    "<b>County: </b>", point$subregion, "<br/>",
                    "<b> Distance from left: </b>", left_px, "<b>, from top: </b>", top_px)))
    )
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)