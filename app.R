library(plyr)
library(rgeos)
library(leaflet)
library(grDevices)
library(gridExtra)
library(maptools)


options('downlad.file.method' = 'curl')

setwd('/srv/shiny-server/spatial-binning')

source('spatial-binning.R')

ui <- fluidPage(
 	leafletOutput("map")
)

server <- function(input, output, session) {
   	output$map <- renderLeaflet({
		loadMap("nl_0_1000")
	})
}

shinyApp(ui, server)
