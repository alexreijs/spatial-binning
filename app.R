library(plyr)
library(rgeos)
library(leaflet)
library(grDevices)
library(gridExtra)
library(maptools)

options(shiny.trace=TRUE)
options('downlad.file.method' = 'curl')

setwd('/srv/shiny-server/spatial-binning')

source('spatial-binning.R')

ui <- fluidPage(
	sidebarLayout(
		sidebarPanel(
			actionButton("goButton", "Render map"),
			textOutput("zoomLevel")
		),
		mainPanel(
	 		leafletOutput("map", width = "1024", height = "1024")
		)
	)
)

server <- function(input, output, session) {
	
	getMap <- eventReactive(input$goButton, {

                bounds <- input$map_bounds
		origZoom <- input$map_zoom

                latRng <- range(bounds$north, bounds$south)
                lngRng <- range(bounds$east, bounds$west)
                latLng <- c(bounds$west, bounds$north)
                latLng <- rbind(latLng, c(bounds$east, bounds$north))
                latLng <- rbind(latLng, c(bounds$east, bounds$south))
                latLng <- rbind(latLng, c(bounds$west, bounds$south))
                latLng <- rbind(latLng, c(bounds$west, bounds$north))

                p = Polygon(latLng)
                ps = Polygons(list(p),1)
                sps = SpatialPolygons(list(ps))
		centroid <- gCentroid(sps)

		if (input$map_zoom >= 16)
			mapName <- "nl_hd_0_0025"
		else if (input$map_zoom >= 14)
			mapName <- "nl_hd_0_0075"
		else if (input$map_zoom >= 13)
			mapName <- "nl_hd_0_0100"
		else if (input$map_zoom >= 12)
			mapName <- "nl_0_0150"
		else if (input$map_zoom >= 11)
			mapName <- "nl_0_0175"
		else if (input$map_zoom >= 10)
			mapName <- "nl_0_0500"
		else
			mapName <- "nl_0_1000"

		map <- loadMap(mapName, sps)
		map %>% setView(map, lat = centroid$x, lng = centroid$y, zoom = origZoom)
		
		map

	})

   	output$map <- renderLeaflet({

		cat("bar\n", file=stderr())
		cat("bar\n", file=stdout())
		print("loll")

		if (input$goButton == 0)
			loadMap("nl_0_0500")
		else
			getMap()
	})

	output$zoomLevel <- renderText({
		paste("Current zoom level:" ,input$map_zoom)
	})

}

shinyApp(ui, server)
