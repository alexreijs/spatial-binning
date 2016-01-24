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
			actionButton("loadMapButton", "Render map"),
			p(),
			textOutput("zoomLevel"),
			p(),
			actionButton("impressionsButton", "Get impressions"),
			p(),
			textOutput("Impressions")
		),
		mainPanel(
	 		leafletOutput("map", width = "1024", height = "1024")
		)
	)
)

server <- function(input, output, session) {
	

	getDataFromArea <- eventReactive(input$impressionsButton, {
		mapPolygons <- subsetSpatialData(prepareSpatialData("nl_0_0175"), getBounds())
		mapPolygons@data
	})


	getBounds <- function() {
                bounds <- input$map_bounds

                latRng <- range(bounds$north, bounds$south)
                lngRng <- range(bounds$east, bounds$west)
                latLng <- c(bounds$west, bounds$north)
                latLng <- rbind(latLng, c(bounds$east, bounds$north))
                latLng <- rbind(latLng, c(bounds$east, bounds$south))
                latLng <- rbind(latLng, c(bounds$west, bounds$south))
                latLng <- rbind(latLng, c(bounds$west, bounds$north))

                p = Polygon(latLng)
                ps = Polygons(list(p),1)

                SpatialPolygons(list(ps))
	}

	getMap <- eventReactive(input$loadMapButton, {

		origZoom <- input$map_zoom

                sps = getBounds()

		centroid <- gCentroid(sps)

		if (input$map_zoom >= 16)
			mapName <- "nl_hd_0_0025"
		if (input$map_zoom >= 15)
			mapName <- "nl_hd_0_0050"
		else if (input$map_zoom >= 14)
			mapName <- "nl_hd_0_0075"
		else if (input$map_zoom >= 13)
			mapName <- "nl_hd_0_0075"
		else if (input$map_zoom >= 12)
			mapName <- "nl_hd_0_0100"
		else if (input$map_zoom >= 11)
			mapName <- "nl_hd_0_0150"
		else if (input$map_zoom >= 10)
			mapName <- "nl_0_0175"
		else
			mapName <- "nl_0_0250"

		map <- loadMap(mapName, sps)
		map %>% setView(map, lat = centroid$x, lng = centroid$y, zoom = origZoom)
		
		map

	})

   	output$map <- renderLeaflet({
		if (input$loadMapButton == 0)
			loadMap("nl_0_0250")
		else
			getMap()
	})

	output$zoomLevel <- renderText({
		paste("Current zoom level:", input$map_zoom)
	})


	output$Impressions <- renderText({
		if (input$impressionsButton == 0)
			"Press button once to calculate impressions (it may take some time)"
		else {
			polygonsDataFrame <- getDataFromArea()
			paste("Impressions in area", sum(polygonsDataFrame$Impressions))
		}
	})


}

shinyApp(ui, server)
