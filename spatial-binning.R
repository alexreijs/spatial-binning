library(plyr)
library(rgeos)
library(leaflet)
library(grDevices)
library(gridExtra)
library(maptools)

setwd("~/Development/Repos/spatial-binning")

remove_outliers <- function(x, na.rm = TRUE, ...) {
    qnt <- quantile(x, probs=c(0, .99999), na.rm = na.rm, ...)
    H <- 1.5 * IQR(x, na.rm = na.rm)
    y <- x
    y[x < (qnt[1] - H)] <- NA
    y[x > (qnt[2] + H)] <- NA
    y
}

loadSpatialData <- function(map, fileName, area) {
    
    if (!file.exists(paste(fileName, ".csv", sep = "")))
        stop("Could not find CSV file")
    
    if (!file.exists('maps'))
        dir.create('maps')
    
    mapPath <- paste('maps/', fileName, '.rds', sep = "")
    
    if (!file.exists(mapPath) || class(area) == "SpatialPolygons") {
    #if (1) {
    
        data <- read.csv(paste(fileName, ".csv", sep = ""), sep = ",", colClasses = c("character", "character", "integer"))
            
        colnames(data) <- c("Bin_ID", "Bin_Text", "Impressions")
        
        data$Impressions <- round_any(data$Impressions, 10, f = ceiling)
        
        data <- data[!(data$Impressions %in% setdiff(data$Impressions, remove_outliers(data$Impressions))), ]
        
        #edges <- ams@polygons[[1]]@Polygons[[1]]@coords
        #map <- fitBounds(map, edges[1, 1], edges[1, 2], edges[3, 1], edges[3, 2])
        #map <- fitBounds(map, 4.738625, 52.428634, 5.032166, 52.27933)
        
        pal <- colorNumeric(
          palette = colorRampPalette(c("blue", "cyan", "green", "yellow", "red"))(max(data$Impressions)),
          domain = c(min(data$Impressions), max(data$Impressions))
        )
        
        
        uniqueImpressions <- unique(data$Impressions)
        
        for (x in 1:length(uniqueImpressions)) {
          
            dataSplit <- data[data$Impressions == uniqueImpressions[x], ]
            spatialPolygons <- numeric(0)
            
            for (i in 1:nrow(dataSplit)) { 
                spatialPolygon <- readWKT(dataSplit$Bin_Text[i], dataSplit$Bin_ID[i])
                
                if (class(area) == "SpatialPolygons" && !gContains(area, gCentroid(spatialPolygon)))
                    next
                
                if (length(spatialPolygons) == 0)
                    spatialPolygons <- spatialPolygon
                else
                    spatialPolygons <- rbind(spatialPolygons, spatialPolygon)
            }
                
            ## DIKKE VERGEET NIET JE PADDING NAAR 4 COLUMNS AAN TE PASSEN
            
            if (length(spatialPolygons) > 0) {
                spatialPolygonsUnion <- gUnaryUnion(spatialPolygons)
                map <- addPolygons(map, data = spatialPolygonsUnion, smoothFactor = 0.0, fillOpacity = 0.5, stroke=FALSE, col = pal(uniqueImpressions[x]))  
            }
    
        }
        
        if (class(area) != "SpatialPolygons")
            saveRDS(map, file = mapPath)
    
    }
    else 
        map <- readRDS(mapPath)
  
    return(map)
  
}

loadMap <- function(mapName, area = FALSE) {
    
    map <- leaflet()
    map <- addTiles(map)
    
    #ams <- readWKT("POLYGON ((4.738625 52.428634, 5.008476 52.425913, 5.032166 52.279332, 4.726265 52.276601, 4.738625 52.428634))")
    map <- loadSpatialData(map, mapName, area)
    
    map

}