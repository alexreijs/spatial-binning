library(plyr)
library(rgeos)
library(leaflet)
library(grDevices)
library(gridExtra)
library(maptools)

remove_outliers <- function(x, na.rm = TRUE, ...) {
    qnt <- quantile(x, probs=c(0, .99999), na.rm = na.rm, ...)
    H <- 1.5 * IQR(x, na.rm = na.rm)
    y <- x
    y[x < (qnt[1] - H)] <- NA
    y[x > (qnt[2] + H)] <- NA
    y
}

loadSpatialData <- function(map, fileName, area) {
    
    dataFile <- paste('data/', fileName, ".csv", sep = "")
    
    if (!file.exists(dataFile))
        stop("Could not find CSV file")
    
    if (!file.exists('maps'))
        dir.create('maps')
    
    mapPath <- paste('maps/', fileName, '.rds', sep = "")
    
    if (!file.exists(mapPath) || class(area) == "SpatialPolygons") {
    #if (1) {
    
        data <- read.csv(dataFile, colClasses = c("character", "character", "integer"))
        colnames(data) <- c("Bin_ID", "Bin_Text", "Impressions")
        row.names(data) <- data$Bin_ID
        
        data$Impressions <- round_any(data$Impressions, 10, f = ceiling)
        
        spatialPolygons <- numeric(0)
        dataPolygons <- data
        
        for (i in 1:nrow(data)) { 
            spatialPolygon <- readWKT(data$Bin_Text[i], data$Bin_ID[i])
            
            if (class(area) == "SpatialPolygons" && !gContains(area, gCentroid(spatialPolygon))) {
                dataPolygons <- subset(dataPolygons, Bin_ID != data$Bin_ID[i])
                next
            }
            
            if (length(spatialPolygons) == 0)
                spatialPolygons <- spatialPolygon
            else
                spatialPolygons <- rbind(spatialPolygons, spatialPolygon)    
        }
        
        mapPolygons <- SpatialPolygonsDataFrame(spatialPolygons, dataPolygons[-2])
        
        pal <- colorNumeric(
            palette = colorRampPalette(c("blue", "cyan", "green", "yellow", "red"))(max(mapPolygons@data$Impressions) - min(mapPolygons@data$Impressions) + 1),
            domain = c(min(mapPolygons@data$Impressions), max(mapPolygons@data$Impressions))
        )  

        for (i in 1:length(mapPolygons@polygons)) {
            polygon <- mapPolygons@polygons[[i]]
            map <- addPolygons(map, data = polygon, smoothFactor = 0.0, fillOpacity = 0.5, stroke=FALSE, col = pal(mapPolygons@data$Impressions[i]))  
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
