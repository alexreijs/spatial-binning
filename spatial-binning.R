library(plyr)
library(rgeos)
library(leaflet)
library(grDevices)
library(gridExtra)
library(maptools)

logTime <- function(action) {
    print(action)
    print(proc.time() - ptm)
}

remove_outliers <- function(x, lower, upper, na.rm = TRUE, ...) {
    qnt <- quantile(x, probs=c(lower, upper), na.rm = na.rm, ...)
    H <- 1.5 * IQR(x, na.rm = na.rm)
    y <- x
    y[x < (qnt[1] - H)] <- NA
    y[x > (qnt[2] + H)] <- NA
    y
}


subsetSpatialData <- function(mapPolygons, area) {   
    inArea <- numeric(0)
    
    for (i in 1:length(mapPolygons@polygons)) {
        spatialPolygon <- SpatialPolygons(list(mapPolygons@polygons[[i]]))
        
        if (gContains(area, gCentroid(spatialPolygon))) {
            if (length(inArea) == 0)
                inArea <- i
            else
                inArea <- rbind(inArea, i)                  
        }
    }
        
    inAreaPolygons <- SpatialPolygons(mapPolygons@polygons[inArea])
    inAreaDataFrame <- data.frame(Bin_ID = mapPolygons@data$Bin_ID[inArea], Impressions = mapPolygons@data$Impressions[inArea])
    row.names(inAreaDataFrame) <- inAreaDataFrame$Bin_ID
    
    SpatialPolygonsDataFrame(inAreaPolygons, inAreaDataFrame)  
}

prepareSpatialData <- function(fileName) {
    
    ptm <<- proc.time()
    csvFile <- paste('data/', fileName, ".csv", sep = "")
    dataFile <- paste('data/', fileName, ".rds", sep = "")
    
    if (!file.exists(csvFile))
        stop(paste("Could not find CSV file -", csvFile))
 
    if (!file.exists(dataFile)) {    
        data <- read.csv(csvFile, colClasses = c("character", "character", "integer"))
        colnames(data) <- c("Bin_ID", "Bin_Text", "Impressions")
        row.names(data) <- data$Bin_ID
                
        spatialPolygons <- numeric(0)
        dataPolygons <- data
        
        for (i in 1:nrow(data)) { 
            spatialPolygon <- readWKT(data$Bin_Text[i], data$Bin_ID[i])
            
            if (length(spatialPolygons) == 0)
                spatialPolygons <- spatialPolygon
            else
                spatialPolygons <- append(spatialPolygons, spatialPolygon)    
        }
        
        spatialPolygons <- do.call("rbind", spatialPolygons)
        mapPolygons <- SpatialPolygonsDataFrame(spatialPolygons, dataPolygons[-2])
        saveRDS(mapPolygons, file = dataFile)
    }
    else
        mapPolygons <- readRDS(dataFile)

   return(mapPolygons)
}


loadSpatialData <- function(map, fileName, area) {
    
    ptm <<- proc.time()
    
    if (!file.exists('maps'))
        dir.create('maps')
    
    mapPath <- paste('maps/', fileName, '.rds', sep = "")
    
    if (!file.exists(mapPath) || class(area) == "SpatialPolygons") {
        mapPolygons <- prepareSpatialData(fileName)    
        mapPolygons@data$Impressions <- round_any(mapPolygons@data$Impressions, 10, f = ceiling)
    
        logTime(paste("Prepared spatial data - loaded", nrow(mapPolygons), "rows of data"))
                
        if (class(area) == "SpatialPolygons") {
            mapPolygons <- subsetSpatialData(mapPolygons, area)    
            logTime(paste("Subsetted spatial data -", nrow(mapPolygons), "remaining rows of data"))
        }
    
        
        polygonArea <- gArea(SpatialPolygons(list(mapPolygons@polygons[[1]])))
        
        if (polygonArea <= 0.00006)
            upper <- 0.925
        else if (polygonArea <= 0.0001)
            upper <- 0.95
        else if (polygonArea <= 0.00015625)
            upper <- 0.975
        else
            upper <- 0.9999
        
        mapPolygons@data$Impressions[mapPolygons@data$Impressions %in% setdiff(mapPolygons@data$Impressions, remove_outliers(mapPolygons@data$Impressions, 0, upper))] <- NA
        logTime(paste("Removed outliers -", sum(!is.na(mapPolygons@data$Impressions)), "remaining rows of data"))
                
        pal <- colorNumeric(
            palette = colorRampPalette(c("blue", "cyan", "green", "yellow", "red"))(max(mapPolygons@data$Impressions, na.rm = TRUE) - min(mapPolygons@data$Impressions, na.rm = TRUE) + 1),
            domain = c(min(mapPolygons@data$Impressions, na.rm = TRUE), max(mapPolygons@data$Impressions, na.rm = TRUE)),
            na.color = "white"
        )  
        
        logTime("Created palette")
        

        if (nrow(mapPolygons) > 500) {
            spatialPolygonsUnion <- mapply(function(polygons) {
                if (length(polygons) > 1)
                    gUnionCascaded(SpatialPolygons(polygons))
                else
                    SpatialPolygons(list(polygons[[1]]))
            }, split(mapPolygons@polygons, mapPolygons@data$Impressions))
            
            logTime(paste("Unioned spacial data -", length(spatialPolygonsUnion), "union polygons"))
            
            for (i in 1:length(spatialPolygonsUnion)) {
                polygons <- spatialPolygonsUnion[[i]]
                map <- addPolygons(map, data = polygons, smoothFactor = 0.0, fillOpacity = 0.5, stroke=FALSE, col = pal(as.integer(names(spatialPolygonsUnion)[i])))  
            }   
        }
        else {
            for (i in 1:length(mapPolygons@polygons)) {
                polygons <- mapPolygons@polygons[[i]]
                map <- addPolygons(map, data = polygons, smoothFactor = 0.0, fillOpacity = 0.5, stroke=FALSE, col = pal(mapPolygons@data$Impressions[i]))  
            }   
        }
        
        logTime("Plotted spatial data")
        
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
