#' Generates tree canopy map for LGA structures
#' 
#' @param df sf object
#' @param uniqueID A unique identifier for each polygon that will be return during the click event
#' @param lng1 Longitude of top left bounding box
#' @param lat1 Latitude of top left bounding box
#' @param lng2 Longitude of bottom right bounding box
#' @param lat2 Latitude of bottom right bounding box
#' @return leaflet map of LGA structure with tree canopy symbology
#' @export


LGA_map <-  function(df, uniqueID, lng1, lat1, lng2, lat2){
  
  bins <- c(0,5,10,15,20,25,30,35,40,45)
  pal <- leaflet::colorBin("Greens", domain = df$PerAnyTree, bins = bins)
  
  leaflet::leaflet() %>% 
    leaflet::addProviderTiles(providers$CartoDB.PositronNoLabels,
                              options = providerTileOptions(minZoom = 9,
                                                            maxZoom = 12)) %>% 
    leaflet::addPolygons(
      data = df,
      layerId = ~ uniqueID,
      color = "#000000",
      weight = 1,
      fillOpacity = 0.7,
      fillColor = ~pal(PerAnyTree),
      highlight = highlightOptions(
        weight = 3.5,
        color = "#666",
        fillOpacity = 0.7,
        bringToFront = TRUE
      ),
      label = map_labels(structureName = df$LGA_NAME16,
                         structurePercent = df$PerAnyTree),
      labelOptions = labelOptions(
        style = list("font-weight" = "normal", padding = "3px 8px",
                     textsize = "15px",
                     direction = "auto")
      )
    ) %>% 
    leaflet::addLegend(pal = colorBin("Greens", domain = df$PerAnyTree, bins = c(0,5,10,15,20,25,30,35,40,45)),
                       values = df$PerAnyTree, 
                       opacity = 0.7, 
                       title = "Tree Cover (%)", 
                       position = "topright") %>% 
    setMaxBounds(
      lng1 = lng1,
      lat1 = lat1,
      lng2 = lng2,
      lat2 = lat2)
    
}

