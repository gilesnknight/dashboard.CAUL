#' Maps canopy cover for structure
#'
#'@param df sf object
#'@param structureIsSuburb TRUE/FALSE
#'@param structureID Unique identifier for structure
#'@param structureName Name for structure polygon
#'@param structureTree Canopy cover for structures
#'@param minZoom Minimum map zoom level (1-12)
#'@param maxZoom Maximum map zoom level (1-12)
#'@param lng1 Longitude of top left bounding box
#'@param lat1 Latitude of top left bounding box
#'@param lng2 Longitude of bottom right bounding box
#'@param lat2 Latitude of bottom right bounding box
#'@param viewLng
#'@param viewLat
#'@param viewZoom
#'
#'@return leaflet map of structure with tree canopy symbology
#'
#' @importFrom magrittr %>% 
#' @export
#' 


base_map <- function() {
  leaflet::leaflet() %>%
    # leaflet::addTiles()
    leaflet::addTiles(urlTemplate = 'https://api.mapbox.com/styles/v1/gilesnknight/ckjje9wsc0jf11ale4cvociam/tiles/256/{z}/{x}/{y}@2x?access_token=pk.eyJ1IjoiZ2lsZXNua25pZ2h0IiwiYSI6ImNraGJwZjh4ejBjem0yeW1wMDd1aGtsdXIifQ.AMSHzUdFu0oZa_rxHnPPKQ',
                      attribution = "© <a href='https://www.mapbox.com/map-feedback/'>Mapbox</a>")
}

map_add_polys <- function(df,
                          mapID,
                          structureID,
                          structureName,
                          structureTree,
                          structureShrub,
                          structureGrass,
                          minZoom = NA,
                          maxZoom = NA,
                          lng1 = NA,
                          lat1 = NA,
                          lng2 = NA,
                          lat2 = NA,
                          viewLng = NA,
                          viewLat = NA,
                          viewZoom = NA) {
  
  #browser()
  
  pal <- colorNumeric(palette = "Greens",
                      domain = structureTree)
  leaflet::leafletProxy(mapId = mapID) %>%
  leaflet::clearShapes() %>%
  leaflet::clearControls() %>%
    leaflet::addPolygons(
      data = df,
      layer = ~ structureID,
      color = "#000000",
      weight = 1,
      fillOpacity = 0.7,
      fillColor = ~ pal(structureTree),
      highlight = highlightOptions(
        weight = 3.5,
        color = "red",
        opacity = 0.4,
        bringToFront = FALSE,
        sendToBack = TRUE
      ),
      label = map_labels(
        labelName = structureName,
        labelTree = structureTree,
        labelShrub = structureShrub,
        labelGrass = structureGrass
      ),
      labelOptions = labelOptions(
        style = list(
          "font-weight" = "normal",
          padding = "3px 8px",
          textsize = "15px",
          direction = "auto"
        )
      )
    ) %>%
    leaflet::addLegend(
      pal = pal,
      opacity = 0.7,
      title = "<center>Tree<br>Canopy<br>Cover</center>",
      position = "topright",
      values = structureTree,
      labFormat = labelFormat(suffix = "%")
    ) %>%
    leaflet::setMaxBounds(
      lng1 = lng1,
      lat1 = lat1,
      lng2 = lng2,
      lat2 = lat2
    ) %>%
    leaflet::setView(lng = viewLng,
                     lat = viewLat,
                     zoom = viewZoom)

}










canopy_map <- function(
  df,
  structureIsSuburb,
  structureID, 
  structureName,
  structureTree, 
  structureShrub,
  structureGrass,
  minZoom = NA,
  maxZoom = NA,
  lng1 = NA, 
  lat1 = NA, 
  lng2 = NA, 
  lat2 = NA,
  viewLng = NA,
  viewLat = NA,
  viewZoom = NA
){
  
  #df %>% dplyr::mutate(df, structureTree = structureTree*100)
  
  pal <- colorNumeric(
    palette = "Greens",
    domain = structureTree
  )
  
  if(structureIsSuburb == TRUE){
  leaflet::leaflet() %>% 
    leaflet::addTiles(urlTemplate = 'https://api.mapbox.com/styles/v1/gilesnknight/ckjje9wsc0jf11ale4cvociam/tiles/256/{z}/{x}/{y}@2x?access_token=pk.eyJ1IjoiZ2lsZXNua25pZ2h0IiwiYSI6ImNraGJwZjh4ejBjem0yeW1wMDd1aGtsdXIifQ.AMSHzUdFu0oZa_rxHnPPKQ', 
                      attribution = "© <a href='https://www.mapbox.com/map-feedback/'>Mapbox</a>") %>% 
    # leaflet::addProviderTiles(providers$CartoDB.PositronNoLabels,
    #                           options = providerTileOptions(minZoom = minZoom,
    #                                                         maxZoom = maxZoom)) %>% 
    leaflet::addPolygons(
      data = df,
      layer = ~structureID,
      color = "#000000",
      weight = 1,
      fillOpacity = 0.7,
      fillColor = ~pal(structureTree),
      highlight = highlightOptions(
        weight = 3.5,
        color = "red",
        opacity = 0.4,
        bringToFront = FALSE,
        sendToBack = TRUE
      ),
      label = map_labels(labelName = structureName,
                         labelTree = structureTree,
                         labelShrub = structureShrub,
                         labelGrass = structureGrass),
      labelOptions = labelOptions(
        style = list("font-weight" = "normal", padding = "3px 8px",
                     textsize = "15px",
                     direction = "auto")
      )
    ) %>% 
    leaflet::addLegend(pal = pal,
  opacity = 0.7, 
  title = "<center>Tree<br>Canopy<br>Cover</center>", 
  position = "topright",
  values = structureTree,
  labFormat = labelFormat(suffix = "%")) %>% 
  leaflet::setMaxBounds(
    lng1 = lng1,
    lat1 = lat1,
    lng2 = lng2,
    lat2 = lat2) %>% 
  leaflet::setView(
    lng = viewLng,
    lat = viewLat,
    zoom = viewZoom
  )
  }
  else{
    leaflet::leaflet() %>% 
      leaflet::addProviderTiles(providers$CartoDB.PositronNoLabels,
                                options = providerTileOptions(minZoom = minZoom,
                                                              maxZoom = maxZoom)) %>% 
      leaflet::addPolygons(
        data = df,
        layer = ~structureID,
        color = "#000000",
        weight = 1,
        fillOpacity = 0.7,
        fillColor = ~pal(structureTree),
        highlight = highlightOptions(
          weight = 3.5,
          color = "#666",
          fillOpacity = 0.7,
          bringToFront = TRUE
        ),
        label = map_labels(structureName = structureName,
                           structurePercent = structureTree),
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px",
                       textsize = "15px",
                       direction = "auto")
        )
      ) %>% 
      leaflet::addLegend(pal = pal,
                         values = structureTree, 
                         opacity = 0.7, 
                         title = "Tree Cover (%)", 
                         position = "topright") %>% 
      leaflet::setMaxBounds(
        lng1 = lng1,
        lat1 = lat1,
        lng2 = lng2,
        lat2 = lat2)
  }

}

