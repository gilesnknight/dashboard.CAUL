#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @importFrom magrittr "%>%"
#' @import dplyr 
#' @noRd
#' 


# IMPORT DATA --------------------------------------------------------------

PER_SSC_GEO <- sf::st_read("inst/extdata/PER_SSC_GEO.gpkg")
PER_SSC_DATA <- base::readRDS("inst/extdata/PER_SSC_DATA.rds")

MEL_SSC_GEO <- sf::st_read("inst/extdata/MEL_SSC_GEO.gpkg")
MEL_SSC_DATA <- base::readRDS("inst/extdata/MEL_SSC_DATA.rds")

SYD_SSC_GEO <- sf::st_read("inst/extdata/SYD_SSC_GEO.gpkg")
SYD_SSC_DATA <- base::readRDS("inst/extdata/SYD_SSC_DATA.rds")

#ALL_SSC_DATA <- base::readRDS("inst/extdata/ALL_SSC_DATA.rds")

# Server ------------------------------------------------------------------

app_server <- function( input, output, session ) {
  
  observeEvent(input$navbar, {
    print(input$navbar)
  })

# Waiter ----------------------------------------------------------------

  
  
  # g <- waiter::Garcon$new("myImage",
  #                         filter = "opacity")
  # 
  # 
  # for(i in 1:10){
  #   Sys.sleep(runif(1))
  #   g$set(i * 10)
  # }
  # 
  # g2 <- waiter::Garcon$new("myImage2",
  #                         filter = "opacity")
  # 
  # 
  # for(i in 1:10){
  #   Sys.sleep(runif(1))
  #   g2$set(i * 10)
  # }
  # 


  
# PER SSC Map -----------------------------------------------------------------

  # Output PER basemap
  output$PER_SSC_map <- renderLeaflet({
    base_map()
  })
  
  # Removes SYD SSC polygons when SYD tab is not visible
  observeEvent(input$navbar,{
    req(input$navbar!='Perth')
    
    leafletProxy("PER_SSC_map", data = PER_SSC_GEO) %>%
      clearShapes() %>%
      clearControls()
  })

  # Add PER SSC polygons to PER basemap when PER tab is visible
  observeEvent(input$navbar,{
    req(input$navbar=='Perth')

    map_add_polys(
      df = PER_SSC_GEO,
      mapID = "PER_SSC_map",
      structureID = PER_SSC_GEO$SSC_CODE16,
      structureName = PER_SSC_GEO$SSC_NAME16,
      structureTree = PER_SSC_GEO$PerAnyTree,
      structureShrub = PER_SSC_GEO$PerGrass,
      structureGrass = PER_SSC_GEO$PerShrub,
      minZoom = 8,
      maxZoom = 14,
      lng1 = 115.191,
      lat1 = -31.243,
      lng2 = 116.743,
      lat2 = -33.116,
      viewLng = 115.850,
      viewLat = -32.100,
      viewZoom = 10
    )
    
    leaflet::leafletProxy("PER_SSC_map") %>%
      leaflet::addPolylines(
        data = (PER_SSC_GEO[PER_SSC_GEO$SSC_CODE16 ==  PER_SSC$active,]),
        fillOpacity = 0,
        color = "red",
        opacity = 1,
        weight = 3.5,
        stroke = T,
        layerId = "GCC",
        options = pathOptions(interactive = FALSE)
      )
    
  })

# PER SSC Plots -----------------------------------------------------------


    
  # Output PER bar charts
  output$PER_barcharts <- ggiraph::renderGirafe({
    
      # Default PER bar chart data 
      PER_SSC_bar_data <- filter_SSC(
        df = PER_SSC_DATA,
        uniqueID = PER_SSC_DATA$SSC_CODE16,
        clickID = PER_SSC$active
      )
      
      # Filters PER_SSC_bar_data for the vegetation-type bar chart
      PER_vegtype_bar_data <- filter_barchart(
        PER_SSC_bar_data,
        columnsToPlot = c('PerGrass', 'PerShrub', 'PerAnyTree', 'PerNonVeg'),
        newNames = c('Grass', 'Shrub', 'Tree', 'Non-veg'),
        order = c('Non-veg', 'Grass', 'Shrub', 'Tree')
      )
      
      
      # Filters PER_SSC_pie_data for the land tenure bar chart
      PER_privpubl_bar_data <- filter_barchart(
        PER_SSC_bar_data,
        columnsToPlot = c('TrPriv', 'TrPubl'),
        newNames = c('Private', 'Public'),
        order = c('Private', 'Public')
      )
      
      # Filters PER_SSC_bar_data for the land use bar chart
      PER_landuse_bar_data <- filter_barchart(PER_SSC_bar_data,
                                              columnsToPlot = c('ArResPer', 'ArParkPer', 'ArInfrPer', 'ArOthPer',
                                                                'ArIndlPer', 'ArEduPer', 'ArCommPer', 'ArHospPer',
                                                                'ArTransPer', 'ArWatPer', 'ArPrimPPer'),
                                              newNames = c('Residential', 'Parkland', 'Infrastructure', 'Other', 
                                                           'Industrial', 'Education', 'Commercial', 'Hospital', 
                                                           'Transport', 'Water', 'Primary Production'),
                                              order = c('Primary Production', 'Water','Transport', 
                                                        'Hospital', 'Commercial','Education',
                                                        'Industrial','Other', 'Infrastructure',
                                                        'Parkland','Residential')
      )
      
      # Filters PER_SSC_bar_data for the TREE land use bar chart
      PER_treelanduse_bar_data <- filter_barchart(PER_SSC_bar_data,
                                                  columnsToPlot = c('TrResPer', 'TrParkPer', 'TrInfrPer', 'TrOthPer',
                                                                    'TrIndlPer', 'TrEduPer', 'TrCommPer', 'TrHospPer',
                                                                    'TrTransPer', 'TrWatPer', 'TrPrimPPer'),
                                                  newNames = c('Residential', 'Parkland', 'Infrastructure', 'Other', 
                                                               'Industrial', 'Education', 'Commercial', 'Hospital', 
                                                               'Transport', 'Water', 'Primary Production'),
                                                  order = c('Primary Production', 'Water','Transport', 
                                                            'Hospital', 'Commercial','Education',
                                                            'Industrial','Other', 'Infrastructure',
                                                            'Parkland','Residential')
      )
      
      # Plots bar charts
      landuseBarcharts(vegtypeData = PER_vegtype_bar_data,
                       vegtypeVals = PER_vegtype_bar_data$percent,
                       vegtypeGroups = PER_vegtype_bar_data$type,
                       tenureData = PER_privpubl_bar_data, 
                       tenureVals = PER_privpubl_bar_data$percent, 
                       tenureGroups = PER_privpubl_bar_data$type,
                       landuseData = PER_landuse_bar_data,
                       landuseVals = PER_landuse_bar_data$percent,
                       landuseGroups = PER_landuse_bar_data$type,
                       treelanduseData = PER_treelanduse_bar_data,
                       treelanduseVals = PER_treelanduse_bar_data$percent,
                       treelanduseGroups = PER_treelanduse_bar_data$type)
    #}
  })
  
  # Output PER density scatters
  output$PER_densityScatter <- ggiraph::renderGirafe({

      # PER scatter chart data - PER_SSC$active
      PER_SSC_scatter_data <- filter_scatter(
        PER_SSC_DATA,
        columnsToPlot = c(
          'SSC_CODE16',
          'SSC_NAME16',
          'PerAnyTree',
          'GrDwDens',
          'GrDenQuint',
          'UrbDwDens',
          'UrbDenQuin',
          'ResDwDens',
          'ResDenQuin'
        ),
        uniqueID = PER_SSC$active
      )
      
      # Filters PER_SSC_scatter_data to just the selected suburb
      PER_SSC_scatter_selected_data <- scatter_selected_data(df = PER_SSC_scatter_data,
                                                             uniqueID = 'SSC_CODE16',
                                                             activeID = PER_SSC$active)
      # Returns the selected suburb's quintile
      PER_SSC_scatter_selected_quint <- scatter_selected_quint(df = PER_SSC_scatter_selected_data,
                                                               quintileNum = 'GrDenQuint')
      # Filters PER_SSC_scatter_data the above quintile, minus the selected suburb
      PER_SSC_scatter_quint_data <- scatter_quint_data(df = PER_SSC_scatter_data,
                                                       quintileNum = 'GrDenQuint',
                                                       selectedQuintile = PER_SSC_scatter_selected_quint,
                                                       uniqueID = 'SSC_CODE16',
                                                       activeID = PER_SSC$active)
      # Filters PER_SSC_scatter_data to remaining suburbs in other quintiles
      PER_SSC_scatter_remaining_data <- scatter_remaining_data(df = PER_SSC_scatter_data,
                                                               quintileNum = 'GrDenQuint',
                                                               selectedQuintile = PER_SSC_scatter_selected_quint)
      # Plots scatter
      densityScatter(
        scatter_selected_data = PER_SSC_scatter_selected_data,
        scatter_quint_data = PER_SSC_scatter_quint_data,
        scatter_remaining_data = PER_SSC_scatter_remaining_data,
        uniqueID = 'SSC_CODE16',
        yAxis = 'PerAnyTree',
        xAxis = input$PER_dens,
        structureName = 'SSC_NAME16'
      )
  })

# PER Active SSC ----------------------------------------------------------

  PER_map_SSC <- reactiveValues(click = vector(mode = 'numeric'))
  PER_SSC <- reactiveValues(active = vector(mode = 'numeric'))
  
  # Records clicked SSC ID from map and updates SelectizeInput
  observeEvent(input$PER_SSC_map_shape_click, {
    click <- isolate(input$PER_SSC_map_shape_click)
    isolate({
      PER_map_SSC$click = click$id
    })
    updateSelectizeInput(inputId = 'PER_SSC_dropdown',
                         session = getDefaultReactiveDomain(),
                         selected = (PER_SSC_DATA[PER_SSC_DATA[['SSC_CODE16']] ==  PER_map_SSC$click, ]['SSC_NAME16']),
                         choices = PER_SSC_DATA$SSC_NAME1)
  })
  
  # Records clicked SSC ID from suburb comparison plot and updates SelectizeInput
  observeEvent(input$PER_densityScatter_selected, {
    updateSelectizeInput(inputId = 'PER_SSC_dropdown',
                         session = getDefaultReactiveDomain(),
                         selected = (PER_SSC_DATA[PER_SSC_DATA[['SSC_CODE16']] ==  input$PER_densityScatter_selected, ]['SSC_NAME16']),
                         choices = PER_SSC_DATA$SSC_NAME1)
  })
  
  # Updates PER_SSC$active from SelectizeInput
  observeEvent(input$PER_SSC_dropdown, {
    if(input$PER_SSC_dropdown==""){
      return(NULL)
    }
    else{
      PER_SSC$active <- base::as.numeric((PER_SSC_DATA[PER_SSC_DATA[['SSC_NAME16']] ==  input$PER_SSC_dropdown, ]['SSC_CODE16']))
      print(PER_SSC$active)
    }
  })
  
  #  Updates map when PER_SSC$active changes e.g. from drop down or map click
  observeEvent(PER_SSC$active, {
    leaflet::leafletProxy("PER_SSC_map") %>%
            leaflet::addPolylines(
              data = (PER_SSC_GEO[PER_SSC_GEO$SSC_CODE16 ==  PER_SSC$active,]),
              fillOpacity = 0,
              color = "red",
              opacity = 1,
              weight = 3.5,
              stroke = T,
              layerId = "GCC",
              options = pathOptions(interactive = FALSE)
            )
  })


# MEL SSC Map -------------------------------------------------------------

  # Output MEL basemap
  output$MEL_SSC_map <- renderLeaflet({
    base_map()
  })
  
  # Removes SYD SSC polygons when MEL tab is not visible
  observeEvent(input$navbar,{
    req(input$navbar!='Melbourne')
    
    leafletProxy("MEL_SSC_map", data = PER_SSC_GEO) %>%
      clearShapes() %>%
      clearControls()
  })
  
  
  # Add MEL SSC polygons to MEL basemap when MEL tab is visible
  observeEvent(input$navbar,{
    req(input$navbar=='Melbourne')

    map_add_polys(
      df = MEL_SSC_GEO,
      mapID = "MEL_SSC_map",
      structureID = MEL_SSC_GEO$SSC_CODE16,
      structureName = MEL_SSC_GEO$SSC_NAME16,
      structureTree = MEL_SSC_GEO$PerAnyTree,
      structureShrub = MEL_SSC_GEO$PerGrass,
      structureGrass = MEL_SSC_GEO$PerShrub,
      minZoom = 8,
       maxZoom = 14,
       lng1 = 144.39203,
       lat1 = -37.31006,
       lng2=145.80494,
       lat2= -38.53502,
       viewLng = 144.96592,
       viewLat = -37.83361,
       viewZoom = 10
    )
    
    leaflet::leafletProxy("MEL_SSC_map") %>%
      leaflet::addPolylines(
        data = (MEL_SSC_GEO[MEL_SSC_GEO$SSC_CODE16 ==  MEL_SSC$active,]),
        fillOpacity = 0,
        color = "red",
        opacity = 1,
        weight = 3.5,
        stroke = T,
        layerId = "GCC",
        options = pathOptions(interactive = FALSE)
      )
    
  })

# MEL SSC Plots -----------------------------------------------------------

  # Output MEL bar charts
  output$MEL_barcharts <- ggiraph::renderGirafe({
    
    # Default MEL bar chart data 
    MEL_SSC_bar_data <- filter_SSC(
      df = MEL_SSC_DATA,
      uniqueID = MEL_SSC_DATA$SSC_CODE16,
      clickID = MEL_SSC$active
    )
    
    # Filters MEL_SSC_bar_data for the vegetation-type bar chart
    MEL_vegtype_bar_data <- filter_barchart(
      MEL_SSC_bar_data,
      columnsToPlot = c('PerGrass', 'PerShrub', 'PerAnyTree', 'PerNonVeg'),
      newNames = c('Grass', 'Shrub', 'Tree', 'Non-veg'),
      order = c('Non-veg', 'Grass', 'Shrub', 'Tree')
    )
    
    
    # Filters MEL_SSC_pie_data for the land tenure bar chart
    MEL_privpubl_bar_data <- filter_barchart(
      MEL_SSC_bar_data,
      columnsToPlot = c('TrPriv', 'TrPubl'),
      newNames = c('Private', 'Public'),
      order = c('Private', 'Public')
    )
    
    # Filters MEL_SSC_bar_data for the land use bar chart
    MEL_landuse_bar_data <- filter_barchart(MEL_SSC_bar_data,
                                            columnsToPlot = c('ArResPer', 'ArParkPer', 'ArInfrPer', 'ArOthPer',
                                                              'ArIndlPer', 'ArEduPer', 'ArCommPer', 'ArHospPer',
                                                              'ArTransPer', 'ArWatPer', 'ArPrimPPer'),
                                            newNames = c('Residential', 'Parkland', 'Infrastructure', 'Other', 
                                                         'Industrial', 'Education', 'Commercial', 'Hospital', 
                                                         'Transport', 'Water', 'Primary Production'),
                                            order = c('Primary Production', 'Water','Transport', 
                                                      'Hospital', 'Commercial','Education',
                                                      'Industrial','Other', 'Infrastructure',
                                                      'Parkland','Residential')
    )
    
    # Filters MEL_SSC_bar_data for the TREE land use bar chart
    MEL_treelanduse_bar_data <- filter_barchart(MEL_SSC_bar_data,
                                                columnsToPlot = c('TrResPer', 'TrParkPer', 'TrInfrPer', 'TrOthPer',
                                                                  'TrIndlPer', 'TrEduPer', 'TrCommPer', 'TrHospPer',
                                                                  'TrTransPer', 'TrWatPer', 'TrPrimPPer'),
                                                newNames = c('Residential', 'Parkland', 'Infrastructure', 'Other', 
                                                             'Industrial', 'Education', 'Commercial', 'Hospital', 
                                                             'Transport', 'Water', 'Primary Production'),
                                                order = c('Primary Production', 'Water','Transport', 
                                                          'Hospital', 'Commercial','Education',
                                                          'Industrial','Other', 'Infrastructure',
                                                          'Parkland','Residential')
    )
    
    # Plots bar charts
    landuseBarcharts(vegtypeData = MEL_vegtype_bar_data,
                     vegtypeVals = MEL_vegtype_bar_data$percent,
                     vegtypeGroups = MEL_vegtype_bar_data$type,
                     tenureData = MEL_privpubl_bar_data, 
                     tenureVals = MEL_privpubl_bar_data$percent, 
                     tenureGroups = MEL_privpubl_bar_data$type,
                     landuseData = MEL_landuse_bar_data,
                     landuseVals = MEL_landuse_bar_data$percent,
                     landuseGroups = MEL_landuse_bar_data$type,
                     treelanduseData = MEL_treelanduse_bar_data,
                     treelanduseVals = MEL_treelanduse_bar_data$percent,
                     treelanduseGroups = MEL_treelanduse_bar_data$type)
    #}
  })
  
  # Output MEL density scatters
  output$MEL_densityScatter <- ggiraph::renderGirafe({
    
    # MEL scatter chart data - MEL_SSC$active
    MEL_SSC_scatter_data <- filter_scatter(
      MEL_SSC_DATA,
      columnsToPlot = c(
        'SSC_CODE16',
        'SSC_NAME16',
        'PerAnyTree',
        'GrDwDens',
        'GrDenQuint',
        'UrbDwDens',
        'UrbDenQuin',
        'ResDwDens',
        'ResDenQuin'
      ),
      uniqueID = MEL_SSC$active
    )
    
    # Filters MEL_SSC_scatter_data to just the selected suburb
    MEL_SSC_scatter_selected_data <- scatter_selected_data(df = MEL_SSC_scatter_data,
                                                           uniqueID = 'SSC_CODE16',
                                                           activeID = MEL_SSC$active)
    # Returns the selected suburb's quintile
    MEL_SSC_scatter_selected_quint <- scatter_selected_quint(df = MEL_SSC_scatter_selected_data,
                                                             quintileNum = 'GrDenQuint')
    # Filters MEL_SSC_scatter_data the above quintile, minus the selected suburb
    MEL_SSC_scatter_quint_data <- scatter_quint_data(df = MEL_SSC_scatter_data,
                                                     quintileNum = 'GrDenQuint',
                                                     selectedQuintile = MEL_SSC_scatter_selected_quint,
                                                     uniqueID = 'SSC_CODE16',
                                                     activeID = MEL_SSC$active)
    # Filters MEL_SSC_scatter_data to remaining suburbs in other quintiles
    MEL_SSC_scatter_remaining_data <- scatter_remaining_data(df = MEL_SSC_scatter_data,
                                                             quintileNum = 'GrDenQuint',
                                                             selectedQuintile = MEL_SSC_scatter_selected_quint)
    # Plots scatter
    densityScatter(
      scatter_selected_data = MEL_SSC_scatter_selected_data,
      scatter_quint_data = MEL_SSC_scatter_quint_data,
      scatter_remaining_data = MEL_SSC_scatter_remaining_data,
      uniqueID = 'SSC_CODE16',
      yAxis = 'PerAnyTree',
      xAxis = input$MEL_dens,
      structureName = 'SSC_NAME16'
    )
  })
  
# MEL Active SSC ----------------------------------------------------------
  MEL_map_SSC <- reactiveValues(click = vector(mode = 'numeric'))
  MEL_SSC <- reactiveValues(active = vector(mode = 'numeric'))
  
  # Records clicked SSC ID from map and updates SelectizeInput
  observeEvent(input$MEL_SSC_map_shape_click, {
    click <- isolate(input$MEL_SSC_map_shape_click)
    isolate({
      MEL_map_SSC$click = click$id
    })
    updateSelectizeInput(inputId = 'MEL_SSC_dropdown',
                         session = getDefaultReactiveDomain(),
                         selected = (MEL_SSC_DATA[MEL_SSC_DATA[['SSC_CODE16']] ==  MEL_map_SSC$click, ]['SSC_NAME16']),
                         choices = MEL_SSC_DATA$SSC_NAME1)
  })
  
  # Records clicked SSC ID from suburb comparison plot and updates SelectizeInput
  observeEvent(input$MEL_densityScatter_selected, {
    updateSelectizeInput(inputId = 'MEL_SSC_dropdown',
                         session = getDefaultReactiveDomain(),
                         selected = (MEL_SSC_DATA[MEL_SSC_DATA[['SSC_CODE16']] ==  input$MEL_densityScatter_selected, ]['SSC_NAME16']),
                         choices = MEL_SSC_DATA$SSC_NAME1)
  })
  
  # Updates MEL_SSC$active from SelectizeInput
  observeEvent(input$MEL_SSC_dropdown, {
    if(input$MEL_SSC_dropdown==""){
      return(NULL)
    }
    else{
      MEL_SSC$active <- base::as.numeric((MEL_SSC_DATA[MEL_SSC_DATA[['SSC_NAME16']] ==  input$MEL_SSC_dropdown, ]['SSC_CODE16']))
      print(MEL_SSC$active)
    }
  })
  
  #  Updates map when MEL_SSC$active changes e.g. from drop down or map click
  observeEvent(MEL_SSC$active, {
    leaflet::leafletProxy("MEL_SSC_map") %>%
      leaflet::addPolylines(
        data = (MEL_SSC_GEO[MEL_SSC_GEO$SSC_CODE16 ==  MEL_SSC$active,]),
        fillOpacity = 0,
        color = "red",
        opacity = 1,
        weight = 3.5,
        stroke = T,
        layerId = "GCC",
        options = pathOptions(interactive = FALSE)
      )
  })
  
# SYD SSC Map -------------------------------------------------------------
  
  # Output SYD basemap
  output$SYD_SSC_map <- renderLeaflet({
    base_map()
  })
  
  # Removes SYD SSC polygons when SYD tab is not visible
  observeEvent(input$navbar,{
    req(input$navbar!='Sydney')
    
    leafletProxy("SYD_SSC_map", data = PER_SSC_GEO) %>%
      clearShapes() %>%
      clearControls()
  })
  
  # Add SYD SSC polygons to SYD basemap when SYD tab is visible
  observeEvent(input$navbar,{
    req(input$navbar=='Sydney')
    
    map_add_polys(
      df = SYD_SSC_GEO,
      mapID = "SYD_SSC_map",
      structureID = SYD_SSC_GEO$SSC_CODE16,
      structureName = SYD_SSC_GEO$SSC_NAME16,
      structureTree = SYD_SSC_GEO$PerAnyTree,
      structureShrub = SYD_SSC_GEO$PerGrass,
      structureGrass = SYD_SSC_GEO$PerShrub,
      minZoom = 8,
      maxZoom = 14,
      lng1 = 150.37754,
      lat1 = -32.70499,
      lng2= 151.90976,
      lat2= -34.83072,
      viewLng = 151.24835,
      viewLat = -33.82814,
      viewZoom = 10
    )
    
    leaflet::leafletProxy("SYD_SSC_map") %>%
      leaflet::addPolylines(
        data = (SYD_SSC_GEO[SYD_SSC_GEO$SSC_CODE16 ==  SYD_SSC$active,]),
        fillOpacity = 0,
        color = "red",
        opacity = 1,
        weight = 3.5,
        stroke = T,
        layerId = "GCC",
        options = pathOptions(interactive = FALSE)
      )
    
  })
  
# SYD SSC Plots -----------------------------------------------------------
  
  # Output SYD bar charts
  output$SYD_barcharts <- ggiraph::renderGirafe({
    
    # Default SYD bar chart data 
    SYD_SSC_bar_data <- filter_SSC(
      df = SYD_SSC_DATA,
      uniqueID = SYD_SSC_DATA$SSC_CODE16,
      clickID = SYD_SSC$active
    )
    
    # Filters SYD_SSC_bar_data for the vegetation-type bar chart
    SYD_vegtype_bar_data <- filter_barchart(
      SYD_SSC_bar_data,
      columnsToPlot = c('PerGrass', 'PerShrub', 'PerAnyTree', 'PerNonVeg'),
      newNames = c('Grass', 'Shrub', 'Tree', 'Non-veg'),
      order = c('Non-veg', 'Grass', 'Shrub', 'Tree')
    )
    
    
    # Filters SYD_SSC_pie_data for the land tenure bar chart
    SYD_privpubl_bar_data <- filter_barchart(
      SYD_SSC_bar_data,
      columnsToPlot = c('TrPriv', 'TrPubl'),
      newNames = c('Private', 'Public'),
      order = c('Private', 'Public')
    )
    
    # Filters SYD_SSC_bar_data for the land use bar chart
    SYD_landuse_bar_data <- filter_barchart(SYD_SSC_bar_data,
                                            columnsToPlot = c('ArResPer', 'ArParkPer', 'ArInfrPer', 'ArOthPer',
                                                              'ArIndlPer', 'ArEduPer', 'ArCommPer', 'ArHospPer',
                                                              'ArTransPer', 'ArWatPer', 'ArPrimPPer'),
                                            newNames = c('Residential', 'Parkland', 'Infrastructure', 'Other', 
                                                         'Industrial', 'Education', 'Commercial', 'Hospital', 
                                                         'Transport', 'Water', 'Primary Production'),
                                            order = c('Primary Production', 'Water','Transport', 
                                                      'Hospital', 'Commercial','Education',
                                                      'Industrial','Other', 'Infrastructure',
                                                      'Parkland','Residential')
    )
    
    # Filters SYD_SSC_bar_data for the TREE land use bar chart
    SYD_treelanduse_bar_data <- filter_barchart(SYD_SSC_bar_data,
                                                columnsToPlot = c('TrResPer', 'TrParkPer', 'TrInfrPer', 'TrOthPer',
                                                                  'TrIndlPer', 'TrEduPer', 'TrCommPer', 'TrHospPer',
                                                                  'TrTransPer', 'TrWatPer', 'TrPrimPPer'),
                                                newNames = c('Residential', 'Parkland', 'Infrastructure', 'Other', 
                                                             'Industrial', 'Education', 'Commercial', 'Hospital', 
                                                             'Transport', 'Water', 'Primary Production'),
                                                order = c('Primary Production', 'Water','Transport', 
                                                          'Hospital', 'Commercial','Education',
                                                          'Industrial','Other', 'Infrastructure',
                                                          'Parkland','Residential')
    )
    
    # Plots bar charts
    landuseBarcharts(vegtypeData = SYD_vegtype_bar_data,
                     vegtypeVals = SYD_vegtype_bar_data$percent,
                     vegtypeGroups = SYD_vegtype_bar_data$type,
                     tenureData = SYD_privpubl_bar_data, 
                     tenureVals = SYD_privpubl_bar_data$percent, 
                     tenureGroups = SYD_privpubl_bar_data$type,
                     landuseData = SYD_landuse_bar_data,
                     landuseVals = SYD_landuse_bar_data$percent,
                     landuseGroups = SYD_landuse_bar_data$type,
                     treelanduseData = SYD_treelanduse_bar_data,
                     treelanduseVals = SYD_treelanduse_bar_data$percent,
                     treelanduseGroups = SYD_treelanduse_bar_data$type)
    #}
  })
  
  # Output SYD density scatters
  output$SYD_densityScatter <- ggiraph::renderGirafe({
    
    # SYD scatter chart data - SYD_SSC$active
    SYD_SSC_scatter_data <- filter_scatter(
      SYD_SSC_DATA,
      columnsToPlot = c(
        'SSC_CODE16',
        'SSC_NAME16',
        'PerAnyTree',
        'GrDwDens',
        'GrDenQuint',
        'UrbDwDens',
        'UrbDenQuin',
        'ResDwDens',
        'ResDenQuin'
      ),
      uniqueID = SYD_SSC$active
    )
    
    # Filters SYD_SSC_scatter_data to just the selected suburb
    SYD_SSC_scatter_selected_data <- scatter_selected_data(df = SYD_SSC_scatter_data,
                                                           uniqueID = 'SSC_CODE16',
                                                           activeID = SYD_SSC$active)
    # Returns the selected suburb's quintile
    SYD_SSC_scatter_selected_quint <- scatter_selected_quint(df = SYD_SSC_scatter_selected_data,
                                                             quintileNum = 'GrDenQuint')
    # Filters SYD_SSC_scatter_data the above quintile, minus the selected suburb
    SYD_SSC_scatter_quint_data <- scatter_quint_data(df = SYD_SSC_scatter_data,
                                                     quintileNum = 'GrDenQuint',
                                                     selectedQuintile = SYD_SSC_scatter_selected_quint,
                                                     uniqueID = 'SSC_CODE16',
                                                     activeID = SYD_SSC$active)
    # Filters SYD_SSC_scatter_data to remaining suburbs in other quintiles
    SYD_SSC_scatter_remaining_data <- scatter_remaining_data(df = SYD_SSC_scatter_data,
                                                             quintileNum = 'GrDenQuint',
                                                             selectedQuintile = SYD_SSC_scatter_selected_quint)
    # Plots scatter
    densityScatter(
      scatter_selected_data = SYD_SSC_scatter_selected_data,
      scatter_quint_data = SYD_SSC_scatter_quint_data,
      scatter_remaining_data = SYD_SSC_scatter_remaining_data,
      uniqueID = 'SSC_CODE16',
      yAxis = 'PerAnyTree',
      xAxis = input$SYD_dens,
      structureName = 'SSC_NAME16'
    )
  })
  
# SYD Active SSC ----------------------------------------------------------
  SYD_map_SSC <- reactiveValues(click = vector(mode = 'numeric'))
  SYD_SSC <- reactiveValues(active = vector(mode = 'numeric'))
  
  # Records clicked SSC ID from map and updates SelectizeInput
  observeEvent(input$SYD_SSC_map_shape_click, {
    click <- isolate(input$SYD_SSC_map_shape_click)
    isolate({
      SYD_map_SSC$click = click$id
    })
    updateSelectizeInput(inputId = 'SYD_SSC_dropdown',
                         session = getDefaultReactiveDomain(),
                         selected = (SYD_SSC_DATA[SYD_SSC_DATA[['SSC_CODE16']] ==  SYD_map_SSC$click, ]['SSC_NAME16']),
                         choices = SYD_SSC_DATA$SSC_NAME1)
  })
  
  # Records clicked SSC ID from suburb comparison plot and updates SelectizeInput
  observeEvent(input$SYD_densityScatter_selected, {
    updateSelectizeInput(inputId = 'SYD_SSC_dropdown',
                         session = getDefaultReactiveDomain(),
                         selected = (SYD_SSC_DATA[SYD_SSC_DATA[['SSC_CODE16']] ==  input$SYD_densityScatter_selected, ]['SSC_NAME16']),
                         choices = SYD_SSC_DATA$SSC_NAME1)
  })
  
  # Updates SYD_SSC$active from SelectizeInput
  observeEvent(input$SYD_SSC_dropdown, {
    if(input$SYD_SSC_dropdown==""){
      return(NULL)
    }
    else{
      SYD_SSC$active <- base::as.numeric((SYD_SSC_DATA[SYD_SSC_DATA[['SSC_NAME16']] ==  input$SYD_SSC_dropdown, ]['SSC_CODE16']))
      print(SYD_SSC$active)
    }
  })
  
  #  Updates map when SYD_SSC$active changes e.g. from drop down or map click
  observeEvent(SYD_SSC$active, {
    leaflet::leafletProxy("SYD_SSC_map") %>%
      leaflet::addPolylines(
        data = (SYD_SSC_GEO[SYD_SSC_GEO$SSC_CODE16 ==  SYD_SSC$active,]),
        fillOpacity = 0,
        color = "red",
        opacity = 1,
        weight = 3.5,
        stroke = T,
        layerId = "GCC",
        options = pathOptions(interactive = FALSE)
      )
  })
  
  
}



