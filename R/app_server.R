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
PER_SSC_DATA <-  dplyr::mutate(PER_SSC_DATA, yaxis_ran = runif(nrow(PER_SSC_DATA), min = 1.98, max =
                                    2.02))
                  

                  

MEL_SSC_GEO <- sf::st_read("inst/extdata/MEL_SSC_GEO.gpkg")
MEL_SSC_DATA <- base::readRDS("inst/extdata/MEL_SSC_DATA.rds")

SYD_SSC_GEO <- sf::st_read("inst/extdata/SYD_SSC_GEO.gpkg")
SYD_SSC_DATA <- base::readRDS("inst/extdata/SYD_SSC_DATA.rds")

#ALL_SSC_DATA <- base::readRDS("inst/extdata/ALL_SSC_DATA.rds")

# Server ------------------------------------------------------------------

app_server <- function( input, output, session ) {
  
  #updateSelectizeInput(session, 'PER_SSC_dropdown', choices = PER_SSC_DATA$SSC_NAME16, server = TRUE)

# Waiter ----------------------------------------------------------------

  
  
  g <- waiter::Garcon$new("myImage",
                          filter = "opacity")


  for(i in 1:10){
    Sys.sleep(runif(1))
    g$set(i * 10)
  }

  g2 <- waiter::Garcon$new("myImage2",
                          filter = "opacity")


  for(i in 1:10){
    Sys.sleep(runif(1))
    g2$set(i * 10)
  }



  
# PER SSC -----------------------------------------------------------------
  
# PER SSC map
  
  # Output PER basemap
  output$PER_SSC_map <- renderLeaflet({
    base_map()
  })

  # Add PER SSC polygons to PER basemap
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
  
  # Output PER piecharts - deprecated
  output$PER_piecharts <- ggiraph::renderGirafe({
    if (is.null(PER_map_SSC$click)) {
      
      # Default PER pie chart data - 51218
      PER_SSC_pie_data <- filter_SSC(
        df = PER_SSC_DATA,
        uniqueID = PER_SSC_DATA$SSC_CODE16,
        clickID = 51218
      )
      
      # Filters PER_SSC_pie_data for the vegetation-type piechart
      PER_vegtype_pie_data <- filter_piechart(
        PER_SSC_pie_data,
        columnsToPlot = c('PerGrass', 'PerShrub', 'PerAnyTree', 'PerNonVeg')
      )
      
      # Filters PER_SSC_pie_data for the land tenure piechart
      PER_privpubl_pie_data <- filter_piechart(
        PER_SSC_pie_data,
        columnsToPlot = c('TrPriv', 'TrPubl')
      )
      
      # Filters PER_SSC_pie_data for the land use piechart
      PER_landuse_pie_data <- filter_piechart(PER_SSC_pie_data,
                      columnsToPlot = c('ArResPer', 'ArParkPer', 'ArInfrPer', 'ArOthPer',
                                        'ArIndlPer', 'ArEduPer', 'ArCommPer', 'ArHospPer',
                                        'ArTransPer', 'ArWatPer', 'ArPrimPPer')
      )
      
      # Filters PER_SSC_pie_data for the TREE land use piechart
      PER_treelanduse_pie_data <- filter_piechart(PER_SSC_pie_data,
                                              columnsToPlot = c('TrResPer', 'TrParkPer', 'TrInfrPer', 'TrOthPer',
                                                                'TrIndlPer', 'TrEduPer', 'TrCommPer', 'TrHospPer',
                                                                'TrTransPer', 'TrWatPer', 'TrPrimPPer')
      )
      
      # Plots Piecharts
      landusePiecharts(vegtypeData = PER_vegtype_pie_data,
                       vegtypeVals = PER_vegtype_pie_data$percent,
                       vegtypeGroups = PER_vegtype_pie_data$type,
                       tenureData = PER_privpubl_pie_data, 
                       tenureVals = PER_privpubl_pie_data$percent, 
                       tenureGroups = PER_privpubl_pie_data$type,
                       landuseData = PER_landuse_pie_data,
                       landuseVals = PER_landuse_pie_data$percent,
                       landuseGroups = PER_landuse_pie_data$type,
                       treelanduseData = PER_treelanduse_pie_data,
                       treelanduseVals = PER_treelanduse_pie_data$percent,
                       treelanduseGroups = PER_treelanduse_pie_data$type,
                       activeSSCname = PER_active_SSC$name)
    }
    else {
      
      # PER pie chart data - updates on map click
      PER_SSC_pie_data <- filter_SSC(
        df = PER_SSC_DATA,
        uniqueID = PER_SSC_DATA$SSC_CODE16,
        clickID = PER_map_SSC$click
      )
      
      # Filters PER_SSC_pie_data for the vegetation-type piechart
      PER_vegtype_pie_data <- filter_piechart(
        PER_SSC_pie_data,
        columnsToPlot = c('PerGrass', 'PerShrub', 'PerAnyTree', 'PerNonVeg')
      )
      
      # Filters PER_SSC_pie_data for the land tenure piechart
      PER_privpubl_pie_data <- filter_piechart(
        PER_SSC_pie_data,
        columnsToPlot = c('TrPriv', 'TrPubl')
      )
      
      # Filters PER_SSC_pie_data for the land use piechart
      PER_landuse_pie_data <- filter_piechart(PER_SSC_pie_data,
                                              columnsToPlot = c('ArResPer', 'ArParkPer', 'ArInfrPer', 'ArOthPer',
                                                                'ArIndlPer', 'ArEduPer', 'ArCommPer', 'ArHospPer',
                                                                'ArTransPer', 'ArWatPer', 'ArPrimPPer')
      )
      
      # Filters PER_SSC_pie_data for the TREE land use piechart
      PER_treelanduse_pie_data <- filter_piechart(PER_SSC_pie_data,
                                                  columnsToPlot = c('TrResPer', 'TrParkPer', 'TrInfrPer', 'TrOthPer',
                                                                    'TrIndlPer', 'TrEduPer', 'TrCommPer', 'TrHospPer',
                                                                    'TrTransPer', 'TrWatPer', 'TrPrimPPer')
      )
      
      # Plots Piecharts
      landusePiecharts(
        vegtypeData = PER_vegtype_pie_data,
        vegtypeVals = PER_vegtype_pie_data$percent,
        vegtypeGroups = PER_vegtype_pie_data$type,
        tenureData = PER_privpubl_pie_data, 
        tenureVals = PER_privpubl_pie_data$percent, 
        tenureGroups = PER_privpubl_pie_data$type,
        landuseData = PER_landuse_pie_data,
        landuseVals = PER_landuse_pie_data$percent,
        landuseGroups = PER_landuse_pie_data$type,
        treelanduseData = PER_treelanduse_pie_data,
        treelanduseVals = PER_treelanduse_pie_data$percent,
        treelanduseGroups = PER_treelanduse_pie_data$type,
        activeSSCname = PER_active_SSC$name
      )
    }
  })
  
  # Output PER psudoscatter - depreceated
  output$PER_psudoscatter <- ggiraph::renderGirafe({
    if (is.null(PER_map_SSC$click)) {
      
      # Default PER scatter chart data - 51218
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
          'ResDenQuin',
          'yaxis_ran'
        ),
        uniqueID = 51218
      )
      
      # Filters PER_SSC_scatter_data to just the selected suburb
      PER_SSC_scatter_selected_data <- scatter_selected_data(df = PER_SSC_scatter_data,
                                                             uniqueID = 'SSC_CODE16',
                                                             activeID = 51218)
      # Returns the selected suburb's quintile
      PER_SSC_scatter_selected_quint <- scatter_selected_quint(df = PER_SSC_scatter_selected_data,
                                                               quintileNum = 'GrDenQuint')
      # Filters PER_SSC_scatter_data the above quintile, minus the selected suburb
      PER_SSC_scatter_quint_data <- scatter_quint_data(df = PER_SSC_scatter_data,
                                                       quintileNum = 'GrDenQuint',
                                                       selectedQuintile = PER_SSC_scatter_selected_quint,
                                                       uniqueID = 'SSC_CODE16',
                                                       activeID = 51218)
      # Filters PER_SSC_scatter_data to remaining suburbs in other quintiles
      PER_SSC_scatter_remaining_data <- scatter_remaining_data(df = PER_SSC_scatter_data,
                                                               quintileNum = 'GrDenQuint',
                                                               selectedQuintile = PER_SSC_scatter_selected_quint)
      # Plots scatter
      pseudoScatter(
        scatter_selected_data = PER_SSC_scatter_selected_data,
        scatter_quint_data = PER_SSC_scatter_quint_data,
        scatter_remaining_data = PER_SSC_scatter_remaining_data,
        uniqueID = 'SSC_CODE16',
        xAxis = 'PerAnyTree',
        yAxis = 'yaxis_ran',
        structureName = 'SSC_NAME16',
        density = 'GrDwDens'
      )
      
    }
    else {
      # Default PER pie chart data - 51218
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
          'ResDenQuin',
          'yaxis_ran'
        ),
        uniqueID = PER_map_SSC$click
      )
      
      # Filters PER_SSC_scatter_data to just the selected suburb
      PER_SSC_scatter_selected_data <- scatter_selected_data(df = PER_SSC_scatter_data,
                                                             uniqueID = 'SSC_CODE16',
                                                             activeID = PER_map_SSC$click)
      # Returns the selected suburb's quintile
      PER_SSC_scatter_selected_quint <- scatter_selected_quint(df = PER_SSC_scatter_selected_data,
                                                               quintileNum = 'GrDenQuint')
      # Filters PER_SSC_scatter_data the above quintile, minus the selected suburb
      PER_SSC_scatter_quint_data <- scatter_quint_data(df = PER_SSC_scatter_data,
                                                       quintileNum = 'GrDenQuint',
                                                       selectedQuintile = PER_SSC_scatter_selected_quint,
                                                       uniqueID = 'SSC_CODE16',
                                                       activeID = PER_map_SSC$click)
      # Filters PER_SSC_scatter_data to remaining suburbs in other quintiles
      PER_SSC_scatter_remaining_data <- scatter_remaining_data(df = PER_SSC_scatter_data,
                                                               quintileNum = 'GrDenQuint',
                                                               selectedQuintile = PER_SSC_scatter_selected_quint)
      # Plots scatter
      pseudoScatter(
        scatter_selected_data = PER_SSC_scatter_selected_data,
        scatter_quint_data = PER_SSC_scatter_quint_data,
        scatter_remaining_data = PER_SSC_scatter_remaining_data,
        uniqueID = 'SSC_CODE16',
        xAxis = 'PerAnyTree',
        yAxis = 'yaxis_ran',
        structureName = 'SSC_NAME16',
        density = 'GrDwDens'
      )
      
    }
  })
  
  # Output PER bar charts
  output$PER_barcharts <- ggiraph::renderGirafe({
    if (is.null(PER_map_SSC$click)) {
      
      # Default PER bar chart data - 51218
      PER_SSC_bar_data <- filter_SSC(
        df = PER_SSC_DATA,
        uniqueID = PER_SSC_DATA$SSC_CODE16,
        clickID = 51218
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
      PER_landuse_bar_data <- filter_piechart(PER_SSC_bar_data,
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
      PER_treelanduse_bar_data <- filter_piechart(PER_SSC_bar_data,
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
    }
    else {
      
      # Default PER bar chart data 
      PER_SSC_bar_data <- filter_SSC(
        df = PER_SSC_DATA,
        uniqueID = PER_SSC_DATA$SSC_CODE16,
        clickID = PER_map_SSC$click
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
    }
  })

#   
# # MEL SSC -----------------------------------------------------------------
#   
# # MEL SSC map
#   
#   output$MEL_SSC_map <- renderLeaflet({
#     canopy_map(df = MEL_SSC_GEO,
#                structureIsSuburb = TRUE,
#                structureID = MEL_SSC_GEO$SSC_CODE16,
#                structureName = MEL_SSC_GEO$SSC_NAME16,
#                structureTree = MEL_SSC_GEO$PerAnyTree,
#                structureShrub = MEL_SSC_GEO$PerGrass,
#                structureGrass = MEL_SSC_GEO$PerShrub,
#                minZoom = 8,
#                maxZoom = 14,
#                lng1 = 144.39203,
#                lat1 = -37.31006,
#                lng2=145.80494,
#                lat2= -38.53502,
#                viewLng = 144.96592,
#                viewLat = -37.83361,
#                viewZoom = 10
#     )
#   })  
#   
#   # MEL SSC Tree Shrub Grass Non-veg piechart 
#   
#   MEL_vegtype_pie_data <- reactive({
#     if(!is.null(MEL_map_SSC$click)){
#       filter_piechart(MEL_SSC_DATA,
#                       uniqueID = MEL_SSC_DATA$SSC_CODE16,
#                       clickID = MEL_map_SSC$click,
#                       columnsToPlot = c('PerGrass', 'PerShrub', 'PerAnyTree', 'PerNonVeg')
#       )
#     }
#     else {
#       return(NULL)
#     }
#   })
#   
#   output$MEL_vegtype_pie <- plotly::renderPlotly({
#     MEL_vegtype_pie <- MEL_vegtype_pie_data()
#     if(!is.null(MEL_vegtype_pie)){
#       vegtype_pie(df = MEL_vegtype_pie,
#                   pieVals = MEL_vegtype_pie$percent
#       )
#     }
#     else {
#       return(NULL)
#     }
#   })
#   
#   # MEL SSC Priv Publ piechart
#   
#   MEL_privpubl_pie_data <- reactive({
#     if(!is.null(MEL_map_SSC$click)){
#       filter_piechart(MEL_SSC_DATA,
#                       uniqueID = MEL_SSC_DATA$SSC_CODE16,
#                       clickID = MEL_map_SSC$click,
#                       columnsToPlot = c('TrPriv', 'TrPubl')
#       )
#     }
#     else {
#       return(NULL)
#     }
#   })
#   
#   output$MEL_privpubl_pie <- plotly::renderPlotly({
#     MEL_privpubl_pie <- MEL_privpubl_pie_data()
#     if(!is.null(MEL_privpubl_pie)){
#       privpubl_pie(df = MEL_privpubl_pie,
#                    pieVals = MEL_privpubl_pie$percent
#       )
#     }
#     else {
#       return(NULL)
#     }
#   })
#   
#   # MEL SSC Land Use piechart
#   
#   MEL_LU_pie_data <- reactive({
#     if(!is.null(MEL_map_SSC$click)){
#       filter_piechart(MEL_SSC_DATA,
#                       uniqueID = MEL_SSC_DATA$SSC_CODE16,
#                       clickID = MEL_map_SSC$click,
#                       columnsToPlot = c('ArResPer', 'ArParkPer', 'ArInfrPer', 'ArOthPer', 
#                                         'ArIndlPer', 'ArEduPer', 'ArCommPer', 'ArHospPer',
#                                         'ArTransPer', 'ArWatPer', 'ArPrimPPer')
#       )
#     }
#     else {
#       return(NULL)
#     }
#   })
#   
#   output$MEL_LU_pie <- plotly::renderPlotly({
#     MEL_LU_pie <- MEL_LU_pie_data()
#     if(!is.null(MEL_LU_pie)){
#       LU_pie(df = MEL_LU_pie,
#              pieVals = MEL_LU_pie$percent
#       )
#     }
#     else {
#       return(NULL)
#     }
#   })
#   
#   # MEL SSC TREE Land Use piechart
#   
#   MEL_TrLU_pie_data <- reactive({
#     if(!is.null(MEL_map_SSC$click)){
#       filter_piechart(MEL_SSC_DATA,
#                       uniqueID = MEL_SSC_DATA$SSC_CODE16,
#                       clickID = MEL_map_SSC$click,
#                       columnsToPlot = c('TrResPer', 'TrParkPer', 'TrInfrPer', 'TrOthPer',
#                                         'TrIndlPer', 'TrEduPer', 'TrCommPer', 'TrHospPer',
#                                         'TrTransPer', 'TrWatPer', 'TrPrimPPer')
#       )
#     }
#     else {
#       return(NULL)
#     }
#   })
#   
#   output$MEL_TrLU_pie <- plotly::renderPlotly({
#     MEL_TrLU_pie <- MEL_TrLU_pie_data()
#     if(!is.null(MEL_TrLU_pie)){
#       TrLU_pie(df = MEL_TrLU_pie,
#                pieVals = MEL_TrLU_pie$percent
#       )
#     }
#     else {
#       return(NULL)
#     }
#   })
#   
#   # MEL SSC filter scatter data
#   
#   MEL_SSC_scatter_data <- filter_scatter(
#     MEL_SSC_DATA,
#     columnsToPlot = c('SSC_CODE16','SSC_NAME16','PerAnyTree', 'GrDwDens', 'GrDenQuint', 'UrbDwDens', 'UrbDenQuin', 'ResDwDens', 'ResDenQuin'),
#     uniqueID = MEL_SSC_DATA$SSC_CODE16
#     
#   )
#   
#   
#   # Renders MEL SSC SCATTER GROSS
#   output$MEL_SSC_gross_scatter <- plotly::renderPlotly({
#     scatter_plot(df = MEL_SSC_scatter_data,
#                  xVals = MEL_SSC_scatter_data$PerAnyTree,
#                  yVals = MEL_SSC_scatter_data$yaxis_ran,
#                  source = 'MEL_SSC_plot1',
#                  structureName = MEL_SSC_scatter_data$SSC_NAME16,
#                  plotTitle = "<b>Tree Canopy (%) & Gross Density Quintiles</b>",
#                  tree = MEL_SSC_scatter_data$PerAnyTree,
#                  densityQuintile = MEL_SSC_scatter_data$GrDenQuint,
#                  density = MEL_SSC_scatter_data$GrDwDens)
#   })
#   
#   observe({
#     if (!is.null(MEL_map_SSC$click)){
#       selected_quintile <- base::as.numeric(MEL_SSC_scatter_data[MEL_SSC_scatter_data[['SSC_CODE16']] == MEL_map_SSC$click, ]['GrDenQuint'])
#       
#       MEL_SSC_selected_quintile <- filter(MEL_SSC_scatter_data, GrDenQuint == selected_quintile)
#       
#       plotly::plotlyProxy("MEL_SSC_gross_scatter") %>%
#         plotly::plotlyProxyInvoke("deleteTraces", list(1)) %>%
#         plotly::plotlyProxyInvoke(
#           "addTraces",
#           list(
#             x = MEL_SSC_selected_quintile$PerAnyTree,
#             y = MEL_SSC_selected_quintile$yaxis_ran,
#             type = 'line',
#             mode = 'markers',
#             marker = list(size = 10,
#                           opacity = 0.8,
#                           color = '#4292c6'),
#             showlegend = FALSE,
#             hoverinfo = 'text',
#             text = quin_hover_text(
#               structureName = MEL_SSC_selected_quintile$SSC_NAME16,
#               tree = MEL_SSC_selected_quintile$PerAnyTree,
#               densityQuintile = MEL_SSC_selected_quintile$GrDenQuint,
#               density = MEL_SSC_selected_quintile$GrDwDens
#             )
#           )
#         ) %>%
#         plotly::plotlyProxyInvoke("relayout",
#                                   list(shapes = list(
#                                     scatter_quinline(
#                                       df = MEL_SSC_scatter_data,
#                                       uniqueID = "SSC_CODE16",
#                                       clickID = MEL_map_SSC$click,
#                                       groupID = "GrDenQuint",
#                                       xVals = "PerAnyTree"
#                                     ),
#                                     scatter_mean(df = MEL_SSC_scatter_data,
#                                                  xVals = "PerAnyTree")
#                                   ),
#                                   annotations = list(scatter_annotation(
#                                     df = MEL_SSC_selected_quintile,
#                                     uniqueID = "SSC_CODE16",
#                                     clickID = MEL_map_SSC$click,
#                                     xVals = "PerAnyTree",
#                                     yVals = "yaxis_ran",
#                                     structureName = "SSC_NAME16"
#                                   ),
#                                   scatter_quinline_label(
#                                     df = MEL_SSC_selected_quintile,
#                                     uniqueID = "SSC_CODE16",
#                                     clickID = MEL_map_SSC$click,
#                                     groupID = "GrDenQuint",
#                                     xVals = "PerAnyTree"
#                                   ),
#                                   scatter_mean_label(df = MEL_SSC_scatter_data, xVals = "PerAnyTree"))))
#     } 
#     else{
#       return(NULL)
#     }
#   })
#   
#   # Renders MEL SSC SCATTER URBAN
#   output$MEL_SSC_urban_scatter <- plotly::renderPlotly({
#     scatter_plot(df = MEL_SSC_scatter_data,
#                  xVals = MEL_SSC_scatter_data$PerAnyTree,
#                  yVals = MEL_SSC_scatter_data$yaxis_ran,
#                  source = 'MEL_SSC_plot1',
#                  structureName = MEL_SSC_scatter_data$SSC_NAME16,
#                  plotTitle = "<b>Tree Canopy (%) & Urban Density Quintiles</b>",
#                  tree = MEL_SSC_scatter_data$PerAnyTree,
#                  densityQuintile = MEL_SSC_scatter_data$UrbDenQuin,
#                  density = MEL_SSC_scatter_data$UrbDwDens)
#   })
#   
#   observe({
#     #if (!is.null(MEL_map_SSC$click)){
#     selected_quintile <- base::as.numeric(MEL_SSC_scatter_data[MEL_SSC_scatter_data[['SSC_CODE16']] == MEL_map_SSC$click, ]['UrbDenQuin'])
#     
#     MEL_SSC_selected_quintile <- filter(MEL_SSC_scatter_data, UrbDenQuin == selected_quintile)
#     
#     plotly::plotlyProxy("MEL_SSC_urban_scatter") %>%
#       plotly::plotlyProxyInvoke("deleteTraces", list(1)) %>%
#       plotly::plotlyProxyInvoke(
#         "addTraces",
#         list(
#           x = MEL_SSC_selected_quintile$PerAnyTree,
#           y = MEL_SSC_selected_quintile$yaxis_ran,
#           type = 'line',
#           mode = 'markers',
#           marker = list(size = 10,
#                         opacity = 0.8,
#                         color = '#4292c6'),
#           showlegend = FALSE,
#           hoverinfo = 'text',
#           text = quin_hover_text(
#             structureName = MEL_SSC_selected_quintile$SSC_NAME16,
#             tree = MEL_SSC_selected_quintile$PerAnyTree,
#             densityQuintile = MEL_SSC_selected_quintile$UrbDenQuin,
#             density = MEL_SSC_selected_quintile$UrbDwDens
#           )
#         )
#       ) %>%
#       plotly::plotlyProxyInvoke("relayout",
#                                 list(shapes = list(
#                                   scatter_quinline(
#                                     df = MEL_SSC_scatter_data,
#                                     uniqueID = "SSC_CODE16",
#                                     clickID = MEL_map_SSC$click,
#                                     groupID = "UrbDenQuin",
#                                     xVals = "PerAnyTree"
#                                   ),
#                                   scatter_mean(df = MEL_SSC_scatter_data,
#                                                xVals = "PerAnyTree")
#                                 ),
#                                 annotations = list(scatter_annotation(
#                                   df = MEL_SSC_selected_quintile,
#                                   uniqueID = "SSC_CODE16",
#                                   clickID = MEL_map_SSC$click,
#                                   xVals = "PerAnyTree",
#                                   yVals = "yaxis_ran",
#                                   structureName = "SSC_NAME16"
#                                 ),
#                                 scatter_quinline_label(
#                                   df = MEL_SSC_selected_quintile,
#                                   uniqueID = "SSC_CODE16",
#                                   clickID = MEL_map_SSC$click,
#                                   groupID = "UrbDenQuin",
#                                   xVals = "PerAnyTree"
#                                 ),
#                                 scatter_mean_label(df = MEL_SSC_scatter_data, xVals = "PerAnyTree"))))
#     # } 
#     # else{
#     #   return(NULL)
#     # }
#   })
#   
#   # Renders MEL SSC SCATTER RES
#   output$MEL_SSC_res_scatter <- plotly::renderPlotly({
#     scatter_plot(df = MEL_SSC_scatter_data,
#                  xVals = MEL_SSC_scatter_data$PerAnyTree,
#                  yVals = MEL_SSC_scatter_data$yaxis_ran,
#                  source = 'MEL_SSC_plot1',
#                  structureName = MEL_SSC_scatter_data$SSC_NAME16,
#                  plotTitle = "<b>Tree Canopy (%) & Residential Density Quintiles</b>",
#                  tree = MEL_SSC_scatter_data$PerAnyTree,
#                  densityQuintile = MEL_SSC_scatter_data$ResDenQuin,
#                  density = MEL_SSC_scatter_data$ResDwDens)
#   })
#   
#   observe({
#     if (!is.null(MEL_map_SSC$click)){
#       selected_quintile <- base::as.numeric(MEL_SSC_scatter_data[MEL_SSC_scatter_data[['SSC_CODE16']] == MEL_map_SSC$click, ]['ResDenQuin'])
#       
#       MEL_SSC_selected_quintile <- filter(MEL_SSC_scatter_data, ResDenQuin == selected_quintile)
#       
#       plotly::plotlyProxy("MEL_SSC_res_scatter") %>%
#         plotly::plotlyProxyInvoke("deleteTraces", list(1)) %>%
#         plotly::plotlyProxyInvoke(
#           "addTraces",
#           list(
#             x = MEL_SSC_selected_quintile$PerAnyTree,
#             y = MEL_SSC_selected_quintile$yaxis_ran,
#             type = 'line',
#             mode = 'markers',
#             marker = list(size = 10,
#                           opacity = 0.8,
#                           color = '#4292c6'),
#             showlegend = FALSE,
#             hoverinfo = 'text',
#             text = quin_hover_text(
#               structureName = MEL_SSC_selected_quintile$SSC_NAME16,
#               tree = MEL_SSC_selected_quintile$PerAnyTree,
#               densityQuintile = MEL_SSC_selected_quintile$ResDenQuin,
#               density = MEL_SSC_selected_quintile$ResDwDens
#             )
#           )
#         ) %>%
#         plotly::plotlyProxyInvoke("relayout",
#                                   list(shapes = list(
#                                     scatter_quinline(
#                                       df = MEL_SSC_scatter_data,
#                                       uniqueID = "SSC_CODE16",
#                                       clickID = MEL_map_SSC$click,
#                                       groupID = "ResDenQuin",
#                                       xVals = "PerAnyTree"
#                                     ),
#                                     scatter_mean(df = MEL_SSC_scatter_data,
#                                                  xVals = "PerAnyTree")
#                                   ),
#                                   annotations = list(scatter_annotation(
#                                     df = MEL_SSC_selected_quintile,
#                                     uniqueID = "SSC_CODE16",
#                                     clickID = MEL_map_SSC$click,
#                                     xVals = "PerAnyTree",
#                                     yVals = "yaxis_ran",
#                                     structureName = "SSC_NAME16"
#                                   ),
#                                   scatter_quinline_label(
#                                     df = MEL_SSC_selected_quintile,
#                                     uniqueID = "SSC_CODE16",
#                                     clickID = MEL_map_SSC$click,
#                                     groupID = "ResDenQuin",
#                                     xVals = "PerAnyTree"
#                                   ),
#                                   scatter_mean_label(df = MEL_SSC_scatter_data, xVals = "PerAnyTree"))))
#     } 
#     else{
#       return(NULL)
#     }
#   })  
#   
# # SYD SSC -----------------------------------------------------------------
#   
# # SYD SSC map
#   
#   output$SYD_SSC_map <- renderLeaflet({
#     canopy_map(df = SYD_SSC_GEO,
#                structureIsSuburb = TRUE,
#                structureID = SYD_SSC_GEO$SSC_CODE16,
#                structureName = SYD_SSC_GEO$SSC_NAME16,
#                structureTree = SYD_SSC_GEO$PerAnyTree,
#                structureShrub = SYD_SSC_GEO$PerGrass,
#                structureGrass = SYD_SSC_GEO$PerShrub,
#                minZoom = 8,
#                maxZoom = 14,
#                lng1 = 150.37754,
#                lat1 = -32.70499,
#                lng2= 151.90976,
#                lat2= -34.83072,
#                viewLng = 151.24835,
#                viewLat = -33.82814,
#                viewZoom = 10
#     )
#   })
#   
#   # SYD SSC Tree Shrub Grass Non-veg piechart 
#   
#   SYD_vegtype_pie_data <- reactive({
#     if(!is.null(SYD_map_SSC$click)){
#       filter_piechart(SYD_SSC_DATA,
#                       uniqueID = SYD_SSC_DATA$SSC_CODE16,
#                       clickID = SYD_map_SSC$click,
#                       columnsToPlot = c('PerGrass', 'PerShrub', 'PerAnyTree', 'PerNonVeg')
#       )
#     }
#     else {
#       return(NULL)
#     }
#   })
#   
#   output$SYD_vegtype_pie <- plotly::renderPlotly({
#     SYD_vegtype_pie <- SYD_vegtype_pie_data()
#     if(!is.null(SYD_vegtype_pie)){
#       vegtype_pie(df = SYD_vegtype_pie,
#                   pieVals = SYD_vegtype_pie$percent
#       )
#     }
#     else {
#       return(NULL)
#     }
#   })
#   
#   # SYD SSC Priv Publ piechart
#   
#   SYD_privpubl_pie_data <- reactive({
#     if(!is.null(SYD_map_SSC$click)){
#       filter_piechart(SYD_SSC_DATA,
#                       uniqueID = SYD_SSC_DATA$SSC_CODE16,
#                       clickID = SYD_map_SSC$click,
#                       columnsToPlot = c('TrPriv', 'TrPubl')
#       )
#     }
#     else {
#       return(NULL)
#     }
#   })
#   
#   output$SYD_privpubl_pie <- plotly::renderPlotly({
#     SYD_privpubl_pie <- SYD_privpubl_pie_data()
#     if(!is.null(SYD_privpubl_pie)){
#       privpubl_pie(df = SYD_privpubl_pie,
#                    pieVals = SYD_privpubl_pie$percent
#       )
#     }
#     else {
#       return(NULL)
#     }
#   })
#   
#   # SYD SSC Land Use piechart
#   
#   SYD_LU_pie_data <- reactive({
#     if(!is.null(SYD_map_SSC$click)){
#       filter_piechart(SYD_SSC_DATA,
#                       uniqueID = SYD_SSC_DATA$SSC_CODE16,
#                       clickID = SYD_map_SSC$click,
#                       columnsToPlot = c('ArResPer', 'ArParkPer', 'ArInfrPer', 'ArOthPer', 
#                                         'ArIndlPer', 'ArEduPer', 'ArCommPer', 'ArHospPer',
#                                         'ArTransPer', 'ArWatPer', 'ArPrimPPer')
#       )
#     }
#     else {
#       return(NULL)
#     }
#   })
#   
#   output$SYD_LU_pie <- plotly::renderPlotly({
#     SYD_LU_pie <- SYD_LU_pie_data()
#     if(!is.null(SYD_LU_pie)){
#       LU_pie(df = SYD_LU_pie,
#              pieVals = SYD_LU_pie$percent
#       )
#     }
#     else {
#       return(NULL)
#     }
#   })
#   
#   # SYD SSC TREE Land Use piechart
#   
#   SYD_TrLU_pie_data <- reactive({
#     if(!is.null(SYD_map_SSC$click)){
#       filter_piechart(SYD_SSC_DATA,
#                       uniqueID = SYD_SSC_DATA$SSC_CODE16,
#                       clickID = SYD_map_SSC$click,
#                       columnsToPlot = c('TrResPer', 'TrParkPer', 'TrInfrPer', 'TrOthPer',
#                                         'TrIndlPer', 'TrEduPer', 'TrCommPer', 'TrHospPer',
#                                         'TrTransPer', 'TrWatPer', 'TrPrimPPer')
#       )
#     }
#     else {
#       return(NULL)
#     }
#   })
#   
#   output$SYD_TrLU_pie <- plotly::renderPlotly({
#     SYD_TrLU_pie <- SYD_TrLU_pie_data()
#     if(!is.null(SYD_TrLU_pie)){
#       TrLU_pie(df = SYD_TrLU_pie,
#                pieVals = SYD_TrLU_pie$percent
#       )
#     }
#     else {
#       return(NULL)
#     }
#   })
#   
#   # SYD SSC filter scatter data
#   
#   SYD_SSC_scatter_data <- filter_scatter(
#     SYD_SSC_DATA,
#     columnsToPlot = c('SSC_CODE16','SSC_NAME16','PerAnyTree', 'GrDwDens', 'GrDenQuint', 'UrbDwDens', 'UrbDenQuin', 'ResDwDens', 'ResDenQuin'),
#     uniqueID = SYD_SSC_DATA$SSC_CODE16
#     
#   )
#   
#   
#   # Renders SYD SSC SCATTER GROSS
#   output$SYD_SSC_gross_scatter <- plotly::renderPlotly({
#     scatter_plot(df = SYD_SSC_scatter_data,
#                  xVals = SYD_SSC_scatter_data$PerAnyTree,
#                  yVals = SYD_SSC_scatter_data$yaxis_ran,
#                  source = 'SYD_SSC_plot1',
#                  structureName = SYD_SSC_scatter_data$SSC_NAME16,
#                  plotTitle = "<b>Tree Canopy (%) & Gross Density Quintiles</b>",
#                  tree = SYD_SSC_scatter_data$PerAnyTree,
#                  densityQuintile = SYD_SSC_scatter_data$GrDenQuint,
#                  density = SYD_SSC_scatter_data$GrDwDens)
#   })
#   
#   observe({
#     if (!is.null(SYD_map_SSC$click)){
#       selected_quintile <- base::as.numeric(SYD_SSC_scatter_data[SYD_SSC_scatter_data[['SSC_CODE16']] == SYD_map_SSC$click, ]['GrDenQuint'])
#       
#       SYD_SSC_selected_quintile <- filter(SYD_SSC_scatter_data, GrDenQuint == selected_quintile)
#       
#       plotly::plotlyProxy("SYD_SSC_gross_scatter") %>%
#         plotly::plotlyProxyInvoke("deleteTraces", list(1)) %>%
#         plotly::plotlyProxyInvoke(
#           "addTraces",
#           list(
#             x = SYD_SSC_selected_quintile$PerAnyTree,
#             y = SYD_SSC_selected_quintile$yaxis_ran,
#             type = 'line',
#             mode = 'markers',
#             marker = list(size = 10,
#                           opacity = 0.8,
#                           color = '#4292c6'),
#             showlegend = FALSE,
#             hoverinfo = 'text',
#             text = quin_hover_text(
#               structureName = SYD_SSC_selected_quintile$SSC_NAME16,
#               tree = SYD_SSC_selected_quintile$PerAnyTree,
#               densityQuintile = SYD_SSC_selected_quintile$GrDenQuint,
#               density = SYD_SSC_selected_quintile$GrDwDens
#             )
#           )
#         ) %>%
#         plotly::plotlyProxyInvoke("relayout",
#                                   list(shapes = list(
#                                     scatter_quinline(
#                                       df = SYD_SSC_scatter_data,
#                                       uniqueID = "SSC_CODE16",
#                                       clickID = SYD_map_SSC$click,
#                                       groupID = "GrDenQuint",
#                                       xVals = "PerAnyTree"
#                                     ),
#                                     scatter_mean(df = SYD_SSC_scatter_data,
#                                                  xVals = "PerAnyTree")
#                                   ),
#                                   annotations = list(scatter_annotation(
#                                     df = SYD_SSC_selected_quintile,
#                                     uniqueID = "SSC_CODE16",
#                                     clickID = SYD_map_SSC$click,
#                                     xVals = "PerAnyTree",
#                                     yVals = "yaxis_ran",
#                                     structureName = "SSC_NAME16"
#                                   ),
#                                   scatter_quinline_label(
#                                     df = SYD_SSC_selected_quintile,
#                                     uniqueID = "SSC_CODE16",
#                                     clickID = SYD_map_SSC$click,
#                                     groupID = "GrDenQuint",
#                                     xVals = "PerAnyTree"
#                                   ),
#                                   scatter_mean_label(df = SYD_SSC_scatter_data, xVals = "PerAnyTree"))))
#     } 
#     else{
#       return(NULL)
#     }
#   })
#   
#   # Renders SYD SSC SCATTER URBAN
#   output$SYD_SSC_urban_scatter <- plotly::renderPlotly({
#     scatter_plot(df = SYD_SSC_scatter_data,
#                  xVals = SYD_SSC_scatter_data$PerAnyTree,
#                  yVals = SYD_SSC_scatter_data$yaxis_ran,
#                  source = 'SYD_SSC_plot1',
#                  structureName = SYD_SSC_scatter_data$SSC_NAME16,
#                  plotTitle = "<b>Tree Canopy (%) & Urban Density Quintiles</b>",
#                  tree = SYD_SSC_scatter_data$PerAnyTree,
#                  densityQuintile = SYD_SSC_scatter_data$UrbDenQuin,
#                  density = SYD_SSC_scatter_data$UrbDwDens)
#   })
#   
#   observe({
#     #if (!is.null(SYD_map_SSC$click)){
#     selected_quintile <- base::as.numeric(SYD_SSC_scatter_data[SYD_SSC_scatter_data[['SSC_CODE16']] == SYD_map_SSC$click, ]['UrbDenQuin'])
#     
#     SYD_SSC_selected_quintile <- filter(SYD_SSC_scatter_data, UrbDenQuin == selected_quintile)
#     
#     plotly::plotlyProxy("SYD_SSC_urban_scatter") %>%
#       plotly::plotlyProxyInvoke("deleteTraces", list(1)) %>%
#       plotly::plotlyProxyInvoke(
#         "addTraces",
#         list(
#           x = SYD_SSC_selected_quintile$PerAnyTree,
#           y = SYD_SSC_selected_quintile$yaxis_ran,
#           type = 'line',
#           mode = 'markers',
#           marker = list(size = 10,
#                         opacity = 0.8,
#                         color = '#4292c6'),
#           showlegend = FALSE,
#           hoverinfo = 'text',
#           text = quin_hover_text(
#             structureName = SYD_SSC_selected_quintile$SSC_NAME16,
#             tree = SYD_SSC_selected_quintile$PerAnyTree,
#             densityQuintile = SYD_SSC_selected_quintile$UrbDenQuin,
#             density = SYD_SSC_selected_quintile$UrbDwDens
#           )
#         )
#       ) %>%
#       plotly::plotlyProxyInvoke("relayout",
#                                 list(shapes = list(
#                                   scatter_quinline(
#                                     df = SYD_SSC_scatter_data,
#                                     uniqueID = "SSC_CODE16",
#                                     clickID = SYD_map_SSC$click,
#                                     groupID = "UrbDenQuin",
#                                     xVals = "PerAnyTree"
#                                   ),
#                                   scatter_mean(df = SYD_SSC_scatter_data,
#                                                xVals = "PerAnyTree")
#                                 ),
#                                 annotations = list(scatter_annotation(
#                                   df = SYD_SSC_selected_quintile,
#                                   uniqueID = "SSC_CODE16",
#                                   clickID = SYD_map_SSC$click,
#                                   xVals = "PerAnyTree",
#                                   yVals = "yaxis_ran",
#                                   structureName = "SSC_NAME16"
#                                 ),
#                                 scatter_quinline_label(
#                                   df = SYD_SSC_selected_quintile,
#                                   uniqueID = "SSC_CODE16",
#                                   clickID = SYD_map_SSC$click,
#                                   groupID = "UrbDenQuin",
#                                   xVals = "PerAnyTree"
#                                 ),
#                                 scatter_mean_label(df = SYD_SSC_scatter_data, xVals = "PerAnyTree"))))
#     # } 
#     # else{
#     #   return(NULL)
#     # }
#   })
#   
#   # Renders SYD SSC SCATTER RES
#   output$SYD_SSC_res_scatter <- plotly::renderPlotly({
#     scatter_plot(df = SYD_SSC_scatter_data,
#                  xVals = SYD_SSC_scatter_data$PerAnyTree,
#                  yVals = SYD_SSC_scatter_data$yaxis_ran,
#                  source = 'SYD_SSC_plot1',
#                  structureName = SYD_SSC_scatter_data$SSC_NAME16,
#                  plotTitle = "<b>Tree Canopy (%) & Residential Density Quintiles</b>",
#                  tree = SYD_SSC_scatter_data$PerAnyTree,
#                  densityQuintile = SYD_SSC_scatter_data$ResDenQuin,
#                  density = SYD_SSC_scatter_data$ResDwDens)
#   })
#   
#   observe({
#     if (!is.null(SYD_map_SSC$click)){
#       selected_quintile <- base::as.numeric(SYD_SSC_scatter_data[SYD_SSC_scatter_data[['SSC_CODE16']] == SYD_map_SSC$click, ]['ResDenQuin'])
#       
#       SYD_SSC_selected_quintile <- filter(SYD_SSC_scatter_data, ResDenQuin == selected_quintile)
#       
#       plotly::plotlyProxy("SYD_SSC_res_scatter") %>%
#         plotly::plotlyProxyInvoke("deleteTraces", list(1)) %>%
#         plotly::plotlyProxyInvoke(
#           "addTraces",
#           list(
#             x = SYD_SSC_selected_quintile$PerAnyTree,
#             y = SYD_SSC_selected_quintile$yaxis_ran,
#             type = 'line',
#             mode = 'markers',
#             marker = list(size = 10,
#                           opacity = 0.8,
#                           color = '#4292c6'),
#             showlegend = FALSE,
#             hoverinfo = 'text',
#             text = quin_hover_text(
#               structureName = SYD_SSC_selected_quintile$SSC_NAME16,
#               tree = SYD_SSC_selected_quintile$PerAnyTree,
#               densityQuintile = SYD_SSC_selected_quintile$ResDenQuin,
#               density = SYD_SSC_selected_quintile$ResDwDens
#             )
#           )
#         ) %>%
#         plotly::plotlyProxyInvoke("relayout",
#                                   list(shapes = list(
#                                     scatter_quinline(
#                                       df = SYD_SSC_scatter_data,
#                                       uniqueID = "SSC_CODE16",
#                                       clickID = SYD_map_SSC$click,
#                                       groupID = "ResDenQuin",
#                                       xVals = "PerAnyTree"
#                                     ),
#                                     scatter_mean(df = SYD_SSC_scatter_data,
#                                                  xVals = "PerAnyTree")
#                                   ),
#                                   annotations = list(scatter_annotation(
#                                     df = SYD_SSC_selected_quintile,
#                                     uniqueID = "SSC_CODE16",
#                                     clickID = SYD_map_SSC$click,
#                                     xVals = "PerAnyTree",
#                                     yVals = "yaxis_ran",
#                                     structureName = "SSC_NAME16"
#                                   ),
#                                   scatter_quinline_label(
#                                     df = SYD_SSC_selected_quintile,
#                                     uniqueID = "SSC_CODE16",
#                                     clickID = SYD_map_SSC$click,
#                                     groupID = "ResDenQuin",
#                                     xVals = "PerAnyTree"
#                                   ),
#                                   scatter_mean_label(df = SYD_SSC_scatter_data, xVals = "PerAnyTree"))))
#     } 
#     else{
#       return(NULL)
#     }
#   })  
#   

# Map Click ---------------------------------------------------------------


  # PER
  
  
  
  PER_map_SSC <- reactiveValues(click = vector(mode = 'numeric'))
  PER_map_SSC$click <- 51218
  
  observeEvent(input$PER_SSC_dropdown,{
    print(input$PER_SSC_dropdown)
  })
  
  observeEvent(input$PER_SSC_map_shape_click, {
    click <- isolate(input$PER_SSC_map_shape_click)
    isolate({
      PER_map_SSC$click = click$id
    })
    
    print(PER_map_SSC$click)
    
if (!is.null(PER_map_SSC$click)) {
  leaflet::leafletProxy("PER_SSC_map") %>%
  leaflet::addPolylines(
    data = (PER_SSC_GEO[PER_SSC_GEO$SSC_CODE16 ==  PER_map_SSC$click, ]),
    fillOpacity = 0,
    color = "red",
    opacity = 1,
    weight = 3.5,
    stroke = T,
    layerId = "GCC",
    options = pathOptions(interactive = FALSE)
  )
}
else {
  leaflet::leafletProxy("PER_SSC_map") %>%
  leaflet::removeShape(map = 'PER_SSC_map',
                       layerId = "GCC") %>%
  leaflet::addPolylines(
    data = (PER_SSC_GEO[PER_SSC_GEO$SSC_CODE16 ==  PER_map_SSC$click, ]),
    fillOpacity = 0,
    color = "red",
    opacity = 1,
    weight = 3.5,
    stroke = T,
    layerId = "GCC",
    options = pathOptions(interactive = FALSE)
  )
}

    
  })
  
  
  
  
  PER_active_SSC <- reactiveValues(
    name = vector(mode = 'character')
  )
  
  PER_active_SSC$name <- 'Perth (WA)'
  
  observeEvent(PER_map_SSC$click,{
    
    PER_active_SSC$name <- (PER_SSC_DATA[PER_SSC_DATA[['SSC_CODE16']] ==  PER_map_SSC$click, ]['SSC_NAME16'])
    
  })


  
  # observeEvent(PER_map_SSC$click, {
  #     leaflet::leafletProxy("PER_SSC_map") %>%
  #       leaflet::addPolylines(
  #         data = (PER_SSC_GEO[PER_SSC_GEO$SSC_CODE16 ==  PER_map_SSC$click, ]),
  #         fillOpacity = 0,
  #         color = "red",
  #         opacity = 1,
  #         weight = 3.5,
  #         stroke = T,
  #         layerId = "GCC",
  #         options = pathOptions(interactive = FALSE)
  #       )
  # })
  # 
  # MEL
  
  MEL_map_SSC <- reactiveValues(
    click = vector(mode = 'numeric')
  )
  MEL_map_SSC$click <- 21629
  
  observeEvent(input$MEL_SSC_map_shape_click,{
    click <- isolate(input$MEL_SSC_map_shape_click)
    isolate({MEL_map_SSC$click = click$id})
    print(MEL_map_SSC$click)
  })
  
  # SYD
  
  SYD_map_SSC <- reactiveValues(
    click = vector(mode = 'numeric')
  )
  SYD_map_SSC$click <- 13715
  
  observeEvent(input$SYD_SSC_map_shape_click,{
    click <- isolate(input$SYD_SSC_map_shape_click)
    isolate({SYD_map_SSC$click = click$id})
    print(SYD_map_SSC$click)
  })
  
  
  
  
}



