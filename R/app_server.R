#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @importFrom magrittr "%>%"
#' @import dplyr 
#' @noRd
#' 


# Data setup --------------------------------------------------------------

# PER import data

#PER_SSC_FULL <- sf::st_read("data-raw/PER_FULL.shp")
PER_SSC_FULL <- sf::st_read("inst/extdata/PER_FULL.shp")
PER_SSC_FULL$PerAnyTree <- PER_SSC_FULL$PerAnyTree*100
PER_SSC_FULL$PerShrub <- PER_SSC_FULL$PerShrub*100
PER_SSC_FULL$PerGrass <- PER_SSC_FULL$PerGrass*100
PER_SSC_FULL$PerNonVeg <- (100 - PER_SSC_FULL$PerAnyTree - PER_SSC_FULL$PerShrub - PER_SSC_FULL$PerGrass)

PER_SSC_FULL$TrPriv <- ((PER_SSC_FULL$TrPriv_ha/PER_SSC_FULL$TrTotT_ha)*100)
PER_SSC_FULL$TrPubl <- ((PER_SSC_FULL$TrPubl_ha/PER_SSC_FULL$TrTotT_ha)*100)

PER_SSC_FULL$ArRes_per <- ((PER_SSC_FULL$ArRes_ha/PER_SSC_FULL$ArTotal_ha)*100)
PER_SSC_FULL$ArPark_per <- ((PER_SSC_FULL$ArPark_ha/PER_SSC_FULL$ArTotal_ha)*100)
PER_SSC_FULL$ArInfr_per <- ((PER_SSC_FULL$ArInfr_ha/PER_SSC_FULL$ArTotal_ha)*100)
PER_SSC_FULL$ArOth_per <- ((PER_SSC_FULL$ArOth_ha/PER_SSC_FULL$ArTotal_ha)*100)
PER_SSC_FULL$ArIndl_per <- ((PER_SSC_FULL$ArIndl_ha/PER_SSC_FULL$ArTotal_ha)*100)
PER_SSC_FULL$ArEdu_per <- ((PER_SSC_FULL$ArEdu_ha/PER_SSC_FULL$ArTotal_ha)*100)
PER_SSC_FULL$ArComm_per <- ((PER_SSC_FULL$ArComm_ha/PER_SSC_FULL$ArTotal_ha)*100)
PER_SSC_FULL$ArHosp_per <- ((PER_SSC_FULL$ArHosp_ha/PER_SSC_FULL$ArTotal_ha)*100)
PER_SSC_FULL$ArTrans_per <- ((PER_SSC_FULL$ArTrans_ha/PER_SSC_FULL$ArTotal_ha)*100)
PER_SSC_FULL$ArWat_per <- ((PER_SSC_FULL$ArWat_ha/PER_SSC_FULL$ArTotal_ha)*100)
PER_SSC_FULL$ArPrimP_per <- ((PER_SSC_FULL$ArPrimP_ha/PER_SSC_FULL$ArTotal_ha)*100)

PER_SSC_FULL$TrRes_per <- ((PER_SSC_FULL$TrRes_ha/PER_SSC_FULL$TrTotal_ha)*100)
PER_SSC_FULL$TrPark_per <- ((PER_SSC_FULL$TrPark_ha/PER_SSC_FULL$TrTotal_ha)*100)
PER_SSC_FULL$TrInfr_per <- ((PER_SSC_FULL$TrInfr_ha/PER_SSC_FULL$TrTotal_ha)*100)
PER_SSC_FULL$TrOth_per <- ((PER_SSC_FULL$TrOth_ha/PER_SSC_FULL$TrTotal_ha)*100)
PER_SSC_FULL$TrIndl_per <- ((PER_SSC_FULL$TrIndl_ha/PER_SSC_FULL$TrTotal_ha)*100)
PER_SSC_FULL$TrEdu_per <- ((PER_SSC_FULL$TrEdu_ha/PER_SSC_FULL$TrTotal_ha)*100)
PER_SSC_FULL$TrComm_per <- ((PER_SSC_FULL$TrComm_ha/PER_SSC_FULL$TrTotal_ha)*100)
PER_SSC_FULL$TrHosp_per <- ((PER_SSC_FULL$TrHosp_ha/PER_SSC_FULL$TrTotal_ha)*100)
PER_SSC_FULL$TrTrans_per <- ((PER_SSC_FULL$TrTran_ha/PER_SSC_FULL$TrTotal_ha)*100)
PER_SSC_FULL$TrWat_per <- ((PER_SSC_FULL$TrWat_ha/PER_SSC_FULL$TrTotal_ha)*100)
PER_SSC_FULL$TrPrimP_per <- ((PER_SSC_FULL$TrPrimP_ha/PER_SSC_FULL$TrTotal_ha)*100)

# MEL import data
#MEL_SSC_FULL <- sf::st_read("data-raw/MEL_FULL.shp")
MEL_SSC_FULL <- sf::st_read("inst/extdata/MEL_FULL.shp")
MEL_SSC_FULL$PerAnyTree <- MEL_SSC_FULL$PerAnyTree*100
MEL_SSC_FULL$PerShrub <- MEL_SSC_FULL$PerShrub*100
MEL_SSC_FULL$PerGrass <- MEL_SSC_FULL$PerGrass*100
MEL_SSC_FULL$PerNonVeg <- (100 - MEL_SSC_FULL$PerAnyTree - MEL_SSC_FULL$PerShrub - MEL_SSC_FULL$PerGrass)

MEL_SSC_FULL$TrPriv <- ((MEL_SSC_FULL$TrPriv_ha/MEL_SSC_FULL$TrTotT_ha)*100)
MEL_SSC_FULL$TrPubl <- ((MEL_SSC_FULL$TrPubl_ha/MEL_SSC_FULL$TrTotT_ha)*100)

MEL_SSC_FULL$ArRes_per <- ((MEL_SSC_FULL$ArRes_ha/MEL_SSC_FULL$ArTotal_ha)*100)
MEL_SSC_FULL$ArPark_per <- ((MEL_SSC_FULL$ArPark_ha/MEL_SSC_FULL$ArTotal_ha)*100)
MEL_SSC_FULL$ArInfr_per <- ((MEL_SSC_FULL$ArInfr_ha/MEL_SSC_FULL$ArTotal_ha)*100)
MEL_SSC_FULL$ArOth_per <- ((MEL_SSC_FULL$ArOth_ha/MEL_SSC_FULL$ArTotal_ha)*100)
MEL_SSC_FULL$ArIndl_per <- ((MEL_SSC_FULL$ArIndl_ha/MEL_SSC_FULL$ArTotal_ha)*100)
MEL_SSC_FULL$ArEdu_per <- ((MEL_SSC_FULL$ArEdu_ha/MEL_SSC_FULL$ArTotal_ha)*100)
MEL_SSC_FULL$ArComm_per <- ((MEL_SSC_FULL$ArComm_ha/MEL_SSC_FULL$ArTotal_ha)*100)
MEL_SSC_FULL$ArHosp_per <- ((MEL_SSC_FULL$ArHosp_ha/MEL_SSC_FULL$ArTotal_ha)*100)
MEL_SSC_FULL$ArTrans_per <- ((MEL_SSC_FULL$ArTrans_ha/MEL_SSC_FULL$ArTotal_ha)*100)
MEL_SSC_FULL$ArWat_per <- ((MEL_SSC_FULL$ArWat_ha/MEL_SSC_FULL$ArTotal_ha)*100)
MEL_SSC_FULL$ArPrimP_per <- ((MEL_SSC_FULL$ArPrimP_ha/MEL_SSC_FULL$ArTotal_ha)*100)

MEL_SSC_FULL$TrRes_per <- ((MEL_SSC_FULL$TrRes_ha/MEL_SSC_FULL$TrTotal_ha)*100)
MEL_SSC_FULL$TrPark_per <- ((MEL_SSC_FULL$TrPark_ha/MEL_SSC_FULL$TrTotal_ha)*100)
MEL_SSC_FULL$TrInfr_per <- ((MEL_SSC_FULL$TrInfr_ha/MEL_SSC_FULL$TrTotal_ha)*100)
MEL_SSC_FULL$TrOth_per <- ((MEL_SSC_FULL$TrOth_ha/MEL_SSC_FULL$TrTotal_ha)*100)
MEL_SSC_FULL$TrIndl_per <- ((MEL_SSC_FULL$TrIndl_ha/MEL_SSC_FULL$TrTotal_ha)*100)
MEL_SSC_FULL$TrEdu_per <- ((MEL_SSC_FULL$TrEdu_ha/MEL_SSC_FULL$TrTotal_ha)*100)
MEL_SSC_FULL$TrComm_per <- ((MEL_SSC_FULL$TrComm_ha/MEL_SSC_FULL$TrTotal_ha)*100)
MEL_SSC_FULL$TrHosp_per <- ((MEL_SSC_FULL$TrHosp_ha/MEL_SSC_FULL$TrTotal_ha)*100)
MEL_SSC_FULL$TrTrans_per <- ((MEL_SSC_FULL$TrTran_ha/MEL_SSC_FULL$TrTotal_ha)*100)
MEL_SSC_FULL$TrWat_per <- ((MEL_SSC_FULL$TrWat_ha/MEL_SSC_FULL$TrTotal_ha)*100)
MEL_SSC_FULL$TrPrimP_per <- ((MEL_SSC_FULL$TrPrimP_ha/MEL_SSC_FULL$TrTotal_ha)*100)

# SYD import data
#SYD_SSC_FULL <- sf::st_read("data-raw/SYD_FULL.shp")
SYD_SSC_FULL <- sf::st_read("inst/extdata/SYD_FULL.shp")
SYD_SSC_FULL$PerAnyTree <- SYD_SSC_FULL$PerAnyTree*100
SYD_SSC_FULL$PerShrub <- SYD_SSC_FULL$PerShrub*100
SYD_SSC_FULL$PerGrass <- SYD_SSC_FULL$PerGrass*100
SYD_SSC_FULL$PerNonVeg <- (100 - SYD_SSC_FULL$PerAnyTree - SYD_SSC_FULL$PerShrub - SYD_SSC_FULL$PerGrass)

SYD_SSC_FULL$TrPriv <- ((SYD_SSC_FULL$TrPriv_ha/SYD_SSC_FULL$TrTotT_ha)*100)
SYD_SSC_FULL$TrPubl <- ((SYD_SSC_FULL$TrPubl_ha/SYD_SSC_FULL$TrTotT_ha)*100)

SYD_SSC_FULL$ArRes_per <- ((SYD_SSC_FULL$ArRes_ha/SYD_SSC_FULL$ArTotal_ha)*100)
SYD_SSC_FULL$ArPark_per <- ((SYD_SSC_FULL$ArPark_ha/SYD_SSC_FULL$ArTotal_ha)*100)
SYD_SSC_FULL$ArInfr_per <- ((SYD_SSC_FULL$ArInfr_ha/SYD_SSC_FULL$ArTotal_ha)*100)
SYD_SSC_FULL$ArOth_per <- ((SYD_SSC_FULL$ArOth_ha/SYD_SSC_FULL$ArTotal_ha)*100)
SYD_SSC_FULL$ArIndl_per <- ((SYD_SSC_FULL$ArIndl_ha/SYD_SSC_FULL$ArTotal_ha)*100)
SYD_SSC_FULL$ArEdu_per <- ((SYD_SSC_FULL$ArEdu_ha/SYD_SSC_FULL$ArTotal_ha)*100)
SYD_SSC_FULL$ArComm_per <- ((SYD_SSC_FULL$ArComm_ha/SYD_SSC_FULL$ArTotal_ha)*100)
SYD_SSC_FULL$ArHosp_per <- ((SYD_SSC_FULL$ArHosp_ha/SYD_SSC_FULL$ArTotal_ha)*100)
SYD_SSC_FULL$ArTrans_per <- ((SYD_SSC_FULL$ArTrans_ha/SYD_SSC_FULL$ArTotal_ha)*100)
SYD_SSC_FULL$ArWat_per <- ((SYD_SSC_FULL$ArWat_ha/SYD_SSC_FULL$ArTotal_ha)*100)
SYD_SSC_FULL$ArPrimP_per <- ((SYD_SSC_FULL$ArPrimP_ha/SYD_SSC_FULL$ArTotal_ha)*100)

SYD_SSC_FULL$TrRes_per <- ((SYD_SSC_FULL$TrRes_ha/SYD_SSC_FULL$TrTotal_ha)*100)
SYD_SSC_FULL$TrPark_per <- ((SYD_SSC_FULL$TrPark_ha/SYD_SSC_FULL$TrTotal_ha)*100)
SYD_SSC_FULL$TrInfr_per <- ((SYD_SSC_FULL$TrInfr_ha/SYD_SSC_FULL$TrTotal_ha)*100)
SYD_SSC_FULL$TrOth_per <- ((SYD_SSC_FULL$TrOth_ha/SYD_SSC_FULL$TrTotal_ha)*100)
SYD_SSC_FULL$TrIndl_per <- ((SYD_SSC_FULL$TrIndl_ha/SYD_SSC_FULL$TrTotal_ha)*100)
SYD_SSC_FULL$TrEdu_per <- ((SYD_SSC_FULL$TrEdu_ha/SYD_SSC_FULL$TrTotal_ha)*100)
SYD_SSC_FULL$TrComm_per <- ((SYD_SSC_FULL$TrComm_ha/SYD_SSC_FULL$TrTotal_ha)*100)
SYD_SSC_FULL$TrHosp_per <- ((SYD_SSC_FULL$TrHosp_ha/SYD_SSC_FULL$TrTotal_ha)*100)
SYD_SSC_FULL$TrTrans_per <- ((SYD_SSC_FULL$TrTran_ha/SYD_SSC_FULL$TrTotal_ha)*100)
SYD_SSC_FULL$TrWat_per <- ((SYD_SSC_FULL$TrWat_ha/SYD_SSC_FULL$TrTotal_ha)*100)
SYD_SSC_FULL$TrPrimP_per <- ((SYD_SSC_FULL$TrPrimP_ha/SYD_SSC_FULL$TrTotal_ha)*100)


# Server ------------------------------------------------------------------

app_server <- function( input, output, session ) {
  
# PER SSC -----------------------------------------------------------------
  
# PER SSC map
  
  output$PER_SSC_map <- renderLeaflet({
    canopy_map(df = PER_SSC_FULL,
               structureIsSuburb = TRUE,
               structureID = PER_SSC_FULL$SSC_CODE16,
               structureName = PER_SSC_FULL$SSC_NAME16,
               structureTree = PER_SSC_FULL$PerAnyTree,
               structureShrub = PER_SSC_FULL$PerGrass,
               structureGrass = PER_SSC_FULL$PerShrub,
               minZoom = 8,
               maxZoom = 14,
               lng1 = 115.191,
               lat1 = -31.243,
               lng2= 116.743,
               lat2= -33.116,
               viewLng = 115.850,
               viewLat = -32.100,
               viewZoom = 10
    )
  })
  
# PER SSC Tree Shrub Grass Non-veg piechart 
  
  PER_vegtype_pie_data <- reactive({
    if(!is.null(PER_map_SSC$click)){
        filter_piechart(PER_SSC_FULL,
                        uniqueID = PER_SSC_FULL$SSC_CODE16,
                        clickID = PER_map_SSC$click,
                        columnsToPlot = c('PerGrass', 'PerShrub', 'PerAnyTree', 'PerNonVeg')
        )
    }
    else {
      return(NULL)
    }
  })

  output$PER_vegtype_pie <- plotly::renderPlotly({
    PER_vegtype_pie <- PER_vegtype_pie_data()
    if(!is.null(PER_vegtype_pie)){
      vegtype_pie(df = PER_vegtype_pie,
                  pieVals = PER_vegtype_pie$percent
                  )
    }
    else {
      return(NULL)
    }
  })
  
# PER SSC Priv Publ piechart
  
  PER_privpubl_pie_data <- reactive({
    if(!is.null(PER_map_SSC$click)){
      filter_piechart(PER_SSC_FULL,
                      uniqueID = PER_SSC_FULL$SSC_CODE16,
                      clickID = PER_map_SSC$click,
                      columnsToPlot = c('TrPriv', 'TrPubl')
      )
    }
    else {
      return(NULL)
    }
  })

  output$PER_privpubl_pie <- plotly::renderPlotly({
    PER_privpubl_pie <- PER_privpubl_pie_data()
    if(!is.null(PER_privpubl_pie)){
      privpubl_pie(df = PER_privpubl_pie,
                  pieVals = PER_privpubl_pie$percent
      )
    }
    else {
      return(NULL)
    }
  })

# PER SSC Land Use piechart
  
  PER_LU_pie_data <- reactive({
    if(!is.null(PER_map_SSC$click)){
      filter_piechart(PER_SSC_FULL,
                      uniqueID = PER_SSC_FULL$SSC_CODE16,
                      clickID = PER_map_SSC$click,
                      columnsToPlot = c('ArRes_per', 'ArPark_per', 'ArInfr_per', 'ArOth_per', 
                                        'ArIndl_per', 'ArEdu_per', 'ArComm_per', 'ArHosp_per',
                                        'ArTrans_per', 'ArWat_per', 'ArPrimP_per')
      )
    }
    else {
      return(NULL)
    }
  })
  
  output$PER_LU_pie <- plotly::renderPlotly({
    PER_LU_pie <- PER_LU_pie_data()
    if(!is.null(PER_LU_pie)){
     LU_pie(df = PER_LU_pie,
                   pieVals = PER_LU_pie$percent
      )
    }
    else {
      return(NULL)
    }
  })
  
# PER SSC TREE Land Use piechart

  PER_TrLU_pie_data <- reactive({
    if(!is.null(PER_map_SSC$click)){
      filter_piechart(PER_SSC_FULL,
                      uniqueID = PER_SSC_FULL$SSC_CODE16,
                      clickID = PER_map_SSC$click,
                      columnsToPlot = c('TrRes_per', 'TrPark_per', 'TrInfr_per', 'TrOth_per',
                                        'TrIndl_per', 'TrEdu_per', 'TrComm_per', 'TrHosp_per',
                                        'TrTrans_per', 'TrWat_per', 'TrPrimP_per')
      )
    }
    else {
      return(NULL)
    }
  })

  output$PER_TrLU_pie <- plotly::renderPlotly({
    PER_TrLU_pie <- PER_TrLU_pie_data()
    if(!is.null(PER_TrLU_pie)){
      TrLU_pie(df = PER_TrLU_pie,
             pieVals = PER_TrLU_pie$percent
      )
    }
    else {
      return(NULL)
    }
  })

# PER SSC filter scatter data

  PER_SSC_scatter_data <- filter_scatter(
    PER_SSC_FULL,
    columnsToPlot = c('SSC_CODE16','SSC_NAME16','PerAnyTree', 'GrDwDens', 'GrDenQuint', 'UrbDwDens', 'UrbDenQuin', 'ResDwDens', 'ResDenQuin'),
    uniqueID = PER_SSC_FULL$SSC_CODE16
    
  )


  # Renders PER SSC SCATTER GROSS
  output$PER_SSC_gross_scatter <- plotly::renderPlotly({
    scatter_plot(df = PER_SSC_scatter_data,
                 xVals = PER_SSC_scatter_data$PerAnyTree,
                 yVals = PER_SSC_scatter_data$yaxis_ran,
                 source = 'PER_SSC_plot1',
                 structureName = PER_SSC_scatter_data$SSC_NAME16,
                 plotTitle = "<b>Tree Canopy (%) & Gross Density Quintiles</b>",
                 tree = PER_SSC_scatter_data$PerAnyTree,
                 densityQuintile = PER_SSC_scatter_data$GrDenQuint,
                 density = PER_SSC_scatter_data$GrDwDens)
  })
  
  observe({
    if (!is.null(PER_map_SSC$click)){
      selected_quintile <- base::as.numeric(PER_SSC_scatter_data[PER_SSC_scatter_data[['SSC_CODE16']] == PER_map_SSC$click, ]['GrDenQuint'])
      
      PER_SSC_selected_quintile <- filter(PER_SSC_scatter_data, GrDenQuint == selected_quintile)
      
      plotly::plotlyProxy("PER_SSC_gross_scatter") %>%
        plotly::plotlyProxyInvoke("deleteTraces", list(1)) %>%
        plotly::plotlyProxyInvoke(
          "addTraces",
          list(
            x = PER_SSC_selected_quintile$PerAnyTree,
            y = PER_SSC_selected_quintile$yaxis_ran,
            type = 'line',
            mode = 'markers',
            marker = list(size = 10,
                          opacity = 0.8,
                          color = '#4292c6'),
            showlegend = FALSE,
            hoverinfo = 'text',
            text = quin_hover_text(
              structureName = PER_SSC_selected_quintile$SSC_NAME16,
              tree = PER_SSC_selected_quintile$PerAnyTree,
              densityQuintile = PER_SSC_selected_quintile$GrDenQuint,
              density = PER_SSC_selected_quintile$GrDwDens
            )
          )
        ) %>%
        plotly::plotlyProxyInvoke("relayout",
                                  list(shapes = list(
                                    scatter_quinline(
                                      df = PER_SSC_scatter_data,
                                      uniqueID = "SSC_CODE16",
                                      clickID = PER_map_SSC$click,
                                      groupID = "GrDenQuint",
                                      xVals = "PerAnyTree"
                                    ),
                                    scatter_mean(df = PER_SSC_scatter_data,
                                                 xVals = "PerAnyTree")
                                  ),
                                  annotations = list(scatter_annotation(
                                    df = PER_SSC_selected_quintile,
                                    uniqueID = "SSC_CODE16",
                                    clickID = PER_map_SSC$click,
                                    xVals = "PerAnyTree",
                                    yVals = "yaxis_ran",
                                    structureName = "SSC_NAME16"
                                  ),
                                  scatter_quinline_label(
                                    df = PER_SSC_selected_quintile,
                                    uniqueID = "SSC_CODE16",
                                    clickID = PER_map_SSC$click,
                                    groupID = "GrDenQuint",
                                    xVals = "PerAnyTree"
                                  ),
                                  scatter_mean_label(df = PER_SSC_scatter_data, xVals = "PerAnyTree"))))
    } 
    else{
      return(NULL)
    }
  })
  
  # Renders PER SSC SCATTER URBAN
  output$PER_SSC_urban_scatter <- plotly::renderPlotly({
    scatter_plot(df = PER_SSC_scatter_data,
                 xVals = PER_SSC_scatter_data$PerAnyTree,
                 yVals = PER_SSC_scatter_data$yaxis_ran,
                 source = 'PER_SSC_plot1',
                 structureName = PER_SSC_scatter_data$SSC_NAME16,
                 plotTitle = "<b>Tree Canopy (%) & Urban Density Quintiles</b>",
                 tree = PER_SSC_scatter_data$PerAnyTree,
                 densityQuintile = PER_SSC_scatter_data$UrbDenQuin,
                 density = PER_SSC_scatter_data$UrbDwDens)
  })
  
  observe({
    #if (!is.null(PER_map_SSC$click)){
      selected_quintile <- base::as.numeric(PER_SSC_scatter_data[PER_SSC_scatter_data[['SSC_CODE16']] == PER_map_SSC$click, ]['UrbDenQuin'])
      
      PER_SSC_selected_quintile <- filter(PER_SSC_scatter_data, UrbDenQuin == selected_quintile)
      
      plotly::plotlyProxy("PER_SSC_urban_scatter") %>%
        plotly::plotlyProxyInvoke("deleteTraces", list(1)) %>%
        plotly::plotlyProxyInvoke(
          "addTraces",
          list(
            x = PER_SSC_selected_quintile$PerAnyTree,
            y = PER_SSC_selected_quintile$yaxis_ran,
            type = 'line',
            mode = 'markers',
            marker = list(size = 10,
                          opacity = 0.8,
                          color = '#4292c6'),
            showlegend = FALSE,
            hoverinfo = 'text',
            text = quin_hover_text(
              structureName = PER_SSC_selected_quintile$SSC_NAME16,
              tree = PER_SSC_selected_quintile$PerAnyTree,
              densityQuintile = PER_SSC_selected_quintile$UrbDenQuin,
              density = PER_SSC_selected_quintile$UrbDwDens
            )
          )
        ) %>%
        plotly::plotlyProxyInvoke("relayout",
                                  list(shapes = list(
                                    scatter_quinline(
                                      df = PER_SSC_scatter_data,
                                      uniqueID = "SSC_CODE16",
                                      clickID = PER_map_SSC$click,
                                      groupID = "UrbDenQuin",
                                      xVals = "PerAnyTree"
                                    ),
                                    scatter_mean(df = PER_SSC_scatter_data,
                                                 xVals = "PerAnyTree")
                                  ),
                                  annotations = list(scatter_annotation(
                                    df = PER_SSC_selected_quintile,
                                    uniqueID = "SSC_CODE16",
                                    clickID = PER_map_SSC$click,
                                    xVals = "PerAnyTree",
                                    yVals = "yaxis_ran",
                                    structureName = "SSC_NAME16"
                                  ),
                                  scatter_quinline_label(
                                    df = PER_SSC_selected_quintile,
                                    uniqueID = "SSC_CODE16",
                                    clickID = PER_map_SSC$click,
                                    groupID = "UrbDenQuin",
                                    xVals = "PerAnyTree"
                                  ),
                                  scatter_mean_label(df = PER_SSC_scatter_data, xVals = "PerAnyTree"))))
   # } 
   # else{
   #   return(NULL)
   # }
  })
  
  # Renders PER SSC SCATTER RES
  output$PER_SSC_res_scatter <- plotly::renderPlotly({
    scatter_plot(df = PER_SSC_scatter_data,
                 xVals = PER_SSC_scatter_data$PerAnyTree,
                 yVals = PER_SSC_scatter_data$yaxis_ran,
                 source = 'PER_SSC_plot1',
                 structureName = PER_SSC_scatter_data$SSC_NAME16,
                 plotTitle = "<b>Tree Canopy (%) & Residential Density Quintiles</b>",
                 tree = PER_SSC_scatter_data$PerAnyTree,
                 densityQuintile = PER_SSC_scatter_data$ResDenQuin,
                 density = PER_SSC_scatter_data$ResDwDens)
  })
  
  observe({
    if (!is.null(PER_map_SSC$click)){
      selected_quintile <- base::as.numeric(PER_SSC_scatter_data[PER_SSC_scatter_data[['SSC_CODE16']] == PER_map_SSC$click, ]['ResDenQuin'])
      
      PER_SSC_selected_quintile <- filter(PER_SSC_scatter_data, ResDenQuin == selected_quintile)
      
      plotly::plotlyProxy("PER_SSC_res_scatter") %>%
        plotly::plotlyProxyInvoke("deleteTraces", list(1)) %>%
        plotly::plotlyProxyInvoke(
          "addTraces",
          list(
            x = PER_SSC_selected_quintile$PerAnyTree,
            y = PER_SSC_selected_quintile$yaxis_ran,
            type = 'line',
            mode = 'markers',
            marker = list(size = 10,
                          opacity = 0.8,
                          color = '#4292c6'),
            showlegend = FALSE,
            hoverinfo = 'text',
            text = quin_hover_text(
              structureName = PER_SSC_selected_quintile$SSC_NAME16,
              tree = PER_SSC_selected_quintile$PerAnyTree,
              densityQuintile = PER_SSC_selected_quintile$ResDenQuin,
              density = PER_SSC_selected_quintile$ResDwDens
            )
          )
        ) %>%
        plotly::plotlyProxyInvoke("relayout",
                                  list(shapes = list(
                                    scatter_quinline(
                                      df = PER_SSC_scatter_data,
                                      uniqueID = "SSC_CODE16",
                                      clickID = PER_map_SSC$click,
                                      groupID = "ResDenQuin",
                                      xVals = "PerAnyTree"
                                    ),
                                    scatter_mean(df = PER_SSC_scatter_data,
                                                 xVals = "PerAnyTree")
                                  ),
                                  annotations = list(scatter_annotation(
                                    df = PER_SSC_selected_quintile,
                                    uniqueID = "SSC_CODE16",
                                    clickID = PER_map_SSC$click,
                                    xVals = "PerAnyTree",
                                    yVals = "yaxis_ran",
                                    structureName = "SSC_NAME16"
                                  ),
                                  scatter_quinline_label(
                                    df = PER_SSC_selected_quintile,
                                    uniqueID = "SSC_CODE16",
                                    clickID = PER_map_SSC$click,
                                    groupID = "ResDenQuin",
                                    xVals = "PerAnyTree"
                                  ),
                                  scatter_mean_label(df = PER_SSC_scatter_data, xVals = "PerAnyTree"))))
    } 
    else{
      return(NULL)
    }
  })  
  
# MEL SSC -----------------------------------------------------------------
  
# MEL SSC map
  
  output$MEL_SSC_map <- renderLeaflet({
    canopy_map(df = MEL_SSC_FULL,
               structureIsSuburb = TRUE,
               structureID = MEL_SSC_FULL$SSC_CODE16,
               structureName = MEL_SSC_FULL$SSC_NAME16,
               structureTree = MEL_SSC_FULL$PerAnyTree,
               structureShrub = MEL_SSC_FULL$PerGrass,
               structureGrass = MEL_SSC_FULL$PerShrub,
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
  })  
  
  # MEL SSC Tree Shrub Grass Non-veg piechart 
  
  MEL_vegtype_pie_data <- reactive({
    if(!is.null(MEL_map_SSC$click)){
      filter_piechart(MEL_SSC_FULL,
                      uniqueID = MEL_SSC_FULL$SSC_CODE16,
                      clickID = MEL_map_SSC$click,
                      columnsToPlot = c('PerGrass', 'PerShrub', 'PerAnyTree', 'PerNonVeg')
      )
    }
    else {
      return(NULL)
    }
  })
  
  output$MEL_vegtype_pie <- plotly::renderPlotly({
    MEL_vegtype_pie <- MEL_vegtype_pie_data()
    if(!is.null(MEL_vegtype_pie)){
      vegtype_pie(df = MEL_vegtype_pie,
                  pieVals = MEL_vegtype_pie$percent
      )
    }
    else {
      return(NULL)
    }
  })
  
  # MEL SSC Priv Publ piechart
  
  MEL_privpubl_pie_data <- reactive({
    if(!is.null(MEL_map_SSC$click)){
      filter_piechart(MEL_SSC_FULL,
                      uniqueID = MEL_SSC_FULL$SSC_CODE16,
                      clickID = MEL_map_SSC$click,
                      columnsToPlot = c('TrPriv', 'TrPubl')
      )
    }
    else {
      return(NULL)
    }
  })
  
  output$MEL_privpubl_pie <- plotly::renderPlotly({
    MEL_privpubl_pie <- MEL_privpubl_pie_data()
    if(!is.null(MEL_privpubl_pie)){
      privpubl_pie(df = MEL_privpubl_pie,
                   pieVals = MEL_privpubl_pie$percent
      )
    }
    else {
      return(NULL)
    }
  })
  
  # MEL SSC Land Use piechart
  
  MEL_LU_pie_data <- reactive({
    if(!is.null(MEL_map_SSC$click)){
      filter_piechart(MEL_SSC_FULL,
                      uniqueID = MEL_SSC_FULL$SSC_CODE16,
                      clickID = MEL_map_SSC$click,
                      columnsToPlot = c('ArRes_per', 'ArPark_per', 'ArInfr_per', 'ArOth_per', 
                                        'ArIndl_per', 'ArEdu_per', 'ArComm_per', 'ArHosp_per',
                                        'ArTrans_per', 'ArWat_per', 'ArPrimP_per')
      )
    }
    else {
      return(NULL)
    }
  })
  
  output$MEL_LU_pie <- plotly::renderPlotly({
    MEL_LU_pie <- MEL_LU_pie_data()
    if(!is.null(MEL_LU_pie)){
      LU_pie(df = MEL_LU_pie,
             pieVals = MEL_LU_pie$percent
      )
    }
    else {
      return(NULL)
    }
  })
  
  # MEL SSC TREE Land Use piechart
  
  MEL_TrLU_pie_data <- reactive({
    if(!is.null(MEL_map_SSC$click)){
      filter_piechart(MEL_SSC_FULL,
                      uniqueID = MEL_SSC_FULL$SSC_CODE16,
                      clickID = MEL_map_SSC$click,
                      columnsToPlot = c('TrRes_per', 'TrPark_per', 'TrInfr_per', 'TrOth_per',
                                        'TrIndl_per', 'TrEdu_per', 'TrComm_per', 'TrHosp_per',
                                        'TrTrans_per', 'TrWat_per', 'TrPrimP_per')
      )
    }
    else {
      return(NULL)
    }
  })
  
  output$MEL_TrLU_pie <- plotly::renderPlotly({
    MEL_TrLU_pie <- MEL_TrLU_pie_data()
    if(!is.null(MEL_TrLU_pie)){
      TrLU_pie(df = MEL_TrLU_pie,
               pieVals = MEL_TrLU_pie$percent
      )
    }
    else {
      return(NULL)
    }
  })
  
  # MEL SSC filter scatter data
  
  MEL_SSC_scatter_data <- filter_scatter(
    MEL_SSC_FULL,
    columnsToPlot = c('SSC_CODE16','SSC_NAME16','PerAnyTree', 'GrDwDens', 'GrDenQuint', 'UrbDwDens', 'UrbDenQuin', 'ResDwDens', 'ResDenQuin'),
    uniqueID = MEL_SSC_FULL$SSC_CODE16
    
  )
  
  
  # Renders MEL SSC SCATTER GROSS
  output$MEL_SSC_gross_scatter <- plotly::renderPlotly({
    scatter_plot(df = MEL_SSC_scatter_data,
                 xVals = MEL_SSC_scatter_data$PerAnyTree,
                 yVals = MEL_SSC_scatter_data$yaxis_ran,
                 source = 'MEL_SSC_plot1',
                 structureName = MEL_SSC_scatter_data$SSC_NAME16,
                 plotTitle = "<b>Tree Canopy (%) & Gross Density Quintiles</b>",
                 tree = MEL_SSC_scatter_data$PerAnyTree,
                 densityQuintile = MEL_SSC_scatter_data$GrDenQuint,
                 density = MEL_SSC_scatter_data$GrDwDens)
  })
  
  observe({
    if (!is.null(MEL_map_SSC$click)){
      selected_quintile <- base::as.numeric(MEL_SSC_scatter_data[MEL_SSC_scatter_data[['SSC_CODE16']] == MEL_map_SSC$click, ]['GrDenQuint'])
      
      MEL_SSC_selected_quintile <- filter(MEL_SSC_scatter_data, GrDenQuint == selected_quintile)
      
      plotly::plotlyProxy("MEL_SSC_gross_scatter") %>%
        plotly::plotlyProxyInvoke("deleteTraces", list(1)) %>%
        plotly::plotlyProxyInvoke(
          "addTraces",
          list(
            x = MEL_SSC_selected_quintile$PerAnyTree,
            y = MEL_SSC_selected_quintile$yaxis_ran,
            type = 'line',
            mode = 'markers',
            marker = list(size = 10,
                          opacity = 0.8,
                          color = '#4292c6'),
            showlegend = FALSE,
            hoverinfo = 'text',
            text = quin_hover_text(
              structureName = MEL_SSC_selected_quintile$SSC_NAME16,
              tree = MEL_SSC_selected_quintile$PerAnyTree,
              densityQuintile = MEL_SSC_selected_quintile$GrDenQuint,
              density = MEL_SSC_selected_quintile$GrDwDens
            )
          )
        ) %>%
        plotly::plotlyProxyInvoke("relayout",
                                  list(shapes = list(
                                    scatter_quinline(
                                      df = MEL_SSC_scatter_data,
                                      uniqueID = "SSC_CODE16",
                                      clickID = MEL_map_SSC$click,
                                      groupID = "GrDenQuint",
                                      xVals = "PerAnyTree"
                                    ),
                                    scatter_mean(df = MEL_SSC_scatter_data,
                                                 xVals = "PerAnyTree")
                                  ),
                                  annotations = list(scatter_annotation(
                                    df = MEL_SSC_selected_quintile,
                                    uniqueID = "SSC_CODE16",
                                    clickID = MEL_map_SSC$click,
                                    xVals = "PerAnyTree",
                                    yVals = "yaxis_ran",
                                    structureName = "SSC_NAME16"
                                  ),
                                  scatter_quinline_label(
                                    df = MEL_SSC_selected_quintile,
                                    uniqueID = "SSC_CODE16",
                                    clickID = MEL_map_SSC$click,
                                    groupID = "GrDenQuint",
                                    xVals = "PerAnyTree"
                                  ),
                                  scatter_mean_label(df = MEL_SSC_scatter_data, xVals = "PerAnyTree"))))
    } 
    else{
      return(NULL)
    }
  })
  
  # Renders MEL SSC SCATTER URBAN
  output$MEL_SSC_urban_scatter <- plotly::renderPlotly({
    scatter_plot(df = MEL_SSC_scatter_data,
                 xVals = MEL_SSC_scatter_data$PerAnyTree,
                 yVals = MEL_SSC_scatter_data$yaxis_ran,
                 source = 'MEL_SSC_plot1',
                 structureName = MEL_SSC_scatter_data$SSC_NAME16,
                 plotTitle = "<b>Tree Canopy (%) & Urban Density Quintiles</b>",
                 tree = MEL_SSC_scatter_data$PerAnyTree,
                 densityQuintile = MEL_SSC_scatter_data$UrbDenQuin,
                 density = MEL_SSC_scatter_data$UrbDwDens)
  })
  
  observe({
    #if (!is.null(MEL_map_SSC$click)){
    selected_quintile <- base::as.numeric(MEL_SSC_scatter_data[MEL_SSC_scatter_data[['SSC_CODE16']] == MEL_map_SSC$click, ]['UrbDenQuin'])
    
    MEL_SSC_selected_quintile <- filter(MEL_SSC_scatter_data, UrbDenQuin == selected_quintile)
    
    plotly::plotlyProxy("MEL_SSC_urban_scatter") %>%
      plotly::plotlyProxyInvoke("deleteTraces", list(1)) %>%
      plotly::plotlyProxyInvoke(
        "addTraces",
        list(
          x = MEL_SSC_selected_quintile$PerAnyTree,
          y = MEL_SSC_selected_quintile$yaxis_ran,
          type = 'line',
          mode = 'markers',
          marker = list(size = 10,
                        opacity = 0.8,
                        color = '#4292c6'),
          showlegend = FALSE,
          hoverinfo = 'text',
          text = quin_hover_text(
            structureName = MEL_SSC_selected_quintile$SSC_NAME16,
            tree = MEL_SSC_selected_quintile$PerAnyTree,
            densityQuintile = MEL_SSC_selected_quintile$UrbDenQuin,
            density = MEL_SSC_selected_quintile$UrbDwDens
          )
        )
      ) %>%
      plotly::plotlyProxyInvoke("relayout",
                                list(shapes = list(
                                  scatter_quinline(
                                    df = MEL_SSC_scatter_data,
                                    uniqueID = "SSC_CODE16",
                                    clickID = MEL_map_SSC$click,
                                    groupID = "UrbDenQuin",
                                    xVals = "PerAnyTree"
                                  ),
                                  scatter_mean(df = MEL_SSC_scatter_data,
                                               xVals = "PerAnyTree")
                                ),
                                annotations = list(scatter_annotation(
                                  df = MEL_SSC_selected_quintile,
                                  uniqueID = "SSC_CODE16",
                                  clickID = MEL_map_SSC$click,
                                  xVals = "PerAnyTree",
                                  yVals = "yaxis_ran",
                                  structureName = "SSC_NAME16"
                                ),
                                scatter_quinline_label(
                                  df = MEL_SSC_selected_quintile,
                                  uniqueID = "SSC_CODE16",
                                  clickID = MEL_map_SSC$click,
                                  groupID = "UrbDenQuin",
                                  xVals = "PerAnyTree"
                                ),
                                scatter_mean_label(df = MEL_SSC_scatter_data, xVals = "PerAnyTree"))))
    # } 
    # else{
    #   return(NULL)
    # }
  })
  
  # Renders MEL SSC SCATTER RES
  output$MEL_SSC_res_scatter <- plotly::renderPlotly({
    scatter_plot(df = MEL_SSC_scatter_data,
                 xVals = MEL_SSC_scatter_data$PerAnyTree,
                 yVals = MEL_SSC_scatter_data$yaxis_ran,
                 source = 'MEL_SSC_plot1',
                 structureName = MEL_SSC_scatter_data$SSC_NAME16,
                 plotTitle = "<b>Tree Canopy (%) & Residential Density Quintiles</b>",
                 tree = MEL_SSC_scatter_data$PerAnyTree,
                 densityQuintile = MEL_SSC_scatter_data$ResDenQuin,
                 density = MEL_SSC_scatter_data$ResDwDens)
  })
  
  observe({
    if (!is.null(MEL_map_SSC$click)){
      selected_quintile <- base::as.numeric(MEL_SSC_scatter_data[MEL_SSC_scatter_data[['SSC_CODE16']] == MEL_map_SSC$click, ]['ResDenQuin'])
      
      MEL_SSC_selected_quintile <- filter(MEL_SSC_scatter_data, ResDenQuin == selected_quintile)
      
      plotly::plotlyProxy("MEL_SSC_res_scatter") %>%
        plotly::plotlyProxyInvoke("deleteTraces", list(1)) %>%
        plotly::plotlyProxyInvoke(
          "addTraces",
          list(
            x = MEL_SSC_selected_quintile$PerAnyTree,
            y = MEL_SSC_selected_quintile$yaxis_ran,
            type = 'line',
            mode = 'markers',
            marker = list(size = 10,
                          opacity = 0.8,
                          color = '#4292c6'),
            showlegend = FALSE,
            hoverinfo = 'text',
            text = quin_hover_text(
              structureName = MEL_SSC_selected_quintile$SSC_NAME16,
              tree = MEL_SSC_selected_quintile$PerAnyTree,
              densityQuintile = MEL_SSC_selected_quintile$ResDenQuin,
              density = MEL_SSC_selected_quintile$ResDwDens
            )
          )
        ) %>%
        plotly::plotlyProxyInvoke("relayout",
                                  list(shapes = list(
                                    scatter_quinline(
                                      df = MEL_SSC_scatter_data,
                                      uniqueID = "SSC_CODE16",
                                      clickID = MEL_map_SSC$click,
                                      groupID = "ResDenQuin",
                                      xVals = "PerAnyTree"
                                    ),
                                    scatter_mean(df = MEL_SSC_scatter_data,
                                                 xVals = "PerAnyTree")
                                  ),
                                  annotations = list(scatter_annotation(
                                    df = MEL_SSC_selected_quintile,
                                    uniqueID = "SSC_CODE16",
                                    clickID = MEL_map_SSC$click,
                                    xVals = "PerAnyTree",
                                    yVals = "yaxis_ran",
                                    structureName = "SSC_NAME16"
                                  ),
                                  scatter_quinline_label(
                                    df = MEL_SSC_selected_quintile,
                                    uniqueID = "SSC_CODE16",
                                    clickID = MEL_map_SSC$click,
                                    groupID = "ResDenQuin",
                                    xVals = "PerAnyTree"
                                  ),
                                  scatter_mean_label(df = MEL_SSC_scatter_data, xVals = "PerAnyTree"))))
    } 
    else{
      return(NULL)
    }
  })  
  
# SYD SSC -----------------------------------------------------------------
  
# SYD SSC map
  
  output$SYD_SSC_map <- renderLeaflet({
    canopy_map(df = SYD_SSC_FULL,
               structureIsSuburb = TRUE,
               structureID = SYD_SSC_FULL$SSC_CODE16,
               structureName = SYD_SSC_FULL$SSC_NAME16,
               structureTree = SYD_SSC_FULL$PerAnyTree,
               structureShrub = SYD_SSC_FULL$PerGrass,
               structureGrass = SYD_SSC_FULL$PerShrub,
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
  })
  
  # SYD SSC Tree Shrub Grass Non-veg piechart 
  
  SYD_vegtype_pie_data <- reactive({
    if(!is.null(SYD_map_SSC$click)){
      filter_piechart(SYD_SSC_FULL,
                      uniqueID = SYD_SSC_FULL$SSC_CODE16,
                      clickID = SYD_map_SSC$click,
                      columnsToPlot = c('PerGrass', 'PerShrub', 'PerAnyTree', 'PerNonVeg')
      )
    }
    else {
      return(NULL)
    }
  })
  
  output$SYD_vegtype_pie <- plotly::renderPlotly({
    SYD_vegtype_pie <- SYD_vegtype_pie_data()
    if(!is.null(SYD_vegtype_pie)){
      vegtype_pie(df = SYD_vegtype_pie,
                  pieVals = SYD_vegtype_pie$percent
      )
    }
    else {
      return(NULL)
    }
  })
  
  # SYD SSC Priv Publ piechart
  
  SYD_privpubl_pie_data <- reactive({
    if(!is.null(SYD_map_SSC$click)){
      filter_piechart(SYD_SSC_FULL,
                      uniqueID = SYD_SSC_FULL$SSC_CODE16,
                      clickID = SYD_map_SSC$click,
                      columnsToPlot = c('TrPriv', 'TrPubl')
      )
    }
    else {
      return(NULL)
    }
  })
  
  output$SYD_privpubl_pie <- plotly::renderPlotly({
    SYD_privpubl_pie <- SYD_privpubl_pie_data()
    if(!is.null(SYD_privpubl_pie)){
      privpubl_pie(df = SYD_privpubl_pie,
                   pieVals = SYD_privpubl_pie$percent
      )
    }
    else {
      return(NULL)
    }
  })
  
  # SYD SSC Land Use piechart
  
  SYD_LU_pie_data <- reactive({
    if(!is.null(SYD_map_SSC$click)){
      filter_piechart(SYD_SSC_FULL,
                      uniqueID = SYD_SSC_FULL$SSC_CODE16,
                      clickID = SYD_map_SSC$click,
                      columnsToPlot = c('ArRes_per', 'ArPark_per', 'ArInfr_per', 'ArOth_per', 
                                        'ArIndl_per', 'ArEdu_per', 'ArComm_per', 'ArHosp_per',
                                        'ArTrans_per', 'ArWat_per', 'ArPrimP_per')
      )
    }
    else {
      return(NULL)
    }
  })
  
  output$SYD_LU_pie <- plotly::renderPlotly({
    SYD_LU_pie <- SYD_LU_pie_data()
    if(!is.null(SYD_LU_pie)){
      LU_pie(df = SYD_LU_pie,
             pieVals = SYD_LU_pie$percent
      )
    }
    else {
      return(NULL)
    }
  })
  
  # SYD SSC TREE Land Use piechart
  
  SYD_TrLU_pie_data <- reactive({
    if(!is.null(SYD_map_SSC$click)){
      filter_piechart(SYD_SSC_FULL,
                      uniqueID = SYD_SSC_FULL$SSC_CODE16,
                      clickID = SYD_map_SSC$click,
                      columnsToPlot = c('TrRes_per', 'TrPark_per', 'TrInfr_per', 'TrOth_per',
                                        'TrIndl_per', 'TrEdu_per', 'TrComm_per', 'TrHosp_per',
                                        'TrTrans_per', 'TrWat_per', 'TrPrimP_per')
      )
    }
    else {
      return(NULL)
    }
  })
  
  output$SYD_TrLU_pie <- plotly::renderPlotly({
    SYD_TrLU_pie <- SYD_TrLU_pie_data()
    if(!is.null(SYD_TrLU_pie)){
      TrLU_pie(df = SYD_TrLU_pie,
               pieVals = SYD_TrLU_pie$percent
      )
    }
    else {
      return(NULL)
    }
  })
  
  # SYD SSC filter scatter data
  
  SYD_SSC_scatter_data <- filter_scatter(
    SYD_SSC_FULL,
    columnsToPlot = c('SSC_CODE16','SSC_NAME16','PerAnyTree', 'GrDwDens', 'GrDenQuint', 'UrbDwDens', 'UrbDenQuin', 'ResDwDens', 'ResDenQuin'),
    uniqueID = SYD_SSC_FULL$SSC_CODE16
    
  )
  
  
  # Renders SYD SSC SCATTER GROSS
  output$SYD_SSC_gross_scatter <- plotly::renderPlotly({
    scatter_plot(df = SYD_SSC_scatter_data,
                 xVals = SYD_SSC_scatter_data$PerAnyTree,
                 yVals = SYD_SSC_scatter_data$yaxis_ran,
                 source = 'SYD_SSC_plot1',
                 structureName = SYD_SSC_scatter_data$SSC_NAME16,
                 plotTitle = "<b>Tree Canopy (%) & Gross Density Quintiles</b>",
                 tree = SYD_SSC_scatter_data$PerAnyTree,
                 densityQuintile = SYD_SSC_scatter_data$GrDenQuint,
                 density = SYD_SSC_scatter_data$GrDwDens)
  })
  
  observe({
    if (!is.null(SYD_map_SSC$click)){
      selected_quintile <- base::as.numeric(SYD_SSC_scatter_data[SYD_SSC_scatter_data[['SSC_CODE16']] == SYD_map_SSC$click, ]['GrDenQuint'])
      
      SYD_SSC_selected_quintile <- filter(SYD_SSC_scatter_data, GrDenQuint == selected_quintile)
      
      plotly::plotlyProxy("SYD_SSC_gross_scatter") %>%
        plotly::plotlyProxyInvoke("deleteTraces", list(1)) %>%
        plotly::plotlyProxyInvoke(
          "addTraces",
          list(
            x = SYD_SSC_selected_quintile$PerAnyTree,
            y = SYD_SSC_selected_quintile$yaxis_ran,
            type = 'line',
            mode = 'markers',
            marker = list(size = 10,
                          opacity = 0.8,
                          color = '#4292c6'),
            showlegend = FALSE,
            hoverinfo = 'text',
            text = quin_hover_text(
              structureName = SYD_SSC_selected_quintile$SSC_NAME16,
              tree = SYD_SSC_selected_quintile$PerAnyTree,
              densityQuintile = SYD_SSC_selected_quintile$GrDenQuint,
              density = SYD_SSC_selected_quintile$GrDwDens
            )
          )
        ) %>%
        plotly::plotlyProxyInvoke("relayout",
                                  list(shapes = list(
                                    scatter_quinline(
                                      df = SYD_SSC_scatter_data,
                                      uniqueID = "SSC_CODE16",
                                      clickID = SYD_map_SSC$click,
                                      groupID = "GrDenQuint",
                                      xVals = "PerAnyTree"
                                    ),
                                    scatter_mean(df = SYD_SSC_scatter_data,
                                                 xVals = "PerAnyTree")
                                  ),
                                  annotations = list(scatter_annotation(
                                    df = SYD_SSC_selected_quintile,
                                    uniqueID = "SSC_CODE16",
                                    clickID = SYD_map_SSC$click,
                                    xVals = "PerAnyTree",
                                    yVals = "yaxis_ran",
                                    structureName = "SSC_NAME16"
                                  ),
                                  scatter_quinline_label(
                                    df = SYD_SSC_selected_quintile,
                                    uniqueID = "SSC_CODE16",
                                    clickID = SYD_map_SSC$click,
                                    groupID = "GrDenQuint",
                                    xVals = "PerAnyTree"
                                  ),
                                  scatter_mean_label(df = SYD_SSC_scatter_data, xVals = "PerAnyTree"))))
    } 
    else{
      return(NULL)
    }
  })
  
  # Renders SYD SSC SCATTER URBAN
  output$SYD_SSC_urban_scatter <- plotly::renderPlotly({
    scatter_plot(df = SYD_SSC_scatter_data,
                 xVals = SYD_SSC_scatter_data$PerAnyTree,
                 yVals = SYD_SSC_scatter_data$yaxis_ran,
                 source = 'SYD_SSC_plot1',
                 structureName = SYD_SSC_scatter_data$SSC_NAME16,
                 plotTitle = "<b>Tree Canopy (%) & Urban Density Quintiles</b>",
                 tree = SYD_SSC_scatter_data$PerAnyTree,
                 densityQuintile = SYD_SSC_scatter_data$UrbDenQuin,
                 density = SYD_SSC_scatter_data$UrbDwDens)
  })
  
  observe({
    #if (!is.null(SYD_map_SSC$click)){
    selected_quintile <- base::as.numeric(SYD_SSC_scatter_data[SYD_SSC_scatter_data[['SSC_CODE16']] == SYD_map_SSC$click, ]['UrbDenQuin'])
    
    SYD_SSC_selected_quintile <- filter(SYD_SSC_scatter_data, UrbDenQuin == selected_quintile)
    
    plotly::plotlyProxy("SYD_SSC_urban_scatter") %>%
      plotly::plotlyProxyInvoke("deleteTraces", list(1)) %>%
      plotly::plotlyProxyInvoke(
        "addTraces",
        list(
          x = SYD_SSC_selected_quintile$PerAnyTree,
          y = SYD_SSC_selected_quintile$yaxis_ran,
          type = 'line',
          mode = 'markers',
          marker = list(size = 10,
                        opacity = 0.8,
                        color = '#4292c6'),
          showlegend = FALSE,
          hoverinfo = 'text',
          text = quin_hover_text(
            structureName = SYD_SSC_selected_quintile$SSC_NAME16,
            tree = SYD_SSC_selected_quintile$PerAnyTree,
            densityQuintile = SYD_SSC_selected_quintile$UrbDenQuin,
            density = SYD_SSC_selected_quintile$UrbDwDens
          )
        )
      ) %>%
      plotly::plotlyProxyInvoke("relayout",
                                list(shapes = list(
                                  scatter_quinline(
                                    df = SYD_SSC_scatter_data,
                                    uniqueID = "SSC_CODE16",
                                    clickID = SYD_map_SSC$click,
                                    groupID = "UrbDenQuin",
                                    xVals = "PerAnyTree"
                                  ),
                                  scatter_mean(df = SYD_SSC_scatter_data,
                                               xVals = "PerAnyTree")
                                ),
                                annotations = list(scatter_annotation(
                                  df = SYD_SSC_selected_quintile,
                                  uniqueID = "SSC_CODE16",
                                  clickID = SYD_map_SSC$click,
                                  xVals = "PerAnyTree",
                                  yVals = "yaxis_ran",
                                  structureName = "SSC_NAME16"
                                ),
                                scatter_quinline_label(
                                  df = SYD_SSC_selected_quintile,
                                  uniqueID = "SSC_CODE16",
                                  clickID = SYD_map_SSC$click,
                                  groupID = "UrbDenQuin",
                                  xVals = "PerAnyTree"
                                ),
                                scatter_mean_label(df = SYD_SSC_scatter_data, xVals = "PerAnyTree"))))
    # } 
    # else{
    #   return(NULL)
    # }
  })
  
  # Renders SYD SSC SCATTER RES
  output$SYD_SSC_res_scatter <- plotly::renderPlotly({
    scatter_plot(df = SYD_SSC_scatter_data,
                 xVals = SYD_SSC_scatter_data$PerAnyTree,
                 yVals = SYD_SSC_scatter_data$yaxis_ran,
                 source = 'SYD_SSC_plot1',
                 structureName = SYD_SSC_scatter_data$SSC_NAME16,
                 plotTitle = "<b>Tree Canopy (%) & Residential Density Quintiles</b>",
                 tree = SYD_SSC_scatter_data$PerAnyTree,
                 densityQuintile = SYD_SSC_scatter_data$ResDenQuin,
                 density = SYD_SSC_scatter_data$ResDwDens)
  })
  
  observe({
    if (!is.null(SYD_map_SSC$click)){
      selected_quintile <- base::as.numeric(SYD_SSC_scatter_data[SYD_SSC_scatter_data[['SSC_CODE16']] == SYD_map_SSC$click, ]['ResDenQuin'])
      
      SYD_SSC_selected_quintile <- filter(SYD_SSC_scatter_data, ResDenQuin == selected_quintile)
      
      plotly::plotlyProxy("SYD_SSC_res_scatter") %>%
        plotly::plotlyProxyInvoke("deleteTraces", list(1)) %>%
        plotly::plotlyProxyInvoke(
          "addTraces",
          list(
            x = SYD_SSC_selected_quintile$PerAnyTree,
            y = SYD_SSC_selected_quintile$yaxis_ran,
            type = 'line',
            mode = 'markers',
            marker = list(size = 10,
                          opacity = 0.8,
                          color = '#4292c6'),
            showlegend = FALSE,
            hoverinfo = 'text',
            text = quin_hover_text(
              structureName = SYD_SSC_selected_quintile$SSC_NAME16,
              tree = SYD_SSC_selected_quintile$PerAnyTree,
              densityQuintile = SYD_SSC_selected_quintile$ResDenQuin,
              density = SYD_SSC_selected_quintile$ResDwDens
            )
          )
        ) %>%
        plotly::plotlyProxyInvoke("relayout",
                                  list(shapes = list(
                                    scatter_quinline(
                                      df = SYD_SSC_scatter_data,
                                      uniqueID = "SSC_CODE16",
                                      clickID = SYD_map_SSC$click,
                                      groupID = "ResDenQuin",
                                      xVals = "PerAnyTree"
                                    ),
                                    scatter_mean(df = SYD_SSC_scatter_data,
                                                 xVals = "PerAnyTree")
                                  ),
                                  annotations = list(scatter_annotation(
                                    df = SYD_SSC_selected_quintile,
                                    uniqueID = "SSC_CODE16",
                                    clickID = SYD_map_SSC$click,
                                    xVals = "PerAnyTree",
                                    yVals = "yaxis_ran",
                                    structureName = "SSC_NAME16"
                                  ),
                                  scatter_quinline_label(
                                    df = SYD_SSC_selected_quintile,
                                    uniqueID = "SSC_CODE16",
                                    clickID = SYD_map_SSC$click,
                                    groupID = "ResDenQuin",
                                    xVals = "PerAnyTree"
                                  ),
                                  scatter_mean_label(df = SYD_SSC_scatter_data, xVals = "PerAnyTree"))))
    } 
    else{
      return(NULL)
    }
  })  
  

# Map Click ---------------------------------------------------------------


  # PER
  
  PER_map_SSC <- reactiveValues(
    click = vector(mode = 'numeric')
  )
  PER_map_SSC$click <- 51218
  
  observeEvent(input$PER_SSC_map_shape_click,{
    click <- isolate(input$PER_SSC_map_shape_click)
    isolate({PER_map_SSC$click = click$id})
    print(PER_map_SSC$click)
  })
  
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



