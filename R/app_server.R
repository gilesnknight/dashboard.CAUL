#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @importFrom magrittr "%>%"
#' @import dplyr 
#' @noRd
#' 

# PER import data
PER_SSC_FULL <- sf::st_read("data-raw/PER_FULL.shp")
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
MEL_SSC_FULL <- sf::st_read("data-raw/MEL_FULL.shp")
MEL_SSC_FULL$PerAnyTree <- MEL_SSC_FULL$PerAnyTree*100
MEL_SSC_FULL$PerShrub <- MEL_SSC_FULL$PerShrub*100
MEL_SSC_FULL$PerGrass <- MEL_SSC_FULL$PerGrass*100
MEL_SSC_FULL$PerNonVeg <- (100 - MEL_SSC_FULL$PerAnyTree - MEL_SSC_FULL$PerShrub - MEL_SSC_FULL$PerGrass)

# SYD import data
SYD_SSC_FULL <- sf::st_read("data-raw/SYD_FULL.shp")
SYD_SSC_FULL$PerAnyTree <- SYD_SSC_FULL$PerAnyTree*100
SYD_SSC_FULL$PerShrub <- SYD_SSC_FULL$PerShrub*100
SYD_SSC_FULL$PerGrass <- SYD_SSC_FULL$PerGrass*100
SYD_SSC_FULL$PerNonVeg <- (100 - SYD_SSC_FULL$PerAnyTree - SYD_SSC_FULL$PerShrub - SYD_SSC_FULL$PerGrass)


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
  

# Map Click ---------------------------------------------------------------


  
  PER_map_SSC <- reactiveValues(
    click = vector(mode = 'numeric')
  )
  PER_map_SSC$click <- 50008
  
  observeEvent(input$PER_SSC_map_shape_click,{
    click <- isolate(input$PER_SSC_map_shape_click)
    isolate({PER_map_SSC$click = click$id})
    print(PER_map_SSC$click)
  })
  
  
  
}



