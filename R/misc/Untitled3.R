
scatter_select_quintile <- function(df, uniqueID, clickID, groupID){
  selected_quintile <- base::as.numeric(df[df[[uniqueID]] == clickID,][groupID])
  dplyr::filter(perth_SSC_plot1_data, DensityQui == selected_quintile)
}


perth_SSC_selected_quintile <- reactive({
  perth_SSC_click <- input$perth_SSC_map_shape_click
  if(!is.null(perth_SSC_click)){
    scatter_select_quintile(
      df = perth_SSC_plot1_data,
      uniqueID = "SSC_CODE16",
      clickID = perth_SSC_click$id,
      groupID = "DensityQui"
    )
  }
  else{
    return(NULL)
  }
})



observeEvent(input$perth_SSC_map_shape_click, {
  
  plotly::plotlyProxy("perth_SSC_plot1", session) %>% 
    plotly::plotlyProxyInvoke("addTraces",
                              list(data = perth_SSC_selected_quintile,
                                   x = ~PerTree16,
                                   y = ~yaxis_ran,
                                   type = 'line',
                                   mode = 'markers'))
})


