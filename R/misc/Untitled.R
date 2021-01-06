
#Updates plot 1 click event
perth_scatter_click1 <- reactive({
  event_data("plotly_click",
             source = 'perth_SSC_plot1',
             priority = "event")
})

#Updates plot 2 click event
plot2_click <- reactive({
  event_data("plotly_click",
             source = 'perth_LGA_plot1',
             priority = "event")
})

#Initialises variable to combine plot click events and record the latest
scatterClick <- reactiveValues(
  latest = vector(mode = 'numeric')
)

#Upon plot 1 click, updates combined click variable with the latest click
observeEvent(perth_scatter_click1(),{
  latest <- isolate(perth_scatter_click1())
  isolate({scatterClick$latest = latest})
})

#Upon plot 2 click, updates combined click variable with the latest click
observeEvent(plot2_click(),{
  latest <- isolate(plot2_click())
  isolate({scatterClick$latest = latest})
})

#prints latest click from plots
observe({
  #browser()
  print(scatterClick$latest)
})




# Initialises the current SSC (from both map and combined plot clicks)
activeSSC <- reactiveValues(
  id = vector(mode = 'numeric')
)

observeEvent(scatterClick[['latest']],{
  currentSSC <- SSC_code_from_plot_click(df = perth_SSC_plot1_data, pointClick = scatterClick[['latest']])
  selected_quintile <-
    base::as.numeric(perth_SSC_plot1_data[perth_SSC_plot1_data[['SSC_CODE16']] == currentSSC, ]['DensityQui'])
  perth_SSC_selected_quintile <-
    filter(perth_SSC_plot1_data, DensityQui == selected_quintile)
})

plotClick <- reactive({
  if(scatterClick[['latest']][['curveNumber']] == 0){
    activeSSC$id <- SSC_code_from_plot_click(df = perth_SSC_plot1_data,
                                             pointClick = scatterClick$latest)
  } else{
    activeSSC$id <- SSC_code_from_plot_click(df = perth_SSC_selected_quintile,
                                             pointClick = scatterClick$latest)
  }
})


# Upon map click updates activeSSC with the code of the clicked SSC
observeEvent(input$perth_SSC_map_shape_click,{
  id <- isolate(input$perth_SSC_map_shape_click)
  isolate({activeSSC$id = id$id})
  print(activeSSC$id)
})


observeEvent(plotClick(),{
  id <- isolate(plotClick())
  isolate({activeSSC$id = id})
  print(activeSSC$id)
})






















































##### Server #####

# Updates SSC scatter quintile colours from map click
scatter_quincolours <- function(df, uniqueID, clickID, groupID){
  selected_quintile <- base::as.numeric(df[df[[uniqueID]] == clickID,][groupID])
  if(selected_quintile == 1){
    return(c('#4292c6','#d9d9d9','#d9d9d9','#d9d9d9','#d9d9d9'))
  } else if(selected_quintile == 2){
    return(c('#d9d9d9','#4292c6','#d9d9d9','#d9d9d9','#d9d9d9'))
  } else if(selected_quintile == 3){
    return(c('#d9d9d9','#d9d9d9','#4292c6','#d9d9d9','#d9d9d9'))
  } else if(selected_quintile == 4){
    return(c('#d9d9d9','#d9d9d9','#d9d9d9','#4292c6','#d9d9d9'))
  } else if(selected_quintile == 5){
    return(c('#d9d9d9','#d9d9d9','#d9d9d9','#d9d9d9','#4292c6'))
  } else{
    return(c('#d9d9d9','#d9d9d9','#d9d9d9','#d9d9d9','#d9d9d9'))
  }
}


# Updates SSC scatter annotation from map click
perth_SSC_map_annotations <- reactive({
  perth_SSC_click <- input$perth_SSC_map_shape_click
  if (!is.null(perth_SSC_click)) {
    scatter_annotation(
      df = perth_SSC_plot1_data,
      uniqueID = "SSC_CODE16",
      clickID = perth_SSC_click$id,
      xVals = "PerTree16",
      yVals = "yaxis_ran",
      structureName = "SSC_NAME16"
    )
  }
  else{
    return(NULL)
  }
})

# Updates SSC scatter quintile mean line from map click
perth_SSC_quin <- reactive({
  perth_SSC_click <- input$perth_SSC_map_shape_click
  if(!is.null(perth_SSC_click)){
    scatter_quinline(
      df = perth_SSC_plot1_data,
      uniqueID = "SSC_CODE16",
      clickID = perth_SSC_click$id,
      groupID = "DensityQui",
      xVals = "PerTree16"
    )
  }
  else{
    return(NULL)
  }
})

# Updates SSC scatter quintile mean label from map click
perth_SSC_quin_label <- reactive({
  perth_SSC_click <- input$perth_SSC_map_shape_click
  if(!is.null(perth_SSC_click)){
    scatter_quinline_label(
      df = perth_SSC_plot1_data,
      uniqueID = "SSC_CODE16",
      clickID = perth_SSC_click$id,
      groupID = "DensityQui",
      xVals = "PerTree16"
    )
  }
  else{
    return(NULL)
  }
})

Updates SSC scatter quintile colours from map click
scatter_pointcolors <- reactive({
  perth_SSC_click <- input$perth_SSC_map_shape_click
  if(!is.null(perth_SSC_click)){
    scatter_quincolours(
      df = perth_SSC_plot1_data,
      uniqueID = "SSC_CODE16",
      clickID = perth_SSC_click$id,
      groupID = "DensityQui"
    )
  }
  else{
    return(c('#d9d9d9','#d9d9d9','#d9d9d9','#d9d9d9','#d9d9d9'))
  }
})