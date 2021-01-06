


#Perth Clicks



# SSC ID from map click




perth_map_active_SSC <- function(mapClick){
  clicked_structure <- mapClick
  base::as.numeric(clicked_structure$id)
}

perth_plot_active_SSC <- function(df, plotClick){
  clicked_point <- event_data("plotly_click",
                              source = 'perth_SSC_plot1',
                              priority = "event")
  base::as.numeric(df[plot_click + 1,]["SSC_CODE16"])
}

perth_active_SSC <-  reactive({
  perth_map_active_SSC(mapClick = input$perth_SSC_map_shape_click)
})

perth_active_SSC <- reactive({
  perth_plot_active_SSC(df = perth_SSC_plot1_data,
                        plotClick = clicked_point$pointNumber)
})






















perth_SSC_to_zoom <- reactive({
  clicked_point <- event_data("plotly_click",
                    source = 'perth_SSC_plot1',
                    priority = "event")
  if(base::is.null(clicked_point)){
    return(NULL)
  } else{
    return(st_coordinates(st_centroid(perth_SSC[clicked_point$pointNumber + 1,]$geometry)))
  }
})

observe({
  if(is.null(perth_SSC_to_zoom))
    return()
  leafletProxy("perth_SSC_map") %>% 
    setView(lng = perth_SSC_to_zoom$lng, lat = perth_SSC_to_zoom$lat, zoom = 10)
})






perth_SSC_plot1_data %>%
  dplyr::filter(42.51982 %in% PerTree16) %>%
  dplyr::filter(1.991681 %in% yaxis_ran)


 perth_SSC_plot1_data %>%
   dplyr::filter(stringr::str_detect(PerTree16, 42.51982))

 stringr::str_extract(42.51982, "\\d{4}")

 #  curveNumber pointNumber        x        y
 # 1           0          27 42.51982 1.991681

#28       50092         Bedfordale 42.5198207  0.181075030
base::substr(42.51982,1,7)

#42.5198
#42.5198999