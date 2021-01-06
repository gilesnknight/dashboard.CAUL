#' Scatter tools
#' @importFrom magrittr "%>%"
#' @importFrom dplyr mutate
#' @export


# Takes sf object and modifies for scatterplotting 
# scatter_data <- function(df, columnsToDrop, uniqueID){
#   df %>% 
#     sf::st_drop_geometry() %>% 
#     dplyr::select(-c(columnsToDrop)) %>% 
#     dplyr::mutate(yaxis_ran = runif(sum(stats::complete.cases(uniqueID)), min = 1.98, max =
#                                       2.02))
# }


filter_scatter <- function(df, columnsToPlot, uniqueID){
  df %>% 
    sf::st_drop_geometry() %>% 
    dplyr::select(columnsToPlot) %>%
    dplyr::mutate(yaxis_ran = runif(sum(stats::complete.cases(uniqueID)), min = 1.98, max =
                                      2.02))
}


# Updates SSC scatter annotation from map click
scatter_annotation <-  function(df, uniqueID, clickID, xVals, yVals, structureName){
  selected_structure <- df[df[[uniqueID]] == clickID,]
  selected_label <- base::list(
    x = selected_structure[[xVals]],
    y = selected_structure[[yVals]],
    text = selected_structure[[structureName]],
    xref = "x",
    yref = "y",
    showarrow = TRUE,
    arrowhead = 4,
    arrowsize = .5,
    ax = 10,
    ay = -20
    # showarrow = FALSE,
    # xanchor = 'bottom center'
  )
}

# Updates SSC scatter quintile mean line from map click
scatter_quinline <- function(df, uniqueID, clickID, groupID, xVals){
  selected_quintile <- base::as.numeric(df[df[[uniqueID]] == clickID,][groupID])
  selected_quintile_filter <- dplyr::filter(df, !!base::as.symbol(groupID) == selected_quintile)
  quintile_abline <- base::list(type = 'line',
                                layer = 'below',
                                line = list(color= "#08519c", width=1, dash="dash"),
                                x0 = base::mean(selected_quintile_filter[, xVals]),
                                x1 = base::mean(selected_quintile_filter[, xVals]),
                                y0 = 1.95,
                                y1 = 2.05)
}

# Updates SSC scatter quintile mean label from map click
scatter_quinline_label <-  function(df, uniqueID, clickID, groupID, xVals){
  selected_quintile <- base::as.numeric(df[df[[uniqueID]] == clickID,][groupID])
  selected_quintile_filter <- dplyr::filter(df, !!base::as.symbol(groupID) == selected_quintile)
  base::list(yref = "paper",
             x = base::mean(selected_quintile_filter[[xVals]]),
             y = 1.04,
             text = base::paste0("Quintile mean (", base::round(base::mean(selected_quintile_filter[[xVals]]),1), "%)"),
             showarrow = FALSE)
}

# Updates SSC scatter mean line from map click
scatter_mean <- function(df, xVals){
  base::list(type = "line",
             layer = 'below',
             line = list(color= "#252525", width=1, dash="dash"),
             x0 = base::mean(df[, xVals]),
             x1 = base::mean(df[, xVals]),
             y0 = 1.95,
             y1 = 2.05)
}

# Updates SSC scatter mean label from map click
scatter_mean_label <- function(df, xVals){
  base::list(yref = "paper",
             x = base::mean(df[, xVals]),
             y = 1.04,
             text = base::paste0("Mean (",base::round(base::mean(df[, xVals]),1),"%)"),
             showarrow = FALSE)
}





# Creates SSC scatterplot
scatter_plot <- function(df, xVals, yVals, source, structureName, plotTitle, tree, densityQuintile, density){
  plotly::plot_ly(
    type = 'scatter',
    mode = 'markers',
    source = source,
    data = df,
    x = ~xVals,
    y = ~yVals,
    marker = list(size = 10,
                  opacity = 0.8,
                  color = "#d9d9d9"
                  #line = list(color = 'black', width = 0.5)
    ),
    showlegend = FALSE,
    hoverinfo = 'text',
    text = ~base::paste0('</br><b>', structureName, '</b>',
                         '</br> Tree canopy: ', base::round(tree, 1), "%",
                         '</br> Density quintile: ', densityQuintile,
                         '</br> Density: ', base::round(density, 2))
  ) %>% 
    plotly::layout(
      title = plotTitle,
      font = list(family = "Helvetica Neue", color = "black", size = 9),
      yaxis = list(
        range = c(1.95, 2.05),
        title = "",
        zeroline = FALSE,
        showline = FALSE,
        showticklabels = FALSE,
        showgrid = FALSE
      ),
      xaxis = list(
        showline = TRUE,
        showticklabels = TRUE,
        zeroline = FALSE,
        title = "Tree Canopy (%)"
      ),
      margin = list(
        l = 5,
        r = 5,
        b = 5,
        t = 40,
        pad = 4
      )
    ) %>%
    plotly::config(displayModeBar = F) %>% 
    plotly::hide_colorbar()
  
}


#Updates scatter from selected structure













#MISC



perth_map_active_SSC <- function(mapClick){
  clicked_structure <- mapClick
  base::as.numeric(clicked_structure$id)
}

SSC_code_from_plot_click <- function(df, pointClick){
  clicked_point <- pointClick
  base::as.numeric(df[clicked_point$pointNumber + 1,]["SSC_CODE16"])
}

perth_plot_active_qui <- function(df, clickID){
  selected_quintile <-
    base::as.numeric(df[df[['SSC_CODE16']] == clickID, ]['DensityQui'])
  perth_SSC_selected_quintile <-
    filter(df, DensityQui == selected_quintile)
}

quin_hover_text <- function(structureName, tree, densityQuintile, density) {
  base::paste0('</br><b>',structureName,'</b>',
    '</br> Tree canopy: ',base::round(tree, 1),"%",
    '</br> Density quintile: ',densityQuintile,
    '</br> Density: ',base::round(density, 2)
  )
}