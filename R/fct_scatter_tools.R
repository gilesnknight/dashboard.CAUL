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
    #sf::st_drop_geometry() %>% 
    dplyr::select(columnsToPlot) #%>%
    # dplyr::mutate(yaxis_ran = runif(sum(stats::complete.cases(uniqueID)), min = 1.98, max =
    #                                   2.02))
}


scatter_selected_data <- function(df, uniqueID, activeID){
  df[df[[uniqueID]] ==  activeID, ]
}

scatter_selected_quint <- function(df, quintileNum){
  df[[quintileNum]]
}

scatter_quint_data <- function(df, quintileNum, selectedQuintile, uniqueID, activeID){
  base::subset(df, df[[quintileNum]] == selectedQuintile & df[[uniqueID]] != activeID)
}

scatter_remaining_data <- function(df, quintileNum, selectedQuintile){
  base::subset(df, df[[quintileNum]] != selectedQuintile)
}

globalMean <- function(scatter_selected_data_vals, scatter_quint_data_vals, scatter_remaining_data_vals, column){
  combined <- data.frame(combined = c(scatter_selected_data_vals[,column], scatter_quint_data_vals[,column], scatter_remaining_data_vals[,column]))
  mean(combined$combined, na.rm = TRUE)
}

pseudoScatter <- function(scatter_selected_data,
                          scatter_quint_data,
                          scatter_remaining_data,
                          uniqueID,
                          xAxis,
                          yAxis,
                          structureName,
                          density){
  
  plot <-  ggplot2::ggplot() +
    ggplot2::geom_vline(
      ggplot2::aes(
        xintercept = base::mean(
          (scatter_quint_data[[xAxis]]/100)) 
        #linetype="Similar density average"
      ),
      color = "#0433FF",
      size=1,
      linetype="dashed",
      show.legend = TRUE
    ) +
    ggplot2::geom_vline(
      ggplot2::aes(
        xintercept = (globalMean(
          scatter_selected_data_vals = scatter_selected_data,
          scatter_quint_data_vals = scatter_quint_data,
          scatter_remaining_data_vals = scatter_remaining_data,
          column = xAxis)/100)
        
      ),
      color = "#1f2224",
      size=1,
      linetype="dashed",
      show.legend = TRUE
    ) +
    ggiraph::geom_point_interactive(
      ggplot2::aes(
        x = scatter_remaining_data[[xAxis]]/100,
        y = scatter_remaining_data[[yAxis]],
        data_id = scatter_remaining_data[[uniqueID]],
        tooltip = paste0(
          "<b>",scatter_remaining_data[[structureName]],"</b>",
          "<br>",
          "Different density: ",
          base::round(scatter_remaining_data[[density]],1),
          "<br>",
          "Tree canopy: ",
          base::round(scatter_remaining_data[[xAxis]],1),
          "%"
        )
      ),
      color = "#a3a3a3",
      alpha = 0.75,
      size = 4,
      shape=16,
      stroke = 0
    ) +
    ggiraph::geom_point_interactive(
      ggplot2::aes(
        x = scatter_quint_data[[xAxis]]/100,
        y = scatter_quint_data[[yAxis]],
        data_id = scatter_quint_data[[uniqueID]],
        tooltip = paste0(
          "<b>",scatter_quint_data[[structureName]],"</b>",
          "<br>",
          "Similar density: ",
          base::round(scatter_quint_data[[density]],1),
          "<br>",
          "Tree canopy: ",
          base::round(scatter_quint_data[[xAxis]],1),
          "%"
        )
      ),
      color = "#4f84e0",
      alpha = 0.75,
      size = 4,
      shape=16,
      stroke = 0
    ) +
    ggiraph::geom_point_interactive(
      ggplot2::aes(
        x = scatter_selected_data[[xAxis]]/100,
        y = scatter_selected_data[[yAxis]],
        data_id = scatter_selected_data[[uniqueID]],
        tooltip = paste0(
          "<b>",scatter_selected_data[[structureName]],"</b>",
          "<br>",
          "Density: ",
          base::round(scatter_selected_data[[density]],1),
          "<br>",
          "Tree canopy: ",
          base::round(scatter_selected_data[[xAxis]],1),
          "%"
        )
      ),
      color = "#fc3003",
      size = 4,
      shape=16,
      stroke = 0
    ) +
    # ggplot2::scale_linetype_manual(name = NULL,
    #                       values = c("Similar density average" = "dashed",
    #                                  "Suburb average" = "solid")) +
    # ggplot2::scale_linetype_manual(name = "Legend",
    #                                values = c("Suburb average" = "solid")) +
    ggplot2::labs(x = NULL, y = NULL) +
    ggplot2::scale_x_continuous(label = scales::percent_format(accuracy = 1),
                                expand = c(0, 0.065)) +
    ggplot2::scale_y_continuous(expand = c(0, 0), limits = c(1.97, 2.03)) +
    ggplot2::theme_classic() +
    ggplot2::theme(
      axis.title.y = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank(),
      axis.line.y = ggplot2::element_blank(),
    )
  
girafe <- ggiraph::girafe(
            code = print(plot),
            fonts = list(serif = "Helvetica"),
            width_svg = 9,
            height_svg = 4,
            options = list(
              ggiraph::opts_tooltip(
                use_fill = TRUE,
                css = "background-color:gray;
                      color:white;
                      font-style:italic;
                      padding:10px;
                      font-family: Helvetica;
                      border-radius:5px;"
              ),
              ggiraph::opts_sizing(rescale = TRUE, width = 1)
            )
        )
girafe <- ggiraph::girafe_options(
  girafe,
  ggiraph::opts_hover(css = "stroke:rgba(207, 207, 207, 0.6);
                      r:3.5pt;")
)  
girafe
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
             y = 1.07,
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