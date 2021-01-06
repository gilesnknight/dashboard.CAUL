#' Plotly piechart of vegetation types for geometric structure
#' 
#' @param df Filtered df using filter_piechart
#' @param pieVals Column of percent values for vegetation types
#' @return A plotly piechart
#' @export

vegtype_pie <- function(df, pieVals){
  plotly::plot_ly(
    df,
    values = ~ pieVals,
    type = 'pie',
    textposition = 'inside',
    textinfo = 'label+percent',
    hoverinfo = 'label+percent',
    showlegend = FALSE,
    source = 'perth_LGA_plot1',
    labels = c('Grass:', 'Shrub:', 'Tree:', 'Non-veg:'),
    marker = list(colors = c('#e5f5e0', '#a1d99b', '#31a354', '#f0f0f0'))
  ) %>% 
    plotly::layout(title = "<b>Vegetation Type (%)</b>",
                   font = list(family = "Helvetica Neue", color = "black"),
                   xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                   yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                   margin = list(
                     l = 5,
                     r = 5,
                     b = 5,
                     t = 50,
                     pad = 4
                   )) %>% 
    plotly::config(displayModeBar = FALSE)
}

privpubl_pie <- function(df, pieVals){
  plotly::plot_ly(
    df,
    values = ~ pieVals,
    type = 'pie',
    textposition = 'inside',
    textinfo = 'label+percent',
    hoverinfo = 'label+percent',
    showlegend = FALSE,
    source = 'perth_LGA_plot1',
    labels = c('Private:', 'Public:'),
    marker = list(colors = c('#de2d26', '#3182bd'))
  ) %>% 
    plotly::layout(title = "<b>Tree Tenure Split (%)</b>",
                   font = list(family = "Helvetica Neue", color = "black"),
                   xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                   yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                   margin = list(
                     l = 5,
                     r = 5,
                     b = 5,
                     t = 50,
                     pad = 4
                   )) %>% 
    plotly::config(displayModeBar = FALSE)
}

LU_pie <- function(df, pieVals){
  plotly::plot_ly(
    df,
    values = ~ pieVals,
    type = 'pie',
    textposition = 'inside',
    textinfo = 'label+percent',
    hoverinfo = 'label+percent',
    showlegend = FALSE,
    source = 'perth_LGA_plot1',
    labels = c('Residential:', 'Parkland:', 'Infrastructure:', 'Other:', 'Industrial:', 
               'Education:', 'Commercial:', 'Hospital/Medical:', 'Transport:', 'Water:', 
               'Primary Production:'),
    marker = list(colors = c('#FDB462', '#B3DE69', '#F5F5F5', '#FB8072', '#BC80BD', 
                             '#FFED6F', '#5CE8C7', '#70AD47', '#264478', '#DAE3F3', 
                             '#E3FF8A'))
  ) %>% 
    plotly::layout(title = "<b>Land Use (%)</b>",
                   font = list(family = "Helvetica Neue", color = "black"),
                   xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                   yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                   margin = list(
                     l = 5,
                     r = 5,
                     b = 5,
                     t = 50,
                     pad = 4
                   )) %>% 
    plotly::config(displayModeBar = FALSE)
}

TrLU_pie <- function(df, pieVals){
  plotly::plot_ly(
    df,
    values = ~ pieVals,
    type = 'pie',
    textposition = 'inside',
    textinfo = 'label+percent',
    hoverinfo = 'label+percent',
    showlegend = FALSE,
    source = 'perth_LGA_plot1',
    labels = c('Residential:', 'Parkland:', 'Infrastructure:', 'Other:', 'Industrial:', 
               'Education:', 'Commercial:', 'Hospital/Medical:', 'Transport:', 'Water:', 
               'Primary Production:'),
    marker = list(colors = c('#FDB462', '#B3DE69', '#F5F5F5', '#FB8072', '#BC80BD', 
                             '#FFED6F', '#5CE8C7', '#70AD47', '#264478', '#DAE3F3', 
                             '#E3FF8A'))
  ) %>% 
    plotly::layout(title = "<b>Tree Land Use (%)</b>",
                   font = list(family = "Helvetica Neue", color = "black"),
                   xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                   yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                   margin = list(
                     l = 5,
                     r = 5,
                     b = 5,
                     t = 50,
                     pad = 4
                   )) %>% 
    plotly::config(displayModeBar = FALSE)
}



# vegtype_pie <- function(df, pieVals){
#   plotly::plot_ly(
#     df,
#     values = ~ pieVals,
#     type = 'pie',
#     textposition = 'inside',
#     textinfo = 'label+percent',
#     hoverinfo = 'percent',
#     showlegend = FALSE,
#     source = 'melb_SSC_plot1',
#     labels = c('Residential:', 'Parkland:', 'Infrastructure:', 'Other:', 'Industrial','Education', 'Commmercial', 'Hospital', 'Transport', 'Water', 'Primary Production'),
#     marker = list(colors = c('#e5f5e0', '#a1d99b', '#31a354', '#f0f0f0'))
#   ) %>% 
#     plotly::layout(title = "<b>Vegetation Type (%)</b>",
#                    font = list(family = "Helvetica Neue", color = "black"),
#                    xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
#                    yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
#                    margin = list(
#                      l = 5,
#                      r = 5,
#                      b = 5,
#                      t = 50,
#                      pad = 4
#                    )) %>% 
#     plotly::config(displayModeBar = FALSE)
# }
