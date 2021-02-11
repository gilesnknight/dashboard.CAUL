#' echarts4r plots for geometric structure
#' 
#' @return A echarts4r plots
#' @export

echarts_vegtype_pie <- function(df, values, columnNames){
  df %>% 
    echarts4r::e_charts(columnNames) %>% 
    echarts4r::e_pie(serie = values, 
                     name = "default",
                     legend = FALSE,
                     label = list(position = 'outside'),
                     labelLine = list(show = TRUE, 
                                      length = '2.5px',
                                      length2 = '5px')
    ) %>% 
    echarts4r::e_labels() %>% 
    echarts4r::e_color(color = c('#a1d99b', '#41ab5d', '#005a32', '#bdbdbd')) %>% 
    echarts4r::e_tooltip(formatter =  echarts4r::e_tooltip_choro_formatter(style = "percent",
                                                                           digits = 1)) %>%
    echarts4r::e_title("Vegetation Type", left = 'center')
}

