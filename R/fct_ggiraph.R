

landusePiecharts <- function(vegtypeData,
                             vegtypeVals, 
                             vegtypeGroups, 
                             tenureData, 
                             tenureVals, 
                             tenureGroups,
                             landuseData,
                             landuseVals,
                             landuseGroups,
                             treelanduseData,
                             treelanduseVals,
                             treelanduseGroups,
                             activeSSCname){

  vegtypePie <- ggplot2::ggplot(vegtypeData, ggplot2::aes(x="", y=vegtypeVals, fill=vegtypeGroups)) +
    ggiraph::geom_bar_interactive(ggplot2::aes(tooltip = paste0(vegtypeGroups,": ", round(vegtypeVals,1), "%")), stat="identity", width=1) +
    ggplot2::coord_polar("y", start=0) +
    ggplot2::scale_fill_manual(values = c('#31a354', '#c7ebbc','#e8e8e8', '#88c981' )) +
    ggplot2::geom_text(ggplot2::aes(label = paste(round(vegtypeVals / sum(vegtypeVals) * 100, 1), "%"), x = 1.3),
              position = ggplot2::position_stack(vjust = 0.5),
              size = 2) +
    ggplot2::ggtitle("Vegetation type") +
    ggplot2::theme_void() +
    ggplot2::theme(legend.position="none",
                   plot.title = ggplot2::element_text(face = "bold", hjust = 0.5)) 
  
  tenurePie <- ggplot2::ggplot(tenureData, ggplot2::aes(x="", y=tenureVals, fill=tenureGroups)) +
    ggiraph::geom_bar_interactive(ggplot2::aes(tooltip = paste0(tenureGroups,": ", round(tenureVals,1), "%")), stat="identity", width=1) +
    ggplot2::coord_polar("y", start=0) +
    ggplot2::scale_fill_manual(values = c('#de2d26', '#3182bd')) +
    ggplot2::ggtitle("Land tenure split") +
    ggplot2::theme_void() +
    ggplot2::theme(legend.position="none",
                   plot.title = ggplot2::element_text(face = "bold", hjust = 0.5))
  
  landusePie <- ggplot2::ggplot(landuseData, ggplot2::aes(x="", y=landuseVals, fill=landuseGroups)) +
    ggiraph::geom_bar_interactive(ggplot2::aes(tooltip = paste0(landuseGroups,": ", round(landuseVals,1), "%")), stat="identity", width=1) +
    ggplot2::coord_polar("y", start=0) +
    ggplot2::scale_fill_manual(values = c('#5CE8C7', '#FFED6F', '#70AD47', '#BC80BD', '#F5F5F5', '#FB8072', '#B3DE69', '#E3FF8A', '#FDB462', '#264478', '#DAE3F3')) +
    ggplot2::ggtitle("Land-use split") +
    ggplot2::theme_void() +
    ggplot2::theme(legend.position="none",
                   plot.title = ggplot2::element_text(face = "bold", hjust = 0.5))
  
  treelandusePie <- ggplot2::ggplot(treelanduseData, ggplot2::aes(x="", y=treelanduseVals, fill=treelanduseGroups)) +
    ggiraph::geom_bar_interactive(ggplot2::aes(tooltip = paste0(treelanduseGroups,": ", round(treelanduseVals,1), "%")), stat="identity", width=1) +
    ggplot2::coord_polar("y", start=0) +
    ggplot2::scale_fill_manual(values = c('#5CE8C7', '#FFED6F', '#70AD47', '#BC80BD', '#F5F5F5', '#FB8072', '#B3DE69', '#E3FF8A', '#FDB462', '#264478', '#DAE3F3')) +
    ggplot2::ggtitle("Tree land-use split") +
    ggplot2::theme_void() +
    ggplot2::theme(legend.position="none",
                   plot.title = ggplot2::element_text(face = "bold", hjust = 0.5))
  
  patch <- (vegtypePie  +  ggplot2::theme(plot.margin = ggplot2::unit(c(0,50,0,0), "pt")) + tenurePie)/(landusePie  + ggplot2::theme(plot.margin = ggplot2::unit(c(0,50,0,0), "pt")) + treelandusePie) + patchwork::plot_annotation(
    title = paste0('Selected suburb: ',activeSSCname)
  )
  
  ggiraph::girafe(code = print(patch),
                  fonts = list(serif = "Helvetica"),
                  width_svg = 9, 
                  height_svg = 8,
                  options = list(ggiraph::opts_tooltip(#use_fill = TRUE,
                                                       css = "background-color:gray;
                                                              color:white;
                                                              font-style:italic;
                                                              padding:10px;
                                                              font-family: Helvetica;
                                                              border-radius:5px;"),
                                 ggiraph::opts_sizing(rescale = TRUE, width = 1)
                  )) 
  
}


# Creates 4 barcharts with interactive features
landuseBarcharts <- function(vegtypeData,
                             vegtypeVals,
                             vegtypeGroups, 
                             tenureData,
                             tenureVals,
                             tenureGroups,
                             landuseData,
                             landuseVals,
                             landuseGroups,
                             treelanduseData,
                             treelanduseVals,
                             treelanduseGroups){
  
  # Vegetation type bar chart
  vegtypeBar <- ggplot2::ggplot(vegtypeData,
                                ggplot2::aes(1, vegtypeVals, group = vegtypeGroups)) +
    ggiraph::geom_bar_interactive(ggplot2::aes(tooltip = paste0(vegtypeGroups, ": ", round(vegtypeVals *
                                                                                    100, 1), "%"), fill = vegtypeGroups),
                                  stat = "identity",
                                  width = 1) +
    ggplot2::scale_fill_manual(
      values = c('#e8e8e8', '#c7ebbc', '#88c981', '#31a354'),
      guide = ggplot2::guide_legend(reverse = TRUE)
    ) +
    ggplot2::coord_flip() +
    ggplot2::scale_y_continuous(position = "left", labels = scales::percent) +
    ggplot2::ggtitle("Vegetation type") +
    ggplot2::theme_classic() +
    ggplot2::theme(
      axis.title.x = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_blank(),
      axis.ticks.x = ggplot2::element_blank(),
      axis.line.x = ggplot2::element_blank(),
      axis.title.y = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank(),
      axis.line.y = ggplot2::element_blank(),
      legend.title = ggplot2::element_blank(),
      legend.text = ggplot2::element_text(size = 8),
      legend.key.size = ggplot2::unit(0.5, "cm"),
      legend.margin = ggplot2::margin(0, 0, 0, 380),
      plot.title = ggplot2::element_text(face = 'bold',
                                         vjust = -5.5),
      legend.position = "top"
    )
  
  # Land tenure bar chart
  tenureBar <- ggplot2::ggplot(tenureData,
                               ggplot2::aes(1, tenureVals, group = tenureGroups)) +
    ggiraph::geom_bar_interactive(ggplot2::aes(tooltip = paste0(tenureGroups, ": ", round(tenureVals *
                                                                                    100, 1), "%"), fill = tenureGroups),
                                  stat = "identity",
                                  width = 1) +
    ggplot2::scale_fill_manual(
      values = c('#de2d26', '#3182bd'),
      guide = ggplot2::guide_legend(reverse = TRUE)
    ) +
    ggplot2::coord_flip() +
    ggplot2::scale_y_continuous(position = "left", labels = scales::percent) +
    ggplot2::ggtitle("Tenure split") +
    ggplot2::theme_classic() +
    ggplot2::theme(
      axis.title.x = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_blank(),
      axis.ticks.x = ggplot2::element_blank(),
      axis.line.x = ggplot2::element_blank(),
      axis.title.y = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank(),
      axis.line.y = ggplot2::element_blank(),
      legend.title = ggplot2::element_blank(),
      legend.text = ggplot2::element_text(size = 8),
      legend.key.size = ggplot2::unit(0.5, "cm"),
      legend.margin = ggplot2::margin(0, 0, 0, 470),
      plot.title = ggplot2::element_text(face = 'bold',
                                         vjust = -5.5),
      legend.position = "top"
    )
  
  # Land-use bar chart
  landuseBar <- ggplot2::ggplot(landuseData,
                                ggplot2::aes(1, landuseVals, group = landuseGroups)) +
    ggiraph::geom_bar_interactive(ggplot2::aes(tooltip = paste0(landuseGroups, ": ", round(landuseVals *
                                                                                    100, 1), "%"), fill = landuseGroups),
                                  stat = "identity",
                                  width = 1) +
    ggplot2::scale_fill_manual(
      values = c('#5CE8C7', '#FFED6F', '#70AD47', '#BC80BD', '#F5F5F5', '#FB8072', '#B3DE69', '#E3FF8A', '#FDB462', '#264478', '#DAE3F3'),
      guide = ggplot2::guide_legend(reverse = TRUE, nrow = 2)
    ) +
    ggplot2::coord_flip() +
    ggplot2::scale_y_continuous(position = "left", labels = scales::percent) +
    ggplot2::ggtitle("Land-use split") +
    ggplot2::theme_classic() +
    ggplot2::theme(
      axis.title.x = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_blank(),
      axis.ticks.x = ggplot2::element_blank(),
      axis.line.x = ggplot2::element_blank(),
      axis.title.y = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank(),
      axis.line.y = ggplot2::element_blank(),
      legend.title = ggplot2::element_blank(),
      legend.text = ggplot2::element_text(size = 8),
      legend.key.size = ggplot2::unit(0.5, "cm"),
      legend.text.align = 0,
      legend.margin = ggplot2::margin(0, 0, 0, 150),
      plot.title = ggplot2::element_text(face = 'bold',
                                         vjust = -5.5),
      legend.position = "top"
    )
  
  # Tree land-use bar chart
  treelanduseBar <- ggplot2::ggplot(treelanduseData,
                                    ggplot2::aes(1, treelanduseVals, group = treelanduseGroups)) +
    ggiraph::geom_bar_interactive(ggplot2::aes(tooltip = paste0(treelanduseGroups, ": ", round(treelanduseVals *
                                                                                    100, 1), "%"), 
                                               fill = treelanduseGroups),
                                  stat = "identity",
                                  width = 1) +
    ggplot2::scale_fill_manual(
      values = c('#5CE8C7', '#FFED6F', '#70AD47', '#BC80BD', '#F5F5F5', '#FB8072', '#B3DE69', '#E3FF8A', '#FDB462', '#264478', '#DAE3F3'),
      guide = ggplot2::guide_legend(reverse = TRUE)
    ) +
    ggplot2::coord_flip() +
    ggplot2::scale_y_continuous(position = "left", labels = scales::percent) +
    ggplot2::ggtitle("Tree land-use split") +
    ggplot2::theme_classic() +
    ggplot2::theme(
      axis.title.x = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_blank(), #
      axis.ticks.x = ggplot2::element_blank(), #
      axis.line.x = ggplot2::element_blank(), #
      axis.title.y = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank(),
      axis.line.y = ggplot2::element_blank(),
      legend.title = ggplot2::element_blank(),
      
      
      legend.margin = ggplot2::margin(0, 0, 0, 0),
      plot.title = ggplot2::element_text(face = 'bold',
                                         vjust = 0),
      legend.position = "none"
    )
  
  # Pass into ggiraph for interactive features
  ggiraph::girafe(code = print(vegtypeBar/tenureBar/landuseBar/treelanduseBar),
                  fonts = list(serif = "Helvetica"),
                  width_svg = 11, 
                  height_svg =9,
                  options = list(ggiraph::opts_tooltip(
                    css = "background-color:gray;
                                                              color:white;
                                                              font-style:italic;
                                                              padding:10px;
                                                              font-family: Helvetica;
                                                              border-radius:5px;"),
                    ggiraph::opts_sizing(rescale = TRUE, width = 1),
                    ggiraph::opts_toolbar(saveaspng = FALSE)
                  )) 
  
  
}


filter_barchart <- function(df, columnsToPlot, newNames, order){
  filtered_barchart <- df %>%
    dplyr::select(columnsToPlot) %>%
    dplyr::rename_at(vars(columnsToPlot), ~ newNames) %>% 
    tidyr::pivot_longer(cols = newNames,
                        names_to = "type",
                        values_to = "percent")
  filtered_barchart[['type']] <- base::factor(newNames, levels = order)
  filtered_barchart[['percent']] <- filtered_barchart[['percent']]/100
  filtered_barchart
}

