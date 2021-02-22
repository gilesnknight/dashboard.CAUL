

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
    ggplot2::scale_y_continuous(position = "left", labels = scales::percent,
                                limits=c(0, 1),
                                expand = c(0, 0)) +
    ggplot2::scale_x_continuous(expand = c(0, 0)) +
    ggplot2::ggtitle("Vegetation type") +
    ggplot2::theme_classic() +
    ggplot2::theme(
      axis.title.x = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_text(vjust=0.5),
      #axis.ticks.x = ggplot2::element_blank(),
      axis.line.x = ggplot2::element_blank(),
      axis.title.y = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank(),
      axis.line.y = ggplot2::element_blank(),
      legend.title = ggplot2::element_blank(),
      legend.text = ggplot2::element_text(size = 8),
      legend.key.size = ggplot2::unit(0.4, "cm"),
      legend.margin = ggplot2::margin(0, 0, 0, 580),
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
    ggplot2::scale_y_continuous(position = "left", labels = scales::percent,
                                limits=c(0, 1),
                                expand = c(0, 0)) +
    ggplot2::scale_x_continuous(expand = c(0, 0)) +
    ggplot2::ggtitle("Tenure split") +
    ggplot2::theme_classic() +
    ggplot2::theme(
      axis.title.x = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_text(vjust=0.5),
      #axis.ticks.x = ggplot2::element_blank(),
      axis.line.x = ggplot2::element_blank(),
      axis.title.y = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank(),
      axis.line.y = ggplot2::element_blank(),
      legend.title = ggplot2::element_blank(),
      legend.text = ggplot2::element_text(size = 8),
      legend.key.size = ggplot2::unit(0.4, "cm"),
      legend.margin = ggplot2::margin(0, 0, 0, 680),
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
      values = c('#e3ff8a', '#a6cee3', '#828282', '#fbd4f2', '#5ce8c7', '#ffed6f', '#bc80bd', '#fb8072', '#dcdcdc', '#b3de69', '#fdb462'),
      guide = ggplot2::guide_legend(reverse = TRUE, nrow = 1)
    ) +
    ggplot2::coord_flip() +
    ggplot2::scale_y_continuous(position = "left", labels = scales::percent,
                                limits=c(0, 1),
                                expand = c(0, 0)) +
    ggplot2::scale_x_continuous(expand = c(0, 0)) +
    ggplot2::ggtitle("Land-use split") +
    ggplot2::theme_classic() +
    ggplot2::theme(
      axis.title.x = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_text(vjust=0.5),
      #axis.ticks.x = ggplot2::element_blank(),
      axis.line.x = ggplot2::element_blank(),
      axis.title.y = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank(),
      axis.line.y = ggplot2::element_blank(),
      legend.title = ggplot2::element_blank(),
      legend.text = ggplot2::element_text(size = 8),
      legend.key.size = ggplot2::unit(0.4, "cm"),
      legend.text.align = 0,
      legend.margin = ggplot2::margin(0, 0, 0, 120),
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
      values = c('#e3ff8a', '#a6cee3', '#828282', '#fbd4f2', '#5ce8c7', '#ffed6f', '#bc80bd', '#fb8072', '#dcdcdc', '#b3de69', '#fdb462'),
      guide = ggplot2::guide_legend(reverse = TRUE)
    ) +
    ggplot2::coord_flip() +
    ggplot2::scale_y_continuous(position = "left", labels = scales::percent,
                                limits=c(0, 1),
                                expand = c(0, 0)) +
    ggplot2::scale_x_continuous(expand = c(0, 0)) +
    ggplot2::ggtitle("Tree land-use split") +
    ggplot2::theme_classic() +
    ggplot2::theme(
      axis.title.x = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_text(vjust=0.5),
      #axis.ticks.x = ggplot2::element_blank(),
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
                  height_svg =8.5,
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

densityLabel <- function(xAxis){
  if(xAxis=='GrDwDens'){
    return("Gross density")
  } else if(xAxis=='UrbDwDens'){
    return("Urban dwelling density")
  } else if(xAxis=='ResDwDens'){
    return("Residential dwelling density")
  } else {
    return("Density")
  }
}

densityScatter <- function(scatter_selected_data,
                           scatter_quint_data,
                           scatter_remaining_data,
                           structureName,
                           uniqueID,
                           xAxis,
                           yAxis){
  plot <- ggplot2::ggplot() +
    ggiraph::geom_point_interactive(
      ggplot2::aes(
        x = scatter_remaining_data[[xAxis]],
        y = scatter_remaining_data[[yAxis]],
        data_id = scatter_remaining_data[[uniqueID]],
        tooltip = paste0(
          "<b>",scatter_remaining_data[[structureName]],"</b>",
          "<br>",
          "Different density: ",
          base::round(scatter_remaining_data[[xAxis]],1),
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
        x = scatter_quint_data[[xAxis]],
        y = scatter_quint_data[[yAxis]],
        data_id = scatter_quint_data[[uniqueID]],
        tooltip = paste0(
          "<b>",scatter_quint_data[[structureName]],"</b>",
          "<br>",
          "Similar density: ",
          base::round(scatter_quint_data[[xAxis]],1),
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
        x = scatter_selected_data[[xAxis]],
        y = scatter_selected_data[[yAxis]],
        data_id = scatter_selected_data[[uniqueID]],
        tooltip = paste0(
          "<b>",scatter_selected_data[[structureName]],"</b>",
          "<br>",
          "Density: ",
          base::round(scatter_selected_data[[xAxis]],1),
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
    ggplot2::labs(x = densityLabel(xAxis), y = "Tree canopy cover") +
    ggplot2::scale_x_continuous(#label = scales::percent_format(accuracy = 1)
                                #expand = c(0, 0.01)
    ) +
    ggplot2::scale_y_continuous(label = scales::percent_format(accuracy = 1, scale = 1)
                                #expand = c(0, 0)
    ) +
    ggplot2::theme_classic() 
  
  girafe <- ggiraph::girafe(
    code = print(plot),
    fonts = list(serif = "Helvetica"),
    width_svg = 11, 
    height_svg =7,
    options = list(
      ggiraph::opts_selection(type = "single", only_shiny = FALSE),
      ggiraph::opts_toolbar(saveaspng = FALSE),
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
    ggiraph::opts_hover(css = "stroke:rgba(207, 207, 207, 0.6);r:3.5pt;")
  )
  
  girafe
  
}

