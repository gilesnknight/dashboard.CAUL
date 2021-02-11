

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
