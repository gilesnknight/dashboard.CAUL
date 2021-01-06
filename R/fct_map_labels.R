#' Generate map labels for structure canopy cover
#' 
#' @param structureName The name of the geometric structure e.g. suburb name
#' @param structurePercent Tree canopy cover of structure
#' @return Labels to be passed into a leaflet map

map_labels <- function(labelName, labelTree, labelShrub, labelGrass) {
  base::sprintf(
    "<strong>%s</strong><br/>Tree: %s<br/>Shrub: %s<br/>Grass: %s",
    labelName,
    scales::percent(labelTree/100, accuracy = 0.1),
    scales::percent(labelShrub/100, accuracy = 0.1),
    scales::percent(labelGrass/100, accuracy = 0.1)
  ) %>% base::lapply(htmltools::HTML)
}
