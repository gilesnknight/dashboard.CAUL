#' Filters sf object to return piechart data from map click
#' 
#' @param df sf object
#' @param uniqueID Unique identifier for geometric structure, must match id of leaflet click event
#' @param columnsToDrop drops unnecessary columns from df
#' @param tree percentage tree in geometric structure
#' @param shrub percentage grass in geometric structure
#' @param grass percentage shrub in geometric structure
#' @return Filtered data ready for piechart plotting


# filter_piechart <- function(df, uniqueID, clickID, columnsToDrop, tree, shrub, grass, nonVeg){
#   filtered_piechart <- df %>%
#     sf::st_drop_geometry() %>%
#     dplyr::mutate(grassCover = grass * 100) %>%
#     dplyr::mutate(shrubCover = shrub * 100) %>%
#     dplyr::mutate(treeCover = tree * 100) %>%
#     dplyr::mutate(nonVeg = 100 - treeCover - shrubCover - grassCover) %>%
#     dplyr::filter(base::as.numeric(uniqueID) == base::as.numeric(clickID)) %>%
#     dplyr::select(-c(columnsToDrop)) %>%
#     tidyr::pivot_longer(cols = c(1, 2, 3, 4),
#                         names_to = "type",
#                         values_to = "percent")
#   filtered_piechart
# }


filter_SSC <- function(df, uniqueID, clickID){
  filtered_SSC <- df %>%
    dplyr::filter(base::as.numeric(uniqueID) == base::as.numeric(clickID))
  filtered_SSC
}


# filter_piechart <- function(df, columnsToPlot){
#   filtered_piechart <- df %>%
#     dplyr::select(columnsToPlot) %>%
#     tidyr::pivot_longer(cols = columnsToPlot,
#                         names_to = "type",
#                         values_to = "percent")
#   filtered_piechart
# }



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







# filter_piechart <- function(df, uniqueID, clickID, columnsToPlot){
#   filtered_piechart <- df %>%
#     sf::st_drop_geometry() %>%
#     dplyr::filter(base::as.numeric(uniqueID) == base::as.numeric(clickID)) %>%
#     dplyr::select(columnsToPlot) %>%
#     tidyr::pivot_longer(cols = columnsToPlot,
#                         names_to = "type",
#                         values_to = "percent")
#   filtered_piechart
# }






























