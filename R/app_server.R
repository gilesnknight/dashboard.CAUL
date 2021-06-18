#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @importFrom magrittr "%>%"
#' @import dplyr
#' @noRd
#'


# IMPORT DATA --------------------------------------------------------------

PER_SSC_GEO <- sf::st_read("inst/extdata/PER_SSC_GEO.gpkg")
PER_SSC_DATA <- base::readRDS("inst/extdata/PER_SSC_DATA.rds")

MEL_SSC_GEO <- sf::st_read("inst/extdata/MEL_SSC_GEO.gpkg")
MEL_SSC_DATA <- base::readRDS("inst/extdata/MEL_SSC_DATA.rds")

SYD_SSC_GEO <- sf::st_read("inst/extdata/SYD_SSC_GEO.gpkg")
SYD_SSC_DATA <- base::readRDS("inst/extdata/SYD_SSC_DATA.rds")

ALL_SSC_DATA <- base::readRDS("inst/extdata/ALL_SSC_DATA.rds")



# Server ------------------------------------------------------------------

app_server <- function(input, output, session) {
  
  shiny::observeEvent(input$navbar, {
    base::print(input$navbar)
  })

  observeEvent(input$navbar, {
    shiny::req(input$navbar == "Perth")
    output$PER_SSC_map <- leaflet::renderLeaflet({
      base_map()
    })
  })

  # Add PER SSC polygons to PER basemap when PER tab is visible
  shiny::observeEvent(input$navbar, {
    shiny::req(input$navbar == "Perth")

    shiny::withProgress(message = NA, style = "old", value = 0, {
      shiny::incProgress(1 / 3, detail = NA)

      map_add_polys(
        df = PER_SSC_GEO,
        mapID = "PER_SSC_map",
        structureID = PER_SSC_GEO$SSC_CODE16,
        structureName = PER_SSC_GEO$SSC_NAME16,
        structureTree = PER_SSC_GEO$PerAnyTree,
        structureShrub = PER_SSC_GEO$PerGrass,
        structureGrass = PER_SSC_GEO$PerShrub,
        minZoom = 8,
        maxZoom = 14,
        lng1 = 115.191,
        lat1 = -31.243,
        lng2 = 116.743,
        lat2 = -33.116,
        viewLng = 115.850,
        viewLat = -32.100,
        viewZoom = 10
      )

      shiny::incProgress(2 / 3, detail = NA)

      leaflet::leafletProxy("PER_SSC_map") %>%
        leaflet::addPolylines(
          data = (PER_SSC_GEO[PER_SSC_GEO$SSC_CODE16 == PER_SSC$active, ]),
          fillOpacity = 0,
          color = "red",
          opacity = 0.9,
          weight = 3,
          stroke = T,
          layerId = "GCC",
          options = leaflet::pathOptions(interactive = FALSE)
        )

      shiny::incProgress(3 / 3, detail = NA)
    })
  })

  # PER SSC Plots -----------------------------------------------------------



  dimensions <- shiny::reactiveValues()

  shiny::observe({

    dimensions$width <- function(){base::round(((input$width/1.5)*0.0104166), 2)}
    dimensions$height <- function(){base::round((input$height*0.0104166), 2)}
    #base::print(base::paste("is",input$width))
    #dimensions$fullWidth  <- function(){base::round((input$width), 2)}
    #dimensions$width <- base::round(((dimensions$fullWidth/1.5)*0.0104166), 2)
    dimensions$currentWidth  <- debounce(dimensions$width,1000)
    dimensions$currentHeight  <- debounce(dimensions$height,1000)
    #dimensions$currentFullWidth  <- debounce(dimensions$fullWidth,1000)
    dimensions$current <- c(dimensions$currentWidth(), dimensions$currentHeight())
    base::print(base::paste0("W: ",dimensions$current[1], ", H: ", dimensions$current[2], "in"))
  })

  shiny::observeEvent(input$PER_barInfo, {
    # Show a modal when the button is pressed
    shinyalert::shinyalert(
      imageUrl = 'www/legend.svg',
      imageWidth = 300,
      imageHeight = 400,
      closeOnClickOutside = TRUE,
      closeOnEsc = TRUE,
      size = 's'
      # imageWidth = 289.45,
      # imageHeight = 476.35
      )
  })

  shiny::observeEvent(input$PER_densityInfo, {
    # Show a modal when the button is pressed
    shinyalert::shinyalert(
      imageUrl = 'www/scatterLegend.png',
      imageWidth = 300,
      imageHeight = 250,
      closeOnClickOutside = TRUE,
      closeOnEsc = TRUE,
      size = 's'
      # imageWidth = 289.45,
      # imageHeight = 476.35
    )
  })



  # Output PER bar charts
  output$PER_barcharts <- ggiraph::renderGirafe({

    # Default PER bar chart data
    PER_SSC_bar_data <- filter_SSC(
      df = PER_SSC_DATA,
      uniqueID = PER_SSC_DATA$SSC_CODE16,
      clickID = PER_SSC$active
    )

    # Filters PER_SSC_bar_data for the vegetation-type bar chart
    PER_vegtype_bar_data <- filter_barchart(
      PER_SSC_bar_data,
      columnsToPlot = base::c("PerGrass", "PerShrub", "PerAnyTree", "PerNonVeg"),
      newNames = base::c("Grass", "Shrub", "Tree", "Non-veg"),
      order = base::c("Non-veg", "Grass", "Shrub", "Tree")
    )

    # Filters PER_SSC_pie_data for the land tenure bar chart
    PER_privpubl_bar_data <- filter_barchart(
      PER_SSC_bar_data,
      columnsToPlot = base::c("TrPriv", "TrPubl"),
      newNames = base::c("Private", "Public"),
      order = base::c("Private", "Public")
    )

    # Filters PER_SSC_bar_data for the land use bar chart
    PER_landuse_bar_data <- filter_barchart(PER_SSC_bar_data,
      columnsToPlot = base::c(
        "ArResPer", "ArParkPer", "ArInfrPer", "ArOthPer",
        "ArIndlPer", "ArEduPer", "ArCommPer", "ArHospPer",
        "ArTransPer", "ArWatPer", "ArPrimPPer"
      ),
      newNames = base::c(
        "Residential", "Parkland", "Infrastructure", "Other",
        "Industrial", "Education", "Commercial", "Hospital",
        "Transport", "Water", "Primary Production"
      ),
      order = base::c(
        "Primary Production", "Water", "Transport",
        "Hospital", "Commercial", "Education",
        "Industrial", "Other", "Infrastructure",
        "Parkland", "Residential"
      )
    )

    # Filters PER_SSC_bar_data for the TREE land use bar chart
    PER_treelanduse_bar_data <- filter_barchart(PER_SSC_bar_data,
      columnsToPlot = base::c(
        "TrResPer", "TrParkPer", "TrInfrPer", "TrOthPer",
        "TrIndlPer", "TrEduPer", "TrCommPer", "TrHospPer",
        "TrTransPer", "TrWatPer", "TrPrimPPer"
      ),
      newNames = base::c(
        "Residential", "Parkland", "Infrastructure", "Other",
        "Industrial", "Education", "Commercial", "Hospital",
        "Transport", "Water", "Primary Production"
      ),
      order = base::c(
        "Primary Production", "Water", "Transport",
        "Hospital", "Commercial", "Education",
        "Industrial", "Other", "Infrastructure",
        "Parkland", "Residential"
      )
    )

    # Plots bar charts
    landuseBarcharts(
      SSCname = PER_SSC$name,
      vegtypeData = PER_vegtype_bar_data,
      vegtypeVals = PER_vegtype_bar_data$percent,
      vegtypeGroups = PER_vegtype_bar_data$type,
      tenureData = PER_privpubl_bar_data,
      tenureVals = PER_privpubl_bar_data$percent,
      tenureGroups = PER_privpubl_bar_data$type,
      landuseData = PER_landuse_bar_data,
      landuseVals = PER_landuse_bar_data$percent,
      landuseGroups = PER_landuse_bar_data$type,
      treelanduseData = PER_treelanduse_bar_data,
      treelanduseVals = PER_treelanduse_bar_data$percent,
      treelanduseGroups = PER_treelanduse_bar_data$type,
      plotWidth = dimensions$current[1],
      plotHeight = dimensions$current[2]
     # windowWidth = dimensions$current[1]
    )
  })

  # Output PER density scatters
  output$PER_densityScatter <- ggiraph::renderGirafe({

    # Determines the density quintile to use based off selected type
    if (input$PER_dens == "GrDwDens") {
      densityQuint  <- "GrDenQuint"
    } else if (input$PER_dens == "UrbDwDens") {
      densityQuint  <- "UrbDenQuin"
    } else if (input$PER_dens == "ResDwDens") {
      densityQuint  <- "ResDenQuin"
    }

    # PER scatter chart data - PER_SSC$active
    PER_SSC_scatter_data <- filter_scatter(
      PER_SSC_DATA,
      columnsToPlot = base::c(
        "SSC_CODE16",
        "SSC_NAME16",
        "PerAnyTree",
        input$PER_dens,
        densityQuint
      ),
      uniqueID = PER_SSC$active
    )

    # Filters PER_SSC_scatter_data to just the selected suburb
    PER_SSC_scatter_selected_data <- scatter_selected_data(
      df = PER_SSC_scatter_data,
      uniqueID = "SSC_CODE16",
      activeID = PER_SSC$active
    )
    # Returns the selected suburb's quintile
    PER_SSC_scatter_selected_quint <- scatter_selected_quint(
      df = PER_SSC_scatter_selected_data,
      quintileNum = densityQuint
    )
    # Filters PER_SSC_scatter_data the above quintile, minus the selected suburb
    PER_SSC_scatter_quint_data <- scatter_quint_data(
      df = PER_SSC_scatter_data,
      quintileNum = densityQuint,
      selectedQuintile = PER_SSC_scatter_selected_quint,
      uniqueID = "SSC_CODE16",
      activeID = PER_SSC$active
    )
    # Filters PER_SSC_scatter_data to remaining suburbs in other quintiles
    PER_SSC_scatter_remaining_data <- scatter_remaining_data(
      df = PER_SSC_scatter_data,
      quintileNum = densityQuint,
      selectedQuintile = PER_SSC_scatter_selected_quint
    )
    # Plots scatter
    densityScatter(
      scatter_full_data = PER_SSC_scatter_data,
      scatter_selected_data = PER_SSC_scatter_selected_data,
      scatter_quint_data = PER_SSC_scatter_quint_data,
      scatter_remaining_data = PER_SSC_scatter_remaining_data,
      uniqueID = "SSC_CODE16",
      xAxis = input$PER_dens,
      yAxis = "PerAnyTree",
      city = "PER",
      structureName = "SSC_NAME16",
      plotWidth = dimensions$current[1],
      plotHeight = dimensions$current[2]
    )
  })

  # PER Active SSC ----------------------------------------------------------

  PER_map_SSC <- shiny::reactiveValues(click = base::vector(mode = "numeric"))
  PER_SSC <- shiny::reactiveValues(active = base::vector(mode = "numeric"))

  # Records clicked SSC ID from map and updates SelectizeInput
  shiny::observeEvent(input$PER_SSC_map_shape_click, {
    click <- shiny::isolate(input$PER_SSC_map_shape_click)
    shiny::isolate({
      PER_map_SSC$click <- click$id
    })
    shiny::updateSelectizeInput(
      inputId = "PER_SSC_dropdown",
      session = shiny::getDefaultReactiveDomain(),
      selected = (PER_SSC_DATA[PER_SSC_DATA[["SSC_CODE16"]] == PER_map_SSC$click, ]["SSC_NAME16"]),
      choices = PER_SSC_DATA$SSC_NAME1
    )
  })

  # Records clicked SSC ID from suburb comparison plot and updates SelectizeInput
  shiny::observeEvent(input$PER_densityScatter_selected, {
    shiny::updateSelectizeInput(
      inputId = "PER_SSC_dropdown",
      session = shiny::getDefaultReactiveDomain(),
      selected = (PER_SSC_DATA[PER_SSC_DATA[["SSC_CODE16"]] == input$PER_densityScatter_selected, ]["SSC_NAME16"]),
      choices = PER_SSC_DATA$SSC_NAME1
    )
  })

  # Updates PER_SSC$active from SelectizeInput
  shiny::observeEvent(input$PER_SSC_dropdown, {
    if(input$PER_SSC_dropdown == ""){
      base::return(NULL)
    }
    else {
      PER_SSC$active <- base::as.numeric((PER_SSC_DATA[PER_SSC_DATA[["SSC_NAME16"]] == input$PER_SSC_dropdown, ]["SSC_CODE16"]))
      PER_SSC$name  <- input$PER_SSC_dropdown
      base::print(
        base::paste0(
          PER_SSC$name,
          " (",
          PER_SSC$active,
          ")")
          )
    }
  })

  #  Updates map when PER_SSC$active changes e.g. from drop down or map click
  shiny::observeEvent(PER_SSC$active, {
    leaflet::leafletProxy("PER_SSC_map") %>%
      leaflet::addPolylines(
        data = (PER_SSC_GEO[PER_SSC_GEO$SSC_CODE16 == PER_SSC$active, ]),
        fillOpacity = 0,
        color = "red",
        opacity = 0.9,
        weight = 3,
        stroke = T,
        layerId = "GCC",
        options = leaflet::pathOptions(interactive = FALSE)
      )
  })


  # MEL -------------------------------------------------------------
 observeEvent(input$navbar, {
    shiny::req(input$navbar == "Melbourne")
    output$MEL_SSC_map <- leaflet::renderLeaflet({
      base_map()
    })
  })

  # Add MEL SSC polygons to MEL basemap when MEL tab is visible
  shiny::observeEvent(input$navbar, {
    shiny::req(input$navbar == "Melbourne")

    shiny::withProgress(message = NA, style = "old", value = 0, {
      shiny::incProgress(1 / 3, detail = NA)

      map_add_polys(
        df = MEL_SSC_GEO,
        mapID = "MEL_SSC_map",
        structureID = MEL_SSC_GEO$SSC_CODE16,
        structureName = MEL_SSC_GEO$SSC_NAME16,
        structureTree = MEL_SSC_GEO$PerAnyTree,
        structureShrub = MEL_SSC_GEO$PerGrass,
        structureGrass = MEL_SSC_GEO$PerShrub,
        minZoom = 8,
        maxZoom = 14,
        lng1 = 144.392,
        lat1 = -37.385,
        lng2 = 145.918,
        lat2 = -38.644,
        viewLng = 144.792,
        viewLat = -37.777,
        viewZoom = 10
      )
      shiny::incProgress(2 / 3, detail = NA)

      leaflet::leafletProxy("MEL_SSC_map") %>%
        leaflet::addPolylines(
          data = (MEL_SSC_GEO[MEL_SSC_GEO$SSC_CODE16 == MEL_SSC$active, ]),
          fillOpacity = 0,
          color = "red",
          opacity = 0.9,
          weight = 3,
          stroke = T,
          layerId = "GCC",
          options = leaflet::pathOptions(interactive = FALSE)
        )

      shiny::incProgress(3 / 3, detail = NA)
    })
  })

  # MEL SSC Plots -----------------------------------------------------------


  shiny::observeEvent(input$MEL_barInfo, {
    # Show a modal when the button is pressed
    shinyalert::shinyalert(
      imageUrl = 'www/legend.svg',
      imageWidth = 300,
      imageHeight = 400,
      closeOnClickOutside = TRUE,
      closeOnEsc = TRUE,
      size = 's'
      # imageWidth = 289.45,
      # imageHeight = 476.35
      )
  })

  shiny::observeEvent(input$MEL_densityInfo, {
    # Show a modal when the button is pressed
    shinyalert::shinyalert(
      imageUrl = 'www/scatterLegend.png',
      imageWidth = 300,
      imageHeight = 250,
      closeOnClickOutside = TRUE,
      closeOnEsc = TRUE,
      size = 's'
      # imageWidth = 289.45,
      # imageHeight = 476.35
      )
  })



  # Output MEL bar charts
  output$MEL_barcharts <- ggiraph::renderGirafe({

    # Default MEL bar chart data
    MEL_SSC_bar_data <- filter_SSC(
      df = MEL_SSC_DATA,
      uniqueID = MEL_SSC_DATA$SSC_CODE16,
      clickID = MEL_SSC$active
    )

    # Filters MEL_SSC_bar_data for the vegetation-type bar chart
    MEL_vegtype_bar_data <- filter_barchart(
      MEL_SSC_bar_data,
      columnsToPlot = base::c("PerGrass", "PerShrub", "PerAnyTree", "PerNonVeg"),
      newNames = base::c("Grass", "Shrub", "Tree", "Non-veg"),
      order = base::c("Non-veg", "Grass", "Shrub", "Tree")
    )

    # Filters MEL_SSC_pie_data for the land tenure bar chart
    MEL_privpubl_bar_data <- filter_barchart(
      MEL_SSC_bar_data,
      columnsToPlot = base::c("TrPriv", "TrPubl"),
      newNames = base::c("Private", "Public"),
      order = base::c("Private", "Public")
    )

    # Filters MEL_SSC_bar_data for the land use bar chart
    MEL_landuse_bar_data <- filter_barchart(MEL_SSC_bar_data,
      columnsToPlot = base::c(
        "ArResPer", "ArParkPer", "ArInfrPer", "ArOthPer",
        "ArIndlPer", "ArEduPer", "ArCommPer", "ArHospPer",
        "ArTransPer", "ArWatPer", "ArPrimPPer"
      ),
      newNames = base::c(
        "Residential", "Parkland", "Infrastructure", "Other",
        "Industrial", "Education", "Commercial", "Hospital",
        "Transport", "Water", "Primary Production"
      ),
      order = base::c(
        "Primary Production", "Water", "Transport",
        "Hospital", "Commercial", "Education",
        "Industrial", "Other", "Infrastructure",
        "Parkland", "Residential"
      )
    )

    # Filters MEL_SSC_bar_data for the TREE land use bar chart
    MEL_treelanduse_bar_data <- filter_barchart(MEL_SSC_bar_data,
      columnsToPlot = base::c(
        "TrResPer", "TrParkPer", "TrInfrPer", "TrOthPer",
        "TrIndlPer", "TrEduPer", "TrCommPer", "TrHospPer",
        "TrTransPer", "TrWatPer", "TrPrimPPer"
      ),
      newNames = base::c(
        "Residential", "Parkland", "Infrastructure", "Other",
        "Industrial", "Education", "Commercial", "Hospital",
        "Transport", "Water", "Primary Production"
      ),
      order = base::c(
        "Primary Production", "Water", "Transport",
        "Hospital", "Commercial", "Education",
        "Industrial", "Other", "Infrastructure",
        "Parkland", "Residential"
      )
    )

    # Plots bar charts
    landuseBarcharts(
      SSCname = MEL_SSC$name,
      vegtypeData = MEL_vegtype_bar_data,
      vegtypeVals = MEL_vegtype_bar_data$percent,
      vegtypeGroups = MEL_vegtype_bar_data$type,
      tenureData = MEL_privpubl_bar_data,
      tenureVals = MEL_privpubl_bar_data$percent,
      tenureGroups = MEL_privpubl_bar_data$type,
      landuseData = MEL_landuse_bar_data,
      landuseVals = MEL_landuse_bar_data$percent,
      landuseGroups = MEL_landuse_bar_data$type,
      treelanduseData = MEL_treelanduse_bar_data,
      treelanduseVals = MEL_treelanduse_bar_data$percent,
      treelanduseGroups = MEL_treelanduse_bar_data$type,
      plotWidth = dimensions$current[1],
      plotHeight = dimensions$current[2]
     # windowWidth = dimensions$current[1]
    )
  })

  # Output MEL density scatters
  output$MEL_densityScatter <- ggiraph::renderGirafe({
    # Determines the density quintile to use based off selected type
    if (input$MEL_dens == "GrDwDens") {
      densityQuint  <- "GrDenQuint"
    } else if (input$MEL_dens == "UrbDwDens") {
      densityQuint  <- "UrbDenQuin"
    } else if (input$MEL_dens == "ResDwDens") {
      densityQuint  <- "ResDenQuin"
    }

    # MEL scatter chart data - MEL_SSC$active
    MEL_SSC_scatter_data <- filter_scatter(
      MEL_SSC_DATA,
      columnsToPlot = base::c(
        "SSC_CODE16",
        "SSC_NAME16",
        "PerAnyTree",
        input$MEL_dens,
        densityQuint
      ),
      uniqueID = MEL_SSC$active
    )

    # Filters MEL_SSC_scatter_data to just the selected suburb
    MEL_SSC_scatter_selected_data <- scatter_selected_data(
      df = MEL_SSC_scatter_data,
      uniqueID = "SSC_CODE16",
      activeID = MEL_SSC$active
    )
    # Returns the selected suburb's quintile
    MEL_SSC_scatter_selected_quint <- scatter_selected_quint(
      df = MEL_SSC_scatter_selected_data,
      quintileNum = densityQuint
    )
    # Filters MEL_SSC_scatter_data the above quintile, minus the selected suburb
    MEL_SSC_scatter_quint_data <- scatter_quint_data(
      df = MEL_SSC_scatter_data,
      quintileNum = densityQuint,
      selectedQuintile = MEL_SSC_scatter_selected_quint,
      uniqueID = "SSC_CODE16",
      activeID = MEL_SSC$active
    )
    # Filters MEL_SSC_scatter_data to remaining suburbs in other quintiles
    MEL_SSC_scatter_remaining_data <- scatter_remaining_data(
      df = MEL_SSC_scatter_data,
      quintileNum = densityQuint,
      selectedQuintile = MEL_SSC_scatter_selected_quint
    )
    # Plots scatter
    densityScatter(
      scatter_full_data = MEL_SSC_scatter_data,
      scatter_selected_data = MEL_SSC_scatter_selected_data,
      scatter_quint_data = MEL_SSC_scatter_quint_data,
      scatter_remaining_data = MEL_SSC_scatter_remaining_data,
      uniqueID = "SSC_CODE16",
      xAxis = input$MEL_dens,
      yAxis = "PerAnyTree",
      city = "MEL",
      structureName = "SSC_NAME16",
      plotWidth = dimensions$current[1],
      plotHeight = dimensions$current[2]
    )
  })

  # MEL Active SSC ----------------------------------------------------------

  MEL_map_SSC <- shiny::reactiveValues(click = base::vector(mode = "numeric"))
  MEL_SSC <- shiny::reactiveValues(active = base::vector(mode = "numeric"))

  # Records clicked SSC ID from map and updates SelectizeInput
  shiny::observeEvent(input$MEL_SSC_map_shape_click, {
    click <- shiny::isolate(input$MEL_SSC_map_shape_click)
    shiny::isolate({
      MEL_map_SSC$click <- click$id
    })
    shiny::updateSelectizeInput(
      inputId = "MEL_SSC_dropdown",
      session = shiny::getDefaultReactiveDomain(),
      selected = (MEL_SSC_DATA[MEL_SSC_DATA[["SSC_CODE16"]] == MEL_map_SSC$click, ]["SSC_NAME16"]),
      choices = MEL_SSC_DATA$SSC_NAME1
    )
  })

  # Records clicked SSC ID from suburb comparison plot and updates SelectizeInput
  shiny::observeEvent(input$MEL_densityScatter_selected, {
    shiny::updateSelectizeInput(
      inputId = "MEL_SSC_dropdown",
      session = shiny::getDefaultReactiveDomain(),
      selected = (MEL_SSC_DATA[MEL_SSC_DATA[["SSC_CODE16"]] == input$MEL_densityScatter_selected, ]["SSC_NAME16"]),
      choices = MEL_SSC_DATA$SSC_NAME1
    )
  })

  # Updates MEL_SSC$active from SelectizeInput
  shiny::observeEvent(input$MEL_SSC_dropdown, {
    if(input$MEL_SSC_dropdown == ""){
      base::return(NULL)
    }
    else {
      MEL_SSC$active <- base::as.numeric((MEL_SSC_DATA[MEL_SSC_DATA[["SSC_NAME16"]] == input$MEL_SSC_dropdown, ]["SSC_CODE16"]))
      MEL_SSC$name  <- input$MEL_SSC_dropdown
      base::print(
        base::paste0(
          MEL_SSC$name,
          " (",
          MEL_SSC$active,
          ")")
          )
    }
  })

  #  Updates map when MEL_SSC$active changes e.g. from drop down or map click
  shiny::observeEvent(MEL_SSC$active, {
    leaflet::leafletProxy("MEL_SSC_map") %>%
      leaflet::addPolylines(
        data = (MEL_SSC_GEO[MEL_SSC_GEO$SSC_CODE16 == MEL_SSC$active, ]),
        fillOpacity = 0,
        color = "red",
        opacity = 0.9,
        weight = 3,
        stroke = T,
        layerId = "GCC",
        options = leaflet::pathOptions(interactive = FALSE)
      )
  })

  # SYD -------------------------------------------------------------
  observeEvent(input$navbar, {
    shiny::req(input$navbar == "Sydney")
    output$SYD_SSC_map <- leaflet::renderLeaflet({
      base_map()
    })
  })
  
  # Add SYD SSC polygons to SYD basemap when SYD tab is visible
  shiny::observeEvent(input$navbar, {
    shiny::req(input$navbar == "Sydney")
    
    shiny::withProgress(message = NA, style = "old", value = 0, {
      shiny::incProgress(1 / 3, detail = NA)
      
      map_add_polys(
        df = SYD_SSC_GEO,
        mapID = "SYD_SSC_map",
        structureID = SYD_SSC_GEO$SSC_CODE16,
        structureName = SYD_SSC_GEO$SSC_NAME16,
        structureTree = SYD_SSC_GEO$PerAnyTree,
        structureShrub = SYD_SSC_GEO$PerGrass,
        structureGrass = SYD_SSC_GEO$PerShrub,
        minZoom = 8,
        maxZoom = 14,
        lng1 =  149.664,
        lat1 = -32.325,
        lng2 = 152.272,
        lat2 = -34.857,
        viewLng = 150.894,
        viewLat = -33.798, 
        viewZoom = 10
      )
      
      
      shiny::incProgress(2 / 3, detail = NA)
      
      leaflet::leafletProxy("SYD_SSC_map") %>%
        leaflet::addPolylines(
          data = (SYD_SSC_GEO[SYD_SSC_GEO$SSC_CODE16 == SYD_SSC$active, ]),
          fillOpacity = 0,
          color = "red",
          opacity = 0.9,
          weight = 3,
          stroke = T,
          layerId = "GCC",
          options = leaflet::pathOptions(interactive = FALSE)
        )
      
      shiny::incProgress(3 / 3, detail = NA)
    })
  })
  
  # SYD SSC Plots -----------------------------------------------------------
  
  shiny::observeEvent(input$SYD_barInfo, {
    # Show a modal when the button is pressed
    shinyalert::shinyalert(
      imageUrl = 'www/legend.svg',
      imageWidth = 300,
      imageHeight = 400,
      closeOnClickOutside = TRUE,
      closeOnEsc = TRUE,
      size = 's'
      # imageWidth = 289.45,
      # imageHeight = 476.35
    )
  })
  
  shiny::observeEvent(input$SYD_densityInfo, {
    # Show a modal when the button is pressed
    shinyalert::shinyalert(
      imageUrl = 'www/scatterLegend.png',
      imageWidth = 300,
      imageHeight = 250,
      closeOnClickOutside = TRUE,
      closeOnEsc = TRUE,
      size = 's'
      # imageWidth = 289.45,
      # imageHeight = 476.35
    )
  })
  
  
  
  # Output SYD bar charts
  output$SYD_barcharts <- ggiraph::renderGirafe({
    
    # Default SYD bar chart data
    SYD_SSC_bar_data <- filter_SSC(
      df = SYD_SSC_DATA,
      uniqueID = SYD_SSC_DATA$SSC_CODE16,
      clickID = SYD_SSC$active
    )
    
    # Filters SYD_SSC_bar_data for the vegetation-type bar chart
    SYD_vegtype_bar_data <- filter_barchart(
      SYD_SSC_bar_data,
      columnsToPlot = base::c("PerGrass", "PerShrub", "PerAnyTree", "PerNonVeg"),
      newNames = base::c("Grass", "Shrub", "Tree", "Non-veg"),
      order = base::c("Non-veg", "Grass", "Shrub", "Tree")
    )
    
    # Filters SYD_SSC_pie_data for the land tenure bar chart
    SYD_privpubl_bar_data <- filter_barchart(
      SYD_SSC_bar_data,
      columnsToPlot = base::c("TrPriv", "TrPubl"),
      newNames = base::c("Private", "Public"),
      order = base::c("Private", "Public")
    )
    
    # Filters SYD_SSC_bar_data for the land use bar chart
    SYD_landuse_bar_data <- filter_barchart(SYD_SSC_bar_data,
                                            columnsToPlot = base::c(
                                              "ArResPer", "ArParkPer", "ArInfrPer", "ArOthPer",
                                              "ArIndlPer", "ArEduPer", "ArCommPer", "ArHospPer",
                                              "ArTransPer", "ArWatPer", "ArPrimPPer"
                                            ),
                                            newNames = base::c(
                                              "Residential", "Parkland", "Infrastructure", "Other",
                                              "Industrial", "Education", "Commercial", "Hospital",
                                              "Transport", "Water", "Primary Production"
                                            ),
                                            order = base::c(
                                              "Primary Production", "Water", "Transport",
                                              "Hospital", "Commercial", "Education",
                                              "Industrial", "Other", "Infrastructure",
                                              "Parkland", "Residential"
                                            )
    )
    
    # Filters SYD_SSC_bar_data for the TREE land use bar chart
    SYD_treelanduse_bar_data <- filter_barchart(SYD_SSC_bar_data,
                                                columnsToPlot = base::c(
                                                  "TrResPer", "TrParkPer", "TrInfrPer", "TrOthPer",
                                                  "TrIndlPer", "TrEduPer", "TrCommPer", "TrHospPer",
                                                  "TrTransPer", "TrWatPer", "TrPrimPPer"
                                                ),
                                                newNames = base::c(
                                                  "Residential", "Parkland", "Infrastructure", "Other",
                                                  "Industrial", "Education", "Commercial", "Hospital",
                                                  "Transport", "Water", "Primary Production"
                                                ),
                                                order = base::c(
                                                  "Primary Production", "Water", "Transport",
                                                  "Hospital", "Commercial", "Education",
                                                  "Industrial", "Other", "Infrastructure",
                                                  "Parkland", "Residential"
                                                )
    )
    
    # Plots bar charts
    landuseBarcharts(
      SSCname = SYD_SSC$name,
      vegtypeData = SYD_vegtype_bar_data,
      vegtypeVals = SYD_vegtype_bar_data$percent,
      vegtypeGroups = SYD_vegtype_bar_data$type,
      tenureData = SYD_privpubl_bar_data,
      tenureVals = SYD_privpubl_bar_data$percent,
      tenureGroups = SYD_privpubl_bar_data$type,
      landuseData = SYD_landuse_bar_data,
      landuseVals = SYD_landuse_bar_data$percent,
      landuseGroups = SYD_landuse_bar_data$type,
      treelanduseData = SYD_treelanduse_bar_data,
      treelanduseVals = SYD_treelanduse_bar_data$percent,
      treelanduseGroups = SYD_treelanduse_bar_data$type,
      plotWidth = dimensions$current[1],
      plotHeight = dimensions$current[2]
      # windowWidth = dimensions$current[1]
    )
  })
  
  # Output SYD density scatters
  output$SYD_densityScatter <- ggiraph::renderGirafe({
    
    # Determines the density quintile to use based off selected type
    if (input$SYD_dens == "GrDwDens") {
      densityQuint  <- "GrDenQuint"
    } else if (input$SYD_dens == "UrbDwDens") {
      densityQuint  <- "UrbDenQuin"
    } else if (input$SYD_dens == "ResDwDens") {
      densityQuint  <- "ResDenQuin"
    }
   # browser()
    # SYD scatter chart data - SYD_SSC$active
    SYD_SSC_scatter_data <- filter_scatter(
      SYD_SSC_DATA,
      columnsToPlot = base::c(
        "SSC_CODE16",
        "SSC_NAME16",
        "PerAnyTree",
        input$SYD_dens,
        densityQuint
      ),
      uniqueID = SYD_SSC$active
    )
    
    # Filters SYD_SSC_scatter_data to just the selected suburb
    SYD_SSC_scatter_selected_data <- scatter_selected_data(
      df = SYD_SSC_scatter_data,
      uniqueID = "SSC_CODE16",
      activeID = SYD_SSC$active
    )
    # Returns the selected suburb's quintile
    SYD_SSC_scatter_selected_quint <- scatter_selected_quint(
      df = SYD_SSC_scatter_selected_data,
      quintileNum = densityQuint
    )
    # Filters SYD_SSC_scatter_data the above quintile, minus the selected suburb
    SYD_SSC_scatter_quint_data <- scatter_quint_data(
      df = SYD_SSC_scatter_data,
      quintileNum = densityQuint,
      selectedQuintile = SYD_SSC_scatter_selected_quint,
      uniqueID = "SSC_CODE16",
      activeID = SYD_SSC$active
    )
    # Filters SYD_SSC_scatter_data to remaining suburbs in other quintiles
    SYD_SSC_scatter_remaining_data <- scatter_remaining_data(
      df = SYD_SSC_scatter_data,
      quintileNum = densityQuint,
      selectedQuintile = SYD_SSC_scatter_selected_quint
    )
    # Plots scatter
    densityScatter(
      scatter_full_data = SYD_SSC_scatter_data,
      scatter_selected_data = SYD_SSC_scatter_selected_data,
      scatter_quint_data = SYD_SSC_scatter_quint_data,
      scatter_remaining_data = SYD_SSC_scatter_remaining_data,
      uniqueID = "SSC_CODE16",
      xAxis = input$SYD_dens,
      yAxis = "PerAnyTree",
      city = "SYD",
      structureName = "SSC_NAME16",
      plotWidth = dimensions$current[1],
      plotHeight = dimensions$current[2]
    )
  })
  
  # SYD Active SSC ----------------------------------------------------------
  
  SYD_map_SSC <- shiny::reactiveValues(click = base::vector(mode = "numeric"))
  SYD_SSC <- shiny::reactiveValues(active = base::vector(mode = "numeric"))
  
  # Records clicked SSC ID from map and updates SelectizeInput
  shiny::observeEvent(input$SYD_SSC_map_shape_click, {
    click <- shiny::isolate(input$SYD_SSC_map_shape_click)
    shiny::isolate({
      SYD_map_SSC$click <- click$id
    })
    shiny::updateSelectizeInput(
      inputId = "SYD_SSC_dropdown",
      session = shiny::getDefaultReactiveDomain(),
      selected = (SYD_SSC_DATA[SYD_SSC_DATA[["SSC_CODE16"]] == SYD_map_SSC$click, ]["SSC_NAME16"]),
      choices = SYD_SSC_DATA$SSC_NAME1
    )
  })
  
  # Records clicked SSC ID from suburb comparison plot and updates SelectizeInput
  shiny::observeEvent(input$SYD_densityScatter_selected, {
    shiny::updateSelectizeInput(
      inputId = "SYD_SSC_dropdown",
      session = shiny::getDefaultReactiveDomain(),
      selected = (SYD_SSC_DATA[SYD_SSC_DATA[["SSC_CODE16"]] == input$SYD_densityScatter_selected, ]["SSC_NAME16"]),
      choices = SYD_SSC_DATA$SSC_NAME1
    )
  })
  
  # Updates SYD_SSC$active from SelectizeInput
  shiny::observeEvent(input$SYD_SSC_dropdown, {
    if(input$SYD_SSC_dropdown == ""){
      base::return(NULL)
    }
    else {
      SYD_SSC$active <- base::as.numeric((SYD_SSC_DATA[SYD_SSC_DATA[["SSC_NAME16"]] == input$SYD_SSC_dropdown, ]["SSC_CODE16"]))
      SYD_SSC$name  <- input$SYD_SSC_dropdown
      base::print(
        base::paste0(
          SYD_SSC$name,
          " (",
          SYD_SSC$active,
          ")")
      )
    }
  })
  
  #  Updates map when SYD_SSC$active changes e.g. from drop down or map click
  shiny::observeEvent(SYD_SSC$active, {
    leaflet::leafletProxy("SYD_SSC_map") %>%
      leaflet::addPolylines(
        data = (SYD_SSC_GEO[SYD_SSC_GEO$SSC_CODE16 == SYD_SSC$active, ]),
        fillOpacity = 0,
        color = "red",
        opacity = 0.9,
        weight = 3,
        stroke = T,
        layerId = "GCC",
        options = leaflet::pathOptions(interactive = FALSE)
      )
  })
  # Compare ---------------------------------------------------------

  SSC_compare <- shiny::reactiveValues(active = base::vector(mode = "numeric"))
  compareTab <- shiny::reactiveValues()
  
  observeEvent(input$PER_SSC_compare_dropdown,{
    if(input$PER_SSC_compare_dropdown == ""){
      SSC_compare$PER <- NULL
    }
    else {
      SSC_compare$PER <- base::as.numeric(PER_SSC_DATA[PER_SSC_DATA[["SSC_NAME16"]] == input$PER_SSC_compare_dropdown, ]["SSC_CODE16"])
      print(SSC_compare$PER)
      compareTab$PER <- data.frame(
        Suburb = c(SSCnameTab(df = PER_SSC_DATA, id = SSC_compare$PER)),
        Tree = c(SSCtreeTab(df = PER_SSC_DATA, id = SSC_compare$PER)),
        localRank = c(SSCrankTab(df = PER_SSC_DATA, id = SSC_compare$PER)),
        globalRank = c(SSCrankTab(df = ALL_SSC_DATA, id = SSC_compare$PER))
      )
    }
  })
  observeEvent(input$MEL_SSC_compare_dropdown,{
    if(input$MEL_SSC_compare_dropdown == ""){
      SSC_compare$MEL <- NULL
    }
    else {
      SSC_compare$MEL <- base::as.numeric(MEL_SSC_DATA[MEL_SSC_DATA[["SSC_NAME16"]] == input$MEL_SSC_compare_dropdown, ]["SSC_CODE16"])
      print(SSC_compare$MEL)
      compareTab$MEL <- data.frame(
        Suburb = c(SSCnameTab(df = MEL_SSC_DATA, id = SSC_compare$MEL)),
        Tree = c(SSCtreeTab(df = MEL_SSC_DATA, id = SSC_compare$MEL)),
        localRank = c(SSCrankTab(df = MEL_SSC_DATA, id = SSC_compare$MEL)),
        globalRank = c(SSCrankTab(df = ALL_SSC_DATA, id = SSC_compare$MEL))
      )
    }
  })
  observeEvent(input$SYD_SSC_compare_dropdown,{
    if(input$SYD_SSC_compare_dropdown == ""){
      SSC_compare$SYD <- NULL
    }
    else {
      SSC_compare$SYD <- base::as.numeric(SYD_SSC_DATA[SYD_SSC_DATA[["SSC_NAME16"]] == input$SYD_SSC_compare_dropdown, ]["SSC_CODE16"])
      print(SSC_compare$SYD)
      compareTab$SYD <- data.frame(
        Suburb = c(SSCnameTab(df = SYD_SSC_DATA, id = SSC_compare$SYD)),
        Tree = c(SSCtreeTab(df = SYD_SSC_DATA, id = SSC_compare$SYD)),
        localRank = c(SSCrankTab(df = SYD_SSC_DATA, id = SSC_compare$SYD)),
        globalRank = c(SSCrankTab(df = ALL_SSC_DATA, id = SSC_compare$SYD))
      )
    }
  })

  output$densityPlot <- renderPlot({
    densityPlot <- ggplot2::ggplot(
    ) +
      densityGeomSwitch(
        switch = input$PER_SSC_compare_check,
        densityVals = PER_SSC_DATA$PerAnyTree,
        df = PER_SSC_DATA,
        GCC = 'PER',
        colour = "#66c2a5"
      ) +
      densityGeomSwitch(
        switch = input$MEL_SSC_compare_check,
        densityVals = MEL_SSC_DATA$PerAnyTree,
        df = MEL_SSC_DATA,
        GCC = 'MEL',
        colour = "#fc8d62"
      ) +
      densityGeomSwitch(
        switch = input$SYD_SSC_compare_check,
        densityVals = SYD_SSC_DATA$PerAnyTree,
        df = SYD_SSC_DATA,
        GCC = 'SYD',
        colour = "#8da0cb"
      ) +
      SSClineGeomSwitch(
        switch = input$PER_SSC_compare_SSC_check,
        df = PER_SSC_DATA,
        colour = "#66c2a5",
        selectedSSC = SSC_compare$PER
      ) +
      SSClineGeomSwitch(
        switch = input$MEL_SSC_compare_SSC_check,
        df = MEL_SSC_DATA,
        colour = "#fc8d62",
        selectedSSC = SSC_compare$MEL
      ) +
      SSClineGeomSwitch(
        switch = input$SYD_SSC_compare_SSC_check,
        df = SYD_SSC_DATA,
        colour = "#8da0cb",
        selectedSSC = SSC_compare$SYD
      ) +
      ggplot2::labs(
        y = "Density",
        x = "Tree canopy cover"
      ) +
      ggplot2::scale_y_continuous(
        expand = c(0, 0.0),
        limits = c(0, 0.08)
      ) +
      ggplot2::scale_x_continuous(
        label = scales::percent_format(accuracy = 1, scale = 1),
        expand = c(0, 0),
        limits = c(0, 95)
      ) +
      ggplot2::theme_light(

      )
    densityPlot
  })
  
  # table <- base::rbind(compareTab$PER,compareTab$MEL,compareTab$SYD)
  # 
  # 
  # output$compareTable <- renderText({
  #   table %>%
  #     kableExtra::kbl(
  #       col.names = c("Suburb", 'Tree canopy cover', "Rank within city", "Rank between cities")
  #     ) %>%
  #     kableExtra::kable_styling()
  # })
  output$compareTable <- function() {
    table <- base::rbind(compareTab$PER,compareTab$MEL,compareTab$SYD)
    table %>%
      kableExtra::kbl(
        col.names = c("Suburb", 'Tree canopy cover', "Rank within city", "Rank between cities")
      ) %>%
      kableExtra::kable_styling()
  }
  
  
  
}

