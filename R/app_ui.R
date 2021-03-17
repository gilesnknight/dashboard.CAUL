#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny leaflet
#' @noRd
app_ui <- function(request) {
  tab <- function(...) {
    shiny::tabPanel(..., class = "p-3 border border-top-0 rounded-bottom")
  }

  shiny::tagList(
    golem_add_external_resources(),
    tags$head(
        tags$script('
                        var width = 0;
                        $(document).on("shiny:connected", function(e) {
                          width = window.innerWidth;
                          Shiny.onInputChange("width", width);
                        });
                        $(window).resize(function(e) {
                          width = window.innerWidth;
                          Shiny.onInputChange("width", width);
                        });
                        '),
        tags$script('
                        var height = 0;
                        $(document).on("shiny:connected", function(e) {
                          height = window.innerHeight;
                          Shiny.onInputChange("height", height);
                        });
                        $(window).resize(function(e) {
                          height = window.innerHeight;
                          Shiny.onInputChange("height", height);
                        });
                        ')
      ),
    shiny::navbarPage(
      windowTitle = "CAUL Dashboard",
      id = "navbar",
      title = shiny::span(
        shiny::img(
          src = "www/logo.png",
          width = 100,
          height = 40,
          style = "position:relative; top:-7px; left:-6px"
        )
      ),
      shiny::tabPanel(
        title = "Home",
        icon = shiny::icon("home"),
        shiny::fillPage(shiny::titlePanel("Home"))
      ),
      shiny::tabPanel(
        "Perth",
        shiny::sidebarLayout(
          shiny::sidebarPanel(
            width = 5,
            shiny::selectizeInput(
              "PER_SSC_dropdown",
              label = "Selected suburb: ", choices = PER_SSC_DATA$SSC_NAME16,
              selected = "Perth (WA)"
            ),
            leaflet::leafletOutput(
              outputId = "PER_SSC_map",
              height = "calc(50vh)"
            )
          ),
          shiny::mainPanel(
            width = 7,
            shiny::tabsetPanel(
              type = "tabs",
              shiny::tabPanel(
                "Land-use",
                ggiraph::girafeOutput("PER_barcharts",
                  height = "calc(82vh - 5px)"
                )
              ),
              shiny::tabPanel(
                "Suburb comparison",
                shiny::selectInput("PER_dens",
                  label = "Select density type:",
                  base::c(
                    "Gross density" = "GrDwDens",
                    "Urban dwelling density" = "UrbDwDens",
                    "Residential dwelling density" = "ResDwDens"
                  )
                ),
                ggiraph::girafeOutput("PER_densityScatter",
                  height = "calc(80.1vh - 20px)"
                )
              ),
              shiny::tabPanel("City comparison")
            )
          )
        )
      ),
      shiny::tabPanel(
        "Melbourne",
        shiny::sidebarLayout(
          shiny::sidebarPanel(
            width = 5,
            shiny::selectizeInput(
              "MEL_SSC_dropdown",
              label = "Selected suburb: ", choices = MEL_SSC_DATA$SSC_NAME16,
              selected = "Melbourne"
            ),
            leaflet::leafletOutput(
              outputId = "MEL_SSC_map",
              height = "calc(70vh - 10px)"
            )
          ),
          shiny::mainPanel(
            width = 7,
            shiny::tabsetPanel(
              type = "tabs",
              shiny::tabPanel(
                "Land-use",
                ggiraph::girafeOutput("MEL_barcharts",
                  height = "calc(97vh - 90px)"
                )
              ),
              shiny::tabPanel(
                "Suburb comparison",
                shiny::selectInput("MEL_dens",
                  label = "Select density type:",
                  base::c(
                    "Gross density" = "GrDwDens",
                    "Urban dwelling density" = "UrbDwDens",
                    "Residential dwelling density" = "ResDwDens"
                  )
                ),
                ggiraph::girafeOutput("MEL_densityScatter",
                  height = "calc(80.1vh - 20px)"
                )
              ),
              shiny::tabPanel("City comparison")
            )
          )
        )
      ),
      shiny::tabPanel(
        "Sydney",
        shiny::sidebarLayout(
          shiny::sidebarPanel(
            width = 5,
            shiny::selectizeInput(
              "SYD_SSC_dropdown",
              label = "Selected suburb: ", choices = SYD_SSC_DATA$SSC_NAME16,
              selected = "Sydney"
            ),
            leaflet::leafletOutput(
              outputId = "SYD_SSC_map",
              height = "calc(70vh - 10px)"
            )
          ),
          shiny::mainPanel(
            width = 7,
            shiny::tabsetPanel(
              type = "tabs",
              shiny::tabPanel(
                "Land-use",
                ggiraph::girafeOutput("SYD_barcharts",
                  height = "calc(97vh - 90px)"
                )
              ),
              shiny::tabPanel(
                "Suburb comparison",
                shiny::selectInput("SYD_dens",
                  label = "Select density type:",
                  base::c(
                    "Gross density" = "GrDwDens",
                    "Urban dwelling density" = "UrbDwDens",
                    "Residential dwelling density" = "ResDwDens"
                  )
                ),
                ggiraph::girafeOutput("SYD_densityScatter",
                  height = "calc(80.1vh - 20px)"
                )
              ),
              shiny::tabPanel("City comparison")
            )
          )
        )
      ),
      shiny::tabPanel(
        title = "About",
        icon = icon("info-circle"),
        shiny::fillPage(shiny::titlePanel("About the dashboard"))
      )
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www", app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "caul.dashboard"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}