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
    shinyalert::useShinyalert(),
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
        shiny::HTML('<meta name="viewport" content="width=device-width, initial-scale=1">'),
        tags$div(id = 'error-message',
                 NULL,
                 "Please enlarge your window"
        ),
        shiny::sidebarLayout(
          shiny::sidebarPanel(
            width = 5,
            tags$div(class = "Suburb", checked = NA,
              tags$p("Selected suburb: ")
            ),
          #"Selected suburb:",
            shiny::selectizeInput(
              "PER_SSC_dropdown",
              label = NULL, #"Selected suburb: ",
              width = '50%',
              choices = PER_SSC_DATA$SSC_NAME16,
              selected = "Perth (WA)"
            ),
            leaflet::leafletOutput(
              outputId = "PER_SSC_map",
              height = "calc(100vh - 129px)"
            )
          ),
          shiny::mainPanel(
            width = 7,
            shiny::tabsetPanel(
              type = "tabs",
              shiny::tabPanel(
                "Suburb land-use",
                shiny::actionButton("PER_barInfo", "LEGEND"),
                ggiraph::girafeOutput("PER_barcharts",
                  height = "calc(82vh - 5px)"
                )
              ),
              shiny::tabPanel(
                "Suburb comparison",
                shiny::actionButton("PER_densityInfo", "LEGEND"),
                tags$div(class = "densitySelector", checked = NA,
                tags$p("Density type: "),

                shiny::selectInput("PER_dens",
                  label = NULL,
                  base::c(
                    "Gross density" = "GrDwDens",
                    "Urban dwelling density" = "UrbDwDens",
                    "Residential dwelling density" = "ResDwDens"
                  )
                )),
                ggiraph::girafeOutput("PER_densityScatter",
                  height ="calc(82vh - 5px)"
                )
              )
              #shiny::tabPanel("City comparison")
            )
          )
        )
      ),
      shiny::tabPanel(
        "Melbourne",
        shiny::HTML('<meta name="viewport" content="width=device-width, initial-scale=1">'),
        tags$div(id = 'error-message',
                 NULL,
                 "Please enlarge your window"
        ),
        shiny::sidebarLayout(
          shiny::sidebarPanel(
            width = 5,
            tags$div(class = "Suburb", checked = NA,
              tags$p("Selected suburb: ")
            ),
          #"Selected suburb:",
            shiny::selectizeInput(
              "MEL_SSC_dropdown",
              label = NULL, #"Selected suburb: ",
              width = '50%',
              choices = MEL_SSC_DATA$SSC_NAME16,
              selected = "Melbourne"
            ),
            leaflet::leafletOutput(
              outputId = "MEL_SSC_map",
              height = "calc(100vh - 129px)"
            )
          ),
          shiny::mainPanel(
            width = 7,
            shiny::tabsetPanel(
              type = "tabs",
              shiny::tabPanel(
                "Suburb land-use",
                shiny::actionButton("MEL_barInfo", "LEGEND"),
                ggiraph::girafeOutput("MEL_barcharts",
                  height = "calc(82vh - 5px)"
                )
              ),
              shiny::tabPanel(
                "Suburb comparison",
                shiny::actionButton("MEL_densityInfo", "LEGEND"),
                tags$div(class = "densitySelector", checked = NA,
                tags$p("Density type: "),

                shiny::selectInput("MEL_dens",
                  label = NULL,
                  base::c(
                    "Gross density" = "GrDwDens",
                    "Urban dwelling density" = "UrbDwDens",
                    "Residential dwelling density" = "ResDwDens"
                  )
                )),
                ggiraph::girafeOutput("MEL_densityScatter",
                  height ="calc(82vh - 5px)"
                )
              )
              #shiny::tabPanel("City comparison")
            )
          )
        )
      ),
      shiny::tabPanel(
        "Sydney",
        shiny::HTML('<meta name="viewport" content="width=device-width, initial-scale=1">'),
        tags$div(id = 'error-message',
                 NULL,
                 "Please enlarge your window"
        ),
        shiny::sidebarLayout(
          shiny::sidebarPanel(
            width = 5,
            tags$div(class = "Suburb", checked = NA,
                     tags$p("Selected suburb: ")
            ),
            #"Selected suburb:",
            shiny::selectizeInput(
              "SYD_SSC_dropdown",
              label = NULL, #"Selected suburb: ",
              width = '50%',
              choices = SYD_SSC_DATA$SSC_NAME16,
              selected = "Sydney"
            ),
            leaflet::leafletOutput(
              outputId = "SYD_SSC_map",
              height = "calc(100vh - 129px)"
            )
          ),
          shiny::mainPanel(
            width = 7,
            shiny::tabsetPanel(
              type = "tabs",
              shiny::tabPanel(
                "Suburb land-use",
                shiny::actionButton("SYD_barInfo", "LEGEND"),
                ggiraph::girafeOutput("SYD_barcharts",
                                      height = "calc(82vh - 5px)"
                )
              ),
              shiny::tabPanel(
                "Suburb comparison",
                shiny::actionButton("SYD_densityInfo", "LEGEND"),
                tags$div(class = "densitySelector", checked = NA,
                         tags$p("Density type: "),
                        
                         shiny::selectInput("SYD_dens",
                                            label = NULL,
                                            base::c(
                                              "Gross density" = "GrDwDens",
                                              "Urban dwelling density" = "UrbDwDens",
                                              "Residential dwelling density" = "ResDwDens"
                                            )
                         )),
                ggiraph::girafeOutput("SYD_densityScatter",
                                      height ="calc(82vh - 5px)"
                )
              )
              #shiny::tabPanel("City comparison")
            )
          )
        )
      ),
      shiny::tabPanel(
        "Compare",
        sidebarLayout(
          sidebarPanel(
            
            width = 4,
            tags$div(class = "compare", checked = NA,
            h3('Perth'),
            checkboxInput("PER_SSC_compare_check", label = "Plot Perth distribution", value = FALSE, width = NULL),
            checkboxInput("PER_SSC_compare_SSC_check", label = "Plot a Perth suburb", value = FALSE, width = NULL),
            conditionalPanel(
              condition = "input.PER_SSC_compare_SSC_check == true",
              shiny::selectizeInput(
                "PER_SSC_compare_dropdown",
                label = NULL,
                choices = PER_SSC_DATA$SSC_NAME16,
                width = '50%',
                selected = NULL,
                options = list(
                  placeholder = "Please select an option below",
                  onInitialize = base::I('function() { this.setValue(""); }')
                )
              )
            ),
            br(),
            h3('Melbourne'),
            checkboxInput("MEL_SSC_compare_check", label = "Plot Melbourne", value = FALSE, width = NULL),
            checkboxInput("MEL_SSC_compare_SSC_check", label = "Plot a Melbourne suburb", value = FALSE, width = NULL),
            conditionalPanel(
              condition = "input.MEL_SSC_compare_SSC_check == true",
              shiny::selectizeInput(
                "MEL_SSC_compare_dropdown",
                label = NULL,
                choices = MEL_SSC_DATA$SSC_NAME16,
                width = '50%',
                selected = NULL,
                options = list(
                  placeholder = "Please select an option below",
                  onInitialize = base::I('function() { this.setValue(""); }')
                )
              )
            ),
            br(),
            h3('Sydney'),
            checkboxInput("SYD_SSC_compare_check", label = "Plot Sydney", value = FALSE, width = NULL),
            checkboxInput("SYD_SSC_compare_SSC_check", label = "Plot a Sydney suburb", value = FALSE, width = NULL),
            conditionalPanel(
              condition = "input.SYD_SSC_compare_SSC_check == true",
              shiny::selectizeInput(
                "SYD_SSC_compare_dropdown",
                label = NULL,
                choices = SYD_SSC_DATA$SSC_NAME16,
                width = '50%',
                selected = NULL,
                options = list(
                  placeholder = "Please select an option below",
                  onInitialize = base::I('function() { this.setValue(""); }')
                )
              )
            ),
          )
          ),
          
          mainPanel(
            width = 8,
            plotOutput("densityPlot"),
            htmlOutput("compareTable") 
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