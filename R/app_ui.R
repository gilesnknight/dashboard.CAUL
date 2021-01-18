#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`. 
#'     DO NOT REMOVE.
#' @import shiny leaflet
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # List the first level UI elements here 
    
    navbarPage(windowTitle = "CAUL Dashboard",
               title = span(
                 img(
                   src = "www/logo.png",
                   width = 100,
                   height = 40,
                   style = "position:relative; top:-7px; left:-6px"
                 )
               ),
               tabPanel("Perth",
                        sidebarLayout(
                          sidebarPanel(
                            leaflet::leafletOutput(outputId = "PER_SSC_map", height = "calc(100vh - 80px)")
                          ),
                          mainPanel(
                            tabsetPanel(type = "tabs",
                                        tabPanel("Piecharts",
                                                 fluidRow(
                                                   column(6, plotly::plotlyOutput("PER_vegtype_pie",
                                                                                  height = 'calc(50vh - 57.5px)')),
                                                   column(6, plotly::plotlyOutput("PER_privpubl_pie",
                                                                                  height = 'calc(50vh - 57.5px)'))
                                                          ),
                                                 fluidRow(
                                                   column(6, plotly::plotlyOutput("PER_LU_pie",
                                                                                  height = 'calc(50vh - 57.5px)')),
                                                   column(6, plotly::plotlyOutput("PER_TrLU_pie",
                                                                                  height = 'calc(50vh - 57.5px)'))
                                                 )
                                                 ),
                                        
                                        tabPanel("Scatters",
                                                 fluidRow(
                                                   column(12, plotly::plotlyOutput("PER_SSC_gross_scatter",
                                                                                  height = 'calc(34vh - 46px)')),
                                                   column(12, plotly::plotlyOutput("PER_SSC_urban_scatter",
                                                                                   height = 'calc(34vh - 46px)')),
                                                   column(12, plotly::plotlyOutput("PER_SSC_res_scatter",
                                                                                   height = 'calc(34vh - 46px)'))
                                                 )
                                                 
                                        )
                                        )
                            )
                            
                          )
                        ),
               tabPanel("Melbourne",
                        sidebarLayout(
                          sidebarPanel(
                            leaflet::leafletOutput(outputId = "MEL_SSC_map", height = "calc(100vh - 80px)")
                          ),
                          mainPanel(
                            tabsetPanel(type = "tabs",
                                        tabPanel("Piecharts",
                                                 fluidRow(
                                                   column(6, plotly::plotlyOutput("MEL_vegtype_pie",
                                                                                  height = 'calc(50vh - 57.5px)')),
                                                   column(6, plotly::plotlyOutput("MEL_privpubl_pie",
                                                                                  height = 'calc(50vh - 57.5px)'))
                                                 ),
                                                 fluidRow(
                                                   column(6, plotly::plotlyOutput("MEL_LU_pie",
                                                                                  height = 'calc(50vh - 57.5px)')),
                                                   column(6, plotly::plotlyOutput("MEL_TrLU_pie",
                                                                                  height = 'calc(50vh - 57.5px)'))
                                                 )
                                        ),
                                        
                                        tabPanel("Scatters",
                                                 fluidRow(
                                                   column(12, plotly::plotlyOutput("MEL_SSC_gross_scatter",
                                                                                   height = 'calc(34vh - 46px)')),
                                                   column(12, plotly::plotlyOutput("MEL_SSC_urban_scatter",
                                                                                   height = 'calc(34vh - 46px)')),
                                                   column(12, plotly::plotlyOutput("MEL_SSC_res_scatter",
                                                                                   height = 'calc(34vh - 46px)'))
                                                 )
                                                 
                                        )
                            )
                          )
                        )
               ),
               tabPanel("Sydney",
                        sidebarLayout(
                          sidebarPanel(
                            leaflet::leafletOutput(outputId = "SYD_SSC_map", height = "calc(100vh - 80px)")
                          ),
                          mainPanel(
                            tabsetPanel(type = "tabs",
                                        tabPanel("Piecharts",
                                                 fluidRow(
                                                   column(6, plotly::plotlyOutput("SYD_vegtype_pie",
                                                                                  height = 'calc(50vh - 57.5px)')),
                                                   column(6, plotly::plotlyOutput("SYD_privpubl_pie",
                                                                                  height = 'calc(50vh - 57.5px)'))
                                                 ),
                                                 fluidRow(
                                                   column(6, plotly::plotlyOutput("SYD_LU_pie",
                                                                                  height = 'calc(50vh - 57.5px)')),
                                                   column(6, plotly::plotlyOutput("SYD_TrLU_pie",
                                                                                  height = 'calc(50vh - 57.5px)'))
                                                 )
                                        ),
                                        
                                        tabPanel("Scatters",
                                                 fluidRow(
                                                   column(12, plotly::plotlyOutput("SYD_SSC_gross_scatter",
                                                                                   height = 'calc(34vh - 46px)')),
                                                   column(12, plotly::plotlyOutput("SYD_SSC_urban_scatter",
                                                                                   height = 'calc(34vh - 46px)')),
                                                   column(12, plotly::plotlyOutput("SYD_SSC_res_scatter",
                                                                                   height = 'calc(34vh - 46px)'))
                                                 )
                                                 
                                        )
                            )
                          )
                        )
               ),
               tabPanel(title = "About",
                        icon = icon("info-circle"),
                        fillPage(titlePanel("About the dashboard"))
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
golem_add_external_resources <- function(){
  
  add_resource_path(
    'www', app_sys('app/www')
  )
 
  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'caul.dashboard'
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert() 
  )
}

