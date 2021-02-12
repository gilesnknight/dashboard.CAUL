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
  
  tagList(
    # Leave this function for adding external resources
    
    golem_add_external_resources(),
    # List the first level UI elements here 
    # waiter::use_waiter(),
    # waiter::use_garcon(),
    # 
    
    
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
                            width = 5,
                            waiter::waiter_show_on_load(color = '#008346', tags$img(
                              src="www/logo.png",
                              height=150,
                              id = "myImage")),
                            waiter::waiter_hide_on_render("PER_SSC_map"),
                            # selectizeInput(
                            #   'PER_SSC_dropdown', label = NULL, choices = PER_SSC_DATA$SSC_NAME16,
                            #   options = list(maxOptions = 5)
                            # ),
                            leaflet::leafletOutput(outputId = "PER_SSC_map", height = "calc(100vh - 80px)")
                            
                            
                          ),
                          
                          mainPanel(
                            width = 7,
                            tabsetPanel(type = "tabs",
                                        tabPanel("Land-use",
                                                 ggiraph::girafeOutput("PER_barcharts",
                                                                       height = 'calc(97vh - 90px)')
                                                 
                                        ),
                                        tabPanel("Suburb comparison"
                                                 # ggiraph::girafeOutput("PER_psudoscatter",
                                                 #                            height = 'calc(100vh - 90px)'
                                                 #                       )
                                                 ),
                                        tabPanel("City comparison"
                                        )
                                        
                                        
                                        )
                            )
                            
                          )
                        ),
               tabPanel("Melbourne",
                        sidebarLayout(
                          sidebarPanel(
                            width = 5,
                            waiter::waiter_show_on_load(color = '#008346', tags$img(
                              src="www/logo.png", 
                              height=150,
                              id = "myImage2")),
                            waiter::waiter_hide_on_render("MEL_SSC_map"),
                            leaflet::leafletOutput(outputId = "MEL_SSC_map", height = "calc(100vh - 80px)")
                          ),
                          mainPanel(
                            width = 7,
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
                            width = 5,
                            leaflet::leafletOutput(outputId = "SYD_SSC_map", height = "calc(100vh - 80px)")
                          ),
                          mainPanel(
                            width = 7,
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

