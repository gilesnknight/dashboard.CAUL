#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
    sidebarPanel(
        h3("Select the first suburb"),
        shiny::selectizeInput(
            "SSC_city_dropdown",
            label = "Select a city",
            choices = c("Perth","Melbourne","Sydney"),
            width = '50%',
            selected = NULL,
            options = list(
                placeholder = "Please select a city",
                onInitialize = base::I('function() { this.setValue(""); }')
            )
        ),
        conditionalPanel(
            condition = "input.SSC_city_dropdown == PER || input.SSC_city_dropdown == MEL || input.SSC_city_dropdown == SYD",
            uiOutput("SSCcomparePER")
        ),
        br(),
        h3("Select a second suburb"),
        shiny::selectizeInput(
            "SSC_city_dropdown2",
            label = "Select a second city",
            choices = c("Perth","Melbourne","Sydney"),
            width = '50%',
            selected = NULL,
            options = list(
                placeholder = "Please select a city",
                onInitialize = base::I('function() { this.setValue(""); }')
            )
        ),
        conditionalPanel(
            condition = "input.SSC_city_dropdown2 == PER || input.SSC_city_dropdown2 == MEL || input.SSC_city_dropdown2 == SYD",
            uiOutput("SSCcompare2")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    SSC_city_dropdown <- shiny::reactiveValues()

    observeEvent(input$SSC_city_dropdown,{
        if(input$SSC_city_dropdown == ""){
            SSC_city_dropdown$city <- NULL
        }
        else if(input$SSC_city_dropdown == "Perth"){
            SSC_city_dropdown$city <- "PER"
        }
        else if(input$SSC_city_dropdown == "Melbourne"){
            SSC_city_dropdown$city <- "MEL"
        }
        else if(input$SSC_city_dropdown == "Sydney"){
            SSC_city_dropdown$city <- "SYD"
        }
        
    })
    observeEvent(SSC_city_dropdown$city,{
        if(base::is.null(SSC_city_dropdown$city)){
            SSClist <- c("")
        } else if(SSC_city_dropdown$city=="PER"){
            SSClist <-ALL_SSC_DATA[ALL_SSC_DATA[["GCC"]] == 'PER', ]['SSC_NAME16']
        } else if(SSC_city_dropdown$city=="MEL"){
            SSClist <- ALL_SSC_DATA[ALL_SSC_DATA[["GCC"]] == 'MEL', ]['SSC_NAME16']
        }else if(SSC_city_dropdown$city=="SYD"){
            SSClist <- ALL_SSC_DATA[ALL_SSC_DATA[["GCC"]] == 'SYD', ]['SSC_NAME16']
        }
        output$SSCcomparePER <- renderUI(
            shiny::selectizeInput(
                "SSCcomparedropdown",
                label = 'Select a suburb',
                choices = SSClist,
                width = '50%',
                selected = NULL,
                options = list(
                    placeholder = "Please select suburb",
                    onInitialize = base::I('function() { this.setValue(""); }')
                )
            )
        )
    })
  
    observeEvent(input$SSC_city_dropdown2,{
        if(input$SSC_city_dropdown2 == ""){
            SSC_city_dropdown$city <- NULL
        }
        else if(input$SSC_city_dropdown2 == "Perth"){
            SSC_city_dropdown$city2 <- "PER"
        }
        else if(input$SSC_city_dropdown2 == "Melbourne"){
            SSC_city_dropdown$city2 <- "MEL"
        }
        else if(input$SSC_city_dropdown2 == "Sydney"){
            SSC_city_dropdown$city2 <- "SYD"
        }
    })
    observeEvent(SSC_city_dropdown$city2,{
        SSCnames2 <- ALL_SSC_DATA[ALL_SSC_DATA$SSC_NAME16 != input$SSCcomparedropdown, ] 
        if(base::is.null(SSC_city_dropdown$city2)){
            SSClist2 <- c("")
        } else if(SSC_city_dropdown$city2=="PER"){
            SSClist2 <-SSCnames2[SSCnames2[["GCC"]] == 'PER', ]['SSC_NAME16']
        } else if(SSC_city_dropdown$city2=="MEL"){
            SSClist2 <- SSCnames2[SSCnames2[["GCC"]] == 'MEL', ]['SSC_NAME16']
        }else if(SSC_city_dropdown$city2=="SYD"){
            SSClist2 <- SSCnames2[SSCnames2[["GCC"]] == 'SYD', ]['SSC_NAME16']
        }
        output$SSCcompare2 <- renderUI(
            shiny::selectizeInput(
                "SSCcomparedropdown2",
                label = 'Select a second suburb',
                choices = SSClist2,
                width = '50%',
                selected = NULL,
                options = list(
                    placeholder = "Please select suburb",
                    onInitialize = base::I('function() { this.setValue(""); }')
                )
            )
        )
    })

    
    
    
    
    
    
    
    
    
    
    
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
