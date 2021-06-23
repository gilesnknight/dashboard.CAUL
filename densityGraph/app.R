
library(shiny)


ui <- fluidPage(
    navbarPage(
        shiny::tabPanel(
            "Compare",
            sidebarLayout(
                sidebarPanel(
                    width = 4,
                    checkboxInput("PER_SSC_compare_check", label = "Plot Perth distribution", value = FALSE, width = NULL),
                    checkboxInput("PER_SSC_compare_SSC_check", label = "Plot a Perth suburb", value = FALSE, width = NULL),
                    conditionalPanel(
                        condition = "input.PER_SSC_compare_SSC_check == true",
                        shiny::selectizeInput(
                            "PER_SSC_compare_dropdown",
                            label = "Select a suburb",
                            choices = PER_SSC_DATA$SSC_NAME16,
                            selected = NULL,
                            options = list(
                                placeholder = "Please select an option below",
                                onInitialize = base::I('function() { this.setValue(""); }')
                            )
                        )
                    ),
                    checkboxInput("MEL_SSC_compare_check", label = "Plot Melbourne", value = FALSE, width = NULL),
                    checkboxInput("MEL_SSC_compare_SSC_check", label = "Plot a Melbourne suburb", value = FALSE, width = NULL),
                    conditionalPanel(
                        condition = "input.MEL_SSC_compare_SSC_check == true",
                        shiny::selectizeInput(
                            "MEL_SSC_compare_dropdown",
                            label = "Select a suburb",
                            choices = MEL_SSC_DATA$SSC_NAME16,
                            selected = NULL,
                            options = list(
                                placeholder = "Please select an option below",
                                onInitialize = base::I('function() { this.setValue(""); }')
                            )
                        )
                    ),
                    checkboxInput("SYD_SSC_compare_check", label = "Plot Sydney", value = FALSE, width = NULL),
                    checkboxInput("SYD_SSC_compare_SSC_check", label = "Plot a Sydney suburb", value = FALSE, width = NULL),
                    conditionalPanel(
                        condition = "input.SYD_SSC_compare_SSC_check == true",
                        shiny::selectizeInput(
                            "SYD_SSC_compare_dropdown",
                            label = "Select a suburb",
                            choices = SYD_SSC_DATA$SSC_NAME16,
                            selected = NULL,
                            options = list(
                                placeholder = "Please select an option below",
                                onInitialize = base::I('function() { this.setValue(""); }')
                            )
                        )
                    ),
                ),
                mainPanel(
                   width = 8,
                   plotOutput("densityPlot")
                )
            )
        )
    )
)

server <- function(input, output) {
    SSClineGeomSwitch <- function(switch, df, colour, selectedSSC){
        if(switch == TRUE){
            return(
                ggplot2::geom_vline(
                    xintercept=base::as.numeric(df[df[["SSC_CODE16"]] == selectedSSC, ]["PerAnyTree"]),
                    size=0.5,
                    color= colour
                )
            )
        } else {
            return(NULL)
        }
    }
    densityGeomSwitch <- function(switch, df, GCC, colour, densityVals) {
        if (switch == TRUE) {
            d <- stats::density(densityVals)
            d <- base::data.frame(x = d$x, y = d$y)
            meanVal <- densityMeanValue(df = df,
                                        GCC = GCC)
            meanHeight <- densityMeanHeight(meanValue = meanVal,
                                            densityVals = densityVals)
            return(densityCurve <- list(
                ggplot2::geom_line(
                    data = d,
                    ggplot2::aes(x, y),
                    colour = colour
                ),
                densityMeanLine(
                    x1 = meanVal,
                    x2 = meanVal,
                    y1 = 0,
                    y2 = meanHeight,
                    colour = colour
                )
            ))
        } else {
            return(NULL)
        }
    }
    densityMeanHeight <- function(meanValue,densityVals){
        d<-stats::density(densityVals)
        dd <- stats::approxfun(d$x, d$y)
        return(dd(meanValue))
       # meanValue <- meanValue
        #df <- stats::approxfun(stats::density(densityVals))
       # return(df(meanValue))
    }
    densityMeanValue <- function(df, GCC){
        return(base::as.numeric(base::lapply(df[df[["GCC"]] == GCC,]["PerAnyTree"], mean)))
    }
    densityMeanLine <- function(x1,y1,x2,y2,colour){
        ggplot2::geom_segment(
            ggplot2::aes(x = x1,
                         y = y1,
                         xend = x2,
                         yend = y2),
            size=0.5,
            lineend = "round",
            colour = colour,
            linetype="dashed"
        )
    }

    SSC_compare <- shiny::reactiveValues(active = base::vector(mode = "numeric"))

    observeEvent(input$PER_SSC_compare_dropdown,{
        if(input$PER_SSC_compare_dropdown == ""){
            SSC_compare$PER <- NULL
        }
        else {
            SSC_compare$PER <- base::as.numeric(PER_SSC_DATA[PER_SSC_DATA[["SSC_NAME16"]] == input$PER_SSC_compare_dropdown, ]["SSC_CODE16"])
            print(SSC_compare$PER)
        }
    })
    observeEvent(input$MEL_SSC_compare_dropdown,{
        if(input$MEL_SSC_compare_dropdown == ""){
            SSC_compare$MEL <- NULL
        }
        else {
            SSC_compare$MEL <- base::as.numeric(MEL_SSC_DATA[MEL_SSC_DATA[["SSC_NAME16"]] == input$MEL_SSC_compare_dropdown, ]["SSC_CODE16"])
            print(SSC_compare$MEL)
        }
    })
    observeEvent(input$SYD_SSC_compare_dropdown,{
        if(input$SYD_SSC_compare_dropdown == ""){
            SSC_compare$SYD <- NULL
        }
        else {
            SSC_compare$SYD <- base::as.numeric(SYD_SSC_DATA[SYD_SSC_DATA[["SSC_NAME16"]] == input$SYD_SSC_compare_dropdown, ]["SSC_CODE16"])
            print(SSC_compare$SYD)
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
}

shinyApp(ui = ui, server = server)
