library(shiny)

SSC_NAMES <- ALL_SSC_DATA$SSC_NAME16
SSC_compare <- data.frame(
  SSC_NAME16 = ALL_SSC_DATA$SSC_NAME16,
  SSC_CODE16 = ALL_SSC_DATA$SSC_CODE16,
  GCC = ALL_SSC_DATA$GCC,
  PerAnyTree = ALL_SSC_DATA$PerAnyTree)

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      width = 4,
      shiny::selectizeInput(
        'CITY_select1',
        label = "Select the first city",
        choices = c('Perth', 'Melbourne', 'Sydney'),
        selected = NULL,
        options = list(
          placeholder = "Please select an option below",
          onInitialize = base::I('function() { this.setValue(""); }')
        )
      ),
      uiOutput('SSC_select1_dropdown_UI'),
      shiny::selectizeInput(
        'CITY_select2',
        label = "Select the second city",
        choices = c('Perth', 'Melbourne', 'Sydney'),
        selected = NULL,
        options = list(
          placeholder = "Please select an option below",
          onInitialize = base::I('function() { this.setValue(""); }')
        )
      ),
      uiOutput('SSC_select2_dropdown_UI')
    ),
    mainPanel(
      width = 8
      #plotOutput("densityPlot")
    )
  )
)

server <- function(input, output) {
  
  selectedCity <- shiny::reactiveValues()
  
  observeEvent(input$CITY_select1,{
    selectedCity$city1 <- switch(input$CITY_select1, Perth={"PER"}, Melbourne={"MEL"}, Sydney={"SYD"})
    print(selectedCity$city1)
  })
  observeEvent(input$CITY_select2,{
    selectedCity$city2 <- switch(input$CITY_select2, Perth={"PER"}, Melbourne={"MEL"}, Sydney={"SYD"})
    print(selectedCity$city2)
  })
  
  selectedSSC <- shiny::reactiveValues()
  selectedSSC$SSC1 <- ""
  selectedSSC$SSC2 <- ""
  
  observeEvent(input$SSC_select1_dropdown,{
    req(input$SSC_select1_dropdown != "")
    selectedSSC$SSC1 <- input$SSC_select1_dropdown
    print(selectedSSC$SSC1)
  })
  observeEvent(input$SSC_select2_dropdown,{
    req(input$SSC_select2_dropdown != "")
    selectedSSC$SSC2 <- input$SSC_select2_dropdown
    print(selectedSSC$SSC2)
  })

  
  output$SSC_select1_dropdown_UI <- renderUI({
    req(selectedCity$city1)
    shiny::selectizeInput(
      "SSC_select1_dropdown",
      label = "Select a suburb",
      choices = SSC_compare[SSC_compare["GCC"] == selectedCity$city1,"SSC_NAME16"][SSC_compare[SSC_compare["GCC"] == selectedCity$city1,"SSC_NAME16"] != selectedSSC$SSC2],
      selected = selectedCity$city1,
      options = list(
        placeholder = "Please select an option below",
        onInitialize = base::I('function() { this.setValue(""); }')
      )
    )
  })
  output$SSC_select2_dropdown_UI <- renderUI({
    req(selectedCity$city1, selectedCity$city2)
    shiny::selectizeInput(
      "SSC_select2_dropdown",
      label = "Select a second suburb",
      choices = SSC_compare[SSC_compare["GCC"] == selectedCity$city2,"SSC_NAME16"][SSC_compare[SSC_compare["GCC"] == selectedCity$city2,"SSC_NAME16"] != selectedSSC$SSC1],
      selected = selectedCity$city2,
      options = list(
        placeholder = "Please select an option below",
        onInitialize = base::I('function() { this.setValue(""); }')
      )
    )
  })
  
  
  
  
  
  
  
  
  
}

shinyApp(ui = ui, server = server)






#SSC_compare[SSC_compare["GCC"] == selectedCity$city2,"SSC_NAME16"][SSC_compare[SSC_compare["GCC"] == selectedCity$city2,"SSC_NAME16"] != selectedSSC$SSC1]


