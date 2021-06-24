library(shiny)

# Goal: The ability to update selectizeInputs so a suburb can never be compared to itself 
# EXAMPLE:
# First city selected: "Perth" -> Suburbs available: "Nedlands", "Crawley", "Subiaco" -> "Nedlands" selected
# Second city selected: "Perth" -> Suburbs available: "Crawley", "Subiaco" -> "Subiaco" selected
# Suburbs options for first city are updated to remove "Subiaco" so a "Subiaco"x"Subiaco" combination cannot be chosen
# Importantly, during the list update the already selected suburb ("Nedlands") is not reset


# Data - 3 suburbs for Perth, Melbourne, and Sydney
suburbData <- data.frame(
  SSC_NAME16 = ALL_SSC_DATA$SSC_NAME16,
  SSC_CODE16 = ALL_SSC_DATA$SSC_CODE16,
  GCC = ALL_SSC_DATA$GCC,
  PerAnyTree = ALL_SSC_DATA$PerAnyTree)

histParts <- function(GCC){
  if(GCC == "PER"){
    return(ggiraph::geom_histogram_interactive(
      suburbData[suburbData["GCC"] == "PER",], 
      mapping = ggplot2::aes(
        x=PerAnyTree, 
        tooltip = base::paste0(
          "Perth suburbs: ", scales::percent(base::mean(suburbData[suburbData["GCC"] == "PER","PerAnyTree"]),scale = 1, accuracy = 0.1), " (average)"
          )
        ), 
      binwidth=0.5, fill="#66c2a5",#"#66c2a5", 
      color="#66c2a5", alpha=0.2, lwd = 0.2, 
      data_id = "Perth"
    ))
  } else if(GCC == "MEL"){
    return(ggiraph::geom_histogram_interactive(
      suburbData[suburbData["GCC"] == "MEL",], 
      mapping = ggplot2::aes(
        x=PerAnyTree, 
        tooltip = base::paste0(
          "Melbourne suburbs: ", scales::percent(base::mean(suburbData[suburbData["GCC"] == "MEL","PerAnyTree"]),scale = 1, accuracy = 0.1), " (average)"
        )
        ), 
      binwidth=0.5, fill= "#fc8d62",#fc8d62", 
      color="#fc8d62", alpha=0.2, lwd = 0.2, 
      data_id = "Melbourne"
    ))
  } else if(GCC == "SYD"){
    return(ggiraph::geom_histogram_interactive(
      suburbData[suburbData["GCC"] == "SYD",],
      mapping = ggplot2::aes(
        x=PerAnyTree, 
        tooltip = base::paste0(
          "Sydney suburbs: ", scales::percent(base::mean(suburbData[suburbData["GCC"] == "SYD","PerAnyTree"]),scale = 1, accuracy = 0.1), " (average)"
        )
        ),
      binwidth=0.5, fill="8da0cb",#"#8da0cb",
      color="#8da0cb", alpha=0.2, lwd = 0.2,
      data_id = "Sydney"
    ))
  }
}
histSelector <- function(input){
  if(input == "PER"){
    return(histParts(GCC = "PER"))
  } else if(input == "MEL"){
    return(histParts(GCC = "MEL"))
  } else if(input == "SYD"){
    return(histParts(GCC = "SYD"))
  }
}
histBrackets <- function(matchingHist, firstHist, secondHist){
  plotDF <- ggplot2::ggplot() +
    matchingSSChist +
    firstSSChist +
    secondSSChist
  plotDF <- base::data.frame(ggplot2::ggplot_build(plotDF)$data[[1]])
  plotMinMax <- base::data.frame(
    x = plotDF$x,
    xmin = plotDF$xmin,
    xmax = plotDF$xmax
  )
  return(plotMinMax)
}
filterBracket <- function(SSCdata, selectedSSCName, selectedSSCGCC, SSCdataTree, histBrackets){
  filteredBracket <- histBrackets[which.min(abs(SSCdata[SSCdata["SSC_NAME16"] == selectedSSCName,"PerAnyTree"]-histBrackets$x)),]
  filteredBracket <- (SSCdata$GCC==selectedSSCGCC & SSCdataTree >= filteredBracket$xmin & SSCdataTree < filteredBracket$xmax )
}
selectedBin <- function(SSCdata, GCC, filteredBracket, SSCname, SSCtree){
  if(GCC == "PER"){
    return(
      ggiraph::geom_histogram_interactive(
        data=SSCdata[filteredBracket,], 
        ggplot2::aes(
          x=PerAnyTree, 
          tooltip = base::paste0(
            SSCname,": ",scales::percent(SSCtree, scale = 1, accuracy = 0.1)
            )
          ), 
        fill="#55a189",
        colour = "#55a189",
        binwidth=0.5,
        lwd = 0.25
        )
      )
  } else if(GCC == "MEL"){
    return(
      ggiraph::geom_histogram_interactive(
        data=SSCdata[filteredBracket,], 
        ggplot2::aes(
          x=PerAnyTree, 
          tooltip = base::paste0(
            SSCname,": ",scales::percent(SSCtree, scale = 1, accuracy = 0.1)
            )
          ), 
        fill="#e8835a", 
        colour = "#e8835a",
        binwidth=0.5,
        lwd = 0.25
        )
      )
  } else if(GCC == "SYD"){
    return(
      ggiraph::geom_histogram_interactive(
        data=SSCdata[filteredBracket,], 
        ggplot2::aes(
          x=PerAnyTree, 
          tooltip = base::paste0(
            SSCname,": ",scales::percent(SSCtree, scale = 1, accuracy = 0.1)
            )
          ), 
        fill="#7389ba", 
        colour = "#7389ba",
        binwidth=0.5,
        lwd = 0.25
        )
      )
  }
}
meanParts <- function(GCC){
  if(GCC == "PER"){
    return(ggiraph::geom_vline_interactive(
      xintercept = base::mean(
        base::mean(suburbData[suburbData["GCC"] == "PER","PerAnyTree"])
      ), 
      color ="#66c2a5", 
      linetype = "dashed",
      lwd = 0.55
    ))
  } else if(GCC == "MEL"){
    return(ggiraph::geom_vline_interactive(
      xintercept = base::mean(
        base::mean(suburbData[suburbData["GCC"] == "MEL","PerAnyTree"])
      ), 
      color ="#fc8d62", 
      linetype = "dashed",
      lwd = 0.55
    ))
  } else if(GCC == "SYD"){
    return(ggiraph::geom_vline_interactive(
      xintercept = base::mean(
        base::mean(suburbData[suburbData["GCC"] == "SYD","PerAnyTree"])
      ), 
      color ="#8da0cb", 
      linetype = "dashed",
      lwd = 0.55
    ))
  }
}

# for(i in 1:52){
#   BRRR::skrrrahh(i) 
#   print(i)
#   Sys.sleep(2)
#   }




meanSelector <- function(input){
  if(input == "PER"){
    return(meanParts(GCC = "PER"))
  } else if(input == "MEL"){
    return(meanParts(GCC = "MEL"))
  } else if(input == "SYD"){
    return(meanParts(GCC = "SYD"))
  }
}

#### UI ####
ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      width = 3,
      shiny::selectizeInput(
        'selectFirstCity',
        label = "Select the first city",
        choices = c('Perth', 'Melbourne', 'Sydney'),
        selected = NULL,
        options = list(
          placeholder = "Please select an option below",
          onInitialize = base::I('function() { this.setValue(""); }') # Used so it appears blank
        )
      ),
      uiOutput('selectFirstSuburb'),
      shiny::selectizeInput(
        'selectSecondCity',
        label = "Select the second city",
        choices = c('Perth', 'Melbourne', 'Sydney'),
        selected = NULL,
        options = list(
          placeholder = "Please select an option below",
          onInitialize = base::I('function() { this.setValue(""); }')
        )
      ),
      uiOutput('selectSecondSuburb'),
      actionButton("histSwitchButton", "Switch"),
      checkboxInput("meanSwichBox", "Show averages", value = FALSE, width = NULL)
    ),
    mainPanel(
      width = 9,
      ggiraph::girafeOutput("SSChist",
                            height = "calc(100vh - 129px)"
      )
    )
  )
)
#### SERVER ####
server <- function(input, output) {
  
  selectedCity <- shiny::reactiveValues()
  
  # Observe city selectizeInputs and returns a reactive value of the abbreviated city name for filtering suburbData later
  observeEvent(input$selectFirstCity,{
    req(input$selectFirstCity)
    selectedCity$FirstCity <- switch(input$selectFirstCity, Perth={"PER"}, Melbourne={"MEL"}, Sydney={"SYD"})
    #print(paste0("selectedCity$FirstCity: ", selectedCity$FirstCity))
  })
  observeEvent(input$selectSecondCity,{
    req(input$selectSecondCity)
    selectedCity$SecondCity <- switch(input$selectSecondCity, Perth={"PER"}, Melbourne={"MEL"}, Sydney={"SYD"})
    #print(paste0("selectedCity$SecondCity: ", selectedCity$SecondCity))
  })
  
  # Outputs a selectizeInput of suburbs filtered to the selected city
  output$selectFirstSuburb <- renderUI({
    req(selectedCity$FirstCity)
    shiny::selectizeInput(
      "selectFirstSuburbUI",
      label = "Select a suburb",
      choices = suburbData[suburbData["GCC"] == selectedCity$FirstCity,"SSC_NAME16"],
      selected = NULL,
      options = list(
        placeholder = "Please select an option below",
        onInitialize = base::I('function() { this.setValue(""); }')
      )
    )
  })
  output$selectSecondSuburb <- renderUI({
    req(selectedCity$SecondCity)
    shiny::selectizeInput(
      "selectSecondSuburbUI",
      label = "Select a suburb",
      choices = suburbData[suburbData["GCC"] == selectedCity$SecondCity,"SSC_NAME16"],
      selected = NULL,
      options = list(
        placeholder = "Please select an option below",
        onInitialize = base::I('function() { this.setValue(""); }')
      )
    )
  })
  
  selectedSuburb <- shiny::reactiveValues()
  
  # Observes suburb selectizeInputs and returns reactive value of the last selected suburb
  observeEvent(input$selectFirstSuburbUI,{
    req(input$selectFirstSuburbUI != "")
    selectedSuburb$FirstSuburb <- input$selectFirstSuburbUI
    #print(paste0("selectedSuburb$FirstSuburb: ", selectedSuburb$FirstSuburb))
  })
  observeEvent(input$selectSecondSuburbUI,{
    req(input$selectSecondSuburbUI != "")
    selectedSuburb$SecondSuburb <- input$selectSecondSuburbUI
    #print(paste0("selectedSuburb$SecondSuburb: ", selectedSuburb$SecondSuburb))
  })
  
  # NOT WORKING: Desired goal here is to update the 2nd suburb selectizeInput minus the suburb from the 1st suburb selectizeInput
  # observeEvent(selectedSuburb$FirstSuburb,{
  #   req(selectedSuburb$SecondSuburb)
  #   updateSelectizeInput(
  #     "selectSecondSuburbUI",
  #     selected = selectedSuburbSecondSuburb,
  #     choices = suburbData[suburbData["GCC"] == selectedCity$SecondCity,"SSC_NAME16"][suburbData[suburbData["GCC"] == selectedCity$SecondCity,"SSC_NAME16"] != selectedSuburb$FirstSuburb],
  #     server = TRUE)
  # })
  
  #The following creates a list of suburbs within the selected city, minus the one already selected:
  #suburbData[suburbData["GCC"] == "PER","SSC_NAME16"][suburbData[suburbData["GCC"] == "PER","SSC_NAME16"] != "Nedlands"]
  
  
  filterSSCname <- function(df, SSCname, selectedSSC){
    filterSSCname <- (df[df[[SSCname]] == selectedSSC, ])
    filterSSCname
  }
  
  observeEvent(selectedSuburb$FirstSuburb,{
    suburb1Data <- filterSSCname(
      df = suburbData,
      SSCname = 'SSC_NAME16',
      selectedSSC = selectedSuburb$FirstSuburb
    )
  })
  
  observeEvent(selectedSuburb$SecondSuburb,{
    suburb2Data <- filterSSCname(
      df = suburbData,
      SSCname = 'SSC_NAME16',
      selectedSSC = selectedSuburb$SecondSuburb
    )
  })
  
  histSwitch <- reactiveVal(FALSE)
  
  observeEvent(input$histSwitchButton, {
    histSwitch(
      !histSwitch() )
  })
  
  histSwitchVal <- reactive({
    if (histSwitch()) {
      TRUE
    } else {
      FALSE
    }
  })
  
  output$SSChist <- ggiraph::renderGirafe({
    req(selectedSuburb$FirstSuburb)
    req(selectedSuburb$SecondSuburb)
    suburb1Data <- filterSSCname(
      df = suburbData,
      SSCname = 'SSC_NAME16',
      selectedSSC = selectedSuburb$FirstSuburb
    )
    suburb2Data <- filterSSCname(
      df = suburbData,
      SSCname = 'SSC_NAME16',
      selectedSSC = selectedSuburb$SecondSuburb
    )
    
    histFlip <- histSwitchVal
    
    if(suburb1Data$GCC==suburb2Data$GCC){
      firstSSChist <- NULL
      secondSSChist <- NULL
      matchingSSChist <- histSelector(input = suburb1Data$GCC)
    } else {
      matchingSSChist <- NULL
      firstSSChist <- histSelector(input = suburb1Data$GCC)
      secondSSChist <- histSelector(input = suburb2Data$GCC)
    }
    
    plotMinMax <- histBrackets(
      matchingHist = matchingSSChist,
      firstHist = firstSSChist,
      secondHist = secondSSChist
    )
    
    filteredBracket1 <- filterBracket(
      SSCdata = suburbData,
      selectedSSCName = suburb1Data$SSC_NAME16,
      selectedSSCGCC = suburb1Data$GCC,
      SSCdataTree = suburbData$PerAnyTree,
      histBrackets = plotMinMax
    )
    
    filteredBracket2 <- filterBracket(
      SSCdata = suburbData,
      selectedSSCName = suburb2Data$SSC_NAME16,
      selectedSSCGCC = suburb2Data$GCC,
      SSCdataTree = suburbData$PerAnyTree,
      histBrackets = plotMinMax
    )
    
    # Mean lines
    if(input$meanSwichBox == FALSE){
      firstSSCmean <- NULL
      secondSSCmean <- NULL
      matchingSSCmean <- NULL
    } else if(input$meanSwichBox == TRUE){
      if(suburb1Data$GCC==suburb2Data$GCC){
        firstSSCmean <- NULL
        secondSSCmean <- NULL
        matchingSSCmean <- meanSelector(input = suburb1Data$GCC)
      } else {
        matchingSSCmean <- NULL
        firstSSCmean <- meanSelector(input = suburb1Data$GCC)
        secondSSCmean <- meanSelector(input = suburb2Data$GCC)
      }
    }
    
    if((suburb1Data$GCC==suburb2Data$GCC)==TRUE & all(suppressWarnings(suburbData[filteredBracket1,'SSC_NAME16'] == suburbData[filteredBracket2,'SSC_NAME16']))==TRUE){
      firstSSChist <- NULL
      secondSSChist <- NULL
      matchingSSChist <- list(matchingSSChist, matchingSSCmean, selectedBin(SSCdata = suburbData, filteredBracket = filteredBracket1, GCC = suburb1Data$GCC, SSCname = suburb1Data$SSC_NAME16, SSCtree = suburb1Data$PerAnyTree))
    }
    
    firstSSChist <- list(firstSSChist,
                         firstSSCmean,
                         selectedBin(SSCdata = suburbData, filteredBracket = filteredBracket1, GCC = suburb1Data$GCC, SSCname = suburb1Data$SSC_NAME16, SSCtree = suburb1Data$PerAnyTree)
    )
    secondSSChist <- list(secondSSChist,
                          secondSSCmean,
                          selectedBin(SSCdata = suburbData, filteredBracket = filteredBracket2, GCC = suburb2Data$GCC, SSCname = suburb2Data$SSC_NAME16, SSCtree = suburb2Data$PerAnyTree)
    )
    
    if(histFlip() == FALSE){
      differentSSChist <- list(firstSSChist,secondSSChist)
    } else{
      differentSSChist <- list(secondSSChist, firstSSChist)
    }
    
    
    
    
    
    
    
    
    
    
    plot <- ggplot2::ggplot() +
      matchingSSChist +
      differentSSChist +
      ggplot2::labs(x = "Tree canopy cover (%)", y = "Number of suburbs") +
      ggplot2::scale_y_continuous(
        expand = c(0,0)
        #limits=c(0, 30)
      ) +
      ggplot2::scale_x_continuous(
        expand = c(0,0), 
        #limits=c(-1, 100),
        labels = scales::percent_format(scale = 1,accuracy = 1)) +
      ggplot2::theme_classic() +
      ggplot2::theme(
        axis.text.x = ggplot2::element_text(colour = "black"),
        axis.text.y = ggplot2::element_text(colour = "black")
      )
    girafe <- ggiraph::girafe(
      ggobj = plot,
      fonts = list(serif = "Helvetica"),
      options = list(#ggiraph::opts_hover_inv(css = "fill:transparent;stroke:transparent;"),
                     ggiraph::opts_hover(css = 'stroke-width:0.5pt;'),
                     ggiraph::opts_sizing(rescale = TRUE, width = 1),
                     ggiraph::opts_toolbar(saveaspng = FALSE)
                     )
    )
    
    girafe
  })
  
}

shinyApp(ui = ui, server = server)




