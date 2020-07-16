library(shiny)
library(dygraphs)
library(shinydashboard)

### Server
server <- function(input, output, clientData, session) {
  values <- reactiveValues()
  values$clicks <- -1
  ###
  observeEvent(input$removeInput, {
    #x <- "input2"
    #r <- input[[x]]
    if (values$clicks > -1) {
      values$clicks <- values$clicks - 2
    }
  })
  
  output$allInputs <- renderUI({
    input$appendInput
    input$removeInput
    isolate(values$clicks <- values$clicks + 1)
    allInputs <- isolate(values$clicks)
    req(!is.null(allInputs) && allInputs > 0)
    inputTagList <- tagList()
    choices <- values$headers[1,-1]
    for (i in 1:allInputs){
      inpID <- paste0("input", i)
      inpLabel <- paste("Variable", i)
      inpVal <- choices[1]
      if (inpID %in% isolate(names(input))) {
        isolate(inpVal <- input[[inpID]])
      }
      newInput <- selectInput(inpID, inpLabel, choices, selected=inpVal)
      inputTagList <- tagAppendChild(inputTagList, newInput)
    }
    return(inputTagList) 
  })
  
  output$center <- renderUI({
    input$appendInput
    input$removeInput
    #isolate(values$clicks <- values$clicks + 1)
    allInputs <- isolate(values$clicks)
    req(!is.null(allInputs) && allInputs > 0)
    inputTagList <- tagList()
    choices <- choices <- c("<", "=", ">")
    for (i in 1:allInputs){
      inpID <- paste0("centerInp", i)
      inpLabel <- paste("Operator", i)
      inpVal <- choices[1]
      if (inpID %in% isolate(names(input))) {
        isolate(inpVal <- input[[inpID]])
      }
      newInput <- selectInput(inpID, inpLabel, choices, selected=inpVal)
      inputTagList <- tagAppendChild(inputTagList, newInput)
    }
    return(inputTagList) 
  })
  
  output$right <- renderUI({
    input$appendInput
    input$removeInput
    #isolate(values$clicks <- values$clicks + 1)
    allInputs <- isolate(values$clicks)
    req(!is.null(allInputs) && allInputs > 0)
    inputTagList <- tagList()
    choices <- choices <- c("<", "=", ">")
    for (i in 1:allInputs){
      inpID <- paste0("rightInp", i)
      inpLabel <- paste("Value", i)
      inpVal <- choices[1]
      if (inpID %in% isolate(names(input))) {
        isolate(inpVal <- input[[inpID]])
      }
      newInput <- numericInput(inpID, inpLabel, value = 0)
      inputTagList <- tagAppendChild(inputTagList, newInput)
    }
    return(inputTagList) 
  })
  ###
  
  
  
  ### Multicolumn barchart support
  dyMultiColumn <- function(dygraph) {
    dyPlotter(dygraph = dygraph,
              name = "MultiColumn",
              path = system.file("plotters/multicolumn.js",
                                 package = "dygraphs"))
  }
  
  ### Choices for input files in dropdown menu
  getChoices <- reactive({
    switch (input$stationType,
            "Agroecosystem at Křešín u Pacova with crops harvested during the growing season" = c("KRP daily data", "KRP half-hourly data", "KRP monthly data", "KRP weekly data"),
            "Evergreen needleleaf forest at Rájec-Jestřebí representing monoculture of Norway spruce" = c("RAJ daily data", "RAJ half-hourly data", "RAJ monthly data", "RAJ weekly data"),
            "Deciduous broadleaf forests at Štítná nad Vláří representing monoculture of European beech" = c("STI daily data", "STI half-hourly data", "STI monthly data", "STI weekly data")
    )
  })
  
  output$station <- renderUI({
    selectInput("selectedFile", "Temporal resolution", choices = getChoices())
  })
  
  output$barGraphChoiceUI <- renderUI({
    if (input$graphType == "Time series") {
      if (getInputFile()[2] != "half-hourly")
        radioButtons("graphSubtype","Graph Subtype", c("Line Graph", "Bar Graph"), selected = "Line Graph")
      else
        radioButtons("graphSubtype","Graph Subtype", c("Line Graph"), selected = "Line Graph")
    }
  })
  
  output$showTimeDateSelect <- renderUI({
    if (input$graphType == "XY" && getInputFile()[2] %in% c("daily", "half-hourly")) {
      dateRangeInput("boundary_date", label = "Date range", startview = "year", start = "1971-01-01", end = "1971-01-01")
    }
    else if (input$graphType == "XY" && getInputFile()[2] %in% c("weekly", "monthly")) {
      sliderInput("boundary_date", "Range", min=-1, max=-1, value= c(-1,-1), step = 1)
    }
  })
  
  output$xLabel <- renderUI({
    if (getInputFile()[2] %in% c("weekly", "monthly") || input$graphType == "XY") {
      numericInput("x_axis_label", label = "Value", value = NULL)
    } else {
      dateInput("x_axis_label", label = "Value", value = NULL)
    }
  })
  
  ### Get choices for x axis
  output$col1 <- renderUI({
    headers <- values$headers[1,-1]
    if (input$graphType == "Time series") {
      myLabel <- "Y-axis"
    } else {
      myLabel <- "X-axis"
    }
    selectInput("col1ID", label = myLabel,
                choices = as.character(as.vector(headers[1,])),
                selected = "Tair")
  })
  
  ### Get choices for y axis
  output$col2 <- renderUI({
    headers <- values$headers[1,-1]
    if (input$graphType == "Time series") {
      selectInput("col2ID", label = "Y2-axis",
                  choices = append("Empty", as.character(as.vector(headers[1,]))),
                  selected = "Tsoil")
    } else {
      selectInput("col2ID", label = "Y-axis",
                  choices = as.character(as.vector(headers[1,])),
                  selected = "Tsoil")
    }
  })
  
  observeEvent(input$selectedFile, {
    fil <- getInputFile()
    inFile <- fil[1]
    dataType <- fil[2]
    
    headers <- read.csv(inFile, sep = ",", header = FALSE, nrows = 2, as.is = TRUE)
    meteo_data <- read.csv(inFile, sep = ",", header = FALSE, skip = 2)
    colnames(meteo_data) = headers[1,]
    colnames(headers) <- headers[1,]
    
    values$meteoData <- meteo_data
    values$headers <- headers
    values$dataType <- dataType
    
    if (dataType == "daily" || dataType == "half-hourly") {
      start <- min(levels(meteo_data[,1]))
      stop <- max(levels(meteo_data[,1]))
      updateDateRangeInput(session, "boundary_date", start = start, end = stop)
    } else if (dataType == "monthly" || dataType == "weekly") {
      stop = max(meteo_data[1])
      updateSliderInput(session, "boundary_date", min = 1, max = stop, value = c(1, stop))
    }
  })
  
  ### Plot Create
  output$plot <- renderDygraph({
    req(!is.null(input$col2ID))
    req(!is.null(input$col1ID))
    
    second_axis <- set_second_axis(input$single_axis)  #'y2', or NULL if no second axis
    isolate(meteo_data <- values$meteoData)
    isolate(headers <- values$headers)
    isolate(dataType <- values$dataType)
    
    ###XY graph
    if(input$graphType == "XY"){    
      req(input$col2ID != "Empty")
      if (dataType == "daily" || dataType == "half-hourly") {
        update_boundary_dates(min(levels(meteo_data[,1])), max(levels(meteo_data[,1]))) 
      } else {
        update_boundary_set(min(meteo_data[1]), max(meteo_data[1]))
      }
      
      startDate = input$boundary_date[1]
      req(startDate)
      endDate = input$boundary_date[2]
      req(endDate)
      req(endDate > startDate)
      all_dates = seq(startDate, endDate, 1); #all dates between startDate and endDate
      
      if (dataType == "daily" || dataType == "half-hourly") {
        meteo_data <- meteo_data[(as.Date(levels(meteo_data$time)) %in% all_dates),]
      } else {
        meteo_data <- subset(meteo_data, meteo_data$time %in% all_dates) #get only the values for dates, that are in all_dates
      }
      
      meteo_data <- meteo_data[order(meteo_data[input$col1ID]),]    #reorder to ascending order, required by dygraphs
      req(dim(meteo_data)[1] > 0)
      meteo_data <- removeInvalid(meteo_data)
      if (nrow(meteo_data) > 1000) graphPointSize <- 1 else graphPointSize <- 3
      
      graph <- dygraph(cbind(meteo_data[input$col1ID],meteo_data[input$col2ID])) %>%
        dyAxis("x", label = paste(input$col1ID, " [" , headers[2, input$col1ID], "]", sep = "")) %>%
        dyAxis("y", label = paste(input$col2ID, " [" , headers[2, input$col2ID], "]", sep = "")) %>%
        dyOptions(drawPoints = TRUE, pointSize = graphPointSize, strokeWidth = 0)
    }
    
    ###Time series graph
    else if(input$graphType == "Time series"){   
      reactiveTriggreHelper <- values$dataType
      req(input$graphSubtype)
      rownames(meteo_data) <- meteo_data[,1]
      req(input$col1ID != input$col2ID)
      meteo_data[,"Empty"] <- NA 
      
      meteo_data <- removeInvalid(meteo_data)
      if (dataType == "weekly" || dataType == "monthly") {  
        final <- cbind(meteo_data[,1],meteo_data[input$col1ID],meteo_data[input$col2ID])
      } else {
        final <- cbind(meteo_data[input$col1ID],meteo_data[input$col2ID])
      }
      
      graph <- dygraph(final) %>%   #graph options
        dySeries(axis = second_axis, input$col2ID) %>%
        dyOptions(animatedZooms = TRUE) %>%
        dyLegend(show = "always")
      
      if (input$col2ID == "Empty") {  #show only one y axis 
        graph <- graph %>% dySeries(input$col1ID) %>%
          dyAxis("y", label = paste(input$col1ID, " [" , headers[2, input$col1ID], "]", sep = ""), independentTicks  = TRUE)
      } else {
        if (is.null(second_axis)) {
          graph <- graph %>% dyAxis("y", label = paste(input$col1ID, " [" , headers[2, input$col1ID], "], ", input$col2ID, " [" , headers[2, input$col2ID], "]", sep = ""), independentTicks  = TRUE)
        } else {
          graph <- graph %>% dyAxis("y2", label = paste(input$col2ID, " [" , headers[2, input$col2ID], "]", sep = ""), independentTicks  = TRUE) %>%
            dyAxis("y", label = paste(input$col1ID, " [" , headers[2, input$col1ID], "]", sep = ""), independentTicks = TRUE) 
        }
      }
      if (input$graphSubtype == "Bar Graph") {      #show as bar graph
        graph <- graph %>% dyMultiColumn() %>%
          dyAxis("x", rangePad = 20)
      }
    }
    
    ###Settings applying to both graphs
    graph <- graph %>% dyRangeSelector()
    if (input$show_Xlabel == TRUE) {
      graph <- graph %>% dyEvent(input$x_axis_label, color = "red")
    }
    if (input$show_label == TRUE) {
      graph <- graph %>% dyLimit(input$y_axis_label, color = "red")
    }
    if(nrow(meteo_data) < 1000) {
      graph <- graph %>% dyHighlight(highlightCircleSize = 5)
    }
    return(graph)
  })
  
  ### Get location and type of input file
  getInputFile <- function() {  
    req(input$selectedFile)
    switch (input$selectedFile,
            "KRP daily data" = inFile <- c("./data/KRP16 daily data.csv", "daily"),
            "KRP half-hourly data" = inFile <- c("./data/KRP16 half-hourly data.csv", "half-hourly"),
            "KRP monthly data" = inFile <- c("./data/KRP16 monthly data.csv", "monthly"),
            "KRP weekly data" = inFile <- c("./data/KRP16 weekly data.csv", "weekly"),
            "RAJ daily data" = inFile <- c("./data/RAJ16 daily data.csv", "daily"),
            "RAJ half-hourly data" = inFile <- c("./data/RAJ16 half-hourly data.csv", "half-hourly"),
            "RAJ monthly data" = inFile <- c("./data/RAJ16 monthly data.csv", "monthly"),
            "RAJ weekly data" = inFile <- c("./data/RAJ16 weekly data.csv", "weekly"),
            "STI daily data" = inFile <- c("./data/STI16 daily data.csv", "daily"),
            "STI half-hourly data" = inFile <- c("./data/STI16 half-hourly data.csv", "half-hourly"),
            "STI monthly data" = inFile <- c("./data/STI16 monthly data.csv", "monthly"),
            "STI weekly data" = inFile <- c("./data/STI16 weekly data.csv", "weekly")
    )
    return(inFile) 
  }
  
  ### Remove invalid values
  removeInvalid <- function(meteo_data) {
    meteo_data[input$col1ID][meteo_data[input$col1ID] == -9999] <- NA
    meteo_data[input$col2ID][meteo_data[input$col2ID] == -9999] <- NA
    return(meteo_data)
  }
  
  ### Update boudnary dates
  update_boundary_dates <- function(from,to) {   
    if(is.null(input$boundary_date[1]) || is.na(input$boundary_date[1]) || input$boundary_date[1] == "1971-01-01") updateDateRangeInput(session, "boundary_date", start = from)
    if(is.null(input$boundary_date[2]) || is.na(input$boundary_date[2]) || input$boundary_date[2] == "1971-01-01") updateDateRangeInput(session, "boundary_date", end = to)
    req(input$boundary_date[1], input$boundary_date[2])
    if (input$boundary_date[1] > input$boundary_date[2]) {  #swap endDate and startDate if startDate > endDate
      tmp <- input$boundary_date[1]
      updateDateRangeInput(session, "boundary_date", start = input$boundary_date[2], end = tmp)
    }
    #this was above IFs in previous version, but for some reason in new version it wasn't working and has to be here
  }
  
  ### Update boudnary set
  update_boundary_set <- function(from,to) { 
    if(is.null(input$boundary_date[1]) || input$boundary_date[1] == -1) {
      updateSliderInput(session, "boundary_date", min = 1, max = to, value = c(1 , to))  
    }
    req(input$boundary_date[1], input$boundary_date[2])
  }
  
  ### Additional indormation
  output$point <- renderText({
    if (getInputFile()[2] == "half-hourly") {
      "<br><b>Tair-</b> Air temperature at the eddy-covariance measurement height <br>
      <b>Tsoil-</b> Soil temperature at the soil surface <br>
      <b>RH-</b> Relative humidity at the eddy-covariance measurement height <br> 
      <b>VPD-</b> Vapor pressure dificit at the eddy-covariance measurement height <br> 
      <b>P-</b> Precipitation at the eddy-covariance measurement height <br>
      <b>GR-</b> Global radiation <br>
      <b>Rn-</b> Net radiation <br>
      <b>PAR-</b> Photosynthetic active radiation <br> 
      <b>H-</b> Sensible heat flux <br>
      <b>LE-</b> Latent heat flux <br> 
      <b>NEE-</b> Net ecosystem exchange <br> 
      <b>Reco-</b> Ecosystem respiration <br>
      <b>GPP-</b> Gross primary production <br>"
    } else {
      "<br><b>Tair-</b> Mean air temperature at the eddy-covariance measurement height <br> 
      <b>Tsoil-</b> Mean soil temperature at the soil surface <br>
      <b>RH-</b> Mean relative humidity at the eddy-covariance measurement height <br>
      <b>VPD-</b> Mean vapor pressure dificit at the eddy-covariance measurement height <br>
      <b>P-</b> Sum of precipitation at the eddy-covariance measurement height <br>
      <b>GR-</b> Sum of global radiation <br> 
      <b>Rn-</b> Sum of net radiation <br>
      <b>PAR-</b> Sum of photosynthetic active radiation <br> 
      <b>H-</b> Sum of sensible heat flux <br>
      <b>LE-</b> Sum of latent heat flux <br> 
      <b>NEP-</b> Net ecosystem production <br>
      <b>Reco-</b> Sum of ecosystem respiration <br>
      <b>GPP-</b> Sum of gross primary production <br>"
    }
  })
  
  set_second_axis <- function(input){
    return(if(input == TRUE) NULL else 'y2')
  }
}
