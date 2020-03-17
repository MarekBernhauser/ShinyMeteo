library(shiny)
library(dygraphs)

### UI
ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      fileInput("boundaryFile", "Choose CSV File",
                accept = c(
                  "text/csv",
                  "text/comma-separated-values,text/plain",
                  ".csv")
      ),
      radioButtons("graphType","Graph type", c("XY", "Time")),
      selectInput("col1ID", label = "X-axis",
                  choices = c("Tair", "Tsoil", "RH", "VPD", "NEE_uStar_fqc", "P", "GR", "Rn", "PAR", "H_f", "LE_f", "Reco_uStar", "GPP_uStar_f"),
                  selected = "Tair"),
      selectInput("col2ID", label = "Y-axis",
                  choices = c("Tair", "Tsoil", "RH", "VPD", "NEE_uStar_fqc", "P", "GR", "Rn", "PAR", "H_f", "LE_f", "Reco_uStar", "GPP_uStar_f"),
                  selected = "Tsoil"),
      hr(),
      conditionalPanel(
        condition = "input.graphType == 'Time'",
        checkboxInput("single_axis", label = "Display on single y-axis", value = FALSE)
      ),
      conditionalPanel(
        condition = "input.graphType == 'XY'",
        dateRangeInput("boundary_date", label = "Date range", startview = "year", start = "0000-00-00", end = "0000-00-00")
      ),
      checkboxInput("show_label", label = "Show label", value = FALSE),
      conditionalPanel(
        condition = "input.show_label == 1",
        numericInput("y_axis_label", label = "Value", 
                     value = NULL, min = -9999, max = 9999, step = 1)
      ),
      hr(),
      div(strong("Additional information: "), textOutput("point", inline = TRUE))
    ),
    mainPanel(
      dygraphOutput("plot")
    )
  )
)

server <- function(input, output, clientData, session) {
  ### Plot Create
  output$plot <- renderDygraph({
    second_axis <- set_second_axis(input$single_axis)  #'y2', or NULL if no second axis
    
    inFile <- input$boundaryFile
    req(inFile)
    
    if(input$graphType == "XY"){    #XY graph
    headers = read.csv(inFile$datapath, sep = ",", header = FALSE, nrows = 1, as.is = TRUE)
    meteo_data <- read.csv(inFile$datapath, sep = ",", header = FALSE, skip = 2)
    colnames(meteo_data) = headers
    
    update_boundary_dates(min(levels(meteo_data[,1])), max(levels(meteo_data[,1]))) 
    startDate = input$boundary_date[1]
    req(startDate)
    endDate = input$boundary_date[2]
    req(endDate)
    req(endDate > startDate)
    
    all_dates = seq(startDate, endDate, 1); #all dates between startDate and endDate
    
    meteo_data <- meteo_data[(as.Date(levels(meteo_data$day)) %in% all_dates),] #get only the values for dates, that are in all_dates
    meteo_data <- meteo_data[order(meteo_data[input$col1ID]),]    #reorder to ascending order, required by dygraphs
    req(dim(meteo_data)[1] > 0)
    
    dygraph(cbind(meteo_data[input$col1ID],meteo_data[input$col2ID])) %>%
      dyAxis("x", label = input$col1ID) %>%
      dyAxis("y", label = input$col2ID) %>%
      dyOptions(drawPoints = TRUE, pointSize = 3, strokeWidth = 0, animatedZooms = TRUE) %>%
      dyHighlight(highlightCircleSize = 5) %>%
      dyLimit(input$y_axis_label, color = "red")
    }
    else if(input$graphType == "Time"){   #Time graph
      headers = read.csv(inFile$datapath, sep = ",", header = FALSE, nrows = 1, as.is = TRUE, row.names = 1)
      meteo_data <- read.csv(inFile$datapath, sep = ",", header = FALSE, row.names = 1, skip = 2)
      colnames(meteo_data) = headers
      
      dygraph(cbind(meteo_data[input$col1ID],meteo_data[input$col2ID])) %>%
        dyAxis("y", label = input$col2ID, independentTicks  = TRUE) %>%
        dyAxis("y2", label = input$col1ID, independentTicks = TRUE) %>%
        dySeries(input$col1ID, axis = second_axis) %>%
        dyOptions(animatedZooms = TRUE) %>%
        dyHighlight(highlightCircleSize = 5, highlightSeriesOpts = list(strokeWidth = 2)) %>%
        dyLegend(show = "always") %>%
        dyLimit(input$y_axis_label, color = "red")
    }
  })
  
  ### Point Click
  output$point <- renderText({
    inFile <- input$boundaryFile
    req(input$plot_click)
    meteo_data <- read.csv(inFile$datapath, sep = ",", header = TRUE)
    meteo_data <- meteo_data[-1,]
    if(!is.numeric(input$plot_click$x_closest_point)){  #if graphType is Time, closest point is day, and time comes out of nowhere, so it has to be removed
      split_string <- unlist(strsplit(input$plot_click$x_closest_point, "T"))   #spliting date from time
      my_row <- which(meteo_data$day == split_string[1], arr.ind = TRUE) #looking up matching day
      
    } else{
      my_row <- which(meteo_data[input$col1ID] == input$plot_click$x_closest_point) #looking up matching rows  
    }
    my_string <- vector()
    
    for (weird_row_num in my_row) {
      my_rown_numb <- rownames(meteo_data)[weird_row_num] #renames weird row number to actual row number
      singleRow <- meteo_data[my_rown_numb,] #gets the entire row

      tmp_string <- paste("Date= ", singleRow$day, "; Tair= ", singleRow$Tair, "; Tsoil= ", singleRow$Tsoil, "; RH= ", singleRow$RH,
                          "; VPD= ", singleRow$VPD, "; NEE_uStar_fqc= ", singleRow$NEE_uStar_fqc, "; P= ", singleRow$P, "; GR= ", singleRow$GR, "; Rn= ", singleRow$Rn, "; PAR= ", singleRow$PAR,
                          "; H_f= ", singleRow$H_f, "; LE_f= ", singleRow$LE_f, "; Reco_uStar= ", singleRow$Reco_uStar, "; GPP_uStar_f= ", singleRow$GPP_uStar_f)
      
      my_string <- append(my_string, tmp_string)
    }
    paste0(my_string)
  })
  
  ### Remove label
  observe({
    if(input$show_label == FALSE) {
      updateNumericInput(session, "y_axis_label", value = -9999)  
    }
  })
  
  ### Update boudnary dates
  update_boundary_dates <- function(from,to) {   
    updateDateRangeInput(session, "boundary_date", min = from, max = to)
    if(is.na(input$boundary_date[1])) updateDateRangeInput(session, "boundary_date", start = from)
    if(is.na(input$boundary_date[2])) updateDateRangeInput(session, "boundary_date", end = to)
    req(input$boundary_date[1] && input$boundary_date[2])
    if (input$boundary_date[1] > input$boundary_date[2]) {  #swap endDate and startDate if startDate > endDate
      tmp <- input$boundary_date[1]
      updateDateRangeInput(session, "boundary_date", start = input$boundary_date[2], end = tmp)
    }
  }
}

set_second_axis <- function(input){
  return(if(input == TRUE) NULL else 'y2')
}

### App End
shinyApp(ui = ui, server = server)

