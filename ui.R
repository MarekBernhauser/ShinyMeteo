library(shiny)
library(dygraphs)
library(shinydashboard)

###UI
ui <- dashboardPage(skin = "purple",
  dashboardHeader(title = "Visualization of eddy-covariance data",titleWidth = 350),
  
  dashboardSidebar(
    tags$head( tags$script(type="text/javascript",'$(document).ready(function(){
                             $(".main-sidebar").css("height","100%");
                             $(".main-sidebar .sidebar").css({"position":"relative","max-height": "100%","overflow": "auto"})
                             })')),
    width = 350,
    div(style= "margin : 10px", h4("Data from three different ecosystem stations for year 2016 are available in four temporal resolutions")),
    selectInput("stationType", label = "Select ecosystem station", 
                choices = c("Agroecosystem at Křešín u Pacova with crops harvested during the growing season",
                            "Evergreen needleleaf forest at Rájec-Jestřebí representing monoculture of Norway spruce",
                            "Deciduous broadleaf forests at Štítná nad Vláří representing monoculture of European beech")),
    uiOutput("station"),
    radioButtons("graphType","Graph type", c("XY", "Time series")),
    uiOutput("barGraphChoiceUI"),
    uiOutput("col1"),
    uiOutput("col2"),
    
    hr(),
    conditionalPanel(
      condition = "input.graphType == 'Time series'",
      checkboxInput("single_axis", label = "Display on single y-axis", value = FALSE)
    ),
    
    uiOutput("showTimeDateSelect"),

    checkboxInput("show_label", label = "Highlit y-axis value", value = FALSE),
    conditionalPanel(
      condition = "input.show_label == 1",
      numericInput("y_axis_label", label = "Value", value = NULL)
    ),
    checkboxInput("show_Xlabel", label = "Highlit x-axis value", value = FALSE),
    conditionalPanel(
      condition = "input.show_Xlabel == 1",
      uiOutput("xLabel")
    ),
    
    ### advanced filtering 
    div(style= "display:inline-block;width:32%;margin:0px;padding:0px;",uiOutput("allInputs")),
    div(style= "display:inline-block;width:32%;margin:0px;padding:0px;",uiOutput("center")),
    div(style= "display:inline-block;width:32%;margin:0px;padding:0px;",uiOutput("right")),
    div(style= "display:inline-block;text-align: center;",actionButton("appendInput", "Add")),
    div(style= "display:inline-block;text-align: center;",actionButton("removeInput", "Remove"))
    
  ),
  
  dashboardBody(
    fluidRow(
      box(status = "primary", width = 10000,
        dygraphOutput("plot")
      ),
      tabBox(
        tabPanel("Axis Information", htmlOutput("point", inline = TRUE)),
        tabPanel("Locality Information", htmlOutput("localityInfo", inline = TRUE))
      )
    )
  )
)
