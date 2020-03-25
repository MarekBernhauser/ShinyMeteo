###UI
ui <- dashboardPage(
  dashboardHeader(title = "Visualization of eddy-covariance data",titleWidth = 350),
  
  dashboardSidebar(
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
    uiOutput("showNumSelect"),
    
    checkboxInput("show_label", label = "Highlite y-axis value", value = FALSE),
    conditionalPanel(
      condition = "input.show_label == 1",
      numericInput("y_axis_label", label = "Value", value = NULL)
    ),
    checkboxInput("show_Xlabel", label = "Highlite x-axis value", value = FALSE),
    conditionalPanel(
      condition = "input.show_Xlabel == 1",
      uiOutput("xLabel")
    ),
    
    hr(),
    div(strong("Additional information: "), htmlOutput("point", inline = TRUE), style= "margin : 10px")
  ),
  dashboardBody(
    #tags$head(tags$style(
    #HTML('.wrapper {height: auto !important; position:relative; overflow-x:hidden; overflow-y:hidden}'))),
    dygraphOutput("plot")
  )
)
