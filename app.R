library(shiny)
library(dygraphs)
library(shinydashboard)
source('ui.R', local = TRUE)
source('server.R')

shinyApp(ui = ui, server = server)
