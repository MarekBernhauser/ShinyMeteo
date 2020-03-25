library(shiny)
library(dygraphs)
source('ui.R', local = TRUE)
source('server.R')

shinyApp(ui = ui, server = server)
