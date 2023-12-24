library(shiny)

options(shiny.port = 8080)

source('ui.R', local = F)
source('server.R', local = F)

shinyApp(ui = ui, server = server)
