##############################################
################# Shiny App UI ###############
##############################################

#library(dplyr)
library(shiny)
library(shinyjs)        # improve user experience with JavaScript
library(shinyauthr)
# library(shinymanager)   # shiny authentication modules
# library(sodium)

ui <- fluidPage(
  # must turn shinyjs on
  shinyjs::useShinyjs(),
  # add logout button UI 
  div(class = "pull-right", logoutUI(id = "logout")),
  # add login panel UI function
  loginUI(id = "login"),
  
  textInput("dels", "Phone number for delivery"),
  actionButton("generate", "Generate spreadsheet"),
  mainPanel(fluidRow(
    htmlOutput("frame")
  )
  )
)

# shinyApp(ui = ui, server = server)
