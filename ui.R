##############################################
################# Shiny App UI ###############
##############################################

library(shiny)
library(shinyjs)        # improve user experience with JavaScript
library(shinyauthr)

ui <- fluidPage(
  # must turn shinyjs on
  shinyjs::useShinyjs(),
  # add logout button UI
  div(class = "pull-right", logoutUI(id = "logout")),
  # add login panel UI function
  loginUI(id = "login"),
  
  textInput("dels", "Phone number for delivery"),
  actionButton("generate", "Generate spreadsheet"),
  mainPanel(fluidRow(htmlOutput("frame"))),
  tags$head(
    HTML(
      "
          <script>
          var socket_timeout_interval
          var n = 0
          $(document).on('shiny:connected', function(event) {
          socket_timeout_interval = setInterval(function(){
          Shiny.onInputChange('count', n++)
          }, 15000)
          });
          $(document).on('shiny:disconnected', function(event) {
          clearInterval(socket_timeout_interval)
          });
          </script>
          "
    )
  ),
)
