##############################################
################# Shiny App Server ###########
##############################################

#library(dplyr)
library(shiny)
library(shinyjs)        # improve user experience with JavaScript
library(shinyauthr)
# library(shinymanager)   # shiny authentication modules
# library(sodium)

# section 1.1 - source module ----
source("FoodSelection.R") 

options(error = function() {
  sink(stderr())
  on.exit(sink(NULL))
  traceback(15, max.lines = 1L)
  if (!interactive()) {
    q(status = 1)
  }
})

# extract username and password from heroku env
admin_user <- Sys.getenv("ADMIN_USERNAME")
admin_password <- Sys.getenv("ADMIN_PASSWORD")

# dataframe that holds usernames, passwords and other user data
user_base <- data.frame(
  user = c(admin_user),
  password = c(admin_password), 
  permissions = c("admin"),
  name = c("User One"),
  stringsAsFactors = FALSE,
  row.names = NULL
)

server <- function(input, output, session) {
  
  # call the logout module with reactive trigger to hide/show
  logout_init <- callModule(shinyauthr::logout, 
                            id = "logout", 
                            active = reactive(credentials()$user_auth))
  
  # call login module supplying data frame, user and password cols
  # and reactive trigger
  credentials <- callModule(shinyauthr::login, 
                            id = "login", 
                            data = user_base,
                            user_col = user,
                            pwd_col = password,
                            log_out = reactive(logout_init()))
  
  
  observeEvent(credentials()$user_auth, {
    if (credentials()$user_auth == TRUE) {
      shinyjs::show(id = "generate")
      shinyjs::show(id = "dels")
    } else {
      shinyjs::hide(id = "generate")
      shinyjs::hide(id = "dels")
    }
  })
  
  x1 <-eventReactive(input$generate, {
    
    iframe_url <- paste("https://docs.google.com/spreadsheets/d/",
            generate_delivery_roster(input, output, session, input$dels), sep = "")
  })
  
  output$frame <- renderUI({
    req(credentials()$user_auth)
    iframe <- tags$iframe(src=x1(), height=1024, width="100%", frameborder = "yes")
    iframe
  })
}
