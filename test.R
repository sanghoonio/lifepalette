library(shiny)
library(firebase)

# define signin
signin <- modalDialog(
  title = "Login",
  actionButton("google", "Google", icon = icon("google"), class = "btn-danger"),
  footer = NULL
)

ui <- fluidPage(
  useFirebase(),
  plotOutput("plot")
)

server <- function(input, output) {
  showModal(signin)
  
  f <- FirebaseSocial$new(config_path = 'data/firebase.rds')
  
  observeEvent(input$google, {
    f$launch_google(flow = 'redirect')
  })
  
  observe({
    f$req_sign_in()
    removeModal()
  })
  
  output$plot <- renderPlot({
    f$req_sign_in()
    plot(cars)
  })
  
  observeEvent(f$get_signed_in(), {
    user <- f$get_signed_in()$response$email
    print(user)
  })
}

shinyApp(ui, server)
