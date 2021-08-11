library(shiny)
library(readxl)
library(ggplot2)
library(shinythemes)
ui = tagList(
  fluidPage(
    theme = shinythemes::shinytheme("superhero"),      #("sandstone"),
    titlePanel("Set Your Goal"),
    sidebarPanel(
      #User profile
      h3("User Profile"),
      textInput("name", "Name", placeholder = "Harshul Garg"),
      selectInput("gender", "Gender",
                  list("Male", "Female", "Other")
      ),
      numericInput("height", "Height", NULL , min = 10, max = 1000),
      numericInput("weight", "Weight", NULL , min = 10, max = 1000),
      #Goal Setting
      h3("Goal Setting"),
      numericInput("weight_goal", "Desired Weight", NULL , min = 10, max = 1000),
      checkboxInput("bmi", "Check this if you want us to set your goal based on your BMI index", FALSE),
      #Data intake
      h3("Upload Health Data:"),
      fileInput("user_data", "File upload:"),
      #Action Button
      actionButton("action", "Submit", class = "btn-primary")
      
      #sample code
      #sliderInput("slider", "Slider input:", 1, 100, 30),
      #$h5("Default actionButton:"),
      #actionButton("action", "Search"),
      #tags$h5("actionButton with CSS class:"),
    ),
    mainPanel(
      tabsetPanel(
        #Sample code
        # tabPanel("Tab 1",
        #          h4("Table"),
        #          tableOutput("table"),
        #          h4("Verbatim text output"),
        #          verbatimTextOutput("txtout"),
        #          h1("Header 1"),
        #          h5("Header 5")
        # ),
        tabPanel("Projections", 
                 h1(textOutput("welcome")),
                 h4(textOutput("goal_txt")),
                 #tableOutput("table"),
                 h3(textOutput("txt_plot1")),
                 plotOutput("plot1"),
                 h3(textOutput("txt_plot2"))
        ),
        tabPanel("Tab 3", "This panel is intentionally left blank")
      )
    )
  )
)

server = function(input, output) {
  # output$txtout <- renderText({
  #   paste(input$txt, input$slider, format(input$date), sep = ", ")
  # })
  # output$table <- renderTable({
  #   head(cars, 4)
  # })
  #Welcome text
  observeEvent(input$action,
               output$welcome <- renderText({
                 paste("Welcome,", isolate(input$name), "!!!")
               })
  )
  #weight Goal Text
  observeEvent(
    input$action,
    output$goal_txt <- renderText({
      paste("We will help you to achieve your goal of ", isolate(input$weight_goal), " pounds.")
    })
  )
  #Playing with data
  df <- reactive({
    read_excel(
      input$user_data$datapath, sheet = "dailyActivity"
    )
  })
  # observeEvent(
  #   input$action,
  #   output$table <- renderTable({
  #     return(df()[,c("ActivityDate","Calories")])
  #   })
  # )
  #Plot daily calories burnt
  observeEvent(input$action,
               output$txt_plot1 <- renderText({
                 paste("Calories burnt this month:")
               })
  )
  observeEvent(
    input$action,
    output$plot1 <- renderPlot(
      ggplot(df(), aes(x = ActivityDate, y = Calories)) + 
        geom_point() +
        stat_smooth(method = "lm", col = "red"),
      width = "auto",
      height = "auto",
      res = 72
    )
  )
  #Plot of future projections
  observeEvent(input$action,
               output$txt_plot2 <- renderText({
                 paste("Projection for the rest of the month:")
               })
  )
}

shinyApp(ui = ui, server = server)
