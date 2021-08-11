library(shiny)
library(dplyr)

# Read data file
library(readxl)
Nutrition <- read_excel("Nutrition.xlsx")

# Define UI for dataset viewer app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("The World's Most Nutritious Foods"),
  
  # Sidebar layout with a input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Selector for choosing dataset ----
      selectInput(inputId = "category",
                  label = "Choose a food category:",
                  choices = c("All", "Protein-rich", "Fat-rich", "Carbohydrate-rich")),
      
      # Input: Variable to filter by ----
      selectInput(inputId = "sort",
                  label = "Select variable to sort by",
                  choices = c("Nutrition Score", "Price")),
      
      # Input: Number of observations to view ----
      sliderInput(inputId = "obs",
                  label = "Number of observations to view:",
                  min = 0, max = 50,
                  value = 10)
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Verbatim text for data summary ----
      verbatimTextOutput("summary"),
      
      # Output: HTML table with requested number of observations ----
      tableOutput("view")
      
    )
  )
)

# Define server logic to summarize and view selected dataset ----
server <- function(input, output) {
  
  # Return the requested dataset ----
  categoryInput <- reactive({
    switch(input$category,
           "All" = c("Protein-rich", "Fat-rich", "Carbohydrate-rich"),
           "Protein-rich" = "Protein-rich",
           "Fat-rich" = "Fat-rich",
           "Carbohydrate-rich" = "Carbohydrate-rich"
    )
  })
  
  # Return the variable to be sorted by
  sortSelector <- reactive({
    switch(input$sort,
           "Nutrition Score" = "Nutritional_Fitness",
           "Price" = "Price_Per_100g")
  })
  
  # Generate a summary of the dataset ----
  output$summary <- renderPrint({
    new_dataset <- Nutrition %>% filter(Category == categoryInput())
    new_dataset <- new_dataset %>% select(Nutritional_Fitness, Price_Per_100g)
    summary(new_dataset)
  })
  
  # Show the first "n" observations ----
  output$view <- renderTable({
    new_dataset <- Nutrition %>% filter(Category == categoryInput()) 
    new_dataset <- new_dataset %>% select(Name, Nutritional_Fitness, Price_Per_100g)
    new_dataset <- new_dataset %>% arrange(desc(get(sortSelector())))
    head(new_dataset, n = input$obs)
  })
}

shinyApp(ui, server)