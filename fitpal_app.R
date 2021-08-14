######################### R Libraries Required #################################
################################################################################

library(shiny)
library(shinyWidgets)
library(shinydashboard)
library(dashboardthemes)
library(openxlsx)
library(readxl)
library(dplyr)
library(tidyr)
library(reshape2)
library(ggplot2)
library(lubridate)
library(sjlabelled)
library(lessR)
library(tidyverse)
library(zoo)
library(forecast)
############################### MAIN APP #######################################

ui <- dashboardPage(
  # dashboard title
  title = "Fitpal - Fitness Tracker",
  dashboardHeader(
    title = shinyDashboardLogo(
      theme = "purple_gradient",
      boldText = "FitPal:",
      mainText = "Fitness Tracker",
      badgeText = "v1.1"
    )),
  
  dashboardSidebar(
    sidebarUserPanel(name = textOutput("text1"),
                     subtitle = a(href = "#", 
                                  icon("circle", 
                                       class = "text-success"), 
                                  "Online")
    ),
    sidebarMenu(
      id = "tabs",
      # upload data & demographic information
      menuItem(text = "Update User Profile", tabName = "user_profile", 
               icon = icon("id-card")),
      # data exploratory analysis & activity review
      menuItem(text = "View Activity Dashboard", tabName = "dashboard", 
               icon = icon("dashboard")),
      # set goals
      menuItem(text = "Set Your Goals", tabName = "change_goals", icon = 
                 icon("bullseye")),
      # view progress
      menuItem(text = "Track Your Progress", icon = icon("bar-chart-o"),
               menuSubItem("Activity", tabName = "subitem1"),
               menuSubItem("Weight Loss", tabName = "subitem2")
      )
    ),
    width = "250px",
    collapsed = FALSE
  ),
  
  dashboardBody(
    # upload theme
    shinyDashboardThemes(
      theme = "blue_gradient"),
    tabItems(
      tabItem(
        # User profile & demographic input        
        tabName = "user_profile",
        h3("User Profile", tagList(shiny::icon("id-card"))),
        fluidRow(
          box(textInput("name", "Name", placeholder = "Enter Your Name")),
          box(selectInput("gender", "Select Your Gender",
                          list("Male", "Female", "Transgender",
                               "Non-binary","Prefer Not To Respond")
          )
          )
        ),
        fluidRow(
          box(numericInput("height", "Enter Your Height (in)",
                           value = 60 , min = 20, max = 110, step = 1)),
          box(numericInput("weight", "Enter Your Weight (lb)", 
                           value = 170 , min = 5, max = 700, step = 1))
        ),
        # Data Upload or Data Sample Selection
        h3("Health Data", tagList(shiny::icon("table"))),
        fluidRow(
          box(fileInput(inputId = "user_data", 
                        label = "File upload:",
                        accept = ".xlsx"),
              # Use uploaded data set
              actionButton(
                inputId = "use_uploaded",
                label = "Use Uploaded File",
                class = "btn-primary")
          ),
          # choose sample data set sheet to preview
          box(selectInput("sample_data",
                          "Choose a sample sheet",
                          choices = c("Hourly Steps",
                                      "Hourly Calories",
                                      "Hourly Intensities",
                                      "Hourly Stand",
                                      "Daily Steps",
                                      "Daily Intensities",
                                      "Daily Activity",
                                      "Sleep Minutes",
                                      "Daily Sleep",
                                      "Daily Stand")),
              # Download 'template' / sample data set
              downloadButton("download_sample",
                             "Download Sample Data"),
              # Use sample data set
              actionButton(
                inputId = "use_sample",
                label = "Use Sample Data",
                class = "btn-primary")
          ),
          # preview sample data set sheet selected
          box(tableOutput("sample_table"), width = 12)
        )
      ),
      tabItem(
        # Review BMI graph and Set Goals     
        tabName = "change_goals",
        # BMI Graph
        h3("BMI Spectrum", tagList(shiny::icon("hand-holding-heart"))),
        fluidRow(
          box(
            plotOutput(outputId = "bmiplot"), width = 12
          )
        ),
        # Goal Setting
        h3("Set Your Goals", tagList(shiny::icon("bullseye"))),
        fluidRow(
          box(
            knobInput(
              inputId = "steps_goal",
              label = "Daily Steps",
              value = 9000,
              min = 0,
              max = 100000,
              displayPrevious = TRUE,
              lineCap = "round",
              fgColor = "#428BCA",
              inputColor = "#428BCA"
            )
          ),
          box(
            knobInput(
              inputId = "stand_goal",
              label = "Daily Standing Hours",
              value = 6,
              min = 0,
              max = 24,
              displayPrevious = TRUE,
              lineCap = "round",
              fgColor = "#428BCA",
              inputColor = "#428BCA"
            )
          ),
          box(
            knobInput(
              inputId = "exercise_goal",
              label = "Daily Calories Burnt",
              value = 1800,
              min = 0,
              max = 4000,
              displayPrevious = TRUE,
              lineCap = "round",
              fgColor = "#428BCA",
              inputColor = "#428BCA"
            )
          ),
          box(
            knobInput(
              inputId = "sleep_goal",
              label = "Daily Sleep Hours",
              value = 8,
              min = 0,
              max = 24,
              displayPrevious = TRUE,
              lineCap = "round",
              fgColor = "#428BCA",
              inputColor = "#428BCA"
            )
          )
        ),
        # Update goals
        actionButton(
          inputId = "set_goal", 
          label = "Update Goals", 
          class = "btn-primary"),
        
        h3("Start Your Weight Loss Journey", 
           tagList(shiny::icon("weight"))),
        fluidRow(
          box(
            knobInput(
              inputId = "weight_goal",
              label = "Set weight desired at the end of the month(lb)",
              value = 127,
              min = 60,
              max = 204,
              displayPrevious = TRUE,
              lineCap = "round",
              fgColor = "#428BCA",
              inputColor = "#428BCA"
            )
          )
        ),
        actionButton(
          inputId = "set_weight_goal", 
          label = "Update Weight Goal", 
          class = "btn-primary")
      ),
      tabItem(
        # Activity Dashboard & Exploratory Analysis
        tabName = "dashboard",
        h3("Activity Dashboard", tagList(shiny::icon("tachometer-alt"))),
        fluidRow(
          # tab for steps analysis
          tabBox(
            title = tagList(shiny::icon("walking"), "Steps"),
            id = "steps", height = "auto",
            tabPanel(
              title = "Hourly", 
              plotOutput(outputId = "hourlysteps")),
            tabPanel(
              title = "Weekly", 
              plotOutput(outputId = "dailysteps")),
            tabPanel(title = "Historic", 
                     plotOutput(outputId = "historicsteps"))
          ),
          # tab for calories burnt analysis
          tabBox(
            title = tagList(shiny::icon("running"), "Calories"),
            id = "cals", height = "auto",
            tabPanel(
              title = "Hourly", 
              plotOutput(outputId = "hourlycalories")),
            tabPanel(
              title = "Weekly", 
              plotOutput(outputId = "weeklycalories")),
            tabPanel(
              title = "Historic", 
              plotOutput(outputId = "historiccalories"))
          ),
          # tab for intensity analysis
          tabBox(
            title = tagList(shiny::icon("dumbbell"), "Intensity"),
            id = "intensity", height = "auto",
            tabPanel(
              title = "Daily", 
              plotOutput(outputId = "dailyintensity")),
            tabPanel(
              title = "Weekly", 
              plotOutput(outputId = "weeklyintensity")),
            tabPanel(
              title = "Historic", 
              plotOutput(outputId = "historicintensity"))
          ),
          # tab for distance analysis
          tabBox(
            title = tagList(shiny::icon("road"), "Distance"),
            id = "distance", height = "auto",
            tabPanel(
              title = "Daily", 
              plotOutput(outputId = "dailydist")),
            tabPanel(
              title = "Weekly", 
              plotOutput(outputId = "weeklydist")),
            tabPanel(
              title = "Historic", 
              plotOutput(outputId = "historicdist"))
          ),        
          # tab for sleep analysis
          tabBox(
            title = tagList(shiny::icon("bed"), "Sleep"),
            id = "sleep", height = "auto", width = 12,
            tabPanel(
              title = "Daily", 
              plotOutput(outputId = "dailysleep")),
            tabPanel(
              title = "Weekly", 
              plotOutput(outputId = "weeklysleep")),
            tabPanel(
              title = "Historic", 
              plotOutput(outputId = "historicsleep"))
          )
        ),
      ),
      # DELETE?
      tabItem(
        tabName = "subitem1",
        h3("Don't Break The Chain", tagList(shiny::icon("calendar-check"))),
        fluidRow(
          box(
            h4("Historical Goal Completion"),
            plotOutput(outputId = "BreakChain")
          ),
          box(
            h4("Streak Count"),
            plotOutput(outputId = "activity_streak")
          )
        ),
        h3("Daily Tracker", tagList(shiny::icon("watch-fitness"))),
        fluidRow(
          box(
            h4("Daily Steps Goal"),
            plotOutput(outputId = "step_goal_plot")
          ),
          box(
            h4("Daily Stand Goal"),
            plotOutput(outputId = "stand_goal_plot")
          ),
          box(
            h4("Daily Excercise Goal"),
            plotOutput(outputId = "calories_goal_plot")
          )
        )
      ),
      tabItem(
        tabName = "subitem2",
        h3("Historical Weight", tagList(shiny::icon("dumbbell"))),
        fluidRow(
          box(
            plotOutput(outputId = "weightLogInfo_plot") , width = 12
          )
        ),
        h3("Weight Loss Tracker", tagList(shiny::icon("bullseye"))),
        fluidRow(
          box(
            valueBoxOutput("calories", width = 12) ,width = 12
          ),
          box(
            valueBoxOutput("Target", width = 6),
            valueBoxOutput("Prediction", width = 6),width = 12
          )
        )
      )
    ) # tab items
  ) # end body
) # end dash page


server <- function(input, output, session) {
  
  ############################# USER PROFILE #####################################
  
  sample_Input <- reactive({
    input$use_sample
    # select data set to show in preview
    switch(input$sample_data,
           "Hourly Steps" = as.data.frame(read_excel("Joined_Dataset_V2.xlsx",
                                                     sheet="hourlySteps",
                                                     col_name=T)),
           "Hourly Calories" = as.data.frame(read_excel("Joined_Dataset_V2.xlsx",
                                                        sheet="hourlyCalories",
                                                        col_name=T)),
           "Hourly Intensities" = as.data.frame(read_excel("Joined_Dataset_V2.xlsx",
                                                           sheet="hourlyIntensities",
                                                           col_name=T)),
           "Hourly Stand" = as.data.frame(read_excel("Joined_Dataset_V2.xlsx",
                                                     sheet="hourlyStand",
                                                     col_name=T)),
           "Daily Steps" = as.data.frame(read_excel("Joined_Dataset_V2.xlsx",
                                                    sheet="dailySteps",
                                                    col_name=T)),
           "Daily Intensities" = as.data.frame(read_excel("Joined_Dataset_V2.xlsx",
                                                          sheet="dailyIntensities",
                                                          col_name=T)),
           "Daily Activity" = as.data.frame(read_excel("Joined_Dataset_V2.xlsx",
                                                       sheet="dailyActivity",
                                                       col_name=T)),
           "Sleep Minutes" = as.data.frame(read_excel("Joined_Dataset_V2.xlsx",
                                                      sheet="minuteSleep",
                                                      col_name=T)),
           "Daily Sleep" = as.data.frame(read_excel("Joined_Dataset_V2.xlsx",
                                                    sheet="sleepDay",
                                                    col_name=T)),
           "Daily Stand" = as.data.frame(read_excel("Joined_Dataset_V2.xlsx",
                                                    sheet="standDay",
                                                    col_name=T))
    )
  })
  
  # sample data set sheet 
  output$sample_table <- renderTable({
    head(sample_Input()[1:ncol(sample_Input())], n = 10)
  })
  
  # download template/sample data
  sample <- loadWorkbook("Joined_Dataset_V2.xlsx")
  
  output$download_sample <- downloadHandler(
    filename = function() {
      file <- "FitPal_Sample-Dataset_Template.xlsx"    
    },
    content = function(file) {
      # save all workbook including sheets
      saveWorkbook(sample, file = file, overwrite = TRUE)
    }
  )
  
  ########################## ACTIVITY DASHVBOARD #################################
  
  # Hourly Steps
  hourlySteps <- reactive({
    c(input$use_uploaded, input$use_sample)
    # prepare the hourly steps data from either uploaded or sample data    
    if (is.null(input$user_data) == F & is.null(input$use_uploaded) == F) {
      hsteps <- read_excel(input$user_data$datapath, sheet = "hourlySteps")
    }
    else if (is.null(input$user_data) == T | is.null(input$use_sample) == F) {
      hsteps <- read_excel("Joined_Dataset_V2.xlsx", sheet = "hourlySteps")
    }
    hsteps <- as.data.frame(hsteps)
    print(str(hsteps))
    hsteps$ActivityHour <- as.POSIXct(hsteps$ActivityHour, format="%Y-%m-%d")
    hsteps<- subset(hsteps, ActivityHour> "2016-05-12")
    hsteps
  })
  
  # hourly steps graph
  observeEvent(
    c(input$use_uploaded, input$use_sample),
    ignoreInit = T,
    output$hourlysteps <- renderPlot(
      hourlySteps() %>%
        group_by(ActivityHour) %>%
        summarize(StepTotal = mean(StepTotal)) %>%
        ggplot(aes(x=ActivityHour, y= StepTotal, fill = StepTotal))+
        geom_col()+
        scale_fill_continuous(low = '#05fbd7', high = "#17667b") +
        labs(title = "Hourly Steps") + 
        theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
              axis.text = element_text(size = 12),
              axis.title = element_text(size = 12, face = "bold"),
              panel.background = element_rect(fill = "white"),
              axis.line = element_line(size = 1, colour = "black"),
              panel.grid.major = element_line(colour = "#f4f4f4"))
    )
  )
  
  # Daily Steps
  dailySteps <- reactive({
    c(input$use_uploaded, input$use_sample)
    # prepare the daily steps data from either uploaded or sample data
    if (is.null(input$user_data) == F & is.null(input$use_uploaded) == F) {
      dsteps <- read_excel(input$user_data$datapath, sheet = "dailySteps")
    }
    else if (is.null(input$user_data) == T | is.null(input$use_sample) == F) {
      dsteps <- read_excel("Joined_Dataset_V2.xlsx", sheet = "dailySteps")
    }
    dsteps <- as.data.frame(dsteps)
    print(str(dsteps))
    dsteps$ActivityDay <- as.POSIXct(dsteps$ActivityDay, format = "%Y-%m-%d")
    dsteps <- subset(dsteps, ActivityDay > "2016-05-05")
    dsteps
  })
  
  # daily steps graph
  observeEvent(
    c(input$use_uploaded, input$use_sample),
    ignoreInit = T,
    output$dailysteps <- renderPlot(
      ggplot(data = dailySteps(), aes(x = ActivityDay, y = StepTotal)) +
        geom_bar(stat = "identity", fill = "#27839a") +
        labs(title = "Weekly Steps",
             x = "Date", y = "Weekly Steps") +
        theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
              axis.text = element_text(size = 12),
              axis.title = element_text(size = 12, face = "bold"),
              panel.background = element_rect(fill = "white"),
              axis.line = element_line(size = 1, colour = "black"),
              panel.grid.major = element_line(colour = "#f4f4f4"))
    )
  )
  
  # Historic Steps
  historicSteps <- reactive({
    c(input$use_uploaded, input$use_sample)
    # prepare the historic steps data from either uploaded or sample data
    if (is.null(input$user_data) == F & is.null(input$use_uploaded) == F) {
      histsteps <- read_excel(input$user_data$datapath, sheet = "dailySteps")
    }
    else if (is.null(input$user_data) == T | is.null(input$use_sample) == F) {
      histsteps <- read_excel("Joined_Dataset_V2.xlsx", sheet = "dailySteps")
    }
    histsteps <- as.data.frame(histsteps)
    print(str(histsteps))
    histsteps
  })
  
  # historic steps graph
  observeEvent(
    c(input$use_uploaded, input$use_sample),
    ignoreInit = T,
    output$historicsteps <- renderPlot(
      ggplot(data = historicSteps(), aes(x = ActivityDay, y = StepTotal)) +
        geom_line(color = "#2cdeeb", size = 2) + 
        xlab("Date")+
        ylab("Total Steps")+
        labs(title = "Historic Steps") + 
        theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
              axis.text = element_text(size = 12),
              axis.title = element_text(size = 12, face = "bold"),
              panel.background = element_rect(fill = "white"),
              axis.line = element_line(size = 1, colour = "black"),
              panel.grid.major = element_line(colour = "#f4f4f4"))
    )
  )
  
  # Hourly Calories
  hourlyCalories <- reactive({
    c(input$use_uploaded, input$use_sample)
    # prepare the hourly calories data from either uploaded or sample data
    if (is.null(input$user_data) == F & is.null(input$use_uploaded) == F) {
      hcal <- read_excel(input$user_data$datapath, sheet = "hourlyCalories")
    }
    else if (is.null(input$user_data) == T | is.null(input$use_sample) == F) {
      hcal <- read_excel("Joined_Dataset_V2.xlsx", sheet = "hourlyCalories")
    }
    hcal <- as.data.frame(hcal)
    print(str(hcal))
    hcal$ActivityHour <- as.POSIXct(hcal$ActivityHour, format="%Y-%m-%d")
    hcal<- subset(hcal, ActivityHour> "2016-05-12")
    hcal
  })
  
  # hourly calories graph
  observeEvent(
    c(input$use_uploaded, input$use_sample),
    ignoreInit = T,
    output$hourlycalories <- renderPlot(
      hourlyCalories() %>%
        group_by(ActivityHour) %>%
        summarize(CaloriesTotal = mean(Calories)) %>%
        ggplot(aes(x=ActivityHour, y= CaloriesTotal, fill = CaloriesTotal))+
        geom_col()+
        scale_fill_continuous(low = '#05fbd7', high = "#17667b") +
        labs(title = "Hourly Calories") +
        theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
              axis.text = element_text(size = 12),
              axis.title = element_text(size = 12, face = "bold"),
              panel.background = element_rect(fill = "white"),
              axis.line = element_line(size = 1, colour = "black"),
              panel.grid.major = element_line(colour = "#f4f4f4"))
    )
  )
  
  # Weekly Calories
  weeklyCalories <- reactive({
    c(input$use_uploaded, input$use_sample)
    # prepare the weekly calories data from either uploaded or sample data
    if (is.null(input$user_data) == F & is.null(input$use_uploaded) == F) {
      wcal <- read_excel(input$user_data$datapath, sheet = "dailyActivity")
    }
    else if (is.null(input$user_data) == T | is.null(input$use_sample) == F) {
      wcal <- read_excel("Joined_Dataset_V2.xlsx", sheet = "dailyActivity")
    }
    wcal <- as.data.frame(wcal)
    print(str(wcal))
    wcal$ActivityDate <-as.POSIXct(wcal$ActivityDate, format = "%Y-%m-%d")
    wcal <- subset(wcal, ActivityDate > "2016-05-06")
    wcal
  })
  
  # weekly calories graph
  observeEvent(
    c(input$use_uploaded, input$use_sample),
    ignoreInit = T,
    output$weeklycalories <- renderPlot(
      ggplot(data = weeklyCalories(), aes(x = ActivityDate, y = Calories)) + 
        geom_bar(stat = "identity", fill = "#27839a") +
        labs(title = "Weekly Calories ",
             x = "Date", y = "Weekly Calories") +
        theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
              axis.text = element_text(size = 12),
              axis.title = element_text(size = 12, face = "bold"),
              panel.background = element_rect(fill = "white"),
              axis.line = element_line(size = 1, colour = "black"),
              panel.grid.major = element_line(colour = "#f4f4f4"))      
    )
  )
  
  # Historic Calories
  historicCalories <- reactive({
    c(input$use_uploaded, input$use_sample)
    # prepare the historic calories data from either uploaded or sample data    
    if (is.null(input$user_data) == F & is.null(input$use_uploaded) == F) {
      histcal <- read_excel(input$user_data$datapath, sheet = "dailyActivity")
    }
    else if (is.null(input$user_data) == T | is.null(input$use_sample) == F) {
      histcal <- read_excel("Joined_Dataset_V2.xlsx", sheet = "dailyActivity")
    }
    histcal <- as.data.frame(histcal)
    print(str(histcal))
    histcal
  })
  
  # historical calories graph
  observeEvent(
    c(input$use_uploaded, input$use_sample),
    ignoreInit = T,
    output$historiccalories <- renderPlot(
      ggplot(historicCalories(), aes(x = ActivityDate, y = Calories))+
        geom_line(color = "#2cdeeb", size = 2) + 
        xlab("Date")+
        ylab("Calories Burned")+
        labs(title = "Historic Calories") + 
        theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
              axis.text = element_text(size = 12),
              axis.title = element_text(size = 12, face = "bold"),
              panel.background = element_rect(fill = "white"),
              axis.line = element_line(size = 1, colour = "black"),
              panel.grid.major = element_line(colour = "#f4f4f4"))
    )
  )
  
  # Daily Intensities
  dailyIntensity <- reactive({
    c(input$use_uploaded, input$use_sample)
    # prepare the daily intensities data from either uploaded or sample data
    if (is.null(input$user_data) == F & is.null(input$use_uploaded) == F) {
      dint <- read_excel(input$user_data$datapath, sheet = "hourlyIntensities")
    }
    else if (is.null(input$user_data) == T | is.null(input$use_sample) == F) {
      dint <- read_excel("Joined_Dataset_V2.xlsx", sheet = "hourlyIntensities")
    }
    dint <- as.data.frame(dint)
    print(str(dint))
    dint$ActivityHour <- as.POSIXct(dint$ActivityHour, format = "%Y-%m-%d")
    dint <- subset(dint, ActivityHour > "2016-05-06")
    dint
  })
  
  # daily intensity graph
  observeEvent(
    c(input$use_uploaded, input$use_sample),
    ignoreInit = T,
    output$dailyintensity <- renderPlot(
      ggplot(data = dailyIntensity(),
             aes(x=ActivityHour, y= AverageIntensity, fill = AverageIntensity))+
        geom_col()+
        scale_fill_continuous(low = '#05fbd7', high = "#17667b") +
        labs(title = "Daily Intensities") +
        theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
              axis.text = element_text(size = 12),
              axis.title = element_text(size = 12, face = "bold"),
              panel.background = element_rect(fill = "white"),
              axis.line = element_line(size = 1, colour = "black"),
              panel.grid.major = element_line(colour = "#f4f4f4"))      
    )
  )
  
  # Weekly Intensities
  weeklyIntensity <- reactive({
    c(input$use_uploaded, input$use_sample)
    # prepare the weekly intensities data from either uploaded or sample data
    if (is.null(input$user_data) == F & is.null(input$use_uploaded) == F) {
      wint <- read_excel(input$user_data$datapath, sheet = "dailyIntensities")
    }
    else if (is.null(input$user_data) == T | is.null(input$use_sample) == F) {
      wint <- read_excel("Joined_Dataset_V2.xlsx", sheet = "dailyIntensities")
    }
    wint <- as.data.frame(wint)
    print(str(wint))
    wint$ActivityDay <- as.POSIXct(wint$ActivityDay, format = "%Y-%m-%d")
    wint <- subset(wint, ActivityDay > "2016-05-06")
    wint <- wint %>% select(ActivityDay, 
                            VeryActiveMinutes, 
                            FairlyActiveMinutes, 
                            LightlyActiveMinutes) %>%  data.frame()
    wint <- melt(wint, id.vars = c("ActivityDay"))
    wint
  })
  
  # weekly intensity graph
  observeEvent(
    c(input$use_uploaded, input$use_sample),
    ignoreInit = T,
    output$weeklyintensity <- renderPlot(
      ggplot(data = weeklyIntensity(),
             aes(fill = variable, y=value, x = ActivityDay)) +
        geom_bar(position = position_fill(reverse = TRUE), stat="identity") +
        labs(title = "Weekly Intensities") + 
        theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
              axis.text = element_text(size = 12),
              axis.title = element_text(size = 12, face = "bold"),
              panel.background = element_rect(fill = "white"),
              axis.line = element_line(size = 1, colour = "black"),
              panel.grid.major = element_line(colour = "#f4f4f4")) +
        scale_colour_manual(values = c('LightlyActiveMinutes' = '#05fbd7',
                                       'FairlyActiveMinutes' = '#17667b',
                                       'VeryActiveMinutes' = "#f8766d"),
                            aesthetics = "fill")
    )
  )  
  
  # Historic Intensities
  historicIntensity <- reactive({
    c(input$use_uploaded, input$use_sample)
    # prepare the historic intensities data from either uploaded or sample data
    if (is.null(input$user_data) == F & is.null(input$use_uploaded) == F) {
      hint <- read_excel(input$user_data$datapath, sheet = "dailyIntensities")
    }
    else if (is.null(input$user_data) == T | is.null(input$use_sample) == F) {
      hint <- read_excel("Joined_Dataset_V2.xlsx", sheet = "dailyIntensities")
    }
    hint <- as.data.frame(hint)
    print(str(hint))
    hint <- hint %>% select(ActivityDay, 
                            VeryActiveMinutes, 
                            FairlyActiveMinutes, 
                            LightlyActiveMinutes) %>%  data.frame()
    hint <- melt(hint, id.vars = c("ActivityDay"))
    hint
  })
  
  # historic intensity graph
  observeEvent(
    c(input$use_uploaded, input$use_sample),
    ignoreInit = T,
    output$historicintensity <- renderPlot(
      ggplot(data = historicIntensity(),
             aes(fill = variable, y=value, x = ActivityDay)) +
        geom_bar(position = position_fill(reverse = TRUE), stat="identity") +
        labs(title = "Historic Intensities") + 
        theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
              axis.text = element_text(size = 12),
              axis.title = element_text(size = 12, face = "bold"),
              panel.background = element_rect(fill = "white"),
              axis.line = element_line(size = 1, colour = "black"),
              panel.grid.major = element_line(colour = "#f4f4f4")) +
        scale_colour_manual(values = c('LightlyActiveMinutes' = '#05fbd7',
                                       'FairlyActiveMinutes' = '#17667b',
                                       'VeryActiveMinutes' = "#f8766d"),
                            aesthetics = "fill")
    )
  )    
  
  # Weekly Distance
  weeklyDistance <- reactive({
    c(input$use_uploaded, input$use_sample)
    # prepare the weekly distance data from either uploaded or sample data
    if (is.null(input$user_data) == F & is.null(input$use_uploaded) == F) {
      wdist <- read_excel(input$user_data$datapath, sheet = "dailyActivity")
    }
    else if (is.null(input$user_data) == T | is.null(input$use_sample) == F) {
      wdist <- read_excel("Joined_Dataset_V2.xlsx", sheet = "dailyActivity")
    }
    wdist <- as.data.frame(wdist)
    print(str(wdist))
    wdist$ActivityDate <- as.POSIXct(wdist$ActivityDate, format = "%Y-%m-%d")
    wdist <- subset(wdist, ActivityDate > "2016-05-06")
    wdist <- wdist %>% select(ActivityDate, 
                              VeryActiveDistance, 
                              ModeratelyActiveDistance, 
                              LightActiveDistance) %>%  data.frame()
    wdist <- melt(wdist, id.vars = c("ActivityDate"))
    wdist
  })
  
  # weekly distance graph
  observeEvent(
    c(input$use_uploaded, input$use_sample),
    ignoreInit = T,
    output$weeklydist <- renderPlot(
      ggplot(data = weeklyDistance(),
             aes(fill = variable, y=value, x = ActivityDate)) +
        geom_bar(position = position_fill(reverse = TRUE), stat="identity") +
        labs(title = "Weekly Distance") + 
        theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
              axis.text = element_text(size = 12),
              axis.title = element_text(size = 12, face = "bold"),
              panel.background = element_rect(fill = "white"),
              axis.line = element_line(size = 1, colour = "black"),
              panel.grid.major = element_line(colour = "#f4f4f4")) + 
        scale_colour_manual(values = c('LightActiveDistance' = '#05fbd7',
                                       'ModeratelyActiveDistance' = '#17667b',
                                       'VeryActiveDistance' = "#f8766d"),
                            aesthetics = "fill")
    )
  )
  
  # Historic Distance
  historicDistance <- reactive({
    c(input$use_uploaded, input$use_sample)
    # prepare the historic distance data from either uploaded or sample data
    if (is.null(input$user_data) == F & is.null(input$use_uploaded) == F) {
      hdist <- read_excel(input$user_data$datapath, sheet = "dailyActivity")
    }
    else if (is.null(input$user_data) == T | is.null(input$use_sample) == F) {
      hdist <- read_excel("Joined_Dataset_V2.xlsx", sheet = "dailyActivity")
    }
    hdist <- as.data.frame(hdist)
    print(str(hdist))
    hdist <- hdist %>% select(ActivityDate, 
                              VeryActiveDistance, 
                              ModeratelyActiveDistance, 
                              LightActiveDistance) %>%  data.frame()
    hdist <- melt(hdist, id.vars = c("ActivityDate"))
    hdist
  })
  
  # historic distance graph
  observeEvent(
    c(input$use_uploaded, input$use_sample),
    ignoreInit = T,
    output$historicdist <- renderPlot(
      ggplot(data = historicDistance(),
             aes(fill = variable, y= value, x = ActivityDate)) +
        geom_bar(position = position_fill(reverse = TRUE), stat="identity") + 
        labs(title = "Historic Distance") +
        theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
              axis.text = element_text(size = 12),
              axis.title = element_text(size = 12, face = "bold"),
              panel.background = element_rect(fill = "white"),
              axis.line = element_line(size = 1, colour = "black"),
              panel.grid.major = element_line(colour = "#f4f4f4")) + 
        scale_colour_manual(values = c('LightActiveDistance' = '#05fbd7',
                                       'ModeratelyActiveDistance' = '#17667b',
                                       'VeryActiveDistance' = "#f8766d"),
                            aesthetics = "fill")
    )
  )   
  
  # Daily Sleep
  dailySleep <- reactive({
    c(input$use_uploaded, input$use_sample)
    # prepare the daily sleep data from either uploaded or sample data
    if (is.null(input$user_data) == F & is.null(input$use_uploaded) == F) {
      dsleep <- read_excel(input$user_data$datapath, sheet = "minuteSleep")
    }
    else if (is.null(input$user_data) == T | is.null(input$use_sample) == F) {
      dsleep <- read_excel("Joined_Dataset_V2.xlsx", sheet = "minuteSleep")
    }
    dsleep <- dsleep %>% separate(date,sep = " ", c("Days", "Time")) %>%
      separate(Time, sep = ":", c("Hours", "Mins", "Seconds"), 
               extra = "merge", fill = "right") %>%
      group_by(Days) %>% summarise(Hours = as.numeric(unique(Hours))) %>% 
      filter(Days == "2016-05-11")
    
    hours_list <- data.frame(Hour_number = seq(1:24))
    dsleep <- merge(hours_list, dsleep, by.x = "Hour_number", 
                    by.y = "Hours", all.x = T)
    dsleep <- dsleep %>% transmute(Hour_number = as.numeric(Hour_number), 
                                   Num = ifelse(is.na(Days), 0, 1))
    print(str(dsleep))
    dsleep
  })
  
  
  # daily sleep graph
  observeEvent(
    c(input$use_uploaded, input$use_sample),
    ignoreInit = T,
    output$dailysleep <- renderPlot(
      ggplot(data = dailySleep(), aes(x = Hour_number, y = Num)) +
        geom_bar(stat="identity", fill = "#27839a") + 
        labs(title = "Daily Sleep Trend", x = "Hour") +
        theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
              axis.text = element_text(size = 12),
              axis.title = element_text(size = 12, face = "bold"),
              panel.background = element_rect(fill = "white"),
              axis.line = element_line(size = 1, colour = "black"),
              panel.grid.major = element_line(colour = "#f4f4f4"))
    )
  )  
  
  # Weekly Sleep
  weeklySleep <- reactive({
    c(input$use_uploaded, input$use_sample)
    # prepare the weekly sleep data from either uploaded or sample data
    if (is.null(input$user_data) == F & is.null(input$use_uploaded) == F) {
      wsleep <- read_excel(input$user_data$datapath, sheet = "sleepDay")
    }
    else if (is.null(input$user_data) == T | is.null(input$use_sample) == F) {
      wsleep <- read_excel("Joined_Dataset_V2.xlsx", sheet = "sleepDay")
    }
    wsleep$SleepDay <- as.POSIXct(wsleep$SleepDay, format = "%Y-%m-%d")
    wsleep <- subset(wsleep, SleepDay > "2016-05-05")
    wsleep <- as.data.frame(wsleep)
    print(str(wsleep))
    wsleep
  })
  
  # weekly sleep graph
  observeEvent(
    c(input$use_uploaded, input$use_sample),
    ignoreInit = T,
    output$weeklysleep <- renderPlot(
      ggplot(data = weeklySleep(), aes(x = SleepDay, y = TotalMinutesAsleep)) +
        geom_bar(stat = "identity", fill = "#27839a") +
        labs(title = "Weekly Sleep",
             x = "Date", y = "Weekly Sleep") + 
        geom_abline(slope=0, intercept=420,  col = "#f8766d", lty=2) +
        theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
              axis.text = element_text(size = 12),
              axis.title = element_text(size = 12, face = "bold"),
              panel.background = element_rect(fill = "white"),
              axis.line = element_line(size = 1, colour = "black"),
              panel.grid.major = element_line(colour = "#f4f4f4"))
    )
  )  
  
  # Historic Sleep
  historicSleep <- reactive({
    c(input$use_uploaded, input$use_sample)
    # prepare the historic sleep data from either uploaded or sample data
    if (is.null(input$user_data) == F & is.null(input$use_uploaded) == F) {
      hsleep <- read_excel(input$user_data$datapath, sheet = "sleepDay")
    }
    else if (is.null(input$user_data) == T | is.null(input$use_sample) == F) {
      hsleep <- read_excel("Joined_Dataset_V2.xlsx", sheet = "sleepDay")
    }
    hsleep$SleepDay <- as.POSIXct(hsleep$SleepDay, format = "%Y-%m-%d")
    hsleep <- subset(hsleep, SleepDay > "2016-05-05")
    hsleep <- as.data.frame(hsleep)
    print(str(hsleep))
    hsleep
  })
  
  # historic sleep graph
  observeEvent(
    c(input$use_uploaded, input$use_sample),
    ignoreInit = T,
    output$historicsleep <- renderPlot(
      ggplot(data = historicSleep(), 
             aes(x = SleepDay, y = TotalMinutesAsleep)) +
        geom_line(color = "#2cdeeb", size = 2) + 
        xlab("Date") +
        ylab("Sleep Time (in Minutes)") +
        geom_abline(slope=0, intercept=420,  col = "#f8766d", lty=2) +
        labs(title = "Historic Sleep") + 
        theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
              axis.text = element_text(size = 12),
              axis.title = element_text(size = 12, face = "bold"),
              panel.background = element_rect(fill = "white"),
              axis.line = element_line(size = 1, colour = "black"),
              panel.grid.major = element_line(colour = "#f4f4f4"))
    )
  )   
  
  
  ############################ SET YOUR GOALS ####################################
  
  # BMI Graph
  observeEvent(
    c(input$use_uploaded, input$use_sample),
    ignoreInit = T,
    # prepare the BMI calculations for uploaded or sample data
    output$bmiplot <- renderPlot({
      # calculate BMI
      weight_ideal <- 100
      lb <- input$weight
      inches <- input$height
      bmi_output <- (lb / (inches^2)) * 703
      
      # BMI Category
      if (inches < 58){
        weight_ideal <- round(mean(seq(from = 60, to = 114, by = 1)))
      } else if (inches == 58){
        weight_ideal <- round(mean(seq(from = 91, to = 118, by = 1)))
      } else if (inches == 59){
        weight_ideal <- round(mean(seq(from = 94, to = 123, by = 1)))
      } else if (inches == 60){
        weight_ideal <- round(mean(seq(from = 97, to = 127, by = 1)))
      } else if (inches == 61){
        weight_ideal <- round(mean(seq(from = 100, to = 131, by = 1)))
      } else if (inches == 62){
        weight_ideal <- round(mean(seq(from = 97, to = 127, by = 1)))
      } else if (inches == 63){
        weight_ideal <- round(mean(seq(from = 97, to = 127, by = 1)))
      } else if (inches == 64){
        weight_ideal <- round(mean(seq(from = 97, to = 127, by = 1)))
      } else if (inches == 65){
        weight_ideal <- round(mean(seq(from = 97, to = 127, by = 1)))
      } else if (inches == 66){
        weight_ideal <- round(mean(seq(from = 97, to = 127, by = 1)))
      } else if (inches == 67){
        weight_ideal <- round(mean(seq(from = 97, to = 127, by = 1)))
      } else if (inches == 68){
        weight_ideal <- round(mean(seq(from = 97, to = 127, by = 1)))
      } else if (inches == 69){
        weight_ideal <- round(mean(seq(from = 97, to = 127, by = 1)))
      } else if (inches == 70){
        weight_ideal <- round(mean(seq(from = 97, to = 127, by = 1)))
      } else if (inches == 71){
        weight_ideal <- round(mean(seq(from = 97, to = 127, by = 1)))
      } else if (inches == 72){
        weight_ideal <- round(mean(seq(from = 97, to = 127, by = 1)))
      } else if (inches == 73){
        weight_ideal <- round(mean(seq(from = 97, to = 127, by = 1)))
      } else if (inches == 74){
        weight_ideal <- round(mean(seq(from = 97, to = 127, by = 1)))
      } else if (inches == 75){
        weight_ideal <- round(mean(seq(from = 97, to = 127, by = 1)))
      } else if (inches == 76){
        weight_ideal <- round(mean(seq(from = 97, to = 127, by = 1)))
      } else if (inches > 76) {
        weight_ideal <- round(mean(seq(from = 160, to = 220, by = 1)))
      }
      
      # calculate ideal and desired BMI
      bmi_ideal <- (weight_ideal / (inches^2)) * 703
      bmi_desired <- (input$weight_goal / (inches^2)) * 703
      
      # calculate parameters for BMI Spectrum Graph
      inch <- seq(from = 10, to = 120)
      # min-max
      lb_min <- numeric(length(inch)) + 10
      lb_max <- numeric(length(inch)) + 690
      # underweight
      lb_udr <- inch^2 * 18.5 / 703
      lb_udr[lb_udr>690] <- 690
      lb_udr[lb_udr<10] <- 10
      # overweight
      lb_ovr <- inch^2 * 25 / 703
      lb_ovr[lb_ovr>690] <- 690
      lb_ovr[lb_ovr<10] <- 10
      # obese
      lb_obe <- inch^2 * 30 / 703
      lb_obe[lb_obe>690] <- 690
      lb_obe[lb_obe<10] <- 10
      
      # create BMI polygon plot
      ggplot(data = NULL, aes(x = 5, y = 700)) + 
        xlim(10,120) + ylim(5,700) + 
        labs(title = "BMI: Current vs. Desired vs. Ideal",
             x = "Height in Inches", 
             y = "Weight in Pounds",
             caption = "Based on data from www.cdc.gov.
             Ideal BMI values are averages within recommended weight range.") +
        geom_polygon(aes(x = c(inch,rev(inch)), 
                         y = c(lb_min,rev(lb_udr)),
                         fill = "#eb347d"),
                     alpha = 0.8) +
        geom_polygon(aes(x = c(inch,rev(inch)),
                         y = c(lb_udr,rev(lb_ovr)),
                         fill = "#34bdeb"),
                     alpha = 0.8) +
        geom_polygon(aes(x = c(inch,rev(inch)),
                         y = c(lb_ovr,rev(lb_obe)),
                         fill = "#9634eb"),
                     alpha = 0.8) +
        geom_polygon(aes(x = c(inch,rev(inch)),
                         y = c(lb_obe,rev(lb_max)),
                         fill = "#eb4034"),
                     alpha = 0.8) +
        
        # add markers for BMI Values
        annotate(geom = "curve", x = input$height + 40, y = input$weight + 120, 
                 xend = input$height, yend = input$weight, curvature = .3, 
                 arrow = arrow(length = unit(2, "mm")),
                 colour = "white", size = 1) +        
        annotate(geom = "text", x = input$height + 48, y = input$weight + 120,
                 label = paste("Your BMI (", round(bmi_output,1),")"
                               , sep = ""), 
                 colour = "white", size = 4.5, fontface = "bold") +
        annotate(geom = "curve", x = input$height + 30, y = weight_ideal + 100,
                 xend = input$height, yend = weight_ideal, curvature = .3,
                 arrow = arrow(length = unit(2, "mm")),
                 colour = "black", size = 1) +
        annotate(geom = "text", x = input$height + 38, y = weight_ideal + 100,
                 label = paste("Your Ideal BMI (", round(bmi_ideal,1),")"
                               , sep = ""), 
                 colour = "black", size = 4.5, fontface = "bold") +
        annotate(geom = "curve", x = input$height + 10, y = input$weight_goal + 80, 
                 xend = input$height, yend = input$weight_goal, curvature = .3, 
                 arrow = arrow(length = unit(2, "mm")),
                 colour = "white", size = 1) +
        annotate(geom = "text", x = input$height + 18, y = input$weight_goal + 80,
                 label = paste("Your Desired BMI (", round(bmi_desired,1),")"
                               , sep = ""), 
                 colour = "white", size = 4.5, fontface = "bold") +
        
        # customize graph title, labels, and legend
        theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
              legend.position = "right",
              legend.title = element_text(size = 16, face = "bold"),
              legend.text = element_text(size = 12),
              axis.text = element_text(size = 12),
              axis.title = element_text(size = 12, face = "bold"),
              panel.background = element_rect(fill = "white"),
              axis.line = element_line(size = 1, colour = "black"),
              panel.grid.major = element_line(colour = "#f4f4f4")) +
        scale_fill_identity(name = "BMI Categories",
                            guide = "legend",
                            labels = c("Normal",
                                       "Overweight",
                                       "Underweight",
                                       "Obese"))
    })
  )
  ############################# Track your activity ####################################
  
  #Dont break the chain
  #historical goal completion chart
  
  df_activity_tall <- reactive({
    c(input$use_uploaded, input$use_sample)
    if (is.null(input$user_data) == F & is.null(input$use_uploaded) == F) {
      D_dailyActivity <- read_excel(input$user_data$datapath, sheet = "dailyActivity")
      D_sleepDay <- read_excel(input$user_data$datapath, sheet = "sleepDay")
      D_standDay <- read_excel(input$user_data$datapath, sheet = "standDay")
    }
    else if (is.null(input$user_data) == T | is.null(input$use_sample) == F) {
      D_dailyActivity <- read_excel("Joined_Dataset_V2.xlsx", sheet = "dailyActivity")
      D_sleepDay <- read_excel("Joined_Dataset_V2.xlsx", sheet = "sleepDay")
      D_standDay <- read_excel("Joined_Dataset_V2.xlsx", sheet = "standDay")
    }
    D_dailyActivity$ActivityDate <- as.POSIXct(D_dailyActivity$ActivityDate, format = "%Y-%m-%d")
    D_sleepDay$SleepDay <- as.POSIXct(D_sleepDay$SleepDay, format = "%Y-%m-%d")
    D_standDay$ActivityDate <- as.POSIXct(D_standDay$ActivityDate, format = "%Y-%m-%d")
    names(D_sleepDay)[names(D_sleepDay) == 'SleepDay'] <- "ActivityDate"
    d_merge <- merge(x=D_dailyActivity,y=D_sleepDay,by ="ActivityDate")
    d_merge <- merge(x=d_merge,y=D_standDay,by ="ActivityDate")
    d_merge <- d_merge %>% select(ActivityDate, TotalSteps, Calories, TotalMinutesAsleep, TotalStand)
    d_merge <- as.data.frame(d_merge)
    d_merge$ActivityDate <- as.POSIXct(d_merge$ActivityDate, format = "%Y-%m-%d")
    #inputs
    step_goal <- as.numeric(input$steps_goal) 
    exercise_goal <- as.numeric(input$exercise_goal)
    sleep_goal <- as.numeric(input$sleep_goal)*60
    stand_goal <- as.numeric(input$stand_goal)*60
    #
    df_activity_tidy <- d_merge %>%
      mutate(date = as.Date(ActivityDate)) %>%
      rename(move = TotalSteps,
             exercise = Calories,
             sleep = TotalMinutesAsleep,
             stand = TotalStand) %>%
      
      mutate(move_pct = move/step_goal,
             exercise_pct = exercise/exercise_goal,
             sleep_pct = sleep/sleep_goal,
             stand_pct = stand/stand_goal,
             move_bool = if_else(move_pct < 1, FALSE, TRUE),
             exercise_bool = if_else(exercise_pct < 1, FALSE, TRUE),
             sleep_bool = if_else(sleep_pct < 1, FALSE, TRUE),
             stand_bool = if_else(stand_pct < 1, FALSE, TRUE))
    
    df_activity_tall_value <- df_activity_tidy %>%
      select(date, Move = move, Exercise = exercise, Sleep = sleep, Stand = stand) %>%
      gather(category, value, -date)
    
    df_activity_tall_pct <- df_activity_tidy %>%
      select(date, Move = move_pct, Exercise = exercise_pct, Sleep = sleep_pct, Stand = stand_pct) %>%
      gather(category, pct, -date)
    
    df_activity_tall_bool <- df_activity_tidy %>%
      select(date, Move = move_bool, Exercise = exercise_bool, Sleep = sleep_bool, Stand = stand_bool) %>%
      gather(category, boolean, -date)
    
    df_activity_tall <- df_activity_tall_value %>%
      left_join(df_activity_tall_pct, by = c("date", "category")) %>%
      left_join(df_activity_tall_bool, by = c("date", "category")) %>%
      mutate(category = as_factor(category#, levels = c("Move", "Exercise", "Sleep", "Stand")
      ),
      month = ymd(paste(year(date), month(date), 1, sep = "-")),
      week = date - wday(date) + 1,
      wday = wday(date),
      day = day(date))
    df_activity_tall
  })
  
  observeEvent(
    input$set_goal,
    output$BreakChain <- renderPlot(
      df_activity_tall() %>% 
        ggplot(aes(x = wday, y = week, fill = boolean)) +
        geom_tile(col = "grey30", na.rm = FALSE) +
        theme(panel.grid.major = element_blank()) +
        scale_fill_manual(values = c("#f4f4f4", "mediumaquamarine")) +
        facet_wrap(~ category) +
        coord_fixed(ratio = 0.15) +
        guides(fill=FALSE) +
        labs(title = NULL,
             caption = 'Green:Goal Completed') +
        theme(axis.text.x = element_blank())
    )
  )
  
  #Streak count graph
  
  df_activity_streak <- reactive({
    c(input$use_uploaded, input$use_sample)
    if (is.null(input$user_data) == F & is.null(input$use_uploaded) == F) {
      D_dailyActivity <- read_excel(input$user_data$datapath, sheet = "dailyActivity")
      D_sleepDay <- read_excel(input$user_data$datapath, sheet = "sleepDay")
      D_standDay <- read_excel(input$user_data$datapath, sheet = "standDay")
    }
    else if (is.null(input$user_data) == T | is.null(input$use_sample) == F) {
      D_dailyActivity <- read_excel("Joined_Dataset_V2.xlsx", sheet = "dailyActivity")
      D_sleepDay <- read_excel("Joined_Dataset_V2.xlsx", sheet = "sleepDay")
      D_standDay <- read_excel("Joined_Dataset_V2.xlsx", sheet = "standDay")
    }
    D_dailyActivity$ActivityDate <- as.POSIXct(D_dailyActivity$ActivityDate, format = "%Y-%m-%d")
    D_sleepDay$SleepDay <- as.POSIXct(D_sleepDay$SleepDay, format = "%Y-%m-%d")
    D_standDay$ActivityDate <- as.POSIXct(D_standDay$ActivityDate, format = "%Y-%m-%d")
    names(D_sleepDay)[names(D_sleepDay) == 'SleepDay'] <- "ActivityDate"
    d_merge <- merge(x=D_dailyActivity,y=D_sleepDay,by ="ActivityDate")
    d_merge <- merge(x=d_merge,y=D_standDay,by ="ActivityDate")
    d_merge <- d_merge %>% select(ActivityDate, TotalSteps, Calories, TotalMinutesAsleep, TotalStand)
    d_merge <- as.data.frame(d_merge)
    d_merge$ActivityDate <- as.POSIXct(d_merge$ActivityDate, format = "%Y-%m-%d")
    #inputs
    step_goal <- as.numeric(input$steps_goal) 
    exercise_goal <- as.numeric(input$exercise_goal)
    sleep_goal <- as.numeric(input$sleep_goal)*60
    stand_goal <- as.numeric(input$stand_goal)*60
    #
    df_activity_tidy <- d_merge %>% 
      mutate(date = as.Date(ActivityDate)) %>% 
      rename(move = TotalSteps,
             exercise = Calories,
             sleep = TotalMinutesAsleep,
             stand = TotalStand) %>%
      
      mutate(move_pct = move/step_goal,
             exercise_pct = exercise/exercise_goal,
             sleep_pct = sleep/sleep_goal,
             stand_pct = stand/stand_goal,
             move_bool = if_else(move_pct < 1, FALSE, TRUE),
             exercise_bool = if_else(exercise_pct < 1, FALSE, TRUE),
             sleep_bool = if_else(sleep_pct < 1, FALSE, TRUE),
             stand_bool = if_else(stand_pct < 1, FALSE, TRUE))
    
    df_activity_tall_value <- df_activity_tidy %>%
      select(date, Move = move, Exercise = exercise, Sleep = sleep, Stand = stand) %>%
      gather(category, value, -date)
    
    df_activity_tall_pct <- df_activity_tidy %>%
      select(date, Move = move_pct, Exercise = exercise_pct, Sleep = sleep_pct, Stand = stand_pct) %>%
      gather(category, pct, -date)
    
    df_activity_tall_bool <- df_activity_tidy %>%
      select(date, Move = move_bool, Exercise = exercise_bool, Sleep = sleep_bool, Stand = stand_bool) %>%
      gather(category, boolean, -date)
    
    df_activity_tall <- df_activity_tall_value %>%
      left_join(df_activity_tall_pct, by = c("date", "category")) %>%
      left_join(df_activity_tall_bool, by = c("date", "category")) %>%
      mutate(category = as_factor(category#, levels = c("Move", "Exercise", "Sleep", "Stand")
      ),
      month = ymd(paste(year(date), month(date), 1, sep = "-")),
      week = date - wday(date) + 1,
      wday = wday(date),
      day = day(date))
    df_activity_streak <- df_activity_tall_bool %>%
      mutate(category = as_factor(category#, levels = c("Move", "Exercise", "Stand")
      )) %>%
      arrange(category, date) %>%
      group_by(category,
               x = cumsum(c(TRUE, diff(boolean) %in% c(-1))),
               y = cumsum(c(TRUE, diff(boolean) %in% c(-1,1)))) %>%
      mutate(streak = if_else(boolean == FALSE, 0L, row_number())) %>%
      ungroup()
    df_activity_streak
  })
  
  observeEvent(
    input$set_goal,
    output$activity_streak <- renderPlot(
      ggplot(df_activity_streak(), aes(x = date, y = streak, group = x, col = category)) +
        geom_line() +
        facet_grid(category~.) +
        guides(fill=FALSE) +
        labs(title = NULL)
    )
  )
  
  #Daily Tracker
  
  #Daily steps goal chart
  
  data_steps <- reactive({
    c(input$use_uploaded, input$use_sample)
    if (is.null(input$user_data) == F & is.null(input$use_uploaded) == F) {
      data_steps <- read_excel(input$user_data$datapath, sheet = "hourlySteps")
    }
    else if (is.null(input$user_data) == T | is.null(input$use_sample) == F) {
      data_steps <- read_excel("Joined_Dataset_V2.xlsx", sheet = "hourlySteps")
    }
    #data_steps$ActivityHour <- as.POSIXct(data_steps$ActivityHour, format = "%Y-%m-%d")
    #latest_date <- head(data_steps %>% distinct(ActivityHour) %>% arrange(desc(ActivityHour)),1)
    #max(as.Date(data_steps$ActivityHour))
    data_steps <- data_steps %>% mutate(date = as_date(ActivityHour)) %>% filter(date == "2016-05-12")
    total_steps <- sum(data_steps["StepTotal"])
    target_steps <- as.numeric(input$steps_goal)
    if (target_steps >= total_steps){
      target_steps <- target_steps - total_steps
    }
    else {
      target_steps <- 0
    }
    
    data_steps <- data.frame("Steps" = c(rep("Achieved", total_steps), rep("Remaining", target_steps)))
    data_steps <- data_steps %>%
      filter(Steps != "NA") %>%
      group_by(Steps) %>%
      count() %>%
      ungroup()%>%
      arrange(desc(Steps)) %>%
      mutate(percentage = round(n/sum(n),4)*100,
             lab.pos = cumsum(percentage)-.5*percentage)
    data_steps
  })
  observeEvent(
    input$set_goal,
    output$step_goal_plot <- renderPlot(
      ggplot(data = data_steps(),
             aes(x = 2, y = percentage, fill = Steps))+
        geom_bar(stat = "identity")+
        coord_polar("y", start = 200) +
        geom_text(aes(y = lab.pos, label = paste(percentage,"%", sep = "")), col = "white") +
        theme_void() +
        scale_fill_brewer(palette = "Dark2")+
        xlim(.2,2.5)
    )
  )
  
  #Daily stand goal chart
  
  data_stand <- reactive({
    c(input$use_uploaded, input$use_sample)
    if (is.null(input$user_data) == F & is.null(input$use_uploaded) == F) {
      data_stand <- read_excel(input$user_data$datapath, sheet = "hourlyStand")
    }
    else if (is.null(input$user_data) == T | is.null(input$use_sample) == F) {
      data_stand <- read_excel("Joined_Dataset_V2.xlsx", sheet = "hourlyStand")
    }
    data_stand <- data_stand %>% mutate(date = as_date(ActivityHour)) %>% filter(date == "2016-05-12")
    total_stand <- sum(data_stand["Stand"])
    target_stand <- (as.numeric(input$stand_goal)*60)
    if (target_stand >= total_stand) {
      target_stand <- target_stand - total_stand
    }
    else {
      target_stand <- 0
    }
    data_stand <- data.frame("Stand" = c(rep("Achieved", total_stand), rep("Remaining", target_stand)))
    data_stand <- data_stand %>%
      filter(Stand != "NA") %>%
      group_by(Stand) %>%
      count() %>%
      ungroup()%>%
      arrange(desc(Stand)) %>%
      mutate(percentage = round(n/sum(n),4)*100,
             lab.pos = cumsum(percentage)-.5*percentage)
    data_stand
  })
  #
  observeEvent(
    input$set_goal,
    output$stand_goal_plot <- renderPlot(
      ggplot(data = data_stand(),
             aes(x = 2, y = percentage, fill = Stand))+
        geom_bar(stat = "identity")+
        coord_polar("y", start = 200) +
        geom_text(aes(y = lab.pos, label = paste(percentage,"%", sep = "")), col = "white") +
        theme_void() +
        scale_fill_brewer(palette = "Dark2")+
        xlim(.2,2.5)
    )
  )
  #daily calories chart
  data_calories <- reactive({
    c(input$use_uploaded, input$use_sample)
    if (is.null(input$user_data) == F & is.null(input$use_uploaded) == F) {
      data_calories <- read_excel(input$user_data$datapath, sheet = "dailyCalories")
    }
    else if (is.null(input$user_data) == T | is.null(input$use_sample) == F) {
      data_calories <- read_excel("Joined_Dataset_V2.xlsx", sheet = "dailyCalories")
    }
    data_calories <- data_calories %>% mutate(date = as_date(ActivityDay)) %>% filter(date == "2016-05-12")
    total_calories <- sum(data_calories["Calories"])
    target_calories <- as.numeric(input$exercise_goal)
    if (target_calories >= total_calories){
      target_calories <- target_calories - total_calories
    }
    else{
      target_calories <-0
    }
    data_calories <- data.frame("Calories" = c(rep("Achieved", total_calories), rep("Remaining", target_calories)))
    data_calories <- data_calories %>%
      filter(Calories != "NA") %>%
      group_by(Calories) %>%
      count() %>%
      ungroup()%>%
      arrange(desc(Calories)) %>%
      mutate(percentage = round(n/sum(n),4)*100,
             lab.pos = cumsum(percentage)-.5*percentage)
    data_calories
  })
  #
  observeEvent(
    input$set_goal,
    output$calories_goal_plot <- renderPlot(
      ggplot(data = data_calories(),
             aes(x = 2, y = percentage, fill = Calories))+
        geom_bar(stat = "identity")+
        coord_polar("y", start = 200) +
        geom_text(aes(y = lab.pos, label = paste(percentage,"%", sep = "")), col = "white") +
        theme_void() +
        scale_fill_brewer(palette = "Dark2")+
        xlim(.2,2.5)
    )
  )
  
  #Weight loss tab
  
  #HIstorical Weight Chart 
  
  weightLogInfo <- reactive({
    c(input$use_uploaded, input$use_sample)
    if (is.null(input$user_data) == F & is.null(input$use_uploaded) == F) {
      weightLogInfo <- read_excel(input$user_data$datapath, sheet = "weightLogInfo")
    }
    else if (is.null(input$user_data) == T | is.null(input$use_sample) == F) {
      weightLogInfo <- read_excel("Joined_Dataset_V2.xlsx", sheet = "weightLogInfo")
    }
    weightLogInfo
  })
  observeEvent(
    c(input$use_uploaded, input$use_sample),
    output$weightLogInfo_plot <- renderPlot(
      ggplot(weightLogInfo(), aes(x = Date, y = WeightPounds))+
        geom_line(color = "#17667b", size = 2) + 
        xlab("Date")+
        ylab("Weight (lb)")+
        theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
              axis.text = element_text(size = 12),
              axis.title = element_text(size = 12, face = "bold"),
              panel.background = element_rect(fill = "white"),
              axis.line = element_line(size = 1, colour = "black"),
              panel.grid.major = element_line(colour = "#f4f4f4")) +
        stat_smooth(method = "lm", col = "red")
    )
  )
  
  #Forecast - ARIMA Model
  
  forcast_calories_burn <- reactive({
    c(input$use_uploaded, input$use_sample)
    if (is.null(input$user_data) == F & is.null(input$use_uploaded) == F) {
      data_calories_F <- read_excel(input$user_data$datapath, sheet = "dailyCalories")
    }
    else if (is.null(input$user_data) == T | is.null(input$use_sample) == F) {
      data_calories_F <- read_excel("Joined_Dataset_V2.xlsx", sheet = "dailyCalories")
    }
    df<-data.frame(data_calories_F)
    df<-select(df, -1)
    
    z <- read.zoo(df, format = "%Y-%m-%d")
    class(z)
    
    tss<-as.ts(z)
    
    tss2 <- log(tss)
    
    moving_avg <- aggregate(tss,FUN=mean)
    
    #lines(moving_avg,col='orange',lwd=2)
    
    arima_1 <- auto.arima(tss)
    accuracy(arima_1)
    list<-forecast(arima_1,h=18)
    
    forcast_calories_burn <- round(sum(list[["mean"]]), digits=0)
    #forcast_weight_loss <- forcast_calories_burn/3500
    forcast_calories_burn 
  })
  
  #Weight Loss Tracker
  
  observeEvent(
    input$weight_goal,
    output$calories <- 
      if (round(input$weight-(forcast_calories_burn()/3500), digits = 0) >= (input$weight_goal)) {
        renderValueBox({
          valueBox(value = tags$p("Based on your activity data you will burn " , forcast_calories_burn(), " calories by the end of the month", style = "font-size: 75%;"),
                   "Calories", icon = icon("exclamation-triangle"), color = "orange")
        })
      }
    else {
      renderValueBox({
        valueBox(value = tags$p("Based on your activity data you will burn " , forcast_calories_burn(), " calories by the end of the month", style = "font-size: 75%;"),
                 "Calories", icon = icon("check-circle"), color = "purple")
      })
    }
  )
  observeEvent(
    input$weight_goal,
    output$Prediction <- 
      if (round(input$weight-(forcast_calories_burn()/3500), digits = 0) >= (input$weight_goal)) {
        renderValueBox({
          valueBox(value = tags$p("You are predicted to weigh ", round(input$weight-(forcast_calories_burn()/3500), digits = 0), " lbs by the end of the month" , style = "font-size: 50%;"),
                   #paste0("You will be ","lbs short of your goal at the end of the month"), style = "font-size: 50%;" ,
                   "Prediction", color = "yellow")
        })
      }
    else {
      renderValueBox({
        valueBox(value = tags$p("You are predicted to weigh ", round(input$weight-(forcast_calories_burn()/3500), digits = 0), " lbs by the end of the month", style = "font-size: 50%;"),
                 #paste0("You will be ","lbs short of your goal at the end of the month"), style = "font-size: 50%;" ,
                 "Prediction", color = "green")
      })
    }
  )
  observeEvent(
    input$weight_goal,
    output$Target <- renderValueBox({
      valueBox(value = tags$p("You Desired Weight is ", input$weight_goal, " lbs", style = "font-size: 50%;"),
               #paste0("Your Desired Weight is"), , style = "font-size: 50%;" ,
               "Target", color = "light-blue")
    })
  )
  
  #DIsplay User Name in the sidebar
  
  observeEvent(
    c(input$use_uploaded, input$use_sample),
    output$text1 <- renderText({paste(isolate(input$name))})
  )
}

# Run the application 
shinyApp(ui = ui, server = server)

