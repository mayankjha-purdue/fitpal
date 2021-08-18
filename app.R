library(shinythemes)
library(shiny)
library(shinyWidgets)
library(shinydashboard)
library(dashboardthemes)
library(dplyr)
library(DT)
library(ggplot2)
library(XML)
library(reshape2)
library(lubridate)
library(scales)
library(ggthemes)
library(ggrepel)
library(plotly)
library(readxl)
library(bslib)
library(writexl)
library(tidyr)

 
 # install.packages('rsconnect')
 # library(rsconnect)
 # rsconnect::setAccountInfo(name='fitpalapp', token='8FD61FC57555BE4F8A8696410D4D1753', secret='PHCKsdeqiBonpZuNsHYHlXpFDFRkeW+DIlmC0rjt')
 # rsconnect::deployApp('path/to/your/app')

 
 server <- function(input, output) {
   
   #Welcome text
   output$welcome <- renderText({
     if (input$name == ""){
       paste("Welcome to FitPal!")
     }
     else {
       paste("Welcome to FitPal ", isolate(input$name), "!", sep = "")
     }
   })
   
   #Playing with data
   sample_Input <- reactive({
     input$use_sample
     
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
                                                     col_name=T))
     )
   })
   
   # sample table
   output$sample_table <- renderTable({
     head(sample_Input()[1:ncol(sample_Input())], n = 10)
   })
   
   output$download_sample <- downloadHandler(
     filename = function() {
       paste(input$sample_data, ".xlsx", sep = "")
     },
     content = function(file) {
       write_xlsx(sample_Input(), file)
     }
   )
   
   hourlySteps <- reactive({
     input$action
     if (is.null(input$user_data) == F) {
       hsteps <- read_excel(input$user_data$datapath, sheet = "hourlySteps")
     }
     else {
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
     input$action,
     output$hourlysteps <- renderPlot(
       hourlySteps() %>%
         group_by(ActivityHour) %>%
         summarize(StepTotal = mean(StepTotal)) %>%
         ggplot(aes(x=ActivityHour, y= StepTotal, fill = StepTotal))+
         geom_col()+
         scale_fill_continuous(low = 'grey70', high = "#008FD5") +
         labs(title = "Hourly Steps")
     )
   )
   
   
   dailySteps <- reactive({
     input$action
     if (is.null(input$user_data) == F) {
       dsteps <- read_excel(input$user_data$datapath, sheet = "dailySteps")
     }
     else {
       dsteps <- read_excel("Joined_Dataset_V2.xlsx", sheet = "dailySteps")
     }
     dsteps <- as.data.frame(dsteps)
     print(str(dsteps))
     dsteps$ActivityDay <- as.POSIXct(dsteps$ActivityDay, format = "%Y-%m-%d")
     weeklysteps <- subset(dsteps, ActivityDay > "2016-05-05")
     weeklysteps
   })
   
   # daily steps graph
   observeEvent(
     input$action,
     output$dailysteps <- renderPlot(
       ggplot(data = dailySteps(), aes(x = ActivityDay, y = StepTotal)) +
         geom_bar(stat = "identity", color= "black", fill = "steelblue") +
         theme_minimal() + 
         labs(title = "Weekly Steps",
              x = "Date", y = "Weekly Steps")
     )
   )
   
   
   historicSteps <- reactive({
     input$action
     if (is.null(input$user_data) == F) {
       histeps <- read_excel(input$user_data$datapath, sheet = "dailySteps")
     }
     else {
       histeps <- read_excel("Joined_Dataset_V2.xlsx", sheet = "dailySteps")
     }
     histeps <- as.data.frame(histeps)
     print(str(histeps))
     histeps

   })
   
   # historic steps graph
   observeEvent(
     input$action,
     output$historicsteps <- renderPlot(
       ggplot(data = historicSteps(), aes(x = ActivityDay, y = StepTotal)) +
         geom_line(color = "steelblue") + 
         xlab("Date")+
         ylab("Total Steps")+
         theme_minimal()
     )
   )
   
   
   hourlyCalories <- reactive({
     input$action
     if (is.null(input$user_data) == F) {
       hcal <- read_excel(input$user_data$datapath, sheet = "hourlyCalories")
     }
     else {
       hcal <-read_excel("Joined_Dataset_V2.xlsx", sheet = "hourlyCalories")
     }
     hcal <- as.data.frame(hcal)
     print(str(hcal))
     hcal$ActivityHour <- as.POSIXct(hcal$ActivityHour, format="%Y-%m-%d")
     hcal<- subset(hcal, ActivityHour> "2016-05-12")
     hcal
   })
   
   # hourly calories graph
   observeEvent(
     input$action,
     output$hourlycalories <- renderPlot(
       hourlyCalories() %>%
         group_by(ActivityHour) %>%
         summarize(CaloriesTotal = mean(Calories)) %>%
         ggplot(aes(x=ActivityHour, y= CaloriesTotal, fill = CaloriesTotal))+
         geom_col()+
         scale_fill_continuous(low = 'grey70', high = "#008FD5") +
         labs(title = "Hourly Calories")
     )
   )
   
   
   weeklyCalories <- reactive({
     input$action
     if (is.null(input$user_data) == F) {
       wcal <- read_excel(input$user_data$datapath, sheet = "dailyActivity")
     }
     else {
       wcal <-read_excel("Joined_Dataset_V2.xlsx", sheet = "dailyActivity") 
     }
     wcal <- as.data.frame(wcal)
     print(str(wcal))
     wcal$ActivityDate <-as.POSIXct(wcal$ActivityDate, format = "%Y-%m-%d")
     wcal <- subset(wcal, ActivityDate > "2016-05-06")
     wcal
   })
   
   # weekly calories graph
   observeEvent(
     input$action,
     output$weeklycalories <- renderPlot(
       ggplot(data = weeklyCalories(), aes(x = ActivityDate, y = Calories)) + 
         geom_bar(stat = "identity", color= "black", fill = "navyblue") +
         theme_minimal() + 
         labs(title = "Weekly Calories ",
              x = "Date", y = "Weekly Calories")
     )
   )
   
   
   historicCalories <- reactive({
     input$action
     if (is.null(input$user_data) == F) {
       histcal <- read_excel(input$user_data$datapath, sheet = "dailyActivity")
     }
     else {
       histcal <- read_excel("Joined_Dataset_V2.xlsx", sheet = "dailyActivity")
     }
     histcal <- as.data.frame(histcal)
     print(str(histcal))
     histcal
   })
   
   # historical calories graph
   observeEvent(
     input$action,
     output$historiccalories <- renderPlot(
       ggplot(historicCalories(), aes(x = ActivityDate, y = Calories))+
         geom_line(color = "steelblue") + 
         xlab("Date")+
         ylab("Calories Burned")+
         theme_minimal()
     )
   )
   
   
   dailyIntensity <- reactive({
     input$action
     if (is.null(input$user_data) == F) {
       dint <- read_excel(input$user_data$datapath, sheet = "hourlyIntensities")
     }
     else {
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
     input$action,
     output$dailyintensity <- renderPlot(
       ggplot(data = dailyIntensity(),
              aes(x=ActivityHour, y= AverageIntensity, fill = AverageIntensity))+
         geom_col()+
         scale_fill_continuous(low = 'grey70', high = "#008FD5") +
         labs(title = "Fit Pal Health Data",
              subtitle = "Hourly Data Sample")
     )
   )
   
   weeklyIntensity <- reactive({
     input$action
     if (is.null(input$user_data) == F) {
       wint <- read_excel(input$user_data$datapath, sheet = "dailyIntensities")
     }
     else {
       wint <- read_excel("Joined_Dataset_V2.xlsx", sheet = "dailyIntensities")
     }
     wint <- as.data.frame(wint)
     print(str(wint))
     wint$ActivityDay <- as.POSIXct(wint$ActivityDay, format = "%Y-%m-%d")
     wint <- subset(wint, ActivityDay > "2016-05-06")
     wint <- wint %>% select(ActivityDay, LightlyActiveMinutes, FairlyActiveMinutes, VeryActiveMinutes) %>%  data.frame()
     wint <- melt(wint, id.vars = c("ActivityDay"))
     wint
   })
   
   # weekly intensity graph
   observeEvent(
     input$action,
     output$weeklyintensity <- renderPlot(
       ggplot(data = weeklyIntensity(),
              aes(fill = variable, y=value, x = ActivityDay)) +
         geom_bar(position="stack", stat="identity")
     )
   )  
   
   historicIntensity <- reactive({
     input$action
     if (is.null(input$user_data) == F) {
       hint <- read_excel(input$user_data$datapath, sheet = "dailyIntensities")
     }
     else {
       hint <- read_excel("Joined_Dataset_V2.xlsx", sheet = "dailyIntensities")
     }
     hint <- as.data.frame(hint)
     print(str(hint))
     # hint$ActivityDay <- as.POSIXct(hint$ActivityDay, format = "%Y-%m-%d")
     # hint <- subset(hint, ActivityDay > "2016-05-06")
     hint <- hint %>% select(ActivityDay, LightlyActiveMinutes, FairlyActiveMinutes, VeryActiveMinutes) %>%  data.frame()
     hint <- melt(hint, id.vars = c("ActivityDay"))
     hint
   })
   
   # historic intensity graph
   observeEvent(
     input$action,
     output$historicintensity <- renderPlot(
       ggplot(data = historicIntensity(),
              aes(fill = variable, y=value, x = ActivityDay)) +
         geom_bar(position="stack", stat="identity")
     )
   )    
   
   
   weeklyDistance <- reactive({
     input$action
     if (is.null(input$user_data) == F) {
       wdist <- read_excel(input$user_data$datapath, sheet = "dailyActivity")
     }
     else {
       wdist <- read_excel("Joined_Dataset_V2.xlsx", sheet = "dailyActivity")
     }
     wdist <- as.data.frame(wdist)
     print(str(wdist))
     wdist$ActivityDate <- as.POSIXct(wdist$ActivityDate, format = "%Y-%m-%d")
     wdist <- subset(wdist, ActivityDate > "2016-05-06")
     wdist <- wdist %>% select(ActivityDate, VeryActiveDistance, ModeratelyActiveDistance, LightActiveDistance) %>%  data.frame()
     wdist <- melt(wdist, id.vars = c("ActivityDate"))
     wdist
   })
   
   # weekly distance graph
   observeEvent(
     input$action,
     output$weeklydist <- renderPlot(
       ggplot(data = weeklyDistance(),
              aes(fill = variable, y=value, x = ActivityDate)) +
         geom_bar(position="stack", stat="identity")
     )
   )
   
   
   historicDistance <- reactive({
     input$action
     if (is.null(input$user_data) == F) {
       hdist <- read_excel(input$user_data$datapath, sheet = "dailyActivity")
     }
     else {
       hdist <- read_excel("Joined_Dataset_V2.xlsx", sheet = "dailyActivity")
     }
     hdist <- as.data.frame(hdist)
     print(str(hdist))
     hdist <- hdist %>% select(ActivityDate, VeryActiveDistance, ModeratelyActiveDistance, LightActiveDistance) %>%  data.frame()
     hdist <- melt(hdist, id.vars = c("ActivityDate"))
     hdist
   })
   
   # historic distance graph
   observeEvent(
     input$action,
     output$historicdist <- renderPlot(
       ggplot(data = historicDistance(),
              aes(fill = variable, y=value, x = ActivityDate)) +
         geom_bar(position="stack", stat="identity")
     )
   )   
   
   
   dailySleep <- reactive({
     input$action
     if (is.null(input$user_data) == F) {
       dsleep <- read_excel(input$user_data$datapath, sheet = "minuteSleep")
     }
     else {
       dsleep <- read_excel("Joined_Dataset_V2.xlsx", sheet = "minuteSleep")
     }
     dsleep <- dsleep %>% separate(date, sep = " ", c("Days", "Time")) %>%
       separate(Time, sep = ":", c("Hours", "Mins", "Seconds"), extra = "merge", fill = "right") %>%
       group_by(Days) %>% summarise(Hours = as.numeric(unique(Hours))) %>% filter(Days == "2016-05-11")
     
     hours_list <- data.frame(Hour_number = seq(1:24))
     str(hours_list)
     dsleep <- merge(hours_list, dsleep, by.x = "Hour_number", by.y = "Hours", all.x = T)
     dsleep <- dsleep %>% transmute(Hour_number = as.numeric(Hour_number), Num = ifelse(is.na(Days), 0, 1))
     dsleep <- as.data.frame(dsleep)
     print(str(dsleep))
     dsleep
   })
   
   # daily sleep graph
   observeEvent(
     input$action,
     output$dailysleep <- renderPlot(
       ggplot(data = dailySleep(), aes(y= Num, x = Hour_number)) +
         geom_bar(stat="identity", color= "black", fill = "steelblue") + 
         labs(title = "Daily Sleep Trend", x = "Hour") +
         theme(axis.title.y=element_blank(), 
               axis.ticks.y=element_blank(), 
               axis.text.y=element_blank())
     )
   )  
   
   
   weeklySleep <- reactive({
     input$action
     if (is.null(input$user_data) == F) {
       wsleep <- read_excel(input$user_data$datapath, sheet = "sleepDay")
     }
     else {
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
     input$action,
     output$weeklysleep <- renderPlot(
       ggplot(data = weeklySleep(), aes(x = SleepDay, y = TotalMinutesAsleep)) +
         geom_bar(stat = "identity", color= "black", fill = "steelblue") +
         theme_minimal() + 
         labs(title = "Weekly Sleep",
              x = "Date", y = "Weekly Sleep") + 
         geom_abline(slope=0, intercept=420,  col = "red", lty=2)
     )
   )  
   
   
   historicSleep <- reactive({
     input$action
     if (is.null(input$user_data) == F) {
       hsleep <- read_excel(input$user_data$datapath, sheet = "sleepDay")
     }
     else {
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
     input$action,
     output$historicsleep <- renderPlot(
       ggplot(data = historicSleep(), 
              aes(x = SleepDay, y = TotalMinutesAsleep)) +
         geom_line(color = "steelblue") + 
         xlab("Date") +
         ylab("Sleep Time (in Minutes)") +
         theme_minimal() + 
         geom_abline(slope=0, intercept=420,  col = "red", lty=2)
     )
   )   
   
   
   # BMI graph
   observeEvent(
     input$action,
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
       
       # create polygon plot
       ggplot(data = NULL, aes(x = 5, y = 700)) + 
         xlim(10,120) + ylim(5,700) + 
         labs(x = "Height in Inches", 
              y = "Weight in Pounds",
              caption = "Based on data from www.cdc.gov.
             Note that ideal BMI values are averages within the recommended weight range.") +
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
         # geom_point(aes(x = input$height, y = input$weight),
         #            shape = 4, colour = "white", 
         #            size = 2, stroke = 3) +         
         annotate(geom = "text", x = input$height + 48, y = input$weight + 120,
                  label = paste("Your BMI (", round(bmi_output,1),")"
                                , sep = ""), 
                  colour = "white", size = 4.5, fontface = "bold") +
         
         # geom_hline(yintercept = weight_ideal, colour = "black") +
         annotate(geom = "curve", x = input$height + 30, y = weight_ideal + 100,
                  xend = input$height, yend = weight_ideal, curvature = .3,
                  arrow = arrow(length = unit(2, "mm")),
                  colour = "black", size = 1) +
         # geom_point(aes(x = input$height, y = weight_ideal),
         #            shape = 4, colour = "black",
         #            size = 2, stroke = 3) +
         annotate(geom = "text", x = input$height + 38, y = weight_ideal + 100,
                  label = paste("Your Ideal BMI (", round(bmi_ideal,1),")"
                                , sep = ""), 
                  colour = "black", size = 4.5, fontface = "bold") +
         
         # geom_point(aes(x = input$height, y = input$weight_goal),
         #            shape = 4, colour = "white", 
         #            size = 2, stroke = 3) +            
         annotate(geom = "curve", x = input$height + 10, y = input$weight_goal + 80, 
                  xend = input$height, yend = input$weight_goal, curvature = .3, 
                  arrow = arrow(length = unit(2, "mm")),
                  colour = "white", size = 1) +
         annotate(geom = "text", x = input$height + 18, y = input$weight_goal + 80,
                  label = paste("Your Desired BMI (", round(bmi_desired,1),")"
                                , sep = ""), 
                  colour = "white", size = 4.5, fontface = "bold") +
         
         # customize graph title, labels, and legend
         theme(legend.position = "right",
               legend.title = element_text(size = 16, face = "bold"),
               legend.text = element_text(size = 12),
               axis.text = element_text(size = 12, face = "bold"),
               axis.title = element_text(size = 16, face = "bold"),
               panel.background = element_rect(fill = "white"),
               axis.line = element_line(size = 1, colour = "black")) +
         scale_fill_identity(name = "BMI Categories",
                             guide = "legend",
                             labels = c("Normal",
                                        "Overweight",
                                        "Underweight",
                                        "Obese"))
     })
   )
   
 }
 
 
shinyApp(ui = htmlTemplate("www/index.html"), server)
