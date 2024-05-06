library(shiny)
library(randomForest)


ui <- fluidPage(
  titlePanel("Quality of Life Prediction"),
  
  sidebarLayout(
    sidebarPanel(
      numericInput("Age", "Age", value = "Enter Age", min = 0, max = 100),
      numericInput("Income", "Income", value = "Enter Income", min = 10000, max = 100000),
      selectInput("Achieving.Ends.Meet", "Achieving Ends Meet", choices = c("No" = 0, "Yes" = 1), selected = 0),
      numericInput("Duration.of.Residency", "Duration of Residency", value = "Enter years", min = 1, max = 100),
      selectInput("English.Speaking", "English Speaking", choices = c("Not at all" = 1, "Not well" = 2, "Well" = 3, "Very well" = 4), selected = 1),
      selectInput("Familiarity.with.America", "Familiarity with America", choices = c("Very low" = 1, "Low" = 2, "High" = 3, "Very high" = 4), selected = 1),
      selectInput("Familiarity.with.Ethnic.Origin", "Familiarity with Ethnic Origin", choices = c("Very low" = 1, "Low" = 2, "High" = 3, "Very high" = 4), selected = 1),
      selectInput("Present.Mental.Health", "Present Mental Health", choices = c("Poor" = 1, "Fair" = 2, "Good" = 3, "Very Good" = 4, "Excellent" = 5), selected = 1),
      radioButtons("Residence.Ownership", "Residence Ownership", choices = c("Own" = 1, "Rent" = 0), selected = 1),
      radioButtons("Car", "Car", choices = c("Yes" = 1, "No" = 0), selected = 1),
      selectInput("Satisfaction.with.Life", "Satisfaction with Life", choices = c("Not at all" = 1, "Not very much" = 2, "Pretty much" = 3, "Very much" = 4), selected = 1),
      radioButtons("GenderFemale", "Gender Female", choices = c("Female" = 1, "Male" = 0), selected = 1),
      actionButton("predict", "Predict Quality of Life")
    ),
    
    mainPanel(
      verbatimTextOutput("qualityOfLife")
    )
  )
)



server <- function(input, output) {
  
  #Defining the SD and Mean values for scaled variables from the regression code
  mean_quality_of_life <- 7.83749200255918
  sd_quality_of_life <- 1.49363201614317
  mean_income <- 60012.7959053103
  sd_income <- 24411.1147844669

  predictQuality <- eventReactive(input$predict, {
    scaled_income <- (input$Income - mean_income) / sd_income
    
    newdata <- data.frame(
      Age = as.numeric(input$Age),
      Income = scaled_income,
      `Achieving.Ends.Meet` = as.numeric(input$`Achieving.Ends.Meet`),
      `Duration.of.Residency` = as.numeric(input$`Duration.of.Residency`),
      `English.Speaking` = as.numeric(input$`English.Speaking`),
      `Familiarity.with.America` = as.numeric(input$`Familiarity.with.America`),
      `Familiarity.with.Ethnic.Origin` = as.numeric(input$`Familiarity.with.Ethnic.Origin`),
      `Present.Mental.Health` = as.numeric(input$`Present.Mental.Health`),
      `Residence.Ownership` = as.numeric(input$`Residence.Ownership`),
      Car = as.numeric(input$Car),
      `Satisfaction.with.Life` = as.numeric(input$`Satisfaction.with.Life`),
      GenderFemale = as.numeric(input$GenderFemale)
    )
    model <- readRDS("rf_model.rds")
    scaled_prediction <- predict(model, newdata = newdata)
    
    # Reverse scaling the predicted quality of life
    original_scale_prediction <- scaled_prediction * sd_quality_of_life + mean_quality_of_life
    return(original_scale_prediction)
  })
  

  output$qualityOfLife <- renderText({
    req(predictQuality())
    paste("Predicted Quality of Life:", round(predictQuality(), 0))
  })
}

#Running the application

shinyApp(ui = ui, server = server)
