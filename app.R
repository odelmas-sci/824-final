#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(dplyr)

# Read the CSV file
dat <- read.csv("monsters_cleaned.csv", header = TRUE, row.names = 1, 
                col.names = c("Monster", "Size", "Type","Alignment","Armor_Class", "Hit_Points","Strength", "Dexterity",
                              "Constitution", "Intelligence", "Wisdom", "Charisma", "Speed", 
                              "Speaks_Language", "Legendary_Creature","Challenge_Rating"))

dat$Size <- factor(dat$Size, levels = c("Tiny", "Small", "Medium", "Large", "Huge", "Gargantuan"), ordered = TRUE)

# Build Prediction Challenge Rating
cr_model <- lm(Challenge_Rating~Armor_Class+Hit_Points+Strength+Dexterity+Constitution+Intelligence+
                 Wisdom+Charisma+Speed+Speaks_Language, data=dat)

# Define UI for application that draws a histogram
ui <- fluidPage(

  titlePanel("Monster Challenge Rating Estimator"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("Hit_Points", "HP Range:", min = min(dat$Hit_Points), max = max(dat$Hit_Points), 
                  value = c(min(dat$Hit_Points), max(dat$Hit_Points))),
      sliderInput("Armor_Class", "AC Range:", min=min(dat$Armor_Class), max=max(dat$Armor_Class), 
                  value=c(min(dat$Armor_Class), max(dat$Armor_Class))),
      checkboxInput("Speaks_Language", "Only monsters with Language", value = FALSE),
      checkboxInput("Legendary_Creature", "Only legendary monsters", value=FALSE),
      #selectInput("alignment_filter", "Select Alignments:",
      #            choices = levels(dat$Alignment),
      #            selected = levels(dat$Alignment),
      #            multiple = TRUE),
      actionButton("clear_alignments", "Clear All")
    ),
    mainPanel(
      plotOutput("comparisonPlot"),
      dataTableOutput("monsterTable")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # Reactive filtered dataset
  filtered_data <- reactive({
    req(dat)  # ensures data is loaded
    
    # Filter by HP range and language checkbox
    dat %>%
      filter(
        Hit_Points >= input$Hit_Points[1],
        Hit_Points <= input$Hit_Points[2],
        Armor_Class >= input$Armor_Class[1],
        Armor_Class <= input$Armor_Class[2],
        if (input$Speaks_Language) Speaks_Language == 1 else TRUE,
        if (input$Legendary_Creature) Legendary_Creature == 1 else TRUE,
        #Alignment %in% input$alignment_filter
      ) %>%
      mutate(PredictedCR = predict(cr_model, newdata = .))
  })
  
  # Plot: Actual vs Predicted CR
  output$comparisonPlot <- renderPlot({
    df <- filtered_data()
    
    dat %>%
    filter(
      Hit_Points >= input$Hit_Points[1],
      Hit_Points <= input$Hit_Points[2],
      Armor_Class >= input$Armor_Class[1],
      Armor_Class <= input$Armor_Class[2],
      if (input$Speaks_Language) Speaks_Language == 1 else TRUE,
      if (input$Legendary_Creature) Legendary_Creature == 1 else TRUE,
      Alignment %in% input$alignment_filter
    ) %>%
    mutate(PredictedCR = predict(cr_model, newdata = .))
    
    ggplot(df, aes(x = Challenge_Rating, y = PredictedCR, size=as.factor(Size))) +
      geom_point() +
      geom_text(aes(label = paste(Monster)),
                hjust = -0.1, vjust = 0.5, size = 3) +
      geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
      labs(title = "Predicted vs Actual Challenge Ratings",
           x = "Actual CR", y = "Predicted CR", size="Monster Size") +
      theme_minimal()
    
    
  })
  
  # Optional table
  output$monsterTable <- renderDataTable({
    filtered_data()
  })

    }

# Run the application 
shinyApp(ui = ui, server = server)
