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
library(ggplot2)
library(shinyWidgets)

# Read the CSV file
dat <- read.csv("monsters_cleaned.csv", header = TRUE, row.names = 1, 
                col.names = c("Monster", "Size", "Type","Alignment","Armor_Class", "Hit_Points","Strength", "Dexterity",
                              "Constitution", "Intelligence", "Wisdom", "Charisma", "Speed", 
                              "Speaks_Language", "Legendary_Creature","Challenge_Rating"))

dat$Size <- factor(dat$Size, levels = c("Tiny", "Small", "Medium", "Large", "Huge", "Gargantuan"), ordered = TRUE)
dat$Type <- trimws(dat$Type)
dat$Type <- as.factor(dat$Type)

alignment_levels <- c(
  "Lawful Good", "Neutral Good", "Chaotic Good",
  "Lawful Neutral", "True Neutral", "Chaotic Neutral",
  "Lawful Evil", "Neutral Evil", "Chaotic Evil",
  "Unaligned", "Any", "Evil (unspecified)", "Chaotic (unspecified)",
  "Non-Good", "Non-Lawful", "Neutral (Good or Evil)", "Other"
)

dat$Alignment <- factor(dat$Alignment,levels = alignment_levels,ordered = TRUE)

# Build Prediction Challenge Rating
cr_model <- lm(Challenge_Rating~Armor_Class+Hit_Points+Strength+Dexterity+Constitution+Intelligence+
                 Wisdom+Charisma+Speed+Speaks_Language+Legendary_Creature, data=dat)

# Define UI for application that draws a histogram
ui <- fluidPage(

  titlePanel("Monster Challenge Estimator"),
  
  tabsetPanel(
    
    tabPanel("Explore Your Monsters",
     sidebarLayout(
       sidebarPanel(
         sliderInput("Hit_Points", "HP Range:", min = min(dat$Hit_Points), max = max(dat$Hit_Points), 
                  value = c(min(dat$Hit_Points), max(dat$Hit_Points))),
         sliderInput("Armor_Class", "AC Range:", min=min(dat$Armor_Class), max=max(dat$Armor_Class), 
                  value=c(min(dat$Armor_Class), max(dat$Armor_Class))),
         checkboxInput("Speaks_Language", "Only monsters that understand languages", value = FALSE),
         checkboxInput("Legendary_Creature", "Only legendary monsters", value=FALSE),
         checkboxInput("fast_only", "Only fast monsters (Speed > 30)", value = FALSE),
         
         h5(strong("Filter by Ability Scores:")),
         
         checkboxInput("high_str", "Strength: Stronger than average monsters", value = FALSE),
         checkboxInput("high_dex", "Dexterity: More Dextrous than average monsters", value = FALSE),
         checkboxInput("high_con", "Constitution: Higher Constitution than average monsters", value = FALSE),
         checkboxInput("high_wis", "Wisdom: Wiser than average monsters", value = FALSE),
         checkboxInput("high_intel", "Intelligence: Smarter than average monsters", value = FALSE),
         checkboxInput("high_cha", "Charisma: More Charming than average monsters", value = FALSE),
         pickerInput("type_filter", "Select Monster Type(s):",
                  choices = sort(unique(dat$Type)),
                  selected = unique(dat$Type),
                  multiple = TRUE,
                  options = list(`actions-box` = TRUE, `live-search` = TRUE)),
         actionButton("clear_all", "Clear All")
       ),
      mainPanel(
         plotOutput("comparisonPlot"),
         dataTableOutput("monsterTable")
      )
     )
    ), # end Tab1
    
    # Tab 2
    
    tabPanel("Monster Encounter Prediction",
             fluidRow(
               column(4,
                      numericInput("num_players", "Number of Players:", value = 4, min = 1, max = 10),
                      numericInput("player_level", "Player Level (1–20):", value = 1, min = 1, max = 20),
                      numericInput("num_monsters", "Number of Monsters:", value = 1, min = 1, max = 50),
                      actionButton("reset_encounter", "Reset")
               ),
               column(8,
                      verbatimTextOutput("dangerAssessment"),
                      dataTableOutput("monsterDetails")
               ),
               pickerInput("monster_filter2", "Select Monster:",
                           choices = sort(unique(dat$Monster)),
                           selected = unique(dat$Monster),
                           multiple = FALSE,
                           options = list(`actions-box` = TRUE, `live-search` = TRUE))
             )
    ), # end tab2
    tabPanel("Tab 3"
    ) # end Tab 3
  ) # tabsetPanel end
) # ui end end

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  # Tab 1: Reset
  observeEvent(input$clear_all, {
    updateSliderInput(session, "Hit_Points", value = c(min(dat$Hit_Points), max(dat$Hit_Points)))
    updateSliderInput(session, "Armor_Class", value = c(min(dat$Armor_Class), max(dat$Armor_Class)))
    updateCheckboxInput(session, "Speaks_Language", value = FALSE)
    updateCheckboxInput(session, "Legendary_Creature", value = FALSE)
    updateCheckboxInput(session, "fast_only", value = FALSE)
    updateCheckboxInput(session, "high_str", value = FALSE)
    updateCheckboxInput(session, "high_dex", value = FALSE)
    updateCheckboxInput(session, "high_con", value = FALSE)
    updateCheckboxInput(session, "high_wis", value = FALSE)
    updateCheckboxInput(session, "high_intel", value = FALSE)
    updateCheckboxInput(session, "high_cha", value = FALSE)
    updateSelectInput(session, "type_filter", selected = unique(dat$Type))
  })
  
  # Tab 1: Reactive filtered dataset
  filtered_data <- reactive({
    req(dat)
    
    dat %>%
      filter(
        Hit_Points >= input$Hit_Points[1],
        Hit_Points <= input$Hit_Points[2],
        Armor_Class >= input$Armor_Class[1],
        Armor_Class <= input$Armor_Class[2],
        if (input$Speaks_Language) Speaks_Language == 1 else TRUE,
        if (input$Legendary_Creature) Legendary_Creature == 1 else TRUE,
        if (input$fast_only) Speed > 30 else TRUE,
        if (input$high_str) Strength > 10 else TRUE,
        if (input$high_dex) Dexterity > 10 else TRUE,
        if (input$high_con) Constitution > 10 else TRUE,
        if (input$high_wis) Wisdom > 10 else TRUE,
        if (input$high_intel) Intelligence > 10 else TRUE,
        if (input$high_cha) Charisma > 10 else TRUE,
        tolower(Type) %in% tolower(input$type_filter)
      ) %>%
      mutate(PredictedCR = predict(cr_model, newdata = .))
  })
  
  # Tab1 Plot
  output$comparisonPlot <- renderPlot({
    df <- filtered_data()
    
    ggplot(df, aes(x = Challenge_Rating, y = PredictedCR, size = as.factor(Size))) +
      geom_point(shape = 21, alpha = 0.5 ,fill = "lightblue3", color="black") +
      geom_text(aes(label = Monster), hjust = -0.1, vjust = 0.5, size = 3) +
      geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
      coord_cartesian(xlim=c(0,30), ylim=c(0,30))+
      labs(title = "Predicted vs Actual Challenge Ratings",
           x = "Actual CR", y = "Predicted CR", size = "Monster Size") +
      theme_minimal()
  })
  
  # Tab1 Table
  output$monsterTable <- renderDataTable({
    filtered_data() %>%
      select(Monster, Size, Type, Alignment, Challenge_Rating, PredictedCR)
  })
  
  # Tab 2: Select monsters
  selected_monsters <- reactive({
    req(input$monster_filter2)
    
    dat %>%
      filter(Monster %in% input$monster_filter2) %>%
      mutate(PredictedCR = predict(cr_model, newdata = .))
  })
  
  output$monsterDetails <- renderDataTable({
    selected_monsters()
  })
  
  # Tab 2: Reset encounter
  observeEvent(input$reset_encounter, {
    updateNumericInput(session, "num_players", value = 4)
    updateNumericInput(session, "player_level", value = 1)
    updateNumericInput(session, "num_monsters", value = 1)
    updatePickerInput(session, "monster_filter", selected = "")
  })
  
  # Tab 2: Output
  
  output$dangerAssessment <- renderText({
    df <- selected_monsters()
    
    if (nrow(df) == 0) {
      return("Please select at least one monster.")
    }
    
    avg_cr <- mean(df$PredictedCR)
    players_power <- input$num_players * input$player_level
    monsters_power <- input$num_monsters * avg_cr
    
    if (players_power < monsters_power) {
      "***Players likely to die or be seriously maimed***"
    } else {
      "Players likely to survive!"
    }
  })
  
  # Tab 3: 
  output$comparisonPlot <- renderPlot({
    df <- filtered_data()
    
    ggplot(df, aes(x = Challenge_Rating, y = PredictedCR, size = as.factor(Size))) +
      geom_point(shape = 21, alpha = 0.5 ,fill = "lightblue3", color="black") +
      geom_text(aes(label = Monster), hjust = -0.1, vjust = 0.5, size = 3) +
      geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
      coord_cartesian(xlim=c(0,30), ylim=c(0,30))+
      labs(title = "Predicted vs Actual Challenge Ratings",
           x = "Actual CR", y = "Predicted CR", size = "Monster Size") +
      theme_minimal()
  })
  
  
} # end server


# Run the application 
shinyApp(ui = ui, server = server)
