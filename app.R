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
library(plotly)

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
    
    # Tab 1
    
    tabPanel("Explore Your Monsters",
     sidebarLayout(
       sidebarPanel(
         sliderInput("CR_slider", "Challenge Rating:", min = min(dat$Challenge_Rating), max = max(dat$Challenge_Rating), 
                     value = c(min(dat$Challenge_Rating), max(dat$Challenge_Rating))),
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
         plotlyOutput("comparisonPlot"),
         dataTableOutput("monsterTable")
      )
     )
    ), # end Tab1
    
    # Tab 2
    
    tabPanel("Predictor Explorer",
             fluidRow(
               sidebarLayout(
                 sidebarPanel(
                   selectInput("predictor_choice", "Select a Predictor:",
                               choices = c("Armor_Class", "Hit_Points", "Strength", "Dexterity", 
                                           "Constitution", "Intelligence", "Wisdom", "Charisma", 
                                           "Speed", "Speaks_Language", "Legendary_Creature"),
                               selected = "Armor_Class")
                   ),
                 mainPanel(
                   plotOutput("predictorPlot", height="600px")
                   )
                 )
               )
             ), # end Tab 2
    
    # Tab 3
    
    tabPanel("Build Your Ideal Monster",
             tabPanel("Monster Match Finder",
                      sidebarLayout(
                        sidebarPanel(
                          numericInput("input_ac", "Armor Class (1-30)", value = 15, min = 1, max = 30),
                          numericInput("input_hp", "Hit Points (1-700)", value = 50, min = 1, max = 700),
                          numericInput("input_str", "Strength (1-30)", value = 10, min = 1, max = 30),
                          numericInput("input_dex", "Dexterity (1-30)", value = 10, min = 1, max = 30),
                          numericInput("input_con", "Constitution (1-30)", value = 10, min = 1, max = 30),
                          numericInput("input_int", "Intelligence (1-30)", value = 10, min = 1, max = 30),
                          numericInput("input_wis", "Wisdom (1-30)", value = 10, min = 1, max = 30),
                          numericInput("input_cha", "Charisma (1-30)", value = 10, min = 1, max = 30),
                          sliderInput("input_speed", "Speed", min = 10, max = 120, value = 30, step = 10),
                          checkboxInput("input_language", "Understands Language", value = TRUE),
                          checkboxInput("input_legend", "Legendary Creature", value=FALSE),
                          actionButton("match_button", "Find Matching Monsters")
                        ),
                        mainPanel(
                          verbatimTextOutput("predicted_cr"),
                          dataTableOutput("matching_monsters"),
                          h4("Selected Monster Details"),
                          verbatimTextOutput("selected_monster_info")
                        )
                      )
             )
    
             ) # end Tab 3
  
  ) # tabsetPanel end
) # ui end end

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  # Tab 1: Reset
  observeEvent(input$clear_all, {
    updateSliderInput(session, "CR_slider", value = c(min(dat$Challenge_Rating), max(dat$Challenge_Rating)))
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
        Challenge_Rating >= input$CR_slider[1],
        Challenge_Rating <= input$CR_slider[2],
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
  
  output$comparisonPlot <- renderPlotly({
    df <- filtered_data()
    
    p <- ggplot(df, aes(x = Challenge_Rating, y = PredictedCR, 
                        text = paste("Monster:", Monster,
                                     "<br>CR:", Challenge_Rating,
                                     "<br>Predicted CR:", round(PredictedCR, 2)))) +
      geom_jitter(shape = 21, alpha = 0.5 , fill = "lightblue3", color = "black") +
      geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
      coord_cartesian(xlim = c(0, 30), ylim = c(0, 30)) +
      labs(title = "Predicted vs Actual Challenge Ratings",
           x = "Actual CR", y = "Predicted CR") +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 20, face = "bold"),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 14),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12)
      )
    
    ggplotly(p, tooltip = "text")
  })
  
  # Tab1 Table
  output$monsterTable <- renderDataTable({
    filtered_data() %>%
      select(Monster, Size, Type, Alignment, Challenge_Rating, PredictedCR)
  })
  
  # Tab 2: Output
  
  output$predictorPlot <- renderPlot({
    predictor <- input$predictor_choice
    df <- dat 
    
    ggplot(df, aes_string(x = predictor, y = "Challenge_Rating")) +
      geom_point(shape = 21, alpha = 0.5, fill = "steelblue", color = "black") +
      geom_smooth(method = "lm", se = FALSE, color = "red", linetype = "dashed") +
      labs(
        x = predictor,
        y = "Challenge Rating",
        title = paste("Relationship between", predictor, "and Challenge Rating")
      ) +
      theme(plot.title = element_text(size = 20, face = "bold"),   
            axis.title = element_text(size = 16),                  
            axis.text = element_text(size = 14),                   
            legend.title = element_text(size = 14),
            legend.text = element_text(size = 12) )+
      theme_minimal()
  })
  
  # Tab 3: 
  
  # Reactive: build a data frame from the user's input
  user_input <- reactive({
    data.frame(
      Armor_Class = input$input_ac,
      Hit_Points = input$input_hp,
      Strength = input$input_str,
      Dexterity = input$input_dex,
      Constitution = input$input_con,
      Intelligence = input$input_int,
      Wisdom = input$input_wis,
      Charisma = input$input_cha,
      Speed = input$input_speed,
      Speaks_Language = as.numeric(input$input_language),
      Legendary_Creature = as.numeric(input$input_legend)
    )
  })
  
  # Reactive: predict CR based on user input
  predicted_cr <- eventReactive(input$match_button, {
    predict(cr_model, newdata = user_input())
  })
  
  # Output predicted CR
  output$predicted_cr <- renderPrint({
    req(predicted_cr())
    paste0("Predicted Challenge Rating of your Monster: ", round(predicted_cr(), 2))
  })
  
  # Find closest matches from dataset
  output$matching_monsters <- DT::renderDataTable({
    req(predicted_cr())
    
    target_cr <- predicted_cr()
    
    dat %>%
      mutate(PredictedCR = predict(cr_model, newdata = .),
             Diff = abs(PredictedCR - target_cr)) %>%
      arrange(Diff) %>%
      select(Monster, Type, Alignment, Size, Challenge_Rating, PredictedCR, Diff) %>%
      head(10)
  }, selection = "single")
  
  # Selection of Monsters when Row is selected
  output$selected_monster_info <- renderPrint({
    selected <- input$matching_monsters_rows_selected
    if (is.null(selected)) {
      return("Select a monster above to view full stats.")
    }
    
    # Get the matching table (must match the one in renderDataTable)
    matching_df <- dat %>%
      mutate(PredictedCR = predict(cr_model, newdata = .),
             Diff = abs(PredictedCR - predicted_cr())) %>%
      arrange(Diff) %>%
      select(Monster, Type, Alignment, Size, Challenge_Rating, PredictedCR, Diff) %>%
      head(10)
    
    # Get selected monster name
    selected_monster <- matching_df$Monster[selected]
    
    # Lookup full info
    full_info <- dat %>% filter(Monster == selected_monster)
    
    # Show full stats
    as.list(full_info)
  })
  
  
} # end server


# Run the application 
shinyApp(ui = ui, server = server)
