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
library(DT)

# Read the CSV file
dat <- read.csv("monsters_cleaned.csv", header = TRUE, row.names = 1, 
                col.names = c("Monster", "Size", "Type","Alignment","Armor_Class", "Hit_Points","Strength", "Dexterity",
                              "Constitution", "Intelligence", "Wisdom", "Charisma", "Speed", 
                              "Speaks_Language", "Legendary_Creature","Challenge_Rating"))

dat$Size <- factor(dat$Size, levels = c("Tiny", "Small", "Medium", "Large", "Huge", "Gargantuan"), ordered = TRUE)

# Build Prediction Challenge Rating
cr_model <- lm(Challenge_Rating~Armor_Class+Hit_Points+Strength+Dexterity+Constitution+Intelligence+
                 Wisdom+Charisma+Speed+Speaks_Language+Legendary_Creature, data=dat)

# Define UI for application that draws a histogram
ui <- fluidPage(

  titlePanel("Monster Challenge Estimator"),
  
  tabsetPanel(
    
    # Tab 1 ####
    
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
    
    # Tab 2 ####
    
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
    
    # Tab 3 ####
    
    tabPanel("PCA by Monster Type",
             tabPanel("Monster Match Finder",
                      sidebarLayout(
                        sidebarPanel(
                          selectInput("pca_type", "Select Monster Type:",
                                      choices = sort(unique(dat$Type)),
                                      selected = unique(dat$Type)[1])
                        ),
                        mainPanel(
                          plotOutput("pcaPlot", height = "600px")
                        )
                      )
                      
             )
    
             ), # end Tab 3
    
    # Tab 4 ####
    
    tabPanel("Search for Monsters",
             tabPanel("Monster Finder",
                      sidebarLayout(
                        sidebarPanel(
                          textInput("search_monster", "Search by Monster Name:", placeholder = "Enter name..."),
                          selectInput("search_type", "Filter by Type:",
                                      choices = c("All", sort(unique(as.character(dat$Type)))),
                                      selected = "All")
                        ),
                        mainPanel(
                          h4("Matching Monsters"),
                          dataTableOutput("search_results"),
                          h4("Selected Monster Details"),
                          verbatimTextOutput("selected_monster_info")
                        )
                      )
             )
             
    ) # end Tab 4
  ) # tabsetPanel end
) # ui end end

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  # Tab 1: ####
  #Reset
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
  
  # Tab 2: ####
  
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
  
  # Tab 3: ####
  
  output$pcaPlot <- renderPlot({
    req(input$pca_type)
    
    # Filter data by selected type
    df <- dat %>% filter(Type == input$pca_type)
    
    # Select numeric predictors
    pca_data <- df %>%
      select(Hit_Points, Armor_Class, Strength, Dexterity, Constitution,
             Intelligence, Wisdom, Charisma, Speed, Speaks_Language, Legendary_Creature)
    
    # Remove columns with zero variance
    pca_data <- pca_data[, sapply(pca_data, function(x) length(unique(x)) > 1)]
    
    # Run PCA
    res.PCA <- prcomp(pca_data, center = TRUE, scale. = TRUE)
    
    # Add monster names as rownames
    rownames(pca_data) <- df$Monster
    
    # Plot PCA
    plot(res.PCA$x[, 1:2],
         pch = 19,
         col = "steelblue",
         xlab = "PC1",
         ylab = "PC2",
         main = paste("PCA of", input$pca_type))
    
    # Add monster names as text
    text(res.PCA$x[, 1:2],
         labels = rownames(pca_data),
         pos = 3,
         cex = 0.8)
  })
  
  # Tab 4: ####
  
  # Reactive: build a data frame from the user's input
  matching_monsters <- reactive({
    df <- dat
    
    # Filter by name
    if (input$search_monster != "") {
      df <- df %>% filter(grepl(input$search_monster, Monster, ignore.case = TRUE))
    }
    
    # Filter by type
    if (input$search_type != "All") {
      df <- df %>% filter(Type == input$search_type)
    }
    
    df %>%
      select(Monster, Type, Alignment, Size, Challenge_Rating)
  })
  
  # Render table with row selection
  output$search_results <- DT::renderDataTable({
    matching_monsters()
  }, selection = "single")
  
  # Show full monster info when selected
  output$selected_monster_info <- renderPrint({
    selected <- input$search_results_rows_selected
    if (is.null(selected)) {
      return("Select a monster above to view full stats.")
    }
    
    # Get selected monster name
    selected_monster <- matching_monsters()$Monster[selected]
    
    # Lookup full info in original data
    full_info <- dat %>% filter(Monster == selected_monster)
    
    as.list(full_info)
  })
  
  
} # end server


# Run the application 
shinyApp(ui = ui, server = server)
