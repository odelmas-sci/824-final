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

alignment_to_coords <- function(Alignment) {
  # Lowercase for safety
  al <- tolower(Alignment)
  x <- ifelse(grepl("lawful", al), 1,
              ifelse(grepl("chaotic", al), -1,
                     0))
  y <- ifelse(grepl("good", al), 1,
              ifelse(grepl("evil", al), -1,
                     0))
  # For "neutral" alone (no good/evil or lawful/chaotic), make (0,0)
  if (al %in% c("neutral", "true neutral")) {
    x <- 0
    y <- 0
  }
  data.frame(X = x, Y = y)
}

coords <- do.call(rbind, lapply(dat$Alignment, alignment_to_coords))
dat <- cbind(dat, coords)

# Build Prediction Challenge Rating
cr_model <- lm(Challenge_Rating~Armor_Class+Hit_Points+Strength+Dexterity+Constitution+Intelligence+
                 Wisdom+Charisma+Speed+Speaks_Language, data=dat)

# Define UI for application that draws a histogram
ui <- fluidPage(

  titlePanel("Monster Challenge Rating Estimator"),
  
  tabsetPanel(
    
    tabPanel("Tab1",
     sidebarLayout(
       sidebarPanel(
         sliderInput("Hit_Points", "HP Range:", min = min(dat$Hit_Points), max = max(dat$Hit_Points), 
                  value = c(min(dat$Hit_Points), max(dat$Hit_Points))),
         sliderInput("Armor_Class", "AC Range:", min=min(dat$Armor_Class), max=max(dat$Armor_Class), 
                  value=c(min(dat$Armor_Class), max(dat$Armor_Class))),
         checkboxInput("Speaks_Language", "Only monsters that understand languages", value = FALSE),
         checkboxInput("Legendary_Creature", "Only legendary monsters", value=FALSE),
         checkboxInput("fast_only", "Only fast monsters (Speed > 30)", value = FALSE),
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
    tabPanel("Alignment View",
             plotOutput("alignmentPlot")
    ) # end tab2
  ) # tabsetPanel end
) # ui end end

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  # Resets all
  observeEvent(input$clear_all, {
    updateSliderInput(session, "Hit_Points", value = c(min(dat$Hit_Points), max(dat$Hit_Points)))
    updateSliderInput(session, "Armor_Class", value = c(min(dat$Armor_Class), max(dat$Armor_Class)))
    updateCheckboxInput(session, "Speaks_Language", value = FALSE)
    updateCheckboxInput(session, "Legendary_Creature", value = FALSE)
    updateSelectInput(session, "type_filter", selected = unique(dat$Type))
  })
  
  # Reactive filtered dataset
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
        tolower(Type) %in% tolower(input$type_filter)
      ) %>%
      mutate(PredictedCR = predict(cr_model, newdata = .))
  })
  
  # Tab1 Plot
  output$comparisonPlot <- renderPlot({
    df <- filtered_data()
    
    ggplot(df, aes(x = Challenge_Rating, y = PredictedCR, size = as.factor(Size))) +
      geom_point(shape = 21, alpha = 0.5, fill = "lightblue3", color="black") +
      geom_text(aes(label = Monster), hjust = -0.1, vjust = 0.5, size = 3) +
      geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
      labs(title = "Predicted vs Actual Challenge Ratings",
           x = "Actual CR", y = "Predicted CR", size = "Monster Size") +
      theme_minimal()
  })
  
  # Tab1 Table
  output$monsterTable <- renderDataTable({
    filtered_data()
  })
  
  # Tab 2 plot: faceted alignment plot
  #output$alignmentPlot <- renderPlot({
  #  df <- filtered_data()
  #  ggplot(df, aes(x = Challenge_Rating, y = PredictedCR)) +
  #    facet_wrap(~ Alignment) +
  #    geom_point(shape = 21, alpha = 0.5, color = "lightblue3") +
  #    geom_text(aes(label = Monster), size = 3) +
  #    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  #    labs(title = "Predicted vs Actual Challenge Ratings by Alignment",
  #         x = "Actual CR", y = "Predicted CR") +
  #    theme_minimal()
  #})
  output$alignmentPlot <- renderPlot({
    df <- filtered_data()
    ggplot(mons, aes(x = X, y = Y)) +
      geom_jitter(aes(size = as.factor(Size), fill = as.factor(Size)), shape = 21, color = "black", alpha = 0.7) +
      geom_text(aes(label = Monster), size = 3) +
      scale_x_continuous(breaks = c(-1, 0, 1), labels = c("Chaotic", "Neutral", "Lawful")) +
      scale_y_continuous(breaks = c(-1, 0, 1), labels = c("Evil", "Neutral", "Good")) +
      labs(title = "Monsters on the Alignment Grid",
         x = "Lawful <-> Chaotic",
         y = "Good <-> Evil") +
      theme_minimal()
  })
  
} # end server


# Run the application 
shinyApp(ui = ui, server = server)
