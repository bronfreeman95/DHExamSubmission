# Set CRAN mirror
options(repos = c(CRAN = "https://cran.rstudio.com"))

# Load necessary libraries
library(ggplot2)
library(plotly)
library(shiny)
library(shinythemes)
library(thematic)
library(shinyjs)
library(shinydashboard)

# Load data
allostatic_load_data <- read.csv("ALI.csv", header = TRUE, sep = ";", row.names = NULL, stringsAsFactors = FALSE)

# Data cleaning for specific columns
columns_to_clean <- c("Age", "HbA1c", "LDL", "Diastolic_Blood_Pressure", "Heart_Rate_Variability", "Waist_Circumference", "Perceived_Stress_Score")

clean_numeric <- function(x) {
  as.numeric(gsub(",", ".", x))
}

clean_columns <- intersect(columns_to_clean, colnames(allostatic_load_data))
allostatic_load_data[, clean_columns] <- lapply(allostatic_load_data[, clean_columns], clean_numeric)
allostatic_load_data <- na.omit(allostatic_load_data)

# Function to calculate ALI score
calculate_ALI_score <- function(row) {
  score <- 0
  
  if (!is.na(row["Diastolic_Blood_Pressure"]) && (as.numeric(row["Diastolic_Blood_Pressure"]) >= 85 | row["BPMedication"] == "Yes")) {
    score <- score + 1
  }
  
  if (!is.na(row["HbA1c"]) && !is.na(as.numeric(row["HbA1c"])) && (as.numeric(row["HbA1c"]) >= 5.7 | row["SugarMedication"] == "Yes")) {
    score <- score + 1
  }
  
  if (!is.na(row["LDL"]) && (as.numeric(row["LDL"]) >= 3.7 | row["CholestrolMedication"] == "Yes")) {
    score <- score + 1
  }
  
  if (!is.na(row["Waist_Circumference"]) &&
      ((row["Gender"] == "Male" & as.numeric(row["Waist_Circumference"]) >= 94) |
       (row["Gender"] == "Female" & as.numeric(row["Waist_Circumference"]) >= 80))) {
    score <- score + 1
  }
  
  if (!is.na(row["Heart_Rate_Variability"]) && as.numeric(row["Heart_Rate_Variability"]) <= 30) {
    score <- score + 1
  }
  
  risk <- ifelse(score >= 4, "High", "Low")
  
  return(list(Score = as.integer(score), Risk = risk))
}

# Assuming 'Age' and 'Gender' are already part of your dataset
# If not, you may need to create these variables based on age ranges and gender information in your dataset

# Create age categories
allostatic_load_data$Age_Category <- cut(allostatic_load_data$Age, breaks = c(0, 30, 40, 50, 60, 70, 80, 90, Inf), labels = c("<30", "30-40", "40-50", "50-60", "60-70", "70-80", "80-90", ">90"))

# Calculate individual ALI scores for each row
ali_scores <- apply(allostatic_load_data, 1, calculate_ALI_score)
allostatic_load_data$ALI_Score <- sapply(ali_scores, function(x) x$Score)
allostatic_load_data$ALI_Risk <- sapply(ali_scores, function(x) x$Risk)

# Define mapping between display names and original variable names
variable_mapping <- c("HbA1c (%)" = "HbA1c",
                      "LDL(mmol/L)" = "LDL",
                      "Diastolic Blood Pressure (mmHg)" = "Diastolic_Blood_Pressure",
                      "Heart Rate Variability (ms)" = "Heart_Rate_Variability",
                      "Waist Circumference (cm)" = "Waist_Circumference",
                      "Perceived Stress Score (out of 40)" = "Perceived_Stress_Score")

# Define UI for the Shiny app
ui <- fluidPage(
  useShinyjs(),  # Enable shinyjs
  
  theme = shinytheme("superhero"),  # Set the theme to 'superhero'
  
  fluidRow(
    column(width = 8, offset = 2,
           # Title without jumbotron
           h2("Interactive ALI Scatterplot", align = "center")
    )
  ),
  
  sidebarLayout(
    sidebarPanel(
      # Dropdown for X-axis variable
      selectInput("x_variable", "Choose a Health Metric to Explore:", 
                  choices = names(variable_mapping),
                  selected = "Perceived Stress Score"),
      # Dropdown for demographic selection
      selectInput("view_option", "Select Demographic:", choices = c("All", "Male", "Female")),
      # Change age filter to selectInput dropdown for different age categories
      selectInput("age_group_selector", "Filter by Age Group:",
                  choices = c("All Ages", "<30", "30-40", "40-50", "50-60", "60-70", "70-80", "80-90", ">90"),
                  selected = "All Ages"),
      
      # Additional Text
      HTML("<br /><h5 style='font-size: 14px;'>How does ALI risk assessment work?</h5>
           <p style='font-size: 12px;'>For each of the five health metrics, individuals get a score of \"1\" if the measurement is outside the normal range or if they are on prescribed medication. A score of \"0\" is given if the measurement is normal. The ALI score is the sum of these, ranging from 0 to 5. A score of 3 or less is low risk, while 4 or more is considered high risk, indicating a higher allostatic load. Credit to Mauss, Jarczok and Fischer for the concept of a streamlined ALI score. </p>"),
      
      # Additional Text
      HTML("<h5 style='font-size: 14px;'>Understanding the Scatter Plot</h5>
           <p style='font-size: 12px;'>The scatter plot shows the relationship between the selected health metric and the ALI score. Each point represents an individual, color-coded by ALI risk (Low/High). Use the sidebar to filter the data and explore the relationships between different health metrics and ALI scores.</p>")
    ),
    
    mainPanel(
      plotlyOutput("ali_scatter_plot"),
      
      # Information for General Audience
      HTML("<h4 style='font-size: 14px;'>Understanding the link between Stress and Health:</h4>
           <p style='font-size: 12px;'>Ever wondered how stress impacts your health? Recent research found that stress has a tangible effect on your body, known as \"allostatic load\"—the wear and tear on your body due to chronic stress. Allostatic load represents the cumulative toll on your health from stressors.Researchers measured stress using the Perceived Stress Scale (PSS), gauging how stressed you feel, and allostatic load using the Allostatic Load Index-5, including five health indicators. The study found that higher perceived stress aligns with a higher allostatic load, indicating that perceived stress could serve as an early indicator of potential health risks related to stress.Monitoring your stress levels and basic health metrics might provide insights into your body's overall stress-related wear and tear, acting as an early warning system for potential health issues. Understanding this connection can help you make lifestyle changes and seek support when needed, contributing to a healthier, less stressful life.</p>")
    )
  )
)

# Define server logic for the Shiny app
server <- function(input, output) {
  output$ali_scatter_plot <- renderPlotly({
    # Filter data based on user inputs
    filtered_data <- allostatic_load_data
    
    if (input$view_option != "All") {
      filtered_data <- subset(filtered_data, Gender == input$view_option)
    }
    
    # Filter data based on age category
    if (input$age_group_selector != "All Ages") {
      filtered_data <- subset(filtered_data, Age_Category == input$age_group_selector)
    }
    
    # Create the scatter plot with jitter and custom tooltip
    p <- ggplot(filtered_data, aes(x = .data[[variable_mapping[input$x_variable]]], y = ALI_Score, color = ALI_Risk)) +
      geom_point(position = position_jitter(width = 0.3), size = 2, alpha = 0.7) +
      theme_minimal() +  # Use thematic theme
      scale_y_continuous(breaks = seq(0, 5, 1), limits = c(0, 5)) +  # Set y-axis breaks and limits
      scale_x_continuous(breaks = seq(min(filtered_data[[variable_mapping[input$x_variable]]]), max(filtered_data[[variable_mapping[input$x_variable]]]), length.out = 5),
                         limits = c(min(filtered_data[[variable_mapping[input$x_variable]]]), max(filtered_data[[variable_mapping[input$x_variable]]]))) +  # Set x-axis breaks and limits
      scale_color_manual(values = c("Low" = "yellow", "High" = "orange"), labels = c("Low" = "ALI Low", "High" = "ALI High")) +  # Color mapping and legend labels for ALI Risk
      labs(
        title = paste(""),
        x = switch(input$x_variable,
                   "Diastolic_Blood_Pressure" = "Diastolic Blood Pressure (mmHg)",
                   "Waist_Circumference" = "Waist Circumference (cm)",
                   "Perceived_Stress_Score" = "Perceived Stress Score (score out of 40)",
                   "HbA1c" = "HbA1c (%)",
                   "LDL" = "LDL (mmol/L)",
                   input$x_variable),
        y = "ALI Score",
        color = "ALI Risk"
      ) +
      theme(
        axis.text = element_text(color = "black"),  # Change axis text color to black for visibility
        axis.ticks = element_line(color = "black"),  # Change axis tick marks color to black
        panel.background = element_rect(fill = "#485563"),  # Adjust panel background color
        legend.text = element_text(color = "black"),  # Change legend text color to black
        legend.position = "bottom"  # Adjust legend position
      )
    
    return(p)
  })
}

# Run the Shiny app
shinyApp(ui = ui, server = server)
