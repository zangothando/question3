# Load necessary libraries
library(shiny)
library(shinythemes)
library(ggplot2)
library(dplyr)
library(tidyr)

# Load the dataset
df <- read.csv("cleaned_survey_data.csv")

# Define UI
ui <- fluidPage(
  theme = shinytheme("flatly"),  # Use a professional theme
  titlePanel("Eduvos IT Graduates Survey Dashboard"),
  sidebarLayout(
    sidebarPanel(
      selectInput("study_field", "Select Study Field:", 
                  choices = unique(df$StudyField)),
      selectInput("tool_category", "Select Tool Category:", 
                  choices = c("Programming Languages", "Databases", "Web Frameworks", "AI Tools")),
      actionButton("update", "Update Dashboard")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Top Tools", plotOutput("top_tools_plot")),
        tabPanel("Industry Analysis", plotOutput("industry_plot")),
        tabPanel("Employment Rate", plotOutput("employment_plot"))
      )
    )
  )
)

# Define server logic
server <- function(input, output) {
  # Reactive data for top tools
  top_tools_data <- reactive({
    tool_column <- switch(input$tool_category,
                          "Programming Languages" = "ProgLang",
                          "Databases" = "Databases",
                          "Web Frameworks" = "WebFramework",
                          "AI Tools" = "AITool")
    
    df %>%
      filter(StudyField == input$study_field) %>%
      select(all_of(tool_column)) %>%
      separate_rows(all_of(tool_column), sep = ";") %>%
      group_by(Tool = get(tool_column)) %>%
      summarise(Count = n(), .groups = "drop") %>%
      arrange(desc(Count)) %>%
      head(10)
  })
  
  # Reactive data for industry analysis
  industry_data <- reactive({
    df %>%
      filter(StudyField == input$study_field) %>%
      separate_rows(Industry, sep = ";") %>%
      group_by(Industry) %>%
      summarise(Count = n(), .groups = "drop") %>%
      arrange(desc(Count)) %>%
      head(10)
  })
  
  # Reactive data for employment rate
  employment_data <- reactive({
    df %>%
      filter(StudyField == input$study_field) %>%
      summarise(EmploymentRate = mean(Employment == "Employed", na.rm = TRUE))
  })
  
  # Plot for top tools
  output$top_tools_plot <- renderPlot({
    data <- top_tools_data()
    ggplot(data, aes(x = reorder(Tool, Count), y = Count)) +
      geom_bar(stat = "identity", fill = "steelblue") +
      coord_flip() +
      labs(title = paste("Top", input$tool_category, "for", input$study_field),
           x = input$tool_category, y = "Count") +
      theme_minimal()
  })
  
  # Plot for industry analysis
  output$industry_plot <- renderPlot({
    data <- industry_data()
    ggplot(data, aes(x = reorder(Industry, Count), y = Count)) +
      geom_bar(stat = "identity", fill = "steelblue") +
      coord_flip() +
      labs(title = paste("Top Industries for", input$study_field),
           x = "Industry", y = "Count") +
      theme_minimal()
  })
  
  # Plot for employment rate
  output$employment_plot <- renderPlot({
    data <- employment_data()
    ggplot(data, aes(x = input$study_field, y = EmploymentRate)) +
      geom_bar(stat = "identity", fill = "steelblue") +
      labs(title = paste("Employment Rate for", input$study_field),
           x = "Study Field", y = "Employment Rate") +
      theme_minimal()
  })
}

# Run the application
shinyApp(ui = ui, server = server)