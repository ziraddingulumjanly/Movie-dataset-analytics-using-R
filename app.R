# Load required libraries
library(shiny)
library(shinydashboard)
library(ggplot2)
library(plotly)
library(dplyr)

# Load the dataset
movies_data <- read.csv("movies_updated.csv")

# Data cleaning
movies_data$budget <- as.numeric(gsub("[^0-9]", "", movies_data$budget))
movies_data$gross <- as.numeric(gsub("[^0-9]", "", movies_data$gross))
movies_data$runtime <- as.numeric(gsub("[^0-9]", "", movies_data$runtime))

# Define UI
ui <- dashboardPage(
  dashboardHeader(title = "Movies Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Trends", tabName = "trends", icon = icon("chart-line")),
      menuItem("Top Movies", tabName = "top_movies", icon = icon("film")),
      menuItem("Genre Insights", tabName = "genre", icon = icon("chart-pie")),
      menuItem("Director/Star", tabName = "director_star", icon = icon("user"))
    )
  ),
  dashboardBody(
    tabItems(
      # Trends Tab
      tabItem(tabName = "trends",
              fluidRow(
                box(title = "Gross vs Budget Over Years", status = "primary", solidHeader = TRUE,
                    plotlyOutput("trendPlot"), width = 12)
              )
      ),
      
      # Top Movies Tab
      tabItem(tabName = "top_movies",
              fluidRow(
                box(title = "Filter by IMDb Score", status = "primary", solidHeader = TRUE,
                    sliderInput("scoreRange", "IMDb Score Range:",
                                min = min(movies_data$score, na.rm = TRUE),
                                max = max(movies_data$score, na.rm = TRUE),
                                value = c(7, 10), step = 0.1), width = 12),
                box(title = "Filter by Year Range", status = "primary", solidHeader = TRUE,
                    sliderInput("yearRange", "Year Range:",
                                min = min(movies_data$year, na.rm = TRUE),
                                max = max(movies_data$year, na.rm = TRUE),
                                value = c(2000, 2020), step = 1), width = 12)
              ),
              fluidRow(
                box(title = "Top Movies Based on Filters", status = "success", solidHeader = TRUE,
                    tableOutput("filteredMoviesTable"), width = 12)
              )
      ),
      
      # Genre Insights Tab
      tabItem(tabName = "genre",
              fluidRow(
                box(title = "Most Popular Genres by Count", status = "info", solidHeader = TRUE,
                    plotlyOutput("genreBarPlot"), width = 6),
                box(title = "Average Gross Earnings by Genre", status = "info", solidHeader = TRUE,
                    plotlyOutput("genreGrossPlot"), width = 6)
              )
      ),
      
      # Director/Star Analysis Tab
      tabItem(tabName = "director_star",
              fluidRow(
                box(title = "Top Directors by Average IMDb Score", status = "warning", solidHeader = TRUE,
                    plotlyOutput("topDirectorsPlot"), width = 6),
                box(title = "Top Stars by Gross Earnings", status = "warning", solidHeader = TRUE,
                    plotlyOutput("topStarsPlot"), width = 6)
              )
      )
    )
  )
)

# Define Server
# Define Server
server <- function(input, output) {
  # Trends Plot
  output$trendPlot <- renderPlotly({
    trend_data <- movies_data %>%
      filter(!is.na(budget) & !is.na(gross)) %>%
      group_by(year) %>%
      summarise(Average_Budget = mean(budget, na.rm = TRUE),
                Average_Gross = mean(gross, na.rm = TRUE))
    
    plot <- ggplot(trend_data, aes(x = year)) +
      geom_line(aes(y = Average_Budget, color = "Average Budget")) +
      geom_line(aes(y = Average_Gross, color = "Average Gross")) +
      theme_minimal() +
      theme(panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank()) +  # Remove grid lines
      labs(title = "Gross vs Budget Trends Over the Years",
           x = "Year", y = "USD (in millions)", color = "Metric")
    
    ggplotly(plot)
  })
  
  # Filtered Movies Table
  output$filteredMoviesTable <- renderTable({
    movies_data %>%
      filter(score >= input$scoreRange[1], score <= input$scoreRange[2],
             year >= input$yearRange[1], year <= input$yearRange[2]) %>%
      arrange(desc(score)) %>%
      select(name, year, genre, score, gross, votes) %>%
      head(10)
  })
  
  # Genre Bar Plot
  output$genreBarPlot <- renderPlotly({
    genre_count <- movies_data %>%
      group_by(genre) %>%
      summarise(Count = n()) %>%
      arrange(desc(Count))
    
    plot <- ggplot(genre_count, aes(x = reorder(genre, Count), y = Count, fill = genre)) +
      geom_bar(stat = "identity", show.legend = FALSE) +
      coord_flip() +
      theme_minimal() +
      theme(panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank()) +  # Remove grid lines
      labs(title = "Most Popular Genres by Count",
           x = "Genre", y = "Number of Movies")
    
    ggplotly(plot)
  })
  
  # Genre Gross Plot
  output$genreGrossPlot <- renderPlotly({
    genre_gross <- movies_data %>%
      group_by(genre) %>%
      summarise(Average_Gross = mean(gross, na.rm = TRUE)) %>%
      arrange(desc(Average_Gross))
    
    plot <- ggplot(genre_gross, aes(x = reorder(genre, Average_Gross), y = Average_Gross, fill = genre)) +
      geom_bar(stat = "identity", show.legend = FALSE) +
      coord_flip() +
      theme_minimal() +
      theme(panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank()) +  # Remove grid lines
      labs(title = "Average Gross Earnings by Genre",
           x = "Genre", y = "Average Gross Earnings (in millions)")
    
    ggplotly(plot)
  })
  
  # Top Directors Plot
  output$topDirectorsPlot <- renderPlotly({
    top_directors <- movies_data %>%
      group_by(director) %>%
      summarise(Average_Score = mean(score, na.rm = TRUE)) %>%
      arrange(desc(Average_Score)) %>%
      head(10)
    
    plot <- ggplot(top_directors, aes(x = reorder(director, Average_Score), y = Average_Score, fill = director)) +
      geom_bar(stat = "identity", show.legend = FALSE) +
      coord_flip() +
      theme_minimal() +
      theme(panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank()) +  # Remove grid lines
      labs(title = "Top Directors by Average IMDb Score",
           x = "Director", y = "Average IMDb Score")
    
    ggplotly(plot)
  })
  
  # Top Stars Plot
  output$topStarsPlot <- renderPlotly({
    top_stars <- movies_data %>%
      group_by(star) %>%
      summarise(Total_Gross = sum(gross, na.rm = TRUE)) %>%
      arrange(desc(Total_Gross)) %>%
      head(10)
    
    plot <- ggplot(top_stars, aes(x = reorder(star, Total_Gross), y = Total_Gross, fill = star)) +
      geom_bar(stat = "identity", show.legend = FALSE) +
      coord_flip() +
      theme_minimal() +
      theme(panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank()) +  # Remove grid lines
      labs(title = "Top Stars by Gross Earnings",
           x = "Star", y = "Total Gross Earnings (in millions)")
    
    ggplotly(plot)
  })
}

# Run the app
shinyApp(ui = ui, server = server)

