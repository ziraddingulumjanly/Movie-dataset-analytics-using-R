# Load necessary libraries
library(ggplot2)
library(dplyr)
library(tidyr)
library(ggcorrplot)
setwd("C:\\Users\\zirad\\Downloads") 

# Load the dataset
movies_data <- read.csv("movies_updated.csv")

# Clean and preprocess the data
movies_data$runtime <- as.numeric(gsub(",", "", movies_data$runtime,,)) 
movies_cleaned <- movies_data %>%
  drop_na(budget, gross, runtime, score, votes) # Drop rows with missing values

# 1. Violin Plot: IMDb Score by Genre
ggplot(movies_cleaned, aes(x = genre, y = score, fill = genre)) +
  geom_violin(trim = FALSE) +
  labs(title = "Distribution of IMDb Scores by Genre",
       x = "Genre",
       y = "IMDb Score") +
  theme_minimal() +
  theme(panel.grid = element_blank(), 
        axis.text.x = element_text(angle = 45, hjust = 1))  

# 2. Box Plot: Runtime by Genre
ggplot(movies_cleaned, aes(x = genre, y = runtime, fill = genre)) +
  geom_boxplot() +
  labs(title = "Distribution of Runtime by Genre",
       x = "Genre",
       y = "Runtime (in minutes)") +
  theme_minimal() +
  theme(panel.grid = element_blank(), 
        axis.text.x = element_text(angle = 45, hjust = 1))  

# 3. Bar Plot: Average Gross Revenue by Genre
avg_gross_by_genre <- movies_cleaned %>%
  group_by(genre) %>%
  summarise(avg_gross = mean(gross, na.rm = TRUE)) %>%
  arrange(desc(avg_gross))

ggplot(avg_gross_by_genre, aes(x = reorder(genre, -avg_gross), y = avg_gross, fill = genre)) +
  geom_bar(stat = "identity") +
  labs(title = "Average Gross Revenue by Genre",
       x = "Genre",
       y = "Average Gross Revenue (in USD)") +
  theme_minimal() +
  theme(panel.grid = element_blank(), 
        axis.text.x = element_text(angle = 45, hjust = 1))  
# 4. Histogram: Number of Movies Released by Year
ggplot(movies_cleaned, aes(x = year)) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "black") +
  labs(title = "Number of Movies Released by Year",
       x = "Year",
       y = "Number of Movies") +
  theme_minimal() +
  theme(panel.grid = element_blank())  
# 5. Histogram: Number of Movies by Genre Released by Year
ggplot(movies_cleaned, aes(x = year)) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "black") +
  labs(title = "Number of Movies Released by Year and Genre",
       x = "Year",
       y = "Number of Movies") +
  theme_minimal() +
  facet_wrap(~ genre, scales = "free_y") +  # Facet by genre
  theme(panel.grid = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1))  

# 6. Stacked Histogram by Genre
ggplot(movies_cleaned, aes(x = year, fill = genre)) +
  geom_histogram(binwidth = 1, color = "black", position = "stack") +
  labs(title = "Number of Movies Released by Year and Genre (Stacked)",
       x = "Year",
       y = "Number of Movies") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set3") +  # Color palette for genres
  theme(panel.grid = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1))  

# 7. Heatmap: Correlation Matrix Using ggplot2

# Compute the correlation matrix
numeric_columns <- c("budget", "gross", "runtime", "score", "votes")
correlation_matrix <- cor(movies_cleaned[, numeric_columns], use = "complete.obs")

# Convert the correlation matrix to a long format for ggplot2
correlation_data <- as.data.frame(as.table(correlation_matrix))

# Plot the heatmap with correlation values displayed
ggplot(correlation_data, aes(Var1, Var2, fill = Freq)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white",
                       midpoint = 0, limit = c(-1, 1), space = "Lab",
                       name = "Correlation") +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Correlation Matrix Heatmap",
       x = "",
       y = "") +
  # Add correlation values as text labels
  geom_text(aes(label = sprintf("%.2f", Freq)), color = "black", size = 4)

