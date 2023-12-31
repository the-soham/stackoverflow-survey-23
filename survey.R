
library(htmltools)
library(plotly)

library(flexdashboard)
library(lubridate)
library(dplyr)
library(ggplot2)
library(ggstance)
library(ggalt)
library(htmltools)
library(plotly)
library(shiny)
library(highcharter)
library(tidyverse)

#library(scales)

# Read the dataset'
setwd('G:/flexdashboards/stackoverflow-survey-23')
survey <- read.csv('survey.csv')
glimpse(survey)

# Assuming your dataset is named 'survey'
edlevel_data <- survey %>% 
  filter(!is.na(EdLevel)) %>%  # Exclude rows with NA values in EdLevel column
  group_by(EdLevel) %>%
  summarise(count = n())

# Calculate percentage
edlevel_data$percentage <- (edlevel_data$count / sum(edlevel_data$count)) * 100

# Reorder the data frame based on percentage
edlevel_data <- edlevel_data[order(edlevel_data$percentage, decreasing = TRUE), ]

# Create the Highchart:



library(stringr)
# Assuming your dataset is named 'survey'
learning_data <- survey %>% 
  filter(!is.na(LearnCode) & !grepl("Other \\(please specify\\)", LearnCode)) %>%  # Exclude rows with NA values and containing "Other (please specify)" in LearnCode column
  separate_rows(LearnCode, sep = ";") %>%  # Split multiple learning methods into separate rows
  group_by(LearnCode) %>%
  summarise(count = n())

# Calculate percentage based on the total number of rows
learning_data$percentage <- (learning_data$count / nrow(survey)) * 100

# Reorder the data frame based on percentage
learning_data <- learning_data[order(learning_data$percentage, decreasing = TRUE), ]

learn_online <- survey|>filter(!is.na(LearnCodeOnline) & !grepl("Other \\(Please specify\\)", LearnCodeOnline))|>
  separate_rows(LearnCodeOnline, sep = ";")|>
  group_by(LearnCodeOnline)|>
  summarise(count = n())


learn_online$percentage = (learn_online$count / nrow(survey|>filter(!is.na(LearnCodeOnline))))*100


learn_online <- learn_online[order(learn_online$percentage, decreasing = TRUE),]
learn_online


lonl = survey |> 
  filter(!is.na(LearnCodeOnline) & !grepl("Other \\(Please specify\\)", LearnCodeOnline)) |>
  separate_rows(LearnCodeOnline, sep = ";") |>
  group_by(LearnCodeOnline) |>
  summarise(count = n()) |>
  mutate(percentage = (count / nrow(survey)) * 100) |>
  arrange(desc(percentage))

lonl