
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



lonl = survey |> 
  filter(!is.na(LearnCodeOnline) & !grepl("Other \\(Please specify\\)", LearnCodeOnline)) |>
  separate_rows(LearnCodeOnline, sep = ";") |>
  group_by(LearnCodeOnline) |>
  summarise(count = n()) |>
  mutate(percentage = (count / nrow(survey)) * 100) |>
  arrange(desc(percentage))

online_courses = survey|>
  filter(!is.na(LearnCodeCoursesCert)) |>
  separate_rows(LearnCodeCoursesCert, sep = ";") |>
  group_by(LearnCodeCoursesCert) |>
  summarise(count = n())  |>
  mutate(percentage = (count/ nrow(survey |>filter(!is.na(LearnCodeCoursesCert)))) * 100) |>
  arrange(desc(percentage))

glimpse(survey)

prog_lang = survey |> 
  filter(!is.na(LanguageHaveWorkedWith)) |>
  separate_rows(LanguageHaveWorkedWith, sep = ";") |>
  group_by(LanguageHaveWorkedWith) |>
  summarise (count = n()) |>
  mutate(percentage = (count/ nrow( survey |>filter(!is.na(LanguageHaveWorkedWith)))) * 100) |>
  arrange(desc(percentage)) |>
  top_n(10)

prog_lang

generate_reactive_chart_data <- function(data, column, separator, count_column, display_option, top_n = NULL) {
  data %>%
    filter(!is.na({{column}}) & !grepl("Other \\(Please specify\\)", {{column}})) %>%
    separate_rows({{column}}, sep = {{separator}}) %>%
    group_by({{column}}) %>%
    summarise(count = n()) %>%
    mutate(percentage = (count / nrow(data)) * 100) %>%
    arrange(desc(percentage)) %>%
    top_n(ifelse(is.null(top_n), n(), top_n))
}



prog_lang_top10 <-   generate_reactive_chart_data(survey, LanguageHaveWorkedWith, ";", "count", input$display_option, top_n = 5)

print(prog_lang_top10, n=41)