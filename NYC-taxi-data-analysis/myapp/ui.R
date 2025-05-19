# Data set NYC Taxi Trip Data 

# Import necessary libraries
library(shiny)
library(shinythemes)
library(MASS)
library(car)
library(magrittr)
library(dplyr)

# Read the main data set 
data <- read.csv("nyc_taxi_data.csv")

# Data transformation
# Create a new column called trip_distance_km 
data$trip_distance_km <- data$trip_distance * 1.60934
NROW(data$trip_distance_km)
sum(is.na(data$trip_distance_km)) # double check if we don't have any NA
# Create a new column called day_of_week_2 
data <- data %>%
  mutate(day_of_week = day_of_week + 1) %>%
  mutate(day_of_week_2 = ifelse(day_of_week %in% 1:5, "weekday", "weekend"))
# Change trip_duration: originally used in second => we will present data in minutes
data$trip_duration <- data$trip_duration / 60
names(data)[names(data) == 'trip_duration'] <- 'trip_duration_min'
# Drop unnecessary columns which will not be used in analyses
data <- data[, !(names(data) %in% c("trip_distance","payment_type", "mta_tax", "day", "year", "total_amount", "fare_amount", "extra", "imp_surcharge", "store_and_fwd_flag"))]
# Last change for column day_of_week
data <- data %>%
  mutate(day_of_week = case_when(
    day_of_week == "1" ~ "Monday",
    day_of_week == "2" ~ "Tuesday",
    day_of_week == "3" ~ "Wednesday",
    day_of_week == "4" ~ "Thursday",
    day_of_week == "5" ~ "Friday",
    day_of_week == "6" ~ "Saturday",
    day_of_week == "7" ~ "Sunday",
    TRUE ~ as.character(day_of_week)
  ))  

shinyUI(fluidPage(
  theme = shinytheme("slate"),
  titlePanel("Analiza zbioru danych NYC Taxi Data"),
  sidebarLayout(
    sidebarPanel(
      p("Autor: am109081"),
      fluidRow(
        column(12, HTML("<p><strong>Zbiór danych:</strong> NYC Taxi Trip Data to zbiór publicznie dostępnych informacji zebranych z przejazdów taksówką w Nowym Jorku. Dane te obejmują różnorodne aspekty podróży, takie jak odległość, czas trwania, obszar podróży, ilość napiwków, a także informacje dotyczące dnia tygodnia czy godziny.</p>
                        <p>Przedmiotem pracy jest analiza czynników wpływających na kwoty napiwków taksówkarzom w Nowym Yorku. Badanie to skupia się na identyfikacji zmiennych, takich jak odległość trasy, godzina przejazdu, dzień (czy to dzień roboczy, czy weekend), czas podróży, miejsce odbioru/ dowozu, które mogą mieć istotny wpływ na wysokość otrzymanych napiwków.</p>
                        <p><strong>Wykres 1:</strong> Wykres przedstawia rozkład kwot napiwków w danej jednostce.</p>
                        <p><strong>Wykres 2:</strong> Wykres przedstawia rozkład kwot napiwków w zależności od grupy dystansu przejazdu.</p>
                        <p><strong>Wykres 3:</strong> Wykres przedstawia średni czas podróży w zależności od godziny dnia.</p>
                        <p><strong>Wykres 4:</strong> Wykres przedstawia średnie kwoty napiwków dla pięciu najczęściej wybieranych lokalizacji docelowych.</p>"))
    ),
      fluidRow(
        column(12, selectInput("selectedDay", "Proszę wybrać dzień tygodnia:",
                               choices = unique(data$day_of_week),
                               selected = "Poniedziałek"))
      )
    ),
    mainPanel(
      fluidRow(
        column(6, plotOutput("plot1")),
        column(6, plotOutput("plot2"))
      ),
      fluidRow(
        column(6, plotOutput("plot3")),
        column(6, plotOutput("plot4"))
      )
    )
  )
))