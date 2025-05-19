# Data set NYC Taxi Trip Data 

# Import necessary libraries
library(ggplot2)
library(dplyr)
library(colorspace)
library(gridExtra)
library(grid)
library(corrplot)
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

data$distance_blocks <- cut(data$trip_distance_km, breaks = c(0, 15, 30, 45, 60, Inf), include.lowest = TRUE, labels = c("[0,15)", "[15,30)", "[30,45)", "[45,60)", "[60,Inf)"))

shinyServer(function(input, output) {
  daneFiltrowane <- reactive({
    dane <- data[data$day_of_week == input$selectedDay, ]
    return(dane)
  })
  
  output$plot1 <- renderPlot({
    daneFiltrowane() %>% 
      ggplot(aes(x = tip_amount)) +
      geom_histogram(binwidth = 1, fill = "seagreen", color = "black", alpha = 0.7) +
      theme_minimal() +
      theme(
        panel.background = element_rect(fill = "lightgray"),
        panel.grid.major = element_line(color = "white"),
        axis.text = element_text(color = "darkblue"),
        axis.title = element_text(color = "darkblue"),
        plot.title = element_text(face = "bold", size = 14)
      ) +
      xlab("Kwota napiwku") +
      ylab("Frekwencja") +
      ggtitle("Rozkład kwot napiwków") +
      xlim(0, 20)
  })
  
  output$plot2 <- renderPlot({
    daneFiltrowane() %>% 
      ggplot(aes(x = distance_blocks, y = tip_amount)) +
      geom_boxplot(fill = "seagreen") +
      theme(
        panel.background = element_rect(fill = "lightgray"),
        panel.grid.major = element_line(color = "white"),
        axis.text = element_text(color = "darkblue"),
        axis.title = element_text(color = "darkblue"),
        plot.title = element_text(face = "bold", lineheight = 14)
      ) +
      xlab("Przejazd w km") +
      ylab("Kwota Napiwku") +
      ggtitle("Rozkład kwot napiwków w Zależności od grupy dystansu")
  })
  
  output$plot3 <- renderPlot({
    daneFiltrowane() %>% 
      group_by(hour_of_day) %>%
      summarise(mean_duration = mean(trip_duration_min)) %>%
      ggplot(aes(x = hour_of_day, y = mean_duration)) +
      geom_line(color = "seagreen", size = 1) +
      labs(
        title = "Średni czas podróży na godzinę",
        x = "Godzina dnia",
        y = "Średni czas podróży (minuty)"
      ) +
      theme_minimal() +
      theme(
        panel.background = element_rect(fill = "lightgray"),
        panel.grid.major = element_line(color = "white"),
        axis.text = element_text(color = "darkblue"),
        axis.title = element_text(color = "darkblue"),
        plot.title = element_text(face = "bold", lineheight = 14)
      ) +
      ylim(30, 35) +
      scale_x_continuous(breaks = seq(0, 24, by = 1))
  })
  
  output$plot4 <- renderPlot({
    top_locations <- c(138, 230, 161, 162, 181)
    
    daneFiltrowane() %>% 
      filter(dropoff_location_id %in% top_locations) %>% 
      group_by(dropoff_location_id) %>% 
      summarise(mean_tip = mean(tip_amount)) %>% 
      ggplot(aes(x = as.factor(dropoff_location_id), y = mean_tip, fill = as.factor(dropoff_location_id))) +
      geom_bar(stat = "identity") +
      stat_summary(fun = mean, geom = "text", aes(label = round(..y.., 2)),
                   position = position_dodge(width = 0.9), vjust = -0.5, color = "black") +
      scale_fill_manual(values = heat_hcl(5)) +
      theme_minimal() +
      theme(
        panel.background = element_rect(fill = "lightgray"),
        panel.grid.major = element_line(color = "white"),
        axis.text = element_text(color = "darkblue"),
        axis.title = element_text(color = "darkblue"),
        plot.title = element_text(face = "bold", lineheight = 14)
      ) +
      xlab("ID lokalizacji docelowej") +
      ylab("Średnia kwota napiwku") +
      ggtitle("Średnie kwoty napiwków dla top 5 lok. doc.") +
      labs(fill = "Location ID")
  })
})
