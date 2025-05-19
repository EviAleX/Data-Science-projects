# Data set NYC Taxi Trip Data 

# Import necessary libraries
library(ggplot2)
library(dplyr)
library(colorspace)
library(gridExtra)
library(grid)
library(corrplot)
library(MASS)
library(magrittr)
library(fastDummies)
library(car)
library(rpart)
library(rpart.plot)
library(caret)
library(nnet)
library(pROC)

# Read the main data set 
data <- read.csv("nyc_taxi_data.csv")

# Data exploration
str(data)
summary(data)

# Check how many NA are in the data set
na_count <- colSums(is.na(data))
print(na_count)

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
data <- data[, !(names(data) %in% c("rate_code", "tolls_amount","trip_distance","payment_type", "mta_tax", "day", "year", "total_amount", "fare_amount", "extra", "imp_surcharge", "store_and_fwd_flag"))]
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

# Column details 
# X - Observation number
# trip_distance - The elapsed trip distance in miles reported by the taximeter 
# tip_amount - Tip amount – This field is automatically populated for credit card tips. Cash tips are not included
# pickup_location_id - TLC Taxi Zone in which the taximeter was engaged
# dropoff_location_id	- TLC Taxi Zone in which the taximeter was disengaged
# year - 2018
# month - months (from 1-12)
# day_of_week - day of the week (0-6)
# hour_of_day - can be (0-23)
# trip_duration_min - value calculated in minutes
# calculated_total_amount - total amount, kinda the same as total_amount
# trip_distance_km - The elapsed trip distance converted to km 
# day_of_week_2 - day of week (weekday or weekend)

# Data analysis by each column 
# observation_number
names(data)[names(data) == 'X'] <- 'observation_number'
is_unique <- length(unique(data$observation_number)) == nrow(data)
print(is_unique)

# trip_distance_km
plot1 <- ggplot(data, aes(x = trip_distance_km)) +
  geom_histogram(binwidth = 1, fill = "seagreen", color = "black", alpha = 0.7) +
    theme_minimal() +
  theme(
    panel.background = element_rect(fill = "lightgray"),
    panel.grid.major = element_line(color = "white"),
    axis.text = element_text(color = "darkblue"),
    axis.title = element_text(color = "darkblue"),
    plot.title = element_text(face = "bold", size = 14)
  ) +
  xlab("Dystans podróży (km)") +
  ylab("Frekwencja") +
  ggtitle("Rozkład dystansów podróży w km")

# It seems that data set contains data with high values of trip_distance
# Although we can't say those are outliers, since it is real data
# Let's identify what was the highest distance
print(max(data$trip_distance_km))
print(sum(data$trip_distance_km == 0))

# Let's also create a histogram which we represent data by groups of 15 km
data$distance_blocks <- cut(data$trip_distance_km, breaks = c(0, 15, 30, 45, 60, Inf), include.lowest = TRUE, labels = c("[0,15)", "[15,30)", "[30,45)", "[45,60)", "[60,Inf)"))
custom_colors <- heat_hcl(length(unique(data$distance_blocks)))

plot5 <- ggplot(data, aes(x = distance_blocks, fill = distance_blocks)) +
  geom_bar() +
  scale_fill_manual(values = custom_colors) +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "lightgray"),
    panel.grid.major = element_line(color = "white"),
    axis.text = element_text(color = "darkblue"),
    axis.title = element_text(color = "darkblue"),
    plot.title = element_text(face = "bold", size = 14)
  ) +
  xlab("Dystans podróży (km)") +
  ylab("Liczba") +
  ggtitle("Rozkład dystansów podróży w grupach 15 km")

# tip_amount 
plot2 <- ggplot(data, aes(x = tip_amount)) +
  geom_histogram(binwidth = 5, fill = "seagreen", color = "black", alpha = 0.7) +
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
  ggtitle("Rozkład kwot napiwków")

print(max(data$tip_amount))

# Let's also create a histogram which we represent data by groups of 10 dollars
data$tip_blocks <- cut(data$tip_amount, breaks = c(0, 10, 20, 30, 40, Inf), include.lowest = TRUE, labels = c("[0,10)", "[10,20)", "[20,30)", "[30,40)", "[40,Inf)"))
custom_colors <- heat_hcl(length(unique(data$tip_blocks)))

plot6 <- ggplot(data, aes(x = tip_blocks, fill = tip_blocks)) +
  geom_bar() +
  scale_fill_manual(values = custom_colors) +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "lightgray"),
    panel.grid.major = element_line(color = "white"),
    axis.text = element_text(color = "darkblue"),
    axis.title = element_text(color = "darkblue"),
    plot.title = element_text(face = "bold", size = 14)
  ) +
  xlab("Kwota napiwku") +
  ylab("Liczba") +
  ggtitle("Rozkład kwot napiwków w grupach 10 $")

# total_amount
plot3 <- ggplot(data, aes(x = calculated_total_amount)) +
  geom_histogram(binwidth = 5, fill = "seagreen", color = "black", alpha = 0.7) +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "lightgray"),
    panel.grid.major = element_line(color = "white"),
    axis.text = element_text(color = "darkblue"),
    axis.title = element_text(color = "darkblue"),
    plot.title = element_text(face = "bold", size = 14)
  ) +
  xlab("Całkowita kwota podróży") +
  ylab("Frekwencja") +
  ggtitle("Rozkład całkowitych kwot podróży")

print(max(data$calculated_total_amount))

# Let's also create a histogram which we represent data by groups of 20 dollars
data$total_blocks <- cut(data$calculated_total_amount, breaks = c(0, 20, 40, 60, 80, 100, Inf), include.lowest = TRUE, labels = c("[0,20)", "[20,40)", "[40,60)", "[60,80)", "[80,100)", "[100, Inf)"))
custom_colors <- heat_hcl(length(unique(data$total_blocks)))

plot7 <- ggplot(data, aes(x = total_blocks, fill = total_blocks)) +
  geom_bar() +
  scale_fill_manual(values = custom_colors) +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "lightgray"),
    panel.grid.major = element_line(color = "white"),
    axis.text = element_text(color = "darkblue"),
    axis.title = element_text(color = "darkblue"),
    plot.title = element_text(face = "bold", size = 14)
  ) +
  xlab("Całkowita kwota podróży") +
  ylab("Liczba") +
  ggtitle("Rozkład całkowitych kwot podróży w grupach 20 $")

# trip_duration_min
plot4 <- ggplot(data, aes(x = trip_duration_min)) +
  geom_histogram(binwidth = 5, fill = "seagreen", color = "black", alpha = 0.7) +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "lightgray"),
    panel.grid.major = element_line(color = "white"),
    axis.text = element_text(color = "darkblue"),
    axis.title = element_text(color = "darkblue"),
    plot.title = element_text(face = "bold", size = 14)
  ) +
  xlab("Czas podróży w minutach") +
  ylab("Frekwencja") +
  ggtitle("Rozkład czasu podróży w minutach")

print(max(data$trip_duration_min))

# Let's also create a histogram which we represent data by groups of 20 dollars
data$duration_blocks <- cut(data$trip_duration_min, breaks = c(0, 20, 40, 60, 80, 100, Inf), include.lowest = TRUE, labels = c("[0,20)", "[20,40)", "[40,60)", "[60,80)", "[80,100)", "[100, Inf)"))
custom_colors <- heat_hcl(length(unique(data$duration_blocks)))

plot8 <- ggplot(data, aes(x = duration_blocks, fill = duration_blocks)) +
  geom_bar() +
  scale_fill_manual(values = custom_colors) +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "lightgray"),
    panel.grid.major = element_line(color = "white"),
    axis.text = element_text(color = "darkblue"),
    axis.title = element_text(color = "darkblue"),
    plot.title = element_text(face = "bold", size = 14)
  ) +
  xlab("Czas podróży w minutach") +
  ylab("Liczba") +
  ggtitle("Rozkład czasu podróży w minutach w grupach 20 min.")

grid.arrange(arrangeGrob(plot1, plot2, plot3, plot4, ncol = 2),
             top = textGrob("Rozkłady zmiennych numerycznych ", gp = gpar(fontsize = 12)))

grid.arrange(arrangeGrob(plot5, plot6, plot7, plot8, ncol = 2),
             top = textGrob("Rozkłady zmiennych numerycznych ", gp = gpar(fontsize = 12)))

# Since it takes around 45 km to drive all NYC, it's was decided to see amount of long and short trips
data$trip_type <- ifelse(data$trip_distance_km <= 45, "short", "long")
trip_type_counts <- table(data$trip_type)
print(trip_type_counts)

# pickup_location_id
top5_pickups <- head(names(sort(table(data$pickup_location_id), decreasing = TRUE)), 5)
top5_pickups_table <- table(data$pickup_location_id)[top5_pickups]

pickups <- barplot(top5_pickups_table, col = "seagreen", main = "Top 5 lokalizacji odbioru",
        ylab = "Liczba", xlab = "ID lokalizacji odbioru",
        border = "black", col.axis = "darkblue", col.lab = "darkblue",
        names.arg = top5_pickups, 
        cex.names = 1, font.axis = 1, font.lab = 1, font.main = 1)

table(data$pickup_location_id)[top5_pickups]

# dropoff_location_id
top5_dropoffs <- head(names(sort(table(data$dropoff_location_id), decreasing = TRUE)), 5)
top5_dropoffs_table <- table(data$dropoff_location_id)[top5_dropoffs]

dropoffs <- barplot(top5_dropoffs_table, col = "seagreen", main = "Top 5 lokalizacji docelowej",
        ylab = "Liczba", xlab = "ID lokalizacji docelowej",
        border = "black", col.axis = "darkblue", col.lab = "darkblue",
        names.arg = top5_dropoffs, 
        cex.names = 1, font.axis = 1, font.lab = 1, font.main = 1)

table(data$dropoff_location_id)[top5_dropoffs]

# hour_of_day
hourly_counts <- data %>%
  group_by(hour_of_day) %>%
  summarise(number_of_trips = n())

print(sum(hourly_counts$number_of_trips))

ggplot(hourly_counts, aes(x = hour_of_day, y = number_of_trips)) + 
  geom_polygon(fill = NA, col = "seagreen") +
  geom_point(size = 5) +
  theme_minimal() + 
  coord_polar() +
  scale_x_continuous("", breaks = 0:24, limits = c(0, 24)) +
  ylim(800, 6200) +
  theme(
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14),
    plot.title = element_text(hjust = 0.5, size = 16)
  ) +
  ggtitle("Rozkład przejazdów każdą godziną")

# day_of_week
daily_counts <- data %>%
  group_by(day_of_week) %>%
  summarise(number_of_trips = n())

print(sum(daily_counts$number_of_trips))
custom_colors <- heat_hcl(length(unique(data$day_of_week)))

daily_counts$day_of_week <- factor(daily_counts$day_of_week,
                                   levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))

ggplot(daily_counts, aes(x = day_of_week, y = number_of_trips, fill = as.factor(day_of_week))) +
  geom_bar(color = "black", stat = "identity") +
  scale_fill_manual(values = custom_colors) + 
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "lightgray"),
    panel.grid.major = element_line(color = "white"),
    axis.text = element_text(color = "darkblue"),
    axis.title = element_text(color = "darkblue"),
    plot.title = element_text(face = "bold", size = 14)
  ) +
  xlab("Dzień tygodnia") +
  ylab("Liczba przejadów") +
  ggtitle("Rozkład przejazdów każdego dnia tygodnia") +
  labs(fill = "Dzień tygodnia") +
  scale_x_discrete(labels = NULL)

# Advanced analysis
# Correlation matrix 
matrix <- cor(select_if(subset(data, select=c(trip_distance_km, pickup_location_id, dropoff_location_id, hour_of_day, trip_duration_min, tip_amount)), is.numeric))
options(repr.plot.width = 14, repr.plot.height = 8)
corrplot(matrix, na.label=" ", tl.cex=0.75, tl.col="#007481", method="color", type = 'lower')

# Plots for trip_distance, trip_duration, tip_amount
ggplot(data, aes(x = distance_blocks, y = tip_amount)) +
  geom_boxplot(fill = "seagreen") +
  theme(
    panel.background = element_rect(fill = "lightgray"),
    panel.grid.major = element_line(color = "white"),
    axis.text = element_text(color = "darkblue"),
    axis.title = element_text(color = "darkblue"),
    plot.title = element_text(face = "bold", size = 11)
  ) +
  xlab("Przejazd w km") +
  ylab("Kwota Napiwku") +
  ggtitle("Rozkład kwot napiwków w zależności od grupy dystansu")

ggplot(data, aes(x = duration_blocks, y = tip_amount)) +
  geom_boxplot(fill = "seagreen") +
  theme(
    panel.background = element_rect(fill = "lightgray"),
    panel.grid.major = element_line(color = "white"),
    axis.text = element_text(color = "darkblue"),
    axis.title = element_text(color = "darkblue"),
    plot.title = element_text(face = "bold", size = 11)
  ) +
  xlab("Czas przejazdu w min") +
  ylab("Kwota Napiwku") +
  ggtitle("Rozkład kwot napiwków w zależności od grupy czasu trwania")

ggplot(data, aes(x = total_blocks, y = tip_amount)) +
  geom_boxplot(fill = "seagreen") +
  theme(
    panel.background = element_rect(fill = "lightgray"),
    panel.grid.major = element_line(color = "white"),
    axis.text = element_text(color = "darkblue"),
    axis.title = element_text(color = "darkblue"),
    plot.title = element_text(face = "bold", size = 11)
  ) +
  xlab("Całkowita kwota") +
  ylab("Kwota Napiwku") +
  ggtitle("Rozkład kwot napiwków w zależności od grupy kwoty całkowitej")

# Please select variables for loop 
variables_to_plot <- c('distance_blocks', 'duration_blocks', 'total_blocks')

for (variable in variables_to_plot) {
  data_percent <- data %>%
    group_by(tip_blocks, !!sym(variable)) %>% 
    summarise(number_cases = n()) %>%
    group_by(tip_blocks) %>% 
    mutate(
      total_cases = sum(number_cases),
      proportion = number_cases / total_cases
    ) 
  
  p <- ggplot(data_percent, aes(x = tip_blocks, y = !!sym(variable), fill = proportion)) +
    geom_tile() +
    geom_text(
      aes(label = paste(round(proportion * 100, 2), "%", sep = "")),
      size = 4, colour = "white"
    ) +
    coord_fixed() +
    theme_minimal() +
    theme(
      panel.background = element_rect(fill = "lightgray"),
      panel.grid.major = element_line(color = "white"),
      axis.text = element_text(color = "darkblue"),
      axis.title = element_text(color = "darkblue"),
      plot.title = element_text(face = "bold", size = 10)
    )
    
  if (variable == "distance_blocks") {
    p <- p + labs(
      title = "Udział obserwacji według kwoty napiwków i dystansu",
      x = "Kwota napiwku",
      y = "Dystans",
      fill = "Udział obserwacji"
    )
  } else if (variable == "duration_blocks") {
    p <- p + labs(
      title = "Udział obserwacji według kwoty napiwku i czasu przejazdu w min",
      x = "Kwota napiwku",
      y = "Duration Blocks",
      fill = "Udział obserwacji"
    )
  } else if (variable == "total_blocks") {
    p <- p + labs(
      title = "Udział obserwcji według kwoty napiwku i całkowitej kwoty",
      x = "Kwota napiwku",
      y = "Kwota całkowita",
      fill = "Udział obserwacji"
    )
  }
  print(p)
}

# day_of_week_2
ggplot(data, aes(x = day_of_week_2, y = tip_amount, fill = day_of_week_2)) +
  stat_summary(fun = mean, geom = "bar", position = "dodge") +
  stat_summary(fun = mean, geom = "text", aes(label = round(..y.., 2)),
               position = position_dodge(width = 0.9), vjust = -0.5, color = "black") +
  scale_fill_manual(values = c("red4", "seagreen")) +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "lightgray"),
    panel.grid.major = element_line(color = "white"),
    axis.text = element_text(color = "darkblue"),
    axis.title = element_text(color = "darkblue"),
    plot.title = element_text(face = "bold", size = 14)
  ) +
  xlab("Dzień tygodnia") +
  ylab("Średnia kwota napiwku") +
  ggtitle("Średnie kwoty napiwków w załeżności od dnia tygodnia") + 
  labs(fill = "Dzień tygodnia")

# hour_of_day
ggplot(data, aes(x = factor(hour_of_day), y = tip_amount, fill = factor(hour_of_day))) +
  stat_summary(fun = mean, geom = "bar", position = "dodge") +
  stat_summary(fun = mean, geom = "text", aes(label = round(..y.., 2)),
               position = position_dodge(width = 0.9), vjust = -0.5, color = "black") +
  scale_fill_manual(values = heat_hcl(24)) +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "lightgray"),
    panel.grid.major = element_line(color = "white"),
    axis.text = element_text(color = "darkblue"),
    axis.title = element_text(color = "darkblue"),
    plot.title = element_text(face = "bold", size = 14)
  ) +
  xlab("Godzina dnia") +
  ylab("Średnia kwota napiwku") +
  ggtitle("Średnie kwoty napiwku w zależności od godziny dnia") + 
  labs(fill = "Godzina dnia")

# Additional information to the shiny application
# Let's check the percentage of observations for variable tip_amount which is in range from 0 to 30\
percentage_in_range <- mean(data$tip_amount >= 0 & data$tip_amount <= 20) * 100
print(paste("Percentage of observations in range 0-20 for tip_amount:", round(percentage_in_range, 2), "%"))

# Advanced models
data$tip_blocks <- cut(data$tip_amount, breaks = c(0, 10, 20, 30, 40, Inf), include.lowest = TRUE, labels = c("1", "2", "3", "4", "5"))
data$tip_blocks <- as.factor(data$tip_blocks)

# Factorise some variables
data$dropoff_location_id <- as.factor(data$dropoff_location_id)
data$pickup_location_id <- as.factor(data$pickup_location_id)
data$month <- as.factor(data$month)
data$day_of_week <- as.factor(data$day_of_week)
data$hour_of_day <- as.factor(data$hour_of_day)

# Select variables for analysis 
predictor_variables <- c("trip_distance_km", "dropoff_location_id", "pickup_location_id", "trip_duration_min", "month", "day_of_week", "hour_of_day", "calculated_total_amount")

# Divide the data set on test and train data
# As set seed we will use index number 
set.seed(109081)
test_prop <- 0.2
data_models <- subset(data, select = c("tip_blocks", predictor_variables))
test.set.index <- (runif(nrow(data_models)) < test_prop)
test <- data_models[test.set.index, ]
train <- data_models[!test.set.index, ]

# Multinomial logistic regression
reg_multinom <- multinom(tip_blocks ~ ., data = train)

# Summary of the regression
summary(reg_multinom)

# Trees
tree <- rpart(tip_blocks ~ .,
              data = train,
              method = "anova",
              cp = 0.01)

tree
plot(tree)
text(tree, pretty = TRUE)
rpart.plot(tree, under = FALSE, tweak = 1.3, fallen.leaves = TRUE)

varImp(tree)

CM <- list()

CM[["tree"]] <- table(predict(tree, new = test, type = "vector"), test$tip_blocks)
CM[["reg_log_full"]] <- table(apply(predict(reg_multinom, newdata = test, type = "probs"), 1, which.max), test$tip_blocks)

# Statistics
statystyki <- function(classif_mx){
  true_positive <- classif_mx[2, 2]
  true_negative <- classif_mx[1, 1]
  condition_positive <- sum(classif_mx[ , 2])
  condition_negative <- sum(classif_mx[ , 1])
  predicted_positive <- sum(classif_mx[2, ])
  predicted_negative <- sum(classif_mx[1, ])
  
  accuracy <- (true_positive + true_negative) / sum(classif_mx)
  MER <- 1 - accuracy
  precision <- true_positive / predicted_positive
  sensitivity <- true_positive / condition_positive
  specificity <- true_negative / condition_negative
  F1 <- (2 * precision * sensitivity) / (precision + sensitivity)
  return(list(accuracy = accuracy, 
              MER = MER,
              precision = precision,
              sensitivity = sensitivity,
              specificity = specificity,
              F1 = F1))
}

lapply(CM, statystyki)
sapply(CM, statystyki)

# ROC curve for regression
probs_reg_multinom <- predict(reg_multinom, newdata = test, type = "probs")
test$tip_blocks <- factor(test$tip_blocks, levels = colnames(probs_reg_multinom))

# Plot ROC curves for each class
par(pty = "s")
for (i in colnames(probs_reg_multinom)) {
  roc_class <- roc(as.numeric(test$tip_blocks == i), as.vector(probs_reg_multinom[, i]))
  plot(roc_class, col = rainbow(length(colnames(probs_reg_multinom)))[as.numeric(i)], main = "Multinomial Logistic Regression ROC Curve", lwd = 2, add = ifelse(i == colnames(probs_reg_multinom)[1], FALSE, TRUE), legacy.axes = TRUE)
}

legend("bottomright", legend = colnames(probs_reg_multinom), col = rainbow(length(colnames(probs_reg_multinom))), lwd = 2)

# Print AUC values for each class
auc_values <- sapply(colnames(probs_reg_multinom), function(class) {
  roc_class <- roc(as.numeric(test$tip_blocks == class), as.vector(probs_reg_multinom[, class]))
  auc_value <- auc(roc_class)
  cat("AUC for class", class, ":", auc_value, "\n")
  return(auc_value)
})

# Overall AUC (macro-average)
cat("Overall AUC:", mean(auc_values), "\n")

# ROC curve for trees
preds_tree <- predict(tree, newdata = test, type = "vector")
responses_tree <- lapply(levels(test$tip_blocks), function(class) {
  as.numeric(test$tip_blocks == class)
})

# Create ROC curves and calculate AUC for each class
rocs_tree <- lapply(seq_along(responses_tree), function(i) {
  roc(response = responses_tree[[i]], predictor = preds_tree)
})

# Plot ROC curves
par(mfrow = c(1, 1))
plot(rocs_tree[[1]], col = rainbow(length(levels(test$tip_blocks)))[1], main = "ROC Curve for Decision Tree - Multiclass", lwd = 2)

for (i in 2:length(rocs_tree)) {
  lines(rocs_tree[[i]], col = rainbow(length(levels(test$tip_blocks)))[i], lwd = 2)
}

legend("bottomright", 
       legend = levels(test$tip_blocks),  
       col = rainbow(length(levels(test$tip_blocks))), 
       lwd = 2)

# Calculate and print AUC values
auc_values_tree <- sapply(rocs_tree, function(roc_curve) {
  auc(roc_curve)
})

cat("AUC values for each class (Trees):\n")
cat(paste(names(auc_values_tree), auc_values_tree, sep = ": "), "\n")

# Overall AUC (macro-average) for Trees
cat("Overall AUC for Trees:", mean(auc_values_tree), "\n")