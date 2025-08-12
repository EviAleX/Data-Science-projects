# 🚖 NYC Taxi Tip Determinants: An Exploratory & Predictive Analysis in R

This repository contains an in-depth exploration of **factors influencing tip amounts** in New York City taxi rides, using the **NYC Yellow Taxi Trip Data (2018)**. The analysis covers **data exploration, statistical relationships, and predictive modeling** to better understand how trip characteristics and timing impact gratuities.

---
## 📅 Dataset

- **Data Source**: NYC Yellow Taxi Trip Data (processed subset)  
- **Year**: 2018  
- **Observations**: ~Large subset of daily trips

## 🔍 Data Exploration
The analysis begins with **data cleaning and transformation**, including:
- Conversion of trip distances from miles to kilometers.
- Standardization of time variables (seconds → minutes).
- Categorization of trips into **weekday/weekend**.
- Removal of unused columns for cleaner analysis.

**Visualizations** include:
- **Histograms** of trip distances, durations, total fares, and tip amounts.
- **Binned distributions** (e.g., tip ranges of $10, distance blocks of 15 km).
- **Top pickup and dropoff zones**.
- **Daily and hourly ride patterns**.
- **Correlation matrix** for numerical features.

---

## 🤖 Predictive Modeling

Two predictive modeling approaches were implemented to classify **tip amount categories**:
1. **Multinomial Logistic Regression** – predicts tip category based on trip and temporal features.
2. **Decision Tree (rpart)** – rule-based classification for interpretability.

**Model evaluation** included:
- **Confusion matrices**
- **Accuracy, precision, recall, specificity, and F1 score**
- **ROC curves & AUC values** for each class

---

## 📌 Key Findings

- **Trip duration, total fare, and distance** show strong influence on tip amount.
- **Time of day** and **day type (weekday/weekend)** also affect tipping patterns.
- Decision trees offer interpretability, while multinomial regression provides robust statistical insight.
- Majority of trips have tips between **$0–20**, with high variability in longer trips.

---

## 💻 Interactive Shiny App

An accompanying **Shiny dashboard** was developed, allowing:
- Dynamic exploration of trip and tip distributions.
- Filtering by trip distance, duration, day type, and time of day.
- Visualization of model predictions and performance metrics.
