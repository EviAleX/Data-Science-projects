# 🧠 NLP for Airline Sentiment Analysis: Keyword Extraction & Text Summarization

This project explores the use of **Natural Language Processing (NLP)** techniques to analyze customer sentiment in airline-related tweets. Using a dataset of traveler opinions about major U.S. airlines, we applied **keyword extraction** and **topic modeling** to identify key themes, customer pain points, and drivers of satisfaction. 
The project was completed in collaboration with my colleague Maciej Kowalczyk.

---

## 📑 Project Overview

- **Goal**: Analyze sentiment-labeled tweets to extract meaningful keywords, summarize content, and uncover topics that influence customer perception of airlines.
- **Techniques Used**:  
  - **TF-IDF** (Term Frequency–Inverse Document Frequency)
  - **RAKE** (Rapid Automatic Keyword Extraction)
  - **LDA** (Latent Dirichlet Allocation Topic Modeling)
  - **NER** (Named Entity Recognition – discussed for applicability)
- **Dataset**: Kaggle’s reformatted version of Crowdflower's **Twitter Airline Sentiment** data (February 2015).

---

## 📅 Dataset Description

- **Total Tweets**: 14,640  
  - Negative: 9,178  
  - Neutral: 3,099  
  - Positive: 2,363  
- **Content**: Passenger opinions about 6 major U.S. airlines.
- **Sentiment Labels**: Positive, Neutral, Negative (with reasons for negative feedback).

## 🛠 Methodology

### 1. **Text Preprocessing**
- Tokenization
- Stopword removal (custom + standard lists)
- Lowercasing
- Removal of URLs, numbers, and special characters

### 2. **TF-IDF Keyword Analysis**
- Identified **most representative words** for positive and negative tweets.
- Found that:
  - **Negative**: "worst", "ridiculous", "unacceptable"
  - **Positive**: "excellent", "communication", "fantastic"

### 3. **RAKE Phrase Extraction**
- Extracted **multi-word key phrases**.
- High-scoring examples:
  - Negative: “late flight” (3.29), “worst service” (2.56), “overhead bin” (2.59)
  - Neutral: “social media”, “customer service”

### 4. **LDA Topic Modeling**
- Generated **6 topics**:
  1. **Airline Brands** — brand-specific mentions
  2. **Flight Disruptions** — delays, cancellations
  3. **Customer Service** — support quality
  4. **Airport Logistics** — gates, boarding
  5. **In-Flight Comfort** — seating, crew, entertainment
  6. **Customer Complaints** — wait times, response delays

### 5. **NER (Named Entity Recognition)**
- Reviewed applicability — not heavily used due to dataset’s low density of named entities.

---

## 📊 Key Findings

- **Negative sentiment dominates** the dataset (63% of tweets).
- **Flight delays** and **service quality issues** are the most common pain points.
- **Comfort and crew interactions** are main satisfaction drivers.
- **Topic modeling** helps pinpoint specific areas for airline improvement.
