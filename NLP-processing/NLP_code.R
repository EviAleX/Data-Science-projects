rm(list = ls())
# Aliaksandr Mazur 109081
# Maciej Kowalczyk 108910
#
# To reload the saved data later:
# doc <- readRDS("D:\\AI_project\\processed_doc.rds")

# Step 1: Install and Load Required Packages
# List of required libraries
libraries <- c("dplyr", "tidytext", "RColorBrewer", "ggplot2", "wordcloud", "tm", "stringr", "udpipe", "topicmodels")

# Function to check if each library is installed, and install it if missing
install_if_missing <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg)
  }
  library(pkg, character.only = TRUE) # Load the library
}

# Apply the function to each library in the list
lapply(libraries, install_if_missing)

# Suppress warnings for cleaner output
options(warn = -1)

# Set working directory and load dataset
setwd("D:\\Studying\\SGH\\Magisterka\\III semestr\\Artificial Intelligence")
dataset <- read.csv('Tweets.csv')

# Step 2: Data Preparation and Initial Exploration
# Examine the structure of the dataset
str(dataset)

# Select relevant columns for analysis
tidy_dataset <- dataset %>%
  select(airline_sentiment, negativereason, airline, text) 

# Count number of tweets for each sentiment category
sentiment_counts <- tidy_dataset %>%
  count(airline_sentiment)
print(sentiment_counts)

# Visualize sentiment distribution for each airline
ggplot(tidy_dataset, aes(x = airline_sentiment, fill = airline_sentiment)) +
  geom_bar() +
  facet_grid(. ~ airline) +
  theme(axis.text.x = element_text(angle = 65, vjust = 0.6),
        plot.margin = unit(c(3, 0, 3, 0), "cm")) +
  ggtitle("Sentiment Distribution for Each Airline") +
  labs(x = "Sentiment", y = "Number of Tweets")

# Step 3: Separate Positive and Negative Sentiments
# Filter dataset for positive and negative reviews separately
positive_reviews <- tidy_dataset %>%
  filter(airline_sentiment == "positive") %>%
  select(text) %>%
  pull() %>%
  paste(collapse = " ") # Combine into a single string for word cloud generation

negative_reviews <- tidy_dataset %>%
  filter(airline_sentiment == "negative") %>%
  select(text) %>%
  pull() %>%
  paste(collapse = " ")

# Step 4: Text Preprocessing and Word Tokenization
# List of common words to exclude from analysis (manual stopwords)
stopwords_manual <- c("@","to", "the","i", "a", "you", "for", "on", "and", "is", "are", "am", 
                      "my", "in", "it", "me", "of", "was", "your", "so","with", "at", "just", 
                      "this", "http", "t.co", "have", "that", "be", "from", "will", "we", "an", "can")

# Convert text to character and tokenize into words
tidy_dataset$text <- as.character(tidy_dataset$text)
tidy_dataset <- tidy_dataset %>%
  unnest_tokens(word, text) %>%
  filter(!(word %in% stopwords_manual)) # Remove custom stopwords

# Separate positive and negative words for word cloud generation
positive_words <- tidy_dataset %>%
  filter(airline_sentiment == "positive") %>%
  count(word, sort = TRUE)

negative_words <- tidy_dataset %>%
  filter(airline_sentiment == "negative") %>%
  count(word, sort = TRUE)

# Step 5: Generate Word Clouds for Positive and Negative Sentiments
# Word cloud for positive reviews
wordcloud(positive_words$word, positive_words$n, max.words = 100, random.order = FALSE, 
          rot.per = 0.30, colors = brewer.pal(10, "Blues"))

# Word cloud for negative reviews
wordcloud(negative_words$word, negative_words$n, max.words = 100, random.order = FALSE, 
          rot.per = 0.3, colors = brewer.pal(10, "Reds"))

# Step 6: Advanced Text Analysis with NLP Pre-trained Model
# Load pre-trained English model for NLP using UDPipe
library(udpipe)
# ud_model <- udpipe_download_model(language = "english")
ud_model <- udpipe_load_model("D:\\Studying\\SGH\\Magisterka\\III semestr\\Artificial Intelligence\\english-ewt-ud-2.5-191206.udpipe")

# Annotate negative reviews text for linguistic processing
doc <- udpipe_annotate(ud_model, x = negative_reviews)
doc <- as.data.frame(doc)

# Display the first 150 lemmas in the annotated text (lemmas represent base forms of words)
head(doc$lemma, 150)

# Step 7: Term Frequency Analysis with Annotated Data
# Compute frequency of each lemma
word_freq <- table(doc$lemma)

# Get the top 40 most frequent lemmas
top_40_words <- head(sort(word_freq, decreasing = TRUE), 40)

# Create a word cloud of the top 40 most frequent words
wordcloud(names(top_40_words), freq = top_40_words, scale = c(4, 0.4), min.freq = 4, 
          max.words = 300, random.order = FALSE, rot.per = 0.35, colors = brewer.pal(8, "Paired"))

# Step 8: TF-IDF Analysis to Identify Important Terms
# Compute TF-IDF scores to highlight important words in each sentiment
tf_idf_data <- tidy_dataset %>%
  count(airline_sentiment, word) %>%
  bind_tf_idf(word, airline_sentiment, n) %>%
  arrange(desc(tf_idf))

# Display top keywords based on high TF-IDF values
head(tf_idf_data, 20)

# Step 9: Keyword Extraction Using RAKE (Rapid Automatic Keyword Extraction)
# Extract keywords with RAKE using nouns and adjectives only
rake_keywords <- keywords_rake(x = doc, term = "lemma", group = "doc_id", relevant = doc$upos %in% c("NOUN", "ADJ"))
head(rake_keywords, 20)

# Step 10: Topic Modeling with LDA (Latent Dirichlet Allocation)
# Create a Document-Term Matrix
tidy_dtm <- tidy_dataset %>%
  count(airline_sentiment, word, sort = TRUE)
dtm <- tidy_dtm %>%
  cast_dtm(document = airline_sentiment, term = word, value = n)

# Apply LDA to extract topics
lda_model <- LDA(dtm, k = 6, control = list(seed = 1234)) # k = number of topics
lda_topics <- tidy(lda_model, matrix = "beta")

# Display top terms for each topic
top_terms <- lda_topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)
print(top_terms)

# Step 11: Named Entity Recognition (NER)
# Extract named entities (e.g., proper nouns) to identify key entities in the text
entities <- doc %>%
  filter(upos %in% c("PROPN")) %>%
  count(lemma, sort = TRUE)

# Display top named entities
head(entities, 20)

# Step 12: Save Processed Data for Future Use
# Save annotated data to avoid reprocessing
saveRDS(doc, "D:\\Studying\\SGH\\Magisterka\\III semestr\\Artificial Intelligence\\processed_doc.rds")

# To reload the saved data later:
doc <- readRDS("D:\\Studying\\SGH\\Magisterka\\III semestr\\Artificial Intelligence\\processed_doc.rds")
