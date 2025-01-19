# Load necessary libraries
library(dplyr)
library(ggplot2)
library(quanteda)
library(quanteda.textplots)
library(tidytext)
library(sentimentr)
library(textstem)

# Load dataset
data <- read.csv("sentiment_data.csv")

# Print the dimensions
dataset_size <- dim(data)
print(dataset_size)

# Function to extract the first few words
extract_words <- function(text, num_words = 10) {
  if (is.na(text) || !is.character(text)) {
    return(NA)
  }
  words <- unlist(strsplit(text, "\\s+")) 
  return(paste(words[1:min(num_words, length(words))], collapse = " "))
}

# Randomly sample 5 rows
set.seed(123) 
random_feedback <- data[sample(nrow(data), size = 5, replace = FALSE), ]

# Extract the first 5 words
random_feedback$text <- sapply(random_feedback$text, function(text) {
  truncated <- extract_words(text)
  paste0(truncated, "...")
})

# Print the data frame
print(random_feedback)

# Cleaning the dataset
corpus <- quanteda::corpus(data$text)
tokens_clean <- quanteda::tokens(corpus, remove_punct = TRUE, remove_numbers = TRUE, remove_symbols = TRUE) %>%
  tokens_tolower() %>%
  tokens_remove(stopwords("en"))

# Convert tokens to character vectors for lemmatization
tokens_char <- sapply(tokens_clean, function(tokens) paste(tokens, collapse = " "))

# Lemmatize tokens using WordNet
lemmatized_texts <- lemmatize_strings(tokens_char)

# Perform sentiment analysis using sentimentr
sentiment_scores <- sentiment(lemmatized_texts)

# Summarize sentiment scores for each document
sentiment_summary <- sentiment_scores %>%
  group_by(element_id) %>%
  summarize(
    positive_words = sum(sentiment > 0),
    negative_words = sum(sentiment < 0),
    compound = sum(sentiment)
  ) %>%
  mutate(
    sentiment = ifelse(compound > 0, "Positive", "Negative")
  )

# Merge with original text for context using row number as a common column
sentiment_summary <- sentiment_summary %>%
  mutate(doc_id = as.character(element_id)) %>%
  left_join(data %>% mutate(doc_id = as.character(1:nrow(data))), by = "doc_id") %>%
  select(text, positive_words, negative_words, compound, sentiment)

# Print the sentiment evaluation table
print(sentiment_summary)

# Create a document-feature matrix (DFM)
dfm <- dfm(tokens_clean)

# Evaluate sentiment proportions as percentages
sentiment_proportion <- sentiment_summary %>%
  group_by(sentiment) %>%
  summarise(count = n()) %>%
  mutate(proportion = count / sum(count) * 100)
print(sentiment_proportion)

# Plot sentiment distribution as percentages
ggplot(sentiment_proportion, aes(x = sentiment, y = proportion, fill = sentiment)) +
  geom_bar(stat = "identity", width = 0.7) +
  scale_fill_manual(values = c("Positive" = "blue", "Negative" = "red")) +
  labs(title = "Distribution of Sentiments",
       x = "Sentiment Type",
       y = "Percentage",
       fill = "Sentiment") +
  theme_minimal() +
  theme(panel.grid = element_blank())

# Plot top 10 terms with a gradient
top_terms <- topfeatures(dfm, 10)
bar_colors <- colorRampPalette(c("lightblue", "blue"))(length(top_terms))

# Plot with gradient
barplot(top_terms, main = "Top 10 Terms", las = 2, col = bar_colors, horiz = TRUE, cex.names = 0.7)

# Word cloud
textplot_wordcloud(dfm, max_words = 200, color = RColorBrewer::brewer.pal(8, "Reds"), min_size = 0.5, max_size = 6)

# Sample 5 sentences from the dataset 
sample_indices <- sample(1:nrow(sentiment_summary), 5)
sample_sentiment_summary <- sentiment_summary[sample_indices, ]

# Print the sample sentences 
print(sample_sentiment_summary)

# Filter positive feedback 
positive_feedback <- sentiment_summary %>% 
  filter(sentiment == "Positive") 

# Create a DFM for positive feedback 
positive_tokens <- quanteda::tokens(positive_feedback$text, remove_punct = TRUE, remove_numbers = TRUE, remove_symbols = TRUE) %>% 
  tokens_tolower() %>% 
  tokens_remove(stopwords("en")) 
positive_dfm <- dfm(positive_tokens)

# Plot top 5 terms with a gradient
top_positive <- topfeatures(positive_dfm, 5)
bar_colors <- colorRampPalette(c("lightblue", "blue"))(length(top_positive))

# Plot with gradient
barplot(top_positive, main = "Top 5 Positive Terms", las = 2, col = bar_colors, horiz = TRUE, cex.names = 0.7)
