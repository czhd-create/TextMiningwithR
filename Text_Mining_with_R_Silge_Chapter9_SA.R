# Text Mining with R
# Julia Silge and David Robinson
# Sentiment Analysis part of Chapter 9 - Analyzing Usenet Text
# 11 December 2024
# Lines 6 to 50 are commented on in a separate file: Text_Mining_with_R_Silge_Chapter9.R
library(dplyr) 
library(tidyr)
library(readr)
library(purrr)
library(ggplot2)
library(stringr)
library(tidytext) # contains the sentiment lexicons for sentiment analysis.

training_folder <- "/Users/dan/Downloads/20news-bydate/20news-bydate-train"

read_folder <- function(infolder) {
  tibble(file=dir(infolder, full.names = TRUE)) %>%
    mutate(text = map(file, read_lines)) %>%
    transmute(id = basename(file), text) %>%
    unnest(text)
}

raw_text <- tibble(folder = dir(training_folder, full.names = TRUE)) %>%
  mutate(folder_out = map(folder, read_folder)) %>%
  unnest(cols = c(folder_out)) %>%
  transmute(newsgroup = basename(folder), id, text)

# Preprocessing the text
cleaned_text <- raw_text %>%
  group_by(newsgroup, id) %>%
  filter(cumsum(text == "") > 0,
         cumsum(str_detect(text, "^--")) == 0) %>%
  ungroup() 

cleaned_text <- cleaned_text %>%
  filter(str_detect(text, "^[^>]+[A-Za-z\\d]") | text == "" )

cleaned_text <- cleaned_text %>%
  filter(!str_detect(text, "writes(:|\\.\\.\\.)$"),
         !str_detect(text, "^In article <"),
         !id %in% c(9704, 9985))

usenet_words <- cleaned_text %>%
  unnest_tokens(word, text) %>%
  filter(str_detect(word, "[a-z']$"),
         !word %in% stop_words$word) 

# When grouped by newsgroup, what are the most frequent words in each of them?
words_by_newsgroup <- usenet_words %>%
  count(newsgroup, word, sort = TRUE)

# Now we can start the sentiment analysis
# Fun fact: The word, afinn comes after the researcher, Finn Arup Nielsen who authored 
# a paper (2011) about a word list for sentiment analysis: https://arxiv.org/abs/1103.2903
newsgroup_sentiments <- words_by_newsgroup %>%
  inner_join(get_sentiments("afinn"), by = "word") %>% # adds a column: value
  group_by(newsgroup) %>%
  # create a dataframe with average scores for each news group
  summarise(avg_score = sum(value * n)/ sum(n)) # summarise(), summarize() are synonyms.

# summarise() creates a new data frame. It returns one row for each combination 
# of grouping variables; If there are no grouping variables, the output will have 
# a single row summarising all observations in the input.

newsgroup_sentiments

# Visualize the average sentiment score of the newsgroups
newsgroup_sentiments %>%
  mutate(newsgroup = reorder(newsgroup, avg_score)) %>%
  ggplot(aes(x=newsgroup, y = avg_score, fill = avg_score > 0)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  ylab("Average sentiment score")
# The most positive newgroup is the misc.forsale group.
# Lets examine why by looking at the words in the newsgroups.

word_contributions <- usenet_words %>%
  inner_join(get_sentiments("afinn"), by = "word") %>%
  group_by(word) %>%
  summarise(occurances = n(), contribution = sum(value))
word_contributions

# Which words were the biggest contributors to sentiment scores overall?
word_contributions %>%
  slice_max(abs(contribution), n=25) %>%
  mutate(word = reorder(word, contribution)) %>%
  ggplot(aes(x= word, y = contribution, fill = contribution > 0)) +
  geom_col(show.legend = FALSE) +
  coord_flip()

# Which words contribute the most within each newsgroup?
sentiment_words <- words_by_newsgroup %>%
  inner_join(get_sentiments("afinn"), by = "word") %>%
  mutate(contribution = value * n/sum(n))

sentiment_words

# Create code for the plot to show words that contribute most within each newsgroup.
# Lets only look at certain newsgroups
selected_newsgroups <-c("talk.politics.guns","talk.politics.mideast","talk.politics.misc",
                        "alt.atheism", "talk.religion.misc", "misc.forsale")
sentiment_words %>%
  filter(newsgroup %in% selected_newsgroups) %>%
  group_by(newsgroup) %>%
  slice_max(abs(contribution), n=12) %>%
  ungroup() %>%
  mutate(word = reorder_within(word, contribution, newsgroup)) %>%
  ggplot(aes(x=word, y=contribution, fill=contribution > 0))+
  geom_col(show.legend = FALSE) +
  facet_wrap(~ newsgroup, scales = "free") +
  # next line from another file, Text_Mining_with_R_Silge_Chapter8_TM.R, line 204
  scale_x_discrete(labels = function(x) gsub("__.+$", "", x))+ 
  ylab("Sentiment score * # of occurances") +
  coord_flip()
# groups positioned by alphabetical order (left to right, then next row left to right etc)
# End of analysis of sentiments among newsgroups 

# Sentiment analysis by message
# Now the focus is to find "positive" IDs (individual messages) among all newsgroups.
sentiment_messages <- usenet_words %>%
  # inner_join(): only include words with sentiment values
  inner_join(get_sentiments("afinn"), by = "word") %>% 
  group_by(newsgroup, id) %>%
  summarise(sentiment = mean(value), n_words = n()) %>%
  ungroup() %>%
  filter(n_words >= 5) #For simplicity, only examine IDs with >=5 words 
  
sentiment_messages %>%
  arrange(desc(sentiment))
# Among the top 10 messages, the hockey newsgroup is the most positive!

# Check the most positive message (ID = 52560)
# To do so, we make a customised function that can print the entire ID (aka message).
print_message <- function(group, message_id) {
  result <- cleaned_text %>%
    filter(newsgroup == group, id == message_id, text != "")
  
  cat(result$text, sep = "\n") # Outputs the objects, concatenating the representations.
}

print_message(group = "rec.sport.hockey", message_id = "53560")

# Most negative message
sentiment_messages %>%
  arrange(sentiment) %>%
  slice_min(sentiment, n=1)
# It is a tie between hockey, electronics and mideast politics.

print_message(group = "sci.electronics", message_id = "53899")

# Analysis of Bigrams
usenet_bigrams <- cleaned_text %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2)

usenet_bigrams_counts <- usenet_bigrams %>%
  count(newsgroup, bigram, sort = TRUE) %>%
  ungroup() %>%
  separate(bigram, c("word1", "word2"), sep = " ")

# WARNING: Do not remove stop words for bigrams analysis, since the negation words that
# we are interested in analyzing are in the set of stop words.

# Recall from Chapter 4 that some words (negation words) will  
# lead to incorrect labelling of passages (eg of such bigram: not like)
# We want to evaluate how many bigrams have a negation word as its first word (word1).
negation_words <- c("not","without", "no", "can't", "don't", "won't")

usenet_bigrams_counts %>%
  filter(word1 %in% negation_words) %>%
  count(word1, word2, wt = n, sort = TRUE) %>%
  inner_join(get_sentiments("afinn"), by = c(word2 = "word")) %>%
  mutate(contribution = value * n) %>%
  group_by(word1) %>%
  slice_max(abs(contribution), n=10) %>% # Due to ties, > 10 words may be plotted
  ungroup() %>%
  mutate(word2 = reorder_within(word2, contribution, word1)) %>%
  ggplot(aes(x=contribution, y=word2, fill = contribution > 0)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ word1, scales = "free", nrow = 3) +
  scale_y_reordered() +
  labs(x= "Sentiment value * # of occurances",
       y = "Words preceded by a negation word")
# End of Chapter 9