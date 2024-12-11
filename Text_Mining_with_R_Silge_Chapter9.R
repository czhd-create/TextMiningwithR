# Text Mining with R
# Julia Silge and David Robinson
# Chapter 9 - Analyzing Usenet Text
# 11 December 2024

library(dplyr)
library(tidyr)
library(purrr)
library(readr)
library(ggplot2)
library(stringr)
library(tidytext)
library(widyr)
library(igraph)
library(ggraph)
library(topicmodels)

# go to this url: http://qwone.com/~jason/20Newsgroups/
# In the website, click "20news-bydate.tar.gz" to download the file.
# To unzip the tar.gz file, doubleclick it. It creates a folder (if user using Mac OS).
# After the new folder is created, click inside. You will see 2 folders (..test, ..train). 
# Using your own directory, create, training_folder, that points to the train folder:
training_folder <- "/Users/dan/Downloads/20news-bydate/20news-bydate-train"

# File reading scripts (lines 27-40) lifted from David Robinson's Github page.
# Define a function (read_folder) to read all files from a folder into a data frame.
read_folder <- function(infolder) {
  tibble(file=dir(infolder, full.names = TRUE)) %>%
    mutate(text = map(file, read_lines)) %>%
    transmute(id = basename(file), text) %>%
    unnest(text)
}

# Use map() and unnest() to apply read_folder to each subfolder
raw_text <- tibble(folder = dir(training_folder, full.names = TRUE)) %>%
  mutate(folder_out = map(folder, read_folder)) %>%
  unnest(cols = c(folder_out)) %>%
  transmute(newsgroup = basename(folder), id, text)

raw_text

# Go to the abovementioned url (line 18: Command + Click) to learn more about the dataset. 
# Also have a look: https://kdd.ics.uci.edu/databases/20newsgroups/20newsgroups.data.html

unique(raw_text$newsgroup)
# 20 unique newsgroups (topics)

sample(raw_text$text, 5)
length(unique(raw_text$id))
# 9840 unique ids

# In each newsgroup, How many unique ids are there?
raw_text %>%
  group_by(newsgroup) %>%
  # n_distinct() counts the no. of unique/distinct combinations in a set of >= 1 vector. 
  summarize(messages = dplyr::n_distinct(id)) %>%
  ggplot(aes(x=reorder(newsgroup, messages), y=messages))+
  geom_col()+
  labs(title = "Number of messages from each newsgroup", x="newsgroup")+
  coord_flip()

# Preprocessing the text
cleaned_text <- raw_text %>%
  group_by(newsgroup, id) %>%
  filter(cumsum(text == "") > 0,
         cumsum(str_detect(text, "^--")) == 0) %>%
  ungroup() 
# ungroup(): This function is critical here, unlike in ggplot where removing "ungroup()"
# still produces the same plot.

# Removes every text that comes before (1st cumsum = 0) the first empty line.
# Also removes every text that comes after (2nd cumsum !=0) the first "--" detected in text.
# Test cumsum() function with : > cumsum(c(TRUE, TRUE, TRUE, TRUE, TRUE)) > 4

# Next, we look for a specific kind of text and delete the rest. (also keep text == "")
cleaned_text <- cleaned_text %>%
  # The regex is seeking "text" that "doesn't start with >" and starts with alphabet/digit
  filter(str_detect(text, "^[^>]+[A-Za-z\\d]") | text == "" )

# Next, remove text that are quotes from other bulletin board users.
cleaned_text <- cleaned_text %>%
  filter(!str_detect(text, "writes(:|\\.\\.\\.)$"),
         !str_detect(text, "^In article <"))

# Next, remove non-text. Preliminary checks from the authors show these are ID 9704, 9985
cleaned_text <- cleaned_text %>%
  filter(!id %in% c(9704, 9985))

# Data Pre-processing of raw text is completed.

# Split the dataset into tokens (unigram in this example)
usenet_words <- cleaned_text %>%
  unnest_tokens(word, text) %>%
  filter(str_detect(word, "[a-z']$"),
         !word %in% stop_words$word) # take out commonly used words in English
# NOTE on usenet_words:
# Duplicate rows will appear if a word appears more than twice in any id of any newsgroup.
# These will be tallied with a word count function such as n() during topic modellng.

# Chat GPT:
# [a-z']$ will match if the last character ($) in a string (or the last character 
# before a line break) is either a lowercase letter (a-z) or an apostrophe (').

# Exploratory Data analysis 
# Most frequent words in the dataset
usenet_words %>% 
  count(word, sort = TRUE)

# When grouped by newsgroup, what are the most frequent words in each of them?
words_by_newsgroup <- usenet_words %>%
  count(newsgroup, word, sort = TRUE)
words_by_newsgroup

# Get the tf_idf of each word of each newsgroup
tf_idf_usenet <- words_by_newsgroup %>%
  tidytext::bind_tf_idf(word, newsgroup, n) %>%
  arrange(desc(tf_idf))

tf_idf_usenet

# Examine the top tf-idf for newsgroups starting with "sci."
tf_idf_usenet %>%
  filter(str_detect(newsgroup, "^sci\\.")) %>%
  group_by(newsgroup) %>%
  slice_max(tf_idf, n = 12) %>%
  ungroup() %>%
  mutate(word = reorder(word, tf_idf)) %>%
  ggplot(aes(x=tf_idf, y=word, fill = newsgroup))+
  geom_col(show.legend = FALSE) +
  facet_wrap(~ newsgroup, scales = "free")+
  labs(x = "tf-idf", y = NULL, 
       title = "Terms with highest tf-idf within each science-related newsgroup")

# Which newsgroups have similar text content?
newsgroup_cors <- words_by_newsgroup %>%
  widyr::pairwise_cor(newsgroup, word, n, sort = TRUE)
newsgroup_cors

# Lets use graphs to visualise newgroups with higher (>=.4) correlation.
set.seed(2017)
newsgroup_cors %>%
  filter(correlation >= .4) %>%
  igraph::graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = correlation))+
  geom_node_point(size = 3, color = "lightblue")+
  geom_node_text(aes(label=name), repel = TRUE)+
  labs(title = "Network of Newsgroups with Word Correlation >= .4")+
  theme_void()+
  theme(legend.position = "bottom")
# To increase edge thickness, add to geom_edge_link() :aes(edge_width = correlation))
# Interpretation: There are 4 clusters of newsgroups: sports, auto-vehicles, 
# computers with electronics, and politics with religion.

# Topic Modelling
# Create a document term matrix for words that:
# (1) Comes from science-related newsgroups, 
# (2) Where word count > 50

word_sci_newsgroups <- usenet_words %>%
  filter(str_detect(newsgroup, "^sci")) %>%
  group_by(word) %>%
  mutate(word_total = n()) %>%
  # word_total adds words within and across all ids and across newsgroups as well.
  ungroup() %>%
  filter(word_total > 50)

# arguments of unite(): df, name_of_new_column, <tidy-select> Columns to unite). 
# In this example, we unit the columns: newsgroup and id, to form a new column: document
# ?tidyr_tidy_select to learn more
sci_dtm <- word_sci_newsgroups %>%
  tidyr::unite(document, newsgroup, id) %>%
  count(document, word) %>% # count(): n = 1 if any word appears in a document
  cast_dtm(document, word, n) # arguments of cast_dtm(): data, document, term, value

# Train the LDA model with k=4
sci_lda <- LDA(sci_dtm, k = 4, control = list(seed = 2016))

# What words are comprised in each topic of the model(k=4)?
# Do the words match with the four newgroups we visualised earlier?

sci_lda %>%
  slice_max(tf_idf, n = 12) %>%
  mutate(word = reorder(word, tf_idf)) %>%
  ggplot(aes(x=tf_idf, y=word, fill = newsgroup))+
  geom_col(show.legend = FALSE) +
  facet_wrap(~ newsgroup, scales = "free")+
  labs(x = "tf-idf", y = NULL, 
       title = "Terms with highest tf-idf within each science-related newsgroup")

# Construct a tidy dataframe that summarises the results of the model.
tidy_sci_LDA <- tidytext::tidy(sci_lda) # default matrix = beta

top_12_sci_words <- tidy_sci_LDA %>%
  group_by(topic) %>%
  slice_max(beta, n=12) %>%
  arrange(topic, -beta)
top_12_sci_words

top_12_sci_words %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  group_by(topic, term) %>%    
  arrange(desc(beta)) %>%  
  ggplot(aes(beta, term, fill = as.factor(topic))) +
  geom_col(show.legend = FALSE) +
  scale_y_reordered() +
  labs(title = "Top 12 terms in each LDA topic",
       x = expression(beta), y = NULL) +
  facet_wrap(~ topic, ncol = 2, scales = "free")
# Interpretation: We know there were 4 science-related groups
# which are electronics, medicine, cryptography and space.
# From the plot, we see that topic 1 is probably related to space (words: space, nasa)
# Topic 2 is cryptography as the top words are key, encryption.
# Topic 3 is related to medicine as the top words include water, food, doctor, patients.
# Topic 4 is electronics with top words such as ground, wire, current, circuit.

# Below is different code for lines 192-209, source: https://www.tidytextmining.com/usenet
sci_lda %>%
  tidy() %>%
  group_by(topic) %>%
  slice_max(beta, n = 8) %>%
  ungroup() %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  scale_y_reordered()
# End of different code

# The distinction between topics is clear for gamma probability as well.
sci_lda %>%
  tidy(matrix = "gamma") %>%
  separate(document, c("newsgroup", "id"), sep = "_") %>%
  mutate(newsgroup = reorder(newsgroup, gamma * topic)) %>%
  ggplot(aes(x = factor(topic), y = gamma))+
  geom_boxplot() +
  facet_wrap(~ newsgroup) +
  labs(x = "Topic", y = "# of messages where this was the highest topic")
# Insights: 
# Topic 1 = Space, Topic 2 = Cryptography, Topic 3 = Medicine, Topic 4 = Electronics

# The part on Sentiment Analysis of this dataset will be in a separate file,
# Text_Mining_with_R_Silge_Chapter9_SA.R