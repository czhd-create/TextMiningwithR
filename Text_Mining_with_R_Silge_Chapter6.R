# Text Mining with R
# Julia Silge and David Robinson
# 18 November 2024
# Chapter 6

# IMPT NOTE on working directory: 
# setwd(your working directory where books.rda is saved)
# see lines 98-106 for information about books.rda

# The main content covers latent dirichlet allocation (LDA) applications, a  
# technique for identifying topics among documents in an unsupervised manner.

# These libraries will be used for this chapter.
library(ggplot2)
library(tidytext)
library(topicmodels) # v0.2-17 requires R version 4.3.3 (2024-02-29)
library(tidyr)
library(dplyr)
library(scales)
library(mallet)
library(forcats)
library(stringr)

# Refer to the book for a flowchart of a typical text analysis (Fig 6.1),
# that includes topic modelling.

# Compared with the previous chapter Fig 5.1, the 2 additions here is the:
# 1) LDA(Document-Term Matrix) -> Model and 2) tidy(Model) -> Tidied Model 

# loads AssociatedPress dataset into the environment.
data("AssociatedPress", package = "topicmodels")
AssociatedPress
# AssociatedPress is a Document Term Matrix of 2246 documents and 10k+ terms.

# Create a 2-topic LDA model, and fit it with AssociatedPress dataset
ap_lda <- topicmodels::LDA(AssociatedPress, k=2, control = list(seed=1234))

ap_topics <- tidy(ap_lda, matrix = "beta")
ap_topics
ap_topics$topic <- factor(ap_topics$topic)

# 'aaron' has a 1.69 x 10^-12 probability of being generated from topic 1,
# and a higher probability 3.90 x 10^-5 of being generated from topic 2.

# Top 10 terms most common within each topic
# Process the data
ap_top_terms <- ap_topics %>%
  group_by(topic) %>%
  slice_max(beta, n = 10) %>%
  ungroup() %>% # Ungroup for plotting
  arrange(topic, -beta)

# reorder_within() has to be used with scale_y_reordered()
ap_top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  scale_y_reordered()

beta_spread <- ap_topics %>%
  # include "topic" as suffix to each value in topic variable
  mutate(topic = paste0("topic", topic)) %>%  
  spread(topic, beta) %>%
  filter(topic1 > .001 | topic2 > .001) %>%
  mutate(log_ratio = log2(topic2/topic1))
beta_spread

beta_spread %>%
  slice_max(abs(log_ratio), n = 20, with_ties = FALSE) %>%
  arrange(term, -log_ratio) %>%
  mutate(term = reorder(term, log_ratio)) %>%
  mutate(topic = if_else(log_ratio > 0, "Topic 2", "Topic 1")) %>%
  ggplot(aes(x=term, y=log_ratio, fill = topic)) +
  geom_col(show.legend = TRUE) +
  scale_y_continuous(breaks = c(-100, -50, 0, 50))+
  xlab("Term") +
  ylab("Log2 ratio of beta in topic2/topic1") +
  #scale_y_reordered() +
  coord_flip() 

# Document - topic probabilities
ap_documents <- tidy(ap_lda, matrix = "gamma")
ap_documents

ap_documents %>% filter(document == 1)
# The model estimates that in document 1, 24.8% of the words are generated 
# from topic 1. The remainder 75.2% of the words are generated from topic 2.

ap_documents %>% filter(document == 6)
# Document 6 : topic 1 gamma probability is too low. For a 2-topic model,
# document 6 is almost entirely made up of topic 2 terms.

tidy(AssociatedPress) %>%
  filter(document == 6) %>%
  arrange(desc(count))
# The most common words appear to speak about American government and 
# Panamanian dictator Manuel Noriega.

# The Great Library Heist
# The code on the published book doesn't work anymore

# Workaround - Load pre-downloaded data files, in similar fashion to Chapter 5.
# Download dataset from:
# https://github.com/dgrtwo/tidy-text-mining/blob/master/data/books.rda

# setwd(your working directory where books.rda is saved)
load("books.rda")

# divide into documents, each representing one chapter
reg <- regex("^chapter ", ignore_case = TRUE)
# Comments from ChatGPT:
# ignore_case= TRUE: makes the pattern case-insensitive
# The caret ^ is an anchor that matches the beginning of a string. 
# chapter : This is the literal string we are searching for 
# — specifically the word "chapter" followed by a space.

# divide into documents, each representing one word per document per row
by_chapter_then_by_word <- books %>%
  group_by(title) %>%
  mutate(chapter = cumsum(str_detect(
    text, reg))) %>%
  ungroup() %>%
  filter(chapter > 0) %>%
  unite(document, title, chapter) %>%
  unnest_tokens(word, text)

# find document-word counts, after removing the stop words
word_counts <- by_chapter_then_by_word %>%
  anti_join(tidytext::stop_words) %>%
  count(document, word, sort = TRUE)

word_counts

# LDA on Chapters
# Now, we have word_counts, the number of words per chapter per book
# after removing stop words, where the words are counted by its frequency of 
# appearance in that chapter.

# The variables in word_counts are:
# document (which specifies the chapter of the particular book)
# word (the word detected in that chapter of the particular book)
# n (the number of times this word appears in that chapter)

# Lets make the DocumentTermMatrix from word_counts
chapters_dtm <- word_counts %>% 
  cast_dtm(document = document, term = word, value = n)
chapters_dtm
# Create the 4-topic model
chapters_LDA <- LDA(chapters_dtm, k=4, control = list(seed=1234))
# Get the beta probabilities, for each term associated with each topic.
chapters_topics <- tidy(chapters_LDA, matrix = "beta")
chapters_topics

# Sieve out the top 5 terms (by beta value) of each topic
top_5_terms <- chapters_topics %>%
  group_by(topic) %>%
  slice_max(beta, n=5) %>%
  arrange(topic, -beta)
top_5_terms

# Visualize it
top_5_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(x=beta, y=term, fill=factor(topic))) +
  geom_col(show.legend = FALSE)+
  facet_wrap(~topic, scales = "free")+
  scale_y_reordered()+
  labs(title = "Most common terms within each topic (Beta probabilities)")
# From the plot, it is clear to identify the books as such:
# Topic 1 belongs to Pride & Prejudice
# Topic 2 belongs to Twenty Thousand Leagues under the Sea
# Topic 3 belongs to The War of the Worlds
# Topic 4 belongs to Great Expectations.
# Change the value of n in line 153 if 5 terms are not convincing enough.

# What about within each document? Find the gamma probabilities of the topics 
# that constitutes each document.
chapters_gamma <- tidy(chapters_LDA, matrix = "gamma")
chapters_gamma

chapters_gamma %>% filter(document =="The War of the Worlds_16")
# Based on the gamma probabilities, chapter 16 of the War of the Worlds 
# is without doubt associated with topic 3. 

# chapters_gamma: split the book and chapter of the 'document' variable
chapters_gamma_spiltted <- chapters_gamma %>%
  separate(document, c("title", "chapter"), sep = "_", convert = TRUE)
# convert = TRUE: Convert a data object to logical, integer, numeric, 
# complex, character or factor as appropriate.
head(chapters_gamma_spiltted)

# reorder titles in order of topic 1 and topic 2 before splitting
chapters_gamma_spiltted %>%
  mutate(title = reorder(title, topic * gamma)) %>%
  ggplot(aes(x=factor(topic), y = gamma)) +
  geom_boxplot() +
  facet_wrap(~title) +
  labs(x = "topic", y = expression(gamma))

# It seems that chapters in Great Expectations were sometimes misclassfied.
# Let examine this more closely.

# Assign each book chapter to its most likely topic
chapter_classifications <- chapters_gamma_spiltted %>%
  group_by(title, chapter) %>%
  slice_max(gamma, n=1) # determines which of the 4 topics has the highest gamma value
chapter_classifications %>% arrange(desc(gamma))

# Make a variable, consensus, by interpreting the plot results (lines 189-194)
consensus_chapter_df <- chapter_classifications %>%
  mutate(consensus = case_when(topic == 1 ~ "Pride and Prejudice", 
                               topic == 2 ~ "Twenty Thousand Leagues under the Sea",
                               topic == 3 ~ "The War of the Worlds", 
                               topic == 4 ~ "Great Expectations"))
consensus_chapter_df %>% filter(title != consensus)

# By-Words Assignments 
# augment() : adds information to the LDA model using its DocumentTermMatrix
assignments <- augment(chapters_LDA, data=chapters_dtm)
assignments
# notice that a new variable(.topic) is created. 

# See which terms are incorrectly classified with a df, 
# followed by a plot (confusion matrix)
assignments_consensus <- assignments %>%
  separate(document, c("title", "chapter"), sep = "_", convert = TRUE) %>%
  mutate(consensus = case_when(.topic == 1 ~ "Pride and Prejudice", 
                               .topic == 2 ~ "Twenty Thousand Leagues under the Sea",
                               .topic == 3 ~ "The War of the Worlds", 
                               .topic == 4 ~ "Great Expectations"))

assignments_consensus %>%
  count(title, consensus, wt = count) %>%
  # wt is frequency weights: If =variable, computes sum(wt) for each group.
  mutate(across(c(title, consensus), ~str_wrap(., width= 20))) %>%
  # str_wrap(): Wrap words into nicely formatted paragraphs
  group_by(title) %>%
  mutate(percent = n / sum(n)) %>%
  ggplot(aes(consensus, title, fill = percent)) +
  geom_tile() +
  scale_fill_gradient2(high = "darkred", label = percent_format()) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        panel.grid = element_blank()) +
  labs(x = "Book words were assigned to",
       y = "Book words came from",
       fill = "% of assignments")
# A very small percentage of words from "Great Expectations" were misassigned
# to other books.

# Misassignment means title not equals consensus
# Common mistakenly assigned words
wrong_words <- assignments_consensus %>%
  filter(title!=consensus)
wrong_words

# Sum total of misassigned words 
wrong_words %>%
  count(title, consensus, term, wt = count) %>%
  arrange(desc(n))

# word 'flopson' was from Great Expectations but notice that all its assigments
# (lines 257-259) vs line 263 that this word has no consensus.
word_counts %>% filter(word == "flopson")

# Alternative implementation of LDA (Mallet)
# The code for the rest of the script is from David Robinson's github site,
# except lines 291-297 which is reuse of code from lines 186-197
# Refer to https://mimno.github.io/Mallet/index for more info on MALLET.

# create a vector with one string per chapter
collapsed <- by_chapter_then_by_word %>%
  anti_join(stop_words, by = "word") %>%
  mutate(word = str_replace(word, "'", "")) %>%
  group_by(document) %>%
  summarize(text = paste(word, collapse = " "))

# create an empty file of "stopwords"
file.create(empty_file <- tempfile())
docs <- mallet.import(collapsed$document, collapsed$text, empty_file)

mallet_model <- MalletLDA(num.topics = 4)
mallet_model$loadDocuments(docs)
mallet_model$train(100)

# word-topic pairs (outputs beta probability)
tidy(mallet_model)

# document-topic pairs
chapters_gamma_mallet <- tidy(mallet_model, matrix = "gamma")

chapters_gamma_mallet %>%
  separate(document, c("title", "chapter"), sep = "_", convert = TRUE) %>%
  mutate(title = reorder(title, topic * gamma)) %>%
  ggplot(aes(x=factor(topic), y = gamma)) +
  geom_boxplot() +
  facet_wrap(~title) +
  labs(x = "topic", y = expression(gamma))
# The results are different from usual LDA implementation.

# column needs to be named "term" for "augment"
term_counts <- rename(word_counts, term = word)
assignments_mallet <- augment(mallet_model, term_counts)

# Procedures similar to previous LDA implementation can be applied again.
# End of script