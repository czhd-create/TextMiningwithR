# Text Mining with R
# Julia Silge and David Robinson
# 14 November 2024

library(dplyr)
library(tidytext)
library(janeaustenr)
library(tidytext)
library(tidyr)
library(ggplot2)
library(readr)
library(stringr)
# The next three libraries are for network visualization
library(igraph)
library(ggraph)
library(grid)
# The last library to examine is for computing correlations between words
library(widyr)

# We have examined unigram (single word) in previous chapters.
# Now lets focus on bigrams (two words that appear after each other).

austen_bigrams <- austen_books() %>%
  unnest_tokens(bigram, text, token = "ngrams", n=2)
nrow(austen_bigrams)
# The book's output is 725000+ rows. 
# For simplicity, ignore this difference for now. 

a <- austen_bigrams %>%
  count(bigram) %>%
  arrange(desc(n)) %>%
  ungroup()
head(a)

# lets drop the NA row
b <-a %>% slice(-1)
head(b)

# most of the bigrams are pairs of common words in English language.

# lets remove those common bigrams. Treat as (2-word) stop words.
# Excluding common bigrams show us other common pairs in Jane Austen's books.

# Note: stop_words is a df of English stop words compiled into tidytext package
bigrams_separated <- austen_bigrams %>%
  tidyr::separate(bigram, c("word1", "word2"), sep = " ")

bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

bigram_counts <- bigrams_filtered %>%
  count(word1, word2, sort = TRUE)
bigram_counts
# lets drop the NA row
bigram_counts <-bigram_counts %>% slice(-1)
bigram_counts

# Insight:
# Names (word2) with salutations (word1) are common in Jane Austen's novels.

# Now that we have sieved out the stop words, recombine to form a new df.
bigrams_united <- bigrams_filtered %>%
  unite(bigram, word1, word2, sep = " ")
bigrams_united

# But there are lots of NA rows. Clean up the dataset.
bigrams_united2 <- bigrams_united %>%
  filter(bigram != 'NA NA')
bigrams_united2

# Trigrams - Consecutive sequences of three words
Trigrams_df <- austen_books() %>%
  unnest_tokens(trigram, text, token = "ngrams", n=3) %>%
  separate(trigram, c("word1", "word2", "word3"), sep = " ") %>%
  filter(!word1 %in% stop_words$word,
         !word2 %in% stop_words$word,
         !word3 %in% stop_words$word) %>%
  count(word1, word2, word3, sort = TRUE)
Trigrams_df 
# Remove the first row (don't need it)
Trigrams_df[-1,]

# Return to analysis of Bigrams
# Look for common streets in Jan Austen's novels 
# Similar analysis can be done for the word 'road'
bigrams_filtered %>%
  filter(word2 == "street") %>%
  count(book, word1, sort = TRUE)

# Recall the concept of tf-idf from Chapter 3. 
# Lets apply it for bigrams
bigram_tf_idf <- bigrams_united2 %>%
  count(book, bigram) %>%
  tidytext::bind_tf_idf(bigram, book, n) %>%
  arrange(desc(tf_idf))
bigram_tf_idf

# Plot the data
bigram_tf_idf %>%
  group_by(book) %>%
  slice_max(tf_idf, n =14) %>%
  ungroup() %>%
  mutate(bigram = reorder(bigram, tf_idf)) %>%
  ggplot(aes(bigram, tf_idf, fill = book))+
  geom_col(show.legend = FALSE) +
  labs(x= NULL, y = "term frequency* inverse document frequency (tf-idf)") +
  facet_wrap(~book, ncol = 2, scales = "free")+
  coord_flip()

# Sentiment analysis on bigrams
bigrams_separated %>%
  filter(word1 == "not") %>%
  count(word1, word2, sort = TRUE)

# Use AFINN lexicon to score the bigram
AFINN <- get_sentiments("afinn")
colnames(AFINN)
# Note: 1st column is "word" which we need for inner joining with bigrams df

not_words <- bigrams_separated %>%
  filter(word1 == "not") %>%
  inner_join(AFINN, by = c(word2 = "word")) %>%
  count(word2, value, sort = TRUE) %>%
  ungroup()

not_words
colnames(not_words)
# Note: 2nd and 3rd column (value, n respectively) are used for a formula later.

# Visualise not words to see how much the bigram with 
# first word 'not' affects sentiment score

# ss - sentiment score
bigram_ss_plot <- not_words %>%
  mutate(contribution = (n * value)) %>% # Formula of new variable, contribution.
  arrange(desc(abs(contribution))) %>%
  head(20) %>%
  mutate(word2 = reorder(word2, contribution))
# word2 is the word that follows after the word not (word1)

# Display the top performing word2 
ggplot(data = bigram_ss_plot, aes(x=word2, y=contribution,
                                  fill = contribution > 0)) +
  geom_col(show.legend = FALSE) +
  xlab("Words preceded by \"not\"") +
  ylab("Contribution") +
  coord_flip()

# Other negated words could be included
# Such words include "not", "never", "without", "no"
negation_words <- c("not", "never", "without", "no")
not_plus_other_words <- bigrams_separated %>%
  filter(word1 %in% negation_words) %>%
  inner_join(AFINN, by = c(word2 = "word")) %>%
  count(word1, word2, value, sort = TRUE)
  # ungroup()
# David Robinson's code @ his Github page - 
# https://github.com/dgrtwo/tidy-text-mining
not_plus_other_words %>%
  mutate(contribution = n * value,
         word2 = reorder(paste(word2, word1, sep = "__"), contribution)) %>%
  group_by(word1) %>%
  slice_max(abs(contribution), n = 12, with_ties = FALSE) %>%
  ggplot(aes(word2, contribution, fill = n * value > 0)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ word1, scales = "free") +
  scale_x_discrete(labels = function(x) gsub("__.+$", "", x)) +
  xlab("Words preceded by negation term") +
  ylab("Sentiment value * # of occurrences") +
  coord_flip()
# End of David Robinson's code 
# My code didn't work (Used David Robinson's codes instead - lines 160-171)
# # This is what I want to plot but my code didn't fix the order.
# see <- bigram_ss_plot_original %>% 
#   group_by(word1) %>% 
#   arrange(desc(contribution), .by_group = TRUE)

# Visualizing a Network of Bigrams with igraph and ggraph packages

# Filter for common combinations - turn bigrams to weights (edges) & vertices
bigram_counts_to_graph <- rename(bigram_counts, from=word1, to=word2, weight=n)
bigram_graph <- bigram_counts_to_graph %>%
  filter(weight > 20) %>%
  graph_from_data_frame(directed = TRUE)
bigram_graph
# output: From the Environment pane: It says Length = 85, Value = List of 85

# Visualize the graph object using ggraph
set.seed(2017)

# dh: Uses Davidson and Harel simulated annealing algorithm to place nodes.
# Drawing graphs nicely using simulated annealing (1996) 
# https://dl.acm.org/doi/10.1145/234535.234538 
ggraph(bigram_graph, layout = "dh")+
  geom_edge_link()+
  geom_node_point()+
  geom_node_text(aes(label = name), vjust=1.2, hjust=1)

# add directions, enhance the aesthetics
a <- grid::arrow(type = "closed", length = unit(.1, "inches"))
ggraph(bigram_graph, layout = "dh")+
  geom_edge_link(aes(edge_alpha = weight), show.legend = FALSE,
                 arrow = a, end_cap = circle(.07, 'inches'))+
  geom_node_point(color = "lightblue", size = 3)+
  geom_node_text(aes(label = name), size = 3, vjust= 0.9, hjust=1)+
  theme_void()
# arrows that are bolder are of higher weight values.
# keep running the ggraph code to find a good fit in the plot space.

# Visualizing bigrams in other texts
# To do so, create two functions that we need to reuse recursively.
count_bigrams <- function(dataset) {
  dataset %>%
    unnest_tokens(bigram, text, token = "ngrams", n =2) %>%
    separate(bigram, c("word1", "word2"), sep = " ") %>%
    filter(!word1 %in% stop_words$word,
           !word2 %in% stop_words$word,) %>%
    count(word1, word2, sort = TRUE) 
}

visualize_bigrams <- function(bigrams) {
  set.seed(2017)
  a <- grid::arrow(type = "closed", length = unit(.1, "inches"))
  
  bigrams %>%
    graph_from_data_frame() %>%
    ggraph(layout = "dh") +
    geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                   arrow = a)+
    geom_node_point(color = "lightblue", size = 3)+
    geom_node_text(aes(label = name), size = 3, vjust= 0.9, hjust=1)+
    theme_void()
}

# The King James Version Bible (Book ID 10 in Project Gutenberg)
# kjv_text <- gutenbergr::gutenberg_download(gutenberg_id = 10)
# above line (when run) > Download failed
# Go to the project website:
# or download from: https://www.gutenberg.org/cache/epub/10/pg10.txt
# Remember to delete the metadata from the text file before feeding into RStudio
holy_bible_kjv_raw_file <- read_file(file.choose())
lines_10 <- strsplit(holy_bible_kjv_raw_file, "\r?\n")[[1]]
kjv_text <- tibble(gutenberg_id = 10,
                                text=lines_10,
                                author = "Various")

kjv_bigrams <- kjv_text %>% count_bigrams()

# Remove NA row
kjv_bigrams <- kjv_bigrams[-1,]

# filter out rare combinations
# filter out digits (verse numbers) 
kjv_bigrams %>%
  filter(n > 40,
         !str_detect(word1, "\\d"),
         !str_detect(word2, "\\d")) %>%
  visualize_bigrams()

# Prompt to Chat GPT: do you know what this regular expression mean: "\\d"?
# The regular expression \\d is used to match digits. 
# d: The character d in regular expressions stands for "digit" and 
# matches any single digit from 0 to 9.
# So, \\d in R means "match any digit (0-9)".

# Error detected: 
# ! object 'weight' not found
# visualize_bigrams() is a function you custom-made (lines 222-234) so naturally
# there is no official documentation and an LLM won't understand it!
# To debug, change the word 'weight' in your function to 'n' 
# It was weight due to line 180 during renaming of column names
# Re-run your visualize_bigrams() at its original source to override it,
# before running line 255 -259 again. There shouldn't be an error this 2nd time.

# The visualization is nice!

# Counting and Correlating Pairs of words with the widyr package
# Do words tend to appear in the same section?
austen_section_words <-  austen_books() %>%
  filter(book == "Pride & Prejudice") %>%
  mutate(section = row_number() %/% 10) %>% # Each section is 10-lines long
  filter(section > 0) %>% # Exclude Preface 
  unnest_tokens(word, text) %>%
  filter(!word %in% tidytext::stop_words$word)

austen_section_words

# Count words co-occuring within the same section
word_pairs <- austen_section_words %>%
  pairwise_count(word, section, sort = TRUE)

colnames(word_pairs)
# Words that often occur with 'darcy'
word_pairs %>%
  filter(item1 == "darcy") %>%
  head()

# Compute pairwise corelation using phi coefficient
word_cors <- austen_section_words %>%
  group_by(word) %>%
  filter(n() >= 20) %>%
  widyr::pairwise_cor(word, section, sort = TRUE)
word_cors

# Words that correlate with 'pounds'
word_cors %>%
  filter(item1 == "pounds") %>%
  head()

# Suppose we have a list of interesting words. 
# Find the other words most associated with them
word_cors %>%
  filter (item1 %in% c("elizabeth", "pounds", "married", "pride")) %>%
  group_by(item1) %>%
  slice_max(correlation, n=6) %>%
  ungroup() %>%
  mutate(item2 = reorder(item2, correlation)) %>%
  ggplot(aes(x = item2, y=correlation)) +
  geom_bar(stat = "identity") +
  facet_wrap(~item1, ncol=2, scales = "free") +
  coord_flip()

# Visualize the correlations to find word clusters
set.seed(2017)

word_cors %>%
  filter(correlation > .15) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "dh") +
  geom_edge_link(aes(edge_alpha = correlation), show.legend = FALSE)+
  geom_node_point(color = "lightblue", size = 3)+
  geom_node_text(aes(label = name), size = 3, repel = TRUE)+
  # If TRUE, text labels will be repelled from each other to avoid overlapping
  theme_void()
