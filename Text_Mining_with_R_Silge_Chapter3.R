# Text Mining with R
# Julia Silge
# Chapter 3
# 13 Nov 2024

# tf-idf
# term frequency multiplied with inverse document frequency

# Show the term frequency in Jane Austen's novels
library(dplyr)
library(janeaustenr)
library(tidytext)
library(ggplot2)
library(gutenbergr)
library(readr)
library(stringr)

# unnest and count no. of words in all Austen' novels 
book_words <- austen_books() %>%
  unnest_tokens(word, text) %>%
  count(book, word, sort = TRUE) %>%
  ungroup()

# determine the no. of words in each novel
total_words <- book_words %>%
  group_by(book) %>%
  summarize(total = sum(n))
total_words

# append the variables (in total_words) to book_words df
book_words <- left_join(book_words, total_words)

# term freq = no. of times a words appears in a novel divided by total words
# i.e. n/total
ggplot(book_words, aes(n/total, fill = book)) +
  geom_histogram(show.legend = FALSE) +
  xlim(NA, 0.0009) +
  facet_wrap(~book, ncol = 2, scales = "free_y")

# Zipf's Law (Wikipedia) 
# George Zipf was an American linguist who in 1932, observed relations for 
# frequencies of words in natural language texts 
# In mathematical statistics, the concept has been formalized as 
# Zipfian distribution.

# Zipf's law applied on Austen's novels
freq_by_rank <- book_words %>%
  group_by(book) %>%
  mutate(rank = row_number(),
         `term frequency` = n/total)
freq_by_rank

# Plot and observe the relationship between rank and term frequency
freq_by_rank %>%
  ggplot(aes(rank, `term frequency`, color = book)) +
  geom_line(size = 1.1, alpha = .8, show.legend = TRUE)+
  scale_x_log10()+
  scale_y_log10()

# Zoom in : 10 < rank < 500 to fit a line on this almost straight section
rank_subset <- subset(freq_by_rank, (rank <500)&(rank >10))

fitted_line <- lm(log10(`term frequency`) ~ log10(rank), data = rank_subset)
fitted_line

freq_by_rank %>%
  ggplot(aes(rank, `term frequency`, color = book))+
  geom_abline(intercept = fitted_line$coefficients[1],
              slope = fitted_line$coefficients[2],
              color = "gray50",
              linetype = 2)+
  geom_line(size = 1, alpha = .8, show.legend = TRUE)+
  scale_x_log10()+
  scale_y_log10()

# Determine tf-idf of each word of each book 
book_words <- book_words %>%
  tidytext::bind_tf_idf(word, book ,n)
book_words

# Plot the high tf-idf words of each book (1 panel per book)
book_words %>%
  arrange(desc(tf_idf))

book_words %>%
  arrange(desc(tf_idf)) %>%
  group_by(book) %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>%
  # reverse sorting of words excluding repeating words
  slice_max(word, n =15) %>%
  ungroup() %>%
  ggplot(aes(word, tf_idf, fill = book))+
  geom_col(show.legend = FALSE) +
  labs(x= NULL, y = "term frequency* inverse document frequency (tf-idf)") +
  facet_wrap(~book, ncol = 2, scales = "free")+
  coord_flip()

# The plot shows what are the most important words to each novel.
# Readers of Jane Austen would agree unreservedly!

# Corpus of Physics Texts - IDs of the books we are interested to study
physics <- gutenberg_download(c(5001, 13476, 14725, 37729),
                              meta_fields = "author")
# Check that the books wanted are downloaded
unique(physics$gutenberg_id)
colnames(physics)
head(physics)
# only one of the book was successfully downloaded- Discourse on Floating Bodies

# Do manual download, and manually clean up each text (especially the metadata)
# Metadata is usually at the top and bottom of the text.
# ID 14725
# https://www.gutenberg.org/cache/epub/14725/pg14725.txt
# ID 13476
# https://www.gutenberg.org/cache/epub/13476/pg13476.txt

treatise_huygens_14725 <- read_file(file.choose())
lines_14725 <- strsplit(treatise_huygens_14725, "\r?\n")[[1]]
lines_for_merge_14725 <- tibble(gutenberg_id = 14725,
                                text=lines_14725,
                                author = "Huygens, Christiaan")

experiments_tesla_13476 <- read_file(file.choose())
lines_13476 <- strsplit(experiments_tesla_13476, "\r?\n")[[1]]
lines_for_merge_13476 <- tibble(gutenberg_id = 13476,
                                text=lines_13476,
                                author = "Tesla, Nikola")

# As suggested by the warning message - Try a different gutenberg mirror
#physics_einstein <- gutenberg_download(gutenberg_id=5001,
#                                    meta_fields = c("title", "author"),
#                                    mirror = "https://gutenberg.nabasny.com")
# doesn't work

# According to David Robinson's Github page, this R script has been updated 
# around Jan 2024. The ID for Einstein's Relativity book is now 30155
# https://github.com/dgrtwo/tidy-text-mining/blob/master/03-tf-idf.Rmd
einstein_relativity <- gutenberg_download(gutenberg_id = 30155,
                              meta_fields = "author")
# doesn't work
rm(einstein_relativity)
# Do manual download, and manually clean up each text (especially the metadata)
# Metadata at the top and bottom of the text.
# https://www.gutenberg.org/cache/epub/30155/pg30155.txt
einstein_30155 <- read_file(file.choose())
lines_30155 <- strsplit(einstein_30155, "\r?\n")[[1]]
lines_for_merge_30155 <- tibble(gutenberg_id = 30155,
                                text=lines_30155,
                                author = "Einstein, Albert")

physics_2books <- bind_rows(lines_for_merge_14725, lines_for_merge_13476)
physics_3books <- bind_rows(physics_2books, lines_for_merge_30155)

physics <- bind_rows(physics, physics_3books)

# Finally, we have the texts of all 4 books!

# Now we can unnest the text and get the highest tf-idf words of each text
physics_words <- physics %>%
  unnest_tokens(word, text) %>%
  count(author, word, sort = TRUE) %>%
  ungroup()
physics_words

plot_physics <- physics_words %>%
  bind_tf_idf(word, author, n) %>%
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>%
  mutate(author = factor(author, levels = c("Galilei, Galileo",
                                            "Huygens, Christiaan",
                                            "Tesla, Nikola",
                                            "Einstein, Albert")))

plot_physics %>%
  group_by(author) %>%
  slice_max(tf_idf, n = 15) %>%
  ungroup() %>%
  mutate(word = reorder(word, tf_idf)) %>%
  ggplot(aes(x = word, y = tf_idf, fill = author))+
  geom_col(show.legend =  FALSE) + # using facet wrap means legend is redundant
  labs(title = "Highest tf-idf words in each classical Physics text",
       x = NULL, y = "term frequency* inverse document frequency (tf-idf)")+
  facet_wrap(~author, ncol = 2, scales = "free")+
  coord_flip()

# notice that in Einstein's text, _k_ and _x are common words.
# Examine these texts. 

# \\b ensures that only exact words show up (Not letters of larger words)
physics %>%
  filter(str_detect(text, "\\b_k_\\b")) %>%
  select(text, author) # only 1 instance

physics %>%
  filter(str_detect(text, "\\b_x\\b")) %>%
  select(text, author)
# There are 50 cases of "_x" yet it is of lower tf-idf than "_k_"

physics %>%
  filter(str_detect(text, "\\bab\\b")) %>%
  select(text, author)
# 3 instances of 'ab'

# Remove some of these less meanining words and re-plot the highest tf-idf.
mystopwords <- tibble(word = c("eq", "co", "ac", "rc", "cg", "fig", "cd",
                               "s", "d", "f", "g", "n", "o", "ad",
                                   "ak", "bn", "cb", "cm", "file", "ab",
                                   "_k_", "co", "_x", "_k"))
physics_words <- anti_join(physics_words, mystopwords, by="word")

plot_physics_2nd <- physics_words %>% 
  bind_tf_idf(word, author, n) %>%
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>%
  mutate(author = factor(author, levels = c("Galilei, Galileo",
                                            "Huygens, Christiaan",
                                            "Tesla, Nikola",
                                            "Einstein, Albert")))

plot_physics_2nd %>%
  group_by(author) %>%
  slice_max(tf_idf, n = 15) %>%
  ungroup() %>%
  mutate(word = reorder(word, tf_idf)) %>%
  ggplot(aes(x = word, y = tf_idf, fill = author))+
  geom_col(show.legend = FALSE)+
  labs(title = "Highest tf-idf words in each classical Physics text",
      subtitle = "after removing stop words",
       x = NULL, y = "term frequency* inverse document frequency (tf-idf)")+
  facet_wrap(~author, ncol = 2, scales = "free")+
  coord_flip()
