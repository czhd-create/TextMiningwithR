# Text Mining with Julia Silge
# Chapter 2
# 09 Nov 2024

# Code tested on
# RStudio 2023.12.1 Build 402
# "Ocean Storm" Release (4da58325, 2024-01-29) for macOS
# R version 4.3.2 (2023-10-31) -- "Eye Holes"

# Please ensure these libraries are installed via install.packages("pkg_name")
library(textdata) # has to be version 0.4.2
#remotes::install_github("emilhvitfeldt/textdata@v0.4.2")
#library(remotes) # for updates in downloading nrc dataset
library(tidytext)
library(readr)
library(janeaustenr)
library(dplyr)
library(stringr)
library(tidyr)
library(ggplot2)
library(wordcloud)
library(reshape2)
library(stopwords)

tidytext::sentiments # doesn't match the book's output (27,314 rows)

bing_df <- tidytext::get_sentiments(lexicon = "bing")
bing_df

# Notice that the sentiments dataset is simply the bing dataset.

# ?get_sentiments shows all the lexicons available in tidytext.

afinn_df <- tidytext::get_sentiments(lexicon = "afinn")
afinn_df

# nrc_df <- tidytext::get_sentiments(lexicon ="nrc")
# get_sentiments does not work for nrc dataset. Based on the error message,
# we want to choose NRC-Emotion-Lexicon-Wordlevel-v0.92.txt

# Download the dataset from the url provided in book: 
# http://bit.ly/2s4B8ts

# Click on the link, and allow the download. The link is the following sentence:  
# Download the NRC Word-Emotion Association Lexicon.... Research or Educational)

#nrc_df <- tidytext::get_sentiments(lexicon ="nrc") 
# Selection: Enter 1 (or esc)
# If 1 is entered, wait. The download would fail. 

# Google search: nrc lexicon cannot run in R
# First hit: stackoverflow > Comments >
# Julia Silge's github : https://github.com/juliasilge/tidytext/issues/189

?lexicon_nrc # Read the documentation carefully and try to adjust its arguments.
# Return path and manual_download = TRUE. try again.
# ret running
# textdata::lexicon_nrc(return_path=TRUE, manual_download = TRUE)
# It won't work. 
# There will be an error message in R Console.
# To fix the error, place the file such that the directory exists (instead of not exist)
# The file should be in the zip file downloaded from the described lines 41-45.
# /Users/dan/Downloads/nrc/NRC-Emotion-Lexicon/NRC-Emotion-Lexicon-v0.92/NRC-Emotion-Lexicon-Wordlevel-v0.92.txt
# Note: User may need to create folders for the directory and save the directory
# in an object, wd (working directory)

# Get the file name from the error message
# NRC-Emotion-Lexicon-Wordlevel-v0.92.txt'
wd <- "/Users/dan/Downloads" # exact words depend on OS and folder structure
wd
nrc_df <- textdata::lexicon_nrc(dir=wd, 
                                return_path=TRUE, 
                                manual_download = TRUE)
nrc_df # 13,872 rows! close to the book's output: 13,901! :) accept this difference.

# Note: English words that are neutral (or invented after lexicons are made)
# won't appear in the lexicon dataframes

# Inner_join operations
# Use the same code from Chapter 1 (line 58), appending one line: unnest_tokens()
# unnest_tokens convert the text into 
original_austen <- janeaustenr::austen_books() %>%
  group_by(book) %>%
  mutate(linenumber = dplyr::row_number(),
         chapter = cumsum(stringr::str_detect(text, regex("^chapter [\\divxlc]",
                                                          ignore_case = TRUE)))) %>%
  ungroup() %>%
  unnest_tokens(word, text) # (dataframe (piped), output, input)

nrc_joyful <- nrc_df %>% filter(sentiment == "joy")

# Get all the joyful words from Jane Austen's Emma and output it in descending order.
original_austen %>%
  filter(book == "Emma") %>%
  inner_join(nrc_joyful) %>%
  count(word, sort = TRUE)

# See how the sentiment changes throughout Emma and the other novels of J Austen.
# Note: index = 0 means the book's lines between 0 to 79 (front part of book)
# fill = 0 : the word doesn't appear in the book
austen_books_sentiment <- original_austen %>%
  inner_join(bing_df) %>%
  count(book, index = linenumber %/% 80, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)
austen_books_sentiment

# Plot it
ggplot(austen_books_sentiment, aes(x = index, y=sentiment, fill=book))+
  geom_col(show.legend = TRUE)+
  facet_wrap(~book, ncol = 2, scales = "free_x")

# Lets zoom in on Pride and Prejudice (PnP)
pride_prejudice <- original_austen %>%
  filter(book == "Pride & Prejudice")
pride_prejudice

# Sum up PnP's affin scores in chunks of text (each chunk is 80 lines long)
pride_prejudice_affin_scores <- pride_prejudice %>%
  inner_join(afinn_df) %>%
  group_by(index = linenumber %/% 80) %>%
  summarise(sentiment = sum(value)) %>%
  mutate(method = "AFFIN")

head(pride_prejudice_affin_scores)

# Sum up PnP's NRC and sentiment scores as well
pride_prejudice_bing_nrc_scores <- bind_rows(
  pride_prejudice %>%
    inner_join(bing_df) %>%
    mutate(method = "Bing et al."), # no filter(): unlike NRC, Bing only has 2 sentiment types
  pride_prejudice %>%
    inner_join(nrc_df) %>%
    filter(sentiment %in% c("positive", "negative")) %>% # to remove other sentiments
    mutate(method = "NRC")) %>%
  count(method, index = linenumber %/% 80, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)

head(pride_prejudice_bing_nrc_scores)

# Lets see all the sentiment lexicon types (Bing, NRC, AFFIN) in a single plot,
# for Pride and Prejudice. 
# Bind all the scores of the different lexicons together
bind_rows(pride_prejudice_affin_scores, 
          pride_prejudice_bing_nrc_scores) %>%
  ggplot(aes(x=index, y=sentiment, fill = method)) +
  geom_col(show.legend=TRUE)+
  facet_wrap(~method, ncol = 1, scales = "free_y")

# NRC vs Bing lexicon comparison of number of positive/negative sentiment
nrc_df %>%
  filter(sentiment %in% c("positive", "negative")) %>%
  count(sentiment)
bing_df %>%
  filter(sentiment %in% c("positive", "negative")) %>%
  count(sentiment)

# Zoom in further: Which words contributed to positive/negative sentiments in 
# Jane Austen's books? Use only the Bing lexicon to examine.

austen_books_bing_word_counts <- original_austen %>%
  inner_join(bing_df, relationship = "many-to-many") %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()
austen_books_bing_word_counts

# Examine the warning messages.
# Warning message: Row 435434 of `x` matches multiple rows in `y`.
# Row 5051 of `y` matches multiple rows in `x`.
original_austen[435434,]
original_austen[original_austen$word == 'envious',]
# word 'envious' having both positive and negative sentiment.
austen_books_bing_word_counts[austen_books_bing_word_counts$word == "envious",]
# The word 'envious' appears twice only in all Jane Austen's books: ignore it.

# Plot it
austen_books_bing_word_counts %>%
  group_by(sentiment) %>%
  slice_max(n, n=10) %>%
  mutate(word = reorder(word,n)) %>%
  ggplot(aes(x = word, y = n, fill=sentiment))+
  geom_col(show.legend = TRUE)+
  facet_wrap(~sentiment, scales = "free_y")+
  labs(y="Contribution to sentiment", x = NULL)+
  coord_flip()

# tidytext vignette (not from book; Cran R Project website)
# https://cran.r-project.org/web/packages/tidytext/vignettes/tidytext.html
# Most common words (the stop words) in all the books in a whole
cleaned_books <- original_austen %>%
  anti_join(tidytext::get_stopwords())

cleaned_books %>% count(word, sort = TRUE)

# 'Miss' is coded negative in the Bing lexicon, but in Austen's books,
# Miss is used to as a title for young unmarried women.
custom_stop_words <- bind_rows(tibble(word = c("miss"), lexicon = c("custom")),
                               stop_words)
custom_stop_words

# Look at the most common words as a wordcloud.
cleaned_books %>%
  #anti_join(custom_stop_words) %>%
  count(word) %>%
  with(wordcloud(word, n, max.words=100))
# max.words: Maximum number of words to be plotted. least frequent terms dropped

# comparison.cloud
# Plot a cloud comparing the frequencies of words across documents
original_austen %>%
  inner_join(bing_df) %>%
  count(word,sentiment, sort = TRUE) %>% # creates data type : list
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%# creates data type: double
  comparison.cloud(colors = c("#F8766D", "#00BFC4"), max.words = 100,
                   fixed.asp=TRUE)
# See the book for description of the comparison cloud.
# Plot looks different. Save the plot to get the original plot appearance.
# Export > Save as pdf > Orientation: Portrait
# Thanks to users at stackoverflow

# Use acast or dcast depending on whether you want vector/matrix/array output or 
# data frame output. Data frames can have at most two dimensions.

# Instead of unigram where we analyse words,
# sentences can also be analysed
# prideprejudice is a dataset of sentences in janeaustenr package
PnP_sentences <- tibble(text = prideprejudice) %>%
  unnest_tokens(sentence, text, token = "sentences")
PnP_sentences$sentence[c(6:9)]
# But the tokenizing result is poor. 
# Sentences desired is not formed (had to choose 6:9, not a single number)

# austen_books() - 2 columns of a tibble (text, book)
# lets use a function that is already split for us into chunks of 70 characters each.
austen_chapters <- austen_books() %>%
  group_by(book) %>%
  unnest_tokens(chapter, text, token = "regex", 
                pattern = "Chapter|CHAPTER [\\dIVXLC]") %>%
  ungroup()
# unnnest_tokens(output, input)
# token argument - Unit for tokenizing, or a custom tokenizing function. 
# [character_group]. - https://hackr.io/blog/regex-cheat-sheet
# It will match any single character present in the character_group. B
# y default, the match is case-sensitive.
austen_chapters %>% 
  group_by(book) %>% 
  summarise(chapters = n())

# Interesting qn - What are the most negative chapters (according to Bing lexicon)
# in each of Jane Austen's novels?

# We will need the total number of negative words in each chapter,
# and total no. of all words in each chapter for calculating the ratio (line 259.

# remove positive words which is not asked in the question
bing_negative <- get_sentiments("bing") %>%
  filter(sentiment == "negative")
bing_negative

# save the wordcount per chapter into an object
wordcounts <- original_austen %>%
  group_by(book, chapter) %>%
  summarize(words = n())
wordcounts

original_austen %>% # x = original_austen, y = bing_negative
  semi_join(bing_negative) %>% # semi_join() return all rows from x with a match in y
  group_by(book, chapter) %>%
  summarize(negativewords = n()) %>%
  left_join(wordcounts, by =c("book", "chapter")) %>% # add the wordcount of each chapter
  mutate(ratio = negativewords/words) %>%
  filter(chapter!=0) %>% # There is no need to count negative words in chapter headings
  slice_max(ratio, n =1)
# - End of script