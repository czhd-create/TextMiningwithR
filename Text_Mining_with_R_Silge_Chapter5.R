# Text Mining with R
# Julia Silge and David Robinson
# 15 November 2024
# Chapter 5 

#------- Please read lines 7 to 21 before proceeding ---------#
# tm.plugin.webmining is an R-package which facilitates text retrieval 
# from real-time online feed formats like XML (RSS, ATOM) and JSON.
# The installation is very technical and requires some sophisticated skills.
# Mac OS with ARM architecture: An ARM-compatible Java version is needed.

# The abovementioned R package will impact one section of the book.
# Thankfully, the authors saved the dataset during the authoring of the book
# Hence the rest of that section can only be followed by using the dataset 
# saved in the author, D. Robinson's github page:
# https://github.com/dgrtwo/tidy-text-mining/tree/master/data
# In particular, look for and download this file into your local directory: 
# "stock_articles.rda"

# Set working directory to where your downloaded file is. Run the next line:
#setwd("/your_directory")
#-------

library(tm)
library(dplyr)
library(tidytext)
library(ggplot2)
library(methods)
library(tidyr)
library(Matrix)
library(janeaustenr)
library(purrr)
library(stringr)
library(readr)
library(topicmodels)
library(quanteda)
library(scales)

# Many text mining applications use non-tidy data formats.
# In this chapter, learn how to connect tidy tools to "non-tidy" R packages 
# such as tm (text mining) & quanteda (quantitative analysis of textual data).

# Refer to the book for a flowchart of a typical text analysis (Fig 5.1)

# load a set of AP newspaper articles into our R environment
data("AssociatedPress", package = "topicmodels")

AssociatedPress
# This object is a Document Term matrix (DTM) of 2246 documents and 10k+ terms.

# ?Terms() A character vector of terms in the DTM
terms <- Terms(AssociatedPress)
head(terms)
length(terms)

# creates a tidy tibble from the document term matrix
ap_td <- tidytext::tidy(AssociatedPress)
ap_td %>% arrange(by = count)

# There is no count= 0 in ap_td

# Use Bing sentiment lexicon to get sentiment of the words (Positive/Negative)
bing_lexicon <- tidytext::get_sentiments("bing")
colnames(bing_lexicon)


ap_sentiments <- ap_td %>%
  inner_join(bing_lexicon, by=c(term = "word"))# term(ap_td), word(bing_lexicon)
ap_sentiments %>% arrange(by = desc(count)) %>% print(n=10)
# 'fair' is a common positive term in document 869 and document 1482

ap_sentiments %>%
  count(sentiment, term, wt = count) %>%
  # wt: frequency weights, wt = count: sums up the count of 
  #each term-sentiment combination
  ungroup() %>%
  filter (n>= 200) %>% # Select only most common words to plot
  mutate(n = ifelse(sentiment == 'negative', -n, n)) %>% # -n: for plotting purpose
  mutate(term = reorder(term, n))%>% # the value of n sets the order of the term
  ggplot(aes(x = term, y = n, fill = sentiment)) +
  geom_bar(stat="identity", show.legend = NULL)+
  labs(title = "Words from AP news articles & its contribution to +/- sentiment", 
       x = "term", y="Contribution to sentiment")+
  coord_flip()

# lets examine another kind of matrix: the document-feature matrix (dfm)
# load a set of president inauguration speeches into our R environment
data("data_corpus_inaugural", package = "quanteda")
inaug_dfm <- data_corpus_inaugural %>%
  quanteda::tokens() %>%
  quanteda::dfm(verbose = FALSE)
# dfm object made

# Unpack inaug_dfm with tidy()
inaug_speeches_tidy_df <- tidy(inaug_dfm)
head(inaug_speeches_tidy_df)

# Calculate td-idf of each term-speech combination
inaug_speeches_td_idf <-inaug_speeches_tidy_df %>%
  bind_tf_idf(term, document, count)
inaug_speeches_td_idf %>% arrange(desc(tf_idf))

# Examine the speeches by Presidents Lincoln, Roosevelt, Kennedy, Obama
smaller_set_president_speeches <- inaug_speeches_td_idf %>%
  filter (document == "1861-Lincoln" | 
            document == "1933-Roosevelt" |
            document == "1961-Kennedy" |
            document == "2009-Obama") 
smaller_set_president_speeches

smaller_set_president_speeches %>%
  group_by(document) %>%
  slice_max(tf_idf, n = 10) %>%
  ungroup() %>%
  mutate(term = reorder(term, tf_idf)) %>%
  ggplot(aes(x = term, y = tf_idf, fill = document))+
  geom_col(show.legend = FALSE)+
  labs(title = "Terms with Highest tf-idf (4 Great US Presidents)",
       x = NULL, y = "tf-idf")+
  facet_wrap(~document, ncol = 2, scales = "free")+
  coord_flip()

# Lets do another plot that shows the changes in the word frequency over time

# tidyr::extract()
# (Chat GPT) The extract() function can be used to extract parts of strings, 
# typically when working with tidyr or dplyr

# tidyr::complete()
# Turns implicit missing values into explicit missing values, which is
# useful for completing missing combinations of data.
# fill = list	: A named list that for each variable supplies a single value 
# to use instead of NA for missing combinations.
# count = 0 : if that year-term combination has no count, let count = 0

year_term_counts <- inaug_speeches_tidy_df %>%
  extract(document, into = "year", regex = "(\\d+)") %>%
  complete(year, term, fill = list(count = 0)) %>%
  group_by(year) %>%
  mutate(year_total = sum(count))

# ChatGPT: 
# Please explain what this means in R language's function extract regex = "(\\d+)"?
# Answer: the regex pattern "(\\d+)" is used inside the extract() function 
# to match and extract one or more digits from a string column.

# lets pick these six words to show how they change in frequency over time.
list_words <- c("god", "america", "foreign", "union", "constitution", "freedom")
year_term_counts %>%
  filter(term %in% list_words) %>%
  mutate(perc_frequency = count/year_total) %>%
  ggplot(aes(x = as.numeric(year), y=perc_frequency))+
  geom_point() +
  geom_smooth(method = "loess") +
  facet_wrap(~term, scales = "free_y")+
  scale_y_continuous(labels = scales::percent_format()) +
  scale_x_continuous(breaks = c(1800, 1850, 1900, 1950, 2000))+
  xlab("Year")+
  ylab("% Frequency of word in inaugural address")

# Convert data from tidy form to document term matrix
ap_td %>%
  tidytext::cast_dtm(document, term, count)

# Convert data from tidy form to document feature matrix
ap_td %>%
  tidytext::cast_dfm(document, term, count)

# Cast data from tidy form to a sparse matrix
m <- ap_td %>% cast_sparse(document, term, count)
class(m)
dim(m)

# Create a document term matrix on Jane Austen's novels
austen_dtm <- austen_books() %>%
  unnest_tokens(word, text) %>%
  count(book, word) %>%
  cast_dtm(book, word, n)
austen_dtm

# Tidying a corpus
data("acq") # acq corpus from tm package (has 50 articles from Reuters)

# first Reuters document
acq[[1]]

# Convert the corpus to a tidy() format
acq_td <- tidy(acq)
acq_td

colnames(acq_td)

# Find the most common words in the Reuters corpus and their tf-idf
acq_tokens <- acq_td %>%
  unnest_tokens(word, text) %>% #recall ?unnest_tokens -> (df, output, input)
  anti_join(tidytext::stop_words, by = "word")

acq_tokens %>% count(word, sort = TRUE)

unique(acq_tokens$id) # The id number of the 50 Reuters articles
length(unique(acq_tokens$id))

# tf-idf
acq_tokens_tf_idf <- acq_tokens %>%
  count(id, word) %>%
  bind_tf_idf(word, id, n)

colnames(acq_tokens_tf_idf)
  
acq_tokens_tf_idf %>%
  arrange(desc(tf_idf))

# Example on Mining Financial articles 
load("stock_articles.rda")
colnames(stock_articles)

# Create one row per word per article
stock_tokens_words <- stock_articles %>%
  mutate(corpus = map(corpus, tidy)) %>%  
  unnest(corpus) %>% # 20 articles per company (total = 180 articles)
  # map(): Applies a function to each element of a vector
  unnest_tokens(word, text)

# Which words were most specific to each company's articles? Use tf-idf
stock_tokens_words_tf_idf <- stock_tokens_words %>%
  count(company, word) %>%
  filter(!str_detect(word, "\\d+")) %>% #\\d+ (regex pattern): exclude numbers 
  bind_tf_idf(word, company, n) %>%
  arrange(-tf_idf)

stock_tokens_words_tf_idf %>% 
  group_by(company) %>% 
  slice_max(tf_idf, n = 10) %>% 
  ungroup() %>% 
  mutate(word = reorder(word, tf_idf)) %>% 
  ggplot(mapping = aes(x = word, y = tf_idf)) + 
  geom_bar(stat = "identity", fill = 'green') + 
  xlab("Word") + 
  ylab("tf-idf") +
  facet_wrap(~company, ncol = 3, scales = "free") + 
  coord_flip() 

# Use AFINN lexicon to determine the positive/negative sentiments of the words
afinn_df <- tidytext::get_sentiments(lexicon = "afinn")
head(afinn_df)

stock_tokens_words %>%
  count(word, id, sort = TRUE) %>%
  anti_join(tidytext::stop_words, by = "word") %>% # removes stop words in articles
  inner_join(afinn_df, by="word") %>%# get sentiment scores from AFINN dataframe
  group_by(word) %>%
  summarize(contribution = sum(n * value)) %>%
  slice_max(abs(contribution), n = 12) %>%
  mutate(word = reorder(word, contribution)) %>%
  ggplot(aes(x=word, y=contribution))+
  geom_col()+
  coord_flip()+
  labs(y = "Contribution = Frequency * AFINN Score")

# Lets use Loughran lexicon to get a different plot 
# Loughran is a lexicon built for financial sentiment terms.
loughran_df <- tidytext::get_sentiments(lexicon = "loughran")
head(loughran_df)

stock_tokens_words %>%
  count(word, sort = TRUE) %>%
  #anti_join(tidytext::stop_words, by = "word") %>% # removes stop words in articles
  inner_join(loughran_df, by="word", relationship = "many-to-many") %>%
  group_by(sentiment) %>% # don't group_by(word) ! which will yield lots of words
  slice_max(n, n = 5) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x=word, y=n))+
  geom_col()+
  xlab("Word") + 
  ylab("Frequency of the word")+
  labs(title = "The most common words associated with Loughran sentiment words",
       subtitle = "ref: Tim Loughran and Bill McDonald 2011")+
  facet_wrap(~sentiment, nrow = 3, ncol = 3, scales = "free")+
  coord_flip()

#Note: furthermore, considered "superfluous" in Loughran lexicon, is a stop word

# Number of words used in each company that is associated with Loughran lexicon
stock_sentiment_count <- stock_tokens_words %>%
  inner_join(loughran_df, by="word", relationship = "many-to-many") %>%
  count(sentiment, company)

sentiment_company <- stock_sentiment_count %>% spread(sentiment, n, fill = 0)

# Plot it
sentiment_company %>% 
  mutate(score = (positive - negative) / (negative + positive)) %>%
  mutate(company = reorder(company, score)) %>%
  ggplot(aes(x=company, y = score, fill = score >0))+
  geom_col(show.legend = FALSE) +
  coord_flip()+
  labs(x = "Company",
       y = "Positive score among 20 news articles of each company")

# Google is the most positive company. In contrast, Twitter is most negative.
