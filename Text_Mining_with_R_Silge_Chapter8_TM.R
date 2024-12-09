# Text Mining with R
# Julia Silge
# Topic Modelling (Chapter 8)
# 9 December 2024

# My computer settings
# MacBookAir M1, Memory 16GB, Mac OS Sonoma 14.5

# Review - 9 December 2024
# Remove stop words from the documents. Custom stop words may be added after doing EDA.
# Create a Document Term Matrix using cast_dtm(). The function comes from tidytext package. 

# Convert the corpus to an LDA model (using LDA())? 
# Test several values of k to find ideal k. 

# use tidy(probability = "beta") to investigate "mixture of words" within a document.

# use tidy(probability = "gamma") to investigate the probability of classifying the topic,
# based on the concept "mixture of topics" of a document.

# Visualise both probability distributions (beta, gamma) using ggplot() and identify
# the top differences (in abs probability values) that identify the document's main topic.

# -------------
library(tidytext)
library(jsonlite) 
library(tidyverse)
library(topicmodels)

# Analysis of metadata of NASA datasets. Latest metadata available in:
# NOTE - The data is vastly different from the printed book (2017) verses the current year.
url <- "https://data.nasa.gov/data.json"
metadata <- jsonlite::fromJSON(url)
names(metadata)
names(metadata$dataset)
nasa_desc <- tibble(id = metadata$dataset$identifier, desc=metadata$dataset$description)

tidy_nasa_desc <- nasa_desc %>%
  tidytext::unnest_tokens(word, desc) 

# The other stop words were from the earlier section of Chapter 8.
other_stop_words <- c(as.character(1:10), "v1.0", "67p", "l3", "l2", "v1", "v2.0", "v3.0")
# tidytext::stop_words is a list of common words from English that are unwanted in analysis
names(tidytext::stop_words) # colnames: word, lexicon
others <- tibble(word = other_stop_words, lexicon = "custom")
my_stop_words <- bind_rows(tidytext::stop_words, others)

further_tidy_nasa_desc <- tidy_nasa_desc %>%
  dplyr::anti_join(my_stop_words)
names(further_tidy_nasa_desc)

# save some memory
#rm(metadata, nasa_desc, tidy_nasa_desc)

word_counts <- further_tidy_nasa_desc %>%
  count(id, word, sort = TRUE)
word_counts %>% print(n=100)
# more stop words detected. lets add to the current list of stop words
more_stop_words <- tibble(word = c("nbsp", "amp", "gt", "lt", "file", "files", "font","td",
                                   "li", "br", "tr", "quot", "src", "strong", "http",
                                   "https", "c3", "img"), lexicon = "custom")
further_tidy_nasa_desc <- further_tidy_nasa_desc %>%
  dplyr::anti_join(more_stop_words)
# repeat lines 45-47
word_counts <- further_tidy_nasa_desc %>%
  count(id, word, sort = TRUE)
word_counts

# Lets make the document term matrix
nasa_desc_dtm <- word_counts %>%
  cast_dtm(id, word, n)
nasa_desc_dtm

# Commence LDA modelling. LDA stands for Latent Dirichlet allocation
# How many topics should we tell the algorithm to make?
# Try k = 24. In practice, different k values are tested.

# ###########
# PLEASE NOTE - The next line takes at least 15 minutes to execute completely.
# See my computer settings.
nasa_desc_LDA <- LDA(nasa_desc_dtm, k = 24, control = list(seed = 1234))

# Construct a tidy dataframe that summarises the results of the model.
tidy_LDA <- tidytext::tidy(nasa_desc_LDA) # default matrix = beta
tidy_LDA

# Examine each topic visually
top_terms <- tidy_LDA %>%
  group_by(topic) %>%
  slice_max(beta, n=10) %>%
  arrange(topic, -beta)
top_terms

top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  group_by(topic, term) %>%    
  arrange(desc(beta)) %>%  
  ggplot(aes(beta, term, fill = as.factor(topic))) +
  geom_col(show.legend = FALSE) +
  scale_y_reordered() +
  labs(title = "Top 10 terms in each LDA topic",
       x = expression(beta), y = NULL) +
  facet_wrap(~ topic, ncol = 4, scales = "free")
# Interpretation: 
# The word 'data' topped the word count in 17 of the 24 topics.

# Topic 12 includes words like "soil", "forest", and "biomass".
tidy_LDA_gamma <- tidytext::tidy(nasa_desc_LDA, matrix = "gamma") 
tidy_LDA_gamma

ggplot(tidy_LDA_gamma, aes(x=gamma))+
  geom_histogram() +
  scale_y_log10() +
  labs(title = "Distribution of probabilities for all topics",
       subtitle = "Topic modelling of NASA metadata description filed texts",
       y = "Number of documents", x = expression(gamma))
# Intepretation: The two ends of the histogram stand out. This means that at the 
# lowest gamma probability, many documents are not confidently assigned to any topic.
# At the opposite end (gamma = 1), there is a significant no. of documents that do 
# belong to a particular topic which is good news.

# We can also examine : how are probabilities being distributed within within each topic?
ggplot(tidy_LDA_gamma, aes(x=gamma, fill=as.factor(topic)))+
  geom_histogram(show.legend = FALSE)+
  facet_wrap(~topic, ncol=4)+
  scale_y_log10()+
  labs(title = "Distribution of probability for each topic (Total: 24)",
       y="Number of documents", x=expression(gamma))
# Intepretation: If the distribution seems flat going towards gamma = 1, 
# test out a lower k value with another model: LDA(k=new value). 

# Reiterate above code # remember to change the title and facet_wrap(ncol=_)
# and look at the distribution again. (eg k = 12: half of k=24)

nasa_desc_LDA_12 <- LDA(nasa_desc_dtm, k = 12, control = list(seed = 1234))
# This took at least 6 minutes to run.

tidy_LDA_12_gamma <- tidytext::tidy(nasa_desc_LDA_12, matrix = "gamma") 
tidy_LDA_12_gamma

ggplot(tidy_LDA_12_gamma, aes(x=gamma))+
  geom_histogram() +
  scale_y_log10() +
  labs(title = "Distribution of probabilities for all topics",
       subtitle = "Topic modelling of NASA metadata description filed texts",
       y = "Number of documents", x = expression(gamma))
# Intepretation: The two ends of the histogram stand out. This means that at the 
# lowest gamma probability, many documents are not confidently assigned to any topic.
# At the opposite end (gamma = 1), there is a significant no. of documents that do 
# belong to a particular topic which is good news.

ggplot(tidy_LDA_12_gamma, aes(x=gamma, fill=as.factor(topic)))+
  geom_histogram(show.legend = FALSE)+
  facet_wrap(~topic, ncol=4)+
  scale_y_log10()+
  labs(title = "Distribution of probability for each topic (Total: 12)",
       y="Number of documents", x=expression(gamma))

# Try k =5 # Reiterate above code # remember to change the title and facet_wrap(ncol=_)
nasa_desc_LDA_5 <- LDA(nasa_desc_dtm, k = 5, control = list(seed = 1234))
tidy_LDA_5_gamma <- tidytext::tidy(nasa_desc_LDA_5, matrix = "gamma") 
tidy_LDA_5_gamma

ggplot(tidy_LDA_5_gamma, aes(x=gamma))+
  geom_histogram() +
  scale_y_log10() +
  labs(title = "Distribution of probabilities for all topics",
       subtitle = "Topic modelling of NASA metadata description filed texts",
       y = "Number of documents", x = expression(gamma))

ggplot(tidy_LDA_5_gamma, aes(x=gamma, fill=as.factor(topic)))+
  geom_histogram(show.legend = FALSE)+
  facet_wrap(~topic, ncol=3)+
  scale_y_log10()+
  labs(title = "Distribution of probability for each topic (Total: 5)",
       y="Number of documents", x=expression(gamma))
# The distribution (towards gamma = 1) looks flat. 5 is not a good number.

# Final section: Connecting topic modelling with keywords
nasa_keyword<- tibble(id = metadata$dataset$identifier, keyword=metadata$dataset$keyword) %>% 
  unnest(keyword)

tidy_LDA_12_gamma <- full_join(tidy_LDA_12_gamma, nasa_keyword, by=c("document"= "id"),
                               relationship = "many-to-many")

top_keywords <- tidy_LDA_12_gamma %>%
  filter(gamma > 0.9) %>%
  count(topic, keyword, sort = TRUE)
top_keywords

# Top keywords for each LDA Topic (k=12)
top_keywords %>%
  group_by(topic) %>%
  slice_max(n, n= 5, with_ties = FALSE) %>%
  mutate(keyword = reorder_within(keyword, n, topic)) %>%
  ggplot(aes(x=keyword, y=n, fill=as.factor(topic)))+
  geom_col(show.legend=FALSE) +
  # remove the label "keyword" by x = NULL
  labs(title = "Top keywords for each LDA topic", y = "Number of documents", x= NULL)+
  coord_flip()+
  #gsub(pattern, replacement, x): the pattern replaces "___" with "". 
  # Regex pattern:
  # (__ finds this) (. and) (+ go to) ($ Find from the end of string line)
  scale_x_discrete(labels = function(x) gsub("__.+$", "", x))+ 
  scale_y_reordered()+ # Reorder an x or y axis within facets
  facet_wrap(~topic, ncol=3, scales = "free_y")
# https://www.digitalocean.com/community/tutorials/sub-and-gsub-function-r

# Earth science is featured in 7 of the topics.
# Atmosphere is featured in 6 of the topics.


