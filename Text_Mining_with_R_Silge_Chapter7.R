# Text Mining with R
# Julia Silge and David Robinson
# Chapter 7
# 4 Dec 2024
# ===================================
# This Chapter is about 1 case study of the authors' Twitter data.
# To keep information up to date. As of 2023, twitter is known as X.
# The case study will be divided into 7 subsections:
# I, II, III, IV, V, VI, VII
# ===================================

# IMPT NOTE on working directory: 
# Subsection I to V:
# setwd() to the directory containing the 2 csv datasets of the authors' tweets,
# and from subsection VI: 2 datasets containing likes & retweets.
# ===================================
# Load the relevant libraries
library(tidyverse)
library(tidytext)
library(scales)
library(broom)

# ===================================
# Subsection I - Getting the data and Distribution of Tweets
# Load the datasets from the working directory
tweets_julia <- read_csv("tweets_julia.csv")
colnames(tweets_julia)
tweets_dave <- read_csv("tweets_dave.csv")
colnames(tweets_dave)

# bind_rows()
# Use this function: The number of columns are equal and column names suggest
#  that the variables are of the same index position in each dataset
tweets <- bind_rows(tweets_julia %>%
                      mutate(person = "Julia"), 
                    # adds a "person" variable to describe its source
                    tweets_dave %>%
                      mutate(person = "David")) %>%
  mutate(timestamp = lubridate::ymd_hms(timestamp))

# ymd_hms()
# Parse date-times with year, month, day, hour, minute, and seconds components.

ggplot(tweets, aes(x = timestamp, fill = person))+
  geom_histogram(position = "identity", bins =20, show.legend = FALSE)+
  facet_wrap(~person, ncol = 1)
# David was more active on twitter from 2015 onwards while Julia had been active 
# since 2008.

# ===================================
# Subsection II - Word Frequencies

counts <- tweets %>% group_by(person) %>% count()
counts
as.numeric(counts[counts$person =='Julia',][2])/as.numeric(counts[counts$person =='David',][2])
# Julia has at least 3 times more tweets than David in their twitter lives

# this regex pattern removes links and cleans out unwanted characters.
replace_reg <- "https://t.co/[A-Za-z\\d]+|http://[A-Za-z\\d]+|&amp;|&lt;|&gt;|RT|https"

# this regex pattern retains hashtags (#) and mentions of usernames (@ symbol).
unnest_reg <- "([^A-Za-z_\\d#@']|'(?![A-Za-z_\\d#@]))"

# Get the tweets into a tidy dataframe. Make use of the above 2 regex functions.
tidy_tweets <- tweets %>% 
  filter(!str_detect(text, "^RT")) %>% # removes retweets (text starts with 'RT')
  mutate(text = str_replace_all(text, replace_reg, "")) %>%
  unnest_tokens(word, text, token = "regex", pattern = unnest_reg) %>%
  filter(!word %in% tidytext::stop_words$word,
         #!word %in% str_remove_all(tidytext::stop_words$word, "'"),
         str_detect(word, "[a-z]")) # detects any words with at least 1 alphabet

head(tidy_tweets)

# Determine the word frequencies of each person
frequency <- tidy_tweets %>%
  # count() creates 'n' variable - count of words in all of each person's tweets
  count(person, word, sort = TRUE) %>% 
  # creates 'total' variable - the sum of all words that each person tweeted
  left_join(tidy_tweets %>% count(person, name = "total")) %>% 
  mutate(freq = n/total) # creates new variable, freq, using 'n' and 'total'
frequency

# Creates another df that shows how frequent each word appears for each person.
frequency_2 <- frequency %>%
  select(person, word, freq) %>%
  pivot_wider(names_from = person, values_from = freq) %>% 
  # creates two columns (person=Julia, person=Dave) using argument, names_from
  arrange(Julia, David) # order the rows using column values
frequency_2

# ===================================
# Subsection III - Plot to compare Word Frequencies of 2 persons
# Plot of frequency_2
ggplot(frequency_2, aes(x = Julia, y = David))+
  geom_jitter(alpha = .1, size = 2.5, width = .25, height = .25) +
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
  scale_x_log10(labels = percent_format())+
  scale_y_log10(labels = percent_format())+
  geom_abline(color = "red") # draws y = x
# Interpretation:
# Words far away from the line are used much more by one person compared
# to the other. Julia uses twitter more for personal reasons 
# (baby, home, dinner). David uses more for professional reasons (broom, @hspter).
# @hspter is Hilary Parker, a Data Science consultant and Formerly startup CTO.

# ===================================
# Subsection IV - Comparing word usage using odd ratios on a plot
# Zoom in on a calendar year of tweets (year 2016)
tidy_tweets_2016 <- tidy_tweets %>%
  filter(timestamp >= as.Date("2016-01-01"),
         timestamp < as.Date("2017-01-01"))

word_ratios <- tidy_tweets_2016 %>%
  filter(!str_detect(word, "^@")) %>% # take away usernames (word starts with @)
  count(word,person) %>%
  group_by(word) %>%
  # filter and keep only those words from Dave and Julia which total count>= 10
  # eg word=='2nd': 5 + 7 >= 10
  filter(sum(n) >= 10) %>% 
  ungroup() %>%
  # creates columns "David" and "Julia" using n values, let missing values = 0
  pivot_wider(names_from = person, values_from = n, values_fill = 0) %>%
  # applies the log odds ratio formula to the columns "David" and "Julia"
  mutate_if(is.numeric, list(~(. + 1) / sum(. + 1))) %>% 
  mutate(logratio = log(David/Julia)) %>%
  arrange(desc(logratio))

word_ratios %>% arrange(abs(logratio))
# Interpretation:
# Since log ratio ~ 0, David and Julia are equally likely to tweet 
# about emails, files and maps.

# Which words are most likely from each author's account?
# Hint: Examine only the top |log ratios|.
word_ratios %>%
  group_by(logratio < 0) %>% 
  # why < 0? Group by which account is more likely: Julia's (<0) or David's (>0)
  # Plot only the biggest 15 absolute log values per person
  slice_max(abs(logratio), n=15) %>%
  ungroup() %>%
  mutate(word = reorder(word, logratio)) %>% 
  ggplot(aes(x = word, y = logratio, fill = logratio < 0)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  ylab("log odds ratio: Julia (left), David (right)") +
  # uses discrete color scales
  scale_fill_discrete(name = "", labels = c("David", "Julia")) +
  labs(title = "Comparing odds ratios of 15 most likely words in the tweets")
# Interpretation:
# David tweets about conferences he had been to in year 2016,
# while Julia tweets about utah, and her family (school, husband, home).

# ===================================
# Subsection V - Comparing word usage across words over time
words_by_time <- tidy_tweets_2016 %>%
  # Take out usernames from analysis
  filter(!str_detect(word, "^@")) %>%
  # Define the time bins to 1-month frames (1 month per unit)
  mutate(time_floor = lubridate::floor_date(timestamp, unit = "1 month")) %>%
  # Count the number of times each person used the words in each unit of time
  count(time_floor, person, word) %>%
  group_by(person, time_floor) %>%
  # time_total: count the total number of words used per person per unit of time
  # If a word is used more than once (n>1), it is included in time_total
  mutate(time_total = sum(n)) %>%
  group_by(word) %>%
  # word_total: total number of times the word is used for yr 2016 (all persons)
  mutate(word_total = sum(n)) %>%
  ungroup() %>%
  rename(count = n) %>%
  filter(word_total > 30)

# Each word can create a list of mini dataframes (call it 'data' variable)
nested_data <- words_by_time %>%
  nest(data = c(-word, -person))
nested_data

nested_data[1,]
# David uses the word '#rstats' in every month of the year 2016 (12 x 4)
nested_data$data[1] 
# 'data' column is a column (datatype = list) that contains a single dataframe
# If the word is used by a person that month (time_floor), a row is created.
# count: total no. of times the word appeared in the user's tweet (single month)
# time_total: Total number of words used in a single month
# word_total: Total no. of times a specific word is used by all persons 
#             across all times.

# Include the variable word in the nested data - for tracking purpose.
nested_data2 <- words_by_time %>%
  nest(data = c(-person))

# David - track the use of word "#user2016' across time.
nested_data2$data[[1]] %>% filter(word=='#user2016')

# Julia - track the use of word "#rstats' across time.
nested_data2$data[[2]] %>% filter(word=='#rstats')

# Apply a model to each of the nested dataframes in nested_data.
# This help in tracking the word - how often is this word mentioned across time?
nested_models <- nested_data %>%
  mutate(models = map(data, ~ glm(cbind(count, time_total) ~ time_floor, ., 
                                  family = "binomial")))
nested_models
# models variable -A new column of glm objects is formed.

glm_slopes <- nested_models %>%
  mutate(models = map(models, tidy)) %>%
  unnest(cols = c(models)) %>%
  filter(term == "time_floor") %>%
  mutate(adjusted.p.value = p.adjust(p.value))
# p.adjust()
# Given a set of p-values, returns p-values adjusted using one of several methods.

# Find the most important slopes. Which words changed frequency at a moderately
# significant level?
top_slopes2 <- glm_slopes %>% 
  filter(adjusted.p.value < 0.1)

# Visualise the results (David's tweets)
# Recall 
# time_total: counts the total number of words used per person per unit of time
words_by_time %>%
  inner_join(top_slopes2, by = c("word", "person")) %>%
  filter(person == "David") %>%
  ggplot(aes(x=time_floor, y=(count/time_total), color = word)) +
  geom_line(linewidth = 1.3) +
  labs(x = NULL, y = "Word frequency", title = "Trending words in David's tweets")
# Interpretation:
# David tweeted a lot about R user conference in June 2016 before stopping suddenly.
# David tweeted about stack overflow more as the year approaches the end.
# David tweeted about ggplot2 in the start of 2016 but it dropped afterwards.

# Counter check
nested_data2$data[[1]] %>% filter(word=='#user2016')

# Visualise the results (Julia's tweets)
words_by_time %>%
  inner_join(top_slopes2, by = c("word", "person")) %>%
  filter(person == "Julia") %>%
  ggplot(aes(x=time_floor, y=(count/time_total), color = word)) +
  geom_line(linewidth = 1.3) +
  labs(x = NULL, y = "Word frequency", title = "Trending words in Julia's tweets")
# Julia's tweeted more during the beginning of the year, but her total tweets
# dropped significantly in the second half of the year.

# Counter-check
nested_data2$data[[2]] %>% filter(word=='#rstats')

# ===================================
# Subsection VI - Examine which word has the highest retweets for each author
tweets_julia <- read_csv("juliasilge_tweets.csv")
tweets_david <- read_csv("drob_tweets.csv")

colnames(tweets_julia)
colnames(tweets_david)
# Each dataframe consists of raw text of the tweet, its timestamp (created_at)
# and no. of retweets (aka reposts in X) and favourites (aka likes in X)
# X is the new name of twitter since 2023.

# Bind the two datasets together
tweets <- bind_rows(tweets_julia %>% 
                      mutate(person = "Julia"),
                    tweets_david %>% 
                      mutate(person = "David")) %>%
  mutate(created_at = ymd_hms(created_at))

tidy_tweets <- tweets %>% 
  # removes author's retweets (RT) OR removes author's replies to other users(@)
  filter(!str_detect(text, "^(RT|@)")) %>%
  mutate(text = str_replace_all(text, replace_reg, "")) %>%
  unnest_tokens(word, text, token = "regex", pattern = unnest_reg) %>%
  filter(!word %in% stop_words$word,
         !word %in% str_remove_all(stop_words$word, "'")) # ?

tidy_tweets

# Add all retweets (id level), then add all retweets (person level)
totals <- tidy_tweets %>% 
  group_by(person, id) %>% 
  # first()
  # first index of 'retweets' vector. At id level, all words have same value of retweets
  summarise(rts = first(retweets)) %>% 
  group_by(person) %>% 
  summarise(total_rts = sum(rts))

# Find the total number of retweets per person
totals
# David's tweets are retweeted much more than Julia's tweets.

# Determine the median number of retweets for each word and person.
word_by_rts <- tidy_tweets %>% 
  group_by(id, word, person) %>% 
  summarise(rts = first(retweets)) %>% 
  group_by(person, word) %>% 
  summarise(retweets = median(rts), uses = n())

word_by_rts %>% 
  filter(uses >= 5) %>%
  arrange(desc(retweets))
# The authors retweeted about 'tidytext' and 'gganimate' packages.

word_by_rts %>%
  filter(uses >= 5) %>%
  group_by(person) %>%
  slice_max(retweets, n = 10) %>% 
  arrange(retweets) %>%
  mutate(word = factor(word, unique(word))) %>%
  ggplot(aes(x=word, y=retweets, fill = person)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ person, scales = "free", ncol = 2) +
  coord_flip() +
  labs(x = NULL, 
       y = "Median # of retweets for tweets containing each word")
# The “0” for David comes from tweets 
# where he mentions version numbers of packages, like “broom 0.4.0” or similar.

# ===================================
# Subsection VII - Examine which word has the highest favourites for each author
totals_favs <- tidy_tweets %>% 
  group_by(person, id) %>% 
  # first()
  # first index of 'favourites' vector. At id level, all words have same no. of favourites
  summarise(favs = first(favorites)) %>% 
  group_by(person) %>% 
  summarise(total_favs = sum(favs))
totals_favs
# David's tweets are favored about 3.38 times more than Julia's tweets.

word_by_favs <- tidy_tweets %>% 
  group_by(id, word, person) %>% 
  summarise(favs = first(favorites)) %>% 
  group_by(person, word) %>% 
  summarise(favorites = median(favs), uses = n())

word_by_favs %>%
  filter(uses >= 5) %>%
  group_by(person) %>%
  slice_max(favorites, n = 10) %>% 
  arrange(favorites) %>%
  mutate(word = factor(word, unique(word))) %>%
  ggplot(aes(x=word, y=favorites, fill = person)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ person, scales = "free", ncol = 2) +
  coord_flip() +
  labs(x = NULL, 
       y = "Median # of favorites for tweets containing each word")
# Interpretation: 
# In general, words like 'animation', 'tidytext', 'gganimate', 'janeaustinr' 
# appear in top retweets and top favourites.
# End of script