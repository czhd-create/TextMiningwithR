# Text Mining with R
# Julia Silge and David Robinson
# Chapter 8
# 5 Dec 2024
# ===================================
# Analysis of metadata of NASA datasets. Latest metadata available in:
url <- "https://data.nasa.gov/data.json"

# Load the libraries
library(jsonlite) # A simple and robust JSON parser and generator for R
library(tidyverse)
library(tidytext)
library(widyr) # A package for widening, processing then re-tidying data
library(topicmodels)
# For visualization through graphs, we load the following:
library(igraph)
library(ggraph)

# NOTE - The data is vastly different from the printed book (2017) verses the current year.
metadata <- jsonlite::fromJSON(url)
#Error in `[.data.frame`(x, start:min(NROW(x), start + len)) : 
#undefined columns selected

# But the code still feeds into object 'metadata' successfully
names(metadata)
names(metadata$dataset)

# To draw connections between datasets, lets explore the following variables:
# title, description, keyword

class(metadata$dataset$title)
# class: "character", character vector
class(metadata$dataset$description)
# class: "character"
class(metadata$dataset$keyword)
# class: list, list of character vectors

nasa_title <- tibble(id = metadata$dataset$identifier, title=metadata$dataset$title)

nasa_desc <- tibble(id = metadata$dataset$identifier, desc=metadata$dataset$description)

nasa_desc %>% select(desc) %>% sample_n(size=5)

# unnest(): expands a list-column containing data frames into rows and columns.
nasa_keyword<- tibble(id = metadata$dataset$identifier, keyword=metadata$dataset$keyword) %>% unnest(keyword)
nasa_keyword
# We get a tidy dataframe of one keyword per row.

tidy_nasa_title <- nasa_title %>%
  unnest_tokens(word, title) %>%
  anti_join(tidytext::stop_words)
tidy_nasa_title

tidy_nasa_desc <- nasa_desc %>%
  unnest_tokens(word, desc) %>%
  anti_join(tidytext::stop_words)
tidy_nasa_desc

# Exploratory Data Analysis
# Most common words in dataset titles
tidy_nasa_title %>% count(word, sort = TRUE)

# Most common words in dataset descriptions
tidy_nasa_desc %>% count(word, sort = TRUE)

# The words 'data' &'global' commonly describe the NASA datasets titles and descriptions.

# Custom stopwords to remove unwanted words from dataframe
my_stopwords <- tibble(word = c(as.character(1:10), "v1.0", "67p", "l3", "l2", "v1", "v2.0", "v3.0"))
# add to your custom stopwords when you find unwanted words during
# visualization via graphs (later code). re-run below code with updated custom stopwords.

further_tidy_nasa_title <- tidy_nasa_title %>%
  anti_join(my_stopwords)                  
further_tidy_nasa_title %>% count(word, sort = TRUE)

further_tidy_nasa_desc <- tidy_nasa_desc %>%
  anti_join(my_stopwords)                  
further_tidy_nasa_desc %>% count(word, sort = TRUE)

# Most common keywords
nasa_keyword %>% count(keyword, sort = TRUE) %>% print(n=50)
# Keywords are already in lowercase! (no need to change uppercases to lowercase)

# Next objective: Determine which datasets are related to each other
# Strategy: Find co-occuring words in the titles.
# make use of pairwise_count() from widyr package.
# upper = FALSE: excludes upper triangle, which may be duplicated
title_word_pairs <- further_tidy_nasa_title %>%
  pairwise_count(word, id, sort=TRUE, upper = FALSE)
title_word_pairs %>% print(n=50)
# some of these word pairs are acronyms that only NASA employees would know.
# eg: ges, rsi, rdr

desc_word_pairs <- further_tidy_nasa_desc %>%
  pairwise_count(word, id, sort=TRUE, upper = FALSE)
desc_word_pairs %>% print(n=50)
# The word 'data' is very common in description of the dataset.

# Plot the co-occuring words in the titles
set.seed(1234)
title_word_pairs %>%
  filter(n >= 200) %>%
  graph_from_data_frame() %>%
  # a layout is the vertical and horizontal placement of nodes when plotting a 
  # particular graph structure. 
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = n, edge_width = n), edge_colour = "cyan4")+
  geom_node_point(size=3) +
  geom_node_text(aes(label = name), repel = TRUE, point.padding = unit(.2, "lines"))+
  labs(title = "Word Network in NASA dataset titles")+
  theme_void()+
  theme(legend.position = "bottom")
# Intepretation: Two major and two minor clusters are observed.
# ?layout_tbl_graph_igraph: to know more about various layouts

# Plot the co-occuring words in the descriptions
set.seed(1234)
desc_word_pairs %>%
  filter(n >= 2000) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = n, edge_width = n), edge_colour = "darkred")+
  geom_node_point(size=3) +
  geom_node_text(aes(label = name), repel = TRUE, point.padding = unit(.2, "lines"))+
  labs(title = "Word Network in NASA dataset descriptions")+
  theme_void()+
  theme(legend.position = "bottom")
# Interpretation: No cluster is observed. Most words connect to 'data' followed by 'set'.

# Plot the co-occuring words in keywords
keyword_pairs <- nasa_keyword %>% pairwise_count(keyword, id, sort = TRUE, upper = FALSE) 
keyword_pairs

set.seed(1234)
keyword_pairs %>%
  filter(n >= 500) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = n, edge_width = n), edge_colour = "royalblue")+
  geom_node_point(size=2) +
  geom_node_text(aes(label=name), repel=TRUE, point.padding=unit(.2, "lines"), size =3)+
  labs(title = "Co-occurance Network in NASA dataset keywords")+
  theme_void()+
  theme(legend.position = "bottom")
# Intepretation: 
# Strong connections observed between 'earth science', 'oceans', 'atmosphere', 
# 'spectral/engineering' & 'land surface' 
# Also between 'international rosetta mission' & '67p/churyumov-gerasimenko 1 (1969 r1)'.

# Which keywords occur together with other keywords?
keyword_cors <- nasa_keyword %>%
  group_by(keyword) %>%
  filter(n() >= 50) %>%
  pairwise_cor(keyword, id, sort = TRUE, upper = FALSE)
keyword_cors

set.seed(1234)
keyword_cors %>%
  filter(correlation > .8) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr")+
  labs(title = "Correlation Network in NASA dataset keywords")+
  geom_edge_link(aes(edge_alpha = correlation, edge_width = correlation), edge_colour = "royalblue", width = .6)+ # width: controls the edge thickness
  geom_node_point(size=2) + # size: controls size of the node
  geom_node_text(aes(label = name), repel = TRUE, 
  check_overlap=TRUE, point.padding=unit(.2,"lines"), size=3)+ # size: controls text size
  theme_void()+
  theme(legend.position = "bottom")
# Intepretation: There are lots of small clusters. Each cluster shows which keywords 
# often occur together (correlation > 0.8).

# Codes of tf_idf section: Some are adapted from: https://www.tidytextmining.com/nasa
desc_tf_idf <- further_tidy_nasa_desc %>% 
  count(id, word, sort = TRUE) %>%
  bind_tf_idf(word, id, n)

desc_tf_idf %>% arrange(desc(tf_idf)) %>% select(-id) # Sort by highest tf-idf

desc_tf_idf <- full_join(desc_tf_idf, nasa_keyword, by="id", relationship="many-to-many")

# Find set of keywords with highest tf-idf with condition: tf < 0.5 
# Why we want tf < 0.5? So we do not analyse single-word documents -> not interesting
keywords_tf_idf_for_plot <- desc_tf_idf %>% 
  filter(!near(tf, 1)) %>% arrange(desc(tf)) %>% select(keyword)
keywords_tf_idf_for_plot_top_6 <- unique(keywords_tf_idf_for_plot$keyword)[1:6]

desc_tf_idf %>% 
  filter(keyword %in% keywords_tf_idf_for_plot_top_6) %>%
  arrange(desc(tf_idf)) %>%
  group_by(keyword) %>%
  distinct(word, keyword, .keep_all = TRUE) %>%
  slice_max(tf_idf, n = 15, with_ties = FALSE) %>% 
  ungroup() %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>%
  ggplot(aes(x=tf_idf, y=word, fill = keyword)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~keyword, ncol = 3, scales = "free") +
  labs(title = "Highest tf-idf words in NASA metadata description fields",
       caption = "NASA metadata from https://data.nasa.gov/data.json",
       x = "tf-idf", y = NULL)

# Most of the datasets in the plot have the words: lt, gt. Some of them had: amp
# Some of the highest tf-idf words are acronyms (e.g. fife, lt, gt, amp).

# The rest of the chapter on topic modeling will be covered in a separate script.