# Text Mining with R by Julia Silge, David Robinson
# Chapter 1
# 07 Nov 2024

# Code tested on
# RStudio 2023.12.1 Build 402
# "Ocean Storm" Release (4da58325, 2024-01-29) for macOS
# R version 4.3.2 (2023-10-31) -- "Eye Holes"

# Please ensure these libraries are installed
library(tidytext)
library(tibble)
library(dplyr)
library(stringr)
library(tidyr)
library(janeaustenr)
library(gutenbergr)
library(ggplot2)
library(readr)
library(scales)

try_text <- c("Because I could not stop for Death -",
          "He kindly stopped for me",
          "The Carriage held but just Ourselves -",
          "and Immortality")

text_df <- tibble::tibble(line = 1:4, text = try_text)  

text_df %>% 
  tidytext::unnest_tokens(word, text) # word is the column name of parsed words
tibble::glimpse(text_df)
# line is the line number that the word came from

# Tidy Jane Austen's literature
# Jane Austen published 6  novels during her lifetime.
# Julia Silge has made the text of all 6 books in a function!
see_see = austen_books()
colnames(see_see)
# The dataset has only two columns: actual text of the book, book title

# Next, we add line number and chapter into the dataset and put the data 
# (novel 'SENSE AND SENSIBILITY' by Jane Austen) into a tibble.
# cumsum(): Returns a vector whose elements are the cumulative sums, products, 
# minima or maxima of the elements of the argument.
# str_detect(): Detect the presence/absence of a match
# regex(): one of the modifier functions in stringr package. 

# regex() uses ICU regular expressions. 
# ICU: International Components for Unicode 
# ICU’s Regular Expressions package provides applications with the ability to 
# apply regular expression matching to Unicode string data. The regular 
# expression patterns and behavior are based on Perl’s regular expressions.

# Should case differences be ignored in the match? 
# For fixed(), this uses a simple algorithm which assumes a 
# one-to-one mapping between upper and lower case letters.

original_austen <- janeaustenr::austen_books() %>%
  group_by(book) %>%
  mutate(linenumber = dplyr::row_number(),
         chapter = cumsum(stringr::str_detect(text, regex("^chapter [\\divlc]",
                                                 ignore_case = TRUE)))) %>%
  ungroup()
# https://github.com/juliasilge/juliasilge.com/issues/98
# Silge Jan 2024 "(the regex expression) That is to identify roman numerals, 
# like to find "chapter iv". It doesn't look like it's necessary here, 
# but can be good for other datasets. 

# After putting in the linenumber and chapter column, lets resturcture 
# it into a one-token-per-row (one word per row) format.

#view(original_austen)
colnames(original_austen)

# Split 'text' column into tokens, flattening the table into one-token-per-row. 
austen_tidy_format <- tidytext::unnest_tokens(original_austen, word, text,
                                              token = "words")

#view(austen_tidy_format)
# Since each row describes one word of the original text, austen_tidy_format
# would have many more rows in it.

# Remove stop words. 
# Stop words are common words in English which are unuseful for analysis
# There are 1149 identified stop words saved in tidytext package
# load these into R environment using data()
data(stop_words) 
# notice the data type: promise!

# anti_join(x, y) return all rows from x without a match in y
austen_tidy_format_stop_words_removed <- 
  anti_join(austen_tidy_format, stop_words)

# The above action removed 70% of text from the dataset.
# Wow, Jane Austen uses a lot of stop words in her literature.

# After removing stop words, find all commmon words in the literature
# sort the words in descending order before presenting in the output
austen_tidy_format_stop_words_removed %>%
  dplyr::count(word, sort=TRUE)

# to treat word as a factor. (possibly) Useful for reordering later in mutate()
austen_tidy_format_stop_words_removed$word <- factor(austen_tidy_format_stop_words_removed$word)

# You many now do the first visulisation of this exercise.
austen_tidy_format_stop_words_removed %>%
  count(word, sort=TRUE) %>% # count() creates "n" column (values sorted but not saved)
  filter(n>600) %>% # display only words with >600 word count
  mutate(word = reorder(word, n)) %>%
  # reorders x=word by X=n (ref: ?reorder())
  ggplot(aes(x=word, y=n)) +
  geom_col() + # heights of the bars represent y-values (n) in the data
  xlab(NULL) + # removes "word" label from x-axis
  coord_flip()+ # flips the x-axis to y-axis and vice-versa
  labs(title="Top Word frequencies of Jane Austen's novels")

# page 7 - resume with gutenburgR package
# Project Gutenberg is an online library of over 70,000 old (but free!) eBooks 
# Some of Jan Austen's books can be found in this website!
# https://www.gutenberg.org/

# Use book ID to download
hgwells <- gutenberg_download(c(35,36,5230,159)) # 159 doesn't work. 
# The book may have been archived. Manually download from website. (Line 120)
# or download from: https://www.gutenberg.org/cache/epub/159/pg159.txt
# remember to remove meta data before choosing file
moreau_159 <- read_file(file.choose())
# With help from chat GPT: 
# 03 Prompts:"i want to split a text into lines using R. Can I show you some of the code I am using?" .... "moreau_159 <- read_file(file.choose())"...
# if the text contains "\r" as well as "\n", how do i split it?
lines <- strsplit(moreau_159, "\r?\n")[[1]]
lines_for_merge <- tibble(gutenberg_id = 159, text=lines)
hgwells2 <- bind_rows(hgwells, lines_for_merge)

hgwells_tidy_format_stop_words_removed <- hgwells2 %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)

# most common words in HG Wells novels
hgwells_tidy_format_stop_words_removed %>%
  count(word, sort = TRUE)

# Turn our attention to the Bronte Sisters 
bronte <- gutenberg_download(c(1260, 768, 969, 9182, 767))

bronte_tidy_format_stop_words_removed <- bronte %>%
  unnest_tokens(word,text) %>%
  anti_join(stop_words)

# most common words in Bronte sisters novels
bronte_tidy_format_stop_words_removed %>%
  count(word, sort = TRUE)

# bind the three authors works together to calculate its combined frequency
frequency <- bind_rows(mutate(bronte_tidy_format_stop_words_removed,
                              author = "Bronte Sisters"),
                       mutate(hgwells_tidy_format_stop_words_removed,
                              author = "Herbert George Wells"),
                       mutate(austen_tidy_format_stop_words_removed,
                              author = "Jane Austen"))

# to learn more about regex: regexlearn.com
frequency <- frequency %>% mutate(word = str_extract(word, "[a-z']+")) %>%
  count(author, word) %>%
  group_by(author) %>%
  mutate(proportion = n/sum(n)) %>%
  select(-n) %>%
  spread(author, proportion) %>%
  # gather columns into key-value pairs
  gather(author, proportion, `Bronte Sisters`:`Herbert George Wells`)

ggplot(frequency, aes(x=proportion, y = `Jane Austen`,
                      color = abs(`Jane Austen` - proportion))) +
  geom_abline(color="gray40", lty = 2) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.3, height = 0.3) +
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
  scale_x_log10(labels = percent_format())+
  scale_y_log10(labels = percent_format())+
  scale_color_gradient(limits = c(0,0.001),
                       low = "darkslategray4", high = "gray75")+
  facet_wrap(~author, ncol=2)+
  theme(legend.position="none")+
  labs(y = "Jane Austen", x = NULL)

cor.test(data = frequency[frequency$author == "Bronte Sisters",],
         ~ proportion + `Jane Austen`)
cor.test(data = frequency[frequency$author == "Herbert George Wells",],
         ~ proportion + `Jane Austen`)
