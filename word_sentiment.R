library(ggplot2)
library(dplyr)
library(stringr)
library(tidytext)
library(XML)
library(janitor)
library(fs)
library(tidyverse)

data <- read_rds("words.rds")

each_word <- data %>%
unnest_tokens(word, text) %>%
  select(word)


senti <- data %>%
  unnest_tokens(word, text) %>%
  group_by(season) %>%
  mutate(linenumber = row_number())%>%
  ungroup()

data(stop_words)

senti <- senti %>%
  anti_join(stop_words) %>%
  count(word, sort = TRUE) 

all_words <- senti %>%
  filter(n > 300) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = n)) +
  scale_colour_gradient2() +
  geom_col() +
  xlab(NULL) +
  coord_flip()

bing <- each_word %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment) %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() 

top_10 <- bing %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free") +  
  coord_flip()

write_rds(all_words, "all_words.rds")
write_rds(top_10, "top_10.rds")
