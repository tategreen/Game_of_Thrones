library(ggplot2)
library(dplyr)
library(stringr)
library(tidytext)
library(XML)
library(janitor)
library(fs)
library(tidyverse)

data <- read_rds("words.rds")

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
  ggplot(aes(word, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()

