## code to prepare `plays_tokens` dataset goes here

library(dplyr)
library(tidytext)

# TODO: tag POS, https://slcladal.github.io/tagging.html

# Read the plays data
load('data/plays_text.rda')

# Unnest into single tokens
tidy_plays <- plays_text %>%
  unnest_tokens(word, line)

# Count word frequency
word_frequency <- tidy_plays %>%
  count(word, sort = TRUE)

# Calculate number of characters but ignore '
word_frequency <-  word_frequency %>%
  mutate(
    nchar = stringr::str_count(word) - stringr::str_count(word, "'")
  )

# Keep 5,6,7 letter words
wordset <- word_frequency %>%
  filter(nchar %in% c(5,6,7))

# quantiles of freq distribution
quant <- quantile(wordset$n, c(0.75, 0.95, 0.9995))

# Choose words that are in the above set
wordset_small <- wordset %>%
  filter(n >= quant[1] & n <= quant[3])

# Sort
wordset_small <- wordset_small %>%
  arrange(nchar, n)

plays_tokens <- wordset_small

usethis::use_data(plays_tokens, overwrite = TRUE)
