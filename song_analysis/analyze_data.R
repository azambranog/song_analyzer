# This document takes an output such as the returned by  getArtistSongs and
#writes a csv for word counts per song and one for sentiment coefficient per song
# the package sentimentr was used (https://github.com/trinker/sentimentr)

library(data.table)
library(tidytext)
library(sentimentr)
library(dplyr)


data <- readRDS('madonna_data.Rds')

#word counts
word_counts <- lapply(data, function(x) {
  text_df <- data_frame(text = x$lyrics)
  tidy_df <- text_df %>%
    unnest_tokens(word, text) %>%
    anti_join(stop_words) %>%
    count(word, sort = TRUE)
  result <- as.data.table(tidy_df)
  result <- cbind(result, x$song_info[,.(album, year, song)])
  return(result)
})

word_counts <- rbindlist(word_counts, use.names = T)

write.csv(word_counts, 'madonna_word_count.csv')

#remove repeated verses in song
sentiments <- lapply(data, function(x) {
  coef <- mean(sentiment(unique(x$lyrics))$sentiment, na.rm = T)
  result <- copy(x$song_info)
  result[, sentiment := coef]
  return(result)
})
sentiments <- rbindlist(sentiments, use.names = T)

write.csv(sentiments, 'madonna_sentiments.csv')







