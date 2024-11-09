library(tidyverse)
library(tidytext)


path = "/home/georgehagstrom/work/Teaching/DATA607/aclImdb/train/"

read_file(str_c(path,"neg/11010_1.txt"))

neg_review_files = list.files(str_c(path,"neg"),pattern = "[.]txt",full.names = TRUE)
pos_review_files = list.files(str_c(path,"pos"),pattern = "[.]txt",full.names = TRUE)

neg_reviews = neg_review_files |> set_names(basename) |> map(read_file)
pos_reviews = pos_review_files |> set_names(basename) |> map(read_file)

parse_file = function(review){
  tibble(review = review)
}

neg_reviews_df = neg_reviews |> map(parse_file) |> 
  list_rbind(names_to = "file_name") |> 
  mutate(file_name = str_remove(file_name,".txt")) |> 
  separate(file_name,c("index","rating"),sep = "_") |> 
  mutate(index = as.numeric(index),rating = as.numeric(rating)) |> arrange(index)

max_index = max(neg_reviews_df$index)
max_index

pos_reviews_df = pos_reviews |> map(parse_file) |> 
  list_rbind(names_to = "file_name") |> 
  mutate(file_name = str_remove(file_name,".txt")) |> 
  separate(file_name,c("index","rating"),sep = "_") |> 
  mutate(index = as.numeric(index) + max_index,rating = as.numeric(rating)) |> arrange(index)

reviews_df = neg_reviews_df |> bind_rows(pos_reviews_df)

reviews_df

reviews_tidy = reviews_df |> unnest_tokens(word,review) |> inner_join(get_sentiments("afinn"))

reviews_sentiment = reviews_tidy |> group_by(index,rating) |> summarise(sentiment = sum(value)/n()) |> ungroup()

reviews_sentiment

reviews_sentiment |>  ggplot(aes(group=rating,y=sentiment)) + geom_boxplot()

extreme = c("extreme","extremely","very","greatly","wildly","absolutely",   "utterly","entirely","fully","vastly","staggeringly","totally")
extreme_words = tibble(word = extreme, mod_value = 2)

not_words = tibble(word = c("not","don't"),mod_value = -1)

mod_words = not_words |> bind_rows(extreme_words)
mod_words

reviews_double = reviews_df |> unnest_tokens(bigram,review,token="ngrams",n=2) |> 
  filter(!is.na(bigram)) |> 
  separate(bigram,c("word1","word2")) |> 
  inner_join(get_sentiments("afinn"),by = join_by("word2" == "word")) |> 
  left_join(mod_words,by = join_by("word1" == "word"))

reviews_double_mod = reviews_double |> mutate(mod_value = if_else(is.na(mod_value),
                                                                  value,
                                                                  value*mod_value))

reviews_mod_sentiment = reviews_double_mod |> group_by(index,rating) |> 
  summarise(sentiment = sum(mod_value)/n())

reviews_mod_sentiment |> ggplot(aes(group=rating,y=sentiment)) + geom_boxplot()

cor(reviews_sentiment$rating,reviews_sentiment$sentiment)

cor(reviews_mod_sentiment$rating,reviews_mod_sentiment$sentiment)


