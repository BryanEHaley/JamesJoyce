library(dplyr)
library(tm.plugin.webmining)
library(purrr)
library(tidytext)
library(gutenbergr)
library(ggplot2)

joyce <- gutenberg_download(c(4300,2814,4217))


tidy_joyce <- joyce %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)

wordfreq <- tidy_joyce %>%
  count(word,sort=T)

tidy_joyce %>%
  count(word, sort = TRUE) %>%
  filter(n > 300) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() + ggtitle("The Most Common Words in James Joyce's Novels")




bing_word_counts <- tidy_joyce %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

bing_word_counts

joyce_bigrams <- joyce %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2)

joyce_bigrams


bing_word_counts %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(y = "Words Contribute to sentiment",
       x = NULL) +
  coord_flip()

# install wordcloud
install.packages("wordcloud")
library(wordcloud)
tidy_joyce %>%
  anti_join(stop_words) %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 100))

#wordcloud of sentiment

library(reshape2)
tidy_joyce %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("#F8766D", "#00BFC4"),
                   max.words = 100)


### Relationships between words
joyce_bigrams2 <- joyce %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2)
joyce_bigrams2

install.packages("tidyr")
library(tidyr)
bigrams_separated <- joyce_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")
bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)
bigram_counts <- bigrams_filtered %>% 
  count(word1, word2, sort = TRUE)
bigram_counts

bigrams_united <- bigrams_filtered %>%
  unite(bigram, word1, word2, sep = " ")
bigrams_united

bigram_tf_idf <- bigrams_united %>%
  count(bigram)
bigram_tf_idf <- bigram_tf_idf %>% filter(n>30)
ggplot(aes(x = reorder(bigram, n), y=n), data=bigram_tf_idf) + geom_bar(stat = 'identity') + ggtitle("The Most Common Bigrams in joyce' novels") + coord_flip()

install.packages("igraph")
library(igraph)
bigram_graph <- bigram_counts %>%
  filter(n > 20) %>%
  graph_from_data_frame()
bigram_graph

install.packages("ggraph")
library(ggraph)
set.seed(2017)
ggraph(bigram_graph, layout = "fr") +
  geom_edge_link() +
  geom_node_point(color = "darkslategray4", size = 3) +
  geom_node_text(aes(label = name), vjust = 1.8) + ggtitle("Common Bigrams in joyce' five Novels")

#
install.packages("readr")
install.packages("stringr")
library(readr)
library(stringr)

getwd()

raw_tale <- read_lines("4300-0.txt", skip = 30, n_max = 15500)
tale <- character()
for (i in seq_along(raw_tale)) {
  if (i%%10 == 1) tale[ceiling(i/10)] <- str_c(raw_tale[i], 
                                               raw_tale[i+1],
                                               raw_tale[i+2],
                                               raw_tale[i+3],
                                               raw_tale[i+4],
                                               raw_tale[i+5],
                                               raw_tale[i+6],
                                               raw_tale[i+7],
                                               raw_tale[i+8],
                                               raw_tale[i+9], sep = " ")
}

tale[9:10]

#
install.packages("syuzhet")
library(syuzhet)
tale_nrc <- cbind(linenumber = seq_along(tale), get_nrc_sentiment(tale))

tale_nrc$negative <- -tale_nrc$negative
pos_neg <- tale_nrc %>% select(linenumber, positive, negative) %>% 
  melt(id = "linenumber")
names(pos_neg) <- c("linenumber", "sentiment", "value")

#
tale_nrc$negative <- -tale_nrc$negative
pos_neg <- tale_nrc %>% select(linenumber, positive, negative) %>% 
  melt(id = "linenumber")
names(pos_neg) <- c("linenumber", "sentiment", "value")
library(ggthemes)
ggplot(data = pos_neg, aes(x = linenumber, y = value, fill = sentiment)) +
  geom_bar(stat = 'identity', position = position_dodge()) + theme_minimal() +
  ylab("Sentiment") + 
  ggtitle("Positive and Negative Sentiment in A Tale of Two Cities") +
  scale_color_manual(values = c("orange", "blue")) +
  scale_fill_manual(values = c("orange", "blue"))