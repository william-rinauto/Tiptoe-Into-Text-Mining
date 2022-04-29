#https://www.tidytextmining.com/

library(DBI)
library(RSQLite)
library(tidytext)
library(tidyverse)
library(lubridate)
library(future)
library(future.apply)
library(igraph)
library(ggraph)


source('sqlite_connection.R')




con <- sqlite_connection()
table <- dbListTables(con)

review_data <- dbGetQuery(sqlite_connection(), paste0('select * from [',table,']'))


words_list <- list('pay','debt')
my_stop_words <- tibble(word = unlist(words_list)) %>%
  mutate(lexicon = 'custom')
my_stop_words1 <- bind_rows(my_stop_words, stop_words)


tokenized <- review_data %>%
  group_by(company) %>%
  unnest_tokens(word, review) %>%
  anti_join(my_stop_words1)


afinn <- get_sentiments('afinn')

add_sentiments <- inner_join(tokenized, afinn) 

###############################################
# Split into positive and negative sentiments #
###############################################

positive <- filter(add_sentiments, value >0) %>% #Most common positive words
  count(company, word) %>%
  arrange(desc(n)) %>%
  group_by(company) %>%
  top_n(10,n) %>%
  arrange(company)

sentiment_scores <- add_sentiments %>%
  group_by(company) %>%
  summarise(average_sentiment = sum(value)/n()) %>%
  arrange(desc(average_sentiment))

negative <- filter(add_sentiments, value < 0) %>% #Most common negative words
  count(company, word) %>%
  arrange(desc(n)) %>%
  group_by(company) %>%
  top_n(10,n) %>%
  arrange(company)

review_types <- list(positive = positive, negative = negative)

plan('multisession',workers = 10)


###########################################
# Most common positive and negative words #
###########################################
plots_list <- future_lapply(1:length(review_types), function(i) {
  
  sentiment_type <- names(review_types)[i]
  reviews <- review_types[[i]]
  companies <- unique(reviews$company)
  
  fill_color <- ifelse(sentiment_type == 'positive', 'blue','red')
  
  sapply(companies, function(c) {
    single_comp <- filter(reviews, company == c) %>%
      mutate(word = forcats::fct_reorder(word, n))
    
    out_plot <- ggplot(single_comp, aes(x = word, y = n)) +
      geom_col(fill = fill_color, alpha = .4) + 
      coord_flip() +
      ggtitle(paste(c,sentiment_type, sep = "-"))
    
    out_plot <- list(out_plot)
    names(out_plot) <- paste(c, sentiment_type, sep = "_")
    
    ggsave(paste('plots/',c,"_", sentiment_type, sep = "",'.jpeg'), device = 'jpeg', height = 8, width = 7)
    
    return(out_plot)
  }) 
})

pos <- plots_list[[1]]      
neg <- plots_list[[2]]  

##################################################
# Strongest contributors to sentiment by company #
##################################################

biggest_contributors <- add_sentiments %>%
  group_by(company, word) %>%
  summarise(contribution = n() * mean(value)) %>%
  mutate(contrib_direction = ifelse(contribution < 0, 'negative','positive'),
         absolute_contribution = abs(contribution)) %>%
  top_n(10, absolute_contribution)


companies <- unique(biggest_contributors$company)
lapply(companies, function(c) {
  single_comp <- biggest_contributors %>%
    filter(company == c) %>%
    mutate(word = fct_reorder(factor(word), contribution)) %>%
    ggplot(aes(x = word, y = contribution)) +
    geom_col(aes(fill = contribution>0),alpha = 8,show.legend = F) + 
    coord_flip() +
    ggtitle(c) 
  
  ggsave(paste('plots/',c,"_sentiment contributors.jpeg"),device = 'jpeg', height = 8, width = 7)
})

#########################
# Taking a look at ngrams
#########################

bi_tokenized <- review_data %>%
  group_by(company) %>%
  unnest_tokens(bigram, review, token = 'ngrams',n = 2) 

bigram_split <- bi_tokenized %>%
  separate(bigram, c("word1","word2"), sep = " ")

bigram_split1 <- bigram_split

bigram_count <- bigram_split %>%
  filter(! word1 %in% my_stop_words1$word) %>%
  filter(! word2 %in% my_stop_words1$word) %>%
  unite(bigram, word1, word2, sep = " ") %>%
  group_by(company) %>%
  count(bigram) %>%
  arrange(desc(n)) %>%
  top_n(10, n)

lapply(companies, function(c) {
  
  data <- bigram_count %>%
    filter(company == c) %>%
    mutate(bigram = fct_reorder(bigram, n)) %>%
    ggplot(aes(x = bigram, y = n)) + 
    geom_col(fill = 'blue', alpha = .8) +
    coord_flip() +
    ggtitle(paste0("bigram_",c))
  
  ggsave(paste0("plots/bigram_",c,".jpeg"),device = 'jpeg',height = 8, width = 7)
})

#########################
# Handle negation words #
#########################

negation_words <- c("not", "no", "never", "without")

bigram_split2 <- bigram_split1 %>%
  anti_join(my_stop_words1, by = c('word2' = 'word')) %>%
  inner_join(afinn, by = c('word2' = 'word')) %>%
  mutate(value = ifelse(word1 %in% negation_words, value * -1, value)) %>%
  anti_join(my_stop_words, by = c('word2' = 'word'))

negation_addressed_sentiment <- bigram_split2 %>%
  group_by(company) %>%
  summarise(average_sentiment = mean(value)) %>%
  arrange(desc(average_sentiment))

visualize_bigrams_data <- bigram_split2 %>%
  anti_join(my_stop_words1, by = c('word1' = 'word')) %>% #already did this with word2 above
  group_by(company) %>%
  count(word1, word2, sort = T) %>%
  group_by(company) %>%
  top_n(20, n)

visualize_bigrams <- function(bigrams,comp) {
  set.seed(2016)
  a <- grid::arrow(type = "closed", length = unit(.15, "inches"))
  bigrams %>%
    filter(company == comp) %>%
    ungroup() %>%
    select(-company) %>%
    graph_from_data_frame() %>%
    ggraph(layout = "fr") +
    geom_edge_link(aes(edge_alpha = n), show.legend = FALSE, arrow = a) +
    geom_node_point(color = "lightblue", size = 5) +
    geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
    theme_void()
}

bigram_plot <- visualize_bigrams(visualize_bigrams_data, 'bhg')


###############################################
# Next time I look at this: word correlations #
###############################################


  