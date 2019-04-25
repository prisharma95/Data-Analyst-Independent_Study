library(tm)
library(SnowballC)
library(RColorBrewer, ggplot2)
library(wordcloud)
library(lsa)
library(quanteda)
library(qdap)
library(textclean)
library(textreg)
library(tibble)
library(tidytext)
library(dplyr)
library(stringr)
library(magrittr)
library(stringi)
library(readr)
library(tidyr)
library(purrr)
library(tokenizers)
library(tidyverse)
library(purrrlyr)
library(igraph)
library(ggraph)
library(widyr)
library(topicmodels)
library(textstem)


cname <- setwd("C:\\Users\\Priya Sharma\\Downloads\\Courses\\Spring 2019\\Independent Study\\data1\\") #setting directory

getwd()
Encoding(cname) <- "UTF-8" #setting encoding to UTF-8

dir(cname)

vcorpus <- VCorpus(DirSource(cname, encoding = "UTF-8"), readerControl = list(reader = readPlain)) #creating corpus

stopwords1 <- bind_rows(stop_words, tibble(word = c("bah", "di", "yeah", "car", "gonna", "â", "alea", "page","questionnaire", "really", "recording", "beep", "bum", "64", "gotcha", "kinda", "1", "2", "iâ"), lexicon = "custom"))

writeLines(as.character(vcorpus[1]))

tidycorpus <- tidy(vcorpus)

head(tidycorpus)

trigrams <- tibble(text = tidycorpus$text) %>% unnest_tokens(trigram, text, token = "ngrams", n = 3, collapse = FALSE)
head(trigrams)
tail(trigrams)

trigrams_separated <- trigrams %>% separate(trigram, c("word1", "word2", "word3"), sep = " ") #separate trigrams #unite() to recombine them
head(trigrams_separated)

trigrams_filtered <- trigrams_separated %>% filter(!word1 %in% stopwords1$word) %>% filter(!word2 %in% stopwords1$word) %>% filter(!word3 %in% stopwords1$word) #remove stopwords
trigrams_filtered 

trigrams_united <- trigrams_filtered %>% unite(trigram, sep = " ") %>% count(trigram, sort = TRUE) %>% mutate(trigram = replace(trigram, str_detect(trigram, "specific word"), "word")) 
dark2 <- brewer.pal(6, "Dark2")
set.seed(900)

trigrams_united %>% with(wordcloud(words = trigram, freq = n, max.words = 50, random.order = FALSE, colors = dark2))
dev.new(width = 20, height = 20, unit = "in")
trigram_counts <- trigrams_filtered %>% count(word1,word2,word3, sort = TRUE) #counting words after the stopwords are removed
trigram_counts

contain_magic_tri <- trigrams_filtered %>% filter(word2 == "magic" | word1 =="magic" | word3 == "magic") %>% count(word1, word2, word3, sort = TRUE) #trigrams containing magic
contain_magic_tri
trigrams_sentiment <- trigrams_separated %>% filter(word1 == "not" | word2 == "not") %>% count(word1,word2,word3, sort = TRUE) #trigrams containing "not"
trigrams_sentiment

word_corr <- trigrams_separated %>% group_by(word1) %>% filter(n() >=20) %>% pairwise_cor(word1, word2) %>% filter(!is.na(correlation)) #pairwise_cor(table, item to compare; will end up in item 1 and item2 columns, column describing the feature that links one item to others)
head(word_corr)

corr_magic <- word_corr %>% filter(item1 == "magic") %>% arrange(desc(correlation))
corr_magic



#---------LDA(Latent Dirichlet Allocation)------


LDA_by_docs <- tibble(text = tidycorpus$text, docs = tidycorpus$id) %>% group_by(docs = tidycorpus$id) %>% ungroup() %>% unite(id, docs)  #creating dataframe #When finding a tdm (i.e. using cast()) you need to count up the words in each document, and tell cast() which columns belong to the word and which belong to the document.

LDA_by_docs

LDA_analysis <- LDA_by_docs %>% unnest_tokens(word, text) %>% anti_join(stopwords1) #unnesting to create tokens
LDA_analysis

word_counts <- LDA_analysis %>% anti_join(stop_words) %>% count(id, word, sort = TRUE) %>% ungroup()  #finding document-word counts
word_counts


chapters_lda <- LDA(LDA_dtm, k = 5, control = list(seed = 1234)) #creating k topics

chapter_topics <- tidy(chapters_lda, matrix = "beta") #per-topic-per-word probability
chapter_topics

top_terms <- chapter_topics %>% group_by(topic) %>% top_n(10, beta) %>% ungroup() %>% arrange(topic, -beta) #STEM PROBABLY
top_terms

chapters_gamma <- tidy(chapters_lda, matrix = "gamma") 
chapters_gamma

chapter_classifications <- chapters_gamma %>% group_by(document) %>% top_n(1, gamma) %>% ungroup()
chapter_classifications

dictwarn <- chapter_classifications %>% count(document, topic) %>% group_by(document) %>% top_n(1, gamma) %>% ungroup() %>% transmute(consensus = tidyexcel$System)


#-----DICTIONARY-----

tempdir <- setwd("C:\\Users\\Priya Sharma\\Downloads\\")

data <- read.csv("excel.csv", stringsAsFactors = FALSE)
data

length(unique(data$System)) #unique values in system; 9
tech <- count(data, System, sort = TRUE)  #correct syntax for dplyr. For plyr package, count(data$System)
#tech <- unique(data$System)
tech
tidyexcel <- data %>% tbl_df %>% group_by(System) #%>% summarize(count(data, System))
head(tidyexcel)

detect_certain_words <- function(x) stri_replace_all_fixed(x, words, translate, vectorize_all = FALSE)
transformed <- detect_certain_words(as.matrix(trigrams))

view(transformed)
view(trigrams)


words <- unique(tidyexcel$Quotation.Content) %>% tolower()
words
translate <- unique(tidyexcel$System) %>% tolower()
translate


#-----PLOTTING------

trigram_graph <- trigram_counts %>% filter(n>2) %>% graph_from_data_frame()
trigram_graph

contain_magic_tri_graph <- contain_magic_tri %>% filter(n>0) %>% graph_from_data_frame()


set.seed(900)

ggraph(trigram_graph, layout = "fr") + 
  geom_edge_link(arrow = a, end_cap = circle(.07, 'inches')) + 
  geom_node_point(color = "lightblue", size = 5) + 
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void()

ggraph(contain_magic_tri_graph, layout = "fr") + 
  geom_edge_link(arrow = a, end_cap = circle(.07, 'inches')) + 
  geom_node_point(color = "lightblue", size = 5) + 
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void()


a <- grid::arrow(type = "closed", length = unit(.10, "inches"))
b <- grid::arrow(type = "closed") #for word_corr_plot
set.seed(400)


trigrams_separated %>%
  group_by(word1) %>%
  filter(n() >= 20) %>%
  pairwise_cor(word1, word2) %>%
  filter(!is.na(correlation),
         correlation > .55) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(arrow = a, end_cap = circle(.07, 'inches')) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), repel = TRUE) +
  theme_void()   #correlation plot

word_corr_plot <- word_corr %>% filter(n() >=20) %>% filter(item1 == "magic", correlation > 0.1) %>% arrange(desc(correlation)) %>% graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = correlation), arrow = b) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), repel = TRUE) +
  theme_void()   #correlation plot related to magic
word_corr_plot

top_terms %>% mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()


##-----Analysis on Quoted Content----

tempdir <- setwd("C:\\Users\\Priya Sharma\\Downloads\\")

data <- read.csv("data.csv", stringsAsFactors = FALSE)
head(data)

tokens <- tibble(text = data$ï..Text.Content) %>% unnest_tokens(word, text) %>% anti_join(stopwords1) %>% count(word, sort = TRUE) #unnesting to create tokens
tail(tokens, 10)

dark2 <- brewer.pal(6, "Dark2")
set.seed(1000)
tokens %>% with(wordcloud(words = word, freq = n, max.words = 55 , random.order = FALSE, colors = dark2))

trigrams <- tibble(text = data$ï..Text.Content) %>% unnest_tokens(trigram, text, token = "ngrams", n = 3, collapse = FALSE)

LDA_by_docs <- tibble(text = data$ï..Text.Content, codes = data$System) %>% group_by(codes = data$System) %>% ungroup() %>% unite(id, codes)  #creating dataframe #When finding a tdm (i.e. using cast()) you need to count up the words in each document, and tell cast() which columns belong to the word and which belong to the document.

LDA_by_docs

LDA_analysis <- LDA_by_docs %>% unnest_tokens(word, text) %>% anti_join(stopwords1) #unnesting to create tokens
LDA_analysis

word_counts <- LDA_analysis %>% anti_join(stop_words) %>% count(id, word, sort = TRUE) %>% ungroup()  #finding document-word counts
word_counts

tf_idf <- word_counts %>% bind_tf_idf(word, id, n)

tf_idf %>% arrange(desc(tf_idf))
LDA_dtm <- tf_idf %>% cast_dtm(id, word, n) #converting dataframe into dtm #replace tf_idf with word_counts
LDA_dtm

chapters_lda <- LDA(LDA_dtm, k = 5, control = list(seed = 1234)) #creating k topics

chapter_topics <- tidy(chapters_lda, matrix = "beta") #per-topic-per-word probability
chapter_topics

top_terms <- chapter_topics %>% group_by(topic) %>% top_n(10, beta) %>% ungroup() %>% arrange(topic, -beta) #STEM PROBABLY
top_terms

chapters_gamma <- tidy(chapters_lda, matrix = "gamma") #estimates that each word in the 007 document has only a 0.0000596% probability of coming from topic 1 
chapters_gamma

chapter_classifications <- chapters_gamma %>% group_by(document) %>% top_n(1, gamma) %>% ungroup()
chapter_classifications


top_terms %>% mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()

