
#------LOADING DATA-------

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


cname <- setwd("C:\\Users\\Priya Sharma\\Downloads\\") #setting directory

getwd()
Encoding(cname) <- "UTF-8" #setting encoding to UTF-8

dir(cname)

vcorpus <- VCorpus(DirSource(cname, encoding = "UTF-8"), readerControl = list(reader = readPlain)) #creating corpus

summary(vcorpus)
inspect(vcorpus)

writeLines(as.character(vcorpus[1])) #reading data of the first document in the corpus

#-------PREPROCESSING DATA--------

vcorpus <- tm_map(x = vcorpus, FUN = removePunctuation) #remove punctuation

vcorpus <- tm_map(x = vcorpus, FUN = tolower) #turn to lower case

vcorpusCopy <- vcorpus

vcorpus <- tm_map(x = vcorpus, FUN = removeWords, stopwords("english")) #removing stopwords

vcorpus <- tm_map(x = vcorpus, FUN = stripWhitespace) #remove whitespace

vcorpus <- tm_map(x = vcorpus, FUN = PlainTextDocument) #tells R to treat your preprocessed documents as text documents.

stopwords1 <- c("page", "remember", "guess", "bit", "yeah", "didnt", "dont", "things", "kind", "youre", "questionnaire", "recordings", "itd", "crap", "feedback", "mightve", "whats", "don'", "ive", "hey", "sorta", "damn", "york", "tells", "recording", "isn'", "yep", "'m", "weve", "didn'", "yup", "'ve", "hes", "wasi", "itll", "doesnt", "theyre", "wasnt", "cool", "youll", "isnt", "wouldnt", "wasthat", "thats", "just", "really","cant", "theres")
vcorpus <- tm_map(x = vcorpus, FUN = removeWords, stopwords1)


vcorpusstem <- tm_map(x = vcorpus, FUN = stemDocument, language = "english") #stems document

vcorpusstemc <- tm_map(x = vcorpusstem, FUN = stemCompletion, dictionary = vcorpusCopy, lazy = TRUE) #adds common endings to improve intrepretability


vcorpusstemc <- tm_map(x = vcorpusstem, FUN = tolower) #had to do this again otherwise the line below was not working; issue with tm package

vcorpusstemc <- tm_map(vcorpusstemc, PlainTextDocument)

writeLines((as.character(vcorpuswo[1])))

#replace_contraction(vcorpus, contraction.key = lexicon::key_contractions, ignore.case = TRUE) (removing slang words)

#----------STAGING THE DATA-----------
dtm <- DocumentTermMatrix(vcorpus)
dtm #terms : 4317

tdm <- TermDocumentMatrix(vcorpus) #transpose of DTM
tdm

freq <- colSums(as.matrix(dtm)) #organizing terms by frequency
length(freq) #4317

subs <- names(freq)
subs <- gsub("gonna", "going to", subs)
subs <- gsub("wanna", "want to", subs)
subs <- gsub("gotcha", "i got you", subs)
subs <- gsub("sorta", "sort of", subs)
subs <- gsub("thats", "that is", subs)

ord <- order((freq))

dtms <- removeSparseTerms(dtm, 0.10)
dtms  #terms :52 / 10% ; terms : 94 / 20%

freq <- sort(colSums(as.matrix(dtms)), decreasing = TRUE)
freq

 #freqR1 <- findFreqTerms(dtm, lowfreq = 50) #another way to find frequent terms

#--------PLOTTING------

set.seed(140)
dark2 <- brewer.pal(6, "Dark2")
wordcloud(names(freq), freq, min.freq = 10, max.freq = 40, colors = dark2)
#all terms are displayed 

mylsa <- lsa(t(dtms))
termvectors <- mylsa$tk * mylsa$sk
docVectors <-  mylsa$dk * mylsa$sk
head(mylsa$dk)

termsinLSA <- cosine(t(termvectors))
mosttwenty <- termsinLSA[c(1:20), c(1:20)]
heatmap(mosttwenty)


#----TOKENIZING-----

tempdir <- setwd("C:\\Users\\Priya Sharma\\Downloads\\")

data <- read.csv("excel.csv", stringsAsFactors = FALSE)
data

length(unique(data$System)) #unique values in system(unique technologies) 9
tech <- count(data, System, sort = TRUE)  #correct syntax for dplyr. For plyr package, count(data$System)
#tech <- unique(data$System)
tech
tidydata <- data %>% tbl_df %>% group_by(System) #%>% summarize(count(data, System))
head(tidydata)

#------DICTIONARY-------

words <-tidydata$Quotation.Content %>% tolower()
head(words)
translate <- tidydata$System %>% tolower()

replacefunc <- function(x) stri_replace_all_fixed(x, words, translate, vectorize_all = FALSE)

view(kw_toks)  #interactive HTML view


