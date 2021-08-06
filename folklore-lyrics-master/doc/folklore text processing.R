#############################################################################################
# ------Analysis of sentiment and emotion in Taylor Swift's newest album - folklore ----- 
# TEXT PROCESSING 

#rm(list=ls())

# install packages 
list.of.packages <- c("tidytext", "tidyverse","stm", "quanteda","wordcloud", 
                      "reshape2","ggplot2", "geometry","Rtsne", "rsvd","syuzhet",
                      "data.table", "tm", "DT")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

# import packages 
library(tidytext)
library(tidyverse)
library(stm)
library(quanteda)
library(wordcloud)
library(reshape2)
library(ggplot2)
library(geometry)
library(Rtsne)
library(rsvd)
library(syuzhet)
library(tm)
library(data.table)
library(DT)
library(dplyr)


# -------------Text Preprocessing---------------------------------------------------------
#### Step 1 - Load the data
library(readr)
folklore <- read_csv("C:/Users/Huizhe ZHU/Desktop/Folklore project/folklore_lyrics.csv")
View(folklore_lyrics)

# group by songs
folklore <- folklore %>%
  group_by(song) %>%
  summarise(full_lyrics= str_c(lyrics, collapse = " ")) %>%
  ungroup()


#### Step 2 - Preliminary cleaning of text
#Convert letters to the lower case, remove punctuation, numbers, empty words and extra white space.
leadingWhitespace <- content_transformer(function(x) str_trim(x, side = "both"))

# load stop words
data(stop_words)
word <- c("lot", "today", "months", "month", "wanna", "wouldnt", "wasnt", "ha", "na", "ooh", "da",
          "gonna", "im", "dont", "aint", "wont", "yeah", "la", "oi", "nigga", "fuck",
          "hey", "year", "years", "last", "past", "feel")
stop_words <- c(stop_words$word, word)

# fixing the encoding
folklore$usable_Text<- iconv(folklore$full_lyrics, "ASCII", "UTF-8", sub="byte")

# clean the data and make a corpus
corpus <- VCorpus(VectorSource(folklore$usable_Text))%>%
  tm_map(content_transformer(tolower))%>%
  tm_map(removePunctuation)%>%
  tm_map(removeWords, character(0))%>%
  tm_map(removeWords, stop_words)%>%
  tm_map(stripWhitespace)%>%
  tm_map(leadingWhitespace)


#### Step 3 - Stemming and converting to tidy object
stemmed <- tm_map(corpus, stemDocument) %>%
  tidy() %>%
  select(text)


#### Step 4 - Tokenization on original words, *1455* rows (words) in total, one column
dict <- tidy(corpus) %>%
  select(text) %>%
  unnest_tokens(dictionary, text)


#### Step 5 - Tokenization on stemmed words  -  Add ID = row_number, dict = original word, 1455 rows, 3 columns
completed <- stemmed %>%
  mutate(id = row_number()) %>%
  unnest_tokens(stems, text) %>%
  bind_cols(dict) 


#### Step 6 - Stem completion
#Picking the original word (from the same root) with the highest frequency. The new column created -  'word' - will be used to replace stem, which is sometimes hard for people to understand. 
completed1 <- completed %>%
  group_by(stems) %>%
  count(dictionary) %>%
  mutate(word = dictionary[which.max(n)]) %>%
  ungroup() %>%
  select(stems, word) %>%
  distinct() %>%
  right_join(completed) %>%
  select(-stems)


#### Step 7 - Pasting stem completed individual words into their respective lyrics
#Put 'word' generated from Step 6, back to each song row - each row contains words like before - but the words are transforme, from -> stemmed -> completed stem
completed2 <- completed1 %>%
  group_by(id) %>%
  summarise(stemmedwords= str_c(word, collapse = " ")) %>%
  ungroup()



#### Step 8 - Keeping a track of the processed lyrics with their own ID
raw_lyrics1 <- folklore %>%
  mutate(id = row_number()) %>%
  inner_join(completed2)



#### Step 9 - Exporting the processed text data into a CSV file
save(raw_lyrics1, file="processed_folklore.csv")













