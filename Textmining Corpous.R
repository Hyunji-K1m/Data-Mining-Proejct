library(dplyr) 
library(tidytext)
library(textdata) 
library(tm) 
library(stringr) 
library(wordcloud) 
library(tidyverse) 
library(readr)


data<-read.csv(file_path)
data <- distinct(data)
data$doc_number <- seq.int(1, nrow(data))
names(data)

data <- distinct(data)
summary(data)  #no duplication


#year
tidy_data1<- data[,c(3,9)] %>%
  select(year, abstract) %>%
  unnest_tokens(word, abstract)  #lower case

#journal
tidy_data2<- data[,c(2,9)] %>%
  select(journal, abstract) %>%
  unnest_tokens(word, abstract)  #lower case


#corpus_year
data_00 <- tidy_data1[tidy_data1$year=="2000",]
data_01 <- tidy_data1[tidy_data1$year=="2001",]
data_02 <- tidy_data1[tidy_data1$year=="2002",]
data_03 <- tidy_data1[tidy_data1$year=="2003",]
data_04 <- tidy_data1[tidy_data1$year=="2004",]
data_05 <- tidy_data1[tidy_data1$year=="2005",]
data_06 <- tidy_data1[tidy_data1$year=="2006",]
data_07 <- tidy_data1[tidy_data1$year=="2007",]
data_08 <- tidy_data1[tidy_data1$year=="2008",]
data_09 <- tidy_data1[tidy_data1$year=="2009",]
data_10 <- tidy_data1[tidy_data1$year=="2010",]
data_11 <- tidy_data1[tidy_data1$year=="2011",]
data_12 <- tidy_data1[tidy_data1$year=="2012",]
data_13 <- tidy_data1[tidy_data1$year=="2013",]
data_14 <- tidy_data1[tidy_data1$year=="2014",]
data_15 <- tidy_data1[tidy_data1$year=="2015",]
data_16 <- tidy_data1[tidy_data1$year=="2016",]
data_17 <- tidy_data1[tidy_data1$year=="2017",]
data_18 <- tidy_data1[tidy_data1$year=="2018",]
data_19 <- tidy_data1[tidy_data1$year=="2019",]
data_20 <- tidy_data1[tidy_data1$year=="2020",]
data_21 <- tidy_data1[tidy_data1$year=="2021",]
data_22 <- tidy_data1[tidy_data1$year=="2022",]


#corpus_journal
data_hs <- tidy_data2[tidy_data2$journal=="Health Systems",]
data_js <- tidy_data2[tidy_data2$journal=="Journal of Simulation",]
data_jors <- tidy_data2[tidy_data2$journal=="Journal of the Operational Research Society",]


#find the common words
data("stop_words")
data_hs <- data_hs %>% 
  anti_join(stop_words)
slice_head(data_hs %>% 
             count(word) %>% 
             arrange(desc(n)),n=10)

#tf-idf example data_hs

library(tidytext)
word_counts <- data_hs %>%
  count(journal, word, sort = TRUE)

# Calculate the total number of documents
total_documents <- n_distinct(data_hs$journal)

# Calculate TF-IDF
tf_idf <- word_counts %>%
  bind_tf_idf(word, journal, n) %>%
  arrange(desc(tf_idf))

tf_idf_top10 <- slice_head(tf_idf, n = 10)
tf_idf_top10


