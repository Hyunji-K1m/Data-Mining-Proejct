library(dplyr)
library(tidytext) 
library(textdata)
library(tm) 
library(stringr) 
library(wordcloud) 
library(tidyverse)
library('topicmodels')
library(readr)
library(SnowballC)
library(udpipe)
library(tm)
library(slam)

data <- read_csv(file_path)
summary(data)
data <- distinct(data)
summary(data)  #no duplication

#corpus_year
data_00 <- data[data$year=="2000"|data$year=="2001"|data$year=="2002"|data$year=="2003",]
data_04 <- data[data$year=="2004"|data$year=="2005"|data$year=="2006"|data$year=="2007",]
data_08 <- data[data$year=="2008"|data$year=="2009"|data$year=="2010"|data$year=="2011",]
data_12 <- data[data$year=="2012"|data$year=="2013"|data$year=="2014"|data$year=="2015",]
data_16 <- data[data$year=="2016"|data$year=="2017"|data$year=="2018"|data$year=="2019",]
data_20 <- data[data$year=="2020"|data$year=="2021"|data$year=="2022",]

#corpus_journal
data_hs <- data[data$journal=="Health Systems",]
data_js <- data[data$journal=="Journal of Simulation",]
data_jors <- data[data$journal=="Journal of the Operational Research Society",]


#hs
data_hs$doc_number <- seq.int(1, nrow(data_hs))
data_hs<- data_hs %>%
  select(doc_number, abstract) %>%
  unnest_tokens(word, abstract)
data("stop_words")
data_hs<-data_hs[-grep("\\b\\d+\\b", data_hs$word),]
custom_stop_words <- data.frame(word = c('system','systems','based','time','paper','research',
                                         'data','model','models','results','analysis','approach'
))

data_hs <- data_hs %>% 
  anti_join(stop_words) %>%
  anti_join(custom_stop_words, by = c("word" = "word"))

#lemmatized
ud_model <- udpipe_download_model(language = "english")
ud_model <- udpipe_load_model(ud_model$file_model)

# Annotate using udpipe
annotated <- udpipe_annotate(ud_model, x = data_hs$word)
annotated <- as.data.frame(annotated)

# Extract lemmatized words
data_hs$word_lemma <- annotated$lemma[match(data_hs$word, annotated$token)]
data_hs <- na.omit(data_hs)

print(slice_head(data_hs %>%
                   count(word_lemma) %>%
                   arrange(desc(n)),n=30),n=30)

#wordcloud
top_50_imdb_data<- slice_head(data_hs %>%
                                count(word_lemma) %>%
                                arrange(desc(n)),n=50)
pal <- brewer.pal(8,"Dark2")
top_50_imdb_data %>%
  with(wordcloud(word_lemma, n, random.order = FALSE,colors=pal))

#ggplot
top_10_imdb_data<- slice_head(data_hs %>%
                                count(word_lemma) %>%
                                arrange(desc(n)),n=10)
ggplot(data=top_10_imdb_data, aes(x=word_lemma, y=n)) +
  geom_bar(stat="identity", color = "red", fill = "pink")

#tf_idf
imdb_words <- data_hs %>%
  count(doc_number, word_lemma, sort = TRUE)
imdb_tf_idf <- imdb_words %>%
  bind_tf_idf(word_lemma, doc_number, n)
head(imdb_tf_idf %>%
       arrange(desc(tf_idf)), 10)


#data_js
data_js$doc_number <- seq.int(1, nrow(data_js))
data_js<- data_js %>%
  select(doc_number, abstract) %>%
  unnest_tokens(word, abstract)
data("stop_words")
data_js<-data_js[-grep("\\b\\d+\\b", data_js$word),]
custom_stop_words <- data.frame(word = c('system','systems','time','paper','research',
                                         'model','models','modelling','approach','data','method','results','cis'))

data_js <- data_js %>% 
  anti_join(stop_words) %>%
  anti_join(custom_stop_words, by = c("word" = "word"))

#lemmatized
ud_model <- udpipe_download_model(language = "english")
ud_model <- udpipe_load_model(ud_model$file_model)

# Annotate using udpipe
annotated <- udpipe_annotate(ud_model, x = data_js$word)
annotated <- as.data.frame(annotated)

# Extract lemmatized words
data_js$word_lemma <- annotated$lemma[match(data_js$word, annotated$token)]
data_js <- na.omit(data_js)
# Assuming lemmatization has been done and lemmatized words are in 'word_lemma'


# Add doc_id after lemmatization
data_js <- data_js %>% 
  mutate(doc_id = row_number())

# Now count the frequency of each lemmatized word in each document
js_words <- data_js %>%
  count(doc_id, word_lemma, sort = TRUE)

print(slice_head(data_js %>%
                   count(word_lemma) %>%
                   arrange(desc(n)),n=30),n=30)

#wordcloud
top_50_imdb_data<- slice_head(data_js %>%
                                count(word_lemma) %>%
                                arrange(desc(n)),n=50)
pal <- brewer.pal(8,"Dark2")
top_50_imdb_data %>%
  with(wordcloud(word_lemma, n, random.order = FALSE,colors=pal))

#ggplot
top_10_imdb_data<- slice_head(data_js %>%
                                count(word_lemma) %>%
                                arrange(desc(n)),n=10)
ggplot(data=top_10_imdb_data, aes(x=word_lemma, y=n)) +
  geom_bar(stat="identity", color = "red", fill = "pink")

#tf_idf
imdb_words <- data_js %>%
  count(doc_number, word_lemma, sort = TRUE)
imdb_tf_idf <- imdb_words %>%
  bind_tf_idf(word_lemma, doc_number, n)
head(imdb_tf_idf %>%
       arrange(desc(tf_idf)), 10)


#data_jors
data_jors$doc_number <- seq.int(1, nrow(data_jors))
data_jors<- data_jors %>%
  select(doc_number, abstract) %>%
  unnest_tokens(word, abstract)
data("stop_words")
data_jors<-data_jors[-grep("\\b\\d+\\b", data_jors$word),]
custom_stop_words <- data.frame(word = c('system','systems','based','time','paper','research','study','method',
                                         'model','models','modelling','approach','data','methods','results','set','mo'))

data_jors <- data_jors %>% 
  anti_join(stop_words) %>%
  anti_join(custom_stop_words, by = c("word" = "word"))

#lemmatized
ud_model <- udpipe_download_model(language = "english")
ud_model <- udpipe_load_model(ud_model$file_model)

# Annotate using udpipe
annotated <- udpipe_annotate(ud_model, x = data_jors$word)
annotated <- as.data.frame(annotated)

# Extract lemmatized words
data_jors$word_lemma <- annotated$lemma[match(data_jors$word, annotated$token)]
data_js <- na.omit(data_jors)


# Add doc_id after lemmatization
data_jors <- data_jors %>% 
  mutate(doc_id = row_number())

# Now count the frequency of each lemmatized word in each document
jors_words <- data_jors %>%
  count(doc_id, word_lemma, sort = TRUE)
data_jors <- na.omit(data_jors)

print(slice_head(data_jors %>%
                   count(word_lemma) %>%
                   arrange(desc(n)),n=30),n=30)

#wordcloud
top_50_imdb_data<- slice_head(data_jors %>%
                                count(word_lemma) %>%
                                arrange(desc(n)),n=50)
pal <- brewer.pal(8,"Dark2")
top_50_imdb_data %>%
  with(wordcloud(word_lemma, n, random.order = FALSE,colors=pal))

#ggplot
top_10_imdb_data<- slice_head(data_jors %>%
                                count(word_lemma) %>%
                                arrange(desc(n)),n=10)
ggplot(data=top_10_imdb_data, aes(x=word_lemma, y=n)) +
  geom_bar(stat="identity", color = "red", fill = "pink")

#tf_idf
imdb_words <- data_jors %>%
  count(doc_number, word_lemma, sort = TRUE)
imdb_tf_idf <- imdb_words %>%
  bind_tf_idf(word_lemma, doc_number, n)
head(imdb_tf_idf %>%
       arrange(desc(tf_idf)), 10)

#data_00
data_00$doc_number <- seq.int(1, nrow(data_00))
data_00<- data_00 %>%
  select(doc_number, abstract) %>%
  unnest_tokens(word, abstract)
data("stop_words")
data_00<-data_00[-grep("\\b\\d+\\b", data_00$word),]
custom_stop_words <- data.frame(word = c('system','systems','based','time','paper','research',
                                         'study','method','results','data','model','models',
                                         'performance','process','solution','method','solutions',
                                         'set','methods','approach','analysis','journal','search',
                                         'procedure','level','information','single','total','proposed','ra'))

data_00 <- data_00 %>% 
  anti_join(stop_words) %>%
  anti_join(custom_stop_words, by = c("word" = "word"))

#lemmatized
ud_model <- udpipe_download_model(language = "english")
ud_model <- udpipe_load_model(ud_model$file_model)

# Annotate using udpipe
annotated <- udpipe_annotate(ud_model, x = data_00$word)
annotated <- as.data.frame(annotated)

# Extract lemmatized words
data_00$word_lemma <- annotated$lemma[match(data_00$word, annotated$token)]
data_00 <- na.omit(data_00)


# Add doc_id after lemmatization
data_00 <- data_00 %>% 
  mutate(doc_id = row_number())

# Now count the frequency of each lemmatized word in each document
words_00 <- data_00 %>%
  count(doc_id, word_lemma, sort = TRUE)
data_00 <- na.omit(data_00)

print(slice_head(data_00 %>%
                   count(word_lemma) %>%
                   arrange(desc(n)),n=30),n=30)

#wordcloud
top_50_imdb_data<- slice_head(data_00 %>%
                                count(word_lemma) %>%
                                arrange(desc(n)),n=50)
pal <- brewer.pal(8,"Dark2")
top_50_imdb_data %>%
  with(wordcloud(word_lemma, n, random.order = FALSE,colors=pal))

#ggplot
top_10_imdb_data<- slice_head(data_00 %>%
                                count(word_lemma) %>%
                                arrange(desc(n)),n=10)
ggplot(data=top_10_imdb_data, aes(x=word_lemma, y=n)) +
  geom_bar(stat="identity", color = "red", fill = "pink")

#tf_idf
imdb_words <- data_00 %>%
  count(doc_number, word_lemma, sort = TRUE)
imdb_tf_idf <- imdb_words %>%
  bind_tf_idf(word_lemma, doc_number, n)
head(imdb_tf_idf %>%
       arrange(desc(tf_idf)), 10)


#data_04
data_04$doc_number <- seq.int(1, nrow(data_04))
data_04<- data_04 %>%
  select(doc_number, abstract) %>%
  unnest_tokens(word, abstract)
data("stop_words")
data_04<-data_04[-grep("\\b\\d+\\b", data_04$word),]
custom_stop_words <- data.frame(word = c('system','systems','based','time','paper','research',
                                         'study','method','results','data','model','models',
                                         'performance','process','solution','method','solutions',
                                         'set','methods','approach','analysis','journal','search',
                                         'procedure','level','information','single','total','proposed','meodelling','approaches','sa','ew','gra','doi'))

data_04 <- data_04 %>% 
  anti_join(stop_words) %>%
  anti_join(custom_stop_words, by = c("word" = "word"))

ud_model <- udpipe_download_model(language = "english")
ud_model <- udpipe_load_model(ud_model$file_model)

# Annotate using udpipe
annotated <- udpipe_annotate(ud_model, x = data_04$word)
annotated <- as.data.frame(annotated)

# Extract lemmatized words
data_04$word_lemma <- annotated$lemma[match(data_04$word, annotated$token)]
data_04 <- na.omit(data_04)

print(slice_head(data_04 %>%
                   count(word_lemma) %>%
                   arrange(desc(n)),n=30),n=30)

#wordcloud
top_50_imdb_data<- slice_head(data_04 %>%
                                count(word_lemma) %>%
                                arrange(desc(n)),n=50)
pal <- brewer.pal(8,"Dark2")
top_50_imdb_data %>%
  with(wordcloud(word_lemma, n, random.order = FALSE,colors=pal))

#ggplot
top_10_imdb_data<- slice_head(data_04 %>%
                                count(word_lemma) %>%
                                arrange(desc(n)),n=10)
ggplot(data=top_10_imdb_data, aes(x=word_lemma, y=n)) +
  geom_bar(stat="identity", color = "red", fill = "pink")

#tf_idf
imdb_words <- data_04 %>%
  count(doc_number, word_lemma, sort = TRUE)
imdb_tf_idf <- imdb_words %>%
  bind_tf_idf(word_lemma, doc_number, n)
head(imdb_tf_idf %>%
       arrange(desc(tf_idf)), 10)


#data_08
data_08$doc_number <- seq.int(1, nrow(data_08))
data_08<- data_08 %>%
  select(doc_number, abstract) %>%
  unnest_tokens(word, abstract)
data("stop_words")
data_08<-data_08[-grep("\\b\\d+\\b", data_08$word),]
custom_stop_words <- data.frame(word = c('system','systems','based','time','paper','research',
                                         'study','method','results','data','model','models',
                                         'performance','process','solution','method','solutions',
                                         'set','methods','approach','analysis','journal','search',
                                         'procedure','level','information','single','total','proposed',
                                         'meodelling','approaches','ads'))

data_08 <- data_08 %>% 
  anti_join(stop_words) %>%
  anti_join(custom_stop_words, by = c("word" = "word"))

ud_model <- udpipe_download_model(language = "english")
ud_model <- udpipe_load_model(ud_model$file_model)
# Annotate using udpipe
annotated <- udpipe_annotate(ud_model, x = data_08$word)
annotated <- as.data.frame(annotated)

# Extract lemmatized words
data_08$word_lemma <- annotated$lemma[match(data_08$word, annotated$token)]
data_08 <- na.omit(data_08)

print(slice_head(data_08 %>%
                   count(word_lemma) %>%
                   arrange(desc(n)),n=30),n=30)

#wordcloud
top_50_imdb_data<- slice_head(data_08 %>%
                                count(word_lemma) %>%
                                arrange(desc(n)),n=50)
pal <- brewer.pal(8,"Dark2")
top_50_imdb_data %>%
  with(wordcloud(word_lemma, n, random.order = FALSE,colors=pal))

#ggplot
top_10_imdb_data<- slice_head(data_08 %>%
                                count(word_lemma) %>%
                                arrange(desc(n)),n=10)
ggplot(data=top_10_imdb_data, aes(x=word_lemma, y=n)) +
  geom_bar(stat="identity", color = "red", fill = "pink")

#tf_idf
imdb_words <- data_08 %>%
  count(doc_number, word_lemma, sort = TRUE)
imdb_tf_idf <- imdb_words %>%
  bind_tf_idf(word_lemma, doc_number, n)
head(imdb_tf_idf %>%
       arrange(desc(tf_idf)), 10)


#data_12
data_12$doc_number <- seq.int(1, nrow(data_12))
data_12<- data_12 %>%
  select(doc_number, abstract) %>%
  unnest_tokens(word, abstract)
data("stop_words")
data_12<-data_12[-grep("\\b\\d+\\b", data_12$word),]
custom_stop_words <- data.frame(word = c('system','systems','based','time','paper','research',
                                         'study','method','results','data','model','models',
                                         'performance','process','solution','method','solutions',
                                         'set','methods','approach','analysis','journal','search',
                                         'procedure','level','information','single','total','proposed',
                                         'meodelling','approaches'))

data_12 <- data_12 %>% 
  anti_join(stop_words) %>%
  anti_join(custom_stop_words, by = c("word" = "word"))

ud_model <- udpipe_download_model(language = "english")
ud_model <- udpipe_load_model(ud_model$file_model)
# Extract lemmatized words
data_12$word_lemma <- annotated$lemma[match(data_12$word, annotated$token)]
data_12 <- na.omit(data_12)

print(slice_head(data_12 %>%
                   count(word_lemma) %>%
                   arrange(desc(n)),n=30),n=30)

#wordcloud
top_50_imdb_data<- slice_head(data_12 %>%
                                count(word_lemma) %>%
                                arrange(desc(n)),n=50)
pal <- brewer.pal(8,"Dark2")
top_50_imdb_data %>%
  with(wordcloud(word_lemma, n, random.order = FALSE,colors=pal))

#ggplot
top_10_imdb_data<- slice_head(data_12 %>%
                                count(word_lemma) %>%
                                arrange(desc(n)),n=10)
ggplot(data=top_10_imdb_data, aes(x=word_lemma, y=n)) +
  geom_bar(stat="identity", color = "red", fill = "pink")

#tf_idf
imdb_words <- data_12 %>%
  count(doc_number, word_lemma, sort = TRUE)
imdb_tf_idf <- imdb_words %>%
  bind_tf_idf(word_lemma, doc_number, n)
head(imdb_tf_idf %>%
       arrange(desc(tf_idf)), 10)


#data_16
data_16$doc_number <- seq.int(1, nrow(data_16))
data_16<- data_16 %>%
  select(doc_number, abstract) %>%
  unnest_tokens(word, abstract)
data("stop_words")
data_16<-data_16[-grep("\\b\\d+\\b", data_16$word),]
custom_stop_words <- data.frame(word = c('system','systems','based','time','paper','research',
                                         'study','method','results','data','model','models',
                                         'performance','process','solution','method','solutions',
                                         'set','methods','approach','analysis','journal','search',
                                         'procedure','level','information','single','total','proposed',
                                         'meodelling','approaches','mus','mus'))

data_16 <- data_16 %>% 
  anti_join(stop_words) %>%
  anti_join(custom_stop_words, by = c("word" = "word"))

ud_model <- udpipe_download_model(language = "english")
ud_model <- udpipe_load_model(ud_model$file_model)
# Extract lemmatized words
data_16$word_lemma <- annotated$lemma[match(data_16$word, annotated$token)]
data_16 <- na.omit(data_16)

print(slice_head(data_16 %>%
                   count(word_lemma) %>%
                   arrange(desc(n)),n=30),n=30)

#wordcloud
top_50_imdb_data<- slice_head(data_16 %>%
                                count(word_lemma) %>%
                                arrange(desc(n)),n=50)
pal <- brewer.pal(8,"Dark2")
top_50_imdb_data %>%
  with(wordcloud(word_lemma, n, random.order = FALSE,colors=pal))

#ggplot
top_10_imdb_data<- slice_head(data_16 %>%
                                count(word_lemma) %>%
                                arrange(desc(n)),n=10)
ggplot(data=top_10_imdb_data, aes(x=word_lemma, y=n)) +
  geom_bar(stat="identity", color = "red", fill = "pink")

#tf_idf
imdb_words <- data_16 %>%
  count(doc_number, word_lemma, sort = TRUE)
imdb_tf_idf <- imdb_words %>%
  bind_tf_idf(word_lemma, doc_number, n)
head(imdb_tf_idf %>%
       arrange(desc(tf_idf)), 10)


#data_20
data_20$doc_number <- seq.int(1, nrow(data_20))
data_20<- data_20 %>%
  select(doc_number, abstract) %>%
  unnest_tokens(word, abstract)
data("stop_words")
data_20<-data_20[-grep("\\b\\d+\\b", data_20$word),]

custom_stop_words <- data.frame(word = c('system','systems','based','time','paper','research',
                                         'study','method','results','data','model','models',
                                         'performance','process','solution','method','solutions',
                                         'set','methods','approach','analysis','journal','search',
                                         'procedure','level','information','single','total','proposed',
                                         'meodelling','approaches','article'))

data_20 <- data_20 %>% 
  anti_join(stop_words) %>%
  anti_join(custom_stop_words, by = c("word" = "word"))

ud_model <- udpipe_download_model(language = "english")
ud_model <- udpipe_load_model(ud_model$file_model)
# Extract lemmatized words
data_20$word_lemma <- annotated$lemma[match(data_20$word, annotated$token)]
data_20 <- na.omit(data_20)

print(slice_head(data_20 %>%
                   count(word_lemma) %>%
                   arrange(desc(n)),n=30),n=30)

#wordcloud
top_50_imdb_data<- slice_head(data_20 %>%
                                count(word_lemma) %>%
                                arrange(desc(n)),n=50)
pal <- brewer.pal(8,"Dark2")
top_50_imdb_data %>%
  with(wordcloud(word_lemma, n, random.order = FALSE,colors=pal))

#ggplot
top_10_imdb_data<- slice_head(data_20 %>%
                                count(word_lemma) %>%
                                arrange(desc(n)),n=10)
ggplot(data=top_10_imdb_data, aes(x=word_lemma, y=n)) +
  geom_bar(stat="identity", color = "red", fill = "pink")

#tf_idf
imdb_words <- data_20 %>%
  count(doc_number, word_lemma, sort = TRUE)
imdb_tf_idf <- imdb_words %>%
  bind_tf_idf(word_lemma, doc_number, n)
head(imdb_tf_idf %>%
       arrange(desc(tf_idf)), 10)
