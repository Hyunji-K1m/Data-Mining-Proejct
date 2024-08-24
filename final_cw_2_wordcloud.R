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


#year
tidy_data1<- data[,c(3,9)] %>%
  select(year, abstract) %>%
  unnest_tokens(word, abstract)  #lower case

#journal
tidy_data2<- data[,c(2,9)] %>%
  select(journal, abstract) %>%
  unnest_tokens(word, abstract)  #lower case


#corpus_year
data_00 <- tidy_data1[tidy_data1$year=="2000"|tidy_data1$year=="2001"|tidy_data1$year=="2002"|tidy_data1$year=="2003",]
data_04 <- tidy_data1[tidy_data1$year=="2004"|tidy_data1$year=="2005"|tidy_data1$year=="2006"|tidy_data1$year=="2007",]
data_08 <- tidy_data1[tidy_data1$year=="2008"|tidy_data1$year=="2009"|tidy_data1$year=="2010"|tidy_data1$year=="2011",]
data_12 <- tidy_data1[tidy_data1$year=="2012"|tidy_data1$year=="2013"|tidy_data1$year=="2014"|tidy_data1$year=="2015",]
data_16 <- tidy_data1[tidy_data1$year=="2016"|tidy_data1$year=="2017"|tidy_data1$year=="2018"|tidy_data1$year=="2019",]
data_20 <- tidy_data1[tidy_data1$year=="2020"|tidy_data1$year=="2021"|tidy_data1$year=="2022",]



#corpus_journal
data_hs <- tidy_data2[tidy_data2$journal=="Health Systems",]
data_js <- tidy_data2[tidy_data2$journal=="Journal of Simulation",]
data_jors <- tidy_data2[tidy_data2$journal=="Journal of the Operational Research Society",]


#hs
data("stop_words")

custom_stop_words <- data.frame(word = c('system','systems','based','time','paper','research',
                                         'data','model','models','results','analysis','approach'
))

data_hs <- data_hs %>% 
  anti_join(stop_words) %>%
  anti_join(custom_stop_words, by = c("word" = "word"))


print(slice_head(data_hs %>%
                   count(word) %>%
                   arrange(desc(n)),n=30),n=30)

ud_model <- udpipe_download_model(language = "english")
ud_model <- udpipe_load_model(ud_model$file_model)

# Annotate using udpipe
annotated <- udpipe_annotate(ud_model, x = data_hs$word)
annotated <- as.data.frame(annotated)

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

#tf-idf

data_hs <- data_hs %>%
  mutate(doc_id = row_number())

tf_filtered <- tf %>%
  filter(tf > 1)

tf <- data_hs %>%
  count(doc_id, word_lemma) %>%
  rename(tf = n)

total_docs <- n_distinct(data_hs$doc_id)
idf <- data_hs %>%
  group_by(word_lemma) %>%
  summarise(n_docs = n_distinct(doc_id)) %>%
  mutate(idf = log(total_docs / n_docs))

tf_idf <- tf %>%
  inner_join(idf, by = "word_lemma") %>%
  mutate(tf_idf = tf * idf)

head(tf_idf)

top_tf_idf <- tf_idf %>%
  arrange(desc(tf_idf)) %>%
  slice_head(n = 10)
print(top_tf_idf)

tf_idf_below_9_76 <- tf_idf %>%
  filter(tf_idf < 9.76)
print(tf_idf_below_9_76)




#js
#discrete-event model/agent-based model
data("stop_words")
custom_stop_words <- data.frame(word = c('system','systems','time','paper','research',
                                         'model','models','modelling','approach','data','method','results'))

data_js <- data_js %>% 
  anti_join(stop_words) %>%
  anti_join(custom_stop_words, by = c("word" = "word"))


print(slice_head(data_js %>%
                   count(word) %>%
                   arrange(desc(n)),n=30),n=30)
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

#tf-idf
# Adding a document ID to each row in data_js
js_tf_idf <- js_words %>%
  bind_tf_idf(word_lemma, doc_id, n)
head(js_tf_idf %>%
       arrange(desc(tf_idf)), 10)
head(js_tf_idf)
tf_idf_below_10.3 <- js_tf_idf %>%
  filter(tf_idf < 10.3)
head(tf_idf_below_10.3 %>%
       arrange(desc(js_tf_idf)), 10)



#jors
data("stop_words")
custom_stop_words <- data.frame(word = c('system','systems','based','time','paper','research','study','method',
                                         'model','models','modelling','approach','data','methods','results','set'))

data_jors <- data_jors %>% 
  anti_join(stop_words) %>%
  anti_join(custom_stop_words, by = c("word" = "word"))


print(slice_head(data_jors %>%
                   count(word) %>%
                   arrange(desc(n)),n=30),n=30)

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

#tf-idf
# Adding a document ID to each row in data_js
js_tf_idf <- jors_words %>%
  bind_tf_idf(word_lemma, doc_id, n)
head(js_tf_idf %>%
       arrange(desc(tf_idf)), 10)
head(js_tf_idf)
tf_idf_below_10.3 <- js_tf_idf %>%
  filter(tf_idf < 12.4)
head(tf_idf_below_10.3)




#2000-2003
data("stop_words")
custom_stop_words <- data.frame(word = c('system','systems','based','time','paper','research',
                                         'study','method','results','data','model','models',
                                         'performance','process','solution','method','solutions',
                                         'set','methods','approach','analysis','journal','search',
                                         'procedure','level','information','single','total','proposed'))

data_00 <- data_00 %>% 
  anti_join(stop_words) %>%
  anti_join(custom_stop_words, by = c("word" = "word"))


print(slice_head(data_00 %>%
                   count(word) %>%
                   arrange(desc(n)),n=30),n=30)

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





#2004-2007
data("stop_words")
custom_stop_words <- data.frame(word = c('system','systems','based','time','paper','research',
                                         'study','method','results','data','model','models',
                                         'performance','process','solution','method','solutions',
                                         'set','methods','approach','analysis','journal','search',
                                         'procedure','level','information','single','total','proposed','meodelling','approaches'))

data_04 <- data_04 %>% 
  anti_join(stop_words) %>%
  anti_join(custom_stop_words, by = c("word" = "word"))


print(slice_head(data_04 %>%
                   count(word) %>%
                   arrange(desc(n)),n=30),n=30)

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



#2008-2011
data("stop_words")
custom_stop_words <- data.frame(word = c('system','systems','based','time','paper','research',
                                         'study','method','results','data','model','models',
                                         'performance','process','solution','method','solutions',
                                         'set','methods','approach','analysis','journal','search',
                                         'procedure','level','information','single','total','proposed',
                                         'meodelling','approaches'))

data_08 <- data_08 %>% 
  anti_join(stop_words) %>%
  anti_join(custom_stop_words, by = c("word" = "word"))


print(slice_head(data_08 %>%
                   count(word) %>%
                   arrange(desc(n)),n=30),n=30)

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




#2012-2015
data("stop_words")
custom_stop_words <- data.frame(word = c('system','systems','based','time','paper','research',
                                         'study','method','results','data','model','models',
                                         'performance','process','solution','method','solutions',
                                         'set','methods','approach','analysis','journal','search',
                                         'procedure','level','information','single','total','proposed',
                                         'meodelling','approaches'))

data_12 <- data_12 %>% 
  anti_join(stop_words) %>%
  anti_join(custom_stop_words, by = c("word" = "word"))

print(slice_head(data_12 %>%
                   count(word) %>%
                   arrange(desc(n)),n=30),n=30)

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


#2016-2019
data("stop_words")
custom_stop_words <- data.frame(word = c('system','systems','based','time','paper','research',
                                         'study','method','results','data','model','models',
                                         'performance','process','solution','method','solutions',
                                         'set','methods','approach','analysis','journal','search',
                                         'procedure','level','information','single','total','proposed',
                                         'meodelling','approaches'))

data_16 <- data_16 %>% 
  anti_join(stop_words) %>%
  anti_join(custom_stop_words, by = c("word" = "word"))


print(slice_head(data_16 %>%
                   count(word) %>%
                   arrange(desc(n)),n=30),n=30)

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



#2020-2023
data("stop_words")
custom_stop_words <- data.frame(word = c('system','systems','based','time','paper','research',
                                         'study','method','results','data','model','models',
                                         'performance','process','solution','method','solutions',
                                         'set','methods','approach','analysis','journal','search',
                                         'procedure','level','information','single','total','proposed',
                                         'meodelling','approaches','article'))

data_20 <- data_20 %>% 
  anti_join(stop_words) %>%
  anti_join(custom_stop_words, by = c("word" = "word"))


print(slice_head(data_20 %>%
                   count(word) %>%
                   arrange(desc(n)),n=30),n=30)

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
