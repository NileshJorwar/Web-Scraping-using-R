#Nilesh Jorwar

rm(list=ls())
setwd("G:")
#install.packages('lubridate')
#install.packages("igraph")
#install.packages("SnowballC")
#install.packages("lubridate")

library(tm)
library(ggmap)
library(ggplot2)
library(twitteR)
library(stringr)
library(wordcloud)
library(lubridate)
library(data.table)
library(SnowballC)
library(httpuv)
library(igraph)

library(stringi)

c_key="3YrMe9Y6q6jCUcTmvWnVZzjS2"
c_secret="Y3fqkyAR2ZRw6Kin4vnzVd2QOnMC1DwLnWks3huO8wPpDHSCko"

setup_twitter_oauth(consumer_key = c_key, consumer_secret = c_secret)
searchTerm ="#Donaldtrump"
tt=searchTwitter(searchTerm, n=1000)
tt.df<-twListToDF(tt)
write.csv(tt.df, file="DonaldTrumph.csv")

dfCorpus = Corpus(VectorSource(tt.df$text))
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
docs <- tm_map(dfCorpus, toSpace, "/")
docs <- tm_map(dfCorpus, toSpace, "@")
docs <- tm_map(dfCorpus, toSpace, "\\|")
docs <- tm_map(docs, removeNumbers)

chunk <- 500
n <- length(dfCorpus)
r <- rep(1:ceiling(n/chunk),each=chunk)[1:n]
d <- split(dfCorpus,r)

for (i in 1:length(d)) {
  docs <- tm_map(docs, removeWords, c(paste(d[[i]])))
}


#docs <- tm_map(docs, removeWords, stopwords("english"))
docs <- tm_map(docs, removePunctuation)
docs <- tm_map(docs, stripWhitespace)
docs <- tm_map(docs, stemDocument)

docs <- tm_map(docs, function(x) iconv(enc2utf8(x), sub = "byte"))
dtm <- TermDocumentMatrix(docs)

m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)

set.seed(1234)

wordcloud(words = d$word, freq = d$freq, min.freq = 1,max.words=200, random.order=FALSE, rot.per=0.35,colors=brewer.pal(8, "Dark2"))

#####2 Feb###
g_full<-make_full_graph(8, directed=FALSE)
plot(g_full)

g_ring<-make_ring(12, directed=FALSE, mutual = FALSE, circular = TRUE)
plot(g_ring)
g_gnp<-sample_gnp(20,0.3, directed=FALSE, loops=FALSE)
plot(g_gnp)
g_gnp<-sample_gnp(20,0.05, directed=FALSE, loops=FALSE)
plot(g_gnp)
