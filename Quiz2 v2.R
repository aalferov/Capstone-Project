library(tm)
library(stringi)
library(RWeka)

setwd("D:/Coursera/Data Science/Capstone Project")

blogs <- stri_read_lines('final/en_US/en_US.blogs.txt',locale = "en")
blogs <- stri_trans_tolower(blogs)
q1 <- blogs[grepl('case', blogs)]

corpus <- Corpus(VectorSource(q1))

FourgramTokenizer <- function(x) 
{
  NGramTokenizer(x, RWeka::Weka_control(min = 4, max = 4))
}

tdm <- TermDocumentMatrix(corpus, control = list(tokenize = FourgramTokenizer))
case <- tdm$dimnames$Terms[grepl('a case of', tdm$dimnames$Terms)]
m = as.matrix(tdm[case, ])
v = sort(rowSums(m), decreasing = TRUE)
print(v)
rm(q1)


q2 <- blogs[grepl('mean', blogs)]

corpus <- Corpus(VectorSource(q2))

tdm <- TermDocumentMatrix(corpus, control = list(tokenize = FourgramTokenizer))
case <- tdm$dimnames$Terms[grepl('would mean the', tdm$dimnames$Terms)]
m = as.matrix(tdm[case, ])
v = sort(rowSums(m), decreasing = TRUE)
print(v)
rm(q2)


q3 <- blogs[grepl('make', blogs)]

corpus <- Corpus(VectorSource(q3))

tdm <- TermDocumentMatrix(corpus, control = list(tokenize = FourgramTokenizer))
case <- tdm$dimnames$Terms[grepl('make me the', tdm$dimnames$Terms)]
m = as.matrix(tdm[case, ])
v = sort(rowSums(m), decreasing = TRUE)
print(v)
rm(q3)

news <- stri_read_lines('final/en_US/en_US.news.txt',locale = "en")
news <- stri_trans_tolower(news)

#tweets <- stri_read_lines('final/en_US/en_US.twitter.txt',locale = "en", encoding = "UTF-8")
conTwitter <- file("final/en_US/en_US.twitter.txt", 'r')
textTwitter <- readLines(conTwitter)
tweets <- stri_trans_tolower(textTwitter)
close(conTwitter)

q4 <- news[grepl('struggling', news)]

corpus <- Corpus(VectorSource(q4))

tdm <- TermDocumentMatrix(corpus, control = list(tokenize = FourgramTokenizer))
case <- tdm$dimnames$Terms[grepl('struggling but the', tdm$dimnames$Terms)]
m = as.matrix(tdm[case, ])
v = sort(rowSums(m), decreasing = TRUE)
print(v)
rm(q4)

case <- q4[grepl('struggling but the', q4)]
case <- tweets[grepl('struggling but the', tweets)]


q5 <- tweets[grepl('date', tweets)]

corpus <- Corpus(VectorSource(q5))

tdm <- TermDocumentMatrix(corpus, control = list(tokenize = FourgramTokenizer))
case <- tdm$dimnames$Terms[grepl('date at the', tdm$dimnames$Terms)]
m = as.matrix(tdm[case, ])
v = sort(rowSums(m), decreasing = TRUE)
print(v)
rm(q5)


q6 <- tweets[grepl('my', tweets)]

corpus <- Corpus(VectorSource(q6))

tdm <- TermDocumentMatrix(corpus, control = list(tokenize = FourgramTokenizer))
case <- tdm$dimnames$Terms[grepl('be on my', tdm$dimnames$Terms)]
m = as.matrix(tdm[case, ])
v = sort(rowSums(m), decreasing = TRUE)
print(v)
rm(q6)


q7 <- tweets[grepl('some', tweets)]

corpus <- Corpus(VectorSource(q7))

tdm <- TermDocumentMatrix(corpus, control = list(tokenize = FourgramTokenizer))
case <- tdm$dimnames$Terms[grepl('in quite some', tdm$dimnames$Terms)]
m = as.matrix(tdm[case, ])
v = sort(rowSums(m), decreasing = TRUE)
print(v)
rm(q7)


q8 <- blogs[grepl('little', blogs)]

corpus <- Corpus(VectorSource(q8))

tdm <- TermDocumentMatrix(corpus, control = list(tokenize = FourgramTokenizer))
case <- tdm$dimnames$Terms[grepl('with his little', tdm$dimnames$Terms)]
m = as.matrix(tdm[case, ])
v = sort(rowSums(m), decreasing = TRUE)
print(v)
rm(q8)


q9 <- news[grepl('faith', news)]

corpus <- Corpus(VectorSource(q9))

tdm <- TermDocumentMatrix(corpus, control = list(tokenize = FourgramTokenizer))
case <- tdm$dimnames$Terms[grepl('faith during the', tdm$dimnames$Terms)]
m = as.matrix(tdm[case, ])
v = sort(rowSums(m), decreasing = TRUE)
print(v)
rm(q9)

q10 <- tweets[grepl('must', tweets)]

corpus <- Corpus(VectorSource(q10))

tdm <- TermDocumentMatrix(corpus, control = list(tokenize = FourgramTokenizer))
case <- tdm$dimnames$Terms[grepl('you must be', tdm$dimnames$Terms)]
m = as.matrix(tdm[case, ])
v = sort(rowSums(m), decreasing = TRUE)
print(v)
rm(q10)


