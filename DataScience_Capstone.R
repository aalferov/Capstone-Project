#Getting Data
library(tm)
library(ggplot2)
library(R.utils)
library(stringi)
library(RWeka)
library(knitr)
library(qdapRegex)

#Download and unzip source file.
setwd("D:/Coursera/Data Science/Capstone Project") #Path should be replaced by your own when reproducing the analysis

url  <- "http://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip"
fileName <- "coursera-swiftkey.zip"
if (!file.exists(fileName)){
  download.file(url = url, destfile = fileName)
  filelist=c("final/en_US/en_US.twitter.txt", "final/en_US/en_US.news.txt","final/en_US/en_US.blogs.txt") 
  unzip(fileName, files = filelist, exdir = "en_US", overwrite = TRUE, junkpaths = TRUE)  
}


#Load text files in memory.
conBlogs <- file("final/en_US/en_US.blogs.txt", 'r')
conNews <- file("final/en_US/en_US.news.txt", 'r')
conTwitter <- file("final/en_US/en_US.twitter.txt", 'r')
textBlogs <- readLines(conBlogs)
textNews <- readLines(conNews)
textTwitter <- readLines(conTwitter)
close(conBlogs)
close(conNews)
close(conTwitter)


# get size of files
fsBlogs <- file.info("final/en_US/en_US.blogs.txt")$size
fsNews <- file.info("final/en_US/en_US.news.txt")$size
fsTwitter <- file.info("final/en_US/en_US.twitter.txt")$size
fileSize <- c(fsBlogs, fsNews, fsTwitter)

# calculate line counts
lcBlogs <- length(textBlogs)
lcNews <- length(textNews)
lcTwitter <- length(textTwitter)
lineCount <- c(lcBlogs, lcNews, lcTwitter)

# calculate word counts
wcBlogs <- sum(stri_count_words(textBlogs))
wcNews <- sum(stri_count_words(textNews))
wcTwitter <- sum(stri_count_words(textTwitter))
wordCount <- c(wcBlogs, wcNews, wcTwitter)

info <- data.frame(row.names=c("en_US.blogs.txt", "en_US.news.txt", "en_US.twitter.txt"), FileSize = fileSize, LineCount = lineCount, WordCount = wordCount)
kable(info, format="markdown")

##Cleaning and Preprocessing Data
textBlogs <- sample(textBlogs, 1000)
textNews <- sample(textNews, 1000)
textTwitter <- sample(textTwitter, 1000)

# convert to "ASCII" encoding to remove weird characters
textBlogs <- iconv(textBlogs, to="ASCII", sub="")
textNews <- iconv(textNews, to="ASCII", sub="")
textTwitter <- iconv(textTwitter, to="ASCII", sub="")

# combine text arrays into a single one 
textUnited <- paste(textBlogs, textNews, textTwitter)

textUnited = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", textUnited)
textUnited = gsub("@\\w+", "", textUnited)
textUnited = gsub("http\\w+", "", textUnited)
textUnited <- rm_emoticon(textUnited)
textUnited <- rm_hash(textUnited)
textUnited <- rm_url(textUnited)

textCorpus <- Corpus(VectorSource(textUnited))

textCorpus <- tm_map(textCorpus, stripWhitespace)
textCorpus <- tm_map(textCorpus, removeNumbers)
textCorpus <- tm_map(textCorpus, removePunctuation)
textCorpus <- tm_map(textCorpus, PlainTextDocument)
textCorpus <- tm_map(textCorpus, content_transformer(tolower))

# download the list of profanity words
url <- "https://raw.githubusercontent.com/shutterstock/List-of-Dirty-Naughty-Obscene-and-Otherwise-Bad-Words/master/en"
fileName <- "profanity.txt"

if (!file.exists(fileName))
download.file(url, destfile=fileName)

profanityWords <- readLines(fileName)

# filter them out of the corpus
textCorpus <- tm_map(textCorpus, removeWords, profanityWords)

## Exploratory Analysis
unigramTokenizer <- function (text)
{
  NGramTokenizer(text, Weka_control(min = 1, max = 1))
}

bigramTokenizer <- function (text)
{
  NGramTokenizer(text, Weka_control(min = 2, max = 2))
}

trigramTokenizer <- function (text)
{
  NGramTokenizer(text, Weka_control(min = 3, max = 3))
}

#Build document-term matrices based on unigram, bigram and trigram tokenizer, e.g. matricies containing frequencies of one, two, free grams appearance in the text corpus
dtmUnigram <- DocumentTermMatrix(textCorpus, control=list(tokenize=unigramTokenizer))
dtmBigram <- DocumentTermMatrix(textCorpus, control=list(tokenize=bigramTokenizer))
dtmTrigram <- DocumentTermMatrix(textCorpus, control=list(tokenize=trigramTokenizer))

#Convert the document-term matrices to normal matrices, sum up all the terms (words) and sort them.

unigramFrequency  <- sort(colSums(as.matrix(dtmUnigram)), decreasing=TRUE)
dfUnigram <- data.frame(word=names(unigramFrequency), freq=unigramFrequency, stringsAsFactors = FALSE)

bigramFrequency <- sort(colSums(as.matrix(dtmBigram)), decreasing=TRUE)
dfBigram <- data.frame(word=names(bigramFrequency), freq=bigramFrequency, stringsAsFactors = FALSE)

trigramFrequency <- sort(colSums(as.matrix(dtmTrigram)), decreasing=TRUE)
dfTrigram <- data.frame(word=names(trigramFrequency), freq=trigramFrequency, stringsAsFactors = FALSE)

#Show the frequinces distribution for top 20 most frequent unigrams, bigrams and trigrams.

barplot(unigramFrequency[1:20], ylab='frequency', main='top 20 most frequent unigrams', names.arg=names(unigramFrequency)[1:20],        
col="red", las=2, cex.names=.7)

barplot(bigramFrequency[1:20], ylab='frequency', main='top 20 most frequent bigrams', names.arg=names(bigramFrequency)[1:20],        
col="green", las=2, cex.names=.7)

barplot(trigramFrequency[1:20], ylab='frequency', main='top 20 most frequent trigrams', names.arg=names(trigramFrequency)[1:20],        
col="blue", las=2, cex.names=.7)


