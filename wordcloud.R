# empty environment 
rm(list = ls())

# set working directory
setwd("~/R Programming/sentiment labelled sentences/sentiment labelled sentences/")

# import dataset
dat <- read.table("imdb_labelled.txt", fill = T, na.strings = c("", " ", NA),
                  sep = "\t")

# import libraries 
library(tm)
library(wordcloud)
library(RSentiment)
library(slam)
library(stringr)

# convert factor variable to character 
dat$V1 <- as.character(dat$V1)

#convert names into corpus
postcorpus <- Corpus(VectorSource(dat$V1))

# convert all words to lowercase 
corpusClean <- tm_map(postcorpus, tolower)

#remove stopwords and words with high frequency which are not related to output
corpusClean <- tm_map(corpusClean, removeWords,c(stopwords('english')))

#remove numbers
corpusClean <- tm_map(corpusClean, removeNumbers)

#remove punctuations
corpusClean <- tm_map(corpusClean, removePunctuation)

#remove white spaces 
corpusClean <- tm_map(corpusClean, stripWhitespace)

#convert document to plain text format
corpusClean <- tm_map(corpusClean, PlainTextDocument)
corpusClean <- Corpus(VectorSource(corpusClean))

#build  wordcloud
pal2 = brewer.pal(6,"Dark2")
png("wordcloud1.png", width = 12, height = 8, units = 'in', res = 300)
wordcloud(corpusClean,random.order = F,max.words = 100 , colors = pal2)      
dev.off()

#Build document term matrix
tdm <- TermDocumentMatrix(corpusClean)

#calculate the terms frequency
words_freq <- rollup(tdm, 2, na.rm=TRUE, FUN = sum)
words_freq <- as.matrix(words_freq)
words_freq <- data.frame(words_freq)
words_freq$words <- row.names(words_freq)
row.names(words_freq) <- NULL
words_freq <- words_freq[,c(2,1)]
names(words_freq) = c("Words", "Frequency")

#wordcloud(words_freq$Words, words_freq$Frequency)

#Most frequent terms which appears in atleast 700 times
findFreqTerms(tdm)

