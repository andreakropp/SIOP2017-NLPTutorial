#Loading packages

library(tm)
library(ggplot2)
library(topicmodels)
library(dplyr)
library(stringi)
library(LDAvis)
library(wordcloud)

############################################

#Helper functions

#For topic modeling, we don't usually need custom scripts for removing punctuation or tokenizing.
#The tm package is a great existing open-source package for creating a document-term matrix. This
#function accepts a set of documents, removes punctuation, transforms to lower case, removes stop
#words, strips white space, and stems the words

create_cleaned_dtm <- function(d_input) {
  docs <- create_corpus(d_input)
  docs <- tm_map(docs, removePunctuation, preserve_intra_word_dashes = TRUE)
  docs <- tm_map(docs, tolower)
  docs <- tm_map(docs, removeWords, stopwords("english"))
  docs <- tm_map(docs, stripWhitespace)
  docs <- tm_map(docs, PlainTextDocument)
  
  docs <- tm_map(docs, stemDocument)
  dtm <- DocumentTermMatrix(docs)
  dtm
}

#An additional helper function just to create the corpus
create_corpus <- function(d_input) {
  ds <- DataframeSource(d_input)
  VCorpus(ds)
}

#This function generates the JSON files required by LDAvis
#The author of this function is Christopher Gandrud, his full text is available on R-Bloggers here:
#https://www.r-bloggers.com/a-link-between-topicmodels-lda-and-ldavis/

topicmodels_json_ldavis <- function(fitted, corpus, doc_term) {
  
  phi <- posterior(fitted)$terms %>% as.matrix
  theta <- posterior(fitted)$topics %>% as.matrix
  vocab <- colnames(phi)
  doc_length <- vector()
  
  for (i in 1:length(corpus)) {
    temp <- paste(corpus[[i]]$content, collapse = ' ')
    doc_length <- c(doc_length, stri_count(temp, regex = '\\S+'))
  }
  
  temp_frequency <- inspect(doc_term)
  freq_matrix <- data.frame(ST = colnames(temp_frequency),
                            Freq = colSums(temp_frequency))
  
  json_lda <- LDAvis::createJSON(phi = phi, theta = theta,
                                 vocab = vocab,
                                 doc.length = doc_length,
                                 term.frequency = freq_matrix$Freq)
  
  return(json_lda)
}

#######################################################

#Input the data from your drive

setwd("L:/CEB Talent Analytics Team/Conferences/SIOP 2017 Text Mining Master Tutorial Materials")
#data <- read.csv("MTurk Goal Data - NOT SHARED.csv")

#Create a corpus file and a document-term matrix for future use
#In this case, Research.Instrument.003 is the name of the column of text we want to use
corpus <- create_corpus(data.frame(data$Research.Instrument.003))
dtm <- create_cleaned_dtm(data.frame(data$Research.Instrument.003))

#########################################################

#EDA and optional cleaning steps to reduce the number of words
freq <- colSums(as.matrix(dtm))
qplot(freq, main = "Frequency of Remaining Words")

dtm <- removeSparseTerms(dtm, 0.999) #This drops terms that are very infrequently used
freq <- colSums(as.matrix(dtm)) #Regenerates frequency table
freq <- sort(freq, decreasing = TRUE)
qplot(freq, main = "Frequency of Remaining Words")

freq[order(freq, decreasing = TRUE)][1:100] #100 most common remaining words

############################################################

#Running our topic model

#Model parameters
seed <- 189 #Setting the seed ensures reproducibility
best <- TRUE

#Model with 10 groups
k <- 10 #Number of topics you want to be looking for
ldaOut <-LDA(dtm, k, method="Gibbs", control=list(seed = seed, best=best)) #Run the model
json_lda <- topicmodels_json_ldavis(ldaOut, corpus, dtm) #Create the JSON data structure needed for LDAvis
serVis(json_lda, out.dir = 'myvis', open.browser = TRUE) #Launch the visualization

#Rerun the model with 25 groups
k <- 25
ldaOut <-LDA(dtm, k, method="Gibbs", control=list(seed = seed, best=best))
json_lda <- topicmodels_json_ldavis(ldaOut, corpus, dtm)
serVis(json_lda, out.dir = 'myvis', open.browser = TRUE)

#Exporting topics and terms
ldaOut.topics <- as.matrix(topics(ldaOut))
data$topic <- ldaOut.topics #assigns the best topic to each row in the dataframe
ldaOut.terms <- as.matrix(terms(ldaOut,25))
write.csv(ldaOut.terms, "lda terms.csv")

#Example: Finding all cases of a single topic (topic 1)
data[data$topic == 1]

##############################################################

#Word clouds - example 1

#Use the helper function to create a new DTM and frequency table based on only the entries corresponding to that topic
dtm_topic1 <- create_cleaned_dtm(data.frame(data$Research.Instrument.003[data$topic == 1]))
freq_topic1 <- colSums(as.matrix(dtm_topic1))
freq_topic1 <- sort(freq_topic1, decreasing = TRUE)

#Generate word clouds using all text in the matching responses
#In this case, the wordcloud is generated off of the frequency of words in all entries in a topic
png(filename = "wordcloud_topic1.png", width = 1280, height = 800, pointsize = 28)
wordcloud(words = names(freq_topic1), freq = freq_topic1, min.freq = 1, scale = c(3, .5),
          max.words = 150, random.order = FALSE, rot.per = 0.1,
          colors = brewer.pal(10, "Spectral"))
dev.off()

#Word clouds - example 2
word_weights <- phi[,2] #A topic is described by words. Phi stores the relative weights for each word.
word_weights <- sort(word_weights, decreasing = TRUE)

#Generate a word cloud showing only the words that defined a topic
#Using phi as input means that you're showing only the "important" words that describe a given topic
png(filename = "wordcloud_topic1_2.png", width = 1280, height = 800, pointsize = 28)
wordcloud(words = names(word_weights), freq = word_weights, min.freq = 1, scale = c(3, .5),
          max.words = 150, random.order = FALSE, rot.per = 0.1,
          colors = brewer.pal(10, "Spectral"))
dev.off()
