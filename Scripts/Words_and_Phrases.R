library(NLP)

setwd("L:/CEB Talent Analytics Team/Conferences/SIOP 2017 Text Mining Master Tutorial Materials/Folder of Attendee Materials/")
load(file = "Picture_Description_Task_Stage1.RData")

data_backup <- data


#create a list of all single words used
unigrams <- unlist(strsplit(data$all_text_clean, " ", fixed = TRUE))
head(unigrams, 40)
length(unigrams)


#create a list of all unique single words used
unique_unigrams <- sort(unique(unigrams))
head(unique_unigrams, 40)
length(unique_unigrams)


#loop to count instances of each unique word
global_unigram_count <- NA
for (i in 1:length(unique_unigrams)){
  global_unigram_count[i] <- sum(unigrams == unique_unigrams[i])
}

#data frame of unigram results
global_unigrams <- data.frame(unique_unigrams,global_unigram_count)
colnames(global_unigrams) <- c("ngram","count")
global_unigrams$length <- "1gram"


write.csv(global_unigrams, "global_unigrams.csv")


unigrams <- NULL
unique_unigrams <- NULL


#Look for ngrams which exceed X percent usage
global_ngrams_use <- as.character(global_unigrams$ngram[global_unigrams$count > .05*nrow(data)])


#create data frame containing ngram counts 
#nested loop says for each person, for each word, do this....

ngram_by_author <- data.frame()
 
for (i in 1:nrow(data)){
  
  temp <- unlist(strsplit(data$all_text_clean[i], " ", fixed = TRUE))
  newrow <- data$applicants_id[i]
  
  for (j in 1:length(global_ngrams_use)){
    value <- sum(temp == global_ngrams_use[j])
    newrow <- cbind(newrow,value)
  }
  
  ngram_by_author <- rbind(ngram_by_author,newrow)
  
}

#add column names 
colnames(ngram_by_author) <- c("applicant_id",paste0("ngram_",global_ngrams_use)) 


#add underscores to fill whitespace in variable names
colnames(ngram_by_author)  <- gsub(" ","_",colnames(ngram_by_author))


#convert all to binary; used the word or did not use the word
for (i in 2:ncol(ngram_by_author)){
temp <- ifelse(ngram_by_author[i] >= 1,1,0)  #generate binary version
colnames(temp) <- paste0(colnames(ngram_by_author[i]),"_bin")   #add _bin to name
ngram_by_author <- cbind(ngram_by_author,temp)  #append to data set
}


#bind word counts to main data set
data <- cbind(data, ngram_by_author)


#convert counts to normalized continuous values IF
#25th percentile >= 1 (means that 25% of authors use it at least once)
sum(sort(apply(ngram_by_author,2,function (x) quantile(x, probs = .25))) >=1)


to_normalize <- apply(ngram_by_author,2,function (x) quantile(x, probs = .25))
to_normalize <- to_normalize[to_normalize >=1]
to_normalize <- to_normalize[grepl("_bin$",names(to_normalize))==FALSE]
to_normalize <- to_normalize[grepl("_id$",names(to_normalize))==FALSE]



#normalize by dividing by word count

for (i in 1:length(to_normalize)){
  col_num <- which(colnames(data) == names(to_normalize[i]))  #find correct column number based on name match
  temp <- round(data[col_num]/data$word_count,3)  #divide by word count
  colnames(temp) <- paste0(names(to_normalize[i]),"_normalized")   #add _normalized to name
  data <- cbind(data,temp)  #append to data set
}



save(data, file = "Picture_Description_Task_Stage2.RData")
write.csv(data, "data_stage2.csv")


