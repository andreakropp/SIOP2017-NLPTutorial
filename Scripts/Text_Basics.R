library(stringr)
library(qdap)
library(tm)

setwd("L:/CEB Talent Analytics Team/Conferences/SIOP 2017 Text Mining Master Tutorial Materials/Folder of Attendee Materials/")
load(file = "Picture_Description_Task_Clean.RData")



###########PART 3: TEXT CLEANING

#copy the raw text into a new field
#the rest of this script will fully clean the copy
data$all_text_clean <- data$images_combined 


#Detect sentance boundaries using function from qdap package
sentences <- lapply(data$images_combined,sent_detect)
data$sent_count <- sapply(sentences,length)


#remove system-generated characters \n\n
data$all_text_clean <- gsub("\\n\\n","", data$all_text_clean, perl = TRUE) 
data$all_text_clean <- gsub("\\n"," ", data$all_text_clean, perl = TRUE) 

#remove system-generated characters &quot;
data$all_text_clean <- gsub("&quot;","", data$all_text_clean, perl = TRUE) 

#replace symbols with meaning
data$all_text_clean <- gsub("&"," and ", data$all_text_clean, perl = TRUE) 
data$all_text_clean <- gsub("%"," percent ", data$all_text_clean, perl = TRUE) 
data$all_text_clean <- gsub("$"," dollar ", data$all_text_clean, perl = TRUE) 
data$all_text_clean <- gsub("\\+"," plus ", data$all_text_clean, perl = TRUE)

#remove between word dashes
data$all_text_clean <- gsub(" - "," ", data$all_text_clean, perl = TRUE) 

#remove all punctuation except apostrophes, intra-word dashes, ampersand, dollar sign and percent sign
data$all_text_clean <- gsub("[^[:alnum:]['-]", " ", data$all_text_clean, perl = TRUE) 

#this version will remove apostrophes which I prefer to keep
#data$all_text_clean <- removePunctuation(data$all_text_clean, preserve_intra_word_dashes = TRUE) #this version will remove apostrophes

#remove double whitespaces
data$all_text_clean <- gsub("  *", " ", data$all_text_clean, perl = TRUE)

#convert all text to upper case
data$all_text_clean <- toupper(data$all_text_clean)

#remove final space
data$all_text_clean <- gsub(" $", "", data$all_text_clean)



#convert text to a list of words
data$word_by_word <- str_split(data$all_text_clean, " ")

#count characters by taking total length and substracting white spaces
data$char_count <- nchar(data$all_text_clean) - str_count(data$all_text_clean," ")


#count words based on detecting word boundaries. The \w metacharacter is used to find a word character.
#A word character is a character from a-z, A-Z, 0-9, including the _ (underscore) character.
#data$word_count <- str_count(data$all_text_clean, "\\w+")
data$word_count <- str_count(data$all_text_clean, " ") + 1



######START PUNCTUATION SECTION###########

data$comma <- str_count(data$images_combined, ",")
data$exclamations <- str_count(data$images_combined, "!")
data$right_parentheses <- str_count(data$images_combined, "\\(")
data$left_parentheses <- str_count(data$images_combined, "\\)")
data$double_quotes <- str_count(data$images_combined, '\\"')
data$single_quotes <- str_count(data$images_combined, "\\'")
data$hyphen <- str_count(data$images_combined, '-')
data$period <- str_count(data$images_combined, '\\.')
data$dollar_sign <- str_count(data$images_combined, '\\$')
data$per_cent <- str_count(data$images_combined, '%')
data$at_sign <- str_count(data$images_combined, '@')
data$ampersand <- str_count(data$images_combined, '&')
data$pound <- str_count(data$images_combined, '#')
data$plus_sign <- str_count(data$images_combined, '\\+')
data$equals_sign <- str_count(data$images_combined, '=')
data$colon <- str_count(data$images_combined, ':')
data$semicolon <- str_count(data$images_combined, ';')
data$ellipses <- str_count(data$images_combined, "\\.\\.\\.")


#Calculate normalized punctuation counts
data$exclamations_normalized <- round(data$exclamations/data$word_count,2)
data$right_parentheses_normalized <- round(data$right_parentheses/data$word_count,2)
data$left_parentheses_normalized <- round(data$left_parentheses/data$word_count,2)
data$double_quotes_normalized <- round(data$double_quotes/data$word_count,2)
data$single_quotes_normalized <- round(data$single_quotes/data$word_count,2)
data$hyphen_normalized <- round(data$hyphen/data$word_count,2)
data$period_normalized <- round(data$period/data$word_count,2)
data$dollar_sign_normalized <- round(data$dollar_sign/data$word_count,2)
data$per_cent_normalized <- round(data$per_cent/data$word_count,2)
data$at_sign_normalized <- round(data$at_sign/data$word_count,2)
data$ampersand_normalized <- round(data$ampersand/data$word_count,2)
data$pound_normalized <- round(data$pound/data$word_count,2)
data$plus_sign_normalized <- round(data$plus_sign/data$word_count,2)
data$equals_sign_normalized <- round(data$equals_sign/data$word_count,2)
data$colon_normalized <- round(data$exclamations/data$word_count,2)
data$semicolon_normalized <- round(data$semicolon/data$word_count,2)
data$ellipses_normalized <- round(data$ellipses/data$word_count,2)


######READABILITY SECTION###########


#count words with 5+, 6+, 7+ and 8+ characters
data$char_5plus <- NA
data$char_6plus <- NA
data$char_7plus <- NA
data$char_8plus <- NA

for(i in 1:nrow(data)){
  temp <- unlist(data$word_by_word[i])
  data$char_5plus[i] <- sum(nchar(temp) >= 5)
  data$char_6plus[i] <- sum(nchar(temp) >= 6)
  data$char_7plus[i] <- sum(nchar(temp) >= 7)
  data$char_8plus[i] <- sum(nchar(temp) >= 8)
  }
  

#count total syllables
#syllable_sum("This is interesting") #sample
#polysyllable_sum("This is interesting") #sample

data$syllables <- syllable_sum(data$all_text_clean)
data$polysyllables <- polysyllable_sum(data$all_text_clean)
  
  

#generate list of unique words used
#unique_words_list <- unique(unlist(data$word_by_word))  #actual word list

#count number of unique words used
data$unique_word_count <- NA
for (i in 1:nrow(data)){
  data$unique_word_count[i] <- length(unique(unlist(data$word_by_word[i])))  #count
}

data$word_by_word <- NULL  #very large and no longer needed

#count number of misspelled words
data$misspelling_count <- NA
for(i in 1:nrow(data)){
  data$misspelling_count[i] <- length(which_misspelled(data$all_text_clean[i]))
}

#Calculate readability metrics
data$avg_len <- data$char_count / data$word_count  #average word length
data$syll_per_word <- data$syllables / data$word_count   #average syllables per word
data$word_per_sent <- data$word_count / data$sent_count   #average words per sentance
data$syll_per_sent <- data$syllables / data$sent_count   #average syllables per sentance

data$flesch_kincaid_level <- 0.39*data$word_per_sent + (11.8*data$syll_per_word)-15.59
data$flesch_kincaid_level[data$word_per_sent > 100] <- NA  #NA if sentances are longer than 100 words

data$flesch_kincaid_ease <- 206.835-1.015*data$word_per_sent - 84.6*data$syll_per_word
data$flesch_kincaid_ease[data$word_per_sent > 100] <- NA #NA if sentances are longer than 100 words

data$coleman_liau <- 0.0588*(100*data$avg_len)-0.296*(100/data$word_per_sent)-15.8
data$coleman_liau[data$word_per_sent > 100] <- NA #NA if sentances are longer than 100 words

data$automated_readability <- 4.71*data$avg_len+0.5*data$word_per_sent-21.43
data$automated_readability[data$word_per_sent > 100] <- NA #NA if sentances are longer than 100 words

data$gunningfog <- 0.4*(data$word_per_sent + 100*data$polysyllables/data$word_count)
data$gunningfog[data$word_per_sent > 100] <- NA #NA if sentances are longer than 100 words

data$smog <- 1.0430*sqrt(data$polysyllables*30/data$sent_count)+3.1291
data$smog[data$word_per_sent > 100] <- NA #NA if sentances are longer than 100 words



#Calculate normalized counts
data$unique_word_count_normalized <- round(data$unique_word_count/data$word_count,2)
data$misspelling_normalized <- round(data$misspelling_count/data$word_count,2)
data$char_5plus_normalized <- round(data$char_5plus/data$word_count,2)
data$char_6plus_normalized <- round(data$char_6plus/data$word_count,2)
data$char_7plus_normalized <- round(data$char_7plus/data$word_count,2)
data$char_8plus_normalized <- round(data$char_8plus/data$word_count,2)




save(data, file = "Picture_Description_Task_Stage1.RData")
