
setwd("L:/CEB Talent Analytics Team/Conferences/SIOP 2017 Text Mining Master Tutorial Materials/Folder of Attendee Materials/")
load(file = "Picture_Description_Task_Stage2.RData")

data_backup <- data

#####

setwd("L:/CEB Talent Analytics Team/Conferences/SIOP 2017 Text Mining Master Tutorial Materials/Folder of Attendee Materials/Word Lists/")
word_lists <- list.files()


#This loop applies all lists in the directory to all authors
#It creates a count, a binary and normalized variable for each list

for(k in 1:length(word_lists)){ 
  
temp_word_list <- scan(word_lists[k], character(), quote = "")
temp_word_count <- NA

  for(i in 1:nrow(data)){
    temp <- unlist(strsplit(data$all_text_clean[i]," "))
    found <- NA
  
      for (j in 1:length(temp_word_list)){
      found[j] <- sum(temp_word_list[j] == temp)
      }
  
  temp_word_count[i] <- sum(found[1:length(found)])

  }

#add new column to data set
data <- cbind(data,temp_word_count)

#create bianry and normalized versions
data$temp_word_bin <- ifelse(data$temp_word_count > 0, 1, 0)
data$temp_word_normalized <- round(data$temp_word_count/data$word_count,5)

list_name <- gsub("\\.txt","",word_lists[k])

colnames(data)[colnames(data) == "temp_word_count"] <- paste0(list_name,"_count")
colnames(data)[colnames(data) == "temp_word_bin"] <- paste0(list_name,"_bin")
colnames(data)[colnames(data) == "temp_word_normalized"] <- paste0(list_name,"_normalized")

}



setwd("L:/CEB Talent Analytics Team/Conferences/SIOP 2017 Text Mining Master Tutorial Materials/Folder of Attendee Materials/")

save(data, file = "Picture_Task_Stage3.RData")
