library(stats)

setwd("L:/CEB Talent Analytics Team/Conferences/SIOP 2017 Text Mining Master Tutorial Materials/Folder of Attendee Materials/")
load(file = "Picture_Task_Stage3.RData")

outcome <- "deductive_reasoning_ability_general_population"
outcome_col_number <- which(colnames(data) == outcome)

groups <- "major_dept_college"
groups_col_number <- which(colnames(data) == groups)


#create aggregate means, sd and count
agg_mean <- aggregate(data[,outcome_col_number], by = list(data[,groups_col_number]), FUN = mean)
agg_sd <- aggregate(data[,outcome_col_number], by = list(data[,groups_col_number]), FUN = sd)
agg_count <- aggregate(data[,outcome_col_number], by = list(data[,groups_col_number]), FUN = length)


#merge into single data frame
agg_data <- merge(agg_mean,agg_sd, by = "Group.1")
agg_data <- merge(agg_data,agg_count, by = "Group.1")
colnames(agg_data) <- c(groups,"mean_outcome","sd_outcome","count")



#different variations of language feature lists to select
normalized_predictors <- which(grepl("normalized", colnames(data)))
ngram_predictors <- which(grepl("ngram", colnames(data)))
readability_predictors <- which(grepl("(char_|syllables|word_count|misspelling|avg_len|per_word|per_sent|flesch|coleman|readability|gunningfog|smog)", colnames(data)))
binary_predictors <- which(grepl("_bin", colnames(data)))


#assemble your custom list by keeping uniques across the lists above
predictor_col_numbers <- sort(unique(c(readability_predictors)))


#calculate group means for each language feature and append to data frame

for (i in 1:length(predictor_col_numbers)){
  agg_mean_temp <- aggregate(data[,predictor_col_numbers[i]], by = list(data[,groups_col_number]), FUN = mean, na.rm=TRUE)
  agg_data <- merge(agg_data,agg_mean_temp, by.x = groups, by.y = "Group.1")
}

#add column names
colnames(agg_data) <- c(groups,"mean_outcome","sd_outcome","count",colnames(data[,predictor_col_numbers]))
       
                 
write.csv(agg_data, "means_by_field_of_study.csv")   

#sample boxplot
boxplot(data$word_count ~ data$major_dept_college, main = "Picture Task Word Count by College Major", cex.axis=0.3)
 