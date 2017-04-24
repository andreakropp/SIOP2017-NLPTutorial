
setwd("L:/CEB Talent Analytics Team/Conferences/SIOP 2017 Text Mining Master Tutorial Materials/Folder of Attendee Materials/")
load(file = "Picture_Task_Stage3.RData")

outcome <- "deductive_reasoning_ability_general_population"
outcome_col_number <- which(colnames(data) == outcome)

#different variations of predictor lists to select
normalized_predictors <- which(grepl("normalized", colnames(data)))
ngram_predictors <- which(grepl("ngram", colnames(data)))
readability_predictors <- which(grepl("(char_|syllables|word_count|misspelling|avg_len|per_word|per_sent|flesch|coleman|readability|gunningfog|smog)", colnames(data)))
binary_predictors <- which(grepl("_bin", colnames(data)))

#assemble your custom list by keeping uniques across the lists above
predictor_col_numbers <- sort(unique(c(normalized_predictors,ngram_predictors,readability_predictors,binary_predictors)))

#create the correlations and add a second column for the absolute value
cor_output <- cor(data[,predictor_col_numbers],data[,outcome_col_number], use = "pairwise.complete.obs" , method = "pearson")
colnames(cor_output) <- "pearsonR"
cor_output <- data.frame(cor_output)
cor_output$abs_pearsonR <- abs(cor_output$pearsonR)

write.csv(cor_output, "deductive_correlations.csv")

