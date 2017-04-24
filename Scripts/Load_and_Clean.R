rm(list=ls()) ## WARNING: This clears your workspace



#Update this to match your actual file location
dir <- "L:/CEB Talent Analytics Team/Conferences/SIOP 2017 Text Mining Master Tutorial Materials/Folder of Attendee Materials/"
setwd(dir)


#PART 1: Load the Raw Data
data <- read.csv("Picture_Description_Task.csv")


#PART 2: Check data types and make decisions about missing or outlier values

#Age: No changes
table(data$age) 

#Gender: 'Prefer not to Answer' and blank are both mapped to Unknown
table(data$gender) 
data$gender <- as.character(data$gender)
data$gender[data$gender == ""] <- "Unknown"
data$gender[data$gender == "Prefer not to Answer"] <- "Unknown"
data$gender <- as.factor(data$gender)
table(data$gender)

#Race: 'blank' is mapped to Unknown
table(data$race) 
data$race <- as.character(data$race)
data$race[data$race == ""] <- "Unknown"
data$race <- as.factor(data$race)
table(data$race) 

#English Proficiency: 'blank' is mapped to Unknown
table(data$english_proficiency)
data$english_proficiency <- as.character(data$english_proficiency)
data$english_proficiency[data$english_proficiency == ""] <- "Unknown"
data$english_proficiency <- as.factor(data$english_proficiency)
table(data$english_proficiency)

#Highest Level of Education: 'blank' is mapped to Unknown; Levels are assigned an order
table(data$highest_level_of_education)
data$highest_level_of_education <- as.character(data$highest_level_of_education)
data$highest_level_of_education[data$highest_level_of_education == ""] <- "Unknown"
data$highest_level_of_education <- factor(data$highest_level_of_education, levels = c("Unknown","Some Secondary or Less","Completed Secondary School","Some College (2 years or less)","Completed technical/associate degree/certificate/diploma","Some College (3 or 4 years)","Bachelor's Degree","Master's Degree","Doctorate","Professional Degree (MD, DDS, DVM, LB, JD)"), ordered = TRUE)
table(data$highest_level_of_education)

#College Major: 'blank' is mapped to Unknown
table(data$major_dept_college)
data$major_dept_college <- as.character(data$major_dept_college)
data$major_dept_college[data$major_dept_college == ""] <- "Unknown"
data$major_dept_college <- as.factor(data$major_dept_college)
table(data$major_dept_college)

#GPA: 'blank' is mapped to Unknown; Levels are assigned an order
table(data$college_gpa)
data$college_gpa <- as.character(data$college_gpa)
data$college_gpa[data$college_gpa == ""] <- "Unknown"
data$college_gpa <- factor(data$college_gpa, levels = c("Unknown","I did not attend college","My school doesn't use this scale","Less than 2.0","2.0 to 2.5","2.5 to 3.0","3.0 to 3.3","3.3 to 3.7","3.7 to 4.0"), ordered = TRUE)
table(data$college_gpa)

#Essay Date: 'blank' is mapped to Missing; Date is refomatted
table(data$essay_started_on)
data$essay_started_on <- as.character(data$essay_started_on)
data$essay_started_on[data$essay_started_on == ""] <- NA
data$essay_started_on <- as.Date(data$essay_started_on, format = "%m/%d/%Y")
table(data$essay_started_on)

#Essay Date: 'blank' is mapped to Missing; Date is refomatted
table(data$essay_completed_on)
data$essay_completed_on <- as.character(data$essay_completed_on)
data$essay_completed_on[data$essay_completed_on == ""] <- NA
data$essay_completed_on <- as.Date(data$essay_completed_on, format = "%m/%d/%Y")
table(data$essay_completed_on)

#Essay Time: Looks good
table(data$essay_time_taken) 


#Assessment Time: Date is refomatted
table(data$verify_started_on)
data$verify_started_on <- as.character(data$verify_started_on)
data$verify_started_on <- as.Date(data$verify_started_on, format = "%m/%d/%Y")
table(data$verify_started_on)


#Assessment Time: Date is refomatted
table(data$verify_completed_on)
data$verify_completed_on <- as.character(data$verify_completed_on)
data$verify_completed_on <- as.Date(data$verify_completed_on, format = "%m/%d/%Y")
table(data$verify_completed_on)


#Assessment Time: Looks clean, but many values suspiciously low or high
table(data$verify_time_taken) 


#Assessment Percentile Score; Looks good
table(data$deductive_reasoning_ability_general_population) 
hist(data$deductive_reasoning_ability_general_population)  


#Image description verbatim responses refomatted as character
data$image_1 <- as.character(data$image_1)
data$image_2 <- as.character(data$image_2)


#Responses to two images combined into single field
data$images_combined <- paste(data$image_1, data$image_2)


save(data, file = "Picture_Description_Task_Clean.RData")

