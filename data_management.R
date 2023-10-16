#team 1, loading in dataset (everyone will probably need diff. load lines, unless someone can get the excel data to work in github)

library(readxl)
#caleb load line
HHSNeedsAssessment_FINAL_DATA <- read_excel("~/Desktop/Folder/University/Junior Year/F23/QAC380/HHSNeedsAssessment FINAL DATA.xlsx")

#sam load line
> Copy_of_HHSNeedsAssessment_FINAL_DATA <- read_excel("~/Desktop/Desktop - Samantha’s MacBook Air/Wesleyan Semesters/Fall 2023/QAC380/Copy of HHSNeedsAssessment FINAL DATA.xlsx")


require(tidyverse)

#taking a subset of the data
myvars <- c("Record ID", "Survey Timestamp...3","Complete?...4" ,"Age", "What is your current gender identity?", "Other gender identity",
            "Which of these best describes your sexual orientation?", "Other sexual orientation",
            "What is your race? (choice=White)", "What is your race? (choice=Black or African American)",
            "What is your race? (choice=American Indian or Alaska Native)", 
            "What is your race? (choice=Native Hawaiian or other Pacific Islander)",
            "What is your race? (choice=Asian)",
            "Other race","Are you Hispanic/Latinx?", "What is the highest education level you have completed?",
            "Biggest health concern", "How could clinics help","Which clinic are currently visiting?"
            )
HHS <-HHSNeedsAssessment_FINAL_DATA[myvars]

#replacing NA values with Open Door Health for clinic name 
HHS$`Which clinic are currently visiting?`[is.na(HHS$`Which clinic are currently visiting?`)] <- "Open Door Health"

#next steps for data management here
