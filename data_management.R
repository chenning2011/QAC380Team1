#team 1, loading in dataset (everyone will probably need diff. load lines, unless someone can get the excel data to work in github)

library(readxl)
#caleb load line
HHSNeedsAssessment_FINAL_DATA <- read_excel("~/Desktop/Folder/University/Junior Year/F23/QAC380/HHSNeedsAssessment FINAL DATA.xlsx")

require(ggplot2)
require(dplyr)
require(tidyr)
require(descr)
require(gert)

#working on subsetting
myvars <- c("Record ID", "Age", "What is your current gender identity?", "Other gender identity",
            "Which of these best describes your sexual orientation?", "Other sexual orientation",
            "What is your race? (choice=White)", "What is your race (choice=Black or African American)",
            "What is your race? (choice=American Indian or Alaska Native)", 
            "What is your race? (choice=Native Hawaiian or other Pacific Islander)",
            "What is your race? (choice=Asian)", "What is your race? (choice=Other (Please specify):{other_race})",
            "Other race", "What is the highest education level you have completed?",
            )