#team 1, loading in dataset (everyone will probably need diff. load lines, unless someone can get the excel data to work in github)

library(readxl)
#caleb load line
HHSNeedsAssessment_FINAL_DATA <- read_excel("~/Desktop/Folder/University/Junior Year/F23/QAC380/HHSNeedsAssessment FINAL DATA.xlsx")

#sam load line 
HHSNeedsAssessment_FINAL_DATA <- read_excel("~/Desktop/Desktop - Samantha’s MacBook Air/Wesleyan Semesters/Fall 2023/QAC380/Copy of HHSNeedsAssessment FINAL DATA.xlsx")

#dean load line
HHSNeedsAssessment_FINAL_DATA <- read_excel("HHSNeedsAssessment FINAL DATA.xlsx", +     sheet = "HHSNeedsAssessmentSu_DATA_LABEL")

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

#data management for gender to combine columns  
HHS <- mutate(HHS, `Gender Identity` = ifelse(`What is your current gender identity?`=="Other (Please specify): {other_gender_identity}", `Other gender identity`, `What is your current gender identity?`) )

#data management for sexuality to combine columns 
HHS <- mutate(HHS, `Sexual Orientation` = ifelse(`Which of these best describes your sexual orientation?`=="Other (Please specify): {other_sexual_orientation}", `Other sexual orientation`, `Which of these best describes your sexual orientation?`) )

#changing all of the names for beau
subvars <- c("Record ID", "Survey Timestamp...3","Complete?...4" ,"Age", 
             "What is your race? (choice=White)", "What is your race? (choice=Black or African American)",
             "What is your race? (choice=American Indian or Alaska Native)", 
             "What is your race? (choice=Native Hawaiian or other Pacific Islander)",
             "What is your race? (choice=Asian)","Other race",
             "Are you Hispanic/Latinx?", "What is the highest education level you have completed?",
             "Biggest health concern", "How could clinics help","Which clinic are currently visiting?",
             "Gender Identity", "Sexual Orientation")
HHS <- HHS[subvars]
names(HHS) <- c("Record ID", "Survey timestamp", "Complete?", "Age", "White", "Black", "Native American",
           "Native Hawaiian", "Asian", "Other race", "Hispanic/Latinx", "Education level",
           "Biggest health concern", "How could clinics help", "Clinic", "Gender", "Sexuality")

#removing all invalid responses - will update with new variable names after Beau does race variable
Missing <- is.na(HHS$Age) & is.na(HHS$`Hispanic/Latinx`) & is.na(HHS$`Education level`)
HHS <- subset(HHS, subset=!Missing)

#creating a results vector which will function as the new variable
HHS$Race <- numeric(nrow(HHS))

#recoding checked/unchecked to 1/0 for each variable
RWhite <- as.numeric(HHS$White == "Checked")
RBlack <- as.numeric(HHS$Black == "Checked")
RNative_American <- as.numeric(HHS$"Native American" == "Checked")
RNative_Hawaiian <- as.numeric(HHS$"Native Hawaiian" == "Checked")
RAsian <- as.numeric(HHS$Asian == "Checked")
RLatin <- as.numeric(HHS$'Hispanic/Latinx' == "Yes")
#this loop is the most disgusting thing I've ever coded, but I can't figure out dictionaries in R and this works
#basically it checks if 1, and only 1 of the questions is checked, then codes a number 2-6 correlating to the checked variable
# 0 = more than one checked, 2 = white, 3 = African american 4 = Native American, 5 = Native Hawaiian, 6 = Asian, 7 = none checked. 

for (i in 1:nrow(HHS)) {
  combined_var <- ifelse((RWhite[i] == 1 & RBlack[i] == 0 & RNative_American[i] == 0 & RNative_Hawaiian[i] == 0 & RAsian[i] == 0) |
                           (RWhite[i] == 0 & RBlack[i] == 1 & RNative_American[i] == 0 & RNative_Hawaiian[i] == 0 & RAsian[i] == 0) |
                           (RWhite[i] == 0 & RBlack[i] == 0 & RNative_American[i] == 1 & RNative_Hawaiian[i] == 0 & RAsian[i] == 0) | 
                           (RWhite[i] == 0 & RBlack[i] == 0 & RNative_American[i] == 0 & RNative_Hawaiian[i] == 1 & RAsian[i] == 0) |
                           (RWhite[i] == 0 & RBlack[i] == 0 & RNative_American[i] == 0 & RNative_Hawaiian[i] == 0 & RAsian[i] == 1), 1, 0)
  if (RWhite[i] == 1 & combined_var == 1) {
    HHS$Race[i] <- 1 }
  if (RBlack[i] == 1 & combined_var == 1) {
    HHS$Race[i] <- 2 }
  if (RNative_American[i] == 1 & combined_var == 1) {
    HHS$Race[i] <- 3 }
  if (RNative_Hawaiian[i] == 1 & combined_var == 1) {
    HHS$Race[i] <- 4 }
  if (RAsian[i] == 1 & combined_var == 1) {
    HHS$Race[i] <- 5  }
  #the commented out code below is to assign 8 to participants who truly didn't mark anything, and assign 7 to participants who put "Yes" for hispanic/latinx
  #it will run fine once null values are taken out of the Hispanic/Latinx columns in the dataset. I fixed it on a personal version of the doc and had no issues. 
  if(RWhite[i] == 0 & RBlack[i] == 0 & RNative_American[i] == 0 & RNative_Hawaiian[i] == 0 & RAsian[i] == 0){
    #if(RLatin[i] == 0){
    #HHS$Race[i] <- 6}
    
    #if(RLatin[i] ==1){}
    HHS$Race[i] <- 6 
  }
}
head(results)
# Access and print the value of a variable for a specific row

#adding other race variable into the race variable, relabelling the numbers with category names
HHS <- mutate(HHS, Race = ifelse(Race==6, `Other race`, Race))
HHS <- mutate(HHS, Race = ifelse(is.na(Race) & `Hispanic/Latinx`=="Yes", "Hispanic/Latinx", Race))
HHS$Race[HHS$Race==0] <- "Mixed"
HHS$Race[HHS$Race==1] <- "White"
HHS$Race[HHS$Race==2] <- "Black/African American"
HHS$Race[HHS$Race==3] <- "Native American/Alaska Native"
HHS$Race[HHS$Race==4] <- "Native Hawaiian/Pacific Islander"
HHS$Race[HHS$Race==5] <- "Asian"
HHS$Race[HHS$Race==6] <- "Other"

#current issue: there is at least one person in the dataset that is white and categorized as so, but is also hispanic/latinx. not sure what to do with that. could keep hispanic/latinx variable in to help fix? will work on this later b/c it's good enough for now

#setting decline to answer to missing
HHS$`Hispanic/Latinx`[HHS$`Hispanic/Latinx` == "Decline to answer"] <- NA
HHS$`Education level`[HHS$`Education level` == "Decline to answer"] <- NA
HHS$`Sexuality`[HHS$`Sexuality` == "Decline to answer"] <- NA
HHS$`Gender`[HHS$`Gender` == "Decline to answer"] <- NA

#taking a final subset of variables with everything data managed
finalsub <- c("Record ID", "Survey timestamp", "Complete?","Education level", "Age",
              "Biggest health concern", "How could clinics help", "Clinic", "Gender", "Sexuality", "Race")
HHS <- HHS[finalsub]


################## CODE FOR VISUALS ############################
require(ggplot2)

#Relationship between Clinic and Gender
ggplot(data = subset(HHS, !is.na(Gender)), aes(x = Clinic, fill = (Gender))) +
  geom_bar() +
  labs(title = "Relationship Between Clinic and Gender",
       x = "Clinic",
       y = "Count") +
  theme_minimal()+
  theme(legend.position = "right", axis.text.x = element_text(angle = 90, vjust = 0.5))


HHS$`Education level` <- trimws(HHS$`Education level`)
filtered_data <- subset(HHS, tolower(`Education level`) != tolower("Decline to Answer"))

#relationship between age and education level
ggplot(data = filtered_data, aes(x = `Education level`, y = Age)) +
  geom_boxplot(fill = "lightblue", color = "blue", outlier.shape = NA) +
  geom_jitter(position = position_jitter(width = 0.2), color = "red", alpha = 0.5, size = 0.5) +
  labs(title = "Relationship Between Age and Education Level",
       x = "Education Level",
       y = "Age") +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Distribution of race
ggplot(data=subset(HHS, !is.na(Race)), aes(x=Race,fill=Race))+
  geom_bar()+
  labs(title="Distribution of Racial Categories (including Hispanic/Latinx)",
       x="Racial Category",
       y="Count")+
  theme_minimal()+
  theme(legend.position="none", axis.text.x = element_text(angle = 50, hjust = 1))

#Gender and Sexuality relationship just for funsies
ggplot(data=subset(HHS, !is.na(Gender) & !is.na(Sexuality)), aes(x=Gender,fill=Sexuality))+
  geom_bar()+
  labs(title="Relationship between Gender and Sexuality",
       x="Gender",
       y="Count")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 50, hjust = 1))

#frequency tables/descriptive stats
require(descr)
#race
freq(HHS$Race)
#gender
freq(HHS$Gender)
#age
summary(HHS$Age) 
mean(HHS$Age, na.rm = TRUE) 
sd(HHS$Age, na.rm = TRUE)
#sexuality
freq(HHS$Sexuality)
#education level
freq(HHS$`Education level`)





