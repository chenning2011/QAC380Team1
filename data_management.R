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
            "What is your annual household income before taxes)?", "Household size",
            "Were you born in the United States?", "Country of birth",
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

#data management for country of birth to combine columns
HHS <- mutate(HHS, `Country of Birth` = ifelse(`Were you born in the United States?`=="No",`Country of birth`,"United States"))

#changing all of the names for beau
subvars <- c("Record ID", "Survey Timestamp...3","Complete?...4" ,"Age", 
             "What is your race? (choice=White)", "What is your race? (choice=Black or African American)",
             "What is your race? (choice=American Indian or Alaska Native)", 
             "What is your race? (choice=Native Hawaiian or other Pacific Islander)",
             "What is your race? (choice=Asian)","Other race",
             "What is your annual household income before taxes)?", "Household size",
             "Are you Hispanic/Latinx?", "What is the highest education level you have completed?",
             "Biggest health concern", "How could clinics help","Which clinic are currently visiting?",
             "Gender Identity", "Sexual Orientation", "Country of Birth")
HHS <- HHS[subvars]
names(HHS) <- c("Record ID", "Survey timestamp", "Complete?", "Age", "White", "Black", "Native American",
           "Native Hawaiian", "Asian", "Other race", "Household Income", "Household Size","Hispanic/Latinx", "Education level",
           "Biggest health concern", "How could clinics help", "Clinic", "Gender", "Sexuality", "Country of Birth")
#dropping people under the age of 18
HHS$Age[HHS$Age < 18] <- NA

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

#cuts "other race" down to 21 categories 
for(i in 1:nrow(HHS)) {
  Otherrace <- (HHS$`Other race`[i]) 
  if(Otherrace %in% c("La tina", "Latino/ hispano", "latino","latina", "hispanic latino", "hispanic/latina", 
                      "latin", "hispano", "hispanos", "hispanic", "Hispana", "Ispano", "Spanish", "Ispana", 
                      "Latino", "Hispanic", "Hispanic Latino", "Hispanic/Latina", "Hispanos", "Latina", "Hispano", "Latin")) {
    HHS$`Other race`[i] <- "Hispanic/Latinx"
  }
  if(Otherrace %in% c("República dominicana", "Republics dominicana", "Dominican Republic", "Dominicana", "Dominicano", "Diminicano", "Santo domingo", 
                      "Dominican", "Santo Domingo")){
    HHS$`Other race`[i] <- "Dominican"
  }
  if(Otherrace %in% c("Cape Verdean", "Cape verde", "Cabo Verde", "Cape verdean")){
    HHS$`Other race`[i] <- "Cape Verdean"
  }
  if(Otherrace %in% c("Guatemalteca", "Guatemala", "Guatemalan")){
    HHS$`Other race`[i] <- "Guatemalan"
  }
  if(Otherrace %in% c("Mexico", "Mexican")){
    HHS$`Other race`[i] <- "Mexican"
  }
  if(Otherrace %in% c("Puerto Rico", "Puerto rico", "Puerto Rican")){
    HHS$`Other race`[i] <- "Puerto Rican"
  }
  if(Otherrace %in% c("Idk", "Human", "Humano", "Usa", "pansexual")){
    HHS$`Other race`[i] <- "Invalid/miscategorized response"
  }
  if(Otherrace %in% c("Multiracial", "Mix", "Mixed", "Native South American and Caribbean", "I'm PR & CV", 
                      "Italian and Puerto Rican", "Indian Asian", "Mulato")){
    HHS$`Other race`[i] <- "Multiracial"
  }
  if(Otherrace %in% c("Centro americana", "Maya")){
    HHS$`Other race`[i] <- "Central American"
  }
}
unique(HHS$`Other race`)
#prints the categories of "other race"
print(length(unique(HHS$`Other race`)))
head(results, 30)
frequency_table <- table(results)
frequency_num <- frequency_table[2]
print(frequency_table)
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

#updating spanish names in race variable to english names 
HHS$Race[HHS$Race=="Caribeña"] <- "Caribbean"
HHS$Race[HHS$Race=="Indio mexicano"] <- "Mexican Indian"
HHS$Race[HHS$Race=="Mixed"] <- "Multiracial"
freq(HHS$Race)

#removing all invalid responses 
Missing <- is.na(HHS$Age) & is.na(HHS$`Hispanic/Latinx`) & is.na(HHS$`Education level`)
HHS <- subset(HHS, subset=!Missing)

#setting decline to answer to missing
HHS$`Hispanic/Latinx`[HHS$`Hispanic/Latinx` == "Decline to answer"] <- NA
HHS$`Education level`[HHS$`Education level` == "Decline to answer"] <- NA
HHS$`Sexuality`[HHS$`Sexuality` == "Decline to answer"] <- NA
HHS$`Gender`[HHS$`Gender` == "Decline to answer"] <- NA
HHS$`Household Income`[HHS$`Household Income` == "Decline to answer"] <- NA

#taking a final subset of variables with everything data managed
finalsub <- c("Record ID", "Survey timestamp", "Complete?","Education level", "Age", "Household Income", "Household Size",
              "Biggest health concern", "How could clinics help", "Clinic", "Gender", "Sexuality", "Race", "Country of Birth")
HHS <- HHS[finalsub]

#collapsing gender categories
HHS$Gender[HHS$Gender == "Agender" | HHS$Gender == "Nonbinary" | HHS$Gender == "nonbinary transmasculine" | HHS$Gender == "Queer Nonbinary" | HHS$Gender == "trans"] <- "Genderqueer, neither exclusively man nor woman"
HHS$Gender[HHS$Gender == "Woman but it means so much less than society thinks it does."] <- "Woman"

#collapsing sexuality categories 
HHS$Sexuality[HHS$Sexuality == "homoflexible" | HHS$Sexuality == "Estoy definido como hombre"] <- "Homosexual, gay or same gender loving"
HHS$Sexuality[HHS$Sexuality == "Me gusta la mujere" | HHS$Sexuality == "Straight"] <- "Heterosexual or straight"
HHS$Sexuality[HHS$Sexuality == "Bisexual" | HHS$Sexuality == "Pansexual"] <- "Bisexual/Pansexual"
HHS$Sexuality[HHS$Sexuality == "Demisexual" | HHS$Sexuality == "Asexual"] <- "Queer"

freq(HHS$`Household Size`)
freq(HHS$`Country of Birth`)

#collapsing country of birth categories
HHS$`Country of Birth`[HHS$`Country of Birth` %in% c("11/25/1986 Republica Dominica", "Dom rep", "Dom Rep",
                                                      "Dom. Rep.", "Dominacn Republic", "Dominican", "Dominican Rep",
                                                      "dominican republic", "Domincan Republic","Dominican republic",
                                                      "Dominican República", "Dominican Rwpublic", "dominicana", "Dominicana",
                                                      "Dominicano", "dR", "Dr", "DR", "En dominicana", "En Republica dominicana",
                                                      "R.D.", "Rd", "RD", "RD.. 6-24-1981", "Rep dom", "Rep Dom", 
                                                      "Rep. Dom", "República D", "Republica Domicana","Republica dominicana",
                                                      "Republica Dominicana", "REPUBLICA Dominicana", "República dominicana",
                                                      "República Dominicana", "Bonao", "Thé Dominican Republic",
                                                      "Puerto plata", "Sto dgo", "Santo domingo", "Santo Domingo",
                                                     "Santos domingo", "Santos Domingo")] <- "Dominican Republic"
HHS$`Country of Birth`[HHS$`Country of Birth`=="Brasil"] <- "Brazil"
HHS$`Country of Birth`[HHS$`Country of Birth` %in% c("Cabo verde", "Cabo Verde", "Cape Verde", "Cape verde")] <- "Republic of Cabo Verde"
HHS$`Country of Birth`[HHS$`Country of Birth` %in% c("Cali", "colombia", "Columbia", "Medellin")] <- "Colombia"
HHS$`Country of Birth`[HHS$`Country of Birth` %in% c("Caguas PR", "PR", "Puerto rico", "Puertorico")] <- "Puerto Rico"
HHS$`Country of Birth`[HHS$`Country of Birth`=="Costa de marfil"] <- "Ivory Coast"
HHS$`Country of Birth`[HHS$`Country of Birth`=="cuba"]<- "Cuba"
HHS$`Country of Birth`[HHS$`Country of Birth`=="Ecuador- guayaquil"] <- "Ecuador"
HHS$`Country of Birth`[HHS$`Country of Birth`=="El salvador"|HHS$`Country of Birth`=="San Salvador"] <- "El Salvador"
HHS$`Country of Birth`[HHS$`Country of Birth` %in% c("Guatemalteca", "Guatemalteco", "Guatemela")] <- "Guatemala"
HHS$`Country of Birth`[HHS$`Country of Birth`=="Guinee"] <- "Guinea"
HHS$`Country of Birth`[HHS$`Country of Birth`=="Haití"] <- "Haiti"
HHS$`Country of Birth`[HHS$`Country of Birth`=="Liberia Africa"] <- "Liberia"
HHS$`Country of Birth`[HHS$`Country of Birth`=="México"] <- "Mexico"
HHS$`Country of Birth`[HHS$`Country of Birth`=="USVI"] <- "US Virgin Islands"
HHS$`Country of Birth`[HHS$`Country of Birth`=="Africa"] <- "Other African Country"

###################### translating how clinics could help column pls ignore the mess that this is ##################
HHS$`How could clinics help`[HHS$`How could clinics help`=="Ya me están ayudando"|HHS$`How could clinics help`=="ya me estan ayudando"]<- "They’re already helping me"
HHS$`How could clinics help`[HHS$`How could clinics help`=="Ayuda en linea para la salud mental"]<- "Online Help for Mental Health"
HHS$`How could clinics help`[HHS$`How could clinics help`=="Asesores en español"]<- "Advisors in Spanish"
HHS$`How could clinics help`[HHS$`How could clinics help`=="Estoy conforme gracias"]<- "I'm happy thank you"
HHS$`How could clinics help`[HHS$`How could clinics help`=="nada" | HHS$`How could clinics help`=="Ninguna"| HHS$`How could clinics help`=="Nada"] <- "Nothing"
HHS$`How could clinics help`[HHS$`How could clinics help`=="estoy bien"] <- "I'm fine"
HHS$`How could clinics help`[HHS$`How could clinics help`=="seguir apollando"] <- "Keep supporting"
HHS$`How could clinics help`[HHS$`How could clinics help`=="ayudarme a controlar y regular mi higado"] <- "Help me control and regulate my liver"
HHS$`How could clinics help`[HHS$`How could clinics help`=="ponerme vacunas"] <- "Get vaccines"
HHS$`How could clinics help`[HHS$`How could clinics help`=="cheqearme para prevenir algunas enfermedades en la mañana"] <- "check me to prevent some illnesses in the morning"
HHS$`How could clinics help`[HHS$`How could clinics help`=="Ayudarme gratis solo atenderme"|HHS$`How could clinics help`=="ayudarme gratis solo atenderme"] <- "help me for free just attend to me"
HHS$`How could clinics help`[HHS$`How could clinics help`=="Segir ayudandonos"|HHS$`How could clinics help`=="segir ayudandonos"] <- "Continue to help us"
HHS$`How could clinics help`[HHS$`How could clinics help`=="seguir dardo servisios gratis"] <- "Continue to provide free services"
HHS$`How could clinics help`[HHS$`How could clinics help`=="atencion cuando lo necesito"|HHS$`How could clinics help`=="atencion cuando lo necesite"] <- "Attention when I need it"
HHS$`How could clinics help`[HHS$`How could clinics help`=="Ayudarme gratis"|HHS$`How could clinics help`=="ayudarme gratis"] <- "Help me for free"
HHS$`How could clinics help`[HHS$`How could clinics help`=="mas maneras de obtener cuidado de salud"] <- "More ways to get healthcare"
HHS$`How could clinics help`[HHS$`How could clinics help`=="informacion sobre grupos educativos"] <- "Information about educational groups"
HHS$`How could clinics help`[HHS$`How could clinics help`=="mas educaion o grupos de nutricion"] <- "More education or nutrition groups"
HHS$`How could clinics help`[HHS$`How could clinics help`=="mas información de medicina"] <- "More medicine information"
HHS$`How could clinics help`[HHS$`How could clinics help`=="anecho mucho"] <- "Other"
HHS$`How could clinics help`[HHS$`How could clinics help`=="en casa de alguna emergencia tener alguna asistencia"] <- "at home in an emergency have some assistance"
HHS$`How could clinics help`[HHS$`How could clinics help`=="no se"|HHS$`How could clinics help`=="No se"] <- "I do not know"
HHS$`How could clinics help`[HHS$`How could clinics help`=="Mejorar mi presión"] <- "Improve my blood pressure"
HHS$`How could clinics help`[HHS$`How could clinics help`=="Psicología"|HHS$`How could clinics help`=="Psicologia"|HHS$`How could clinics help`=="Psicólogo"] <- "Psychologist"
HHS$`How could clinics help`[HHS$`How could clinics help`=="Dar citas más seguidas"] <- "Give appointments more often"
HHS$`How could clinics help`[HHS$`How could clinics help`=="Seguir mis citas"] <- "Track my appointments"
HHS$`How could clinics help`[HHS$`How could clinics help`=="Darme citas"] <- "Give me appointments"
HHS$`How could clinics help`[HHS$`How could clinics help`=="Estoy agradecida por la ayuda que me prestan sin seguro"] <- "I’m grateful for the help they give me without insurance"
HHS$`How could clinics help`[HHS$`How could clinics help`=="Siempre me ayudan  Estonian agradecida"] <- "They always help me and I am grateful"
HHS$`How could clinics help`[HHS$`How could clinics help`=="Atención médica"|HHS$`How could clinics help`=="Atencion medica"] <- "Medical attention"
HHS$`How could clinics help`[HHS$`How could clinics help`=="Con medicinas"] <- "With medicines"
HHS$`How could clinics help`[HHS$`How could clinics help`=="Ayudarme más con medicamentos fuertes para el dolor"] <- "Help me more with strong pain medicine"
HHS$`How could clinics help`[HHS$`How could clinics help`=="Dar seguimiento más continuo"] <- "Provide more continuous followup"
HHS$`How could clinics help`[HHS$`How could clinics help`=="Más apoyo con los médicos"] <- "More support with doctors"
HHS$`How could clinics help`[HHS$`How could clinics help`=="Esta clínica me salvo la vida. Por favor, ayúdenla!"] <- "This clinic saved my life. Please help it!"
HHS$`How could clinics help`[HHS$`How could clinics help`=="Que las citas sean más constantes y no tan alejadas"] <- "Make appointments more constant and not so far apart"
HHS$`How could clinics help`[HHS$`How could clinics help`=="Seguimiento"] <- "Tracking"
HHS$`How could clinics help`[HHS$`How could clinics help`=="Derivarme con el profesional que corresponda"] <- "Refer me to the appropriate professional"
HHS$`How could clinics help`[HHS$`How could clinics help`=="Seguimiento de vista"] <- "View tracking"
HHS$`How could clinics help`[HHS$`How could clinics help`=="Necesito un chequeo general"] <- "I need a general checkup"
HHS$`How could clinics help`[HHS$`How could clinics help`=="Atención ,medicina"] <- "Care, medicine"
HHS$`How could clinics help`[HHS$`How could clinics help`=="Citas"] <- "Appointments"
HHS$`How could clinics help`[HHS$`How could clinics help`=="Seguir con loscontroles"] <- "Continue with the controls"
HHS$`How could clinics help`[HHS$`How could clinics help`=="Ayudarme a chequiar"] <- "Help me check"
HHS$`How could clinics help`[HHS$`How could clinics help`=="Medicina"|HHS$`How could clinics help`=="Medicamentos"|HHS$`How could clinics help`=="Medicamento"] <- "Medicine"
HHS$`How could clinics help`[HHS$`How could clinics help`=="Atención medica"] <- "Medical care"
HHS$`How could clinics help`[HHS$`How could clinics help`=="Atencion"] <- "Attention"
HHS$`How could clinics help`[HHS$`How could clinics help`=="Citas medicas"] <- "Medical appointments"
HHS$`How could clinics help`[HHS$`How could clinics help`=="Estoy satisfecha con lo que ya han hecho"] <- "I’m pleased with what they’ve already done"
HHS$`How could clinics help`[HHS$`How could clinics help`=="Que me den citas"] <- "Give me appointments"
HHS$`How could clinics help`[HHS$`How could clinics help`=="Darme asistencia"] <- "Give me assistance"
HHS$`How could clinics help`[HHS$`How could clinics help`=="Atenderme cuando yo lo necesite"] <- "Attend to me when I need it"
HHS$`How could clinics help`[HHS$`How could clinics help`=="Dar servicio"] <- "Provide service"
HHS$`How could clinics help`[HHS$`How could clinics help`=="Me gusta lo que hacen"] <- "I like what they do"
HHS$`How could clinics help`[HHS$`How could clinics help`=="Citas medicamentos"] <- "Medication appointments"
HHS$`How could clinics help`[HHS$`How could clinics help`=="Seguir citas y medicamentos"] <- "Appointments and medications"
HHS$`How could clinics help`[HHS$`How could clinics help`=="Programarme una cita"] <- "Schedule an appointment for me"
HHS$`How could clinics help`[HHS$`How could clinics help`=="Ayudarme quitar el dolor y corregirlo"] <- "Help me take away the pain and correct it"
HHS$`How could clinics help`[HHS$`How could clinics help`=="Ya me están ayudando , dándome citas con los doctores que necesito, gracias"] <- "They are already helping me, giving me appointments with the doctors I need, thank you"
HHS$`How could clinics help`[HHS$`How could clinics help`=="Nutricional"] <- "They are already helping me, giving me appointments with the doctors I need, thank you"
HHS$`How could clinics help`[HHS$`How could clinics help`=="Me están ayudando en este momento"] <- "They’re helping me right now"
HHS$`How could clinics help`[HHS$`How could clinics help`=="Tener una mejor vivienda pues la renta está super cara y no se gana mucho dinero"] <- "Have a better home because the rent is super expensive and you don't earn much money"
HHS$`How could clinics help`[HHS$`How could clinics help`=="En todo mi proceso de salud"] <- "In general my health journey"
HHS$`How could clinics help`[HHS$`How could clinics help`=="Ya me estab ayudando y estoy agradecida por eso"] <- "It was already helping me and I’m grateful for that"
HHS$`How could clinics help`[HHS$`How could clinics help`=="Bueno ya lo hacen al atendernos"] <- "They already do it when they serve us"
HHS$`How could clinics help`[HHS$`How could clinics help`=="Una aplicación para información"] <- "An application for information"
HHS$`How could clinics help`[HHS$`How could clinics help`=="Me Han atendido muy bien pero me senti muy cuestionado en la entrada"] <- "I have been treated very well but I felt very questioned at the entrance"
HHS$`How could clinics help`[HHS$`How could clinics help`=="Medicamentos; tratamiento gratis"] <- "Drugs; Free treatment"
HHS$`How could clinics help`[HHS$`How could clinics help`=="Quisiera operate el estomago"] <- "I’d like to operate on the stomach"
HHS$`How could clinics help`[HHS$`How could clinics help`=="Ciempre he recibido alluda de free clinic"] <- "I have always received advice from free clinic"
HHS$`How could clinics help`[HHS$`How could clinics help`=="Tratamiento medicamentos"] <- "Medication treatment"
HHS$`How could clinics help`[HHS$`How could clinics help`=="Bariatrica cirugía"] <- "Bariatric surgery"
HHS$`How could clinics help`[HHS$`How could clinics help`=="Hasta ahora  me Han dado seguimiento a mis necesidades.  Estonian agrarecida pot la ayuda."] <- "So far they have followed up on my needs. I am grateful for the help."
HHS$`How could clinics help`[HHS$`How could clinics help`=="Medicamento; nutrition;"] <- "Medication; nutrition"
HHS$`How could clinics help`[HHS$`How could clinics help`=="Citas, medicamentos"] <- "Appointments, medication"
HHS$`How could clinics help`[HHS$`How could clinics help`=="Asesoría nutricional"] <- "Nutritional counselling"
HHS$`How could clinics help`[HHS$`How could clinics help`=="Tratamiento y medicamentos"] <- "Treatment and medications"
HHS$`How could clinics help`[HHS$`How could clinics help`=="Están bien"] <- "They're fine"
HHS$`How could clinics help`[HHS$`How could clinics help`=="No tiene acceso a muchas cosas sin seguro"] <- "You don’t have access to a lot of things without insurance"
HHS$`How could clinics help`[HHS$`How could clinics help`=="Medicamentos- examenes"] <- "Medications, exams"
HHS$`How could clinics help`[HHS$`How could clinics help`=="Examines- terrapin-examenes"] <- "Exams"
HHS$`How could clinics help`[HHS$`How could clinics help`=="Medicamento-  nutritionists"] <- "Medications, nutritionists"
HHS$`How could clinics help`[HHS$`How could clinics help`=="Dar bien servicio"] <- "Provide good service"
HHS$`How could clinics help`[HHS$`How could clinics help`=="Mantenerme siempre informado"] <- "Always keep me informed"
HHS$`How could clinics help`[HHS$`How could clinics help`=="Medicamentos y me ha tratado may bien el medico"] <- "Medication and I have been treated very well by the doctor"
HHS$`How could clinics help`[HHS$`How could clinics help`=="Sizemore me Han ayudado"] <- "They have helped me"
HHS$`How could clinics help`[HHS$`How could clinics help`=="Seguimientos de rutina . Orientación sobre la diabetes"] <- "Routine follow-ups. Diabetes Counseling"
HHS$`How could clinics help`[HHS$`How could clinics help`=="Manteniendonos mas Informados"] <- "Keeping us more informed"
HHS$`How could clinics help`[HHS$`How could clinics help`=="Escucharme necesidades"] <- "Listen to my needs"
HHS$`How could clinics help`[HHS$`How could clinics help`=="Continuar las citas de control"] <- "Continue control follow-up appointments"
HHS$`How could clinics help`[HHS$`How could clinics help`=="Tratamiento clinico"] <- "Clincial treatment"
HHS$`How could clinics help`[HHS$`How could clinics help`=="Radiografías, tratamientos, y medicamentos"] <- "X-rays, treatments, and medications"
HHS$`How could clinics help`[HHS$`How could clinics help`=="Citas medicas, y consejeria psicologica"] <- "Medical appointments, and psychological counseling"
HHS$`How could clinics help`[HHS$`How could clinics help`=="Chequear  mi diabetes"] <- "Check my diabetes"
HHS$`How could clinics help`[HHS$`How could clinics help`=="Asistencia para mis hijos"] <- "Assistance for my children"
HHS$`How could clinics help`[HHS$`How could clinics help`=="Profundizar más en los problemas de salud del paciente y no sólo enfocarse en los síntomas"] <- "Digging deeper into the patient's health issues and not just focusing on the symptoms"
HHS$`How could clinics help`[HHS$`How could clinics help`=="Ayudame a poder estar bien"] <- "Help me to be well"
HHS$`How could clinics help`[HHS$`How could clinics help`=="Mejores exámenes"|HHS$`How could clinics help`=="Mejores exam"] <- "Better exams"
HHS$`How could clinics help`[HHS$`How could clinics help`=="Consejeria"] <- "Counselling"
HHS$`How could clinics help`[HHS$`How could clinics help`=="Una buena atención médica para lidiar porque mi problemas de la depresión"] <- "Good medical care to deal with my depression problems"
HHS$`How could clinics help`[HHS$`How could clinics help`=="Contine healthcare"] <- "Continue healthcare"
HHS$`How could clinics help`[HHS$`How could clinics help`=="Ya esta ayudandome"] <- "It's already helping me"
HHS$`How could clinics help`[HHS$`How could clinics help`=="Recibir una terapia"] <- "Receiving therapy"
HHS$`How could clinics help`[HHS$`How could clinics help`=="Proporcionarme todos los medicamentos y atenciones médicas requerida"] <- "Provide me with all medications and medical care required"
HHS$`How could clinics help`[HHS$`How could clinics help`=="Ellos son muy buenos"] <- "They are very good"
HHS$`How could clinics help`[HHS$`How could clinics help`=="Consulta psicológica"] <- "Psychological consultation"
HHS$`How could clinics help`[HHS$`How could clinics help`=="Seguimiento medico igual"] <- "Same medical follow-up"
HHS$`How could clinics help`[HHS$`How could clinics help`=="Realizar una carta para que en mi trabajo consideren mi sulud"] <- "Make a letter so that my work will consider my health"
HHS$`How could clinics help`[HHS$`How could clinics help`=="Todo es bueno"|HHS$`How could clinics help`=="Todo está bien"|HHS$`How could clinics help`=="Todo esta bien"|HHS$`How could clinics help`=="Estan bien"] <- "It's all good"
HHS$`How could clinics help`[HHS$`How could clinics help`=="Todo perfecto"] <- "Everything is perfect"
HHS$`How could clinics help`[HHS$`How could clinics help`=="La ayuda que me han dado a mi a mis hijos es bueno"] <- "The help they have given me and my children is good"
HHS$`How could clinics help`[HHS$`How could clinics help`=="Ellosvhacen todo lo necessario"] <- "They do everything you need"
HHS$`How could clinics help`[HHS$`How could clinics help`=="Encontrar una forma de ayuda con los servicios de luz y aceite y pago de vivienda"] <- "Find a way to help with electricity and oil services and housing payments"
HHS$`How could clinics help`[HHS$`How could clinics help`=="Ser más accesible"] <- "Be more accessible"
HHS$`How could clinics help`[HHS$`How could clinics help`=="Lo hacen todo"] <- "They do it all"
HHS$`How could clinics help`[HHS$`How could clinics help`=="Buenos trabajadores,buenos intérpretes"] <- "Good workers, good interpreters"
HHS$`How could clinics help`[HHS$`How could clinics help`=="Hasta hora todo esta muy bien"] <- "So far so good"
HHS$`How could clinics help`[HHS$`How could clinics help`=="Ofrecer mejor servicio y atención a la hora de realizar una cita"] <- "Offer better service and attention when making an appointment"
HHS$`How could clinics help`[HHS$`How could clinics help`=="Lo que se pueda dentro de lo que estipulado dentro de sus capacidades"] <- "Whatever you can within what you stipulate within your capabilities"
HHS$`How could clinics help`[HHS$`How could clinics help`=="Ayudarme con mi salud mental"] <- "Help me with my mental health"
HHS$`How could clinics help`[HHS$`How could clinics help`=="Si"] <- "Yes"
HHS$`How could clinics help`[HHS$`How could clinics help`=="Mejorar sus doctores"] <- "Improve your doctors"
HHS$`How could clinics help`[HHS$`How could clinics help`=="Estoy satisfecho con el servicio"] <- "I am satisfied with the service"
HHS$`How could clinics help`[HHS$`How could clinics help`=="Seguro"] <- "Sure"
HHS$`How could clinics help`[HHS$`How could clinics help`=="Tratar de encontrar ayuda para mi problemas de depresión ansiedad  imperactividad concentración entre otras"] <- "Trying to find help for my depression problems, anxiety, inactivity, concentration, among others"
HHS$`How could clinics help`[HHS$`How could clinics help`=="Terapia"] <- "Therapy"
HHS$`How could clinics help`[HHS$`How could clinics help`=="Enviarme con un especialista en fertilidad"] <- "Send me to a fertility specialist"
HHS$`How could clinics help`[HHS$`How could clinics help`=="que hubiera mas atencion medica"] <- "That there would be more medical attention"
HHS$`How could clinics help`[HHS$`How could clinics help`=="Un buen tratamiento"] <- "A good treatment"
HHS$`How could clinics help`[HHS$`How could clinics help`=="Proveer los medicamentos a tiempo"] <- "Provide medications on time"
HHS$`How could clinics help`[HHS$`How could clinics help`=="Tener más citas disponibles"] <- "Have more appointments available"
HHS$`How could clinics help`[HHS$`How could clinics help`=="Tener más citas disponibles"] <- "Have more appointments available"

########## translating the biggest health concerns columns pls ignore how horrible this is ############
HHS$`Biggest health concern`[HHS$`Biggest health concern`=="Depresión; tango una amiga que la tiene y me preocupa"] <- "Depression; I have a friend who has it and I worry"
HHS$`Biggest health concern`[HHS$`Biggest health concern`=="Salud mental"] <- "Mental health"
HHS$`Biggest health concern`[HHS$`Biggest health concern`=="Por ahora ninguna"|HHS$`Biggest health concern`=="Hasta ahora ninguno"] <- "For now, none"
HHS$`Biggest health concern`[HHS$`Biggest health concern`=="Ninguna"|HHS$`Biggest health concern`=="Ninguno"|HHS$`Biggest health concern`=="ningunes"|HHS$`Biggest health concern`=="Nada"|HHS$`Biggest health concern`=="ninguna"|HHS$`Biggest health concern`=="Ningina"] <- "Nothing"
HHS$`Biggest health concern`[HHS$`Biggest health concern`=="no tener seguro medico"] <- "Not having medical insurance"
HHS$`Biggest health concern`[HHS$`Biggest health concern`=="higado graso"] <- "Fatty liver"
HHS$`Biggest health concern`[HHS$`Biggest health concern`=="No quisiera tener que depender de ningún medicamento para sentirme bien"] <- "I don't want to have to rely on any medication to feel good"
HHS$`Biggest health concern`[HHS$`Biggest health concern`=="mi presion y obedsidad"] <- "My blood pressure and obesity"
HHS$`Biggest health concern`[HHS$`Biggest health concern`=="Problemas del corazon"|HHS$`Biggest health concern`=="problemas del corazon"] <- "Heart problems"
HHS$`Biggest health concern`[HHS$`Biggest health concern`=="poder mantenerme fisicamente bien"] <- "to be able to stay physically well"
HHS$`Biggest health concern`[HHS$`Biggest health concern`=="mis piernas con mucho dolor"] <- "My legs in a lot of pain"
HHS$`Biggest health concern`[HHS$`Biggest health concern`=="tener dinero para comprar medicinas cuando me enfermo"] <- "Have money to buy medicine when I get sick"
HHS$`Biggest health concern`[HHS$`Biggest health concern`=="un pie drecho desquice"] <- "A sprained right foot"
HHS$`Biggest health concern`[HHS$`Biggest health concern`=="He tenido accidentes con huesos rotos y presento docrs alguian veses"] <- "I've had accidents with broken bones and I have presented some cases"
HHS$`Biggest health concern`[HHS$`Biggest health concern`=="actualmente embarazada"] <- "Currently pregnant"
HHS$`Biggest health concern`[HHS$`Biggest health concern`=="no tener seguro medico"] <- "Not having health insurance"
HHS$`Biggest health concern`[HHS$`Biggest health concern`=="enfermarme con covid perder seguro medico"] <- "Getting sick with COVID Losing Health Insurance"
HHS$`Biggest health concern`[HHS$`Biggest health concern`=="Presión alta"|HHS$`Biggest health concern`=="Alta presión Alterial"] <- "High blood pressure"
HHS$`Biggest health concern`[HHS$`Biggest health concern`=="Ansiedad"] <- "Anxiety"
HHS$`Biggest health concern`[HHS$`Biggest health concern`=="Depresión"|HHS$`Biggest health concern`=="La depresión"] <- "Depression"
HHS$`Biggest health concern`[HHS$`Biggest health concern`=="Mami grafía y papa Nicolasa y presión"] <- "Mammography and blood pressure check"
HHS$`Biggest health concern`[HHS$`Biggest health concern`=="Obtener seguro médico"] <- "Get health insurance"
HHS$`Biggest health concern`[HHS$`Biggest health concern`=="No quiero  ser  diabetica y mejorar  mi rodilla poder  caminar sin dolor"] <- "I don't want to be diabetic and improve my knee to be able to walk without pain"
HHS$`Biggest health concern`[HHS$`Biggest health concern`=="No tener seguro médico y otros exámenes en el hospital"] <- "Not having health insurance and other tests at the hospital"
HHS$`Biggest health concern`[HHS$`Biggest health concern`=="La fibromialgia"] <- "Fibromyalgia"
HHS$`Biggest health concern`[HHS$`Biggest health concern`=="La tiroides"] <- "The thyroid"
HHS$`Biggest health concern`[HHS$`Biggest health concern`=="Creo que tengo artrosis en mi rodilla derecha, que con el tiempo, me duele cada mes más"] <- "I think I have osteoarthritis in my right knee, which over time, hurts more and more every month"
HHS$`Biggest health concern`[HHS$`Biggest health concern`=="No tener seguro médico"] <- "Not having health insurance"
HHS$`Biggest health concern`[HHS$`Biggest health concern`=="Obesidad"|HHS$`Biggest health concern`=="Mi obecidad"] <- "Obesity"
HHS$`Biggest health concern`[HHS$`Biggest health concern`=="Estoy teniendo mucha fatiga"] <- "I’m having a lot of fatigue"
HHS$`Biggest health concern`[HHS$`Biggest health concern`=="Mantenerme en salud"] <- "Staying healthy"
HHS$`Biggest health concern`[HHS$`Biggest health concern`=="Epilepsia"] <- "Epilepsy"
HHS$`Biggest health concern`[HHS$`Biggest health concern`=="La diabetes"|HHS$`Biggest health concern`=="Diebetes"] <- "Diabetes"
HHS$`Biggest health concern`[HHS$`Biggest health concern`=="Los seno"] <- "Breasts"
HHS$`Biggest health concern`[HHS$`Biggest health concern`=="Presion"] <- "Blood pressure"
HHS$`Biggest health concern`[HHS$`Biggest health concern`=="Cáncer prostata"] <- "Prostate cancer"
HHS$`Biggest health concern`[HHS$`Biggest health concern`=="Dolor pierna"] <- "Leg pain"
HHS$`Biggest health concern`[HHS$`Biggest health concern`=="Dolor de estomago Fuentes"] <- "Strong stomach pain"
HHS$`Biggest health concern`[HHS$`Biggest health concern`=="A vices me da depresion"] <- "Sometimes I am depressed"
HHS$`Biggest health concern`[HHS$`Biggest health concern`=="La presión que se sane"] <- "Fixing blood pressure"
HHS$`Biggest health concern`[HHS$`Biggest health concern`=="Los dientes"] <- "Teeth"
HHS$`Biggest health concern`[HHS$`Biggest health concern`=="nada por ahora"] <- "Nothing for now"
HHS$`Biggest health concern`[HHS$`Biggest health concern`=="Mi hipertension"] <- "My hypertension"
HHS$`Biggest health concern`[HHS$`Biggest health concern`=="Un dolor en el bravo"] <- "A pain in the arm"
HHS$`Biggest health concern`[HHS$`Biggest health concern`=="Ser vista por un ginecólogo y un oftarmologo"] <- "Be seen by a gynecologist and ophthalmologist"
HHS$`Biggest health concern`[HHS$`Biggest health concern`=="Dolor columna"] <- "Spinal pain"
HHS$`Biggest health concern`[HHS$`Biggest health concern`=="Tengo depresión, y no quiero que se me complique mas"] <- "I have depression and I don't want it to get more complicated"
HHS$`Biggest health concern`[HHS$`Biggest health concern`=="Peso"] <- "Weight"
HHS$`Biggest health concern`[HHS$`Biggest health concern`=="Estrés"|HHS$`Biggest health concern`=="El Estres"] <- "Stress"
HHS$`Biggest health concern`[HHS$`Biggest health concern`=="Mi sobre peso mis dolores de rodillas y espalda"] <- "Being overweight, my knee and back pain"
HHS$`Biggest health concern`[HHS$`Biggest health concern`=="Pulmonary infeccion"] <- "Pulmonary infection"
HHS$`Biggest health concern`[HHS$`Biggest health concern`=="Azucar alta"] <- "High blood sugar"
HHS$`Biggest health concern`[HHS$`Biggest health concern`=="Reflujo"] <- "Reflux"
HHS$`Biggest health concern`[HHS$`Biggest health concern`=="Bajo de peso, tiroidea, depresión"] <- "Underweight, thyroid, depression"
HHS$`Biggest health concern`[HHS$`Biggest health concern`=="Ahora mismo es un dolor de siatica."] <- "Right now it’s my sciatica pain"
HHS$`Biggest health concern`[HHS$`Biggest health concern`=="Diabetica"] <- "Diabetic"
HHS$`Biggest health concern`[HHS$`Biggest health concern`=="Operar de ovaries"] <- "Operating on ovaries"
HHS$`Biggest health concern`[HHS$`Biggest health concern`=="La ciatica"] <- "Sciatica"
HHS$`Biggest health concern`[HHS$`Biggest health concern`=="No tenga debetes"] <- "I don't have diabetes"
HHS$`Biggest health concern`[HHS$`Biggest health concern`=="Colesterol"] <- "Cholesetrol"
HHS$`Biggest health concern`[HHS$`Biggest health concern`=="Nervio asiático"] <- "Sciatic nerve"
HHS$`Biggest health concern`[HHS$`Biggest health concern`=="Muy poca energía física"] <- "Very little physical energy"
HHS$`Biggest health concern`[HHS$`Biggest health concern`=="Espalda con mucho dolor"] <- "A lot of back pain"
HHS$`Biggest health concern`[HHS$`Biggest health concern`=="Que algun dia que le dicen que su salud esta mal"] <- "That someday they'll tell me my health is bad"
HHS$`Biggest health concern`[HHS$`Biggest health concern`=="Dolor en pecho"] <- "Chest pain"
HHS$`Biggest health concern`[HHS$`Biggest health concern`=="Dolor del brazo"] <- "Arm pain"
HHS$`Biggest health concern`[HHS$`Biggest health concern`=="Rebajar peso- esquizofrenia"] <- "Weight loss, schizophrenia"
HHS$`Biggest health concern`[HHS$`Biggest health concern`=="Cancer de seno"] <- "Breast cancer"
HHS$`Biggest health concern`[HHS$`Biggest health concern`=="Que todo salga bien en mi embarazo"] <- "That all goes well with my pregnancy"
HHS$`Biggest health concern`[HHS$`Biggest health concern`=="Desbebes y la depresión"] <- "Drinking and depression"
HHS$`Biggest health concern`[HHS$`Biggest health concern`=="Nos  cuidamos del covid"] <- "We are careful of covid"
HHS$`Biggest health concern`[HHS$`Biggest health concern`=="Que me vaya bien el parto"] <- "That my birth goes well"
HHS$`Biggest health concern`[HHS$`Biggest health concern`=="Preocupada por diabetes"] <- "Worried about diabetes"
HHS$`Biggest health concern`[HHS$`Biggest health concern`=="Problemas prostata"] <- "Prostate problems"
HHS$`Biggest health concern`[HHS$`Biggest health concern`=="Dolor en la rodilla"] <- "Knee pain"
HHS$`Biggest health concern`[HHS$`Biggest health concern`=="Presion Alta- colesterol"] <- "High blood pressure, cholesterol"
HHS$`Biggest health concern`[HHS$`Biggest health concern`=="Mi depresion y mi peso"] <- "My depression and my weight"
HHS$`Biggest health concern`[HHS$`Biggest health concern`=="Mi metodo de planificacion"] <- "My planning method"
HHS$`Biggest health concern`[HHS$`Biggest health concern`=="No poder trabajar"] <- "Not being able to work"
HHS$`Biggest health concern`[HHS$`Biggest health concern`==".no poder trabajo"] <- "Not being able to work"
HHS$`Biggest health concern`[HHS$`Biggest health concern`=="Mi espalda y pulmones"] <- "My back and lungs"
HHS$`Biggest health concern`[HHS$`Biggest health concern`=="Que no me puedan atender en una cita médica"] <- "That they cannot attend to me at a medical appointment"
HHS$`Biggest health concern`[HHS$`Biggest health concern`=="No soy la misa que antes"] <- "I am not the same as before"
HHS$`Biggest health concern`[HHS$`Biggest health concern`=="Dolor en mi columna vertebral"] <- "Pain in my spine"
HHS$`Biggest health concern`[HHS$`Biggest health concern`=="Estar bien"] <- "To be well"
HHS$`Biggest health concern`[HHS$`Biggest health concern`=="Salad en general"] <- "Health in general"
HHS$`Biggest health concern`[HHS$`Biggest health concern`=="La presión y la diabetes"|HHS$`Biggest health concern`=="Presion arterial y diabetes"] <- "Blood pressure and diabetes"
HHS$`Biggest health concern`[HHS$`Biggest health concern`=="Que no tengo mejora, cada vez estoy peor"] <- "That I don't get better, I'm getting worse and worse"
HHS$`Biggest health concern`[HHS$`Biggest health concern`=="La seguridad"] <- "Safety"
HHS$`Biggest health concern`[HHS$`Biggest health concern`=="Eye problems y diabetes"] <- "Eye problems and diabetes"
HHS$`Biggest health concern`[HHS$`Biggest health concern`=="Depresión y Ansiedad"] <- "Depression and anxiety"
HHS$`Biggest health concern`[HHS$`Biggest health concern`=="Que llevo más de 3 meses con tos y no an dado con lo que tengo"] <- "I've been coughing for more than 3 months and they haven't found what I have"
HHS$`Biggest health concern`[HHS$`Biggest health concern`=="Ginecólogia"] <- "Gynecology"
HHS$`Biggest health concern`[HHS$`Biggest health concern`=="Me enojo con mucha facilidad y aveces no puedo controlar eso y mi mente también"] <- "I get angry very easily and sometimes I can't control that and my mind too"
HHS$`Biggest health concern`[HHS$`Biggest health concern`=="El no poder dejar de tomar medicina para siempre"] <- "Not being able to stop taking medicine forever"
HHS$`Biggest health concern`[HHS$`Biggest health concern`=="Mi artritis y el síndrome q tengo"] <- "My arthritis and the syndrome that I have"
HHS$`Biggest health concern`[HHS$`Biggest health concern`=="No tengo"] <- "I don't have any"
HHS$`Biggest health concern`[HHS$`Biggest health concern`=="Poder ser atendido de manera más rápida"] <- "To be able to be attended to more quickly"
HHS$`Biggest health concern`[HHS$`Biggest health concern`=="Calcificaciones malignas de pecho"] <- "Malignant breast calcifications"
HHS$`Biggest health concern`[HHS$`Biggest health concern`=="Sufro de mucdha depresión ansiedad dolor en mis huesos dolor de cabeza cansancio"] <- "I suffer from a lot of depression, anxiety, pain in my bones, headache, tiredness"
HHS$`Biggest health concern`[HHS$`Biggest health concern`=="No e podido tener hijos"] <- "Haven't been able to have children"
HHS$`Biggest health concern`[HHS$`Biggest health concern`=="He tenido accidentes con huesos rotos y presento docrs alguian veses"] <- "I've had accidents with broken bones and I have presented some cases"

##### removing nothing/none/no/idk/etc. responses from biggest health concern and how could clinics help#######
HHS$`Biggest health concern`[HHS$`Biggest health concern` %in% c("no", "None", "No", "none", "NA", "Na", "N/A", 0, 
                                                                 "Don't have any", "For now, none", "I don't have any",
                                                                 "I'm not concern", "N/a", "No concern",
                                                                 "No concerns at this time", "No issue", "Non",
                                                                 "None al", "None right now", "None yet",
                                                                 "Nonegive", "Nothing", "NOTHING", "Nothing at the moment",
                                                                 "Nothing for now")] <- NA
HHS$`How could clinics help`[HHS$`How could clinics help` %in% c("no", "None", "No", "none", "NA", "Na", "N/A", 0, 
                                                                 "Don't have any", "For now, none", "I don't have any",
                                                                 "I'm not concern", "N/a", "No concern",
                                                                 "No concerns at this time", "No issue", "Non",
                                                                 "None al", "None right now", "None yet",
                                                                 "Nonegive", "Nothing", "NOTHING", "Nothing at the moment",
                                                                 "Nothing for now", "?", "??? I don't think there is more they could do",
                                                                 "don't know", "I do not know", "I don't know", "I'm not sure",
                                                                 "I'm not sure.", "Idk", "Nc", "No health needs",
                                                                 "No needs currently", "None, satistfied", "Not sure", "not sure",
                                                                 "Not sure what to say", "Nothing different", "Nothing right now ty",
                                                                 "Nothing right now", "Unsure")] <- NA

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

############ DESCRIPTIVE STATS TABLE ###################
require(table1)
table1::table1(~`Education level` + Age + `Household Income` + `Household Size` + Gender + Sexuality, data=HHS)
table1::table1(~`Country of Birth`, data=HHS)

#doing some stuff for the report here
filtered_data <- HHS %>% 
  group_by(`Education level`,`Household Income`) %>% 
  count(name = "freq")


freq(HHS$Race)

#dean's visualizations 

# Calculate the average household size for each income category
average_size <- HHS %>%
  group_by(`Household Income`) %>%
  summarize(Average_Size = mean(`Household Size`, na.rm = TRUE))%>%
  arrange(Average_Size)


# Create a bar plot

ggplot(data=subset(average_size, !is.na(`Household Income`)), aes(x = reorder(`Household Income`, Average_Size), y = Average_Size)) +
  geom_bar(stat = "identity", fill = "blue", color = "black") +  # Add black outline
  labs(x = "Household Income", y = "Average Household Size") +
  ggtitle("Average Household Size by Income Category") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"))  # Bold x-axis labels

recoded_country <- function(data) {
  data$`Country of Birth` <- tolower(data$`Country of Birth`)
  data$`Country of Birth` <- ifelse(grepl("^do", data$`Country of Birth`) | 
                                      grepl("rep[úu]blica d", data$`Country of Birth`) | 
                                      grepl("^rd", data$`Country of Birth`) | 
                                      grepl("d[ro]", data$`Country of Birth`) | 
                                      grepl("6-24-1981", data$`Country of Birth`) | 
                                      grepl("r\\.d\\.", data$`Country of Birth`), "Dominican Republic",
                                    ifelse(grepl("^gua", data$`Country of Birth`), "Guatemala",
                                           ifelse(grepl("^col", data$`Country of Birth`), "Colombia",
                                                  ifelse(grepl("^liberia", data$`Country of Birth`), "Liberia",
                                                         ifelse(grepl("^haiti", data$`Country of Birth`), "Haiti",
                                                                ifelse(grepl("^pr", data$`Country of Birth`), "Puerto Rico",
                                                                       ifelse(grepl("mexico", data$`Country of Birth`), "México",
                                                                              data$`Country of Birth`)))))))
  
  return(data)
}

# Apply the recoding function to the "Country of Birth" column
HHS2 <- recoded_country(HHS)

# Create a data frame for the heatmap
heatmap_data <- as.data.frame(table(HHS2$`Country of Birth`, HHS2$`Education level`))
colnames(heatmap_data) <- c("Country", "Education Level", "Count")

ggplot(heatmap_data, aes(x = factor(Country, levels = unique(Country)), y = `Education Level`, fill = Count)) +
  geom_tile() +
  labs(title = "Relationship between Country of Birth and Education Level", x = "Country of Birth", y = "Education Level", fill = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 1)) +
  scale_fill_gradient(low = "white", high = "darkblue")

library(scales)

# Calculate the percentage by country and education level
heatmap_data <- HHS %>%
  group_by(`Country of Birth`, `Education level`) %>%
  count() %>%
  group_by(`Country of Birth`) %>%
  mutate(percentage = n / sum(n) * 100)

my_colors <- c("#FFEDA0", "#FED976", "#FEB24C", "#FD8D3C", "#FC4E2A", "#E31A1C", "#BD0026")

# Create the heatmap plot
ggplot(data=subset(heatmap_data, !is.na(`Education level`) & !is.na(`Country of Birth`)), aes(x = factor(`Country of Birth`, levels = unique(`Country of Birth`)), y = `Education level`, fill = percentage)) +
  geom_tile() +
  labs(title = "Percentage of Educational Attainment by Country of Birth", x = "Country of Birth", y = "Education Level", fill = "Percentage") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 1)) +
  scale_fill_continuous(labels = scales::percent_format(scale = 1))+
  scale_fill_gradientn(colors = my_colors)

######################### TEXT ANALYSIS MAYBE IDK #################
library(tm)
#find the terms that appear at least 5 times
corpus <- Corpus(VectorSource(HHS$`How could clinics help`))
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removeWords, stopwords("en"))
dtm <- DocumentTermMatrix(corpus)
term_counts <- colSums(as.matrix(dtm))
most_frequent_term <- names(which.max(term_counts))
cat("Most frequent term:", most_frequent_term, "\n")
selected_terms <- findFreqTerms(dtm, 5)

#finding the terms that appear at least 5 times 
corpus2 <- Corpus(VectorSource(HHS$`Biggest health concern`))
corpus2 <- tm_map(corpus2, content_transformer(tolower))
corpus2 <- tm_map(corpus2, removePunctuation)
corpus2 <- tm_map(corpus2, removeNumbers)
corpus2 <- tm_map(corpus2, removeWords, stopwords("en"))
dtm2 <- DocumentTermMatrix(corpus2)
term_counts2 <- colSums(as.matrix(dtm2))
most_frequent_term2 <- names(which.max(term_counts2))
cat("Most frequent term:", most_frequent_term2, "\n")
selected_terms_2 <- findFreqTerms(dtm2, 5)

#just giving up and trying regex at this point 
HHS$`How could clinics help` <- tolower(HHS$`How could clinics help`)
  
# Regular expressions
regex_appointments <- "\\b(tracking|appointments|scheduling|making|patient portal|telehealth|consistent care|appts|followups|referrals|attention|refer|appointment|on time|more ways|followup|continue with controls|service|assistance|follow-ups|regularly)\\b"
regex_routine_care <- "\\b(need more help with other things|take care of me|help me to be well|to help me|routine care|medical care|preventative|test|testing|primary care|pcp|monitor|primary careservices|cancer screening|medications|medicines|check ups|physicals|checkup|checking|vaccines|vaccine|help me with health|medication|medical attention|prevent|meds|medicine|doctors|help me check|exams|treatment|physical|follow-up|examine)\\b"
regex_insurance <- "\\b(insurance|coverage|money|check cashing|paying|for free|free services|free|payments|reduce cost|discount)\\b"
regex_specialist <- "\\b(gender affirming|endocrinology|sti|testosterone|plan of addressing|optho|skin|liver|specialty|blood pressure|take away the pain|stomach|osteoporosis|surgery|ophthalmologist|diabetes|pchc|bladder|wheelchair|pregnancy|birth control|contraceptives|deliver|accessible|fertility|pain)\\b"
regex_communication <- "\\b(communication|resources|interpretation|telephone|advisor|attentive|advisors|talk|portal|informacion|up to date|informed|listen|advice)\\b"
regex_nutrition <- "\\b(nutrition|dietician|dietitian|diet|dieting|food classes|eat and exercise|meals|nutritionists|food)\\b"
regex_mental_health <- "\\b(mental health|psychologist|psychology|depression|anxiety|therapy|counseling|counselling|bipolar|psychological|therapist|rehab|psychiatrist)\\b"
regex_education <- "\\b(education|awareness|prevention|vaccine|information|resources|advisement|learn|learning)\\b"
regex_clinic_meeting_needs <- "\\b(nothing|do so much|excellent|great|help me already|already helping me|confident|everything that i need|love|priceless|needs being met|needs are being met|god sent|helped a lot|helping much more|helpful|listening|anything they can|not sure|continue to exist|helping me|happy|doing really well|meets all my health care needs|good|prescribed|does really well|doing enough|i come here|i'm fine|gives me a lot of concern|keep supporting|continue to help us|thank you|always help me|grateful|ok|saved my life|pleased|helps so much|i like|already|received advice|already helps|amazing|already|do a lot|helped|keep seeing|keep giving|all they can|satisfied|continue to treat|tx|they help me|awesome|a lot|they help me|perfect|they do it all|it meet|treated very well|everything|continue|they do everything)\\b"

library(tidyverse)
HHS <- HHS %>%
  mutate(
    Help_Category = case_when(
      grepl(regex_appointments, HHS$`How could clinics help`) ~ "appointment/access",
      grepl(regex_routine_care, HHS$`How could clinics help`) ~ "routine/primary care",
      grepl(regex_insurance, HHS$`How could clinics help`) ~ "insurance",
      grepl(regex_communication, HHS$`How could clinics help`) ~ "communication",
      grepl(regex_nutrition, HHS$`How could clinics help`) ~ "nutrition",
      grepl(regex_mental_health, HHS$`How could clinics help`) ~ "mental health",
      grepl(regex_education, HHS$`How could clinics help`) ~ "education",
      grepl(regex_clinic_meeting_needs, HHS$`How could clinics help`) ~ "clinic meeting needs",
      grepl(regex_specialist, HHS$`How could clinics help`) ~ "specialist",
      TRUE ~ "other"
    )
  ) 
HHS$Help_Category[is.na(HHS$`How could clinics help`)] <- NA

library(descr)
freq(HHS$Help_Category)

#graph of clusters 
library(tidyverse)
ggplot(data=subset(HHS, !is.na(Help_Category)))+
  geom_bar(aes(x=Help_Category, fill=Clinic), position="fill")+
  labs(x = "Category of Biggest Expressed Need", y = "Percentage Reporting Each Category", title="Percentage Reporting Each Category of Expressed Need Across All Six Clinics") +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust=1))+
  scale_fill_brewer(palette="Dark2")+
  scale_y_continuous(labels = scales::percent)

#just giving up and trying regex at this point 
HHS$`Biggest health concern` <- tolower(HHS$`Biggest health concern`)

# Regular expression
regex_mental_health2 <- "\\b(depresión|angry|metal|emotional|mental|depressed|sad|anixety|stress|bipolar|adhd|bpd|mental healt|mental health|psychologist|psychology|depression|anxiety|therapy|counseling|counselling|bipolar|psychological|therapist|rehab|psychiatrist)\\b"
regex_chronic <- "\\b(worsening condition|lack of function|addiction|arthritis|chronic|chronic liver disease|disability|chronic illness|chronic pain|smoking|chronic sinus|vaping|pain i live with|immuno disorder)\\b"
regex_sexual_health <- "\\b(haven't been able to have children|gynecology|planning method|birth|pregnancy|fertility|gynecologist|pregnant|gynecological|std|sexual|sexual health|sti|hiv|reproductivo)\\b"
regex_health_conditions <- "\\b(kidneys|allergies|breast|dementia|tumor|nerves|ibd|neurologist|aches|bleeding|coughing|breathing|monkey pox|vision|celiac|skin|tooth|vitamin|b12|keloid|bladder|prostate|rare disease|shoulder|feet|veins|body aches|throat|very little physical energy|hand|ovaries|back|polio|abscess|artritis|eye|chest|arm|osteoporosis|prediabetic|cardiologist|cholesetrol|circulation|sciatic|broken bones|diabetic|leg pain|pain in the arm|head|liver|sciatica|illness|operating|reflux|infection|dental|blood sugar|eyesight|spinal|fatigue|tired|lung|epilepsy|breasts|eyes|stomach|anemia|teeth|hypertension|thyroid|gout|headaches|knee|fibromyalgia|heart|blood pressure|lungs|legs|foot|pacemaker|asthma|eczema|glaucoma|heart attack|spine|injuries|siatic pains|high blood pressure|kidney|diabetes'|surgery|dental issues|knees|scoliosis|cancer|cholesterol|diabetes|prediabetes|bowel issues|knee injury|period pain|rectal pain)\\b"
regex_nutrition_weight <- "\\b(eating well|heavier|underweight|weight gain|obesity|food storage|overweight|not eating enough|weight|diet|nutrition|dietician|dietitian|diet|dieting|food classes|eat and exercise|meals|nutritionists|food)\\b"
regex_insurance2 <- "\\b(cost|funds|no medicine|money|insurance|coverage|money|check cashing|paying|for free|free services|free|payments|reduce cost|discount)\\b"
regex_access <- "\\b(attention|hormones|endocrinologist|hrt|lgbtq+|bipoc|falling through the cracks|trans|transgender|gender affirming)\\b"
regex_covid <-"\\b(covid|covid-19|pandemic)\\b"
regex_worry <-"\\b(new issues|not getting worse|to be well|health in general|worse|safety|90|proper care|everything|having something|healthy|dying young|old|sick|stay physically well|live long|not being well enough|health is bad|physical health|accident)\\b"


library(tidyverse)
HHS <- HHS %>%
  mutate(
    Concern_Category = case_when(
      grepl(regex_access, HHS$`Biggest health concern`) ~ "transgender health care",
      grepl(regex_mental_health2, HHS$`Biggest health concern`) ~ "mental health",
      grepl(regex_insurance2, HHS$`Biggest health concern`) ~ "insurance",
      grepl(regex_covid, HHS$`Biggest health concern`) ~ "covid",
      grepl(regex_nutrition_weight, HHS$`Biggest health concern`) ~ "nutrition/weight",
      grepl(regex_chronic, HHS$`Biggest health concern`) ~ "chronic conditions",
      grepl(regex_sexual_health, HHS$`Biggest health concern`) ~ "sexual health",
      grepl(regex_health_conditions, HHS$`Biggest health concern`) ~ "specific health concerns",
      grepl(regex_worry, HHS$`Biggest health concern`) ~ "general worry/fear",
      TRUE ~ "other"
    )
  ) 
HHS$Concern_Category[is.na(HHS$`Biggest health concern`)] <- NA

library(descr)
library(scales)
freq(HHS$Concern_Category)

#graph of clusters 
library(tidyverse)

HHS <- HHS %>% 
  mutate(Clinic = recode(Clinic, 
                         "Providence Community Health Center: Atwood" = "Providence Community\nHealth Center: Atwood",
                         "Providence Community Health Center: Chafee" = "Providence Community\nHealth Center: Chafee",
                         "Providence Community Health Center: Randall Square" = "Providence Community\nHealth Center: Randall Square"))

ggplot(data=subset(HHS, !is.na(Concern_Category)))+
  geom_bar(aes(x=Concern_Category, fill=Clinic), position="fill")+
  labs(x = "Category of Biggest Health Concern", y = "Percentage Reporting Each Category", title="Percentage Reporting Each Category of Health Concern\nAcross All Six Clinics") +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust=1, size=10))+
  scale_fill_brewer(palette="Dark2")+
  scale_y_continuous(labels = scales::percent)

freq(HHS$Concern_Category)

#clusters for how clinics can help 
ggplot(data=subset(HHS, !is.na(Help_Category)))+
  geom_bar(aes(x=Help_Category, fill=Clinic), position="fill")+
  labs(x = "Category of Biggest Expressed Need", y = "Percentage Reporting Each Category", title="Percentage Reporting Each Category of Expressed Need\nAcross All Six Clinics") +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust=1, size=10))+
  scale_fill_brewer(palette="Dark2")+
  scale_y_continuous(labels = scales::percent)

freq(HHS$Help_Category)

#clusters by clinic 
HHS <- HHS %>% 
  mutate(Gender = recode(Gender,
                         "Genderqueer, neither exclusively man nor woman" = "Genderqueer"))

ggplot(data=subset(HHS, !is.na(Help_Category) & Clinic=="Open Door Health" & !is.na(Gender)))+
  geom_bar(aes(x=Help_Category, fill=Gender), position="fill")+
  labs(x = "Category of Biggest Expressed Need", y = "Percentage Reporting Each Category", title="Percentage Reporting Each Category of Expressed Need \nat Open Door Health") +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust=1, size=10))+
  scale_fill_brewer(palette="Dark2")+
  scale_y_continuous(labels = scales::percent)

freq(HHS$Race)
HHS$Race[HHS$Race=="Mestiza"|HHS$Race=="Mexican"|HHS$Race=="Mexican Indian"|HHS$Race=="Dominican"|HHS$Race=="Caribbean"|HHS$Race=="Colombia"|HHS$Race=="Guatemalan"|HHS$Race=="Haitian"|HHS$Race=="Puerto Rican"|HHS$Race=="Bolivia"|HHS$Race=="Central American"]<- "Hispanic/Latinx"
HHS$Race[HHS$Race=="Cape Verdean"|HHS$Race=="Lebanon"|HHS$Race=="Somalian"|HHS$Race=="African"] <- "Black/African American"
HHS$Race[HHS$Race=="Middle Eastern"] <- "White"
HHS$Race[HHS$Race=="China"] <- "Asian"

summary(HHS$Age)
HHS$`Household Size`[HHS$`Household Size`==0] <- NA
summary(HHS$`Household Size`)

#Creating poverty variable 
HHS$Poverty <- "No Poverty"
HHS$Poverty[HHS$`Household Income`=="Less than $10,000"|HHS$`Household Income`=="$10,000 to $15,000"|HHS$`Household Income`=="$15,001 to $20,000" & HHS$`Household Size`>= 2] <- "Poverty"
HHS$Poverty[HHS$`Household Income`=="Less than $10,000"|HHS$`Household Income`=="$10,000 to $15,000"& HHS$`Household Size`==1]<- "Poverty"

freq(HHS$Poverty)

#clusters by poverty level
ggplot(data=subset(HHS, !is.na(Help_Category) & Clinic=="Open Door Health" & !is.na(Poverty)))+
  geom_bar(aes(x=Help_Category, fill=Poverty), position="fill")+
  labs(x = "Category of Biggest Expressed Need", y = "Percentage Reporting Each Category", title="Percentage Reporting Each Category of Expressed Need\nat Open Door Health") +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust=1, size = 10))+
  scale_fill_brewer(palette="Set2")+
  scale_y_continuous(labels = scales::percent)

#poverty and education level 
#making education level an ordered factor 
HHS$`Education level` <- factor(HHS$`Education level`, levels = c("Never attended school or only attended kindergarten",
                                                                  "Grades 1 through 8 (Elementary/Middle school)",
                                                                  "Grades 9 through 11 (Some high school)",
                                                                  "Grade 12 or GED (High school graduate)",
                                                                  "Some college or technical school", 
                                                                  "College graduate",
                                                                  "Graduate School (Master's degree or above)"),
                                ordered=T)
HHS <- HHS %>% 
  mutate(`Education level` = recode(`Education level`, 
                           "Never attended school or only attended kindergarten" = "Never attended school.",
                           "Grades 1 through 8 (Elementary/Middle school)" = "Grades 1 through 8",
                           "Grades 9 through 11 (Some high school)" = "Grades 9 through 11",
                           "Grade 12 or GED (High school graduate)" = "High school graduate",
                           "Graduate School (Master's degree or above)" = "Graduate School"))



ggplot(data=subset(HHS, !is.na(`Education level`) & Clinic=="Open Door Health" & !is.na(Poverty)))+
  geom_bar(aes(x=`Education level`, fill=Poverty), position="fill")+
  labs(x = "Education Level", 
       y = "Percentage Impoverished for Each Level", 
       title="Relationship Between Education Level and Poverty Status") +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust=1, size = 10))+
  scale_fill_brewer(palette="Reds")+
  scale_y_continuous(labels = scales::percent)

#all clinics, poverty and relationship to expressed need
b <- ggplot(data=subset(HHS, !is.na(Help_Category) & !is.na(Poverty)))+
  geom_bar(aes(x=Help_Category, fill=Poverty), position="fill")+
  labs(x = "Category of Biggest Expressed Need", y = "Percentage Reporting Each Category", title="Percentage Reporting Each Category of Expressed Need\nAcross Clinics") +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust=1, size = 10))+
  scale_fill_brewer(palette="Set2")+
  scale_y_continuous(labels = scales::percent)

#all clinics, poverty and relationship to biggest health concern
a <- ggplot(data=subset(HHS, !is.na(Concern_Category) & !is.na(Poverty)))+
  geom_bar(aes(x=Concern_Category, fill=Poverty), position="fill")+
  labs(x = "Category of Biggest Health Concern", y = "Percentage Reporting Each Category", title="Percentage Reporting Each Category of Health Concern\nAcross Clinics") +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust=1, size = 10))+
  scale_fill_brewer(palette="Set2")+
  scale_y_continuous(labels = scales::percent)

#combining both graphs into one image for report
library(ggplot2)
library(ggpubr)

ggarrange(a, b,
          ncol = 1, nrow = 2)

#gender across clinics 
ggplot(data=subset(HHS, !is.na(Help_Category) & !is.na(Gender)))+
  geom_bar(aes(x=Help_Category, fill=Gender), position="fill")+
  labs(x = "Category of Biggest Expressed Need", y = "Percentage Reporting Each Category", title="Percentage Reporting Each Category of Expressed Need \nAcross Clinics") +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust=1, size=10))+
  scale_fill_brewer(palette="Dark2")+
  scale_y_continuous(labels = scales::percent)

library(descr)
freq(HHS$Gender)

HHS2 <- subset(HHS, Clinic=="Open Door Health")
freq(HHS2$Gender)

ggplot(data=subset(HHS, !is.na(Concern_Category) & !is.na(Gender)))+
  geom_bar(aes(x=Concern_Category, fill=Gender), position="fill")+
  labs(x = "Category of Biggest Health Concern", y = "Percentage Reporting Each Category", title="Percentage Reporting Each Category of Health Concern\nAcross Clinics") +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust=1, size=10))+
  scale_fill_brewer(palette="Dark2")+
  scale_y_continuous(labels = scales::percent)

#race across clinics 
ggplot(data=subset(HHS, !is.na(Help_Category) & !is.na(Race)))+
  geom_bar(aes(x=Help_Category, fill=Race), position="fill")+
  labs(x = "Category of Biggest Expressed Need", y = "Percentage Reporting Each Category", title="Percentage Reporting Each Category of Expressed Need \nAcross Clinics") +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust=1, size=10))+
  scale_fill_brewer(palette="Spectral")+
  scale_y_continuous(labels = scales::percent)

library(descr)
freq(HHS$Race)

HHS2 <- subset(HHS, Clinic=="Open Door Health")
freq(HHS2$Race)

ggplot(data=subset(HHS, !is.na(Concern_Category) & !is.na(Race)))+
  geom_bar(aes(x=Concern_Category, fill=Race), position="fill")+
  labs(x = "Category of Biggest Health Concern", y = "Percentage Reporting Each Category", title="Percentage Reporting Each Category of Health Concern\nAcross Clinics") +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust=1, size=10))+
  scale_fill_brewer(palette="Spectral")+
  scale_y_continuous(labels = scales::percent)

#age graph 
ggplot(data = subset(HHS, !is.na(Help_Category) & !is.na(Age)))+
  geom_boxplot(aes(x=Help_Category, y=Age, fill=Help_Category))+
  labs(x = "Category of Biggest Expressed Need", y = "Percentage Reporting Each Category", title="Percentage Reporting Each Category of Expressed Need \nAcross Clinics") +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust=1, size=10), legend.position="none")
#this was unhelpful, not discussing it in analysis

#country of birth, new variable to show if it is english-speaking or not 
freq(HHS$`Country of Birth`)

HHS <- HHS %>% 
  mutate(English = ifelse(`Country of Birth`=="Canada"|`Country of Birth`=="United States"|`Country of Birth`=="Ghana"|`Country of Birth`=="Nigeria"|`Country of Birth`=="India"|`Country of Birth`=="Gambia"|`Country of Birth`=="Malta"|`Country of Birth`=="Zimbabwe"|
                            `Country of Birth`=="Australia"|`Country of Birth`=="Singapore"|`Country of Birth`=="Belize"|`Country of Birth`=="New Zealand"|`Country of Birth`=="Barbados"|`Country of Birth`=="Guyana"|
                            `Country of Birth`=="Grenada"|`Country of Birth`=="Botswana"|`Country of Birth`=="South Africa"|`Country of Birth`=="United Kingdom"|`Country of Birth`=="Philippines"|
                            `Country of Birth`=="Bahamas"|`Country of Birth`=="Dominica"|`Country of Birth`=="Namibia"|`Country of Birth`=="Ireland"|`Country of Birth`=="US Virgin Islands","English-speaking","Non-English speaking"))

#country of birth across clinics 
ggplot(data=subset(HHS, !is.na(Help_Category) & !is.na(English)))+
  geom_bar(aes(x=Help_Category, fill=English), position="fill")+
  labs(x = "Category of Biggest Expressed Need", y = "Percentage Reporting Each Category", title="Percentage Reporting Each Category of Expressed Need \nAcross Clinics") +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust=1, size=10))+
  scale_fill_brewer(palette="Set2")+
  scale_y_continuous(labels = scales::percent)

ggplot(data=subset(HHS, !is.na(Concern_Category) & !is.na(English)))+
  geom_bar(aes(x=Concern_Category, fill=English), position="fill")+
  labs(x = "Category of Biggest Health Concern", y = "Percentage Reporting Each Category", title="Percentage Reporting Each Category of Health Concern\nAcross Clinics") +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust=1, size=10))+
  scale_fill_brewer(palette="Set2")+
  scale_y_continuous(labels = scales::percent)
freq(HHS$English)
