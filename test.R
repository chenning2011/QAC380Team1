#team 1, loading in dataset (everyone will probably need diff. load lines, unless someone can get the excel data to work in github)

library(readxl)
#caleb load line
HHSNeedsAssessment_FINAL_DATA <- read_excel("~/Desktop/Folder/University/Junior Year/F23/QAC380/HHSNeedsAssessment FINAL DATA.xlsx")


require(ggplot2)
require(dplyr)
require(tidyr)
require(descr)
require(gert)