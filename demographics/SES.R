rm(list=ls())
#load required libraries
library(gtsummary)
library(tidyverse)
library(readxl)
library(readr)
library(dplyr)
library(ggplot2)
library(writexl)

#read in excel file
demog <- read_excel("Matching_samples_image_blood_pbmcs_July_2023.xlsx")
pses <- read_csv("HumanConnectomeProje-ParentalSESNDAJuly20_DATA_2023-08-28_0337.csv")
subs <- read_csv("caselist.csv")

#change label of ID column

names(pses)[1] <- "ID"
pses$ID <- substring(pses$ID, 1, nchar(pses$ID) -2)
demog$ID <- as.character(demog$ID)

#merge and exclude na values
total <- merge(demog, pses, by = "ID", all = TRUE)

#only import subjects with imaging data listed in miss
total <- total[(total$ID %in% subs$ID),]

total <- total[ which(!is.na(total$Age)),  ]
total <- total[ which(!is.na(total$Group)),  ]

#initialise group variable
Group = rep(NA,length(total$ID))
Group[which(total$Group == 1)] <- 'Psychosis'
Group[which(total$Group == 0)] <- 'Control'

pses2 <- as.data.frame(cbind(Group, total$ID, total$Age, total$Sex, total$father, total$father_s_educational_level, total$mother_s_employment_level, total$mother_s_educational_level, total$parental_socioeconomic_lev))
names(pses2) <- c("Group", "ID", "Age", "Sex", "Father Employment Level", "Father Education Level", "Mother Employment Level", "Mother Education Level", "Parental SES Level")
pses2 <- subset(pses2, !is.na(pses2$`Parental SES Level`))
write_xlsx(pses2,"/matrices/SES.xlsx")

pses3 <- as.data.frame(cbind(pses2$Group, pses2$`Father Employment Level`, pses2$`Father Education Level`, pses2$`Mother Employment Level`, pses2$`Mother Education Level`, pses2$`Parental SES Level`))
names(pses3) <- c("Group", "Father Employment Level", "Father Education Level", "Mother Employment Level", "Mother Education Level", "Parental SES Level")

pses3$`Father Employment Level` <- as.numeric(pses3$`Father Employment Level`)
pses3$`Mother Employment Level` <- as.numeric(pses3$`Mother Employment Level`)
pses3$`Father Education Level` <- as.numeric(pses3$`Father Education Level`)
pses3$`Mother Education Level` <- as.numeric(pses3$`Mother Education Level`)
pses3$`Parental SES Level` <- as.numeric(pses3$`Parental SES Level`)


pses3%>%
  tbl_summary(
    by = Group, 
    missing = "no", #only want to see missing values for smoking status, which is expressed as missing category
    type = list(`Father Employment Level` ~ "continuous", `Father Education Level` ~ "continuous", `Mother Employment Level` ~ "continuous", `Mother Education Level` ~ "continuous", `Parental SES Level` ~ "continuous"),
    statistic = list(
      all_continuous() ~ "{mean} ({sd})", #continuous variables show mean (sd) 
      all_categorical() ~ "{n} ({p}%)" #categorical variables show n (p%)
    ), digits = all_continuous() ~ 2) %>% 
  bold_labels() %>%
  add_p() %>% bold_p() %>%
  modify_spanning_header(c("stat_1", "stat_2") ~ "**Group**") #make spanning header over the group sub-categories

