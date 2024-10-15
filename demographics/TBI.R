rm(list = ls())

#load required libraries
library(gtsummary)
library(tidyverse)
library(readxl)
library(readr)
library(dplyr)
library(writexl)

#read files in
demog <- read_excel("Matching_samples_image_blood_pbmcs_July_2023.xlsx")
tbi <- read_csv("HumanConnectomeProje-TBIScreenSummaryUpda_DATA_2023-06-05_0223.csv")
subs <- read_csv("caselist.csv")

#list of subjects with imaging data from spartan (excluding subjects with poor image quality)
#made using ls > subjects.txt
names(tbi)[1] <- "ID"
tbi$ID <- substring(tbi$ID, 1, nchar(tbi$ID) -2)
total <- merge(demog, tbi, by = "ID", all = TRUE)

#only import subjects with imaging data listed in miss
total <- total[(total$ID %in% subs$ID),]
total <- total[ which(!is.na(total$Age)),  ]
total <- total[ which(!is.na(total$Group)),  ]

tbi2 <- as.data.frame(cbind(total$ID, total$Group, total$Age, total$Sex, total$tbi_number_inj, total$age_first_inj, total$age_recent_inj, total$tbi_severity_rating))
names(tbi2) <- c("ID", "Group", "Age", "Sex", "Number of TBIs", "Age of First TBI", "Age of Most Recent TBI", "TBI Severity")
tbi2 <- subset(tbi2, tbi2$`Number of TBIs` > 0)
write_xlsx(tbi2,"tbi.xlsx")

#initialise group variable
Group = rep(NA,length(tbi2$ID))
Group[which(tbi2$Group == 1)] <- 'Psychosis'
Group[which(tbi2$Group == 0)] <- 'Control'

tbi3 <- as_data_frame(cbind(Group, tbi2$`Number of TBIs`, tbi2$`Age of First TBI`, tbi2$`Age of Most Recent TBI`, tbi2$`TBI Severity`))
names(tbi3) <- c("Group", "Number of TBIs", "Age of First TBI", "Age of Most Recent TBI", "TBI Severity")
tbi3$`Number of TBIs` <- as.numeric(tbi3$`Number of TBIs`)
tbi3$`Age of First TBI` <- as.numeric(tbi3$`Age of First TBI`)
tbi3$`Age of Most Recent TBI` <- as.numeric(tbi3$`Age of Most Recent TBI`)
tbi3$`TBI Severity` <- as.numeric(tbi3$`TBI Severity`)


tbi3%>%
  tbl_summary(
    by = Group, 
    missing = "no", #only want to see missing values for smoking status, which is expressed as missing category
  type = list(`Number of TBIs` ~ "continuous", `TBI Severity` ~ "continuous"),
    statistic = list(
      all_continuous() ~ "{mean} ({sd})", #continuous variables show mean (sd) 
      all_categorical() ~ "{n} ({p}%)" #categorical variables show n (p%)
    ), digits = all_continuous() ~ 2) %>% 
  bold_labels() %>%
  add_p() %>% bold_p() %>%
  modify_spanning_header(c("stat_1", "stat_2") ~ "**Group**") #make spanning header over the group sub-categories




