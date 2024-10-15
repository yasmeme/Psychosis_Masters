rm(list=ls())
#load required libraries
library(gtsummary)
library(tidyverse)
library(readxl)
library(readr)
library(dplyr)

#read in excel file
demog <- read_excel("Matching_samples_image_blood_pbmcs_July_2023.xlsx")
scid <- read_csv("HumanConnectomeProje-SCID5NDAJuly2023_DATA_2023-08-28_0342.csv")
subs <- read_csv("caselist.csv")

#change label of ID column

names(scid)[1] <- "ID"
scid$ID <- substring(scid$ID, 1, nchar(scid$ID) -2)
demog$ID <- as.character(demog$ID)
subs$ID <- as.character(subs$ID)

total <- merge(demog, scid, by = "ID", all = TRUE)

#only import subjects with imaging data listed in miss
total <- total[(total$ID %in% subs$ID),]

total <- total[ which(!is.na(total$Age)),  ]
total <- total[ which(!is.na(total$Group)),  ]

total <- subset(total, !is.na(total$scid_29_lp))
total2 <- as.data.frame(cbind(total$ID, total$Group, total$Age, total$Sex, total$scid_21_lp, total$scid_21_p12m, total$scid_22_lp, total$scid_22_p12m, total$scid_23_lp, total$scid_23_p12m, total$scid_24_lp, total$scid_24_p12m, total$scid_25_lp, total$scid_25_p12m, total$scid_26_lp, total$scid_26_p12m, total$scid_27_lp, total$scid_27_p12m, total$scid_28_lp, total$scid_28_p12m, total$scid_29_lp, total$scid_29_p12m))
names(total2) <- c("ID", "Group", "Age", "Sex", "Alcohol Life", "Alcohol 12m", "Sedative Life", "Sedative 12m", "Cannabis Life", "Cannabis 12m", "Stimulant Life", "Stimulant 12m", "Opioid Life", "Opioid 12m", "PCP Life", "PCP 12m", "Hallucinogen Life", "Hallucinogen 12m", "Inhalant Life", "Inhalant 12m", "Other Life", "Other 12m")

#initialise group variable
Group = rep(0,length(total2$ID))
Group[which(total2$Group == 1)] <- 'Psychosis'
Group[which(total2$Group == 0)] <- 'Control'

#initialise alcohol variable
alcohol <- rep(0,length(total2$ID))
alcohol[which(total2$`Alcohol Life` == 1)] <- 0
alcohol[which(total2$`Alcohol Life` == 2)] <- 1
alcohol[which(total2$`Alcohol Life`== 3)] <- 1
alcohol2 <- rep(0,length(total2$ID))
#alcohol2[which(is.na(alcohol2))] <- 0
alcohol2[which(total2$`Alcohol 12m` == 1)] <- 0
#alcohol2[which(total2$`Alcohol 12m` == "1")] <- 0 
alcohol2[which(total2$`Alcohol 12m` == 2)] <- 1
alcohol2[which(total2$`Alcohol 12m` == 3)] <- 1

#initialise sedative variable
sedative <- rep(0,length(total2$ID))
sedative[which(total2$`Sedative Life` == 1)] <- 0
sedative[which(total2$`Sedative Life` == 2)] <- 1
sedative[which(total2$`Sedative Life` == 3)] <- 1
sedative2 <- rep(0,length(total2$ID))
sedative2[which(total2$`Sedative 12m` == 1)] <- 0
sedative2[which(total2$`Sedative 12m` == 2)] <- 1
sedative2[which(total2$`Sedative 12m` == 3)] <- 1

#initialise cannabis variable
cannabis <- rep(0,length(total2$ID))
cannabis[which(total2$`Cannabis Life` == 1)] <- 0
cannabis[which(total2$`Cannabis Life` == 2)] <- 1
cannabis[which(total2$`Cannabis Life` == 3)] <- 1
cannabis2 <- rep(0,length(total2$ID))
cannabis2[which(total2$`Cannabis 12m` == 1)] <- 0
cannabis2[which(total2$`Cannabis 12m` == 2)] <- 1
cannabis2[which(total2$`Cannabis 12m` == 3)] <- 1

#initialise stimulant variable
stimulant <- rep(0,length(total2$ID))
stimulant[which(total2$`Stimulant Life` == 1)] <- 0
stimulant[which(total2$`Stimulant Life` == 2)] <- 1
stimulant[which(total2$`Stimulant Life` == 3)] <- 1
stimulant2 <- rep(0,length(total2$ID))
stimulant2[which(total2$`Stimulant 12m` == 1)] <- 0
stimulant2[which(total2$`Stimulant 12m` == 2)] <- 1
stimulant2[which(total2$`Stimulant 12m` == 3)] <- 1

#initialise opioid variable
opioid <- rep(0,length(total2$ID))
opioid[which(total2$`Opioid Life` == 1)] <- 0
opioid[which(total2$`Opioid Life` == 2)] <- 1
opioid[which(total2$`Opioid Life` == 3)] <- 1
opioid2 <- rep(0,length(total2$ID))
opioid2[which(total2$`Opioid 12m` == 1)] <- 0
opioid2[which(total2$`Opioid 12m` == 2)] <- 1
opioid2[which(total2$`Opioid 12m` == 3)] <- 1

#initialise PCP variable
PCP <- rep(0,length(total$ID))
PCP[which(total2$`PCP Life` == 1)] <- 0
PCP[which(total2$`PCP Life` == 2)] <- 1
PCP[which(total2$`PCP Life` == 3)] <- 1
PCP2 <- rep(0,length(total$ID))
PCP2[which(total2$`PCP 12m` == 1)] <- 0
PCP2[which(total2$`PCP 12m` == 2)] <- 1
PCP2[which(total2$`PCP 12m` == 3)] <- 1

#initialise hallucinogen variable
hallucinogen <- rep(0,length(total$ID))
hallucinogen[which(total2$`Hallucinogen Life` == 1)] <- 0
hallucinogen[which(total2$`Hallucinogen Life` == 2)] <- 1
hallucinogen[which(total2$`Hallucinogen Life` == 3)] <- 1
hallucinogen2 <- rep(0,length(total$ID))
hallucinogen2[which(total2$`Hallucinogen 12m` == 1)] <- 0
hallucinogen2[which(total2$`Hallucinogen 12m` == 2)] <- 1
hallucinogen2[which(total2$`Hallucinogen 12m` == 3)] <- 1

#initialise inhalant variable
inhalant <- rep(0,length(total$ID))
inhalant[which(total2$`Inhalant Life` == 1)] <- 0
inhalant[which(total2$`Inhalant Life` == 2)] <- 1
inhalant[which(total2$`Inhalant Life` == 3)] <- 1
inhalant2 <- rep(0,length(total$ID))
inhalant2[which(total2$`Inhalant 12m` == 1)] <- 0
inhalant2[which(total2$`Inhalant 12m` == 2)] <- 1
inhalant2[which(total2$`Inhalant 12m` == 3)] <- 1

#initialise other variable
other <- rep(0,length(total$ID))
other[which(total2$`Other Life` == 1)] <- 0
other[which(total2$`Other Life` == 2)] <- 1
other[which(total2$`Other Life` == 3)] <- 1
other2 <- rep(0,length(total$ID))
other2[which(total2$`Other 12m` == 1)] <- 0
other2[which(total2$`Other 12m` == 2)] <- 1
other2[which(total2$`Other 12m` == 3)] <- 1

#create new combined data frame with above initialised variables so that table can be generated
scid2 <- as.data.frame(cbind(Group, alcohol, cannabis, sedative, stimulant, hallucinogen, inhalant, other))

#rename variables for table 
names(scid2) <- c("Group", "Alcohol Lifetime Diagnosis","Cannabis Lifetime Diagnosis", "Sedative Lifetime Diagnosis", "Stimulant Lifetime Diagnosis", "Hallucinogen Lifetime Diagnosis","Inhalant Lifetime Diagnosis","Other Lifetime Diagnosis")
scid2$`Alcohol Lifetime Diagnosis` <- as.numeric(scid2$`Alcohol Lifetime Diagnosis`)
scid2$`Sedative Lifetime Diagnosis` <- as.numeric(scid2$`Sedative Lifetime Diagnosis`)
scid2$`Cannabis Lifetime Diagnosis` <- as.numeric(scid2$`Cannabis Lifetime Diagnosis`)
scid2$`Stimulant Lifetime Diagnosis` <- as.numeric(scid2$`Stimulant Lifetime Diagnosis`)
scid2$`Hallucinogen Lifetime Diagnosis` <- as.numeric(scid2$`Hallucinogen Lifetime Diagnosis`)
scid2$`Inhalant Lifetime Diagnosis` <- as.numeric(scid2$`Inhalant Lifetime Diagnosis`)
scid2$`Other Lifetime Diagnosis` <- as.numeric(scid2$`Other Lifetime Diagnosis`)

#convert to numeric in order to recognise continuous variables in table instead of listing each unique
#value as a separate category

#run table
scid2%>%
  tbl_summary(
    by = Group, #separates into control and psychosis group
    missing = "ifany", #only want to see missing values for smoking status, which is expressed as missing category
    include = c("Group", "Alcohol Lifetime Diagnosis","Cannabis Lifetime Diagnosis","Sedative Lifetime Diagnosis", "Stimulant Lifetime Diagnosis","Hallucinogen Lifetime Diagnosis","Inhalant Lifetime Diagnosis","Other Lifetime Diagnosis"),
    statistic = list(
      all_continuous() ~ "{mean} ({sd})", #continuous variables show mean (sd) 
      all_categorical() ~ "{n} ({p}%)" #categorical variables show n (p%)
    ), digits = all_continuous() ~ 2) %>% 
  bold_labels() %>%
  add_p() %>% bold_p() %>%
  modify_spanning_header(c("stat_1", "stat_2") ~ "**Group**") #make spanning header over the group sub-categories
