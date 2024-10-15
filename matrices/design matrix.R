rm(list=ls())
#load required libraries
library(tidyverse)
library(readxl)
library(readr)
library(dplyr)
library(writexl)
library(Jmisc)
library(normalize)

#import demog and subjects with imaging data
demog <- read_excel("Matching_samples_image_blood_pbmcs_July_2023.xlsx")
subs <- read_csv("caselist.csv")
tiv <- read_csv("asegstats.csv")

#list of subjects with imaging data from spartan (excluding subjects with poor image quality)
#made using ls > subjects.txt
names(tiv)[1] <- "ID"
names(subs)[1] <- "ID"

demog <- merge(demog, tiv, by = "ID", all = TRUE)

#only import subjects with imaging data listed in miss
#demog_subset <- demog[demog$image_data_available == 1, ]
#demog <-demog_subset
demog <- demog[(demog$ID %in% subs$ID),]

#remove subjects without Age or Group data
demog <- subset(demog, !is.na(demog$Age))
demog <- subset(demog, !is.na(demog$Group))
demog <- subset(demog, !is.na(demog$Sex))

tivmean <- subset(demog$EstimatedTotalIntraCranialVol, !is.na(demog$EstimatedTotalIntraCranialVol))

#replace NA values with mean of TIV
demog$EstimatedTotalIntraCranialVol <- ifelse(is.na(demog$EstimatedTotalIntraCranialVol), 
                                              mean(tivmean, na.rm = TRUE), 
                                              demog$EstimatedTotalIntraCranialVol)
mean(demog$EstimatedTotalIntraCranialVol)

patient = rep(NA,length(demog$ID))
patient[which(demog$Group == 1)] <- 1
patient[which(demog$Group == 0)] <- 0

control = rep(NA,length(demog$ID))
control[which(demog$Group == 1)] <- 0
control[which(demog$Group == 0)] <- 1

#change to numeric to enable analyses
demog$Group <- as.numeric(demog$Group)
demog$Age <- as.numeric(demog$Age)
demog$Sex <- as.numeric(demog$Sex)

#make df for scanner information
scanner <- data.frame(Indiana= rep(0, length(demog$ID)),
                      BIDMC= rep(0, length(demog$ID)))
#scanner 1 = indiana, scanner 2 = BIDMC, scanner 3 = McLean (don't need to list)
scanner$Indiana[which(demog$Scanner_SerialNumber == '66027')] <- 1
scanner$BIDMC[which(demog$Scanner_SerialNumber == '66109')] <- 1

#list of subjects who have imaging and demographic data
subjects <- as.data.frame(demog$ID)
names(subjects) <- c("subjects")

#print into subjects file for fixelcfestats command
write_xlsx(subjects,"~/subjects.xlsx")

#create design matrix for fixelcfestats command
scanner2 <- as.data.frame(cbind(subjects, patient, control, demog$Age, demog$Sex, demog$EstimatedTotalIntraCranialVol))
names(scanner2) <- c("Subjects", "Patient", "Control", "Age", "Sex", "TIV")

scanner2$Age <- demean(scanner2$Age)
scanner2$Sex <- demean(scanner2$Sex)
scanner2$TIV <- log(scanner2$TIV)
scanner2$TIV <- demean(scanner2$TIV)

#print design matrix
write_xlsx(scanner2,"/home/gucci/Documents/matrices/design_matrixFC_harmonized_LABELLED.xlsx")
