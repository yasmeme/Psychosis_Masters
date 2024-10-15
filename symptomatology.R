rm(list = ls())

#load required libraries
library(gtsummary)
library(tidyverse)
library(readxl)
library(readr)
library(dplyr)

#read files in
diagnosis <- read_csv("/home/gucci/Documents/input/HumanConnectomeProje-DiagnosisNDAJuly2023_DATA_2023-08-07_0228.csv")
panss <- read_csv("/home/gucci/Documents/input/HumanConnectomeProje-PANSSUpdatedNDAJuly2_DATA_2023-08-07_0217.csv")
madrss <- read_csv("/home/gucci/Documents/input/HumanConnectomeProje-MADRSNDAJuly2023_DATA_2023-08-07_0218.csv")
wasi <- read_csv("/home/gucci/Documents/input/HumanConnectomeProje-WASIIINDAJuly2023_DATA_2023-08-07_0219.csv")
demog <- read_excel("/home/gucci/Documents/input/Matching_samples_image_blood_pbmcs_July_2023.xlsx")
subs <- read_csv("/home/gucci/Documents/input/caselist.csv")

#make column 1 name match so NA values can be excluded
names(diagnosis)[1] <- "ID"
diagnosis$ID <- substring(diagnosis$ID, 1, nchar(diagnosis$ID) -2)
names(panss)[1] <- "ID"
panss$ID <- substring(panss$ID, 1, nchar(panss$ID) -2)
names(madrss)[1] <- "ID"
madrss$ID <- substring(madrss$ID, 1, nchar(madrss$ID) -2)
names(wasi)[1] <- "ID"
wasi$ID <- substring(wasi$ID, 1, nchar(wasi$ID) -2)
demog$ID <- as.character(demog$ID)

#merge all datasets
total_list <- list(demog, diagnosis, panss, madrss, wasi)
totalsymp <- total_list %>% reduce(inner_join, by='ID')

#only import subjects with imaging data listed in miss
totalsymp <- totalsymp[(totalsymp$ID %in% subs$ID),]
totalsymp <- totalsymp[ which(!is.na(totalsymp$ID)),  ]

#initialise group variable
Group = rep(NA,length(totalsymp$ID))
Group[which(totalsymp$Group == 1)] <- 'Psychosis'
Group[which(totalsymp$Group == 0)] <- NA

#Allocating diagnosis code to diagnosis name
Diagnosis_df <- totalsymp$cp_dc
Diagnosis_df[which(totalsymp$cp_dc == 295.9)] <- 'Schizophrenia'
Diagnosis_df[which(totalsymp$cp_dc == '295.90')] <- 'Schizophrenia'
Diagnosis_df[which(totalsymp$cp_dc == 295.4)] <- 'Schizophreniform'
Diagnosis_df[which(totalsymp$cp_dc == '295.40')] <- 'Schizophreniform'
Diagnosis_df[which(totalsymp$cp_dc == 295.7)] <- 'Schizoaffective Disorder'
Diagnosis_df[which(totalsymp$cp_dc == '295.70')] <- 'Schizoaffective Disorder'
Diagnosis_df[which(totalsymp$cp_dc == 296.24)] <- 'Major Depressive Disorder'
Diagnosis_df[which(totalsymp$cp_dc == 297.1)] <- 'Delusional Disorder'
Diagnosis_df[which(totalsymp$cp_dc == '297.10')] <- 'Delusional Disorder'
Diagnosis_df[which(totalsymp$cp_dc == 298.8)] <- 'Psychosis NOS'
Diagnosis_df[which(totalsymp$cp_dc == '298.80')] <- 'Psychosis NOS'
Diagnosis_df[which(totalsymp$cp_dc == 296.44)] <- 'Bipolar Disorder'
Diagnosis_df[which(totalsymp$cp_dc == 300.29)] <- NA
Diagnosis_df[which(totalsymp$cp_dc == '305.00')] <- NA
Diagnosis_df[which(totalsymp$cp_dc == 296.2)] <- NA
Diagnosis_df[which(totalsymp$cp_dc == '296.20')] <- NA
Diagnosis_df[which(totalsymp$cp_dc == 292.9)] <- NA
Diagnosis_df[which(totalsymp$cp_dc == '292.90')] <- NA
Diagnosis_df[which(totalsymp$cp_dc == 296.1)] <- 'Bipolar Disorder'
Diagnosis_df[which(totalsymp$cp_dc == 296.21)] <- NA
Diagnosis_df[which(totalsymp$cp_dc == 296.34)] <- 'Major Depressive Disorder'
Diagnosis_df[which(totalsymp$cp_dc == '296.40')] <- 'Bipolar Disorder'
Diagnosis_df[which(totalsymp$cp_dc == 296.53)] <- 'Bipolar Disorder'
Diagnosis_df[which(totalsymp$cp_dc == 296.54)] <- 'Bipolar Disorder'
Diagnosis_df[which(totalsymp$cp_dc == 296.89)] <- 'Bipolar Disorder'
Diagnosis_df[which(totalsymp$cp_dc == 298.9)] <- 'Psychosis NOS'
Diagnosis_df[which(totalsymp$cp_dc == 'F20.81')] <- 'Schizophreniform'
Diagnosis_df[which(totalsymp$cp_dc == 'F31.5')] <- 'Bipolar Disorder'
Diagnosis_df[which(totalsymp$cp_dc == 'F31')] <- 'Bipolar Disorder'
Diagnosis_df[which(totalsymp$cp_dc == 'V296.44')] <- 'Bipolar Disorder'
Diagnosis_df[which(totalsymp$cp_dc == 'V71.09')] <- NA

#if(totalsymp$Group == 1) {
#  print(totalsymp$ID)
#}
totalsymp['Diagnosis'] = Diagnosis_df
totalsymp2 <- as.data.frame(cbind(Group, Diagnosis_df, totalsymp$madrs_total, totalsymp$panss_totalscore, totalsymp$marder_neg, totalsymp$marder_pos))

names(totalsymp2) <- c("Group", "Diagnosis", "MADRSS Total", "PANSS Total", "PANSS Negative", "PANSS Positive")

totalsymp$Diagnosis <- factor(totalsymp2$Diagnosis, levels = c("Schizophrenia", "Bipolar Disorder", "Schizoaffective Disorder", "Major Depressive Disorder", "Schizophreniform", "Psychosis NOS", "Delusional Disorder"))


totalsymp2$`PANSS Negative` <- as.numeric(totalsymp2$`PANSS Negative`)
totalsymp2$`PANSS Positive` <- as.numeric(totalsymp2$`PANSS Positive`)
totalsymp2$`PANSS Total` <- as.numeric(totalsymp2$`PANSS Total`)
totalsymp2$`MADRSS Total` <- as.numeric(totalsymp2$`MADRSS Total`)

#full cohort with missing
#totalsymp%>%
#  tbl_summary(
#    by = `Diagnosis`,
#    include = c(`Diagnosis`),
#    missing = "ifany",
#    missing_text = "Missing Data",
#    statistic = list(
#      all_continuous() ~ "{mean} ({sd})",
#      all_categorical() ~ "{n} ({p}%)"
#    ), digits = all_continuous() ~ 2) %>% 
#  bold_labels()

totalsymp2%>%
  tbl_summary(
    by = Group,
    missing = "ifany",
    missing_text = "Missing Data",
    statistic = list(
      all_continuous() ~ "{mean} ({sd})",
      all_categorical() ~ "{n} ({p}%)"
    ), digits = all_continuous() ~ 2) %>% 
  bold_labels()

Group[which(totalsymp$Group == 1)] <- 'Psychosis'
Group[which(totalsymp$Group == 0)] <- 'Control'

wasi2 <- as.data.frame(cbind(Group, totalsymp$wasi_full_iq))
names(wasi2) <- c("Group", "Wasi IQ")
wasi2$`Wasi IQ` <- as.numeric(wasi2$`Wasi IQ`)

wasi2%>%
  tbl_summary(
    by = Group, 
    missing = "no", #only want to see missing values for smoking status, which is expressed as missing category
    statistic = list(
      all_continuous() ~ "{mean} ({sd})", #continuous variables show mean (sd) 
      all_categorical() ~ "{n} ({p}%)" #categorical variables show n (p%)
    ), digits = all_continuous() ~ 2) %>% 
  bold_labels() %>%
  add_p() %>% bold_p() %>%
  modify_spanning_header(c("stat_1", "stat_2") ~ "**Group**") #make spanning header over the group sub-categories

