# script used to find and tabulate missing data

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
demog <- read_excel("/home/gucci/Documents/input/Matching_samples_image_blood_pbmcs_July_2023.xlsx")
subs <- read_csv("/home/gucci/Documents/input/caselist.csv")

#list of subjects with imaging data from spartan (excluding subjects with poor image quality)
#made using ls > subjects.txt
names(subs)[1] <- "ID"

#only import subjects with imaging data listed in miss
demog_subset <- demog[demog$image_data_available == 1, ]
demog <-demog_subset

demog <- subset(demog, !is.na(demog$Age))
demog <- subset(demog, !is.na(demog$Group))
demog <- subset(demog, !is.na(demog$Sex))

demog <- demog[(demog$ID %in% subs$ID),]

# Use anti_join to get values from subs that are missing in demog
missing <- anti_join(demog, subs, by = "ID")

# Print the result
print(missing)

