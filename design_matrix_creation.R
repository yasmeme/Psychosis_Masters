# Clear Workspace -----------------------------------------------------------
rm(list = ls())

# Load Required Libraries ---------------------------------------------------
# Essential libraries for data manipulation, reading, and writing
library(tidyverse)   # Includes dplyr, readr, ggplot2, etc.
library(readxl)      # For reading Excel files
library(writexl)     # For writing Excel files
library(Jmisc)       # For data normalization functions like 'demean'

# Import Demographics and Subjects with Imaging Data ------------------------
# Read demographic data from Excel
demog <- read_excel("Matching_samples_image_blood_pbmcs_July_2023.xlsx")

# Read subjects list and TIV (Total Intracranial Volume) data from CSV
subs  <- read_csv("caselist.csv")
tiv   <- read_csv("asegstats.csv")

# Rename the first column to "ID" in 'tiv' and 'subs' dataframes for consistency
names(tiv)[1] <- "ID"
names(subs)[1] <- "ID"

# Merge 'demog' and 'tiv' dataframes by "ID", keeping all entries from 'demog'
demog <- merge(demog, tiv, by = "ID", all = TRUE)

# Filter 'demog' to include only subjects listed in 'subs'
demog <- demog %>%
  filter(ID %in% subs$ID)

# Remove subjects without Age, Group, or Sex data
demog <- demog %>%
  filter(!is.na(Age), !is.na(Group), !is.na(Sex))

# Handle Missing EstimatedTotalIntraCranialVol (TIV) Values ---------------
# Calculate mean TIV excluding NA
tiv_mean <- mean(demog$EstimatedTotalIntraCranialVol, na.rm = TRUE)

# Replace NA TIV values with the mean TIV
demog$EstimatedTotalIntraCranialVol <- ifelse(
  is.na(demog$EstimatedTotalIntraCranialVol),
  tiv_mean,
  demog$EstimatedTotalIntraCranialVol
)

# Display the mean of 'EstimatedTotalIntraCranialVol' after replacement
mean_tiv <- mean(demog$EstimatedTotalIntraCranialVol)
print(paste("Mean TIV after replacement:", round(mean_tiv, 2)))

# Create 'patient' and 'control' Variables Based on 'Group' -------------------
# Assuming 'Group' is 1 = Patient, 0 = Control
demog <- demog %>%
  mutate(
    patient = if_else(Group == 1, 1, 0),
    control = if_else(Group == 0, 1, 0)
  )

# Convert 'Group', 'Age', and 'Sex' to numeric for analyses
demog <- demog %>%
  mutate(
    Group = as.numeric(Group),
    Age = as.numeric(Age),
    Sex = as.numeric(Sex)
  )

# Create a Dataframe of Subjects with Imaging and Demographic Data ----------
subjects <- demog %>%
  select(ID) %>%
  rename(subjects = ID)

# Save the list of subjects for the 'fixelcfestats' command
write_xlsx(subjects, "~/matrices/subjects.xlsx")

# Create Design Matrix for fixelcfestats Command ---------------------------
# Combine relevant variables into a design matrix
design_matrix_fd <- demog %>%
  select(ID, patient, control, Age, Sex) %>%
  rename(
    Subjects = ID
  ) %>%
  mutate(
    Age = demean(Age),
    Sex = demean(Sex)
  )

# Save the design matrix as an Excel file
write_xlsx(design_matrix_fd, "~/matrices/design_matrixFD_LABELLED.xlsx")


# Create Design Matrix for fixelcfestats Command ---------------------------
# Combine relevant variables into a design matrix
design_matrix_fc <- demog %>%
  select(ID, patient, control, Age, Sex, EstimatedTotalIntraCranialVol) %>%
  rename(
    Subjects = ID,
    TIV = EstimatedTotalIntraCranialVol
  ) %>%
  mutate(
    Age = demean(Age),
    Sex = demean(Sex),
    TIV = log(TIV),
    TIV = demean(TIV)
  )

# Save the design matrix as an Excel file
write_xlsx(design_matrix_fc, "~/matrices/design_matrixFC_LABELLED.xlsx")
