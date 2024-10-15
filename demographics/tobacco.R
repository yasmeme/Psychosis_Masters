# This script was used to create a demographics table and analyse nicotine data (particularly
# demographic data)

rm(list=ls())
#load required libraries
library(gtsummary)
library(tidyverse)
library(readxl)
library(readr)
library(dplyr)
library(ggplot2)
library(writexl)
library(Jmisc)

#read in excel file
tobacco <- read_csv("HumanConnectomeProje-OlfactoryQuestionnai_DATA_2023-06-05_0228.csv")
demog <- read_excel("Matching_samples_image_blood_pbmcs_July_2023.xlsx")
scid <- read_csv("HumanConnectomeProje-SCID5NDAJuly2023_DATA_2023-08-28_0342.csv")
subs <- read_csv("caselist.csv")
tiv <- read_csv("asegstats.csv")

#change label of ID column
names(tiv)[1] <- "ID"
names(tobacco)[1] <- "ID"
tobacco$ID <- substring(tobacco$ID, 1, nchar(tobacco$ID) -2)
demog$ID <- as.character(demog$ID)

#merge and exclude na values
total <- merge(demog, tobacco, by = "ID", all = TRUE)
total <- merge(total, tiv, by = "ID", all = TRUE)

#only import subjects with imaging data listed in miss
total <- total[(total$ID %in% subs$ID),]

total <- total[ which(!is.na(total$Age)),  ]
total <- total[ which(!is.na(total$Group)), ]

#initialise group variable
Group = rep(NA,length(total$ID))
Group[which(total$Group == 1)] <- 'Psychosis'
Group[which(total$Group == 0)] <- 'Control'

Sex = rep(NA,length(total$ID))
Sex[which(total$Sex == 1)] <- 'Male'
Sex[which(total$Sex == 0)] <- 'Female'

#combine race variables into a single dataframe
race_df <- as.data.frame(cbind(total$Race_Black_or_African_American, total$Race_Asian, 
                              total$Race_American_Indian_or_Alaskan_Native, total$Race_Native_Hawaiian_or_Other_Pacific_Islander,
                              total$`Race_White/Caucasian/Europea_Heritage`))

#initialise race variable
Race = rep("nan",length(total$ID))

#concatenate race variables into one column
Race[which(total$Race_Unknown == 1)] <- 'Unknown/Other'
Race[which(total$Race_Other == 1)] <- 'Unknown/Other'
Race[which(total$`Race_Multi-Racial` == 1)] <- 'Multi-racial'
Race[which(total$Race_Black_or_African_American == 1)] <- 'African American'
Race[which(total$Race_Asian == 1)] <- 'Asian'
Race[which(total$Race_American_Indian_or_Alaskan_Native == 1)] <- 'American Indian or Alaskan Native'
Race[which(total$Race_Native_Hawaiian_or_Other_Pacific_Islander == 1)] <- 'Native Hawaiian or Other Pacific Islander'
Race[which(total$`Race_White/Caucasian/Europea_Heritage`== 1)] <- 'Caucasian'
Race[rowSums(race_df) > 1] <- 'Multi-racial'

#create new dataframe 
demog2 <- as.data.frame(cbind(Group, as.data.frame(total$Age), Sex, Race))
names(demog2) <- c("Group", "Age", "Sex", "Race")
demog2$Sex <- as.factor(demog2$Sex)
demog2$Group <- as.factor(demog2$Group)

demog2$Race <- factor(demog2$Race, levels = c("Caucasian", "African American", "Multi-racial", "Asian", "Unknown/Other"))

# Custom function to calculate test statistics and p-values
calculate_stat_pval <- function(data, variable, group) {
  if (is.numeric(data[[variable]])) {
    # For continuous variables, use t-test
    test_result <- t.test(data[[variable]] ~ data[[group]])
    test_stat <- round(test_result$statistic, 2)
    p_value <- round(test_result$p.value, 4)
    df <- test_result$parameter  # Degrees of freedom for t-test
    return(paste0("t = ", test_stat, ", df = ", df, ", p = ", p_value))
  } else if (is.factor(data[[variable]])) {
    # For categorical variables, use Pearson's Chi-squared test
    contingency_table <- table(data[[variable]], data[[group]])
    test_result <- chisq.test(contingency_table)
    test_stat <- round(test_result$statistic, 2)
    p_value <- round(test_result$p.value, 4)
    df <- test_result$parameter  # Degrees of freedom for Chi-squared test
    return(paste0("χ² = ", test_stat, ", df = ", df, ", p = ", p_value))
  } else {
    return(NA)
  }
}

# Perform Chi-squared tests for each level of the Race variable
race_levels <- levels(demog2$Race)
race_test_results <- sapply(race_levels, function(race) {
  # Subset data for each race level
  subset_data <- demog2 %>%
    filter(Race == race)
  
  # Create contingency table and perform Chi-squared test
  contingency_table <- table(subset_data$Group)
  
  # Perform the Chi-squared test (note: may not always be valid for small counts)
  if (min(contingency_table) > 0) {
    test_result <- chisq.test(contingency_table)
    test_stat <- round(test_result$statistic, 2)
    p_value <- round(test_result$p.value, 14)
    df <- test_result$parameter  # Degrees of freedom for Chi-squared test
    return(paste0("χ² = ", test_stat, ", df = ", df, ", p = ", p_value))
  } else {
    return("Not enough data")
  }
})

# Create the summary table
demogt <- demog2 %>%
  tbl_summary(
    by = Group,
    missing = "no",
    statistic = list(
      all_continuous() ~ "{mean} ({sd})",
      all_categorical() ~ "{n} ({p}%)"
    ),
    digits = all_continuous() ~ 2
  ) %>%
  bold_labels()

# Prepare a vector for additional statistics for all variables
additional_stats <- sapply(names(demog2), function(var) {
  calculate_stat_pval(demog2, var, "Group")
})

# Add additional statistics in a new column for all variables
demogt <- demogt %>%
  modify_table_body(
    ~ .x %>%
      mutate(statistics = additional_stats[label])  # Add additional statistics in a new column
  )

# Add the race test results to the demogt summary table
demogt <- demogt %>%
  modify_table_body(
    ~ .x %>%
      mutate(statistics = ifelse(label %in% race_levels, race_test_results[label], statistics))
  )

# Update header names to reflect changes
demogt <- demogt %>%
  modify_header(
    stat_1 ~ "**Control Group (n, %)**",   # Keeping original n(%)
    stat_2 ~ "**Psychosis Group (n, %)**",  # Keeping original n(%)
    statistics ~ "**Test Statistic (p-value)**"  # New column for test statistics
  ) %>%
  modify_spanning_header(c(stat_1, stat_2) ~ "**Group Statistics**")

# Output the final table
demogt

max(demog2$Age)

observed <- table(demog2$Sex, demog2$Group)
obsage <- table(demog2$Age, demog2$Group)
obsrace <- table(demog2$Race, demog2$Group)
sexgroup <- chisq.test(observed, y = NULL, correct = TRUE)
agegroup <- wilcox.test(obsage)
chisq.test(obsrace)
fisher.test(obsrace)
wilcox.test(obsage)

associationTest(formula = ~Sex+Group, data = demog2)

#final_table <- tbl_stack(list(summary1, summary2))
#print(final_table)

#initialise smoking status variable
SmokingStatus <- rep("Never Smoked",length(total$ID))

#concatenate smoking status variables into one column
# I want a variable that shows who has ever been a smoker. I need to combine the
#people that are currently smokers and past smokers into one variable
SmokingStatus[which(is.na(total$oq_5a))] <- 'Missing' #find NA values and label as missing data
SmokingStatus[which(is.na(total$oq_5g))] <- 'Missing' #find NA values and label as missing data
SmokingStatus[which(total$oq_5a == 1)] <- 'Current Smoker'
SmokingStatus[which(total$oq_5g == 1)] <- 'Past Smoker' 

#initialise AgeSmoked and CigSmoked variables

AgeSmoked <- rep(NA,length(total$ID))
#combining values from 2 columns where each has values where the other does not
#ifelse tests if total$oq_5f is NOT NA
#if TRUE value = total$oq_5f, if FALSE (total$oq_5f is NA) value = total$oq_5h
AgeSmoked <- ifelse(!is.na(total$oq_5f), total$oq_5f, total$oq_5h)
AgeSmoked[which(total$oq_5f == 0)] <- NA #find 0 values and make them NA to avoid including in calculations
AgeSmoked[which(total$oq_5h == 0)] <- NA #find 0 values and make them NA to avoid including in calculations

CigSmoked <- rep(NA,length(total$ID))
CigSmoked <- ifelse(!is.na(total$oq_5e), total$oq_5e, total$oq_5j)
CigSmoked[which(total$oq_5e == 0)] <- NA #find 0 values and make them NA to avoid including in calculations
CigSmoked[which(total$oq_5j == 0)] <- NA  #find 0 values and make them NA to avoid including in calculations

#initialising variables for YearsSmoked and AgeStopSmoke
YearsSmoked <-rep(NA,length(total$ID))
YearsSmoked <- total$oq_5c
YearsSmoked[which(total$oq_5c == 0)] <- NA #find 0 values and make them NA to avoid including in calculations

#create new combined data frame with above initialised variables so that table can be generated
tobacco2 <- as.data.frame(cbind(Group, SmokingStatus, AgeSmoked, YearsSmoked, CigSmoked))

#reorder smoking status categories in table
tobacco2$SmokingStatus <- factor(tobacco2$SmokingStatus, levels = c("Current Smoker", "Past Smoker", "Never Smoked", "Missing"))

#rename variables for table 
names(tobacco2) <- c("Group", "Smoking Status", "Age Started Smoking, N = 93",
                     "Years Smoking, N = 49", "Cigarettes Smoked per Day, N = 81")

#convert to numeric in order to recognise continuous variables in table instead of listing each unique
#value as a separate category
tobacco2$`Age Started Smoking, N = 93` <- as.numeric(tobacco2$`Age Started Smoking, N = 93`)
tobacco2$`Years Smoking, N = 49` <- as.numeric(tobacco2$`Years Smoking, N = 49`)
tobacco2$`Cigarettes Smoked per Day, N = 81` <- as.numeric(tobacco2$`Cigarettes Smoked per Day, N = 81`)
tobacco2$Group <- as.factor(Group)
# Custom function to calculate test statistics and p-values
calculate_stat_pval_t2 <- function(data, variable, group) {
  if (is.numeric(data[[variable]])) {
    # For continuous variables, use t-test
    test_result <- t.test(data[[variable]] ~ data[[group]])
    test_stat <- round(test_result$statistic, 2)
    p_value <- round(test_result$p.value, 4)
    df <- test_result$parameter  # Degrees of freedom for t-test
    return(paste0("t = ", test_stat, ", df = ", df, ", p = ", p_value))
  } else if (is.factor(data[[variable]]) && variable == "Smoking Status") {
    # For Smoking Status, perform Chi-squared tests for each level
    levels_of_smoking <- levels(data[[variable]])
    results <- sapply(levels_of_smoking, function(level) {
      # Create contingency table for each level
      contingency_table <- table(data[[group]][data[[variable]] == level])
      if (length(contingency_table) == 2 && min(contingency_table) > 0) {
        test_result <- chisq.test(contingency_table)
        test_stat <- round(test_result$statistic, 2)
        p_value <- round(test_result$p.value, 4)
        df <- test_result$parameter  # Degrees of freedom for Chi-squared test
        return(paste0("χ² = ", test_stat, ", df = ", df, ", p = ", p_value))
      } else {
        return("Not enough data")
      }
    })
    return(results)
  } else {
    return(NA)
  }
}

# Create the summary table
t2 <- tobacco2 %>%
  tbl_summary(
    by = Group,  # Separates into control and psychosis group
    missing = "no",  # Only want to see missing values for smoking status
    include = c("Group", "Smoking Status", "Age Started Smoking, N = 93",
                "Years Smoking, N = 49", "Cigarettes Smoked per Day, N = 81"),
    statistic = list(
      all_continuous() ~ "{mean} ({sd})",  # Continuous variables show mean (sd)
      all_categorical() ~ "{n} ({p}%)"  # Categorical variables show n (p%)
    ),
    digits = all_continuous() ~ 2
  ) %>%
  bold_labels() %>%
  add_p() %>% 
  bold_p() %>%
  modify_spanning_header(c("stat_1", "stat_2") ~ "**Group**")  # Make spanning header over the group sub-categories

# Prepare a vector for additional statistics for each level of Smoking Status
smoking_levels <- levels(tobacco2$`Smoking Status`)
smoking_test_results <- sapply(smoking_levels, function(level) {
  # Filter data for the current smoking level
  subset_data <- tobacco2 %>% filter(`Smoking Status` == level)
  return(calculate_stat_pval_t2(subset_data, "Smoking Status", "Group"))
})

# Convert results into a format suitable for adding to the table
smoking_test_results <- as.data.frame(t(smoking_test_results))
colnames(smoking_test_results) <- smoking_levels

# Add the Chi-squared results for each smoking level into the table
t2 <- t2 %>%
  modify_table_body(
    ~ .x %>%
      mutate(statistics = ifelse(label == "Smoking Status", 
                                 paste(smoking_test_results[1, ], collapse = "; "), 
                                 NA))  # Add Chi-squared results in the Smoking Status row
  )

# Update header names to reflect changes
t2 <- t2 %>%
  modify_header(
    stat_1 ~ "**Control Group (n, %)**",   # Keeping original n(%)
    stat_2 ~ "**Psychosis Group (n, %)**",  # Keeping original n(%)
    statistics ~ "**Test Statistic (p-value)**"  # New column for test statistics
  ) %>%
  modify_spanning_header(c(stat_1, stat_2) ~ "**Group Statistics**")

# Output the final table
t2

#check whether 0 values are being included in calculations
#ggplot(tobacco2, aes(x= tobacco2$`Years Smoking`)) +
  #geom_histogram(binwidth = 1, fill = "blue", color = "black") +
  #labs(title = "Histogram of Cigarettes per Day",
   #    x = "Cigarettes per Day",
    #   y= "frequency")

# Assuming 'tobacco2' is your data frame
#non_na_count <- tobacco2 %>%
#  summarize(non_na_count = sum(!is.na(tobacco2$`Age Started Smoking`)))

# Print the result
#print(non_na_count)

smoked <- rep(0,length(total$ID))

smoked[which(is.na(total$oq_5a))] <- NA  #find NA values and label as missing data
smoked[which(is.na(total$oq_5g))] <- NA  #find NA values and label as missing data
smoked[which(total$oq_5a == 1)] <- 1
smoked[which(total$oq_5g == 1)] <- 1

neversmoked <- rep(1,length(total$ID))

neversmoked[which(is.na(total$oq_5a))] <- NA  #find NA values and label as missing data
neversmoked[which(is.na(total$oq_5g))] <- NA  #find NA values and label as missing data
neversmoked[which(total$oq_5a == 1)] <- 0
neversmoked[which(total$oq_5g == 1)] <- 0

#initialise AgeSmoked and CigSmoked variables

AgeSmoked2 <- rep(NA,length(total$ID))
#combining values from 2 columns where each has values where the other does not
#ifelse tests if total$oq_5f is NOT NA
#if TRUE value = total$oq_5f, if FALSE (total$oq_5f is NA) value = total$oq_5h
AgeSmoked2 <- ifelse(!is.na(total$oq_5f), total$oq_5f, total$oq_5h)
AgeSmoked2[which(total$oq_5f == 0)] <- NA #find 0 values and make them NA to avoid including in calculations
AgeSmoked2[which(total$oq_5h == 0)] <- NA #find 0 values and make them NA to avoid including in calculations

CigSmoked2 <- rep(NA,length(total$ID))
CigSmoked2 <- ifelse(!is.na(total$oq_5e), total$oq_5e, total$oq_5j)
CigSmoked2[which(total$oq_5e == 0)] <- NA #find 0 values and make them NA to avoid including in calculations
CigSmoked2[which(total$oq_5j == 0)] <- NA  #find 0 values and make them NA to avoid including in calculations

#initialising variables for YearsSmoked and AgeStopSmoke
YearsSmoked2 <-rep(NA,length(total$ID))
YearsSmoked2 <- total$oq_5c

tivmean <- subset(total$EstimatedTotalIntraCranialVol, !is.na(total$EstimatedTotalIntraCranialVol))

#replace NA values with mean of TIV
total$EstimatedTotalIntraCranialVol <- ifelse(is.na(total$EstimatedTotalIntraCranialVol), 
                                              mean(tivmean, na.rm = TRUE), 
                                              total$EstimatedTotalIntraCranialVol)

px <- as.data.frame(cbind(total$ID, total$Group, smoked, neversmoked, total$Age, total$Sex, total$EstimatedTotalIntraCranialVol))
names(px) <- c("ID", "Group", "Smoker", "Non-Smoker", "Age", "Sex", "TIV")
px <- subset(px, !is.na(px$Smoker))
px <- subset(px, px$Group == 1)
#remove TIV for FD matrices
#px <- subset(px, select = -c(TIV))

px$Age <- as.numeric(px$Age)
px$Sex <- as.numeric(px$Sex)
px$TIV <- as.numeric(px$TIV)

px$Age <- demean(px$Age)
px$Sex <- demean(px$Sex)
px$TIV <- log(pxhc$TIV)
px$TIV <- demean(pxhc$TIV)

write_xlsx(pxhc,"~/px_nicotineFD.xlsx")

#Is there an effect of smoking status on white matter, such that white matter 
#integrity is highest in individuals with schizophrenia who never smoked 
#(status = 0), lowest in individuals who currently smoke (status = 2) and 
#intermediate in individuals that smoked in the past (status = 1)

SmokingStatus3 <- rep(0,length(total$ID))

SmokingStatus3[which(is.na(total$oq_5a))] <- NA  #find NA values and label as missing data
SmokingStatus3[which(is.na(total$oq_5g))] <- NA  #find NA values and label as missing data
SmokingStatus3[which(total$oq_5a == 1)] <- 2
SmokingStatus3[which(total$oq_5g == 1)] <- 1

px <- as.data.frame(cbind(total$ID, total$Group, SmokingStatus3, total$Age, total$Sex, total$EstimatedTotalIntraCranialVol))
names(px) <- c("ID", "Group", "Smoking Status", "Age", "Sex", "TIV")
px <- subset(px, !is.na(px$`Smoking Status`))
px <- subset(px, px$Group == 1)
px$Age <- as.numeric(px$Age)
px$Sex <- as.numeric(px$Sex)
px$TIV <- as.numeric(px$TIV)

px$Age <- demean(px$Age)
px$Sex <- demean(px$Sex)
px$TIV <- log(px$TIV)
px$TIV <- demean(px$TIV)

write_xlsx(px,"~/nicotinelvlFC.xlsx")

#no interaction effect for nicotine, no need
#pxhc2$`Smoking Status` <- as.numeric(pxhc2$`Smoking Status`)
#pxhc2$Group <- as.numeric(pxhc2$Group)
#interaction <- pxhc2$Group*pxhc2$`Smoking Status`
#pxhc3 <- as.data.frame(cbind(pxhc2$ID, pxhc2$Group, pxhc2$`Smoking Status`, pxhc2$Age, pxhc2$Sex, interaction))
#names(pxhc3) <- c("ID", "Group", "Smoking Status", "Age", "Sex", "Status x Dx Interaction")
#write_xlsx(pxhc3,"/home/gucci/Documents/matrices/nicotineintx.xlsx")

                       
