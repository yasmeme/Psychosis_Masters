rm(list = ls())

library(readxl)
library(writexl)
library(MatchIt)
library(gtsummary)
library(tidyr)
library(dplyr)

#read in excel file
demog <- read_excel("Matching_samples_image_blood_pbmcs_July_2023.xlsx")
subs <- read_csv("caselist.csv")

#only import subjects with imaging data listed in miss
demog <- demog[(demog$ID %in% subs$ID),]

demog <- demog[ which(!is.na(demog$Age)),  ]
demog <- demog[ which(!is.na(demog$Group)),  ]

#write_xlsx(demog,"/home/gucci/Documents/demographics_compiled_YG.xlsx")


m.out0 <- matchit(Group ~ Sex + Ethnicity_Hispanic_or_Latino_or_of_Spanish_Origin + Race_Black_or_African_American + Race_Asian + Race_American_Indian_or_Alaskan_Native 
                  + Race_Native_Hawaiian_or_Other_Pacific_Islander + Race_White + Race_Unknown + Race_Other + Race_Multi + Age,
                  data = data, method = NULL, distance = "glm",
                  ratio = 1,
                  replace = FALSE)

summary(m.out0)

m.out1 <- matchit(Group ~ Sex + Ethnicity_Hispanic_or_Latino_or_of_Spanish_Origin + Race_Black_or_African_American + Race_Asian + Race_American_Indian_or_Alaskan_Native 
                  + Race_Native_Hawaiian_or_Other_Pacific_Islander + Race_White + Race_Unknown + Race_Other + Race_Multi + Age,
                  data = data, method = "nearest", distance = "glm",
                  ratio = 1,
                  replace = FALSE)

summary(m.out1)

#create new dataframe with matched data
matched.data <- match.data(m.out1)

#separate data to match group 1 and 0
group_0<- matched.data[matched.data$Group == 0, ]
group_1 <- matched.data[matched.data$Group == 1, ]

#choose top 20 matches from each group
best_matched_group_0 <- group_0[1:40, ]
best_matched_group_1 <- group_1[1:40, ]

#print matched data
write_xlsx(matched.data,"~/matched.data.xlsx")

#print 20 best matches for each group
write_xlsx(best_matched_group_0,"~/best_matched_group_0.xlsx")
write_xlsx(best_matched_group_1,"~/best_matched_group_1.xlsx")

#combine race variables into a single dataframe
race_df = as.data.frame(cbind(demog$Race_Black_or_African_American, demog$Race_Asian, 
                              demog$Race_American_Indian_or_Alaskan_Native, demog$Race_Native_Hawaiian_or_Other_Pacific_Islander,
                              demog$`Race_White/Caucasian/Europea_Heritage`))

#initialise race variable
Race = rep("nan",length(demog$ID))

#concatenate race variables into one column
Race[which(demog$Race_Unknown == 1)] <- 'Unknown/Other'
Race[which(demog$Race_Other == 1)] <- 'Unknown/Other'
Race[which(demog$`Race_Multi-Racial` == 1)] <- 'Multi-racial'
Race[which(demog$Race_Black_or_African_American == 1)] <- 'African American'
Race[which(demog$Race_Asian == 1)] <- 'Asian'
Race[which(demog$Race_American_Indian_or_Alaskan_Native == 1)] <- 'American Indian or Alaskan Native'
Race[which(demog$Race_Native_Hawaiian_or_Other_Pacific_Islander == 1)] <- 'Native Hawaiian or Other Pacific Islander'
Race[which(demog$`Race_White/Caucasian/Europea_Heritage` == 1)] <- 'Caucasian'
Race[rowSums(race_df) > 1] <- 'Multi-racial'

#create new dataframe
demog2 <- as.data.frame(cbind(meansn$Age, meansn$Sex, smoke))
names(demog2) <- c("Age", "Sex (% female)", "Smoking Status")
demog2$`Smoking Status`[which(demog2$`Smoking Status` == 1)] <- 'Smoker'
means$Group[which(means$Group == 0)] <- demog2$`Smoking Status`[which(demog2$`Smoking Status` == 0)] <- 'Non-Smoker'

#create demographics table
demog2%>%
  tbl_summary(
    by = `Smoking Status`,
    missing = "no",
    statistic = list(
      all_continuous() ~ "{mean} ({sd})",
      all_categorical() ~ "{n} ({p}%)"
    ), digits = all_continuous() ~ 2) %>% 
  bold_labels() %>%
  add_p() %>% bold_p()%>%
  modify_spanning_header(c("stat_1", "stat_2") ~ "**Group**")
