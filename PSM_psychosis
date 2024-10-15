library(readxl)
library(writexl)
library(MatchIt)
library(gtsummary)
library(tidyr)
library(dplyr)

#read in excel file
demog <- read_excel("/home/gucci/Documents/Imaging/Matching_samples_image_blood_pbmcs_July_2023.xlsx")

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
write_xlsx(matched.data,"/home/gucci/Documents/Imaging/matched.data.xlsx")

#print 20 best matches for each group
write_xlsx(best_matched_group_0,"/home/gucci/Documents/Imaging/best_matched_group_0.xlsx")
write_xlsx(best_matched_group_1,"/home/gucci/Documents/Imaging/best_matched_group_1.xlsx")
