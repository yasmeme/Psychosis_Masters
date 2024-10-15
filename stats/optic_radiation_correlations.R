rm(list=ls())
# Load Libraries ----------------------------------------------------------
library(tidyverse)
library(readxl)
library(readr)
library(dplyr)
library(ggplot2)
library(patchwork)
library(stringr)

# Dataframes --------------------------------------------------------------
ormeans <- read_csv("OR_fba_means.csv")
demog <- read_excel("Matching_samples_image_blood_pbmcs_July_2023.xlsx")
tobacco <- read_csv("HumanConnectomeProje-OlfactoryQuestionnai_DATA_2023-06-05_0228.csv")
panss <- read_csv("HumanConnectomeProje-PANSSUpdatedNDAJuly2_DATA_2023-08-07_0217.csv")
scid <- read_csv("HumanConnectomeProje-SCID5NDAJuly2023_DATA_2023-08-28_0342.csv")
pses <- read_csv("HumanConnectomeProje-ParentalSESNDAJuly20_DATA_2023-08-28_0337.csv")
subs <- read_csv("caselist.csv")

names(tobacco)[1] <- "ID"
names(panss)[1] <- "ID"
names(demog)[1] <- "ID"
names(pses)[1] <- "ID"
names(scid)[1] <- "ID"

tobacco$ID <- substring(tobacco$ID, 1, nchar(tobacco$ID) -2)
panss$ID <- substring(panss$ID, 1, nchar(panss$ID) -2)
pses$ID <- substring(pses$ID, 1, nchar(pses$ID) -2)
scid$ID <- substring(scid$ID, 1, nchar(scid$ID) -2)

# Merge Dataframe ---------------------------------------------------------
means <- merge(demog, ormeans, by = "ID", all.y = TRUE)
means <- merge(means, panss, by = "ID", all.y = TRUE)
means <- merge(means, tobacco, by = "ID", all.y = TRUE)
means <- merge(means, pses, by = "ID", all.y = TRUE)
means <- merge(means, scid, by = "ID", all.y = TRUE)
means <- means[(means$ID %in% subs$ID),]
means$ID <- as.character(means$ID)

#test <- fbameans %>% anti_join(scid, by = "ID")
#initialise group variable
Group = rep(0,length(means$ID))
Group[which(means$Group == 1)] <- 'Psychosis'
Group[which(means$Group == 0)] <- 'Control'

# Theme -------------------------------------------------------------------
theme_nogrid <- function (base_size = 16, base_family = "") {
  theme_bw(base_size = base_size, base_family = base_family) %+replace% 
    theme(
      panel.grid = element_blank()
    )   
}

theme_set(theme_nogrid())


# PANSS -------------------------------------------------------------------

complete_case <- complete.cases(means$marder_pos)
means <- means[complete_case, ]

pp <- lm(formula = marder_pos~ FD + Sex + Age, data = means)
pp2 <- lm(formula = marder_pos ~ FDC + Sex + Age, data = means)
pp3 <- lm(formula = marder_pos ~ FC + Sex + Age, data = means)
summary(pp3)



pan <- plot(means$marder_cog_diso, means$FC, pch = 19, col = "lightblue", xlab = "PANSS cognitive", ylab = "FC")
pan <- abline(lm(means$FC ~ means$marder_cog_diso + means$Sex + means$Age), col= "purple", lwd = 3)
pan <- lm(formula = FD ~ marder_cog_diso + Sex + Age, data = means)

correlation <- cor.test(means$marder_cog_diso, means$FD, method = "spearman", 
                        alternative = "two.sided",
                        formula = as.formula("y ~ x + Age + Sex + Group"), 
                        data = means)
corplot <- means %>%
  ggplot(aes(marder_cog_diso, FD)) +
  geom_boxplot()
plot(corplot)

# Nicotine ----------------------------------------------------------------

means['Smoking Status'] <- ifelse(means$oq_5a == 1, means$oq_5a, means$oq_5g)
complete_case <- complete.cases(means$`Smoking Status`)
means <- means[complete_case, ]
patient <- subset(means, Group == 1)

nic <- lm(formula = FD ~ `Smoking Status` + Sex + Age + Group, data = means)
nic2 <- lm(formula = FDC ~ `Smoking Status` + Sex + Age + Group, data = means)
nic3 <- lm(formula = FC ~ `Smoking Status` + Sex + Age + Group, data = means)

nic4 <- lm(formula = FD ~ `Smoking Status` + Sex + Age, data = patient)
nic5 <- lm(formula = FDC ~ `Smoking Status` + Sex + Age, data = patient)
nic6 <- lm(formula = FC ~ `Smoking Status` + Sex + Age, data = patient)

summary(nic3)

# Cannabis ----------------------------------------------------------------
cannabis <- rep(NA,length(means$ID))
cannabis[which(means$scid_23_lp == 1)] <- 0
cannabis[which(means$scid_23_lp == 2)] <- 1
cannabis[which(means$scid_23_lp == 3)] <- 1
means['Cannabis'] = cannabis
complete_case_cb <- complete.cases(means$Cannabis)
patient <- subset(means, Group == 1)

cud <- lm(formula = FD ~ Cannabis + Sex + Age + Group, data = means)
cud2 <- lm(formula = FDC ~ Cannabis + Sex + Age + Group, data = means)
cud3 <- lm(formula = FC ~ Cannabis + Sex + Age + Group, data = means)

cud4 <- lm(formula = FD ~ Cannabis + Sex + Age, data = patient)
cud5 <- lm(formula = FDC ~ Cannabis + Sex + Age, data = patient)
cud6 <- lm(formula = FC ~ Cannabis + Sex + Age, data = patient)

summary(cud6)



