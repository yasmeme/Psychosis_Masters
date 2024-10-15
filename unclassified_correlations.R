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
ucmeans <- read_csv("uc_fba_means.csv")
demog <- read_excel("Matching_samples_image_blood_pbmcs_July_2023.xlsx")
panss <- read_csv("HumanConnectomeProje-PANSSUpdatedNDAJuly2_DATA_2023-08-07_0217.csv")
subs <- read_csv("caselist.csv")

names(ucmeans)[1] <- "ID"
names(demog)[1] <- "ID"
names(panss)[1] <- "ID"
names(subs)[1] <- "ID"

panss$ID <- substring(panss$ID, 1, nchar(panss$ID) -2)
ucmeans$ID <- substring(ucmeans$ID, +5, nchar(ucmeans$ID))

# Merge Dataframe ---------------------------------------------------------
means <- merge(demog, ucmeans, by = "ID", all.y = TRUE)
means <- merge(means, panss, by = "ID", all.y = TRUE)
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

means <- subset(means, means$Group == 1)

# PANSS ----------------------------------------------------------------

complete_case <- complete.cases(means$marder_neg)
means <- means[complete_case, ]

pp <- lm(formula = FD ~ marder_pos + Sex + Age, data = means)
pp2 <- lm(formula = FDC ~ marder_pos + Sex + Age, data = means)
summary(pp2)



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

# Extract the correlation coefficient and p-value
spearman_r <- correlation$estimate
p_value <- correlation$p.value



