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
panss <- read_csv("HumanConnectomeProje-PANSSUpdatedNDAJuly2_DATA_2023-08-07_0217.csv")
subs <- read_csv("caselist.csv")

names(ormeans)[1] <- "ID"
names(demog)[1] <- "ID"
names(panss)[1] <- "ID"
names(subs)[1] <- "ID"

panss$ID <- substring(panss$ID, 1, nchar(panss$ID) -2)

# Merge Dataframe ---------------------------------------------------------
means <- merge(demog, ormeans, by = "ID", all.y = TRUE)
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

complete_case <- complete.cases(means$panss_totalscore)
means <- means[complete_case, ]

pp <- lm(formula = FD ~ marder_pos + Sex + Age, data = means)
pp2 <- lm(formula = FDC ~ marder_pos + Sex + Age, data = means)
pp3 <- lm(formula = FC ~ marder_pos + Sex + Age, data = means)
summary(pp)

#FC and marder pos

