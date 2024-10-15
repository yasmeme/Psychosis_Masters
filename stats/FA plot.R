rm(list=ls())
#load required libraries
library(tidyverse)
library(readxl)
library(readr)
library(dplyr)
library(ggplot2)

FAmean <- read_csv("fa_means.csv")
RDmean <- read_csv("RD_means.csv")
ADmean <- read_csv("AD_means.csv")
ADCmean <- read_csv("ADC_means.csv")
demog <- read_excel("Matching_samples_image_blood_pbmcs_July_2023.xlsx")


FAmean$ID <- substring(FAmean$ID, 5, nchar(FAmean$ID) +4)
RDmean$ID <- substring(RDmean$ID, 5, nchar(RDmean$ID) +4)
ADmean$ID <- substring(ADmean$ID, 5, nchar(ADmean$ID) +4)
ADCmean$ID <- substring(ADCmean$ID, 5, nchar(ADCmean$ID) +4)

means2 <- merge(demog, FAmean, by = "ID", all = TRUE)
means2 <- means2[(demog$ID %in% FAmean$ID),]
means2 <- merge(means2, RDmean, by = "ID", all = TRUE)
means2 <- merge(means2, ADmean, by = "ID", all = TRUE)
means2 <- merge(means2, ADCmean, by = "ID", all = TRUE)
means2$ID <- as.character(means2$ID)

patient = rep(NA,length(means2$ID))
patient[which(means2$Group == 1)] <- 1
patient[which(means2$Group == 0)] <- 0

control = rep(NA,length(means2$ID))
control[which(means2$Group == 1)] <- 0
control[which(means2$Group == 0)] <- 1

PT_mean_FA <- mean(means2$FA[means2$Group==1])
HC_mean_FA <- mean(means2$FA[means2$Group==0])
PT_mean_RD <- mean(means2$RD[means2$Group==1])
HC_mean_RD <- mean(means2$RD[means2$Group==0])
PT_mean_AD <- mean(means2$AD[means2$Group==1])
HC_mean_AD <- mean(means2$AD[means2$Group==0])
PT_mean_ADC <- mean(means2$ADC[means2$Group==1])
HC_mean_ADC <- mean(means2$ADC[means2$Group==0])


plot_FA <- ggplot(data = means2, aes(x = FA, color = factor(patient))) +
  geom_line(stat = "density") +  # Use geom_line() with stat = "density" for a line plot
  scale_colour_manual(values = c("0" = "blue", "1" = "red")) +
  geom_vline(xintercept = PT_mean_FA, linetype = "dashed", color = "red", size = 1) +
  geom_vline(xintercept = HC_mean_FA, linetype = "dashed", color = "blue", size = 1) +
  ggtitle("Density Plot of FA with Mean Lines")

print(plot_FA)



means2$Group[which(means2$Group == 1)] <- 'Patient'
means2$Group[which(means2$Group == 0)] <- 'Control'

means2$Group <- as.factor(means2$Group)
levels(means2$Group) <- c("Control, N=74","Patient, N=205")

boxplot(means2$FA ~ means2$Group,
        xlab = "Group",
        ylab = "FA")
