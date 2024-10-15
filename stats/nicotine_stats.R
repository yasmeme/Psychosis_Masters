rm(list=ls())
#load required libraries
library(tidyverse)
library(readxl)
library(readr)
library(dplyr)
library(ggplot2)
library(patchwork)

fbameans <- read_csv("nicotine_fba_means.csv")
fbasigmeans <- read_csv("nicotine_fba_sig_means.csv")
tobacco <- read_csv("HumanConnectomeProje-OlfactoryQuestionnai_DATA_2023-06-05_0228.csv")
demog <- read_excel("Matching_samples_image_blood_pbmcs_July_2023.xlsx")
names(tobacco)[1] <- "ID"
names(demog)[1] <- "ID"
tobacco$ID <- substring(tobacco$ID, 1, nchar(tobacco$ID) -2)
fbameans$ID <- substring(fbameans$ID, 5, nchar(fbameans$ID) +4)
fbasigmeans$ID <- as.character(fbasigmeans$ID)

means <- merge(tobacco, fbameans, by = "ID", all.y = TRUE)
means2 <- merge(tobacco, demog, by = "ID", all.y = TRUE)
means2 <- merge(means2, fbasigmeans, by = "ID", all.y = TRUE)

means$ID <- as.character(means$ID)
means2$ID <- as.character(means2$ID)

smoke = rep(0,length(means$ID))
smoke[which(means$oq_5a == 1)] <- 1
smoke[which(means$oq_5g == 1)] <- 1

means['Smoking Status'] = smoke
means2['Smoking Status'] = smoke

nonsmoke = rep(1,length(means$ID))
nonsmoke[which(means$oq_5a == 1)] <- 0
nonsmoke[which(means$oq_5g == 1)] <- 0

smoke_mean_FD <- mean(means$FD[means$`Smoking Status`==1])
nonsmoke_mean_FD <- mean(means$FD[means$`Smoking Status`==0])
smoke_mean_FC <- mean(means$FC[means$`Smoking Status`==1])
nonsmoke_mean_FC <- mean(means$FC[means$`Smoking Status`==0])
smoke_mean_FDC <- mean(means$FDC[means$`Smoking Status`==1])
nonsmoke_mean_FDC <- mean(means$FDC[means$`Smoking Status`==0])

smoke_smean_FD <- mean(means2$FD[means2$`Smoking Status`==1])
nonsmoke_smean_FD <- mean(means2$FD[means2$`Smoking Status`==0])
smoke_smean_FC <- mean(means2$FC[means2$`Smoking Status`==1])
nonsmoke_smean_FC <- mean(means2$FC[means2$`Smoking Status`==0])
smoke_smean_FDC <- mean(means2$FDC[means2$`Smoking Status`==1])
nonsmoke_smean_FDC <- mean(means2$FDC[means2$`Smoking Status`==0])

theme_nogrid <- function (base_size = 12, base_family = "") {
  theme_bw(base_size = base_size, base_family = base_family) %+replace% 
    theme(
      panel.grid = element_blank()
    )   
}

theme_set(theme_nogrid())


#whole brain density plots
  
plot_FDC <- ggplot(data = means, aes(x = FDC, color = factor(smoke))) +
  geom_line(stat = "density") +  # Use geom_line() with stat = "density" for a line plot
  scale_colour_manual(values = c("0" = "purple", "1" = "orange"),
                      name = "Smoking Status",
                      labels = c("Non-Smoker", "Smoker")) +
  geom_vline(xintercept = smoke_mean_FDC, linetype = "dashed", color = "orange", size = 1) +
  geom_vline(xintercept = nonsmoke_mean_FDC, linetype = "dashed", color = "purple", size = 1) +
  theme(legend.position = "top") +
  labs(y = "Density") +
  ylim(c(0,12)) +
  xlim(c(0.1, 0.8)) +
  scale_y_continuous(breaks = seq(0, 12, by = 3), limits = c(0, 12)) +
  ggtitle("Whole Brain")

plot_FD <- ggplot(data = means, aes(x = FD, color = factor(smoke))) +
  geom_line(stat = "density") +  # Use geom_line() with stat = "density" for a line plot
  scale_colour_manual(values = c("0" = "purple", "1" = "orange"),
                      name = "Smoking Status",
                      labels = c("Non-Smoker", "Smoker")) +
  geom_vline(xintercept = smoke_mean_FD, linetype = "dashed", color = "orange", size = 1) +
  geom_vline(xintercept = nonsmoke_mean_FD, linetype = "dashed", color = "purple", size = 1) +
  theme(legend.position = "none") +
  labs(y = NULL) +
  ylim(c(0,12)) +
  xlim(c(0.1, 0.8)) +
  scale_y_continuous(breaks = seq(0, 12, by = 3), limits = c(0, 12)) +
  ggtitle("Whole Brain")

plot_FC <- ggplot(data = means, aes(x = FC, color = factor(smoke))) +
  geom_line(stat = "density") +  # Use geom_line() with stat = "density" for a line plot
  scale_colour_manual(values = c("0" = "purple", "1" = "orange"),
                      name = "Smoking Status",
                      labels = c("Non-Smoker", "Smoker")) +
  geom_vline(xintercept = smoke_mean_FC, linetype = "dashed", color = "orange", size = 1) +
  geom_vline(xintercept = nonsmoke_mean_FC, linetype = "dashed", color = "purple", size = 1) +
  theme(legend.position = "none") +
  labs(y = NULL) +
  ylim(c(0,12)) +
  xlim(c(-0.4, 0.6)) +
  scale_y_continuous(breaks = seq(0, 12, by = 3), limits = c(0, 12)) +
  ggtitle("Whole Brain")

#significant density plots

plot_sigFDC <- ggplot(data = means2, aes(x = FDC, color = factor(smoke))) +
  geom_line(stat = "density") +  # Use geom_line() with stat = "density" for a line plot
  scale_colour_manual(values = c("0" = "purple", "1" = "orange"),
                      name = "Smoking Status",
                      labels = c("Non-Smoker", "Smoker")) +
  geom_vline(xintercept = smoke_smean_FDC, linetype = "dashed", color = "orange", size = 1) +
  geom_vline(xintercept = nonsmoke_smean_FDC, linetype = "dashed", color = "purple", size = 1) +
  theme(legend.position = "none") +
  ylim(c(0,12)) +
  xlim(c(0.1, 0.8)) +
  scale_y_continuous(breaks = seq(0, 12, by = 3), limits = c(0, 12))
  

plot_sigFD <- ggplot(data = means2, aes(x = FD, color = factor(smoke))) +
  geom_line(stat = "density") +  # Use geom_line() with stat = "density" for a line plot
  scale_colour_manual(values = c("0" = "purple", "1" = "orange"),
                      name = "Smoking Status",
                      labels = c("Non-Smoker", "Smoker")) +
  geom_vline(xintercept = smoke_smean_FD, linetype = "dashed", color = "orange", size = 1) +
  geom_vline(xintercept = nonsmoke_smean_FD, linetype = "dashed", color = "purple", size = 1) +
  theme(legend.position = "none") +
  ylim(c(0,12)) +
  xlim(c(0.1, 0.8)) +
  scale_y_continuous(breaks = seq(0, 12, by = 3), limits = c(0, 12))
  

plot_sigFC <- ggplot(data = means2, aes(x = FC, color = factor(smoke))) +
  geom_line(stat = "density") +  # Use geom_line() with stat = "density" for a line plot
  scale_colour_manual(values = c("0" = "purple", "1" = "orange"),
                      name = "Smoking Status",
                      labels = c("Non-Smoker", "Smoker")) +
  geom_vline(xintercept = smoke_smean_FC, linetype = "dashed", color = "orange", size = 1) +
  geom_vline(xintercept = nonsmoke_smean_FC, linetype = "dashed", color = "purple", size = 1) +
  theme(legend.position = "none") +
  ylim(c(0,12)) +
  xlim(c(-0.4, 0.6)) +
  scale_y_continuous(breaks = seq(0, 12, by = 3), limits = c(0, 12))

#means$`Smoking Status`[which(means$`Smoking Status` == 1)] <- 'Smoker'
#means$`Smoking Status`[which(means$`Smoking Status` == 0)] <- 'Non-Smoker'

means$`Smoking Status` <- as.factor(means$`Smoking Status`)
levels(means$`Smoking Status`) <- c("Non-Smoker, N=101","Smoker, N=95")

means2$`Smoking Status` <- as.factor(means2$`Smoking Status`)
levels(means2$`Smoking Status`) <- c("Non-Smoker, N=101","Smoker, N=95")

#whole brain boxplots

box_FDC <- ggplot(data = means, aes(x = FDC, y = `Smoking Status`, color = factor(smoke))) +
  geom_boxplot() +
  scale_colour_manual(values = c("0" = "purple", "1" = "orange"),
                      name = "Smoking Status",
                      labels = c("Non-Smokers", "Smokers")) + 
  theme(legend.position = "none") +
  labs(y = "Smoking Status") +
  scale_y_discrete(labels = function(y) str_wrap(y, width = 10)) +
  xlim(c(0.1, 0.8))

box_FD <- ggplot(data = means, aes(x = FD,  y = `Smoking Status`, color = factor(smoke))) +
  geom_boxplot() +
  scale_colour_manual(values = c("0" = "purple", "1" = "orange"),
                      name = "Smoking Status",
                      labels = c("Non-Smokers", "Smokers")) + 
  theme(legend.position = "none") +
  labs(y = NULL) +
  theme(axis.title.y = element_blank(), axis.text.y = element_blank()) + 
  scale_y_discrete(labels = function(y) str_wrap(y, width = 10)) +
  xlim(c(0.1, 0.8))

box_FC <- ggplot(data = means, aes(x = FC, y = `Smoking Status`, color = factor(smoke))) +
  geom_boxplot() +
  scale_colour_manual(values = c("0" = "purple", "1" = "orange"),
                      name = "Smoking Status",
                      labels = c("Non-Smokers", "Smokers")) + 
  theme(legend.position = "none") +
  labs(y = NULL) +
  theme(axis.title.y = element_blank(), axis.text.y = element_blank()) + 
  scale_y_discrete(labels = function(y) str_wrap(y, width = 10)) +
  xlim(c(-0.4, 0.6))

#significant boxplots

box_FDC_sig <- ggplot(data = means2, aes(x = FDC, y = `Smoking Status`, color = factor(smoke))) +
  geom_boxplot() +
  scale_colour_manual(values = c("0" = "purple", "1" = "orange"),
                      name = "Smoking Status",
                      labels = c("Non-Smokers, N=", "Smokers")) +
  theme(legend.position = "none") +
  scale_y_discrete(labels = function(y) str_wrap(y, width = 10)) +
  xlim(c(0.1, 0.8))

box_FD_sig <- ggplot(data = means2, aes(x = FD, y = `Smoking Status`, color = factor(smoke))) +
  geom_boxplot() +
  scale_colour_manual(values = c("0" = "purple", "1" = "orange"),
                      name = "Smoking Status",
                      labels = c("Non-Smokers", "Smokers")) + 
  theme(legend.position = "none") +
  scale_y_discrete(labels = function(y) str_wrap(y, width = 10)) +
  xlim(c(0.1, 0.8))

box_FC_sig <- ggplot(data = means2, aes(x = FC, y = `Smoking Status`, color = factor(smoke))) +
  geom_boxplot() +
  scale_colour_manual(values = c("0" = "purple", "1" = "orange"),
                      name = "Smoking Status",
                      labels = c("Non-Smokers", "Smokers")) + 
  theme(legend.position = "none") +
  scale_y_discrete(labels = function(y) str_wrap(y, width = 10)) +
  xlim(c(-0.4, 0.6))


((plot_sigFDC| box_FDC_sig)/ (plot_sigFD | box_FD_sig) / (plot_sigFC | box_FC_sig)) +
  plot_layout(guides = 'collect' & theme(legend.position='top')) + plot_layout(axis_titles = "collect")

ggsave("/home/gucci/Documents/nicotine_FBA.tiff", width = 6, height = 8)

#legend <-   theme(legend.position = "top",
#                  legend.key.width = unit(0.5, "lines"),
#                  legend.text = element_text(size = 7), 
#                  legend.title = element_text(size = 8))


#using significant mask

CigSmoked <- rep(NA,length(means$ID))
CigSmoked <- ifelse(!is.na(means$oq_5e), means$oq_5e, means$oq_5j)

YearsSmoked <-rep(NA,length(means$ID))
YearsSmoked <- means$oq_5c
YearsSmoked <- as.numeric(YearsSmoked)

prevsmoke = rep(0,length(means$ID))
prevsmoke[which(means$oq_5a == 1)] <- 2
prevsmoke[which(means$oq_5g == 1)] <- 1


means2['Years Smoked'] = YearsSmoked
means2['Cigarettes Smoked'] = CigSmoked
means2['Smoking Status2'] = prevsmoke

means2$`Cigarettes Smoked`[which(means2$`Cigarettes Smoked` == 0)] <- NA #find 0 values and make them NA to avoid including in calculations
means2$`Cigarettes Smoked`[which(means2$`Cigarettes Smoked` == 0)] <- NA  #find 0 values and make them NA to avoid including in calculations

means2$`Years Smoked`[which(means2$`Years Smoked` == 0)] <- NA #find 0 values and make them NA to avoid including in calculations
means2$`Years Smoked`[which(means2$`Years Smoked` == 08)] <- 8
means2$`Years Smoked`[which(means2$`Years Smoked`  == 04)] <- 4
means2$`Years Smoked`[which(means2$`Years Smoked` == 0.5)] <- NA

means2$oq_5b <- as.numeric(means2$oq_5b)
means2['Packs'] = means2$oq_5b/20
means2$Packs <- as.numeric(means2$Packs)
means2$oq_5c <- as.numeric(means2$oq_5c)
means2['Packs/Year'] = means2$Packs*means2$oq_5c
means2$`Packs/Year`[which(means2$`Packs/Year` == 0)] <- NA

complete_case2 <- complete.cases(means2$`Packs/Year`)
means3 <- as.data.frame(cbind(means2$ID, means2$Age, means2$Sex, means2$`Packs/Year`, means2$FDC, means2$FD, means2$FC))
means3 <- means3[complete_case2, ]
names(means3) <- c("ID", "Age", "Sex", "Packs/Year", "FDC", "FD", "FC")
means3$Age <- as.numeric(means3$Age)
means3$Sex <- as.numeric(means3$Sex)
means3$`Packs/Year` <- as.numeric(means3$`Packs/Year`)
complete_case3 <- complete.cases(means2$`Years Smoked`)
means4 <- means2[complete_case3, ]

pwrss.f.reg(r2 = 0.15, k = 2, power = 0.80, alpha = 0.05)

plot_cig <- ggplot(data = means3, aes(x = `Cigarettes Smoked`, color = factor(means3$`Smoking Status2`))) +
  geom_line(stat = "density") +  # Use geom_line() with stat = "density" for a line plot
  scale_colour_manual(values = c("1" = "pink", "2" = "orange"),
                      name = "Smoking Status",
                      labels = c("Previous Smoker", "Current Smoker"))

# Plotting multiple Regression Lines 
ggplot+geom_smooth(method=lm,se=FALSE,fullrange=TRUE)

lmpyfdc <- lm(means3$FDC ~ means3$`Packs/Year` + means3$Age, data = means3)
lmcsfdc <- lm(means3$FDC ~ means3$`Cigarettes Smoked` + means3$Age + means3$Sex, data = means3)

plot(y=means3$`Packs/Year`, x=means3$FDC, pch = 16, col = "blue")

lmysfd <- lm(means4$FD ~ means4$`Years Smoked`, data = means4)
lmysfc <- lm(means4$FC ~ means4$`Years Smoked`, data = means4)


summary(lmpyfdc)
plot(lmpyfdc$residuals)
abline(a = 0.587848, b = 0.011434)
  
