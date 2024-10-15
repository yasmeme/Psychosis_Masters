rm(list=ls())
# Load Libraries ----------------------------------------------------------
library(tidyverse)
library(readxl)
library(readr)
library(dplyr)
library(ggplot2)
library(patchwork)
library(pwr)
library(stringr)
# Dataframes --------------------------------------------------------------
fbameans <- read_csv("/home/gucci/Documents/input/fba_all_means.csv")
demog <- read_excel("/home/gucci/Documents/input/Matching_samples_image_blood_pbmcs_July_2023.xlsx")
fbameans$ID <- substring(fbameans$ID, 5, nchar(fbameans$ID) +4)
means <- merge(demog, fbameans, by = "ID", all = TRUE)
means <- means[(demog$ID %in% fbameans$ID),]
means$ID <- as.character(means$ID)

patient = rep(NA,length(means$ID))
patient[which(means$Group == 1)] <- 1
patient[which(means$Group == 0)] <- 0

control = rep(NA,length(means$ID))
control[which(means$Group == 1)] <- 0
control[which(means$Group == 0)] <- 1
# Mean calculations -------------------------------------------------------
#wholebrain 
PT_mean_FD <- mean(means$FD[means$Group==1])
HC_mean_FD <- mean(means$FD[means$Group==0])
PT_mean_FC <- mean(means$FC[means$Group==1])
HC_mean_FC <- mean(means$FC[means$Group==0])
PT_mean_FDC <- mean(means$FDC[means$Group==1])
HC_mean_FDC <- mean(means$FDC[means$Group==0])
PT_mean_FA <- mean(means$mrFA[means$Group==1])
HC_mean_FA <- mean(means$mrFA[means$Group==0])
PT_mean_tbss_FA <-mean(means$tbssFA[means$Group == 1])
HC_mean_tbss_FA <-mean(means$tbssFA[means$Group == 0])

#significant
PT_mean_FD_sig <- mean(means$sigFD[means$Group==1])
HC_mean_FD_sig <- mean(means$sigFD[means$Group==0])
PT_mean_FC_sig <- mean(means$sigFC[means$Group==1])
HC_mean_FC_sig <- mean(means$sigFC[means$Group==0])
PT_mean_FDC_sig <- mean(means$sigFDC[means$Group==1])
HC_mean_FDC_sig <- mean(means$sigFDC[means$Group==0])
PT_mean_FA_sig <- mean(means$sigmrFA[means$Group==1])
HC_mean_FA_sig <- mean(means$sigmrFA[means$Group==0])
PT_mean_tbss_sig <- mean(means$sigtbssFA[means$Group==1])
HC_mean_tbss_sig <- mean(means$sigtbssFA[means$Group==0])

hcpx <- lm(formula = sigFD ~ Cannabis + Sex + Age, data = means3)
patient <- subset(means, Group == 1)
control <- subset(means, Group == 0)

###### power and effect size #################
library(effsize)
d <- cohen.d(patient$sigFD, control$sigFD) 
print(d)

# Theme -------------------------------------------------------------------
theme_nogrid <- function (base_size = 16, base_family = "") {
  theme_bw(base_size = base_size, base_family = base_family) %+replace% 
    theme(
      panel.grid = element_blank()
    )   
}

theme_set(theme_nogrid())

# Whole Brain Density Plots -----------------------------------------------
plot_FDC <- ggplot(data = means, aes(x = FDC, color = factor(patient))) +
  geom_line(stat = "Density") +  # Use geom_line() with stat = "density" for a line plot
  scale_colour_manual(values = c("0" = "blue", "1" = "red"),
                      name = "Group",
                      labels = c("Control", "Psychosis")) + 
  geom_vline(xintercept = PT_mean_FDC, linetype = "dashed", color = "red", size = 1) +
  geom_vline(xintercept = HC_mean_FDC, linetype = "dashed", color = "blue", size = 1) +
  theme(legend.position = "none") +
  labs(y = "Density") +
  labs(x = NULL) + 
  ylim(c(0,12)) +
  xlim(c(0.2, 0.8)) +
  scale_y_continuous(breaks = seq(0, 12, by = 3), limits = c(0, 12)) +
  ggtitle("Whole Brain")

plot_FD <- ggplot(data = means, aes(x = FD, color = factor(patient))) +
  geom_line(stat = "Density") +
  scale_colour_manual(values = c("0" = "blue", "1" = "red"),
                      name = "Group",
                      labels = c("Control", "Psychosis")) + 
  geom_vline(xintercept = PT_mean_FD, linetype = "dashed", color = "red", size = 1) +
  geom_vline(xintercept = HC_mean_FD, linetype = "dashed", color = "blue", size = 1) +
  theme(legend.position = "none") +
  labs(x = NULL) + 
  labs(y = NULL) + 
  ylim(c(0,12)) +
  xlim(c(0.2, 0.8)) + 
  scale_y_continuous(breaks = seq(0, 12, by = 3), limits = c(0, 12)) +
  ggtitle("Whole Brain")

plot_FC <- ggplot(data = means, aes(x = FC, color = factor(patient))) +
  geom_line(stat = "Density") +  # Use geom_line() with stat = "density" for a line plot
  scale_colour_manual(values = c("0" = "blue", "1" = "red"),
                      name = "Group",
                      labels = c("Control", "Psychosis")) +   
  geom_vline(xintercept = PT_mean_FC, linetype = "dashed", color = "red", size = 1) +
  geom_vline(xintercept = HC_mean_FC, linetype = "dashed", color = "blue", size = 1) +
  theme(legend.position = "none") +
  labs(x = NULL) + 
  labs(y = NULL) + 
  ylim(c(0,12)) +
  xlim(c(-0.4, 0.6)) +
  scale_y_continuous(breaks = seq(0, 12, by = 3), limits = c(0, 12)) +
  ggtitle("Whole Brain")

plot_FA <- ggplot(data = means, aes(x = mrFA, color = factor(patient))) +
  geom_line(stat = "Density") +  # Use geom_line() with stat = "density" for a line plot
  scale_colour_manual(values = c("0" = "blue", "1" = "red"),
                      name = "Group",
                      labels = c("Control", "Psychosis")) +  
  geom_vline(xintercept = PT_mean_FA, linetype = "dashed", color = "red", size = 1) +
  geom_vline(xintercept = HC_mean_FA, linetype = "dashed", color = "blue", size = 1) +
  theme(legend.position = "none") +
  labs(x = NULL) + 
  labs(y = NULL) +
  xlim(c(0.4, 0.6)) +
  ggtitle("Whole Brain")
print(plot_FA)

means$Group[which(means$Group == 1)] <- 'Patient'
means$Group[which(means$Group == 0)] <- 'Control'

# Significant Density Plots -----------------------------------------------
plot_sigFDC <- ggplot(data = means2, aes(x = FDC, color = factor(patient))) +
  geom_line(stat = "Density") +  # Use geom_line() with stat = "density" for a line plot
  scale_colour_manual(values = c("0" = "blue", "1" = "red"),
                      name = "Group",
                      labels = c("Control", "Psychosis")) + 
  geom_vline(xintercept = PT_mean_FDC_sig, linetype = "dashed", color = "red", size = 1) +
  geom_vline(xintercept = HC_mean_FDC_sig, linetype = "dashed", color = "blue", size = 1) +
  theme(legend.position = "none") +
  ylim(c(0,12)) +
  xlim(c(0.2, 0.8)) +
  scale_y_continuous(breaks = seq(0, 12, by = 3), limits = c(0, 12))

plot_sigFD <- ggplot(data = means2, aes(x = FD, color = factor(patient))) +
  geom_line(stat = "Density") +
  scale_colour_manual(values = c("0" = "blue", "1" = "red"),
                      name = "Group",
                      labels = c("Control", "Psychosis")) + 
  geom_vline(xintercept = PT_mean_FD_sig, linetype = "dashed", color = "red", size = 1) +
  geom_vline(xintercept = HC_mean_FD_sig, linetype = "dashed", color = "blue", size = 1) +
  theme(legend.position = "none") +
  ylim(c(0,12)) +
  xlim(c(0.2, 0.8)) + 
  scale_y_continuous(breaks = seq(0, 12, by = 3), limits = c(0, 12))

plot_sigFC <- ggplot(data = means2, aes(x = FC, color = factor(patient))) +
  geom_line(stat = "Density") +  # Use geom_line() with stat = "density" for a line plot
  scale_colour_manual(values = c("0" = "blue", "1" = "red"),
                      name = "Group",
                      labels = c("Control", "Psychosis")) +   
  geom_vline(xintercept = PT_mean_FC_sig, linetype = "dashed", color = "red", size = 1) +
  geom_vline(xintercept = HC_mean_FC_sig, linetype = "dashed", color = "blue", size = 1) +
  theme(legend.position = "none") +
  ylim(c(0,12)) +
  xlim(c(-0.4, 0.6)) +
  scale_y_continuous(breaks = seq(0, 12, by = 3), limits = c(0, 12))

plot_sigFA <- ggplot(data = means, aes(x = sigmrFA, color = factor(patient))) +
  geom_line(stat = "Density") +  # Use geom_line() with stat = "density" for a line plot
  scale_colour_manual(values = c("0" = "blue", "1" = "red"),
                      name = "Group",
                      labels = c("Control", "Psychosis")) +  
  geom_vline(xintercept = PT_mean_FA_sig, linetype = "dashed", color = "red", size = 1) +
  geom_vline(xintercept = HC_mean_FA_sig, linetype = "dashed", color = "blue", size = 1) +
  theme(legend.position = "none")

means$Group[which(means$Group == 1)] <- 'Patient'
means$Group[which(means$Group == 0)] <- 'Control'

# Whole Brain Boxplots ----------------------------------------------------
box_FDC <- ggplot(data = means, aes(y = Group, x = FDC, color = factor(patient))) +
  geom_boxplot() +
  scale_colour_manual(values = c("0" = "blue", "1" = "red"),
                      name = "Group",
                      labels = c("Control", "Psychosis")) + 
  theme(legend.position = "none") +
  labs(y = "Group") +
  xlim(c(0.2, 0.8)) +
  scale_y_discrete(labels = function(y) str_wrap(y, width = 10))

box_FD <- ggplot(data = means, aes(y = Group, x = FD, color = factor(patient))) +
  geom_boxplot() +
  scale_colour_manual(values = c("0" = "blue", "1" = "red"),
                      name = "Group",
                      labels = c("Control", "Psychosis")) +   
  theme(legend.position = "none") +
  labs(y = NULL) + 
  theme(axis.title.y = element_blank(), axis.text.y = element_blank()) + 
  xlim(c(0.2, 0.8))
  

box_FC <- ggplot(data = means, aes(y = Group, x = FC, color = factor(patient))) +
  geom_boxplot() +
  scale_colour_manual(values = c("0" = "blue", "1" = "red"),
                      name = "Group",
                      labels = c("Control", "Psychosis")) + 
  theme(legend.position = "none") +
  labs(y = NULL) + 
  theme(axis.title.y = element_blank(), axis.text.y = element_blank()) + 
  xlim(c(-0.4, 0.6))

box_FA <- ggplot(data = means, aes(y = Group, x = FA, color = factor(patient))) +
  geom_boxplot() +
  scale_colour_manual(values = c("0" = "blue", "1" = "red"),
                      name = "Group",
                      labels = c("Control", "Psychosis")) +
  theme(legend.position = "none") +
  labs(y = NULL) + 
  theme(axis.title.y = element_blank(), axis.text.y = element_blank()) +
  xlim(c(0.035, 0.05))
# Significant Boxplots ----------------------------------------------------
box_FDC_sig <- ggplot(data = means2, aes(y = Group, x = FDC, color = factor(patient))) +
  geom_boxplot() +
  scale_colour_manual(values = c("0" = "blue", "1" = "red"),
                      name = "Group",
                      labels = c("Control", "Psychosis")) + 
  theme(legend.position = "none") +
  xlim(c(0.2, 0.8)) +
  scale_y_discrete(labels = function(y) str_wrap(y, width = 10))


box_FD_sig <- ggplot(data = means2, aes(y = Group, x = FD, color = factor(patient))) +
  geom_boxplot() +
  scale_colour_manual(values = c("0" = "blue", "1" = "red"),
                      name = "Group",
                      labels = c("Control", "Psychosis")) +   
  theme(legend.position = "none") +
  xlim(c(0.2, 0.8)) +
  scale_y_discrete(labels = function(y) str_wrap(y, width = 10))


box_FC_sig <- ggplot(data = means2, aes(y = Group, x = FC, color = factor(patient))) +
  geom_boxplot() +
  scale_colour_manual(values = c("0" = "blue", "1" = "red"),
                      name = "Group",
                      labels = c("Control", "Psychosis")) + 
  theme(legend.position = "none") +
  xlim(c(-0.4, 0.6)) +
  scale_y_discrete(labels = function(y) str_wrap(y, width = 10))

box_FA_sig <- ggplot(data = means2, aes(y = Group, x = FA, color = factor(patient))) +
  geom_boxplot() +
  scale_colour_manual(values = c("0" = "blue", "1" = "red"),
                      name = "Group",
                      labels = c("Control", "Psychosis")) + 
  theme(legend.position = "none") +
  scale_y_discrete(labels = function(y) str_wrap(y, width = 10))

((plot_sigFDC | box_FDC_sig) / (plot_sigFD | box_FD_sig) / (plot_sigFC | box_FC_sig) / (plot_sigFA | box_FA_sig)) +
  plot_layout(guides = 'collect' & theme(legend.position='top')) + plot_layout(axis_titles = "collect")
                    
ggsave("/home/gucci/Documents/wholebrain_FBA.tiff", width = 6, height = 8)


# Significant Violin Plots ------------------------------------------------
means$Group <- as.factor(means$Group)
levels(means$Group) <- c("Control, N=74","Patient, N=205")

violin_FDC_sig <- ggplot(data = means, aes(y = Group, x = FDC, color = factor(patient))) +
  geom_violin() +
  geom_boxplot() +
  scale_colour_manual(values = c("0" = "blue", "1" = "red"),
                      name = "Group",
                      labels = c("Control", "Psychosis")) + 
  theme(legend.position = "none",
        axis.title.x = element_text(size = 18),
        axis.title.y = element_text(size = 18),
        axis.text = element_text(size = 18)) +
  xlim(c(0.2, 0.8)) +
  scale_y_discrete(labels = function(y) str_wrap(y, width = 12))


violin_FD_sig <- ggplot(data = means, aes(y = Group, x = FD, color = factor(patient))) +
  geom_violin() +
  geom_boxplot() +
  scale_colour_manual(values = c("0" = "blue", "1" = "red"),
                      name = "Group",
                      labels = c("Control", "Psychosis")) +   
  theme(legend.position = "none",
        axis.title.x = element_text(size = 18),
        axis.title.y = element_text(size = 18),
        axis.text = element_text(size = 18)) +
  xlim(c(0.2, 0.8)) +
  scale_y_discrete(labels = function(y) str_wrap(y, width = 10))


violin_FC_sig <- ggplot(data = means, aes(y = Group, x = FC, color = factor(patient))) +
  geom_violin() +
  geom_boxplot() +
  scale_colour_manual(values = c("0" = "blue", "1" = "red"),
                      name = "Group",
                      labels = c("Control", "Psychosis")) + 
  theme(legend.position = "none",
        axis.title.x = element_text(size = 18),
        axis.title.y = element_text(size = 18),
        axis.text = element_text(size = 18)) +
  labs(x = "logFC") +
  xlim(c(-0.4, 0.6)) +
  scale_y_discrete(labels = function(y) str_wrap(y, width = 10))

violin_FA_sig <- ggplot(data = means, aes(y = Group, x = mrFA, color = factor(patient))) +
  geom_violin() +
  geom_boxplot() +
  scale_colour_manual(values = c("0" = "blue", "1" = "red"),
                      name = "Group",
                      labels = c("Control", "Psychosis")) + 
  theme(legend.position = "none",
        axis.title.x = element_text(size = 18),
        axis.title.y = element_text(size = 18),
        axis.text = element_text(size = 18)) +
  scale_y_discrete(labels = function(y) str_wrap(y, width = 10))


violin=(violin_FD_sig / violin_FC_sig / violin_FDC_sig)

plot(violin)

ggsave(file="/home/gucci/Documents/violin_FBA.svg", plot=violin, width = 8, height = 15)

violin_tbss_sig <- ggplot(data = means, aes(y = Group, x = sigtbssFA, color = factor(patient))) +
  geom_violin() +
  geom_boxplot() +
  scale_colour_manual(values = c("0" = "blue", "1" = "red"),
                      name = "Group",
                      labels = c("Control", "Psychosis")) + 
  theme(legend.position = "none",
        axis.title.x = element_text(size = 18),
        axis.title.y = element_text(size = 18),
        axis.text = element_text(size = 18)) +
  scale_y_discrete(labels = function(y) str_wrap(y, width = 10))
