rm(list=ls())
# Load Libraries ----------------------------------------------------------
library(tidyverse)
library(readxl)
library(readr)
library(dplyr)
library(ggplot2)
library(patchwork)
library(sjPlot)
library(hrbrthemes)
library(viridisLite)
library(viridis)
library(gtsummary)
# Load Dataframes ---------------------------------------------------------
fbameans <- read_csv("fba_all_means.csv")
tobacco <- read_csv("HumanConnectomeProje-OlfactoryQuestionnai_DATA_2023-06-05_0228.csv")
demog <- read_excel("Matching_samples_image_blood_pbmcs_July_2023.xlsx")
scid <- read_csv("HumanConnectomeProje-SCID5NDAJuly2023_DATA_2023-08-28_0342.csv")
pses <- read_csv("HumanConnectomeProje-ParentalSESNDAJuly20_DATA_2023-08-28_0337.csv")
subs <- read_csv("caselist.csv")

names(tobacco)[1] <- "ID"
names(demog)[1] <- "ID"
names(pses)[1] <- "ID"
names(scid)[1] <- "ID"

tobacco$ID <- substring(tobacco$ID, 1, nchar(tobacco$ID) -2)
pses$ID <- substring(pses$ID, 1, nchar(pses$ID) -2)
scid$ID <- substring(scid$ID, 1, nchar(scid$ID) -2)
fbameans$ID <- substring(fbameans$ID, 5, nchar(fbameans$ID) +4)
fbameans$ID <- as.character(fbameans$ID)
# Merge Dataframe ---------------------------------------------------------
means <- merge(demog, fbameans, by = "ID", all.y = TRUE)
means <- merge(means, tobacco, by = "ID", all.y = TRUE)
means <- merge(means, pses, by = "ID", all.y = TRUE)
means <- merge(means, scid, by = "ID", all.y = TRUE)
means <- means[(means$ID %in% subs$ID),]
#test <- fbameans %>% anti_join(scid, by = "ID")
#initialise group variable
Group = rep(0,length(means$ID))
Group[which(means$Group == 1)] <- 'Psychosis'
Group[which(means$Group == 0)] <- 'Control'
# Age Vs. FA --------------------------------------------------------------

#means2 <- subset(means, Group == 0)
#summary(means2$tbssFA)

agefa <- plot(means$Age, means$tbssFA, pch = 19, col = "lightblue", xlab = "Age", ylab = "FA")
agefa <- abline(lm(means$tbssFA ~ means$Age), col= "purple", lwd = 3)
agefa <- lm(formula = means$tbssFA ~ means$Age, data = means)

summary(agefa)

# Compute Spearman correlation and its p-value
correlation <- cor.test(means$Age, means$tbssFA, method = "spearman")
correlation$estimate
corplot <- means %>%
  ggplot(aes(Age, tbssFA)) +
  geom_boxplot()
plot(corplot)

# Extract the correlation coefficient and p-value
spearman_r <- correlation$estimate
p_value <- correlation$p.value

# Check if p-value is not available due to ties
if (is.na(p_value)) {
  warning("Exact p-value cannot be computed due to ties. Approximation is used.")
}

summary(means$Age)

ggplot(means, aes(x = Age, y = tbssFA)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x, se = TRUE) +
  labs(title = "Correlation between Age and FA",
       x = "Age",
       y = "FA") +
  annotate("text", x = 30, y = 0.44, label = paste("p =", round(p_value, digits = 3)), hjust = 0) +
  annotate("text", x = 30, y = 0.45, label = paste("Spearman r =", round(spearman_r, digits = 3)), hjust = 0)

# Nicotine ------------------------------------------------
means['Smoking Status'] <- ifelse(means$oq_5a == 1, means$oq_5a, means$oq_5g)
complete_case <- complete.cases(means$`Smoking Status`)
means <- means[complete_case, ]

t.test(patient$`Smoking Status`, patient$sigFD, alternative = "two.sided", var.equal = FALSE)

means$`Smoking Status`[which(means$`Smoking Status` == 1)] <- 'Past/Current Smoker'
means$`Smoking Status`[which(means$`Smoking Status` == 0)] <- 'Non-Smoker'
means$`Smoking Status` <- as.factor(means$`Smoking Status`)

nic <- lm(formula = sigFD ~ `Smoking Status` + Sex + Age, data = means)
means$nicfd_fitted_values <- predict(nic)
# Create scatterplot for sigFD vs Smoking Status with regression line
ggplot(means, aes(x = `Smoking Status`, y = sigFD)) +
  geom_point(aes(color = `Smoking Status`), size = 3) +  # Color points by Smoking Status
  geom_line(aes(y = nicfd_fitted_values, group = 1), colour = "red", size = 1) +  # Add regression line for Smoking Status
  labs(title = "Scatterplot of sigFD vs Smoking Status",
       x = "Smoking Status",
       y = "sigFD") +
  scale_color_manual(values = c("Past/Current Smoker" = "orange", "Non-Smoker" = "purple")) +
  theme(legend.position = "none")

means$Sex <- as.factor(means$Sex)

nic <- lm(formula = sigFD ~ `Smoking Status` + Sex + Age, data = means)
means$nicfd_fitted_values <- predict(nic)
nic2 <- lm(formula = sigFDC ~ `Smoking Status` + Sex + Age, data = means)
nic3 <- lm(formula = sigFC ~ `Smoking Status` + Sex + Age, data = means)
means$nicfc_fitted_values <- predict(nic3)
# Create scatterplot for sigFD vs Smoking Status with regression line
ggplot(means, aes(x = `Smoking Status`, y = sigFC)) +
  geom_point(aes(color = `Smoking Status`), size = 3) +  # Color points by Smoking Status
  geom_line(aes(y = nicfc_fitted_values, group = 1), colour = "red", size = 1) +  # Add regression line for Smoking Status
  labs(title = "Scatterplot of sigFC vs Smoking Status",
       x = "Smoking Status",
       y = "sigFC") +
  scale_color_manual(values = c("Past/Current Smoker" = "orange", "Non-Smoker" = "purple")) +
  theme(legend.position = "none")
nic4 <- lm(formula = sigmrFA ~ `Smoking Status` + Sex + Age, data = means)
means$nicfa_fitted_values <- predict(nic4)
# Create scatterplot for sigFD vs Smoking Status with regression line
ggplot(means, aes(x = `Smoking Status`, y = sigmrFA)) +
  geom_point(aes(color = `Smoking Status`), size = 3) +  # Color points by Smoking Status
  geom_line(aes(y = nicfa_fitted_values, group = 1), colour = "red", size = 1) +  # Add regression line for Smoking Status
  labs(title = "Scatterplot of mrFA vs Smoking Status",
       x = "Smoking Status",
       y = "mrFA") +
  scale_color_manual(values = c("Past/Current Smoker" = "orange", "Non-Smoker" = "purple")) +
  theme(legend.position = "none")
summary(nic4)

#with(control, shapiro.test(sigFC[`Smoking Status` == 1]))

means %>%
  ggplot(aes(`Smoking Status`, mrFA),
         group = `Smoking Status`) +
  geom_boxplot()

cst <- plot(means$`Smoking Status`, means$sigmrFA, pch = 19, col = "lightblue", xlab = "Smoking Status", ylab = "FC")
cst <- abline(lm(means$sigmrFA ~ means$`Smoking Status` + means$Sex + means$Age + means$Group), col= "purple", lwd = 3)
cst <- lm(formula = sigmrFA ~ `Smoking Status` + Sex + Age, data = patient)

# Compute Spearman correlation and its p-value
correlation <- cor.test(means$`Smoking Status`, means$sigmrFA, method = "spearman", 
                        alternative = "two.sided",
                        formula = as.formula("y ~ x + Age + Sex + Group"), 
                        data = means)
corplot <- means %>%
  ggplot(aes(`Smoking Status`, sigmrFA)) +
  geom_boxplot()
plot(corplot)

# Extract the correlation coefficient and p-value
spearman_r <- correlation$estimate
p_value <- correlation$p.value

summary(cst)

# Check if p-value is not available due to ties
if (is.na(p_value)) {
  warning("Exact p-value cannot be computed due to ties. Approximation is used.")
}
ggplot(means, aes(x = `Smoking Status`, y = tbssFA)) +
  geom_point(na.rm = TRUE) +
  geom_smooth(method = "lm", formula = y ~ x, se = TRUE, na.rm = TRUE) +
  labs(title = "Smoking Status vs. tbssFA",
       x = "Smoking Status",
       y = "tbssFA") +
  annotate("text", x = 0.4, y = 0.52, label = paste("Spearman r =", round(spearman_r, digits = 3)), hjust = 0) +
  annotate("text", x = 0.4, y = 0.51, label = paste("p =", round(p_value, digits = 3)), hjust = 0) +
  annotate("text", x = 0.4, y = 0.5, label = paste("n = 69"), hjust = 0)

library(pwrss)
pwrss.f.reg(r2 = 0.15, k = 2, power = 0.80, alpha = 0.05)

# Nicotine graph ---------------------------------------------------------
patient <- subset(means, Group == 1)

nic_FD <- ggplot(data = means, aes(x = `Smoking Status`, y = sigFD, color = factor(`Smoking Status`))) +
  geom_violin() +
  geom_boxplot() +
  geom_jitter(size=0.4, alpha=0.9) +
  scale_colour_manual(values = c("Non-Smoker" = "purple", "Past/Current Smoker" = "orange"),
                      name = "Smoking Status",
                      labels = c("Non-Smoker", "Past/Current Smoker")) +  
  theme(legend.position = "none",
        axis.title.y = element_text(size = 14),
        axis.title.x = element_text(size = 16),
        axis.text = element_text(size = 14)) +
  ylab("FD") +
  ylim(c(0.4, 0.8)) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 8))

nic_FC <- ggplot(data = means, aes(x = `Smoking Status`, y = sigFC, color = factor(`Smoking Status`))) +
  geom_violin() +
  geom_boxplot() +
  geom_jitter(size=0.4, alpha=0.9) +
  scale_colour_manual(values = c("Non-Smoker" = "purple", "Past/Current Smoker" = "orange"),
                      name = "Smoking Status",
                      labels = c("Non-Smoker", "Past/Current Smoker")) +  
  theme(legend.position = "none",
        axis.title.y = element_text(size = 14),
        axis.title.x = element_text(size = 16),
        axis.text = element_text(size = 14)) +
  ylab("FC") +
  ylim(c(-0.4, 0.5))

nic_FDC <- ggplot(data = means, aes(x = `Smoking Status`, y = sigFDC, color = factor(`Smoking Status`))) +
  geom_violin() +
  geom_boxplot() +
  geom_jitter(size=0.4, alpha=0.9) +
  scale_colour_manual(values = c("Non-Smoker" = "purple", "Past/Current Smoker" = "orange"),
                      name = "Smoking Status",
                      labels = c("Non-Smoker", "Past/Current Smoker")) +  
  theme(legend.position = "none",
        axis.title.y = element_text(size = 14),
        axis.title.x = element_text(size = 16),
        axis.text = element_text(size = 14)) +
  ylab("FDC") +
  ylim(c(0.2, 0.8))

pnic_mrFA <- ggplot(data = patient, aes(y = `Smoking Status`, x = sigmrFA, color = factor(`Smoking Status`))) +
  geom_violin() +
  geom_boxplot() +
  scale_colour_manual(values = c("Non-Smoker" = "purple", "Past/Current Smoker" = "orange"),
                      name = "Smoking Status",
                      labels = c("Non-Smoker", "Past/Current Smoker")) +  
  theme(legend.position = "none",
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        axis.text = element_text(size = 14)) +
  xlab("FA") +
  scale_y_discrete(labels = function(y) str_wrap(y, width = 6))
plot(nic_tbFA)

smokinggraphics=(nic_FD | nic_FC | nic_FDC) / (pnic_FD | pnic_FC | pnic_FDC)

smokinggraphics=(nic_FD | pnic_FD)/(nic_FC | pnic_FC)/(nic_FDC |pnic_FDC)

fa=((nic_mrFA | nic_tbFA) / (pnic_mrFA | pnic_tbFA))

ggsave(file="bothnicotine2.svg", plot=smokinggraphics, width = 14, height = 8)

# Cannabis ------------------------------------------------
cannabis <- rep(NA,length(means$ID))
cannabis[which(means$scid_23_lp == 1)] <- 0
cannabis[which(means$scid_23_lp == 2)] <- 1
cannabis[which(means$scid_23_lp == 3)] <- 1
means['Cannabis'] = cannabis
complete_case_cb <- complete.cases(means$Cannabis)
means2 <- means[complete_case_cb, ]
means3 <- subset(means2, Group == 1)

t.test(means2$Cannabis, means2$sigFD, alternative = "two.sided", var.equal = FALSE)

#cud <- plot(means2$Cannabis, means2$tbssFA, pch = 19, col = "lightblue", xlab = "CUD", ylab = "tbssFA")
#cud <- abline(lm(means2$tbssFA ~ means2$Cannabis + means2$Sex + means2$Age + means2$Group), col= "purple", lwd = 3)
cud <- lm(formula = sigFD ~ Cannabis + Sex + Age + Group, data = means2)
cud2 <- lm(formula = sigFDC ~ Cannabis + Sex + Age + Group, data = means2)
cud3 <- lm(formula = sigFC ~ Cannabis + Sex + Age + Group, data = means2)
cud4 <- lm(formula = sigmrFA ~ Cannabis+ Sex + Age + Group, data = means2)
summary(cud)

# Compute Spearman correlation and its p-value
correlation <- cor.test(means2$Cannabis, means2$sigFC, method = "spearman", 
                        alternative = "two.sided",
                        formula = as.formula("y ~ x + Age + Sex + Group"), 
                        data = means2)

# Extract the correlation coefficient and p-value
spearman_r <- correlation$estimate
p_value <- correlation$p.value

# Check if p-value is not available due to ties
if (is.na(p_value)) {
  warning("Exact p-value cannot be computed due to ties. Approximation is used.")
}

ggplot(means2, aes(x = Cannabis, y = sigFC)) +
  geom_point(na.rm = TRUE) +
  geom_smooth(method = "lm", formula = y ~ x, se = TRUE, na.rm = TRUE) +
  labs(title = "CUD vs. sigFC",
       x = "CUD",
       y = "sigFDC") +
  annotate("text", x = 0.4, y = 0.525, label = paste("Spearman r =", round(spearman_r, digits = 3)), hjust = 0) +
  annotate("text", x = 0.4, y = 0.515, label = paste("p =", round(p_value, digits = 3)), hjust = 0) +
  annotate("text", x = 0.4, y = 0.505, label = paste("n = 203"), hjust = 0)

means2$Cannabis[which(means2$Cannabis == 1)] <- 'CUD'
means2$Cannabis[which(means2$Cannabis == 0)] <- 'No CUD'
means2$Cannabis <- as.factor(means2$Cannabis)

# cannabis graph ----------------------------------------------------------
can_FD <- ggplot(data = means2, aes(x = Cannabis, y = sigFD, color = factor(Cannabis))) +
  geom_violin() +
  geom_boxplot() +
  geom_jitter(size=0.4, alpha=0.9) +
  scale_colour_manual(values = c("No CUD" = "lightgreen", "CUD" = "darkgreen"),
                      name = "Cannabis",
                      labels = c("No CUD", "CUD")) +  
  theme(legend.position = "none",
        axis.title.y = element_text(size = 14),
        axis.title.x = element_text(size = 16),
        axis.text = element_text(size = 14)) +
  ylab("FD") +
  ylim(c(0.4, 0.8)) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 6))

can_FC <- ggplot(data = means2, aes(x = Cannabis, y = sigFC, color = factor(Cannabis))) +
  geom_violin() +
  geom_boxplot() +
  geom_jitter(size=0.4, alpha=0.9) +
  scale_colour_manual(values = c("No CUD" = "lightgreen", "CUD" = "darkgreen"),
                      name = "Cannabis",
                      labels = c("No CUD", "CUD")) +
  theme(legend.position = "none",
        axis.title.y = element_text(size = 14),
        axis.title.x = element_text(size = 16),
        axis.text = element_text(size = 14)) +
  ylab("FC") +
  ylim(c(-0.4, 0.6)) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 6))

can_FDC <- ggplot(data = means2, aes(x = Cannabis, y = sigFDC, color = factor(Cannabis))) +
  geom_violin() +
  geom_boxplot() +
  geom_jitter(size=0.4, alpha=0.9) +
  scale_colour_manual(values = c("No CUD" = "lightgreen", "CUD" = "darkgreen"),
                      name = "Cannabis",
                      labels = c("No CUD", "CUD"))+
  theme(legend.position = "none",
        axis.title.y = element_text(size = 14),
        axis.title.x = element_text(size = 16),
        axis.text = element_text(size = 14)) +
  ylab("FDC") +
  ylim(c(0.2, 0.8)) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 6))

pcan_FD <- ggplot(data = means3, aes(x = Cannabis, y = sigFD, color = factor(Cannabis))) +
  geom_violin() +
  geom_boxplot() +
  geom_jitter(size=0.4, alpha=0.9) +
  scale_colour_manual(values = c("No CUD" = "lightgreen", "CUD" = "darkgreen"),
                      name = "Cannabis",
                      labels = c("No CUD", "CUD")) + 
  theme(legend.position = "none",
        axis.title.y = element_text(size = 14),
        axis.title.x = element_text(size = 16),
        axis.text = element_text(size = 14)) +
  ylab("FD") +
  ylim(c(0.4, 0.8)) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 6))

pcan_FC <- ggplot(data = means3, aes(x = Cannabis, y = sigFC, color = factor(Cannabis))) +
  geom_violin() +
  geom_boxplot() +
  geom_jitter(size=0.4, alpha=0.9) +
  scale_colour_manual(values = c("No CUD" = "lightgreen", "CUD" = "darkgreen"),
                      name = "Cannabis",
                      labels = c("No CUD", "CUD")) +   
  theme(legend.position = "none",
        axis.title.y = element_text(size = 14),
        axis.title.x = element_text(size = 16),
        axis.text = element_text(size = 14)) +
  ylab("FC") +
  ylim(c(-0.4, 0.6)) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 6))

pcan_FDC <- ggplot(data = means3, aes(x = Cannabis, y = sigFDC, color = factor(Cannabis))) +
  geom_violin() +
  geom_boxplot() +
  geom_jitter(size=0.4, alpha=0.9) +
  scale_colour_manual(values = c("No CUD" = "lightgreen", "CUD" = "darkgreen"),
                      name = "Cannabis",
                      labels = c("No CUD", "CUD"))+   
  theme(legend.position = "none",
        axis.title.y = element_text(size = 14),
        axis.title.x = element_text(size = 16),
        axis.text = element_text(size = 14)) +
  ylab("FDC") +
  ylim(c(0.2, 0.8)) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 6))

plot(can_FD)

cannabisgraphic=(can_FD | can_FC | can_FDC) / (pcan_FD | pcan_FC | pcan_FDC)

ggsave(file="bothcannabis2.svg", plot=cannabisgraphic, width = 14, height = 8)


# Demographics ------------------------------------------------------------
means$Group[which(means$Group == 1)] <- 'Psychosis'
means$Group[which(means$Group == 0)] <- 'Control'
means$Group <- as.factor(means$Group)
means$Cannabis[which(means$Cannabis == 1)] <- 'CUD'
means$Cannabis[which(means$Cannabis == 0)] <- 'No CUD'
means$Cannabis <- as.factor(means$Cannabis)
means$`Smoking Status`[which(means$`Smoking Status` == 1)] <- 'Past/Current Smoker'
means$`Smoking Status`[which(means$`Smoking Status` == 0)] <- 'Non-Smoker'
means$`Smoking Status` <- as.factor(means$`Smoking Status`)
patient$Sex <- as.factor(patient$Sex)
means3$Sex <- as.factor(means3$Sex)
means$Sex <- as.factor(means$Sex)

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

# Create the summary table for means
nic <- means %>%
  tbl_summary(
    by = Group,
    include = c(Age, Sex, `Smoking Status`),
    missing = "ifany",
    missing_text = "Missing Data",
    statistic = list(
      all_continuous() ~ "{mean} ({sd})",
      all_categorical() ~ "{n} ({p}%)"
    ),
    digits = all_continuous() ~ 2
  ) %>%
  bold_labels()

# Prepare a vector for additional statistics for all variables
additional_stats <- sapply(c("Age", "Sex", "Smoking Status"), function(var) {
  calculate_stat_pval(means, var, "Group")
})

# Add additional statistics in a new column for the nic table
nic <- nic %>%
  modify_table_body(
    ~ .x %>%
      mutate(statistics = additional_stats[label])  # Add additional statistics in a new column
  )

# Create the summary table for cannabis data
cud <- means %>%
  tbl_summary(
    by = Group,
    include = c(Age, Sex, `Cannabis`),
    missing = "ifany",
    missing_text = "Missing Data",
    statistic = list(
      all_continuous() ~ "{mean} ({sd})",
      all_categorical() ~ "{n} ({p}%)"
    ),
    digits = all_continuous() ~ 2
  ) %>%
  bold_labels()

# Additional statistics for cud table
additional_stats_cud <- sapply(c("Age", "Sex", "Cannabis"), function(var) {
  calculate_stat_pval(means, var, "Group")
})

cud <- cud %>%
  modify_table_body(
    ~ .x %>%
      mutate(statistics = additional_stats_cud[label])  # Add additional statistics in a new column
  )

# Combine tables
niccud <- tbl_stack(
  tbls = list(nic, cud)
)

# Update header names to reflect changes
niccud <- niccud %>%
  modify_header(
    stat_1 ~ "**Control Group (n, %)**",   # Keeping original n(%)
    stat_2 ~ "**Psychosis Group (n, %)**",  # Keeping original n(%)
    statistics ~ "**Test Statistic (p-value)**"  # New column for test statistics
  ) %>%
  modify_spanning_header(c(stat_1, stat_2) ~ "**Group Statistics**")

# Output the final table
print(niccud)

# Create the summary table for patient data (nicpx) comparing CUD and No CUD groups
nicpx <- patient %>%
  tbl_summary(
    by = `Smoking Status`,  # Use the existing 'Smoking Status' column
    include = c(Age, Sex),
    missing = "ifany",
    missing_text = "Missing Data",
    statistic = list(
      all_continuous() ~ "{mean} ({sd})",
      all_categorical() ~ "{n} ({p}%)"
    ),
    digits = all_continuous() ~ 2
  ) %>%
  bold_labels()

# Additional statistics for nicpx
additional_stats_nicpx <- sapply(c("Age", "Sex"), function(var) {
  calculate_stat_pval(patient, var, "Smoking Status")  # Use 'Smoking Status' for statistics calculation
})

nicpx <- nicpx %>%
  modify_table_body(
    ~ .x %>%
      mutate(statistics = additional_stats_nicpx[label])  # Add additional statistics in a new column
  )

# Update header names for nicpx to reflect CUD and No CUD comparison
nicpx <- nicpx %>%
  modify_header(
    stat_1 ~ "**Non-smoker (n, %)**",  # Update label to reflect CUD group
    stat_2 ~ "**Current/Past Smoker (n, %)**",  # Update label to reflect No CUD group
    statistics ~ "**Test Statistic (p-value)**"   # New column for test statistics
  ) %>%
  modify_spanning_header(c(stat_1, stat_2) ~ "**Smoking Status Statistics**")  # Update the span header

# Create the summary table for means3 dataset (cudpx) comparing CUD and No CUD groups
cudpx <- means3 %>%
  tbl_summary(
    by = `Cannabis`,  # Use the same 'Cannabis' column here
    include = c(Age, Sex),
    missing = "ifany",
    missing_text = "Missing Data",
    statistic = list(
      all_continuous() ~ "{mean} ({sd})",
      all_categorical() ~ "{n} ({p}%)"
    ),
    digits = all_continuous() ~ 2
  ) %>%
  bold_labels()

# Additional statistics for cudpx
additional_stats_cudpx <- sapply(c("Age", "Sex"), function(var) {
  calculate_stat_pval(means3, var, "Cannabis")  # Use 'Cannabis' for statistics calculation
})

cudpx <- cudpx %>%
  modify_table_body(
    ~ .x %>%
      mutate(statistics = additional_stats_cudpx[label])  # Add additional statistics in a new column
  )

# Update header names for cudpx to reflect CUD and No CUD comparison
cudpx <- cudpx %>%
  modify_header(
    stat_1 ~ "**CUD Group (n, %)**",  # Update label to reflect CUD group
    stat_2 ~ "**No CUD Group (n, %)**",  # Update label to reflect No CUD group
    statistics ~ "**Test Statistic (p-value)**"   # New column for test statistics
  ) %>%
  modify_spanning_header(c(stat_1, stat_2) ~ "**CUD vs. No CUD Group Statistics**")  # Update the span header


# Correlation Plots -------------------------------------------------------
ysfdc_base <- plot(means4$`Years Smoked`, means4$FDC, pch = 19, col = "lightblue", xlab = "Years Smoked", ylab = "FDC")
ysfdc_base <- abline(lm(means4$ ~ means4$`Years Smoked`), col= "purple", lwd = 3)
ysfdc_base <- text(paste("Correlation:", round(cor(means4$`Years Smoked`, means4$FDC), 0.5)), x = 15, y = 0.55)

plot(means$Age, means$FDC, pch = 19, col = "lightblue", xlab = "Age", ylab = "FDC")
abline(lm(means$FDC ~ means$Age), col= "purple", lwd = 3)
cbcor_fdc <- text(paste("Correlation:", round(cor(cannabis, means2$FDC), 0.5)), x = 15, y = 0.55)
# Linear Models -----------------------------------------------------------
lmpyfdc <- lm(FDC ~`Packs/Year (current)` + Age + Sex, data = current)
lmcsfdc <- lm(current$FDC ~ current$`Cigarettes Smoked` + current$Age + current$Sex, data = current)

#plot(y=current$`Packs/Year (current)`, x=current$sigFDC, pch = 16, col = "blue")

tab_model(lmpyfdc, collapse.ci = TRUE, 
          pred.labels = c("Intercept", "Pack/Years (current)", "Age", "Sex"))

summary(lmpyfdc)
plot(lmpyfdc$residuals)
abline(a = 0.587848, b = 0.011434)
