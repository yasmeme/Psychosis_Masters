# Clear Workspace -----------------------------------------------------------
rm(list = ls())

# Load Required Libraries ---------------------------------------------------
# Essential libraries for data manipulation, visualization, and statistical analysis
library(tidyverse)   # Includes ggplot2, dplyr, readr, etc.
library(readxl)      # For reading Excel files
library(patchwork)   # For combining ggplot2 plots
library(viridis)     # For color scales
library(gtsummary)   # For creating summary tables
library(pwrss)       # For power analysis

# Load Dataframes ------------------------------------------------------------
# Read CSV and Excel files into dataframes
fbameans <- read_csv("fba_all_means.csv")
tobacco  <- read_csv("HumanConnectomeProje-OlfactoryQuestionnai_DATA_2023-06-05_0228.csv")
demog    <- read_excel("Matching_samples_image_blood_pbmcs_July_2023.xlsx")
scid     <- read_csv("HumanConnectomeProje-SCID5NDAJuly2023_DATA_2023-08-28_0342.csv")
pses     <- read_csv("HumanConnectomeProje-ParentalSESNDAJuly20_DATA_2023-08-28_0337.csv")
subs     <- read_csv("caselist.csv")

# Function to Rename First Column to "ID" and Convert to Character ----------------
prepare_id_column <- function(df) {
  names(df)[1] <- "ID"          # Rename first column to "ID"
  df$ID <- as.character(df$ID)   # Convert "ID" column to character
  return(df)
}

# Apply the function to relevant dataframes
tobacco <- prepare_id_column(tobacco)
demog   <- prepare_id_column(demog)
pses    <- prepare_id_column(pses)
scid    <- prepare_id_column(scid)

# Ensure 'ID' in fbameans is character and clean it
fbameans$ID <- as.character(fbameans$ID)
fbameans$ID <- substring(fbameans$ID, 5, nchar(fbameans$ID) + 4)  # Adjust based on specific requirements

# Clean 'ID' columns by trimming unnecessary characters where needed
# Define a trimming function
trim_id <- function(df, start = 1, end = -2) {
  df$ID <- substring(df$ID, start, nchar(df$ID) + end)
  return(df)
}

# Apply trimming to relevant dataframes
tobacco <- trim_id(tobacco, end = -2)
pses    <- trim_id(pses, end = -2)
scid    <- trim_id(scid, end = -2)
# fbameans already trimmed above

# Validate that all 'ID' columns are character
# This step is crucial to prevent join errors
data_frames <- list(demog, fbameans, tobacco, pses, scid, subs)
df_names <- c("demog", "fbameans", "tobacco", "pses", "scid", "subs")

for (i in seq_along(data_frames)) {
  df <- data_frames[[i]]
  if (!is.character(df$ID)) {
    stop(paste("Error: ID column in", df_names[i], "is not character."))
  }
}

# Merge Dataframes -----------------------------------------------------------
# Merge all dataframes by "ID", keeping all entries from fbameans
# Using inner_join to retain only matching IDs across dataframes
means <- demog %>%
  inner_join(fbameans, by = "ID") %>%
  inner_join(tobacco, by = "ID") %>%
  inner_join(pses, by = "ID") %>%
  inner_join(scid, by = "ID") %>%
  filter(ID %in% subs$ID)

# Check for successful merge
cat("Number of observations after merging:", nrow(means), "\n")

# Initialize Group Variable --------------------------------------------------
# Assign descriptive labels to the 'Group' variable
# Assuming 'Group' is originally numeric: 0 = Control, 1 = Psychosis
means <- means %>%
  mutate(Group = factor(Group, levels = c(0, 1), labels = c("Control", "Psychosis")))

# Verify Group Variable
table(means$Group, useNA = "ifany")

# Theme -------------------------------------------------------------------
theme_nogrid <- function (base_size = 16, base_family = "") {
  theme_bw(base_size = base_size, base_family = base_family) %+replace% 
    theme(
      panel.grid = element_blank()
    )   
}

theme_set(theme_nogrid())

# Age vs. FA Analysis --------------------------------------------------------
# Scatter plot of Age vs. FA with linear regression line
age_fa_plot <- ggplot(means, aes(x = Age, y = tbssFA)) +
  geom_point(color = "lightblue", alpha = 0.6) +
  geom_smooth(method = "lm", color = "purple", se = TRUE, size = 1.5) +
  labs(title = "Correlation between Age and FA",
       x = "Age",
       y = "Fractional Anisotropy (FA)") +
  theme_minimal()

# Display the plot
print(age_fa_plot)

# Linear Model for Age vs. FA
age_fa_lm <- lm(tbssFA ~ Age, data = means)
summary(age_fa_lm)

# Spearman Correlation for Age vs. FA
correlation_age_fa <- cor.test(means$Age, means$tbssFA, method = "spearman")
spearman_r_age_fa <- correlation_age_fa$estimate
p_value_age_fa   <- correlation_age_fa$p.value

# Display Correlation Results
cat("Spearman Correlation between Age and FA:\n")
cat("r =", round(spearman_r_age_fa, 3), ", p-value =", round(p_value_age_fa, 3), "\n")

# Smoking Status Analysis ----------------------------------------------------
# Create 'Smoking Status' variable based on 'oq_5a' and 'oq_5g'
means <- means %>%
  mutate(`Smoking Status` = if_else(!is.na(oq_5a), oq_5a, oq_5g)) %>%
  mutate(`Smoking Status` = factor(`Smoking Status`, levels = c(0, 1),
                                   labels = c("Non-Smoker", "Past/Current Smoker"))) %>%
  drop_na(`Smoking Status`)

# Verify Smoking Status
table(means$`Smoking Status`, useNA = "ifany")

# Linear Models for Smoking Status
lm_sigFD <- lm(sigFD ~ `Smoking Status` + Sex + Age, data = means)
lm_sigFDC <- lm(sigFDC ~ `Smoking Status` + Sex + Age, data = means)
lm_sigFC  <- lm(sigFC ~ `Smoking Status` + Sex + Age, data = means)
lm_sigmrFA <- lm(sigmrFA ~ `Smoking Status` + Sex + Age, data = means)

# Add Fitted Values to Dataframe for Plotting
means <- means %>%
  mutate(nicfd_fitted  = predict(lm_sigFD),
         nicfc_fitted  = predict(lm_sigFC),
         nicfa_fitted  = predict(lm_sigmrFA))

# Plot sigFD vs Smoking Status with Regression Line
plot_sigFD <- ggplot(means, aes(x = `Smoking Status`, y = sigFD)) +
  geom_jitter(aes(color = `Smoking Status`), width = 0.2, size = 2, alpha = 0.7) +
  geom_line(aes(y = nicfd_fitted, group = 1), color = "red", size = 1) +
  labs(title = "sigFD vs Smoking Status",
       x = "Smoking Status",
       y = "sigFD") +
  scale_color_manual(values = c("Non-Smoker" = "purple", "Past/Current Smoker" = "orange")) +
  theme_minimal() +
  theme(legend.position = "none")

# Plot sigFC vs Smoking Status with Regression Line
plot_sigFC <- ggplot(means, aes(x = `Smoking Status`, y = sigFC)) +
  geom_jitter(aes(color = `Smoking Status`), width = 0.2, size = 2, alpha = 0.7) +
  geom_line(aes(y = nicfc_fitted, group = 1), color = "red", size = 1) +
  labs(title = "sigFC vs Smoking Status",
       x = "Smoking Status",
       y = "sigFC") +
  scale_color_manual(values = c("Non-Smoker" = "purple", "Past/Current Smoker" = "orange")) +
  theme_minimal() +
  theme(legend.position = "none")

# Plot sigmrFA vs Smoking Status with Regression Line
plot_sigmrFA <- ggplot(means, aes(x = `Smoking Status`, y = sigmrFA)) +
  geom_jitter(aes(color = `Smoking Status`), width = 0.2, size = 2, alpha = 0.7) +
  geom_line(aes(y = nicfa_fitted, group = 1), color = "red", size = 1) +
  labs(title = "mrFA vs Smoking Status",
       x = "Smoking Status",
       y = "mrFA") +
  scale_color_manual(values = c("Non-Smoker" = "purple", "Past/Current Smoker" = "orange")) +
  theme_minimal() +
  theme(legend.position = "none")

# Combine Smoking Status Plots Using Patchwork
smoking_plots <- (plot_sigFD | plot_sigFC | plot_sigmrFA)
print(smoking_plots)

# Save Smoking Status Plots
ggsave(file = "/home/gucci/Documents/bothnicotine2.svg", plot = smoking_plots, width = 14, height = 8)

# Cannabis Use Disorder (CUD) Analysis --------------------------------------
# Create 'Cannabis' variable based on 'scid_23_lp'
means <- means %>%
  mutate(Cannabis = case_when(
    scid_23_lp == 1 ~ "No CUD",
    scid_23_lp %in% c(2, 3) ~ "CUD",
    TRUE ~ NA_character_
  )) %>%
  mutate(Cannabis = factor(Cannabis, levels = c("No CUD", "CUD"))) %>%
  drop_na(Cannabis)

# Verify Cannabis Variable
table(means$Cannabis, useNA = "ifany")

# Subset data for Psychosis Group
patient <- means %>%
  filter(Group == "Psychosis")

# Linear Models for Cannabis
lm_sigFD_cannabis    <- lm(sigFD ~ Cannabis + Sex + Age + Group, data = means)
lm_sigFDC_cannabis   <- lm(sigFDC ~ Cannabis + Sex + Age + Group, data = means)
lm_sigFC_cannabis    <- lm(sigFC ~ Cannabis + Sex + Age + Group, data = means)
lm_sigmrFA_cannabis  <- lm(sigmrFA ~ Cannabis + Sex + Age + Group, data = means)

# Plot sigFC vs Cannabis with Regression Line
correlation_cannabis_fc <- cor.test(as.numeric(means$Cannabis), means$sigFC, method = "spearman")
spearman_r_cannabis_fc <- correlation_cannabis_fc$estimate
p_value_cannabis_fc   <- correlation_cannabis_fc$p.value

plot_sigFC_cannabis <- ggplot(means, aes(x = Cannabis, y = sigFC)) +
  geom_jitter(aes(color = Cannabis), width = 0.2, size = 2, alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  labs(title = "CUD vs. sigFC",
       x = "Cannabis Use Disorder",
       y = "sigFC") +
  annotate("text", x = 1.5, y = max(means$sigFC, na.rm = TRUE),
           label = paste("Spearman r =", round(spearman_r_cannabis_fc, 3),
                         "\np-value =", round(p_value_cannabis_fc, 3),
                         "\nn =", nrow(means)),
           hjust = 0) +
  scale_color_manual(values = c("No CUD" = "lightgreen", "CUD" = "darkgreen")) +
  theme_minimal()

# Create Cannabis Related Plots
plot_can_FD <- ggplot(means, aes(x = Cannabis, y = sigFD, color = Cannabis)) +
  geom_violin(trim = FALSE) +
  geom_boxplot(width = 0.1, outlier.shape = NA) +  # Remove outliers from boxplot
  geom_jitter(width = 0.2, alpha = 0.7, size = 1.5) +
  scale_color_manual(values = c("No CUD" = "lightgreen", "CUD" = "darkgreen")) +
  labs(title = "FD by Cannabis Use",
       x = "Cannabis Use Disorder",
       y = "Fractional Diffusivity (FD)") +
  theme_minimal() +
  theme(legend.position = "none")

plot_can_FC <- ggplot(means, aes(x = Cannabis, y = sigFC, color = Cannabis)) +
  geom_violin(trim = FALSE) +
  geom_boxplot(width = 0.1, outlier.shape = NA) +
  geom_jitter(width = 0.2, alpha = 0.7, size = 1.5) +
  scale_color_manual(values = c("No CUD" = "lightgreen", "CUD" = "darkgreen")) +
  labs(title = "FC by Cannabis Use",
       x = "Cannabis Use Disorder",
       y = "Functional Connectivity (FC)") +
  theme_minimal() +
  theme(legend.position = "none")

plot_can_FDC <- ggplot(means, aes(x = Cannabis, y = sigFDC, color = Cannabis)) +
  geom_violin(trim = FALSE) +
  geom_boxplot(width = 0.1, outlier.shape = NA) +
  geom_jitter(width = 0.2, alpha = 0.7, size = 1.5) +
  scale_color_manual(values = c("No CUD" = "lightgreen", "CUD" = "darkgreen")) +
  labs(title = "FDC by Cannabis Use",
       x = "Cannabis Use Disorder",
       y = "FDC") +
  theme_minimal() +
  theme(legend.position = "none")

# Combine Cannabis Plots Using Patchwork
cannabis_plots <- (plot_can_FD | plot_can_FC | plot_can_FDC)
print(cannabis_plots)

# Save Cannabis Plots
ggsave(file = "/home/gucci/Documents/bothcannabis2.svg", plot = cannabis_plots, width = 14, height = 8)

# Demographics Summary Tables -----------------------------------------------
# Function to calculate test statistics and p-values
calculate_stat_pval <- function(data, variable, group) {
  if (is.numeric(data[[variable]])) {
    # For continuous variables, use t-test
    test_result <- t.test(data[[variable]] ~ data[[group]])
    test_stat <- round(test_result$statistic, 2)
    p_val <- round(test_result$p.value, 4)
    df <- round(test_result$parameter, 2)  # Degrees of freedom
    return(paste0("t = ", test_stat, ", df = ", df, ", p = ", p_val))
  } else if (is.factor(data[[variable]])) {
    # For categorical variables, use Chi-squared test
    contingency_table <- table(data[[variable]], data[[group]])
    test_result <- chisq.test(contingency_table)
    test_stat <- round(test_result$statistic, 2)
    p_val <- round(test_result$p.value, 4)
    df <- round(test_result$parameter, 2)  # Degrees of freedom
    return(paste0("χ² = ", test_stat, ", df = ", df, ", p = ", p_val))
  } else {
    return(NA)
  }
}

# Create Summary Table for Smoking Status by Group
nic_summary <- means %>%
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

# Add Test Statistics to Summary Table
additional_stats_nic <- sapply(c("Age", "Sex", "Smoking Status"), function(var) {
  calculate_stat_pval(means, var, "Group")
})

nic_summary <- nic_summary %>%
  modify_table_body(
    ~ .x %>%
      mutate(`Test Statistic (p-value)` = additional_stats_nic[label])
  )

# Create Summary Table for Cannabis by Group
cud_summary <- means %>%
  tbl_summary(
    by = Group,
    include = c(Age, Sex, Cannabis),
    missing = "ifany",
    missing_text = "Missing Data",
    statistic = list(
      all_continuous() ~ "{mean} ({sd})",
      all_categorical() ~ "{n} ({p}%)"
    ),
    digits = all_continuous() ~ 2
  ) %>%
  bold_labels()

# Add Test Statistics to Cannabis Summary Table
additional_stats_cud <- sapply(c("Age", "Sex", "Cannabis"), function(var) {
  calculate_stat_pval(means, var, "Group")
})

cud_summary <- cud_summary %>%
  modify_table_body(
    ~ .x %>%
      mutate(`Test Statistic (p-value)` = additional_stats_cud[label])
  )

# Combine Summary Tables for Nicotine and Cannabis
combined_summary <- tbl_stack(
  tbls = list(nic_summary, cud_summary)
) %>%
  modify_header(
    stat_1 ~ "**Control Group (n, %)**",
    stat_2 ~ "**Psychosis Group (n, %)**",
    `Test Statistic (p-value)` ~ "**Test Statistics**"
  ) %>%
  modify_spanning_header(c(stat_1, stat_2) ~ "**Group Statistics**")

# Display Combined Summary Table
print(combined_summary)

# Summary Table for Smoking Status within Psychosis Group
nicpx_summary <- patient %>%
  tbl_summary(
    by = `Smoking Status`,
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

# Add Test Statistics to nicpx Summary Table
additional_stats_nicpx <- sapply(c("Age", "Sex"), function(var) {
  calculate_stat_pval(patient, var, "Smoking Status")
})

nicpx_summary <- nicpx_summary %>%
  modify_table_body(
    ~ .x %>%
      mutate(`Test Statistic (p-value)` = additional_stats_nicpx[label])
  )

# Update Headers for nicpx Summary Table
nicpx_summary <- nicpx_summary %>%
  modify_header(
    stat_1 ~ "**Non-Smoker (n, %)**",
    stat_2 ~ "**Past/Current Smoker (n, %)**",
    `Test Statistic (p-value)` ~ "**Test Statistics**"
  ) %>%
  modify_spanning_header(c(stat_1, stat_2) ~ "**Smoking Status Statistics**")

# Display nicpx Summary Table
print(nicpx_summary)

# Summary Table for Cannabis within Psychosis Group
cudpx_summary <- patient %>%
  tbl_summary(
    by = Cannabis,
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

# Add Test Statistics to cudpx Summary Table
additional_stats_cudpx <- sapply(c("Age", "Sex"), function(var) {
  calculate_stat_pval(patient, var, "Cannabis")
})

cudpx_summary <- cudpx_summary %>%
  modify_table_body(
    ~ .x %>%
      mutate(`Test Statistic (p-value)` = additional_stats_cudpx[label])
  )

# Update Headers for cudpx Summary Table
cudpx_summary <- cudpx_summary %>%
  modify_header(
    stat_1 ~ "**No CUD Group (n, %)**",
    stat_2 ~ "**CUD Group (n, %)**",
    `Test Statistic (p-value)` ~ "**Test Statistics**"
  ) %>%
  modify_spanning_header(c(stat_1, stat_2) ~ "**CUD vs. No CUD Group Statistics**")

# Display cudpx Summary Table
print(cudpx_summary)

# Correlation Plots (Optional) -----------------------------------------------
# If you have additional data frames like 'means4', ensure they are properly loaded and processed
# Uncomment and adjust the following code as needed

# ysfdc_base <- ggplot(means4, aes(x = `Years Smoked`, y = FDC)) +
#   geom_point(color = "lightblue") +
#   geom_smooth(method = "lm", color = "purple", se = TRUE) +
#   labs(title = "Correlation between Years Smoked and FDC",
#        x = "Years Smoked",
#        y = "FDC") +
#   theme_minimal()
# print(ysfdc_base)

# Linear Models and Diagnostic Plots ----------------------------------------
# Example Linear Model for current dataset
# Ensure 'current' data frame exists and is properly processed
# Uncomment and adjust the following code as needed

# lmpyfdc <- lm(FDC ~ `Packs/Year (current)` + Age + Sex, data = current)
# tab_model(lmpyfdc, collapse.ci = TRUE, 
#           pred.labels = c("Intercept", "Pack/Years (current)", "Age", "Sex"))
# summary(lmpyfdc)
# plot(lmpyfdc$residuals)
# abline(h = 0, col = "red", lwd = 2)

# Power Analysis (Example)
# Adjust parameters as needed
# Uncomment and adjust the following code as needed

# pwrss.f.reg(r2 = 0.15, k = 2, power = 0.80, alpha = 0.05)
