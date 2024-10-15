rm(list=ls())
# Load required libraries
library(gtsummary)
library(tidyverse)
library(readxl)
library(readr)
library(dplyr)
library(ggplot2)
library(patchwork)

# Read data
tract_count <- read_csv("tract_fixels.csv")

# Calculate percentage significant fixels
tract_count['Percentage FD'] = (tract_count$FD / 7443) * 100
tract_count['Percentage FDC'] = (tract_count$FDC / 2067) * 100
tract_count['Percentage FC'] = (tract_count$FC / 16) * 100

# Subset data
fd <- subset(tract_count, `Percentage FD` > 0)
fdc <- subset(tract_count, `Percentage FDC` > 0)
fc <- subset(tract_count, `Percentage FC` > 0)
fd$Group <- as.factor(fd$Group)
fdc$Group <- as.factor(fdc$Group)
fc$Group <- as.factor(fc$Group)

# Define theme
theme_nogrid <- function (base_size = 16, base_family = "") {
  theme_bw(base_size = base_size, base_family = base_family) %+replace%
    theme(
      panel.grid = element_blank()
    )
}

theme_set(theme_nogrid())



# Ensure Tracts are sorted within each Group
fd <- fd %>% arrange(Group, Tract)
fdc <- fdc %>% arrange(Group, Tract)
fc <- fc %>% arrange(Group, Tract)

# Create combined Tract and Group for proper sorting
fd$TractGroup <- factor(paste(fd$Group, fd$Tract, sep = "_"))
fd <- fd %>%
  group_by(Group) %>%
  arrange(`Percentage FD`) %>%
  mutate(TractGroup = factor(TractGroup, levels = unique(TractGroup)))

####          FD2        #####
fd2 <- subset(fd, fd$Tract !='UC')
fd2 <- fd2 %>%
  group_by(Group) %>%
  arrange(`Percentage FD`) %>%
  mutate(TractGroup = factor(TractGroup, levels = unique(TractGroup)))
fd2 <- fd2 %>% arrange(Tract, `Percentage FD`)


####          FDC2        #####
fdc2 <- subset(fdc, fdc$Tract !='UC')
fdc2 <- fdc2 %>%
  group_by(Group) %>%
  arrange(`Percentage FDC`) %>%
  mutate(TractGroup = factor(TractGroup, levels = unique(TractGroup)))
fdc2 <- fdc2 %>% arrange(Tract, `Percentage FDC`)

####          FC2        #####
fc2 <- subset(fc, fc$Tract !='UC')
fc2 <- fc2 %>%
  group_by(Group) %>%
  arrange(`Percentage FC`) %>%
  mutate(TractGroup = factor(TractGroup, levels = unique(TractGroup)))
fc2 <- fc2 %>% arrange(Tract, `Percentage FC`)




fdc$TractGroup <- factor(paste(fdc$Group, fdc$Tract, sep = "_"))
fdc <- fdc %>%
  group_by(Group) %>%
  arrange(`Percentage FDC`) %>%
  mutate(TractGroup = factor(TractGroup, levels = unique(TractGroup)))

fc$TractGroup <- factor(paste(fc$Group, fc$Tract, sep = "_"))
fc <- fc %>%
  group_by(Group) %>%
  arrange(`Percentage FC`) %>%
  mutate(TractGroup = factor(TractGroup, levels = unique(TractGroup)))


# Plot FD
plot_fd <- ggplot(fd, aes(x = TractGroup, y = `Percentage FD`, fill = Group)) +
  geom_bar(stat = "identity") +
  ggtitle("FD") +
  xlab("Tracts") +
  ylab("Percentage of Significant Fixels (%)") +
  scale_x_discrete(labels = fd$Tract[order(fd$TractGroup)]) +  # Show only the Tract name on the x-axis in the new order
  theme(legend.position = "none") +
  scale_fill_manual(values = c("Thalamic" = "orchid", "Striatum" = "mediumpurple", "Projection" = "deepskyblue", "Commissural" = "orange2", "Peduncle" = "lightseagreen", "Limbic" = "blue3", "Unclassified" = "hotpink", "Association" = "salmon")) + 
  theme(legend.position = "none") +
  coord_flip()  # This line flips the x and y axes


# Plot FDC
plot_fdc <- ggplot(fdc, aes(x = TractGroup, y = `Percentage FDC`, fill = Group)) +
  geom_bar(stat = "identity") +
  ggtitle("FDC") +
  xlab("Tracts") +
  ylab("Percentage of Significant Fixels (%)") +
  scale_x_discrete(labels = fdc$Tract[order(fdc$TractGroup)]) +
  scale_fill_manual(values = c("Thalamic" = "orchid", "Striatum" = "mediumpurple", "Projection" = "deepskyblue", "Commissural" = "orange2", "Peduncle" = "lightseagreen", "Limbic" = "blue3", "Unclassified" = "hotpink", "Association" = "salmon")) + 
  # Show only the Tract name on the x-axis in the new order
  theme(legend.position = "none") +
  coord_flip()  # This line flips the x and y axes

# Plot FC
plot_fc <- ggplot(fc, aes(x = TractGroup, y = `Percentage FC`, fill = Group)) +
  geom_bar(stat = "identity") +
  ggtitle("log(FC)") +
  xlab("Tracts") +
  ylab("Percentage of Significant Fixels (%)") +
  scale_x_discrete(labels = fc$Tract[order(fc$TractGroup)]) +  # Show only the Tract name on the x-axis in the new order
  scale_fill_manual(values = c("Thalamic" = "orchid", "Striatum" = "mediumpurple", "Projection" = "deepskyblue", "Commissural" = "orange2", "Peduncle" = "lightseagreen", "Limbic" = "blue3", "Unclassified" = "hotpink", "Association" = "salmon")) + 
  theme(legend.position = "none") +
  coord_flip()  # This line flips the x and y axes


# Subset fd to include only rows correlating to certain TractGroup
fd_subset <- fd %>% filter(grepl("Commissural", TractGroup))

# Extract the `Percentage FD` column from the subset
percentage_fd_subset <- fd_subset$`Percentage FD`
percentage_fd_subset <- fd %>% filter(grepl("Commissural", TractGroup)) %>% pull(`Percentage FD`)
sum(percentage_fd_subset)

# Subset fd to include only rows correlating to certain TractGroup
fdc_subset <- fdc %>% filter(grepl("Association", TractGroup))

# Extract the `Percentage FD` column from the subset
percentage_fdc_subset <- fdc_subset$`Percentage FDC`
percentage_fdc_subset <- fdc %>% filter(grepl("Association", TractGroup)) %>% pull(`Percentage FDC`)
sum(percentage_fdc_subset)

# Subset fd to include only rows correlating to certain TractGroup
fc_subset <- fc %>% filter(grepl("Unclassified", TractGroup))

# Extract the `Percentage FD` column from the subset
percentage_fc_subset <- fc_subset$`Percentage FC`
percentage_fc_subset <- fc %>% filter(grepl("Unclassified", TractGroup)) %>% pull(`Percentage FC`)
sum(percentage_fc_subset)

# Save the plot
ggsave("/home/gucci/Documents/fd_tracts.tiff", width = 8, height = 20)

