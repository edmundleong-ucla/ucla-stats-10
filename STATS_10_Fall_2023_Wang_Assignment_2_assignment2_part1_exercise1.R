# STATS 10 Fall 2023 Assignment 2 Part I Exercise 1
# Edmund Leong 206049891
# Give current working directory
getwd()
# Set working directory to flint.csv
setwd("C:/Users/leong/Downloads")
# Read flint.csv in flint
flint <- read.csv("C:/Users/leong/Downloads/flint.csv")
# Calculate proportion of dangerous lead levels
 # flint$Pb >= 15 returns TRUE for each element with a corresponding Pb level of 15 PPB or greater and FALSE otherwise
 # mean(flint$Pb >= 15) calculates the proportion of TRUE values in flint$Pb >= 15
proportion_dangerous_lead_level = mean(flint$Pb >= 15)
# Filter flint for North
north <- flint[flint$Region == "North",]
# Calculate the mean copper level for test sites in the North region
 # na.rm = TRUE ignores NA values while calculating the mean
mean_north_copper_level <- mean(north$Cu, na.rm = TRUE)
# Filter flint for dangerous lead levels
dangerous_lead_levels <- flint[flint$Pb >= 15,]
# Calculate the mean dangerous copper level for test sites with dangerous lead levels
 # na.rm = TRUE ignores NA values while calculating the mean
mean_dangerous_copper_level = mean(dangerous_lead_levels$Cu, na.rm = TRUE)
# Calculate the mean lead level
 # na.rm = TRUE ignores NA values while calculating the mean
mean_lead_level <- mean(flint$Pb, na.rm = TRUE)
# Calculate the mean copper level
 # na.rm = TRUE ignores NA values while calculating the mean
mean_copper_level <- mean(flint$Cu, na.rm = TRUE)
# Create a box plot for the lead levels
 # main = "Lead Levels in Flint, Michigan" sets the title of the box plot
 # ylab = "Lead Levels (PPB)" labels the vertical axis
boxplot(flint$Pb, main = "Lead Levels in Flint, Michigan", ylab = "Lead Levels (PPB)")
# Calculate the median lead level
 # na.rm = TRUE ignores NA values while calculating the mean
median_lead_level <- median(flint$Pb, na.rm = TRUE)