# STATS 10 Fall 2023 Assignment 2 Part I Exercise 3
# Edmund Leong 206049891
# Give current working directory
getwd()
# Set working directory to flint.csv
setwd("C:/Users/leong/Downloads")
# Read soil.txt in maas
maas <- read.table("C:/Users/leong/Downloads/soil.txt", header = T)
# Compute summary statistics for lead
summary(maas$lead)
# Compute summary statistics for zinc
summary(maas$zinc)
# Plot a histogram of lead
hist(maas$lead, main = "Histogram of Lead Concentrations in Mass River", xlab = "Lead Concentration (PPM)")
# Plot a histogram of log(lead)
hist(log(maas$lead), main = "Histogram of the Natural Logarithm of Lead Concentrations in Mass River", xlab = "Natural Logarithm of Lead Concentration (PPM)")
# Construct a scatterplot of log(lead) against log(zinc)
plot(log(maas$zinc), log(maas$lead), main = "Natural Logarithm of Lead Concentrations against Natural Logarithm of Zinc Concentrations in Mass River", xlab = "Natural Logarithm of Zinc Concentration (PPM)", ylab = "Natural Logarithm of Lead Concentration (PPM)")
# Define risk levels based on lead concentration
risk_levels <- cut(maas$lead, breaks = c(0, 150, 400, Inf), labels = c("Lead-free", "Lead-safe", "Significant environmental lead hazard"))
# Define colors with aquamarine, blue, and violet
colors <- c("aquamarine", "blue", "violet")
# Define point size as 15
point_size <- 15
# Construct a scatterplot of log(lead) against log(zinc) with colors based on risk level
plot(log(maas$zinc), log(maas$lead), col = colors[as.numeric(risk_levels)], pch = point_size, main = "Natural Logarithm of Lead Concentrations against Natural Logarithm of Zinc Concentrations in Maas River", xlab = "Natural Logarithm of Zinc Concentration (PPM)", ylab = "Natural Logarithm of Lead Concentration (PPM)")