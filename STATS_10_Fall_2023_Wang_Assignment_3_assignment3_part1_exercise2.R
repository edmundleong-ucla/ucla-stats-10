# STATS 10 Fall 2023 Assignment 3 Part I Exercise 2
# Edmund Leong 206049891
# Give current working directory
getwd()
# Set working directory to flint.csv
setwd("C:/Users/leong/Downloads")
# Read sea_ice.csv in ice
ice <- read.csv("sea_ice.csv", header = TRUE)
# Convert the Date column to class "date"
ice$Date <- as.Date(ice$Date, "%m/%d/%Y")
# Run a linear regression on sea ice extent against time
extent_time_linear_regression <- lm(Extent ~ Date, data = ice)
# Output summary of extent_time_linear_regression
summary(extent_time_linear_regression)
# Construct a scatterplot of sea ice extent against time
plot(ice$Date, ice$Extent, main = "Sea Ice Extent against Time", xlab = "Date (MM/DD/YY)", ylab = "Extent (millions of square kilometers)")
# Overlay the regression line for the scatterplot
abline(extent_time_linear_regression, col = "steelblue")
# Compute the residuals of extent_time_linear_regression in extent_time_residuals
extent_time_residuals <- residuals(extent_time_linear_regression)
# Construct a residual plot from extent_time_residuals
plot(extent_time_residuals, main = "Residuals of Sea Ice Extent against Time", xlab = "Time (months since 1988)", ylab = "Residual Sea Ice Extent (MM of km^2)")
# Overlay the horizontal line for the residual plot
abline(h = mean(extent_time_residuals), col = "steelblue", lwd = 2)