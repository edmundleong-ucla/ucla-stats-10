# STATS 10 Fall 2023 Assignment 3 Part I Exercise 1
# Edmund Leong 206049891
# Give current working directory
getwd()
# Set working directory to Downloads
setwd("C:/Users/leong/Downloads")
# Read soil_complete.txt in soil
soil <- read.table("C:/Users/leong/Downloads/soil_complete.txt", header = T)
# Run a linear regression on lead against zinc in lead_zinc_linear_regression
lead_zinc_linear_regression <- lm(lead ~ zinc, data = soil)
# Output summary of lead_zinc_linear_regression
summary(lead_zinc_linear_regression)
# Construct a scatterplot of lead against zinc
plot(soil$zinc, soil$lead, main = "Lead against Zinc Concentrations", xlab = "Zinc Concentration (PPM)", ylab = "Lead Concentration (PPM)")
# Overlay the regression line for the scatterplot of lead against zinc
 # Setting col = "steelblue" distinguishes regression line from all other parts of the scatterplot
abline(lead_zinc_linear_regression, col = "steelblue")
# Read the residuals of the linear regression of lead against zinc in lead_zinc_residuals
lead_zinc_residuals <- residuals(lead_zinc_linear_regression)
# Construct a residual plot from lead_zinc_residuals
plot(lead_zinc_residuals, main = "Residuals for the Linear Regression of Lead against Zinc Concentrations", cex.main = 0.9, xlab = "Zinc Concentration (PPM)", ylab = "Residuals (PPM)")
# Overlay the horizontal line for the residual plot
 # lwd = 2 increases the thickness of the horizontal line
abline(h = mean(lead_zinc_residuals), lwd = 2, col = "steelblue")
# Compute the correlation coefficient between zinc and lead
correlation_coefficient <- cor(soil$zinc, soil$lead)
# Square correlation_coefficient to compute determination_coefficient
determination_coefficient <- correlation_coefficient ^ 2
# Compute mean residual
mean_residual <- mean(lead_zinc_residuals)
