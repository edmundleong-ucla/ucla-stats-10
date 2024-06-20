# STATS 10 Fall 2023 Assignment 2 Part I Exercise 4
# Edmund Leong 206049891
# Give current working directory
getwd()
# Set working directory to flint.csv
setwd("C:/Users/leong/Downloads")
# Read la_data.txt in LA
LA <- read.table("C:/Users/leong/Downloads/la_data.txt", header = T)
# Construct a scatterplot of Latitude against Longitude
plot(LA$Longitude, LA$Latitude, main = "Scatterplot of Latitude against Longitude of Neighborhoods in the City of Los Angeles", cex.main = 0.75, xlab = "Longitude", ylab = "Latitude")
# Install the maps package
if (!require(maps)) install.packages('maps')
# Add the outline of LA County
map("county", "california", add = TRUE)
# Construct a scatterplot of Schools against Income
plot(LA$Income, LA$Schools, main = "Scatterplot of Schools against Income in the City of Los Angeles", cex.main = 0.75, xlab = "Income ($)", ylab = "Number of Schools")
# Subset LA to ignore Schools = 0 in LA_subset
LA_subset <- subset(LA, Schools != 0)
# Construct a scatterplot of Schools against Income for LA_subset
plot(LA_subset$Income, LA_subset$Schools, main = "Scatterplot of Schools against Income in the City of Los Angeles (excluding 0 Schools)", cex.main =  0.75, xlab = "Income ($)", ylab = "Number of Schools")