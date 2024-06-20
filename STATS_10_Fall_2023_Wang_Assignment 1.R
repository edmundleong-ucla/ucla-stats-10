# Author: Edmund Leong
# ID: 206049891
heights <- c(71, 74, 66)
# Prints 71 74 66
print(heights)
names <- c("Edmund", "Neel", "Rahul")
# Prints "Edmund" "Neel" "Rahul"
print(names)
# Creates a matrix of columns formed by vectors heights and names
# Performing class(cbind(heights, names)) returns "matrix"
cbind(heights, names)
# Sets working directory to Downloads
setwd("C:/Users/leong/Downloads")
NCBirths <- read.csv("births.csv")
head(NCBirths)
# Installs "maps" package
install.packages("maps")
# Verifies installation of "maps" package
find.package("maps")
# Loads "maps" package
library(maps)
# Outputs all continental states of the U.S.
map("state")
# Extracts weight column of NCbirths into vector weight 
weights <- NCBirths$weight # The values in weights suggest their unit is ounces
# Dividies weights by 16 to extract weight column of NCbirths in terms of pounds into vector weights_in_pounds
weights_in_pounds <- weights / 16
# Outputs elements 1 thru 20 of weights_in_pounds
weights_in_pounds[1:20]
# Outputs the mean weight of weights_in_pounds
mean(weights_in_pounds) # Returns 7.2352 is the mean weight of babies in pounds
# Installs "mosaic" package
install.packages("mosaic")
# Loads "mosaic" package
library(mosaic)
# Returns percentage of females who smoke
tally(~ Gender + Habit, data = NCBirths, format = "percent") # Outputs 4.769076% of females smoke
# Returns percentage of adults who smoke
tally(~ Habit, data = NCBirths, format = "percent") # Outputs 9.38755% of adults smoke which is about 11.61245% less than the CDC average
# Plots a histogram of weight_in_pounds with 3 bins == 2 breaks
hist(weights_in_pounds, breaks = 2) # Underfits data
# Plots a histogram of weight_in_pounds with 20 bins == 19 breaks
hist(weights_in_pounds, breaks = 19) # Fits data well
# Plots a histogram of weight_in_pounds with 100 bins == 99 breaks
hist(weights_in_pounds, breaks = 99) # Overfits data
# Plots a side-by-side boxplot of Fage and Mage
boxplot(NCBirths$Fage, NCBirths$Mage)
# Plots two side-by-side histograms comparing baby weights between smoking moms and non-smoking moms
histogram(~ weights | Habit, data = NCBirths, layout = c(1, 2))
# Plots a dotplot of weights_in_pounds
dotPlot(weights_in_pounds)
# Outputs a two-way summary table to check for any relationship between Habit and MomPriorCond
table(NCBirths$Habit, NCBirths$MomPriorCond)
# Outputs a two-way summary table to check for any relationship between Habit and BirthDef
table(NCBirths$Habit, NCBirths$BirthDef)
# Outputs a two-way summary table to check for any relationship between Habit and DelivComp
table(NCBirths$Habit, NCBirths$DelivComp)
# Outputs a two-way summary table to check for any relationship between Habit and BirthComp
table(NCBirths$Habit, NCBirths$BirthComp)
# Outputs a scatterplot with weights_in_pounds on the vertical axis and Mage on the horizontal axis
plot(weights_in_pounds ~ Mage, data = NCBirths, main = "Mother's Ages Plotted Against Baby Weights", xlab = "Mother's Age", ylab = "Baby Weight")