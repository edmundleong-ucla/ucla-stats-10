# STATS 10 Fall 2023 Assignment 2 Part I Exercise 2
# Edmund Leong 206049891
# Give current working directory
getwd()
# Set working directory to flint.csv
setwd("C:/Users/leong/Downloads")
# Read countries_life.txt in life
 # header = T specifies the first line is a header
life <- read.table("C:/Users/leong/Downloads/countries_life.txt", header = T)
# Construct a scatterplot of Life against Income
plot(life$Income, life$Life, main = "Life Expectancy against Per Capita Income", xlab = "Per Capita Income (1974 dollars)", ylab = "Life Expectancy (years)")
# Construct a boxplot of Income
boxplot(life$Income, main = "Per Capita Income (1974 dollars)")
# Construct a histogram of Income
hist(life$Income, main = "Per Capita Income (1974 dollars)")
# Filter life for Income below $1000
life_below_1000 <- life[life$Income < 1000,]
# Filter life for Income above $1000
life_above_1000 <- life[life$Income > 1000,]
# Construct a scatterplot of Life against Income for life_below_1000
plot(life_below_1000$Income, life_below_1000$Life, main = "Life Expectancy against Per Capita Income below $1000", xlab = "Per Capita Income (1974 dollars)", ylab = "Life Expectancy (years)")
# Compute the correlation coefficient between Income and Life for life_below_1000
correlation_coefficient_below_1000 <- cor(life_below_1000$Income, life_below_1000$Life)