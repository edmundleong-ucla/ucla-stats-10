# STATS 10 Fall 2023 Project
# Edmund Leong 206049891
# Set working directory to Downloads
setwd("C:/Users/leong/Downloads")
# Read "data.csv" into data
data <- read.csv("data.csv", header = TRUE)
## Part 1: Data Handling
# Print a handful of rows of data
head(data)
# Return a matrix of TRUE and FALSE values that determine which values are NA
NA_matrix <- is.na(data)
# Calculate the sum of TRUE values in each row of NA_matrix
TRUE_NA <- rowSums(NA_matrix)
# Return the indices of the rows of TRUE_NA that contain NA values
TRUE_NA_indices <- which(TRUE_NA > 0)
# Return the ID column of data for TRUE_NA_indices
ID_TRUE_NA_indices <- data$ID[TRUE_NA_indices]
# Homes with ID 1, 14, 51, or 80 have at least one NA value
# Remove homes with NA values
data_no_NA <- data[complete.cases(data),]
# Print a handful of rows of data_no_NA
head(data_no_NA)
# Return which rows of data_no_NA have duplicate ID values
row_duplicate_ID <- duplicated(data_no_NA$ID)
# Return all duplicate ID values in data_no_NA
duplicate_ID <- data_no_NA[row_duplicate_ID, "ID"]
# Return data_no_NA without duplicate ID values
data_no_NA_no_duplicate_ID <- data_no_NA[!row_duplicate_ID,]
# Print a handful of rows of data_no_NA_no_duplicate_ID
head(data_no_NA_no_duplicate_ID)
## Part 2: Variable Summarization
# Return a five-number summary of building prices
summary(data_no_NA_no_duplicate_ID$BuildingPrice)
# The mean home price is $133577
# The median home price is $110260
Q1_BuildingPrice <- 84680
Q3_BuildingPrice <- 154377
# Calculate the IQR for BuildingPrice
IQR_BuildingPrice <- Q3_BuildingPrice - Q1_BuildingPrice
# Calculate the lower bound for BuildingPrice outliers
lower_bound_BuildingPrice <- Q1_BuildingPrice - 1.5 * IQR_BuildingPrice
# lower_bound_BuildingPrice < 0, so only an upper bound for BuildingPrice outliers exist
# Calculate the upper bound for BuildingPrice outliers
upper_bound_BuildingPrice <- Q3_BuildingPrice + 1.5 * IQR_BuildingPrice
# The lower bound for BuildingPrice outliers is negative, so only consider the upper bound
outliers_BuildingPrice <- sum(data_no_NA_no_duplicate_ID$BuildingPrice > upper_bound_BuildingPrice)
# 7 homes have a building price that is an outlier compared to the entire dataset
# Create a boxplot of BuildingPrice
boxplot(data_no_NA_no_duplicate_ID$BuildingPrice, main = "Boxplot of Building Prices", ylab = "Building Price ($)", side = 4, las = 1, yaxt = "n")
# Create a sequence of labels for the y-axis of the BuildingPrice boxplot
y_labels_BuildingPrice <- seq(0, 650000, 50000)
# Add the y-axis labels to the BuildingPrice boxplot
axis(2, at = y_labels_BuildingPrice, labels = format(y_labels_BuildingPrice, big.mark = ","), las = 1)
# Create a histogram of BuildingPrice
hist(data_no_NA_no_duplicate_ID$BuildingPrice, main = "Histogram of Building Prices", xlab = "Building Price ($)", ylab = "Frequency")
# The BuildingPrice distribution of building prices is right-tailed to a trivial degree, indicating the median is a better measure of the typical building price than the mean
# It is better to say the typical building price is moreso the median price of $110260 than $133577
# Return the five-number summary of acres
summary(data_no_NA_no_duplicate_ID$Acres)
# The mean house occupies 0.8065 acres
# The median house occupies 0.3000 acres
Q1_Acres <- 0.2000
Q3_Acres <- 0.5025
# Calculate the IQR for Acres
IQR_Acres <- Q3_Acres - Q1_Acres
# Calculate the lower bound for Acres outliers
lower_bound_Acres <- Q1_Acres - 1.5 * IQR_Acres
# The lower bound for Acres outliers is negative, so only consider the upper bound
upper_bound_Acres <- Q3_Acres + 1.5 * IQR_Acres
# Calculate the number of outliers for Acres
outliers_Acres <- sum(data_no_NA_no_duplicate_ID$Acres > upper_bound_Acres)
# 9 homes occupy a quantity of acres that are outliers compared to the entire dataset
# Create a boxplot of Acres
boxplot(data_no_NA_no_duplicate_ID$Acres, main = "Boxplot of Acres Occupied per House", ylab = "Acres Occupied", side = 4, las = 1, yaxt = "n")
# Create a sequence of labels for the y-axis of the Acres boxplot
y_labels_Acres <- seq(0, 40, 0.50)
# Add the y-axis labels to the Acres boxplot
axis(2, at = y_labels_Acres, labels = format(y_labels_Acres, big.mark = ","), las = 1)
# The Acres distribution is right-tailed to a trivial degree, indicating the median is a better measure of the amount of acres occupied by the typical home than the mean
# It is better to say the typical amount of acres occupied by a home is moreso the median coverage of 0.3000 acres than the mean coverage of 0.8065 acres
## Part 3: Price Comparison
# Calculate the correlation between Fireplace and TotalPrice
correlation_Fireplace_TotalPrice <- cor(data_no_NA_no_duplicate_ID$Fireplace, data_no_NA_no_duplicate_ID$TotalPrice)
# The correlation is near-identical to 0, which shows that the presence of a fireplace has little effect on a house's total price
# Create a boxplot of TotalPrice for homes with and without a fireplace
boxplot(data_no_NA_no_duplicate_ID$TotalPrice ~ data_no_NA_no_duplicate_ID$Fireplace, main = "Boxplot of Total Price by Fireplace", xlab = "Does the house have a fireplace?", ylab = "Total Price ($)")
# The boxplot displays miniscule, yet also visible, differences in summary statistics for TotalPrice by Fireplace
# Return all homes without a fireplace
houses_without_fireplace <- data_no_NA_no_duplicate_ID[data_no_NA_no_duplicate_ID$Fireplace == FALSE,]
# Return the five-number summary of TotalPrice for houses_without_fireplace
summary(houses_without_fireplace$TotalPrice)
median_TotalPrice_without_fireplace <- 100207
Q1_TotalPrice_without_fireplace <- 82387
Q3_TotalPrice_without_fireplace <- 145710
# Calculate the IQR of TotalPrice for homes without a fireplace
IQR_TotalPrice_without_fireplace <- Q3_TotalPrice_without_fireplace - Q1_TotalPrice_without_fireplace
# Calculate the lower bound for TotalPrice outliers for homes without a fireplace
lower_bound_TotalPrice_without_fireplace <- Q1_TotalPrice_without_fireplace - 1.5 * IQR_TotalPrice_without_fireplace
# Calculate the upper bound for TotalPrice outliers for homes without a fireplace
upper_bound_TotalPrice_without_fireplace <- Q3_TotalPrice_without_fireplace + 1.5 * IQR_TotalPrice_without_fireplace
# Count the number of TotalPrice outliers for homes without a fireplace
outliers_TotalPrice_without_fireplace <- sum(houses_without_fireplace$TotalPrice < lower_bound_TotalPrice_without_fireplace) + sum(houses_without_fireplace$TotalPrice > upper_bound_TotalPrice_without_fireplace)
# Since the lower bound is negative, consider only the upper bound
# This means the 2 outliers for TotalPrice for homes without a fireplace are above the upper bound
# Return all homes with a fireplace
houses_with_fireplace <- data_no_NA_no_duplicate_ID[data_no_NA_no_duplicate_ID$Fireplace == TRUE,]
# Return the five-number summary of TotalPrice for houses_with_fireplace
summary(houses_with_fireplace$TotalPrice)
median_TotalPrice_with_fireplace <- 201733
# Calculate the change in TotalPrice with the addition of a fireplace
median_TotalPrice_with_fireplace - median_TotalPrice_without_fireplace
Q1_TotalPrice_with_fireplace <- 142576
Q3_TotalPrice_with_fireplace <- 320419
# Calculate the IQR of TotalPrice for homes with a fireplace
IQR_TotalPrice_with_fireplace <- Q3_TotalPrice_with_fireplace - Q1_TotalPrice_with_fireplace
# Calculate the lower bound for TotalPrice outliers for homes with a fireplace
lower_bound_TotalPrice_with_fireplace <- Q1_TotalPrice_with_fireplace - 1.5 * IQR_TotalPrice_with_fireplace
# Calculate the upper bound for TotalPrice outliers for homes with a fireplace
upper_bound_TotalPrice_with_fireplace <- Q3_TotalPrice_with_fireplace + 1.5 * IQR_TotalPrice_with_fireplace
# Count the number of TotalPrice outliers for homes with a fireplace
outliers_TotalPrice_with_fireplace <- sum(houses_with_fireplace$TotalPrice < lower_bound_TotalPrice_with_fireplace) + sum(houses_with_fireplace$TotalPrice > upper_bound_TotalPrice_with_fireplace)
# Because the set of homes without fireplaces has more extreme outliers for TotalPrice than the set of homes with fireplaces, the distribution of TotalPrice for homes without fireplaces will be more right-tailed than for homes with fireplaces
## Part 4: Numerical Relationship Explanation:
# Calculate the correlation between YearBuilt and SqFt
correlation_YearBuilt_SqFt <- cor(data_no_NA_no_duplicate_ID$YearBuilt, data_no_NA_no_duplicate_ID$SqFt)
# Calculate the correlation between YearBuilt and Story
correlation_YearBuilt_Story <- cor(data_no_NA_no_duplicate_ID$YearBuilt, data_no_NA_no_duplicate_ID$Story)
# Calculate the correlation between YearBuilt and Acres
correlation_YearBuilt_Acres <- cor(data_no_NA_no_duplicate_ID$YearBuilt, data_no_NA_no_duplicate_ID$Acres)
# Calculate the correlation between YearBuilt and N_Baths
correlation_YearBuilt_N_Baths <- cor(data_no_NA_no_duplicate_ID$YearBuilt, data_no_NA_no_duplicate_ID$N_Baths)
# Calculate the correlation between YearBuilt and TotalPrice
correlation_YearBuilt_TotalPrice <- cor(data_no_NA_no_duplicate_ID$YearBuilt, data_no_NA_no_duplicate_ID$TotalPrice)
# Calculate the correlation between YearBuilt and LandPrice
correlation_YearBuilt_LandPrice <- cor(data_no_NA_no_duplicate_ID$YearBuilt, data_no_NA_no_duplicate_ID$LandPrice)
# Calculate the correlation between YearBuilt and BuildingPrice
correlation_YearBuilt_BuildingPrice <- cor(data_no_NA_no_duplicate_ID$YearBuilt, data_no_NA_no_duplicate_ID$BuildingPrice)
# Calculate the correlation between SqFt and Story
correlation_SqFt_Story <- cor(data_no_NA_no_duplicate_ID$SqFt, data_no_NA_no_duplicate_ID$Story)
# Calculate the correlation between SqFt and Acres
correlation_SqFt_Acres <- cor(data_no_NA_no_duplicate_ID$SqFt, data_no_NA_no_duplicate_ID$Acres)
# Calculate the correlation between SqFt and N_Baths
correlation_SqFt_N_Baths <- cor(data_no_NA_no_duplicate_ID$SqFt, data_no_NA_no_duplicate_ID$N_Baths)
# Calculate the correlation between SqFt and TotalPrice
correlation_SqFt_TotalPrice <- cor(data_no_NA_no_duplicate_ID$SqFt, data_no_NA_no_duplicate_ID$TotalPrice)
# Calculate the correlation between SqFt and LandPrice
correlation_SqFt_LandPrice <- cor(data_no_NA_no_duplicate_ID$SqFt, data_no_NA_no_duplicate_ID$LandPrice)
# Calculate the correlation between SqFt and BuildingPrice
correlation_SqFt_BuildingPrice <- cor(data_no_NA_no_duplicate_ID$SqFt, data_no_NA_no_duplicate_ID$BuildingPrice)
# There appears to be a strong, positive relationship between SqFt and BuildingPrice
# Return a five-number summary of SqFt
summary(data_no_NA_no_duplicate_ID$SqFt)
Q1_SqFt <- 1193
Q3_SqFt <- 2023
# Calculate the IQR for SqFt
IQR_SqFt <- Q3_SqFt - Q1_SqFt
# Calculate the lower bound for SqFt values
lower_bound_SqFt <- Q1_SqFt - 1.5 * IQR_SqFt
# Calculate the upper bound for SqFt values
upper_bound_SqFt <- Q3_SqFt + 1.5 * IQR_SqFt
# Create a boxplot of SqFt
boxplot(data_no_NA_no_duplicate_ID$SqFt, main = "Boxplot of Square Footage", ylab = "Area (square ft)", side = 4, las = 1, yaxt = "n")
# Remove BuildingPrice outliers from data_no_NA_no_duplicate_ID
data_no_BuildingPrice_outliers <- data_no_NA_no_duplicate_ID[data_no_NA_no_duplicate_ID$BuildingPrice >= lower_bound_BuildingPrice & data_no_NA_no_duplicate_ID$BuildingPrice <= upper_bound_BuildingPrice,]
# Remove SqFt outliers from data_no_NA_no_duplicate_ID
data_no_BuildingPrice_outliers_no_SqFt_outliers <- data_no_BuildingPrice_outliers[data_no_BuildingPrice_outliers$SqFt >= lower_bound_SqFt & data_no_BuildingPrice_outliers$SqFt <= upper_bound_SqFt,]
# Return the five-number summary for SqFt in the new dataset
summary(data_no_BuildingPrice_outliers_no_SqFt_outliers$SqFt)
# Create a boxplot of SqFt from the new dataset
boxplot(data_no_BuildingPrice_outliers_no_SqFt_outliers$SqFt, main = "Boxplot of Square Footage except Outliers", ylab = "Area (square ft", side = 4, las = 1, yaxt = "n")
# Return the five-number summary for BuildingPrice from the new dataset
summary(data_no_BuildingPrice_outliers_no_SqFt_outliers$BuildingPrice)
# Create a boxplot of BuildingPrice from the new dataset
boxplot(data_no_BuildingPrice_outliers_no_SqFt_outliers$BuildingPrice, main = "Boxplot of Building Price except Outliers", ylab = "Building Price ($)", side = 4, las = 1, yaxt = "n")
# Calculate the correlation between SqFt and BuildingPrice from the new dataset
correlation_SqFt_BuildingPrice_no_outliers <- cor(data_no_BuildingPrice_outliers_no_SqFt_outliers$SqFt, data_no_BuildingPrice_outliers_no_SqFt_outliers$BuildingPrice)
# Create a scatterplot of BuildingPrice against SqFt from the new dataset
plot(data_no_BuildingPrice_outliers_no_SqFt_outliers$SqFt, data_no_BuildingPrice_outliers_no_SqFt_outliers$BuildingPrice, main = "Scatterplot of Building Price without Outliers against Square Feet without Outliers", cex.main = 0.8, xlab = "Square feet", ylab = "Building Price")
## Part 5: Linear Regression Analysis
# Create a linear model for SqFt and BuildingPrice
model_SqFt_BuildingPrice <- lm(BuildingPrice ~ SqFt, data = data_no_BuildingPrice_outliers_no_SqFt_outliers)
# Return a summary of the model
summary(model_SqFt_BuildingPrice)
# Return the slope of the model
slope <- round(coef(model_SqFt_BuildingPrice)[2], 2)
# Return the intercept of the model
intercept <- round(coef(model_SqFt_BuildingPrice)[1], 2)
# Plot the regression line on the scatterplot
abline(model_SqFt_BuildingPrice, col = "red")
# Add the equation of the regression line to the scatterplot
equation <- paste0("y = ", slope, "x + ", intercept)
mtext(equation, side = 1, line = 4)