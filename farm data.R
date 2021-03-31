# Script for analyzing data from HBS. Quant Exam.
# Created by Daniel Asay on March 3rd, 2021 at 7:00pm PT


# Read in the data and take a look at it
library(readxl)
farm_data <- read.csv("~/Downloads/nhgis0025_ds211_1925_county.csv")
View(farm_data)

# Rename and Label the AB84002 and AB84006 columns. Cut out unncessary columns

colnames(farm_data)[9] <- "acres_corn"
colnames(farm_data)[13] <- "acres_wheat"

library(expss)
farm_data = apply_labels(farm_data,
                         AB84002 = "Acres of Harvested Corn",
                         AB84006 = "Acres of Wheat")

farm_subset = farm_data[,c(1,2,3,5,9,13)]

View(farm_subset)

# Read in the data from the .dta file 

library(tidyverse)
library(haven)
data_to_merge <- read_dta(file = "~/Downloads/test_mergefile.dta")
View(data_to_merge)

# Merge the two dataframes into 1 based on GISJOIN.

merged_data <- merge(farm_subset, data_to_merge,by=c("GISJOIN", "YEAR", "STATE", "COUNTY"))
View(merged_data)


# Two new variables that have farmland percentages of corn and wheat.

merged_data$pct_corn <- (merged_data$acres_corn / merged_data$farmland) * 100

merged_data$pct_wheat <- (merged_data$acres_wheat / merged_data$farmland) * 100

merged_data = apply_labels(merged_data,
                         pct_corn = "Percentage of Farmland in Corn",
                         pct_wheat = "Percentage of Farmland in Wheat")
View(merged_data)

# Average pct_corn and average pct_wheat for counties in Kansas, Iowa, and Michigan.

kansas_corn <- merged_data[merged_data$STATE == "Kansas", "pct_corn"]
kansas_corn_avg <- mean(kansas_corn)
kansas_wheat <- merged_data[merged_data$STATE == "Kansas", "pct_wheat"]
kansas_wheat_avg <- mean(kansas_wheat)
print(kansas_corn_avg) #12.94 %
print(kansas_wheat_avg) #20.27 %

michigan_corn <- merged_data[merged_data$STATE == "Michigan", "pct_corn"]
michigan_corn_avg <- mean(michigan_corn)
michigan_wheat <- merged_data[merged_data$STATE == "Michigan", "pct_wheat"]
michigan_wheat_avg <- mean(michigan_wheat)
print(michigan_corn_avg) # 3.11 %
print(michigan_wheat_avg) # 3.06%

iowa_corn <- merged_data[merged_data$STATE == "Iowa", "pct_corn"]
iowa_corn_avg <- mean(iowa_corn)
iowa_wheat <- merged_data[merged_data$STATE == "Iowa", "pct_wheat"]
iowa_wheat_avg <- mean(iowa_wheat)
print(iowa_corn_avg) # 26.09 %
print(iowa_wheat_avg) # 1.42 %

# Create Scatterplot w/ line of best fit comparing percent corn and percent wheat

plot(merged_data$pct_corn, merged_data$pct_wheat,
     main="Scatterplot of Percent of Farmland in Corn vs. Wheat", xlab="Percent Corn ", ylab="Percent Wheat ", pch=19)
attach(merged_data)
abline(lm(pct_corn~pct_wheat), col="red")


## Create variable of tractors per farm and label it

merged_data$tractors_per_farm <- merged_data$tractors / merged_data$farms
merged_data = apply_labels(merged_data,
                           tractors_per_farm = "Tractors per Farm")
View(merged_data)

# Create regression models 

regression <- summary(lm(tractors_per_farm  ~ pct_corn + pct_wheat, data = merged_data))
print(regression)

regression_states <- summary(lm(tractors_per_farm  ~ pct_corn + pct_wheat + STATE, data = merged_data))
print(regression_states)

# Interpretation

## The results from these regression models indicate that the number of tractors per farm is slightly greater when
# percent wheat is higher compared to percent corn of total farmland. Additionally, some states seem to be predictive 
# of tractor per farm ratios. Missouri for example is a state that is a strong predictor of the ratio.













