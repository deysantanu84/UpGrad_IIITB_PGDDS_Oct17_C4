############################################################
# HR Analytics Case Study (Group Project)
# Members:
# 1. Ashutosh Kumar (DDA1730347)
# 2. Daniel Narendra(DDA1730118)
# 3. Medhavi Shruti (DDA1730035) 
# 4. Santanu Dey    (DDA1730043)
############################################################
library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)
library(corrplot)
library(ggplot2)
library(gridExtra)
library(MASS)
library(car)
library(GGally)
library(e1071)
library(caret)
library(cowplot)
library(caTools)
library(ROCR)

general_data <- read.csv("general_data.csv", stringsAsFactors = F)
employee_survey_data <- read.csv("employee_survey_data.csv", stringsAsFactors = F)
manager_survey_data <- read.csv("manager_survey_data.csv", stringsAsFactors = F)
in_time <- read.csv("in_time.csv", stringsAsFactors = F)
out_time <- read.csv("out_time.csv", stringsAsFactors = F)

str(general_data)
str(employee_survey_data)
str(manager_survey_data)
str(in_time)
str(out_time)

# Change name of first column from 'X' to 'EmployeeID' for in_time and out_time
colnames(in_time) [1]  <- "EmployeeID"
colnames(out_time) [1] <- "EmployeeID"

# Check for duplicates
sum(duplicated(general_data$EmployeeID))    # 0 duplicates
sum(duplicated(employee_survey_data$EmployeeID)) # 0 duplicates
sum(duplicated(manager_survey_data$EmployeeID))  # 0 duplicates
sum(duplicated(in_time$EmployeeID))         # 0 duplicates
sum(duplicated(out_time$EmployeeID))        # 0 duplicates

# Data cleaning
# General Data
colSums(is.na(general_data))
# 19 NAs for NumCompaniesWorked and 9 NAs for TotalWorkingYears

# Replace NAs for NumCompaniesWorked
general_data[which(is.na(general_data$NumCompaniesWorked)), c(15, 20, 22)]

# Replace each NA with 2 if TotalWorkingYears (Non-NA) > YearsAtCompany
general_data$NumCompaniesWorked[which(is.na(general_data$NumCompaniesWorked) & !is.na(general_data$TotalWorkingYears))] <- ifelse(general_data$TotalWorkingYears[which(is.na(general_data$NumCompaniesWorked) & !is.na(general_data$TotalWorkingYears))] > general_data$YearsAtCompany[which(is.na(general_data$NumCompaniesWorked) & !is.na(general_data$TotalWorkingYears))], 2, NA) # 14 NAs replaced with 2

# Replace each NA with 1 if TotalWorkingYears (Non-NA) == YearsAtCompany
general_data$NumCompaniesWorked[which(is.na(general_data$NumCompaniesWorked) & !is.na(general_data$TotalWorkingYears))] <- ifelse(general_data$TotalWorkingYears[which(is.na(general_data$NumCompaniesWorked) & !is.na(general_data$TotalWorkingYears))]==general_data$YearsAtCompany[which(is.na(general_data$NumCompaniesWorked) & !is.na(general_data$TotalWorkingYears))], 1, NA) # 5 NAs replaced with 1

# Multiple 0 values for NumCompaniesWorked which is NOT VALID.
length(which(general_data$NumCompaniesWorked==0)) # 586

# Replace each 0 with 2 if TotalWorkingYears (Non-NA) > YearsAtCompany
general_data$NumCompaniesWorked[which(general_data$NumCompaniesWorked==0 & !is.na(general_data$TotalWorkingYears))] <- ifelse(general_data$TotalWorkingYears[which(general_data$NumCompaniesWorked==0 & !is.na(general_data$TotalWorkingYears))] > general_data$YearsAtCompany[which(general_data$NumCompaniesWorked==0 & !is.na(general_data$TotalWorkingYears))], 2, 0)

# Replace each 0 with 1 if TotalWorkingYears (Non-NA) == YearsAtCompany
general_data$NumCompaniesWorked[which(general_data$NumCompaniesWorked==0 & !is.na(general_data$TotalWorkingYears))] <- ifelse(general_data$TotalWorkingYears[which(general_data$NumCompaniesWorked==0 & !is.na(general_data$TotalWorkingYears))] == general_data$YearsAtCompany[which(general_data$NumCompaniesWorked==0 & !is.na(general_data$TotalWorkingYears))], 1, 0)
# 2 0's remaining for NumCompaniesWorked corresponding to NA TotalWorkingYears

# Replace NAs for TotalWorkingYears
general_data[which(is.na(general_data$TotalWorkingYears)), c(15, 20, 22)]

# Replace each NA with YearsAtCompany if NumCompaniesWorked is 1 or 0
general_data$TotalWorkingYears[which(is.na(general_data$TotalWorkingYears))] <- ifelse(general_data$NumCompaniesWorked[which(is.na(general_data$TotalWorkingYears))] == 1 | general_data$NumCompaniesWorked[which(is.na(general_data$TotalWorkingYears))] == 0, general_data$YearsAtCompany[which(is.na(general_data$TotalWorkingYears))], NA)
# 4 NAs replaced with YearsAtCompany

# Replace remaining two 0s in NumCompaniesWorked with 1
general_data$NumCompaniesWorked[which(general_data$NumCompaniesWorked==0)] <- 1

# Corresponding to the remaining 5 NAs for TotalWorkingYears, we can replace with the column Median value of 10.
general_data$TotalWorkingYears[which(is.na(general_data$TotalWorkingYears))] <- 10

# Employee Survey Data
colSums(is.na(employee_survey_data)) # Few NAs
sum(is.na(employee_survey_data)) # 83, which is less than 1% of the entire data
# Since the NA values are for employee survey ratings, and represent less than 1% of the data, we can replace each NA with the median value of each column
summary(employee_survey_data)
employee_survey_data$EnvironmentSatisfaction[which(is.na(employee_survey_data$EnvironmentSatisfaction))] <- 3 # Column Median is 3
employee_survey_data$JobSatisfaction[which(is.na(employee_survey_data$JobSatisfaction))] <- 3 # Column Median is 3
employee_survey_data$WorkLifeBalance[which(is.na(employee_survey_data$WorkLifeBalance))] <- 3 # Column Median is 3

# Manager Survey Data
colSums(is.na(manager_survey_data)) # 0 NAs

################################################################
# Convert in_time and out_time from long format to wide format
in_time <-  gather(in_time, date, swipe_in, X2015.01.01:X2015.12.31)
in_time$date <- gsub("X", "", in_time$date)
in_time <- in_time[!is.na(in_time$swipe_in),]
in_time$swipe_in <- ymd_hms(in_time$swipe_in, tz = "")

out_time <-  gather(out_time, date, swipe_out, X2015.01.01:X2015.12.31)
out_time$date <- gsub("X", "", out_time$date)
out_time <- out_time[!is.na(out_time$swipe_out),]
out_time$swipe_out <- ymd_hms(out_time$swipe_out, tz = "")

# Merge in_time and out_time
colSums(is.na(in_time)) # 0 NAs
colSums(is.na(out_time)) # 0 NAs
employee_attendance <- cbind(in_time, out_time$swipe_out)
colnames(employee_attendance)[4] <- 'swipe_out'
# Calculate work duration upto 5 decimal places
employee_attendance$work_duration <- as.numeric(round(employee_attendance$swipe_out - employee_attendance$swipe_in, 5))

# Calculate average work duration
avg_work_hours <- aggregate(employee_attendance$work_duration, by = list(employee_attendance$EmployeeID), mean)
colnames(avg_work_hours) <- c('EmployeeID', 'avg_work_hours')
avg_work_hours$avg_work_hours <- round(avg_work_hours$avg_work_hours, 5)

###############################################################################
# Collate the data together in one single file
length(unique(tolower(general_data$EmployeeID))) # 4410, confirming EmployeeID is key 
length(unique(tolower(employee_survey_data$EmployeeID))) # 4410, confirming EmployeeID is key
length(unique(tolower(manager_survey_data$EmployeeID))) # 4410, confirming EmployeeID is key
length(unique(tolower(avg_work_hours$EmployeeID))) # 4410, confirming EmployeeID is key

setdiff(general_data$EmployeeID, employee_survey_data$EmployeeID) # Identical EmployeeID across these datasets
setdiff(general_data$EmployeeID, manager_survey_data$EmployeeID) # Identical EmployeeID across these datasets
setdiff(general_data$EmployeeID, avg_work_hours$EmployeeID) # Identical EmployeeID across these datasets

# Merge all the data into a single data frame
employee_hr_data <- merge(general_data, employee_survey_data, by = "EmployeeID")
employee_hr_data <- merge(employee_hr_data, manager_survey_data, by = "EmployeeID")
employee_hr_data <- merge(employee_hr_data, avg_work_hours, by = "EmployeeID")

View(employee_hr_data)

str(employee_hr_data)

length(which(employee_hr_data$StandardHours == 8)) # 4410, all entries are 8
length(which(employee_hr_data$Over18 == 'Y')) # 4410, all entries are 'Y'
length(which(employee_hr_data$EmployeeCount == 1)) # 4410, all entries are 1
# We can remove the EmployeeCount, StandardHours and Over18 columns
employee_hr_data <- employee_hr_data[, -c(9, 16, 18)]

# De-Duplication
sum(duplicated(employee_hr_data)) # 0 duplicates

# Outlier treatment of continuous features
# Create a vector of continuous features
employee_hr_data_num <- c("Age", "DistanceFromHome", "MonthlyIncome", "NumCompaniesWorked", "PercentSalaryHike", "TotalWorkingYears", "YearsAtCompany", "YearsSinceLastPromotion", "YearsWithCurrManager", "avg_work_hours")

# Check quantiles
sapply(employee_hr_data[, employee_hr_data_num], function(x) quantile(x, seq(0, 1, .01)))

# MonthlyIncome, TotalWorkingYears, YearsAtCompany, YearsSinceLastPromotion, YearsWithCurrManager, and avg_work_hours have outliers as per boxplot and quantiles
# But outliers for each of these columns are practically possible, so no outlier treatment required

################################################################
### Exploratory Data Analysis
# Barcharts for categorical features with stacked employee information
bar_theme1 <- theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5), legend.position="top")

plot_grid(ggplot(employee_hr_data, aes(x=JobLevel,fill=factor(Attrition)))+ geom_bar()+ bar_theme1,
          ggplot(employee_hr_data, aes(x=JobRole,fill=factor(Attrition)))+ geom_bar()+ bar_theme1,
          align = "h")
# Attrition is high among employees in job levels 1 and 2, while employees in the job roles Lab Technician, Research Scientist, and Sales Executives mostly tend to leave.

plot_grid(ggplot(employee_hr_data, aes(x=BusinessTravel,fill=factor(Attrition)))+ geom_bar()+bar_theme1,
          ggplot(employee_hr_data, aes(x=StockOptionLevel,fill=factor(Attrition)))+ geom_bar()+bar_theme1,
          align = "h")
# Attrition is highest among employees who travel rarely, while employees having stock option levels of 0 and 1 mostly tend to leave.

plot_grid(ggplot(employee_hr_data, aes(x=Education,fill=factor(Attrition)))+ geom_bar()+bar_theme1,
          ggplot(employee_hr_data, aes(x=EducationField,fill=factor(Attrition)))+ geom_bar()+bar_theme1,
          align = "h")
# Attrition is high among employees with Education levels of 2, 3, and 4, while employees from the education fields of Life Sciences and Medical mostly tend to leave.

plot_grid(ggplot(employee_hr_data, aes(x=Gender,fill=factor(Attrition)))+ geom_bar()+bar_theme1,
          ggplot(employee_hr_data, aes(x=MaritalStatus,fill=factor(Attrition)))+ geom_bar()+bar_theme1,
          align = "h") 
# Attrition is highest among Male and Single employees

plot_grid(ggplot(employee_hr_data, aes(x=Department,fill=factor(Attrition)))+ geom_bar()+ bar_theme1,
          ggplot(employee_hr_data, aes(x=EnvironmentSatisfaction,fill=factor(Attrition)))+ geom_bar()+bar_theme1,
          align = "h") 
# Attrition is highest in the R&D department, while it is similar across all EnvironmentSatisfaction ratings

plot_grid(ggplot(employee_hr_data, aes(x=JobSatisfaction,fill=factor(Attrition)))+ geom_bar()+bar_theme1,
          ggplot(employee_hr_data, aes(x=WorkLifeBalance,fill=factor(Attrition)))+ geom_bar()+bar_theme1,
          align = "h")
# Attrition is similar across all JobSatisfaction ratings, while it is highest for WorkLifeBalance rating of 3

plot_grid(ggplot(employee_hr_data, aes(x=JobInvolvement,fill=factor(Attrition)))+ geom_bar()+bar_theme1,
          ggplot(employee_hr_data, aes(x=factor(PerformanceRating),fill=factor(Attrition)))+ geom_bar()+bar_theme1,
          align = "h")
# The employees getting JobInvolvement rating of 3, and Performance rating of 3 mostly tend to leave the company.

####################################################
# Histogram for numeric variables 
plot_grid(ggplot(employee_hr_data, aes(Age, fill = factor(Attrition)), col = "Black")+ geom_histogram(breaks=seq(10, 60, by=10), col = "Black"),
          ggplot(employee_hr_data, aes(MonthlyIncome, fill = factor(Attrition)), col = "Black")+ geom_histogram(breaks=seq(0, 200000, by=20000), col = "Black"), 
          align = "v",ncol = 1)
# Attrition is high among employees in the Age group 20-40 years, and those with Monthly income 20K to 60K INR per month

plot_grid(ggplot(employee_hr_data, aes(DistanceFromHome, fill = factor(Attrition)), col = "Black")+ geom_histogram(breaks=seq(0, 30, by=5), col = "Black"),
          ggplot(employee_hr_data, aes(NumCompaniesWorked, fill = factor(Attrition)), col = "Black")+ geom_histogram(breaks=seq(0, 10, by=2), col = "Black"), 
          align = "v",ncol = 1)
# Attrition is highest among employees whose homes are at a distance of upto 10 kms, and those who have worked in 2 companies

plot_grid(ggplot(employee_hr_data, aes(PercentSalaryHike, fill = factor(Attrition)), col = "Black")+ geom_histogram(breaks=seq(10, 25, by=5), col = "Black"),
          ggplot(employee_hr_data, aes(TotalWorkingYears, fill = factor(Attrition)), col = "Black")+ geom_histogram(breaks=seq(0, 40, by=10), col = "Black"), 
          align = "v",ncol = 1)
# Attrition is highest among employees who get 10-15 percent Salary Hike, and those who have a total of upto 10 years work experience.

plot_grid(ggplot(employee_hr_data, aes(YearsAtCompany, fill = factor(Attrition)), col = "Black")+ geom_histogram(breaks=seq(0, 40, by=10), col = "Black"),
          ggplot(employee_hr_data, aes(YearsSinceLastPromotion, fill = factor(Attrition)), col = "Black")+ geom_histogram(breaks=seq(0, 15, by=5), col = "Black"), 
          align = "v",ncol = 1)
# Attrition is highest among employees who have worked upto 10 years in the current company, and those who have not received a promotion in the last 0-5 years.

plot_grid(ggplot(employee_hr_data, aes(YearsWithCurrManager, fill = factor(Attrition)), col = "Black")+ geom_histogram(breaks=seq(0, 20, by=5), col = "Black"),
          ggplot(employee_hr_data, aes(avg_work_hours, fill = factor(Attrition)), col = "Black")+ geom_histogram(breaks=seq(5, 12, by=1), col = "Black"), 
          align = "v",ncol = 1)
# Attrition is highest among employees among employees who have worked upto 5 years with the current manager, and is similar for those with 6-11 average working hours.

################################################################
# Feature standardisation
# Normalising continuous features
employee_hr_data[, employee_hr_data_num] <- sapply(employee_hr_data[, employee_hr_data_num], function(x) scale(x))

# Convert target variable Attrition from No/Yes character to factor with levels 0 and 1 
employee_hr_data$Attrition <- ifelse(employee_hr_data$Attrition == "Yes", 1, 0)

# Checking attrition rate of employees
Attrition <- sum(employee_hr_data$Attrition)/nrow(employee_hr_data)
Attrition # 16.12% Attrition Rate

# Create a dataframe of categorical features
employee_hr_data_chr <- employee_hr_data[, c("Attrition", "BusinessTravel", "Department", "Education", "EducationField", "Gender", "JobLevel", "JobRole", "MaritalStatus", "StockOptionLevel", "EnvironmentSatisfaction", "JobSatisfaction", "WorkLifeBalance", "JobInvolvement", "PerformanceRating")]

# Convert categorical attributes to factor
employee_hr_data_fact <- data.frame(sapply(employee_hr_data_chr, function(x) factor(x)))
str(employee_hr_data_fact)

# creating dummy variables for factor attributes
dummies <- data.frame(sapply(employee_hr_data_fact, 
                            function(x) data.frame(model.matrix(~x-1, data = employee_hr_data_fact))[ , -1]))

# Final dataset
employee_hr_data_final <- cbind(employee_hr_data[, employee_hr_data_num], dummies) 
View(employee_hr_data_final) # 4410 obs. of  55 variables

########################################################################
# Split the data between train and test
set.seed(100)
indices <- sample.split(employee_hr_data_final$Attrition, SplitRatio = 0.7)
train <- employee_hr_data_final[indices,]
test <- employee_hr_data_final[!(indices),]

########################################################################
# Logistic Regression:

# Initial model
model_1 <- glm(Attrition ~ ., data = train, family = "binomial")
summary(model_1) # AIC 2159....54 coeff..nullDev 2728.0...resDev 2049

# Stepwise selection
model_2 <- stepAIC(model_1, direction = "both")
summary(model_2) # AIC 2132.2....33 coeff..nullDev 2728.0...resDev 2064.2

# Removing multicollinearity through VIF check
vif(model_2)

# The variables with high VIF have high significance, and hence cannot be excluded
# Let's exclude EducationField.xLife.Sciences which has the highest p-value
model_3 <- glm(formula = Attrition ~ Age + DistanceFromHome + MonthlyIncome + 
                   NumCompaniesWorked + TotalWorkingYears + YearsAtCompany + 
                   YearsSinceLastPromotion + YearsWithCurrManager + avg_work_hours + 
                   BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                   Department.xResearch...Development + Department.xSales + 
                   Education.x3 + Education.x4 + JobLevel.x2 + JobRole.xLaboratory.Technician + 
                   JobRole.xResearch.Director + JobRole.xResearch.Scientist + 
                   JobRole.xSales.Executive + MaritalStatus.xSingle + StockOptionLevel.x1 + 
                   EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                   EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                   JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                   WorkLifeBalance.x4 + JobInvolvement.x3, 
               family = "binomial", data = train)
summary(model_3) # AIC 2132.4....32 coeff..nullDev 2728.0...resDev 2066.4
# AIC has not changed significantly
vif(model_3)

# The variables with high VIF have high significance, and hence cannot be excluded
# Let's exclude Education.x3 which has the highest p-value
model_4 <- glm(formula = Attrition ~ Age + DistanceFromHome + MonthlyIncome + 
                   NumCompaniesWorked + TotalWorkingYears + YearsAtCompany + 
                   YearsSinceLastPromotion + YearsWithCurrManager + avg_work_hours + 
                   BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                   Department.xResearch...Development + Department.xSales + 
                   Education.x4 + JobLevel.x2 + JobRole.xLaboratory.Technician + 
                   JobRole.xResearch.Director + JobRole.xResearch.Scientist + 
                   JobRole.xSales.Executive + MaritalStatus.xSingle + StockOptionLevel.x1 + 
                   EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                   EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                   JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                   WorkLifeBalance.x4 + JobInvolvement.x3, 
               family = "binomial", data = train)
summary(model_4) # AIC 2133.1 .... 31 coeff..nullDev 2728.0...resDev 2069.1
# AIC has not changed significantly
vif(model_4)

# The variables with high VIF have high significance, and hence cannot be excluded
# Let's exclude Education.x4 which has the highest p-value
model_5 <- glm(formula = Attrition ~ Age + DistanceFromHome + MonthlyIncome + 
                   NumCompaniesWorked + TotalWorkingYears + YearsAtCompany + 
                   YearsSinceLastPromotion + YearsWithCurrManager + avg_work_hours + 
                   BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                   Department.xResearch...Development + Department.xSales + 
                   JobLevel.x2 + JobRole.xLaboratory.Technician + 
                   JobRole.xResearch.Director + JobRole.xResearch.Scientist + 
                   JobRole.xSales.Executive + MaritalStatus.xSingle + StockOptionLevel.x1 + 
                   EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                   EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                   JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                   WorkLifeBalance.x4 + JobInvolvement.x3, 
               family = "binomial", data = train)
summary(model_5) # AIC 2133.1 .... 30 coeff..nullDev 2728.0...resDev 2071.1
# AIC has not changed significantly
vif(model_5)

# The variables with high VIF have high significance, and hence cannot be excluded
# Let's exclude StockOptionLevel.x1 which has the highest p-value
model_6 <- glm(formula = Attrition ~ Age + DistanceFromHome + MonthlyIncome + 
                   NumCompaniesWorked + TotalWorkingYears + YearsAtCompany + 
                   YearsSinceLastPromotion + YearsWithCurrManager + avg_work_hours + 
                   BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                   Department.xResearch...Development + Department.xSales + 
                   JobLevel.x2 + JobRole.xLaboratory.Technician + 
                   JobRole.xResearch.Director + JobRole.xResearch.Scientist + 
                   JobRole.xSales.Executive + MaritalStatus.xSingle + 
                   EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                   EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                   JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                   WorkLifeBalance.x4 + JobInvolvement.x3, 
               family = "binomial", data = train)
summary(model_6) # AIC 2133.5....29 coeff..nullDev 2728.0...resDev 2073.5
# AIC has not changed significantly
vif(model_6)

# The variables with high VIF have high significance, and hence cannot be excluded
# Let's exclude DistanceFromHome which has the highest p-value
model_7 <- glm(formula = Attrition ~ Age + MonthlyIncome + 
                   NumCompaniesWorked + TotalWorkingYears + YearsAtCompany + 
                   YearsSinceLastPromotion + YearsWithCurrManager + avg_work_hours + 
                   BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                   Department.xResearch...Development + Department.xSales + 
                   JobLevel.x2 + JobRole.xLaboratory.Technician + 
                   JobRole.xResearch.Director + JobRole.xResearch.Scientist + 
                   JobRole.xSales.Executive + MaritalStatus.xSingle + 
                   EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                   EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                   JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                   WorkLifeBalance.x4 + JobInvolvement.x3, 
               family = "binomial", data = train)
summary(model_7) # AIC 2134.3....28 coeff..nullDev 2728.0...resDev 2076.3
# AIC has not changed significantly
vif(model_7)

# The variables with high VIF have high significance, and hence cannot be excluded
# Let's exclude MonthlyIncome which has the highest p-value
model_8 <- glm(formula = Attrition ~ Age + 
                   NumCompaniesWorked + TotalWorkingYears + YearsAtCompany + 
                   YearsSinceLastPromotion + YearsWithCurrManager + avg_work_hours + 
                   BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                   Department.xResearch...Development + Department.xSales + 
                   JobLevel.x2 + JobRole.xLaboratory.Technician + 
                   JobRole.xResearch.Director + JobRole.xResearch.Scientist + 
                   JobRole.xSales.Executive + MaritalStatus.xSingle + 
                   EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                   EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                   JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                   WorkLifeBalance.x4 + JobInvolvement.x3, 
               family = "binomial", data = train)
summary(model_8) # AIC 2136.6....27 coeff..nullDev 2728.0...resDev 2080.6
# AIC has not changed significantly
vif(model_8)

# The variables with high VIF have high significance, and hence cannot be excluded
# Let's exclude JobInvolvement.x3 which has the highest p-value
model_9 <- glm(formula = Attrition ~ Age + 
                   NumCompaniesWorked + TotalWorkingYears + YearsAtCompany + 
                   YearsSinceLastPromotion + YearsWithCurrManager + avg_work_hours + 
                   BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                   Department.xResearch...Development + Department.xSales + 
                   JobLevel.x2 + JobRole.xLaboratory.Technician + 
                   JobRole.xResearch.Director + JobRole.xResearch.Scientist + 
                   JobRole.xSales.Executive + MaritalStatus.xSingle + 
                   EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                   EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                   JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                   WorkLifeBalance.x4, 
               family = "binomial", data = train)
summary(model_9) # AIC 2139.3....26 coeff..nullDev 2728.0...resDev 2085.3
# AIC has not changed significantly
vif(model_9)

# The variables with high VIF have high significance, and hence cannot be excluded
# Let's exclude YearsAtCompany which has the highest p-value
model_10 <- glm(formula = Attrition ~ Age + 
                    NumCompaniesWorked + TotalWorkingYears + 
                    YearsSinceLastPromotion + YearsWithCurrManager + avg_work_hours + 
                    BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                    Department.xResearch...Development + Department.xSales + 
                    JobLevel.x2 + JobRole.xLaboratory.Technician + 
                    JobRole.xResearch.Director + JobRole.xResearch.Scientist + 
                    JobRole.xSales.Executive + MaritalStatus.xSingle + 
                    EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                    EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                    JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                    WorkLifeBalance.x4, 
                family = "binomial", data = train)
summary(model_10) # AIC 2141.8....25 coeff..nullDev 2728.0...resDev 2089.8
# AIC has not changed significantly
vif(model_10)

# The variables with high VIF have high significance, and hence cannot be excluded
# Let's exclude JobLevel.x2 which has the highest p-value
model_11 <- glm(formula = Attrition ~ Age + 
                    NumCompaniesWorked + TotalWorkingYears + 
                    YearsSinceLastPromotion + YearsWithCurrManager + avg_work_hours + 
                    BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                    Department.xResearch...Development + Department.xSales + 
                    JobRole.xLaboratory.Technician + 
                    JobRole.xResearch.Director + JobRole.xResearch.Scientist + 
                    JobRole.xSales.Executive + MaritalStatus.xSingle + 
                    EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                    EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                    JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                    WorkLifeBalance.x4, 
                family = "binomial", data = train)
summary(model_11) # AIC 2144.6....24 coeff..nullDev 2728.0...resDev 2094.6
# AIC has not changed significantly
vif(model_11)

# The variables with high VIF have high significance, and hence cannot be excluded
# Let's exclude JobRole.xLaboratory.Technician which has the highest p-value
model_12 <- glm(formula = Attrition ~ Age + 
                    NumCompaniesWorked + TotalWorkingYears + 
                    YearsSinceLastPromotion + YearsWithCurrManager + avg_work_hours + 
                    BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                    Department.xResearch...Development + Department.xSales + 
                    JobRole.xResearch.Director + JobRole.xResearch.Scientist + 
                    JobRole.xSales.Executive + MaritalStatus.xSingle + 
                    EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                    EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                    JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                    WorkLifeBalance.x4, 
                family = "binomial", data = train)
summary(model_12) # AIC 2147.7....23 coeff..nullDev 2728.0...resDev 2099.7
# AIC has not changed significantly
vif(model_12)

# The variables with high VIF have high significance, and hence cannot be excluded
# Let's exclude JobRole.xResearch.Scientist which has the highest p-value
model_13 <- glm(formula = Attrition ~ Age + 
                    NumCompaniesWorked + TotalWorkingYears + 
                    YearsSinceLastPromotion + YearsWithCurrManager + avg_work_hours + 
                    BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                    Department.xResearch...Development + Department.xSales + 
                    JobRole.xResearch.Director + 
                    JobRole.xSales.Executive + MaritalStatus.xSingle + 
                    EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                    EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                    JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                    WorkLifeBalance.x4, 
                family = "binomial", data = train)
summary(model_13) # AIC 2151.3....22 coeff..nullDev 2728.0...resDev 2105.3
# AIC has not changed significantly
vif(model_13)

# The variables with high VIF have high significance, and hence cannot be excluded
# Let's exclude JobRole.xResearch.Director which has the highest p-value
model_14 <- glm(formula = Attrition ~ Age + 
                    NumCompaniesWorked + TotalWorkingYears + 
                    YearsSinceLastPromotion + YearsWithCurrManager + avg_work_hours + 
                    BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                    Department.xResearch...Development + Department.xSales + 
                    JobRole.xSales.Executive + MaritalStatus.xSingle + 
                    EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                    EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                    JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                    WorkLifeBalance.x4, 
                family = "binomial", data = train)
summary(model_14) # AIC 2156.4....21 coeff..nullDev 2728.0...resDev 2112.4
# AIC has not changed significantly
vif(model_14)

# The variables with high VIF have high significance, and hence cannot be excluded
# Let's exclude JobRole.xSales.Executive which has the highest p-value
model_15 <- glm(formula = Attrition ~ Age + 
                    NumCompaniesWorked + TotalWorkingYears + 
                    YearsSinceLastPromotion + YearsWithCurrManager + avg_work_hours + 
                    BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                    Department.xResearch...Development + Department.xSales + 
                    MaritalStatus.xSingle + 
                    EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                    EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                    JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                    WorkLifeBalance.x4, 
                family = "binomial", data = train)
summary(model_15) # AIC 2160.9....20 coeff..nullDev 2728.0...resDev 2118.9
# AIC has not changed significantly
vif(model_15)

# The variables with high VIF have high significance, and hence cannot be excluded
# Let's exclude JobSatisfaction.x3 which has the highest p-value
model_16 <- glm(formula = Attrition ~ Age + 
                    NumCompaniesWorked + TotalWorkingYears + 
                    YearsSinceLastPromotion + YearsWithCurrManager + avg_work_hours + 
                    BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                    Department.xResearch...Development + Department.xSales + 
                    MaritalStatus.xSingle + 
                    EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                    EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + 
                    JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                    WorkLifeBalance.x4, 
                family = "binomial", data = train)
summary(model_16) # AIC 2167.9....19 coeff..nullDev 2728.0...resDev 2127.9
# AIC has not changed significantly
vif(model_16)

# The variables with high VIF have high significance, and hence cannot be excluded
# Let's exclude JobSatisfaction.x2 which has the highest p-value
model_17 <- glm(formula = Attrition ~ Age + 
                    NumCompaniesWorked + TotalWorkingYears + 
                    YearsSinceLastPromotion + YearsWithCurrManager + avg_work_hours + 
                    BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                    Department.xResearch...Development + Department.xSales + 
                    MaritalStatus.xSingle + 
                    EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                    EnvironmentSatisfaction.x4 + 
                    JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                    WorkLifeBalance.x4, 
                family = "binomial", data = train)
summary(model_17) # AIC 2171.2....18 coeff..nullDev 2728.0...resDev 2133.2
# AIC has not changed significantly
vif(model_17)

# The variables with high VIF have high significance, and hence cannot be excluded
# Let's exclude WorkLifeBalance.x4 which has the highest p-value
model_18 <- glm(formula = Attrition ~ Age + 
                    NumCompaniesWorked + TotalWorkingYears + 
                    YearsSinceLastPromotion + YearsWithCurrManager + avg_work_hours + 
                    BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                    Department.xResearch...Development + Department.xSales + 
                    MaritalStatus.xSingle + 
                    EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                    EnvironmentSatisfaction.x4 + 
                    JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3, 
                family = "binomial", data = train)
summary(model_18) # AIC 2178.2....17 coeff..nullDev 2728.0...resDev 2142.2
# AIC has not changed significantly
vif(model_18)

# The variables with high VIF have high significance, and hence cannot be excluded
# Let's exclude Age which has the highest p-value
model_19 <- glm(formula = Attrition ~ NumCompaniesWorked + TotalWorkingYears + 
                    YearsSinceLastPromotion + YearsWithCurrManager + avg_work_hours + 
                    BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                    Department.xResearch...Development + Department.xSales + 
                    MaritalStatus.xSingle + 
                    EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                    EnvironmentSatisfaction.x4 + 
                    JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3, 
                family = "binomial", data = train)
summary(model_19) # AIC 2195.2....16 coeff..nullDev 2728.0...resDev 2161.2
# AIC has changed SIGNIFICANTLY, so Age SHOULD NOT be removed.
vif(model_19)

# The variables with high VIF have high significance, and hence cannot be excluded
# Let's exclude WorkLifeBalance.x2 which has the second highest p-value, instead of Age
model_19 <- glm(formula = Attrition ~ Age + 
                    NumCompaniesWorked + TotalWorkingYears + 
                    YearsSinceLastPromotion + YearsWithCurrManager + avg_work_hours + 
                    BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                    Department.xResearch...Development + Department.xSales + 
                    MaritalStatus.xSingle + 
                    EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                    EnvironmentSatisfaction.x4 + 
                    JobSatisfaction.x4 + WorkLifeBalance.x3, 
                family = "binomial", data = train)
summary(model_19) # AIC 2183.6....16 coeff..nullDev 2728.0...resDev 2149.6
# AIC has not changed significantly
vif(model_19)

# The variables with high VIF have high significance, and hence cannot be excluded
# Let's exclude WorkLifeBalance.x3 which has the highest p-value
model_20 <- glm(formula = Attrition ~ Age + 
                    NumCompaniesWorked + TotalWorkingYears + 
                    YearsSinceLastPromotion + YearsWithCurrManager + avg_work_hours + 
                    BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                    Department.xResearch...Development + Department.xSales + 
                    MaritalStatus.xSingle + 
                    EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                    EnvironmentSatisfaction.x4 + 
                    JobSatisfaction.x4, 
                family = "binomial", data = train)
summary(model_20) # AIC 2193.4....15 coeff..nullDev 2728.0...resDev 2161.4
# AIC has SIGNIFICANTLY, so WorkLifeBalance.x3 SHOULD NOT be removed.
vif(model_20)

# All the remaining variables have very high significance, and hence model_19 can be considered the final model.
final_model <- model_19

#######################################################################
### Model Evaluation
### Test Data ####
# Predicted probabilities of Attrition for test data
test_pred <- predict(final_model, type = "response", newdata = test)

# Let's see the summary 
summary(test_pred)

test$prob <- test_pred
View(test)
#######################################################################
# Let's Choose the cutoff value. 
# Let's find out the optimal probalility cutoff 
test_actual_attrition <- factor(ifelse(test$Attrition == 1, "Yes", "No"))

perform_fn <- function(cutoff) 
{
    predicted_attrition <- factor(ifelse(test_pred >= cutoff, "Yes", "No"))
    conf <- confusionMatrix(predicted_attrition, test_actual_attrition, positive = "Yes")
    acc <- conf$overall[1]
    sens <- conf$byClass[1]
    spec <- conf$byClass[2]
    out <- t(as.matrix(c(sens, spec, acc))) 
    colnames(out) <- c("sensitivity", "specificity", "accuracy")
    return(out)
}

# Summary of test probability
summary(test_pred)

s <- seq(.01, .80, length = 100)

OUT <- matrix(0, 100, 3)

for(i in 1:100)
{
    OUT[i,] = perform_fn(s[i])
}

plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT[,2],col="darkgreen",lwd=2)
lines(s,OUT[,3],col=4,lwd=2)
box()
legend(0,.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))

cutoff <- s[which(abs(OUT[,1]-OUT[,2])<0.01)]

# Let's choose a cutoff value of 0.162 for the final model
test_pred_attrition <- factor(ifelse(test_pred >= 0.162, "Yes", "No"))

table(test_actual_attrition, test_pred_attrition)

test_cutoff_attrition <- factor(ifelse(test_pred >=0.162, "Yes", "No"))
conf_final <- confusionMatrix(test_cutoff_attrition, test_actual_attrition, positive = "Yes")

acc <- conf_final$overall[1]
sens <- conf_final$byClass[1]
spec <- conf_final$byClass[2]
acc
sens
spec

View(test)

########## KS -statistic - Test Data ######
test_cutoff_attrition <- ifelse(test_cutoff_attrition == "Yes", 1, 0)
test_actual_attrition <- ifelse(test_actual_attrition == "Yes", 1, 0)

pred_object_test <- prediction(test_cutoff_attrition, test_actual_attrition)
performance_measures_test <- performance(pred_object_test, "tpr", "fpr")

ks_table_test <- attr(performance_measures_test, "y.values")[[1]] - 
    (attr(performance_measures_test, "x.values")[[1]])

max(ks_table_test)

####################################################################
# Lift & Gain Chart 

# plotting the lift chart

lift <- function(labels, predicted_prob, groups = 10) {
    if(is.factor(labels)) labels  <- as.integer(as.character(labels ))
    if(is.factor(predicted_prob)) predicted_prob <- as.integer(as.character(predicted_prob))
    helper <- data.frame(cbind(labels, predicted_prob))
    helper[,"bucket"] = ntile(-helper[,"predicted_prob"], groups)
    gaintable = helper %>% group_by(bucket)  %>%
        summarise_at(vars(labels ), funs(total = n(),
                                         totalresp=sum(., na.rm = TRUE))) %>%
        
        mutate(Cumresp = cumsum(totalresp),
               Gain=Cumresp/sum(totalresp)*100,
               Cumlift=Gain/(bucket*(100/groups))) 
    return(gaintable)
}

Attrition_decile <- lift(test_actual_attrition, test_pred, groups = 10)

#######################################################################
### Model Evaluation
### Train + Test Data ####
# Predicted probabilities of Attrition for training data
train_pred <- predict(final_model, type = "response", newdata = train)
summary(train_pred)
train$prob <- train_pred
train_test <- rbind(train, test)

train_test_actual_attrition <- factor(ifelse(train_test$Attrition == 1, "Yes", "No"))

# Let's repeat the cutoff value of 0.162
train_test_pred_attrition <- factor(ifelse(train_test$prob >= 0.162, "Yes", "No"))

table(train_test_actual_attrition, train_test_pred_attrition)

train_test_cutoff_attrition <- factor(ifelse(train_test$prob >=0.162, "Yes", "No"))
conf_final <- confusionMatrix(train_test_cutoff_attrition, train_test_actual_attrition, positive = "Yes")

acc <- conf_final$overall[1]
sens <- conf_final$byClass[1]
spec <- conf_final$byClass[2]
acc
sens
spec

########## KS -statistic - Train + Test Data ######
train_test_cutoff_attrition <- ifelse(train_test_cutoff_attrition == "Yes", 1, 0)
train_test_actual_attrition <- ifelse(train_test_actual_attrition == "Yes", 1, 0)

pred_object_train_test <- prediction(train_test_cutoff_attrition, train_test_actual_attrition)
performance_measures_train_test <- performance(pred_object_train_test, "tpr", "fpr")

ks_table_train_test <- attr(performance_measures_train_test, "y.values")[[1]] -
    (attr(performance_measures_train_test, "x.values")[[1]])

max(ks_table_train_test)

####################################################################
# Lift & Gain Chart - Train + Test Data
lift <- function(labels, predicted_prob, groups = 10) {
    if(is.factor(labels)) labels  <- as.integer(as.character(labels ))
    if(is.factor(predicted_prob)) predicted_prob <- as.integer(as.character(predicted_prob))
    helper <- data.frame(cbind(labels, predicted_prob))
    helper[,"bucket"] = ntile(-helper[,"predicted_prob"], groups)
    gaintable = helper %>% group_by(bucket)  %>%
        summarise_at(vars(labels ), funs(total = n(),
                                         totalresp=sum(., na.rm = TRUE))) %>%
        
        mutate(Cumresp = cumsum(totalresp),
               Gain=Cumresp/sum(totalresp)*100,
               Cumlift=Gain/(bucket*(100/groups))) 
    return(gaintable)
}

Attrition_decile <- lift(train_test_actual_attrition, train_test$prob, groups = 10)

# Plot the lift chart
plot(Attrition_decile$Cumlift, type="l", lwd=2, col="red",
     xlim = c(0,10),
     ylim = c(0,4),
     main = "Lift Chart",
     xlab = "Decile",
     ylab = "Lift")
abline(h=1, col="brown")
axis(1, 1:10)
abline(h=0:10, v=0:10, lty=3)

# Plot the gain chart
library(InformationValue)
ks_plot(train_test_actual_attrition, train_test_cutoff_attrition)
