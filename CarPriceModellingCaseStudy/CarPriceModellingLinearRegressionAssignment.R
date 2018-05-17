# Linear Regression Assignment
# Date: 25th Feb 2018
# Author: Santanu Dey (DDA1730043, Upgrad IIITB PGDDS Cohort 4)

# Load relevant libraries
library(ggplot2)
library(stringr)
library(car)
library(MASS)

# Read the data
cars <- read.csv("CarPrice_Assignment.csv", stringsAsFactors = F)

# View the data frame
View(cars)

# Summary and Structure of the data frame
summary(cars)
str(cars)

###### Data Preparation ######

# Specific Instruction for the assignment:
# There is a variable named CarName which is comprised of two parts - the first word is the name of 'car company' and the second is the 'car model'.
# For example, chevrolet impala has 'chevrolet' as the car company name and 'impala' as the car model name.
# You need to consider only company name as the independent variable for the model building.

# Split the CarName variable and store only the Company Name
cars$carCompany <- str_split_fixed(cars$CarName, " ", 2)[, 1]
# Remove the CarName column
cars <- cars[, -3]

# We find that few company names are spelled incorrectly or have different case
# Let's correct these car company names
unique(cars$carCompany)
cars$carCompany <- tolower(cars$carCompany)
cars$carCompany[which(cars$carCompany == "maxda")] <- "mazda"
cars$carCompany[which(cars$carCompany == "porcshce")] <- "porsche"
cars$carCompany[which(cars$carCompany == "toyouta")] <- "toyota"
cars$carCompany[which(cars$carCompany == "vokswagen")] <- "volkswagen"
cars$carCompany[which(cars$carCompany == "vw")] <- "volkswagen"

# Check for duplicates
sum(duplicated(cars))
# No duplicates

# Check for NAs
sum(is.na(cars))
# No NA values

# Convert all categorical variables to factor type
cars$symboling <- as.factor(cars$symboling)
cars$fueltype <- as.factor(cars$fueltype)
cars$aspiration <- as.factor(cars$aspiration)
cars$doornumber <- as.factor(cars$doornumber)
cars$carbody <- as.factor(cars$carbody)
cars$drivewheel <- as.factor(cars$drivewheel)
cars$enginelocation <- as.factor(cars$enginelocation)
cars$enginetype <- as.factor(cars$enginetype)
cars$cylindernumber <- as.factor(cars$cylindernumber)
cars$fuelsystem <- as.factor(cars$fuelsystem)
cars$carCompany <- as.factor(cars$carCompany)

str(cars)

# Treat outliers for continuous variables
# Wheelbase
quantile(cars$wheelbase, seq(0, 1, 0.01))
# There is a sudden jump from 99% to 100%. So, we set values greater than 99% to that for 99%
cars$wheelbase[which(cars$wheelbase > 115.544)] <- 115.544

# CarLength
quantile(cars$carlength, seq(0, 1, 0.01))
# There is a sudden jumps at each percentage from 0% to 3%. So, we set values less than 3% to that for 3%.
cars$carlength[which(cars$carlength < 155.900)] <- 155.900

# CarWidth
quantile(cars$carwidth, seq(0, 1, 0.01))
# There is a sudden jump from 0% to 1%. So, we set values less than 1% to that for 1%
cars$carwidth[which(cars$carwidth < 62.536)] <- 62.536

# CarHeight
quantile(cars$carheight, seq(0, 1, 0.01))
# No sudden jumps

# CurbWeight
quantile(cars$curbweight, seq(0, 1, 0.01))
# There is a sudden jump from 0% to 1%. So, we set values less than 1% to that for 1%
cars$curbweight[which(cars$curbweight < 1819.72)] <- 1819.72

# EngineSize
quantile(cars$enginesize, seq(0, 1, 0.01))
# There is a sudden jump from 96% upto 100%. So, we set values greater than 96% to that for 96%
cars$enginesize[which(cars$enginesize > 209.00)] <- 209.00

# BoreRatio
quantile(cars$boreratio, seq(0, 1, 0.01))
# There is a sudden jump from 0% to 1%. So, we set values less than 1% to that for 1%
cars$boreratio[which(cars$boreratio < 2.9100)] <- 2.9100

# Stroke
quantile(cars$stroke, seq(0, 1, 0.01))
# There is a sudden jump from 0% upto 2%. So, we set values less than 2% to that for 2%
cars$stroke[which(cars$stroke < 2.6400)] <- 2.6400
# There is also a sudden jump from 95% upto 100%. So, we set values greater than 95% to that for 95%
cars$stroke[which(cars$stroke > 3.6400)] <- 3.6400

# CompressionRatio
quantile(cars$compressionratio, seq(0, 1, 0.01))
# There is a sudden jump from 0% upto 4%. So, we set values less than 4% to that for 4%
cars$compressionratio[which(cars$compressionratio < 7.5000)] <- 7.5000
# There is also a sudden jump from 89% upto 100%. So, we set values greater than 89% to that for 89%
cars$compressionratio[which(cars$compressionratio > 10.0000)] <- 10.0000

# Horsepower
quantile(cars$horsepower, seq(0, 1, 0.01))
# There is a sudden jump from 97% to 100%. So, we set values greater than 97% to that for 97%
cars$horsepower[which(cars$horsepower > 184.00)] <- 184.00

# Peak RPM
quantile(cars$peakrpm, seq(0, 1, 0.01))
# There is a sudden jump from 99% to 100%. So, we set values greater than 99% to that for 99%
cars$peakrpm[which(cars$peakrpm > 6000)] <- 6000

# City MPG
quantile(cars$citympg, seq(0, 1, 0.01))
# There is a sudden jump from 98% to 100%. So, we set values greater than 98% to that for 98%
cars$citympg[which(cars$citympg > 38.00)] <- 38.00

# Highway MPG
quantile(cars$highwaympg, seq(0, 1, 0.01))
# There is a sudden jump from 98% to 100%. So, we set values greater than 98% to that for 98%
cars$highwaympg[which(cars$highwaympg > 46.92)] <- 46.92

# Dummy Variable Creation
# Two Levels
# Fuel Type: 0 for diesel, 1 for gas
levels(cars$fueltype) <- c(0, 1)
cars$fueltype <- as.numeric(levels(cars$fueltype))[cars$fueltype]

# Aspiration: 0 for std, 1 for turbo
levels(cars$aspiration) <- c(0, 1)
cars$aspiration <- as.numeric(levels(cars$aspiration))[cars$aspiration]

# Number of Doors: 0 for four, 1 for two
levels(cars$doornumber) <- c(0, 1)
cars$doornumber <- as.numeric(levels(cars$doornumber))[cars$doornumber]

# Engine Location: 0 for front, 1 for rear
levels(cars$enginelocation) <- c(0, 1)
cars$enginelocation <- as.numeric(levels(cars$enginelocation))[cars$enginelocation]

# More than two levels
# Drive Wheel
driveWheelDummy <- data.frame(model.matrix(~drivewheel, data = cars))
driveWheelDummy <- driveWheelDummy[ , -1]
cars_1 <- cbind(cars[ , -7], driveWheelDummy)

# Cylinder Number
cylinderNumberDummy <- data.frame(model.matrix(~cylindernumber, data = cars_1))
cylinderNumberDummy <- cylinderNumberDummy[ , -1]
cars_1 <- cbind(cars_1[ , -14], cylinderNumberDummy)

# Engine Type
engineTypeDummy <- data.frame(model.matrix(~enginetype, data = cars_1))
engineTypeDummy <- engineTypeDummy[ , -1]
cars_1 <- cbind(cars_1[ , -13], engineTypeDummy)

# Car Body
carBodyDummy <- data.frame(model.matrix(~carbody, data = cars_1))
carBodyDummy <- carBodyDummy[ , -1]
cars_1 <- cbind(cars_1[ , -6], carBodyDummy)

# Fuel System
fuelSystemDummy <- data.frame(model.matrix(~fuelsystem, data = cars_1))
fuelSystemDummy <- fuelSystemDummy[ , -1]
cars_1 <- cbind(cars_1[ , -13], fuelSystemDummy)

# Symboling
symbolingDummy <- data.frame(model.matrix(~symboling, data = cars_1))
symbolingDummy <- symbolingDummy[ , -1]
cars_1 <- cbind(cars_1[ , -2], symbolingDummy)

# Car Company
carCompanyDummy <- data.frame(model.matrix(~carCompany, data = cars_1))
carCompanyDummy <- carCompanyDummy[ , -1]
cars_1 <- cbind(cars_1[ , -20], carCompanyDummy)

# Remove Car_ID as it is not a significant variable
cars_1 <- cars_1[ , -1]

# Separate training (70%) and testing (30%) data
set.seed(100)
trainindices <- sample(1:nrow(cars_1), 0.7*nrow(cars_1))
train <- cars_1[trainindices, ]
test <- cars_1[-trainindices, ]

# Build model_1 containing all variables except price
model_1 <- lm(price~., data = train)
summary(model_1)

# Now we perform StepAIC on model_1
step <- stepAIC(model_1, direction = "both")
step

# Build model_2 based on the model derived using StepAIC
model_2 <- lm(formula = price ~ aspiration + enginelocation + carlength + 
                  carwidth + carheight + curbweight + enginesize + stroke + 
                  horsepower + drivewheelrwd + cylindernumberfive + cylindernumberfour + 
                  cylindernumbersix + cylindernumberthree + enginetypel + enginetypeohc + 
                  enginetypeohcf + carbodyhardtop + carbodyhatchback + carbodysedan + 
                  carbodywagon + symboling1 + symboling2 + carCompanybmw + 
                  carCompanybuick + carCompanydodge + carCompanyjaguar + carCompanymazda + 
                  carCompanymercury + carCompanymitsubishi + carCompanynissan + 
                  carCompanyplymouth + carCompanyrenault + carCompanysaab + 
                  carCompanytoyota + carCompanyvolkswagen, data = train)
summary(model_2)

vif(model_2)

# horsepower has a high VIF and its p-value is high as well
# Build model_3 after removing horsepower from model_2
model_3 <- lm(formula = price ~ aspiration + enginelocation + carlength + 
                  carwidth + carheight + curbweight + enginesize + stroke + 
                  drivewheelrwd + cylindernumberfive + cylindernumberfour + 
                  cylindernumbersix + cylindernumberthree + enginetypel + enginetypeohc + 
                  enginetypeohcf + carbodyhardtop + carbodyhatchback + carbodysedan + 
                  carbodywagon + symboling1 + symboling2 + carCompanybmw + 
                  carCompanybuick + carCompanydodge + carCompanyjaguar + carCompanymazda + 
                  carCompanymercury + carCompanymitsubishi + carCompanynissan + 
                  carCompanyplymouth + carCompanyrenault + carCompanysaab + 
                  carCompanytoyota + carCompanyvolkswagen, data = train)
summary(model_3)
# No major change in Adjusted R-squared
vif(model_3)

# All variables with high VIF have low p-values.
# So, we remove cylindernumberthree which has a high p-value
# Build model_4 after removing cylindernumberthree from model_3
model_4 <- lm(formula = price ~ aspiration + enginelocation + carlength + 
                  carwidth + carheight + curbweight + enginesize + stroke + 
                  drivewheelrwd + cylindernumberfive + cylindernumberfour + 
                  cylindernumbersix + enginetypel + enginetypeohc + 
                  enginetypeohcf + carbodyhardtop + carbodyhatchback + carbodysedan + 
                  carbodywagon + symboling1 + symboling2 + carCompanybmw + 
                  carCompanybuick + carCompanydodge + carCompanyjaguar + carCompanymazda + 
                  carCompanymercury + carCompanymitsubishi + carCompanynissan + 
                  carCompanyplymouth + carCompanyrenault + carCompanysaab + 
                  carCompanytoyota + carCompanyvolkswagen, data = train)
summary(model_4)
# No major change in Adjusted R-squared
vif(model_4)

# All variables with high VIF have low p-values.
# So, we remove carheight which has a high p-value
# Build model_5 after removing carheight from model_4
model_5 <- lm(formula = price ~ aspiration + enginelocation + carlength + 
                  carwidth + curbweight + enginesize + stroke + 
                  drivewheelrwd + cylindernumberfive + cylindernumberfour + 
                  cylindernumbersix + enginetypel + enginetypeohc + 
                  enginetypeohcf + carbodyhardtop + carbodyhatchback + carbodysedan + 
                  carbodywagon + symboling1 + symboling2 + carCompanybmw + 
                  carCompanybuick + carCompanydodge + carCompanyjaguar + carCompanymazda + 
                  carCompanymercury + carCompanymitsubishi + carCompanynissan + 
                  carCompanyplymouth + carCompanyrenault + carCompanysaab + 
                  carCompanytoyota + carCompanyvolkswagen, data = train)
summary(model_5)
# No major change in Adjusted R-squared
vif(model_5)

# All variables with high VIF have low p-values.
# So, we remove carbodyhardtop which has a high p-value
# Build model_6 after removing carbodyhardtop from model_5
model_6 <- lm(formula = price ~ aspiration + enginelocation + carlength + 
                  carwidth + curbweight + enginesize + stroke + 
                  drivewheelrwd + cylindernumberfive + cylindernumberfour + 
                  cylindernumbersix + enginetypel + enginetypeohc + 
                  enginetypeohcf + carbodyhatchback + carbodysedan + 
                  carbodywagon + symboling1 + symboling2 + carCompanybmw + 
                  carCompanybuick + carCompanydodge + carCompanyjaguar + carCompanymazda + 
                  carCompanymercury + carCompanymitsubishi + carCompanynissan + 
                  carCompanyplymouth + carCompanyrenault + carCompanysaab + 
                  carCompanytoyota + carCompanyvolkswagen, data = train)
summary(model_6)
# No major change in Adjusted R-squared
vif(model_6)

# All variables with high VIF have low p-values.
# So, we remove symboling2 which has a high p-value
# Build model_7 after removing symboling2 from model_6
model_7 <- lm(formula = price ~ aspiration + enginelocation + carlength + 
                  carwidth + curbweight + enginesize + stroke + 
                  drivewheelrwd + cylindernumberfive + cylindernumberfour + 
                  cylindernumbersix + enginetypel + enginetypeohc + 
                  enginetypeohcf + carbodyhatchback + carbodysedan + 
                  carbodywagon + symboling1 + carCompanybmw + 
                  carCompanybuick + carCompanydodge + carCompanyjaguar + carCompanymazda + 
                  carCompanymercury + carCompanymitsubishi + carCompanynissan + 
                  carCompanyplymouth + carCompanyrenault + carCompanysaab + 
                  carCompanytoyota + carCompanyvolkswagen, data = train)
summary(model_7)
# No major change in Adjusted R-squared
vif(model_7)

# All variables with high VIF have low p-values.
# So, we remove carCompanymercury which has a high p-value
# Build model_8 after removing carCompanymercury from model_7
model_8 <- lm(formula = price ~ aspiration + enginelocation + carlength + 
                  carwidth + curbweight + enginesize + stroke + 
                  drivewheelrwd + cylindernumberfive + cylindernumberfour + 
                  cylindernumbersix + enginetypel + enginetypeohc + 
                  enginetypeohcf + carbodyhatchback + carbodysedan + 
                  carbodywagon + symboling1 + carCompanybmw + 
                  carCompanybuick + carCompanydodge + carCompanyjaguar + carCompanymazda + 
                  carCompanymitsubishi + carCompanynissan + 
                  carCompanyplymouth + carCompanyrenault + carCompanysaab + 
                  carCompanytoyota + carCompanyvolkswagen, data = train)
summary(model_8)
# No major change in Adjusted R-squared
vif(model_8)

# All variables with high VIF have low p-values.
# So, we remove carlength which has a high p-value
# Build model_9 after removing carlength from model_8
model_9 <- lm(formula = price ~ aspiration + enginelocation + 
                  carwidth + curbweight + enginesize + stroke + 
                  drivewheelrwd + cylindernumberfive + cylindernumberfour + 
                  cylindernumbersix + enginetypel + enginetypeohc + 
                  enginetypeohcf + carbodyhatchback + carbodysedan + 
                  carbodywagon + symboling1 + carCompanybmw + 
                  carCompanybuick + carCompanydodge + carCompanyjaguar + carCompanymazda + 
                  carCompanymitsubishi + carCompanynissan + 
                  carCompanyplymouth + carCompanyrenault + carCompanysaab + 
                  carCompanytoyota + carCompanyvolkswagen, data = train)
summary(model_9)
# No major change in Adjusted R-squared
vif(model_9)

# All variables with high VIF have low p-values.
# So, we remove carbodysedan which has a high p-value
# Build model_10 after removing carbodysedan from model_9
model_10 <- lm(formula = price ~ aspiration + enginelocation + 
                  carwidth + curbweight + enginesize + stroke + 
                  drivewheelrwd + cylindernumberfive + cylindernumberfour + 
                  cylindernumbersix + enginetypel + enginetypeohc + 
                  enginetypeohcf + carbodyhatchback + 
                  carbodywagon + symboling1 + carCompanybmw + 
                  carCompanybuick + carCompanydodge + carCompanyjaguar + carCompanymazda + 
                  carCompanymitsubishi + carCompanynissan + 
                  carCompanyplymouth + carCompanyrenault + carCompanysaab + 
                  carCompanytoyota + carCompanyvolkswagen, data = train)
summary(model_10)
# No major change in Adjusted R-squared
vif(model_10)

# All variables with high VIF have low p-values.
# So, we remove carbodyhatchback which has a high p-value
# Build model_11 after removing carbodyhatchback from model_10
model_11 <- lm(formula = price ~ aspiration + enginelocation + 
                   carwidth + curbweight + enginesize + stroke + 
                   drivewheelrwd + cylindernumberfive + cylindernumberfour + 
                   cylindernumbersix + enginetypel + enginetypeohc + 
                   enginetypeohcf + carbodywagon + symboling1 + carCompanybmw + 
                   carCompanybuick + carCompanydodge + carCompanyjaguar + carCompanymazda +
                   carCompanymitsubishi + carCompanynissan + 
                   carCompanyplymouth + carCompanyrenault + carCompanysaab + 
                   carCompanytoyota + carCompanyvolkswagen, data = train)
summary(model_11)
# No major change in Adjusted R-squared
vif(model_11)

# All variables with high VIF have low p-values.
# So, we remove carbodywagon which has a high p-value
# Build model_12 after removing carbodywagon from model_11
model_12 <- lm(formula = price ~ aspiration + enginelocation + 
                   carwidth + curbweight + enginesize + stroke + 
                   drivewheelrwd + cylindernumberfive + cylindernumberfour + 
                   cylindernumbersix + enginetypel + enginetypeohc + 
                   enginetypeohcf + symboling1 + carCompanybmw + 
                   carCompanybuick + carCompanydodge + carCompanyjaguar + carCompanymazda +
                   carCompanymitsubishi + carCompanynissan + 
                   carCompanyplymouth + carCompanyrenault + carCompanysaab + 
                   carCompanytoyota + carCompanyvolkswagen, data = train)
summary(model_12)
# No major change in Adjusted R-squared
vif(model_12)

# All variables with high VIF have low p-values.
# So, we remove carCompanysaab which has a high p-value
# Build model_13 after removing carCompanysaab from model_12
model_13 <- lm(formula = price ~ aspiration + enginelocation + 
                   carwidth + curbweight + enginesize + stroke + 
                   drivewheelrwd + cylindernumberfive + cylindernumberfour + 
                   cylindernumbersix + enginetypel + enginetypeohc + 
                   enginetypeohcf + symboling1 + carCompanybmw + 
                   carCompanybuick + carCompanydodge + carCompanyjaguar + carCompanymazda +
                   carCompanymitsubishi + carCompanynissan + 
                   carCompanyplymouth + carCompanyrenault + 
                   carCompanytoyota + carCompanyvolkswagen, data = train)
summary(model_13)
# No major change in Adjusted R-squared
vif(model_13)

# All variables with high VIF have low p-values.
# So, we remove carCompanyvolkswagen which has a high p-value
# Build model_14 after removing carCompanyvolkswagen from model_13
model_14 <- lm(formula = price ~ aspiration + enginelocation + 
                   carwidth + curbweight + enginesize + stroke + 
                   drivewheelrwd + cylindernumberfive + cylindernumberfour + 
                   cylindernumbersix + enginetypel + enginetypeohc + 
                   enginetypeohcf + symboling1 + carCompanybmw + 
                   carCompanybuick + carCompanydodge + carCompanyjaguar + carCompanymazda +
                   carCompanymitsubishi + carCompanynissan + 
                   carCompanyplymouth + carCompanyrenault + 
                   carCompanytoyota, data = train)
summary(model_14)
# No major change in Adjusted R-squared
vif(model_14)

# All variables with high VIF have low p-values.
# So, we remove symboling1 which has a high p-value
# Build model_15 after removing symboling1 from model_14
model_15 <- lm(formula = price ~ aspiration + enginelocation + 
                   carwidth + curbweight + enginesize + stroke + 
                   drivewheelrwd + cylindernumberfive + cylindernumberfour + 
                   cylindernumbersix + enginetypel + enginetypeohc + 
                   enginetypeohcf + carCompanybmw + 
                   carCompanybuick + carCompanydodge + carCompanyjaguar + carCompanymazda +
                   carCompanymitsubishi + carCompanynissan + 
                   carCompanyplymouth + carCompanyrenault + 
                   carCompanytoyota, data = train)
summary(model_15)
# No major change in Adjusted R-squared
vif(model_15)

# All variables with high VIF have low p-values.
# So, we remove carCompanynissan which has a high p-value
# Build model_16 after removing carCompanynissan from model_15
model_16 <- lm(formula = price ~ aspiration + enginelocation + 
                   carwidth + curbweight + enginesize + stroke + 
                   drivewheelrwd + cylindernumberfive + cylindernumberfour + 
                   cylindernumbersix + enginetypel + enginetypeohc + 
                   enginetypeohcf + carCompanybmw + 
                   carCompanybuick + carCompanydodge + carCompanyjaguar + carCompanymazda +
                   carCompanymitsubishi + carCompanyplymouth + carCompanyrenault + 
                   carCompanytoyota, data = train)
summary(model_16)
# No major change in Adjusted R-squared
vif(model_16)

# All variables with high VIF have low p-values.
# So, we remove carCompanyplymouth which has a high p-value
# Build model_17 after removing carCompanyplymouth from model_16
model_17 <- lm(formula = price ~ aspiration + enginelocation + 
                   carwidth + curbweight + enginesize + stroke + 
                   drivewheelrwd + cylindernumberfive + cylindernumberfour + 
                   cylindernumbersix + enginetypel + enginetypeohc + 
                   enginetypeohcf + carCompanybmw + 
                   carCompanybuick + carCompanydodge + carCompanyjaguar + carCompanymazda +
                   carCompanymitsubishi + carCompanyrenault + 
                   carCompanytoyota, data = train)
summary(model_17)
# No major change in Adjusted R-squared
vif(model_17)

# All variables with high VIF have low p-values.
# So, we remove carCompanydodge which has a high p-value
# Build model_18 after removing carCompanydodge from model_17
model_18 <- lm(formula = price ~ aspiration + enginelocation + 
                   carwidth + curbweight + enginesize + stroke + 
                   drivewheelrwd + cylindernumberfive + cylindernumberfour + 
                   cylindernumbersix + enginetypel + enginetypeohc + 
                   enginetypeohcf + carCompanybmw + 
                   carCompanybuick + carCompanyjaguar + carCompanymazda +
                   carCompanymitsubishi + carCompanyrenault + 
                   carCompanytoyota, data = train)
summary(model_18)
# No major change in Adjusted R-squared
vif(model_18)

# All variables with high VIF have low p-values.
# So, we remove aspiration which has a high p-value
# Build model_19 after removing aspiration from model_18
model_19 <- lm(formula = price ~ enginelocation + 
                   carwidth + curbweight + enginesize + stroke + 
                   drivewheelrwd + cylindernumberfive + cylindernumberfour + 
                   cylindernumbersix + enginetypel + enginetypeohc + 
                   enginetypeohcf + carCompanybmw + 
                   carCompanybuick + carCompanyjaguar + carCompanymazda +
                   carCompanymitsubishi + carCompanyrenault + 
                   carCompanytoyota, data = train)
summary(model_19)
# No major change in Adjusted R-squared
vif(model_19)

# All variables with high VIF have low p-values.
# So, we remove drivewheelrwd which has a high p-value
# Build model_20 after removing drivewheelrwd from model_19
model_20 <- lm(formula = price ~ enginelocation + 
                   carwidth + curbweight + enginesize + stroke + 
                   cylindernumberfive + cylindernumberfour + 
                   cylindernumbersix + enginetypel + enginetypeohc + 
                   enginetypeohcf + carCompanybmw + 
                   carCompanybuick + carCompanyjaguar + carCompanymazda +
                   carCompanymitsubishi + carCompanyrenault + 
                   carCompanytoyota, data = train)
summary(model_20)
# No major change in Adjusted R-squared
vif(model_20)

# All variables with high VIF have low p-values.
# So, we remove carCompanymitsubishi which has a high p-value
# Build model_21 after removing carCompanymitsubishi from model_20
model_21 <- lm(formula = price ~ enginelocation + 
                   carwidth + curbweight + enginesize + stroke + 
                   cylindernumberfive + cylindernumberfour + 
                   cylindernumbersix + enginetypel + enginetypeohc + 
                   enginetypeohcf + carCompanybmw + 
                   carCompanybuick + carCompanyjaguar + carCompanymazda +
                   carCompanyrenault + carCompanytoyota, data = train)
summary(model_21)
# No major change in Adjusted R-squared
vif(model_21)

# All variables with high VIF have low p-values.
# So, we remove carCompanyrenault which has a high p-value
# Build model_22 after removing carCompanyrenault from model_21
model_22 <- lm(formula = price ~ enginelocation + 
                   carwidth + curbweight + enginesize + stroke + 
                   cylindernumberfive + cylindernumberfour + 
                   cylindernumbersix + enginetypel + enginetypeohc + 
                   enginetypeohcf + carCompanybmw + 
                   carCompanybuick + carCompanyjaguar + carCompanymazda +
                   carCompanytoyota, data = train)
summary(model_22)
# No major change in Adjusted R-squared
vif(model_22)

# All variables with high VIF have low p-values.
# So, we remove enginetypeohc which has a high p-value
# Build model_23 after removing enginetypeohc from model_22
model_23 <- lm(formula = price ~ enginelocation + carwidth + curbweight + enginesize +
                   stroke + cylindernumberfive + cylindernumberfour + 
                   cylindernumbersix + enginetypel + enginetypeohcf + carCompanybmw + 
                   carCompanybuick + carCompanyjaguar + carCompanymazda +
                   carCompanytoyota, data = train)
summary(model_23)
# No major change in Adjusted R-squared
vif(model_23)

# All variables with high VIF have low p-values.
# So, we remove enginesize which has a high p-value
# Build model_24 after removing enginesize from model_23
model_24 <- lm(formula = price ~ enginelocation + carwidth + curbweight +
                   stroke + cylindernumberfive + cylindernumberfour + 
                   cylindernumbersix + enginetypel + enginetypeohcf + carCompanybmw + 
                   carCompanybuick + carCompanyjaguar + carCompanymazda +
                   carCompanytoyota, data = train)
summary(model_24)
# No major change in Adjusted R-squared
vif(model_24)

# All variables with high VIF have low p-values.
# So, we remove carCompanytoyota which has a high p-value
# Build model_25 after removing carCompanytoyota from model_24
model_25 <- lm(formula = price ~ enginelocation + carwidth + curbweight +
                   stroke + cylindernumberfive + cylindernumberfour + 
                   cylindernumbersix + enginetypel + enginetypeohcf + carCompanybmw + 
                   carCompanybuick + carCompanyjaguar + carCompanymazda, data = train)
summary(model_25)
# No major change in Adjusted R-squared
vif(model_25)

# All variables with high VIF have low p-values.
# So, we remove stroke which has a high p-value
# Build model_26 after removing stroke from model_25
model_26 <- lm(formula = price ~ enginelocation + carwidth + curbweight +
                   cylindernumberfive + cylindernumberfour + 
                   cylindernumbersix + enginetypel + enginetypeohcf + carCompanybmw + 
                   carCompanybuick + carCompanyjaguar + carCompanymazda, data = train)
summary(model_26)
# No major change in Adjusted R-squared
vif(model_26)

# All variables with high VIF have low p-values.
# So, we remove enginetypeohcf which has a high p-value
# Build model_27 after removing enginetypeohcf from model_26
model_27 <- lm(formula = price ~ enginelocation + carwidth + curbweight +
                   cylindernumberfive + cylindernumberfour + 
                   cylindernumbersix + enginetypel + carCompanybmw + 
                   carCompanybuick + carCompanyjaguar + carCompanymazda, data = train)
summary(model_27)
# No major change in Adjusted R-squared
vif(model_27)

# All variables with high VIF have low p-values.
# So, we remove carCompanymazda which has a high p-value
# Build model_28 after removing carCompanymazda from model_27
model_28 <- lm(formula = price ~ enginelocation + carwidth + curbweight +
                   cylindernumberfive + cylindernumberfour + 
                   cylindernumbersix + enginetypel + carCompanybmw + 
                   carCompanybuick + carCompanyjaguar, data = train)
summary(model_28)
# No major change in Adjusted R-squared
vif(model_28)

# Now all variables have very low p-values. So, we can consider model_28 to be out final model
# Also, the R-squared (94.2%) and adjusted R-squared (93.76%) are very close, which suggests that we have a pretty good model.

# Predicting the results in test dataset
Predict_1 <- predict(model_28, test[,-1])
test$test_price <- Predict_1

# Now, we need to test the r square between actual and predicted sales. 
r <- cor(test$price, test$test_price)
rsquared <- cor(test$price,test$test_price)^2
rsquared
# 87% value of r-squared suggests that we have a pretty good model.

# Predicted price error
test$error <- test$price - test$test_price

# Plot errors
ggplot(test, aes(price, error)) + geom_point() + geom_hline(yintercept = 0)
# The error pattern is randomly distributed. This essentially confirms that there are no missing variables that can improve the model further.

# So, our final model is model_28, and the significant variables and their respective coefficients are as follows:
# Intercept = -7.302e+04
# Beta1 (enginelocation) = 2.126e+04
# Beta2 (carwidth) = 1.124e+03
# Beta3 (curbweight) = 6.236e+00
# Beta4 (cylindernumberfive) = -5.710e+03
# Beta5 (cylindernumberfour) = -5.052e+03
# Beta6 (cylindernumbersix) = -4.082e+03
# Beta7 (enginetypel) = -4.032e+03
# Beta8 (carCompanybmw) = 9.230e+03
# Beta9 (carCompanybuick) = 7.035e+03
# Beta10 (carCompanyjaguar) = 7.382e+03
