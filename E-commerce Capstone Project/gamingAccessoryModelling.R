####################################################################################
#                  :::::::: Ecommerce Capstone Solution ::::::::
####################################################################################
####################################################################################
#                  :::::::: Market Mix Modelling :::::::::::::::
####################################################################################
####################################################################################
#                  :::::::: Model Building - Gaming Accessory ::::::::
####################################################################################

CrossVal_Gaming <- rep(0,5)

####################################################################################
#                  :::::::: Basic Linear Model - Gaming Accessory ::::::::
####################################################################################

colnames(GamingAccessory_final)
# Removing week_year, Lag variables and derived price category variables
# Removing the following columns for the initial Basic Linear Model:
# week_year, Mass_p, Middle_p, Premium_p,
# list_price-1, list_price-2, list_price-3,
# promotional_offer-1, promotional_offer-2, promotional_offer-3
# NPS-1, NPS-2, NPS-3
# holiday_freq-1, holiday_freq-2, holiday_freq-3
GamingLinearData <- GamingAccessory_final[, -c(1, 10:12, 28:39)]

# Scaling
GamingLinearData <- data.frame(scale(GamingLinearData))

# Initial Linear Model
LinearModel_1 <- lm(gmv ~ ., GamingLinearData)
summary(LinearModel_1)
# Multiple R-squared:  0.8076,	Adjusted R-squared:  0.6509

LinearModel_2 <- stepAIC(LinearModel_1, direction = "both")
summary(LinearModel_2)
# Multiple R-squared:  0.7991,	Adjusted R-squared:  0.7017

LinearModel_3 <- lm(formula = gmv ~ list_price + MRP + Units + Procurement_SLA + 
                        per_order + NPS + TV + Digital + Sponsorship + 
                        ContentMarketing + SEM + inc_LP_MA2 + inc_LP_MA3 + 
                        inc_PO_MA1 + inc_PO_MA2 + inc_PO_MA3, 
                    data = GamingLinearData)
summary(LinearModel_3)
# Multiple R-squared:  0.7991,	Adjusted R-squared:  0.7017
vif(LinearModel_3)

# Removing TV (high p-value)
LinearModel_4 <- lm(formula = gmv ~ list_price + MRP + Units + Procurement_SLA + 
                        per_order + NPS + Digital + Sponsorship + 
                        ContentMarketing + SEM + inc_LP_MA2 + inc_LP_MA3 + 
                        inc_PO_MA1 + inc_PO_MA2 + inc_PO_MA3, 
                    data = GamingLinearData)
summary(LinearModel_4)
# Multiple R-squared:  0.7895,	Adjusted R-squared:  0.6966
vif(LinearModel_4)

# Removing list_price (high p-value)
LinearModel_5 <- lm(formula = gmv ~ MRP + Units + Procurement_SLA + 
                        per_order + NPS + Digital + Sponsorship + 
                        ContentMarketing + SEM + inc_LP_MA2 + inc_LP_MA3 + 
                        inc_PO_MA1 + inc_PO_MA2 + inc_PO_MA3, 
                    data = GamingLinearData)
summary(LinearModel_5)
# Multiple R-squared:  0.7835,	Adjusted R-squared:  0.6969
vif(LinearModel_5)

# Removing inc_PO_MA1 (high p-value)
LinearModel_6 <- lm(formula = gmv ~ MRP + Units + Procurement_SLA + 
                        per_order + NPS + Digital + Sponsorship + 
                        ContentMarketing + SEM + inc_LP_MA2 + inc_LP_MA3 + 
                        inc_PO_MA2 + inc_PO_MA3, 
                    data = GamingLinearData)
summary(LinearModel_6)
# Multiple R-squared:  0.7764,	Adjusted R-squared:  0.6957
vif(LinearModel_6)

# Removing Units (high p-value)
LinearModel_7 <- lm(formula = gmv ~ MRP + Procurement_SLA + 
                        per_order + NPS + Digital + Sponsorship + 
                        ContentMarketing + SEM + inc_LP_MA2 + inc_LP_MA3 + 
                        inc_PO_MA2 + inc_PO_MA3, 
                    data = GamingLinearData)
summary(LinearModel_7)
# Multiple R-squared:  0.746,	Adjusted R-squared:  0.6636
vif(LinearModel_7)

# Removing MRP (high p-value)
LinearModel_8 <- lm(formula = gmv ~ Procurement_SLA + 
                        per_order + NPS + Digital + Sponsorship + 
                        ContentMarketing + SEM + inc_LP_MA2 + inc_LP_MA3 + 
                        inc_PO_MA2 + inc_PO_MA3, 
                    data = GamingLinearData)
summary(LinearModel_8)
# Multiple R-squared:  0.7316,	Adjusted R-squared:  0.6539
vif(LinearModel_8)

# Removing inc_PO_MA3 (high collinearity and low significance)
LinearModel_9 <- lm(formula = gmv ~ Procurement_SLA + per_order + NPS + Digital +
                        Sponsorship + ContentMarketing + SEM + inc_LP_MA2 + 
                        inc_LP_MA3 + inc_PO_MA2, 
                    data = GamingLinearData)
summary(LinearModel_9)
# Multiple R-squared:  0.7042,	Adjusted R-squared:  0.6283
vif(LinearModel_9)

# Removing inc_LP_MA3 (high collinearity and low significance)
LinearModel_10 <- lm(formula = gmv ~ Procurement_SLA + per_order + NPS + Digital +
                         Sponsorship + ContentMarketing + SEM + inc_LP_MA2 + 
                         inc_PO_MA2, 
                     data = GamingLinearData)
summary(LinearModel_10)
# Multiple R-squared:  0.6906,	Adjusted R-squared:  0.6209
vif(LinearModel_10)

# Removing per_order (high p-value)
LinearModel_11 <- lm(formula = gmv ~ Procurement_SLA + NPS + Digital + Sponsorship +
                         ContentMarketing + SEM + inc_LP_MA2 + inc_PO_MA2, 
                     data = GamingLinearData)
summary(LinearModel_11)
# Multiple R-squared:  0.6458,	Adjusted R-squared:  0.5767
vif(LinearModel_11)

# Removing ContentMarketing (high collinearity and low significance)
LinearModel_12 <- lm(formula = gmv ~ Procurement_SLA + NPS + Digital + Sponsorship +
                         SEM + inc_LP_MA2 + inc_PO_MA2, 
                     data = GamingLinearData)
summary(LinearModel_12)
# Multiple R-squared:  0.6006,	Adjusted R-squared:  0.534
vif(LinearModel_12)

# Removing Digital (high collinearity and low significance)
LinearModel_13 <- lm(formula = gmv ~ Procurement_SLA + NPS + Sponsorship +
                         SEM + inc_LP_MA2 + inc_PO_MA2, 
                     data = GamingLinearData)
summary(LinearModel_13)
# Multiple R-squared:  0.5685,	Adjusted R-squared:  0.5083
vif(LinearModel_13)

# Removing Sponsorship (high p-value)
LinearModel_14 <- lm(formula = gmv ~ Procurement_SLA + NPS +
                         SEM + inc_LP_MA2 + inc_PO_MA2, 
                     data = GamingLinearData)
summary(LinearModel_14)
# Multiple R-squared:  0.5341,	Adjusted R-squared:  0.4811
vif(LinearModel_14)

# Removing SEM (high p-value)
LinearModel_15 <- lm(formula = gmv ~ Procurement_SLA + NPS + inc_LP_MA2 + inc_PO_MA2, 
                     data = GamingLinearData)
summary(LinearModel_15)
# Multiple R-squared:  0.4954,	Adjusted R-squared:  0.4506
vif(LinearModel_15)

# Removing Procurement_SLA (high p-value)
LinearModel_16 <- lm(formula = gmv ~ NPS + inc_LP_MA2 + inc_PO_MA2, 
                     data = GamingLinearData)
summary(LinearModel_16)
# Multiple R-squared:  0.4222,	Adjusted R-squared:  0.3846
vif(LinearModel_16)

# Removing inc_LP_MA2 (high p-value)
LinearModel_17 <- lm(formula = gmv ~ NPS + inc_PO_MA2, 
                     data = GamingLinearData)
summary(LinearModel_17)
# Multiple R-squared:  0.3106,	Adjusted R-squared:  0.2813
vif(LinearModel_17)
# R-squared reduces considerably, hence, inc_LP_MA2 variable cannot be removed
# Rest of the variables in LinearModel_16 cannot be removed.
# Hence, LinearModel_16 is the final model
GamingLinearModel <- LinearModel_16
# 3 variables: NPS, inc_LP_MA2, inc_PO_MA2
# Multiple R-squared:  0.4222,	Adjusted R-squared:  0.3846

######################################################################################
# Cross Validation
# Calculate K-fold cross-validation, for K=10.
crossval <- cv.lm(data = GamingLinearData, 
                  form.lm = formula(gmv ~ NPS + inc_LP_MA2 + inc_PO_MA2), m = 10)

CrossVal_Gaming[1] <- attr(crossval, "ms")

# Gaming Accessories Linear Model Elasticity Analysis
trainingSet <- GamingLinearData

# Gaming Accessories Linear Model Elasticity coefficients
getGamingElasticity <- function(var){
    val <-as.numeric(GamingLinearModel$coefficients[var] * 
                         mean(trainingSet[, var]) / mean(trainingSet$gmv))
    return(val)
}

gamingVarList <- list()

for(i in 2:length(GamingLinearModel$coefficients)){
    gamingVarList[i-1] <- getGamingElasticity(names(GamingLinearModel$coefficients)[i])
}

gamingLinearModelElasticity <- data.frame(names(
    GamingLinearModel$coefficients[2:length(GamingLinearModel$coefficients)]))

gamingLinearModelElasticity <- cbind(gamingLinearModelElasticity, 
                                     do.call(rbind.data.frame, gamingVarList))

colnames(gamingLinearModelElasticity) <- c("Variable", "Elasticity")

gamingLinearModelElasticity$direction <- ifelse(
    gamingLinearModelElasticity$Elasticity > 0, "Positive", "Negative")

ggplot(data = gamingLinearModelElasticity, 
       aes(x = reorder(Variable, Elasticity), y = Elasticity)) +
    geom_bar(position = "dodge", stat = "identity") + 
    coord_flip() +
    ggtitle("Gaming Accessories - Linear Model") + xlab("Variables")

#####################################################################################

####################################################################################
#           :::::::: Multiplicative Model - Gaming Accessory ::::::::
####################################################################################

colnames(GamingAccessory_final)
# Removing Moving Averages, Lag and derived price category variables
# Removing the following columns for the initial Multiplicative Model:
# week_year, Mass_p, Middle_p, Premium_p, holiday_freq,
# inc_LP_MA1, inc_LP_MA2, inc_LP_MA3,
# inc_PO_MA1, inc_PO_MA2, inc_PO_MA3
# list_price-1, list_price-2, list_price-3,
# promotional_offer-1, promotional_offer-2, promotional_offer-3
# NPS-1, NPS-2, NPS-3
# holiday_freq-1, holiday_freq-2, holiday_freq-3
GamingMultiData <- GamingAccessory_final[, -c(1, 10:13, 22:39)]

# Treatment for Multiplicative Modelling
GamingMultiData$ContentMarketing[which(GamingMultiData$ContentMarketing==0)] <- 0.01
GamingMultiData$per_order[which(GamingMultiData$per_order==0)] <- 0.01
GamingMultiData <- log(GamingMultiData)

# Build the initial Multiplicative Model
multiModel_1 <- lm(gmv ~ ., GamingMultiData)
summary(multiModel_1)
# Multiple R-squared:  0.87,	Adjusted R-squared:  0.813

multiModel_2 <- stepAIC(multiModel_1, direction = "both")
summary(multiModel_2)
# Multiple R-squared:  0.862,	Adjusted R-squared:  0.835

multiModel_3 <- lm(formula = gmv ~ SLA + per_order + NPS + TV + Sponsorship + 
                       ContentMarketing + OnlineMarketing + Affiliates, 
                   data = GamingMultiData)
summary(multiModel_3)
# Multiple R-squared:  0.862,	Adjusted R-squared:  0.835
vif(multiModel_3)

# Removing NPS (high p-value)
multiModel_4 <- lm(formula = gmv ~ SLA + per_order + TV + Sponsorship + 
                       ContentMarketing + OnlineMarketing + Affiliates, 
                   data = GamingMultiData)
summary(multiModel_4)
# Multiple R-squared:  0.855,	Adjusted R-squared:  0.831
vif(multiModel_4)

# Removing per_order (high p-value)
multiModel_5 <- lm(formula = gmv ~ SLA + TV + Sponsorship + 
                       ContentMarketing + OnlineMarketing + Affiliates, 
                   data = GamingMultiData)
summary(multiModel_5)
# Multiple R-squared:  0.848,	Adjusted R-squared:  0.827
vif(multiModel_5)

# Removing Sponsorship (high p-value)
multiModel_6 <- lm(formula = gmv ~ SLA + TV + ContentMarketing + 
                       OnlineMarketing + Affiliates, 
                   data = GamingMultiData)
summary(multiModel_6)
# Multiple R-squared:  0.827,	Adjusted R-squared:  0.807
vif(multiModel_6)

# Removing TV (high multicollinearity and low significance)
multiModel_7 <- lm(formula = gmv ~ SLA + ContentMarketing + 
                       OnlineMarketing + Affiliates, 
                   data = GamingMultiData)
summary(multiModel_7)
# Multiple R-squared:  0.818,	Adjusted R-squared:  0.802
vif(multiModel_7)

# Removing SLA (high p-value)
multiModel_8 <- lm(formula = gmv ~ ContentMarketing + OnlineMarketing + Affiliates, 
                   data = GamingMultiData)
summary(multiModel_8)
# Multiple R-squared:  0.804,	Adjusted R-squared:  0.791
vif(multiModel_8)

# No more variables can be removed from multiModel_8, hence, this is the final model
GamingMultiModel <- multiModel_8
# 3 variables: ContentMarketing, OnlineMarketing, Affiliates
# Multiple R-squared:  0.804,	Adjusted R-squared:  0.791

######################################################################################
# Cross Validation
# Calculate K-fold cross-validation, for K=10.
crossval <- cv.lm(data = GamingMultiData, 
                  form.lm = formula(gmv ~ ContentMarketing + OnlineMarketing + Affiliates), m = 10)
CrossVal_Gaming[2] <- attr(crossval, "ms")

# Gaming Accessories Multiplicative Model Elasticity Analysis
trainingSet <- GamingMultiData

# Gaming Accessories Multiplicative Model Elasticity coefficients
getGamingMultiElasticity <- function(var){
    val <-as.numeric(GamingMultiModel$coefficients[var] * 
                         mean(trainingSet[, var]) / mean(trainingSet$gmv))
    return(val)
} 

gamingVarList <- list()

for(i in 2:length(GamingMultiModel$coefficients)){
    gamingVarList[i-1] <- getGamingMultiElasticity(names(GamingMultiModel$coefficients)[i])
}

gamingMultiModelElasticity <- data.frame(names(
    GamingMultiModel$coefficients[2:length(GamingMultiModel$coefficients)]))

gamingMultiModelElasticity <- cbind(gamingMultiModelElasticity, 
                                    do.call(rbind.data.frame, gamingVarList))

colnames(gamingMultiModelElasticity) <- c("Variable", "Elasticity")

gamingMultiModelElasticity$direction <- ifelse(
    gamingMultiModelElasticity$Elasticity > 0, "Positive", "Negative")

ggplot(data = gamingMultiModelElasticity,
       aes(x = reorder(Variable, Elasticity), y = Elasticity)) +
    geom_bar(position = "dodge", stat = "identity") + 
    coord_flip() +
    ggtitle("Gaming Accessories - Multiplicative Model") + xlab("Variables")

#####################################################################################

####################################################################################
#                  :::::::: Koyck Model - Gaming Accessory ::::::::
####################################################################################

colnames(GamingAccessory_final)
# Removing week_year, Lag variables and derived price category variables
# Removing the following columns for the initial Koyck Model:
# week_year, Mass_p, Middle_p, Premium_p,
# list_price-1, list_price-2, list_price-3,
# promotional_offer-1, promotional_offer-2, promotional_offer-3
# NPS-1, NPS-2, NPS-3
# holiday_freq-1, holiday_freq-2, holiday_freq-3
GamingKoyckData <- GamingAccessory_final[, -c(1, 10:12, 28:39)]

# Lag GMV by 1 Unit
GamingKoyckData <- slide(GamingKoyckData, Var = "gmv", slideBy = -1)

# Remove NAs and perform scaling
GamingKoyckData <- data.frame(scale(na.omit(GamingKoyckData)))

# Build initial Koyck model
koyckModel_1 <- lm(gmv ~ ., GamingKoyckData)
summary(koyckModel_1)
# Multiple R-squared:  0.81,	Adjusted R-squared:  0.636

koyckModel_2 <- stepAIC(koyckModel_1, direction = "both")
summary(koyckModel_2)
# Multiple R-squared:  0.77,	Adjusted R-squared:  0.694

koyckModel_3 <- lm(formula = gmv ~ MRP + Procurement_SLA + per_order + NPS + 
                       Digital + Sponsorship + ContentMarketing + SEM + inc_LP_MA2 + 
                       inc_LP_MA3 + inc_PO_MA2 + inc_PO_MA3, 
                   data = GamingKoyckData)
summary(koyckModel_3)
# Multiple R-squared:  0.77,	Adjusted R-squared:  0.694
vif(koyckModel_3)

# Removing MRP (high p-value)
koyckModel_4 <- lm(formula = gmv ~ Procurement_SLA + per_order + NPS + 
                       Digital + Sponsorship + ContentMarketing + SEM + inc_LP_MA2 + 
                       inc_LP_MA3 + inc_PO_MA2 + inc_PO_MA3, 
                   data = GamingKoyckData)
summary(koyckModel_4)
# Multiple R-squared:  0.751,	Adjusted R-squared:  0.677
vif(koyckModel_4)

# Removing per_order (high p-value)
koyckModel_5 <- lm(formula = gmv ~ Procurement_SLA + NPS + 
                       Digital + Sponsorship + ContentMarketing + SEM + inc_LP_MA2 + 
                       inc_LP_MA3 + inc_PO_MA2 + inc_PO_MA3, 
                   data = GamingKoyckData)
summary(koyckModel_5)
# Multiple R-squared:  0.711,	Adjusted R-squared:  0.635
vif(koyckModel_5)

# Removing Digital (high multicollinearity and low significance)
koyckModel_6 <- lm(formula = gmv ~ Procurement_SLA + NPS + Sponsorship + 
                       ContentMarketing + SEM + inc_LP_MA2 + inc_LP_MA3 + 
                       inc_PO_MA2 + inc_PO_MA3, 
                   data = GamingKoyckData)
summary(koyckModel_6)
# Multiple R-squared:  0.685,	Adjusted R-squared:  0.612
vif(koyckModel_6)

# Removing NPS (high p-value)
koyckModel_7 <- lm(formula = gmv ~ Procurement_SLA + Sponsorship + 
                       ContentMarketing + SEM + inc_LP_MA2 + inc_LP_MA3 + 
                       inc_PO_MA2 + inc_PO_MA3, 
                   data = GamingKoyckData)
summary(koyckModel_7)
# Multiple R-squared:  0.678,	Adjusted R-squared:  0.614
vif(koyckModel_7)

# Removing inc_LP_MA3 (high multicollinearity and low significance)
koyckModel_8 <- lm(formula = gmv ~ Procurement_SLA + Sponsorship + 
                       ContentMarketing + SEM + inc_LP_MA2 + 
                       inc_PO_MA2 + inc_PO_MA3, 
                   data = GamingKoyckData)
summary(koyckModel_8)
# Multiple R-squared:  0.601,	Adjusted R-squared:  0.532
vif(koyckModel_8)

# Removing inc_PO_MA3 (high multicollinearity and low significance)
koyckModel_9 <- lm(formula = gmv ~ Procurement_SLA + Sponsorship + 
                       ContentMarketing + SEM + inc_LP_MA2 + inc_PO_MA2, 
                   data = GamingKoyckData)
summary(koyckModel_9)
# Multiple R-squared:  0.569,	Adjusted R-squared:  0.508
vif(koyckModel_9)

# Removing Procurement_SLA (high p-value)
koyckModel_10 <- lm(formula = gmv ~ Sponsorship + ContentMarketing + SEM + 
                        inc_LP_MA2 + inc_PO_MA2, 
                    data = GamingKoyckData)
summary(koyckModel_10)
# Multiple R-squared:  0.475,	Adjusted R-squared:  0.414
vif(koyckModel_10)
# Adjusted R-squared reduces considerably, hence, Procurement_SLA cannot be removed
# Rest of the variables in koyckModel_9 cannot be removed.
# Hence, koyckModel_9 can be considered to be the final model

GamingKoyckModel <- koyckModel_9
# Multiple R-squared:  0.569,	Adjusted R-squared:  0.508
# 6 variables: Procurement_SLA, Sponsorship, ContentMarketing, SEM, inc_LP_MA2 and inc_PO_MA2

######################################################################################
# Cross Validation
# Calculate K-fold cross-validation, for K=10.
crossval <- cv.lm(data = GamingKoyckData, 
                  form.lm = formula(gmv ~ Procurement_SLA + Sponsorship + ContentMarketing + SEM + inc_LP_MA2 + inc_PO_MA2), m = 10)

CrossVal_Gaming[3] <- attr(crossval, "ms")

# Gaming Accessories Koyck Model Elasticity Analysis
trainingSet <- GamingKoyckData

# Gaming Accessories Koyck Model Elasticity coefficients
getGamingKoyckElasticity <- function(var){
    val <-as.numeric(GamingKoyckModel$coefficients[var] * 
                         mean(trainingSet[, var]) / mean(trainingSet$gmv))
    return(val)
} 

gamingVarList <- list()

for(i in 2:length(GamingKoyckModel$coefficients)){
    gamingVarList[i-1] <- getGamingKoyckElasticity(names(GamingKoyckModel$coefficients)[i])
}

gamingKoyckModelElasticity <- data.frame(names(
    GamingKoyckModel$coefficients[2:length(GamingKoyckModel$coefficients)]))

gamingKoyckModelElasticity <- cbind(gamingKoyckModelElasticity, 
                                    do.call(rbind.data.frame, gamingVarList))

colnames(gamingKoyckModelElasticity) <- c("Variable", "Elasticity")

gamingKoyckModelElasticity$direction <- ifelse(
    gamingKoyckModelElasticity$Elasticity > 0, "Positive", "Negative")

ggplot(data = gamingKoyckModelElasticity,
       aes(x = reorder(Variable, Elasticity), y = Elasticity)) +
    geom_bar(position = "dodge", stat = "identity") + 
    coord_flip() +
    ggtitle("Gaming Accessories - Koyck Model") + xlab("Variables")

#####################################################################################

####################################################################################
#       :::::::: Distributed Lag Model - Gaming Accessory ::::::::
####################################################################################

colnames(GamingAccessory_final)
# Removing week_year and derived price category variables
# Removing the following columns for the initial Distributed Lag Model:
# week_year, Mass_p, Middle_p, Premium_p,
GamingDistLagData <- GamingAccessory_final[, -c(1, 10:12)]

# Lag GMV by 1 Time Unit
GamingDistLagData_1 <- slide(GamingDistLagData, Var = "gmv", slideBy = -1)

# Lag GMV by 2 Time Units
GamingDistLagData_1 <- slide(GamingDistLagData_1, Var = "gmv", slideBy = -2)

# Lag GMV by 3 Time Units
GamingDistLagData_1 <- slide(GamingDistLagData_1, Var = "gmv", slideBy = -3)

# Remove NAs and Scaling
GamingDistLagData <- data.frame(scale(na.omit(GamingDistLagData_1)))

# Build the initial Distributed Lag Model
DistLagModel_1 <- lm(gmv ~ ., GamingDistLagData)
summary(DistLagModel_1)
# Multiple R-squared:  0.963,	Adjusted R-squared:  0.812

DistLagModel_2 <- stepAIC(DistLagModel_1, direction = "both")
summary(DistLagModel_2)
# Multiple R-squared:  0.962,	Adjusted R-squared:  0.898

DistLagModel_3 <- lm(formula = gmv ~ list_price + MRP + Units + SLA + 
                         promotional_offer + Procurement_SLA + per_order + NPS + TV +
                         Sponsorship + ContentMarketing + OnlineMarketing + 
                         Affiliates + SEM + inc_LP_MA2 + inc_LP_MA3 + inc_PO_MA1 +
                         inc_PO_MA3 + list_price.1 + list_price.2 + 
                         promotional_offer.1 + promotional_offer.2 + 
                         promotional_offer.3 + NPS.2 + NPS.3 + holiday_freq.1 +
                         holiday_freq.3 + gmv.1 + gmv.3, 
                     data = GamingDistLagData)
summary(DistLagModel_3)
# Multiple R-squared:  0.962,	Adjusted R-squared:  0.898
vif(DistLagModel_3)

# Removing Affiliates (high multicollinearity and low significance)
DistLagModel_4 <- lm(formula = gmv ~ list_price + MRP + Units + SLA + 
                         promotional_offer + Procurement_SLA + per_order + NPS + TV +
                         Sponsorship + ContentMarketing + OnlineMarketing + 
                         SEM + inc_LP_MA2 + inc_LP_MA3 + inc_PO_MA1 +
                         inc_PO_MA3 + list_price.1 + list_price.2 + 
                         promotional_offer.1 + promotional_offer.2 + 
                         promotional_offer.3 + NPS.2 + NPS.3 + holiday_freq.1 +
                         holiday_freq.3 + gmv.1 + gmv.3, 
                     data = GamingDistLagData)
summary(DistLagModel_4)
# Multiple R-squared:  0.955,	Adjusted R-squared:  0.886
vif(DistLagModel_4)

# Removing promotional_offer (high multicollinearity and low significance)
DistLagModel_5 <- lm(formula = gmv ~ list_price + MRP + Units + SLA + 
                         Procurement_SLA + per_order + NPS + TV +
                         Sponsorship + ContentMarketing + OnlineMarketing + 
                         SEM + inc_LP_MA2 + inc_LP_MA3 + inc_PO_MA1 +
                         inc_PO_MA3 + list_price.1 + list_price.2 + 
                         promotional_offer.1 + promotional_offer.2 + 
                         promotional_offer.3 + NPS.2 + NPS.3 + holiday_freq.1 +
                         holiday_freq.3 + gmv.1 + gmv.3, 
                     data = GamingDistLagData)
summary(DistLagModel_5)
# Multiple R-squared:  0.948,	Adjusted R-squared:  0.874
vif(DistLagModel_5)

# Removing inc_LP_MA2 (high multicollinearity and low significance)
DistLagModel_6 <- lm(formula = gmv ~ list_price + MRP + Units + SLA + 
                         Procurement_SLA + per_order + NPS + TV +
                         Sponsorship + ContentMarketing + OnlineMarketing + 
                         SEM + inc_LP_MA3 + inc_PO_MA1 +
                         inc_PO_MA3 + list_price.1 + list_price.2 + 
                         promotional_offer.1 + promotional_offer.2 + 
                         promotional_offer.3 + NPS.2 + NPS.3 + holiday_freq.1 +
                         holiday_freq.3 + gmv.1 + gmv.3, 
                     data = GamingDistLagData)
summary(DistLagModel_6)
# Multiple R-squared:  0.944,	Adjusted R-squared:  0.871
vif(DistLagModel_6)

# Removing inc_LP_MA3 (high multicollinearity and low significance)
DistLagModel_7 <- lm(formula = gmv ~ list_price + MRP + Units + SLA + 
                         Procurement_SLA + per_order + NPS + TV +
                         Sponsorship + ContentMarketing + OnlineMarketing + 
                         SEM + inc_PO_MA1 + inc_PO_MA3 + list_price.1 + list_price.2 + 
                         promotional_offer.1 + promotional_offer.2 + 
                         promotional_offer.3 + NPS.2 + NPS.3 + holiday_freq.1 +
                         holiday_freq.3 + gmv.1 + gmv.3, 
                     data = GamingDistLagData)
summary(DistLagModel_7)
# Multiple R-squared:  0.93,	Adjusted R-squared:  0.848
vif(DistLagModel_7)

# Removing TV (high p-value)
DistLagModel_8 <- lm(formula = gmv ~ list_price + MRP + Units + SLA + 
                         Procurement_SLA + per_order + NPS +
                         Sponsorship + ContentMarketing + OnlineMarketing + 
                         SEM + inc_PO_MA1 + inc_PO_MA3 + list_price.1 + list_price.2 + 
                         promotional_offer.1 + promotional_offer.2 + 
                         promotional_offer.3 + NPS.2 + NPS.3 + holiday_freq.1 +
                         holiday_freq.3 + gmv.1 + gmv.3, 
                     data = GamingDistLagData)
summary(DistLagModel_8)
# Multiple R-squared:  0.93,	Adjusted R-squared:  0.854
vif(DistLagModel_8)

# Removing NPS.2 (high p-value)
DistLagModel_9 <- lm(formula = gmv ~ list_price + MRP + Units + SLA + 
                         Procurement_SLA + per_order + NPS +
                         Sponsorship + ContentMarketing + OnlineMarketing + 
                         SEM + inc_PO_MA1 + inc_PO_MA3 + list_price.1 + list_price.2 + 
                         promotional_offer.1 + promotional_offer.2 + 
                         promotional_offer.3 + NPS.3 + holiday_freq.1 +
                         holiday_freq.3 + gmv.1 + gmv.3, 
                     data = GamingDistLagData)
summary(DistLagModel_9)
# Multiple R-squared:  0.93,	Adjusted R-squared:  0.859
vif(DistLagModel_9)

# Removing list_price (high p-value)
DistLagModel_10 <- lm(formula = gmv ~ MRP + Units + SLA + 
                          Procurement_SLA + per_order + NPS +
                          Sponsorship + ContentMarketing + OnlineMarketing + 
                          SEM + inc_PO_MA1 + inc_PO_MA3 + list_price.1 + 
                          list_price.2 + promotional_offer.1 + promotional_offer.2 + 
                          promotional_offer.3 + NPS.3 + holiday_freq.1 +
                          holiday_freq.3 + gmv.1 + gmv.3, 
                      data = GamingDistLagData)
summary(DistLagModel_10)
# Multiple R-squared:  0.928,	Adjusted R-squared:  0.862
vif(DistLagModel_10)

# Removing Sponsorship (high p-value)
DistLagModel_11 <- lm(formula = gmv ~ MRP + Units + SLA + 
                          Procurement_SLA + per_order + NPS +
                          ContentMarketing + OnlineMarketing + 
                          SEM + inc_PO_MA1 + inc_PO_MA3 + list_price.1 + 
                          list_price.2 + promotional_offer.1 + promotional_offer.2 + 
                          promotional_offer.3 + NPS.3 + holiday_freq.1 +
                          holiday_freq.3 + gmv.1 + gmv.3, 
                      data = GamingDistLagData)
summary(DistLagModel_11)
# Multiple R-squared:  0.925,	Adjusted R-squared:  0.863
vif(DistLagModel_11)

# Removing NPS (high p-value)
DistLagModel_12 <- lm(formula = gmv ~ MRP + Units + SLA + 
                          Procurement_SLA + per_order +
                          ContentMarketing + OnlineMarketing + 
                          SEM + inc_PO_MA1 + inc_PO_MA3 + list_price.1 + 
                          list_price.2 + promotional_offer.1 + promotional_offer.2 + 
                          promotional_offer.3 + NPS.3 + holiday_freq.1 +
                          holiday_freq.3 + gmv.1 + gmv.3, 
                      data = GamingDistLagData)
summary(DistLagModel_12)
# Multiple R-squared:  0.911,	Adjusted R-squared:  0.843
vif(DistLagModel_12)

# Removing OnlineMarketing (high p-value)
DistLagModel_13 <- lm(formula = gmv ~ MRP + Units + SLA + 
                          Procurement_SLA + per_order +
                          ContentMarketing + SEM + inc_PO_MA1 + inc_PO_MA3 + 
                          list_price.1 + list_price.2 + promotional_offer.1 + 
                          promotional_offer.2 + promotional_offer.3 + NPS.3 + 
                          holiday_freq.1 + holiday_freq.3 + gmv.1 + gmv.3, 
                      data = GamingDistLagData)
summary(DistLagModel_13)
# Multiple R-squared:  0.903,	Adjusted R-squared:  0.835
vif(DistLagModel_13)

# Removing Units (high p-value)
DistLagModel_14 <- lm(formula = gmv ~ MRP + SLA + Procurement_SLA + per_order +
                          ContentMarketing + SEM + inc_PO_MA1 + inc_PO_MA3 + 
                          list_price.1 + list_price.2 + promotional_offer.1 + 
                          promotional_offer.2 + promotional_offer.3 + NPS.3 + 
                          holiday_freq.1 + holiday_freq.3 + gmv.1 + gmv.3, 
                      data = GamingDistLagData)
summary(DistLagModel_14)
# Multiple R-squared:  0.875,	Adjusted R-squared:  0.795
vif(DistLagModel_14)

# Removing holiday_freq.3 (high p-value)
DistLagModel_15 <- lm(formula = gmv ~ MRP + SLA + Procurement_SLA + per_order +
                          ContentMarketing + SEM + inc_PO_MA1 + inc_PO_MA3 + 
                          list_price.1 + list_price.2 + promotional_offer.1 + 
                          promotional_offer.2 + promotional_offer.3 + NPS.3 + 
                          holiday_freq.1 + gmv.1 + gmv.3, 
                      data = GamingDistLagData)
summary(DistLagModel_15)
# Multiple R-squared:  0.844,	Adjusted R-squared:  0.752
vif(DistLagModel_15)

# Removing gmv.3 (high p-value)
DistLagModel_16 <- lm(formula = gmv ~ MRP + SLA + Procurement_SLA + per_order +
                          ContentMarketing + SEM + inc_PO_MA1 + inc_PO_MA3 + 
                          list_price.1 + list_price.2 + promotional_offer.1 + 
                          promotional_offer.2 + promotional_offer.3 + NPS.3 + 
                          holiday_freq.1 + gmv.1, 
                      data = GamingDistLagData)
summary(DistLagModel_16)
# Multiple R-squared:  0.815,	Adjusted R-squared:  0.717
vif(DistLagModel_16)

# Removing SLA (high p-value)
DistLagModel_17 <- lm(formula = gmv ~ MRP + Procurement_SLA + per_order +
                          ContentMarketing + SEM + inc_PO_MA1 + inc_PO_MA3 + 
                          list_price.1 + list_price.2 + promotional_offer.1 + 
                          promotional_offer.2 + promotional_offer.3 + NPS.3 + 
                          holiday_freq.1 + gmv.1, 
                      data = GamingDistLagData)
summary(DistLagModel_17)
# Multiple R-squared:  0.781,	Adjusted R-squared:  0.675
vif(DistLagModel_17)

# Removing NPS.3 (high p-value)
DistLagModel_18 <- lm(formula = gmv ~ MRP + Procurement_SLA + per_order +
                          ContentMarketing + SEM + inc_PO_MA1 + inc_PO_MA3 + 
                          list_price.1 + list_price.2 + promotional_offer.1 + 
                          promotional_offer.2 + promotional_offer.3 + 
                          holiday_freq.1 + gmv.1, 
                      data = GamingDistLagData)
summary(DistLagModel_18)
# Multiple R-squared:  0.737,	Adjusted R-squared:  0.622
vif(DistLagModel_18)

# Removing ContentMarketing (high p-value)
DistLagModel_19 <- lm(formula = gmv ~ MRP + Procurement_SLA + per_order +
                          SEM + inc_PO_MA1 + inc_PO_MA3 + 
                          list_price.1 + list_price.2 + promotional_offer.1 + 
                          promotional_offer.2 + promotional_offer.3 + 
                          holiday_freq.1 + gmv.1, 
                      data = GamingDistLagData)
summary(DistLagModel_19)
# Multiple R-squared:  0.704,	Adjusted R-squared:  0.588
vif(DistLagModel_19)

# Removing SEM (high p-value)
DistLagModel_20 <- lm(formula = gmv ~ MRP + Procurement_SLA + per_order +
                          inc_PO_MA1 + inc_PO_MA3 + list_price.1 + list_price.2 +
                          promotional_offer.1 + promotional_offer.2 + 
                          promotional_offer.3 + holiday_freq.1 + gmv.1, 
                      data = GamingDistLagData)
summary(DistLagModel_20)
# Multiple R-squared:  0.699,	Adjusted R-squared:  0.593
vif(DistLagModel_20)

# Removing inc_PO_MA1 (high multicollinearity and low significance)
DistLagModel_21 <- lm(formula = gmv ~ MRP + Procurement_SLA + per_order +
                          inc_PO_MA3 + list_price.1 + list_price.2 +
                          promotional_offer.1 + promotional_offer.2 + 
                          promotional_offer.3 + holiday_freq.1 + gmv.1, 
                      data = GamingDistLagData)
summary(DistLagModel_21)
# Multiple R-squared:  0.66,	Adjusted R-squared:  0.554
vif(DistLagModel_21)

# Removing promotional_offer.2 (high p-value)
DistLagModel_22 <- lm(formula = gmv ~ MRP + Procurement_SLA + per_order +
                          inc_PO_MA3 + list_price.1 + list_price.2 +
                          promotional_offer.1 + promotional_offer.3 + 
                          holiday_freq.1 + gmv.1, 
                      data = GamingDistLagData)
summary(DistLagModel_22)
# Multiple R-squared:  0.648,	Adjusted R-squared:  0.55
vif(DistLagModel_22)

# Removing Procurement_SLA (high p-value)
DistLagModel_23 <- lm(formula = gmv ~ MRP + per_order + inc_PO_MA3 + list_price.1 +
                          list_price.2 + promotional_offer.1 + promotional_offer.3 + 
                          holiday_freq.1 + gmv.1, 
                      data = GamingDistLagData)
summary(DistLagModel_23)
# Multiple R-squared:  0.599,	Adjusted R-squared:  0.502
vif(DistLagModel_23)

# Removing promotional_offer.1 (high p-value)
DistLagModel_24 <- lm(formula = gmv ~ MRP + per_order + inc_PO_MA3 + list_price.1 +
                          list_price.2 + promotional_offer.3 + holiday_freq.1 + 
                          gmv.1, 
                      data = GamingDistLagData)
summary(DistLagModel_24)
# Multiple R-squared:  0.568,	Adjusted R-squared:  0.477
vif(DistLagModel_24)

# Removing list_price.1 (high p-value)
DistLagModel_25 <- lm(formula = gmv ~ MRP + per_order + inc_PO_MA3 +
                          list_price.2 + promotional_offer.3 + holiday_freq.1 + 
                          gmv.1, 
                      data = GamingDistLagData)
summary(DistLagModel_25)
# Multiple R-squared:  0.561,	Adjusted R-squared:  0.482
vif(DistLagModel_25)

# Removing per_order (high p-value)
DistLagModel_26 <- lm(formula = gmv ~ MRP + inc_PO_MA3 + list_price.2 + 
                          promotional_offer.3 + holiday_freq.1 + gmv.1, 
                      data = GamingDistLagData)
summary(DistLagModel_26)
# Multiple R-squared:  0.512,	Adjusted R-squared:  0.439
vif(DistLagModel_26)

# Removing promotional_offer.3 (high p-value)
DistLagModel_27 <- lm(formula = gmv ~ MRP + inc_PO_MA3 + list_price.2 + 
                          holiday_freq.1 + gmv.1, 
                      data = GamingDistLagData)
summary(DistLagModel_27)
# Multiple R-squared:  0.448,	Adjusted R-squared:  0.381
vif(DistLagModel_27)

# Removing inc_PO_MA3 (high p-value)
DistLagModel_28 <- lm(formula = gmv ~ MRP + list_price.2 + holiday_freq.1 + gmv.1, 
                      data = GamingDistLagData)
summary(DistLagModel_28)
# Multiple R-squared:  0.415,	Adjusted R-squared:  0.359
vif(DistLagModel_28)

# Removing any variable in DistLagModel_28 reduces the Adjusted R-squared considerably
# Hence, DistLagModel_28 can be considered as the final model

GamingDistLagModel <- DistLagModel_28
# 4 variables: MRP, list_price.2, holiday_freq.1, gmv.1

######################################################################################
# Cross Validation
# Calculate K-fold cross-validation, for K=10.
crossval <- cv.lm(data = GamingDistLagData, 
                  form.lm = formula(gmv ~ MRP + list_price.2 + holiday_freq.1 + gmv.1), m = 10)

CrossVal_Gaming[4] <- attr(crossval, "ms")

# Gaming Accessories Distributed Lag Model Elasticity Analysis
trainingSet <- GamingDistLagData

# Gaming Accessories Distributed Lag Model Elasticity coefficients
getGamingDistLagElasticity <- function(var){
    val <-as.numeric(GamingDistLagModel$coefficients[var] * 
                         mean(trainingSet[, var]) / mean(trainingSet$gmv))
    return(val)
}

gamingVarList <- list()

for(i in 2:length(GamingDistLagModel$coefficients)){
    gamingVarList[i-1] <- getGamingDistLagElasticity(names(GamingDistLagModel$coefficients)[i])
}

gamingDistLagModelElasticity <- data.frame(names(
    GamingDistLagModel$coefficients[2:length(GamingDistLagModel$coefficients)]))

gamingDistLagModelElasticity <- cbind(gamingDistLagModelElasticity, 
                                      do.call(rbind.data.frame, gamingVarList))

colnames(gamingDistLagModelElasticity) <- c("Variable", "Elasticity")

gamingDistLagModelElasticity$direction <- ifelse(
    gamingDistLagModelElasticity$Elasticity > 0, "Positive", "Negative")

ggplot(data = gamingDistLagModelElasticity,
       aes(x = reorder(Variable, Elasticity), y = Elasticity)) +
    geom_bar(position = "dodge", stat = "identity") + 
    coord_flip() +
    ggtitle("Gaming Accessories - Distributed Lag Model") + xlab("Variables")

#####################################################################################

####################################################################################
#    :::::::: Multiplicative + Distributed Lag Model - Gaming Accessory ::::::::
####################################################################################

colnames(GamingAccessory_final)
# Removing week_year, holiday_freq, Moving Averages and derived price category variables
# Removing the following columns for the initial Multiplicative + Distributed Lag Model:
# week_year, Mass_p, Middle_p, Premium_p, holiday_freq,
# inc_LP_MA1, inc_LP_MA2, inc_LP_MA3,
# inc_PO_MA1, inc_PO_MA2, inc_PO_MA3,
# holiday_freq-1, holiday_freq-2, holiday_freq-3
GamingMultiDistLagData <- GamingAccessory_final[, -c(1, 10:13, 22:27, 37:39)]

# Lag GMV by 1 Time Unit
GamingMultiDistLagData_1 <- slide(GamingMultiDistLagData, Var = "gmv", slideBy = -1)

# Lag GMV by 2 Time Units
GamingMultiDistLagData_1 <- slide(GamingMultiDistLagData_1, Var = "gmv", slideBy = -2)

# Lag GMV by 3 Time Units
GamingMultiDistLagData_1 <- slide(GamingMultiDistLagData_1, Var = "gmv", slideBy = -3)

# Remove NAs
GamingMultiDistLagData <- na.omit(GamingMultiDistLagData_1)

# Data Treatment
GamingMultiDistLagData$ContentMarketing[which(GamingMultiDistLagData$ContentMarketing==0)] <- 1
GamingMultiDistLagData$per_order[which(GamingMultiDistLagData$per_order==0)] <- 0.01

# Applying Logarithm
GamingMultiDistLagData <- log(GamingMultiDistLagData)

# Updating column names
colnames(GamingMultiDistLagData)[17:28] <- c("list_price.1", "list_price.2", "list_price.3", "promotional_offer.1", "promotional_offer.2", "promotional_offer.3", "NPS.1", "NPS.2", "NPS.3", "gmv.1", "gmv.2", "gmv.3")

# Build the Initial Multiplicative + Distributed Lag Model
MultiDistLagModel_1 <- lm(gmv ~ ., GamingMultiDistLagData)
summary(MultiDistLagModel_1)
# Multiple R-squared:  0.982,	Adjusted R-squared:  0.956

MultiDistLagModel_2 <- stepAIC(MultiDistLagModel_1, direction = "both")
summary(MultiDistLagModel_2)
# Multiple R-squared:  0.98,	Adjusted R-squared:  0.968

MultiDistLagModel_3 <- lm(formula = gmv ~ SLA + promotional_offer + Procurement_SLA + 
                              TV + Sponsorship + ContentMarketing + OnlineMarketing +
                              Affiliates + SEM + list_price.2 + list_price.3 +
                              promotional_offer.1 + promotional_offer.2 + 
                              promotional_offer.3 + NPS.1 + NPS.2 + NPS.3, 
                          data = GamingMultiDistLagData)
summary(MultiDistLagModel_3)
# Multiple R-squared:  0.98,	Adjusted R-squared:  0.968
vif(MultiDistLagModel_3)

# Removing NPS.3 (high p-value)
MultiDistLagModel_4 <- lm(formula = gmv ~ SLA + promotional_offer + Procurement_SLA + 
                              TV + Sponsorship + ContentMarketing + OnlineMarketing +
                              Affiliates + SEM + list_price.2 + list_price.3 +
                              promotional_offer.1 + promotional_offer.2 + 
                              promotional_offer.3 + NPS.1 + NPS.2, 
                          data = GamingMultiDistLagData)
summary(MultiDistLagModel_4)
# Multiple R-squared:  0.979,	Adjusted R-squared:  0.968
vif(MultiDistLagModel_4)

# Removing Sponsorship (high p-value)
MultiDistLagModel_5 <- lm(formula = gmv ~ SLA + promotional_offer + Procurement_SLA + 
                              TV + ContentMarketing + OnlineMarketing +
                              Affiliates + SEM + list_price.2 + list_price.3 +
                              promotional_offer.1 + promotional_offer.2 + 
                              promotional_offer.3 + NPS.1 + NPS.2, 
                          data = GamingMultiDistLagData)
summary(MultiDistLagModel_5)
# Multiple R-squared:  0.977,	Adjusted R-squared:  0.966
vif(MultiDistLagModel_5)

# Removing TV (high multicollinearity)
MultiDistLagModel_6 <- lm(formula = gmv ~ SLA + promotional_offer + Procurement_SLA + 
                              ContentMarketing + OnlineMarketing +
                              Affiliates + SEM + list_price.2 + list_price.3 +
                              promotional_offer.1 + promotional_offer.2 + 
                              promotional_offer.3 + NPS.1 + NPS.2, 
                          data = GamingMultiDistLagData)
summary(MultiDistLagModel_6)
# Multiple R-squared:  0.938,	Adjusted R-squared:  0.91
vif(MultiDistLagModel_6)

# Removing ContentMarketing (high p-value)
MultiDistLagModel_7 <- lm(formula = gmv ~ SLA + promotional_offer + Procurement_SLA + 
                              OnlineMarketing + Affiliates + SEM + list_price.2 +
                              list_price.3 + promotional_offer.1 + 
                              promotional_offer.2 + promotional_offer.3 + 
                              NPS.1 + NPS.2, 
                          data = GamingMultiDistLagData)
summary(MultiDistLagModel_7)
# Multiple R-squared:  0.937,	Adjusted R-squared:  0.911
vif(MultiDistLagModel_7)

# Removing promotional_offer (high p-value)
MultiDistLagModel_8 <- lm(formula = gmv ~ SLA + Procurement_SLA + OnlineMarketing +
                              Affiliates + SEM + list_price.2 + list_price.3 +
                              promotional_offer.1 + promotional_offer.2 + 
                              promotional_offer.3 + NPS.1 + NPS.2, 
                          data = GamingMultiDistLagData)
summary(MultiDistLagModel_8)
# Multiple R-squared:  0.935,	Adjusted R-squared:  0.912
vif(MultiDistLagModel_8)

# Removing SLA (high p-value)
MultiDistLagModel_9 <- lm(formula = gmv ~  Procurement_SLA + OnlineMarketing +
                              Affiliates + SEM + list_price.2 + list_price.3 +
                              promotional_offer.1 + promotional_offer.2 + 
                              promotional_offer.3 + NPS.1 + NPS.2, 
                          data = GamingMultiDistLagData)
summary(MultiDistLagModel_9)
# Multiple R-squared:  0.932,	Adjusted R-squared:  0.911
vif(MultiDistLagModel_9)

# Removing SEM (high p-value)
MultiDistLagModel_10 <- lm(formula = gmv ~  Procurement_SLA + OnlineMarketing +
                               Affiliates + list_price.2 + list_price.3 +
                               promotional_offer.1 + promotional_offer.2 + 
                               promotional_offer.3 + NPS.1 + NPS.2, 
                           data = GamingMultiDistLagData)
summary(MultiDistLagModel_10)
# Multiple R-squared:  0.924,	Adjusted R-squared:  0.903
vif(MultiDistLagModel_10)

# Removing promotional_offer.2 (high p-value)
MultiDistLagModel_11 <- lm(formula = gmv ~  Procurement_SLA + OnlineMarketing +
                               Affiliates + list_price.2 + list_price.3 +
                               promotional_offer.1 + 
                               promotional_offer.3 + NPS.1 + NPS.2, 
                           data = GamingMultiDistLagData)
summary(MultiDistLagModel_11)
# Multiple R-squared:  0.914,	Adjusted R-squared:  0.893
vif(MultiDistLagModel_11)

# Removing list_price.3 (high p-value)
MultiDistLagModel_12 <- lm(formula = gmv ~  Procurement_SLA + OnlineMarketing +
                               Affiliates + list_price.2 + promotional_offer.1 + 
                               promotional_offer.3 + NPS.1 + NPS.2, 
                           data = GamingMultiDistLagData)
summary(MultiDistLagModel_12)
# Multiple R-squared:  0.907,	Adjusted R-squared:  0.888
vif(MultiDistLagModel_12)

# Removing NPS.2 (high p-value)
MultiDistLagModel_13 <- lm(formula = gmv ~  Procurement_SLA + OnlineMarketing +
                               Affiliates + list_price.2 + promotional_offer.1 + 
                               promotional_offer.3 + NPS.1, 
                           data = GamingMultiDistLagData)
summary(MultiDistLagModel_13)
# Multiple R-squared:   0.9,	Adjusted R-squared:  0.882
vif(MultiDistLagModel_13)

# Removing list_price.2 (high p-value)
MultiDistLagModel_14 <- lm(formula = gmv ~  Procurement_SLA + OnlineMarketing +
                               Affiliates + promotional_offer.1 + 
                               promotional_offer.3 + NPS.1, 
                           data = GamingMultiDistLagData)
summary(MultiDistLagModel_14)
# Multiple R-squared:  0.897,	Adjusted R-squared:  0.881
vif(MultiDistLagModel_14)

# Removing NPS.1 (high p-value)
MultiDistLagModel_15 <- lm(formula = gmv ~  Procurement_SLA + OnlineMarketing +
                               Affiliates + promotional_offer.1 + 
                               promotional_offer.3, 
                           data = GamingMultiDistLagData)
summary(MultiDistLagModel_15)
# Multiple R-squared:  0.874,	Adjusted R-squared:  0.859
vif(MultiDistLagModel_15)

# Removing promotional_offer.1 (high p-value)
MultiDistLagModel_16 <- lm(formula = gmv ~  Procurement_SLA + OnlineMarketing +
                               Affiliates + promotional_offer.3, 
                           data = GamingMultiDistLagData)
summary(MultiDistLagModel_16)
# Multiple R-squared:  0.854,	Adjusted R-squared:  0.84
vif(MultiDistLagModel_16)

# Removing Procurement_SLA (high p-value)
MultiDistLagModel_17 <- lm(formula = gmv ~  OnlineMarketing +
                               Affiliates + promotional_offer.3, 
                           data = GamingMultiDistLagData)
summary(MultiDistLagModel_17)
# Multiple R-squared:  0.838,	Adjusted R-squared:  0.827
vif(MultiDistLagModel_17)

# We will consider MultiDistLagModel_17 as the final model

GamingMultiDistLagModel <- MultiDistLagModel_17
# 3 variables: OnlineMarketing, Affiliates, promotional_offer.3
# Multiple R-squared:  0.838,	Adjusted R-squared:  0.827

######################################################################################
# Cross Validation
# Calculate K-fold cross-validation, for K=10.
crossval <- cv.lm(data = GamingMultiDistLagData, 
                  form.lm = formula(gmv ~ OnlineMarketing + Affiliates + promotional_offer.3), m = 10)

CrossVal_Gaming[5] <- attr(crossval, "ms")

# Gaming Accessories Multiplicative + Distributed Lag Model Elasticity Analysis
trainingSet <- GamingMultiDistLagData

# Gaming Accessories Multiplicative + Distributed Lag Model Elasticity coefficients
getGamingMultiDistLagElasticity <- function(var){
    val <-as.numeric(GamingMultiDistLagModel$coefficients[var] * 
                         mean(trainingSet[, var]) / mean(trainingSet$gmv))
    return(val)
} 

gamingVarList <- list()

for(i in 2:length(GamingMultiDistLagModel$coefficients)){
    gamingVarList[i-1] <- getGamingMultiDistLagElasticity(names(GamingMultiDistLagModel$coefficients)[i])
}

gamingMultiDistLagModelElasticity <- data.frame(names(
    GamingMultiDistLagModel$coefficients[2:length(GamingMultiDistLagModel$coefficients)]))

gamingMultiDistLagModelElasticity <- cbind(gamingMultiDistLagModelElasticity, 
                                           do.call(rbind.data.frame, gamingVarList))

colnames(gamingMultiDistLagModelElasticity) <- c("Variable", "Elasticity")

gamingMultiDistLagModelElasticity$direction <- ifelse(
    gamingMultiDistLagModelElasticity$Elasticity > 0, "Positive", "Negative")

ggplot(data = gamingMultiDistLagModelElasticity,
       aes(x = reorder(Variable, Elasticity), y = Elasticity)) +
    geom_bar(position = "dodge", stat = "identity") + 
    coord_flip() +
    ggtitle("Gaming Accessories - Multiplicative + Distributed Lag Model") + xlab("Variables")

#####################################################################################
