####################################################################################
#                  :::::::: Ecommerce Capstone Solution ::::::::
####################################################################################
####################################################################################
#                  :::::::: Market Mix Modelling :::::::::::::::
####################################################################################
####################################################################################
#                  :::::::: Model Building - Camera Accessory ::::::::
####################################################################################

CrossVal_Camera <- rep(0,5)

####################################################################################
#                  :::::::: Basic Linear Model - Camera Accessory ::::::::
####################################################################################

colnames(CameraAccessory_final)
# Removing week_year, Lag variables and derived price category variables
# Removing the following columns for the Initial Basic Linear Model:
# week_year, Mass_p, Middle_p, Premium_p,
# list_price-1, list_price-2, list_price-3,
# promotional_offer-1, promotional_offer-2, promotional_offer-3
# NPS-1, NPS-2, NPS-3
# holiday_freq-1, holiday_freq-2, holiday_freq-3
CameraLinearData <- CameraAccessory_final[, -c(1, 10:12, 28:39)]

# Scaling
CameraLinearData <- data.frame(scale(CameraLinearData))

# Initial Linear Model
LinearModel_1 <- lm(gmv ~ ., CameraLinearData)
summary(LinearModel_1)
# Multiple R-squared:  0.8444,	Adjusted R-squared:  0.7128

LinearModel_2 <- stepAIC(LinearModel_1, direction = "both")
summary(LinearModel_2)
# Multiple R-squared:  0.8297,	Adjusted R-squared:  0.7523

LinearModel_3 <- lm(formula = gmv ~ MRP + SLA + Procurement_SLA + per_order + 
                        NPS + holiday_freq + TV + Digital + Sponsorship + 
                        OnlineMarketing + SEM + inc_LP_MA1 + inc_LP_MA3 + 
                        inc_PO_MA1 + inc_PO_MA2, 
                    data = CameraLinearData)
summary(LinearModel_3)
# Multiple R-squared:  0.8297,	Adjusted R-squared:  0.7523
vif(LinearModel_3)

# Removing inc_LP_MA1 (high multicollinearity and low significance)
LinearModel_4 <- lm(formula = gmv ~ MRP + SLA + Procurement_SLA + per_order + 
                        NPS + holiday_freq + TV + Digital + Sponsorship + 
                        OnlineMarketing + SEM + inc_LP_MA3 + 
                        inc_PO_MA1 + inc_PO_MA2, 
                    data = CameraLinearData)
summary(LinearModel_4)
# Multiple R-squared:  0.8198,	Adjusted R-squared:  0.7457
vif(LinearModel_4)

# Removing SLA (high p-value)
LinearModel_5 <- lm(formula = gmv ~ MRP + Procurement_SLA + per_order + 
                        NPS + holiday_freq + TV + Digital + Sponsorship + 
                        OnlineMarketing + SEM + inc_LP_MA3 + 
                        inc_PO_MA1 + inc_PO_MA2, 
                    data = CameraLinearData)
summary(LinearModel_5)
# Multiple R-squared:  0.8058,	Adjusted R-squared:  0.7337
vif(LinearModel_5)

# Removing NPS (high multicollinearity and low significance)
LinearModel_6 <- lm(formula = gmv ~ MRP + Procurement_SLA + per_order + 
                        holiday_freq + TV + Digital + Sponsorship + 
                        OnlineMarketing + SEM + inc_LP_MA3 + 
                        inc_PO_MA1 + inc_PO_MA2, 
                    data = CameraLinearData)
summary(LinearModel_6)
# Multiple R-squared:  0.7865,	Adjusted R-squared:  0.7153
vif(LinearModel_6)

# Removing per_order (high p-value)
LinearModel_7 <- lm(formula = gmv ~ MRP + Procurement_SLA + 
                        holiday_freq + TV + Digital + Sponsorship + 
                        OnlineMarketing + SEM + inc_LP_MA3 + 
                        inc_PO_MA1 + inc_PO_MA2, 
                    data = CameraLinearData)
summary(LinearModel_7)
# Multiple R-squared:  0.7764,	Adjusted R-squared:   0.71
vif(LinearModel_7)

# Removing holiday_freq (high p-value)
LinearModel_8 <- lm(formula = gmv ~ MRP + Procurement_SLA + TV + Digital + 
                        Sponsorship + OnlineMarketing + SEM + inc_LP_MA3 + 
                        inc_PO_MA1 + inc_PO_MA2, 
                    data = CameraLinearData)
summary(LinearModel_8)
# Multiple R-squared:  0.7575,	Adjusted R-squared:  0.6937
vif(LinearModel_8)

# Removing Procurement_SLA (high p-value)
LinearModel_9 <- lm(formula = gmv ~ MRP + TV + Digital + Sponsorship + 
                        OnlineMarketing + SEM + inc_LP_MA3 + inc_PO_MA1 + inc_PO_MA2, 
                    data = CameraLinearData)
summary(LinearModel_9)
# Multiple R-squared:  0.7413,	Adjusted R-squared:  0.6816
vif(LinearModel_9)

# Removing inc_PO_MA2 (high p-value)
LinearModel_10 <- lm(formula = gmv ~ MRP + TV + Digital + Sponsorship + 
                         OnlineMarketing + SEM + inc_LP_MA3 + inc_PO_MA1, 
                     data = CameraLinearData)
summary(LinearModel_10)
# Multiple R-squared:  0.7291,	Adjusted R-squared:  0.6749
vif(LinearModel_10)

# Removing MRP (high p-value)
LinearModel_11 <- lm(formula = gmv ~ TV + Digital + Sponsorship + 
                         OnlineMarketing + SEM + inc_LP_MA3 + inc_PO_MA1, 
                     data = CameraLinearData)
summary(LinearModel_11)
# Multiple R-squared:  0.7199,	Adjusted R-squared:  0.6721
vif(LinearModel_11)

# Removing Digital (high multicollinearity and low significance)
LinearModel_12 <- lm(formula = gmv ~ TV + Sponsorship + 
                         OnlineMarketing + SEM + inc_LP_MA3 + inc_PO_MA1, 
                     data = CameraLinearData)
summary(LinearModel_12)
# Multiple R-squared:  0.6583,	Adjusted R-squared:  0.6095
vif(LinearModel_12)

# Removing TV (high p-value)
LinearModel_13 <- lm(formula = gmv ~ Sponsorship + OnlineMarketing + SEM + 
                         inc_LP_MA3 + inc_PO_MA1, 
                     data = CameraLinearData)
summary(LinearModel_13)
# Multiple R-squared:  0.6378,	Adjusted R-squared:  0.5957
vif(LinearModel_13)

# Removing SEM (high p-value)
LinearModel_14 <- lm(formula = gmv ~ Sponsorship + OnlineMarketing + 
                         inc_LP_MA3 + inc_PO_MA1, 
                     data = CameraLinearData)
summary(LinearModel_14)
# Multiple R-squared:  0.6153,	Adjusted R-squared:  0.5803
vif(LinearModel_14)

# Removing OnlineMarketing (high p-value)
LinearModel_15 <- lm(formula = gmv ~ Sponsorship + inc_LP_MA3 + inc_PO_MA1, 
                     data = CameraLinearData)
summary(LinearModel_15)
# Multiple R-squared:  0.579,	Adjusted R-squared:  0.551
vif(LinearModel_15)

# No more variables can be removed from LinearModel_15.
# Hence, LinearModel_15 is the final model
CameraLinearModel <- LinearModel_15
# 3 variables: Sponsorship, inc_LP_MA3, inc_PO_MA1
# Multiple R-squared:  0.579,	Adjusted R-squared:  0.551

######################################################################################
# Cross Validation
# Calculate K-fold cross-validation, for K=10.
crossval <- cv.lm(data = CameraLinearData, 
                  form.lm = formula(gmv ~ Sponsorship + inc_LP_MA3 + inc_PO_MA1), m = 10)

CrossVal_Camera[1] <- attr(crossval, "ms")

# Camera Accessories Linear Model Elasticity Analysis
trainingSet <- CameraLinearData

# Camera Accessories Linear Model Elasticity coefficients
getCameraElasticity <- function(var){
    val <-as.numeric(CameraLinearModel$coefficients[var] * 
                         mean(trainingSet[, var]) / mean(trainingSet$gmv))
    return(val)
}

cameraVarList <- list()

for(i in 2:length(CameraLinearModel$coefficients)){
    cameraVarList[i-1] <- getCameraElasticity(names(CameraLinearModel$coefficients)[i])
}

cameraLinearModelElasticity <- data.frame(names(
    CameraLinearModel$coefficients[2:length(CameraLinearModel$coefficients)]))

cameraLinearModelElasticity <- cbind(cameraLinearModelElasticity, 
                                     do.call(rbind.data.frame, cameraVarList))

colnames(cameraLinearModelElasticity) <- c("Variable", "Elasticity")

cameraLinearModelElasticity$direction <- ifelse(
    cameraLinearModelElasticity$Elasticity > 0, "Positive", "Negative")

ggplot(data = cameraLinearModelElasticity, 
       aes(x = reorder(Variable, Elasticity), y = Elasticity)) +
    geom_bar(position = "dodge", stat = "identity") + 
    coord_flip() +
    ggtitle("Camera Accessories - Linear Model") + xlab("Variables")

#####################################################################################

####################################################################################
#           :::::::: Multiplicative Model - Camera Accessory ::::::::
####################################################################################

colnames(CameraAccessory_final)
# Removing Moving Averages, Lag and derived price category variables
# Removing the following columns for the initial Multiplicative Model:
# week_year, Mass_p, Middle_p, Premium_p, holiday_freq,
# inc_LP_MA1, inc_LP_MA2, inc_LP_MA3,
# inc_PO_MA1, inc_PO_MA2, inc_PO_MA3
# list_price-1, list_price-2, list_price-3,
# promotional_offer-1, promotional_offer-2, promotional_offer-3
# NPS-1, NPS-2, NPS-3
# holiday_freq-1, holiday_freq-2, holiday_freq-3
CameraMultiData <- CameraAccessory_final[, -c(1, 10:13, 22:39)]

# Treatment for Multiplicative Modelling
CameraMultiData$ContentMarketing[which(CameraMultiData$ContentMarketing==0)] <- 0.01
CameraMultiData$per_order[which(CameraMultiData$per_order==0)] <- 0.01
CameraMultiData <- log(CameraMultiData)

# Build the initial Multiplicative Model
multiModel_1 <- lm(gmv ~ ., CameraMultiData)
summary(multiModel_1)
# Multiple R-squared:  0.94,	Adjusted R-squared:  0.913

multiModel_2 <- stepAIC(multiModel_1, direction = "both")
summary(multiModel_2)
# Multiple R-squared:  0.939,	Adjusted R-squared:  0.923

multiModel_3 <- lm(formula = gmv ~ list_price + MRP + Units + SLA + 
                       promotional_offer + per_order + Digital + 
                       OnlineMarketing + Affiliates + SEM, 
                   data = CameraMultiData)
summary(multiModel_3)
# Multiple R-squared:  0.939,	Adjusted R-squared:  0.923
vif(multiModel_3)

# Removing SLA (high p-value)
multiModel_4 <- lm(formula = gmv ~ list_price + MRP + Units + 
                       promotional_offer + per_order + Digital + 
                       OnlineMarketing + Affiliates + SEM, 
                   data = CameraMultiData)
summary(multiModel_4)
# Multiple R-squared:  0.933,	Adjusted R-squared:  0.918
vif(multiModel_4)

# Removing MRP (high p-value)
multiModel_5 <- lm(formula = gmv ~ list_price + Units + promotional_offer + 
                       per_order + Digital + OnlineMarketing + Affiliates + SEM, 
                   data = CameraMultiData)
summary(multiModel_5)
# Multiple R-squared:  0.931,	Adjusted R-squared:  0.918
vif(multiModel_5)

# Removing list_price (high p-value)
multiModel_6 <- lm(formula = gmv ~ Units + promotional_offer + 
                       per_order + Digital + OnlineMarketing + Affiliates + SEM, 
                   data = CameraMultiData)
summary(multiModel_6)
# Multiple R-squared:  0.922,	Adjusted R-squared:  0.908
vif(multiModel_6)

# Removing promotional_offer (high p-value)
multiModel_7 <- lm(formula = gmv ~ Units + per_order + Digital + 
                       OnlineMarketing + Affiliates + SEM, 
                   data = CameraMultiData)
summary(multiModel_7)
# Multiple R-squared:  0.92,	Adjusted R-squared:  0.908
vif(multiModel_7)

# Removing Digital (high p-value)
multiModel_8 <- lm(formula = gmv ~ Units + per_order + 
                       OnlineMarketing + Affiliates + SEM, 
                   data = CameraMultiData)
summary(multiModel_8)
# Multiple R-squared:  0.909,	Adjusted R-squared:  0.899
vif(multiModel_8)

# Removing SEM (high p-value)
multiModel_9 <- lm(formula = gmv ~ Units + per_order + OnlineMarketing + Affiliates, 
                   data = CameraMultiData)
summary(multiModel_9)
# Multiple R-squared:  0.897,	Adjusted R-squared:  0.887
vif(multiModel_9)

# Removing Affiliates (high multicollinearity and low significance)
multiModel_10 <- lm(formula = gmv ~ Units + per_order + OnlineMarketing, 
                    data = CameraMultiData)
summary(multiModel_10)
# Multiple R-squared:  0.886,	Adjusted R-squared:  0.878
vif(multiModel_10)

# No more variables can be removed from multiModel_10, hence, this is the final model
CameraMultiModel <- multiModel_10
# 3 variables: Units, per_order, OnlineMarketing
# Multiple R-squared:  0.886,	Adjusted R-squared:  0.878

######################################################################################
# Cross Validation
# Calculate K-fold cross-validation, for K=10.
crossval <- cv.lm(data = CameraMultiData, 
                  form.lm = formula(gmv ~ Units + per_order + OnlineMarketing), m = 10)

CrossVal_Camera[2] <- attr(crossval, "ms")

# Camera Accessories Multiplicative Model Elasticity Analysis
trainingSet <- CameraMultiData

# Camera Accessories Multiplicative Model Elasticity coefficients
getCameraMultiElasticity <- function(var){
    val <-as.numeric(CameraMultiModel$coefficients[var] * 
                         mean(trainingSet[, var]) / mean(trainingSet$gmv))
    return(val)
} 

cameraVarList <- list()

for(i in 2:length(CameraMultiModel$coefficients)){
    cameraVarList[i-1] <- getCameraMultiElasticity(names(CameraMultiModel$coefficients)[i])
}

cameraMultiModelElasticity <- data.frame(names(
    CameraMultiModel$coefficients[2:length(CameraMultiModel$coefficients)]))

cameraMultiModelElasticity <- cbind(cameraMultiModelElasticity, 
                                    do.call(rbind.data.frame, cameraVarList))

colnames(cameraMultiModelElasticity) <- c("Variable", "Elasticity")

cameraMultiModelElasticity$direction <- ifelse(
    cameraMultiModelElasticity$Elasticity > 0, "Positive", "Negative")

ggplot(data = cameraMultiModelElasticity,
       aes(x = reorder(Variable, Elasticity), y = Elasticity)) +
    geom_bar(position = "dodge", stat = "identity") + 
    coord_flip() +
    ggtitle("Camera Accessories - Multiplicative Model") + xlab("Variables")

#####################################################################################

####################################################################################
#                  :::::::: Koyck Model - Camera Accessory ::::::::
####################################################################################

colnames(CameraAccessory_final)
# Removing week_year, Lag variables and derived price category variables
# Removing the following columns for the initial Koyck Model:
# week_year, Mass_p, Middle_p, Premium_p,
# list_price-1, list_price-2, list_price-3,
# promotional_offer-1, promotional_offer-2, promotional_offer-3
# NPS-1, NPS-2, NPS-3
# holiday_freq-1, holiday_freq-2, holiday_freq-3
CameraKoyckData <- CameraAccessory_final[, -c(1, 10:12, 28:39)]

# Lag GMV by 1 Unit
CameraKoyckData <- slide(CameraKoyckData, Var = "gmv", slideBy = -1)

# Remove NAs and perform scaling
CameraKoyckData <- data.frame(scale(na.omit(CameraKoyckData)))

# Build initial Koyck model
koyckModel_1 <- lm(gmv ~ ., CameraKoyckData)
summary(koyckModel_1)
# Multiple R-squared:  0.871,	Adjusted R-squared:  0.748

koyckModel_2 <- stepAIC(koyckModel_1, direction = "both")
summary(koyckModel_2)
# Multiple R-squared:  0.859,	Adjusted R-squared:  0.792

koyckModel_3 <- lm(formula = gmv ~ MRP + SLA + Procurement_SLA + per_order + 
                       NPS + holiday_freq + TV + Digital + Sponsorship + 
                       OnlineMarketing + SEM + inc_LP_MA1 + inc_LP_MA3 + 
                       inc_PO_MA1 + inc_PO_MA2, 
                   data = CameraKoyckData)
summary(koyckModel_3)
# Multiple R-squared:  0.859,	Adjusted R-squared:  0.792
vif(koyckModel_3)

# Removing inc_LP_MA1 (high p-value)
koyckModel_4 <- lm(formula = gmv ~ MRP + SLA + Procurement_SLA + per_order + 
                       NPS + holiday_freq + TV + Digital + Sponsorship + 
                       OnlineMarketing + SEM + inc_LP_MA3 + 
                       inc_PO_MA1 + inc_PO_MA2, 
                   data = CameraKoyckData)
summary(koyckModel_4)
# Multiple R-squared:  0.843,	Adjusted R-squared:  0.777
vif(koyckModel_4)

# Removing SLA (high p-value)
koyckModel_5 <- lm(formula = gmv ~ MRP + Procurement_SLA + per_order + 
                       NPS + holiday_freq + TV + Digital + Sponsorship + 
                       OnlineMarketing + SEM + inc_LP_MA3 + 
                       inc_PO_MA1 + inc_PO_MA2, 
                   data = CameraKoyckData)
summary(koyckModel_5)
# Multiple R-squared:  0.827,	Adjusted R-squared:  0.761
vif(koyckModel_5)

# Removing per_order (high p-value)
koyckModel_6 <- lm(formula = gmv ~ MRP + Procurement_SLA + NPS + holiday_freq + 
                       TV + Digital + Sponsorship + OnlineMarketing + SEM + 
                       inc_LP_MA3 + inc_PO_MA1 + inc_PO_MA2, 
                   data = CameraKoyckData)
summary(koyckModel_6)
# Multiple R-squared:  0.808,	Adjusted R-squared:  0.743
vif(koyckModel_6)

# Removing NPS (high p-value)
koyckModel_7 <- lm(formula = gmv ~ MRP + Procurement_SLA + holiday_freq + 
                       TV + Digital + Sponsorship + OnlineMarketing + SEM + 
                       inc_LP_MA3 + inc_PO_MA1 + inc_PO_MA2, 
                   data = CameraKoyckData)
summary(koyckModel_7)
# Multiple R-squared:  0.801,	Adjusted R-squared:  0.741
vif(koyckModel_7)

# Removing Procurement_SLA (high p-value)
koyckModel_8 <- lm(formula = gmv ~ MRP + holiday_freq + 
                       TV + Digital + Sponsorship + OnlineMarketing + SEM + 
                       inc_LP_MA3 + inc_PO_MA1 + inc_PO_MA2, 
                   data = CameraKoyckData)
summary(koyckModel_8)
# Multiple R-squared:  0.782,	Adjusted R-squared:  0.723
vif(koyckModel_8)

# Removing inc_PO_MA2 (high p-value)
koyckModel_9 <- lm(formula = gmv ~ MRP + holiday_freq + TV + Digital + Sponsorship +
                       OnlineMarketing + SEM + inc_LP_MA3 + inc_PO_MA1, 
                   data = CameraKoyckData)
summary(koyckModel_9)
# Multiple R-squared:  0.768,	Adjusted R-squared:  0.713
vif(koyckModel_9)

# Removing MRP (high p-value)
koyckModel_10 <- lm(formula = gmv ~ holiday_freq + TV + Digital + Sponsorship +
                        OnlineMarketing + SEM + inc_LP_MA3 + inc_PO_MA1, 
                    data = CameraKoyckData)
summary(koyckModel_10)
# Multiple R-squared:  0.747,	Adjusted R-squared:  0.695
vif(koyckModel_10)

# Removing holiday_freq (high p-value)
koyckModel_11 <- lm(formula = gmv ~ TV + Digital + Sponsorship +
                        OnlineMarketing + SEM + inc_LP_MA3 + inc_PO_MA1, 
                    data = CameraKoyckData)
summary(koyckModel_11)
# Multiple R-squared:  0.731,	Adjusted R-squared:  0.684
vif(koyckModel_11)

# Removing Digital (high multicollinearity and low significance)
koyckModel_12 <- lm(formula = gmv ~ TV + Sponsorship +
                        OnlineMarketing + SEM + inc_LP_MA3 + inc_PO_MA1, 
                    data = CameraKoyckData)
summary(koyckModel_12)
# Multiple R-squared:  0.668,	Adjusted R-squared:  0.619
vif(koyckModel_12)

# Removing TV (high p-value)
koyckModel_13 <- lm(formula = gmv ~ Sponsorship + OnlineMarketing + SEM + 
                        inc_LP_MA3 + inc_PO_MA1, 
                    data = CameraKoyckData)
summary(koyckModel_13)
# Multiple R-squared:  0.646,	Adjusted R-squared:  0.604
vif(koyckModel_13)

# Removing SEM (high p-value)
koyckModel_14 <- lm(formula = gmv ~ Sponsorship + OnlineMarketing + 
                        inc_LP_MA3 + inc_PO_MA1, 
                    data = CameraKoyckData)
summary(koyckModel_14)
# Multiple R-squared:  0.618,	Adjusted R-squared:  0.582
vif(koyckModel_14)

# Removing OnlineMarketing (high p-value)
koyckModel_15 <- lm(formula = gmv ~ Sponsorship + inc_LP_MA3 + inc_PO_MA1, 
                    data = CameraKoyckData)
summary(koyckModel_15)
# Multiple R-squared:  0.577,	Adjusted R-squared:  0.548
vif(koyckModel_15)

# No more variables can be removed from koyckModel_15.
# Hence, koyckModel_15 can be considered to be the final model

CameraKoyckModel <- koyckModel_15
# Multiple R-squared:  0.577,	Adjusted R-squared:  0.548
# 3 variables: Sponsorship, inc_LP_MA3 and inc_PO_MA1

######################################################################################
# Cross Validation
# Calculate K-fold cross-validation, for K=10.
crossval <- cv.lm(data = CameraKoyckData, 
                  form.lm = formula(gmv ~ Sponsorship + inc_LP_MA3 + inc_PO_MA1), m = 10)

CrossVal_Camera[3] <- attr(crossval, "ms")

# Camera Accessories Koyck Model Elasticity Analysis
trainingSet <- CameraKoyckData

# Camera Accessories Koyck Model Elasticity coefficients
getCameraKoyckElasticity <- function(var){
    val <-as.numeric(CameraKoyckModel$coefficients[var] * 
                         mean(trainingSet[, var]) / mean(trainingSet$gmv))
    return(val)
} 

cameraVarList <- list()

for(i in 2:length(CameraKoyckModel$coefficients)){
    cameraVarList[i-1] <- getCameraKoyckElasticity(names(CameraKoyckModel$coefficients)[i])
}

cameraKoyckModelElasticity <- data.frame(names(
    CameraKoyckModel$coefficients[2:length(CameraKoyckModel$coefficients)]))

cameraKoyckModelElasticity <- cbind(cameraKoyckModelElasticity, 
                                    do.call(rbind.data.frame, cameraVarList))

colnames(cameraKoyckModelElasticity) <- c("Variable", "Elasticity")

cameraKoyckModelElasticity$direction <- ifelse(
    cameraKoyckModelElasticity$Elasticity > 0, "Positive", "Negative")

ggplot(data = cameraKoyckModelElasticity,
       aes(x = reorder(Variable, Elasticity), y = Elasticity)) +
    geom_bar(position = "dodge", stat = "identity") + 
    coord_flip() +
    ggtitle("Camera Accessories - Koyck Model") + xlab("Variables")

#####################################################################################

####################################################################################
#       :::::::: Distributed Lag Model - Camera Accessory ::::::::
####################################################################################

colnames(CameraAccessory_final)
# Removing week_year and derived price category variables
# Removing the following columns for the initial Distributed Lag Model:
# week_year, Mass_p, Middle_p, Premium_p,
CameraDistLagData <- CameraAccessory_final[, -c(1, 10:12)]

# Lag GMV by 1 Time Unit
CameraDistLagData_1 <- slide(CameraDistLagData, Var = "gmv", slideBy = -1)

# Lag GMV by 2 Time Units
CameraDistLagData_1 <- slide(CameraDistLagData_1, Var = "gmv", slideBy = -2)

# Lag GMV by 3 Time Units
CameraDistLagData_1 <- slide(CameraDistLagData_1, Var = "gmv", slideBy = -3)

# Remove NAs and Scaling
CameraDistLagData <- data.frame(scale(na.omit(CameraDistLagData_1)))

# Build the initial Distributed Lag Model
DistLagModel_1 <- lm(gmv ~ ., CameraDistLagData)
summary(DistLagModel_1)
# Multiple R-squared:  0.969,	Adjusted R-squared:  0.828

DistLagModel_2 <- stepAIC(DistLagModel_1, direction = "both")
summary(DistLagModel_2)
# Multiple R-squared:  0.968,	Adjusted R-squared:  0.903

DistLagModel_3 <- lm(formula = gmv ~ list_price + MRP + SLA + Procurement_SLA + 
                         per_order + NPS + holiday_freq + TV + Digital + 
                         Sponsorship + ContentMarketing + OnlineMarketing + 
                         Affiliates + SEM + inc_LP_MA1 + inc_LP_MA2 + inc_LP_MA3 +
                         inc_PO_MA1 + inc_PO_MA2 + inc_PO_MA3 + list_price.2 +
                         list_price.3 + promotional_offer.1 + promotional_offer.3 + 
                         NPS.2 + holiday_freq.1 + holiday_freq.2 + holiday_freq.3 + 
                         gmv.1 + gmv.2, 
                     data = CameraDistLagData)
summary(DistLagModel_3)
# Multiple R-squared:  0.968,	Adjusted R-squared:  0.903
vif(DistLagModel_3)

# Removing Affiliates (high multicollinearity and low significance)
DistLagModel_4 <- lm(formula = gmv ~ list_price + MRP + SLA + Procurement_SLA + 
                         per_order + NPS + holiday_freq + TV + Digital + 
                         Sponsorship + ContentMarketing + OnlineMarketing + 
                         SEM + inc_LP_MA1 + inc_LP_MA2 + inc_LP_MA3 +
                         inc_PO_MA1 + inc_PO_MA2 + inc_PO_MA3 + list_price.2 +
                         list_price.3 + promotional_offer.1 + promotional_offer.3 + 
                         NPS.2 + holiday_freq.1 + holiday_freq.2 + holiday_freq.3 + 
                         gmv.1 + gmv.2, 
                     data = CameraDistLagData)
summary(DistLagModel_4)
# Multiple R-squared:  0.965,	Adjusted R-squared:  0.902
vif(DistLagModel_4)

# Removing inc_LP_MA3 (high multicollinearity and low significance)
DistLagModel_5 <- lm(formula = gmv ~ list_price + MRP + SLA + Procurement_SLA + 
                         per_order + NPS + holiday_freq + TV + Digital + 
                         Sponsorship + ContentMarketing + OnlineMarketing + 
                         SEM + inc_LP_MA1 + inc_LP_MA2 + inc_PO_MA1 + inc_PO_MA2 +
                         inc_PO_MA3 + list_price.2 + list_price.3 + 
                         promotional_offer.1 + promotional_offer.3 + NPS.2 + 
                         holiday_freq.1 + holiday_freq.2 + holiday_freq.3 + 
                         gmv.1 + gmv.2, 
                     data = CameraDistLagData)
summary(DistLagModel_5)
# Multiple R-squared:  0.964,	Adjusted R-squared:  0.904
vif(DistLagModel_5)

# Removing Procurement_SLA (high multicollinearity and low significance)
DistLagModel_6 <- lm(formula = gmv ~ list_price + MRP + SLA + 
                         per_order + NPS + holiday_freq + TV + Digital + 
                         Sponsorship + ContentMarketing + OnlineMarketing + 
                         SEM + inc_LP_MA1 + inc_LP_MA2 + inc_PO_MA1 + inc_PO_MA2 +
                         inc_PO_MA3 + list_price.2 + list_price.3 + 
                         promotional_offer.1 + promotional_offer.3 + NPS.2 + 
                         holiday_freq.1 + holiday_freq.2 + holiday_freq.3 + 
                         gmv.1 + gmv.2, 
                     data = CameraDistLagData)
summary(DistLagModel_6)
# Multiple R-squared:  0.962,	Adjusted R-squared:  0.904
vif(DistLagModel_6)

# Removing list_price (high multicollinearity and low significance)
DistLagModel_7 <- lm(formula = gmv ~ MRP + SLA + per_order + NPS + holiday_freq + 
                         TV + Digital + Sponsorship + ContentMarketing + 
                         OnlineMarketing + SEM + inc_LP_MA1 + inc_LP_MA2 + 
                         inc_PO_MA1 + inc_PO_MA2 + inc_PO_MA3 + list_price.2 +
                         list_price.3 + promotional_offer.1 + promotional_offer.3 +
                         NPS.2 + holiday_freq.1 + holiday_freq.2 + holiday_freq.3 + 
                         gmv.1 + gmv.2, 
                     data = CameraDistLagData)
summary(DistLagModel_7)
# Multiple R-squared:  0.96,	Adjusted R-squared:  0.905
vif(DistLagModel_7)

# Removing inc_PO_MA1 (high multicollinearity and low significance)
DistLagModel_8 <- lm(formula = gmv ~ MRP + SLA + per_order + NPS + holiday_freq + 
                         TV + Digital + Sponsorship + ContentMarketing + 
                         OnlineMarketing + SEM + inc_LP_MA1 + inc_LP_MA2 + 
                         inc_PO_MA2 + inc_PO_MA3 + list_price.2 + list_price.3 +
                         promotional_offer.1 + promotional_offer.3 + NPS.2 + 
                         holiday_freq.1 + holiday_freq.2 + holiday_freq.3 + 
                         gmv.1 + gmv.2, 
                     data = CameraDistLagData)
summary(DistLagModel_8)
# Multiple R-squared:  0.958,	Adjusted R-squared:  0.906
vif(DistLagModel_8)

# Removing holiday_freq.3 (high p-value)
DistLagModel_9 <- lm(formula = gmv ~ MRP + SLA + per_order + NPS + holiday_freq + 
                         TV + Digital + Sponsorship + ContentMarketing + 
                         OnlineMarketing + SEM + inc_LP_MA1 + inc_LP_MA2 + 
                         inc_PO_MA2 + inc_PO_MA3 + list_price.2 + list_price.3 +
                         promotional_offer.1 + promotional_offer.3 + NPS.2 + 
                         holiday_freq.1 + holiday_freq.2 + gmv.1 + gmv.2, 
                     data = CameraDistLagData)
summary(DistLagModel_9)
# Multiple R-squared:  0.957,	Adjusted R-squared:  0.908
vif(DistLagModel_9)

# Removing gmv.1 (high p-value)
DistLagModel_10 <- lm(formula = gmv ~ MRP + SLA + per_order + NPS + holiday_freq + 
                          TV + Digital + Sponsorship + ContentMarketing + 
                          OnlineMarketing + SEM + inc_LP_MA1 + inc_LP_MA2 + 
                          inc_PO_MA2 + inc_PO_MA3 + list_price.2 + list_price.3 +
                          promotional_offer.1 + promotional_offer.3 + NPS.2 + 
                          holiday_freq.1 + holiday_freq.2 + gmv.2, 
                      data = CameraDistLagData)
summary(DistLagModel_10)
# Multiple R-squared:  0.949,	Adjusted R-squared:  0.896
vif(DistLagModel_10)

# Removing inc_LP_MA1 (high multicollinearity and low significance)
DistLagModel_11 <- lm(formula = gmv ~ MRP + SLA + per_order + NPS + holiday_freq + 
                          TV + Digital + Sponsorship + ContentMarketing + 
                          OnlineMarketing + SEM + inc_LP_MA2 + 
                          inc_PO_MA2 + inc_PO_MA3 + list_price.2 + list_price.3 +
                          promotional_offer.1 + promotional_offer.3 + NPS.2 + 
                          holiday_freq.1 + holiday_freq.2 + gmv.2, 
                      data = CameraDistLagData)
summary(DistLagModel_11)
# Multiple R-squared:  0.935,	Adjusted R-squared:  0.874
vif(DistLagModel_11)

# Removing inc_PO_MA2 (high multicollinearity and low significance)
DistLagModel_12 <- lm(formula = gmv ~ MRP + SLA + per_order + NPS + holiday_freq + 
                          TV + Digital + Sponsorship + ContentMarketing + 
                          OnlineMarketing + SEM + inc_LP_MA2 + 
                          inc_PO_MA3 + list_price.2 + list_price.3 +
                          promotional_offer.1 + promotional_offer.3 + NPS.2 + 
                          holiday_freq.1 + holiday_freq.2 + gmv.2, 
                      data = CameraDistLagData)
summary(DistLagModel_12)
# Multiple R-squared:  0.919,	Adjusted R-squared:  0.848
vif(DistLagModel_12)

# Removing promotional_offer.3 (high p-value)
DistLagModel_13 <- lm(formula = gmv ~ MRP + SLA + per_order + NPS + holiday_freq + 
                          TV + Digital + Sponsorship + ContentMarketing + 
                          OnlineMarketing + SEM + inc_LP_MA2 + 
                          inc_PO_MA3 + list_price.2 + list_price.3 +
                          promotional_offer.1 + NPS.2 + 
                          holiday_freq.1 + holiday_freq.2 + gmv.2, 
                      data = CameraDistLagData)
summary(DistLagModel_13)
# Multiple R-squared:  0.914,	Adjusted R-squared:  0.845
vif(DistLagModel_13)

# Removing ContentMarketing (high multicollinearity and low significance)
DistLagModel_14 <- lm(formula = gmv ~ MRP + SLA + per_order + NPS + holiday_freq + 
                          TV + Digital + Sponsorship + OnlineMarketing + SEM +
                          inc_LP_MA2 + inc_PO_MA3 + list_price.2 + list_price.3 +
                          promotional_offer.1 + NPS.2 + holiday_freq.1 + 
                          holiday_freq.2 + gmv.2, 
                      data = CameraDistLagData)
summary(DistLagModel_14)
# Multiple R-squared:  0.908,	Adjusted R-squared:  0.841
vif(DistLagModel_14)

# Removing list_price.2 (high p-value)
DistLagModel_15 <- lm(formula = gmv ~ MRP + SLA + per_order + NPS + holiday_freq + 
                          TV + Digital + Sponsorship + OnlineMarketing + SEM +
                          inc_LP_MA2 + inc_PO_MA3 + list_price.3 +
                          promotional_offer.1 + NPS.2 + holiday_freq.1 + 
                          holiday_freq.2 + gmv.2, 
                      data = CameraDistLagData)
summary(DistLagModel_15)
# Multiple R-squared:  0.905,	Adjusted R-squared:  0.842
vif(DistLagModel_15)

# Removing NPS (high p-value)
DistLagModel_16 <- lm(formula = gmv ~ MRP + SLA + per_order + holiday_freq + 
                          TV + Digital + Sponsorship + OnlineMarketing + SEM +
                          inc_LP_MA2 + inc_PO_MA3 + list_price.3 +
                          promotional_offer.1 + NPS.2 + holiday_freq.1 + 
                          holiday_freq.2 + gmv.2, 
                      data = CameraDistLagData)
summary(DistLagModel_16)
# Multiple R-squared:  0.89,	Adjusted R-squared:  0.824
vif(DistLagModel_16)

# Removing per_order (high p-value)
DistLagModel_17 <- lm(formula = gmv ~ MRP + SLA + holiday_freq + TV + Digital +
                          Sponsorship + OnlineMarketing + SEM + inc_LP_MA2 + 
                          inc_PO_MA3 + list_price.3 + promotional_offer.1 + NPS.2 +
                          holiday_freq.1 + holiday_freq.2 + gmv.2, 
                      data = CameraDistLagData)
summary(DistLagModel_17)
# Multiple R-squared:  0.885,	Adjusted R-squared:  0.821
vif(DistLagModel_17)

# Removing holiday_freq (high p-value)
DistLagModel_18 <- lm(formula = gmv ~ MRP + SLA + TV + Digital +
                          Sponsorship + OnlineMarketing + SEM + inc_LP_MA2 + 
                          inc_PO_MA3 + list_price.3 + promotional_offer.1 + NPS.2 +
                          holiday_freq.1 + holiday_freq.2 + gmv.2, 
                      data = CameraDistLagData)
summary(DistLagModel_18)
# Multiple R-squared:  0.873,	Adjusted R-squared:  0.809
vif(DistLagModel_18)

# Removing holiday_freq.1 (high p-value)
DistLagModel_19 <- lm(formula = gmv ~ MRP + SLA + TV + Digital +
                          Sponsorship + OnlineMarketing + SEM + inc_LP_MA2 + 
                          inc_PO_MA3 + list_price.3 + promotional_offer.1 + NPS.2 +
                          holiday_freq.2 + gmv.2, 
                      data = CameraDistLagData)
summary(DistLagModel_19)
# Multiple R-squared:  0.846,	Adjusted R-squared:  0.777
vif(DistLagModel_19)

# Removing SLA (high p-value)
DistLagModel_20 <- lm(formula = gmv ~ MRP + TV + Digital + Sponsorship + 
                          OnlineMarketing + SEM + inc_LP_MA2 + inc_PO_MA3 + 
                          list_price.3 + promotional_offer.1 + NPS.2 +
                          holiday_freq.2 + gmv.2, 
                      data = CameraDistLagData)
summary(DistLagModel_20)
# Multiple R-squared:  0.822,	Adjusted R-squared:  0.749
vif(DistLagModel_20)

# Removing holiday_freq.2 (high p-value)
DistLagModel_21 <- lm(formula = gmv ~ MRP + TV + Digital + Sponsorship + 
                          OnlineMarketing + SEM + inc_LP_MA2 + inc_PO_MA3 + 
                          list_price.3 + promotional_offer.1 + NPS.2 + gmv.2, 
                      data = CameraDistLagData)
summary(DistLagModel_21)
# Multiple R-squared:  0.777,	Adjusted R-squared:  0.696
vif(DistLagModel_21)

# Removing gmv.2 (high p-value)
DistLagModel_22 <- lm(formula = gmv ~ MRP + TV + Digital + Sponsorship + 
                          OnlineMarketing + SEM + inc_LP_MA2 + inc_PO_MA3 + 
                          list_price.3 + promotional_offer.1 + NPS.2, 
                      data = CameraDistLagData)
summary(DistLagModel_22)
# Multiple R-squared:  0.722,	Adjusted R-squared:  0.632
vif(DistLagModel_22)

# Removing NPS.2 (high p-value)
DistLagModel_23 <- lm(formula = gmv ~ MRP + TV + Digital + Sponsorship + 
                          OnlineMarketing + SEM + inc_LP_MA2 + inc_PO_MA3 + 
                          list_price.3 + promotional_offer.1, 
                      data = CameraDistLagData)
summary(DistLagModel_23)
# Multiple R-squared:  0.694,	Adjusted R-squared:  0.606
vif(DistLagModel_23)

# Removing MRP (high p-value)
DistLagModel_24 <- lm(formula = gmv ~ TV + Digital + Sponsorship + 
                          OnlineMarketing + SEM + inc_LP_MA2 + inc_PO_MA3 + 
                          list_price.3 + promotional_offer.1, 
                      data = CameraDistLagData)
summary(DistLagModel_24)
# Multiple R-squared:  0.658,	Adjusted R-squared:  0.572
vif(DistLagModel_24)

# Removing Digital (high multicollinearity and low significance)
DistLagModel_25 <- lm(formula = gmv ~ TV + Sponsorship + OnlineMarketing + SEM +
                          inc_LP_MA2 + inc_PO_MA3 + list_price.3 + 
                          promotional_offer.1, 
                      data = CameraDistLagData)
summary(DistLagModel_25)
# Multiple R-squared:  0.596,	Adjusted R-squared:  0.508
vif(DistLagModel_25)

# Removing SEM (high p-value)
DistLagModel_26 <- lm(formula = gmv ~ TV + Sponsorship + OnlineMarketing +
                          inc_LP_MA2 + inc_PO_MA3 + list_price.3 + 
                          promotional_offer.1, 
                      data = CameraDistLagData)
summary(DistLagModel_26)
# Multiple R-squared:  0.554,	Adjusted R-squared:  0.472
vif(DistLagModel_26)

# Removing OnlineMarketing (high p-value)
DistLagModel_27 <- lm(formula = gmv ~ TV + Sponsorship + inc_LP_MA2 + inc_PO_MA3 +
                          list_price.3 + promotional_offer.1, 
                      data = CameraDistLagData)
summary(DistLagModel_27)
# Multiple R-squared:  0.531,	Adjusted R-squared:  0.459
vif(DistLagModel_27)

# Removing TV (high p-value)
DistLagModel_28 <- lm(formula = gmv ~ Sponsorship + inc_LP_MA2 + inc_PO_MA3 +
                          list_price.3 + promotional_offer.1, 
                      data = CameraDistLagData)
summary(DistLagModel_28)
# Multiple R-squared:  0.506,	Adjusted R-squared:  0.445
vif(DistLagModel_28)

# Removing inc_PO_MA3 (high p-value)
DistLagModel_29 <- lm(formula = gmv ~ Sponsorship + inc_LP_MA2 +
                          list_price.3 + promotional_offer.1, 
                      data = CameraDistLagData)
summary(DistLagModel_29)
# Multiple R-squared:  0.468,	Adjusted R-squared:  0.416
vif(DistLagModel_29)

# Removing any variable in DistLagModel_29 reduces the Adjusted R-squared considerably
# Hence, DistLagModel_29 can be considered as the final model

CameraDistLagModel <- DistLagModel_29
# 4 variables: Sponsorship, inc_LP_MA2, list_price.3, promotional_offer.1

######################################################################################
# Cross Validation
# Calculate K-fold cross-validation, for K=10.
crossval <- cv.lm(data = CameraDistLagData, 
                  form.lm = formula(gmv ~ Sponsorship + inc_LP_MA2 + list_price.3 + promotional_offer.1), m = 10)

CrossVal_Camera[4] <- attr(crossval, "ms")

# Camera Accessories Distributed Lag Model Elasticity Analysis
trainingSet <- CameraDistLagData

# Camera Accessories Distributed Lag Model Elasticity coefficients
getCameraDistLagElasticity <- function(var){
    val <- as.numeric(CameraDistLagModel$coefficients[var] * 
                         mean(trainingSet[, var]) / mean(trainingSet$gmv))
    return(val)
}

cameraVarList <- list()

for(i in 2:length(CameraDistLagModel$coefficients)){
    cameraVarList[i-1] <- getCameraDistLagElasticity(names(CameraDistLagModel$coefficients)[i])
}

cameraDistLagModelElasticity <- data.frame(names(
    CameraDistLagModel$coefficients[2:length(CameraDistLagModel$coefficients)]))

cameraDistLagModelElasticity <- cbind(cameraDistLagModelElasticity, 
                                      do.call(rbind.data.frame, cameraVarList))

colnames(cameraDistLagModelElasticity) <- c("Variable", "Elasticity")

cameraDistLagModelElasticity$direction <- ifelse(
    cameraDistLagModelElasticity$Elasticity > 0, "Positive", "Negative")

ggplot(data = cameraDistLagModelElasticity,
       aes(x = reorder(Variable, Elasticity), y = Elasticity)) +
    geom_bar(position = "dodge", stat = "identity") + 
    coord_flip() +
    ggtitle("Camera Accessories - Distributed Lag Model") + xlab("Variables")

#####################################################################################

####################################################################################
#    :::::::: Multiplicative + Distributed Lag Model - Camera Accessory ::::::::
####################################################################################

colnames(CameraAccessory_final)
# Removing week_year, holiday_freq, Moving Averages and derived price category variables
# Removing the following columns for the initial Multiplicative + Distributed Lag Model:
# week_year, Mass_p, Middle_p, Premium_p, holiday_freq,
# inc_LP_MA1, inc_LP_MA2, inc_LP_MA3,
# inc_PO_MA1, inc_PO_MA2, inc_PO_MA3,
# holiday_freq-1, holiday_freq-2, holiday_freq-3
CameraMultiDistLagData <- CameraAccessory_final[, -c(1, 10:13, 22:27, 37:39)]

# Lag GMV by 1 Time Unit
CameraMultiDistLagData_1 <- slide(CameraMultiDistLagData, Var = "gmv", slideBy = -1)

# Lag GMV by 2 Time Units
CameraMultiDistLagData_1 <- slide(CameraMultiDistLagData_1, Var = "gmv", slideBy = -2)

# Lag GMV by 3 Time Units
CameraMultiDistLagData_1 <- slide(CameraMultiDistLagData_1, Var = "gmv", slideBy = -3)

# Remove NAs
CameraMultiDistLagData <- na.omit(CameraMultiDistLagData_1)

# Data Treatment
CameraMultiDistLagData$ContentMarketing[which(CameraMultiDistLagData$ContentMarketing==0)] <- 1
CameraMultiDistLagData$per_order[which(CameraMultiDistLagData$per_order==0)] <- 0.01

# Applying Logarithm
CameraMultiDistLagData <- log(CameraMultiDistLagData)

# Updating column names
colnames(CameraMultiDistLagData)[17:28] <- c("list_price.1", "list_price.2", "list_price.3", "promotional_offer.1", "promotional_offer.2", "promotional_offer.3", "NPS.1", "NPS.2", "NPS.3", "gmv.1", "gmv.2", "gmv.3")

# Build the Initial Multiplicative + Distributed Lag Model
MultiDistLagModel_1 <- lm(gmv ~ ., CameraMultiDistLagData)
summary(MultiDistLagModel_1)
# Multiple R-squared:  0.965,	Adjusted R-squared:  0.913

MultiDistLagModel_2 <- stepAIC(MultiDistLagModel_1, direction = "both")
summary(MultiDistLagModel_2)
# Multiple R-squared:  0.963,	Adjusted R-squared:  0.944

MultiDistLagModel_3 <- lm(formula = gmv ~ list_price + Units + promotional_offer +
                              Procurement_SLA + NPS + TV + Digital + OnlineMarketing +
                              Affiliates + SEM + list_price.2 + promotional_offer.1 +
                              promotional_offer.2 + NPS.1 + gmv.1, 
                          data = CameraMultiDistLagData)
summary(MultiDistLagModel_3)
# Multiple R-squared:  0.963,	Adjusted R-squared:  0.944
vif(MultiDistLagModel_3)

# Removing TV (high p-value)
MultiDistLagModel_4 <- lm(formula = gmv ~ list_price + Units + promotional_offer +
                              Procurement_SLA + NPS + Digital + OnlineMarketing +
                              Affiliates + SEM + list_price.2 + promotional_offer.1 +
                              promotional_offer.2 + NPS.1 + gmv.1, 
                          data = CameraMultiDistLagData)
summary(MultiDistLagModel_4)
# Multiple R-squared:  0.961,	Adjusted R-squared:  0.944
vif(MultiDistLagModel_4)

# Removing Units (high p-value)
MultiDistLagModel_5 <- lm(formula = gmv ~ list_price + promotional_offer +
                              Procurement_SLA + NPS + Digital + OnlineMarketing +
                              Affiliates + SEM + list_price.2 + promotional_offer.1 +
                              promotional_offer.2 + NPS.1 + gmv.1, 
                          data = CameraMultiDistLagData)
summary(MultiDistLagModel_5)
# Multiple R-squared:  0.96,	Adjusted R-squared:  0.943
vif(MultiDistLagModel_5)

# Removing NPS.1 (high multicollinearity and low significance)
MultiDistLagModel_6 <- lm(formula = gmv ~ list_price + promotional_offer +
                              Procurement_SLA + NPS + Digital + OnlineMarketing +
                              Affiliates + SEM + list_price.2 + promotional_offer.1 +
                              promotional_offer.2 + gmv.1, 
                          data = CameraMultiDistLagData)
summary(MultiDistLagModel_6)
# Multiple R-squared:  0.957,	Adjusted R-squared:  0.941
vif(MultiDistLagModel_6)

# Removing promotional_offer.2 (high p-value)
MultiDistLagModel_7 <- lm(formula = gmv ~ list_price + promotional_offer +
                              Procurement_SLA + NPS + Digital + OnlineMarketing +
                              Affiliates + SEM + list_price.2 + promotional_offer.1 +
                              gmv.1, 
                          data = CameraMultiDistLagData)
summary(MultiDistLagModel_7)
# Multiple R-squared:  0.956,	Adjusted R-squared:  0.941
vif(MultiDistLagModel_7)

# Removing NPS (high p-value)
MultiDistLagModel_8 <- lm(formula = gmv ~ list_price + promotional_offer +
                              Procurement_SLA + Digital + OnlineMarketing +
                              Affiliates + SEM + list_price.2 + promotional_offer.1 +
                              gmv.1, 
                          data = CameraMultiDistLagData)
summary(MultiDistLagModel_8)
# Multiple R-squared:  0.955,	Adjusted R-squared:  0.942
vif(MultiDistLagModel_8)

# Removing promotional_offer.1 (high p-value)
MultiDistLagModel_9 <- lm(formula = gmv ~ list_price + promotional_offer +
                              Procurement_SLA + Digital + OnlineMarketing +
                              Affiliates + SEM + list_price.2 + gmv.1, 
                          data = CameraMultiDistLagData)
summary(MultiDistLagModel_9)
# Multiple R-squared:  0.952,	Adjusted R-squared:  0.94
vif(MultiDistLagModel_9)

# Removing list_price (high p-value)
MultiDistLagModel_10 <- lm(formula = gmv ~ promotional_offer +
                               Procurement_SLA + Digital + OnlineMarketing +
                               Affiliates + SEM + list_price.2 + gmv.1, 
                           data = CameraMultiDistLagData)
summary(MultiDistLagModel_10)
# Multiple R-squared:  0.949,	Adjusted R-squared:  0.938
vif(MultiDistLagModel_10)

# Removing list_price.2 (high p-value)
MultiDistLagModel_11 <- lm(formula = gmv ~ promotional_offer + Procurement_SLA +
                               Digital + OnlineMarketing + Affiliates + SEM + gmv.1, 
                           data = CameraMultiDistLagData)
summary(MultiDistLagModel_11)
# Multiple R-squared:  0.941,	Adjusted R-squared:  0.931
vif(MultiDistLagModel_11)

# Removing promotional_offer (high p-value)
MultiDistLagModel_12 <- lm(formula = gmv ~ Procurement_SLA + Digital + 
                               OnlineMarketing + Affiliates + SEM + gmv.1, 
                           data = CameraMultiDistLagData)
summary(MultiDistLagModel_12)
# Multiple R-squared:  0.929,	Adjusted R-squared:  0.918
vif(MultiDistLagModel_12)

# Removing Digital (high p-value)
MultiDistLagModel_13 <- lm(formula = gmv ~ Procurement_SLA + 
                               OnlineMarketing + Affiliates + SEM + gmv.1, 
                           data = CameraMultiDistLagData)
summary(MultiDistLagModel_13)
# Multiple R-squared:  0.917,	Adjusted R-squared:  0.907
vif(MultiDistLagModel_13)

# Removing Affiliates (high multicollinearity and low significance)
MultiDistLagModel_14 <- lm(formula = gmv ~ Procurement_SLA + OnlineMarketing + 
                               SEM + gmv.1, 
                           data = CameraMultiDistLagData)
summary(MultiDistLagModel_14)
# Multiple R-squared:  0.894,	Adjusted R-squared:  0.884
vif(MultiDistLagModel_14)

# Removing OnlineMarketing (high p-value)
MultiDistLagModel_15 <- lm(formula = gmv ~ Procurement_SLA + SEM + gmv.1, 
                           data = CameraMultiDistLagData)
summary(MultiDistLagModel_15)
# Multiple R-squared:  0.885,	Adjusted R-squared:  0.876
vif(MultiDistLagModel_15)

# No further variables can be removed from MultiDistLagModel_15
# Hence, MultiDistLagModel_15 is the final model

CameraMultiDistLagModel <- MultiDistLagModel_15
# 3 variables: Procurement_SLA, SEM, gmv.1
# Multiple R-squared:  0.885,	Adjusted R-squared:  0.876

######################################################################################
# Cross Validation
# Calculate K-fold cross-validation, for K=10.
crossval <- cv.lm(data = CameraMultiDistLagData, 
                  form.lm = formula(gmv ~ Procurement_SLA + SEM + gmv.1), m = 10)

CrossVal_Camera[5] <- attr(crossval, "ms")

# Camera Accessories Multiplicative + Distributed Lag Model Elasticity Analysis
trainingSet <- CameraMultiDistLagData

# Camera Accessories Multiplicative + Distributed Lag Model Elasticity coefficients
getCameraMultiDistLagElasticity <- function(var){
    val <-as.numeric(CameraMultiDistLagModel$coefficients[var] * 
                         mean(trainingSet[, var]) / mean(trainingSet$gmv))
    return(val)
} 

cameraVarList <- list()

for(i in 2:length(CameraMultiDistLagModel$coefficients)){
    cameraVarList[i-1] <- getCameraMultiDistLagElasticity(names(CameraMultiDistLagModel$coefficients)[i])
}

cameraMultiDistLagModelElasticity <- data.frame(names(
    CameraMultiDistLagModel$coefficients[2:length(CameraMultiDistLagModel$coefficients)]))

cameraMultiDistLagModelElasticity <- cbind(cameraMultiDistLagModelElasticity, 
                                           do.call(rbind.data.frame, cameraVarList))

colnames(cameraMultiDistLagModelElasticity) <- c("Variable", "Elasticity")

cameraMultiDistLagModelElasticity$direction <- ifelse(
    cameraMultiDistLagModelElasticity$Elasticity > 0, "Positive", "Negative")

ggplot(data = cameraMultiDistLagModelElasticity,
       aes(x = reorder(Variable, Elasticity), y = Elasticity)) +
    geom_bar(position = "dodge", stat = "identity") + 
    coord_flip() +
    ggtitle("Camera Accessories - Multiplicative + Distributed Lag Model") + xlab("Variables")

#####################################################################################
