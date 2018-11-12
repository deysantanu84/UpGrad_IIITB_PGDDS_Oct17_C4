####################################################################################
#                  :::::::: Ecommerce Capstone Solution ::::::::
####################################################################################
####################################################################################
#                  :::::::: Market Mix Modelling :::::::::::::::
####################################################################################
####################################################################################
#                  :::::::: Model Building - Home Audio ::::::::
####################################################################################

CrossVal_HomeAudio <- rep(0,5)

####################################################################################
#                  :::::::: Basic Linear Model - Home Audio ::::::::
####################################################################################

colnames(HomeAudio_final)
# Removing week_year, Lag variables and derived price category variables
# Removing the following columns for the initial Basic Linear Model:
# week_year, Mass_p, Middle_p, Premium_p,
# list_price-1, list_price-2, list_price-3,
# promotional_offer-1, promotional_offer-2, promotional_offer-3
# NPS-1, NPS-2, NPS-3
# holiday_freq-1, holiday_freq-2, holiday_freq-3
AudioLinearData <- HomeAudio_final[, -c(1, 10:12, 28:39)]

# Scaling
AudioLinearData <- data.frame(scale(AudioLinearData))

# Initial Linear Model
LinearModel_1 <- lm(gmv ~ ., AudioLinearData)
summary(LinearModel_1)
# Multiple R-squared:  0.6957,	Adjusted R-squared:  0.428

LinearModel_2 <- stepAIC(LinearModel_1, direction = "both")
summary(LinearModel_2)
# Multiple R-squared:  0.6648,	Adjusted R-squared:  0.5366

LinearModel_3 <- lm(formula = gmv ~ list_price + Units + SLA + Procurement_SLA + 
                        NPS + TV + Sponsorship + ContentMarketing + OnlineMarketing + 
                        Affiliates + SEM + inc_LP_MA3 + inc_PO_MA3, 
                    data = AudioLinearData)
summary(LinearModel_3)
# Multiple R-squared:  0.6648,	Adjusted R-squared:  0.5366
vif(LinearModel_3)

# Removing inc_LP_MA3 (high p-value)
LinearModel_4 <- lm(formula = gmv ~ list_price + Units + SLA + Procurement_SLA + 
                        NPS + TV + Sponsorship + ContentMarketing + OnlineMarketing + 
                        Affiliates + SEM + inc_PO_MA3, 
                    data = AudioLinearData)
summary(LinearModel_4)
# Multiple R-squared:  0.6471,	Adjusted R-squared:  0.5261
vif(LinearModel_4)

# Removing NPS (high p-value)
LinearModel_5 <- lm(formula = gmv ~ list_price + Units + SLA + Procurement_SLA + 
                        TV + Sponsorship + ContentMarketing + OnlineMarketing + 
                        Affiliates + SEM + inc_PO_MA3, 
                    data = AudioLinearData)
summary(LinearModel_5)
# Multiple R-squared:  0.6214,	Adjusted R-squared:  0.5058
vif(LinearModel_5)

# Removing Units (high p-value)
LinearModel_6 <- lm(formula = gmv ~ list_price + SLA + Procurement_SLA + 
                        TV + Sponsorship + ContentMarketing + OnlineMarketing + 
                        Affiliates + SEM + inc_PO_MA3, 
                    data = AudioLinearData)
summary(LinearModel_6)
# Multiple R-squared:  0.5864,	Adjusted R-squared:  0.4747
vif(LinearModel_6)

# Removing Procurement_SLA (high p-value)
LinearModel_7 <- lm(formula = gmv ~ list_price + SLA + TV + Sponsorship + 
                        ContentMarketing + OnlineMarketing + Affiliates + SEM +
                        inc_PO_MA3, 
                    data = AudioLinearData)
summary(LinearModel_7)
# Multiple R-squared:  0.5566,	Adjusted R-squared:  0.4516
vif(LinearModel_7)

# Removing SLA (high p-value)
LinearModel_8 <- lm(formula = gmv ~ list_price + TV + Sponsorship + 
                        ContentMarketing + OnlineMarketing + Affiliates + SEM +
                        inc_PO_MA3, 
                    data = AudioLinearData)
summary(LinearModel_8)
# Multiple R-squared:  0.5209,	Adjusted R-squared:  0.4227
vif(LinearModel_8)

# Removing list_price (high p-value)
LinearModel_9 <- lm(formula = gmv ~ TV + Sponsorship + ContentMarketing + 
                        OnlineMarketing + Affiliates + SEM + inc_PO_MA3, 
                    data = AudioLinearData)
summary(LinearModel_9)
# Multiple R-squared:  0.5104,	Adjusted R-squared:  0.4247
vif(LinearModel_9)

# Removing TV (high p-value)
LinearModel_10 <- lm(formula = gmv ~ Sponsorship + ContentMarketing + 
                         OnlineMarketing + Affiliates + SEM + inc_PO_MA3, 
                     data = AudioLinearData)
summary(LinearModel_10)
# Multiple R-squared:  0.4872,	Adjusted R-squared:  0.4122
vif(LinearModel_10)

# Removing Affiliates (high multicollinearity and low significance)
LinearModel_11 <- lm(formula = gmv ~ Sponsorship + ContentMarketing + 
                         OnlineMarketing + SEM + inc_PO_MA3, 
                     data = AudioLinearData)
summary(LinearModel_11)
# Multiple R-squared:  0.4557,	Adjusted R-squared:  0.3909
vif(LinearModel_11)

# Removing OnlineMarketing (high p-value)
LinearModel_12 <- lm(formula = gmv ~ Sponsorship + ContentMarketing + SEM + 
                         inc_PO_MA3, 
                     data = AudioLinearData)
summary(LinearModel_12)
# Multiple R-squared:  0.4524,	Adjusted R-squared:  0.4014
vif(LinearModel_12)

# Removing SEM (high multicollinearity and low significance)
LinearModel_13 <- lm(formula = gmv ~ Sponsorship + ContentMarketing + inc_PO_MA3, 
                     data = AudioLinearData)
summary(LinearModel_13)
# Multiple R-squared:  0.4189,	Adjusted R-squared:  0.3793
vif(LinearModel_13)

# Removing Sponsorship (high p-value)
LinearModel_14 <- lm(formula = gmv ~ ContentMarketing + inc_PO_MA3, 
                     data = AudioLinearData)
summary(LinearModel_14)
# Multiple R-squared:  0.3864,	Adjusted R-squared:  0.3591
vif(LinearModel_14)

# We will consider LinearModel_14 as the final model
AudioLinearModel <- LinearModel_14
# 2 variables: ContentMarketing, inc_PO_MA3
# Multiple R-squared:  0.3864,	Adjusted R-squared:  0.3591

######################################################################################
# Cross Validation
# Calculate K-fold cross-validation, for K=10.
crossval <- cv.lm(data = AudioLinearData, 
                  form.lm = formula(gmv ~ ContentMarketing + inc_PO_MA3), m = 10)

CrossVal_HomeAudio[1] <- attr(crossval, "ms")

# Home Audio Linear Model Elasticity Analysis
trainingSet <- AudioLinearData

# Home Audio Linear Model Elasticity coefficients
getAudioElasticity <- function(var){
    val <-as.numeric(AudioLinearModel$coefficients[var] * 
                         mean(trainingSet[, var]) / mean(trainingSet$gmv))
    return(val)
}

audioVarList <- list()

for(i in 2:length(AudioLinearModel$coefficients)){
    audioVarList[i-1] <- getAudioElasticity(names(AudioLinearModel$coefficients)[i])
}

audioLinearModelElasticity <- data.frame(names(
    AudioLinearModel$coefficients[2:length(AudioLinearModel$coefficients)]))

audioLinearModelElasticity <- cbind(audioLinearModelElasticity, 
                                    do.call(rbind.data.frame, audioVarList))

colnames(audioLinearModelElasticity) <- c("Variable", "Elasticity")

audioLinearModelElasticity$direction <- ifelse(
    audioLinearModelElasticity$Elasticity > 0, "Positive", "Negative")

ggplot(data = audioLinearModelElasticity, 
       aes(x = reorder(Variable, Elasticity), y = Elasticity)) +
    geom_bar(position = "dodge", stat = "identity") + 
    coord_flip() +
    ggtitle("Home Audio - Linear Model") + xlab("Variables")

#####################################################################################

####################################################################################
#           :::::::: Multiplicative Model - Home Audio ::::::::
####################################################################################

colnames(HomeAudio_final)
# Removing Moving Averages, Lag and derived price category variables
# Removing the following columns for the initial Multiplicative Model:
# week_year, Mass_p, Middle_p, Premium_p, holiday_freq,
# inc_LP_MA1, inc_LP_MA2, inc_LP_MA3,
# inc_PO_MA1, inc_PO_MA2, inc_PO_MA3
# list_price-1, list_price-2, list_price-3,
# promotional_offer-1, promotional_offer-2, promotional_offer-3
# NPS-1, NPS-2, NPS-3
# holiday_freq-1, holiday_freq-2, holiday_freq-3
AudioMultiData <- HomeAudio_final[, -c(1, 10:13, 22:39)]

# Treatment for Multiplicative Modelling
AudioMultiData$ContentMarketing[which(AudioMultiData$ContentMarketing==0)] <- 0.01
AudioMultiData$per_order[which(AudioMultiData$per_order==0)] <- 0.01
AudioMultiData <- log(AudioMultiData)

# Build the initial Multiplicative Model
multiModel_1 <- lm(gmv ~ ., AudioMultiData)
summary(multiModel_1)
# Multiple R-squared:  0.456,	Adjusted R-squared:  0.201

multiModel_2 <- stepAIC(multiModel_1, direction = "both")
summary(multiModel_2)
# Multiple R-squared:  0.415,	Adjusted R-squared:  0.312

multiModel_3 <- lm(formula = gmv ~ list_price + MRP + promotional_offer + NPS + 
                       TV + OnlineMarketing + Affiliates, 
                   data = AudioMultiData)
summary(multiModel_3)
# Multiple R-squared:  0.415,	Adjusted R-squared:  0.312
vif(multiModel_3)

# Removing promotional_offer (high multicollinearity and low significance)
multiModel_4 <- lm(formula = gmv ~ list_price + MRP + NPS + 
                       TV + OnlineMarketing + Affiliates, 
                   data = AudioMultiData)
summary(multiModel_4)
# Multiple R-squared:  0.382,	Adjusted R-squared:  0.291
vif(multiModel_4)

# Removing list_price (high p-value)
multiModel_5 <- lm(formula = gmv ~ MRP + NPS + TV + OnlineMarketing + Affiliates, 
                   data = AudioMultiData)
summary(multiModel_5)
# Multiple R-squared:  0.344,	Adjusted R-squared:  0.266
vif(multiModel_5)

# Removing NPS (high p-value)
multiModel_6 <- lm(formula = gmv ~ MRP + TV + OnlineMarketing + Affiliates, 
                   data = AudioMultiData)
summary(multiModel_6)
# Multiple R-squared:  0.312,	Adjusted R-squared:  0.248
vif(multiModel_6)

# Removing any further variable from multiModel_6 reduces the adjusted R-squared significantly.
# Hence, we will consider multiModel_6 as the final model
AudioMultiModel <- multiModel_6
# 4 variables: MRP, TV, OnlineMarketing, Affiliates
# Multiple R-squared:  0.312,	Adjusted R-squared:  0.248

######################################################################################
# Cross Validation
# Calculate K-fold cross-validation, for K=10.
crossval <- cv.lm(data = AudioMultiData, 
                  form.lm = formula(gmv ~ MRP + TV + OnlineMarketing + Affiliates), m = 10)

CrossVal_HomeAudio[2] <- attr(crossval, "ms")

# Home Audio Multiplicative Model Elasticity Analysis
trainingSet <- AudioMultiData

# Home Audio Multiplicative Model Elasticity coefficients
getAudioMultiElasticity <- function(var){
    val <-as.numeric(AudioMultiModel$coefficients[var] * 
                         mean(trainingSet[, var]) / mean(trainingSet$gmv))
    return(val)
} 

audioVarList <- list()

for(i in 2:length(AudioMultiModel$coefficients)){
    audioVarList[i-1] <- getAudioMultiElasticity(names(AudioMultiModel$coefficients)[i])
}

audioMultiModelElasticity <- data.frame(names(
    AudioMultiModel$coefficients[2:length(AudioMultiModel$coefficients)]))

audioMultiModelElasticity <- cbind(audioMultiModelElasticity, 
                                   do.call(rbind.data.frame, audioVarList))

colnames(audioMultiModelElasticity) <- c("Variable", "Elasticity")

audioMultiModelElasticity$direction <- ifelse(
    audioMultiModelElasticity$Elasticity > 0, "Positive", "Negative")

ggplot(data = audioMultiModelElasticity,
       aes(x = reorder(Variable, Elasticity), y = Elasticity)) +
    geom_bar(position = "dodge", stat = "identity") + 
    coord_flip() +
    ggtitle("Home Audio - Multiplicative Model") + xlab("Variables")

#####################################################################################

####################################################################################
#                  :::::::: Koyck Model - Home Audio ::::::::
####################################################################################

colnames(HomeAudio_final)
# Removing week_year, Lag variables and derived price category variables
# Removing the following columns for the initial Koyck Model:
# week_year, Mass_p, Middle_p, Premium_p,
# list_price-1, list_price-2, list_price-3,
# promotional_offer-1, promotional_offer-2, promotional_offer-3
# NPS-1, NPS-2, NPS-3
# holiday_freq-1, holiday_freq-2, holiday_freq-3
AudioKoyckData <- HomeAudio_final[, -c(1, 10:12, 28:39)]

# Lag GMV by 1 Unit
AudioKoyckData <- slide(AudioKoyckData, Var = "gmv", slideBy = -1)

# Remove NAs and perform scaling
AudioKoyckData <- data.frame(scale(na.omit(AudioKoyckData)))

# Build initial Koyck model
koyckModel_1 <- lm(gmv ~ ., AudioKoyckData)
summary(koyckModel_1)
# Multiple R-squared:  0.753,	Adjusted R-squared:  0.507

koyckModel_2 <- stepAIC(koyckModel_1, direction = "both")
summary(koyckModel_2)
# Multiple R-squared:  0.739,	Adjusted R-squared:  0.571

koyckModel_3 <- lm(formula = gmv ~ list_price + MRP + Units + SLA + Procurement_SLA + 
                       NPS + holiday_freq + TV + Digital + Sponsorship + 
                       ContentMarketing + OnlineMarketing + Affiliates + SEM +
                       inc_LP_MA2 + inc_LP_MA3 + inc_PO_MA2 + inc_PO_MA3, 
                   data = AudioKoyckData)
summary(koyckModel_3)
# Multiple R-squared:  0.739,	Adjusted R-squared:  0.571
vif(koyckModel_3)

# Removing Digital (high multicollinearity and low signifiance)
koyckModel_4 <- lm(formula = gmv ~ list_price + MRP + Units + SLA + Procurement_SLA + 
                       NPS + holiday_freq + TV + Sponsorship + 
                       ContentMarketing + OnlineMarketing + Affiliates + SEM +
                       inc_LP_MA2 + inc_LP_MA3 + inc_PO_MA2 + inc_PO_MA3, 
                   data = AudioKoyckData)
summary(koyckModel_4)
# Multiple R-squared:  0.725,	Adjusted R-squared:  0.564
vif(koyckModel_4)

# Removing MRP (high multicollinearity and low signifiance)
koyckModel_5 <- lm(formula = gmv ~ list_price + Units + SLA + Procurement_SLA + 
                       NPS + holiday_freq + TV + Sponsorship + 
                       ContentMarketing + OnlineMarketing + Affiliates + SEM +
                       inc_LP_MA2 + inc_LP_MA3 + inc_PO_MA2 + inc_PO_MA3, 
                   data = AudioKoyckData)
summary(koyckModel_5)
# Multiple R-squared:  0.705,	Adjusted R-squared:  0.547
vif(koyckModel_5)

# Removing inc_LP_MA2 (high multicollinearity and low significance)
koyckModel_6 <- lm(formula = gmv ~ list_price + Units + SLA + Procurement_SLA + 
                       NPS + holiday_freq + TV + Sponsorship + 
                       ContentMarketing + OnlineMarketing + Affiliates + SEM +
                       inc_LP_MA3 + inc_PO_MA2 + inc_PO_MA3, 
                   data = AudioKoyckData)
summary(koyckModel_6)
# Multiple R-squared:  0.693,	Adjusted R-squared:  0.544
vif(koyckModel_6)

# Removing inc_PO_MA2 (high p-value)
koyckModel_7 <- lm(formula = gmv ~ list_price + Units + SLA + Procurement_SLA + 
                       NPS + holiday_freq + TV + Sponsorship + 
                       ContentMarketing + OnlineMarketing + Affiliates + SEM +
                       inc_LP_MA3 + inc_PO_MA3, 
                   data = AudioKoyckData)
summary(koyckModel_7)
# Multiple R-squared:  0.687,	Adjusted R-squared:  0.55
vif(koyckModel_7)

# Removing holiday_freq (high p-value)
koyckModel_8 <- lm(formula = gmv ~ list_price + Units + SLA + Procurement_SLA + 
                       NPS + TV + Sponsorship + ContentMarketing + OnlineMarketing +
                       Affiliates + SEM + inc_LP_MA3 + inc_PO_MA3, 
                   data = AudioKoyckData)
summary(koyckModel_8)
# Multiple R-squared:  0.674,	Adjusted R-squared:  0.546
vif(koyckModel_8)

# Removing inc_LP_MA3 (high p-value)
koyckModel_9 <- lm(formula = gmv ~ list_price + Units + SLA + Procurement_SLA + 
                       NPS + TV + Sponsorship + ContentMarketing + OnlineMarketing +
                       Affiliates + SEM + inc_PO_MA3, 
                   data = AudioKoyckData)
summary(koyckModel_9)
# Multiple R-squared:  0.648,	Adjusted R-squared:  0.524
vif(koyckModel_9)

# Removing NPS (high p-value)
koyckModel_10 <- lm(formula = gmv ~ list_price + Units + SLA + Procurement_SLA + 
                        TV + Sponsorship + ContentMarketing + OnlineMarketing +
                        Affiliates + SEM + inc_PO_MA3, 
                    data = AudioKoyckData)
summary(koyckModel_10)
# Multiple R-squared:  0.622,	Adjusted R-squared:  0.503
vif(koyckModel_10)

# Removing Units (high p-value)
koyckModel_11 <- lm(formula = gmv ~ list_price + SLA + Procurement_SLA + 
                        TV + Sponsorship + ContentMarketing + OnlineMarketing +
                        Affiliates + SEM + inc_PO_MA3, 
                    data = AudioKoyckData)
summary(koyckModel_11)
# Multiple R-squared:  0.586,	Adjusted R-squared:  0.472
vif(koyckModel_11)

# Removing Procurement_SLA (high p-value)
koyckModel_12 <- lm(formula = gmv ~ list_price + SLA + TV + Sponsorship + 
                        ContentMarketing + OnlineMarketing + Affiliates + SEM +
                        inc_PO_MA3, 
                    data = AudioKoyckData)
summary(koyckModel_12)
# Multiple R-squared:  0.557,	Adjusted R-squared:  0.449
vif(koyckModel_12)

# Removing SLA (high p-value)
koyckModel_13 <- lm(formula = gmv ~ list_price + TV + Sponsorship + 
                        ContentMarketing + OnlineMarketing + Affiliates + SEM +
                        inc_PO_MA3, 
                    data = AudioKoyckData)
summary(koyckModel_13)
# Multiple R-squared:  0.523,	Adjusted R-squared:  0.423
vif(koyckModel_13)

# Removing list_price (high p-value)
koyckModel_14 <- lm(formula = gmv ~ TV + Sponsorship + ContentMarketing + 
                        OnlineMarketing + Affiliates + SEM + inc_PO_MA3, 
                    data = AudioKoyckData)
summary(koyckModel_14)
# Multiple R-squared:  0.512,	Adjusted R-squared:  0.424
vif(koyckModel_14)

# Removing TV (high p-value)
koyckModel_15 <- lm(formula = gmv ~ Sponsorship + ContentMarketing + 
                        OnlineMarketing + Affiliates + SEM + inc_PO_MA3, 
                    data = AudioKoyckData)
summary(koyckModel_15)
# Multiple R-squared:  0.487,	Adjusted R-squared:  0.41
vif(koyckModel_15)

# Removing Affiliates (high multicollinearity and low significance)
koyckModel_16 <- lm(formula = gmv ~ Sponsorship + ContentMarketing + 
                        OnlineMarketing + SEM + inc_PO_MA3, 
                    data = AudioKoyckData)
summary(koyckModel_16)
# Multiple R-squared:  0.456,	Adjusted R-squared:  0.389
vif(koyckModel_16)

# Removing OnlineMarketing (high p-value)
koyckModel_17 <- lm(formula = gmv ~ Sponsorship + ContentMarketing + SEM + inc_PO_MA3,
                    data = AudioKoyckData)
summary(koyckModel_17)
# Multiple R-squared:  0.453,	Adjusted R-squared:   0.4
vif(koyckModel_17)

# Removing SEM (high multicollinearity and low significance)
koyckModel_18 <- lm(formula = gmv ~ Sponsorship + ContentMarketing + inc_PO_MA3,
                    data = AudioKoyckData)
summary(koyckModel_18)
# Multiple R-squared:  0.42,	Adjusted R-squared:  0.38
vif(koyckModel_18)

# Removing SEM (high multicollinearity and low significance)
koyckModel_19 <- lm(formula = gmv ~ ContentMarketing + inc_PO_MA3,
                    data = AudioKoyckData)
summary(koyckModel_19)
# Multiple R-squared:  0.389,	Adjusted R-squared:  0.362
vif(koyckModel_19)
# We will consider koyckModel_19 as the final model

AudioKoyckModel <- koyckModel_19
# Multiple R-squared:  0.389,	Adjusted R-squared:  0.362
# 2 variables: ContentMarketing and inc_PO_MA3

######################################################################################
# Cross Validation
# Calculate K-fold cross-validation, for K=10.
crossval <- cv.lm(data = AudioKoyckData, 
                  form.lm = formula(gmv ~ ContentMarketing + inc_PO_MA3), m = 10)

CrossVal_HomeAudio[3] <- attr(crossval, "ms")

# Home Audio Koyck Model Elasticity Analysis
trainingSet <- AudioKoyckData

# Home Audio Koyck Model Elasticity coefficients
getAudioKoyckElasticity <- function(var){
    val <-as.numeric(AudioKoyckModel$coefficients[var] * 
                         mean(trainingSet[, var]) / mean(trainingSet$gmv))
    return(val)
} 

audioVarList <- list()

for(i in 2:length(AudioKoyckModel$coefficients)){
    audioVarList[i-1] <- getAudioKoyckElasticity(names(AudioKoyckModel$coefficients)[i])
}

audioKoyckModelElasticity <- data.frame(names(
    AudioKoyckModel$coefficients[2:length(AudioKoyckModel$coefficients)]))

audioKoyckModelElasticity <- cbind(audioKoyckModelElasticity, 
                                   do.call(rbind.data.frame, audioVarList))

colnames(audioKoyckModelElasticity) <- c("Variable", "Elasticity")

audioKoyckModelElasticity$direction <- ifelse(
    audioKoyckModelElasticity$Elasticity > 0, "Positive", "Negative")

ggplot(data = audioKoyckModelElasticity,
       aes(x = reorder(Variable, Elasticity), y = Elasticity)) +
    geom_bar(position = "dodge", stat = "identity") + 
    coord_flip() +
    ggtitle("Home Audio - Koyck Model") + xlab("Variables")

#####################################################################################

####################################################################################
#       :::::::: Distributed Lag Model - Home Audio ::::::::
####################################################################################

colnames(HomeAudio_final)
# Removing week_year and derived price category variables
# Removing the following columns for the initial Distributed Lag Model:
# week_year, Mass_p, Middle_p, Premium_p,
AudioDistLagData <- HomeAudio_final[, -c(1, 10:12)]

# Lag GMV by 1 Time Unit
AudioDistLagData_1 <- slide(AudioDistLagData, Var = "gmv", slideBy = -1)

# Lag GMV by 2 Time Units
AudioDistLagData_1 <- slide(AudioDistLagData_1, Var = "gmv", slideBy = -2)

# Lag GMV by 3 Time Units
AudioDistLagData_1 <- slide(AudioDistLagData_1, Var = "gmv", slideBy = -3)

# Remove NAs and Scaling
AudioDistLagData <- data.frame(scale(na.omit(AudioDistLagData_1)))

# Build the initial Distributed Lag Model
DistLagModel_1 <- lm(gmv ~ ., AudioDistLagData)
summary(DistLagModel_1)
# Multiple R-squared:  0.992,	Adjusted R-squared:  0.948

DistLagModel_2 <- stepAIC(DistLagModel_1, direction = "both")
summary(DistLagModel_2)
# Multiple R-squared:  0.991,	Adjusted R-squared:  0.962

DistLagModel_3 <- lm(formula = gmv ~ list_price + MRP + Units + SLA + 
                         promotional_offer + Procurement_SLA + per_order + NPS +
                         holiday_freq + TV + Digital + Sponsorship + 
                         ContentMarketing + OnlineMarketing + Affiliates + 
                         inc_LP_MA1 + inc_LP_MA2 + inc_LP_MA3 + inc_PO_MA1 + 
                         inc_PO_MA2 + inc_PO_MA3 + list_price.2 + list_price.3 +
                         promotional_offer.1 + promotional_offer.2 + 
                         promotional_offer.3 + NPS.1 + NPS.2 + NPS.3 + 
                         holiday_freq.2 + holiday_freq.3 + gmv.1 + gmv.2 + gmv.3, 
                     data = AudioDistLagData)
summary(DistLagModel_3)
# Multiple R-squared:  0.991,	Adjusted R-squared:  0.962
vif(DistLagModel_3)

# Removing gmv.2 (high p-value)
DistLagModel_4 <- lm(formula = gmv ~ list_price + MRP + Units + SLA + 
                         promotional_offer + Procurement_SLA + per_order + NPS +
                         holiday_freq + TV + Digital + Sponsorship + 
                         ContentMarketing + OnlineMarketing + Affiliates + 
                         inc_LP_MA1 + inc_LP_MA2 + inc_LP_MA3 + inc_PO_MA1 + 
                         inc_PO_MA2 + inc_PO_MA3 + list_price.2 + list_price.3 +
                         promotional_offer.1 + promotional_offer.2 + 
                         promotional_offer.3 + NPS.1 + NPS.2 + NPS.3 + 
                         holiday_freq.2 + holiday_freq.3 + gmv.1 + gmv.3, 
                     data = AudioDistLagData)
summary(DistLagModel_4)
# Multiple R-squared:  0.991,	Adjusted R-squared:  0.964
vif(DistLagModel_4)

# Removing promotional_offer.1 (high multicollinearity and low significance)
DistLagModel_5 <- lm(formula = gmv ~ list_price + MRP + Units + SLA + 
                         promotional_offer + Procurement_SLA + per_order + NPS +
                         holiday_freq + TV + Digital + Sponsorship + 
                         ContentMarketing + OnlineMarketing + Affiliates + 
                         inc_LP_MA1 + inc_LP_MA2 + inc_LP_MA3 + inc_PO_MA1 + 
                         inc_PO_MA2 + inc_PO_MA3 + list_price.2 + list_price.3 +
                         promotional_offer.2 + promotional_offer.3 + NPS.1 + NPS.2 +
                         NPS.3 + holiday_freq.2 + holiday_freq.3 + gmv.1 + gmv.3, 
                     data = AudioDistLagData)
summary(DistLagModel_5)
# Multiple R-squared:  0.991,	Adjusted R-squared:  0.966
vif(DistLagModel_5)

# Removing promotional_offer (high multicollinearity and low significance)
DistLagModel_6 <- lm(formula = gmv ~ list_price + MRP + Units + SLA + 
                         Procurement_SLA + per_order + NPS +
                         holiday_freq + TV + Digital + Sponsorship + 
                         ContentMarketing + OnlineMarketing + Affiliates + 
                         inc_LP_MA1 + inc_LP_MA2 + inc_LP_MA3 + inc_PO_MA1 + 
                         inc_PO_MA2 + inc_PO_MA3 + list_price.2 + list_price.3 +
                         promotional_offer.2 + promotional_offer.3 + NPS.1 + NPS.2 +
                         NPS.3 + holiday_freq.2 + holiday_freq.3 + gmv.1 + gmv.3, 
                     data = AudioDistLagData)
summary(DistLagModel_6)
# Multiple R-squared:  0.99,	Adjusted R-squared:  0.965
vif(DistLagModel_6)

# Removing holiday_freq (high p-value)
DistLagModel_7 <- lm(formula = gmv ~ list_price + MRP + Units + SLA + 
                         Procurement_SLA + per_order + NPS +
                         TV + Digital + Sponsorship + ContentMarketing + 
                         OnlineMarketing + Affiliates + inc_LP_MA1 + inc_LP_MA2 +
                         inc_LP_MA3 + inc_PO_MA1 + inc_PO_MA2 + inc_PO_MA3 + 
                         list_price.2 + list_price.3 + promotional_offer.2 + 
                         promotional_offer.3 + NPS.1 + NPS.2 + NPS.3 + 
                         holiday_freq.2 + holiday_freq.3 + gmv.1 + gmv.3, 
                     data = AudioDistLagData)
summary(DistLagModel_7)
# Multiple R-squared:  0.987,	Adjusted R-squared:  0.959
vif(DistLagModel_7)

# Removing NPS.3 (high p-value)
DistLagModel_8 <- lm(formula = gmv ~ list_price + MRP + Units + SLA + 
                         Procurement_SLA + per_order + NPS +
                         TV + Digital + Sponsorship + ContentMarketing + 
                         OnlineMarketing + Affiliates + inc_LP_MA1 + inc_LP_MA2 +
                         inc_LP_MA3 + inc_PO_MA1 + inc_PO_MA2 + inc_PO_MA3 + 
                         list_price.2 + list_price.3 + promotional_offer.2 + 
                         promotional_offer.3 + NPS.1 + NPS.2 + 
                         holiday_freq.2 + holiday_freq.3 + gmv.1 + gmv.3, 
                     data = AudioDistLagData)
summary(DistLagModel_8)
# Multiple R-squared:  0.982,	Adjusted R-squared:  0.948
vif(DistLagModel_8)

# Removing list_price (high multicollinearity and low significance)
DistLagModel_9 <- lm(formula = gmv ~ MRP + Units + SLA + 
                         Procurement_SLA + per_order + NPS +
                         TV + Digital + Sponsorship + ContentMarketing + 
                         OnlineMarketing + Affiliates + inc_LP_MA1 + inc_LP_MA2 +
                         inc_LP_MA3 + inc_PO_MA1 + inc_PO_MA2 + inc_PO_MA3 + 
                         list_price.2 + list_price.3 + promotional_offer.2 + 
                         promotional_offer.3 + NPS.1 + NPS.2 + 
                         holiday_freq.2 + holiday_freq.3 + gmv.1 + gmv.3, 
                     data = AudioDistLagData)
summary(DistLagModel_9)
# Multiple R-squared:  0.979,	Adjusted R-squared:  0.941
vif(DistLagModel_9)

# Removing MRP (high multicollinearity and low significance)
DistLagModel_10 <- lm(formula = gmv ~ Units + SLA + 
                          Procurement_SLA + per_order + NPS +
                          TV + Digital + Sponsorship + ContentMarketing + 
                          OnlineMarketing + Affiliates + inc_LP_MA1 + inc_LP_MA2 +
                          inc_LP_MA3 + inc_PO_MA1 + inc_PO_MA2 + inc_PO_MA3 + 
                          list_price.2 + list_price.3 + promotional_offer.2 + 
                          promotional_offer.3 + NPS.1 + NPS.2 + 
                          holiday_freq.2 + holiday_freq.3 + gmv.1 + gmv.3, 
                      data = AudioDistLagData)
summary(DistLagModel_10)
# Multiple R-squared:  0.974,	Adjusted R-squared:  0.933
vif(DistLagModel_10)

# Removing per_order (high p-value)
DistLagModel_11 <- lm(formula = gmv ~ Units + SLA + Procurement_SLA + 
                          NPS + TV + Digital + Sponsorship + ContentMarketing + 
                          OnlineMarketing + Affiliates + inc_LP_MA1 + inc_LP_MA2 +
                          inc_LP_MA3 + inc_PO_MA1 + inc_PO_MA2 + inc_PO_MA3 + 
                          list_price.2 + list_price.3 + promotional_offer.2 + 
                          promotional_offer.3 + NPS.1 + NPS.2 + 
                          holiday_freq.2 + holiday_freq.3 + gmv.1 + gmv.3, 
                      data = AudioDistLagData)
summary(DistLagModel_11)
# Multiple R-squared:  0.972,	Adjusted R-squared:  0.933
vif(DistLagModel_11)

# Removing Affiliates (high multicollinearity and low significance)
DistLagModel_12 <- lm(formula = gmv ~ Units + SLA + Procurement_SLA + 
                          NPS + TV + Digital + Sponsorship + ContentMarketing + 
                          OnlineMarketing + inc_LP_MA1 + inc_LP_MA2 +
                          inc_LP_MA3 + inc_PO_MA1 + inc_PO_MA2 + inc_PO_MA3 + 
                          list_price.2 + list_price.3 + promotional_offer.2 + 
                          promotional_offer.3 + NPS.1 + NPS.2 + 
                          holiday_freq.2 + holiday_freq.3 + gmv.1 + gmv.3, 
                      data = AudioDistLagData)
summary(DistLagModel_12)
# Multiple R-squared:  0.97,	Adjusted R-squared:  0.93
vif(DistLagModel_12)

# Removing promotional_offer.2 (high multicollinearity and low significance)
DistLagModel_13 <- lm(formula = gmv ~ Units + SLA + Procurement_SLA + 
                          NPS + TV + Digital + Sponsorship + ContentMarketing + 
                          OnlineMarketing + inc_LP_MA1 + inc_LP_MA2 +
                          inc_LP_MA3 + inc_PO_MA1 + inc_PO_MA2 + inc_PO_MA3 + 
                          list_price.2 + list_price.3 + 
                          promotional_offer.3 + NPS.1 + NPS.2 + 
                          holiday_freq.2 + holiday_freq.3 + gmv.1 + gmv.3, 
                      data = AudioDistLagData)
summary(DistLagModel_13)
# Multiple R-squared:  0.967,	Adjusted R-squared:  0.928
vif(DistLagModel_13)

# Removing promotional_offer.3 (high p-value)
DistLagModel_14 <- lm(formula = gmv ~ Units + SLA + Procurement_SLA + 
                          NPS + TV + Digital + Sponsorship + ContentMarketing + 
                          OnlineMarketing + inc_LP_MA1 + inc_LP_MA2 +
                          inc_LP_MA3 + inc_PO_MA1 + inc_PO_MA2 + inc_PO_MA3 + 
                          list_price.2 + list_price.3 + NPS.1 + NPS.2 + 
                          holiday_freq.2 + holiday_freq.3 + gmv.1 + gmv.3, 
                      data = AudioDistLagData)
summary(DistLagModel_14)
# Multiple R-squared:  0.965,	Adjusted R-squared:  0.927
vif(DistLagModel_14)

# Removing inc_PO_MA1 (high p-value)
DistLagModel_15 <- lm(formula = gmv ~ Units + SLA + Procurement_SLA + 
                          NPS + TV + Digital + Sponsorship + ContentMarketing + 
                          OnlineMarketing + inc_LP_MA1 + inc_LP_MA2 +
                          inc_LP_MA3 + inc_PO_MA2 + inc_PO_MA3 + 
                          list_price.2 + list_price.3 + NPS.1 + NPS.2 + 
                          holiday_freq.2 + holiday_freq.3 + gmv.1 + gmv.3, 
                      data = AudioDistLagData)
summary(DistLagModel_15)
# Multiple R-squared:  0.962,	Adjusted R-squared:  0.924
vif(DistLagModel_15)

# Removing inc_PO_MA2 (high multicollinearity and low significance)
DistLagModel_16 <- lm(formula = gmv ~ Units + SLA + Procurement_SLA + 
                          NPS + TV + Digital + Sponsorship + ContentMarketing + 
                          OnlineMarketing + inc_LP_MA1 + inc_LP_MA2 +
                          inc_LP_MA3 + inc_PO_MA3 + list_price.2 + list_price.3 + 
                          NPS.1 + NPS.2 + holiday_freq.2 + holiday_freq.3 + 
                          gmv.1 + gmv.3, 
                      data = AudioDistLagData)
summary(DistLagModel_16)
# Multiple R-squared:  0.954,	Adjusted R-squared:  0.912
vif(DistLagModel_16)

# Removing TV (high p-value)
DistLagModel_17 <- lm(formula = gmv ~ Units + SLA + Procurement_SLA + 
                          NPS + Digital + Sponsorship + ContentMarketing + 
                          OnlineMarketing + inc_LP_MA1 + inc_LP_MA2 +
                          inc_LP_MA3 + inc_PO_MA3 + list_price.2 + list_price.3 + 
                          NPS.1 + NPS.2 + holiday_freq.2 + holiday_freq.3 + 
                          gmv.1 + gmv.3, 
                      data = AudioDistLagData)
summary(DistLagModel_17)
# Multiple R-squared:  0.945,	Adjusted R-squared:   0.9
vif(DistLagModel_17)

# Removing holiday_freq.3 (high p-value)
DistLagModel_18 <- lm(formula = gmv ~ Units + SLA + Procurement_SLA + 
                          NPS + Digital + Sponsorship + ContentMarketing + 
                          OnlineMarketing + inc_LP_MA1 + inc_LP_MA2 +
                          inc_LP_MA3 + inc_PO_MA3 + list_price.2 + list_price.3 + 
                          NPS.1 + NPS.2 + holiday_freq.2 + gmv.1 + gmv.3, 
                      data = AudioDistLagData)
summary(DistLagModel_18)
# Multiple R-squared:  0.941,	Adjusted R-squared:  0.896
vif(DistLagModel_18)

# Removing holiday_freq.2 (high p-value)
DistLagModel_19 <- lm(formula = gmv ~ Units + SLA + Procurement_SLA + 
                          NPS + Digital + Sponsorship + ContentMarketing + 
                          OnlineMarketing + inc_LP_MA1 + inc_LP_MA2 +
                          inc_LP_MA3 + inc_PO_MA3 + list_price.2 + list_price.3 + 
                          NPS.1 + NPS.2 + gmv.1 + gmv.3, 
                      data = AudioDistLagData)
summary(DistLagModel_19)
# Multiple R-squared:  0.924,	Adjusted R-squared:  0.871
vif(DistLagModel_19)

# Removing Sponsorship (high p-value)
DistLagModel_20 <- lm(formula = gmv ~ Units + SLA + Procurement_SLA + 
                          NPS + Digital + ContentMarketing + 
                          OnlineMarketing + inc_LP_MA1 + inc_LP_MA2 +
                          inc_LP_MA3 + inc_PO_MA3 + list_price.2 + list_price.3 + 
                          NPS.1 + NPS.2 + gmv.1 + gmv.3, 
                      data = AudioDistLagData)
summary(DistLagModel_20)
# Multiple R-squared:  0.908,	Adjusted R-squared:  0.85
vif(DistLagModel_20)

# Removing gmv.3 (high p-value)
DistLagModel_21 <- lm(formula = gmv ~ Units + SLA + Procurement_SLA + 
                          NPS + Digital + ContentMarketing + 
                          OnlineMarketing + inc_LP_MA1 + inc_LP_MA2 +
                          inc_LP_MA3 + inc_PO_MA3 + list_price.2 + list_price.3 + 
                          NPS.1 + NPS.2 + gmv.1, 
                      data = AudioDistLagData)
summary(DistLagModel_21)
# Multiple R-squared:  0.883,	Adjusted R-squared:  0.816
vif(DistLagModel_21)

# Removing Units (high p-value)
DistLagModel_22 <- lm(formula = gmv ~ SLA + Procurement_SLA + 
                          NPS + Digital + ContentMarketing + 
                          OnlineMarketing + inc_LP_MA1 + inc_LP_MA2 +
                          inc_LP_MA3 + inc_PO_MA3 + list_price.2 + list_price.3 + 
                          NPS.1 + NPS.2 + gmv.1, 
                      data = AudioDistLagData)
summary(DistLagModel_22)
# Multiple R-squared:  0.842,	Adjusted R-squared:  0.76
vif(DistLagModel_22)

# Removing SLA (high p-value)
DistLagModel_23 <- lm(formula = gmv ~ Procurement_SLA + NPS + Digital + 
                          ContentMarketing + OnlineMarketing + inc_LP_MA1 + 
                          inc_LP_MA2 + inc_LP_MA3 + inc_PO_MA3 + list_price.2 +
                          list_price.3 + NPS.1 + NPS.2 + gmv.1, 
                      data = AudioDistLagData)
summary(DistLagModel_23)
# Multiple R-squared:  0.815,	Adjusted R-squared:  0.729
vif(DistLagModel_23)

# Removing Digital (high p-value)
DistLagModel_24 <- lm(formula = gmv ~ Procurement_SLA + NPS + 
                          ContentMarketing + OnlineMarketing + inc_LP_MA1 + 
                          inc_LP_MA2 + inc_LP_MA3 + inc_PO_MA3 + list_price.2 +
                          list_price.3 + NPS.1 + NPS.2 + gmv.1, 
                      data = AudioDistLagData)
summary(DistLagModel_24)
# Multiple R-squared:  0.789,	Adjusted R-squared:   0.7
vif(DistLagModel_24)

# Removing OnlineMarketing (high p-value)
DistLagModel_25 <- lm(formula = gmv ~ Procurement_SLA + NPS + 
                          ContentMarketing + inc_LP_MA1 + 
                          inc_LP_MA2 + inc_LP_MA3 + inc_PO_MA3 + list_price.2 +
                          list_price.3 + NPS.1 + NPS.2 + gmv.1, 
                      data = AudioDistLagData)
summary(DistLagModel_25)
# Multiple R-squared:  0.775,	Adjusted R-squared:  0.69
vif(DistLagModel_25)

# Removing ContentMarketing (high p-value)
DistLagModel_26 <- lm(formula = gmv ~ Procurement_SLA + NPS + 
                          inc_LP_MA1 + inc_LP_MA2 + inc_LP_MA3 + inc_PO_MA3 +
                          list_price.2 + list_price.3 + NPS.1 + NPS.2 + gmv.1, 
                      data = AudioDistLagData)
summary(DistLagModel_26)
# Multiple R-squared:  0.745,	Adjusted R-squared:  0.661
vif(DistLagModel_26)

# Removing Procurement_SLA (high p-value)
DistLagModel_27 <- lm(formula = gmv ~ NPS + inc_LP_MA1 + inc_LP_MA2 + inc_LP_MA3 +
                          inc_PO_MA3 + list_price.2 + list_price.3 + NPS.1 + 
                          NPS.2 + gmv.1, 
                      data = AudioDistLagData)
summary(DistLagModel_27)
# Multiple R-squared:  0.714,	Adjusted R-squared:  0.629
vif(DistLagModel_27)

# Removing NPS.2 (high p-value)
DistLagModel_28 <- lm(formula = gmv ~ NPS + inc_LP_MA1 + inc_LP_MA2 + inc_LP_MA3 +
                          inc_PO_MA3 + list_price.2 + list_price.3 + NPS.1 + gmv.1, 
                      data = AudioDistLagData)
summary(DistLagModel_28)
# Multiple R-squared:  0.682,	Adjusted R-squared:   0.6
vif(DistLagModel_28)

# Removing NPS (high p-value)
DistLagModel_29 <- lm(formula = gmv ~ inc_LP_MA1 + inc_LP_MA2 + inc_LP_MA3 +
                          inc_PO_MA3 + list_price.2 + list_price.3 + NPS.1 + gmv.1, 
                      data = AudioDistLagData)
summary(DistLagModel_29)
# Multiple R-squared:  0.641,	Adjusted R-squared:  0.561
vif(DistLagModel_29)

# Removing NPS.1 (high p-value)
DistLagModel_30 <- lm(formula = gmv ~ inc_LP_MA1 + inc_LP_MA2 + inc_LP_MA3 +
                          inc_PO_MA3 + list_price.2 + list_price.3 + gmv.1, 
                      data = AudioDistLagData)
summary(DistLagModel_30)
# Multiple R-squared:  0.579,	Adjusted R-squared:   0.5
vif(DistLagModel_30)

# Removing any variable in DistLagModel_30 reduces the Adjusted R-squared significantly
# Hence, DistLagModel_30 can be considered as the final model

AudioDistLagModel <- DistLagModel_30
# 6 variables: inc_LP_MA1, inc_LP_MA2, inc_LP_MA3, inc_PO_MA3, list_price.2, list_price.3, gmv.1
# Multiple R-squared:  0.579,	Adjusted R-squared:   0.5

######################################################################################
# Cross Validation
# Calculate K-fold cross-validation, for K=10.
crossval <- cv.lm(data = AudioDistLagData, 
                  form.lm = formula(gmv ~ inc_LP_MA1 + inc_LP_MA2 + inc_LP_MA3 + inc_PO_MA3 + list_price.2 + list_price.3 + gmv.1), m = 10)

CrossVal_HomeAudio[4] <- attr(crossval, "ms")

# Home Audio Distributed Lag Model Elasticity Analysis
trainingSet <- AudioDistLagData

# Home Audio Distributed Lag Model Elasticity coefficients
getAudioDistLagElasticity <- function(var){
    val <-as.numeric(AudioDistLagModel$coefficients[var] * 
                         mean(trainingSet[, var]) / mean(trainingSet$gmv))
    return(val)
}

audioVarList <- list()

for(i in 2:length(AudioDistLagModel$coefficients)){
    audioVarList[i-1] <- getAudioDistLagElasticity(names(AudioDistLagModel$coefficients)[i])
}

audioDistLagModelElasticity <- data.frame(names(
    AudioDistLagModel$coefficients[2:length(AudioDistLagModel$coefficients)]))

audioDistLagModelElasticity <- cbind(audioDistLagModelElasticity, 
                                      do.call(rbind.data.frame, audioVarList))

colnames(audioDistLagModelElasticity) <- c("Variable", "Elasticity")

audioDistLagModelElasticity$direction <- ifelse(
    audioDistLagModelElasticity$Elasticity > 0, "Positive", "Negative")

ggplot(data = audioDistLagModelElasticity,
       aes(x = reorder(Variable, Elasticity), y = Elasticity)) +
    geom_bar(position = "dodge", stat = "identity") + 
    coord_flip() +
    ggtitle("Home Audio - Distributed Lag Model") + xlab("Variables")

#####################################################################################

####################################################################################
#    :::::::: Multiplicative + Distributed Lag Model - Home Audio ::::::::
####################################################################################

colnames(HomeAudio_final)
# Removing week_year, holiday_freq, Moving Averages and derived price category variables
# Removing the following columns for the initial Multiplicative + Distributed Lag Model:
# week_year, Mass_p, Middle_p, Premium_p, holiday_freq,
# inc_LP_MA1, inc_LP_MA2, inc_LP_MA3,
# inc_PO_MA1, inc_PO_MA2, inc_PO_MA3,
# holiday_freq-1, holiday_freq-2, holiday_freq-3
AudioMultiDistLagData <- HomeAudio_final[, -c(1, 10:13, 22:27, 37:39)]

# Lag GMV by 1 Time Unit
AudioMultiDistLagData_1 <- slide(AudioMultiDistLagData, Var = "gmv", slideBy = -1)

# Lag GMV by 2 Time Units
AudioMultiDistLagData_1 <- slide(AudioMultiDistLagData_1, Var = "gmv", slideBy = -2)

# Lag GMV by 3 Time Units
AudioMultiDistLagData_1 <- slide(AudioMultiDistLagData_1, Var = "gmv", slideBy = -3)

# Remove NAs
AudioMultiDistLagData <- na.omit(AudioMultiDistLagData_1)

# Data Treatment
AudioMultiDistLagData$ContentMarketing[which(AudioMultiDistLagData$ContentMarketing==0)] <- 1
AudioMultiDistLagData$per_order[which(AudioMultiDistLagData$per_order==0)] <- 0.01

# Applying Logarithm
AudioMultiDistLagData <- log(AudioMultiDistLagData)

# Updating column names
colnames(AudioMultiDistLagData)[17:28] <- c("list_price.1", "list_price.2", "list_price.3", "promotional_offer.1", "promotional_offer.2", "promotional_offer.3", "NPS.1", "NPS.2", "NPS.3", "gmv.1", "gmv.2", "gmv.3")

# Build the Initial Multiplicative + Distributed Lag Model
MultiDistLagModel_1 <- lm(gmv ~ ., AudioMultiDistLagData)
summary(MultiDistLagModel_1)
# Multiple R-squared:  0.645,	Adjusted R-squared:  0.0816

MultiDistLagModel_2 <- stepAIC(MultiDistLagModel_1, direction = "both")
summary(MultiDistLagModel_2)
# Multiple R-squared:  0.594,	Adjusted R-squared:  0.405

MultiDistLagModel_3 <- lm(formula = gmv ~ list_price + SLA + promotional_offer +
                              Procurement_SLA + per_order + NPS + TV + Sponsorship +
                              OnlineMarketing + Affiliates + list_price.1 + 
                              promotional_offer.2 + promotional_offer.3 + NPS.1, 
                          data = AudioMultiDistLagData)
summary(MultiDistLagModel_3)
# Multiple R-squared:  0.594,	Adjusted R-squared:  0.405
vif(MultiDistLagModel_3)

# Removing list_price (high p-value)
MultiDistLagModel_4 <- lm(formula = gmv ~ SLA + promotional_offer +
                              Procurement_SLA + per_order + NPS + TV + Sponsorship +
                              OnlineMarketing + Affiliates + list_price.1 + 
                              promotional_offer.2 + promotional_offer.3 + NPS.1, 
                          data = AudioMultiDistLagData)
summary(MultiDistLagModel_4)
# Multiple R-squared:  0.574,	Adjusted R-squared:  0.396
vif(MultiDistLagModel_4)

# Removing Sponsorship (high p-value)
MultiDistLagModel_5 <- lm(formula = gmv ~ SLA + promotional_offer +
                              Procurement_SLA + per_order + NPS + TV +
                              OnlineMarketing + Affiliates + list_price.1 + 
                              promotional_offer.2 + promotional_offer.3 + NPS.1, 
                          data = AudioMultiDistLagData)
summary(MultiDistLagModel_5)
# Multiple R-squared:  0.559,	Adjusted R-squared:  0.394
vif(MultiDistLagModel_5)

# Removing promotional_offer.3 (high p-value)
MultiDistLagModel_6 <- lm(formula = gmv ~ SLA + promotional_offer +
                              Procurement_SLA + per_order + NPS + TV +
                              OnlineMarketing + Affiliates + list_price.1 + 
                              promotional_offer.2 + NPS.1, 
                          data = AudioMultiDistLagData)
summary(MultiDistLagModel_6)
# Multiple R-squared:  0.539,	Adjusted R-squared:  0.386
vif(MultiDistLagModel_6)

# Removing Procurement_SLA (high p-value)
MultiDistLagModel_7 <- lm(formula = gmv ~ SLA + promotional_offer + per_order + NPS +
                              TV + OnlineMarketing + Affiliates + list_price.1 + 
                              promotional_offer.2 + NPS.1, 
                          data = AudioMultiDistLagData)
summary(MultiDistLagModel_7)
# Multiple R-squared:  0.513,	Adjusted R-squared:  0.37
vif(MultiDistLagModel_7)

# Removing promotional_offer.2 (high p-value)
MultiDistLagModel_8 <- lm(formula = gmv ~ SLA + promotional_offer + per_order + NPS +
                              TV + OnlineMarketing + Affiliates + list_price.1 + 
                              NPS.1, 
                          data = AudioMultiDistLagData)
summary(MultiDistLagModel_8)
# Multiple R-squared:  0.498,	Adjusted R-squared:  0.369
vif(MultiDistLagModel_8)

# Removing per_order (high p-value)
MultiDistLagModel_9 <- lm(formula = gmv ~ SLA + promotional_offer + NPS + TV +
                              OnlineMarketing + Affiliates + list_price.1 + NPS.1, 
                          data = AudioMultiDistLagData)
summary(MultiDistLagModel_9)
# Multiple R-squared:  0.471,	Adjusted R-squared:  0.354
vif(MultiDistLagModel_9)

# Removing list_price.1 (high p-value)
MultiDistLagModel_10 <- lm(formula = gmv ~ SLA + promotional_offer + NPS + TV +
                               OnlineMarketing + Affiliates + NPS.1, 
                           data = AudioMultiDistLagData)
summary(MultiDistLagModel_10)
# Multiple R-squared:  0.447,	Adjusted R-squared:  0.343
vif(MultiDistLagModel_10)

# Removing SLA (high p-value)
MultiDistLagModel_11 <- lm(formula = gmv ~ promotional_offer + NPS + TV +
                               OnlineMarketing + Affiliates + NPS.1, 
                           data = AudioMultiDistLagData)
summary(MultiDistLagModel_11)
# Multiple R-squared:  0.417,	Adjusted R-squared:  0.324
vif(MultiDistLagModel_11)

# Removing any further variable from MultiDistLagModel_11 reduces the adjusted R-squared significantly.
# Hence, we will consider MultiDistLagModel_11 as the final model

AudioMultiDistLagModel <- MultiDistLagModel_11
# 6 variables: promotional_offer, NPS, TV, OnlineMarketing, Affiliates, NPS.1
# Multiple R-squared:  0.417,	Adjusted R-squared:  0.324

######################################################################################
# Cross Validation
# Calculate K-fold cross-validation, for K=10.
crossval <- cv.lm(data = AudioMultiDistLagData, 
                  form.lm = formula(gmv ~ promotional_offer + NPS + TV + OnlineMarketing + Affiliates + NPS.1), m = 10)

CrossVal_HomeAudio[5] <- attr(crossval, "ms")

# Home Audio Multiplicative + Distributed Lag Model Elasticity Analysis
trainingSet <- AudioMultiDistLagData

# Home Audio Multiplicative + Distributed Lag Model Elasticity coefficients
getAudioMultiDistLagElasticity <- function(var){
    val <- as.numeric(AudioMultiDistLagModel$coefficients[var] * 
                         mean(trainingSet[, var]) / mean(trainingSet$gmv))
    return(val)
} 

audioVarList <- list()

for(i in 2:length(AudioMultiDistLagModel$coefficients)){
    audioVarList[i-1] <- getAudioMultiDistLagElasticity(names(AudioMultiDistLagModel$coefficients)[i])
}

audioMultiDistLagModelElasticity <- data.frame(names(
    AudioMultiDistLagModel$coefficients[2:length(AudioMultiDistLagModel$coefficients)]))

audioMultiDistLagModelElasticity <- cbind(audioMultiDistLagModelElasticity, 
                                           do.call(rbind.data.frame, audioVarList))

colnames(audioMultiDistLagModelElasticity) <- c("Variable", "Elasticity")

audioMultiDistLagModelElasticity$direction <- ifelse(
    audioMultiDistLagModelElasticity$Elasticity > 0, "Positive", "Negative")

ggplot(data = audioMultiDistLagModelElasticity,
       aes(x = reorder(Variable, Elasticity), y = Elasticity)) +
    geom_bar(position = "dodge", stat = "identity") + 
    coord_flip() +
    ggtitle("Home Audio - Multiplicative + Distributed Lag Model") + xlab("Variables")

#####################################################################################
