####################################################################################
#                  :::::::: Ecommerce Capstone Solution ::::::::
####################################################################################
####################################################################################
#                  :::::::: Market Mix Modelling :::::::::::::::
####################################################################################

### Business Understanding::
# ElecKart is an e-commerce firm specialising in electronic products. 
# Over the last one year, they had spent a significant amount of money in marketing.
# They also offered big-ticket promotions. 
# They are about to create a marketing budget for the next year which includes spending on commercials, online campaigns, and pricing & promotion strategies.


### Solution Objective::
# The aim is to develop a market mix model to observe the actual impact of different marketing variables over the last year.
# Basically needs to optimize the marketing levers to improve the revenue response.

# Below are the data or variables needs to be consider for analysis::
# Products Sales data
# Media Investment
# NPS Score
# Special Sale days [Holidays]

####################################################################################
#                  :::::::: Data Preparation & EDA ::::::::
####################################################################################

# Load relevant libraries
library(dplyr)
library(plyr)
library(lubridate)
library(MASS)
library(car)
library(DataCombine)
library(zoo)
library(glmnet)
library(DAAG)
library(ggplot2)
library(readxl)

# Load ConsumerElectronics.csv Dataset
orderDetails <- read.csv("dataset/ConsumerElectronics.csv", stringsAsFactors = F)
# 1648824 observations of 20 variables

# Set proper names for the columns
colnames(orderDetails) <- c("FSN_ID", "Order_date", "Year", "Month", "Order_id", "Order_item_id", "gmv", "Units", "deliverybdays", "deliverycdays", "payment_mode", "SLA", "cust_id", "pincode", "P_super_category", "P_analytic_category", "P_sub_category", "P_analytic_vertical", "MRP", "Procurement_SLA")

str(orderDetails)
summary(orderDetails)


######### Load Final_adstock.csv dataset
adstockDetails <- read.csv("dataset/Final_adstock.csv", stringsAsFactors = F)
colnames(adstockDetails) <- c('week_year', 'TV', 'Digital', 'Sponsorship',
                              'ContentMarketing', 'OnlineMarketing', 'Affiliates',
                              'SEM')


##### Data Sanity Check
# orderDetails
# Step 1 - Data sanity Check: Data should be from July-2015 to June -2016
orderDetails <- subset(orderDetails, !(Month==5 & Year==2015 | Month==6 & Year==2015 | Month ==7 & Year==2016))
# 1648215 observations of 20 variables

# Step 2 - Add a week column in the dataset. 
# We have to convert the data set in a weekly level. 
# Check if the date formats are correct or not.
orderDetails$Order_date <- date(orderDetails$Order_date)
orderDetails$week_year <- week(orderDetails$Order_date)

# Step 3 - Inspect Data quality issues
# For example - Jan 2016 should be week 54, not week 1 etc.
orderDetails$week_year <- ifelse(orderDetails$week_year <= 26 & orderDetails$Year == 2016, orderDetails$week_year + 53, orderDetails$week_year)

# Step 4 - We should not considering free products
orderDetails <- subset(orderDetails, MRP != 0)
# 1642907 observations of 21 variables

# Step 5 - removing rows with NA values
sum(is.na(orderDetails)) # 14658
orderDetails <- na.omit(orderDetails)
# 1638021 observations of 21 variables

# Step 6 - Make "gmv" =1 if they are 0
orderDetails$gmv[which(orderDetails$gmv == 0)] <- 1

# Step 7 - gmv should not be more than MRP*units 
# Since we can offer discounts but should not charge higher.
# So, we will take a subset such that (MRP*Units)>=gmv
orderDetails <- subset(orderDetails, (MRP * Units) >= gmv)
# 1604389 observations of 21 variables

# Step 8 - Divide the data in 3 buckets based on category.
GamingAccessory <- orderDetails[orderDetails$P_sub_category == "GamingAccessory", ] 
# 197225 observations of 21 variables

CameraAccessory <- orderDetails[orderDetails$P_sub_category == "CameraAccessory", ]
# 230725 observations of 21 variables

HomeAudio <- orderDetails[orderDetails$P_sub_category == "HomeAudio", ]
# 118995 observations of 21 variables


# Function to perform the following:
# 1. Prepare KPIs 
# 2. Perform Clustering 
# 3. Data aggregation as and when required 
# 4. Analyzing NPS data 
# 5. Using Holiday information 
# 6. Merging master data with holiday data

BucketAnalysisFunc <- function(bucket) {
    # 1. KPI - List price for all the products
    bucket$list_price <- bucket$gmv/bucket$Units
    
    # 2. KPI - Promotional Offer for all the products
    bucket$promotional_offer <- (bucket$MRP-bucket$list_price)/bucket$MRP
    
    #  3. Clustering - divide products into three categories 
    # based on MRP and num units (Units) and List Price - 
    # mass market, medium market and premium product
    bucket$P_analytic_vertical <- factor(bucket$P_analytic_vertical)
    cluster<- aggregate(cbind(Units, list_price, MRP)~P_analytic_vertical, bucket, mean)
    
    if(nrow(cluster)<=3) {
        cluster$price_category <-NA
        cluster$price_category[which(cluster$MRP>=mean(cluster$MRP))] <- "Middle_p"
        cluster$price_category[-which(cluster$MRP>=mean(cluster$MRP))] <- "Mass_p"
        
        cluster <- cluster[,-c(2:4)]
        
        bucket <- merge(bucket, cluster, by="P_analytic_vertical")
    } else {
        # Perform a K-Means clustering by scaling the 3 variables:
        # Units, list_price, and MRP
        cluster$list_price_1 <- scale(cluster$list_price)
        cluster$MRP_1<- scale(cluster$MRP)
        cluster$Units_1 <- scale(cluster$Units)
        
        k1 <- cluster[,-c(2:4)]
        clust <- kmeans(k1[,-1], centers = 3, iter.max = 50, nstart = 50)
        cluster$price_category <- as.factor(clust$cluster)
        cluster <- cluster[,c(1,8)]
        
        # Bring the 3-Cluster solution cluster in the master dataset 
        # by merging with P_analytic_vertical as the key
        bucket <- merge(bucket, cluster, by=c("P_analytic_vertical"), all.x=TRUE)
        
        k2 <- table(bucket$price_category)
        levels(bucket$price_category)[which(k2==max(table(bucket$price_category)))] <- "Mass_p"
        levels(bucket$price_category)[which(k2==min(table(bucket$price_category)))] <- "Premium_p"
        levels(bucket$price_category)[which(k2!=max(table(bucket$price_category))& k2!=min(table(bucket$price_category)))] <- "Middle_p"
    }
    
    # 4. KPI - Payment model indicator
    bucket$order_pay_ind <- ifelse(bucket$payment_mode=="Prepaid",1,0)
    
    # 5. Create Percentage Online Order = (Online_order/Total Order) KPI
    # Total order
    total_order <- aggregate(order_pay_ind ~ week_year, data=bucket, FUN=NROW)
    
    # Total online order
    Online_order <- aggregate(order_pay_ind ~ week_year, data=bucket, FUN=sum)
    
    # Merge both the above datasets
    merged <- merge(total_order, Online_order, by=c("week_year"), all.x=TRUE)
    
    # Create new column of percentage online
    merged$per_order <- merged$order_pay_ind.y/merged$order_pay_ind.x
    
    # Drop columns that are redundant
    merged <- merged[,-c(2,3)]
    
    # Add "per_order" column in bucket
    bucket<- merge(bucket, merged, by=c("week_year"), all.x=TRUE)
    
    bucket$P_sub_category <- NULL
    bucket$P_super_category <- NULL
    bucket$P_analytic_category <- NULL
    
    # NPS data
    nps <- read.csv("dataset/nps.csv", h=T)
    nps$Month <- as.character(nps$Month)
    bucket<-merge(bucket, nps, by=c("Month", "Year"), all.x=TRUE)
    
    # Holiday data
    holiday_list<-c("2015-07-18","2015-07-19","2015-08-15",
                    "2015-08-16","2015-08-17","2015-08-28",
                    "2015-08-29","2015-08-30","2015-10-15",
                    "2015-10-16","2015-10-17","2015-11-07",
                    "2015-11-08","2015-11-09","2015-11-10",
                    "2015-10-11","2015-10-12","2015-11-13",
                    "2015-11-14","2015-12-25","2015-12-26",
                    "2015-12-27","2015-12-28","2015-12-29",
                    "2015-12-30","2016-01-01","2016-01-02",
                    "2016-01-03","2016-01-20","2016-01-21",
                    "2016-01-22","2016-02-01","2016-02-02",
                    "2016-02-20","2016-02-21","2016-02-14",
                    "2016-02-15","2016-03-07","2016-03-08",
                    "2016-03-09","2016-05-25","2016-05-26",
                    "2016-05-27")
    
    holiday_list <- as.Date(holiday_list)
    week_year <- week(holiday_list)
    year <- year(holiday_list)
    holiday <- data.frame(week_year, year)
    holiday$week_year<- ifelse(holiday$week_year<=26 & holiday$year==2016,
                               holiday$week_year+53, holiday$week_year)
    holiday$year <-NULL
    holiday$holiday_freq <- 1
    
    holiday <- aggregate(holiday_freq ~ week_year, holiday, sum)
    
    products <- as.data.frame.matrix(t(table(bucket$price_category, bucket$week_year)))
    
    products$week_year <- row.names(products)
    
    bucket_1 <- aggregate(gmv~week_year, bucket, sum)
    
    bucket<- aggregate(cbind(list_price,MRP,Units,SLA,promotional_offer,
                             Procurement_SLA,per_order,NPS)~ week_year,
                       data=bucket,FUN = mean)
    
    bucket <- merge(bucket, products, by="week_year", all.x=T)
    bucket <- merge(bucket, holiday, by="week_year", all.x=T)
    bucket$holiday_freq[is.na(bucket$holiday_freq)] <-0
    bucket <- merge(bucket, bucket_1, by="week_year", all.x=T)
    
    return(bucket)
}

# Call BucketAnalysisFunc on the three buckets
GamingAccessory <- BucketAnalysisFunc(GamingAccessory)
CameraAccessory <- BucketAnalysisFunc(CameraAccessory)
HomeAudio <- BucketAnalysisFunc(HomeAudio)


# Merge adstockDetails with GamingAccessory
GamingAccessory <- merge(GamingAccessory, adstockDetails, by.x = "week_year")

# Merge adstockDetails with CameraAccessory
CameraAccessory <- merge(CameraAccessory, adstockDetails, by.x = "week_year")

# Merge adstockDetails with HomeAudio
HomeAudio <- merge(HomeAudio,adstockDetails, by.x = "week_year")


# Create moving average variables
movingAveragesFunc <- function(bucket)
{
    twoPMAfunc <- function(x) rollmean(x, k = 2, fill = NA, align = "right")
    threePMAfunc <- function(x) rollmean(x, k = 3, fill = NA, align = "right")
    fourPMAfunc <- function(x) rollmean(x, k = 4, fill = NA, align = "right")
    
    MAsubset = bucket[, c("week_year", "list_price", "promotional_offer")]
    
    MAsubset1 <- MAsubset %>% 
        mutate_each(funs(twoPMAfunc), list_price, promotional_offer) %>% 
        data.frame()
    
    MAsubset2 <- MAsubset %>% 
        mutate_each(funs(threePMAfunc), list_price, promotional_offer) %>% 
        data.frame()
    
    MAsubset3 <- MAsubset %>% 
        mutate_each(funs(fourPMAfunc), list_price, promotional_offer) %>% 
        data.frame()
    
    MAsubset1$LP_MA1 <- (MAsubset1$list_price)
    MAsubset1$PO_MA1 <- (MAsubset1$promotional_offer)
    
    MAsubset2$LP_MA2 <- (MAsubset2$list_price)
    MAsubset2$PO_MA2 <- (MAsubset2$promotional_offer)
    
    MAsubset3$LP_MA3 <- (MAsubset3$list_price)
    MAsubset3$PO_MA3 <- (MAsubset3$promotional_offer)
    
    MAsubset4 <- cbind(MAsubset1[, -c(2:3)], 
                       MAsubset2[, -c(1:3)], 
                       MAsubset3[, -c(1:3)])

    bucket <- merge(bucket, MAsubset4, by = "week_year")
    
    # Calculate the incremental List Price lifts
    bucket$inc_LP_MA1 <- (bucket$list_price - bucket$LP_MA1) / bucket$LP_MA1
    bucket$inc_LP_MA2 <- (bucket$list_price - bucket$LP_MA2) / bucket$LP_MA2
    bucket$inc_LP_MA3 <- (bucket$list_price - bucket$LP_MA3) / bucket$LP_MA3
    
    # Calculate the Promotional Offer lifts
    bucket$inc_PO_MA1 <- (bucket$promotional_offer - bucket$PO_MA1) / bucket$PO_MA1
    bucket$inc_PO_MA2 <- (bucket$promotional_offer - bucket$PO_MA2) / bucket$PO_MA2
    bucket$inc_PO_MA3 <- (bucket$promotional_offer - bucket$PO_MA3) / bucket$PO_MA3
    
    # Drop the MA variables as our main concern are the lifts
    bucket$LP_MA1<-NULL
    bucket$LP_MA2<-NULL
    bucket$LP_MA3<-NULL
    
    bucket$PO_MA1<-NULL
    bucket$PO_MA2<-NULL
    bucket$PO_MA3<-NULL
    
    names(bucket)[22:27] <- c("inc_LP_MA1", "inc_LP_MA2", "inc_LP_MA3",
                              "inc_PO_MA1", "inc_PO_MA2", "inc_PO_MA3")
    
    # 1) Lag of List price by 1 week, 2 week, and 3 week
    lagData <- slide(bucket, Var = "list_price", slideBy = -1)
    lagData <- slide(lagData, Var = "list_price", slideBy = -2)
    lagData <- slide(lagData, Var = "list_price", slideBy = -3)
    
    # 2) Lag of Promotional offer by 1 week, 2 week, and 3 week
    lagData <- slide(lagData, Var = "promotional_offer", slideBy = -1)
    lagData <- slide(lagData, Var = "promotional_offer", slideBy = -2)
    lagData <- slide(lagData, Var = "promotional_offer", slideBy = -3)
    
    # 3) Lag of Promotional NPS by 1 week,2 week, 3 week
    lagData <- slide(lagData, Var = "NPS", slideBy = -1)
    lagData <- slide(lagData, Var = "NPS", slideBy = -2)
    lagData <- slide(lagData, Var = "NPS", slideBy = -3)
    
    # 4) Lag of Promotional Holiday Frequency by 1 week,2 week, 3 week
    lagData <- slide(lagData, Var = "holiday_freq", slideBy = -1)
    lagData <- slide(lagData, Var = "holiday_freq", slideBy = -2)
    lagData <- slide(lagData, Var = "holiday_freq", slideBy = -3)
    
    bucket <- na.omit(lagData) 
    
    return(bucket)
}

GamingAccessory_final <- movingAveragesFunc(GamingAccessory)
CameraAccessory_final <- movingAveragesFunc(CameraAccessory)
HomeAudio_final <- movingAveragesFunc(HomeAudio)



############################ Performing EDA ############################
####### EDA for Gaming Accessory

# TV AdStock by GMV
plot1 <- ggplot(GamingAccessory_final, aes(TV, gmv)) + geom_point() + 
    geom_smooth(aes(method="lm")) + ggtitle("Gaming Accessory") + 
    labs(x = "TV AdStock", y = "GMV")
plot1
    
# Digital AdStock by GMV
plot2 <- ggplot(GamingAccessory_final, aes(Digital, gmv)) + geom_point() + 
    geom_smooth(aes(method="lm")) + ggtitle("Gaming Accessory") + 
    labs(x = "Digital AdStock", y = "GMV")
plot2
    
# Sponsorship AdStock by GMV
plot3 <- ggplot(GamingAccessory_final, aes(Sponsorship, gmv)) + geom_point() + 
    geom_smooth(aes(method="lm")) + ggtitle("Gaming Accessory") + 
    labs(x = "Sponsorship AdStock", y = "GMV")
plot3
    
# Content Marketing AdStock by GMV
plot4 <- ggplot(GamingAccessory_final, aes(ContentMarketing, gmv)) + geom_point() + 
    geom_smooth(aes(method="lm")) + ggtitle("Gaming Accessory") + 
    labs(x = "Content Marketing AdStock", y = "GMV")
plot4
    
# Online Marketing AdStock by GMV
plot5 <- ggplot(GamingAccessory_final, aes(OnlineMarketing, gmv)) + geom_point() + 
    geom_smooth(aes(method="lm")) + ggtitle("Gaming Accessory") + 
    labs(x = "Online Marketing AdStock", y = "GMV")
plot5
    
# Affiliates AdStock by GMV
plot6 <- ggplot(GamingAccessory_final, aes(Affiliates, gmv)) + geom_point() + 
    geom_smooth(aes(method="lm")) + ggtitle("Gaming Accessory") + 
    labs(x = "Affiliates AdStock", y = "GMV")
plot6
    
# SEM AdStock by GMV
plot7 <- ggplot(GamingAccessory_final, aes(SEM, gmv)) + geom_point() + 
    geom_smooth(aes(method="lm")) + ggtitle("Gaming Accessory") + 
    labs(x = "SEM AdStock", y = "GMV")
plot7
    

####### EDA for Camera Accessory

# TV AdStock by GMV
plot8 <- ggplot(CameraAccessory_final, aes(TV, gmv)) + geom_point() + 
    geom_smooth(aes(method="lm")) + ggtitle("Camera Accessory") + 
    labs(x = "TV AdStock", y = "GMV")
plot8

# Digital AdStock by GMV
plot9 <- ggplot(CameraAccessory_final, aes(Digital, gmv)) + geom_point() + 
    geom_smooth(aes(method="lm")) + ggtitle("Camera Accessory") + 
    labs(x = "Digital AdStock", y = "GMV")
plot9

# Sponsorship AdStock by GMV
plot10 <- ggplot(CameraAccessory_final, aes(Sponsorship, gmv)) + geom_point() + 
    geom_smooth(aes(method="lm")) + ggtitle("Camera Accessory") + 
    labs(x = "Sponsorship AdStock", y = "GMV")
plot10

# Content Marketing AdStock by GMV
plot11 <- ggplot(CameraAccessory_final, aes(ContentMarketing, gmv)) + geom_point() + 
    geom_smooth(aes(method="lm")) + ggtitle("Camera Accessory") + 
    labs(x = "Content Marketing AdStock", y = "GMV")
plot11

# Online Marketing AdStock by GMV
plot12 <- ggplot(CameraAccessory_final, aes(OnlineMarketing, gmv)) + geom_point() + 
    geom_smooth(aes(method="lm")) + ggtitle("Camera Accessory") + 
    labs(x = "Online Marketing AdStock", y = "GMV")
plot12

# Affiliates AdStock by GMV
plot13 <- ggplot(CameraAccessory_final, aes(Affiliates, gmv)) + geom_point() + 
    geom_smooth(aes(method="lm")) + ggtitle("Camera Accessory") + 
    labs(x = "Affiliates AdStock", y = "GMV")
plot13

# SEM AdStock by GMV
plot14 <- ggplot(CameraAccessory_final, aes(SEM, gmv)) + geom_point() + 
    geom_smooth(aes(method="lm")) + ggtitle("Camera Accessory") + 
    labs(x = "SEM AdStock", y = "GMV")
plot14


####### EDA for Camera Accessory

# TV AdStock by GMV
plot15 <- ggplot(HomeAudio_final, aes(TV, gmv)) + geom_point() + 
    geom_smooth(aes(method="lm")) + ggtitle("Home Audio") + 
    labs(x = "TV AdStock", y = "GMV")
plot15

# Digital AdStock by GMV
plot16 <- ggplot(HomeAudio_final, aes(Digital, gmv)) + geom_point() + 
    geom_smooth(aes(method="lm")) + ggtitle("Home Audio") + 
    labs(x = "Digital AdStock", y = "GMV")
plot16

# Sponsorship AdStock by GMV
plot17 <- ggplot(HomeAudio_final, aes(Sponsorship, gmv)) + geom_point() + 
    geom_smooth(aes(method="lm")) + ggtitle("Home Audio") + 
    labs(x = "Sponsorship AdStock", y = "GMV")
plot17

# Content Marketing AdStock by GMV
plot18 <- ggplot(HomeAudio_final, aes(ContentMarketing, gmv)) + geom_point() + 
    geom_smooth(aes(method="lm")) + ggtitle("Home Audio") + 
    labs(x = "Content Marketing AdStock", y = "GMV")
plot18

# Online Marketing AdStock by GMV
plot19 <- ggplot(HomeAudio_final, aes(OnlineMarketing, gmv)) + geom_point() + 
    geom_smooth(aes(method="lm")) + ggtitle("Home Audio") + 
    labs(x = "Online Marketing AdStock", y = "GMV")
plot19

# Affiliates AdStock by GMV
plot20 <- ggplot(HomeAudio_final, aes(Affiliates, gmv)) + geom_point() + 
    geom_smooth(aes(method="lm")) + ggtitle("Home Audio") + 
    labs(x = "Affiliates AdStock", y = "GMV")
plot20

# SEM AdStock by GMV
plot21 <- ggplot(HomeAudio_final, aes(SEM, gmv)) + geom_point() + 
    geom_smooth(aes(method="lm")) + ggtitle("Home Audio") + 
    labs(x = "SEM AdStock", y = "GMV")
plot21

#####################################################################################
#####################################################################################
