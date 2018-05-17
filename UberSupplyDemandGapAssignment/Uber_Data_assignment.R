#####################################################################################
###################### Uber Supply Demand Gap Analysis ##############################
###################### Author: Santanu Dey (DDA1730043) #############################
#####################################################################################

library(lubridate)
library(tidyr)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(ggthemes)

# Load the data set
uberData <- read.csv("Uber Request Data.csv", stringsAsFactors = F)

#####################################################################################
######################### Understand the data frame #################################
#####################################################################################
summary(uberData)
str(uberData)
unique(uberData$Status) # 3 unique values
table(uberData$Status)  # Number of entries for each of the 3 unique Status values

# Find how many NA values in the data frame
colSums(is.na(uberData))
# Driver.id column has 2650 NA entries, which corresponds to the 2650 entries for "No Cars Available" Status entries.
# Also, Drop.timestamp column has 3914 NA entries, which corresponds to the sum of 1264 "Cancelled" and 2650 "No Cars Available" Status entries.

#####################################################################################
################################# Data Cleaning #####################################
#####################################################################################
# It is observed that there are multiple formats present in the dataset for the timestamp columns
# We will convert each timestamp column to the same format
# Request.timestamp
# The dmy_hm(s) functions from the lubridate package convert the entries not matching the format to NAs
# Entries without 'seconds' value
request_Fmt_1 <- dmy_hm(uberData$Request.timestamp, tz = "")
# Entries with 'seconds' value
request_Fmt_2 <- dmy_hms(uberData$Request.timestamp, tz = "")
# Replace the NA values in request_Fmt_1 with the non-NA values in request_Fmt_2
request_Fmt_1[is.na(request_Fmt_1)] <- request_Fmt_2[!is.na(request_Fmt_2)]
# Replace the Request.timestamp column with the cleaned request_Fmt_1
uberData$Request.timestamp <- request_Fmt_1

# Drop.timestamp
# The dmy_hm(s) functions from the lubridate package convert the entries not matching # the format to NAs. But we already have NA entries in the Drop.timestamp column.
# So, to avoid confusion, replace the NA values with "01-01-1900 00:00:01", which is a valid timestamp not present in the column already.
uberData$Drop.timestamp[which(is.na(uberData$Drop.timestamp))] <- "01-01-1900 00:00:01"

# Verify the NA values again
colSums(is.na(uberData))
# Drop.timestamp column does not have any NA values

# Entries without 'seconds' value
drop_Fmt_1 <- dmy_hm(uberData$Drop.timestamp, tz = "")
# Entries with 'seconds' value
drop_Fmt_2 <- dmy_hms(uberData$Drop.timestamp, tz = "")
# Replace the NA values in drop_Fmt_1 with the non-NA values in drop_Fmt_2
drop_Fmt_1[is.na(drop_Fmt_1)] <- drop_Fmt_2[!is.na(drop_Fmt_2)]
# Replace the Drop.timestamp column with the cleaned drop_Fmt_1
uberData$Drop.timestamp <- drop_Fmt_1

# Separate timestamp column into date and time columns
# Request
uberData$Request.date <- format(uberData$Request.timestamp, '%m-%d-%Y')
uberData$Request.time <- format(uberData$Request.timestamp, '%H:%M:%S')
# Remove Request.timestamp column
uberData <- uberData[,-5]

# Drop
uberData$Drop.date <- format(uberData$Drop.timestamp, '%m-%d-%Y')
uberData$Drop.time <- format(uberData$Drop.timestamp, '%H:%M:%S')
# Remove Drop.timestamp column
uberData <- uberData[,-5]

# Check unique dates on which cabs were requested and completed
unique(uberData$Request.date)
unique(uberData$Drop.date)
# We find that the dataset covers cab requests and drops for 5 particular dates

# Extract the hour from the time columns
# Request
uberData$Request.hour <- hour(hms(uberData$Request.time))
# Drop
uberData$Drop.hour <- hour(hms(uberData$Drop.time))

# Plot the number of cab requests in a particular hour for all five days
ggplot(uberData, aes(x = factor(Request.hour), fill = factor(Pickup.point))) +
    geom_bar(stat = 'count', position = "dodge") +
    labs(x = "Hour", y = "Number of Cabs Requested", title = "Cab Requests (Hourwise)", fill = "Pickup Point") + 
    theme_economist() + 
    scale_color_economist()

# PLOT OBSERVATION: 
# 1. Frequency of requests from City to Airport is highest during the morning hours 5 to 9.
# 2. Frequency of requests from Airport to City is highest during the evening hours 17 to 21.
# Based on these observations, it is suitable to partition the data into different time slots, in order to have a focussed approach for the analysis.

# Assign a time slot to each request hour
# Generate a sequence of numbers for each hour of the day
Request.hour <- c(0:23)

# Repeat a vector of time slot names a specific number of times such that the number of elements in the generated vector should correspond to 24
# Assign these time slots to each hour of the day
timeSlots <- data.frame(Request.TimeSlot = rep(c("Post_Midnight", "Early_Morning", "Morning", "Mid_Day", "Evening", "Night"), c(2, 3, 5, 7, 5, 2)), Request.hour)

# Merge the timeSlots data frame with uberData
uberData <- merge(uberData, timeSlots, by="Request.hour", all=T)

#####################################################################################
################################# Data Analysis #####################################
#####################################################################################

# Plot a bar chart for request time slots by number of requests
ggplot(uberData, aes(x = factor(Request.TimeSlot), fill = factor(Status))) +
    geom_bar(stat = "count", position = "stack", col = "black") +
    labs(x = "Cab Request Time Slots", y = "Number of Trip Requests (Status-wise)", fill="Trip Status", title = "Status-wise Requests against Different Time Slots") +
    scale_x_discrete(limits = c("Evening", "Morning", "Mid_Day", "Night", "Early_Morning", "Post_Midnight"))+
    scale_fill_discrete(limits = c("Trip Completed", "No Cars Available", "Cancelled")) + 
    theme_economist() + 
    scale_color_economist()

# PLOT OBSERVATION: The top two problems are as follows:
# 1. A lot of cab requests get CANCELLED in the MORNING Time Slot
# 2. A lot of cab requests get the NO CARS AVAILABLE response in the EVENING Time Slot.

# Let us analyse these two problems further

# Common functions
pickup_status_summary <- function(pickupPoint) {
    temp_df <- subset(uberData, Pickup.point == pickupPoint) %>% group_by(Status) %>% summarise(status_wise_requests = length(Status)) %>% mutate(percentage = paste0(round(100*status_wise_requests/sum(status_wise_requests), 2), '%'))
    return(temp_df)
}

timeslot_status_summary_1 <- function(timeSlot) {
    temp_df <- subset(uberData, Request.TimeSlot == timeSlot) %>% group_by(Status, Pickup.point) %>% summarise(status_wise_requests = length(Status)) %>% mutate(percentage = paste0(round(100*status_wise_requests/sum(status_wise_requests), 2), '%'))
    return(temp_df)
}

timeslot_status_summary_2 <- function(timeSlot) {
    temp_df <- subset(uberData, Request.TimeSlot == timeSlot) %>% group_by(Status) %>% summarise(status_wise_requests = length(Status)) %>% mutate(percentage = paste0(round(100*status_wise_requests/sum(status_wise_requests), 2), '%'))
    return(temp_df)
}

pickup_timeslot_status_summary <- function(pickupPoint, timeSlot) {
    temp_df <- subset(uberData, Pickup.point == pickupPoint & Request.TimeSlot == timeSlot) %>% group_by(Status) %>% summarise(status_wise_requests = length(Status)) %>% mutate(percentage = paste0(round(100*status_wise_requests/sum(status_wise_requests), 2), '%'))
    return(temp_df)
}

#####################################################################################
######################### Top Problem And its Analysis ##############################
#####################################################################################

# Plot a bar chart for Status by Number of Requests for Morning time slot
timeslot_status_summary_1("Morning") %>% ggplot(aes(y = status_wise_requests, x = factor(Status), fill = factor(Pickup.point))) + 
    geom_bar(stat = "identity", position = "stack", col = "black") +
    geom_text(aes(label = percentage), position = position_stack(vjust = 0.5), size = 4) +
    labs(y = "Morning Requests", x = "Status", fill="Trip Status", title = "Morning Requests vs. Status (Pickup point wise)") +
    scale_x_discrete(limits = c("Trip Completed", "Cancelled", "No Cars Available")) + 
    theme_economist() + 
    scale_color_economist()

# PLOT OBSERVATION: In the Morning Time Slot:
# 1. 97.27% of all CANCELLED Requests are from the City
# 2. 94.83% of all NO CARS AVAILABLE are from the City

#####################################################################################
####################### Second Top Problem And its Analysis #########################
#####################################################################################

# Plot a bar chart for Status by Number of Requests for Evening time slot
timeslot_status_summary_1("Evening") %>% ggplot(aes(y = status_wise_requests, x = factor(Status), fill = factor(Pickup.point))) + 
    geom_bar(stat = "identity", position = "stack", col = "black") +
    geom_text(aes(label = percentage), position = position_stack(vjust = 0.5), size = 4) +
    labs(y = "Evening Requests", x = "Status", fill="Trip Status", title = "Evening Requests vs. Status (Pickup point wise)") +
    scale_x_discrete(limits = c("No Cars Available", "Trip Completed", "Cancelled")) + 
    theme_economist() + 
    scale_color_economist()

# PLOT OBSERVATION: In the Evening Time Slot:
# 1. 63.86% of all CANCELLED Requests are from the Airport
# 2. 94.9% of all NO CARS AVAILABLE are from the Airport

#####################################################################################
########################## Demand Supply Gap Analysis ###############################
#####################################################################################
# Prepare the demand data frame
demand_df <- uberData %>% group_by(Request.TimeSlot) %>% summarise(requests = length(Status))
colnames(demand_df) <- c('TimeSlot', 'Overall')
demand_df[, 3] <- (uberData %>% subset(Pickup.point == "City") %>% group_by(Request.TimeSlot) %>% summarise(City = length(Status)))[, 2]
demand_df[, 4] <- (uberData %>% subset(Pickup.point == "Airport") %>% group_by(Request.TimeSlot) %>% summarise(Airport = length(Status)))[, 2]

# Prepare the demand supply gap data frame
func_demand_supply_gap <- function(df) {
    temp_df <- df %>% group_by(Request.TimeSlot) %>% summarise(requests = length(Status))
    colnames(temp_df) <- c('TimeSlot', 'Demand')
    temp_df[, 3] <- (df %>% subset(Status == "Trip Completed") %>% group_by(Request.TimeSlot) %>% summarise(Supply = length(Status)))[, 2]
    temp_df[, 4] <- (df %>% subset(Status == "Cancelled" | Status == "No Cars Available") %>% group_by(Request.TimeSlot) %>% summarise(Gap = length(Status)))[, 2]
    temp_df <- temp_df %>% mutate(Supply_percent = paste0(round(100*Supply/Demand, 2), '%')) %>% mutate(Gap_percent = paste0(round(100*Gap/Demand, 2), '%'))
    return(temp_df)
}

# Plot a bar graph for the Overall Demand for each Time Slot
p100 <- demand_df %>% ggplot(aes(x = TimeSlot, y = Overall, fill = TimeSlot)) + 
    geom_bar(stat = "identity", position = "stack", col = "black") +
    geom_text(aes(label = Overall), position = position_stack(vjust = 0.5), size = 4) +
    labs(y = "Demand (Overall)", fill="Time Slot", title = "Demand (Overall)") + 
    theme_economist() + 
    scale_color_economist()+
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank())

# Plot a bar graph for the Overall Demand for each Time Slot from City to Airport
p101 <- demand_df %>% ggplot(aes(x = TimeSlot, y = City, fill = TimeSlot)) + 
    geom_bar(stat = "identity", position = "stack", col = "black") +
    geom_text(aes(label = City), position = position_stack(vjust = 0.5), size = 4) +
    labs(y = "Demand (City)", x = "Time Slot", fill="Time Slot", title = "Demand (City)") + 
    theme_economist() + 
    scale_color_economist()+
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank())

# Plot a bar graph for the Overall Demand for each Time Slot from Airport to City
p102 <- demand_df %>% ggplot(aes(x = TimeSlot, y = Airport, fill = TimeSlot)) + 
    geom_bar(stat = "identity", position = "stack", col = "black") +
    geom_text(aes(label = Airport), position = position_stack(vjust = 0.5), size = 4) +
    labs(y = "Demand (Airport)", x = "Time Slot", fill="Time Slot", title = "Demand (Airport)") + 
    theme_economist() + 
    scale_color_economist()+
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank())

grid.arrange(p100, p101, p102, nrow=1, ncol=3)

# Plot a bar graph for the Overall Supply for each Time Slot
p1 <- func_demand_supply_gap(uberData) %>% ggplot(aes(x = TimeSlot, y = Supply_percent, fill = TimeSlot)) + 
    geom_bar(stat = "identity", position = "stack", col = "black") +
    geom_text(aes(label = Supply_percent), position = position_stack(vjust = 0.5), size = 4) +
    labs(y = "Supply (Overall)", x = "Time Slot", fill="Time Slot", title = "Supply vs. Time Slot (Overall)") + 
    theme_economist() + 
    scale_color_economist()

# Plot a bar graph for the Overall Gap for each Time Slot
p2 <- func_demand_supply_gap(uberData) %>% ggplot(aes(x = TimeSlot, y = Gap_percent, fill = TimeSlot)) + 
    geom_bar(stat = "identity", position = "stack", col = "black") +
    geom_text(aes(label = Gap_percent), position = position_stack(vjust = 0.5), size = 4) +
    labs(y = "Gap (Overall)", x = "Time Slot", fill="Time Slot", title = "Gap vs. Time Slot (Overall)") + 
    theme_economist() + 
    scale_color_economist()

# Plot a bar graph for the Supply from City to Airport for each Time Slot
p3 <- func_demand_supply_gap(uberData %>% subset(Pickup.point == "City")) %>% ggplot(aes(x = TimeSlot, y = Supply_percent, fill = TimeSlot)) + 
    geom_bar(stat = "identity", position = "stack", col = "black") +
    geom_text(aes(label = Supply_percent), position = position_stack(vjust = 0.5), size = 4) +
    labs(y = "Supply (City to Airport)", x = "Time Slot", fill="Time Slot", title = "Supply vs. Time Slot (City to Airport)") + 
    theme_economist() + 
    scale_color_economist()

# Plot a bar graph for the Gap from City to Airport for each Time Slot
p4 <- func_demand_supply_gap(uberData %>% subset(Pickup.point == "City")) %>% ggplot(aes(x = TimeSlot, y = Gap_percent, fill = TimeSlot)) + 
    geom_bar(stat = "identity", position = "stack", col = "black") +
    geom_text(aes(label = Gap_percent), position = position_stack(vjust = 0.5), size = 4) +
    labs(y = "Gap (City to Airport)", x = "Time Slot", fill="Time Slot", title = "Gap vs. Time Slot (City to Airport)") + 
    theme_economist() + 
    scale_color_economist()

# Plot a bar graph for the Supply from Airport to City for each Time Slot
p5 <- func_demand_supply_gap(uberData %>% subset(Pickup.point == "Airport")) %>% ggplot(aes(x = TimeSlot, y = Supply_percent, fill = TimeSlot)) + 
    geom_bar(stat = "identity", position = "stack", col = "black") +
    geom_text(aes(label = Supply_percent), position = position_stack(vjust = 0.5), size = 4) +
    labs(y = "Supply (Airport to City)", x = "Time Slot", fill="Time Slot", title = "Supply vs. Time Slot (Airport to City)") + 
    theme_economist() + 
    scale_color_economist()

# Plot a bar graph for the Gap from Airport to City for each Time Slot
p6 <- func_demand_supply_gap(uberData %>% subset(Pickup.point == "Airport")) %>% ggplot(aes(x = TimeSlot, y = Gap_percent, fill = TimeSlot)) + 
    geom_bar(stat = "identity", position = "stack", col = "black") +
    geom_text(aes(label = Gap_percent), position = position_stack(vjust = 0.5), size = 4) +
    labs(y = "Gap (Airport to City)", x = "Time Slot", fill="Time Slot", title = "Gap vs. Time Slot (Airport to City)") + 
    theme_economist() + 
    scale_color_economist()

grid.arrange(p1, p2, nrow=1, ncol=2)
grid.arrange(p3, p4, nrow=1, ncol=2)
grid.arrange(p5, p6, nrow=1, ncol=2)

# PLOT OBSERVATION: Supply is =>
# 1. Only 40.61% of overall morning requests
# 2. Only 33.48% of overall evening requests
# 3. Only 28.15% of morning requests from City to Airport
# 4. A very high 75.83% evening requests from City to Airport
# 5. A very high 89.67% morning requests from Airport to City
# 6. Only 20.72% evening requests from Airport to City