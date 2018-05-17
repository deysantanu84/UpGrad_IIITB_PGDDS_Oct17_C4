############################################################
# Spark Funds Investment Case Study (Group Project)
# Members:
# 1. Ashutosh Kumar (DDA1730347)
# 2. Medhavi Shruti (DDA1730035) 
# 3. Santanu Dey    (DDA1730043)
############################################################
library(dplyr)
library(tidyr)
library(stringr)
library(Hmisc)
############################################################
# Checkpoints - Part 1
############################################################
# Checkpoint 1: Data Cleaning 1
############################################################
# Load the companies and rounds data into two data frames 
# and name them companies and rounds2 respectively.
companies <- read.delim("companies.txt", na.strings = "")
rounds2 <- read.csv("rounds2.csv", na.strings = "")

# Understand the Data Set
# How many unique companies are present in rounds2?
# Convert all observations in the company_permalink comlumn to 
# uppercase letters
rounds2$company_permalink <- toupper(rounds2$company_permalink)
unique_comp_rounds2 <- length(unique(rounds2$company_permalink))

# How many unique companies are present in companies?
# Convert all observations in the permalink comlumn to uppercase 
# letters
companies$permalink <- toupper(companies$permalink)
unique_comp_companies <- length(unique(companies$permalink))

# In the companies data frame, which column can be used as the 
# unique key for each company? Write the name of the column.
# Answer: "permalink"

# Are there any companies in the rounds2 file which are not present 
# in companies ? Answer Y/N.
length(which(!(unique(rounds2$company_permalink) %in% 
                   companies$permalink)))
# Answer: N, as the result of the command above is 0

# Merge the two data frames so that all variables (columns) in the 
# companies frame are added to the rounds2 data frame. Name the 
# merged frame master_frame. How many observations are present 
# in master_frame ?
# Rename "company_permalink" column in rounds2 to "permalink"
names(rounds2)[1] <- paste("permalink")
# Merge rounds2 and companies data frames into master_frame
master_frame <- merge(rounds2, companies, by = "permalink", all = T)
nrow(master_frame)

# Find number of NA values in each column of master_frame
colSums(is.na(master_frame))

# The above output shows that only the "raised_amount_usd" column 
# has NA values
# Replace all NA values in the "raised_amount_usd" column with 0
master_frame$raised_amount_usd[is.na(master_frame$raised_amount_usd)] <- 0

############################################################
# Checkpoint 2: Funding Type Analysis
############################################################
# Goal 1: Investment Type Analysis
############################################################
# Group the master_frame based on funding round type and 
# Summarise the funding_round_type by the mean of raised_amount_usd
funding_types <- group_by(master_frame, funding_round_type)

avg_funding_types <- summarise(funding_types, average_funding_amt = 
                               mean(raised_amount_usd))

# Average funding amount of venture type
avg_funding_types[avg_funding_types$funding_round_type=="venture",]

# Average funding amount of angel type
avg_funding_types[avg_funding_types$funding_round_type=="angel",]

# Average funding amount of seed type
avg_funding_types[avg_funding_types$funding_round_type=="seed",]

# Average funding amount of private equity type
avg_funding_types[avg_funding_types$funding_round_type=="private_equity",]

# Considering that Spark Funds wants to invest between 5 to 15 million 
# USD per investment round, which investment type is the most 
# suitable for them?
avg_funding_types[(avg_funding_types$funding_round_type %in% 
                       c("venture", "angel", "seed", "private_equity"))
                  & (avg_funding_types$average_funding_amt
                     >= 5000000 
                     & avg_funding_types$average_funding_amt
                     <= 15000000), ]
# From the output of the above command, we know that the most 
# suitable investment type is "venture"

############################################################
# Checkpoints - Part 2
############################################################
# Checkpoint 3: Country Analysis
############################################################
# Goal 2: Country Analysis
############################################################
# 1. Spark Funds wants to see the top nine countries which have 
# received the highest total funding (across ALL sectors for 
# the chosen investment type)
# 2. For the chosen investment type, make a data frame named top9 
# with the top nine countries (based on the total investment amount 
# each country has received)
df_venture <- subset(master_frame, 
                     master_frame$funding_round_type == "venture")

# Group By the "country_code" column of df_venture
country_groups <- group_by(df_venture, country_code)

# Summarise "country_groups" on the total funding amount
country_total_funds <- summarise(country_groups, 
                                 total_funding = sum(raised_amount_usd))

# Remove NA observations, and sort in descending order of total funding,
# and get the top 9 observations into the top9 data frame
top9 <- arrange(country_total_funds[complete.cases(country_total_funds), 1:2], 
                desc(total_funding))[1:9, ]

# From top9, we get USA as the Top English Speaking country, discard
# CHN (China) as English is not an official language here, and then 
# find GBR (Great Britain) and IND (India) as the second and third 
# English Speaking countries respectively
# 1st :USA
# 2nd : GBR
# 3rd : IND
############################################################
# Checkpoint 4: Sector Analysis 1
############################################################
# Goal 3: Sector Analysis
############################################################
# Extract the primary sector of each category list from the 
# category_list column
df_venture$primary_sector <- str_split(df_venture$category_list,
                                       "\\|", simplify = TRUE)[,1]

# Use the mapping file 'mapping.csv' to map each primary sector 
# to one of the eight main sectors (Note that ‘Others’ is also 
# considered one of the main sectors)

# Load the mapping.csv file into mapping data frame
mapping <- read.csv("mapping.csv", na.strings = "")

# On manual analysis of the category_list column, it is found
# that the data is dirty and needs to be cleaned. All 'na'
# occurences in strings are present as '0'.
# Find all such occurences
mapping[grep("0", mapping$category_list), 1]

# Clean the mapping data frame by replacing occurrences of 0 in 
# category_list column by 'na'. Example: replace "A0lytics" with
# "Analytics"
mapping$category_list <- as.factor(gsub("([a-zA-z]*)0([ ]*)([a-zA-Z]+)", 
                                        "\\1na\\2\\3", mapping$category_list))

# Capitalize all category_lists for those starting with lowercase 'n'
mapping$category_list <- as.factor(capitalize(as.character(mapping$category_list)))

# Verify replacement is successful or not
mapping[grep("[nN]a", mapping$category_list), 1]

# Remove Blank column from mapping
mapping <- mapping[, -3]

# Convert mapping data frame from wide to long format
mapping_long <- gather(mapping, main_sector, is_main_sector, 2:9)

# Discard redundant rows having "is_main_sector" equal to 0
mapping_sectors <- mapping_long[!(mapping_long$is_main_sector == 0),]

# Remove the "is_main_sector" column as all entries are 1
mapping_sectors <- mapping_sectors[, -3]

# Rename the "category_list" column in mapping_sectors to 
# "primary_sector"
names(mapping_sectors)[1] <- paste("primary_sector")

# Merge the df_venture and the mapping_sectors data frames by the
# "primary_sector" column
df_venture_sectors <- merge(df_venture, mapping_sectors, 
                            by="primary_sector")

############################################################
# Checkpoint 5: Sector Analysis 2
############################################################
# Goal 3: Sector Analysis
############################################################
# Create three separate data frames D1, D2 and D3 for each of the 
# three countries containing the observations of funding type FT 
# falling within the 5-15 million USD range. The three data frames 
# should contain:
# 1. All the columns of the master_frame along with the primary 
# sector and the main sector
# 2. The total number (or count) of investments for each main 
# sector in a separate column
# 3. The total amount invested in each main sector in a separate column

# Subset df_venture_sectors to get all the funding types falling 
# within the 5-15 million USD range
funding_5_to_15 <- subset(df_venture_sectors, 
                          df_venture_sectors$raised_amount_usd >= 5000000 
                          & df_venture_sectors$raised_amount_usd <= 15000000)

# Create three separate data frames D1, D2, D3 for USA, GBR, IND respectively
D1 <- subset(funding_5_to_15, funding_5_to_15$country_code == "USA")
D2 <- subset(funding_5_to_15, funding_5_to_15$country_code == "GBR")
D3 <- subset(funding_5_to_15, funding_5_to_15$country_code == "IND")

# Create groups of main_sector in D1
sectors_groups_D1 <- group_by(D1, main_sector)

# Summarize sectors_groups_D1 on count of investments
sectors_investments_count_D1 <- summarise(sectors_groups_D1, 
                                          num_of_investments = length(raised_amount_usd))

# Summarize sectors_groups_D1 on total investments
sectors_investments_sum_D1 <- summarise(sectors_groups_D1, 
                                        total_investments = sum(raised_amount_usd))

# Merge the two summaries above by main_sector
sectors_investments_D1 <- merge(sectors_investments_count_D1, sectors_investments_sum_D1,
                                by = "main_sector")

# Merge sectors_investments_D1 by D1 by main_sectors
D1 <- merge(sectors_investments_D1, D1, by = "main_sector")
# Now D1 has all the required columns

# Create groups of main_sector in D2
sectors_groups_D2 <- group_by(D2, main_sector)

# Summarize sectors_groups_D2 on count of investments
sectors_investments_count_D2 <- summarise(sectors_groups_D2, 
                                          num_of_investments = length(raised_amount_usd))

# Summarize sectors_groups_D2 on total investments
sectors_investments_sum_D2 <- summarise(sectors_groups_D2, 
                                        total_investments = sum(raised_amount_usd))

# Merge the two summaries above by main_sector
sectors_investments_D2 <- merge(sectors_investments_count_D2, sectors_investments_sum_D2,
                                by = "main_sector")

# Merge sectors_investments_D2 by D2 by main_sectors
D2 <- merge(sectors_investments_D2, D2, by = "main_sector")
# Now D2 has all the required columns

# Create groups of main_sector in D3
sectors_groups_D3 <- group_by(D3, main_sector)

# Summarize sectors_groups_D3 on count of investments
sectors_investments_count_D3 <- summarise(sectors_groups_D3, 
                                          num_of_investments = length(raised_amount_usd))

# Summarize sectors_groups_D3 on total investments
sectors_investments_sum_D3 <- summarise(sectors_groups_D3, 
                                        total_investments = sum(raised_amount_usd))

# Merge the two summaries above by main_sector
sectors_investments_D3 <- merge(sectors_investments_count_D3, sectors_investments_sum_D3,
                                by = "main_sector")

# Merge sectors_investments_D3 by D3 by main_sectors
D3 <- merge(sectors_investments_D3, D3, by = "main_sector")
# Now D3 has all the required columns

# Total number of Investments (count)
# C1 (USA/D1)
nrow(D1)

# C2 (GBR/D2)
nrow(D2)

# C3 (IND/D3)
nrow(D3)

# Total amount of investment (USD)
# C1 (USA/D1)
sum(D1$raised_amount_usd)

# C2 (GBR/D2)
sum(D2$raised_amount_usd)

# C3 (IND/D3)
sum(D3$raised_amount_usd)

# Sort D1 in descending order of number of investments
sectors_investments_D1 <- arrange(sectors_investments_D1, desc(num_of_investments))

# Sort D2 in descending order of number of investments
sectors_investments_D2 <- arrange(sectors_investments_D2, desc(num_of_investments))

# Sort D3 in descending order of number of investments
sectors_investments_D3 <- arrange(sectors_investments_D3, desc(num_of_investments))

# Top sector for C1 (USA/D1)
top_sector_D1 <- sectors_investments_D1[1, 1]

# Second top sector for C1 (USA/D1)
second_top_sector_D1 <- sectors_investments_D1[2, 1]

# Third top sector for C1 (USA/D1)
third_top_sector_D1 <- sectors_investments_D1[3, 1]

# Top sector for C2 (GBR/D2)
top_sector_D2 <- sectors_investments_D2[1, 1]

# Second top sector for C2 (GBR/D2)
second_top_sector_D2 <- sectors_investments_D2[2, 1]

# Third top sector for C2 (GBR/D2)
third_top_sector_D2 <- sectors_investments_D2[3, 1]

# Top sector for C3 (IND/D3)
top_sector_D3 <- sectors_investments_D3[1, 1]

# Second top sector for C3 (IND/D3)
second_top_sector_D3 <- sectors_investments_D3[2, 1]

# Third top sector for C3 (IND/D3)
third_top_sector_D3 <- sectors_investments_D3[3, 1]

# Number of investments in top sector
# C1(USA/D1)
top_sector_inv_count_D1 <- sectors_investments_D1[1, 2]

# C2(GBR/D2)
top_sector_inv_count_D2 <- sectors_investments_D2[1, 2]

# C3(IND/D3)
top_sector_inv_count_D3 <- sectors_investments_D3[1, 2]

# Number of investments in second top sector
# C1(USA/D1)
second_top_sector_inv_count_D1 <- sectors_investments_D1[2, 2]

# C2(GBR/D2)
second_top_sector_inv_count_D2 <- sectors_investments_D2[2, 2]

# C3(IND/D3)
second_top_sector_inv_count_D3 <- sectors_investments_D3[2, 2]

# Number of investments in third top sector
# C1(USA/D1)
third_top_sector_inv_count_D1 <- sectors_investments_D1[3, 2]

# C2(GBR/D2)
third_top_sector_inv_count_D2 <- sectors_investments_D2[3, 2]

# C3(IND/D3)
third_top_sector_inv_count_D3 <- sectors_investments_D3[3, 2]

# For point 3 (top sector count-wise), which company received 
# the highest investment for country C1 (USA)?
company1_D1 <- subset(D1, D1$main_sector == top_sector_D1)
company1_D1_permalinks <- group_by(company1_D1, permalink)
company1_D1_total_inv_arranged <- arrange(summarise(company1_D1_permalinks, 
                                                    total_investment = sum(raised_amount_usd)), 
                                          desc(total_investment))

# Company with the highest investment in top sector for country C1 (USA)
companies[which(companies$permalink==toString(company1_D1_total_inv_arranged[1, 1])), "name"]

# For point 3 (top sector count-wise), which company received 
# the highest investment for country C2 (GBR)?
company1_D2 <- subset(D2, D2$main_sector == top_sector_D2)
company1_D2_permalinks <- group_by(company1_D2, permalink)
company1_D2_total_inv_arranged <- arrange(summarise(company1_D2_permalinks, 
                                                    total_investment = sum(raised_amount_usd)),
                                          desc(total_investment))

# Company with the highest investment in top sector for country C2 (GBR)
companies[which(companies$permalink==toString(company1_D2_total_inv_arranged[1, 1])), "name"]

# For point 3 (top sector count-wise), which company received 
# the highest investment for country C3 (IND)?
company1_D3 <- subset(D3, D3$main_sector == top_sector_D3)
company1_D3_permalinks <- group_by(company1_D3, permalink)
company1_D3_total_inv_arranged <- arrange(summarise(company1_D3_permalinks, 
                                                    total_investment = sum(raised_amount_usd)),
                                          desc(total_investment))

# Company with the highest investment in top sector for country C3 (IND)
companies[which(companies$permalink==toString(company1_D3_total_inv_arranged[1, 1])), "name"]

# For point 4 (second best sector count-wise), which company received 
# the highest investment for country C1 (USA)?
company2_D1 <- subset(D1, D1$main_sector == second_top_sector_D1)
company2_D1_permalinks <- group_by(company2_D1, permalink)
company2_D1_total_inv_arranged <- arrange(summarise(company2_D1_permalinks, 
                                                    total_investment = sum(raised_amount_usd)), 
                                          desc(total_investment))

# Company with the highest investment in second top sector for country C1 (USA)
companies[which(companies$permalink==toString(company2_D1_total_inv_arranged[1, 1])), "name"]

# For point 4 (second best sector count-wise), which company received 
# the highest investment for country C2 (GBR)?
company2_D2 <- subset(D2, D2$main_sector == second_top_sector_D2)
company2_D2_permalinks <- group_by(company2_D2, permalink)
company2_D2_total_inv_arranged <- arrange(summarise(company2_D2_permalinks, 
                                                    total_investment = sum(raised_amount_usd)), 
                                          desc(total_investment))

# Company with the highest investment in second top sector for country C2 (GBR)
companies[which(companies$permalink==toString(company2_D2_total_inv_arranged[1, 1])), "name"]

# For point 4 (second best sector count-wise), which company received 
# the highest investment for country C3 (IND)?
company2_D3 <- subset(D3, D3$main_sector == second_top_sector_D3)
company2_D3_permalinks <- group_by(company2_D3, permalink)
company2_D3_total_inv_arranged <- arrange(summarise(company2_D3_permalinks, 
                                                    total_investment = sum(raised_amount_usd)), 
                                          desc(total_investment))

# Company with the highest investment in second top sector for country C3 (IND)
companies[which(companies$permalink==toString(company2_D3_total_inv_arranged[1, 1])), "name"]
