
###################################################
###################################################
### Air France Business Case Analysis
### Created by Team 3:
### Bethel Abah
### Andrés Cravioto
### Alejandro Espinoza
### Oceana Reyes
### created on: 05/5/2026
###################################################
###################################################


###################################################
### #1 - Load packages and import data
###################################################

library(readxl)

air_france_df <- read_excel(
  "Business-Insights-through-Data-3/Air France Case Spreadsheet Supplement.xls", 
  sheet = "DoubleClick"
) #closing read_excel function

#Optional: view the imported dataset
View(air_france_df)

#Creating basic scalars to check dataset size
num_ads <- nrow(air_france_df)
num_obs <- ncol(air_france_df)

#Optional: Check column names before creating new variables
names(air_france_df)

###################################################
### #2 - Create performance metrics
###################################################

#Creating an NET_Amount vector (revenue after advertising cost)
air_france_df$NET_Amount <- air_france_df$Amount - air_france_df$`Total Cost`

#Creating an NET_ROA vector (net return generated for every $1 spent)
#If Total Cost is 0, NET_ROA is set to NA to avoid Inf values from division by zero.
air_france_df$NET_ROA <- ifelse(
  air_france_df$`Total Cost` > 0,
  air_france_df$NET_Amount / air_france_df$`Total Cost`,
  NA
) #Closing the ifelse statement for NET_ROA

###################################################
### #3 - Create binary success variables
###################################################

###Applying business logic to ROA
##ROA success: 1 if NET_ROA is at least 4, otherwise 0
#This means the ad generated at least $4 in net return per $1 spent.

air_france_df$NET_ROA_binary <- ifelse(
                                !is.na(air_france_df$NET_ROA) & air_france_df$NET_ROA >= 4,
                                1,
                                0
) #Closing ifelse statement for NET_ROA_binary

###Applying business logic to CPC
##CPC success: 1 if average cost per click is $2 or lower, otherwise 0

air_france_df$CPC_binary <- ifelse(air_france_df$`Avg. Cost per Click` <= 2, 
                                 1,
                                 0
) #Closing ifelse statement for CPC_binary

###Applying business logic to volume of bookings
##Booking success: 1 if the ad generated at least 5 bookings, otherwise 0

air_france_df$Bookings_binary <- ifelse(air_france_df$`Total Volume of Bookings` >= 5, 
                                      1, 
                                      0
) #Closing ifelse statement for Bookings_binary

###Applying business logic to CTR (click through rate)
##CTR is used as a diagnostic KPI, not part of the final Optimal variable.

air_france_df$CTR_binary <- ifelse(air_france_df$`Engine Click Thru %` >= 8.24, 
                                 1,
                                 0
) #Closing ifelse statement for CTR_binary

###Optimal ad:1 if the ad meets ROA, CPC, and booking success criteria
##This is our main business success variable.

#Generating a binary field that combines success in Volume, CPC and ROA.

air_france_df$Optimal_binary <- ifelse(air_france_df$Bookings_binary == 1 & 
                                         air_france_df$CPC_binary == 1 & 
                                         air_france_df$NET_ROA_binary == 1, 
                                     1,
                                     0
) #Closing ifelse statement for Optimal_binary

#Check how many ads are classified as optimal
table(air_france_df$Optimal_binary)

###################################################
### #4 - Classify branded and unbranded keywords
###################################################

###We need a field that checks if "air france" is contained in the Keyword Group and Keyword fields. 
##Clean text field first so N/A or unassigned values do not break the logic

#Clean text field - Keyword Group

keyword_group_clean <- ifelse(
  is.na(air_france_df$`Keyword Group`),
  "",
  air_france_df$`Keyword Group`
) #Closing ifelse statement for keyword_group_clean 

#Cleaning text field - Keyword

keyword_clean <- ifelse(
  is.na(air_france_df$Keyword),
  "",
  air_france_df$Keyword
) #Closing ifelse statement for keyword_clean

#Combine Keyword Group and Keyword into one searchable text field
branded_text <- paste(keyword_group_clean, keyword_clean)

##Create branded variable
#Branded = 1 if Keyword Group or Keyword contains "air france" or "airfrance"
#Unbranded = 0 if all other keywords,including "unassigned"

air_france_df$Branded <- ifelse(
  grepl("air france|airfrance", branded_text, ignore.case = TRUE),
  1,
  0
) #closing ifelse statement for branded variable

#Check if branded classification
table(air_france_df$Branded)

View(air_france_df[air_france_df$Branded == 1, c("Keyword", "Keyword Group", "Branded")])
View(air_france_df[air_france_df$Branded == 0, c("Keyword", "Keyword Group", "Branded")])

###################################################
### #5 - Classify match type
###################################################

#Check existing match type categories
table(air_france_df$`Match Type`, useNA = "ifany")

#Convert Match Type into a factor for cleaner analysis

air_france_df$Match_factor <- factor(
  air_france_df$`Match Type`,
  levels = c("Broad", "Exact", "Standard", "Advanced", "N/A")
) #Closing Match Type factor function

##Create a business-friendly match category:
#Focused = Exact or Advanced match types
#Broad/Standard = Broad or Standard
#Unknown = N/A or missing values

air_france_df$Match_Category <- ifelse(air_france_df$`Match Type` %in% c("Exact", "Advanced"),
                                "Focused",
                                  ifelse(air_france_df$`Match Type` %in% c("Broad", "Standard"),
                                         "Broad/Standard",
                                         "Unknown")
) #Closing ifelse statement for Match_Category

#Convert Match_Category into a factor
air_france_df$Match_Category <- factor(air_france_df$Match_Category,
                                       levels = c("Broad/Standard", "Focused", "Unknown")
) #Closing ifelse statement for Match_Category factor function

#Check final match classification
table(air_france_df$Match_Category, useNA = "ifany")

###################################################
### #6 - Descriptive Analysis
###################################################

###This section summarizes campaign performance by key business categories:
## 1. Publisher / search engine
## 2. Branded vs. unbranded keywords
## 3. Match category
## 4. Optimal vs non-optimal ads

###################################################
### #6.1 - Performance by publisher
###################################################

publisher_summary <- aggregate(
  cbind(
    NET_Amount, 
    NET_ROA, 
    `Avg. Cost per Click`,
    `Engine Click Thru %`,
    `Trans. Conv. %`,
    Amount,
    `Total Cost`,
    `Total Volume of Bookings`
) ~ `Publisher Name`,
  data = air_france_df,
FUN = function(x) mean(x, na.rm = TRUE)
) #Closing aggregate function for publisher_summary

#Rename columns for readability
colnames(publisher_summary) <- c(
  "Publisher_Name",
  "Avg_NET_Amount",
  "Avg_NET_ROA",
  "Avg_CPC",
  "Avg_CTR",
  "Avg_Conversion_Rate",
  "Avg_Revenue",
  "Avg_Total_Cost",
  "Avg_Bookings"
) #Closing colnames function for publisher_summary

#Sort publishers by highest average NET_ROA
publisher_summary <- publisher_summary[order(-publisher_summary$Avg_NET_ROA), ]

#View publisher_summary to see which search engine / publisher gives Air France the strongest return
print(publisher_summary) 

###################################################
### #6.2 - Performance by branded vs. unbranded keywords
###################################################

branded_summary <- aggregate(
  cbind(
    NET_Amount,
    NET_ROA,
    `Avg. Cost per Click`,
    `Engine Click Thru %`,
    `Trans. Conv. %`,
    Amount,
    `Total Cost`,
    `Total Volume of Bookings`
  ) ~ Branded,
  data = air_france_df,
  FUN = function(x) mean(x, na.rm = TRUE)
) #Closing aggregate function for branded_summary

#Rename columns for readability
colnames(branded_summary) <- c(
  "Branded",
  "Avg_NET_Amount",
  "Avg_NET_ROA",
  "Avg_CPC",
  "Avg_CTR",
  "Avg_Conversion_Rate",
  "Avg_Revenue",
  "Avg_Total_Cost",
  "Avg_Bookings"
)
#Create a readable keyword type label for the branded summary table
branded_summary$Keyword_Type <- ifelse(
  branded_summary$Branded == 1, 
  "Branded",
  "Unbranded"
) #Closing ifelse statement for keyword_type from branded_summary

#View branded_summary to see which branded keywords are more efficient than unbranded keywords
names(branded_summary)
print(branded_summary)

###################################################
### #6.3 - Performance by match category
###################################################

match_summary <- aggregate(
  cbind(
    NET_Amount,
    NET_ROA,
    `Avg. Cost per Click`,
    `Engine Click Thru %`,
    `Trans. Conv. %`,
    Amount,
    `Total Cost`,
    `Total Volume of Bookings`
  ) ~ Match_Category,
  data = air_france_df,
  FUN = function(x) mean(x, na.rm= TRUE)
) #Closing aggregate function for match_summary

#Rename columns for readability
colnames(match_summary) <- c(
  "Match_Category",
  "Avg_NET_Amount",
  "Avg_NET_ROA",
  "Avg_CPC",
  "Avg_CTR",
  "Avg_Conversion_Rate",
  "Avg_Revenue",
  "Avg_Total_Cost",
  "Avg_Bookings"
) #Closing colnames function for match_summary

#View match_summary to compare focused keywords vs broad/standard keywords
print(match_summary)

###################################################
### #6.4 - Performance by optimal vs non-optimal ads
###################################################

optimal_summary <- aggregate(
  cbind(
    NET_Amount,
    NET_ROA,
    `Avg. Cost per Click`,
    `Engine Click Thru %`,
    `Trans. Conv. %`,
    Amount,
    `Total Cost`,
    `Total Volume of Bookings`
  ) ~ Optimal_binary,
  data = air_france_df,
  FUN = function(x) mean(x, na.rm = TRUE)
) #Closing aggregate function for optimal_summary

#Rename columns for readability
colnames(optimal_summary) <-c(
  "Optimal_binary",
  "Avg_NET_Amount",
  "Avg_NET_ROA",
  "Avg_CPC",
  "Avg_CTR",
  "Avg_Conversion Rate",
  "Avg_Revenue",
  "Avg_Total_Cost",
  "Avg_Bookings"
) #Closing colnames function for optimal_summary

#Create readable labels for optimal vs non-optimal ads
optimal_summary$ad_Status <- ifelse(
  optimal_summary$Optimal_binary == 1,
  "Optimal",
  "Not_Optimal"
) #Closing ifelse statement for Ad_Status from optimal_summary

#View optimal_summary to compare successful vs unsuccessful ads
print(optimal_summary)
