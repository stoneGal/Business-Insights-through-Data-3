
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
  "Air France Case Spreadsheet Supplement.xls",
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
air_france_df$NET_ROA <- air_france_df$NET_Amount / air_france_df$`Total Cost`

###################################################
### #3 - Create binary success variables
###################################################

###Applying business logic to ROA
##ROA success: 1 if NET_ROA is at least 4, otherwise 0
#This means the ad generated at least $4 in net return per $1 spent.

air_france_df$NET_ROA_binary <- ifelse(air_france_df$NET_ROA >= 4,
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
