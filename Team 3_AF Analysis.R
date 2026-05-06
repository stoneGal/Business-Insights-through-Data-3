
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

air_france_df$Bookings_binary <- ifelse(air_france_df$`Total Volume of Bookings` >= 5, 1, 0) #Closing ifelse statement for Bookings_binary

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


#######################Starting the new code ##############################################
#######################Starting the new code ##############################################
#######################Starting the new code ##############################################
#######################Starting the new code ##############################################

### #6 - Cleaning Bid strategy to remove the blanks and NAs in the raw data


table(air_france_df$`Bid Strategy`, useNA = "ifany") 

######replace NAs and blank strings with "Unknown" ###

air_france_df$`Bid Strategy`[is.na(air_france_df$`Bid Strategy`)]  <- "Unknown" 

air_france_df$`Bid Strategy`[air_france_df$`Bid Strategy` == ""]   <- "Unknown"

#####converting to factor so R treats it as a category instead of just "Text" 

air_france_df$Bid_Strategy_factor <- factor(air_france_df$`Bid Strategy`) 

######checckig the clean result 

table(air_france_df$Bid_Strategy_factor)

################################################### 

####Descriptive statistics for all KPIs 

################################################### 



#### Reusing the same desc_stats function from the German credit card project 

desc_stats <- function(x) { 
  x_clean  <- x[!is.na(x)] 
  my_min   <- min(x_clean) 
  my_max   <- max(x_clean) 
  my_mu    <- mean(x_clean) 
  my_sigma <- sd(x_clean) 
  my_vect  <- c(my_min, my_mu, my_sigma, my_max) 
  
  return(my_vect) 
  
} #closing desc_stats 


# financial KPIs 

desc_stats(air_france_df$NET_ROA) 

desc_stats(air_france_df$NET_Amount) 

desc_stats(air_france_df$Amount) 

desc_stats(air_france_df$`Total Cost`) 


############ Efficiency KPIs listed #####

desc_stats(air_france_df$`Avg. Cost per Click`) 
desc_stats(air_france_df$`Engine Click Thru %`) 
desc_stats(air_france_df$`Avg. Pos.`) 
desc_stats(air_france_df$`Total Volume of Bookings`) 
desc_stats(air_france_df$`Trans. Conv. %`) 



####Binary flags: using table() function instead of desc_stats() for 0/1 variables 

table(air_france_df$NET_ROA_binary)    # how many ads beat 400% ROA 

table(air_france_df$CPC_binary)        # how many ads have CPC under $2

table(air_france_df$Bookings_binary)   # how many ads have 5+ bookings

table(air_france_df$CTR_binary)        # how many ads beat 8.24% CTR

table(air_france_df$Optimal_binary)    # how many ads meet ALL three criteria

table(air_france_df$Branded)           # how many branded vs unbranded 




###8 Branded vs Unbranded comparison to compare performance between branded and unbranded.
################################################### 




# checking if branded status affect ROA using tapply

tapply(air_france_df$NET_ROA, 
       
       air_france_df$Branded, 
       
       mean, na.rm = TRUE) 


# checcck if branded status affect conversion rate 

tapply(air_france_df$`Trans. Conv. %`, 
       
       air_france_df$Branded, 
       
       mean, na.rm = TRUE) 



# checking if branded status affect cost per click

tapply(air_france_df$`Avg. Cost per Click`, 
       
       air_france_df$Branded, 
       
       mean, na.rm = TRUE) 


# checking if branded status affect bookings

tapply(air_france_df$`Total Volume of Bookings`, 
       
       air_france_df$Branded, 
       
       mean, na.rm = TRUE) 



# What % of branded ads are Optimal vs unbranded ads

tapply(air_france_df$Optimal_binary, 
       
       air_france_df$Branded, 
       
       mean, na.rm = TRUE) 



################################################### 
### #9  Engine-specific analysis 
################################################### 



# Average NET_ROA per publisher — which engine gives best return which inturn leads to success 

tapply(air_france_df$NET_ROA, 
       
       air_france_df$`Publisher Name`, 
       
       mean, na.rm = TRUE) 



# % of Optimal ads per publisher — which engine produces most winners 

tapply(air_france_df$Optimal_binary, 
       
       air_france_df$`Publisher Name`, 
       
       mean, na.rm = TRUE) 



# Average conversion rate per publisher 

tapply(air_france_df$`Trans. Conv. %`, 
       
       air_france_df$`Publisher Name`, 
       
       mean, na.rm = TRUE) 



# Average CPC per publisher — which engine is cheapest per click

tapply(air_france_df$`Avg. Cost per Click`, 
       
       air_france_df$`Publisher Name`, 
       
       mean, na.rm = TRUE) 



# Total bookings per publisher — which engine drives most volume 

tapply(air_france_df$`Total Volume of Bookings`, 
       
       air_france_df$`Publisher Name`, 
       
       sum, na.rm = TRUE) 



# Total NET_Amount per publisher, which engine is most profitable overall 

tapply(air_france_df$NET_Amount, 
       
       air_france_df$`Publisher Name`, 
       
       sum, na.rm = TRUE) 



################################################### 
####10 Normalize before modeling 
################################################### 

# Reusing the same normalize function 

normalize <- function(x) { 
  x_clean <- x[!is.na(x)] 
  min_max <- (x - min(x_clean)) / (max(x_clean) - min(x_clean)) 
  
  return(min_max) } #closing normalize 

air_france_df$bid_norm <- normalize(air_france_df$`Search Engine Bid`) 
air_france_df$cpc_norm <- normalize(air_france_df$`Avg. Cost per Click`) 
air_france_df$ctr_norm <- normalize(air_france_df$`Engine Click Thru %`) 
air_france_df$pos_norm <- normalize(air_france_df$`Avg. Pos.`) 
air_france_df$cost_norm <- normalize(air_france_df$`Total Cost`) 
air_france_df$bookings_norm <- normalize(air_france_df$`Total Volume of Bookings`) 
air_france_df$conv_norm     <- normalize(air_france_df$`Trans. Conv. %`) 

# Verify normalization worked — min= 0, max= 1 

desc_stats(air_france_df$bid_norm) 

desc_stats(air_france_df$cpc_norm) 

desc_stats(air_france_df$ctr_norm) 

desc_stats(air_france_df$pos_norm) 


################## 11 - Regression model: predicting bookings ####################### 

# build a clean modeling dataset normalized predictors + binary/factor categoricalsfilter out rows where bookings is NA to avoid model errors 

model_df <- air_france_df[!is.na(air_france_df$`Total Volume of Bookings`), ] 

##runningh the regression 
# X ==>independent variables = bid, position, CPC, CTR, match type, branded
# Y ==> dependent variable = Total Volume of Bookings 

bookings_model <- lm(`Total Volume of Bookings` ~ bid_norm + pos_norm + cpc_norm + ctr_norm + 
     conv_norm + Branded + Match_Category,data = model_df ) #closing lm 


#########Reading the results 
summary(bookings_model) 

##### 12 - Regression model: predicting NET_ROA 
#roa_model <- lm(NET_ROA ~ bid_norm + pos_norm + cpc_norm + ctr_norm + conv_norm + Branded + 
 #  Match_Category,data = model_df[!is.na(model_df$NET_ROA), ])  

sum(is.na(air_france_df$NET_ROA))    # counts NA
sum(is.infinite(air_france_df$NET_ROA))  # counts Inf
sum(is.nan(air_france_df$NET_ROA))   # counts NaN

# STEP 2: replace Inf and NaN with NA so R can filter them out cleanly
air_france_df$NET_ROA[is.infinite(air_france_df$NET_ROA)] <- NA
air_france_df$NET_ROA[is.nan(air_france_df$NET_ROA)] <- NA

# STEP 3: rebuild the clean modeling dataset filtering ALL bad Y values
roa_model_df <- model_df[!is.na(model_df$NET_ROA) & !is.infinite(model_df$NET_ROA) &
    !is.nan(model_df$NET_ROA), ]

# STEP 4: check how many clean rows we have left
nrow(roa_model_df)

# STEP 5: now run the regression safely
roa_model <- lm( NET_ROA ~ bid_norm + pos_norm + cpc_norm + ctr_norm + conv_norm + Branded +
    Match_Category, data = roa_model_df) #closing lm 
summary(roa_model) 
########### #13 - Optimal ad profile summary 

# Which % of optimal ads are branded
tapply(air_france_df$Branded, air_france_df$Optimal_binary,mean, na.rm = TRUE) 


# Which match type dominates optimal ads
table(air_france_df$Match_Category, air_france_df$Optimal_binary) 

# Which publisher has the most optimal ads
table(air_france_df$`Publisher Name`, air_france_df$Optimal_binary) 


# Final view of complete dataframe 
View(air_france_df) 









