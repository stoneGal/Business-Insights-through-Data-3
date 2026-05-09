
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
### 1 - Load packages and import data
###################################################

library(readxl)

air_france_df <- read_excel(
  "Business-Insights-through-Data-3/Air France Case Spreadsheet Supplement.xls",
  sheet = "DoubleClick"
  ) #closing read_excel function

#Optional: view the imported dataset
#View(air_france_df)

#Creating basic scalars to check dataset size
num_ads <- nrow(air_france_df)
num_obs <- ncol(air_france_df)

#Optional: Check column names before creating new variables
names(air_france_df)

###################################################
### 2 - Create performance metrics
###################################################

#Creating an NET_Amount vector (revenue after advertising cost)
air_france_df$NET_Amount <- air_france_df$Amount - air_france_df$`Total Cost`

##Creating an NET_ROA variable
#If Total Cost is 0, NET_ROA is set to NA to avoid Inf value
air_france_df$NET_ROA <- ifelse(
  air_france_df$`Total Cost` > 0,
  air_france_df$NET_Amount / air_france_df$`Total Cost`,
  NA
) #Closing the ifelse statment for NET_ROA variable

###################################################
### 3 - Create binary success variables
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
### 4 - Classify branded and unbranded keywords
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

#View(air_france_df[air_france_df$Branded == 1, c("Keyword", "Keyword Group", "Branded")])
#View(air_france_df[air_france_df$Branded == 0, c("Keyword", "Keyword Group", "Branded")])

###################################################
### 5 - Classify match type
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
### 6 - Cleaning Bid Strategy to remove the blanks and NAs in the raw data
###################################################

table(air_france_df$`Bid Strategy`, useNA = "ifany") 

#Replace NAs and blank strings with "Unknown"
air_france_df$`Bid Strategy`[is.na(air_france_df$`Bid Strategy`)]  <- "Unknown" 
air_france_df$`Bid Strategy`[air_france_df$`Bid Strategy` == ""]   <- "Unknown"

#Converting to factor so R treats it as a category instead of just "Text" 
air_france_df$Bid_Strategy_factor <- factor(air_france_df$`Bid Strategy`) 

#Checking the clean result 
table(air_france_df$Bid_Strategy_factor)

###Descriptive statistics for all KPIs 
## Reusing the same desc_stats function learned from class 

desc_stats <- function(x) { 
  x_clean  <- x[!is.na(x)] 
  my_min   <- min(x_clean) 
  my_max   <- max(x_clean) 
  my_mu    <- mean(x_clean) 
  my_sigma <- sd(x_clean) 
  my_vect  <- c(my_min, my_mu, my_sigma, my_max) 
  
  return(my_vect) 
  
} #closing desc_stats 

#Financial KPIs 

desc_stats(air_france_df$NET_ROA) 

desc_stats(air_france_df$NET_Amount) 

desc_stats(air_france_df$Amount) 

desc_stats(air_france_df$`Total Cost`) 

### Efficiency KPIs listed ###

desc_stats(air_france_df$`Avg. Cost per Click`) 
desc_stats(air_france_df$`Engine Click Thru %`) 
desc_stats(air_france_df$`Avg. Pos.`) 
desc_stats(air_france_df$`Total Volume of Bookings`) 
desc_stats(air_france_df$`Trans. Conv. %`) 

##Binary flags: using table() function instead of desc_stats() for 0/1 variables 

table(air_france_df$NET_ROA_binary)    #How many ads beat 400% ROA?

table(air_france_df$CPC_binary)        #How many ads have CPC under $2?

table(air_france_df$Bookings_binary)   #How many ads have 5+ bookings?

table(air_france_df$CTR_binary)        #How many ads beat 8.24% CTR?

table(air_france_df$Optimal_binary)    #How many ads meet ALL three criteria?

table(air_france_df$Branded)           #How many branded vs unbranded?


###################################################
### 7 - Branded vs Unbranded comparison
################################################### 

#Checking if branded status affect ROA using tapply

tapply(air_france_df$NET_ROA, 
       
       air_france_df$Branded, 
       
       mean, na.rm = TRUE) 

#Check if branded status affect conversion rate 

tapply(air_france_df$`Trans. Conv. %`, 
       
       air_france_df$Branded, 
       
       mean, na.rm = TRUE) 

#Checking if branded status affect cost per click

tapply(air_france_df$`Avg. Cost per Click`, 
       
       air_france_df$Branded, 
       
       mean, na.rm = TRUE) 

#Checking if branded status affect bookings

tapply(air_france_df$`Total Volume of Bookings`, 
       
       air_france_df$Branded, 
       
       mean, na.rm = TRUE) 

#What % of branded ads are Optimal vs unbranded ads

tapply(air_france_df$Optimal_binary, 
       
       air_france_df$Branded, 
       
       mean, na.rm = TRUE) 

#Is there a relation between Branded and higher Transaction %
head(air_france_df$`Trans. Conv. %`)
#Looking at Tran. Con. % there is a 900 value, which makes no sense and is certainly an error. 
#Changing it to 90.
library(tidyverse)
air_france_df <- air_france_df %>%
  mutate(`Trans. Conv. %` = if_else(`Trans. Conv. %` == 900, 90, `Trans. Conv. %`))
#checking if it worked
air_france_df %>% 
  summarise(max_conv = max(`Trans. Conv. %`, na.rm = TRUE))

#Building a linear regression model
branded_lm_model <- lm(`Trans. Conv. %` ~ Branded, data = air_france_df)
summary(branded_lm_model)

#P value shows some significance but R squared sucks, so we can ignore this model.
###########
#Is there a relation between Branded and higher Click Thru %
#Building a linear regression model
brandedclickthru_lm_model <- lm(`Engine Click Thru %` ~ Branded, data = air_france_df)
summary(brandedclickthru_lm_model)

#P is much stronger here but R squared is still too low, ignore this model also.

###################################################
### 8 - Engine-specific analysis 
################################################### 

#Average NET_ROA per publisher — which engine gives best return which inturn leads to success 

tapply(air_france_df$NET_ROA, 
       
       air_france_df$`Publisher Name`, 
       
       mean, na.rm = TRUE) 

#% of Optimal ads per publisher — which engine produces most winners 

tapply(air_france_df$Optimal_binary, 
       
       air_france_df$`Publisher Name`, 
       
       mean, na.rm = TRUE) 

#Average conversion rate per publisher 

tapply(air_france_df$`Trans. Conv. %`, 
       
       air_france_df$`Publisher Name`, 
       
       mean, na.rm = TRUE) 

#Average CPC per publisher — which engine is cheapest per click

tapply(air_france_df$`Avg. Cost per Click`, 
       
       air_france_df$`Publisher Name`, 
       
       mean, na.rm = TRUE) 

#Total bookings per publisher — which engine drives most volume 

tapply(air_france_df$`Total Volume of Bookings`, 
       
       air_france_df$`Publisher Name`, 
       
       sum, na.rm = TRUE) 

#Total NET_Amount per publisher, which engine is most profitable overall 

tapply(air_france_df$NET_Amount, 
       
       air_france_df$`Publisher Name`, 
       
       sum, na.rm = TRUE) 

###################################################
### 9 - Normalize data before modeling 
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

###################################################
### 10 - Regression model: predicting bookings 
################################################### 

#Build a clean modeling dataset normalized predictors and categorical variables.
#Rows with missing bookings are filtered out to avoid model errors.

model_df <- air_france_df[!is.na(air_france_df$`Total Volume of Bookings`), ] 

##Running the regression 
# X ==>independent variables = bid, position, CPC, CTR, match type, branded
# Y ==> dependent variable = Total Volume of Bookings 

bookings_model <- lm(`Total Volume of Bookings` ~ bid_norm + pos_norm + cpc_norm + ctr_norm + 
     conv_norm + Branded + Match_Category,data = model_df ) #closing lm 


#Reading the results 
summary(bookings_model) 

###################################################
### 11 - Regression model: predicting NET_ROA 
###################################################

##roa_model <- lm(NET_ROA ~ bid_norm + pos_norm + cpc_norm + ctr_norm + conv_norm + Branded + 
#Match_Category,data = model_df[!is.na(model_df$NET_ROA), ])  

sum(is.na(air_france_df$NET_ROA))    # counts NA
sum(is.infinite(air_france_df$NET_ROA))  # counts Inf
sum(is.nan(air_france_df$NET_ROA))   # counts NaN

#STEP 2: replace Inf and NaN with NA so R can filter them out cleanly
air_france_df$NET_ROA[is.infinite(air_france_df$NET_ROA)] <- NA
air_france_df$NET_ROA[is.nan(air_france_df$NET_ROA)] <- NA

#STEP 3: rebuild the clean modeling dataset filtering ALL bad Y values
roa_model_df <- model_df[!is.na(model_df$NET_ROA) & !is.infinite(model_df$NET_ROA) &
    !is.nan(model_df$NET_ROA), ]

#STEP 4: check how many clean rows we have left
nrow(roa_model_df)

#STEP 5: now run the regression safely
roa_model <- lm( NET_ROA ~ bid_norm + pos_norm + cpc_norm + ctr_norm + conv_norm + Branded +
    Match_Category, data = roa_model_df) #closing lm 
summary(roa_model) 

###################################################
### 12 - Optimal ad profile summary
################################################### 

#Which % of optimal ads are branded
tapply(air_france_df$Branded, air_france_df$Optimal_binary,mean, na.rm = TRUE) 


#Which match type dominates optimal ads
table(air_france_df$Match_Category, air_france_df$Optimal_binary) 

#Which publisher has the most optimal ads
table(air_france_df$`Publisher Name`, air_france_df$Optimal_binary) 


#Final view of complete dataframe 
#View(air_france_df) 

###################################################
### 13 - Data visualization using ggplot2
###################################################

library(ggplot2)

#Create readable labels for charts: Keyword_Type & Ad_Status
air_france_df$`Keyword_Type` <- ifelse(
  air_france_df$Branded == 1,
  "Branded",
  "Unbranded"
) #Closing ifelse statement for Keyword_Type

air_france_df$Ad_Status <- ifelse(air_france_df$Optimal_binary == 1,
                                  "Optimal",
                                  "Not Optimal"
) #Closing the ifelse statement for Ad_Status

###################################################
### 13.1 - Optimal ads by branded vs. unbranded
###################################################

plot_keyword_optimal <- ggplot(air_france_df, aes(x = Keyword_Type, fill = Ad_Status)) +
  geom_bar(position = "fill") +
  labs(
    title = "Share of Optimal Ads by Keyword Type",
    x = "Keyword Type",
    y = "Share of Ads",
    fill = "Ad Status"
  ) #Closing labs function

#Print plot_keyword_optimal
plot_keyword_optimal

###################################################
### 13.2 - Total bookings by publisher
###################################################

##This summary table calculates total booking volume by publisher.
#It helps identify which search engine/publisher contributes the most scale.

publisher_bookings <- aggregate(
  `Total Volume of Bookings` ~ `Publisher Name`,
  data = air_france_df,
  FUN = sum
) #Closing aggregate function for publisher_bookings

#Rename columns for readability and easier plotting
colnames(publisher_bookings) <- c(
  "Publisher_Name",
  "Total_Bookings"
) #Closing colnames function for publisher_bookings

##This chart ranks publishers by total booking volume
#It helps show which publisher drives the highest number of bookings

plot_publisher_bookings <- ggplot(
  data = publisher_bookings,
  mapping = aes(x = reorder(Publisher_Name, Total_Bookings), y = Total_Bookings)
  ) + geom_col() +
      coord_flip() +
        labs(
          title = "Total Bookings by Publisher",
          x = "Publisher",
          y = "Total Bookings"
        ) #Closing labs function

#Print plot_publisher_bookings
plot_publisher_bookings

###################################################
### 13.3 - Optimal ad rate by publisher
###################################################

##This summary table calculates the share of ads classified as "Optimal" for each publisher.
#Since Optimal_binary is coded as 1 = Optimal and 0 = Not Optimal, the mean gives the optimal ad rate.

publisher_optimal_rate <- aggregate(
  Optimal_binary ~ `Publisher Name`,
  data = air_france_df,
  FUN = mean
) #Closing aggregate function for publisher_optimal_rate

#Rename columns for readability and easier plotting
colnames(publisher_optimal_rate) <- c(
  "Publisher_Name",
  "Optimal_Rate"
) #Closing colnames function for publisher_optimal_rate

##This chart ranks publishers by optimal ad rate.
#It helps identify which publishers are more efficient, not just which ones generate the most booking volume.

plot_optimal_rate <- ggplot(
  data = publisher_optimal_rate,
  mapping = aes(x = reorder(Publisher_Name, Optimal_Rate), y = Optimal_Rate)
  ) + geom_col() +
  coord_flip() +
  labs(
  title = "Optimal Ad Rate by Publisher",
  x = "Publisher",
  y = "Optimal Ad Rate"
  ) #Closing labs function

#Print plot_optimal_rate
plot_optimal_rate

###################################################
### 13.4 - Average CPC by keyword type
###################################################

##This summary table calculates average cost per click by keyword type.
#It helps compare cost efficiency between branded and unbranded keywords.

keyword_cpc <-aggregate(
  `Avg. Cost per Click` ~ Keyword_Type,
  data = air_france_df,
  FUN = mean
) #Closing aggregate function for keyword_cpc

#Rename columns for readability and easier plotting
colnames(keyword_cpc) <- c(
  "Keyword_Type",
  "Avg_CPC"
) #Closing colnames function for keyword_cpc

##This chart compares average CPC between branded and unbranded keywords.
#It helps show whether branded keywords are more cost-efficient.

plot_keyword_cpc <- ggplot(
  data = keyword_cpc,
  mapping = aes(x = Keyword_Type, y = Avg_CPC)
  ) + geom_col() +
  labs(
    title = "Average CPC by Keyword Type",
    x = "Keyword Type",
    y = "Average Cost per Click"
    ) #Closing labs function

#Print plot_keyword_cpc
plot_keyword_cpc

###################################################
### 13.5 - Optimal ads by match category
###################################################

##This chart compares Broad/Standard, Focused, and Unknown match categories by the share of ads classified as Optimal or Not Optimal.
#It helps evaluate whether match type is connected to campaign success.

plot_match_category <- ggplot(
  data = air_france_df,
  mapping = aes(x = Match_Category, fill = Ad_Status)
  ) + geom_bar(position = "fill") +
  labs(
    title = "Share of Optimal Ads by Match Category",
    x = "Match Category", 
    y = "Share of Ads",
    fill= "Ad Status"
  ) #Closing labs function

#Print plot_match_category
plot_match_category

###################################################
### 14 - Gini decision tree
###################################################

##This section builds a decision tree to predict whether an ad is Optimal or Not Optimal.
#The tree uses Gini method to identify which campaign variables best split the data.
#This supports the predictive analysis part of the project.

library(rpart)
library(rpart.plot)

#Convert Optimal_binary into a factor because decision trees for classification need a target variable to be categorical.
air_france_df$Optimal_factor <- factor(
  air_france_df$Optimal_binary,
  levels = c(0, 1),
  labels = c("Not Optimal", "Optimal")
) #Closing factor function for Optimal_factor

#Build a clean dataset for the decision tree.
#We include campaign features that may help classify an ad as Optimal or Not Optimal.
tree_df <- air_france_df[
  !is.na(air_france_df$Optimal_factor),
  c(
    "Optimal_factor",
    "Branded",
    "Match_Category",
    "Avg. Cost per Click",
    "Engine Click Thru %",
     "Publisher Name"
  )
] #Closing tree_df subset

##Check the target variable distribution.
#This shows how many ads are Optimal vs. Not Optimal.
table(tree_df$Optimal_factor)

##Build the Gini decision tree.
#The dependent variable is Optimal_factor.
#The independent variables are branded status, match type, bid strategy, CPC, CTR, conversion rate, and publisher.
optimal_tree <- rpart(
  Optimal_factor ~ Branded + Match_Category +
    `Avg. Cost per Click` + `Engine Click Thru %` + `Publisher Name`,
  data = tree_df,
  method = "class",
  parms = list(split = "gini"),
  control = rpart.control(
    cp = 0.001,
    minsplit = 20,
    minbucket = 7
  )  
) #Closing rpart function for optimal_tree

#Print the decision tree model output.
print(optimal_tree)

##Display the variable importance from the tree.
#This helps identify which variables contributed most to classifying optimal ads.
optimal_tree$variable.importance

##Plot the decision tree 
#This visual can be used in Predictive Analysis section of the presentation.
png("decision_tree.png", width = 1700, height = 1400, res = 150)# Creating a png with adjusted dimensions
par(mar = c(0.5, 0.5, 1, 0.5))  # Adjust margins if needed
rpart.plot(
  optimal_tree,
  type = 2,
  extra = 104,
  fallen.leaves = TRUE,
  main = "Gini Decision Tree for Optimal Ads",
  cex = 0.6, #Controls the font size. 
  space = 2 #Adds vertical space between the nodes. 
) #Closing the rpart.plot function
dev.off()# CLosing the .png function.

###################################################
### 15 - Decision tree model evaluation using caret
###################################################

##Evaluate the decision tree using caret.
#The confusion matrix compares predicted vs actual classifications and reports model performance metrics such as accuracy, sensitivity, specificity, and balanced accuracy.

library(caret)

#Predict classes using the decision tree model
tree_predictions <- predict(
  optimal_tree,
  newdata = tree_df,
  type = "class"
) #Closing the predict function for tree_predictions

#Check predicted class distribution.
table(tree_predictions)

##Create a confusion matrix comparing actual vs. predicted classes.
#The positive class is set to "Optimal" because that is the main outcome of interest.
tree_confusion_matrix <- confusionMatrix(
  data = tree_predictions,
  reference = tree_df$Optimal_factor,
  positive = "Optimal"
) #Closing confusionMatrix function

#View confusion matrix and performance metrics
tree_confusion_matrix

###################################################
### 15.1 - Train/test evaluation for decision tree
###################################################

###Evaluate the decision tree using train/test split.
##set.seed() makes the random split reproducible so results stay the same each time the script is run.

set.seed(123)

##Instead of dropping rows with missing values, we clean and recode them so the dataset stays intact for modeling.
#Create a copy of tree_df for model validation.
tree_df_clean <- tree_df

#Check missing values by column before cleaning/recode.
colSums(is.na(tree_df_clean))

##Replace missing numeric values with the column median.
#This keeps the rows in the dataset instead of removing them.
tree_df_clean$`Avg. Cost per Click`[is.na(tree_df_clean$`Avg. Cost per Click`)] <- median(
  tree_df_clean$`Avg. Cost per Click`,
  na.rm = TRUE
) #Closing median function

tree_df_clean$`Engine Click Thru %`[is.na(tree_df_clean$`Engine Click Thru %`)] <- median(
  tree_df_clean$`Engine Click Thru %`,
  na.rm = TRUE
) #Closing median function


#Replace missing categorical values with "Unknown"
tree_df_clean$Match_Category <- as.character(tree_df_clean$Match_Category)
tree_df_clean$Match_Category[is.na(tree_df_clean$Match_Category)] <- "Unknown"
tree_df_clean$Match_Category <- factor(tree_df_clean$Match_Category)


tree_df_clean$`Publisher Name` <- as.character(tree_df_clean$`Publisher Name`)
tree_df_clean$`Publisher Name`[is.na(tree_df_clean$`Publisher Name`)] <- "Unknown"
tree_df_clean$`Publisher Name` <- factor(tree_df_clean$`Publisher Name`)

#Check missing values again after cleaning
colSums(is.na(tree_df_clean))

#Check that the target variable still has both classes.
table(tree_df_clean$Optimal_factor)

##Create train/test split using caret.
#70% of the data is used for training and 30% is used for testing.
train_index <- createDataPartition(
  tree_df_clean$Optimal_factor,
  p = 0.70,
  list = FALSE
) #Closing createDataPartition function

#Create training and testing datasets.
train_tree_df <- tree_df_clean[train_index, ]
test_tree_df <- tree_df_clean[-train_index, ]

#Build the Gini decision tree on the training data only.
optimal_tree_train <- rpart(
  Optimal_factor ~ Branded + Match_Category + `Avg. Cost per Click` + `Engine Click Thru %` + `Publisher Name`,
  data = train_tree_df,
  method = "class",
  parms = list(split = "gini"),
  control = rpart.control(
    cp = 0.001,
    minsplit = 20,
    minbucket = 7
  )
) #Closing rpart function for optimal_tree_train

#Predict classes on the test data only
test_predictions <- predict(
  optimal_tree_train,
  newdata = test_tree_df,
  type = "class"
) #Closing predict function for test_predictions

#Check that predictions and actual values have the same length
length(test_predictions)
length(test_tree_df$Optimal_factor)

#Confirm that predicted and actual values have the same length before confusion matrix.
length(test_predictions) == length(test_tree_df$Optimal_factor)

#Create confusion matrix for test data.
test_confusion_matrix <- confusionMatrix(
  data = test_predictions,
  reference = test_tree_df$Optimal_factor,
  positive = "Optimal"
) #Closing confusionMatrix function for test_confusion_matrix

#View train/test model performance
test_confusion_matrix

###################################################
### 16 - Final data quality checks
###################################################

##This section checks that the key variables used in the analysis were created correctly.

#Check dataset size
num_ads
num_obs

###Check that NET_ROA does not contain Inf or NaN values
sum(is.infinite(air_france_df$NET_ROA))
sum(is.nan(air_france_df$NET_ROA))
##There is one NA in NET_ROA, which is expected if a row has Total Cost = 0.
#This prevents division by zero from creating Inf values.
sum(is.na(air_france_df$NET_ROA))

#Check binary variables for missing values  
sum(is.na(air_france_df$NET_ROA_binary))
sum(is.na(air_france_df$CPC_binary))
sum(is.na(air_france_df$Bookings_binary))
sum(is.na(air_france_df$CTR_binary))
sum(is.na(air_france_df$Optimal_binary))

#Check final category distributions
table(air_france_df$Optimal_binary)
table(air_france_df$Branded)
table(air_france_df$Match_Category)
table(air_france_df$Bid_Strategy_factor)

#Check chart labels
table(air_france_df$Keyword_Type)
table(air_france_df$Ad_Status)

###################################################
### 17 - Save charts for presentation
###################################################

##This section saves final ggplot charts as PNG files.
#These files will be inserted in the business case presentation.

if(!dir.exists("outputs")){
  dir.create("outputs")
} #Closing if statement

ggsave(
  filename = "outputs/share_optimal_ads_by_keyword_type.png",
  plot = plot_keyword_optimal,
  width = 8,
  height = 5
) #Closing ggsave function

ggsave(
  filename = "outputs/total_bookings_by_publisher.png",
  plot = plot_publisher_bookings,
  width = 8,
  height = 5
) #Closing ggsave function

ggsave(
  filename = "outputs/optimal_ad_rate_by_publisher.png",
  plot = plot_optimal_rate,
  width = 8,
  height = 5
) #Closing ggsave function

ggsave(
  filename = "outputs/average_cpc_by_keyword_type.png",
  plot = plot_keyword_cpc,
  width = 8,
  height = 5
) #Closing ggsave function

ggsave(
  filename = "outputs/share_optimal_ads_by_match_category.png",
  plot = plot_match_category,
  width = 8,
  height = 5
) #Closing ggsave function

#Save Gini decision tree plot as PNG for presentation use
png(
  filename = "outputs/gini_decision_tree_optimal_ads.png",
  width = 1200,
  height = 800
)

rpart.plot(
  optimal_tree,
  type = 2,
  extra = 104,
  fallen.leaves = TRUE,
  main = "Gini Decision Tree for Optimal Ads"
)

dev.off()
