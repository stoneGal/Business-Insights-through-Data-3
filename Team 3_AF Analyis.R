
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

#Importing data from the Air France case spreadsheet
library(readxl)
air_france_df <- read_excel("Air France Case Spreadsheet Supplement.xls", 
                                                     sheet = "DoubleClick")
View(air_france_df)

#Creating basic scalars
num_ads <- nrow(airfrancedf)
num_obs <- ncol(airfrancedf)

#Creating an NET Amount vector
airfrancedf$NET_Amount <- airfrancedf$Amount - airfrancedf$`Total Cost`

#Creating an NET ROA Vector
airfrancedf$NET_ROA <- airfrancedf$NET_Amount / airfrancedf$`Total Cost`

#Applying business logic to ROA
#Benchmark is 4
#Defining success into a binary vector
airfrancedf$NET_ROA_binary <- ifelse(airfrancedf$NET_ROA >= 4, 
                                     1, 0)

#Applying business logic to CPC
#Benchmark is lower than 2
#Defining success into a binary vector
airfrancedf$CPC_binary <- ifelse(airfrancedf$`Avg. Cost per Click` <= 2, 
                                 1, 0)

#Applying business logic to Volume
#Benchmark is greater than 5
#Defining success into a binary vector
airfrancedf$Bookings_binary <- ifelse(airfrancedf$`Total Volume of Bookings` >= 5, 
                                      1, 0)

#Applying business logic to CTR
#Benchmark is greater than 8.24
#Defining success into a binary vector
airfrancedf$CTR_binary <- ifelse(airfrancedf$`Engine Click Thru %` >= 8.24, 
                                 1, 0)

#Generating a binary field that combines success in Volume, CPC and ROA.
airfrancedf$Optimal_binary <- ifelse(airfrancedf$Bookings_binary == 1 & 
                                       airfrancedf$CPC_binary == 1 & 
                                       airfrancedf$NET_ROA_binary == 1, 
                                     1, 0)


#Figuring out Keywords
print (airfrancedf$`Keyword Group`)

#We need a field that checks if "Air France" is contained in the Keyword Group field. 
#Creating a Branded vs Unbranded Field
airfrancedf$Branded <- ifelse(grepl("Air France", airfrancedf$`Keyword Group`, 
                                    ignore.case = TRUE), 1, 0)

#Checking if it worked
print (airfrancedf$Branded)
View(airfrancedf[airfrancedf$Branded == 1, c("Keyword Group", "Branded")])
View(airfrancedf[airfrancedf$Branded == 0, c("Keyword Group", "Branded")])

#Changing Match Type to factor
table(airfrancedf$`Match Type`)
airfrancedf$Match_factor <- factor(airfrancedf$`Match Type`, 
                                        levels = c("Broad", "Exact", "Standard", "Advanced", "N/A"))
