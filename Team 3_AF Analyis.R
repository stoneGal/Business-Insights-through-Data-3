
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



