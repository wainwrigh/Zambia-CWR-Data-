######################################################################################################
####################  Determining CWR conservation selection for GIS mapping #########################
######################################################################################################

# 1.0 Set working directory and load the data ----
setwd ("C:/Users/wwainwright/Documents/R/Zambia_Analysis/LPOutput/Individual/Data/CWR/CostPerCWR")
Zambia <- read.csv("C:/Users/wwainwright/Documents/R/Zambia_Analysis/LPOutput/Individual/Data/LPOutput_IndividualNEW4.CSV")

# 2.0 Load the packages ----

# Load packages
library(tidyr)
library(dplyr)
library(ggplot2)
library(readr)
library(gridExtra)
library(scales)
library(psych)
library(corrplot)
library(openxlsx)


# 3.0 Subset the spreadsheet ----

# Subset by "objective"  
MCWR <- Zambia[Zambia$Objective == "DIVERSITY" ,] 

# 4.0 Using the aggregate function ----

# Aggregate by one category 
aggregate(Zambia$Richness, by=list(Category=Zambia$CommunityID), FUN=sum)

# Does this for all categories
M <- aggregate(. ~ CommunityID, MCWR, mean) #Determine mean or sum values associated with conservation by community
L <- aggregate(. ~ VignaJuncea, MCWR, sum) #Determine mean or sum values assoicated with conserving a specific CWR
names(Zambia)

# Tyring to write object "K" and "M" to Excel 
write.xlsx(L, file="L.xlsx")

# 5.0 Mathematic calculations based on data ----

# Makes all the 0 values in data sheet be N/A 
K[K==0]<-NA
complete.cases(K)
attach(K)  # Need too reattach K 

# Simple match functions
attach(K)
names(K) 
sum(Bidoffer)
Meanpriceha <- Bidoffer/Area  #create a variable that is Price per hectare
mean(Bidoffer/Area)
mean(Meanpriceha)

# Cost of conserving CWR accross selected communities, relative to price of farmer bid offers
CC <- K[,c("Bidoffer")] / K[,c("VignaUnguiculata", "VignaJuncea", "EleusineCoracana","PennisetumPurpureum",
                         "VignaUnguiculata", "SorghumBicolar","SolanumIncanum", "EleusineIndica",
                         "OryzaLongistaminata", "Cucumis.Zeyheri", "Richness")]
write.xlsx(CC, file="CCCost.xlsx")

# Mean price ha of conserving CWR accross selected communities, relative to price of farmer bid offers
MC <- Meanpriceha / K[,c("VignaUnguiculata", "VignaJuncea", "EleusineCoracana","PennisetumPurpureum",
                         "VignaUnguiculata", "SorghumBicolar","SolanumIncanum", "EleusineIndica",
                         "OryzaLongistaminata", "Cucumis.Zeyheri", "Richness")]
write.xlsx(MC, file="MCCost.xlsx")

# How many times each record occurs
table(VignaUnguiculata)
table(VignaJuncea)
table(EleusineCoracana)

# Other summations or counts of variables 
sum(DecisionCells)
mean(DecisionCells)
mean(Area)

# Calculate proportion of GMA sites selected  
G <- sum(GMA, na.rm=TRUE)
GN <- sum(nonGMA, na.rm=TRUE)
GNS <- sum(G+GN)
GND <- G/GNS*100
  
# 6.0 Cost per ha of Ecoregion 1 and 2 relative to model used ----

# Load the data 
Zambia <- read.csv("C:/Users/wwainwright/Documents/R/Zambia_Analysis/LPOutput/Individual/Data/LPOutput_IndividualNEW2.csv")
names(Zambia)

# Subset the data for different objectives, models and Ecoregions
Eco1 <- Zambia[with(Zambia, Ecoregion == "1"), ]
Eco2 <- Zambia[with(Zambia, Ecoregion == "2"), ]

B1 <- Zambia[with(Zambia, Objective == "Basic"), ]
D1 <- Zambia[with(Zambia, Objective == "Diversity"), ]
A1 <- Zambia[with(Zambia, Objective == "Area"), ]
E1 <- Zambia[with(Zambia, Objective == "Equitability"), ]

# Aggregate the data (to make numerical) then calculate the means of each column for the difference models 
B1N <- aggregate(. ~ ID,B1, mean)
D1N <- aggregate(. ~ ID,D1, mean)
A1N <- aggregate(. ~ ID,A1, mean)
E1N <- aggregate(. ~ ID,E1, mean)
B1means <- colMeans(B1N)
D1means <- colMeans(D1N)
A1means <- colMeans(A1N)
E1means <- colMeans(E1N)

B1sums <- colSums(B1N)
D1sums <- colSums(D1N)
A1sums <- colSums(A1N)
E1sums <- colSums(E1N)

# Tyring to write object "K" and "M" to Excel 
write.xlsx(K, file="CWRCommunitySelectionSum3.xlsx")
write.xlsx(M, file="CWRCommunitySelectionMean3.xlsx")

# Diveristy for Ecoregion 1
DL1 <- Zambia[with(Zambia, Ecoregion1 == "1" & Objective == "Diversity" & Model == "Low"), ]
DM1 <- Zambia[with(Zambia, Ecoregion1 == "1" & Objective == "Diversity" & Model == "Medium"), ]
DH1 <- Zambia[with(Zambia, Ecoregion1 == "1" & Objective == "Diversity" & Model == "High"), ]

# Diveristy for Ecoregion 2
DL2 <- Zambia[with(Zambia, Ecoregion2 == "1" & Objective == "Diversity" & Model == "Low"), ]
DM2 <- Zambia[with(Zambia, Ecoregion2 == "1" & Objective == "Diversity" & Model == "Medium"), ]
DH2 <- Zambia[with(Zambia, Ecoregion2 == "1" & Objective == "Diversity" & Model == "High"), ]

# Area for Ecoregion 1
AL1 <- Zambia[with(Zambia, Ecoregion1 == "1" & Objective == "Area" & Model == "Low"), ]
AM1 <- Zambia[with(Zambia, Ecoregion1 == "1" & Objective == "Area" & Model == "Medium"), ]
AH1 <- Zambia[with(Zambia, Ecoregion1 == "1" & Objective == "Area" & Model == "High"), ]

# Area for Ecoregion 2
AL2 <- Zambia[with(Zambia, Ecoregion2 == "1" & Objective == "Area" & Model == "Low"), ]
AM2 <- Zambia[with(Zambia, Ecoregion2 == "1" & Objective == "Area" & Model == "Medium"), ]
AH2 <- Zambia[with(Zambia, Ecoregion2 == "1" & Objective == "Area" & Model == "High"), ]

# Equitability for Ecoregion 1
EL1 <- Zambia[with(Zambia, Ecoregion1 == "1" & Objective == "Equitability" & Model == "Low"), ]
EM1 <- Zambia[with(Zambia, Ecoregion1 == "1" & Objective == "Equitability" & Model == "Medium"), ]
EH1 <- Zambia[with(Zambia, Ecoregion1 == "1" & Objective == "Equitability" & Model == "High"), ]

# Equitability for Ecoregion 2
EL2 <- Zambia[with(Zambia, Ecoregion2 == "1" & Objective == "Equitability" & Model == "Low"), ]
EM2 <- Zambia[with(Zambia, Ecoregion2 == "1" & Objective == "Equitability" & Model == "Medium"), ]
EH2 <- Zambia[with(Zambia, Ecoregion2 == "1" & Objective == "Equitability" & Model == "High"), ]

# Cost per ha under the different ecoregions

# Diversity Ecoregion 1
mean(DL1$PriceHa)
mean(DM1$PriceHa)
mean(DH1$PriceHa)

# Diversity Ecoregion 2
mean(DL2$PriceHa)
mean(DM2$PriceHa)
mean(DH2$PriceHa)

# Area Ecoregion 1
mean(AL1$PriceHa)
mean(AM1$PriceHa)
mean(AH1$PriceHa)

# Area Ecoregion 2
mean(EL2$PriceHa)
mean(EM2$PriceHa)
mean(EH2$PriceHa)

# Equitability Ecoregion 1
mean(EL1$PriceHa)
mean(EM1$PriceHa)
mean(EH1$PriceHa)

# Equitability Ecoregion 2
mean(EL2$PriceHa)
mean(EM2$PriceHa)
mean(EH2$PriceHa)