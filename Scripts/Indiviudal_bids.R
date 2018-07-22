###########################################################################################################
###############################  Graph Plotting indiviudal bid offers #####################################
###########################################################################################################

#Load the data 
setwd ("C:/Users/wwainwright/Documents/R/Zambia_Analysis")
Zambia <- read.csv("C:/Users/wwainwright/Documents/R/Zambia_Analysis/Data/Individual_Final.csv")

#Load the packages

# Libraries
library(tidyr)
library(dplyr)
library(ggplot2)
library(readr)
library(gridExtra)

# Explore the data
names(Zambia)
show(Zambia)

# Barplots

#Non-ordered bar chart of respondent against cost per ha (AMOB)
(ww_barplot2 <- ggplot(Zambia1, aes(x=Respondent, y=USD_Ha_AMOB, colour=Ecoregion)) +
  geom_bar(position=position_dodge(), stat="identity", colour="black", fill="#00868B") +
  theme_bw() +
  ylab("Total cost per hectare (USD) AMOB") +                             
  xlab("Respondent"))

#Low to high (with small gap between bars) for B (USD/Ha)

(ww_barplot3 <- ggplot(Zambia1, aes(x=reorder(Respondent, USD_Ha_AMOB), y=USD_Ha_AMOB, colour=Ecoregion)) +
  geom_bar(position=position_dodge(width=0.1), width = 0.15, stat="identity", colour="black", fill="#00868B") +
  theme_bw() +
  ylab("Total cost per hectare (USD)") +
  xlab("Range of bid offers"))

#Low to high (with small gap between bars) for proportion of land in B

(ww_barplot4 <- ggplot(Zambia1, aes(x=reorder(Respondent, B_Proportion), y=B_Proportion, colour=Ecoregion)) +
  geom_bar(position=position_dodge(width=0.1), width = 0.15, stat="identity", colour="black", fill="#00868B") +
  theme_bw() +
  ylab("Proportion of land enrolled in AMO Borders") +
  xlab("Range of bid offers"))

#################################### Descriptive stats ############################################
###################################################################################################

Zambia1 [ Zambia1 == 0] <-NA #Make all the 0 be NA

#AMOB USD per ha
summary(Zambia1$USD_Ha_AMOB)
summary(Zambia1$B_Proportion)
summary(eco2$USD_Ha_AMOB)

#AMOB USD
summary(Zambia1$B_USD)
logzam <- log(Zambia1$B_USD) #Create the log of AMOB bid offers
summary(logzam)
plot(logzam)


library(pastecs)
stat.desc(Zambia1$USD_Ha_AMOB) 

#################################### Regression Analysis ##########################################
###################################################################################################

#Regression analysis bid offer (AMO B) against Ha enrollment 
plot(B_USD ~ B_Ha, data = Zambia1) #Simple x,y plot of the data 
mod.1 = lm(formula = B_USD ~ B_Ha, data = Zambia1) #Simple linear regression model 
summary(mod.1)

#Regression analysis bid offer (AMO B) against Plots
plot(B_USD ~ B_Plots, data = Zambia1) #Simple x,y plot of the data 
mod.2 = lm(formula = B_USD ~ B_Plots, data = Zambia1) #Simple liear regression model 
summary(mod.2)

#Regression analysis bid offer (AMO B/ha) against proportion of land
plot(USD_Ha_AMOB ~ B_Proportion, data = Zambia1) #Simple x,y plot of the data 
mod.3 = lm(formula = USD_Ha_AMOB ~ B_Proportion, data = Zambia1) #Simple linear regression model 
summary(mod.3)

#Regression analysis bid offer (AMO B) against age
plot(B_USD ~ Age, data = Zambia1) #Simple x,y plot of the data 
mod.4 = lm(formula = B_USD ~ Age, data = Zambia1) #Simple linear regression model 
summary(mod.4)

#Regression analysis proportion land (AMOB) against age
plot(B_Proportion ~ Age, data = Zambia1) #Simple x,y plot of the data 
mod.5 = lm(formula = B_Proportion ~ Age, data = Zambia1) #Simple linear regression model 
summary(mod.5)

#Regression analysis usd/ha (AMOB) against farm size
plot(USD_Ha_AMOB ~ Farm_Size_ha, data = Zambia1) #Simple x,y plot of the data 
mod.6 = lm(formula = USD_Ha_AMOB ~ Farm_Size_ha, data = Zambia1) #Simple linear regression model 
summary(mod.6)

#Regression analysis usd/ha (AMOB) against farm size
plot(B_Proportion ~ Farm_Size_ha, data = Zambia1) #Simple x,y plot of the data 
mod.7 = lm(formula = B_Proportion ~ Farm_Size_ha, data = Zambia1) #Simple linear regression model 
summary(mod.7)

############################ Plots which have been subset ########################################
##################################################################################################

#Subset ecoregion 1 and B (USD/HA)

eco1 <- Zambia1[Zambia1$Ecoregion == "1" ,]
(ww_bar5 <- ggplot(eco1, aes(x=reorder(Respondent, USD_Ha_AMOB), y=USD_Ha_AMOB, colour=Ecoregion)) +
  geom_bar(position=position_dodge(width=0.1), width = 0.15, stat="identity", colour="black", fill="#00868B") +
  ylab("Cost per hectare (AMO B)") +
  xlab("Range of bid offers Ecoregion 1") +
  theme(
    panel.border = element_blank(),
    panel.background = element_blank(),
    panel.grid.minor = element_line(size = 0.1, linetype = 'solid', colour = "black")))

#Subset ecoregion 2 and and B (USD/HA)

eco2 <- Zambia1[Zambia1$Ecoregion == "2" ,]
(ww_bar6 <- ggplot(eco2, aes(x=reorder(Respondent, USD_Ha_AMOB), y=USD_Ha_AMOB, colour=Ecoregion)) +
  geom_bar(position=position_dodge(width=0.1), width = 0.15, stat="identity", colour="black", fill="#00868B") +
  ylab("Cost per hectare (AMO B)") +
  xlab("Range of bid offers Ecoregion 2") +
  theme(
    panel.border = element_blank(),
    panel.background = element_blank(),
    panel.grid.minor = element_line(size = 0.1, linetype = 'solid', colour = "black")))

# Arrange the two plots into a Panel 
(panel <- grid.arrange(ww_bar5, ww_bar6, ncol=2))

## Subset GMA sites (USD/HA)

GMA1 <- Zambia1[Zambia1$GMA == "yes" ,]
(ww_bar7 <- ggplot(GMA1, aes(x=reorder(Respondent, USD_Ha_AMOB), y=USD_Ha_AMOB, colour=Ecoregion)) +
  geom_bar(position=position_dodge(width=0.1), width = 0.15, stat="identity", colour="black", fill="#00868B") +
  ylab("Cost per hectare (AMO B)") +
  xlab("Range of bid offers GMA sites") +
  theme(
    panel.border = element_blank(),
    panel.background = element_blank(),
    panel.grid.minor = element_line(size = 0.1, linetype = 'solid', colour = "black")))


## Subset ecoregion 2 and and B (USD/HA)

GMA2 <- Zambia1[Zambia1$GMA == "no" ,]
(ww_bar8 <- ggplot(GMA2, aes(x=reorder(Respondent, USD_Ha_AMOB), y=USD_Ha_AMOB, colour=Ecoregion)) +
  geom_bar(position=position_dodge(width=0.1), width = 0.15, stat="identity", colour="black", fill="#00868B") +
  ylab("Cost per hectare (AMO B)") +
  xlab("Range of bid offers for non-GMA sites") +
  theme(
    panel.border = element_blank(),
    panel.background = element_blank(),
    panel.grid.minor = element_line(size = 0.1, linetype = 'solid', colour = "black")))

## Arrange the two plots into a Panel 
(panel <- grid.arrange(ww_bar7, ww_bar8, ncol=2))


