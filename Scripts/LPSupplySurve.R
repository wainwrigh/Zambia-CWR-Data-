######################################################################################################
#################################  LP Supply Curve ###################################################
######################################################################################################

# Set working directory and load the data ----
setwd ("C:/Users/wwainwright/Documents/R/Zambia_Analysis")
LP <- read.csv("C:/Users/wwainwright/Documents/R/Zambia_Analysis/LPOutput/Individual/Data/LPSupplyCurve/LPSupply2.csv")

# Libraries
library(tidyr)
library(dplyr)
install.packages('ggplot2')
library(ggplot2)
library(readr)
library(gridExtra)
library(directlabels)
library(ggrepel)
library(scales)

# Plot a step line chart for the per unit cost of procuring conservation services from farmers, for each model.

LPP <- ggplot(LP, aes(x=AreaHa, y=Cost, group=MODEL)) +
  geom_step(size=1.3)+ 
  geom_point()+ #(aes(colour=GMA))+
  geom_dl(aes(label=MODEL),method="last.points")+
  ylab("Landholder bids (USD)\n") +
  xlab("\nArea (Ha)") +
  theme(
    axis.text.x=element_text (size=14, angle=50, vjust=0.5, hjust=0.5),
    axis.text.y=element_text (size=14, angle=0, vjust=0.5, hjust=0.5),
      axis.line = element_line(color="black", size = 0.1),
      axis.title=element_text(size=14),
      panel.border = element_rect(fill=NA, colour = "black"),
      panel.background = element_blank())
LPP

######################## Formulating an LP supply curve for each CWR #############################

# Load the data
LP2 <- read.csv("C:/Users/wwainwright/Documents/R/Zambia_Analysis/LPOutput/Individual/Data/CWR/CostPerCWR/CWR_LPSupply.csv")

# Subset by the diversity model
# Subset by "objective" and then the "model" 
LPP <- LP2[LP2$Model == "DIVERSITY" ,] 

# Plot a step line chart for the per unit cost of conserving each CWR, in the diversity model.

LP22 <- ggplot(LPP, aes(x=Ha, y=Cost, group=CWR)) +
  geom_step(size=1.3)+ 
  geom_point()+ #(aes(colour=GMA))+
  geom_dl(aes(label=CWR),method=list("last.bumpup",cex=0.9, hjust = -.3))+
  ylab("Landholder bids (USD)\n") +
  xlab("\nArea (Ha)") +
theme(
  axis.text.x=element_text (size=14, angle=50, vjust=0.5, hjust=0.5),
  axis.text.y=element_text (size=14, angle=0, vjust=0.5, hjust=0.5),
  axis.line = element_line(color="black", size = 0.1),
  axis.title=element_text(size=12),
  panel.border = element_rect(fill=NA, colour = "black"),
  panel.background = element_blank())
LP22

