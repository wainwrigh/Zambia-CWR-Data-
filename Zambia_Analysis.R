#############################################################################################################
######################################### Zambia Group / Individual Data ###################################
############################################################################################################

Zambia <- read.csv ("C:/Users/wwainwright/Desktop/KW_H_Province.csv")

# Libraries
library(tidyr)
library(dplyr)
install.packages("ggplot2")
library(ggplot2)
library(readr)
library(gridExtra)

###########################################################################################################
###############################  Graph Plotting ###########################################################
###########################################################################################################

#Read in the data file 
Zambia <- read.csv ("C:/Users/wwainwright/Documents/R/Zambia_Analysis/Data/Group_Bids1.csv")

###########Scatterplot 

# Using default ggplot2 graphics for farm size Vs kwacha per ha for AMO B (try different independant variables here)
(ww_scatter <- ggplot(Zambia, aes(x=Community, y=Cost_ha, colour=AMO)) +
  geom_point() + geom_smooth(aes(colour=AMO))) +
  ylab("Total cost per hectare (Kwacha)") +                             
  xlab("Farm Size")  
  Legend=("Richness")
    
###############Box plot########################################################################################

#Read in the data file 
Zambia <- read.csv ("C:/Users/wwainwright/Documents/R/Zambia_Analysis/Data/Individual_Bids1.csv")

#Box plot of individual community bids (US/ha) for Province
  (ww_boxplot <- ggplot (Zambia, aes(Community, USH_AMOA)) + geom_boxplot(aes(fill=Province))+ 
   geom_point()+
   ylab("Total cost per hectare (USD))") +                             
   xlab("\nCommunity") +
   guides(fill=guide_legend(title="Province")) + 
   theme(axis.text.x=element_text(size=11, angle=90, vjust=1, hjust=1)))
  
#Box plot of individual community bids (US/ha) for Richness Index 
  (ww_boxplot1 <- ggplot (Zambia, aes(Community, USH_AMOA)) + geom_boxplot(aes(fill=Richness_Index))+ 
    geom_point() +
    ylab("Total cost per hectare (USD))") +                             
    xlab("\nCommunity") +
    guides(fill=guide_legend(title="Richness Index")) + 
    theme(axis.text.x=element_text(size=11, angle=90, vjust=1, hjust=1)))
        
    
  ##### Arrange the two plots into a Panel 
  
  panel <- grid.arrange(ww_boxplot, ww_boxplot1, ncol=2)
  ggsave(panel, file="Pop_trend_panel.png", width=10, height=8)

###############################################################################################################
######## Histogram ############################################################################################
  
  # With ggplot2
  (ww_hist <- ggplot(Zambia, aes(x=Total_KW))  +
     geom_histogram()) # putting your entire ggplot code in () creates the graph and shows it in the plot viewer

  ##############################################################################################################
  ################## Line plot #################################################################################
  
 
  (ww_scatter <- ggplot(Zambia, aes(x=Number, y=Bid_2, colour=AMO_2)) +
     geom_line() + geom_point(aes(colour=AMO))) +
    ylab("Total cost per hectare (Kwacha)") +                             
    xlab("Farm Size")  
  
  
  #############################################################################################################
  ######################################### LP Model Analysis ################################################
  ############################################################################################################

  
  Zambia <- read.csv ("C:/Users/wwainwright/Desktop/LP_Results.csv")
  
  # Libraries
  library(tidyr)
  library(dplyr)
  library(ggplot2)
  library(readr)
  library(gridExtra)
  install.packages("scales")
  library(Scales)
  
  ###########################################################################################################
  ###############################  Graph Plotting ###########################################################
  ###########################################################################################################
  
  #Line Plot
  (ww_scatter <- ggplot(Zambia, aes(x=ID, y=PriceHa_USD, colour=AMO)) +
     geom_line() + geom_point(aes(colour=AMO))) +
    ylab("Total cost per hectare (Kwacha)") +                             
    xlab("Farm Size")  
  
  # Barplot
  
(ww_barplot <- ggplot(Zambia, aes(x=AMO, y=PriceHa_USD, colour=GMA)) +
    geom_bar() + geom_bar(aes(colour=GMA))) +
    ylab("Total cost per hectare (Kwacha)") +                             
    xlab("Farm Size") 
  
(ww_barplot <- ggplot(Zambia, aes(x=AMO, y=PriceHa_USD, colour=GMA)) +
    geom_bar(position=position_dodge(), stat="identity", colour="black", fill="#00868B") +
    theme_bw() +
    ylab("Species richness\n") +                             
    xlab("Country")  

# Bar plot (faceted) for the different AMO's

#Turn your 'Scinario' column into a character vector
Zambia$Scinario <- as.character(Zambia$Scinario)
#Then turn 'Scinario' it back into an ordered factor
Zambia$Scinario <- factor(Zambia$Scinario, levels=unique(Zambia$Scinario))

  ggplot(Zambia, aes(x=Scinario, y=PriceHa_USD, fill=GMA)) + geom_bar(stat="identity") +
    facet_wrap(~AMO) +
    ylab("Total cost per hectare (USD)") +                             
    xlab("Scenario")  
  