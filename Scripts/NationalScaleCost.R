######################################################################################################
#################################  National Scale CWR Costing ###################################################
######################################################################################################

# Set working directory and load the data ----
setwd ("C:/Users/wwainwright/Documents/R/Zambia_Analysis")
NC <- read.csv("C:/Users/wwainwright/Documents/R/Zambia_Analysis/LPOutput/Individual/Data/CWR/NationalCWRCosting/NationalCosting.csv")
show(NC)

# Libraries
library(tidyr)
library(dplyr)
library(ggplot2)
library(readr)
library(gridExtra)
library(directlabels)
library(ggrepel)
library(scales)

# Plotting a bar plot based on CWR national cost estimate
(wbar1 <- ggplot(NC, aes(x=Region, y=Cost, fill = Estimate)) +
    geom_bar(stat = "identity", width=0.5, position=position_dodge()) +
    ylab("Total Cost (USD)") +
    xlab("Ecoregion") +
    theme(
      panel.background = element_blank(),
      legend.position="top", # Puts legend at the top
      panel.border = element_rect(colour = "black", fill=NA, size=1)))
      #panel.grid.minor.y = element_line(size = 0.1, linetype = 'solid', colour = "black")))

# Make the chart grey style
wbar1+scale_fill_grey()
