###########################################################################################################
############################  Exploring data from model outputs ###########################################
###########################################################################################################

# This code specifies the descriptive statistics used for the indiviudal data for AMO Border. 

# 1.0 Set working directory and load the data ----
setwd ("C:/Users/wwainwright/Documents/R/Zambia_Analysis")
Zambia <- read.csv("C:/Users/wwainwright/Documents/R/Zambia_Analysis/LPOutput/Individual/Data/LPOutput_IndividualNEW2.csv")

# 2.0 Load the packages ----

# Install additional packages
install.packages("psych")
install.packages("corrplot")

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

# 3.0 Aggregate the data for summary stats----

# GMA / non-GMA sites  
aggregate(Zambia[, 10:20], list(Zambia$GMA), mean)  

# Ecoregion 1 / Ecoregion 2
aggregate(Zambia[, 10:20], list(Zambia$ECOREGION), mean)  

# Male / Female
aggregate(Zambia[, 10:20], list(Zambia$MALE), mean)  


# 4.0 Subset data into GMA and non-GMA / Ecoregion 1 and Ecoregion 2 ----

GMA <- Zambia[Zambia$GMA == "1" ,]
nonGMA <- Zambia[Zambia$GMA == "0" ,]

Eco1 <- Zambia[Zambia$ECOREGION == "1" ,]
Eco2 <- Zambia[Zambia$ECOREGION == "2" ,]

# Makes all the 0 values in data sheet be N/A
Zambia[Zambia==0]<-NA

GMA[GMA==0]<-NA
nonGMA[nonGMA==0]<-NA
Eco1[Eco1==0]<-NA
Eco2[Eco2==0]<-NA

# 5.0 Bar Plot farmer bids as cost per hectare for GMA and non-GMA sites----

# Plot all farmer bids from GMA sites  

(wbar1 <- ggplot(GMA, aes(x=reorder(RESPONDENT, USDHA), y=USDHA)) +
   geom_bar(position=position_dodge(width=0.1), width = 0.15, stat="identity", colour="black", fill="#00868B") +
   geom_smooth(method = "loess", se=TRUE, color="blue", aes(group=1)) +
   ylab("Cost per hectare (USD)") +
   xlab("Farmer bid offer GMA sites") +
   theme(
     panel.border = element_blank(),
     panel.background = element_blank(),
     axis.text.x=element_blank(),
     axis.ticks.x=element_blank(),
     panel.grid.minor = element_line(size = 0.1, linetype = 'solid', colour = "black")))

# Plot all farmer bids from nonGMA sites 

(wbar2 <- ggplot(nonGMA, aes(x=reorder(RESPONDENT, USDHA), y=USDHA)) +
    geom_bar(position=position_dodge(width=0.1), width = 0.15, stat="identity", colour="black", fill="#00868B") +
    geom_smooth(method = "loess", se=TRUE, color="blue", aes(group=1)) +
    ylab("Cost per hectare (USD)") +
    xlab("Farmer bid offer non-GMA sites") +
    theme(
      panel.border = element_blank(),
      panel.background = element_blank(),
      axis.text.x=element_blank(),
      axis.ticks.x=element_blank(),
      panel.grid.minor = element_line(size = 0.1, linetype = 'solid', colour = "black")))

## Arrange the two plots into a Panel 

limits <- c(0, 1500)
breaks <- seq(limits[1], limits[2], by=100)

# assign common axis to both plots
wbar1.common.y <- wbar1 + scale_y_continuous(limits=limits, breaks=breaks)
wbar2.common.y <- wbar2 + scale_y_continuous(limits=limits, breaks=breaks)

# build the plots 
wbar1.common.y <- ggplot_gtable(ggplot_build(wbar1.common.y))
wbar2.common.y <- ggplot_gtable(ggplot_build(wbar2.common.y))

# copy the plot height from p1 to p2
wbar1.common.y$heights <- wbar2.common.y$heights

# Display
grid.arrange(wbar1.common.y,wbar2.common.y,ncol=2,widths=c(11,9))

# 6.0 Boxplot of elected farmer bids (US/ha) for the different budget scenarios and three contrasting models ----

# Ordered box plot of selected farmer bids (US/ha) for the different budget scenarios and three contrasting models 

# Order the Objective Functions 
#Turn your 'treatment' column into a character vector
Zambia$Objective <- as.character(Zambia$Objective)
#Then turn it back into an ordered factor
Zambia$Objective <- factor(Zambia$Objective, levels=unique(Zambia$Objective))

# Order the Budget scenarios 
#Turn your 'treatment' column into a character vector
Zambia$Model <- as.character(Zambia$Model)
#Then turn it back into an ordered factor
Zambia$Model <- factor(Zambia$Model, levels=unique(Zambia$Model))

# Order the Ecoregions
#Turn your 'treatment' column into a character vector
Zambia$Ecoregion <- as.character(Zambia$Ecoregion)
#Then turn it back into an ordered factor
Zambia$Ecoregion <- factor(Zambia$Ecoregion, levels=unique(Zambia$Ecoregion))

# PLot the box plot
(box2 <- ggplot (Zambia, aes(Objective, PriceHa)) + geom_boxplot(aes(fill=Ecoregion), notch=FALSE)+ 
    ylab("Total cost per hectare (USD))") +                             
    xlab("Objective Function") +
    guides(fill=guide_legend(title="Ecoregion")) +
    theme(
      axis.text.x=element_text(size=11, angle=90, vjust=1, hjust=1),
      axis.line = element_line(color="black", size = 0.1),
      panel.background = element_blank()))

box2 + theme(legend.position="top", legend.text=element_text(size=12)) 

# 7.0 Creating categorical variables and calculating means of categories ----

### 7.1 Creating factors with different levels from continuous variables, with right-closed=FALSE.  Scale is based on a best-worst scoring (i.e. 5=good and 1=bad).   

# For area
AreaIF <-cut(Zambia$Area, breaks=c(0,0.5,1,1.5,2,2.5,3,100), labels=c("1", "2", "3", "4", "5", "6", "7"), right=FALSE)
AreaIF[1:50]

# For plots
PlotIF <-cut(Zambia$Plots, breaks=c(0,1,2,3,4,5,6,100), labels=c("1", "2", "3", "4", "5", "6", "7"), right=FALSE)
PlotIF[1:50]

# For PriceHa
PriceHaIF <-cut(Zambia$PriceHa, breaks=c(0,25,35,45,55,65,75,Inf), labels=c("7", "6", "5", "4", "3", "2", "1"), right=FALSE)
PriceHaIF[1:50]

# For Age
AgeIF <-cut(Zambia$Age, breaks=c(0,25,30,35,40,45,50,100), labels=c("7", "6", "5", "4", "3", "2", "1"), right=FALSE)
AgeIF[1:50]

# For Farmsize
FarmsizeIF <-cut(Zambia$FarmSize, breaks=c(0,2,3,4,5,6,7,100), labels=c("7", "6", "5", "4", "3", "2", "1"), right=FALSE)
FarmsizeIF[1:50]

# For CWR species richness
RichnessIF <-cut(Zambia$Richness, breaks=c(0,1,2,3,4,5,6,100), labels=c("1", "2", "3", "4", "5","6","7"), right=FALSE)
RichnessIF[1:50]


### 7.2 Convert FACTOR to a NUMERIC factor
AgeIF=as.numeric(AgeIF)
is.numeric(AgeIF)

AreaIF=as.numeric(AreaIF)
is.numeric(AreaIF)

PlotIF=as.numeric(PlotIF)
is.numeric(PlotIF)

PriceHaIF=as.numeric(PriceHaIF)
is.numeric(PriceHaIF)

FarmsizeIF=as.numeric(FarmsizeIF)
is.numeric(FarmsizeIF)

RichnessIF=as.numeric(RichnessIF)
is.numeric(RichnessIF)

### 7.3 Summary stats on the NUMERIC fator
summary(AgeIF)
hist(AgeIF)

summary(AreaIF)
hist(AreaIF)

summary(PlotIF)
hist(PlotIF)

summary(PriceHaIF)  
hist(PriceHaIF)

hist(FarmsizeIF)

summary(RichnessIF)
hist(RichnessIF)

# Calculate the means of the factors subject to each model and corrisponding objective function 
G <- aggregate(cbind(AgeIF, AreaIF, PlotIF, PriceHaIF, FarmsizeIF, RichnessIF)~Zambia$Objective, FUN=mean)
K <- aggregate(cbind(Zambia$YoungFarmer, Zambia$LargePlot, Zambia$Female, Zambia$GMA, Zambia$SmallFarms, Zambia$Ecoregion1)~Zambia$Objective, FUN=sum)

# Tyring to write object "G" and "K" to Excel 
setwd("C:/Users/wwainwright/Documents/R/Zambia_Analysis/LPOutput/Individual/Data/RadarChart/NEW")
write.xlsx(G, file="RadarMean.csv")
write.csv(K, file="RadarSum.csv")

# 8.0 Radar Plots for the four different models ---- 

# Load in the data.  The spreadsheets are different data matrices for the three different models. 
# These are used to produce three different radar charts for the various models.  
BASIC<- read.csv("C:/Users/wwainwright/Documents/R/Zambia_Analysis/LPOutput/Individual/Data/RadarChart/NEW/BASIC.csv")
Area <- read.csv("C:/Users/wwainwright/Documents/R/Zambia_Analysis/LPOutput/Individual/Data/RadarChart/NEW/AREA.csv")
Div <- read.csv("C:/Users/wwainwright/Documents/R/Zambia_Analysis/LPOutput/Individual/Data/RadarChart/NEW/DIVERSITY.csv")
Equ <- read.csv("C:/Users/wwainwright/Documents/R/Zambia_Analysis/LPOutput/Individual/Data/RadarChart/NEW/EQUITABILITY.csv")

# load packages
library(fmsb)
library(ggplot2)
library(gridExtra)
library(dplyr)
library(ggpubr)
library(grid)
library(lattice)

# To arrange all the plots into a planel with 2 rows and 2 column
op <- par(
  oma=c(0,0,0,0), # Room for the title and legend
  mar=c(3,3,3,3),
  mfrow=c(1,2)
)

# Plot 1: Basic model plot
colors_border=c( rgb(0.2,0.5,0.5,0.9), rgb(0.8,0.2,0.5,0.9) , rgb(0.7,0.5,0.1,0.9) )
colors_in=c( rgb(0.2,0.5,0.5,0.4), rgb(0.8,0.2,0.5,0.4) , rgb(0.7,0.5,0.1,0.4) )
radarchart( BASIC  , axistype=1 , 
            #custom polygon
            pcol=colors_border , pfcol=colors_in , plwd=4 , plty=1,
            #custom the grid
            cglcol="black", cglty=1, axislabcol="black", caxislabels=seq(0,100,25), cglwd=1.0, 
            # Title for the chart 
            title="A) Untargeted goal", line = 1,
            #custom labels
            vlcex=1, 
            vlabels=c("Young\nFarmers", "Larger\nPlots",
                      "Female\nFarmers", "GMA\nSites", "CWR\nVerified", "Commu-\nnities")) 

# Plot 2: Area model plot
colors_border=c( rgb(0.2,0.5,0.5,0.9), rgb(0.8,0.2,0.5,0.9) , rgb(0.7,0.5,0.1,0.9) )
colors_in=c( rgb(0.2,0.5,0.5,0.4), rgb(0.8,0.2,0.5,0.4) , rgb(0.7,0.5,0.1,0.4) )
radarchart( Area  , axistype=1 , 
            #custom polygon
            pcol=colors_border , pfcol=colors_in , plwd=4 , plty=1,
            #custom the grid
            cglcol="black", cglty=1, axislabcol="black", caxislabels=seq(0,100,25), cglwd=1.0, 
            # Title for the chart 
            title="B) Targeted goal", line = 1,
            #custom labels
            vlcex=1, 
            vlabels=c("Young\nFarmers", "Larger\nPlots",
                      "Female\nFarmers", "GMA\nSites", "CWR\nVerified", "Commu-\nnities"))

# Plot 3: Diversity model plot 
colors_border=c( rgb(0.2,0.5,0.5,0.9), rgb(0.8,0.2,0.5,0.9) , rgb(0.7,0.5,0.1,0.9) )
colors_in1=c( rgb(0.2,0.5,0.5,0.4), rgb(0.8,0.2,0.5,0.4) , rgb(0.7,0.5,0.1,0.4) )
radarchart( Div  , axistype=1 , 
            #custom polygon
            pcol=colors_border , pfcol=colors_in , plwd=4 , plty=1,
            #custom the grid
            cglcol="black", cglty=1, axislabcol="black", caxislabels=seq(0,100,25), cglwd=0.8,
            # Title for the chart 
            title="C) Diversity goal", line = 1,
            #custom labels
            vlcex=1, 
            vlabels=c("Young\nFarmers", "Larger\nPlots",
                      "Female\nFarmers", "GMA\nSites", "CWR\nVerified", "Commu-\nnities")) 

# Plotting the legend so it appears horizontal accross the bottom of the plots.  
# This works if plotting the three different model scenarios together (Low, Medium and High) on the same graph. 
#legend(x = "bottom", inset = 0, legend =c("Low Budget", "Medium Budget", "High Budget"), ncol=1, xpd=NA, bty="n", pch=20 , col=colors_in , text.col = "black", cex=1.0, pt.cex=3.0, horiz = TRUE)

# Plot 4: Equitability model plot 
colors_border=c( rgb(0.2,0.5,0.5,0.9), rgb(0.8,0.2,0.5,0.9) , rgb(0.7,0.5,0.1,0.9) )
colors_in=c( rgb(0.2,0.5,0.5,0.4), rgb(0.8,0.2,0.5,0.4) , rgb(0.7,0.5,0.1,0.4) )
radarchart( Equ  , axistype=1 , 
            #custom polygon
            pcol=colors_border , pfcol=colors_in , plwd=4 , plty=1,
            #custom the grid
            cglcol="black", cglty=1, axislabcol="black", caxislabels=seq(0,100,25), cglwd=0.8,
            # Title for the chart 
            title="D) Equity goal",line = 1,
            #custom labels
            vlcex=1, 
            vlabels=c("Young\nFarmers", "Larger\nPlots",
                      "Female\nFarmers", "GMA\nSites", "CWR\nVerified", "Commu-\nnities"))

# 9.0 Plotting a panel of multiple charts based on variables and model objectives ----

# Load in the data file
Zambia <- read.csv("C:/Users/wwainwright/Documents/R/Zambia_Analysis/LPOutput/Individual/Data/LPOutput_IndividualNEW2.csv")

# 9.1 Ordered box plot of selected farmer bids (US/ha) for the different budget scenarios and three contrasting models 

# Order the Objective Functions 
#Turn your 'treatment' column into a character vector
Zambia$Objective <- as.character(Zambia$Objective)
#Then turn it back into an ordered factor
Zambia$Objective <- factor(Zambia$Objective, levels=unique(Zambia$Objective))

# Order the Ecoregions scenarios 
#Turn your 'treatment' column into a character vector
Zambia$Ecoregion <- as.character(Zambia$Ecoregion)
#Then turn it back into an ordered factor
Zambia$Ecoregion <- factor(Zambia$Ecoregion, levels=unique(Zambia$Ecoregion))

# PLot the box plot
(box2 <- ggplot (Zambia, aes(Objective, PriceHa)) + geom_boxplot(aes(fill=Ecoregion), notch=FALSE)+ 
    ylab("Total cost per hectare (USD))") +                             
    xlab("Objective Function") +
    ggtitle("A) Cost per hectare from farms selected for conservation services")+ 
    guides(fill=guide_legend(title="Ecoregion"))+
    theme(
      axis.text.x=element_text(size=11, angle=0, vjust=0.5, hjust=0.5),
      axis.line = element_line(color="black", size = 0.1),
      panel.background = element_blank(),
      legend.position = c(.02, .98),
      legend.justification = c("left", "top"),
      legend.box.just = "left",
      legend.margin = margin(6, 6, 6, 6)))

# 9.2 Plot a line chart based on average size of farm

# Creating an object called x, based on x variables, and then plotting in a model, ordered by Farm Size

x = Zambia %>%
  select(ID, Objective, FarmSize)
#x$Model = factor(x$Model, levels=c("Low", "Medium", "High"))

# Plotting the point graph based on X
p <- x %>% 
  #filter(Model=="Medium") %>% 
  mutate(ID=reorder(ID, FarmSize)) %>% 
  ggplot(aes(ID, FarmSize, colour=Objective, group=1)) +
  geom_point() +
  labs(x="Farmer", y="Farm size (ha)")+ 
  ggtitle("B) Size of farms selected for conservation services")+ 
  scale_y_log10() + # This is to plot using the log of the data
  facet_wrap(~Objective) +
  #scale_y_continuous(breaks=seq(10,150,25))+
  theme(#axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(), 
        axis.line = element_line(colour = "black"),
        panel.background = element_blank(),
        legend.position="none")
        #legend.position="bottom", legend.direction="horizontal",
        #legend.title = element_blank())
p

# 9.3 Plotting the age of farmers and price per ha (bar plot) 

v = Zambia %>%
  select(ID, Objective, FarmSize, PriceHa, Age, Bidoffer, Averagesizeplot, PricePlot, Area, Plots)
#v$Model = factor(v$Model, levels=c("Low", "Medium", "High"))

# Plotting the point graph based on v
  #filter(v$Model=="Medium") %>% 
  mutate(v$Age=reorder(v$Age, v$PriceHa)) %>% 

  u <- ggplot(v, aes(Age, PriceHa), y=PriceHa) +
    geom_bar(position=position_dodge(width=0.1), width = 0.15, stat="identity", colour=Zambia$Objective, fill="#00868B") +
    geom_smooth(method = "glm", se=FALSE, color="blue", aes(group=1)) +
    ggtitle("C) Cost per hectare and age of farmers selected for conservation services")+ 
    ylab("Price per hectare (USD)") +
    xlab("Age") +
    facet_wrap(~Objective) +
  theme(
      #panel.border = element_blank(),
      panel.background = element_blank(),
      #axis.text.x=element_blank(),
      #axis.ticks.x=element_blank(),
      axis.line = element_line(colour = "black"),
      #panel.grid.minor = element_line(size = 0.1, linetype = 'solid', colour = "black"),
      legend.position="bottom", legend.direction="horizontal",
      legend.title = element_blank())
u

# 9.4 Plot a line chart based on total area (Y) and total plots (x)

# Creating an object called k, based on k variables, and then plotting in a model, ordered by Farm Size

k = Zambia %>%
  select(ID, Objective, FarmSize, Area, Plots)
#k$Model = factor(k$Model, levels=c("Low", "Medium", "High"))

# Plotting the point graph based on X
g <- k %>% 
  #filter(Model=="Medium") %>% 
  mutate(ID=reorder(Area, Plots)) %>% 
  ggplot(aes(Plots, Area, colour=Objective, group=1)) +
  geom_point() +
  geom_smooth(method = "glm", se=FALSE, color="blue", aes(group=1)) +
  #theme_bw() +
  labs(x="Area (Ha)", y="Plots")+ 
  ggtitle("D) Area and plots selected for conservation services")+ 
  facet_wrap(~Objective) +
  #scale_y_continuous(breaks=seq(10,150,25))+
  theme(panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.position="none")
g

# Arrange the plots (box2, p, u and g) into a grid 
blank<-rectGrob(gp=gpar(col="white"))   # Create a blank plot to use as a space
grid.arrange(box2, p, blank, blank, u, g, heights=c(0.475, 0.05, 0.475), nrow=3) # plot with the blank space


# 10.0 Line Plot of CWR inhabiting selected sites ---- 

# Read in the data 
Zam <- read.csv("C:/Users/wwainwright/Documents/R/Zambia_Analysis/LPOutput/Individual/Data/CWR/CPT2.csv")

# Inspecting the data frame 
dim(Zam)
lapply(Zam, class)
str(Zam)

# Check the variables are factors with different levels 
Zam$Model

# Specify that the model objective is either High, Low or Medium 
#Zam$Objective = factor(Zam$Objective, levels=c("H", "L", "M"))
#ZamM <- Zam[Zam$Objective == "M" ,] # Filter for just the Medium scenario 
  
# Plot a line chart with different colours for each Model 
P <- ggplot(Zam, aes(x=CWR, y=Count, group=Model)) +
      #facet_wrap(~Zam$Objective)+
      geom_line(linetype="dashed", size=1.3, aes(color=Model))+
      geom_point(aes(color=Model))+
      ylab("Number of sites selected inhabited by CWR species\n") +
      xlab("\nCWR Species") +
      theme(
      axis.text.x=element_text(face= c('italic', 'bold.italic', 'italic', 'italic',
                                       'bold.italic', 'italic', 'italic', 'italic', 
                                       'bold.italic', 'bold.italic'), # Makes some of the labels italic, others bold and italic
                              size=12, angle=50, vjust=0.5, hjust=0.5),
      axis.line = element_line(color="black", size = 0.1),
      axis.title=element_text(size=12),
     panel.border = element_blank(),
     panel.background = element_blank())
P

# Puts the legend on the top and increases the size
P + theme(legend.position="top", legend.text=element_text(size=12)) +
  # Takes the plot and changes the axis lables from original to modification
    scale_x_discrete(labels=c("EchinochloaC" = "(1) Echinochloa.C", "EleusineC" = "(2) Eleusine.C"
                              , "EleusineI" = "(3) Eleusine.I", "OryzaL" = "(4) Oryza.L", "PennisetumP" = "(5) Pennisetum.P"
                              , "PennisetumP2" = "(6) Pennisetum.P(2)", "SolanumI" = "(7) Solanum.I", 
                              "SorghumB" = "(8) Sorghum.B", "VignaJ" = "(9) Vigna.J", "VignaU" = "(10) Vigna.U"))


# 11.0 Significance tests of model outputs

# Load the data 
ZamP <- read.csv("C:/Users/wwainwright/Documents/R/Zambia_Analysis/LPOutput/Individual/Data/LPOutput_IndividualNEW3.csv")
names(ZamP)
