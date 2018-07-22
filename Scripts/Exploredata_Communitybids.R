###########################################################################################################
############################  Exploring data for community bid offers ####################################
###########################################################################################################

# This code specifies the descriptive statistics used for the community data for AMO Common. Note the sample size for this 
# data is much smaller and so statistical analysis offers less power.  

# 1.0 Set working directory and load the data ----
setwd ("C:/Users/wwainwright/Documents/R/Zambia_Analysis")
Zambia <- read.csv("C:/Users/wwainwright/Documents/R/Zambia_Analysis/Data/Community_Final.csv")
View(Zambia)

# Makes all the 0 values in data sheet be N/A
Zambia[Zambia==0]<-NA

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

# 3.0 Explore the data ----
names(Zambia)
show(Zambia)

# 4.0 summary of all data ----

# Summaries of data 
summary(Zambia$ECOREGION)
summary(Zambia$GMA)
summary(Zambia$HA)
summary(Zambia$USD)
summary(Zambia$USDHA)
summary(Zambia$USDPLOT)

# Simple plots of the data 
barplot(Zambia$HA)
barplot(Zambia$USD)


# 5.0 Aggregate the data for summary stats----

# GMA / non-GMA sites  
aggregate(Zambia[, 3:34], list(Zambia$GMA), mean)  

# Ecoregion 1 / Ecoregion 2
aggregate(Zambia[, 3:34], list(Zambia$ECOREGION1), mean)  



# 6.0 Subset data into GMA and non-GMA / Ecoregion 1 and Ecoregion 2 ----

GMA <- Zambia[Zambia$GMA == "1" ,]
nonGMA <- Zambia[Zambia$GMA == "0" ,]

Eco1 <- Zambia[Zambia$ECOREGION == "1" ,]
Eco2 <- Zambia[Zambia$ECOREGION == "0" ,]

# 7.0 t-test (Ecoregion and GMA differences) ----

# independent 2-sample t-test for i) ecoregion and ii) GMA differences
t.test(Zambia$ECOREGION1,Zambia$ELEVATION)  
t.test(Zambia$ECOREGION1,Zambia$CWRRICHNESS)  
t.test(Zambia$ECOREGION1,Zambia$DISTANCEHOTSPOT)
t.test(Zambia$ECOREGION1,Zambia$vigna_unguiculata)
t.test(Zambia$ECOREGION1,Zambia$vigna_juncea)
t.test(Zambia$ECOREGION1,Zambia$eleusine_coracana_subsp.africana)  
t.test(Zambia$ECOREGION1,Zambia$HA) 
t.test(Zambia$ECOREGION1,Zambia$FARMERS) 
t.test(Zambia$ECOREGION1,Zambia$DISTANCECOMMUNITY) 
t.test(Zambia$ECOREGION1,Zambia$USD) 
t.test(Zambia$ECOREGION1,Zambia$USDHA) 

t.test(Zambia$GMA,Zambia$ELEVATION)  
t.test(Zambia$GMA,Zambia$CWRRICHNESS)  
t.test(Zambia$GMA,Zambia$DISTANCEHOTSPOT)
t.test(Zambia$GMA,Zambia$vigna_unguiculata)
t.test(Zambia$GMA,Zambia$vigna_juncea)
t.test(Zambia$GMA,Zambia$eleusine_coracana_subsp.africana)  
t.test(Zambia$GMA,Zambia$HA) 
t.test(Zambia$GMA,Zambia$FARMERS) 
t.test(Zambia$GMA,Zambia$DISTANCECOMMUNITY) 
t.test(Zambia$GMA,Zambia$USD) 
t.test(Zambia$GMA,Zambia$USDHA) 



# 8.0 Standard deviations of parameters ----

# Ecoregion 1
sd(Eco1$ELEVATION)
sd(Eco1$CWRRICHNESS)
sd(Eco1$vigna_unguiculata)
sd(Eco1$vigna_juncea)
sd(Eco1$eleusine_coracana_subsp.africana)
sd(Eco1$DISTANCEHOTSPOT)
sd(Eco1$HA)
sd(Eco1$FARMERS)
sd(Eco1$DISTANCECOMMUNITY)
sd(Eco1$USD)
sd(Eco1$USDHA)

# Ecoregion 2
sd(Eco2$ELEVATION)
sd(Eco2$CWRRICHNESS)
sd(Eco2$vigna_unguiculata)
sd(Eco2$vigna_juncea)
sd(Eco2$eleusine_coracana_subsp.africana)
sd(Eco2$DISTANCEHOTSPOT)
sd(Eco2$HA)
sd(Eco2$FARMERS)
sd(Eco2$DISTANCECOMMUNITY)
sd(Eco2$USD)
sd(Eco2$USDHA)

# GMA
sd(GMA$ELEVATION)
sd(GMA$CWRRICHNESS)
sd(GMA$vigna_unguiculata)
sd(GMA$vigna_juncea)
sd(GMA$eleusine_coracana_subsp.africana)
sd(GMA$DISTANCEHOTSPOT)
sd(GMA$HA)
sd(GMA$FARMERS)
sd(GMA$DISTANCECOMMUNITY)
sd(GMA$USD)
sd(GMA$USDHA)

# non-GMA
sd(nonGMA$ELEVATION)
sd(nonGMA$CWRRICHNESS)
sd(nonGMA$vigna_unguiculata)
sd(nonGMA$vigna_juncea)
sd(nonGMA$eleusine_coracana_subsp.africana)
sd(nonGMA$DISTANCEHOTSPOT)
sd(nonGMA$HA)
sd(nonGMA$FARMERS)
sd(nonGMA$DISTANCECOMMUNITY)
sd(nonGMA$USD)
sd(nonGMA$USDHA)

# 9.0 Bar Plot community bids as cost per hectare for GMA and non-GMA sites----

# Plot all farmer bids from GMA sites (with confidence intervals)  

(wbar1 <- ggplot(GMA, aes(x=reorder(COMMUNITY, USDHA), y=USDHA)) +
   geom_bar(position=position_dodge(width=0.1), width = 0.15, stat="identity", colour="black", fill="#00868B") +
   geom_smooth(method = "loess", se=TRUE, color="blue", aes(group=1)) +
   ylab("Cost per hectare (USD)") +
   xlab("Community bid offer GMA sites") +
   theme(
     panel.border = element_blank(),
     panel.background = element_blank(),
     axis.text.x=element_text(size=12, angle=45, vjust=1, hjust=1),       # making the years at a bit of an angle
     axis.text.y=element_text(size=12),
     axis.title.x=element_text(size=14, face="plain"),             
     axis.title.y=element_text(size=14, face="plain"),             
     #panel.grid.major.x=element_blank(),                                  # Removing the background grid lines                
     #panel.grid.minor.x=element_blank(),
     #panel.grid.minor.y=element_blank(),
     #panel.grid.major.y=element_blank(),  
     plot.margin = unit(c(1,1,1,1), units = , "cm"),                    # Adding a 1cm margin around the plot
     #axis.text.x=element_blank(),
     #axis.ticks.x=element_blank(),
     panel.grid.minor = element_line(size = 0.1, linetype = 'solid', colour = "black"))) 

# Plot all farmer bids from nonGMA sites (with confidence intervals)  

(wbar2 <- ggplot(nonGMA, aes(x=reorder(COMMUNITY, USDHA), y=USDHA)) +
    geom_bar(position=position_dodge(width=0.1), width = 0.15, stat="identity", colour="black", fill="#00868B") +
    geom_smooth(method = "loess", se=TRUE, color="blue", aes(group=1)) +
    ylab("Cost per hectare (USD)") +
    xlab("Community bid offer non-GMA sites") +
    theme(
      panel.border = element_blank(),
      panel.background = element_blank(),
      axis.text.x=element_text(size=12, angle=45, vjust=1, hjust=1),       # making the years at a bit of an angle
      axis.text.y=element_text(size=12),
      axis.title.x=element_text(size=14, face="plain"),             
      axis.title.y=element_text(size=14, face="plain"),             
      #panel.grid.major.x=element_blank(),                                  # Removing the background grid lines                
      #panel.grid.minor.x=element_blank(),
      #panel.grid.minor.y=element_blank(),
      #panel.grid.major.y=element_blank(),  
      plot.margin = unit(c(1,1,1,1), units = , "cm"),                    # Adding a 1cm margin around the plot
      #axis.text.x=element_blank(),
      #axis.ticks.x=element_blank(),
      panel.grid.minor = element_line(size = 0.1, linetype = 'solid', colour = "black")))

## Arrange the two plots into a Panel 

limits <- c(0, 1700)
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
grid.arrange(wbar1.common.y,wbar2.common.y,ncol=2,widths=c(10,10))

# 10.0 Bar Plot community bids as cost per hectare for Ecoregion1 and Ecoregion2 ----

(wbar3 <- ggplot(Eco1, aes(x=reorder(COMMUNITY, USDHA), y=USDHA)) +
   geom_bar(position=position_dodge(width=0.1), width = 0.15, stat="identity", colour="black", fill="#00868B") +
   geom_smooth(method = "loess", se=TRUE, color="blue", aes(group=1)) +
   ylab("Cost per hectare (USD)") +
   xlab("Community bid offer Ecoregion 1") + 
   theme(
     panel.border = element_blank(),
     panel.background = element_blank(),
     axis.text.x=element_text(size=12, angle=45, vjust=1, hjust=0.8),       # making the years at a bit of an angle
     axis.text.y=element_text(size=12),
     axis.title.x=element_text(size=14, face="plain"),             
     axis.title.y=element_text(size=14, face="plain"),             
     #panel.grid.major.x=element_blank(),                                  # Removing the background grid lines                
     #panel.grid.minor.x=element_blank(),
     #panel.grid.minor.y=element_blank(),
     #panel.grid.major.y=element_blank(),  
     plot.margin = unit(c(1,1,1,1), units = , "cm"),                    # Adding a 1cm margin around the plot
     #axis.text.x=element_blank(),
     #axis.ticks.x=element_blank(),
     panel.grid.minor = element_line(size = 0.1, linetype = 'solid', colour = "black"))) 

# Plot all farmer bids from nonGMA sites (with confidence intervals)  

(wbar4 <- ggplot(Eco2, aes(x=reorder(COMMUNITY, USDHA), y=USDHA)) +
    geom_bar(position=position_dodge(width=0.1), width = 0.15, stat="identity", colour="black", fill="#00868B") +
    geom_smooth(method = "loess", se=TRUE, color="blue", aes(group=1)) +
    ylab("Cost per hectare (USD)") +
    xlab("Community bid offer Ecoregion 2") +
    theme(
      panel.border = element_blank(),
      panel.background = element_blank(),
      axis.text.x=element_text(size=12, angle=45, vjust=1, hjust=0.8),       # making the years at a bit of an angle
      axis.text.y=element_text(size=12),
      axis.title.x=element_text(size=14, face="plain"),             
      axis.title.y=element_text(size=14, face="plain"),             
      #panel.grid.major.x=element_blank(),                                  # Removing the background grid lines                
      #panel.grid.minor.x=element_blank(),
      #panel.grid.minor.y=element_blank(),
      #panel.grid.major.y=element_blank(),  
      plot.margin = unit(c(1,1,1,1), units = , "cm"),                    # Adding a 1cm margin around the plot
      #axis.text.x=element_blank(),
      #axis.ticks.x=element_blank(),
      panel.grid.minor = element_line(size = 0.1, linetype = 'solid', colour = "black")))

## Arrange the two plots into a Panel 

limits <- c(0, 1700)
breaks <- seq(limits[1], limits[2], by=100)

# assign common axis to both plots
wbar3.common.y <- wbar3 + scale_y_continuous(limits=limits, breaks=breaks)
wbar4.common.y <- wbar4 + scale_y_continuous(limits=limits, breaks=breaks)

# build the plots 
wbar3.common.y <- ggplot_gtable(ggplot_build(wbar3.common.y))
wbar4.common.y <- ggplot_gtable(ggplot_build(wbar4.common.y))

# copy the plot height from p1 to p2
wbar3.common.y$heights <- wbar4.common.y$heights

# Display
grid.arrange(wbar3.common.y,wbar4.common.y,ncol=2,widths=c(10,10))

# 10.0 Correlation matrix analysis ----

# Create data frame of variables with selected columns using column indices
Zamcor <- Zambia[,c(3,7,12,13,14,15,16,17,33)]
Zamcor1 <- Zambia[,c(3,7,8,9,10,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,33)] # With extra variables included 

# Group correlation test 
corr.test(Zamcor[1:9])
corr.test(Zamcor1[1:26])

# Visulisations
pairs.panels(Zamcor[1:9])  

# Simple visualisation of correlation analysis effect size  including significance
x <- cor(Zamcor[1:9])
colnames (x) <- c("Farmers", "Ecoregion", "CWR Richness", "Elevation", "Distance to hotspot", "Area", "Distance from community", "GMA", "Price")
rownames(x) <- c("Farmers", "Ecoregion", "CWR Richness", "Elevation", "Distance to hotspot", "Area", "Distance from community", "GMA", "Price")
p.mat <- cor.mtest(Zamcor, conf.level = .95)
p.mat <- cor.mtest(Zamcor)$p
corrplot(x, p.mat = res1$p, sig.level = .05)
corrplot(x, type="upper", order="hclust", addrect = 2, p.mat = p.mat, sig.level = 0.05, insig = "blank")

# 11.0 Regression analysis ----

# Overarching regression model using bid offer price for all data (Note small sample size)
mod.1 = lm(formula = USD ~ HA, data = Zambia) 
summary(mod.1)
