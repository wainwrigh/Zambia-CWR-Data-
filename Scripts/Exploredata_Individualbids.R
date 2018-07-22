###########################################################################################################
############################  Exploring data for indiviudal bid offers ####################################
###########################################################################################################

# This code specifies the descriptive statistics used for the indiviudal data for AMO Border. 

# 1.0 Set working directory and load the data ----
setwd ("C:/Users/wwainwright/Documents/R/Zambia_Analysis")
Zambia <- read.csv("C:/Users/wwainwright/Documents/R/Zambia_Analysis/AllData/INDIVIDUALAll.csv")

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

# 3.0 Explore the data ----
names(Zambia)
show(Zambia)

# 4.0 summary of all data ----

summary(Zambia$AGE)
summary(Zambia$COMMUNITY)
summary(Zambia$PROVINCE)
summary(Zambia$ECOREGION)
summary(Zambia$GMA)
summary(Zambia$FARMSIZE)
summary(Zambia$MALE)
summary(Zambia$FEMALE)
summary(Zambia$PLOTS)
summary(Zambia$HA)
summary(Zambia$PROPORTION)
summary(Zambia$USD)
summary(Zambia$USDHA)
summary(Zambia$USDPLOT)

# 5.0 Aggregate the data for summary stats----

# GMA / non-GMA sites  
aggregate(Zambia[, 10:20], list(Zambia$GMA), mean)  

# Ecoregion 1 / Ecoregion 2
aggregate(Zambia[, 10:20], list(Zambia$ECOREGION), mean)  

# Male / Female
aggregate(Zambia[, 10:20], list(Zambia$MALE), mean)  


# 6.0 Subset data into GMA and non-GMA / Ecoregion 1 and Ecoregion 2 ----

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

# Summary of data 
summary(GMA)

# 7.0 t-test (Ecoregion and GMA differences) ----

# Using a Fisher's F-test to verify the homoskedasticity (homogeneity of variances).

var.test(GMA$FarmSize, nonGMA$FarmSize) # Sig diff
var.test(GMA$Age, nonGMA$Age) # Not sig
var.test(GMA$Plots, nonGMA$Plots) # Not sig
var.test(GMA$Area, nonGMA$Area) # Sig dif
var.test(GMA$Proportion, nonGMA$Proportion) # Not sig
var.test(GMA$Bidoffer, nonGMA$Bidoffer) # Sig dif
var.test(GMA$PriceHa, nonGMA$PriceHa) # Sig dif
var.test(GMA$PricePlot, nonGMA$PricePlot) # Sig dif
var.test(GMA$RichnessIndex, nonGMA$RichnessIndex) #Sig dif
var.test(GMA$AveSizePlot, nonGMA$Averagesizeplot) #Sig dif

# independent 2-sample t-test for GMA differences
t.test(GMA$FarmSize, nonGMA$FarmSize, var.equal=FALSE, paired=FALSE)
t.test(GMA$Age, nonGMA$Age, var.equal=TRUE, paired=FALSE)
t.test(GMA$Plots, nonGMA$Plots, var.equal=TRUE, paired=FALSE)
t.test(GMA$Area, nonGMA$Area, var.equal=FALSE, paired=FALSE)
t.test(GMA$Proportion, nonGMA$Proportion, var.equal=TRUE, paired=FALSE)
t.test(GMA$Bidoffer, nonGMA$Bidoffer, var.equal=FALSE, paired=FALSE)
t.test(GMA$PriceHa, nonGMA$PriceHa, var.equal=FALSE, paired=FALSE)
t.test(GMA$PricePlot, nonGMA$PricePlot, var.equal=FALSE, paired=FALSE)
t.test(GMA$RichnessIndex, nonGMA$RichnessIndex, var.equal=FALSE, paired=FALSE)
t.test(GMA$Averagesizeplot, nonGMA$Averagesizeplot, var.equal=FALSE, paired=FALSE)

# 8.0 Standard deviations of parameters ----

# GMA
sd(GMA$FarmSize)
sd(GMA$Age, na.rm=TRUE)
sd(GMA$Plots, na.rm=TRUE)
sd(GMA$Area)
sd(GMA$Proportion, na.rm=TRUE)
sd(GMA$Bidoffer)
sd(GMA$PriceHa)
sd(GMA$PricePlot, na.rm=TRUE)
sd(GMA$RichnessIndex)
sd(GMA$AveSizePlot, na.rm=TRUE)

# non-GMA
sd(nonGMA$FarmSize)
sd(nonGMA$Age, na.rm=TRUE)
sd(nonGMA$Plots, na.rm=TRUE)
sd(nonGMA$Area)
sd(nonGMA$Proportion, na.rm=TRUE)
sd(nonGMA$Bidoffer)
sd(nonGMA$PriceHa)
sd(nonGMA$PricePlot, na.rm=TRUE)
sd(nonGMA$RichnessIndex)
sd(nonGMA$AveSizePlot, na.rm=TRUE)

# 9.0 Bar Plot farmer bids as cost per hectare for GMA and non-GMA sites----

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

# 10.0 Supply curve of farmer bids as cost per hectare for Ecoregion1 and Ecoregion2 ----

# Creating an object called x, based on x variables, and then plotting in a model, ordered by Price per Ha

x = Zambia %>%
  select(COMMUNITYID, USDHA, Gender, RESPONDENT, GMASite, EcoregionSite)
p <- x %>% 
  mutate(RESPONDENT=reorder(RESPONDENT, USDHA)) %>% 
  ggplot(aes(RESPONDENT, USDHA,colour=GMASite, group=1)) +
  geom_point() +
  labs(x="Farmer bidding in tender", y="Price per hectare (USD)")+ 
  #ggtitle("B) Size of farms selected for conservation services")+ 
  facet_wrap(~EcoregionSite) +
  theme(#axis.title.x=element_blank(),
    axis.text.x=element_blank(),
    axis.ticks.x=element_blank(), 
    axis.line = element_line(colour = "black"),
    panel.background = element_blank(),
    legend.position = c(.02, .98),
    legend.justification = c("left", "top"),
    legend.box.just = "left",
    legend.margin = margin(6, 6, 6, 6) ,legend.direction="horizontal",
    legend.title = element_blank())
p


# 11.0 Bar Plot farmer bids as cost per hectare for Ecoregion1 and Ecoregion2 ----

(wbar3 <- ggplot(Eco1, aes(x=reorder(RESPONDENT, USDHA), y=USDHA)) +
    geom_bar(position=position_dodge(width=0.1), width = 0.15, stat="identity", colour="black", fill="#00868B") +
    geom_smooth(method = "loess", se=TRUE, color="blue", aes(group=1)) +
    ylab("Cost per hectare (USD)") +
    xlab("Farmer bid offer Ecoregion 1") +
    theme(
      panel.border = element_blank(),
      panel.background = element_blank(),
      axis.text.x=element_blank(),
      axis.ticks.x=element_blank(),
      panel.grid.minor = element_line(size = 0.1, linetype = 'solid', colour = "black")))

# Plot all farmer bids for Ecoregion 2 

(wbar4 <- ggplot(Eco2, aes(x=reorder(RESPONDENT, USDHA), y=USDHA)) +
    geom_bar(position=position_dodge(width=0.1), width = 0.15, stat="identity", colour="black", fill="#00868B") +
    geom_smooth(method = "loess", se=TRUE, color="blue", aes(group=1)) +
    ylab("Cost per hectare (USD)") +
    xlab("Farmer bid offer Ecoregion 2") +
    theme(
      panel.border = element_blank(),
      panel.background = element_blank(),
      axis.text.x=element_blank(),
      axis.ticks.x=element_blank(),
      panel.grid.minor = element_line(size = 0.1, linetype = 'solid', colour = "black")))

# Arrange the two plots into a Panel 
limits <- c(0, 1500)
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
grid.arrange(wbar3.common.y,wbar4.common.y,ncol=2,widths=c(11,9))

# 12.0 Boxplot ----

# Box plot of community farmer bids (US/ha) for GMA/non-GMA sites

(box1 <- ggplot (Zambia, aes(COMMUNITY, USDHA)) + geom_boxplot(aes(fill=GMA))+ 
    geom_point()+
    ylab("Total cost per hectare (USD))") +                             
    xlab("\nCommunity") +
    guides(fill=guide_legend(title="GMA or non-GMA sites")) + 
    theme(axis.text.x=element_text(size=11, angle=90, vjust=1, hjust=1)))

# Ordered box plot of individual community bids (US/ha) for GMA/non-GMA sites

(box2 <- ggplot (Zambia, aes(x=reorder(COMMUNITY, USDHA, FUN=median),y=USDHA)) + geom_boxplot(aes(fill=GMA2))+ 
    ylab("Total cost per hectare (USD))") +                             
    xlab("\nCommunity") +
    guides(fill=guide_legend(title="GMA or non-GMA sites")) +
    theme(
      axis.text.x=element_text(size=11, angle=90, vjust=1, hjust=1),
      axis.line = element_line(color="black", size = 0.1),
      panel.background = element_blank()))

# 13.0 Bar plot and trendline of farmer proportions enrolled for conservation services relative ----

# Ecoregion 1
(wline <- ggplot(Eco1, aes(x=reorder(FARMSIZE, PROPORTION), y=PROPORTION)) +
    geom_bar(position=position_dodge(width=0.1), width = 0.15, stat="identity", colour="black", fill="#00868B") +
    geom_smooth(method = "loess", se=TRUE, color="blue", aes(group=1)) +
    ylab("Proportion (%) of land enrolled in bid offers") +
    xlab("Farm size (ha)") +
    theme(
      panel.border = element_blank(),
      panel.background = element_blank(),
      axis.text.x=element_blank(),
      axis.ticks.x=element_blank(),
      panel.grid.minor = element_line(size = 0.1, linetype = 'solid', colour = "black")))

# Ecoregion 2
(wline2 <- ggplot(Eco2, aes(x=reorder(FARMSIZE, PROPORTION), y=PROPORTION)) +
    geom_bar(position=position_dodge(width=0.1), width = 0.15, stat="identity", colour="black", fill="#00868B") +
    geom_smooth(method = "loess", se=TRUE, color="blue", aes(group=1)) +
    ylab("Proportion (%) of land enrolled in bid offers") +
    xlab("Farm size (ha)") +
    theme(
      panel.border = element_blank(),
      panel.background = element_blank(),
      axis.text.x=element_blank(),
      axis.ticks.x=element_blank(),
      panel.grid.minor = element_line(size = 0.1, linetype = 'solid', colour = "black")))

# Arrange the two plots into a Panel 
limits <- c(0, 100)
breaks <- seq(limits[1], limits[2], by=25)

# assign common axis to both plots
wline.common.y <- wline + scale_y_continuous(limits=limits, breaks=breaks)
wline2.common.y <- wline2 + scale_y_continuous(limits=limits, breaks=breaks)

# build the plots 
wline.common.y <- ggplot_gtable(ggplot_build(wline.common.y))
wline2.common.y <- ggplot_gtable(ggplot_build(wline2.common.y))

# copy the plot height from p1 to p2
wline.common.y$heights <- wline2.common.y$heights

# Display
grid.arrange(wline.common.y,wline2.common.y,ncol=2,widths=c(10,10))
    
# 14.0 Regression analysis ----

# Overarching regression model using bid offer price 
mod.1 = lm(formula = USD ~ HA + PLOTS + ECOREGION + GMA + FARMSIZE, data = Zambia) 
summary(mod.1)

# Plot of all regressions 
plot(Zamcor, pch=16, col="blue")

# Other regression models 
# GMA using bid offer price 
mod.2 = lm(formula = USD ~ HA + ECOREGION + PROPORTION + FARMSIZE + RICHNESSINDEX, data = GMA)  
summary(mod.2)

# non-GMA using bid offer price 
mod.3 = lm(formula = USD ~ HA + PROPORTION + FARMSIZE , data = nonGMA) 
summary(mod.3)

# Ecoregion 1 using bid offer price 
mod.4 = lm(formula = USD ~ HA + GMA + PROPORTION + RICHNESSINDEX , data = Eco1) 
summary(mod.4)

# Ecoregion 2 using bid offer price 
mod.5 = lm(formula = USD ~ HA + FARMSIZE , data = Eco2) 
summary(mod.5)

# 15.0 Correlation matrix analysis ----

# Create data frame of variables with selected columns using column indices
Zamcor <- Zambia[,c(14,22,4,26,28,12,11,8,7,5,3)]
    names(Zamcor)
    Zamcor[is.na(Zamcor)] <- 0

# Group correlation test 
corr.test(Zamcor[1:11])

# Visulisations
pairs.panels(Zamcor[1:11])  

# Simple visualisation of correlation analysis effect size including significance    
x <- cor(Zamcor[1:11])
cor(x,use="pairwise.complete.obs")
colnames (x) <- c("Price/ha", "Ecoregion 1", "Socio-status index", "GMA", "Farm size", "Gender", "Age", "Plots", "Area", "Proportion enrolled", "Bid offer")
rownames(x) <- c("Price/ha", "Ecoregion 1", "Socio-status index", "GMA", "Farm size", "Gender", "Age", "Plots", "Area", "Proportion enrolled", "Bid offer")
p.mat <- cor.mtest(Zamcor, conf.level = .95)
p.mat <- cor.mtest(Zamcor)$p
corrplot(x, p.mat = p.mat, sig.level = .05)

M2 <- x
diag(M2) = NA
corrplot(M2, na.label = "NA", type="upper",tl.col = "black", p.mat = p.mat, sig.level = 0.05, insig = "blank") # Ensures non-significant values are not displayed 
#corrplot(x, order = "hclust", addrect = 2)



# ***The below is a longer version of the above***
# Bid offer 
cor.test(Zambia$USD, Zambia$HA)
cor.test(Zambia$USD, Zambia$PLOTS)
cor.test(Zambia$USD, Zambia$AGE)
cor.test(Zambia$USD, Zambia$FARMSIZE)
cor.test(Zambia$USD, Zambia$GENDER)
cor.test(Zambia$USD, Zambia$PROPORTION)
cor.test(Zambia$USD, Zambia$GMA)
cor.test(Zambia$USD, Zambia$ECOREGION)
cor.test(Zambia$USD, Zambia$COMMUNITYID)
cor.test(Zambia$USD, Zambia$RICHNESSINDEX)

# Area
cor.test(Zambia$HA, Zambia$PLOTS)
cor.test(Zambia$HA, Zambia$AGE)
cor.test(Zambia$HA, Zambia$FARMSIZE)
cor.test(Zambia$HA, Zambia$GENDER)
cor.test(Zambia$HA, Zambia$PROPORTION)
cor.test(Zambia$HA, Zambia$GMA)
cor.test(Zambia$HA, Zambia$ECOREGION)
cor.test(Zambia$HA, Zambia$COMMUNITYID)
cor.test(Zambia$HA, Zambia$RICHNESSINDEX)

# Plots
cor.test(Zambia$PLOTS, Zambia$AGE)
cor.test(Zambia$PLOTS, Zambia$FARMSIZE)
cor.test(Zambia$PLOTS, Zambia$GENDER)
cor.test(Zambia$PLOTS, Zambia$PROPORTION)
cor.test(Zambia$PLOTS, Zambia$GMA)
cor.test(Zambia$PLOTS, Zambia$ECOREGION)
cor.test(Zambia$PLOTS, Zambia$COMMUNITYID)
cor.test(Zambia$PLOTS, Zambia$RICHNESSINDEX)

# Age
cor.test(Zambia$AGE, Zambia$FARMSIZE)
cor.test(Zambia$AGE, Zambia$GENDER)
cor.test(Zambia$AGE, Zambia$PROPORTION)
cor.test(Zambia$AGE, Zambia$GMA)
cor.test(Zambia$AGE, Zambia$ECOREGION)
cor.test(Zambia$AGE, Zambia$COMMUNITYID)
cor.test(Zambia$AGE, Zambia$RICHNESSINDEX)

# Farm size
cor.test(Zambia$FARMSIZE, Zambia$GENDER)
cor.test(Zambia$FARMSIZE, Zambia$PROPORTION)
cor.test(Zambia$FARMSIZE, Zambia$GMA)
cor.test(Zambia$FARMSIZE, Zambia$ECOREGION)
cor.test(Zambia$FARMSIZE, Zambia$COMMUNITYID)
cor.test(Zambia$FARMSIZE, Zambia$RICHNESSINDEX)

# Gender
cor.test(Zambia$GENDER, Zambia$PROPORTION)
cor.test(Zambia$GENDER, Zambia$GMA)
cor.test(Zambia$GENDER, Zambia$ECOREGION)
cor.test(Zambia$GENDER, Zambia$COMMUNITYID)
cor.test(Zambia$GENDER, Zambia$RICHNESSINDEX)

# Proportion
cor.test(Zambia$PROPORTION, Zambia$GMA)
cor.test(Zambia$PROPORTION, Zambia$ECOREGION)
cor.test(Zambia$PROPORTION, Zambia$COMMUNITYID)
cor.test(Zambia$PROPORTION, Zambia$RICHNESSINDEX)

# GMA
cor.test(Zambia$GMA, Zambia$ECOREGION)
cor.test(Zambia$GMA, Zambia$COMMUNITYID)
cor.test(Zambia$GMA, Zambia$RICHNESSINDEX)

# ECoregion

cor.test(Zambia$ECOREGION, Zambia$COMMUNITYID)
cor.test(Zambia$ECOREGION, Zambia$RICHNESSINDEX)

# Community 
cor.test(Zambia$COMMUNITYID, Zambia$RICHNESSINDEX)


