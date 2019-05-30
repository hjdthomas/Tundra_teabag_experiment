############S#################
## TRAIT CHANGE OVER SPACE ##
############S#################

rm(list=ls())

#Detach packages####
detachAllPackages <- function() {
  
  basic.packages <- c("package:stats","package:graphics","package:grDevices","package:utils","package:datasets","package:methods","package:base")
  
  package.list <- search()[ifelse(unlist(gregexpr("package:",search()))==1,TRUE,FALSE)]
  
  package.list <- setdiff(package.list,basic.packages)
  
  if (length(package.list)>0)  for (package in package.list) detach(package, character.only=TRUE)
  
}

detachAllPackages()


#Question 1 - Differences between teas

####Open packages####
library(raster)
library(rgdal)
library(lme4)
library(nlme)
library(stringr)
library(plyr)
library(dplyr)
library(ggplot2)
require(gridExtra)
#library(brms)
library(rstan)
library(StanHeaders)
library(MuMIn)
library(MCMCglmm)
library(postMCMCglmm)

#### CHOOSE TRAIT NAME AND CLIMATE VARIABLE HERE ----

###Read in tea
tea<-read.csv("scripts/users/hthomas/tea/combined_tea.csv", stringsAsFactors = F)

#Remove daily tea - too confusing!
tea<-subset(tea,!grepl("CG_DT_HT",tea$Plot))

#Remove sub zero plots
tea<-subset(tea,Loss>0)
tea[tea$Tea_Type=="Rooibos" & tea$Loss >0.5,]$Loss<-NA

# #Remove air temp plots
# `%notin%` <- function(x,y) !(x %in% y)
# non_viables<-c("Weather Station","Weather station","Weather station (5 year mean)","Online weather station",
#                "Online weather station (regional average) & Local weather station(Endalen)",
#                "Weather station (airport)","weather station","weather station averages (three stations)",
#                "Weather station (min and max temps)")
# 
# tea<-subset(tea,airtemp_source%notin%non_viables)

#Remove red tea (k) and plot replicates
tea<-subset(tea,Tea_Type == "Rooibos")


#Make sure only using control plots
ambient<-subset(tea,Treatment=="None")

#Split into seasons to make things easier

summer<-subset(ambient,Season=="Summer")
year<-subset(ambient,Season=="Year")
winter<-subset(ambient,Season=="Winter")


#Airtemp vs CHELSA

#Remove NAs
summer_narm<-summer%>%
  filter(is.finite(CHELSA_summer_temp),is.finite(airtemp_mean))

#Take site means
summer_narm <- summer_narm %>%
  group_by(ESA_cell,Site) %>%
  mutate(airtemp_mean = mean(airtemp_mean)) %>%
  distinct(airtemp_mean, .keep_all = TRUE)

#Take site means


#Take extent
min_air<-min(summer_narm$airtemp_mean,na.rm=TRUE)
max_air<-max(summer_narm$airtemp_mean,na.rm=TRUE)

#Run model
MC_ML_air<-MCMCglmm(CHELSA_summer_temp ~ airtemp_mean, random=~Region:Site, data=summer_narm, nitt = 150000, burnin = 50000)


#Model preditions
mm <- expand.grid(airtemp_mean = seq(min_air, max_air, 0.1), CHELSA_summer_temp = 0)  # Create a blank dataset with the years we want
mm2 <- model.matrix(terms(CHELSA_summer_temp ~ airtemp_mean), mm)  # Create matrix of relevant effect sizes
mm$CHELSA_summer_temp <- mm2 %*% fixef(MC_ML_air, use = "mean")  # Calculate based on the relevant effect sizes
mm$plo <- mm2 %*% summary(MC_ML_air)$solutions[,2]
mm$phi <- mm2 %*% summary(MC_ML_air)$solutions[,3]


(air <- ggplot()+
    #stat_smooth(data=summer[summer$Tea_Type=="Green",],aes(airtemp_mean,Loss*100,group=Region),method="lm",se=FALSE,colour="forestgreen",size=0.6, alpha=0.8)+
    #stat_smooth(data=summer[summer$Tea_Type=="Rooibos",],aes(airtemp_mean,Loss*100,group=Region),method="lm",se=FALSE,colour="red",size=0.6, alpha=0.8)+
    #geom_ribbon(data = mm.red, mapping = aes(x = airtemp_mean, ymin = plo*100, ymax = phi*100), fill = "red3", alpha = 0.3) +
    #geom_line(data = mm.red, mapping = aes(x = airtemp_mean, y=var*100), colour="red3", size=1) +
    geom_ribbon(data = mm, mapping = aes(x = airtemp_mean, ymin = plo, ymax = phi), fill = "grey", alpha = 0.5) +
    geom_point(data=summer_narm,aes(airtemp_mean,CHELSA_summer_temp),colour="grey50",pch=16)+
    geom_line(data = mm, mapping = aes(x = airtemp_mean,y=CHELSA_summer_temp), colour="black", size=1.25)+
    theme_classic()+
    geom_abline(slope=1, intercept=0, linetype = "dashed")+
    labs(y="CHELSA Temperature (°C, 1979-2013)",x="Measured Air Temeprature (°C)")+
    ggtitle("Air Temperature vs CHELSA Temperature")+
    theme(plot.title = element_text(size=12, face = "bold")))

#Soiltemp vs CHELSA

#Remove NAs
summer_narm<-summer%>%
  filter(is.finite(CHELSA_summer_temp),is.finite(soiltemp_mean))

#Take site means
summer_narm <- summer_narm %>%
  group_by(ESA_cell,Site) %>%
  mutate(soiltemp_mean = mean(soiltemp_mean)) %>%
  distinct(soiltemp_mean, .keep_all = TRUE)

#Take site means


#Take extent
min_air<-min(summer_narm$soiltemp_mean,na.rm=TRUE)
max_air<-max(summer_narm$soiltemp_mean,na.rm=TRUE)

#Run model
MC_ML_air<-MCMCglmm(CHELSA_summer_temp ~ soiltemp_mean, random=~Region:Site, data=summer_narm, nitt = 150000, burnin = 50000)


#Model preditions
mm <- expand.grid(soiltemp_mean = seq(min_air, max_air, 0.1), CHELSA_summer_temp = 0)  # Create a blank dataset with the years we want
mm2 <- model.matrix(terms(CHELSA_summer_temp ~ soiltemp_mean), mm)  # Create matrix of relevant effect sizes
mm$CHELSA_summer_temp <- mm2 %*% fixef(MC_ML_air, use = "mean")  # Calculate based on the relevant effect sizes
mm$plo <- mm2 %*% summary(MC_ML_air)$solutions[,2]
mm$phi <- mm2 %*% summary(MC_ML_air)$solutions[,3]


(soil <- ggplot()+
    #stat_smooth(data=summer[summer$Tea_Type=="Green",],aes(soiltemp_mean,Loss*100,group=Region),method="lm",se=FALSE,colour="forestgreen",size=0.6, alpha=0.8)+
    #stat_smooth(data=summer[summer$Tea_Type=="Rooibos",],aes(soiltemp_mean,Loss*100,group=Region),method="lm",se=FALSE,colour="red",size=0.6, alpha=0.8)+
    #geom_ribbon(data = mm.red, mapping = aes(x = soiltemp_mean, ymin = plo*100, ymax = phi*100), fill = "red3", alpha = 0.3) +
    #geom_line(data = mm.red, mapping = aes(x = soiltemp_mean, y=var*100), colour="red3", size=1) +
    geom_ribbon(data = mm, mapping = aes(x = soiltemp_mean, ymin = plo, ymax = phi), fill = "grey", alpha = 0.5) +
    geom_point(data=summer_narm,aes(soiltemp_mean,CHELSA_summer_temp),colour="grey50",pch=16)+
    geom_line(data = mm, mapping = aes(x = soiltemp_mean,y=CHELSA_summer_temp), colour="black", size=1.25)+
    theme_classic()+
    geom_abline(slope=1, intercept=0, linetype = "dashed")+
    labs(y="CHELSA Temperature (°C, 1979-2013)",x="Measured Soil Temeprature (°C)")+
    ggtitle("Soil Temperature vs CHELSA Temperature")+
    theme(plot.title = element_text(size=12, face = "bold")))

#Soil moisture vs CHELSA precip

#Remove NAs
summer_narm<-summer%>%
  filter(is.finite(ESA_moisture),is.finite(moisture_mean))

#Take site means
summer_narm <- summer_narm %>%
  group_by(ESA_cell,Site) %>%
  mutate(moisture_mean = mean(moisture_mean)) %>%
  distinct(moisture_mean, .keep_all = TRUE)

#Take site means


#Take extent
min_air<-min(summer_narm$moisture_mean,na.rm=TRUE)
max_air<-max(summer_narm$moisture_mean,na.rm=TRUE)

#Run model
MC_ML_air<-MCMCglmm(ESA_moisture ~ moisture_mean, random=~Region:Site, data=summer_narm, nitt = 150000, burnin = 50000)


#Model preditions
mm <- expand.grid(moisture_mean = seq(min_air, max_air, 0.1), ESA_moisture = 0)  # Create a blank dataset with the years we want
mm2 <- model.matrix(terms(ESA_moisture ~ moisture_mean), mm)  # Create matrix of relevant effect sizes
mm$ESA_moisture <- mm2 %*% fixef(MC_ML_air, use = "mean")  # Calculate based on the relevant effect sizes
mm$plo <- mm2 %*% summary(MC_ML_air)$solutions[,2]
mm$phi <- mm2 %*% summary(MC_ML_air)$solutions[,3]


(moisture <- ggplot()+
    #stat_smooth(data=summer[summer$Tea_Type=="Green",],aes(moisture_mean,Loss*100,group=Region),method="lm",se=FALSE,colour="forestgreen",size=0.6, alpha=0.8)+
    #stat_smooth(data=summer[summer$Tea_Type=="Rooibos",],aes(moisture_mean,Loss*100,group=Region),method="lm",se=FALSE,colour="red",size=0.6, alpha=0.8)+
    #geom_ribbon(data = mm.red, mapping = aes(x = moisture_mean, ymin = plo*100, ymax = phi*100), fill = "red3", alpha = 0.3) +
    #geom_line(data = mm.red, mapping = aes(x = moisture_mean, y=var*100), colour="red3", size=1) +
    geom_ribbon(data = mm, mapping = aes(x = moisture_mean, ymin = plo, ymax = phi), fill = "grey", alpha = 0.5) +
    geom_point(data=summer_narm,aes(moisture_mean,ESA_moisture),colour="grey50",pch=16)+
    geom_line(data = mm, mapping = aes(x = moisture_mean,y=ESA_moisture), colour="black", size=1.25)+
    theme_classic()+
    #geom_abline(slope=1, intercept=0, linetype = "dashed")+
    labs(y="ESA Soil Moisture (%, 1979-2013)",x="Measured Soil Moisture (%)")+
    ggtitle("Soil Moisture vs ESA Soil Moisture")+
    theme(plot.title = element_text(size=12, face = "bold")))

#Soil moisture vs CHELSA precip

#Remove NAs
summer_narm<-summer%>%
  filter(is.finite(CHELSA_summer_precip),is.finite(moisture_mean))

#Take site means
summer_narm <- summer_narm %>%
  group_by(ESA_cell,Site) %>%
  mutate(moisture_mean = mean(moisture_mean)) %>%
  distinct(moisture_mean, .keep_all = TRUE)



#Take extent
min_air<-min(summer_narm$moisture_mean,na.rm=TRUE)
max_air<-max(summer_narm$moisture_mean,na.rm=TRUE)

#Run model
MC_ML_air<-MCMCglmm(CHELSA_summer_precip ~ moisture_mean, random=~Region:Site, data=summer_narm, nitt = 150000, burnin = 50000)


#Model preditions
mm <- expand.grid(moisture_mean = seq(min_air, max_air, 0.1), CHELSA_summer_precip = 0)  # Create a blank dataset with the years we want
mm2 <- model.matrix(terms(CHELSA_summer_precip ~ moisture_mean), mm)  # Create matrix of relevant effect sizes
mm$CHELSA_summer_precip <- mm2 %*% fixef(MC_ML_air, use = "mean")  # Calculate based on the relevant effect sizes
mm$plo <- mm2 %*% summary(MC_ML_air)$solutions[,2]
mm$phi <- mm2 %*% summary(MC_ML_air)$solutions[,3]


(precip <- ggplot()+
    #stat_smooth(data=summer[summer$Tea_Type=="Green",],aes(moisture_mean,Loss*100,group=Region),method="lm",se=FALSE,colour="forestgreen",size=0.6, alpha=0.8)+
    #stat_smooth(data=summer[summer$Tea_Type=="Rooibos",],aes(moisture_mean,Loss*100,group=Region),method="lm",se=FALSE,colour="red",size=0.6, alpha=0.8)+
    #geom_ribbon(data = mm.red, mapping = aes(x = moisture_mean, ymin = plo*100, ymax = phi*100), fill = "red3", alpha = 0.3) +
    #geom_line(data = mm.red, mapping = aes(x = moisture_mean, y=var*100), colour="red3", size=1) +
    geom_ribbon(data = mm, mapping = aes(x = moisture_mean, ymin = plo, ymax = phi), fill = "grey", alpha = 0.5) +
    geom_point(data=summer_narm,aes(moisture_mean,CHELSA_summer_precip),colour="grey50",pch=16)+
    geom_line(data = mm, mapping = aes(x = moisture_mean,y=CHELSA_summer_precip), colour="black", size=1.25)+
    theme_classic()+
    #geom_abline(slope=1, intercept=0, linetype = "dashed")+
    labs(y="CHELSA Precipitation (mm, 1979-2013)",x="Measured Soil Moisture (%)")+
    ggtitle("Soil Moisture vs CHELSA Precipitation")+
    theme(plot.title = element_text(size=12, face = "bold")))


#Create overall figure

pdf(file="scripts/users/hthomas/Output_Images/Tea/Final/Site_vs_Gridded.pdf", width = 8, height = 8)
grid.arrange(air,soil,precip,moisture,nrow = 2)
dev.off()

