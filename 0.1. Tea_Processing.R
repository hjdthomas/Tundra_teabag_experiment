#Tea bag full analysis script
#20 Nov 2017
#Sleeping willow is making a brew

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
#library(rstan)
#library(StanHeaders)
library(MuMIn)
library(MCMCglmm)
library(postMCMCglmm)

#Set some custom functions#
`%notin%` <- function(x,y) !(x %in% y)
se <- function(x) sqrt(var(x, na.rm=T)/length(x))

#Import data####
tea<-read.csv(file="scripts/users/hthomas/data/Tea/teabag_data_update.csv")

tea$Tea_init<-as.numeric(as.character(tea$Tea_init)) #Convert to number
tea$Tea_final<-as.numeric(as.character(tea$Tea_final)) #Convert to number
tea$Lat<-as.numeric(as.character(tea$Lat))
tea$Lon<-as.numeric(as.character(tea$Lon))
tea$Loss<-1-(tea$Tea_final / tea$Tea_init) #Recalculate loss using full values
tea$Days<-as.numeric(as.character(tea$Days)) #Convert to number
tea<-tea[!is.na(tea$Loss),] #Remove NAs (4859 -> 4728)
tea<-tea[!is.na(tea$Days),] #Remove NAs (none)
tea$Burial<-as.Date(tea$Burial, format = "%d/%m/%Y") #Convert date format
tea$Recovery<-as.Date(tea$Recovery, format = "%d/%m/%Y") #Convert date format
tea$latlon<-paste(tea$Lat,"_",tea$Lon,sep="") #Add unique coordinate column

#Test decay rates (Common garden data)####
#Check tea decay rates for Common Garden

Daily_Tea<-tea[grep("DT", tea$Plot),] #Daily tea only
Common_garden<-tea[grep("CG", tea$Plot),] #All common garden ambient plots
two_yr_Kluane<-filter(tea, Site == "Kluane Plateau")
two_yr_Common_Garden<-filter(tea, Plot == "CG_HT_year"|
                               Plot == "CG_HT_year"|
                               Plot == "CG_2Y"|
                               Plot == "CG_Y1_3m"|
                               Plot == "CG_Y2_3m")

#Take mean of daily tea
Daily_Tea_mean<-Daily_Tea %>%
  group_by(Days,Tea_Type) %>%
  summarise(Loss = mean(Loss))

summary(lm(log((1-Loss)*100) ~ Days, Daily_Tea_mean[Daily_Tea_mean$Tea_Type=="Green",]))
summary(lm(log((1-Loss)*100) ~ Days, Daily_Tea_mean[Daily_Tea_mean$Tea_Type=="Rooibos",]))

(daily<-ggplot(Daily_Tea_mean,aes(Days,(1-Loss)*100,colour=factor(Tea_Type)))+
  geom_point()+
  geom_smooth(method="lm", formula= (y ~ (log(x))), aes(fill = Tea_Type))+
  theme_classic()+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  scale_colour_manual(values = c("#006400", "#8B2323"),name="Tea Type")+
  scale_fill_manual(values = c("#006400", "#8B2323"),name="Tea Type")+
  labs(y="Mass remaining (%)",x="Days since burial")+
    scale_x_continuous(breaks = seq(0,50,10))+
    theme(legend.position = "none")+
  ggtitle("Kluane (warm site):\nTwo months"))



#Create figure for El's Manuscript
pdf(file="scripts/users/ewalker/Final_Outputs/Figures/Fig_S6.pdf", width = 4, height = 3)
daily
dev.off()

#Kluane

library(effects)
m1 <- lme((1-Loss)*100 ~ (log(Days)), data = two_yr_Kluane[two_yr_Kluane$Tea_Type=="Green",], random = ~1 | Plot)
ef_g <- as.data.frame(effect("log(Days)", m1, xlevels = list(Days = seq(min(two_yr_Kluane$Days), max(two_yr_Kluane$Days), 1))))

m2 <- lme((1-Loss)*100 ~ (log(Days)), data = two_yr_Kluane[two_yr_Kluane$Tea_Type=="Rooibos",], random = ~1 | Plot)
ef_r <- as.data.frame(effect("log(Days)", m2, xlevels = list(Days = seq(min(two_yr_Kluane$Days), max(two_yr_Kluane$Days), 1))))

(Kluane<-ggplot(two_yr_Kluane)+
  geom_point(aes(Days,(1-Loss)*100,colour=factor(Tea_Type)))+
  geom_ribbon(data = ef_g, mapping = aes(x = Days, ymin = lower, ymax = upper),fill="#006400", alpha=0.5)+
  geom_ribbon(data = ef_r, mapping = aes(x = Days, ymin = lower, ymax = upper),fill="#8B2323", alpha=0.5)+
  geom_line(data = ef_g, mapping = aes(x = Days, y = fit), colour = "#006400", lwd = 1.25)+
  geom_line(data = ef_r, mapping = aes(x = Days, y = fit), colour = "#8B2323", lwd = 1.25)+
  scale_colour_manual(values = c("#006400", "#8B2323"),name="Tea Type")+
  theme_classic()+
    theme(legend.position = "none")+
  labs(y="Mass remaining (%)",x="Days since burial")+
  ggtitle("Kluane (cold site):\nTwo years"))

#Common Garden
m1 <- lme((1-Loss)*100 ~ (log(Days)), data = two_yr_Common_Garden[two_yr_Common_Garden$Tea_Type=="Green",], random = ~1 | Plot)
ef_g <- as.data.frame(effect("log(Days)", m1, xlevels = list(Days = seq(min(two_yr_Common_Garden$Days), max(two_yr_Common_Garden$Days), 1))))

m2 <- lme((1-Loss)*100 ~ (log(Days)), data = two_yr_Common_Garden[two_yr_Common_Garden$Tea_Type=="Rooibos",], random = ~1 | Plot)
ef_r <- as.data.frame(effect("log(Days)", m2, xlevels = list(Days = seq(min(two_yr_Common_Garden$Days), max(two_yr_Common_Garden$Days), 1))))

(Common<-ggplot(two_yr_Common_Garden)+
    geom_point(aes(Days,(1-Loss)*100,colour=factor(Tea_Type)))+
    geom_ribbon(data = ef_g, mapping = aes(x = Days, ymin = lower, ymax = upper),fill="#006400", alpha=0.5)+
    geom_ribbon(data = ef_r, mapping = aes(x = Days, ymin = lower, ymax = upper),fill="#8B2323", alpha=0.5)+
    geom_line(data = ef_g, mapping = aes(x = Days, y = fit), colour = "#006400", lwd = 1.25)+
    geom_line(data = ef_r, mapping = aes(x = Days, y = fit), colour = "#8B2323", lwd = 1.25)+
    scale_colour_manual(values = c("#006400", "#8B2323"),name="Tea Type")+
    theme_classic()+
    theme(legend.position = "none")+
    labs(y="Mass remaining (%)",x="Days since burial")+
    ggtitle("Kluane (warm site):\nTwo years"))

pdf(file="scripts/users/hthomas/Output_images/Tea/Fig_decay.pdf", width = 8, height = 3)
grid.arrange(daily,Common,Kluane,ncol=3)
dev.off()

#Combine warm site
all_Common_Garden<-rbind(Daily_Tea, two_yr_Kluane)

m1 <- lme((1-Loss)*100 ~ (log(Days)), data = all_Common_Garden[all_Common_Garden$Tea_Type=="Green",], random = ~1 | Plot)
ef_g <- as.data.frame(effect("log(Days)", m1, xlevels = list(Days = seq(min(all_Common_Garden$Days), max(all_Common_Garden$Days), 1))))

m2 <- lme((1-Loss)*100 ~ (log(Days)), data = all_Common_Garden[all_Common_Garden$Tea_Type=="Rooibos",], random = ~1 | Plot)
ef_r <- as.data.frame(effect("log(Days)", m2, xlevels = list(Days = seq(min(all_Common_Garden$Days), max(all_Common_Garden$Days), 1))))

(Common_all<-ggplot(all_Common_Garden)+
    geom_point(aes(Days,(1-Loss)*100,colour=factor(Tea_Type)))+
    geom_ribbon(data = ef_g, mapping = aes(x = Days, ymin = lower, ymax = upper),fill="#006400", alpha=0.5)+
    geom_ribbon(data = ef_r, mapping = aes(x = Days, ymin = lower, ymax = upper),fill="#8B2323", alpha=0.5)+
    geom_line(data = ef_g, mapping = aes(x = Days, y = fit), colour = "#006400", lwd = 1.25)+
    geom_line(data = ef_r, mapping = aes(x = Days, y = fit), colour = "#8B2323", lwd = 1.25)+
    scale_colour_manual(values = c("#006400", "#8B2323"),name="Tea Type")+
    theme_classic()+
    theme(legend.position = "none")+
    labs(y="Mass remaining (%)",x="Days since burial")+
    ggtitle("Kluane (warm site):\nTwo years"))


#Clean data####
#Set minimum of three weeks incubation (i.e. first few DTs)
tea<-tea[tea$Days>21,]

#Calculate mass loss per day 
tea$Loss_Day<-tea$Loss / tea$Days



#NOT APPROPIATE - you need multiple time points!!

#Shift points that models will think are in the sea
# tea[tea$Site=="Storfjord, Lofoten Island",]$Lon<-15.75397
# tea$latlon<-paste(tea$Lat,"_",tea$Lon,sep="")

#Check meta data####

length(unique(tea$Region)) #34 regions
length(unique(tea$latlon)) #412 unique sites (unique coordinates)

#Make sure plots aren't replicated across incubation lengths
tea$Plot_unique<-paste(tea$Plot,tea$Season,sep="_")

length(unique(tea$Plot_unique)) #782 unique plots (unique coordinates - 649 but includes NA)

#Remove pentlands (boo)
tea<-subset(tea,Site!="Pentlands")

#Add tea bag index variables####

#Tea bag hydrolisable fractions:
Hg<-0.842
Hr<-0.552

#Note that I have code to group tea in multiple ways (pairwise, by plot, by site etc), 
#but here I am grouping by plot

plot_means<-tea %>% 
  group_by(Site,Plot_unique,Tea_Type,Days) %>% 
  select(Tea_ID,Site,Plot_unique,Days,Tea_Type,Loss) %>%
  dplyr::summarise(plot_mean = mean(Loss)) %>%
  tidyr::spread(Tea_Type, plot_mean) %>%
  arrange(Site) %>%
  ungroup()

#Calculate TBI values
plot_means$S<-1-(plot_means$Green/Hg)
plot_means$ar<-Hr*(1-plot_means$S)
plot_means$k<-log(plot_means$ar/((1-plot_means$Rooibos)-(1-plot_means$ar)))/plot_means$Days

#Add data to plot_means
plot_means$Region<-tea$Region[match(plot_means$Plot_unique,tea$Plot_unique)] #Add region
plot_means$Season<-tea$Season[match(plot_means$Plot_unique,tea$Plot_unique)] #Add season

#Examine realtionship between S & K#
str(plot_means)

ggplot(plot_means,aes(S,k))+
  geom_point(aes(colour=factor(Season)),alpha=0.5)+
  theme_bw()+theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  ggtitle("S vs k - All time periods")

ggplot(plot_means[plot_means$Season=="Summer",],aes(S,k))+
  geom_point(aes(colour=factor(Region)),alpha=0.5)+
  theme_bw()+theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  ggtitle("S vs k - Summer")

#Add back to tea#
tea$TBI_S<-plot_means$S[match(tea$Plot_unique,plot_means$Plot_unique)]
tea$TBI_k<-plot_means$k[match(tea$Plot_unique,plot_means$Plot_unique)]

#Add climatic information####

#Correct Norway plots

tea$Plot<-as.character(tea$Plot)
tea[tea$Region=="Norway",]$Plot<-paste0(tea[tea$Region=="Norway",]$Plot,"_",tea[tea$Region=="Norway",]$Season)

#1) Field measured variables ####
##NB - these could be updated with 2017 values
#Add plot-measured variables####
Plot_Variables_Soil<-read.csv(file="scripts/users/hthomas/Data/Tea/Env.Vars/Soil_Temps.csv")
Plot_Variables_Air<-read.csv(file="scripts/users/hthomas/Data/Tea/Env.Vars/Air_Temps.csv")
Plot_Variables_Moisture<-read.csv(file="scripts/users/hthomas/Data/Tea/Env.Vars/Moisture.csv")

#Soil
tea$soiltemp_mean<-Plot_Variables_Soil$Mean_Temperature[match(tea$Plot,Plot_Variables_Soil$Plot)]
tea$soiltemp_GDD5<-Plot_Variables_Soil$GDD_5[match(tea$Plot,Plot_Variables_Soil$Plot)]
tea$soiltemp_source<-Plot_Variables_Soil$source[match(tea$Plot,Plot_Variables_Soil$Plot)]

#Air
tea$airtemp_mean<-Plot_Variables_Air$Mean_Temperature[match(tea$Plot,Plot_Variables_Air$Plot)]
tea$airtemp_GDD5<-Plot_Variables_Air$GDD_5[match(tea$Plot,Plot_Variables_Air$Plot)]
tea$airtemp_source<-Plot_Variables_Air$source[match(tea$Plot,Plot_Variables_Air$Plot)]

#Moisture
tea$moisture_mean<-Plot_Variables_Moisture$Mean_moisture[match(tea$Plot,Plot_Variables_Moisture$Plot)]
tea$moisture_mean_source<-Plot_Variables_Moisture$source[match(tea$Plot,Plot_Variables_Moisture$Plot)]

moisture<-subset(tea,!is.na(moisture_mean))
#Convert HOBO to %


#NB - could decide to exclude all (online) weather station data at this point#

#2) CHELSA data ####
chelsa_year<-raster("/Volumes/Teamshrub/Climate_Data/Chelsa/CHELSA_bio10_1.tif")
chelsa_summer<-raster("/Volumes/Teamshrub/Climate_Data/Chelsa/CHELSA_bio10_10.tif")
chelsa_summer_tundra<-raster("/Volumes/Teamshrub/Climate_Data/Chelsa/workshop/CHELSA_summer_mean_temp_arctic_10corrected.tif")
chelsa_winter<-raster("/Volumes/Teamshrub/Climate_Data/Chelsa/CHELSA_bio10_11.tif") 
chelsa_precip_year<-raster("/Volumes/Teamshrub/Climate_Data/Chelsa/CHELSA_bio10_12.tif")
chelsa_precip_summer<-raster("/Volumes/Teamshrub/Climate_Data/Chelsa/CHELSA_bio10_18.tif")
chelsa_precip_summer_tundra<-raster("/Volumes/Teamshrub/Climate_Data/Chelsa/workshop/CHELSA_summer_mean_precip_arctic_10corrected.tif")
chelsa_precip_winter<-raster("/Volumes/Teamshrub/Climate_Data/Chelsa/CHELSA_bio10_19.tif")

#NB - May also want to create composite rasters - e.g. JJA, or !JJA / Spring

#tea$Lon<-as.numeric(tea$Lon)
#tea$Lat<-as.numeric(tea$Lat)

tea_coords<-cbind(tea$Lon, tea$Lat)
str(tea_coords)

tea$CHELSA_year_temp<-extract(chelsa_year,tea_coords)/10
tea$CHELSA_summer_temp<-extract(chelsa_summer,tea_coords)/10
tea$CHELSA_winter_temp<-extract(chelsa_winter,tea_coords)/10
tea$CHELSA_year_precip<-extract(chelsa_precip_year,tea_coords)/10
tea$CHELSA_summer_precip<-extract(chelsa_precip_summer,tea_coords)/10
tea$CHELSA_winter_precip<-extract(chelsa_precip_winter,tea_coords)/10

#3) Soil moisture data
ESA_summer_NH<-raster("scripts/users/hthomas/ESA_Soil_Moisture/1979_2013_mean_JJA.tif")
ESA_summer_SH<-raster("scripts/users/hthomas/ESA_Soil_Moisture/1979_2013_mean_DJF_SH.tif")

ESA_summer<-merge(ESA_summer_NH,ESA_summer_SH)

ESA_winter_NH<-raster("scripts/users/hthomas/ESA_Soil_Moisture/1979_2013_mean_JJA.tif")
ESA_winter_SH<-raster("scripts/users/hthomas/ESA_Soil_Moisture/1979_2013_mean_DJF_SH.tif")
ESA_winter<-merge(ESA_winter_NH,ESA_winter_SH)

ESA_year<-raster("scripts/users/hthomas/ESA_Soil_Moisture/1978_2015_mean.tif")


unique(tea_coords)
#Add to tea object
tea$ESA_moisture<-raster::extract(ESA_summer,tea_coords)*100
tea$ESA_moisture_winter<-extract(ESA_winter,tea_coords)*100
tea$ESA_moisture_year<-extract(ESA_year,tea_coords)*100

#Rename sites and regions to consistent set

sites_names<-read.csv(file="scripts/users/hthomas/data/Tea/teabag_data_update_SiteNames_NEW.csv")
sites_names$Tea_init<-as.numeric(as.character(sites_names$Tea_init)) #Convert to number
sites_names$Tea_final<-as.numeric(as.character(sites_names$Tea_final)) #Convert to number
sites_names$Loss<-1-(sites_names$Tea_final / sites_names$Tea_init) #Recalculate loss using full values
sites_names<-sites_names[!is.na(sites_names$Loss),] #Remove NAs (4859 -> 4728)
sites_names<-sites_names[!is.na(sites_names$Days),] #Remove NAs (none)
sites_names<-sites_names[sites_names$Days>21,]
sites_names<-subset(sites_names,Site!="Pentlands")

tea$Region <- sites_names$Region
tea$Site <- sites_names$Site

#Add hierarchy###################-------------------------------------

#Decomposition sites####

#Region = ESA cell#
tea$ESA_cell <- extract(ESA_summer,tea_coords, cellnumbers=TRUE)[,1]
#ESA cell_season
tea$ESA_cell_season<-paste(tea$ESA_cell,tea$Season,sep="_")

#Add plots and sites within region
nsite<- tea %>%
  group_by(ESA_cell_season) %>%
  summarise(Unique_Sites = n_distinct(Site),
            Unique_Plots = n_distinct(Plot))

nsite$has.sites<-ifelse(nsite$Unique_Sites <=1, 0,1)
nsite$has.plots<-ifelse(nsite$Unique_Plots <=1, 0,1)

#Add plots within site
nsite.plot <- tea %>%
  group_by(ESA_cell_season,Site) %>%
  summarise(Unique_Site_Plot = n_distinct(Plot))

nsite.plot$has.site.plots<-ifelse(nsite.plot$Unique_Site_Plot <=1, 0,1)

#Merge object
nsite$has.site.plots<-nsite.plot$has.site.plots[match(nsite$ESA_cell_season,nsite.plot$ESA_cell_season)]

#Define four levels
#1 - region only (plot = site = region)
nsite$is.region<-ifelse(nsite$has.sites == 0 & nsite$has.plots == 0, 1,0)

#2 - site only (plot = site, multiple sites in region)
nsite$is.site<-ifelse(nsite$has.sites == 1 & nsite$has.plots == 0, 1,0)

#3 - plot only (multiple plots in site, site = region)
nsite$is.plot<-ifelse(nsite$has.sites == 0 & nsite$has.plots == 1, 1,0)

#4 - site_plot (plot within site within region)
nsite$is.site.plot<-ifelse(nsite$has.sites == 1 & nsite$has.plots == 1, 1,0)

#Check - should all add up to one
nsite$check<-nsite$is.region + nsite$is.site + nsite$is.plot + nsite$is.site.plot
max(nsite$check); min(nsite$check)

#Add back to tea
tea<-merge(tea,nsite[,c(1,7:10)],by = "ESA_cell_season")


#Environmental variables#######-------------------------------------------

env.list <- c("airtemp_mean", "soiltemp_mean", "moisture_mean", "CHELSA_summer_temp", "CHELSA_summer_precip", "ESA_moisture")

for(i in 1:6){

  var = env.list[i]
  
  #Are variables unique to site
  nsite<- tea %>%
    group_by(ESA_cell_season) %>%
    summarise(Unique_site = n_distinct(get(var)))
  
  nsite$has.site<-ifelse(nsite$Unique_site <=1, 0,1)
  
  #Are variables unique to plot
  nplot<- tea %>%
    group_by(ESA_cell_season,Site) %>%
    summarise(Unique_plot = n_distinct(get(var)))
  
  nplot$has.plot<-ifelse(nplot$Unique_plot <=1, 0,1)
  
  #Merge object
  nsite$has.plot<-nplot$has.plot[match(nsite$ESA_cell_season,nplot$ESA_cell_season)]
  
  #Sum - if has site and plot = 2, if just site = 1, if just region = 0
  nsite$sum<-nsite$has.plot + nsite$has.site
  
  #Add category
  nsite$cat<-ifelse(nsite$sum == 2,"Plot",ifelse(nsite$sum==1, "Site","Region"))
  names(nsite)[6]<-paste0(var,"_var_level")
  
  tea<-merge(tea,nsite[,c(1,6)],by = "ESA_cell_season")
  }
  



#Write output csv

write.csv(tea,"scripts/users/hthomas/tea/combined_tea.csv")

tea<-read.csv("scripts/users/hthomas/tea/combined_tea.csv")




tea_stats <- tea %>%
  group_by(Region,Site) %>%
  summarise(tea = length(Loss),
            ESA_cell = length(unique(ESA_cell)),
            Plot = length(unique(Plot)))

tea$Region<-as.character(tea$Region)

tea[tea$Contributor == "Jonathan von Oppen, Sonja Wipf" | tea$Contributor == "Christian Rixen, Janet Prevey",]$Region<-"Swiss Alps"

tea_stats3 <- tea %>%
  group_by(Region,Site) %>%
  summarise(sites = length(unique(Site)),
            Plot = length(unique(Plot)),
            tea = length(Loss),
            MAT = mean(CHELSA_year_temp),
            MST = mean(CHELSA_summer_temp),
            MWT = mean(CHELSA_winter_temp),
            MAP = mean(CHELSA_year_precip),
            MSP = mean(CHELSA_summer_precip),
            MWP = mean(CHELSA_winter_precip),
            MAM = mean(ESA_moisture_year),
            MSM = mean(ESA_moisture),
            MWM = mean(ESA_moisture_winter))

sum(tea_stats$tea)

unique(tea_stats$Region)

min(tea$CHELSA_year_temp,na.rm = T)

tea[3251,]

#Tea comparison figure####
litter_Haydn<-read.csv("scripts/users/hthomas/Data/Litterbags_2017.csv")
litter_Haydn$Species


Common_garden2<-tea[grep("CG", tea$Plot),]
litterbed<-subset(tea,Plot=="DC_HT_1yr")
litterbed$Type<-"Surface"
Common_garden2$Type<-"Soil"
tea_CG<-rbind(Common_garden2,litterbed)

tea_CG2<-tea_CG[grep("DT", tea_CG$Plot),] #Daily tea only

tea_CG2$Plot<-as.character(tea_CG2$Plot)
tea_CG$Plot<-as.character(tea_CG$Plot)
tea_CG<-subset(tea_CG, Plot %notin% tea_CG2$Plot)

pdf("scripts/users/hthomas/Output_Images/Tea/Final/SF_Surface.pdf",height = 3, width = 5)
ggplot(tea_CG[tea_CG$Season == "Year",])+
  geom_boxplot(aes(Tea_Type,Loss,fill = factor(Type)))+
  theme_classic()+
  scale_fill_manual(values = c("grey40","white"),
                      labels = c("Buried (5cm depth)","Surface"),
                      name = "")+
  theme(legend.justification = "top")+
  labs(x = "Tea type", y = "Mass loss (%)")+
  ylim(0,1)
dev.off()


