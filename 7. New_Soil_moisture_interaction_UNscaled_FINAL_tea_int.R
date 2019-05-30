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

#Make sure only using control plots
ambient<-subset(tea,Treatment=="None")

#Split into seasons to make things easier

summer<-subset(ambient,Season=="Summer")
year<-subset(ambient,Season=="Year")
winter<-subset(ambient,Season=="Winter")


## STAN MODEL - soil temperature ----
#soil temperature#
var.list <- c("Loss", "Loss_Day", "k", "TBI_k", "TBI_S")

#Calculate mean burial length


#Get column number
i=1
var.num<-which(colnames(summer)==var.list[i])

season_narm<-summer %>%
  filter(is.finite(summer[,var.num]),is.finite(soiltemp_mean))

#Subset for tea types
#season_narm_r<-subset(season_narm,Tea_Type=="Rooibos") #AB NOTE: Keeping both tea types and including as interaction in model
season_narm_r <- season_narm #just so I don't have to rename everything

# AB: MULTIPLE OBSERVATION
season_narm_r <- ddply(season_narm_r, c("ESA_cell","Site","Plot","Tea_Type"), transform, NObsPlot = length(Loss))
season_narm_r$MultipleObs <- ifelse(season_narm_r$NObsPlot > 4, 1, 0)

# Multiple Sites
count.sites <- ddply(season_narm_r, c("ESA_cell"), summarise, n.sub = length(unique(Site)))
season_narm_r$MultipleSites <- ifelse(season_narm_r$ESA_cell %in% count.sites$ESA_cell[count.sites$n.sub > 1], 1, 0)

# Multiple plots per Site (more than 1)

count.plots <- ddply(season_narm_r, c("ESA_cell", "Site"), summarise, n.plots = length(unique(Plot)))
season_narm_r$MultiplePlots <- ifelse(season_narm_r$Site %in% count.plots$Site[count.plots$n.plots > 1], 1, 0)

#Add env.levels (alternative)
#Add env.levels (new - based on nestedness)
env.levels<- season_narm_r %>%
  select(soiltemp_mean,ESA_cell,Site,Plot) 

season_narm_r$envlevel_soil<-0

env.levels2<-ddply(env.levels, c("ESA_cell"), summarise, n.plots = length(unique(soiltemp_mean)))
season_narm_r$envlevel_soil <- ifelse(season_narm_r$ESA_cell %in% env.levels2$ESA_cell[env.levels2$n.plots > 1], 1, season_narm_r$envlevel_soil)

env.levels2<-ddply(env.levels, c("ESA_cell","Site"), summarise, n.plots = length(unique(soiltemp_mean)))
season_narm_r$envlevel_soil <- ifelse(season_narm_r$Site %in% env.levels2$Site[env.levels2$n.plots > 1], 2, season_narm_r$envlevel_soil)

#And for moisture
env.levels<- season_narm_r %>%
  select(moisture_mean,ESA_cell,Site,Plot) 

season_narm_r$envlevel_moisture<-0

env.levels2<-ddply(env.levels, c("ESA_cell"), summarise, n.plots = length(unique(moisture_mean)))
season_narm_r$envlevel_moisture <- ifelse(season_narm_r$ESA_cell %in% env.levels2$ESA_cell[env.levels2$n.plots > 1], 1, season_narm_r$envlevel_moisture)

env.levels2<-ddply(env.levels, c("ESA_cell","Site"), summarise, n.plots = length(unique(moisture_mean)))
season_narm_r$envlevel_moisture <- ifelse(season_narm_r$Site %in% env.levels2$Site[env.levels2$n.plots > 1], 2, season_narm_r$envlevel_moisture)

#Now take lowest env level as overall one
season_narm_r$envlevel <- apply(season_narm_r[, 63:64], 1, max)#Add categories

#Subset so only using data I want to check model

#season_narm_r<-subset(season_narm_r, Cat == 1 | Cat == 2 | Cat == 3)

# AB: REMOVE MISSING VALUES OF SOIL soiltemp AND TEMPERATURE FOR THE soiltemp X TEMPERATURE INTERACTION MODEL
season_narm_r <- season_narm_r[!is.na(season_narm_r$soiltemp_mean) & !is.na(season_narm_r$moisture_mean),]

#Add Region numbers
season_narm_r<-season_narm_r %>% 
  mutate(RegionNum = group_indices_(season_narm_r, .dots=c("ESA_cell","Tea_Type"))) 

#Reorder by site number
season_narm_r<-season_narm_r[order(season_narm_r$RegionNum),] 

#Add Site numbers
season_narm_r<-season_narm_r %>% 
  mutate(SiteNum = group_indices_(season_narm_r, .dots=c("ESA_cell","Site","Tea_Type"))) 

#Reorder by site number
season_narm_r<-season_narm_r[order(season_narm_r$SiteNum),] 

#Add Plot numbers
season_narm_r<-season_narm_r %>% 
  mutate(PlotNum = group_indices_(season_narm_r, .dots=c("ESA_cell","Site","Plot","Tea_Type"))) #AB NOTE: This now includes tea type as well! So there will be a unique plot number for each tea type within a plot

#Reorder by plot number
season_narm_r<-season_narm_r[order(season_narm_r$PlotNum),] 

LQ_moist<-quantile(season_narm_r$moisture_mean,0.25)
UQ_moist<-quantile(season_narm_r$moisture_mean,0.75)
mean_moist<-mean(season_narm_r$moisture_mean)
med_moist<-quantile(season_narm_r$moisture_mean,0.5)

#Centre values - AB note: Either don't name this the same thing or save the amount you center by first so we can add it to the xhats later
soiltemp_cent_amount <- attr(scale(season_narm_r$soiltemp_mean, center = TRUE, scale = FALSE), 'scaled:center')
soiltemp_scale_amount <- attr(scale(season_narm_r$soiltemp_mean, center = TRUE, scale = FALSE), 'scaled:scale')
season_narm_r$soiltemp_mean<-scale(season_narm_r$soiltemp_mean, center = TRUE, scale = FALSE)
moist_cent_amount <- attr(scale(season_narm_r$moisture_mean, center = TRUE, scale = FALSE), 'scaled:center')
moist_scale_amount <- attr(scale(season_narm_r$moisture_mean, center = TRUE, scale = FALSE), 'scaled:scale')
season_narm_r$moisture_mean<-scale(season_narm_r$moisture_mean, center = TRUE, scale = FALSE)
days_cent_amount <- attr(scale(season_narm_r$Days, center = TRUE, scale = FALSE), 'scaled:center')
days_scale_amount <- attr(scale(season_narm_r$Days, center = TRUE, scale = FALSE), 'scaled:scale')
season_narm_r$Days<-scale(season_narm_r$Days, center = TRUE, scale = FALSE)

#AB: caluclate mean and sd per site - YOU CAN THINK ABOUT WHETHER YOU WANT THIS TO BE THE OVERALL MEAN OR THE MEAN OF MEANS - MEAN OF MEANS MIGHT BE BETTER IN THIS CASE
season_narm_r_sites<-season_narm_r %>%
  group_by(SiteNum) %>%
  summarise(soiltemp_mean_site = mean(soiltemp_mean),
            soiltemp_sd_site = sd(soiltemp_mean),
            moist_mean_site = mean(moisture_mean),
            moist_sd_site = sd(moisture_mean))

season_narm_r$soiltemp_mean_site<-season_narm_r_sites$soiltemp_mean_site[match(season_narm_r$SiteNum, season_narm_r_sites$SiteNum)]
season_narm_r$soiltemp_sd_site<-season_narm_r_sites$soiltemp_sd_site[match(season_narm_r$SiteNum, season_narm_r_sites$SiteNum)]
season_narm_r$moist_mean_site<-season_narm_r_sites$moist_mean_site[match(season_narm_r$SiteNum, season_narm_r_sites$SiteNum)]
season_narm_r$moist_sd_site<-season_narm_r_sites$moist_sd_site[match(season_narm_r$SiteNum, season_narm_r_sites$SiteNum)]

season_narm_r$soiltemp_sd_site[season_narm_r$soiltemp_sd_site==0] <- mean(season_narm_r$soiltemp_sd_site[season_narm_r$soiltemp_sd_site>0],na.rm = T)
season_narm_r$soiltemp_sd_site[is.na(season_narm_r$soiltemp_sd_site)] <- 0.001
season_narm_r$moist_sd_site[season_narm_r$moist_sd_site==0] <- mean(season_narm_r$moist_sd_site[season_narm_r$moist_sd_site>0],na.rm = T)
season_narm_r$moist_sd_site[is.na(season_narm_r$moist_sd_site)] <- 0.001

#AB: caluclate mean and sd per region - YOU CAN THINK ABOUT WHETHER YOU WANT THIS TO BE THE OVERALL MEAN OR THE MEAN OF MEANS - MEAN OF MEANS MIGHT BE BETTER IN THIS CASE
season_narm_r_regions<-season_narm_r %>%
  group_by(RegionNum) %>%
  summarise(soiltemp_mean_region = mean(soiltemp_mean),
            soiltemp_sd_region = sd(soiltemp_mean),
            moist_mean_region = mean(moisture_mean),
            moist_sd_region = sd(moisture_mean))

season_narm_r$soiltemp_mean_region<-season_narm_r_regions$soiltemp_mean_region[match(season_narm_r$RegionNum, season_narm_r_regions$RegionNum)]
season_narm_r$soiltemp_sd_region<-season_narm_r_regions$soiltemp_sd_region[match(season_narm_r$RegionNum, season_narm_r_regions$RegionNum)]
season_narm_r$moist_mean_region<-season_narm_r_regions$moist_mean_region[match(season_narm_r$RegionNum, season_narm_r_regions$RegionNum)]
season_narm_r$moist_sd_region<-season_narm_r_regions$moist_sd_region[match(season_narm_r$RegionNum, season_narm_r_regions$RegionNum)]

season_narm_r$soiltemp_sd_region[season_narm_r$soiltemp_sd_region==0] <- mean(season_narm_r$soiltemp_sd_region[season_narm_r$soiltemp_sd_region>0],na.rm = T)
season_narm_r$soiltemp_sd_region[is.na(season_narm_r$soiltemp_sd_region)] <- 0.001
season_narm_r$moist_sd_region[season_narm_r$moist_sd_region==0] <- mean(season_narm_r$moist_sd_region[season_narm_r$moist_sd_region>0],na.rm = T)
season_narm_r$moist_sd_region[is.na(season_narm_r$moist_sd_region)] <- 0.001

#Add mean days per region
season_narm_r<-season_narm_r %>%
  group_by(SiteNum) %>%
  mutate(SiteDays = mean(Days),
         SiteDays_sd = sd(Days))

season_narm_r$SiteDays_sd[season_narm_r$SiteDays_sd==0 | is.na(season_narm_r$SiteDays_sd)] <- 0.001

#Add mean days per region
season_narm_r<-season_narm_r %>%
  group_by(RegionNum) %>%
  mutate(RegionDays = mean(Days),
         RegionDays_sd = sd(Days))

season_narm_r$RegionDays_sd[season_narm_r$RegionDays_sd==0 | is.na(season_narm_r$RegionDays_sd)] <- 0.001


mean_burial<-mean(season_narm_r$Days)
min_soil<-min(season_narm_r$soiltemp_mean,na.rm=TRUE)
max_soil<-max(season_narm_r$soiltemp_mean,na.rm=TRUE)
min_soiltemp<-min(season_narm_r$soiltemp_mean,na.rm=TRUE)
max_soiltemp<-max(season_narm_r$soiltemp_mean,na.rm=TRUE)

moist_x_cent_amount <- attr(scale(c(LQ_moist,UQ_moist,mean_moist,med_moist), center = TRUE, scale = FALSE), 'scaled:center')
xhats <- expand.grid(xhat1=seq(min_soiltemp, max_soiltemp,by=0.01), xhat2=scale(c(LQ_moist,UQ_moist,mean_moist,med_moist), center = TRUE, scale = FALSE),xhat3 = mean_burial) #AB: predicting soil soiltemp at 25% and 75% (assuming you will graph temperature as continuous) but of course you can change this to whatever you want


####Third attempt - adding temperature levels#######
jags.dat<-list(
  Nobs=nrow(season_narm_r),
  NSite=length(unique(season_narm_r$SiteNum)),
  NRegion=length(unique(season_narm_r$RegionNum)),
  NPlot=length(unique(season_narm_r$PlotNum)),
  NSiteDays=length(unique(season_narm_r$SiteDays)),  
  NRegionDays=length(unique(season_narm_r$RegionDays)),
  NTea=length(unique(season_narm_r$Tea_Type)),
  Region=season_narm_r$RegionNum,
  Site=season_narm_r$SiteNum,
  Plot=season_narm_r$PlotNum,
  SiteDays=season_narm_r$SiteDays[!duplicated(season_narm_r$SiteNum)],
  SiteDays_sd=season_narm_r$SiteDays_sd[!duplicated(season_narm_r$SiteNum)],
  RegionDays=season_narm_r$RegionDays[!duplicated(season_narm_r$RegionNum)],
  RegionDays_sd=season_narm_r$RegionDays_sd[!duplicated(season_narm_r$RegionNum)],
  Site_short=season_narm_r$SiteNum[!duplicated(season_narm_r$PlotNum)],
  Plot_short=unique(season_narm_r$PlotNum),
  tea_type_site=ifelse(season_narm_r$Tea_Type[!duplicated(season_narm_r$SiteNum)]=="Green", 1, 2),
  tea_type_region=ifelse(season_narm_r$Tea_Type[!duplicated(season_narm_r$RegionNum)]=="Green", 1, 2),
  multobs_lobs=season_narm_r$MultipleObs,
  multobs_lplot=season_narm_r$MultipleObs[!duplicated(season_narm_r$PlotNum)],
  multsites_lobs=season_narm_r$MultipleSites,
  multsites_lplot=season_narm_r$MultipleSites[!duplicated(season_narm_r$PlotNum)],
  multsites_lsite=season_narm_r$MultipleSites[!duplicated(season_narm_r$SiteNum)],
  multsites_lregion=season_narm_r$MultipleSites[!duplicated(season_narm_r$RegionNum)],
  multplots_lobs=season_narm_r$MultiplePlots,
  multplots_lplot=season_narm_r$MultiplePlots[!duplicated(season_narm_r$PlotNum)],
  multplots_lsite=season_narm_r$MultiplePlots[!duplicated(season_narm_r$SiteNum)],
  multplots_lregion=season_narm_r$MultiplePlots[!duplicated(season_narm_r$RegionNum)],
  multplots_region_lobs=season_narm_r$MultiplePlots_Region,
  multplots_region_lplot=season_narm_r$MultiplePlots_Region[!duplicated(season_narm_r$PlotNum)],
  multplots_region_lsite=season_narm_r$MultiplePlots_Region[!duplicated(season_narm_r$SiteNum)],
  multplots_region_lregion=season_narm_r$MultiplePlots_Region[!duplicated(season_narm_r$RegionNum)],
  traitobs=season_narm_r$Loss,
  #temp_plot=as.numeric(season_narm_r[!duplicated(season_narm_r$PlotNum),]$soiltemp_mean),
  #temp_site=as.numeric(season_narm_r[!duplicated(season_narm_r$SiteNum),]$soiltemp_mean),
  temp_mean_region=as.numeric(season_narm_r[!duplicated(season_narm_r$RegionNum),]$soiltemp_mean_region),
  temp_sd_region=as.numeric(season_narm_r[!duplicated(season_narm_r$RegionNum),]$soiltemp_sd_region),
  temp_mean_site=as.numeric(season_narm_r[!duplicated(season_narm_r$SiteNum),]$soiltemp_mean_site),
  temp_sd_site=as.numeric(season_narm_r[!duplicated(season_narm_r$SiteNum),]$soiltemp_sd_site),
  moist_mean_region=as.numeric(season_narm_r[!duplicated(season_narm_r$RegionNum),]$moist_mean_region),
  moist_sd_region=as.numeric(season_narm_r[!duplicated(season_narm_r$RegionNum),]$moist_sd_region),
  moist_mean_site=as.numeric(season_narm_r[!duplicated(season_narm_r$SiteNum),]$moist_mean_site),
  moist_sd_site=as.numeric(season_narm_r[!duplicated(season_narm_r$SiteNum),]$moist_sd_site),
  obs_envlevel=season_narm_r$envlevel,
  plot_envlevel=season_narm_r[!duplicated(season_narm_r$PlotNum),]$envlevel,
  site_envlevel=season_narm_r[!duplicated(season_narm_r$SiteNum),]$envlevel,
  region_envlevel=season_narm_r[!duplicated(season_narm_r$RegionNum),]$envlevel,
  meanT=mean(as.numeric(season_narm_r$soiltemp_mean[!duplicated(season_narm_r$ESA_cell)])),
  xhat1=xhats$xhat1,
  xhat2=xhats$xhat2,
  xhat3=xhats$xhat3,
  Nxhat=length(xhats$xhat1)
)

str(jags.dat)


# MODEL - ANNE EDITS####

write("
      
      data {
      int<lower=0> Nobs; //Number of observations
      int<lower=0> NRegion; //Number of regions
      int<lower=0> NSite; //Number of sites
      int<lower=0> NPlot; //Number of plots
      int<lower=0> Nxhat; //No. predictor variables
      int<lower=0> NTea; //No. of tea types
      int<lower=0> NSiteDays; //No. of days
      int<lower=0> NRegionDays; //No. of days
      int<lower=1,upper=NPlot> Plot[Nobs]; //Plots (all observations)
      int<lower=1,upper=NSite> Site[Nobs]; //Plots (all observations)
      int<lower=1,upper=NRegion> Region[Nobs]; //Plots (all observations)
      int<lower=1,upper=2> tea_type_site[NSite]; //Tea type (1=Green, 2=Rooibos)
      int<lower=1,upper=2> tea_type_region[NRegion]; //Tea type (1=Green, 2=Rooibos)

      int<lower=0,upper=1> multobs_lobs[Nobs]; //Are sites nested in region (all obs)
      int<lower=0,upper=1> multobs_lplot[NPlot]; //Are sites nested in region (all obs)
      int<lower=0,upper=1> multsites_lobs[Nobs]; //Are sites nested in region (all obs)
      int<lower=0,upper=1> multsites_lplot[NPlot]; //Are sites nested in region (no. plots)
      int<lower=0,upper=1> multsites_lsite[NSite]; //Are sites nested in region (no. plots)
      int<lower=0,upper=1> multsites_lregion[NRegion]; //Are sites nested in region (no. plots)
      int<lower=0,upper=1> multplots_lobs[Nobs]; //Are plots nested in site  (all obs)
      int<lower=0,upper=1> multplots_lplot[NPlot]; //Are plots nested in site  (no plots)
      int<lower=0,upper=2> obs_envlevel[Nobs];
      int<lower=0,upper=2> site_envlevel[NSite];
      int<lower=0,upper=2> region_envlevel[NRegion];

      vector[Nobs] traitobs; //Mass Loss
      vector[NSite] temp_mean_site; //Temperature (unique regions)
      vector[NSite] temp_sd_site; //Temperature SD (unique regions)
      vector[NRegion] temp_mean_region; //Temperature (unique regions)
      vector[NRegion] temp_sd_region; //Temperature SD (unique regions)
      vector[NSite] moist_mean_site; //Temperature (unique regions)
      vector[NSite] moist_sd_site; //Temperature SD (unique regions)
      vector[NRegion] moist_mean_region; //Temperature (unique regions)
      vector[NRegion] moist_sd_region; //Temperature SD (unique regions)
      vector[NSite] SiteDays; //
      vector[NSite] SiteDays_sd; //
      vector[NRegion] RegionDays; //
      vector[NRegion] RegionDays_sd; //

      vector[Nxhat] xhat1; //Predictor variables
      vector[Nxhat] xhat2; //Predictor variables
      vector[Nxhat] xhat3; //Predictor variables
      
      }
      
      parameters {
      real<lower=-3,upper=3> aMeanRegion[NRegion];  // Region effect
      real<lower=-5,upper=5> ap[NPlot];
      real<lower=-5,upper=5> as[NSite];
      real<lower=-2,upper=2> gamma0[NTea];  // intercept of relationship between mass loss and temp change 
      real<lower=-2,upper=2> gamma1[NTea];  // slope of temperature - loss relationship
      real<lower=-2,upper=2> gamma2[NTea];  // slope of soiltemp - loss relationship
      real<lower=-2,upper=2> gamma3[NTea];  // temperature - soiltemp interaction
      real<lower=-2,upper=2> gamma4[NTea];  // temperature - soiltemp interaction
      
      real<lower=0,upper=5> sigma_overall; //Error around loss- temp relationship
      real<lower=0,upper=5> sigma_plot;
      real<lower=0,upper=5> sigma_site;
      real<lower=0,upper=5> sigma_region;
      real<lower=0,upper=5> sigma_resid;
      
      vector[NSite] temp_pred_site;
      vector[NSite] moist_pred_site;
      vector[NSite] days_pred_site;
      vector[NRegion] temp_pred_region;
      vector[NRegion] moist_pred_region;
      vector[NRegion] days_pred_region;
      
      }
      
      transformed parameters {
      
      vector[Nobs] mu;   
      vector[Nobs] app;
      vector[Nobs] ass;
      vector[Nobs] arr;
      
      for (i in 1:Nobs){
      
      if((multobs_lobs[i]==1 && multplots_lobs[i]==1))
      app[i] = ap[Plot[i]];
      // set plot effects to 0 for plots that don't have multiple obs or are the only plot within a site
      else app[i] = 0;
      
      if(multsites_lobs[i] == 1)
      ass[i] = as[Site[i]];
      else ass[i] = 0;
      
      if(multsites_lobs[i]==1 && obs_envlevel[i] >0)
      arr[i] = 0;
      else arr[i] = aMeanRegion[Region[i]];
      
      mu[i] = app[i] + ass[i] + arr[i];;
      
      
      }
      
      //print(\"ap=\",ap[1:10],\"as=\",as[1:10],\"aMeanSite=\",aMeanSite[1:8],\"mu=\",mu[1:10])
      
      }
      
      model {
      
      for (i in 1:Nobs){
      traitobs[i] ~ normal(mu[i], sigma_resid);
      }
      
      //Set up plot and site random effects
      
      for (i in 1:NPlot){
      if(multobs_lplot[i]==1 && multplots_lplot[i]==1)
      ap[i] ~ normal(0, sigma_plot);
      }
      

      //Bring in environmental data means and SD per region
      
      for (i in 1:NRegion){
      temp_pred_region[i] ~ normal(temp_mean_region[i], temp_sd_region[i]); //temp_mean_region and temp_sd are given as data
      moist_pred_region[i] ~ normal(moist_mean_region[i], moist_sd_region[i]); //temp_mean_region and temp_sd are given as data
      days_pred_region[i] ~ normal(RegionDays[i], RegionDays_sd[i]); //temp_mean_region and temp_sd are given as data
      }

      for (i in 1:NSite){
      temp_pred_site[i] ~ normal(temp_mean_site[i], temp_sd_site[i]); //temp_mean_region and temp_sd are given as data
      moist_pred_site[i] ~ normal(moist_mean_site[i], moist_sd_site[i]); //temp_mean_region and temp_sd are given as data
      days_pred_site[i] ~ normal(SiteDays[i], SiteDays_sd[i]); //temp_mean_region and temp_sd are given as data
      }
      
      //Relationship between mass loss at the region level and temperature and soiltemp, per tea type
      
      for (i in 1:NSite){
      if(multsites_lsite[i] == 1 && site_envlevel[i] >0)
      as[i] ~ normal(gamma0[tea_type_site[i]] + gamma1[tea_type_site[i]]*temp_pred_site[i] + gamma2[tea_type_site[i]]*moist_pred_site[i] + gamma3[tea_type_site[i]]*temp_pred_site[i]*moist_pred_site[i] + gamma4[tea_type_site[i]]*days_pred_site[i], sigma_overall); 
      else as[i] ~ normal(0, sigma_site);
      }

      for (i in 1:NRegion){
      if(multsites_lregion[i] == 1 && region_envlevel[i] >0)
      aMeanRegion[i] ~ normal(0, sigma_region);
      else aMeanRegion[i] ~ normal(gamma0[tea_type_region[i]] + gamma1[tea_type_region[i]]*temp_pred_region[i] + gamma2[tea_type_region[i]]*moist_pred_region[i] + gamma3[tea_type_region[i]]*temp_pred_region[i]*moist_pred_region[i] + gamma4[tea_type_region[i]]*days_pred_region[i], sigma_overall); 
      }

      
      } //Close model
      
      generated quantities{
      
      matrix[Nxhat,NTea] preds; //matrix of predictions
      real<lower=-5,upper=5> teaDiff;
      real<lower=-5,upper=5> tempDiff;
      real<lower=-5,upper=5> moistDiff;
      real<lower=-5,upper=5> intDiff;
      real<lower=-5,upper=5> daysDiff;
      
      for (i in 1:Nxhat){
      for (j in 1:NTea){
      preds[i,j] = (gamma0[j] + gamma1[j]*xhat1[i] + gamma2[j]*xhat2[i] + gamma3[j]*xhat1[i]*xhat2[i] + gamma4[j]*xhat3[i]); //predictions 
      }
      }
      
      teaDiff <- gamma0[1]-gamma0[2]; //AB: if you want to know whether the intercepts of the tea types are significantly different, can also do this with the slopes or a prediction at a particular xhat if you want
      tempDiff <- gamma1[1]-gamma1[2]; //AB: if you want to know whether the intercepts of the tea types are significantly different, can also do this with the slopes or a prediction at a particular xhat if you want
      moistDiff <- gamma2[1]-gamma2[2]; //AB: if you want to know whether the intercepts of the tea types are significantly different, can also do this with the slopes or a prediction at a particular xhat if you want
      intDiff <- gamma3[1]-gamma3[2]; //AB: if you want to know whether the intercepts of the tea types are significantly different, can also do this with the slopes or a prediction at a particular xhat if you want
      daysDiff <- gamma4[1]-gamma4[2]; //AB: if you want to know whether the intercepts of the tea types are significantly different, can also do this with the slopes or a prediction at a particular xhat if you want
      
      }
      
      ","scripts/users/hthomas/Tea/soiltemp_loss_3.stan")



stanc('scripts/users/hthomas/Tea/soiltemp_loss_3.stan') #check model

options(mc.cores = parallel::detectCores())

initsA <- list(ap=rep(0.6,jags.dat$NPlot), aMeanRegion=rep(0.6,jags.dat$NRegion),as=rep(0.6,jags.dat$NSite))
initsB <- list(ap=rep(0.3,jags.dat$NPlot), aMeanRegion=rep(0.3,jags.dat$NRegion),as=rep(0.3,jags.dat$NSite))
inits <- list(initsA, initsB)

fit_space <- stan(file = 'scripts/users/hthomas/Tea/soiltemp_loss_3.stan', data = jags.dat, init=inits, iter = 15000, chains = 2, thin = 1, verbose = TRUE, control=list(adapt_delta=0.99,max_treedepth = 15), algorithm = "NUTS") 


s = summary(fit_space)
rownames(s$summary)
(s$summary)[202]
max(s$summary[,10],na.rm = T) # max Rhat
hist(s$summary[,"Rhat"], breaks=100)
hist(s$summary[,"n_eff"])

print(fit_space)
stan_trace(fit_space, inc_warmup = TRUE, pars = c("gamma0","gamma1"))
stan_trace(fit_space, inc_warmup = TRUE, pars = c("aMeanSite[1]","aMeanSite[2]"))

cout <- as.data.frame(s$summary)
cout$Param <- unlist(lapply(rownames(cout), function (x) {strsplit(x,split="[",fixed=T)}[[1]][1]))
cout$Number <- as.vector(sapply(strsplit(rownames(cout),"[^0-9]+",fixed=FALSE), "[", 2))
cout[cout$Rhat > 1.1 & !is.na(cout$Rhat),]

hist(cout$mean[cout$Param=="aMeanSite"])
cout[cout$Param %in% c("gamma0","gamma1","gamma2","gamma3"),] #these will tell you about the "significance" of your environmental predictors
#gamma1 = temperature, gamma2 = soiltemp, gamma3 = temp X soiltemp interaction (for each tea type)

#Compare to raw data

# plot.compare <- ddply(season_narm_r[season_narm_r$MultipleObs==1,], c("Site","Plot","PlotNum","Tea_Type"), summarise,
#                       rawLoss = mean(Loss))
# 
# plot.compare$StanEst <- cout$mean[match(plot.compare$PlotNum, cout$Number[cout$Param=="ap"])]
# ggplot(plot.compare)+
#   geom_point(aes(x=rawLoss,y=StanEst,colour=Tea_Type))

region.compare <- ddply(season_narm_r, c("RegionNum","Tea_Type"), summarise,
                        rawLoss = mean(Loss))

region.compare$StanEst <- cout$mean[match(region.compare$RegionNum, cout$Number[cout$Param=="aMeanSite"])]
ggplot(region.compare)+
  geom_point(aes(x=rawLoss,y=StanEst,colour=Tea_Type))

# Graph predictions

predsout.space <- cout[cout$Param %in% c("preds"),]
predsout.space$soiltemp <- rep(jags.dat$xhat1, each=2)
predsout.space$TempBT <- predsout.space$soiltemp  + soiltemp_cent_amount 
predsout.space$Moisture <- rep(jags.dat$xhat2, each=2)
predsout.space$MoistureBT <- predsout.space$Moisture + moist_x_cent_amount
predsout.space$Tea_TypeNum <- rep(c(1,2), times = (length(predsout.space$mean)/2))
predsout.space$Tea_Type <- ifelse(predsout.space$Tea_TypeNum==1,"Green","Rooibos")

save(predsout.space, file = "scripts/users/hthomas/Tea/Stan_outputs/soiltemp_moisture_preds_tea_int_unscaled.Rdata")
save(cout, file = "scripts/users/hthomas/Tea/Stan_outputs/soiltemp_moisture_fits_tea_int_unscaled.Rdata")

#Anne graph
pdf("scripts/users/hthomas/Output_Images/Tea/soiltemp_moisture_unscaled.pdf", width = 3, height = 3)
ggplot()+
  geom_ribbon(data=predsout.space[predsout.space$MoistureBT==LQ_moist | predsout.space$MoistureBT==UQ_moist,],aes(x=soiltemp +soiltemp_cent_amount,ymin=(`2.5%`),ymax=(`97.5%`),fill=factor(Tea_Type):factor(Moisture)),alpha=0.2)+
  geom_point(data=season_narm_r[season_narm_r$Tea_Type=="Green",],aes(x=soiltemp_mean+soiltemp_cent_amount,y=Loss),colour = "#006400",pch =16 ,alpha=0.6)+
  geom_point(data=season_narm_r[season_narm_r$Tea_Type=="Rooibos",],aes(x=soiltemp_mean+soiltemp_cent_amount,y=Loss), colour = "#8B2323",pch =16 ,alpha=0.6)+
  geom_line(data=predsout.space[predsout.space$MoistureBT==LQ_moist | predsout.space$MoistureBT==UQ_moist,],aes(x=soiltemp+soiltemp_cent_amount,y=mean, colour=factor(Tea_Type):factor(Moisture)), alpha=0.8, lwd = 1.5)+
  theme_classic()+
  coord_cartesian(y = c(0,1))+
  scale_colour_manual(values = c("#00b100","#003100","#c83232","#4e1414"), name = "Tea Type")+
  scale_fill_manual(values = c("#00b100","#003100","#c83232","#4e1414"), name = "Tea Type")+
  scale_linetype_manual(values = c("dashed","solid"), name = "Moisture", labels = c("low","high"))+
  labs(x = "Soil Temperature (°C)", y = "Mass Loss (%)")+
  theme(legend.position = "none")
dev.off()


(soiltemp_moist_tea_int_unscaled<-ggplot()+
    geom_ribbon(data=predsout.space[predsout.space$MoistureBT==LQ_moist | predsout.space$MoistureBT==UQ_moist,],aes(x=soiltemp +soiltemp_cent_amount,ymin=(`2.5%`),ymax=(`97.5%`),fill=factor(Tea_Type):factor(Moisture)),alpha=0.2)+
    geom_point(data=season_narm_r[season_narm_r$Tea_Type=="Green",],aes(x=soiltemp_mean+soiltemp_cent_amount,y=Loss),colour = "#006400",pch =16 ,alpha=0.6)+
    geom_point(data=season_narm_r[season_narm_r$Tea_Type=="Rooibos",],aes(x=soiltemp_mean+soiltemp_cent_amount,y=Loss), colour = "#8B2323",pch =16 ,alpha=0.6)+
    geom_line(data=predsout.space[predsout.space$MoistureBT==LQ_moist | predsout.space$MoistureBT==UQ_moist,],aes(x=soiltemp+soiltemp_cent_amount,y=mean, colour=factor(Tea_Type):factor(Moisture)), alpha=0.8, lwd = 1.5)+
    theme_classic()+
    coord_cartesian(y = c(0,1))+
    scale_colour_manual(values = c("#00b100","#003100","#c83232","#4e1414"), name = "Tea Type")+
    scale_fill_manual(values = c("#00b100","#003100","#c83232","#4e1414"), name = "Tea Type")+
    scale_linetype_manual(values = c("dashed","solid"), name = "Moisture", labels = c("low","high"))+
    labs(x = "Soil Temperature (°C)", y = "Mass Loss (%)")+
    theme(legend.position = "none"))

save(soiltemp_moist_tea_int_unscaled, file = "scripts/users/hthomas/Tea/soiltemp_moist_tea_int_unscaled.Rdata")

