##### Temperature data for teabag incubations #####

#Detach packages####
detachAllPackages <- function() {
  
  basic.packages <- c("package:stats","package:graphics","package:grDevices","package:utils","package:datasets","package:methods","package:base")
  
  package.list <- search()[ifelse(unlist(gregexpr("package:",search()))==1,TRUE,FALSE)]
  
  package.list <- setdiff(package.list,basic.packages)
  
  if (length(package.list)>0)  for (package in package.list) detach(package, character.only=TRUE)
  
}

detachAllPackages()

#### packages ####
library(xts)
library(dplyr)
library(tools)
library(reshape2)
library(stringr)
library(tidyr)
library(ggplot2)
library(data.table)
library(lubridate)
`%notin%` <- function(x,y) !(x %in% y)

#Read in base data####
tea<-read.csv(file="scripts/users/hthomas/data/Tea/teabag_data_update.csv")

tea$Burial<-as.Date(tea$Burial, format = "%d/%m/%Y")
tea$Recovery<-as.Date(tea$Recovery, format = "%d/%m/%Y")

Plot_Variables_Air<-NULL
Plot_Variables_Soil<-NULL
Plot_Variables_Moisture<-NULL

#Munt Buffalora####

#### data processing
filenames<- list.files(path="scripts/users/hthomas/Data/Tea/Env.Vars/Davos", pattern=".{3}_.{1}_.{1}") # create list of temperature data files

tempdata<- lapply(filenames, function(x) read.csv(paste("scripts/users/hthomas/Data/Tea/Env.Vars/Davos/",x,sep=""), sep=";", header=T, skip=18)) # read in all data files into large list

for(i in 1:length(filenames)) {
  colnames(tempdata[[i]]) <- as.character(unlist(tempdata[[i]][1,])); tempdata[[i]] <- tempdata[[i]][-1,]; # put first line as header
  tempdata[[i]] <- tempdata[[i]][,-2]; # delete dispensable second column 
  tempdata[[i]][,2] <- as.numeric(as.character(tempdata[[i]][,2]))
  tempdata[[i]][,1] <- as.POSIXct(tempdata[[i]][,1],format="%d.%m.%Y %H:%M") # convert date column into date format
  days <- tempdata[[i]][,1]
  tempdata[[i]] <- aggregate(tempdata[[i]][,2], by = list(days), mean); # compute mean temperatures per day
  
  fileinfo <- strsplit(file_path_sans_ext(filenames[i]),"_")[[1]]; # extract plot infos from file name
  
  for (j in 1:3) {
    tempdata[[i]][,2+j] <- fileinfo[j]
  } # add columns for plot infos
  colnames(tempdata[[i]])[c(1:5)] <- c("date","temp","summit","aspect","rep")
}

#Raw Data
temp <- do.call("rbind", tempdata)
temp$Day<-cut(temp$date, "day") #Extract days
temp<-subset(temp,summit=="BUF") #Only look at sites in tea bag experiment

temp$Plot<-paste("CH-",temp$summit,"_",temp$aspect,sep="") #Add tea plot name

# ggplot(temp,aes(date,temp,colour=factor(Plot)))+
#   geom_point(size=0.1)+
#   geom_smooth()+
#   theme_bw()

#Daily means
daily_temp<- temp %>%
  group_by(Day,Plot) %>%
  summarise(Daily_mean = mean(temp))

daily_temp$Day<- as.Date(daily_temp$Day)

# ggplot(daily_temp,aes(Day,Daily_mean,colour=factor(Plot)))+
#   geom_point(size=0.4)+
#   geom_line()+
#   theme_bw()

#Plot means
#Trim data to dates of tea burial
plot_soiltemp_means=NULL
for(i in unique(tea[tea$Contributor=="Jonathan von Oppen, Sonja Wipf",]$Plot)){
  DOB<-min(tea[tea$Plot==i,]$Burial) #Find date of burial
  DOR<-max(tea[tea$Plot==i,]$Recovery) #Find date of recovery
  daily_temp_cut<-subset(daily_temp,Day>=DOB&Day<=DOR) #Trim data
  
  Out<- daily_temp_cut %>%
    filter(Plot==i) %>%
    group_by(Plot) %>%
    summarise(Mean_Temperature = mean(Daily_mean),
              sd_Temperature = sd(Daily_mean),
              GSL_0 = length(which(Daily_mean > 0)),
              GSL_5 = length(which(Daily_mean > 5)),
              GDD_0 = sum(Daily_mean[Daily_mean>0]),
              GDD_5 = sum(Daily_mean[Daily_mean>5]))
  
  plot_soiltemp_means<-rbind(plot_soiltemp_means,Out)
}

plot_soiltemp_means$source<-"ibutton data"
plot_soiltemp_means$scale<-"plot"

Plot_Variables_Soil<-rbind(Plot_Variables_Soil,plot_soiltemp_means)

#Munt Buffalora - Air####
temp<-read.csv(file="scripts/users/hthomas/Data/Tea/Env.Vars/Mt_Buff_Air/Weather_data.csv")


#Add plot and date
temp$date<-as.POSIXct(temp$Date,format="%d/%m/%Y")
temp$Site<-"Munt Buffalora"

# #Plot data
# ggplot(temp,aes(date,avg))+
#   geom_point(size=0.1)+
#   geom_smooth()+
#   theme_bw()

#Daily means
daily_temp<- temp %>%
  group_by(date,Site) %>%
  summarise(Daily_mean = mean(avg))

#Plot means
#Trim data to dates of tea burial
plot_temp_means=NULL
for(i in unique(tea[tea$Contributor=="Jonathan von Oppen, Sonja Wipf",]$Plot)){
  DOB<-min(tea[tea$Plot==i,]$Burial) #Find date of burial
  DOR<-max(tea[tea$Plot==i,]$Recovery) #Find date of recovery
  daily_temp_cut<-subset(daily_temp,date>=DOB&date<=DOR) #Trim data
  
  Out<- daily_temp_cut %>%
    group_by() %>%
    summarise(Mean_Temperature = mean(Daily_mean,na.rm=T),
              sd_Temperature = sd(Daily_mean,na.rm=T),
              GSL_0 = length(which(Daily_mean > 0)),
              GSL_5 = length(which(Daily_mean > 5)),
              GDD_0 = sum(Daily_mean[Daily_mean>0],na.rm=T),
              GDD_5 = sum(Daily_mean[Daily_mean>5],na.rm=T))
  
  Out$Plot<-i
  plot_temp_means<-rbind(plot_temp_means,Out)
}

plot_temp_means$source<-"Online weather station"
plot_temp_means$scale<-"Region"

Plot_Variables_Air<-rbind(Plot_Variables_Air,plot_temp_means)

#Kluane Plateau####

#Read Data
filenames <- list.files(path="scripts/users/hthomas/Data/Tea/Env.Vars/Kluane_Plateau") # create list of temperature data files

tempdata <- lapply(filenames, function(x) read.csv(paste("scripts/users/hthomas/Data/Tea/Env.Vars/Kluane_Plateau/",x,sep=""), sep=",", header=F, skip=15)) # read in all data files into large list

for(i in 1:length(filenames)) {
  #colnames(tempdata[[i]]) <- tempdata[[i]][-1,]; # put first line as header
  tempdata[[i]] <- tempdata[[i]][,-2]; # delete dispensable second column 
  tempdata[[i]][,2] <- as.numeric(as.character(tempdata[[i]][,2]))
  tempdata[[i]][,3] <- as.numeric(as.character(tempdata[[i]][,3]))
  names(tempdata[[i]])[c(1:3)]<-c("Date/Time","Value","Decimal")
  tempdata[[i]][is.na(tempdata[[i]]$Decimal),3] <- 0
  tempdata[[i]][,1] <- as.POSIXct(tempdata[[i]][,1]) # convert date column into date format
  #days <- tempdata[[i]][,1]
  #tempdata[[i]] <- aggregate(tempdata[[i]][,2], by = list(days), mean); # compute mean temperatures per day
  
  fileinfo <- strsplit(file_path_sans_ext(filenames[i]),"-")[[1]]; # extract plot infos from file name
  
  for (j in 1:4) {
    tempdata[[i]][,3+j] <- fileinfo[j]
  } # add columns for plot infos
  colnames(tempdata[[i]])[c(1:7)] <- c("date","value","decimal","site","treatment","location","rep")
}

#Raw Data
temp <- do.call("rbind", tempdata)
temp$date<-temp$date + years(2000) #correct years
temp$temp<-paste(temp$value,temp$decimal,sep=".")
temp$temp<-as.numeric(as.character(temp$temp))
temp$Day<-cut(temp$date, "day") #Extract days

temp$Plot<-"Kluane Plateau"

KP_temp<-temp

# ggplot(temp,aes(date,temp,colour=factor(location)))+
#   geom_point(size=0.1)+
#   geom_smooth()+
#   theme_bw()

#Daily means
daily_temp<- temp %>%
  group_by(Day,location) %>%
  summarise(Daily_mean = mean(temp))

daily_temp$Day<- as.Date(daily_temp$Day)

# ggplot(daily_temp,aes(Day,Daily_mean,colour=factor(location)))+
#   geom_point(size=0.4)+
#   geom_smooth()+
#   theme_bw()

#Plot means
#Trim data to dates of tea burial
unique(tea[tea$Site=="Kluane Plateau",]$Plot) #Look up plot names
#1st summer
DOB<-min(tea[tea$Plot=="KP_Y1_3m",]$Burial) #Find date of burial
DOR<-max(tea[tea$Plot=="KP_Y1_3m",]$Recovery) #Find date of recovery
daily_temp_cut<-subset(daily_temp,Day>=DOB&Day<=DOR) #Trim data

plot_temp_means_summer1<- daily_temp_cut %>%
  group_by(location) %>%
  summarise(Mean_Temperature = mean(Daily_mean),
            sd_Temperature = sd(Daily_mean),
            GSL_0 = length(which(Daily_mean > 0)),
            GSL_5 = length(which(Daily_mean > 5)),
            GDD_0 = sum(Daily_mean[Daily_mean>0]),
            GDD_5 = sum(Daily_mean[Daily_mean>5]))
plot_temp_means_summer1$Plot<-"KP_Y1_3m"

#winter
DOB<-min(tea[tea$Plot=="KP_HT_winter",]$Burial) #Find date of burial
DOR<-max(tea[tea$Plot=="KP_HT_winter",]$Recovery) #Find date of recovery
daily_temp_cut<-subset(daily_temp,Day>=DOB&Day<=DOR) #Trim data

plot_temp_means_winter<- daily_temp_cut %>%
  group_by(location) %>%
  summarise(Mean_Temperature = mean(Daily_mean),
            sd_Temperature = sd(Daily_mean),
            GSL_0 = length(which(Daily_mean > 0)),
            GSL_5 = length(which(Daily_mean > 5)),
            GDD_0 = sum(Daily_mean[Daily_mean>0]),
            GDD_5 = sum(Daily_mean[Daily_mean>5]))
plot_temp_means_winter$Plot<-"KP_HT_winter"

#year
DOB<-min(tea[tea$Plot=="KP_HT_year",]$Burial) #Find date of burial
DOR<-max(tea[tea$Plot=="KP_HT_year",]$Recovery) #Find date of recovery
daily_temp_cut<-subset(daily_temp,Day>=DOB&Day<=DOR) #Trim data

plot_temp_means_year<- daily_temp_cut %>%
  group_by(location) %>%
  summarise(Mean_Temperature = mean(Daily_mean),
            sd_Temperature = sd(Daily_mean),
            GSL_0 = length(which(Daily_mean > 0)),
            GSL_5 = length(which(Daily_mean > 5)),
            GDD_0 = sum(Daily_mean[Daily_mean>0]),
            GDD_5 = sum(Daily_mean[Daily_mean>5]))
plot_temp_means_year$Plot<-"KP_HT_year"

#2nd summer - Uses data from first summer (not even converting to DAY so could change)####
DOB<-min(tea[tea$Plot=="KP_Y1_3m",]$Burial) #Find date of burial
DOR<-max(tea[tea$Plot=="KP_Y1_3m",]$Recovery) #Find date of recovery
daily_temp_cut<-subset(daily_temp,Day>=DOB&Day<=DOR) #Trim data

plot_temp_means_summer2<- daily_temp_cut %>%
  group_by(location) %>%
  summarise(Mean_Temperature = mean(Daily_mean),
            sd_Temperature = sd(Daily_mean),
            GSL_0 = length(which(Daily_mean > 0)),
            GSL_5 = length(which(Daily_mean > 5)),
            GDD_0 = sum(Daily_mean[Daily_mean>0]),
            GDD_5 = sum(Daily_mean[Daily_mean>5]))
plot_temp_means_summer2$Plot<-"KP_HT_s2"

plot_temp_means<-rbind(plot_temp_means_summer1,plot_temp_means_winter,plot_temp_means_year,plot_temp_means_summer2)

plot_temp_means$source<-"ibutton data"
plot_temp_means$scale<-"plot"

plot_airtemp_means<-plot_temp_means[plot_temp_means$location=="Air",c(8,2:7,9:10)]
plot_soiltemp_means<-plot_temp_means[plot_temp_means$location=="Soil",c(8,2:7,9:10)]

Plot_Variables_Air<-rbind(Plot_Variables_Air,plot_airtemp_means)
Plot_Variables_Soil<-rbind(Plot_Variables_Soil,plot_soiltemp_means)

#Pika Camp####

#Read Data
filenames <- list.files(path="scripts/users/hthomas/Data/Tea/Env.Vars/Pika_Camp") # create list of temperature data files

tempdata <- lapply(filenames, function(x) read.csv(paste("scripts/users/hthomas/Data/Tea/Env.Vars/Pika_Camp/",x,sep=""), sep=",", header=F, skip=15)) # read in all data files into large list

for(i in 1:length(filenames)) {
  #colnames(tempdata[[i]]) <- tempdata[[i]][-1,]; # put first line as header
  tempdata[[i]] <- tempdata[[i]][,-2]; # delete dispensable second column 
  tempdata[[i]][,2] <- as.numeric(as.character(tempdata[[i]][,2]))
  tempdata[[i]][,3] <- as.numeric(as.character(tempdata[[i]][,3]))
  names(tempdata[[i]])[c(1:3)]<-c("Date/Time","Value","Decimal")
  tempdata[[i]][is.na(tempdata[[i]]$Decimal),3] <- 0
  tempdata[[i]][,1] <- as.POSIXct(tempdata[[i]][,1]) # convert date column into date format
  #days <- tempdata[[i]][,1]
  #tempdata[[i]] <- aggregate(tempdata[[i]][,2], by = list(days), mean); # compute mean temperatures per day
  
  fileinfo <- strsplit(file_path_sans_ext(filenames[i]),"-")[[1]]; # extract plot infos from file name
  
  for (j in 1:4) {
    tempdata[[i]][,3+j] <- fileinfo[j]
  } # add columns for plot infos
  colnames(tempdata[[i]])[c(1:7)] <- c("date","value","decimal","site","treatment","location","rep")
}

#Raw Data
temp <- do.call("rbind", tempdata)
temp$date<-temp$date + years(2000) #correct years
temp$temp<-paste(temp$value,temp$decimal,sep=".")
temp$temp<-as.numeric(as.character(temp$temp))
temp$Day<-cut(temp$date, "day") #Extract days

temp$Plot<-paste(substr(temp$site,1,2),"HT","1yr",sep="_") #Add tea plot name

# a<-ggplot(temp[temp$site=="PP16",],aes(date,temp,colour=factor(location)))+
#   geom_point(size=0.1)+
#   geom_smooth()+
#   labs(title = "Printers Pass")+
#   theme_bw()
# 
# b<-ggplot(temp[temp$site=="PC16",],aes(date,temp,colour=factor(location)))+
#   geom_point(size=0.1)+
#   geom_smooth()+
#   labs(title = "Pika Camp")+
#   theme_bw()
# 
# grid.arrange(a,b,nrow=2)

#Daily means
daily_temp<- temp %>%
  group_by(Day,site,Plot,location) %>%
  summarise(Daily_mean = mean(temp))

daily_temp$Day<- as.Date(daily_temp$Day)

# a<-ggplot(daily_temp[daily_temp$site=="PP16",],aes(Day,Daily_mean,colour=factor(location)))+
#   geom_point(size=0.4)+
#   geom_smooth()+
#   labs(title = "Printers Pass")+
#   theme_bw()
# 
# b<-ggplot(daily_temp[daily_temp$site=="PC16",],aes(Day,Daily_mean,colour=factor(location)))+
#   geom_point(size=0.4)+
#   geom_smooth()+
#   labs(title = "Pika Camp")+
#   theme_bw()
# 
# grid.arrange(a,b,nrow=2)

#Plot means
#Trim data to dates of tea burial
plot_temp_means=NULL
for(i in unique(tea[tea$Site=="Pika Camp"|tea$Site=="Printers Pass",]$Plot)){
  DOB<-min(tea[tea$Plot==i,]$Burial) #Find date of burial
  DOR<-max(tea[tea$Plot==i,]$Recovery) #Find date of recovery
  daily_temp_cut<-subset(daily_temp,Day>=DOB&Day<=DOR) #Trim data
  
  Out<- daily_temp_cut %>%
    filter(Plot==i) %>%
    group_by(Plot,location) %>%
    summarise(Mean_Temperature = mean(Daily_mean),
              sd_Temperature = sd(Daily_mean),
              GSL_0 = length(which(Daily_mean > 0)),
              GSL_5 = length(which(Daily_mean > 5)),
              GDD_0 = sum(Daily_mean[Daily_mean>0]),
              GDD_5 = sum(Daily_mean[Daily_mean>5]))
  
  plot_temp_means<-rbind(plot_temp_means,Out)
}

plot_temp_means$source<-"ibutton data"
plot_temp_means$scale<-"plot"

plot_airtemp_means<-as.data.frame(plot_temp_means[plot_temp_means$location=="Air",-2])
plot_soiltemp_means<-as.data.frame(plot_temp_means[plot_temp_means$location=="Soil",-2])

Plot_Variables_Air<-rbind(Plot_Variables_Air,plot_airtemp_means)
Plot_Variables_Soil<-rbind(Plot_Variables_Soil,plot_soiltemp_means)

#Qikiqtaruk####

#Read Data
filenames <- list.files(path="scripts/users/hthomas/Data/Tea/Env.Vars/Herschel") # create list of temperature data files

tempdata <- lapply(filenames, function(x) read.csv(paste("scripts/users/hthomas/Data/Tea/Env.Vars/Herschel/",x,sep=""), sep=",", header=F, skip=15)) # read in all data files into large list

for(i in 1:length(filenames)) {
  #colnames(tempdata[[i]]) <- tempdata[[i]][-1,]; # put first line as header
  tempdata[[i]] <- tempdata[[i]][,-2]; # delete dispensable second column 
  tempdata[[i]][,2] <- as.numeric(as.character(tempdata[[i]][,2]))
  tempdata[[i]][,3] <- as.numeric(as.character(tempdata[[i]][,3]))
  names(tempdata[[i]])[c(1:3)]<-c("Date/Time","Value","Decimal")
  tempdata[[i]][is.na(tempdata[[i]]$Decimal),3] <- 0
  tempdata[[i]][,1] <- as.POSIXct(tempdata[[i]][,1]) # convert date column into date format
  #days <- tempdata[[i]][,1]
  #tempdata[[i]] <- aggregate(tempdata[[i]][,2], by = list(days), mean); # compute mean temperatures per day
  
  fileinfo <- strsplit(file_path_sans_ext(filenames[i]),"-")[[1]]; # extract plot infos from file name
  
  for (j in 1:3) {
    tempdata[[i]][,3+j] <- fileinfo[j]
  } # add columns for plot infos
  colnames(tempdata[[i]])[c(1:6)] <- c("date","value","decimal","site","location","rep")
}

#Raw Data
temp <- do.call("rbind", tempdata)
temp$date<-temp$date + years(2000) #correct years
temp$temp<-paste(temp$value,temp$decimal,sep=".")
temp$temp<-as.numeric(as.character(temp$temp))
temp$Day<-cut(temp$date, "day") #Extract days

temp$Plot<-paste(substr(temp$site,1,2),"HT","1yr",sep="_") #Add tea plot name

#Plot data
# ggplot(temp,aes(date,temp,colour=factor(Plot)))+
#   geom_point(size=0.1)+
#   geom_smooth()+
#   theme_bw()

#Daily means
daily_temp<- temp %>%
  group_by(Day,Plot,location) %>%
  summarise(Daily_mean = mean(temp))

daily_temp$Day<- as.Date(daily_temp$Day)

# ggplot(daily_temp,aes(Day,Daily_mean,colour=factor(Plot)))+
#   geom_point(size=0.4)+
#   geom_smooth()+
#   theme_bw()

#Plot means
#Trim data to dates of tea burial
plot_temp_means=NULL
for(i in unique(daily_temp$Plot)){
  DOB<-min(tea[tea$Plot==i,]$Burial) #Find date of burial
  DOR<-max(tea[tea$Plot==i,]$Recovery) #Find date of recovery
  daily_temp_cut<-subset(daily_temp,Day>=DOB&Day<=DOR) #Trim data
  
  Out<- daily_temp_cut %>%
    filter(Plot==i) %>%
    group_by(Plot,location) %>%
    summarise(Mean_Temperature = mean(Daily_mean),
              sd_Temperature = sd(Daily_mean),
              GSL_0 = length(which(Daily_mean > 0)),
              GSL_5 = length(which(Daily_mean > 5)),
              GDD_0 = sum(Daily_mean[Daily_mean>0]),
              GDD_5 = sum(Daily_mean[Daily_mean>5]))
  
  plot_temp_means<-rbind(plot_temp_means,Out)
}

plot_temp_means$source<-"ibutton data"
plot_temp_means$scale<-"plot"

plot_airtemp_means<-as.data.frame(plot_temp_means[plot_temp_means$location=="air",-2])
plot_soiltemp_means<-as.data.frame(plot_temp_means[plot_temp_means$location=="soil",-2])

Plot_Variables_Air<-rbind(Plot_Variables_Air,plot_airtemp_means)
Plot_Variables_Soil<-rbind(Plot_Variables_Soil,plot_soiltemp_means)

#Abisko 2016####
#Read Data
filenames <- list.files(path="scripts/users/hthomas/Data/Tea/Env.Vars/Abisko_2016") # create list of temperature data files

tempdata <- lapply(filenames, function(x) read.csv(paste("scripts/users/hthomas/Data/Tea/Env.Vars/Abisko_2016/",x,sep=""), sep=",", header=F, skip=15)) # read in all data files into large list

for(i in 1:length(filenames)) {
  #colnames(tempdata[[i]]) <- tempdata[[i]][-1,]; # put first line as header
  tempdata[[i]] <- tempdata[[i]][,-2]; # delete dispensable second column 
  tempdata[[i]][,2] <- as.numeric(as.character(tempdata[[i]][,2]))
  tempdata[[i]][,3] <- as.numeric(as.character(tempdata[[i]][,3]))
  names(tempdata[[i]])[c(1:3)]<-c("Date/Time","Value","Decimal")
  tempdata[[i]][is.na(tempdata[[i]]$Decimal),3] <- 0
  tempdata[[i]][,1] <- as.POSIXct(tempdata[[i]][,1],format="%d/%m/%y %H:%M") # convert date column into date format
  #days <- tempdata[[i]][,1]
  #tempdata[[i]] <- aggregate(tempdata[[i]][,2], by = list(days), mean); # compute mean temperatures per day
  
  fileinfo <- strsplit(file_path_sans_ext(filenames[i]),"-")[[1]]; # extract plot infos from file name
  
  for (j in 1:1) {
    tempdata[[i]][,3+j] <- fileinfo[j]
  } # add columns for plot infos
  colnames(tempdata[[i]])[c(1:4)] <- c("date","value","decimal","Plot")
}

#Raw Data
temp <- do.call("rbind", tempdata)
temp$temp<-paste(temp$value,temp$decimal,sep=".")
temp$temp<-as.numeric(as.character(temp$temp))
temp$Day<-cut(temp$date, "day") #Extract days

#Plot data
# ggplot(temp,aes(date,temp,colour=factor(Plot)))+
#   geom_point(size=0.1)+
#   geom_smooth()+
#   theme_bw()+
#   theme(legend.position="none")

#Daily means
daily_temp<- temp %>%
  group_by(Day,Plot) %>%
  summarise(Daily_mean = mean(temp))

daily_temp$Day<- as.Date(daily_temp$Day)

#Plot means
#Trim data to dates of tea burial
plot_temp_means=NULL

for(i in unique(daily_temp$Plot)){
  DOB<-min(tea[tea$Plot==i,]$Burial) #Find date of burial
  DOR<-max(tea[tea$Plot==i,]$Recovery) #Find date of recovery
  daily_temp_cut<-subset(daily_temp,Day>=DOB&Day<=DOR) #Trim data
  
  Out<- daily_temp_cut %>%
    filter(Plot==i) %>%
    group_by(Plot) %>%
    summarise(Mean_Temperature = mean(Daily_mean),
              sd_Temperature = sd(Daily_mean),
              GSL_0 = length(which(Daily_mean > 0)),
              GSL_5 = length(which(Daily_mean > 5)),
              GDD_0 = sum(Daily_mean[Daily_mean>0]),
              GDD_5 = sum(Daily_mean[Daily_mean>5]))
  
  plot_temp_means<-rbind(plot_temp_means,Out)
}

plot_temp_means$source<-"ibutton data"
plot_temp_means$scale<-"plot"

Plot_Variables_Soil<-rbind(Plot_Variables_Soil,plot_temp_means)

#Norway 2016 - Soil####

#Read data
temp<-read.table(file="scripts/users/hthomas/Data/Tea/Env.Vars/Norway_2016/iButtons_Norway_2016.txt")
#Add plot and date
temp$Plot<-paste(temp$mountain,temp$transect,temp$plot,sep="")
#Rename NO14R
temp[temp$Plot=="NO14R",]$Plot<-"NO14N"

temp$Plot<-paste0(temp$Plot,"_Year")
temp$date<-as.POSIXct(paste("20",temp$year,"/",temp$month,"/",temp$day," ",temp$hour,":",temp$minute,sep=""),format="%Y/%m/%d %H:%M")
temp$Day<-cut(temp$date, "day") #Extract days



# #Plot data
# ggplot(temp,aes(date,temp,colour=factor(Plot)))+
#   geom_point(size=0.1)+
#   geom_smooth()+
#   theme_bw()+
#   theme(legend.position="none")

#Daily means
daily_temp<- temp %>%
  group_by(Day,Plot) %>%
  summarise(Daily_mean = mean(temp))

daily_temp$Day<- as.Date(daily_temp$Day)

#Plot means
#Trim data to dates of tea burial

#Find Norway Sites
norway<-subset(tea,Region=="Norway")
norway$Plot<-paste0(norway$Plot,"_",norway$Season)
plot_temp_means=NULL
for(i in unique(norway$Plot)){
  
  DOB<-min(norway[norway$Plot==i,]$Burial) #Find date of burial
  DOR<-max(norway[norway$Plot==i,]$Recovery) #Find date of recovery
  
  #j<-str_split(unique(norway[norway$Plot==i,]$Plot),"_")[[1]][1]
  
  Out<- daily_temp %>%
    #filter(Plot==j) %>%
    filter(Day>=DOB & Day<=DOR) %>% #Trim data
    group_by(Plot) %>%
    summarise(Mean_Temperature = mean(Daily_mean),
              sd_Temperature = sd(Daily_mean),
              GSL_0 = length(which(Daily_mean > 0)),
              GSL_5 = length(which(Daily_mean > 5)),
              GDD_0 = sum(Daily_mean[Daily_mean>0]),
              GDD_5 = sum(Daily_mean[Daily_mean>5]))
  
  tryCatch(Out$Plot<-i,error=function(e) NULL)
  
  plot_temp_means<-rbind(plot_temp_means,Out)
}

plot_temp_means$source<-"ibutton data"
plot_temp_means$scale<-"plot"

Plot_Variables_Soil<-rbind(Plot_Variables_Soil,plot_temp_means)

#Norway 2016 - Air####

#Read data
temp<-read.table(file="scripts/users/hthomas/Data/Tea/Env.Vars/Norway_2016/iButtons_Norway_2016AIR.txt")
#Add plot and date
temp$Plot<-paste(temp$mountain,temp$transect,substr(temp$plot,1,1),sep="")
temp$Plot<-paste0(temp$Plot,"_Year")

temp$date<-as.POSIXct(paste("20",temp$year,"/",temp$month,"/",temp$day," ",temp$hour,":",temp$minute,sep=""),format="%Y/%m/%d %H:%M")
temp$Day<-cut(temp$date, "day") #Extract days

#Plot data
# ggplot(temp,aes(date,temp,colour=factor(Plot)))+
#   geom_point(size=0.1)+
#   geom_smooth()+
#   theme_bw()+
#   theme(legend.position="none")

#Daily means
daily_temp<- temp %>%
  group_by(Day,Plot) %>%
  summarise(Daily_mean = mean(temp))

daily_temp$Day<- as.Date(daily_temp$Day)

#Plot means
#Trim data to dates of tea burial
plot_temp_means=NULL
for(i in unique(norway$Plot)){
  
  DOB<-min(norway[norway$Plot==i,]$Burial) #Find date of burial
  DOR<-max(norway[norway$Plot==i,]$Recovery) #Find date of recovery
  
  #j<-str_split(unique(norway[norway$Plot==i,]$Plot),"_")[[1]][1]
  
  Out<- daily_temp %>%
    #filter(Plot==j) %>%
    filter(Day>=DOB & Day<=DOR) %>% #Trim data
    group_by(Plot) %>%
    summarise(Mean_Temperature = mean(Daily_mean),
              sd_Temperature = sd(Daily_mean),
              GSL_0 = length(which(Daily_mean > 0)),
              GSL_5 = length(which(Daily_mean > 5)),
              GDD_0 = sum(Daily_mean[Daily_mean>0]),
              GDD_5 = sum(Daily_mean[Daily_mean>5]))
  
  tryCatch(Out$Plot<-i,error=function(e) NULL)
  
  plot_temp_means<-rbind(plot_temp_means,Out)
}

plot_temp_means$source<-"ibutton data"
plot_temp_means$scale<-"plot"

air_plots<-subset(Plot_Variables_Air,Plot%notin%plot_temp_means$Plot)
Plot_Variables_Air<-rbind(plot_temp_means,air_plots)

#Abisko_Gradient####

#Read data
temp<-read.table(file="scripts/users/hthomas/Data/Tea/Env.Vars/Abisko_2016_2/L400-900_N400-900.txt")
#Add plot and date
temp$Plot<-paste(temp$mountain,temp$elevation,sep="")
temp$date<-as.POSIXct(paste("20",temp$year,"/",temp$month,"/",temp$day," ",temp$hour,":",temp$minute,sep=""),format="%Y/%m/%d %H:%M")
temp$Day<-cut(temp$date, "day") #Extract days

# #Plot data
# ggplot(temp,aes(date,temp,colour=factor(Plot)))+
#   geom_point(size=0.1)+
#   geom_smooth()+
#   theme_bw()+
#   theme(legend.position="none")

#Cut extreme site crazyness (L400)!
L400<-subset(temp,Plot=="L400")
temp<-subset(temp,Plot!="L400")
L400<-subset(L400,Day!="2016-03-16 23:00:00"&Day!="2016-03-17 23:00:00"&Day!="2016-03-15 23:00:00"&Day!="2016-03-18 23:00:00"&Day!="2016-06-26 00:00:00")
temp_2016<-rbind(temp,L400)

#Add 2015 data
#Read data
temp<-read.table(file="scripts/users/hthomas/Data/Tea/Env.Vars/Abisko_2015/iButtons_400-900masl.txt",header=T)
#Add plot and date
temp$Plot<-paste(temp$mountain,temp$elevation,sep="")
temp$date<-as.POSIXct(paste("20",temp$year,"/",temp$month,"/",temp$day," ",temp$hour,":",temp$minute,sep=""),format="%Y/%m/%d %H:%M")
temp$Day<-cut(temp$date, "day") #Extract days

temp<-rbind(select(temp_2016,Plot,Day,temp),select(temp,Plot,Day,temp))
# #Plot data
# ggplot(temp,aes(date,temp,colour=factor(Plot)))+
#   geom_point(size=0.1)+
#   geom_smooth()+
#   theme_bw()+
#   theme(legend.position="none")

#Daily means
daily_temp<- temp %>%
  group_by(Day,Plot) %>%
  summarise(Daily_mean = mean(temp))

daily_temp$Day<- as.Date(daily_temp$Day)

#Plot means
#Trim data to dates of tea burial
plot_temp_means=NULL
for(i in unique(tea[tea$Contributor=="Jonas Lembrechts",]$Plot)){
  DOB<-min(tea[tea$Plot==i,]$Burial) #Find date of burial
  DOR<-max(tea[tea$Plot==i,]$Recovery) #Find date of recovery
  daily_temp_cut<-subset(daily_temp,Day>=DOB&Day<=DOR) #Trim data
  j=head(strsplit(i,"_")[[1]],1)
  
  Out<- daily_temp_cut %>%
    filter(Plot==j) %>%
    group_by(Plot) %>%
    summarise(Mean_Temperature = mean(Daily_mean),
              sd_Temperature = sd(Daily_mean),
              GSL_0 = length(which(Daily_mean > 0)),
              GSL_5 = length(which(Daily_mean > 5)),
              GDD_0 = sum(Daily_mean[Daily_mean>0]),
              GDD_5 = sum(Daily_mean[Daily_mean>5]))
  
  tryCatch(Out$Plot<-i, error=function(e) NULL)
  plot_temp_means<-rbind(plot_temp_means,Out)
}

plot_temp_means$source<-"ibutton data"
plot_temp_means$scale<-"plot"

Plot_Variables_Soil<-rbind(Plot_Variables_Soil,plot_temp_means)

#Abisko - Moisture####
temp<-read.csv(file="scripts/users/hthomas/Data/Tea/Env.Vars/Abisko_Moisture/Soil moisture_Abisko.csv")
#Add plot and date
temp$Plot<-paste(substr(temp$Site.name,1,4),"_",str_sub(temp$Site.name, start= 5),sep="")
temp$date<-as.POSIXct(paste(temp$year,"/",temp$month,"/",temp$day,sep=""),format="%Y/%m/%d")

#Plot data
ggplot(temp,aes(date,Humidity,colour=factor(Plot)))+
  geom_point(size=0.1)+
  geom_smooth()+
  theme_bw()+
  theme(legend.position="none")

plot_temp_means=NULL
for(i in unique(tea[tea$Contributor=="Jonas Lembrechts",]$Plot)){
  DOB<-min(tea[tea$Plot==i,]$Burial) #Find date of burial
  DOR<-max(tea[tea$Plot==i,]$Recovery) #Find date of recovery
  daily_temp_cut<-subset(daily_temp,Day>=DOB&Day<=DOR) #Trim data
  
  Out<- temp %>%
    filter(Plot==i) %>%
    group_by(Plot) %>%
    summarise(Mean_moisture = mean(Humidity,na.rm=T),
              sd_moisture = sd(Humidity,na.rm=T))
  
  tryCatch(Out$Plot<-i, error=function(e) NULL)
  plot_temp_means<-rbind(plot_temp_means,Out)
}

plot_temp_means$source<-"Moisture probe"
plot_temp_means$scale<-"plot"

Plot_Variables_Moisture<-rbind(Plot_Variables_Moisture,plot_temp_means)

#Abisko-L1000####
temp<-read.csv(file="scripts/users/hthomas/Data/Tea/Env.Vars/Abisko_Moisture/Soil moisture_L1000.csv")
#Create plot mean
plot_temp_means <- temp %>%
  select(Plot,Humidity,X28.08.2015,X10.08.2015,X22.07.2015,X16.07.2015) %>%
  melt(id.vars = c("Plot")) %>%
  group_by(Plot) %>%
  summarise(Mean_moisture = mean(value,na.rm=T),
            sd_moisture = sd(value,na.rm=T)) %>%
  mutate(source = "Moisture Probe", scale = "Plot")

Plot_Variables_Moisture<-rbind(Plot_Variables_Moisture,plot_temp_means)

#Norway - Moisture####
temp<-read.csv(file="scripts/users/hthomas/Data/Tea/Env.Vars/Abisko_Moisture/Soil moisture_Norway_2016.csv")

temp$Plot<-paste0(temp$Plot,"_Year")
#Create plot mean
plot_temp_means <- temp %>%
  select(Plot,Humidity_2015,Humidity_2016) %>%
  melt(id.vars = c("Plot")) %>%
  group_by(Plot) %>%
  summarise(Mean_moisture = mean(value,na.rm=T),
            sd_moisture = sd(value,na.rm=T)) %>%
  mutate(source = "Moisture Probe", scale = "Plot")

Plot_Variables_Moisture<-rbind(Plot_Variables_Moisture,plot_temp_means)

#Svalbard - Moisture####

temp<-read.csv(file="scripts/users/hthomas/Data/Tea/Env.Vars/Svalbard_other/Moisture_2016.csv")
#Add plot and date
temp$Plot<-paste("Svalbard",temp$Site,temp$Community,temp$Treatment,sep="_")
temp$Date<-as.POSIXct(temp$Date,format="%d/%m/%Y")
temp$Date<-temp$Date + years(2000) #correct years



#Extract useful columns and tranform to long form
plot_moisture_means<-temp %>%
  select(Plot,Date,Soil1,Soil2,Soil3,Soil4,Soil5) %>%
  melt(id.vars = c("Plot", "Date")) %>%
  select(Plot,Date,Moisture = value) %>%
  filter(!is.na(Date)) %>%
  mutate(Moisture = as.numeric(as.character(Moisture))) %>%
  group_by(Plot) %>%
  summarise(Mean_moisture = mean(Moisture),
            sd_moisture = sd(Moisture))

plot_moisture_means$source<-"Moisture probe"
plot_moisture_means$scale<-"plot"

#Find missing plots
missing<-unique(subset(tea[tea$Contributor=="Matteo Bon Petit",],Plot%notin%plot_moisture_means$Plot)$Plot)
plot_moisture_means_missing<-plot_moisture_means[c(1:length(missing)),]
plot_moisture_means_missing[,c(2:5)]<-NA
plot_moisture_means_missing$Plot<-as.character(missing)

#Add higher level plot
plot_moisture_means_missing$Site<-substr(plot_moisture_means_missing$Plot,1,13)
plot_moisture_means$Site<-substr(plot_moisture_means$Plot,1,13)

#Moisture
PMM_averages<-plot_moisture_means %>%
  group_by(Site) %>%
  summarise(Mean_moisture = mean(Mean_moisture),
            sd_moisture = mean(sd_moisture))

#Add to missing plots
plot_moisture_means_missing$Mean_moisture<-PMM_averages$Mean_moisture[match(plot_moisture_means_missing$Site,PMM_averages$Site)]
plot_moisture_means_missing$sd_moisture<-PMM_averages$sd_moisture[match(plot_moisture_means_missing$Site,PMM_averages$Site)]
plot_moisture_means_missing$source<-"estimated from surrounding plots"
plot_moisture_means_missing$scale<-"Site"

plot_moisture_means<-rbind(plot_moisture_means[,-6],plot_moisture_means_missing[,-6])

Svalbard_sites<-plot_moisture_means

Plot_Variables_Moisture<-rbind(Plot_Variables_Moisture,plot_moisture_means)

#Svalbard - Soil Temperature####

#Read Data
filenames <- list.files(path="scripts/users/hthomas/Data/Tea/Env.Vars/Svalbard_Temp") # create list of temperature data files

tempdata <- lapply(filenames, function(x) read.csv(paste("scripts/users/hthomas/Data/Tea/Env.Vars/Svalbard_Temp/",x,sep=""), sep=",", header=F, skip=15)) # read in all data files into large list
for(i in 1:length(filenames)) {
  #colnames(tempdata[[i]]) <- tempdata[[i]][-1,]; # put first line as header
  tempdata[[i]] <- tempdata[[i]][,-2]; # delete dispensable second column 
  tempdata[[i]][,2] <- as.numeric(as.character(tempdata[[i]][,2]))
  tempdata[[i]][,3] <- as.numeric(as.character(tempdata[[i]][,3]))
  names(tempdata[[i]])[c(1:3)]<-c("Date/Time","Value","Decimal")
  tempdata[[i]][is.na(tempdata[[i]]$Decimal),3] <- 0
  tempdata[[i]][,1] <- as.POSIXct(tempdata[[i]][,1],format="%d.%m.%Y %H:%M") # convert date column into date format
  #days <- tempdata[[i]][,1]
  #tempdata[[i]] <- aggregate(tempdata[[i]][,2], by = list(days), mean); # compute mean temperatures per day
  
  fileinfo <- strsplit(file_path_sans_ext(filenames[i]),"_")[[1]]; # extract plot infos from file name
  
  for (j in 1:4) {
    tempdata[[i]][,3+j] <- fileinfo[j]
  } # add columns for plot infos
  colnames(tempdata[[i]])[c(1:7)] <- c("date","value","decimal","site","community","treatment","location")
}

#Raw Data
temp <- do.call("rbind", tempdata)
temp$date<-temp$date + years(2000) #correct years
temp$temp<-paste(temp$value,temp$decimal,sep=".")
temp$temp<-as.numeric(as.character(temp$temp))
temp$Day<-cut(temp$date, "day") #Extract days

temp$Plot<-paste("Svalbard",temp$site,temp$community,temp$treatment,sep="_") #Add tea plot name

# ggplot(temp,aes(date,temp,colour=factor(Plot)))+
#   geom_point(size=0.1)+
#   geom_smooth()+
#   theme_bw()

#Daily means
daily_temp<- temp %>%
  group_by(Day,site,Plot,location) %>%
  summarise(Daily_mean = mean(temp))

daily_temp$Day<- as.Date(daily_temp$Day)


#Plot means
#Trim data to dates of tea burial
plot_temp_means=NULL
for(i in unique(daily_temp$Plot)){
  DOB<-min(tea[tea$Plot==i,]$Burial) #Find date of burial
  DOR<-max(tea[tea$Plot==i,]$Recovery) #Find date of recovery
  daily_temp_cut<-subset(daily_temp,Day>=DOB&Day<=DOR) #Trim data
  
  Out<- daily_temp_cut %>%
    filter(Plot==i) %>%
    group_by(Plot,location) %>%
    summarise(Mean_Temperature = mean(Daily_mean),
              sd_Temperature = sd(Daily_mean),
              GSL_0 = length(which(Daily_mean > 0)),
              GSL_5 = length(which(Daily_mean > 5)),
              GDD_0 = sum(Daily_mean[Daily_mean>0]),
              GDD_5 = sum(Daily_mean[Daily_mean>5]))
  
  plot_temp_means<-rbind(plot_temp_means,Out)
}

#Pull out deep soil temepratures
plot_soiltemp_means<-as.data.frame(plot_temp_means[plot_temp_means$location=="D",-2])
#Label
plot_soiltemp_means$source<-"ibutton data (6cm depth)"
plot_soiltemp_means$scale<-"plot"
#Pull out shallow
plot_soil_shallow_means<-NULL
plot_soil_shallow_means<-as.data.frame(plot_temp_means[plot_temp_means$location=="S",-2])
plot_soil_shallow_means<-subset(plot_soil_shallow_means,Plot%notin%plot_soiltemp_means$Plot)
#Label
plot_soil_shallow_means$source<-"ibutton data (2cm depth)"
plot_soil_shallow_means$scale<-"plot"

#Combine
plot_soiltemp_means<-rbind(plot_soiltemp_means,plot_soil_shallow_means)

#Identify  missing sites
#Find missing plots
missing<-unique(subset(tea[tea$Contributor=="Matteo Bon Petit",],Plot%notin%plot_soiltemp_means$Plot)$Plot)
plot_soiltemp_means_missing<-plot_soiltemp_means[c(1:length(missing)),]
plot_soiltemp_means_missing[,c(2:5)]<-NA
plot_soiltemp_means_missing$Plot<-as.character(missing)

#Add higher level plot
plot_soiltemp_means_missing$Site<-substr(plot_soiltemp_means_missing$Plot,1,13)
plot_soiltemp_means$Site<-substr(plot_soiltemp_means$Plot,1,13)

#Soil
PTM_averages<-plot_soiltemp_means %>%
  group_by(Site) %>%
  summarise(Mean_Temperature = mean(Mean_Temperature),
            sd_Temperature = mean(sd_Temperature),
            GSL_0 = mean(GSL_0),
            GSL_5 = mean(GSL_5),
            GDD_0 = mean(GDD_0),
            GDD_5 = mean(GDD_5))

#Add to missing plots
plot_soiltemp_means_missing$Mean_Temperature<-PTM_averages$Mean_Temperature[match(plot_soiltemp_means_missing$Site,PTM_averages$Site)]
plot_soiltemp_means_missing$sd_Temperature<-PTM_averages$sd_Temperature[match(plot_soiltemp_means_missing$Site,PTM_averages$Site)]
plot_soiltemp_means_missing$GSL_0<-PTM_averages$GSL_0[match(plot_soiltemp_means_missing$Site,PTM_averages$Site)]
plot_soiltemp_means_missing$GSL_5<-PTM_averages$GSL_5[match(plot_soiltemp_means_missing$Site,PTM_averages$Site)]
plot_soiltemp_means_missing$GDD_0<-PTM_averages$GDD_0[match(plot_soiltemp_means_missing$Site,PTM_averages$Site)]
plot_soiltemp_means_missing$GDD_5<-PTM_averages$GDD_5[match(plot_soiltemp_means_missing$Site,PTM_averages$Site)]
plot_soiltemp_means_missing$source<-"estimated from surrounding plots"
plot_soiltemp_means_missing$scale<-"Site"

plot_soiltemp_means<-rbind(plot_soiltemp_means[,-10],plot_soiltemp_means_missing[,-10])

Plot_Variables_Soil<-rbind(Plot_Variables_Soil,plot_soiltemp_means)

#Svalbard - Air Temp####

temp<-read.csv(file="scripts/users/hthomas/Data/Tea/Env.Vars/Svalbard_other/Weather_station.csv",header=F,skip=1)

temp$Date<-paste(temp[,2],temp[,3])
temp$Date<-as.POSIXct(temp$Date,format="%m.%d.%Y %H:%M")
temp$Date<-temp$Date + years(2000) #correct years
temp<-temp[-1,c(9,7)]
names(temp)<-c("Date","Air_Temp")
temp$Air_Temp<-as.numeric(as.character(temp$Air_Temp))
temp$Day<-cut(temp$Date, "day") #Extract days

# ggplot(temp,aes(Date,Air_Temp))+
#   geom_point(size=0.1)+
#   geom_smooth()+
#   theme_bw()

Svalbard_temp<-temp

#Daily means
daily_temp<- temp %>%
  group_by(Day) %>%
  summarise(Daily_mean = mean(Air_Temp))

daily_temp$Day<- as.Date(daily_temp$Day)

# ggplot(daily_temp,aes(Day,Daily_mean))+
#   geom_point(size=0.4)+
#   stat_smooth()+
#   theme_bw()

#Plot means
#Trim data to dates of tea burial
plot_airtemp_means=NULL
plot_airtemp=NULL
for(i in unique(Svalbard_sites$Plot)){
  DOB<-min(tea[tea$Plot==i,]$Burial) #Find date of burial
  DOR<-max(tea[tea$Plot==i,]$Recovery) #Find date of recovery
  daily_temp_cut<-subset(daily_temp,Day>=DOB&Day<=DOR) #Trim data
  
  Out<- daily_temp_cut %>%
    summarise(Mean_Temperature = mean(Daily_mean),
              sd_Temperature = sd(Daily_mean),
              GSL_0 = length(which(Daily_mean > 0)),
              GSL_5 = length(which(Daily_mean > 5)),
              GDD_0 = sum(Daily_mean[Daily_mean>0]),
              GDD_5 = sum(Daily_mean[Daily_mean>5]))
  
  plot_airtemp<-cbind(i,Out)
  plot_airtemp_means<-rbind(plot_airtemp_means,plot_airtemp)
}

plot_airtemp_means$source<-"Weather Station"
plot_airtemp_means$scale<-"Site"
names(plot_airtemp_means)[1]<-"Plot"

Plot_Variables_Air<-rbind(Plot_Variables_Air,plot_airtemp_means)

#Fairbanks####

#
# Define new functions to get dates and times separately
my.split1 <- function(x){strsplit(x," ")[[1]][1]}	# date extraction function
my.split2 <- function(x){strsplit(x," ")[[1]][2]}	# time extraction function
#
## Loop over all Logger-Files to plot them
filenames <- list.files("scripts/users/hthomas/Data/Tea/Env.Vars/Fairbanks/")
sites <- c("Cold-Forest","Cold-Treeline", "Cold-BeyondTreeline", "Core", "Dry-Forest", "Dry-Transition", "DryBluff")
# read all files:

d <- list()
for (i in 1:length(filenames))
{
  d[[i]] <- read.csv(paste("scripts/users/hthomas/Data/Tea/Env.Vars/Fairbanks/",filenames[i],sep=""), sep=",", dec=".", stringsAsFactor=FALSE)
  d[[i]] <- d[[i]][,2:3]
  colnames(d[[i]]) <- c("Time", "Temperature")
  
  fileinfo <- strsplit(filenames[i],"_")[[1]]; # extract plot infos from file name
  
  for (j in 1:5) {
    d[[i]][,2+j] <- fileinfo[j]
  }
  colnames(d[[i]])[c(1:7)] <- c("Time", "Temperature","Site","Name1","Name2","location","enddate")
  
}

temp <- do.call("rbind", d)
temp$Time<-as.POSIXct(temp$Time,format="%Y-%m-%d %H:%M")
temp$Day<-cut(temp$Time, "day") #Extract days

#Daily means
daily_temp<- temp %>%
  group_by(Day,Site,location) %>%
  summarise(Daily_mean = mean(Temperature))
daily_temp$Day<- as.Date(daily_temp$Day)

# ggplot(daily_temp,aes(Day,Daily_mean,colour=factor(Site)))+
#   geom_point(size=0.4)+
#   geom_smooth()+
#   theme_bw()

#Plot means
#1 year
#Trim data to dates of tea burial
plot_temp_means=NULL
for(i in unique(daily_temp$Site)){
  j=paste(i,"1y",sep="_")
  DOB<-min(tea[tea$Plot==j,]$Burial) #Find date of burial
  DOR<-max(tea[tea$Plot==j,]$Recovery) #Find date of recovery
  daily_temp_cut<-subset(daily_temp,Day>=DOB&Day<=DOR) #Trim data
  
  Out<- daily_temp_cut %>%
    filter(Site==i) %>%
    group_by(Site,location) %>%
    summarise(Mean_Temperature = mean(Daily_mean),
              sd_Temperature = sd(Daily_mean),
              GSL_0 = length(which(Daily_mean > 0)),
              GSL_5 = length(which(Daily_mean > 5)),
              GDD_0 = sum(Daily_mean[Daily_mean>0]),
              GDD_5 = sum(Daily_mean[Daily_mean>5]))
  
  Out$Plot<-j
  plot_temp_means<-rbind(plot_temp_means,Out)
}

one_year<-plot_temp_means

#3 months
plot_temp_means=NULL
for(i in unique(daily_temp$Site)){
  j=paste(i,"3m",sep="_")
  DOB<-min(tea[tea$Plot==j,]$Burial) #Find date of burial
  DOR<-max(tea[tea$Plot==j,]$Recovery) #Find date of recovery
  daily_temp_cut<-subset(daily_temp,Day>=DOB&Day<=DOR) #Trim data
  
  Out<- daily_temp_cut %>%
    filter(Site==i) %>%
    group_by(Site,location) %>%
    summarise(Mean_Temperature = mean(Daily_mean),
              sd_Temperature = sd(Daily_mean),
              GSL_0 = length(which(Daily_mean > 0)),
              GSL_5 = length(which(Daily_mean > 5)),
              GDD_0 = sum(Daily_mean[Daily_mean>0]),
              GDD_5 = sum(Daily_mean[Daily_mean>5]))
  
  Out$Plot<-j
  plot_temp_means<-rbind(plot_temp_means,Out)
}

three_months<-plot_temp_means

plot_temp_means<-rbind(one_year,three_months)[,c(9,2:8)]

plot_soiltemp_means<-subset(plot_temp_means,location=="-0.2m"|location=="0m")
plot_soiltemp_means<-plot_soiltemp_means %>% group_by(Plot) %>%
  summarise(Mean_Temperature = mean(Mean_Temperature),
            sd_Temperature = max(sd_Temperature),
            GSL_0 = mean(GSL_0),
            GSL_5 = mean(GSL_5),
            GDD_0 = mean(GDD_0),
            GDD_5 = mean(GDD_5))

plot_soiltemp_means$source<-"ibutton data (mean of 20cm deep and surface(0cm))"
plot_soiltemp_means$scale<-"plot"

Plot_Variables_Soil<-rbind(Plot_Variables_Soil,plot_soiltemp_means)

plot_airtemp_means<-subset(plot_temp_means,location=="2m"|location=="0.5m")
plot_airtemp_means<-plot_airtemp_means %>% group_by(Plot) %>%
  summarise(Mean_Temperature = mean(Mean_Temperature),
            sd_Temperature = max(sd_Temperature),
            GSL_0 = mean(GSL_0),
            GSL_5 = mean(GSL_5),
            GDD_0 = mean(GDD_0),
            GDD_5 = mean(GDD_5))
plot_airtemp_means$source<-"ibutton data (2m)"
plot_airtemp_means$scale<-"plot"

Plot_Variables_Air<-rbind(Plot_Variables_Air,plot_airtemp_means)

#Pentlands####
temp<-read.csv(file="scripts/users/hthomas/Data/Tea/Env.Vars/Pentlands/Pentlands.temp.csv")

#Add plot and date
temp$Plot<-"PL_HT"
temp$date<-as.POSIXct(temp$GMT,format="%d/%m/%Y")
temp$date<-temp$date + years(2000) #correct years
temp$Day<-cut(temp$date, "day") #Extract days

# #Plot data
# ggplot(temp,aes(date,Mean.TemperatureC))+
#   geom_point(size=0.1)+
#   geom_smooth()+
#   theme_bw()+
#   theme(legend.position="none")

#Daily means
daily_temp<- temp %>%
  group_by(Day,Plot) %>%
  summarise(Daily_mean = mean(Mean.TemperatureC))

daily_temp$Day<- as.Date(daily_temp$Day)

#Plot means
#Trim data to dates of tea burial
plot_temp_means=NULL
for(i in unique(daily_temp$Plot)){
  DOB<-min(tea[tea$Plot==i,]$Burial) #Find date of burial
  DOR<-max(tea[tea$Plot==i,]$Recovery) #Find date of recovery
  daily_temp_cut<-subset(daily_temp,Day>=DOB&Day<=DOR) #Trim data
  
  Out<- daily_temp_cut %>%
    group_by(Plot) %>%
    summarise(Mean_Temperature = mean(Daily_mean),
              sd_Temperature = sd(Daily_mean),
              GSL_0 = length(which(Daily_mean > 0)),
              GSL_5 = length(which(Daily_mean > 5)),
              GDD_0 = sum(Daily_mean[Daily_mean>0]),
              GDD_5 = sum(Daily_mean[Daily_mean>5]))
  
  plot_temp_means<-rbind(plot_temp_means,Out)
}

plot_temp_means$source<-"Online weather station"
plot_temp_means$scale<-"Region"

Plot_Variables_Air<-rbind(Plot_Variables_Air,plot_temp_means)

plot_soiltemp_means<-plot_temp_means
plot_soiltemp_means[,c(2:6)]<-NA

Plot_Variables_Soil<-rbind(Plot_Variables_Soil,plot_soiltemp_means)

#Gavia####
temp<-read.csv(file="scripts/users/hthomas/Data/Tea/Env.Vars/Gavia/gavia_ita_env_data_2016&2017_v2.csv",header=TRUE)

#Add plot and date
temp$Plot<-paste("GV",substr(temp$community,1,1),substr(temp$treatment,1,1),sep="_")

temp[temp$Incubation=="Winter",]$Plot<-paste0(temp[temp$Incubation=="Winter",]$Plot,"_Winter")
temp[temp$Incubation=="year",]$Plot<-paste0(temp[temp$Incubation=="year",]$Plot,"_Year")

temp<-temp[-15,]

#Soil temps
plot_temp_means<-select(temp,Plot,Mean_Temperature = soil_temp)
plot_temp_means$sd_Temperature<-NA
plot_temp_means$GSL_0<-NA
plot_temp_means$GSL_5<-NA
plot_temp_means$GDD_0<-NA
plot_temp_means$GDD_5<-NA

plot_temp_means$source<-"ibutton (site summaries)"
plot_temp_means$scale<-"Plot"

Plot_Variables_Soil<-rbind(Plot_Variables_Soil,plot_temp_means)

plot_airtemp_means<-plot_temp_means
plot_airtemp_means[,c(2:6)]<-NA

Plot_Variables_Air<-rbind(Plot_Variables_Air,plot_airtemp_means)

#Soil Moisture
plot_temp_means<-select(temp,Plot,Mean_moisture = Soil_moist)
plot_temp_means$sd_moisture<-NA

plot_temp_means$source<-"moisture probe (summary)"
plot_temp_means$scale<-"Plot"

Plot_Variables_Moisture<-rbind(Plot_Variables_Moisture,plot_temp_means)

#Quebec####

#Read Data
temp<-read.csv(file="scripts/users/hthomas/Data/Tea/Env.Vars/Quebec/Quebec.csv")
#Raw Data

temp$date<-as.POSIXct(temp$Date.Time,format="%d/%m/%Y %H:%M")
temp$date<-temp$date + years(2000) #correct years
temp$Temp<-as.numeric(as.character(temp$Temp))
temp$Day<-cut(temp$date, "day") #Extract days

# #Plot data
# ggplot(temp,aes(date,Temp))+
#   geom_point(size=0.1)+
#   geom_smooth()+
#   theme_bw()

#Daily means
daily_temp<- temp %>%
  group_by(Day) %>%
  summarise(Daily_mean = mean(Temp,na.rm=TRUE))

daily_temp$Day<- as.Date(daily_temp$Day)

# ggplot(daily_temp,aes(Day,Daily_mean))+
#   geom_point(size=0.4)+
#   geom_smooth()+
#   theme_bw()

#Plot means
#Trim data to dates of tea burial
plot_temp_means=NULL
for(i in unique(tea[tea$Contributor=="Sandra Angers-Blondin, Stephane Boudreau",]$Plot)){
  DOB<-min(tea[tea$Plot==i,]$Burial) #Find date of burial
  DOR<-max(tea[tea$Plot==i,]$Recovery) #Find date of recovery
  daily_temp_cut<-subset(daily_temp,Day>=DOB&Day<=DOR) #Trim data
  
  Out<- daily_temp_cut %>%
    summarise(Mean_Temperature = mean(Daily_mean,na.rm=TRUE),
              sd_Temperature = sd(Daily_mean,na.rm=TRUE),
              GSL_0 = length(which(Daily_mean > 0)),
              GSL_5 = length(which(Daily_mean > 5)),
              GDD_0 = sum(Daily_mean[Daily_mean>0],na.rm=TRUE),
              GDD_5 = sum(Daily_mean[Daily_mean>5],na.rm=TRUE))
  
  Out$Plot<-i
  plot_temp_means<-rbind(plot_temp_means,Out)
}

plot_temp_means$source<-"Weather station (airport)"
plot_temp_means$scale<-"site"
plot_temp_means<-plot_temp_means[,c(7,1:6,8:9)]

Plot_Variables_Air<-rbind(Plot_Variables_Air,plot_temp_means)

plot_soiltemp_means<-plot_temp_means
plot_soiltemp_means[,c(2:6)]<-NA

Plot_Variables_Soil<-rbind(Plot_Variables_Soil,plot_soiltemp_means)

#Lofoten####
temp_w<-read.csv(file="scripts/users/hthomas/Data/Tea/Env.Vars/Lofoten/Leknes_W.csv")
temp_w$station<-"W"
temp_l<-read.csv(file="scripts/users/hthomas/Data/Tea/Env.Vars/Lofoten/Leknes_L.csv")
temp_l$station<-"L"
temp<-rbind(temp_w,temp_l[-1,])

#Add plot and date
temp$date<-as.POSIXct(temp$CEST,format="%d/%m/%Y")
temp$date<-temp$date + years(2000) #correct years

# #Plot data
# ggplot(temp,aes(date,Mean.TemperatureC,colour=station))+
#   geom_point(size=0.1)+
#   geom_smooth()+
#   theme_bw()

#Daily means
daily_temp<- temp %>%
  group_by(date) %>%
  summarise(Daily_mean = mean(Mean.TemperatureC))

daily_temp$date<- as.Date(daily_temp$date)

#Plot means
#Trim data to dates of tea burial
plot_temp_means=NULL
for(i in unique(tea[tea$Region=="Lofoten Island",]$Plot)){
  DOB<-min(tea[tea$Plot==i,]$Burial) #Find date of burial
  DOR<-max(tea[tea$Plot==i,]$Recovery) #Find date of recovery
  daily_temp_cut<-subset(daily_temp,date>=DOB&date<=DOR) #Trim data
  
  Out<- daily_temp_cut %>%
    summarise(Mean_Temperature = mean(Daily_mean),
              sd_Temperature = sd(Daily_mean),
              GSL_0 = length(which(Daily_mean > 0)),
              GSL_5 = length(which(Daily_mean > 5)),
              GDD_0 = sum(Daily_mean[Daily_mean>0]),
              GDD_5 = sum(Daily_mean[Daily_mean>5]))
  
  Out$Plot<-i
  plot_temp_means<-rbind(plot_temp_means,Out)
}

plot_temp_means$source<-"Online weather station"
plot_temp_means$scale<-"Region"
plot_temp_means<-plot_temp_means[,c(7,1:6,8:9)]

Plot_Variables_Air<-rbind(Plot_Variables_Air,plot_temp_means)

plot_soiltemp_means<-plot_temp_means
plot_soiltemp_means[,c(2:6)]<-NA

Plot_Variables_Soil<-rbind(Plot_Variables_Soil,plot_soiltemp_means)

#Greenland (Toke - Moisture & Soil)####
temp<-read.csv(file="scripts/users/hthomas/Data/Tea/Env.Vars/Greenland_Toke/Soil_moisture.csv")

#Add plot and date
temp$date<-as.POSIXct(paste(temp$Day,temp$Month,temp$Year,sep="/"),format="%d/%m/%Y")
temp$Plot<-ifelse(temp$Site=="N",paste("Narsarsuaq",paste(temp$Elevation,temp$Habitat,temp$Plot,sep=""),temp$Treatment,sep="_"),paste("Qaqortoq",paste("Q",temp$Elevation,temp$Habitat,temp$Plot,sep=""),temp$Treatment,sep="_"))

#Add season to plot
temp_summer<-temp
temp_summer$Plot <- paste0(temp_summer$Plot,"_Summer")

temp_year<-temp
temp_year$Plot <- paste0(temp_year$Plot,"_Year")

temp_winter<-temp
temp_winter$Plot <- paste0(temp_winter$Plot,"_Winter")

temp<-rbind(temp_summer,temp_winter,temp_year)

#Plot data
# ggplot(temp,aes(date,Soilmoist,colour=Plot))+
#   geom_point(size=0.1)+
#   geom_smooth()+
#   theme_bw()

#Daily means
daily_temp<- temp %>%
  group_by(Plot,date) %>%
  summarise(Daily_mean = mean(Soilmoist,na.rm=TRUE),
            Daily_mean_temp = mean(Soiltemp,na.rm=TRUE))

daily_temp$date<- as.Date(daily_temp$date)

#Plot means (Moisture)
#Trim data to dates of tea burial
plot_temp_means=NULL

#Select plots
plots<-subset(daily_temp,Plot%in%tea[tea$Contributor=="Toke Thomas Hoye"&tea$Site!="Greenland",]$Plot)$Plot
for(i in unique(plots)){
  DOB<-min(tea[tea$Plot==i,]$Burial, na.rm = T) #Find date of burial
  DOR<-max(tea[tea$Plot==i,]$Recovery, na.rm = T) #Find date of recovery
  daily_temp_cut<-subset(daily_temp,date>=DOB&date<=DOR) #Trim data
  
  Out<- daily_temp_cut %>%
    filter(Plot==i) %>%
    summarise(Mean_moisture = mean(Daily_mean),
              sd_moisture = sd(Daily_mean))
  
  Out$Plot<-i
  plot_temp_means<-rbind(plot_temp_means,Out)
}

#Soil Moisture
plot_temp_means$source<-"moisture probe"
plot_temp_means$scale<-"Plot"

Plot_Variables_Moisture<-rbind(Plot_Variables_Moisture,plot_temp_means)

#Add missing sites
missing<-unique(subset(tea[tea$Contributor=="Toke Thomas Hoye"&tea$Site!="Greenland",],Plot%notin%Plot_Variables_Moisture$Plot)$Plot)
plot_temp_means<-plot_temp_means[c(1:length(missing)),]
plot_temp_means[,c(2:5)]<-NA
plot_temp_means$Plot<-missing
Plot_Variables_Moisture<-rbind(Plot_Variables_Moisture,plot_temp_means)


#Plot means (Soil temp)
#Trim data to dates of tea burial
plot_temp_means=NULL

#Select plots
for(i in unique(plots)){
  DOB<-min(tea[tea$Plot==i,]$Burial, na.rm = T) #Find date of burial
  DOR<-max(tea[tea$Plot==i,]$Recovery, na.rm = T) #Find date of recovery
  daily_temp_cut<-subset(daily_temp,date>=DOB&date<=DOR) #Trim data
  
  Out<- daily_temp_cut %>%
    filter(Plot==i) %>%
    summarise(Mean_Temperature = mean(Daily_mean_temp,na.rm=T),
              sd_Temperature = sd(Daily_mean_temp,na.rm=T),
              GSL_0 = length(which(Daily_mean_temp > 0)),
              GSL_5 = length(which(Daily_mean_temp > 5)),
              GDD_0 = sum(Daily_mean[Daily_mean_temp>0],na.rm=T),
              GDD_5 = sum(Daily_mean[Daily_mean_temp>5],na.rm=T))
  
  Out$Plot<-i
  plot_temp_means<-rbind(plot_temp_means,Out)
}

plot_temp_means$Mean_Temperature<-NA

#Soil Temp
plot_temp_means$source<-"Point measurement (one)"
plot_temp_means$scale<-"Plot"



Plot_Variables_Soil<-rbind(Plot_Variables_Soil,plot_temp_means)

#Add missing sites
missing<-unique(subset(tea[tea$Contributor=="Toke Thomas Hoye"&tea$Site!="Greenland",],Plot%notin%Plot_Variables_Soil$Plot)$Plot)
plot_temp_means<-plot_temp_means[c(1:length(missing)),]
plot_temp_means[,c(2:9)]<-NA
plot_temp_means$Plot<-missing
Plot_Variables_Soil<-rbind(Plot_Variables_Soil,plot_temp_means)

#Greenland (Toke - Air)####
temp_N<-read.csv(file="scripts/users/hthomas/Data/Tea/Env.Vars/Greenland_Toke/N.csv")
temp_N$Site<-"Narsarsuaq"
temp_Q<-read.csv(file="scripts/users/hthomas/Data/Tea/Env.Vars/Greenland_Toke/Q.csv")
temp_Q$Site<-"Qaqortoq"
temp<-rbind(temp_N,temp_Q[-1,])

#Add plot and date
temp$date<-as.POSIXct(temp$WGST,format="%d/%m/%Y")
temp$date<-temp$date + years(2000) #correct years

# #Plot data
# ggplot(temp,aes(date,Mean.TemperatureC,colour=Site))+
#   geom_point(size=0.1)+
#   geom_smooth()+
#   theme_bw()

#Daily means
daily_temp<- temp %>%
  group_by(Site,date) %>%
  summarise(Daily_mean = mean(Mean.TemperatureC))

daily_temp$date<- as.Date(daily_temp$date)

#Plot means
#Trim data to dates of tea burial
plot_temp_means=NULL
for(i in unique(tea[tea$Contributor=="Toke Thomas Hoye"&tea$Site!="Greenland"&!is.na(tea$Burial),]$Plot)){
  DOB<-min(tea[tea$Plot==i,]$Burial, na.rm = T) #Find date of burial
  DOR<-max(tea[tea$Plot==i,]$Recovery, na.rm = T) #Find date of recovery
  daily_temp_cut<-subset(daily_temp,date>=DOB&date<=DOR) #Trim data
  
  j<-ifelse(substr(i,1,1)=="N","Narsarsuaq","Qaqortoq")
  
  Out<- daily_temp_cut %>%
    filter(Site==j) %>%
    summarise(Mean_Temperature = mean(Daily_mean),
              sd_Temperature = sd(Daily_mean),
              GSL_0 = length(which(Daily_mean > 0)),
              GSL_5 = length(which(Daily_mean > 5)),
              GDD_0 = sum(Daily_mean[Daily_mean>0]),
              GDD_5 = sum(Daily_mean[Daily_mean>5]))
  
  Out$Site<-i
  names(Out)[1]<-"Plot"
  plot_temp_means<-rbind(plot_temp_means,Out)
}

plot_temp_means$source<-"Online weather station"
plot_temp_means$scale<-"Region"

Plot_Variables_Air<-rbind(Plot_Variables_Air,plot_temp_means)

#Iceland####
temp_e<-read.csv(file="scripts/users/hthomas/Data/Tea/Env.Vars/Iceland/E.csv")
temp_e$station<-"E"
temp_N<-read.csv(file="scripts/users/hthomas/Data/Tea/Env.Vars/Iceland/N.csv")
temp_N$station<-"N"
temp_w<-read.csv(file="scripts/users/hthomas/Data/Tea/Env.Vars/Iceland/W.csv")
temp_w$station<-"W"
temp<-rbind(temp_e,temp_N[-1,],temp_w[-1,])
temp$Site<-"Auokiluheioi"
temp_e$Site<-"Theistareykir"
temp<-rbind(temp,temp_e[-1,])
temp_l<-read.csv(file="scripts/users/hthomas/Data/Tea/Env.Vars/Iceland/L.csv")
temp_l$station<-"L"
temp_l$Site<-"Endalen"
names(temp_l)[1]<-"GMT"
temp<-rbind(temp,temp_l[-1,])

#Add plot and date
temp$date<-as.POSIXct(temp$GMT,format="%d/%m/%Y")
temp$date<-temp$date + years(2000) #correct years
temp$Day<-cut(temp$date, "day") #Extract days

temp<-select(temp,Date = date, Air_Temp = Mean.TemperatureC,Day,station,Site)
Svalbard_temp$station <- "Recorded"
Svalbard_temp$Site <- "Endalen"


Svalbard_temp$Day<-as.Date(Svalbard_temp$Day)
temp$Day<-as.Date(temp$Day)

temp<-rbind(temp, Svalbard_temp)

#Plot data
# ggplot(temp,aes(Day,Air_Temp,colour=station))+
#   geom_point(size=0.1)+
#   geom_smooth()+
#   theme_bw()

#Daily means
daily_temp<- temp %>%
  group_by(Site,Day) %>%
  summarise(Daily_mean = mean(Air_Temp))

daily_temp$Day<- as.Date(daily_temp$Day)

#Plot means
#Trim data to dates of tea burial
plot_temp_means=NULL
for(i in unique(tea[tea$Contributor=="Isabel Barrio, Ingibj\x9arg Svala J\x97nsd\x97ttir, Katr\x92n Bj\x9arnsd\x97ttir",]$Plot)){
  DOB<-min(tea[tea$Plot==i,]$Burial) #Find date of burial
  DOR<-max(tea[tea$Plot==i,]$Recovery) #Find date of recovery
  daily_temp_cut<-subset(daily_temp,Day>=DOB&Day<=DOR) #Trim data
  
  j<-ifelse(substr(i,1,1)=="A","Auokiluheioi",ifelse(substr(i,1,1)=="P","Theistareykir","Endalen"))
  
  Out<- daily_temp_cut %>%
    filter(Site==j) %>%
    summarise(Mean_Temperature = mean(Daily_mean,na.rm=T),
              sd_Temperature = sd(Daily_mean,na.rm=T),
              GSL_0 = length(which(Daily_mean > 0)),
              GSL_5 = length(which(Daily_mean > 5)),
              GDD_0 = sum(Daily_mean[Daily_mean>0],na.rm=T),
              GDD_5 = sum(Daily_mean[Daily_mean>5],na.rm=T))
  
  Out$Plot<-i
  plot_temp_means<-rbind(plot_temp_means,Out)
}

plot_temp_means$source<-"Online weather station (regional average) & Local weather station(Endalen)"
plot_temp_means$scale<-"Region"
plot_temp_means<-plot_temp_means[,c(8,2:7,9:10)]

Plot_Variables_Air<-rbind(Plot_Variables_Air,plot_temp_means)

plot_soiltemp_means<-plot_temp_means
plot_soiltemp_means[,c(2:9)]<-NA

Plot_Variables_Soil<-rbind(Plot_Variables_Soil,plot_soiltemp_means)

#Latnja####
temp<-read.csv(file="scripts/users/hthomas/Data/Tea/Env.Vars/Latnja/Temp.station.csv")

#Add plot and date
temp$Date<-as.POSIXct(temp$Date,format="%d/%m/%Y")
temp$Average<-as.numeric(as.character(temp$Average))

#Plot data
# ggplot(temp,aes(Date,Average))+
#   geom_point(size=0.1)+
#   geom_smooth()+
#   theme_bw()

#Daily means
daily_temp<- temp %>%
  group_by(Date) %>%
  summarise(Daily_mean = mean(Average))

daily_temp$Date<- as.Date(daily_temp$Date)

#Plot means
#Trim data to dates of tea burial
plot_temp_means=NULL
for(i in unique(tea[tea$Contributor=="Juha Alatalo",]$Plot)){
  DOB<-min(tea[tea$Plot==i,]$Burial) #Find date of burial
  DOR<-max(tea[tea$Plot==i,]$Recovery) #Find date of recovery
  daily_temp_cut<-subset(daily_temp,Date>=DOB&Date<=DOR) #Trim data
  
  Out<- daily_temp_cut %>%
    summarise(Mean_Temperature = mean(Daily_mean),
              sd_Temperature = sd(Daily_mean),
              GSL_0 = length(which(Daily_mean > 0)),
              GSL_5 = length(which(Daily_mean > 5)),
              GDD_0 = sum(Daily_mean[Daily_mean>0]),
              GDD_5 = sum(Daily_mean[Daily_mean>5]))
  
  Out$Plot<-i
  plot_temp_means<-rbind(plot_temp_means,Out)
}

plot_temp_means$source<-"Online weather station"
plot_temp_means$scale<-"Region"
plot_temp_means<-plot_temp_means[,c(7,1:6,8:9)]

Plot_Variables_Air<-rbind(Plot_Variables_Air,plot_temp_means)

plot_soiltemp_means<-plot_temp_means
plot_soiltemp_means[,c(2:8)]<-NA

Plot_Variables_Soil<-rbind(Plot_Variables_Soil,plot_soiltemp_means)

#Kluane Air temperatures####
temp<-read.csv("scripts/users/hthomas/Data/Tea/Env.Vars/CG/Kluane_-_Common_garden_3Jun2016_dateastext.csv")

temp$date<-as.character(substr(temp$Date.Time..GMT.06.00,1,14))
temp$date<-as.POSIXct(temp$date,format="%m/%d/%Y %H:%M")
temp$date<-temp$date + years(2000) #correct years
temp$Day<-cut(temp$date, "day") #Extract days

temp2<-read.csv("scripts/users/hthomas/Data/Tea/Env.Vars/CG/Kluane_-_Common_garden_3Jun2016_dateasdate.csv")

temp2$date<-as.POSIXct(temp2$Date.Time..GMT.06.00,format="%m/%d/%Y %H:%M")
temp2$date<-temp2$date + years(2000) #correct years
temp2$Day<-cut(temp2$date, "day") #Extract days

temp<-rbind(temp,temp2)
temp$Soil.moisture<-temp$Soil.moisture*100

names(temp)[c(3:6)]<-c("Moisture","Ground","Air","Soil")
temp<-melt(temp[,c(-1,-2)],id.vars = c("date","Day"))
temp$value<-as.numeric(as.character(temp$value))
temp$Plot<-"CG_HT_year"
names(temp)[3]<-"location"
temp$Day<-as.Date(temp$Day)

CG_temp<-temp

KP_temp<-KP_temp[,c(1,8,10,9,6)]
KP_temp$Plot<-"KP_HT_s2"
names(KP_temp)[2]<-"value"

KP_comb<-rbind(temp,KP_temp)
KP_comb<-subset(KP_comb,location=="Air")

# #Plot data
# ggplot(KP_comb,aes(Day,value,colour=factor(Plot)))+
#   geom_point(size=0.1)+
#   geom_smooth()+
#   theme_bw()

#Daily means
daily_temp<- KP_comb %>%
  group_by(Plot,Day,location) %>%
  summarise(Daily_mean = mean(value))

# #Plot data
# ggplot(daily_temp,aes(Day,Daily_mean,colour=Plot))+
#   geom_point(size=0.1)+
#   geom_smooth()+
#   theme_bw()

daily_temp$doy<-as.numeric(strftime(daily_temp$Day, format = "%j"))
names(daily_temp)[1]<-"Site"

plot_temp_means=NULL
for(i in unique(tea[tea$Contributor=="Eleanor Walker, Haydn Thomas"&tea$Site!="Common Garden",]$Plot)){
  DOB<-as.numeric(strftime(min(tea[tea$Plot=="10A",]$Burial),format= "%j")) #Find date of burial
  DOR<-as.numeric(strftime(max(tea[tea$Plot=="10A",]$Recovery),format= "%j")) #Find date of recovery
  daily_temp_cut<-subset(daily_temp,doy>=DOB&doy<=DOR) #Trim data
  
  Out<- daily_temp_cut %>%
    group_by(Site) %>%
    summarise(Mean_Temperature = mean(Daily_mean),
              sd_Temperature = sd(Daily_mean),
              GSL_0 = length(which(Daily_mean > 0)),
              GSL_5 = length(which(Daily_mean > 5)),
              GDD_0 = sum(Daily_mean[Daily_mean>0]),
              GDD_5 = sum(Daily_mean[Daily_mean>5]))
  
  Out$Plot<-i  
  plot_temp_means<-rbind(plot_temp_means,Out)
}

for(i in unique(plot_temp_means$Plot)){
  
  
  #Calc difference between sites
  ele<-unique(as.numeric(as.character(tea[tea$Plot==i,]$Elevation)))
  diffKP<-ele-1734
  diffCG<-ele-794
  diff<-1734-794
  
  multKP<-(ifelse(diffKP>=0,1,(1-(1/diff*diffKP*-1))))
  multCG<-(ifelse(diffKP>=0,0,(1-1/diff*diffCG)))
  
  Mean_Temperature<-unique(plot_temp_means[plot_temp_means$Site=="KP_HT_s2"&plot_temp_means$Plot==i,]$Mean_Temperature*multKP + plot_temp_means[plot_temp_means$Site=="CG_HT_year"&plot_temp_means$Plot==i,]$Mean_Temperature*multCG)
  GDD_0<-unique(plot_temp_means[plot_temp_means$Site=="KP_HT_s2"&plot_temp_means$Plot==i,]$GDD_0*multKP + plot_temp_means[plot_temp_means$Site=="CG_HT_year"&plot_temp_means$Plot==i,]$GDD_0*multCG)
  GDD_5<-unique(plot_temp_means[plot_temp_means$Site=="KP_HT_s2"&plot_temp_means$Plot==i,]$GDD_5*multKP + plot_temp_means[plot_temp_means$Site=="CG_HT_year"&plot_temp_means$Plot==i,]$GDD_5*multCG)
  GSL_0<-unique(plot_temp_means[plot_temp_means$Site=="KP_HT_s2"&plot_temp_means$Plot==i,]$GSL_0*multKP + plot_temp_means[plot_temp_means$Site=="CG_HT_year"&plot_temp_means$Plot==i,]$GSL_0*multCG)
  GSL_5<-unique(plot_temp_means[plot_temp_means$Site=="KP_HT_s2"&plot_temp_means$Plot==i,]$GSL_5*multKP + plot_temp_means[plot_temp_means$Site=="CG_HT_year"&plot_temp_means$Plot==i,]$GSL_5*multCG)
  
  plot_temp_means[plot_temp_means$Plot==i,]$Mean_Temperature<-Mean_Temperature
  plot_temp_means[plot_temp_means$Plot==i,]$GDD_0<-GDD_0
  plot_temp_means[plot_temp_means$Plot==i,]$GDD_5<-GDD_5
  plot_temp_means[plot_temp_means$Plot==i,]$GSL_0<-GSL_0
  plot_temp_means[plot_temp_means$Plot==i,]$GSL_5<-GSL_5
}

plot_temp_means<-subset(plot_temp_means,Site=="KP_HT_s2")[,c(8,2:7)]
plot_temp_means$source<-"ibutton & Hobo data (averaged)"
plot_temp_means$scale<-"site"

Plot_Variables_Air<-rbind(Plot_Variables_Air,plot_temp_means)

#El Gradient####

#Read Data
filenames <- list.files(path="scripts/users/hthomas/Data/Tea/Env.Vars/Kluane_Gradient/") # create list of temperature data files

tempdata <- lapply(filenames, function(x) read.csv(paste("scripts/users/hthomas/Data/Tea/Env.Vars/Kluane_Gradient/",x,sep=""), sep=",", header=F, skip=15)) # read in all data files into large list

for(i in 1:length(filenames)) {
  #colnames(tempdata[[i]]) <- tempdata[[i]][-1,]; # put first line as header
  tempdata[[i]] <- tempdata[[i]][,-2]; # delete dispensable second column 
  tempdata[[i]][,2] <- as.numeric(as.character(tempdata[[i]][,2]))
  names(tempdata[[i]])[c(1:2)]<-c("Date/Time","Value")
  tempdata[[i]][,1] <- as.POSIXct(tempdata[[i]][,1],format="%d/%m/%Y %H:%M") # convert date column into date format
  #days <- tempdata[[i]][,1]
  #tempdata[[i]] <- aggregate(tempdata[[i]][,2], by = list(days), mean); # compute mean temperatures per day
  
  fileinfo <- strsplit(file_path_sans_ext(filenames[i]),"_")[[1]]; # extract plot infos from file name
  
  for (j in 1:3) {
    tempdata[[i]][,2+j] <- fileinfo[j]
  } # add columns for plot infos
  colnames(tempdata[[i]])[c(1:5)] <- c("date","value","name","site","Plot")
}

#Raw Data
temp <- do.call("rbind", tempdata)
temp$date<-temp$date + years(2000) #correct years
temp$Day<-cut(temp$date, "day") #Extract days
temp$gradient_site<-substr(temp$Plot, 1, nchar(temp$Plot)-1)

#Plot data
# ggplot(temp,aes(date,value,colour=factor(gradient_site)))+
#   geom_point(size=0.1)+
#   geom_smooth()+
#   theme_bw()

#Daily means
daily_temp<- temp %>%
  group_by(Day,Plot) %>%
  summarise(Daily_mean_soil = mean(value))

daily_temp$Day<- as.Date(daily_temp$Day)

# ggplot(daily_temp,aes(Day,Daily_mean,colour=factor(Plot)))+
#   geom_point(size=0.4)+
#   geom_smooth()+
#   theme_bw()

#Plot means
#Trim data to dates of tea burial
plot_temp_means=NULL
for(i in unique(daily_temp$Plot)){
  DOB<-min(tea[tea$Plot==i,]$Burial) #Find date of burial
  DOR<-max(tea[tea$Plot==i,]$Recovery) #Find date of recovery
  daily_temp_cut<-subset(daily_temp,Day>=DOB&Day<=DOR) #Trim data
  
  Out<- daily_temp_cut %>%
    filter(Plot==i) %>%
    group_by(Plot) %>%
    summarise(Mean_Temperature = mean(Daily_mean_soil),
              sd_Temperature = sd(Daily_mean_soil),
              GSL_0 = length(which(Daily_mean_soil > 0)),
              GSL_5 = length(which(Daily_mean_soil > 5)),
              GDD_0 = sum(Daily_mean_soil[Daily_mean_soil>0]),
              GDD_5 = sum(Daily_mean_soil[Daily_mean_soil>5]))
  
  plot_temp_means<-rbind(plot_temp_means,Out)
}

plot_temp_means$source<-"ibutton data"
plot_temp_means$scale<-"plot"

Plot_Variables_Soil<-rbind(Plot_Variables_Soil,plot_temp_means)

#Add missing sites
missing<-unique(subset(tea[tea$Contributor=="Eleanor Walker, Haydn Thomas"&tea$Site!="Common Garden",],Plot%notin%Plot_Variables_Soil$Plot)$Plot)
plot_temp_means<-plot_temp_means[c(1:length(missing)),]
plot_temp_means[,c(2:5)]<-NA
plot_temp_means$Plot<-missing
Plot_Variables_Soil<-rbind(Plot_Variables_Soil,plot_temp_means)

#CG Tea####
filenames <- list.files(path="scripts/users/hthomas/Data/Tea/Env.Vars/CG/ibuttons/") # create list of temperature data files

tempdata <- lapply(filenames, function(x) read.csv(paste("scripts/users/hthomas/Data/Tea/Env.Vars/CG/ibuttons/",x,sep=""), sep=",", header=F, skip=15)) # read in all data files into large list

for(i in 1:length(filenames)) {
  #colnames(tempdata[[i]]) <- tempdata[[i]][-1,]; # put first line as header
  tempdata[[i]] <- tempdata[[i]][,-2]; # delete dispensable second column 
  tempdata[[i]][,2] <- as.numeric(as.character(tempdata[[i]][,2]))
  tempdata[[i]][,3] <- as.numeric(as.character(tempdata[[i]][,3]))
  names(tempdata[[i]])[c(1:3)]<-c("Date/Time","Value","Decimal")
  tempdata[[i]][is.na(tempdata[[i]]$Decimal),3] <- 0
  tempdata[[i]][,1] <- as.POSIXct(tempdata[[i]][,1]) # convert date column into date format
  #days <- tempdata[[i]][,1]
  #tempdata[[i]] <- aggregate(tempdata[[i]][,2], by = list(days), mean); # compute mean temperatures per day
  
  fileinfo <- strsplit(file_path_sans_ext(filenames[i]),"_")[[1]]; # extract plot infos from file name
  
  for (j in 1:2) {
    tempdata[[i]][,3+j] <- fileinfo[j]
  } # add columns for plot infos
  colnames(tempdata[[i]])[c(1:5)] <- c("date","value","decimal","location","replicate")
}

#Raw Data
temp <- do.call("rbind", tempdata)
temp$date<-temp$date + years(2000) #correct years
temp$Day<-as.Date(cut(temp$date, "day")) #Extract days
temp$Site<-"Common Garden"
temp$source<-"ibutton"
temp$value<-as.numeric(as.character(paste(temp$value,temp$decimal,sep=".")))
temp<-temp[,-3]

# #Plot data
# ggplot(temp,aes(date,value,colour=factor(location)))+
#   geom_point(size=0.1)+
#   geom_smooth()+
#   theme_bw()

CG_temp$source<-"HOBO"
CG_temp$Site<-"Common Garden"
CG_temp<-CG_temp[,-5]
CG_temp$replicate<-"1"
CG_temp$Day<-as.Date(CG_temp$Day)

#Change any negative moisture scores to NA
CG_temp[CG_temp$location=="Moisture" & CG_temp$value<0,]$value<-NA

temp<-rbind(temp,CG_temp)

#Daily means
daily_temp<- temp %>%
  group_by(Day,location,source) %>%
  summarise(Daily_mean = mean(value))

daily_temp$Day<- as.Date(daily_temp$Day)

# ggplot(daily_temp,aes(Day,Daily_mean,colour=factor(location)))+
#   geom_point(size=0.4)+
#   geom_smooth()+
#   theme_bw()

#Plot means
#Trim data to dates of tea burial
plot_temp_means=NULL
for(i in unique(tea[tea$Site=="Common Garden"&substr(tea$Plot,1,5)!="CG_DT"&tea$Plot!="DC_HT_1yr"&tea$Contributor=="Haydn Thomas",]$Plot)){
  DOB<-min(tea[tea$Plot==i,]$Burial) #Find date of burial
  DOR<-max(tea[tea$Plot==i,]$Recovery) #Find date of recovery
  daily_temp_cut<-subset(daily_temp,Day>=DOB&Day<=DOR) #Trim data
  
  Out<- daily_temp_cut %>%
    group_by(location,source) %>%
    summarise(Mean_Temperature = mean(Daily_mean,na.rm=T),
              sd_Temperature = sd(Daily_mean,na.rm=T),
              GSL_0 = length(which(Daily_mean > 0)),
              GSL_5 = length(which(Daily_mean > 5)),
              GDD_0 = sum(Daily_mean[Daily_mean>0],na.rm=T),
              GDD_5 = sum(Daily_mean[Daily_mean>5],na.rm=T))
  
  Out$Plot<-i
  plot_temp_means<-rbind(plot_temp_means,Out)
}

plot_temp_means$scale<-"plot"

plot_soiltemp_means<-subset(plot_temp_means,location == "Soil"&source=="ibutton")[,-1]
Plot_Variables_Soil<-rbind(Plot_Variables_Soil,plot_soiltemp_means)

plot_soiltemp_means<-subset(plot_temp_means,location == "Soil"&source=="HOBO"&Plot=="CG_Y2_3m")[,-1]
Plot_Variables_Soil<-rbind(Plot_Variables_Soil,plot_soiltemp_means)

plot_airtemp_means<-subset(plot_temp_means,location == "Air"&source=="ibutton")[,-1]
Plot_Variables_Air<-rbind(Plot_Variables_Air,plot_airtemp_means)

plot_airtemp_means<-subset(plot_temp_means,location == "Air"&source=="HOBO"&Plot=="CG_Y2_3m")[,-1]
Plot_Variables_Air<-rbind(Plot_Variables_Air,plot_airtemp_means)

plot_moisture_means<-subset(plot_temp_means,location == "Moisture")[,-c(1,5:8)]
plot_moisture_means<-filter(plot_moisture_means,Plot=="CG_HT_year"|Plot=="CG_HT_winter")
names(plot_moisture_means)[c(2:3)]<-c("Mean_moisture","sd_moisture")
Plot_Variables_Moisture<-rbind(Plot_Variables_Moisture,plot_moisture_means)


#El Experiment - Soil
filenames <- list.files(path="scripts/users/hthomas/Data/Tea/Env.Vars/CG_Expmt/") # create list of temperature data files

tempdata <- lapply(filenames, function(x) read.csv(paste("scripts/users/hthomas/Data/Tea/Env.Vars/CG_Expmt/",x,sep=""), sep=",", header=F, skip=15)) # read in all data files into large list

for(i in 1:length(filenames)) {
  #colnames(tempdata[[i]]) <- tempdata[[i]][-1,]; # put first line as header
  tempdata[[i]] <- tempdata[[i]][,-2]; # delete dispensable second column 
  tempdata[[i]][,2] <- as.numeric(as.character(tempdata[[i]][,2]))
  #tempdata[[i]][,3] <- as.numeric(as.character(tempdata[[i]][,3]))
  #names(tempdata[[i]])[c(1:3)]<-c("Date/Time","Value","Decimal")
  #tempdata[[i]][is.na(tempdata[[i]]$Decimal),3] <- 0
  tempdata[[i]][,1] <- as.POSIXct(tempdata[[i]][,1],format="%d/%m/%y %H:%M:%S") # convert date column into date format
  #days <- tempdata[[i]][,1]
  #tempdata[[i]] <- aggregate(tempdata[[i]][,2], by = list(days), mean); # compute mean temperatures per day
  
  fileinfo <- strsplit(file_path_sans_ext(filenames[i]),"_")[[1]]; # extract plot infos from file name
  
  for (j in 1:4) {
    tempdata[[i]][,2+j] <- fileinfo[j]
  } # add columns for plot infos
  colnames(tempdata[[i]])[c(1:6)] <- c("date","value","Contributor","Site","Plot_Type","replicate")
}

#Raw Data
temp <- do.call("rbind", tempdata)
#temp$date<-temp$date + years(2000) #correct years
temp$Day<-as.Date(cut(temp$date, "day")) #Extract days
temp$Plot<-paste(temp$Plot_Type,temp$replicate,sep="_")
temp$source<-"ibutton"

# #Plot data
# ggplot(temp,aes(date,value,colour=factor(Plot)))+
#   geom_point(size=0.1)+
#   geom_smooth()+
#   theme_bw()

#Daily means
daily_temp<- temp %>%
  group_by(Day,Plot) %>%
  summarise(Daily_mean = mean(value))
daily_temp$Day<- as.Date(daily_temp$Day)

# ggplot(daily_temp,aes(Day,Daily_mean,colour=factor(Plot)))+
#   geom_point(size=0.4)+
#   geom_smooth()+
#   theme_bw()

#Data for NW1 missing after 4th July - use NW2
subset<-subset(daily_temp,Plot=="NW_2"&Day>2016-07-04)
subset$Plot<-"NW_1"
daily_temp<-rbind(daily_temp,subset)

#LWC_2 missing, use LWC_1
subset<-subset(daily_temp,Plot=="LWC_1")
subset$Plot<-"LWC_2"
daily_temp<-rbind(daily_temp,subset)

El_plots<-unique(daily_temp$Plot)
ctl_plots<-subset(daily_temp,Plot=="CTL_1"|Plot=="CTL_2")

#Plot means
#Trim data to dates of tea burial
plot_temp_means=NULL
for(i in unique(El_plots)){
  DOB<-min(tea[tea$Plot==i,]$Burial) #Find date of burial
  DOR<-max(tea[tea$Plot==i,]$Recovery) #Find date of recovery
  daily_temp_cut<-subset(daily_temp,Day>=DOB&Day<=DOR) #Trim data
  
  Out<- daily_temp_cut %>%
    group_by(Plot) %>%
    filter(Plot==i) %>%
    summarise(Mean_Temperature = mean(Daily_mean),
              sd_Temperature = sd(Daily_mean),
              GSL_0 = length(which(Daily_mean > 0)),
              GSL_5 = length(which(Daily_mean > 5)),
              GDD_0 = sum(Daily_mean[Daily_mean>0]),
              GDD_5 = sum(Daily_mean[Daily_mean>5]))
  
  plot_temp_means<-rbind(plot_temp_means,Out)
}

plot_temp_means$scale<-"plot"
plot_temp_means$source<-"ibutton"

Plot_Variables_Soil<-rbind(Plot_Variables_Soil,plot_temp_means)

#El Experiment - Air
temp<-CG_temp

#Daily means
daily_temp<- temp %>%
  group_by(Day,location) %>%
  summarise(Daily_mean = mean(value))
daily_temp$Day<- as.Date(daily_temp$Day)

#Plot means
#Trim data to dates of tea burial
plot_temp_means=NULL
for(i in unique(El_plots)){
  DOB<-min(tea[tea$Plot==i,]$Burial) #Find date of burial
  DOR<-max(tea[tea$Plot==i,]$Recovery) #Find date of recovery
  daily_temp_cut<-subset(daily_temp,Day>=DOB&Day<=DOR) #Trim data
  
  Out<- daily_temp_cut %>%
    group_by(location) %>%
    summarise(Mean_Temperature = mean(Daily_mean),
              sd_Temperature = sd(Daily_mean),
              GSL_0 = length(which(Daily_mean > 0)),
              GSL_5 = length(which(Daily_mean > 5)),
              GDD_0 = sum(Daily_mean[Daily_mean>0]),
              GDD_5 = sum(Daily_mean[Daily_mean>5]))
  
  Out$Plot<-i
  plot_temp_means<-rbind(plot_temp_means,Out)
}

plot_temp_means$source<-"HOBO"
plot_temp_means$scale<-"site"

plot_airtemp_means<-subset(plot_temp_means,location == "Air")[,-1]
Plot_Variables_Air<-rbind(Plot_Variables_Air,plot_airtemp_means)

#Daily tea
plot_temp_means=NULL
i="DC_HT_1yr"
DOB<-min(tea[tea$Plot==i,]$Burial) #Find date of burial
DOR<-max(tea[tea$Plot==i,]$Recovery) #Find date of recovery
daily_temp_cut<-subset(daily_temp,Day>=DOB&Day<=DOR) #Trim data

Out<- daily_temp_cut %>%
  group_by(location) %>%
  summarise(Mean_Temperature = mean(Daily_mean,na.rm = T),
            sd_Temperature = sd(Daily_mean,na.rm = T),
            GSL_0 = length(which(Daily_mean > 0)),
            GSL_5 = length(which(Daily_mean > 5)),
            GDD_0 = sum(Daily_mean[Daily_mean>0]),
            GDD_5 = sum(Daily_mean[Daily_mean>5]))

Out$Plot<-i
plot_temp_means<-rbind(plot_temp_means,Out)

plot_temp_means$scale<-"Plot"
plot_temp_means$source<-"HOBO"

plot_soiltemp_means<-subset(plot_temp_means,location == "Soil")[,-1]
Plot_Variables_Soil<-rbind(Plot_Variables_Soil,plot_soiltemp_means)

plot_airtemp_means<-subset(plot_temp_means,location == "Air")[,-1]
Plot_Variables_Air<-rbind(Plot_Variables_Air,plot_airtemp_means)

plot_moisture_means<-subset(plot_temp_means,location == "Moisture")[,-c(1,4:7)]
names(plot_moisture_means)[c(1:2)]<-c("Mean_moisture","sd_moisture")
Plot_Variables_Moisture<-rbind(Plot_Variables_Moisture,plot_moisture_means)

#Daily Tea
ctl_plots$location<-"Soil"
daily_temp<-rbind(daily_temp,ctl_plots)

#Cut to tea
plot_temp_means=NULL
for(i in unique(tea[substr(tea$Plot,1,5)=="CG_DT",]$Plot)){
  DOB<-min(tea[tea$Plot==i,]$Burial) #Find date of burial
  DOR<-max(tea[tea$Plot==i,]$Recovery) #Find date of recovery
  daily_temp_cut<-subset(daily_temp,Day>=DOB&Day<=DOR) #Trim data
  
  Out<- daily_temp_cut %>%
    group_by(location) %>%
    summarise(Mean_Temperature = mean(Daily_mean),
              sd_Temperature = sd(Daily_mean),
              GSL_0 = length(which(Daily_mean > 0)),
              GSL_5 = length(which(Daily_mean > 5)),
              GDD_0 = sum(Daily_mean[Daily_mean>0]),
              GDD_5 = sum(Daily_mean[Daily_mean>5]))
  
  Out$Plot<-i
  plot_temp_means<-rbind(plot_temp_means,Out)
}

plot_temp_means$scale<-"Site"

plot_soiltemp_means<-subset(plot_temp_means,location == "Soil")[,-1]
plot_soiltemp_means$source<-"HOBO & ibutton"
Plot_Variables_Soil<-rbind(Plot_Variables_Soil,plot_soiltemp_means)

plot_airtemp_means<-subset(plot_temp_means,location == "Air")[,-1]
plot_airtemp_means$source<-"HOBO & ibutton"
Plot_Variables_Air<-rbind(Plot_Variables_Air,plot_airtemp_means)


plot_moisture_means<-subset(plot_temp_means,location == "Moisture")[,-c(1,4:7)]
daily_tea_plots<-plot_moisture_means$Plot

#Kluane Gradient Moisture####
temp<-read.csv("scripts/users/hthomas/Data/Tea/Env.Vars/Kluane_Moisture/Plat_Moisture_Data.csv")

temp<-temp[,c(1:6)]
temp$Date<-as.POSIXct(temp$Date,format="%d/%m/%Y")
temp$Date<-temp$Date + years(2000) #correct years
names(temp)[4]<-"Moisture.Orig"

temp$Plot<-paste(temp$Site,temp$Treatment,sep="")

#Extract dates for which both
cal.data<-subset(temp,Date=="2016-08-13")

#Predict chinese sensor variables
#Currently no overlap
cal.data<-cal.data %>%
  group_by(Plot, Probe.Type) %>%
  summarise(Moisture.Orig = mean(Moisture.Orig)) %>%
  spread(Probe.Type,Moisture.Orig)

y<-cal.data$HydroSense
x<-as.numeric(as.character(cal.data$Chinese))

fit <- lm(y ~ x)

temp2<-subset(temp,Probe.Type=="Chinese")
temp<-subset(temp,Probe.Type=="HydroSense")
temp$Moisture<-temp$Moisture.Orig

temp2$Moisture<-(predict(fit, list(x=temp2$Moisture.Orig)))[1:122]

temp<-rbind(temp,temp2)

#Check what it looks like
ggplot(temp,aes(Plot,Moisture,colour=Probe.Type))+
  geom_boxplot()
#DECENT
#Using both data but could choose to use just one if more appropriate

#Site means
plot_temp_means<- temp %>%
  group_by(Plot) %>%
  summarise(Mean_moisture = mean(Moisture),
            sd_moisture = sd(Moisture))

plot_temp_means$source<-"Moisture Probe"
plot_temp_means$scale<-"plot"

Plot_Variables_Moisture<-rbind(Plot_Variables_Moisture,plot_temp_means)

#El Experiment - Moisture####
temp<-read.csv("scripts/users/hthomas/Data/Tea/Env.Vars/Kluane_Moisture/CG_Soil_Moisture.csv")

temp<-temp[,c(1:6)]
temp$Date<-as.POSIXct(temp$Date,format="%d/%m/%Y")
temp$Date<-temp$Date + years(2000) #correct years

#Extract Chinese
temp2<-subset(temp,Probe.type=="Chinese")
temp<-subset(temp,Probe.type=="HydroSense")
temp$Moisture<-temp$Moiture..VWC....

#Convert
temp2$Moisture<-(predict(fit, list(x=temp2$Moiture..VWC....)))
#Combine
temp<-rbind(temp,temp2)

#Check what it looks like
ggplot(temp,aes(Date,Moisture,colour=factor(Plot_Type)))+
  geom_point(size=0.1)+
  geom_smooth(method="lm")+
  theme_bw()

#Site means
plot_temp_means<- temp %>%
  group_by(Plot_Type) %>%
  summarise(Mean_moisture = mean(Moisture,na.rm=T),
            sd_moisture = sd(Moisture,na.rm=T))

#Add plot
plot_temp_means2<-plot_temp_means
plot_temp_means2$Plot<-paste(plot_temp_means$Plot_Type,"2",sep="_")
plot_temp_means$Plot<-paste(plot_temp_means$Plot_Type,"1",sep="_")
plot_temp_means<-rbind(plot_temp_means,plot_temp_means2)

plot_temp_means$source<-"Moisture Probe and Hydrosense"
plot_temp_means$scale<-"plot (averaged over treatment)"

Plot_Variables_Moisture<-rbind(Plot_Variables_Moisture,plot_temp_means[-1])

#Add ambient moisture to daily tea and common garden
plot_temp_means_ambient<-plot_temp_means[plot_temp_means$Plot_Type=="CTL",][1,]


out<-cbind(Plot = daily_tea_plots, plot_temp_means_ambient)
plot_temp_means<-out[,c(3,4,1,6)]

plot_temp_means<-rbind(plot_temp_means,cbind(Plot = "CG_Y2_3m", plot_temp_means_ambient)[,c(3,4,1,6)])


#Trail Creek####
temp<-read.csv(file="scripts/users/hthomas/Data/Tea/Env.Vars/Inuvik/TM2_2016_EditedforHaydn_Jan-20-17_CW.csv")

#Add plot and date
temp$Date<-as.Date(temp$Julian.Day - 1, origin = "2016-01-01")
temp$Air.Temp..C.<-as.numeric(as.character(temp$Air.Temp..C.))

#Plot data
# ggplot(temp,aes(Date,Air.Temp..C.))+
#   geom_point(size=0.1)+
#   geom_smooth()+
#   theme_bw()

#Daily means
daily_temp<- temp %>%
  group_by(Date) %>%
  summarise(Daily_mean = mean(Air.Temp..C.))

daily_temp$Date<- as.Date(daily_temp$Date)

#Plot means
#Trim data to dates of tea burial
plot_temp_means=NULL
for(i in unique(tea[tea$Contributor=="Cory Wallace",]$Plot)){
  DOB<-min(tea[tea$Plot==i,]$Burial) #Find date of burial
  DOR<-max(tea[tea$Plot==i,]$Recovery) #Find date of recovery
  daily_temp_cut<-subset(daily_temp,Date>=DOB&Date<=DOR) #Trim data
  
  Out<- daily_temp_cut %>%
    summarise(Mean_Temperature = mean(Daily_mean),
              sd_Temperature = sd(Daily_mean),
              GSL_0 = length(which(Daily_mean > 0)),
              GSL_5 = length(which(Daily_mean > 5)),
              GDD_0 = sum(Daily_mean[Daily_mean>0]),
              GDD_5 = sum(Daily_mean[Daily_mean>5]))
  
  Out$Plot<-i
  plot_temp_means<-rbind(plot_temp_means,Out)
}

plot_temp_means$source<-"Weather station"
plot_temp_means$scale<-"Site"
plot_temp_means<-plot_temp_means[,c(7,1:6,8:9)]

Plot_Variables_Air<-rbind(Plot_Variables_Air,plot_temp_means)

plot_soiltemp_means<-plot_temp_means
plot_soiltemp_means[,c(2:8)]<-NA

Plot_Variables_Soil<-rbind(Plot_Variables_Soil,plot_soiltemp_means)

#Trail Creek Moisture####
temp<-read.csv(file="scripts/users/hthomas/Data/Tea/Env.Vars/Inuvik_Moisture/Env.Variable.Extraction.csv")

#Add plot and date
temp$Plot<-paste0("Trail_Valley_",temp$Patch,temp$Position)
temp$Fivecm<-as.numeric(as.character(temp$Fivecm))*100

#Plot data
# ggplot(temp,aes(Date,Air.Temp..C.))+
#   geom_point(size=0.1)+
#   geom_smooth()+
#   theme_bw()

plot_moisture_means <- temp %>%
  group_by(Plot) %>%
  summarise(Mean_moisture = mean(Fivecm,na.rm = T),
            sd_moisture = sd(Fivecm,na.rm = T)) %>%
  filter(!is.na(Mean_moisture))

plot_moisture_means$scale<-"Plot"
plot_moisture_means$source<-"Moisture Probe"

Plot_Variables_Moisture<-rbind(Plot_Variables_Moisture,plot_moisture_means)


#Kangerlussuaq####
temp<-read.csv(file="scripts/users/hthomas/Data/Tea/Env.Vars/Kangerlussuaq/Town.csv")

#Add plot and date
temp$date<-as.POSIXct(temp$WGST,format="%d/%m/%Y")
temp$date<-temp$date + years(2000) #correct years

#Plot data
# ggplot(temp,aes(date,Mean.TemperatureC))+
#   geom_point(size=0.1)+
#   geom_smooth()+
#   theme_bw()

#Daily means
daily_temp<- temp %>%
  group_by(date) %>%
  summarise(Daily_mean = mean(Mean.TemperatureC))

daily_temp$date<- as.Date(daily_temp$date)
daily_temp$Plot<-"Kangerlussuaq_Town"

temp2<-read.table(file="scripts/users/hthomas/Data/Tea/Env.Vars/Kangerlussuaq/KAN_B_hour.txt",header=T)

#Add plot and date
temp2$date<-as.POSIXct(paste(temp2$Year,"/",temp2$MonthOfYear,"/",temp2$DayOfMonth,sep=""),format="%Y/%m/%d")
temp2<-subset(temp2,AirTemperature.C.>-250)

#Plot data
# ggplot(temp2,aes(date,AirTemperature.C.))+
#   geom_point(size=0.1)+
#   geom_smooth()+
#   theme_bw()

#Daily means
daily_temp2<- temp2 %>%
  group_by(date) %>%
  summarise(Daily_mean = mean(AirTemperature.C.))

daily_temp2$date<- as.Date(daily_temp2$date)
daily_temp2$Plot<-"Kangerlussuaq_Ice"

#Combine
daily_temp<-rbind(daily_temp,daily_temp2)

#Plot data
# ggplot(daily_temp,aes(date,Daily_mean,colour=Plot))+
#   geom_point(size=0.1)+
#   geom_smooth()+
#   theme_bw()

#Plot means
#Trim data to dates of tea burial
plot_temp_means=NULL
for(i in unique(daily_temp$Plot)){
  DOB<-min(tea[tea$Plot==i,]$Burial) #Find date of burial
  DOR<-max(tea[tea$Plot==i,]$Recovery) #Find date of recovery
  daily_temp_cut<-subset(daily_temp,date>=DOB&date<=DOR) #Trim data
  
  Out<- daily_temp_cut %>%
    filter(Plot==i) %>%
    group_by(Plot) %>%
    summarise(Mean_Temperature = mean(Daily_mean),
              sd_Temperature = sd(Daily_mean),
              GSL_0 = length(which(Daily_mean > 0)),
              GSL_5 = length(which(Daily_mean > 5)),
              GDD_0 = sum(Daily_mean[Daily_mean>0]),
              GDD_5 = sum(Daily_mean[Daily_mean>5]))
  
  plot_temp_means<-rbind(plot_temp_means,Out)
}

plot_temp_means$source<-"Weather station"
plot_temp_means$scale<-"Plot"

Plot_Variables_Air<-rbind(Plot_Variables_Air,plot_temp_means)

plot_soiltemp_means<-plot_temp_means
plot_soiltemp_means[,c(2:8)]<-NA

Plot_Variables_Soil<-rbind(Plot_Variables_Soil,plot_soiltemp_means)

#Toolik####
temp<-read.csv(file="scripts/users/hthomas/Data/Tea/Env.Vars/Toolik/2011-Present_MAT_HourlyWeather.csv")

#Add plot and date
temp$date<-as.POSIXct(temp$DATE,format="%d-%b-%Y %H:%M")
temp$temp<-as.numeric(as.character(temp$CTEMP.3M))
temp$Day<-as.Date(cut(temp$date, "day")) #Extract days
temp$Site<-"MATCT1"

temp<-select(temp,date,Day,Site,temp)
i<-min(temp$Day,na.rm=T)

temp2<-read.csv(file="scripts/users/hthomas/Data/Tea/Env.Vars/Toolik/1999-present_MNT_air_temperature.csv")

#Add plot and date
temp2$date<-as.POSIXct(temp2$Date,format="%d-%b-%Y %H:%M")
temp2$temp<-as.numeric(as.character(temp2$CT.Air.Temp))
temp2$Day<-as.Date(cut(temp2$date, "day")) #Extract days
temp2$Site<-"MNATCT"

temp2<-select(temp2,date,Day,Site,temp)
temp2<-subset(temp2,Day>i) #Trim data to after 2011

temp<-rbind(temp,temp2)
temp$doy<-as.numeric(strftime(temp$Day, format = "%j"))

#Plot data
# ggplot(temp,aes(date,temp,colour=Site))+
#   geom_point(size=0.1)+
#   geom_smooth()+
#   theme_bw()

#Daily means
daily_temp<- temp %>%
  group_by(doy,Site) %>%
  summarise(Daily_mean = mean(temp))

#Plot
ggplot(daily_temp,aes(doy,Daily_mean,colour=Site))+
  geom_point()


#Plot means
#Trim data to dates of tea burial
plot_temp_means=NULL
for(i in unique(tea[tea$Contributor=="Laura Gough, Dan Ackerman",]$Plot)){
  DOB<-as.numeric(strftime(min(tea[tea$Plot==i,]$Burial),format= "%j")) #Find date of burial
  DOR<-as.numeric(strftime(max(tea[tea$Plot==i,]$Recovery),format= "%j")) #Find date of recovery
  daily_temp_cut<-subset(daily_temp,doy>=DOB&doy<=DOR) #Trim data
  daily_temp_cut<-rbind(daily_temp,daily_temp_cut) #Add back in as full year
  site<-unique(tea[tea$Plot==i,]$Site)
  
  Out<- daily_temp_cut %>%
    filter(Site==site) %>%
    group_by(Site) %>%
    summarise(Mean_Temperature = mean(Daily_mean,na.rm=T),
              sd_Temperature = sd(Daily_mean,na.rm=T),
              GSL_0 = length(which(Daily_mean > 0)),
              GSL_5 = length(which(Daily_mean > 5)),
              GDD_0 = sum(Daily_mean[Daily_mean>0],na.rm=T),
              GDD_5 = sum(Daily_mean[Daily_mean>5],na.rm=T))
  
  Out$Plot<-i
  plot_temp_means<-rbind(plot_temp_means,Out)
}

plot_temp_means$source<-"Weather station (5 year mean)"
plot_temp_means$scale<-"Site"
plot_temp_means<-plot_temp_means[,c(8,2:7,9:10)]

Plot_Variables_Air<-rbind(Plot_Variables_Air,plot_temp_means)

plot_soiltemp_means<-plot_temp_means
plot_soiltemp_means[,c(2:8)]<-NA

Plot_Variables_Soil<-rbind(Plot_Variables_Soil,plot_soiltemp_means)

#Gothic####
temp<-read.csv(file="scripts/users/hthomas/Data/Tea/Env.Vars/Gothic/Habitat_air_temperature/Gothic_aspen_forest_air_temperature_2016.csv")
temp<-rbind(temp,read.csv(file="scripts/users/hthomas/Data/Tea/Env.Vars/Gothic/Habitat_air_temperature/Gothic_rocky_meadow_air_temperature_2016.csv"))
temp<-rbind(temp,read.csv(file="scripts/users/hthomas/Data/Tea/Env.Vars/Gothic/Habitat_air_temperature/Gothic_veratrum_meadow_air_temperature_2016.csv"))
temp<-rbind(temp,read.csv(file="scripts/users/hthomas/Data/Tea/Env.Vars/Gothic/Habitat_air_temperature/Gothic_wet_meadow_air_temperature_2016.csv"))                        
temp<-rbind(temp,read.csv(file="scripts/users/hthomas/Data/Tea/Env.Vars/Gothic/Habitat_air_temperature/Gothic_willow-wet_meadow_interface_air_temperature_2016.csv"))

temp$Date<-paste(temp$day,temp$month,temp$year,sep=":")
temp$Date<-as.POSIXct(temp$Date,format="%d:%m:%Y")

# #Plot data
# ggplot(temp,aes(Date,temperature.C, colour = habitat))+
#   geom_point(size=0.1)+
#   geom_smooth()+
#   theme_bw()

#Daily means
daily_temp<- temp %>%
  group_by(Date,habitat) %>%
  summarise(Daily_mean = mean(temperature.C))

daily_temp$habitat<-as.character(daily_temp$habitat)

daily_temp[daily_temp$habitat=="willow-wet.meadow.interface",]$habitat<-"Willow-wet.meadow.interface"
daily_temp[daily_temp$habitat=="rocky.meadow",]$habitat<-"Rocky.meadow"
daily_temp[daily_temp$habitat=="aspen.forest",]$habitat<-"Aspen.forest"
daily_temp[daily_temp$habitat=="veratrum.meadow",]$habitat<-"Veratrum.meadow"
daily_temp[daily_temp$habitat=="wet.meadow",]$habitat<-"Wet.meadow"

#Plot means
#Trim data to dates of tea burial
plot_temp_means=NULL
for(i in unique(tea[tea$Contributor=="David Inouye, Jane Ogilvie",]$Plot)){
  DOB<-min(tea[tea$Plot==i,]$Burial) #Find date of burial
  DOR<-max(tea[tea$Plot==i,]$Recovery) #Find date of recovery
  daily_temp_cut<-subset(daily_temp,Date>=DOB&Date<=DOR & habitat == i) #Trim data
  
  Out<- daily_temp_cut %>%
    ungroup() %>%
    dplyr::summarise(Mean_Temperature = mean(Daily_mean,na.rm=T),
              sd_Temperature = sd(Daily_mean,na.rm=T),
              GSL_0 = length(which(Daily_mean > 0)),
              GSL_5 = length(which(Daily_mean > 5)),
              GDD_0 = sum(Daily_mean[Daily_mean>0],na.rm=T),
              GDD_5 = sum(Daily_mean[Daily_mean>5],na.rm=T))
  
  Out$Plot<-i
  plot_temp_means<-rbind(plot_temp_means,Out)
}

plot_temp_means$source<-"Weather station (min and max temps)"
plot_temp_means$scale<-"Site"
plot_temp_means<-plot_temp_means[,c(7,1:6,8:9)]

Plot_Variables_Air<-rbind(Plot_Variables_Air,plot_temp_means)

#Gothic Moisture####
plot_soiltemp_means<-plot_temp_means
plot_soiltemp_means[,c(2:8)]<-NA

temp<-read.csv(file="scripts/users/hthomas/Data/Tea/Env.Vars/Gothic/Habitat_soil_moisture/Gothic_rocky_meadow_soil_moisture_2016.csv")
temp<-rbind(temp,read.csv(file="scripts/users/hthomas/Data/Tea/Env.Vars/Gothic/Habitat_soil_moisture/Gothic_wet_meadow_soil_moisture_2016.csv"))

temp$Date<-as.POSIXct(temp$date,format="%d-%B-%y")

temp$south_5cm<-as.numeric(as.character(temp$south_5cm))
temp$north_5cm<-as.numeric(as.character(temp$north_5cm))

#Take mean moisture
temp$moisture <- rowMeans(temp[c('south_5cm', 'north_5cm')], na.rm=TRUE)*100

#Plot data
ggplot(temp,aes(Date,moisture, colour = habitat))+
  geom_point(size=0.1)+
  geom_smooth()+
  theme_bw()

#Daily means
daily_temp<- temp %>%
  group_by(Date,habitat) %>%
  summarise(Daily_mean = mean(moisture))

WM<-subset(daily_temp, habitat == "Wet.meadow")
aspen<-WM
aspen$habitat<-"Aspen.forest"

WW<-WM
WW$habitat<-"Willow-wet.meadow.interface"

VW<-WM
VW$habitat<-"Veratrum.meadow"

daily_temp<-rbind(daily_temp, aspen, WW, VW)
daily_temp$habitat<-as.character(daily_temp$habitat)

unique(daily_temp$habitat)
unique(tea[tea$Contributor=="David Inouye, Jane Ogilvie",]$Plot)
#Plot means
#Trim data to dates of tea burial
plot_temp_means=NULL
for(i in unique(tea[tea$Contributor=="David Inouye, Jane Ogilvie",]$Plot)){
  DOB<-min(tea[tea$Plot==i,]$Burial) #Find date of burial
  DOR<-max(tea[tea$Plot==i,]$Recovery) #Find date of recovery
  daily_temp_cut<-subset(daily_temp,Date>=DOB&Date<=DOR & habitat == i) #Trim data
  
  Out<- daily_temp_cut %>%
    ungroup() %>%
    summarise(Mean_moisture = mean(Daily_mean,na.rm=T),
              sd_moisture = sd(Daily_mean,na.rm=T))
  
  Out$Plot<-i
  plot_temp_means<-rbind(plot_temp_means,Out)
}

plot_temp_means$source<-"HOBO"
plot_temp_means$scale<-"Site"


Plot_Variables_Moisture<-rbind(Plot_Variables_Moisture,plot_temp_means)


plot_soiltemp_means<-plot_temp_means[,c(3,1,2,4,5)]
plot_soiltemp_means[,c(2:9)]<-NA
names(plot_soiltemp_means)<-colnames(Plot_Variables_Soil)

Plot_Variables_Soil<-rbind(Plot_Variables_Soil,plot_soiltemp_means)

#Val Bercla - Summer, Soils####
temp<-read.csv(file="scripts/users/hthomas/Data/Tea/Env.Vars/ValBercla/2014_temps.csv")

temp$Date<-as.POSIXct(temp$Date,format="%d/%m/%Y")
temp$Date<-temp$Date + years(2000) #correct years


# #Plot data
# ggplot(temp,aes(Date,Temp,colour=Treatment))+
#   geom_point(size=0.1)+
#   geom_smooth()+
#   theme_bw()

#Working at the treatment level as lots of missing plots

#Daily means
daily_temp<- temp %>%
  group_by(Date,Treatment) %>%
  summarise(Daily_mean = mean(Temp))

daily_temp$doy<-as.numeric(strftime(daily_temp$Date, format = "%j"))

#Plot means
#Trim data to dates of tea burial
plot_temp_means=NULL
for(i in unique(tea[tea$Site=="Val Bercla"&tea$Season=="Summer",]$Plot)){
  DOB<-as.numeric(strftime(min(tea[tea$Plot=="10A",]$Burial),format= "%j")) #Find date of burial
  DOR<-as.numeric(strftime(max(tea[tea$Plot=="10A",]$Recovery),format= "%j")) #Find date of recovery
  daily_temp_cut<-subset(daily_temp,doy>=DOB&doy<=DOR) #Trim data
  
  Out<- daily_temp_cut %>%
    group_by(Treatment) %>%
    summarise(Mean_Temperature = mean(Daily_mean,na.rm=T),
              sd_Temperature = sd(Daily_mean,na.rm=T),
              GSL_0 = length(which(Daily_mean > 0)),
              GSL_5 = length(which(Daily_mean > 5)),
              GDD_0 = sum(Daily_mean[Daily_mean>0],na.rm=T),
              GDD_5 = sum(Daily_mean[Daily_mean>5],na.rm=T))
  
  Out$Plot<-i
  plot_temp_means<-rbind(plot_temp_means,Out)
}

plot_temp_means$source<-"ibuttons (2014 data)"
plot_temp_means$scale<-"Site (mean of available plots)"

#Pick appropriate treatment
warmed<-plot_temp_means %>%
  mutate(plot_treatment = substr(Plot,28,28)) %>%
  filter(plot_treatment!="c") %>%
  filter(Treatment=="warmed")

control<-plot_temp_means %>%
  mutate(plot_treatment = substr(Plot,28,28)) %>%
  filter(plot_treatment!="c") %>%
  filter(Treatment=="control")

plot_temp_means<-rbind(warmed,control)
plot_temp_means<-plot_temp_means[,-c(1,11)]

Plot_Variables_Soil<-rbind(Plot_Variables_Soil,plot_temp_means)

#Val Bercla, Year, Soils####
temp <- read.csv("scripts/users/hthomas/Data/Tea/Env.Vars/ValBercla/valbercla_near6_winter2015.csv",skip=19) # read in all data files into large list

temp$date<-as.POSIXct(temp$Date.Time,format="%d.%m.%Y %H:%M") # convert date column into date format
temp<-temp[,-c(1,2)]
temp$Day<-cut(temp$date, "day") #Extract days

#Plot temps
ggplot(temp,aes(date,Value))+
  geom_point(size=0.1)+
  geom_smooth()+
  theme_bw()

#Daily means
daily_temp<- temp %>%
  group_by(Day) %>%
  summarise(Daily_mean = mean(Value))
daily_temp$doy<-as.numeric(strftime(daily_temp$Day, format = "%j"))

daily_temp$Day<-as.Date(daily_temp$Day)

#Trim to data
plot_temp_means=NULL
for(i in unique(tea[tea$Site=="Val Bercla"&tea$Season=="Year",]$Plot)){
  DOB<-min(tea[tea$Plot==i,]$Burial) #Find date of burial
  DOB<-DOB-years(1)
  DOR<-max(tea[tea$Plot==i,]$Recovery) #Find date of recovery
  DOR<-DOR-years(1)
  daily_temp_cut<-subset(daily_temp,Day>=DOB&Day<=DOR) #Trim data
  
  Out<- daily_temp_cut %>%
    summarise(Mean_Temperature = mean(Daily_mean,na.rm=T),
              sd_Temperature = sd(Daily_mean,na.rm=T),
              GSL_0 = length(which(Daily_mean > 0)),
              GSL_5 = length(which(Daily_mean > 5)),
              GDD_0 = sum(Daily_mean[Daily_mean>0],na.rm=T),
              GDD_5 = sum(Daily_mean[Daily_mean>5],na.rm=T))
  
  Out$Plot<-i
  plot_temp_means<-rbind(plot_temp_means,Out)
}

plot_temp_means$source<-"ibutton - single (2014 data)"
plot_temp_means$scale<-"Site"

Plot_Variables_Soil<-rbind(Plot_Variables_Soil,plot_temp_means)

#Val Bercla, Winter, Soils####
temp <- read.csv("scripts/users/hthomas/Data/Tea/Env.Vars/ValBercla/valbercla_near6_winter2015.csv",skip=19) # read in all data files into large list

temp$date<-as.POSIXct(temp$Date.Time,format="%d.%m.%Y %H:%M") # convert date column into date format
temp<-temp[,-c(1,2)]
temp$Day<-cut(temp$date, "day") #Extract days

#Plot temps
ggplot(temp,aes(date,Value))+
  geom_point(size=0.1)+
  geom_smooth()+
  theme_bw()

#Daily means
daily_temp<- temp %>%
  group_by(Day) %>%
  summarise(Daily_mean = mean(Value))
daily_temp$doy<-as.numeric(strftime(daily_temp$Day, format = "%j"))

daily_temp$Day<-as.Date(daily_temp$Day)

#Trim to data
plot_temp_means=NULL
for(i in unique(tea[tea$Site=="Val Bercla"&tea$Season=="Winter",]$Plot)){
  DOB<-min(tea[tea$Plot==i,]$Burial)
  DOB<-DOB-years(1)#Find date of burial
  DOR<-min(tea[tea$Plot==i,]$Recovery) #Find date of recovery
  DOR<-DOR-years(1)
  daily_temp_cut<-subset(daily_temp,Day<=DOR) #Trim data
  
  Out<- daily_temp_cut %>%
    summarise(Mean_Temperature = mean(Daily_mean,na.rm=T),
              sd_Temperature = sd(Daily_mean,na.rm=T),
              GSL_0 = length(which(Daily_mean > 0)),
              GSL_5 = length(which(Daily_mean > 5)),
              GDD_0 = sum(Daily_mean[Daily_mean>0],na.rm=T),
              GDD_5 = sum(Daily_mean[Daily_mean>5],na.rm=T))
  
  Out$Plot<-i
  plot_temp_means<-rbind(plot_temp_means,Out)
}

plot_temp_means$source<-"ibutton - single (2014 data)"
plot_temp_means$scale<-"Site"

Plot_Variables_Soil<-rbind(Plot_Variables_Soil,plot_temp_means)

#Val Bercla - Air####
temp<-read.csv("scripts/users/hthomas/Data/Tea/Env.Vars/ValBercla/weather_station.csv")

temp$date<-as.POSIXct(paste(temp$Year,"/",temp$Month,"/",temp$Day," ",temp$Hour,":",temp$Minute,sep=""),format="%Y/%m/%d %H:%M")

temp<-temp %>%
  select(date,JUL2,PMA2,VMA2) %>%
  gather(date)

names(temp)[2]<-"station"
temp$Day<-cut(temp$date, "day") #Extract days

# #Plot temps
# ggplot(temp,aes(date,value,colour=station))+
#   geom_point(size=0.1)+
#   geom_smooth()+
#   theme_bw()

#Daily means
daily_temp<- temp %>%
  group_by(Day,station) %>%
  summarise(Daily_mean = mean(value))
daily_temp$Day<- as.Date(daily_temp$Day)


#Trim to data
plot_temp_means=NULL
for(i in unique(tea[tea$Site=="Val Bercla",]$Plot)){
  DOB<-min(tea[tea$Plot==i,]$Burial) #Find date of burial
  DOR<-max(tea[tea$Plot==i,]$Recovery) #Find date of recovery
  daily_temp_cut<-subset(daily_temp,Day>=DOB&Day<=DOR) #Trim data
  
  Out<- daily_temp_cut %>%
    group_by() %>%
    summarise(Mean_Temperature = mean(Daily_mean,na.rm=T),
              sd_Temperature = sd(Daily_mean,na.rm=T),
              GSL_0 = length(which(Daily_mean > 0)),
              GSL_5 = length(which(Daily_mean > 5)),
              GDD_0 = sum(Daily_mean[Daily_mean>0],na.rm=T),
              GDD_5 = sum(Daily_mean[Daily_mean>5],na.rm=T))
  
  Out$Plot<-i
  plot_temp_means<-rbind(plot_temp_means,Out)
}

plot_temp_means$source<-"weather station averages (three stations)"
plot_temp_means$scale<-"Site"

Plot_Variables_Air<-rbind(Plot_Variables_Air,plot_temp_means)

#Stillberg - Air####
temp<-read.csv("scripts/users/hthomas/Data/Tea/Env.Vars/Stillberg/weather_station.csv")

temp$date<-as.POSIXct(temp$TIMESTAMP,format="%d.%m.%Y %H:%M")
temp$Day<-cut(temp$date, "day") #Extract days

# #Plot temps
# ggplot(temp,aes(Day, TA_Thygan))+
#   geom_point(size=0.1)+
#   geom_smooth()+
#   theme_bw()

#Daily means
daily_temp<- temp %>%
  group_by(Day) %>%
  summarise(Daily_mean = mean(TA_Thygan))
daily_temp$Day<- as.Date(daily_temp$Day)

#Trim to data
plot_temp_means=NULL
for(i in unique(tea[tea$Site=="Stillberg",]$Plot)){
  DOB<-min(tea[tea$Plot==i,]$Burial) #Find date of burial
  DOR<-max(tea[tea$Plot==i,]$Recovery) #Find date of recovery
  daily_temp_cut<-subset(daily_temp,Day>=DOB&Day<=DOR) #Trim data
  
  Out<- daily_temp_cut %>%
    group_by() %>%
    summarise(Mean_Temperature = mean(Daily_mean,na.rm=T),
              sd_Temperature = sd(Daily_mean,na.rm=T),
              GSL_0 = length(which(Daily_mean > 0)),
              GSL_5 = length(which(Daily_mean > 5)),
              GDD_0 = sum(Daily_mean[Daily_mean>0],na.rm=T),
              GDD_5 = sum(Daily_mean[Daily_mean>5],na.rm=T))
  
  Out$Plot<-i
  plot_temp_means<-rbind(plot_temp_means,Out)
}

plot_temp_means$source<-"weather station"
plot_temp_means$scale<-"Site"

Plot_Variables_Air<-rbind(Plot_Variables_Air,plot_temp_means)

#Japan####
temp<-read.csv(file="scripts/users/hthomas/Data/Tea/Env.Vars/Japan/soil_temps.csv")

temp$Plot<-paste("Japan_",temp$sitecode,sep="")

plot_temp_means=NULL
for(i in unique(tea[tea$Contributor=="Satoshi Suzuki",]$Plot)){
  
  Mean_Temperature<-subset(temp,Plot==i)$Soil.temperature.during.experimental.period
  sd_Temperature = NA
  GSL_0 = NA
  GSL_5 = NA
  GDD_0 = NA
  GDD_5 = NA
  
  Out<-as.data.frame(cbind(i,Mean_Temperature,sd_Temperature,GSL_0,GSL_5,GDD_0,GDD_0))
  plot_temp_means<-rbind(plot_temp_means,Out)
}

plot_temp_means$source<-"summary soil data"
plot_temp_means$scale<-"Plot"
names(plot_temp_means)<-names(Plot_Variables_Soil)

Plot_Variables_Soil<-rbind(Plot_Variables_Soil,plot_temp_means)

plot_airtemp_means<-plot_temp_means
plot_airtemp_means[,c(2:8)]<-NA

Plot_Variables_Air<-rbind(Plot_Variables_Air,plot_airtemp_means)

#Southampton Island####
temp<-read.csv(file="scripts/users/hthomas/Data/Tea/Env.Vars/Southampton_Island/weather_data.csv")

#Add plot and date
temp$date<-as.POSIXct(temp$EDT,format="%d/%m/%Y")
temp$date<-temp$date + years(2000) #correct years
temp$Day<-cut(temp$date, "day") #Extract days
temp$Plot<-"EBM, Southampton Island, NU, Canada"

# #Plot data
# ggplot(temp,aes(date,Mean.TemperatureC))+
#   geom_point(size=0.1)+
#   geom_smooth()+
#   theme_bw()

#Daily means
daily_temp<- temp %>%
  group_by(date,Plot) %>%
  summarise(Daily_mean = mean(Mean.TemperatureC))

#Plot means
#Trim data to dates of tea burial
plot_temp_means=NULL
for(i in unique(daily_temp$Plot)){
  DOB<-min(tea[tea$Plot==i,]$Burial) #Find date of burial
  DOR<-max(tea[tea$Plot==i,]$Recovery) #Find date of recovery
  daily_temp_cut<-subset(daily_temp,date>=DOB&date<=DOR) #Trim data
  
  Out<- daily_temp_cut %>%
    group_by(Plot) %>%
    summarise(Mean_Temperature = mean(Daily_mean,na.rm=T),
              sd_Temperature = sd(Daily_mean,na.rm=T),
              GSL_0 = length(which(Daily_mean > 0)),
              GSL_5 = length(which(Daily_mean > 5)),
              GDD_0 = sum(Daily_mean[Daily_mean>0],na.rm=T),
              GDD_5 = sum(Daily_mean[Daily_mean>5],na.rm=T))
  
  plot_temp_means<-rbind(plot_temp_means,Out)
}
plot_temp_means$source<-"Online weather station"
plot_temp_means$scale<-"Region"

Plot_Variables_Air<-rbind(Plot_Variables_Air,plot_temp_means)

plot_soiltemp_means<-plot_temp_means
plot_soiltemp_means[,c(2:9)]<-NA

Plot_Variables_Soil<-rbind(Plot_Variables_Soil,plot_soiltemp_means)

#QHI (Isla)####
temp<-read.csv("scripts/users/hthomas/Data/Tea/Env.Vars/QHI_Phenology/QHI_Phenology_asText.csv")

temp$date<-as.character(substr(temp$Date,1,14))
temp$date<-as.POSIXct(temp$date,format="%m/%d/%Y %H:%M")
temp$date<-temp$date + years(2000) #correct years
temp$Day<-cut(temp$date, "day") #Extract days

temp2<-read.csv("scripts/users/hthomas/Data/Tea/Env.Vars/QHI_Phenology/QHI_Phenology_asDate.csv")

temp2$date<-as.POSIXct(temp2$Date,format="%m/%d/%Y %H:%M")
temp2$date<-temp2$date + years(2000) #correct years
temp2$Day<-cut(temp2$date, "day") #Extract days

temp<-rbind(temp,temp2)[,c(11,12,3:6)]

temp<-melt(temp,id.vars = c("date","Day"))
temp$value<-as.numeric(as.character(temp$value))
names(temp)[3]<-"location"
temp$Day<-as.Date(temp$Day)

#Plot data
# ggplot(temp,aes(Day,value,colour=factor(location)))+
#   geom_point(size=0.1)+
#   geom_smooth()+
#   theme_bw()

#Daily means
daily_temp<- temp %>%
  group_by(Day,location) %>%
  summarise(Daily_mean = mean(value))

#Clip to data
plot_temp_means=NULL
for(i in unique(tea[substr(tea$Plot,1,3)=="QHI",]$Plot)){
  DOB<-min(tea[tea$Plot==i,]$Burial) #Find date of burial
  DOR<-max(tea[tea$Plot==i,]$Recovery) #Find date of recovery
  daily_temp_cut<-subset(daily_temp,Day>=DOB&Day<=DOR) #Trim data
  
  Out<- daily_temp_cut %>%
    group_by(location) %>%
    summarise(Mean_Temperature = mean(Daily_mean,na.rm=T),
              sd_Temperature = sd(Daily_mean,na.rm=T),
              GSL_0 = length(which(Daily_mean > 0)),
              GSL_5 = length(which(Daily_mean > 5)),
              GDD_0 = sum(Daily_mean[Daily_mean>0],na.rm=T),
              GDD_5 = sum(Daily_mean[Daily_mean>5],na.rm=T))
  
  Out$Plot=i
  plot_temp_means<-rbind(plot_temp_means,Out)
}

plot_temp_means$source<-"HOBO"
plot_temp_means$scale<-"Site"

plot_soiltemp_means<-subset(plot_temp_means,location == "Soil")[,-1]
Plot_Variables_Soil<-rbind(Plot_Variables_Soil,plot_soiltemp_means)

plot_airtemp_means<-subset(plot_temp_means,location == "Air")[,-1]
Plot_Variables_Air<-rbind(Plot_Variables_Air,plot_airtemp_means)

#QHI (Isla) - Moisture####
temp<-read.csv(file="scripts/users/hthomas/Data/Tea/Env.Vars/QHI_Moisture/QHI_Moisture.csv")

temp$Plot<-paste(temp$Site,temp$Sub.site,sep="_")

#Plot means
plot_moisture_means <- temp %>%
  group_by(Plot) %>%
  summarise(Mean_moisture = mean(Soil.moisture),
            sd_moisture = sd(Soil.moisture)) %>%
  filter(!is.na(Mean_moisture))

plot_moisture_means$scale<-"Plot"
plot_moisture_means$source<-"Moisture Probe"

Plot_Variables_Moisture<-rbind(Plot_Variables_Moisture,plot_moisture_means)

#Disko Island - Soil temp and moisture
temp<-read.csv(file="scripts/users/hthomas/Data/Tea/Env.Vars/Disko_Island/Soil_temp_moisture.csv")
temp<-melt(temp, 
            variable.name = "Plot",
            value.names = "value",
            id.vars = c("Site_name"))

#Moisture
plot_moisture_means<-subset(temp,Site_name=="Soil_moisture")[,c(2,3)]
names(plot_moisture_means)[2]<-"Mean_moisture"
plot_moisture_means$sd_moisture<-0
plot_moisture_means$scale<-"Plot"
plot_moisture_means$source<-"Moisture Probe"

Plot_Variables_Moisture<-rbind(Plot_Variables_Moisture,plot_moisture_means)

#Soil Temp
plot_soiltemp_means<-subset(temp,Site_name=="Soil_temp")[,c(2,3)]
names(plot_soiltemp_means)[2]<-"Mean_Temperature"
plot_soiltemp_means$sd_Temperature<-0
plot_soiltemp_means$GSL_0<-NA
plot_soiltemp_means$GSL_5<-NA
plot_soiltemp_means$GDD_0<-NA
plot_soiltemp_means$GDD_5<-NA
plot_soiltemp_means$scale<-"Plot"
plot_soiltemp_means$source<-"iButton"

Plot_Variables_Soil<-rbind(Plot_Variables_Soil,plot_soiltemp_means)

#Air Temp
temp<-read.csv(file="scripts/users/hthomas/Data/Tea/Env.Vars/Disko_Island/air_temp.csv")
temp$Date<-as.POSIXct(temp$Date,format="%d/%m/%y")

# #Plot data
# ggplot(temp,aes(Date,Air_temp))+
#   geom_point(size=0.1)+
#   geom_smooth()+
#   theme_bw()

#Clip to data
plot_temp_means=NULL
for(i in unique(tea[tea$Contributor=="Casper Tai Christianen",]$Plot)){
  DOB<-min(tea[tea$Plot==i,]$Burial) #Find date of burial
  DOR<-max(tea[tea$Plot==i,]$Recovery) #Find date of recovery
  daily_temp_cut<-subset(temp,Date>=DOB&Date<=DOR) #Trim data
  
  Out<- daily_temp_cut %>%
    summarise(Mean_Temperature = mean(Air_temp,na.rm=T),
              sd_Temperature = sd(Air_temp,na.rm=T),
              GSL_0 = length(which(Air_temp > 0)),
              GSL_5 = length(which(Air_temp > 5)),
              GDD_0 = sum(Air_temp[Air_temp>0],na.rm=T),
              GDD_5 = sum(Air_temp[Air_temp>5],na.rm=T))
  
  Out$Plot=i
  plot_temp_means<-rbind(plot_temp_means,Out)
}

plot_temp_means$source<-"Weather Station"
plot_temp_means$scale<-"Region"

Plot_Variables_Air<-rbind(Plot_Variables_Air,plot_temp_means)

#Abisko - air temp####
temp<-read.csv(file="scripts/users/hthomas/Data/Tea/Env.Vars/Abisko_Air/Air_Temps.csv")
temp$Date<-as.POSIXct(temp$Date)

# #Plot data
# ggplot(temp,aes(Date,AirTemperature))+
#   geom_point(size=0.1)+
#   geom_smooth()+
#   theme_bw()

#Find Abisko plots with missing air
tea.abisko<-tea %>%
  filter(Region == "Abisko"| Region == "Abisko 2" | Region == "Latnja")
tea.abisko<-as.data.frame(unique(tea.abisko$Plot))
tea.abisko$Air_Temp<-Plot_Variables_Air$Mean_Temperature[match(tea.abisko$`unique(tea.abisko$Plot)`,Plot_Variables_Air$Plot)]

missing.abisko.air<-subset(tea.abisko,is.na(Air_Temp))[1]
names(missing.abisko.air)<-"Plot"

plot_temp_means=NULL
for(i in unique(missing.abisko.air$Plot)){
  DOB<-min(tea[tea$Plot==i,]$Burial) #Find date of burial
  DOR<-max(tea[tea$Plot==i,]$Recovery) #Find date of recovery
  daily_temp_cut<-subset(temp,Date>=DOB&Date<=DOR) #Trim data
  
  Out<- daily_temp_cut %>%
    summarise(Mean_Temperature = mean(AirTemperature,na.rm=T),
              sd_Temperature = sd(AirTemperature,na.rm=T),
              GSL_0 = length(which(AirTemperature > 0)),
              GSL_5 = length(which(AirTemperature > 5)),
              GDD_0 = sum(AirTemperature[AirTemperature>0],na.rm=T),
              GDD_5 = sum(AirTemperature[AirTemperature>5],na.rm=T))
  
  Out$Plot=i
  plot_temp_means<-rbind(plot_temp_means,Out)
}

plot_temp_means$source<-"Weather Station"
plot_temp_means$scale<-"Region"

Plot_Variables_Air<-rbind(Plot_Variables_Air,plot_temp_means)

#Finland####

#AIR#
temp<-read.csv(file="scripts/users/hthomas/Data/Tea/Env.Vars/Finland/air_temp_logger.csv")

temp<-filter(temp, !is.na(air_temp))
temp$site<-as.factor(temp$site)

temp$date<-as.POSIXct(temp$date_time)
temp$Day<-cut(temp$date, "day") #Extract days
temp$Day<-as.Date(temp$Day)

#Daily means
daily_temp<- temp %>%
  group_by(Day,site) %>%
  summarise(Daily_mean = mean(air_temp))

# #Plot data
# ggplot(daily_temp,aes(Day,Daily_mean, colour = site))+
#   geom_point(size=0.1)+
#   geom_smooth()+
#   theme_classic()+
#   theme(legend.position = "none")

#For winter incubations (temp):
temp_winter<-daily_temp
temp_winter$Plot<-paste0("FIN_",temp_winter$site,"_Winter")

temp_year<-daily_temp
temp_winter$Plot<-paste0("FIN_",temp_winter$site,"_Year")

temp<-rbind(temp_winter,temp_year)


plot_temp_means<-NULL
for(i in unique(tea[tea$Region=="Finland",]$Plot)){
  temp_cut<-subset(temp, Plot == i)
  DOB<-min(tea[tea$Plot==i,]$Burial) #Find date of burial
  DOR<-max(tea[tea$Plot==i,]$Recovery) #Find date of recovery
  daily_temp_cut<-subset(temp_cut,Day>=DOB&Day<=DOR) #Trim data
  
  Out<- daily_temp_cut %>%
    ungroup() %>%
    summarise(Mean_Temperature = mean(Daily_mean,na.rm=T),
              sd_Temperature = sd(Daily_mean,na.rm=T),
              GSL_0 = length(which(Daily_mean > 0)),
              GSL_5 = length(which(Daily_mean > 5)),
              GDD_0 = sum(Daily_mean[Daily_mean>0],na.rm=T),
              GDD_5 = sum(Daily_mean[Daily_mean>5],na.rm=T))
  
  Out$Plot=i
  plot_temp_means<-rbind(plot_temp_means,Out)
}

plot_temp_means$source<-"Temp_logger"
plot_temp_means$scale<-"Plot"

Plot_Variables_Air<-rbind(Plot_Variables_Air,plot_temp_means)

#SOIL#
temp<-read.csv(file="scripts/users/hthomas/Data/Tea/Env.Vars/Finland/soil_temp_logger.csv")

temp<-filter(temp, !is.na(soil_temp))
temp$site<-as.factor(temp$site)

temp$date<-as.POSIXct(temp$date_time, format = "%d.%m.%Y")
temp$Day<-cut(temp$date, "day") #Extract days
temp$Day<-as.Date(temp$Day)

#Daily means
daily_temp<- temp %>%
  group_by(Day,site) %>%
  summarise(Daily_mean = mean(soil_temp))

# #Plot data
# ggplot(daily_temp,aes(Day,Daily_mean, colour = site))+
#   geom_point(size=0.1)+
#   geom_smooth()+
#   theme_classic()+
#   theme(legend.position = "none")

#For winter incubations (temp):
temp_winter<-daily_temp
temp_winter$Plot<-paste0("FIN_",temp_winter$site,"_Winter")

temp_year<-daily_temp
temp_winter$Plot<-paste0("FIN_",temp_winter$site,"_Year")

temp<-rbind(temp_winter,temp_year)

plot_temp_means<-NULL
for(i in unique(tea[tea$Region=="Finland",]$Plot)){
  temp_cut<-subset(temp, Plot == i)
  DOB<-min(tea[tea$Plot==i,]$Burial)+years(2000) #Find date of burial
  DOR<-max(tea[tea$Plot==i,]$Recovery)+years(2000) #Find date of recovery
  daily_temp_cut<-subset(temp_cut,Day>=DOB&Day<=DOR) #Trim data
  
  Out<- daily_temp_cut %>%
    ungroup() %>%
    summarise(Mean_Temperature = mean(Daily_mean,na.rm=T),
              sd_Temperature = sd(Daily_mean,na.rm=T),
              GSL_0 = length(which(Daily_mean > 0)),
              GSL_5 = length(which(Daily_mean > 5)),
              GDD_0 = sum(Daily_mean[Daily_mean>0],na.rm=T),
              GDD_5 = sum(Daily_mean[Daily_mean>5],na.rm=T))
  
  Out$Plot=i
  plot_temp_means<-rbind(plot_temp_means,Out)
}

plot_temp_means$source<-"Temp_logger"
plot_temp_means$scale<-"Plot"

Plot_Variables_Soil<-rbind(Plot_Variables_Soil,plot_temp_means)

#Soil moisture
temp<-read.csv(file="scripts/users/hthomas/Data/Tea/Env.Vars/Finland/metadata_v3.csv")

temp_moisture<-select(temp,site, soil_moisture_mean, soil_moisture_stdev)
#Winter
temp_moisture_w<-temp_moisture
temp_moisture_w$Plot<-paste0(temp_moisture$site,"_Winter")
#Year
temp_moisture_y<-temp_moisture
temp_moisture_y$Plot<-paste0(temp_moisture$site,"_Year")

plot_moisture_means<-rbind(temp_moisture_y,temp_moisture_w)
plot_moisture_means<-select(plot_moisture_means,Plot,Mean_moisture = soil_moisture_mean, sd_moisture = soil_moisture_stdev)
plot_moisture_means$source <- "Moisture Probe"
plot_moisture_means$scale = "Site"

Plot_Variables_Moisture<-rbind(Plot_Variables_Moisture,plot_moisture_means)



#Check against tea variables####
checking<-tea %>%
  select(Plot,Site,Contributor) %>%
  filter(!duplicated(Plot))

#Apply elevation correction factor####
#NOTE: Using - 0.005°C/m based on Damien's analysis

weather_station_meta<-read.csv("scripts/users/hthomas/Data/Tea/Env.Vars/Weather_Elevations.csv")

correction_plots<-select(subset(tea,Site%in%weather_station_meta$Site),Site,Plot,Elevation)
correction_plots$Site_ele<-as.numeric(as.character(weather_station_meta$Elevation[match(correction_plots$Site,weather_station_meta$Site)]))
correction_plots$Diff<-correction_plots$Site_ele-as.numeric(as.character(correction_plots$Elevation))
correction_plots$diff_temp<-correction_plots$Diff*0.005

correct<-subset(Plot_Variables_Air,Plot%in%correction_plots$Plot) 
Plot_Variables_Air<-subset(Plot_Variables_Air,Plot%notin%correction_plots$Plot) 

correct$diff_temp<-correction_plots$diff_temp[match(correct$Plot,correction_plots$Plot)]
correct$Mean_Temperature<-correct$Mean_Temperature+correct$diff_temp
correct<-correct[,-10]

Plot_Variables_Air<-rbind(Plot_Variables_Air,correct)



#Add variables
checking$Air<-Plot_Variables_Air$Mean_Temperature[match(checking$Plot,Plot_Variables_Air$Plot)]
checking$Soil<-Plot_Variables_Soil$Mean_Temperature[match(checking$Plot,Plot_Variables_Soil$Plot)]
checking$Moisture<-Plot_Variables_Moisture$Mean_moisture[match(checking$Plot,Plot_Variables_Air$Plot)]



#Find missing####
#Anything
missing<-subset(checking,is.na(Air)&is.na(Soil))

missing_air<-subset(checking,is.na(Air))

#Final export script####
write.csv(Plot_Variables_Soil,file="scripts/users/hthomas/Data/Tea/Env.Vars/Soil_Temps.csv")
write.csv(Plot_Variables_Air,file="scripts/users/hthomas/Data/Tea/Env.Vars/Air_Temps.csv")
write.csv(Plot_Variables_Moisture,file="scripts/users/hthomas/Data/Tea/Env.Vars/Moisture.csv")

