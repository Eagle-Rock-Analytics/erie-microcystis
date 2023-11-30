#Owen Doherty - May 9, 2018
#Eagle Rock Analytics

#############################################
#Aggregate all sites into master array
#############################################

#Go to directory with raw data
setwd('/Users/owen/Documents/research/hab/korea_japan/ground_truth/compiled_dat')
list.files() #print list of data files


#load files
load('chesapeake_36.165N_75.988W_daily.Rdata')
load('chesapeake_36.915N_75.720W_daily.Rdata')
load('chesapeake_36.943N_76.329W_daily.Rdata')
names(which(sapply(.GlobalEnv, is.data.frame))) #print all loaded data.frames

#Merge data.frames by hand (tedious)
station.merge = merge(chesapeake_36.165N_75.988W.daily.df, chesapeake_36.915N_75.720W.daily.df, all=TRUE)
station.merge = merge(station.merge, chesapeake_36.943N_76.329W.daily.df, all=TRUE)

#Drop 2017 observations (no SST data for this)
station.merge$year <- format(station.merge$timedate,'%Y')
station.merge<-station.merge[!(station.merge$year == 2017),]
#############################################
#Compare to closest sat measurement
#############################################

#0: Load SST data
setwd('/Users/owen/Documents/research/hab/korea_japan/ground_truth/compiled_dat')
load("ches_sst.Rdata")


#define function to extract SST as function of date and station
extract_sst <- function(timedate,sst.lati,sst.loni){
  #extract array indicies from timedate (must be numeric)
  timedate<-as.POSIXct(timedate)
  year <- format(timedate,'%Y')
  doy <- as.numeric(strftime(timedate, format = "%j"))
  sst.lati<- as.numeric(sst.lati)
  sst.loni<- as.numeric(sst.loni)
  
  #2 pick the right file (i.e.) year & extract data
  if (year==2015){
    sst.2015[sst.loni,sst.lati,doy] 
  }else if (year==2016){
    sst.2016[sst.loni,sst.lati,doy] 
  }else if (year==2014){
    sst.2014[sst.loni,sst.lati,doy] 
  }else if (year==2013){
    sst.2013[sst.loni,sst.lati,doy] 
  }else if (year==2012){
    sst.2012[sst.loni,sst.lati,doy] 
  }else if (year==2011){
    sst.2011[sst.loni,sst.lati,doy] 
  }else if (year==2010){
    sst.2010[sst.loni,sst.lati,doy] 
  }else if (year==2009){
    sst.2009[sst.loni,sst.lati,doy]
  }else if (year==2008){
    sst.2008[sst.loni,sst.lati,doy]
  }else if (year==2007){
    sst.2007[sst.loni,sst.lati,doy]
  }else if (year==2006){
    sst.2006[sst.loni,sst.lati,doy]
  }else if (year==2005){
    sst.2005[sst.loni,sst.lati,doy]
  }
    
}

#FOR EACH OBSERVATION, append output it to the data.frame
station.merge$nearest.sst<-apply(station.merge,1,function(x) extract_sst(x['timedate'],x['sst.lati'],x['sst.loni']))

#############################################
#Export in Standard Format
#############################################

#Save data.frame to RData
setwd('/Users/owen/Documents/research/hab/korea_japan/ground_truth/compiled_dat')
save(station.merge,file="aggregated_chesapeake_all.RData")

#dynamic filename generation
# #open, grab, close
# ncid<-nc_open(fname,write=TRUE)
#   ncvar_get(ncid,"sst")[sst.loni,sst.lati,doy]
#   lat<-ncvar_get(ncid,"lat")
#   lon<-ncvar_get(ncid,"lon")
#   mask<-ncvar_get(ncid,"mask")
#   sst<-ncvar_get(ncid,"sst")
# nc_close(ncid)

#Test Plot
library(ggplot2)
p<-ggplot(station.merge,aes(x=nearest.sst,y=temp.dmin,color=format(timedate,'%Y')))
p+geom_point()+facet_grid(. ~ site)+
  geom_abline(intercept=0,slope=1,color="black")+
  ylab('Observation')+xlab('Satelite')+theme(legend.position = "bottom")+scale_color_discrete(name="year")

#Test Plot by Month
station.merge$mon <- as.factor(format(station.merge$timedate,'%B'))

p<-ggplot(subset(station.merge,site %in% "36.943N_76.329W"),aes(x=nearest.sst,y=temp.dmin,color=mon))
p+geom_point()+
  #facet_grid(. ~ site)+
  geom_abline(intercept=0,slope=1,color="black")+
  ylab('Observation')+xlab('Satelite')+theme(legend.position = "bottom")+scale_color_discrete(name="year")

#Test June
june.df <- subset(station.merge,site %in% "36.943N_76.329W")
june.df <- subset(june.df,mon %in% "June")
table(june.df$temp.dmin)
