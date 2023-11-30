#Owen Doherty - April 4, 2018
#Eagle Rock Analytics

#############################################
#Load Data - accounting for wierd file types
#############################################

#Go to directory with raw data
setwd('/Users/owen/Documents/research/hab/korea_japan/ground_truth/raw_dat')
list.files() #print list of data files

library("readxl")
ches <- read_excel("chesapeake_36.915N_75.720W.xlsx",na='NA')
station_name = "chesapeake_36.915N_75.720W"

ches_in <- data.frame(ches$yr,ches$mo,ches$day,ches$hr,ches$wtmp)
names(ches_in)[1]<-"year"
names(ches_in)[2]<-"month"
ches_in$month<-sprintf("%02d",ches_in$month) #pad to make sure two digit dates
names(ches_in)[3]<-"day"
ches_in$day<-sprintf("%02d",ches_in$day) #pad to make sure two digit dates
names(ches_in)[4]<-"hour"
ches_in$hour<-sprintf("%02d",ches_in$hour) #pad to make sure two digit dates
names(ches_in)[5]<-"temp"

ches_in$datetime <- paste(paste(ches_in$year,ches_in$month,ches_in$day,sep="-"),paste(ches_in$hour,ches_in$minute,sep=":"),sep=" ")
ches_in$datetime <- as.POSIXct(ches_in$datetime)

#############################################
#Calculate Daily Aggregates
#############################################
#add daily timestamp variable
ches_in$uniday <- format(ches_in$datetime,'%m-%d-%Y')

#load functions to calculate skewness etc
library(moments)

#aggregate by date (1 value per day)
ches.daily <- aggregate(ches_in['temp'],list(ches_in$uniday),FUN = function(x) c(dmean=mean(x,na.rm=TRUE),dmedian=median(x,na.rm=TRUE),dmax=max(x,na.rm=TRUE),dmin=min(x,na.rm=TRUE),dstd=sd(x,na.rm=TRUE),dkurt=kurtosis(x,na.rm=TRUE),dskew=skewness(x,na.rm=TRUE)))
ches.daily.df <- do.call(data.frame,ches.daily) 

#fix time stamp after aggregating
ches.daily.df$timedate<-as.POSIXct(ches.daily.df$Group.1,format='%m-%d-%Y')
ches.daily.df <- subset(ches.daily.df, select= -c(Group.1)) #drop the date as formated value

#old code for reference
#flax.max <- aggregate(flax_pond[,5],list(flax_pond$uniday),max)
#str(do.call(data.frame,aggregate(flax_pond[,5],list(flax_pond$uniday),FUN = function(x) c(dmean=mean(x),dmedian=median(x),dmax=max(x),dmin=min(x),dstd=sd(x),dkurt=kurtosis(x),dskew=skewness(x)))))

#############################################
#Add meta data
#############################################
site.lat <- 36.915
site.lon <- -75.720

#Add identifying variables (site, sitetype)
ches.daily.df$site <-rep("36.915N_75.720W",nrow(ches.daily.df))
ches.daily.df$sitetype <-rep("TBD",nrow(ches.daily.df))
ches.daily.df$lat <-rep(site.lat,nrow(ches.daily.df))
ches.daily.df$lon <-rep(site.lon,nrow(ches.daily.df))

############
# Find nearest point with SST data available
############
#Dummy dataset for nearest station ID

library("ncdf4") # Load the packages:

fname <- '/Volumes/data1/li-hr/jpl-ches-hr-2016.nc'
#open, grab, close
ncid<-nc_open(fname,write=TRUE)
lat<-ncvar_get(ncid,"lat")
lon<-ncvar_get(ncid,"lon")
mask<-ncvar_get(ncid,"mask")
nc_close(ncid)

#arrays of distance from station
dist.x <- abs(lon-site.lon)
dist.y <- abs(lat-site.lat)

#Create a matrix of distance (in 0.01 degrees) from station
dist.mat <- array(dim=c(224,285)) #create a matrix size of SST data
for (k in 1:285){
  for (i in 1:224){
    dist.mat[i,k]=dist.x[i]+dist.y[k] #this works as each step is 0.01 degrees lat/lon
  }
}

#return the row and column index of closest non-land point (i.e. mask isn't 2)
pair.return<-which(dist.mat == min(dist.mat[mask!=2]), arr.ind=TRUE) #first row of pair.return is most northerly

#right now only includes the first (northern most) of potentially multiple equally spaced locations from station
ches.daily.df$sst.lati<-rep(pair.return[1,2],nrow(ches.daily.df))
ches.daily.df$sst.loni<-rep(pair.return[1,1],nrow(ches.daily.df))

#############################################
#Export in Standard Format
#############################################

#Save data.frame to RData
setwd('/Users/owen/Documents/research/hab/korea_japan/ground_truth/compiled_dat')

#Rename outgoing variable to include station_name
dfout=paste0(station_name,".daily.df")
assign(dfout,ches.daily.df)

#Name output data file based on station
fout=paste0(station_name,"_daily.Rdata")
save(list=eval(dfout),file=fout)
#save(dfout,file=fout)