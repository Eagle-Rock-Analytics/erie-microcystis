#Owen Doherty - March 27, 2018
#Eagle Rock Analytics

#Load data.frame to Environment
setwd('/Users/owen/Documents/research/hab/korea_japan/ground_truth/compiled_dat')
load(file="aggregated_all_stations.RData")

#For Andrew: Variable Naming Convention
#temp.dmean = Mean Daily Temp
#temp.dmedian = Median Daily Temp
#temp.dmax = Maximum Daily Temp
#temp.dmin = Minimum Daily Temp
#temp.std = Standard Deviation of Daily Temps
#temp.dkurt = Kurtosis of Daily Temps
#temp.skew = Skewness of Daily Temps
#timedate = POSIXct object of timestamp
#site = sitename
#sitetype = open_ocean, bay or pond
#lat = latitude of observation
#lon = longitude of observation
#sst.lat = index of satelite lat
#sst.lon = index of satelite lon
#nearest.sst = closest satelite SST observed temp
#NOTE: sometimes multiple pixels are close, southern/easternmost point selected in this case


#New sitetype of "shallow" for 105 and Robbins Island, reclassify flax_pond as a 'bay'
sta.shallow=c("bridge_105","robbins_island")
station.merge <- within(station.merge, sitetype[station.merge$site %in% sta.shallow] <-'shallow')
sta.bay=c("flax_pond")
station.merge <- within(station.merge, sitetype[station.merge$site %in% sta.bay] <-'bay')

#Remove cold season Jan - May, Nov & Dec
drop.months =c(1,2,3,4,5,11,12)
station.merge.gs<-station.merge[ which( ! station.merge$month %in% drop.months),]
unique(station.merge.gs$month) #test: shouldreturn only months in grown season

#Calculate measures of skill
station.merge.gs$mae=abs(station.merge.gs$temp.dmean-station.merge.gs$nearest.sst)
station.merge.gs$medae=abs(station.merge.gs$temp.dmedian-station.merge.gs$nearest.sst)
station.merge.gs$mede=station.merge.gs$nearest.sst-station.merge.gs$temp.dmedian

#bias by month and site
station.merge.gs$month <- as.numeric(format(station.merge.gs$timedate,'%m'))
bias.month.site.df<-aggregate(station.merge.gs[,'mede'], list(station.merge.gs[,'month'],station.merge.gs[,'site']), mean)
bias.month.sitetype.df<-aggregate(station.merge.gs[,'mede'], list(station.merge.gs[,'month'],station.merge.gs[,'sitetype']), mean)

#rename the variables after aggregation
names(bias.month.sitetype.df)[1]<-"month"
names(bias.month.sitetype.df)[2]<-"site"
names(bias.month.sitetype.df)[3]<-"bias"

#reshape data from long to wide format bias
library(tidyr)
bias.df<-spread(bias.month.sitetype.df,site,bias)

#create new data.frame for median absolute error & aggregate by sitetype & reshape
keepers = c("month","site","sitetype","medae")
error.all.df <- station.merge.gs[keepers]
medae.site.df<-aggregate(error.all.df[,'medae'], list(error.all.df[,'month'],error.all.df[,'site'],error.all.df[,'sitetype']), mean) #take mean across each sites by type and month
names(medae.site.df)[1]<-"month"
names(medae.site.df)[2]<-"site"
names(medae.site.df)[3]<-"sitetype"
names(medae.site.df)[4]<-"medae"

medae.long.df<-aggregate(medae.site.df[,'medae'], list(medae.site.df[,'month'],medae.site.df[,'sitetype']), mean) #aggregate over each sitetype
names(medae.long.df)[1]<-"month"
names(medae.long.df)[2]<-"sitetype"
names(medae.long.df)[3]<-"medae"
medae.df<-spread(medae.long.df,sitetype,medae) #reshape data medae data frame

#make a pretty table for bias
library(kable)
kable(bias.df)

kable(medae.df)

#make a pretty table for median absolute error

#optional code
#Remove Shallow Locations
#removed 105 bridge, robbbins island, and Flax
#sta.remove=c("bridge_105","robbins_island","flax_pond")
#station.merge.sub<-station.merge[ which( ! station.merge$site %in% sta.remove),]

#Dummy Plot Examples
library(ggplot2)

#Median Daily Temp vs. Satellite
p<-ggplot(station.merge.gs,aes(x=nearest.sst,y=temp.dmedian,color=format(timedate,'%Y')))
p+geom_point()+facet_grid(. ~ site)+
  geom_abline(intercept=0,slope=1,color="black")+
  ylab('Median in-situ temperature (°C)')+xlab('Satelite-based temperature (°C) ')+theme(legend.position = "bottom")+scale_color_discrete(name="year")

#Minimum Daily Temp vs. Satellite
p<-ggplot(station.merge.gs,aes(x=nearest.sst,y=temp.dmin,color=format(timedate,'%Y')))
p+geom_point()+facet_grid(. ~ site)+
  geom_abline(intercept=0,slope=1,color="black")+
  ylab('Min Daily in-situ temperature (°C)')+xlab('Satelite-based temperature (°C) ')+theme(legend.position = "bottom")+scale_color_discrete(name="year")

#Median Daily Temp vs. Satellite as Function of Type of Site
p<-ggplot(station.merge.gs,aes(x=nearest.sst,y=temp.dmedian,color=site))
p+geom_point()+facet_grid(. ~ sitetype)+
  geom_abline(intercept=0,slope=1,color="black")+
  ylab('Median in-situ temperature (°C)')+xlab('Satelite-based temperature (°C) ')+theme(legend.position = "bottom")

#Add correlation co-efficient to plots
library(plyr)

lm_eqn = function(station.merge.gs){
  m = lm(nearest.sst ~ temp.dmedian, station.merge.gs);
  eq <- substitute(~~R^2~"="~r2, 
                   list(r2 = format(summary(m)$r.squared, digits = 4)))
  as.character(as.expression(eq));                 
}

eq <- ddply(station.merge.gs,.(sitetype),lm_eqn)


by_type<-ggplot(station.merge.gs,aes(x=nearest.sst,y=temp.dmedian,color=site))+
  geom_point()+facet_grid(. ~ sitetype)+
  theme_bw()+
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
  geom_abline(intercept=0,slope=1,color="black")+
  ylab('Land-based temperature (°C)')+xlab('Satelite-based temperature (°C) ')+theme(legend.position = "bottom")+
  geom_text(data=eq,aes(x = 5, y = 23,label=V1), parse = TRUE, inherit.aes=FALSE) + facet_grid(.~sitetype)

by_type