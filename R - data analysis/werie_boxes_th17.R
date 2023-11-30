## Owen Doherty: Eagle Rock Analytics
## August 8, 2018
## Updated by Grace Di Cecco, 1/11/23
## Test for 17 C threshold

# Input directory
dirin = 'C:/Users/Grace Di Cecco/OneDrive - Eagle Rock Analytics/Desktop/Postdoc-EagleRock/hab_proj/HAB_code/output_data'
setwd(dirin)

# Graphics directory
dirout = 'C:/Users/Grace Di Cecco/OneDrive - Eagle Rock Analytics/Desktop/Postdoc-EagleRock/hab_proj/HAB_code/pdf'



#Load netcdf
library(fields)
library(ncdf4)
library(chron)

nc = nc_open('annual.micro.werie.th17.nc', write=FALSE, readunlim =FALSE)
year <- ncvar_get(nc, "year")
year <- paste(year,"-06-30",sep="")
yearp <- as.POSIXct(year)
time <- as.Date(year)
tend=length(year)

lat <- ncvar_get(nc, "lat")
lon <- ncvar_get(nc, "lon")
#start_day <-ncvar_get(nc, "start_day_med")

#----------------------
#Part 1: Mean by Domain
#----------------------
txtout=paste('calculating mean LST over subdomains for year ',year,sep=' ')
print(txtout)


#Define Subdomains:
#Maumee Bay: East = -83.33, North = 41.75, West = -83.49, South = 41.68
bbox=c(-83.49,41.68,-83.33,41.75)
xindicesinwindow=which(lon>bbox[1]&lon<bbox[3]);
mb.xmin=min(xindicesinwindow)
mb.xmax=max(xindicesinwindow)
xcount=(mb.xmax-mb.xmin)+1; # needs to be at least 1

yindicesinwindow=which(lat>bbox[2]&lat<=bbox[4]);
mb.ymin=min(yindicesinwindow)
mb.ymax=max(yindicesinwindow)
ycount=(mb.ymax-mb.ymin)+1;# needs to be at least 1

print(paste("Maumee Bay Domain:",bbox[1],bbox[2],bbox[3],bbox[4]));# <== print bbox in indices
print(paste("Maumee Bay Indices:",mb.xmin,mb.ymin,mb.xmax,mb.ymax));# <== print bbox in indices

#variables are size (year,lon,lat)
# mb.start_arr = c(1,mb.xmin,mb.ymin)
# mb.end_arr = c(tend,mb.xmax,mb.ymax)
# mb.count_arr = (mb.end_arr - mb.start_arr) + 1

#variables are size (lon,lat,year)
mb.start_arr = c(mb.xmin,mb.ymin,1)
mb.end_arr = c(mb.xmax,mb.ymax,tend)
mb.count_arr = (mb.end_arr - mb.start_arr) + 1
#mb_lst = sst.sub[xmin:xmax,ymin:ymax,] #create a subset
#maumee_bay_daymean[1:365,i] = apply(mb_lst, 3, mean, na.rm=TRUE) #take mean over lon and lat

#Western Lake Erie: East = -82.54, North = 42.08, West = -83.49, South = 41.40    
bbox=c(-83.49,41.40,-82.54,42.08)
xindicesinwindow=which(lon>bbox[1]&lon<bbox[3]);
we.xmin=min(xindicesinwindow)
we.xmax=max(xindicesinwindow)
xcount=(we.xmax-we.xmin)+1; # needs to be at least 1

yindicesinwindow=which(lat>bbox[2]&lat<bbox[4]);
we.ymin=min(yindicesinwindow)
we.ymax=max(yindicesinwindow)
ycount=(we.ymax-we.ymin)+1;# needs to be at least 1

print(paste("Western Lake Erie Domain:",bbox[1],bbox[2],bbox[3],bbox[4]));# <== print bbox in indices
print(paste("Western Lake Erie Indices:",we.xmin,we.ymin,we.xmax,we.ymax));# <== print bbox in indices

#variables are size (lon,lat,year)
we.start_arr = c(we.xmin,we.ymin,1)
we.end_arr = c(we.xmax,we.ymax,tend)
we.count_arr = (we.end_arr - we.start_arr) + 1
#werie_lst = sst.sub[xmin:xmax,ymin:ymax,] #create a subset
#werie_daymean[1:365,i] = apply(werie_lst, 3, mean, na.rm=TRUE) #take mean over lon and lat

#Sandusky Bay: East = -82.54, North = 42.08, West = -83.49, South = 41.40    
bbox=c(-83.10,41.41,-82.70,41.52)
xindicesinwindow=which(lon>bbox[1]&lon<bbox[3]);
sb.xmin=min(xindicesinwindow)
sb.xmax=max(xindicesinwindow)
xcount=(sb.xmax-sb.xmin)+1; # needs to be at least 1

yindicesinwindow=which(lat>bbox[2]&lat<bbox[4]);
sb.ymin=min(yindicesinwindow)
sb.ymax=max(yindicesinwindow)
ycount=(sb.ymax-sb.ymin)+1;# needs to be at least 1

print(paste("Sandusky Bay Domain:",bbox[1],bbox[2],bbox[3],bbox[4]));# <== print bbox in indices
print(paste("Sandusky Bay Indices:",sb.xmin,sb.ymin,sb.xmax,sb.ymax));# <== print bbox in indices

#variables are size (lon,lat,year)
sb.start_arr = c(sb.xmin,sb.ymin,1)
sb.end_arr = c(sb.xmax,sb.ymax,tend)
sb.count_arr = (sb.end_arr - sb.start_arr) + 1

#werie_lst = sst.sub[xmin:xmax,ymin:ymax,] #create a subset
#werie_daymean[1:365,i] = apply(werie_lst, 3, mean, na.rm=TRUE) #take mean over lon and lat


txtout=paste('identified geographic domains boxes and indicies ')
print(txtout)

## -------------------------------------------
## Subset #1: First Day
## -------------------------------------------

#Test to make sure indicies line up:
# "Maumee Bay Domain: -83.49 41.68 -83.33 41.75"
# "Maumee Bay Indices: 22 43 38 49"
# ncvar_get(nc, "lat", start=43, count=8)
# ncvar_get(nc, "lon", start=22, count=17)
# ncvar_get(nc, "year", start=1, count=15)

#Maumee Bay: Load All
first_lo_mb <- ncvar_get(nc,varid="mic.lo.first_day",start=mb.start_arr, count=mb.count_arr)
first_hi_mb <- ncvar_get(nc,varid="mic.hi.first_day",start=mb.start_arr, count=mb.count_arr)
first_med_mb <- ncvar_get(nc,varid="mic.med.first_day",start=mb.start_arr, count=mb.count_arr)

#Maumee Bay: Spatial Mean
first.mb<-data.frame(apply(first_med_mb, 3, function(x) mean(x,na.rm=TRUE)))
colnames(first.mb) <- c("med")
first.mb$hi<-apply(first_hi_mb, 3, function(x) mean(x,na.rm=TRUE))
first.mb$lo<-apply(first_lo_mb, 3, function(x) mean(x,na.rm=TRUE))
first.mb$year<-yearp

#Western Lake Erie: Load All
first_lo_we <- ncvar_get(nc,varid="mic.lo.first_day",start=we.start_arr, count=we.count_arr)
first_hi_we <- ncvar_get(nc,varid="mic.hi.first_day",start=we.start_arr, count=we.count_arr)
first_med_we <- ncvar_get(nc,varid="mic.med.first_day",start=we.start_arr, count=we.count_arr)

#Western Lake Erie: Spatial Mean
first.we<-data.frame(apply(first_med_we, 3, function(x) mean(x,na.rm=TRUE)))
colnames(first.we) <- c("med")
first.we$hi<-apply(first_hi_we, 3, function(x) mean(x,na.rm=TRUE))
first.we$lo<-apply(first_lo_we, 3, function(x) mean(x,na.rm=TRUE))
first.we$year<-yearp

#Sandusky Bay: Load All
first_lo_sb <- ncvar_get(nc,varid="mic.lo.first_day",start=sb.start_arr, count=sb.count_arr)
first_hi_sb <- ncvar_get(nc,varid="mic.hi.first_day",start=sb.start_arr, count=sb.count_arr)
first_med_sb <- ncvar_get(nc,varid="mic.med.first_day",start=sb.start_arr, count=sb.count_arr)

#Sandusky Bay: Spatial Mean
first.sb<-data.frame(apply(first_med_sb, 3, function(x) mean(x,na.rm=TRUE)))
colnames(first.sb) <- c("med")
first.sb$hi<-apply(first_hi_sb, 3, function(x) mean(x,na.rm=TRUE))
first.sb$lo<-apply(first_lo_sb, 3, function(x) mean(x,na.rm=TRUE))
first.sb$year<-yearp

## -------------------------------------------
## Subset #2: Last Day
## -------------------------------------------
#Maumee Bay: Load All
last_lo_mb <- ncvar_get(nc,varid="mic.lo.last_day",start=mb.start_arr, count=mb.count_arr)
last_hi_mb <- ncvar_get(nc,varid="mic.hi.last_day",start=mb.start_arr, count=mb.count_arr)
last_med_mb <- ncvar_get(nc,varid="mic.med.last_day",start=mb.start_arr, count=mb.count_arr)

#Maumee Bay: Spatial Mean
last.mb<-data.frame(apply(last_med_mb, 3, function(x) mean(x,na.rm=TRUE)))
colnames(last.mb) <- c("med")
last.mb$hi<-apply(last_hi_mb, 3, function(x) mean(x,na.rm=TRUE))
last.mb$lo<-apply(last_lo_mb, 3, function(x) mean(x,na.rm=TRUE))
last.mb$year<-yearp

#Western Lake Erie: Load All
last_lo_we <- ncvar_get(nc,varid="mic.lo.last_day",start=we.start_arr, count=we.count_arr)
last_hi_we <- ncvar_get(nc,varid="mic.hi.last_day",start=we.start_arr, count=we.count_arr)
last_med_we <- ncvar_get(nc,varid="mic.med.last_day",start=we.start_arr, count=we.count_arr)

#Western Lake Erie: Spatial Mean
last.we<-data.frame(apply(last_med_we, 3, function(x) mean(x,na.rm=TRUE)))
colnames(last.we) <- c("med")
last.we$hi<-apply(last_hi_we, 3, function(x) mean(x,na.rm=TRUE))
last.we$lo<-apply(last_lo_we, 3, function(x) mean(x,na.rm=TRUE))
last.we$year<-yearp

#Sandusky Bay: Load All
last_lo_sb <- ncvar_get(nc,varid="mic.lo.last_day",start=sb.start_arr, count=sb.count_arr)
last_hi_sb <- ncvar_get(nc,varid="mic.hi.last_day",start=sb.start_arr, count=sb.count_arr)
last_med_sb <- ncvar_get(nc,varid="mic.med.last_day",start=sb.start_arr, count=sb.count_arr)

#Sandusky Bay: Spatial Mean
last.sb<-data.frame(apply(last_med_sb, 3, function(x) mean(x,na.rm=TRUE)))
colnames(last.sb) <- c("med")
last.sb$hi<-apply(last_hi_sb, 3, function(x) mean(x,na.rm=TRUE))
last.sb$lo<-apply(last_lo_sb, 3, function(x) mean(x,na.rm=TRUE))
last.sb$year<-yearp

## -------------------------------------------
## Subset #3: Mean Growth Rate
## -------------------------------------------
#Maumee Bay: Load All
mgr_lo_mb <- ncvar_get(nc,varid="mic.lo.mean_annual_growth",start=mb.start_arr, count=mb.count_arr)
mgr_hi_mb <- ncvar_get(nc,varid="mic.hi.mean_annual_growth",start=mb.start_arr, count=mb.count_arr)
mgr_med_mb <- ncvar_get(nc,varid="mic.med.mean_annual_growth",start=mb.start_arr, count=mb.count_arr)

#Maumee Bay: Spatial Mean
mgr.mb<-data.frame(apply(mgr_med_mb, 3, function(x) mean(x,na.rm=TRUE)))
colnames(mgr.mb) <- c("med")
mgr.mb$hi<-apply(mgr_hi_mb, 3, function(x) mean(x,na.rm=TRUE))
mgr.mb$lo<-apply(mgr_lo_mb, 3, function(x) mean(x,na.rm=TRUE))
mgr.mb$year<-yearp

#Western Lake Erie: Load All
mgr_lo_we <- ncvar_get(nc,varid="mic.lo.mean_annual_growth",start=we.start_arr, count=we.count_arr)
mgr_hi_we <- ncvar_get(nc,varid="mic.hi.mean_annual_growth",start=we.start_arr, count=we.count_arr)
mgr_med_we <- ncvar_get(nc,varid="mic.med.mean_annual_growth",start=we.start_arr, count=we.count_arr)

#Western Lake Erie: Spatial Mean
mgr.we<-data.frame(apply(mgr_med_we, 3, function(x) mean(x,na.rm=TRUE)))
colnames(mgr.we) <- c("med")
mgr.we$hi<-apply(mgr_hi_we, 3, function(x) mean(x,na.rm=TRUE))
mgr.we$lo<-apply(mgr_lo_we, 3, function(x) mean(x,na.rm=TRUE))
mgr.we$year<-yearp

#Sandusky Bay: Load All
mgr_lo_sb <- ncvar_get(nc,varid="mic.lo.mean_annual_growth",start=sb.start_arr, count=sb.count_arr)
mgr_hi_sb <- ncvar_get(nc,varid="mic.hi.mean_annual_growth",start=sb.start_arr, count=sb.count_arr)
mgr_med_sb <- ncvar_get(nc,varid="mic.med.mean_annual_growth",start=sb.start_arr, count=sb.count_arr)

#Sandusky Bay: Spatial Mean
mgr.sb<-data.frame(apply(mgr_med_sb, 3, function(x) mean(x,na.rm=TRUE)))
colnames(mgr.sb) <- c("med")
mgr.sb$hi<-apply(mgr_hi_sb, 3, function(x) mean(x,na.rm=TRUE))
mgr.sb$lo<-apply(mgr_lo_sb, 3, function(x) mean(x,na.rm=TRUE))
mgr.sb$year<-yearp

## -------------------------------------------
## Subset #4: Growing Days
## -------------------------------------------
#Maumee Bay: Load All
gd_lo_mb <- ncvar_get(nc,varid="mic.lo.growing_days",start=mb.start_arr, count=mb.count_arr)
gd_hi_mb <- ncvar_get(nc,varid="mic.hi.growing_days",start=mb.start_arr, count=mb.count_arr)
gd_med_mb <- ncvar_get(nc,varid="mic.med.growing_days",start=mb.start_arr, count=mb.count_arr)

#Maumee Bay: Spatial Mean
gd.mb<-data.frame(apply(gd_med_mb, 3, function(x) mean(x,na.rm=TRUE)))
colnames(gd.mb) <- c("med")
gd.mb$hi<-apply(gd_hi_mb, 3, function(x) mean(x,na.rm=TRUE))
gd.mb$lo<-apply(gd_lo_mb, 3, function(x) mean(x,na.rm=TRUE))
gd.mb$year<-yearp

#Western Lake Erie: Load All
gd_lo_we <- ncvar_get(nc,varid="mic.lo.growing_days",start=we.start_arr, count=we.count_arr)
gd_hi_we <- ncvar_get(nc,varid="mic.hi.growing_days",start=we.start_arr, count=we.count_arr)
gd_med_we <- ncvar_get(nc,varid="mic.med.growing_days",start=we.start_arr, count=we.count_arr)

#Western Lake Erie: Spatial Mean
gd.we<-data.frame(apply(gd_med_we, 3, function(x) mean(x,na.rm=TRUE)))
colnames(gd.we) <- c("med")
gd.we$hi<-apply(gd_hi_we, 3, function(x) mean(x,na.rm=TRUE))
gd.we$lo<-apply(gd_lo_we, 3, function(x) mean(x,na.rm=TRUE))
gd.we$year<-yearp

#Sandusky Bay: Load All
gd_lo_sb <- ncvar_get(nc,varid="mic.lo.growing_days",start=sb.start_arr, count=sb.count_arr)
gd_hi_sb <- ncvar_get(nc,varid="mic.hi.growing_days",start=sb.start_arr, count=sb.count_arr)
gd_med_sb <- ncvar_get(nc,varid="mic.med.growing_days",start=sb.start_arr, count=sb.count_arr)

#Sandusky Bay: Spatial Mean
gd.sb<-data.frame(apply(gd_med_sb, 3, function(x) mean(x,na.rm=TRUE)))
colnames(gd.sb) <- c("med")
gd.sb$hi<-apply(gd_hi_sb, 3, function(x) mean(x,na.rm=TRUE))
gd.sb$lo<-apply(gd_lo_sb, 3, function(x) mean(x,na.rm=TRUE))
gd.sb$year<-yearp

## -------------------------------------------
## Subset #5: Growing Season
## -------------------------------------------
#Maumee Bay: Load All
gs_lo_mb <- ncvar_get(nc,varid="mic.lo.season_window",start=mb.start_arr, count=mb.count_arr)
gs_hi_mb <- ncvar_get(nc,varid="mic.hi.season_window",start=mb.start_arr, count=mb.count_arr)
gs_med_mb <- ncvar_get(nc,varid="mic.med.season_window",start=mb.start_arr, count=mb.count_arr)

#Maumee Bay: Spatial Mean
gs.mb<-data.frame(apply(gs_med_mb, 3, function(x) mean(x,na.rm=TRUE)))
colnames(gs.mb) <- c("med")
gs.mb$hi<-apply(gs_hi_mb, 3, function(x) mean(x,na.rm=TRUE))
gs.mb$lo<-apply(gs_lo_mb, 3, function(x) mean(x,na.rm=TRUE))
gs.mb$year<-yearp

#Western Lake Erie: Load All
gs_lo_we <- ncvar_get(nc,varid="mic.lo.season_window",start=we.start_arr, count=we.count_arr)
gs_hi_we <- ncvar_get(nc,varid="mic.hi.season_window",start=we.start_arr, count=we.count_arr)
gs_med_we <- ncvar_get(nc,varid="mic.med.season_window",start=we.start_arr, count=we.count_arr)

#Western Lake Erie: Spatial Mean
gs.we<-data.frame(apply(gs_med_we, 3, function(x) mean(x,na.rm=TRUE)))
colnames(gs.we) <- c("med")
gs.we$hi<-apply(gs_hi_we, 3, function(x) mean(x,na.rm=TRUE))
gs.we$lo<-apply(gs_lo_we, 3, function(x) mean(x,na.rm=TRUE))
gs.we$year<-yearp

#Sandusky Bay: Load All
gs_lo_sb <- ncvar_get(nc,varid="mic.lo.season_window",start=sb.start_arr, count=sb.count_arr)
gs_hi_sb <- ncvar_get(nc,varid="mic.hi.season_window",start=sb.start_arr, count=sb.count_arr)
gs_med_sb <- ncvar_get(nc,varid="mic.med.season_window",start=sb.start_arr, count=sb.count_arr)

#Sandusky Bay: Spatial Mean
gs.sb<-data.frame(apply(gs_med_sb, 3, function(x) mean(x,na.rm=TRUE)))
colnames(gs.sb) <- c("med")
gs.sb$hi<-apply(gs_hi_sb, 3, function(x) mean(x,na.rm=TRUE))
gs.sb$lo<-apply(gs_lo_sb, 3, function(x) mean(x,na.rm=TRUE))
gs.sb$year<-yearp

######
## Perform statistical trend tests
#####
library(trend)
mk.first.mb<-mk.test(first.mb$med)
mk.first.all<-mk.test(first.we$med)
mk.gd.mb<-mk.test(gd.mb$med)
mk.gd.all<-mk.test(gd.we$med)
mk.gs.mb<-mk.test(gs.mb$med)
mk.gs.all<-mk.test(gs.we$med)
mk.last.mb<-mk.test(last.mb$med)
mk.last.all<-mk.test(last.we$med)
mk.mgr.mb<-mk.test(mgr.mb$med)
mk.mgr.all<-mk.test(mgr.we$med)

#######
# Figure 1a: Maumee Bay Compare locations same threshold
#######
library(ggplot2)
library(scales)

theme_set(theme_classic())

#pdf(file='figure3b_ne_th17.pdf', width=6, height=4.5, onefile=FALSE)
p.mb.first<-ggplot(first.mb,aes(x=year))+
  geom_line(aes(y=med),color="black",group="identity")+
  #geom_smooth(aes(y=med),method = "lm")+ #comment out when statistically insignificant
  geom_point(aes(y=med),size=3,position=position_dodge(width=0.5))+
  geom_ribbon(aes(ymax=lo,ymin=hi),fill="grey10",alpha=0.3)+
  scale_x_datetime(labels = date_format("%Y"))+
  xlab("")+ ylab("Julian day")+ggtitle('First day')+
  annotate("text", x = as.POSIXct('2005',format='%Y'), y = 130, label = paste0("p = ", round(mk.first.mb$p.value, 2)))
  #geom_point(aes(group=station,color=station,shape=station,y=mean.med.growth),size=3,position=position_dodge(width=0.5))+
  #theme(axis.text.x = element_text(angle = 90, hjust = 0),text = element_text(size=12),panel.grid.major=element_blank(),panel.grid.minor=element_blank(),legend.position = "none")+
  #theme_set(theme_bw(base_size = 24, base_family = "Helvetica"))+
  #theme(axis.text.x = element_text(angle = 45, hjust = 0, vjust=0.5),panel.grid.major=element_blank(),panel.grid.minor=element_blank(),legend.position = "none")+
  #coord_cartesian(ylim = c(0.13, 0.29)) +
  #xlab("")+ ylab("First Day") + ggtitle("Maumee Bay")
  #scale_x_continuous(breaks = c(1982,1986,1990,1994,1998,2002,2006,2010,2014))+
  #scale_x_continuous(breaks = c(1982,1984,1986,1988,1990,1992,1994,1996,1998,2000,2002,2004,2006,2008,2010,2012,2014))+
  #ggtitle('a)') + theme(plot.title=element_text(hjust=0)) + 
  #scale_color_manual("Location",labels = c("NW Atlantic"), values = c("red"))+
  #scale_fill_manual("Location",labels = c("NW Atlantic"), values = c("red"))+
  #scale_shape_manual("Location",labels = c("NW Atlantic"), values = c(19))+
  #theme(strip.background = element_blank(),strip.text.y = element_blank(),legend.title=element_blank())+
  #facet_grid(station ~ . , labeller = as_labeller(region_names))
#dev.off()

#######
# Figure 1b: Western Lake Erie Compare locations same threshold
#######
library(ggplot2)
library(scales)
#pdf(file='figure3b_ne.pdf', width=6, height=4.5, onefile=FALSE)
p.we.first<-ggplot(first.we,aes(x=year))+
  geom_line(aes(y=med),color="black",group="identity")+
  #geom_smooth(aes(y=med),method = "lm")+ #comment out when trend insiginificant
  geom_point(aes(y=med),size=3,position=position_dodge(width=0.5))+
  geom_ribbon(aes(ymax=lo,ymin=hi),fill="grey10",alpha=0.3)+
  scale_x_datetime(labels = date_format("%Y"))+
  xlab("")+ ylab("Julian day")+ggtitle('First day')+
  annotate("text", x = as.POSIXct('2005',format='%Y'), y = 145, label = paste0("p = ", round(mk.first.all$p.value, 2)))
  #geom_point(aes(group=station,color=station,shape=station,y=mean.med.growth),size=3,position=position_dodge(width=0.5))+
  #theme(axis.text.x = element_text(angle = 90, hjust = 0),text = element_text(size=12),panel.grid.major=element_blank(),panel.grid.minor=element_blank(),legend.position = "none")+
  #theme_set(theme_bw(base_size = 24, base_family = "Helvetica"))+
  #theme(axis.text.x = element_text(angle = 45, hjust = 0, vjust=0.5),panel.grid.major=element_blank(),panel.grid.minor=element_blank(),legend.position = "none")+
  #coord_cartesian(ylim = c(0.13, 0.29)) +
  #xlab("")+ ylab("First Day") + ggtitle("Western Lake Erie")


#######
# Figure 2a: Last Day of Maumee Bay Compare locations same threshold
#######
library(ggplot2)
library(scales)
#pdf(file='figure3b_ne.pdf', width=6, height=4.5, onefile=FALSE)
p.mb.last<-ggplot(last.mb,aes(x=year))+
  geom_line(aes(y=med),color="black",group="identity")+
  # geom_smooth(aes(y=med),method = "lm")+
  geom_point(aes(y=med),size=3,position=position_dodge(width=0.5))+
  geom_ribbon(aes(ymax=lo,ymin=hi),fill="grey10",alpha=0.3)+
  scale_x_datetime(labels = date_format("%Y"))+
  xlab("")+ ylab("Julian day")+ggtitle('Last day')+
  annotate("text", x = as.POSIXct('2005',format='%Y'), y = 300, label = paste0("p = ", round(mk.last.mb$p.value, 2)))
  #geom_point(aes(group=station,color=station,shape=station,y=mean.med.growth),size=3,position=position_dodge(width=0.5))+
  #theme(axis.text.x = element_text(angle = 90, hjust = 0),text = element_text(size=12),panel.grid.major=element_blank(),panel.grid.minor=element_blank(),legend.position = "none")+
  #theme_set(theme_bw(base_size = 24, base_family = "Helvetica"))+
  #theme(axis.text.x = element_text(angle = 45, hjust = 0, vjust=0.5),panel.grid.major=element_blank(),panel.grid.minor=element_blank(),legend.position = "none")+
  #coord_cartesian(ylim = c(0.13, 0.29)) +
  #xlab("")+ ylab("Last Day") + ggtitle("Maumee Bay")
#scale_x_continuous(breaks = c(1982,1986,1990,1994,1998,2002,2006,2010,2014))+
#scale_x_continuous(breaks = c(1982,1984,1986,1988,1990,1992,1994,1996,1998,2000,2002,2004,2006,2008,2010,2012,2014))+
#ggtitle('a)') + theme(plot.title=element_text(hjust=0)) + 
#scale_color_manual("Location",labels = c("NW Atlantic"), values = c("red"))+
#scale_fill_manual("Location",labels = c("NW Atlantic"), values = c("red"))+
#scale_shape_manual("Location",labels = c("NW Atlantic"), values = c(19))+
#theme(strip.background = element_blank(),strip.text.y = element_blank(),legend.title=element_blank())+
#facet_grid(station ~ . , labeller = as_labeller(region_names))
#dev.off()

#######
# Figure 2b: Last Day of Western Lake Erie Compare locations same threshold
#######
library(ggplot2)
library(scales)
#pdf(file='figure3b_ne.pdf', width=6, height=4.5, onefile=FALSE)
p.we.last<-ggplot(last.we,aes(x=year))+
  geom_line(aes(y=med),color="black",group="identity")+
  # geom_smooth(aes(y=med),method = "lm")+
  geom_point(aes(y=med),size=3,position=position_dodge(width=0.5))+
  geom_ribbon(aes(ymax=lo,ymin=hi),fill="grey10",alpha=0.3)+
  scale_x_datetime(labels = date_format("%Y"))+
  xlab("")+ ylab("Julian day")+ggtitle('Last day')+
  annotate("text", x = as.POSIXct('2005',format='%Y'), y = 300, label = paste0("p = ", round(mk.last.all$p.value, 2)))
  #geom_point(aes(group=station,color=station,shape=station,y=mean.med.growth),size=3,position=position_dodge(width=0.5))+
  #theme(axis.text.x = element_text(angle = 90, hjust = 0),text = element_text(size=12),panel.grid.major=element_blank(),panel.grid.minor=element_blank(),legend.position = "none")+
  #theme_set(theme_bw(base_size = 24, base_family = "Helvetica"))+
  #theme(axis.text.x = element_text(angle = 45, hjust = 0, vjust=0.5),panel.grid.major=element_blank(),panel.grid.minor=element_blank(),legend.position = "none")+
  #coord_cartesian(ylim = c(0.13, 0.29)) +
  #xlab("")+ ylab("Last Day") + ggtitle("Western Lake Erie")


#######
# Figure 3a: Mean Growth Rate of Maumee Bay Compare locations same threshold
#######
library(ggplot2)
library(scales)
#pdf(file='figure3b_ne.pdf', width=6, height=4.5, onefile=FALSE)
p.mb.mgr<-ggplot(mgr.mb,aes(x=year))+
  geom_line(aes(y=med),color="black",group="identity")+
  geom_smooth(aes(y=med),method = "lm")+
  geom_point(aes(y=med),size=3,position=position_dodge(width=0.5))+
  geom_ribbon(aes(ymax=lo,ymin=hi),fill="grey10",alpha=0.3)+
  scale_x_datetime(labels = date_format("%Y"))+
  xlab("")+ ylab("Growth rate per day")+ggtitle('Growth rate')+
  annotate("text", x = as.POSIXct('2005',format='%Y'), y = 0.25, label = paste0("p = ", round(mk.mgr.mb$p.value, 3)))
  #geom_point(aes(group=station,color=station,shape=station,y=mean.med.growth),size=3,position=position_dodge(width=0.5))+
  #theme(axis.text.x = element_text(angle = 90, hjust = 0),text = element_text(size=12),panel.grid.major=element_blank(),panel.grid.minor=element_blank(),legend.position = "none")+
  #theme_set(theme_bw(base_size = 24, base_family = "Helvetica"))+
  #theme(axis.text.x = element_text(angle = 45, hjust = 0, vjust=0.5),panel.grid.major=element_blank(),panel.grid.minor=element_blank(),legend.position = "none")+
  #coord_cartesian(ylim = c(0.13, 0.29)) +
  #xlab("")+ ylab("Mean Growth Rate") + ggtitle("Maumee Bay")
#scale_x_continuous(breaks = c(1982,1986,1990,1994,1998,2002,2006,2010,2014))+
#scale_x_continuous(breaks = c(1982,1984,1986,1988,1990,1992,1994,1996,1998,2000,2002,2004,2006,2008,2010,2012,2014))+
#ggtitle('a)') + theme(plot.title=element_text(hjust=0)) + 
#scale_color_manual("Location",labels = c("NW Atlantic"), values = c("red"))+
#scale_fill_manual("Location",labels = c("NW Atlantic"), values = c("red"))+
#scale_shape_manual("Location",labels = c("NW Atlantic"), values = c(19))+
#theme(strip.background = element_blank(),strip.text.y = element_blank(),legend.title=element_blank())+
#facet_grid(station ~ . , labeller = as_labeller(region_names))
#dev.off()

#######
# Figure 3b: Mean Growth Rate of Western Lake Erie Compare locations same threshold
#######
library(ggplot2)
library(scales)
#pdf(file='figure3b_ne.pdf', width=6, height=4.5, onefile=FALSE)
p.we.mgr<-ggplot(mgr.we,aes(x=year))+
  geom_line(aes(y=med),color="black",group="identity")+
  geom_smooth(aes(y=med),method = "lm")+
  geom_point(aes(y=med),size=3,position=position_dodge(width=0.5))+
  geom_ribbon(aes(ymax=lo,ymin=hi),fill="grey10",alpha=0.3)+
  scale_x_datetime(labels = date_format("%Y"))+
  xlab("")+ ylab("Growth rate per day")+ggtitle('Growth rate')+
  annotate("text", x = as.POSIXct('2005',format='%Y'), y = 0.25, label = paste0("p = ", round(mk.mgr.all$p.value, 2)))
  #geom_point(aes(group=station,color=station,shape=station,y=mean.med.growth),size=3,position=position_dodge(width=0.5))+
  #theme(axis.text.x = element_text(angle = 90, hjust = 0),text = element_text(size=12),panel.grid.major=element_blank(),panel.grid.minor=element_blank(),legend.position = "none")+
  #theme_set(theme_bw(base_size = 24, base_family = "Helvetica"))+
  #theme(axis.text.x = element_text(angle = 45, hjust = 0, vjust=0.5),panel.grid.major=element_blank(),panel.grid.minor=element_blank(),legend.position = "none")+
  #coord_cartesian(ylim = c(0.13, 0.29)) +
  #xlab("")+ ylab("Mean Growth Rate") + ggtitle("Western Lake Erie")


#######
# Figure 4a: Growing Days of Maumee Bay Compare locations same threshold
#######
library(ggplot2)
library(scales)
#pdf(file='figure3b_ne.pdf', width=6, height=4.5, onefile=FALSE)
p.mb.gd<-ggplot(gd.mb,aes(x=year))+
  geom_line(aes(y=med),color="black",group="identity")+
  geom_smooth(aes(y=med),method = "lm")+
  geom_point(aes(y=med),size=3,position=position_dodge(width=0.5))+
  geom_ribbon(aes(ymax=lo,ymin=hi),fill="grey10",alpha=0.3)+
  scale_x_datetime(labels = date_format("%Y"))+
  xlab("")+ ylab("Julian day")+ggtitle("Bloom days")+
  annotate("text", x = as.POSIXct('2005',format='%Y'), y = 150, label = paste0("p = ", round(mk.gd.mb$p.value, 2)))
  #geom_point(aes(group=station,color=station,shape=station,y=mean.med.growth),size=3,position=position_dodge(width=0.5))+
  #theme(axis.text.x = element_text(angle = 90, hjust = 0),text = element_text(size=12),panel.grid.major=element_blank(),panel.grid.minor=element_blank(),legend.position = "none")+
  #theme_set(theme_bw(base_size = 24, base_family = "Helvetica"))+
  #theme(axis.text.x = element_text(angle = 45, hjust = 0, vjust=0.5),panel.grid.major=element_blank(),panel.grid.minor=element_blank(),legend.position = "none")+
  #coord_cartesian(ylim = c(0.13, 0.29)) +
  #xlab("")+ ylab("Growing Days") + ggtitle("Maumee Bay")
#scale_x_continuous(breaks = c(1982,1986,1990,1994,1998,2002,2006,2010,2014))+
#scale_x_continuous(breaks = c(1982,1984,1986,1988,1990,1992,1994,1996,1998,2000,2002,2004,2006,2008,2010,2012,2014))+
#ggtitle('a)') + theme(plot.title=element_text(hjust=0)) + 
#scale_color_manual("Location",labels = c("NW Atlantic"), values = c("red"))+
#scale_fill_manual("Location",labels = c("NW Atlantic"), values = c("red"))+
#scale_shape_manual("Location",labels = c("NW Atlantic"), values = c(19))+
#theme(strip.background = element_blank(),strip.text.y = element_blank(),legend.title=element_blank())+
#facet_grid(station ~ . , labeller = as_labeller(region_names))
#dev.off()

#######
# Figure 4b: Growing Days of Western Lake Erie Compare locations same threshold
#######
library(ggplot2)
library(scales)
#pdf(file='figure3b_ne.pdf', width=6, height=4.5, onefile=FALSE)
p.we.gd<-ggplot(gd.we,aes(x=year))+
  geom_line(aes(y=med),color="black",group="identity")+
  # geom_smooth(aes(y=med),method = "lm")+
  geom_point(aes(y=med),size=3,position=position_dodge(width=0.5))+
  geom_ribbon(aes(ymax=lo,ymin=hi),fill="grey10",alpha=0.3)+
  scale_x_datetime(labels = date_format("%Y"))+
  xlab("")+ ylab("Julian day")+ggtitle("Bloom days")+
  annotate("text", x = as.POSIXct('2005',format='%Y'), y = 150, label = paste0("p = ", round(mk.gd.all$p.value, 2)))
  #geom_point(aes(group=station,color=station,shape=station,y=mean.med.growth),size=3,position=position_dodge(width=0.5))+
  #theme(axis.text.x = element_text(angle = 90, hjust = 0),text = element_text(size=12),panel.grid.major=element_blank(),panel.grid.minor=element_blank(),legend.position = "none")+
  #theme_set(theme_bw(base_size = 24, base_family = "Helvetica"))+
  #theme(axis.text.x = element_text(angle = 45, hjust = 0, vjust=0.5),panel.grid.major=element_blank(),panel.grid.minor=element_blank(),legend.position = "none")+
  #coord_cartesian(ylim = c(0.13, 0.29)) +
  #xlab("")+ ylab("Growing Days") + ggtitle("Western Lake Erie")

#######
# Figure 5a: Growing Season of Maumee Bay Compare locations same threshold
#######
library(ggplot2)
library(scales)
#pdf(file='figure3b_ne.pdf', width=6, height=4.5, onefile=FALSE)
p.mb.gs<-ggplot(gs.mb,aes(x=year))+
  geom_line(aes(y=med),color="black",group="identity")+
  # geom_smooth(aes(y=med),method = "lm")+
  geom_point(aes(y=med),size=3,position=position_dodge(width=0.5))+
  geom_ribbon(aes(ymax=lo,ymin=hi),fill="grey10",alpha=0.3)+
  scale_x_datetime(labels = date_format("%Y"))+
  xlab("")+ ylab("Julian day")+ggtitle('Bloom season')+
  annotate("text", x = as.POSIXct('2005',format='%Y'), y = 155, label =paste0("p = ", round(mk.gs.mb$p.value, 2)))
  #geom_point(aes(group=station,color=station,shape=station,y=mean.med.growth),size=3,position=position_dodge(width=0.5))+
  #theme(axis.text.x = element_text(angle = 90, hjust = 0),text = element_text(size=12),panel.grid.major=element_blank(),panel.grid.minor=element_blank(),legend.position = "none")+
  #theme_set(theme_bw(base_size = 24, base_family = "Helvetica"))+
  #theme(axis.text.x = element_text(angle = 45, hjust = 0, vjust=0.5),panel.grid.major=element_blank(),panel.grid.minor=element_blank(),legend.position = "none")+
  #coord_cartesian(ylim = c(0.13, 0.29)) +
  #xlab("")+ ylab("Growing Season") + ggtitle("Maumee Bay")
#scale_x_continuous(breaks = c(1982,1986,1990,1994,1998,2002,2006,2010,2014))+
#scale_x_continuous(breaks = c(1982,1984,1986,1988,1990,1992,1994,1996,1998,2000,2002,2004,2006,2008,2010,2012,2014))+
#ggtitle('a)') + theme(plot.title=element_text(hjust=0)) + 
#scale_color_manual("Location",labels = c("NW Atlantic"), values = c("red"))+
#scale_fill_manual("Location",labels = c("NW Atlantic"), values = c("red"))+
#scale_shape_manual("Location",labels = c("NW Atlantic"), values = c(19))+
#theme(strip.background = element_blank(),strip.text.y = element_blank(),legend.title=element_blank())+
#facet_grid(station ~ . , labeller = as_labeller(region_names))
#dev.off()

#######
# Figure 5b: Growing Season of Western Lake Erie Compare locations same threshold
#######
library(ggplot2)
library(scales)
#pdf(file='figure3b_ne.pdf', width=6, height=4.5, onefile=FALSE)
p.we.gs<-ggplot(gs.we,aes(x=year))+
  geom_line(aes(y=med),color="black",group="identity")+
  # geom_smooth(aes(y=med),method = "lm")+
  geom_point(aes(y=med),size=3,position=position_dodge(width=0.5))+
  geom_ribbon(aes(ymax=lo,ymin=hi),fill="grey10",alpha=0.3)+
  scale_x_datetime(labels = date_format("%Y"))+
  xlab("")+ ylab("Julian day")+ggtitle('Bloom season')+
  annotate("text", x = as.POSIXct('2005',format='%Y'), y = 150, label = paste0("p = ", round(mk.gs.all$p.value, 2)))
  #geom_point(aes(group=station,color=station,shape=station,y=mean.med.growth),size=3,position=position_dodge(width=0.5))+
  #theme(axis.text.x = element_text(angle = 90, hjust = 0),text = element_text(size=12),panel.grid.major=element_blank(),panel.grid.minor=element_blank(),legend.position = "none")+
  #theme_set(theme_bw(base_size = 24, base_family = "Helvetica"))+
  #theme(axis.text.x = element_text(angle = 45, hjust = 0, vjust=0.5),panel.grid.major=element_blank(),panel.grid.minor=element_blank(),legend.position = "none")+
  #coord_cartesian(ylim = c(0.13, 0.29)) +
  #xlab("")+ ylab("Growing Season") + ggtitle("Western Lake Erie")


library("ggpubr")
library("cowplot")
setwd(dirout)

pdf(file='maumee_5p_all_th17.pdf', width=11, height=8.5, onefile=FALSE)
plot_grid(p.mb.first, p.mb.last, p.mb.mgr, p.mb.gd, p.mb.gs, 
          labels = c("A", "B", "C", "D", "E"),
          ncol = 3, nrow = 2)
dev.off()

pdf(file='werie_5p_all_th17.pdf', width=11, height=8.5, onefile=FALSE)
plot_grid(p.we.first, p.we.last, p.we.mgr, p.we.gd, p.we.gs, 
          labels = c("A", "B", "C", "D", "E"),
          ncol = 3, nrow = 2)
dev.off()



# 
# #######
# # Figure 1c: Sandusky Bay Compare locations same threshold
# #######
# library(ggplot2)
# library(scales)
# #pdf(file='figure3b_ne.pdf', width=6, height=4.5, onefile=FALSE)
# p.sb.first<-ggplot(first.sb,aes(x=year))+
#   geom_line(aes(y=med),color="black",group="identity")+geom_smooth(aes(y=med),method = "lm")+
#   geom_point(aes(y=med),size=3,position=position_dodge(width=0.5))+
#   geom_ribbon(aes(ymax=lo,ymin=hi),fill="grey10",alpha=0.3)+
#   scale_x_datetime(labels = date_format("%Y"))+
#   xlab("")+ ylab("")+ggtitle('First Day')
# #geom_point(aes(group=station,color=station,shape=station,y=mean.med.growth),size=3,position=position_dodge(width=0.5))+
# #theme(axis.text.x = element_text(angle = 90, hjust = 0),text = element_text(size=12),panel.grid.major=element_blank(),panel.grid.minor=element_blank(),legend.position = "none")+
# #theme_set(theme_bw(base_size = 24, base_family = "Helvetica"))+
# #theme(axis.text.x = element_text(angle = 45, hjust = 0, vjust=0.5),panel.grid.major=element_blank(),panel.grid.minor=element_blank(),legend.position = "none")+
# #coord_cartesian(ylim = c(0.13, 0.29)) +
# #xlab("")+ ylab("First Day") + ggtitle("Sandusky Bay")
# 
# #######
# # Figure 2c: Last Day of Sandusky Bay Compare locations same threshold
# #######
# library(ggplot2)
# library(scales)
# #pdf(file='figure3b_ne.pdf', width=6, height=4.5, onefile=FALSE)
# p.sb.last<-ggplot(last.sb,aes(x=year))+
#   geom_line(aes(y=med),color="black",group="identity")+geom_smooth(aes(y=med),method = "lm")+
#   geom_point(aes(y=med),size=3,position=position_dodge(width=0.5))+
#   geom_ribbon(aes(ymax=lo,ymin=hi),fill="grey10",alpha=0.3)+
#   scale_x_datetime(labels = date_format("%Y"))+
#   xlab("")+ ylab("")+ggtitle('Last Day')
# #geom_point(aes(group=station,color=station,shape=station,y=mean.med.growth),size=3,position=position_dodge(width=0.5))+
# #theme(axis.text.x = element_text(angle = 90, hjust = 0),text = element_text(size=12),panel.grid.major=element_blank(),panel.grid.minor=element_blank(),legend.position = "none")+
# #theme_set(theme_bw(base_size = 24, base_family = "Helvetica"))+
# #theme(axis.text.x = element_text(angle = 45, hjust = 0, vjust=0.5),panel.grid.major=element_blank(),panel.grid.minor=element_blank(),legend.position = "none")+
# #coord_cartesian(ylim = c(0.13, 0.29)) +
# #xlab("")+ ylab("Last Day") + ggtitle("Sandusky Bay")
# 
# #######
# # Figure 3c: Mean Growth Rate of Sandusky Bay Compare locations same threshold
# #######
# library(ggplot2)
# library(scales)
# #pdf(file='figure3b_ne.pdf', width=6, height=4.5, onefile=FALSE)
# p.sb.mgr<-ggplot(mgr.sb,aes(x=year))+
#   geom_line(aes(y=med),color="black",group="identity")+geom_smooth(aes(y=med),method = "lm")+
#   geom_point(aes(y=med),size=3,position=position_dodge(width=0.5))+
#   geom_ribbon(aes(ymax=lo,ymin=hi),fill="grey10",alpha=0.3)+
#   scale_x_datetime(labels = date_format("%Y"))+
#   xlab("")+ ylab("")+ggtitle('Grow. Rate')
# #geom_point(aes(group=station,color=station,shape=station,y=mean.med.growth),size=3,position=position_dodge(width=0.5))+
# #theme(axis.text.x = element_text(angle = 90, hjust = 0),text = element_text(size=12),panel.grid.major=element_blank(),panel.grid.minor=element_blank(),legend.position = "none")+
# #theme_set(theme_bw(base_size = 24, base_family = "Helvetica"))+
# #theme(axis.text.x = element_text(angle = 45, hjust = 0, vjust=0.5),panel.grid.major=element_blank(),panel.grid.minor=element_blank(),legend.position = "none")+
# #coord_cartesian(ylim = c(0.13, 0.29)) +
# #xlab("")+ ylab("Mean Growth Rate") + ggtitle("Sandusky Bay")
# 
# 
# #######
# # Figure 4c: Growing Days of Sandusky Bay Compare locations same threshold
# #######
# library(ggplot2)
# library(scales)
# #pdf(file='figure3b_ne.pdf', width=6, height=4.5, onefile=FALSE)
# p.sb.gd<-ggplot(gd.sb,aes(x=year))+
#   geom_line(aes(y=med),color="black",group="identity")+geom_smooth(aes(y=med),method = "lm")+
#   geom_point(aes(y=med),size=3,position=position_dodge(width=0.5))+
#   geom_ribbon(aes(ymax=lo,ymin=hi),fill="grey10",alpha=0.3)+
#   scale_x_datetime(labels = date_format("%Y"))+
#   xlab("")+ ylab("")+ggtitle("Grow. Days")
# #geom_point(aes(group=station,color=station,shape=station,y=mean.med.growth),size=3,position=position_dodge(width=0.5))+
# #theme(axis.text.x = element_text(angle = 90, hjust = 0),text = element_text(size=12),panel.grid.major=element_blank(),panel.grid.minor=element_blank(),legend.position = "none")+
# #theme_set(theme_bw(base_size = 24, base_family = "Helvetica"))+
# #theme(axis.text.x = element_text(angle = 45, hjust = 0, vjust=0.5),panel.grid.major=element_blank(),panel.grid.minor=element_blank(),legend.position = "none")+
# #coord_cartesian(ylim = c(0.13, 0.29)) +
# #xlab("")+ ylab("Growing Days") + ggtitle("Sandusky Bay")
# 
# #######
# # Figure 5c: Growing Season of Sandusky Bay Compare locations same threshold
# #######
# library(ggplot2)
# library(scales)
# #pdf(file='figure3b_ne.pdf', width=6, height=4.5, onefile=FALSE)
# p.sb.gs<-ggplot(gs.sb,aes(x=year))+
#   geom_line(aes(y=med),color="black",group="identity")+geom_smooth(aes(y=med),method = "lm")+
#   geom_point(aes(y=med),size=3,position=position_dodge(width=0.5))+
#   geom_ribbon(aes(ymax=lo,ymin=hi),fill="grey10",alpha=0.3)+
#   scale_x_datetime(labels = date_format("%Y"))+
#   xlab("")+ ylab("")+ggtitle('Grow. Seas.')
# #geom_point(aes(group=station,color=station,shape=station,y=mean.med.growth),size=3,position=position_dodge(width=0.5))+
# #theme(axis.text.x = element_text(angle = 90, hjust = 0),text = element_text(size=12),panel.grid.major=element_blank(),panel.grid.minor=element_blank(),legend.position = "none")+
# #theme_set(theme_bw(base_size = 24, base_family = "Helvetica"))+
# #theme(axis.text.x = element_text(angle = 45, hjust = 0, vjust=0.5),panel.grid.major=element_blank(),panel.grid.minor=element_blank(),legend.position = "none")+
# #coord_cartesian(ylim = c(0.13, 0.29)) +
# #xlab("")+ ylab("Growing Season") + ggtitle("Sandusky Bay")
# 
# # pdf(file='sand_5p_all_th17.pdf', width=11, height=8.5, onefile=FALSE)
# # plot_grid(p.sb.first, p.sb.last, p.sb.mgr, p.sb.gd, p.sb.gs, 
# #           labels = c("A", "B", "C", "D", "E"),
# #           ncol = 3, nrow = 2)
# # dev.off()
# 
