#Owen Doherty
#June 8, 2018 - Growth Season Only and Fixed Threshold
## Updated by Grace Di Cecco, 5/19/2023
#Eagle Rock Analytics

# Load the packages:

library("ncdf4")
library(lubridate)

#--------------------
#Part 0 of the code: pre allocate
#--------------------
y_start = 1995
y_end = 2022
nyr = y_end-y_start+1
nlon = 85 #change this
nlat = 68 #change this

#base_rate =0.7183369 #maximum growth rate observed from Gobler experiments
base_rate_med = 0.71284166 #maximum growth rate in 50th percentile curve
base_rate_lo = 0.64805765 #maximum growth rate in 1st percentile curve
base_rate_hi = 0.740034450 #maximum growth rate in 99th percentile curve


#allocate arrays
mic.lo.grow.mean<-array(dim=c(nlon,nlat,nyr))
mic.med.grow.mean<-array(dim=c(nlon,nlat,nyr))
mic.hi.grow.mean<-array(dim=c(nlon,nlat,nyr))

sst.mean<-array(dim=c(nlon,nlat,nyr))

mic.lo.first<-array(dim=c(nlon,nlat,nyr))
mic.med.first<-array(dim=c(nlon,nlat,nyr))
mic.hi.first<-array(dim=c(nlon,nlat,nyr))

mic.lo.last<-array(dim=c(nlon,nlat,nyr))
mic.med.last<-array(dim=c(nlon,nlat,nyr))
mic.hi.last<-array(dim=c(nlon,nlat,nyr))

mic.lo.season_window<-array(dim=c(nlon,nlat,nyr))
mic.med.season_window<-array(dim=c(nlon,nlat,nyr))
mic.hi.season_window<-array(dim=c(nlon,nlat,nyr))

mic.lo.growing_days<-array(dim=c(nlon,nlat,nyr))
mic.med.growing_days<-array(dim=c(nlon,nlat,nyr))
mic.hi.growing_days<-array(dim=c(nlon,nlat,nyr))

cnt_yr = 0

#------------------
#Part 1 of code: load the data
#Load lat, lon, and mask -- come up with bounds
#------------------

#in data
years = seq(from=y_start,to=y_end,by=1)
in_dir<-'glsea_data/'

fstr = '_glsea_sst'
fend = '.nc'
fnames<-paste(in_dir,years,fstr,fend,sep='')


for(i in 1:length(fnames)){
  
  
  #grab year from filename
  year<-years[i]
  txtout=paste('starting year',year,sep=' ')
  print(txtout)
  
  #open the file
  ncin=fnames[i]
  ncid<-nc_open(ncin,write=TRUE)
  
  #Load variables
  sst.in<-ncvar_get(ncid,'sst')
  sst.fillvalue<-ncatt_get(ncid,'sst',attname='_FillValue')
  lat<-ncvar_get(ncid,"latitude")
  lon<-ncvar_get(ncid,"longitude")
  time<-ncvar_get(ncid,"time")
  
  #Now deal with the time variable
  yyyymmdd <- as_date(as_datetime(c(time)))
  time.stamp <- as.POSIXct(strptime(paste0(year(yyyymmdd), "-", yday(yyyymmdd)), '%Y-%j'))
  
  # UNCOMMENT THIS SECTION TO USE SEASONAL SUBSET
  #Subset the sst data to season
  #start day
  sd <- as.POSIXct(strptime(paste("06-01",year,sep="-"),format="%m-%d-%Y"))
  sdi <- format(sd,format="%j")

  #end day
  ed <- as.POSIXct(strptime(paste("10-31",year,sep="-"),format="%m-%d-%Y"))
  edi <- format(ed,format="%j")

  sst.sub <- sst.in[,,sdi:edi]
  time.sub <- time[sdi:edi]
  
  # COMMENT THIS SECTION WHEN USING A SEASONAL SUBSET
  # sst.sub <- sst.in  
  # time.sub <- time
  
  #THIS SECTION Changes any SST of X or lower to NA
  #sst.med curve blows up below 10 C
  #Commented out, now growth rates below 0 are set to NA instead - discuss with Chris and Tim
  #sst.nolo <- sst.in
  #sst.nolo[sst.nolo<10]<-NA
  
  #close the netcdf
  nc_close(ncid)
  
  #----------------------
  #Part 2: Calculate a growth rate
  #----------------------
  txtout=paste('calculating growth rates for year ',year,sep=' ')
  print(txtout)
  
  #Polynomials from growth_curve_interpolated.R (printed to output)
  #Lo is 1st ptile; hi is 99th ptile
  gr_lo <- array(c(-2.751602e+00,4.507835e-01,-2.886584e-02,9.347239e-04,-1.158503e-05))
  gr_med <- array(c(1.497026e+00,-3.486953e-01,2.705047e-02,-7.434991e-04,6.659022e-06))
  gr_hi <- array(c(-3.418779e-01,1.019944e-01,-4.722763e-03,1.347631e-04,-1.686465e-06))
  
  grt.med<- 0.75*base_rate_med #above 75% of max growth is considered possible bloom conditions
  grt.lo <- 0.75*base_rate_lo #above 75% of max growth is considered possible bloom conditions
  grt.hi <- 0.75*base_rate_hi #above 75% of max growth is considered possible bloom conditions
  
  micro_lo<-gr_lo[1]+(gr_lo[2]*sst.sub)+(gr_lo[3]*(sst.sub^2))+(gr_lo[4]*(sst.sub^3))+(gr_lo[5]*(sst.sub^4))
  micro_hi<-gr_hi[1]+(gr_hi[2]*sst.sub)+(gr_hi[3]*(sst.sub^2))+(gr_hi[4]*(sst.sub^3))+(gr_hi[5]*(sst.sub^4))
  micro_med<-gr_med[1]+(gr_med[2]*sst.sub)+(gr_med[3]*(sst.sub^2))+(gr_med[4]*(sst.sub^3))+(gr_med[5]*(sst.sub^4))
  
  #first run the model then correct for specific values
  #micro_med<-gr_med[1]+(gr_med[2]*sst.nolo)+(gr_med[3]*(sst.nolo^2))+(gr_med[4]*(sst.nolo^3))+(gr_med[5]*(sst.nolo^4))
  
  #this code sets GR to 09 to 10 C is 0.02, 08 to 09 is 0.01, below 8 is 0
  micro_med[sst.sub<=10] <- 0.02
  micro_med[sst.sub<=9] <- 0.01
  micro_med[sst.sub<=8] <- 0
  
  #don't allow negative growth rates to occur
  micro_lo[micro_lo<0]<-0
  micro_med[micro_med<0]<-0
  micro_hi[micro_hi<0]<-0
  
  txtout=paste('finished calculating growth rates for year ',year,sep=' ')
  print(txtout)
  
  #--------------------
  #Step 4: Annual Stats - now before Step 3 to preserve missing
  #--------------------
  txtout=paste('calcuating means and stats for ',year,sep=' ')
  print(txtout)
  
  #Mean growth rate
  mic.lo.grow.mean[,,i] <- apply(micro_lo,c(1,2), function(x) mean(x,na.rm=TRUE))
  mic.med.grow.mean[,,i] <- apply(micro_med,c(1,2), function(x) mean(x,na.rm=TRUE))
  mic.hi.grow.mean[,,i] <- apply(micro_hi,c(1,2), function(x) mean(x,na.rm=TRUE))
  
  #Mean SST
  sst.mean[,,i] <- apply(sst.sub,c(1,2), function(x) mean(x,na.rm=TRUE))
  
  #First day of growth
  ##mic.hi.first[,,i] <- apply(micro_hi,c(1,2), function(x) min(which(x>=grt.hi),na.rm=TRUE))
  #mic.hi.first[,,i] <- apply(micro_hi,c(1,2), function(x) min(which(x>=grt.med),na.rm=TRUE))
  #mic.med.first[,,i] <- apply(micro_med,c(1,2), function(x) min(which(x>=grt.med),na.rm=TRUE))
  ##mic.lo.first[,,i] <- apply(micro_lo,c(1,2), function(x) min(which(x>=grt.lo),na.rm=TRUE))
  #mic.lo.first[,,i] <- apply(micro_lo,c(1,2), function(x) min(which(x>=grt.med),na.rm=TRUE))
 
  #Not great code - fix indexing problem
  #There are 151 days between Jan1 and Jun1 that are dropped, add them back
  mic.hi.first[,,i] <- apply(micro_hi,c(1,2), function(x) 151+min(which(x>=grt.med),na.rm=TRUE))
  mic.med.first[,,i] <- apply(micro_med,c(1,2), function(x) 151+min(which(x>=grt.med),na.rm=TRUE))
  mic.lo.first[,,i] <- apply(micro_lo,c(1,2), function(x) 151+min(which(x>=grt.med),na.rm=TRUE))
  
  # #MASK out Land
  # plot.first <- first
  # plot.first[mask==2] <-NA
  # 
  # library(maps)
  #  rgb.palette=colorRampPalette(c('darkblue','palegreen1','yellow','red2'),interpolate='spline')
  #  int=seq(min(plot.first,na.rm=TRUE),max(plot.first,na.rm=TRUE),length.out=30)
  #  filled.contour(lon, lat, plot.first, color.palette=rgb.palette, levels=int,
  #                 plot.title=title(main='First day in year above thresh 2003', xlab='Longitude [°]', ylab='Latitude [°]'),
  #                 plot.axes={axis(1); axis(2);map('world2', add=TRUE);grid()})
  
  #Last day of growth
  ##mic.lo.last[,,i] <- apply(micro_lo,c(1,2), function(x) max(which(x>=grt.lo),na.rm=TRUE))
  #mic.lo.last[,,i] <- apply(micro_lo,c(1,2), function(x) max(which(x>=grt.med),na.rm=TRUE))
  #mic.med.last[,,i] <- apply(micro_med,c(1,2), function(x) max(which(x>=grt.med),na.rm=TRUE))
  ##mic.hi.last[,,i] <- apply(micro_hi,c(1,2), function(x) max(which(x>=grt.hi),na.rm=TRUE))
  #mic.hi.last[,,i] <- apply(micro_hi,c(1,2), function(x) max(which(x>=grt.med),na.rm=TRUE))
  
  #Not great code - fix indexing problem
  #There are 151 days between Jan1 and Jun1 that are dropped, add them back
  mic.lo.last[,,i] <- apply(micro_lo,c(1,2), function(x) 151+max(which(x>=grt.med),na.rm=TRUE))
  mic.med.last[,,i] <- apply(micro_med,c(1,2), function(x) 151+max(which(x>=grt.med),na.rm=TRUE))
  mic.hi.last[,,i] <- apply(micro_hi,c(1,2), function(x) 151+max(which(x>=grt.med),na.rm=TRUE))
  
  
  #  #MASK out Land
  #  plot.last <- last
  #  plot.last[mask==2] <-NA
  #  
  #  rgb.palette=colorRampPalette(c('darkblue','palegreen1','yellow','red2'),interpolate='spline')
  #  int=seq(min(plot.last,na.rm=TRUE),max(plot.last,na.rm=TRUE),length.out=30)
  #  filled.contour(lon, lat, plot.last, color.palette=rgb.palette, levels=int,
  #                 plot.title=title(main='last day in year above thresh 2003', xlab='Longitude [°]', ylab='Latitude [°]'),
  #                 plot.axes={axis(1); axis(2);map('world2', add=TRUE);grid()})
  
  #Length of growing season
  mic.lo.season_window[,,i] = (mic.lo.last[,,i] - mic.lo.first[,,i]) + 1
  mic.med.season_window[,,i] = (mic.med.last[,,i] - mic.med.first[,,i]) + 1
  mic.hi.season_window[,,i] = (mic.hi.last[,,i] - mic.hi.first[,,i]) + 1
  
  #  #MASK out Land
  #  plot.season_window <- season_window
  #  plot.season_window[mask==2] <-NA
  #  
  #  rgb.palette=colorRampPalette(c('darkblue','palegreen1','yellow','red2'),interpolate='spline')
  #  int=seq(min(plot.season_window,na.rm=TRUE),max(plot.season_window,na.rm=TRUE),length.out=30)
  #  filled.contour(lon, lat, plot.season_window, color.palette=rgb.palette, levels=int,
  #                 plot.title=title(main='season_window in year 2003', xlab='Longitude [°]', ylab='Latitude [°]'),
  #                 plot.axes={axis(1); axis(2);map('world2', add=TRUE);grid()})
  
  #Total number of growing days
  #mic.lo.growing_days[,,i] <- apply(micro_lo,c(1,2), function(x) sum((x>=grt.lo),na.rm=TRUE))
  mic.lo.growing_days[,,i] <- apply(micro_lo,c(1,2), function(x) sum((x>=grt.med),na.rm=TRUE))
  mic.med.growing_days[,,i] <- apply(micro_med,c(1,2), function(x) sum((x>=grt.med),na.rm=TRUE))
  #mic.hi.growing_days[,,i] <- apply(micro_hi,c(1,2), function(x) sum((x>=grt.hi),na.rm=TRUE))
  mic.hi.growing_days[,,i] <- apply(micro_hi,c(1,2), function(x) sum((x>=grt.med),na.rm=TRUE))
  
  
  #  #MASK out Land
   # plot.growing_days <- mic.med.growing_days[,,i]
   # plot.growing_days[mask==2] <-NA
   # 
   # rgb.palette=colorRampPalette(c('darkblue','palegreen1','yellow','red2'),interpolate='spline')
   # int=seq(min(plot.growing_days,na.rm=TRUE),max(plot.growing_days,na.rm=TRUE),length.out=30)
   # filled.contour(lon, lat, plot.growing_days, color.palette=rgb.palette, levels=int,
   #                plot.title=title(main='growing_days in year 2003', xlab='Longitude [°]', ylab='Latitude [°]'),
   #                plot.axes={axis(1); axis(2);map('world2', add=TRUE);grid()})
  
  txtout=paste('finished calcuating means and stats for ',year,sep=' ')
  print(txtout)
 
  #--------------------
  #Part 3: Create new growth rate netcdf
  #--------------------
  txtout=paste('saving netcdf of growth rates for ',year,sep=' ')
  print(txtout)
  
  #create output array with fill values
  #grow = cochlo
  fillvalue <- 1e32
  fillvalue_int <- as.integer((-128))
  micro_lo[is.na(micro_lo)]<-fillvalue #if NA now fillvalue
  micro_med[is.na(micro_med)]<-fillvalue #if NA now fillvalue
  micro_hi[is.na(micro_hi)]<-fillvalue #if NA now fillvalue
  
  #size of array to write
  nt = dim(micro_lo)[3]
  nlat = dim(micro_lo)[2]
  nlon = dim(micro_lo)[1]
  
  #dimensions for output
  londim <- ncdim_def("lon","degrees_east",as.double(lon)) 
  latdim <- ncdim_def("lat","degrees_north",as.double(lat)) 
  timedim <- ncdim_def("time","day_of_year",time.sub)
  
  #define the array
  dlname <- "micro_lo_gr"
  lo_def <- ncvar_def("micro_lo_gr","per day",list(londim,latdim,timedim),fillvalue,dlname,prec="double")
  dlname <- "micro_med_gr"
  med_def <- ncvar_def("micro_med_gr","per day",list(londim,latdim,timedim),fillvalue,dlname,prec="double")
  dlname <- "micro_hi_gr"
  hi_def <- ncvar_def("micro_hi_gr","per day",list(londim,latdim,timedim),fillvalue,dlname,prec="double")
  
  # create netCDF file and put arrays
  outdir <- 'output_data_glsea/'
  ncfname <- paste(outdir,"micro-growth-werie-growsea",year,".nc",sep="")
  ncout <- nc_create(ncfname,list(lo_def,med_def,hi_def),force_v4=F,verbose=F)
  
  ncvar_put(ncout,lo_def,micro_lo)
  ncvar_put(ncout,med_def,micro_med)
  ncvar_put(ncout,hi_def,micro_hi)

  
  #close the netcdf
  nc_close(ncout)
  
  txtout=paste('completed saving netcdf of growth rates for ',year,sep=' ')
  print(txtout)
  
   #--------------------
  #Step 5: Clear annual data
  #--------------------
  
  #clear large arrays
  rm(micro_lo)
  rm(micro_med)
  rm(micro_hi)
  rm(med_def)
  rm(lo_def)
  rm(hi_def)
  rm(latdim)
  rm(londim)
  rm(mask_def)
  rm(sst.fillvalue)
  rm(sst.in)
  rm(sst.sub)
  rm(sst.nolo)
  rm(time)
  rm(time.stamp)
  rm(timedim)
  rm(yyyyjjj)
  
  txtout=paste(year,'is complete',sep=' ')
  print(txtout)
  
} #end the loop over years

# -----------------------
#Step 6: output the annual variables
# -----------------------

print('creating netcdf of annual means ')

#Clean up variables, remove na and inf
mic.med.last[!is.finite(mic.med.last)] <- NA
mic.hi.last[!is.finite(mic.hi.last)] <- NA
mic.lo.last[!is.finite(mic.lo.last)] <- NA

mic.med.growing_days[mic.med.growing_days==0] <- NA
mic.hi.growing_days[mic.hi.growing_days==0] <- NA
mic.lo.growing_days[mic.lo.growing_days==0] <- NA

mic.med.first[!is.finite(mic.med.first)] <- NA
mic.hi.first[!is.finite(mic.hi.first)] <- NA
mic.lo.first[!is.finite(mic.lo.first)] <- NA

mic.med.season_window[!is.finite(mic.med.season_window)] <- NA
mic.hi.season_window[!is.finite(mic.hi.season_window)] <- NA
mic.lo.season_window[!is.finite(mic.lo.season_window)] <- NA

#Convert to integer
# last<-as.integer(last)
# first<-as.integer(first)
# season_window<-as.integer(season_window)
# growing_days<-as.integer(growing_days)

# define dimensions
londim <- ncdim_def("lon","degrees_east",as.double(lon)) 
latdim <- ncdim_def("lat","degrees_north",as.double(lat)) 
timedim <- ncdim_def("year","calendar year",years)

#3 dimension variables
dlname <- "mic.med.grow.mean"
mic.med.grow.mean_def <- ncvar_def("mic.med.mean_annual_growth","per day",list(londim,latdim,timedim),fillvalue,dlname,prec="double")
dlname <- "mic.hi.grow.mean"
mic.hi.grow.mean_def <- ncvar_def("mic.hi.mean_annual_growth","per day",list(londim,latdim,timedim),fillvalue,dlname,prec="double")
dlname <- "mic.lo.grow.mean"
mic.lo.grow.mean_def <- ncvar_def("mic.lo.mean_annual_growth","per day",list(londim,latdim,timedim),fillvalue,dlname,prec="double")

dlname <- "sst.mean"
sst.mean_def <- ncvar_def("mean_annual_sst","deg_C",list(londim,latdim,timedim),fillvalue,dlname,prec="double")

dlname <- "mic.med.first"
mic.med.first_def <- ncvar_def("mic.med.first_day","julian day",list(londim,latdim,timedim),fillvalue,dlname,prec="double")
dlname <- "mic.hi.first"
mic.hi.first_def <- ncvar_def("mic.hi.first_day","julian day",list(londim,latdim,timedim),fillvalue,dlname,prec="double")
dlname <- "mic.lo.first"
mic.lo.first_def <- ncvar_def("mic.lo.first_day","julian day",list(londim,latdim,timedim),fillvalue,dlname,prec="double")


dlname <- "mic.med.last"
mic.med.last_def <- ncvar_def("mic.med.last_day","julian day",list(londim,latdim,timedim),fillvalue,dlname,prec="double")
dlname <- "mic.hi.last"
mic.hi.last_def <- ncvar_def("mic.hi.last_day","julian day",list(londim,latdim,timedim),fillvalue,dlname,prec="double")
dlname <- "mic.lo.last"
mic.lo.last_def <- ncvar_def("mic.lo.last_day","julian day",list(londim,latdim,timedim),fillvalue,dlname,prec="double")


dlname <- "mic.med.season_window"
mic.med.season_window_def <- ncvar_def("mic.med.season_window","days between onset and end of season",list(londim,latdim,timedim),fillvalue,dlname,prec="double")
dlname <- "mic.hi.season_window"
mic.hi.season_window_def <- ncvar_def("mic.hi.season_window","days between onset and end of season",list(londim,latdim,timedim),fillvalue,dlname,prec="double")
dlname <- "mic.lo.season_window"
mic.lo.season_window_def <- ncvar_def("mic.lo.season_window","days between onset and end of season",list(londim,latdim,timedim),fillvalue,dlname,prec="double")


dlname <- "mic.med.growing_days"
mic.med.growing_days_def <- ncvar_def("mic.med.growing_days","calculated potential days of growth",list(londim,latdim,timedim),fillvalue,dlname,prec="double")
dlname <- "mic.hi.growing_days"
mic.hi.growing_days_def <- ncvar_def("mic.hi.growing_days","calculated potential days of growth",list(londim,latdim,timedim),fillvalue,dlname,prec="double")
dlname <- "mic.lo.growing_days"
mic.lo.growing_days_def <- ncvar_def("mic.lo.growing_days","calculated potential days of growth",list(londim,latdim,timedim),fillvalue,dlname,prec="double")



# create netCDF file and put arrays
outdir <- 'output_data_glsea/'
ncfname <- paste(outdir,"annual.micro.werie.growsea.nc",sep="")
ncout <- nc_create(ncfname,list(mic.med.grow.mean_def,mic.hi.grow.mean_def,mic.lo.grow.mean_def,sst.mean_def,mic.med.first_def,mic.lo.first_def,mic.hi.first_def,mic.med.last_def,mic.hi.last_def,mic.lo.last_def,mic.med.season_window_def,mic.hi.season_window_def,mic.lo.season_window_def,mic.med.growing_days_def,mic.hi.growing_days_def,mic.lo.growing_days_def),force_v4=F,verbose=F)

# put variables
ncvar_put(ncout,mic.med.grow.mean_def,mic.med.grow.mean)
ncvar_put(ncout,mic.hi.grow.mean_def,mic.hi.grow.mean)
ncvar_put(ncout,mic.lo.grow.mean_def,mic.lo.grow.mean)
ncvar_put(ncout,sst.mean_def,sst.mean)
ncvar_put(ncout,mic.med.first_def,mic.med.first)
ncvar_put(ncout,mic.hi.first_def,mic.hi.first)
ncvar_put(ncout,mic.lo.first_def,mic.lo.first)
ncvar_put(ncout,mic.med.last_def,mic.med.last)
ncvar_put(ncout,mic.hi.last_def,mic.hi.last)
ncvar_put(ncout,mic.lo.last_def,mic.lo.last)
ncvar_put(ncout,mic.med.season_window_def,mic.med.season_window)
ncvar_put(ncout,mic.hi.season_window_def,mic.hi.season_window)
ncvar_put(ncout,mic.lo.season_window_def,mic.lo.season_window)
ncvar_put(ncout,mic.med.growing_days_def,mic.med.growing_days)
ncvar_put(ncout,mic.hi.growing_days_def,mic.hi.growing_days)
ncvar_put(ncout,mic.lo.growing_days_def,mic.lo.growing_days)

# put additional attributes into dimension and data variables
# ncatt_put(ncout,"lon","axis",lons) #,verbose=FALSE) #,definemode=FALSE)
# ncatt_put(ncout,"lat","axis",lats)
# ncatt_put(ncout,"time","axis",time)

# add global attributes
ncatt_put(ncout,0,"title","Western Lake Erie Subset of Growth Rates and Counts")
ncatt_put(ncout,0,"institution","Eagle Rock Analytics")
#ncatt_put(ncout,0,"source","JPL podaac-opendap server")
history <- paste("O.M. Doherty", date(), sep=", ")

# close the file, writing data to disk, clean up variables
nc_close(ncout)

txtout=paste('file',ncfname,'has been created', sep=' ')
print(txtout)

# #----------------------
# #Part 3: Make the figures
# #----------------------
# countt=dim(sst.in)[3]
# 
# # for(i in 1:countt)
# # {
# i = 45
# 
# #Plot data
# plot.sst <- sst.in[,,i]
# plot.cochlo <- cochlo[,,i]
# 
# #MASK out Land
# plot.sst[mask==2] <-NA
# plot.cochlo[mask==2] <-NA
# 
# #Extract single point
# single.df$shin <-
#   
#   #Convert to data.frame
#   library(reshape2)
# plot.sst.df<-melt(plot.sst)
# colnames(plot.sst.df)<-c("loni","lati","sst")
# plot.sst.df$lat <-lat[plot.sst.df$lati]
# plot.sst.df$lon <-lon[plot.sst.df$loni]
# #
# plot.cochlo.df<-melt(plot.cochlo)
# colnames(plot.cochlo.df)<-c("loni","lati","cochlo")
# plot.cochlo.df$lat <-lat[plot.cochlo.df$lati]
# plot.cochlo.df$lon <-lon[plot.cochlo.df$loni]
# 
# #Figure metadata
# sst.palette=colorRampPalette(c('darkblue','palegreen1','yellow','red2'),interpolate='spline')
# cochlo.palette=colorRampPalette(c('gray100','gray90','gray80','gray70','gray60','aquamarine','darkgreen'),interpolate='spline')
# c.p <- cochlo.palette(256)
# 
# sst.min <- 0
# sst.max <- max(sst.in,na.rm=TRUE)
# cochlo.min <- 0
# cochlo.max <- max(cochlo,na.rm=TRUE)
# 
# # 
# #Try a dumb map
# #
# 
# library(ggplot2)
# #Cochlodinium
# a<-ggplot(plot.cochlo.df)+
#   aes(x=lon,y=lat,z=cochlo,fill=cochlo)+
#   geom_tile()+
#   ggtitle(time.stamp[i])+
#   coord_equal()+
#   geom_contour(color="white",alpha=0.5)+
#   #scale_fill_distiller(palette="Greens",direction=1,na.value="wheat4",limits=c(cochlo.min,cochlo.max))+
#   #scale_fill_distiller(palette=c.p,direction=1,na.value="wheat4",limits=c(cochlo.min,cochlo.max))+
#   #scale_fill_brewer(palette=c.p,direction=1,na.value="wheat4",limits=c(cochlo.min,cochlo.max))+
#   xlim(-74.5,-71)+
#   scale_fill_gradient2(low="grey1",mid="honeydew",high="green",midpoint=grt,na.value="wheat4")+
#   theme_bw()
# 
# #SST
# b<-ggplot(plot.sst.df)+
#   aes(x=lon,y=lat,z=sst,fill=sst)+
#   geom_tile()+
#   ggtitle(time.stamp[i])+
#   coord_equal()+
#   geom_contour(color="white",alpha=0.5)+
#   scale_fill_distiller(palette="Spectral",na.value="wheat4",limits=c(sst.min,sst.max))+
#   theme_bw()
# 
# library(gridExtra)
# outdir='/Users/owen/Documents/research/hab/korea_japan/hi_res/png/'
# ggout<-paste(outdir,time[i],"_2pan.png",sep="")
# png(file=ggout,width=11,height=8.5,units="in",res=300)
# grid.arrange(a,b,ncol=1,nrow=2)
# dev.off()
# # }