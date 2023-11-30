# Load the packages:

library("ncdf4")

#Part 1 of code: time in-sensitive objects
#Load lat, lon, and mask -- come up with bounds

# order: min-x, min-y, max-x, max-y
bboxInDegrees=c(-83.7,41.25,-80,43) #Western Half of Lake Erie
variableName='analysed_sst'
fillvalue_int <- as.integer((-128))
fillvalue <- 1e32

#Load Example File
url_d1 = 'http://podaac-opendap.jpl.nasa.gov:80/opendap/allData/ghrsst/data/GDS2/L4/GLOB/JPL/MUR/v4.1/2016/230/20160817090000-JPL-L4_GHRSST-SSTfnd-MUR-GLOB-v02.0-fv04.1.nc'
ds1 = nc_open(url_d1)

bbox=bboxInDegrees;
# Get lon and lat variables, which are the dimensions of depth. For this specific dataset they have the names lon and lat
G.x=ncvar_get(ds1,"lon")
G.y=ncvar_get(ds1,"lat")

# Make a selection of indices which fall in our subsetting window
# E.g. translate degrees to indices of arrays.
xindicesinwindow=which(G.x>bbox[1]&G.x<bbox[3]);
xmin=min(xindicesinwindow)
xmax=max(xindicesinwindow)
xcount=(xmax-xmin)+1; # needs to be at least 1

yindicesinwindow=which(G.y>bbox[2]&G.y<bbox[4]);
ymin=min(yindicesinwindow)
ymax=max(yindicesinwindow)
ycount=(ymax-ymin)+1;# needs to be at least 1

print(paste("Indices:",xmin,ymin,xmax,ymax));# <== print bbox in indices

#Load the Land-Sea Mask
mask=ncvar_get(ds1, "mask",start=c(xmin,ymin,1), count=c(xcount,ycount,1))

#For plotting get the coordinates
lats = ncvar_get(ds1,"lat",start=ymin,count=ycount)
lons = ncvar_get(ds1,"lon",start=xmin,count=xcount)

#Get global attributes
jpl.title<-ncatt_get(ds1,0,"title")
jpl.institution<-ncatt_get(ds1,0,"institution")
jpl.datasource <- ncatt_get(ds1,0,"source")
jpl.references <- ncatt_get(ds1,0,"references")
jpl.history <- ncatt_get(ds1,0,"history")
jpl.conventions <- ncatt_get(ds1,0,"Conventions")
jpl.comment <- ncatt_get(ds1,0,"comment")
jpl.product_version <-ncatt_get(ds1,0,"product_version")


#----------------------
#Part 2 loop over the days in the year
#----------------------

for (j in 2003:2017) {
  d.in.y <- seq.Date(from=as.Date(paste(j,"01-01",sep='-')),to=as.Date(paste(j,"12-31",sep='-')),by="days")
  tcount <- length(d.in.y)
  
  #load library for matrix manipulation
  # library (abind)
  #initialize our storage matrix
  sst.in=array(data=fillvalue,c(xcount,ycount,tcount))
  jd.out=array(data=fillvalue_int,tcount)
  
  for (i in 1:tcount)
  {
    date.to.grab <- d.in.y[i]
    jd <- format(date.to.grab,'%j')
    jd.out[i] <- as.numeric(jd) #use as the unlimited dimensino
    yyyy <- format(date.to.grab,'%Y')
    mm <- format(date.to.grab,'%m')
    dd <- format(date.to.grab,'%d')
    
    #generate file to open
    header<-'http://podaac-opendap.jpl.nasa.gov:80/opendap/allData/ghrsst/data/GDS2/L4/GLOB/JPL/MUR/v4.1/'
    #this part varies: 2002/152/20020601
    ender<-'090000-JPL-L4_GHRSST-SSTfnd-MUR-GLOB-v02.0-fv04.1.nc'
    f.to.open <- paste(header,yyyy,'/',jd,'/',yyyy,mm,dd,ender,sep="")
    
    #open netcdf
    ncid = nc_open(f.to.open)
    
    #load daily SST map
    sst.in[,,i]=ncvar_get(ncid, "analysed_sst",start=c(xmin,ymin,1), count=c(xcount,ycount,1))
    
    #clean up & close netcdt
    nc_close(ncid)
    print(paste("done with day",jd,"in year",yyyy,"counter is",i))
  }
  
  #Convert to Kelvin and NA to fill value
  sst.in = sst.in - 273.15 #Kelvin to Centigrade
  sst.in[is.na(sst.in)]<-fillvalue #if NA now fillvalue
  
  #---------------------
  #Part 3: save 1 year of data to netcdf
  #---------------------
  # define dimensions
  londim <- ncdim_def("lon","degrees_east",as.double(lons)) 
  latdim <- ncdim_def("lat","degrees_north",as.double(lats)) 
  timedim <- ncdim_def("time","day_of_year",jd.out)
  
  #3 dimension variables
  dlname <- "analyized_sst"
  sst_def <- ncvar_def("sst","deg_C",list(londim,latdim,timedim),fillvalue,dlname,prec="double")
  #2 dimension variables
  dlname <- "land_sea_mask"
  mask_def <- ncvar_def("mask","mask_index",list(londim,latdim),fillvalue_int,dlname,prec="integer")
  #1 dimension variables
  # dlname <- "lat"
  # lat_def <- ncvar_def("lat","degrees_north",list(latdim),fillvalue,dlname,prec="double")
  # dlname <- "lon"
  # lon_def <- ncvar_def("lon","degrees_east",list(londim),fillvalue,dlname,prec="double")
  # dlname <- "time"
  # time_def <- ncvar_def("time","day_of_year",list(timedim),fillvalue_int,dlname,prec="integer")
  
  # create netCDF file and put arrays
  outdir <- '/Volumes/data1/werie-hr/'
  ncfname <- paste(outdir,"jpl-werie-hr-",yyyy,".nc",sep="")
  
  #ncout <- nc_create(ncfname,list(sst_def,mask_def,lat_def,lon_def,time_def),force_v4=T)
  ncout <- nc_create(ncfname,list(sst_def,mask_def),force_v4=F,verbose=T)
  
  # put variables
  ncvar_put(ncout,sst_def,sst.in)
  ncvar_put(ncout,mask_def,mask)
  # ncvar_put(ncout,lat_def,lats)
  # ncvar_put(ncout,lon_def,lons)
  # ncvar_put(ncout,time_def,jd.out)
  
  # put additional attributes into dimension and data variables
  # ncatt_put(ncout,"lon","axis",lons) #,verbose=FALSE) #,definemode=FALSE)
  # ncatt_put(ncout,"lat","axis",lats)
  # ncatt_put(ncout,"time","axis",time)
  
  # add global attributes
  ncatt_put(ncout,0,"title",paste("W Lake Erie Subset of JPL_1km_SST for year",yyyy,sep=" "))
  ncatt_put(ncout,0,"institution","Eagle Rock Analytics")
  ncatt_put(ncout,0,"source","JPL podaac-opendap server")
  history <- paste("O.M. Doherty", date(), sep=", ")
  
  ncatt_put(ncout,0,"jpl.title",jpl.title$value)
  ncatt_put(ncout,0,"jpl.institution",jpl.institution$value)
  ncatt_put(ncout,0,"jpl.datasource",jpl.datasource$value)
  ncatt_put(ncout,0,"jpl.references",jpl.references$value)
  ncatt_put(ncout,0,"jpl.history",jpl.history$value)
  ncatt_put(ncout,0,"jpl.conventions",jpl.conventions$value)
  ncatt_put(ncout,0,"jpl.comment",jpl.comment$value)
  ncatt_put(ncout,0,"jpl.product_version",jpl.product_version$value)
  
  # close the file, writing data to disk, clean up variables
  nc_close(ncout)
  rm(ncout)
  rm(sst_in)
  rm(d.in.y)
  rm(jd.out)
  
} #loop over year
