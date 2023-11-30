## GLSEA data download

library(ncdf4)
library(tidyverse)
library(lubridate)
library(terra)

# Daily gridded SST over great lakes
site <- 'https://coastwatch.glerl.noaa.gov/erddap/files/GLSEA_GCS/'

# define time period of interest
yrs <- c(1995:2022)

# define output directory to deposit downloaded nc files
outdir <- 'glsea_data/'

# Western Erie extent
werie_bbox <- c(-83.7,41.25,-82.5,42)

# get jdays for time period of interest
# construct glsea file names for each date
dates <- data.frame(date = seq(ymd(paste0(yrs[1], '-01-01')), 
                               ymd(paste0(yrs[length(yrs)], '-12-31')), by='days')) %>%
  mutate(mon = month(date), 
         mon_2dig = ifelse(nchar(mon) < 2, paste0('0', mon), mon),
         yr = year(date), 
         day = day(date)) %>%
  mutate(yday = yday(date),
         yday_3dig = ifelse(nchar(yday) < 3, 
                            ifelse(nchar(yday) < 2, paste0("00", yday), paste0('0', yday)), 
                            yday),
         file_path = paste0(yr, "/", mon_2dig, "/", yr, "_", yday_3dig, "_glsea_sst.nc"))

# download files for time period of interest

# define file download function with error handling
possibly_download <- possibly(download.file, otherwise = NULL)

map(dates$file_path, ~possibly_download(paste0(site, .),
                                    paste0(outdir, substr(., 1, 4), '/', substr(., 9, 29)),
                                    mode = 'wb'))

### Process GLSEA data into 1 file per year, cropped to spatial extent of interest
map(yrs, ~{
  y <- .
  dir <- paste0(outdir, y, "/")
  
  files <- list.files(dir)
  
  sst <- rast(paste0(dir, files))
  
  ext_werie <- ext(-83.7, -82.5, 41.25, 42.2)
  
  crop_sst <- crop(sst, ext_werie)
  
  writeCDF(crop_sst, paste0(outdir, y, "_glsea_sst.nc"), varname = 'sst', overwrite = T)
  
  print(paste0(y, " complete"))
  
})

### Process GLSEA data into 1 file for year, full Erie
map(yrs, ~{
  y <- .
  dir <- paste0(outdir, y, "/")
  
  files <- list.files(dir)
  
  sst <- rast(paste0(dir, files))
  
  writeCDF(sst, paste0(outdir, y, "allErie_glsea_sst.nc"), varname = 'sst', overwrite = T)
  
  print(paste0(y, " complete"))
  
})


