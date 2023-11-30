## Compare bias in SST vs buoy water temps
## June temps most important ecologically

library(tidyverse)
library(raster)
library(ncdf4)
library(lubridate)

## Data directories

buoy_dir <- "buoy_data"
sst_dir <- "sea_surface_data"

## Daily mean temperature at each buoy
## Need to find point in Maumee Bay to compare to - sea surface data does not extend into river

## River buoys
# Average sub-daily obs, remove no-data records
maumee_wat <- read_csv("buoy_data/maumee_waterville_temp.csv") %>%
  rename(WTMP = `110661_00010`) %>%
  mutate(YYYY = year(datetime),
         MM = month(datetime),
         DD = day(datetime)) %>%
  group_by(site_no, YYYY, MM, DD) %>%
  summarize(mean_tmp = mean(WTMP, na.rm = T))

## Lake buoys
# stdmet files: need to remove 999s - no data
# Keep only WTMP col
# average daily temp

stdmet_files <- list.files(buoy_dir)[grepl("stdmet_", list.files(buoy_dir))]

stdmet_data <- data.frame(file = stdmet_files) %>%
  group_by(file) %>%
  nest() %>%
  mutate(data = map(file, ~{
    f <- .
    
    temp <- read_csv(paste0(buoy_dir, "/", f), comment = "#")
    
    if(is.character(temp[[1,1]])) {
      temp <- temp %>%
        slice(-1) %>%
        mutate_at(c("WTMP"), ~as.numeric(.))
    }
    
    temp %>%
      select(contains("YY"), MM, DD, hh, contains("WTMP")) %>%
      rename(YYYY = contains("YY"), WTMP = contains("WTMP")) %>%
      filter(WTMP != 999) %>%
      group_by(YYYY, MM, DD) %>%
      summarize(mean_tmp = mean(WTMP, na.rm = T))
  }))

## Clean bad data

stdmet_cleaned <- stdmet_data %>%
  mutate(nrows = map_dbl(data, ~nrow(.))) %>%
  filter(nrows > 0) %>%
  mutate(station = word(file, 2, 2, sep = "_"),
         year = substr(file, 14,17))

## Spatial points of buoys for extracting SST
# mrho1: 41.544 N 82.731 W
# thro1: 41.694 N 83.473 W 

## Buoy coords from NBDC are out of extent of SST obs
## Using approximate nearby coords

buoy_pts <- data.frame(station = c("mrho1", "thro1"),
                       lon = c(-82.731, -83.473),
                       lat = c(41.544, 41.694))

buoy_pts_sf <- buoy_pts %>%
  st_as_sf(coords = c("lon", "lat")) %>%
  st_set_crs(4326)

buoy_pts_adj <- data.frame(station = c("mrho1", "thro1"),
                           lon = c(-82.74, -83.43),
                           lat = c(41.55, 41.72)) %>%
  st_as_sf(coords = c("lon", "lat")) %>%
  st_set_crs(4326)

buoy_map <- tm_shape(jpl[[1]]) + tm_raster(title = "deg. C") +
  tm_shape(buoy_pts_sf) + tm_dots(size = 0.25) +
  tm_shape(buoy_pts_adj) + tm_dots(col = "red", size = 0.25)
tmap_save(buoy_map, "pdf/buoy_station_coords.pdf")

## Join buoy coords to stdmet data

stdmet_coords <- stdmet_cleaned %>%
  left_join(data.frame(station = c("mrho1", "thro1"),
                       lon = c(-82.74, -83.43),
                       lat = c(41.55, 41.72)))

## Get SST data for each buoy temp obs (daily means)
## Add DOY to buoy obs

get_sst <- function(data, year, lon, lat, ...) {
 
   buoy_df <- data %>%
    mutate(date = paste0(YYYY, "-", MM, "-", DD),
           doy = yday(date),
           YYYY = as.character(YYYY))
  
  jpl <- brick(paste0(sst_dir, "/jpl-werie-hr-", year, ".nc"))
  
  sst_all <- extract(jpl, data.frame(x = lon, y = lat))
  
  buoy_sst <- buoy_df %>%
    mutate(sst = sst_all[doy])
  
  return(buoy_sst)
}

stdmet_sst <- stdmet_coords %>%
  mutate(data_sst = purrr::pmap(list(data, year, lon, lat), get_sst))

stdmet_unnest <- stdmet_sst %>%
  dplyr::select(-data) %>%
  unnest(cols = c("data_sst")) %>%
  rename(buoy_tmp = "mean_tmp")

stdmet_unnest <- read_csv("buoy_data/station_sst_temps.csv")

## Extract data for point at Maumee River mouth, SW region of Maumee Bay to compare with river buoy
## Use thro1 site coords -83.43, 41.72

maumee_sst <- maumee_wat %>%
  mutate(date = paste0(YYYY, "-", MM, "-", DD),
         doy = yday(date),
         lon = -83.43,
         lat = 41.72,
         year = YYYY) %>%
  filter(year < 2021) %>%
  group_by(site_no, year, lon, lat) %>%
  nest() %>%
  mutate(data_sst = purrr::pmap(list(data, year, lon, lat), get_sst)) %>%
  dplyr::select(-data) %>%
  unnest(cols = c("data_sst")) %>%
  rename(buoy_tmp = "mean_tmp")

## Join river buoy to lake buoys datasets

all_buoy_sst <- maumee_sst %>%
  rename(station = "site_no") %>%
  mutate_at(c("YYYY"), ~as.numeric(.)) %>%
  mutate_at(c("DD", "MM"), ~as.character(.)) %>%
  mutate_at(c("date"), ~as.Date(.)) %>%
  bind_rows(dplyr::select(stdmet_unnest, -file, -nrows))

## Calculate average monthly bias by site

bias_sst <- all_buoy_sst %>%
  mutate(bias = buoy_tmp - sst) %>%
  group_by(station, MM, year) %>%
  mutate(avg_monthly = mean(bias, na.rm = T),
         month = month(date)) %>%
  mutate(station_plot = case_when(station == "mrho1" ~ "Marblehead",
                                  station == "thro1" ~ "USCG Toledo",
                                  station == "04193490" ~ "Maumee River"))

bias_sst %>%
  filter(MM == 6 | MM == "06") %>%
  dplyr::select(year, avg_monthly) %>%
  unique()

## Make 1:1 plots for June

theme_set(theme_classic(base_size = 15))
ggplot(filter(bias_sst, MM == 6 | MM == "06"), aes(x = sst, y = buoy_tmp, col = station)) + 
  geom_point(size = 2) +
  geom_abline(intercept= 0, slope = 1) +
  labs(x = "SST (deg. C)", y = "Station water temp (deg. C)", col = "Site", title = "June daily mean temperature") +
  scale_color_manual(values = c("skyblue", "orchid"), 
                     labels = c("mrho1" = "Marblehead", "04193490" = "Maumee River")) +
  theme(legend.position = c(0.8, 0.2))
ggsave("pdf/sst_station_temps_june.pdf")

## Avg monthly bias
ggplot(bias_sst, aes(x = month, y = avg_monthly, group = year, col = year)) + 
  geom_line(cex = 1) + 
  geom_hline(yintercept = 0, lty = 2) +
  labs(x = "Month", y = "Station temp - SST", col = "Year") + 
  scale_x_continuous(breaks = c(1:12)) +
  facet_wrap(~station_plot, nrow = 3)
ggsave('pdf/monthly_temps_bias.pdf')

