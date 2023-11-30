## Visualize summer and fall warming trends GLSEA

library(tidyverse)
library(raster)
library(lubridate)
library(ncdf4)
library(wql)
library(sf)
library(tmap)

# w Erie bounding box
bboxInDegrees=c(-83.7, -82.5, 41.25, 42.15) #Western Half of Lake Erie
erie_bbox = c(-83.7, -78.8, 41.25, 43)

# ncdf directory
ncdir <- 'glsea_data/'

# ncdf files
files <- list.files(ncdir)[grepl('.nc', list.files(ncdir))]

werie <- files[!grepl('allErie', files)]
erie <- files[grepl('allErie', files)]

# For each file: open, crop to bbox, avg temp over bbox
sst <- map_dfr(erie, ~{
  f <- .
  
  r <- stack(paste0(ncdir, f))
  
  werie <- crop(r, bboxInDegrees)
  all_erie <- crop(r, erie_bbox)
  
  # mean SST for whole lake and wErie basin
  
  mean_sst <- cellStats(werie, stat = 'mean')
  erie_mean <- cellStats(all_erie, stat = 'mean')
  
  # Count # days above 17c, first day, last day
  grow_days <- length(erie_mean[erie_mean > 17])
  gd_werie <- length(mean_sst[mean_sst > 17])
  
  grow_length <- which(erie_mean == erie_mean[erie_mean > 17][grow_days]) - which(erie_mean == erie_mean[erie_mean > 17][1])
  gl_werie <- which(mean_sst == mean_sst[mean_sst > 17][gd_werie]) - which(mean_sst == mean_sst[mean_sst > 17][1])
  
  return(data.frame(file = f, werie_gdays = gd_werie, full_gdays = grow_days,
                    werie_glength = gl_werie, full_glength = grow_length))
  
  print(f)
  
})
# write this df

# Count days above 17C
sst_df <- sst %>%
  mutate(date = row.names(.),
         year = substr(file, 1, 4))

# Plot Bloom days for wErie and whole lake
ggplot(sst_df, aes(x = as.numeric(year))) +
  geom_line(aes(y = full_gdays, col = 'Lake Erie'), cex = 0.75, alpha = 0.7) + 
  geom_line(aes(y = werie_gdays, col = 'Western Basin'), cex = 0.75, alpha = 0.7) +
  geom_smooth(aes(x = as.numeric(year), y = full_gdays, col = 'Lake Erie'), method = 'lm', se = F) +
  geom_smooth(aes(x = as.numeric(year), y = werie_gdays, col = 'Western Basin'),
              method = 'lm', se = F, lty = 2) +
  scale_color_manual(values = c('Western Basin' = 'blue', 'Lake Erie' = 'black')) +
  labs(x = "Year", y = "Bloom days", col = '') +
  theme_classic()

summary(lm(full_gdays ~ as.numeric(year), sst_df))
summary(lm(werie_gdays ~ as.numeric(year), sst_df))

ggplot(sst_df, aes(x = as.numeric(year))) +
  geom_line(aes(y = full_glength, col = 'Lake Erie'), cex = 0.75, alpha = 0.7) + 
  geom_line(aes(y = werie_glength, col = 'Western Basin'), cex = 0.75, alpha = 0.7) +
  #geom_smooth(aes(x = as.numeric(year), y = full_glength, col = 'Lake Erie'), method = 'lm', se = F) +
  geom_smooth(aes(x = as.numeric(year), y = werie_glength, col = 'Western Basin'),
              method = 'lm', se = F, lty = 2) +
  scale_color_manual(values = c('Western Basin' = 'blue', 'Lake Erie' = 'black')) +
  labs(x = "Year", y = "Bloom season length", col = '') +
  theme_classic()

summary(lm(full_glength ~ as.numeric(year), sst_df))
summary(lm(werie_glength ~ as.numeric(year), sst_df))

## Create raster brick of temperature changes June 1 - Oct 31
## jday 152-304

map(erie, ~{
  f <- .
  
  fname <- word(f, 1, 1, sep = "_")
  
  r <- stack(paste0(ncdir, f))
  
  erie_ras <- crop(r, erie_bbox)
  
  growsea <- subset(erie_ras, 152:304)
  
  ann_mean <- calc(growsea, fun = mean)
  
  writeRaster(ann_mean, paste0('glsea_data/annual_means/june_oct_', fname), overwrite = T)
  
})

# stack growsea files

growsea_files <- list.files('glsea_data/annual_means')[grepl('.grd', list.files('glsea_data/annual_means'))]

erie_stack <- stack(paste0('glsea_data/annual_means/', growsea_files))
#writeRaster(erie_stack, 'erie_grow_sea_means.nc', format = 'CDF', varname = 'growsea_temp', overwrite = T)

## Function: Mann-Kendall trend test & extract p-value
mk_pval <- function(x) {
  mk_res <- mannKen(x)
  return(mk_res$p.value)
}

## Function: Sen slope from M-K trend test
mk_slope <- function(x) {
  mk_res <- mannKen(x)
  return(mk_res$sen.slope)
}

erie_pval <- calc(erie_stack, mk_pval)
erie_sig <- reclassify(erie_pval, c(0.05, Inf, NA))

erie_sig_pts <- rasterToPoints(erie_sig)
erie_sig_df <- data.frame(erie_sig_pts) %>%
  st_as_sf(coords = c('x', 'y')) %>%
  st_set_crs(st_crs(erie_pval))

erie_slope <- calc(erie_stack, mk_slope)

# erie shapefile to crop lake st clair
erie_shp <- read_sf('hydro_p_LakeErie.shp') %>%
  st_transform(st_crs(erie_slope))

erie_borders <- erie_shp %>%
  st_union()

erie_slope_cropped <- crop(erie_slope, erie_bbox)
erie_slope_crop <- raster::mask(erie_slope_cropped, erie_shp)

erie_sig_crop <- erie_sig_df %>%
  st_intersection(erie_shp)

## Plot slope and significance 
degrees_plot <- tm_shape(erie_slope_crop) + tm_raster(palette = 'Reds', 
                                                      title = 'Degrees per year', style = 'cont',
                                                      legend.is.portrait = F) + 
  tm_shape(erie_sig_crop) + tm_dots(col = 'black', size = 0.000001, alpha = 0.7) +
  tm_shape(erie_borders) + tm_borders(lwd = 2) +
  tm_layout(legend.position = c('right', 'bottom'), inner.margins = c(0,0,0,0), legend.text.size = 0.8)
tmap_save(degrees_plot, 'Erie_temp_trend_map.pdf', units = 'in', height = 6, width = 10)
