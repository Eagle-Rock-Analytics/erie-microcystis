## Temperature trend plot

library(tidyverse)
library(ncdf4)
library(raster)
library(lubridate)

# Data directories
sst_dir <- "sea_surface_data"

## Calculate growing season average across western Lake Erie
# Growing season: July-October

# Function to get average growing season temp from W Lake Erie sea surface data given year
get_mean_sst <- function(year, ...) {
  
  gs_start <- yday(paste0(year, "-07-01"))
  gs_end <- yday(paste0(year, "-10-31"))
  
  #Read in w. Erie temp data for a given year
  jpl <- brick(paste0(sst_dir, "/jpl-werie-hr-", year, ".nc"))
  
  #Subset to growing season months (July 1-October 31)
  gs_sst <- subset(jpl, gs_start:gs_end)
  
  #Average growing season temp
  monthly_avg <- cellStats(gs_sst, mean, na.rm = T)
  
  avg_temp <- mean(monthly_avg)
  
  return(avg_temp)
}

# Calculate average temperatures for each year
yearly_avg <- purrr::map_dbl(c(2003:2022), ~get_mean_sst(.))

avg_temps_plot <- data.frame(year = c(2003:2022),
                             temp = yearly_avg)

theme_set(theme_classic(base_size = 15))
ggplot(avg_temps_plot, aes(x = year,y = temp)) +
  geom_line(cex = 0.5) + 
  labs(x = "Year", y = expression("Average growing season temperature " ( degree~C)))
ggsave("pdf/avg_growing_season_temp_time_series.pdf")

