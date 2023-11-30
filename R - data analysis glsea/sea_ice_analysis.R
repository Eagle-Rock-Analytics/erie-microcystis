## Sea ice analysis

library(tidyverse)
library(lubridate)

# Data directories
sst_dir <- "sea_surface_data"

# Daily average ice cover Lake Erie

erie_ice <- read.table(file = "https://www.glerl.noaa.gov/data/ice/daily/eri.txt",
                       row.names = 1)

ice_cleaned <- erie_ice %>%
  # dplyr::select(X2003:X2020) %>%
  rownames_to_column(var = "date") %>%
  pivot_longer(X1973:X2021, names_to = "year", values_to = "ice") %>%
  mutate_at(c("year"), ~substr(., 2,5)) %>%
  mutate(month = word(date, 1, 1, sep = "-"),
         day = word(date, 2, 2, sep = "-"),
         month_num = match(month, month.abb),
         date_num = as.Date(paste0(year, "/", month_num, "/", day)),
         doy = yday(date_num)) %>%
  mutate_at(c("ice"), ~replace_na(., 0)) %>%
  group_by(year) %>%
  nest() %>%
  mutate(first_zero = map_dbl(data, ~{
    df <- .
    
    first_zero <- df %>%
      filter(doy > 58 & doy < 300) %>% # Want to look for zeroes only after February 28 and before fall
      filter(ice == 0 & doy == min(doy[ice == 0], na.rm = T)) # NA dates in cases where it is not a leap year (but 2/29 in data table)
      
    first_zero$doy

  }))

ice_plot <- ice_cleaned %>%
  dplyr::select(-data) %>%
  ungroup()

theme_set(theme_classic(base_size = 15))
ggplot(ice_cleaned, aes(x = as.numeric(year), y = first_zero)) + geom_line(cex = 0.5) +
  labs(x = "Year", y = "First ice-free day (day of year)")
ggsave("pdf/first_ice_free_time_series.pdf")
