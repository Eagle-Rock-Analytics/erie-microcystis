### Download buoy station temperature data w. Erie

library(tidyverse)

# Directories
# Buoy data
data_dir <- "buoy_data/"

# Western Erie buoys (W of Sandusky Bay)

buoys <- tibble(stationID = c("mrho1","thlo1","thro1", "sbio1"),
                    start = c(2005, 2005, 2005, 2003),
                    end = c(2020, 2020, 2020, 2020))

# Example path to text file
# "https://www.ndbc.noaa.gov/view_text_file.php?filename=thlo1h2005.txt.gz&dir=data/historical/stdmet/"

# Function to pull standard meteorological variables for a given buoy station/year from NDBC website
# Output: writes csv of data file
get_ndbc_txt <- function(station, year) {
  # Create the url to the text file
  ndbc_path <- paste0("https://www.ndbc.noaa.gov/view_text_file.php?filename=", 
                      station, "h", year, ".txt.gz&dir=data/historical/stdmet/")
  
  # Read in txt file of standard meteorological variables
  met_txt <- read_table2(ndbc_path)
  
  # Write file as csv to buoy data directory
  write.csv(met_txt, paste0(data_dir, "stdmet_", station, "_", year, ".csv"), row.names = F)

}

# Execute data grabbing fn for years/stations of interest

buoy_years <- buoys %>%
  group_by(stationID, start, end) %>%
  nest() %>%
  mutate(data = map2(start, end, ~.x:.y))

map2(buoy_years$stationID, buoy_years$data, ~{
    s <- .x
    
    for(yr in .y) {
      get_ndbc_txt(s, yr)
    }
  })

# Download and clean up river gauge data

# Maumee river near Defiance
# maumee_def <- read_table2("https://waterdata.usgs.gov/nwis/dv?cb_all_=on&cb_00010=on&cb_00060=on&cb_00608=on&cb_00625=on&cb_00631=on&cb_00665=on&cb_00671=on&cb_62961=on&cb_80154=on&cb_80155=on&cb_91007=on&cb_91057=on&cb_91060=on&cb_91061=on&format=rdb&site_no=04192500&referred_module=sw&period=&begin_date=2014-03-03&end_date=2021-10-27", 
#                           comment = "#")
# write.csv(maumee_def, "buoy_data/maumee_defiance_nutrient_temp.csv", row.names = F)

maumee_def <- read_csv("buoy_data/maumee_defiance_nutrient_temp.csv")

# Near Waterville
# maumee_wat <- read_delim("https://nwis.waterservices.usgs.gov/nwis/iv/?sites=04193490&parameterCd=00010&startDT=2011-02-17T00:00:00.000-05:00&endDT=2020-12-31T23:59:59.999-05:00&siteStatus=all&format=rdb",
#                          delim = "\t", skip = 27)[-1, ]
# write.csv(maumee_wat, "buoy_data/maumee_waterville_temp.csv", row.names = F)

maumee_wat <- read_csv("buoy_data/maumee_waterville_temp.csv")



