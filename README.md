# Decadal warming has intensified Microcystis-dominated cyanobacterial blooms in Lake Erie
**Christopher Gobler, Grace Di Cecco, Owen Doherty, Benjamin Kramer**

This repository contains code and data to replicate analyses examining lake surface warming impacts on harmful algal blooms in Western Lake Erie. 

- `R - data analysis glsea` contains scripts to pull lake surface temperature records from [GLSEA](https://apps.glerl.noaa.gov/erddap/info/glerl_p_m_lcb/index.html), conduct main analyses, and produce manuscript figures with these data
- `R - data analysis` contains scripts to conduct main analyses and produce manuscript figures with [GHRSST](https://podaac.jpl.nasa.gov/GHRSST) lake surface temperature data
- `R - growth response curves` contains scripts to fit growth response curves to experimental *Microcystis* data
- `R - lake temperature processing` contains scripts to pull GHRSST lake surface temperature records
- `buoy_comparison` contains scripts to pull buoy temperature records and compare with gridded lake surface temperature sources
- `experiment_data` contains data from laboratory growth experiments for *Microcystis* cultures
- `ncl-trends-glsea` contains NCL scripts to perform trend analysis of warming impacts on *Microcystis* bloom conditions using GLSEA temperature records
- `ncl-trends` contains NCL scripts to perform trend analysis of warming impacts on *Microcystis* bloom conditions using GHRSST temperature records
- `shapefiles` conatains shapefile of Lake Erie borders used in figures and analyses
