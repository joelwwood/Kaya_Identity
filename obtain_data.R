library(tidyverse)
library(cansim)
library(lubridate)

energy_data_raw<-get_cansim("25-10-0029-01")

saveRDS(energy_data_raw,"Data/energy_data")


#Get GDP data by Province (for sectoral GDP by province, use Table 36-10-0402-01)
gdp_data_raw<-get_cansim("36-10-0222-01") %>%
  normalize_cansim_values
saveRDS(gdp_data_raw,"Data/gdp_data")


#Get GDP data by Province and Sector
gdp_sectoral_data_raw<-get_cansim("36-10-0402-01") %>%
  normalize_cansim_values

saveRDS(gdp_sectoral_data_raw,"Data/gdp_sectoral_data")

#Get Greenhouse Gas Emissions Data from Environment Canada
ghg_data_raw<-read_csv("http://donnees.ec.gc.ca/data/substances/monitor/canada-s-official-greenhouse-gas-inventory/GHG_IPCC_Can_Prov_Terr.csv")
saveRDS(ghg_data_raw,"Data/ghg_data")


