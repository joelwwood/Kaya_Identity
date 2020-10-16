
library(tidyverse)

library(lubridate)

#data<-get_cansim("25-10-0029-01")
data<-readRDS("Data/energy_data")

data$REF_DATE<-parse_integer(data$REF_DATE)


data<- data %>%
  select(REF_DATE, GEO, `Fuel type`,`Supply and demand characteristics`, VALUE) %>%
  rename(
    Year=REF_DATE,
    Region=GEO,
    Fuel=`Fuel type`,
    Variable=`Supply and demand characteristics`
  )


data.prov<- data %>%
  filter(Variable=="Net supply"|Variable=="Energy use, final demand")%>%
  filter(Fuel %in% unique(data$Fuel)[c(1,2,12,14:16,28)]) %>%
  pivot_wider(names_from=Fuel,values_from=VALUE) 
  

#QC and BC are missing net-supply and final demand data for 2017 and 2018 for "Total primary and secondary energy"
#but it is possible to get an estimate of net-supply by adding up net-supply for "Primary energy","Total refined petroleum products", and "Secondary electricity, thermal"
#it is the Coke data that is redacted and are preventing the release of the total net-Supply number
data.prov<-data.prov %>%
   mutate(
    estimate = rowSums(data.prov[,c(5,9:10)],na.rm=TRUE),
    diff = `Total primary and secondary energy`-estimate
  )

#draw graph to check to see how well the estimate for BC and QC net-supply matches up to the StatCan values 1995-2016
data.prov %>%
  filter(Region=="Quebec"|Region=="British Columbia", Variable=="Net supply") %>%
  ggplot(aes(Year,diff/`Total primary and secondary energy`,colour=Region))+
  geom_line() #For BC the difference is almost always less than 0.1% of the StatCan value; and for QC, it is always less than 1%


data.prov<- data.prov %>%
  rename(
    Energy=`Total primary and secondary energy`
  ) %>%
  mutate(Energy=ifelse(is.na(Energy),estimate,Energy)) %>%
  mutate( RenewableEnergy=ifelse(Variable=="Net supply",`Primary electricity, hydro and nuclear`,"NA")) %>%
  select(Year,Region,Variable,Energy,RenewableEnergy) 


data.renewables<- data.prov %>%
  filter(Variable=="Net supply") %>%
  select(Year, Region,Variable, RenewableEnergy)

data_energy<-data.prov %>%
  select(-RenewableEnergy) %>%
  pivot_wider(names_from=Variable,values_from=Energy) %>%
  mutate( Energy = ifelse((Region=="Quebec"|Region=="British Columbia"),`Net supply`,`Energy use, final demand`)) %>%
  full_join(data.renewables,by=c("Year","Region")) %>%
  select(Year,Region,Energy,RenewableEnergy)
  

  




#Get sectoral energy use data. Maybe use later
data_sectoral<- data %>%
  filter(Variable %in% unique(data$Variable)[17:43])
  



#Get GDP data by Province (for sectoral GDP by province, use Table 36-10-0402-01)
#data<-get_cansim("36-10-0222-01") %>%
  #normalize_cansim_values
data<-readRDS("Data/gdp_data")

#Remove some unimportant columns that complicate rearranging the data
data$REF_DATE<-parse_integer(data$REF_DATE)
names_to_rm<-c(3,6:9,11:20)
colnames(data)[names_to_rm]

data<-data %>%
  select(-colnames(data)[names_to_rm]) %>%
  filter(Prices=="Chained (2012) dollars",Estimates=="Gross domestic product at market prices") %>%
  rename(
    Year=REF_DATE,
    Region=GEO,
    GDP=VALUE
  ) %>%
  select(Year,Region,GDP)

#Create GDP for Atlantic Provinces in case we want to group them
atl_provs<-unique(data$Region)[2:5]

atl_gdp<- data %>%
  filter(Region %in% atl_provs) %>%
  group_by(Year) %>%
  summarise( "Atlantic provinces" = sum(GDP)) %>%
  pivot_longer("Atlantic provinces",names_to="Region",values_to="GDP")



data_gdp<-bind_rows(data,atl_gdp)


#Get Greenhouse Gas Emissions Data from Environment Canada
#ghg_data<-read_csv("http://donnees.ec.gc.ca/data/substances/monitor/canada-s-official-greenhouse-gas-inventory/GHG_IPCC_Can_Prov_Terr.csv")
ghg_data<-readRDS("Data/ghg_data")


atl_ghgs<- ghg_data %>%
  filter(Region %in% atl_provs,Category=="TOTAL") %>%
  group_by(Year) %>%
  summarise( "Atlantic provinces" = sum(CO2eq)*1000) %>%
  pivot_longer("Atlantic provinces",names_to="Region",values_to="CO2")

data_ghg<-ghg_data %>%
  mutate( CO2eq = CO2eq*1000) %>%
  filter(Category=="TOTAL") %>% 
  select(Year,Region,CO2eq) %>%
  rename(CO2=CO2eq) %>%
  bind_rows(.,atl_ghgs)



#next step is combining the tibbles by (Year,Region)

full_data<-data_gdp %>%
  left_join(data_ghg,by=c("Year","Region")) %>%
  left_join(data_energy,by=c("Year","Region")) 

full_data %>%
  filter(Year>1994) %>%
  saveRDS(file="Kaya_data")



