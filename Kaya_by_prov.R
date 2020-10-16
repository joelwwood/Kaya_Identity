library(tidyverse)
library(cowplot)

data<-readRDS(file="Kaya_data")


data_for_plot<-data %>%
  mutate(
    EnerInt = Energy/GDP,
    CarbIntEner = CO2/Fossil
      ) %>%
  group_by(GEO)%>%
  mutate(
    GDP = GDP/GDP[Year==1995],
    CO2 = CO2/CO2[Year==1995],
    EnerInt=EnerInt/EnerInt[Year==1995],
    CarbIntEner=CarbIntEner/CarbIntEner[Year==1995],
    ShareFossil=ShareFossil/ShareFossil[Year==1995],
    GEO=as.factor(GEO)
      ) %>%
  filter(GEO!="Manitoba")%>%
   select(Year,GEO,CO2,GDP,EnerInt,CarbIntEner,ShareFossil)%>%
  pivot_longer(cols=3:7,names_to="Variable",names_ptypes = list(Variable = factor()),values_to="Value")

GEO_levels<-levels(data_for_plot$GEO)

data_for_plot$GEO <-fct_relevel(data_for_plot$GEO,GEO_levels[c(4,3,1,5,6,7,2)]) 


data_for_plot %>%
  ggplot(aes(Year,Value,colour=Variable))+
  geom_line(size=1)+
  facet_wrap(~GEO)+
  theme_minimal_hgrid(16)+
  colorblindr::scale_color_OkabeIto()+
  scale_x_continuous(breaks=c(1995,2005,2015))+
  labs(y="Index (1995=1.0)")



data %>%
  ggplot(aes(Year,Energy,group=GEO))+
  geom_line()+
  facet_wrap(~GEO)

filter(data,GEO=="Manitoba")
