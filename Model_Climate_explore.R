## Plotting and understading the climate data this is the results of clustering done in JMP


library(ggplot2)
library(dplyr)
library(tidyverse)


#### modelled data

climate_grid_values <- read.csv("V:/Marlborough regional/climate/climate_data_2022_vineyards_R/Climate_data_as_pts/Climate_all_JMP_IN.csv")

# summary(climate_grid_values)
names(climate_grid_values)


climate_grid_values <- climate_grid_values %>%
  dplyr::select(for_join:Region_Nam,
                DOV_2013:DOH_2021)


names(climate_grid_values)

### needs to be long format
climate_grid_values_narrow <- climate_grid_values %>% 
pivot_longer(cols = DOV_2013:DOH_2021,
             names_to = "variable_year",
             values_to = "value"
)

head(climate_grid_values_narrow)


climate_grid_values_narrow <- climate_grid_values_narrow %>% 
  separate(variable_year, c("variable", "year"), sep = "_" )
head(climate_grid_values_narrow)


## I need to make it wide again? so I have clm for year and then a clm for each variable
climate_grid_values_wide <- climate_grid_values_narrow %>% 
  pivot_wider(names_from = variable,
              values_from = value)

head(climate_grid_values_wide)

climate_grid_values_wide %>% 
  ggplot(aes(DOF, DOH))+
  geom_point()+
  facet_wrap(.~ year)

climate_grid_values_wide %>% 
  ggplot(aes(DOV, DOH))+
  geom_point()+
  facet_wrap(.~ year)
  