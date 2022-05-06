## Plotting and understading the climate data this is the results of clustering done in JMP


library(ggplot2)
library(dplyr)
library(tidyverse)
library(ggpubr)
library(terra)
library(sp)
# library(sf)
# install.packages("sf")

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



### plot the modelled climate data - how well are they correlated?

climate_grid_values_wide %>%  
  ggplot( mapping = aes(DOF, DOV)) +
  geom_point() +
  geom_smooth(method = lm, se = FALSE, na.rm = TRUE) +
  stat_regline_equation(
    #aes(label =  paste(..eq.label.., ..rr.label.., sep = "~~~~")),
    aes(label =  paste( ..rr.label..)),
    formula = (y ~ x)
  ) +
  theme_bw()+
  theme(legend.position = "none")+
  labs(title = "Modelled climate data",
       x = "Day of flowering", y = "Day of verasion")+
  facet_wrap(.~ year)

climate_grid_values_wide %>%  
  ggplot( mapping = aes(DOF, DOH)) +
  geom_point() +
  geom_smooth(method = lm, se = FALSE, na.rm = TRUE) +
  stat_regline_equation(
    #aes(label =  paste(..eq.label.., ..rr.label.., sep = "~~~~")),
    aes(label =  paste( ..rr.label..)),
    formula = (y ~ x)
  ) +
  theme_bw()+
  theme(legend.position = "none")+
  labs(title = "Modelled climate data",
       x = "Day of flowering", y = "Day of harvest")+
  facet_wrap(.~ year)

climate_grid_values_wide %>%  
  ggplot( mapping = aes(DOV, DOH)) +
  geom_point() +
  geom_smooth(method = lm, se = FALSE, na.rm = TRUE) +
  stat_regline_equation(
    #aes(label =  paste(..eq.label.., ..rr.label.., sep = "~~~~")),
    aes(label =  paste( ..rr.label..)),
    formula = (y ~ x)
  ) +
  theme_bw()+
  theme(legend.position = "none")+
  labs(title = "Modelled climate data",
       x = "Day of verasion", y = "Day of harvest")+
  facet_wrap(.~ year)
  


###########################################################################################
### extract the raw vineyard data vs modelled data.

#load a study vineyards with captured data 
study_vineyards <- vect("V:/Marlborough regional/climate/modelled_climate_vs_vineyard/for_mapping_Aug2021_edit.shp")
study_vineyardss_df <- as.data.frame(study_vineyards)

names(study_vineyardss_df)
study_vineyardss_df <- study_vineyardss_df %>% 
  rename(x = x_coord,
         y = y_coord)

### not all sites have harvest date data
study_vineyards_DOH <- terra::subset(study_vineyards, study_vineyards$julian > "0")
study_vineyards_DOH_df <- as.data.frame(study_vineyards_DOH)

study_vineyards_DOH_df <- study_vineyards_DOH_df %>% 
  rename(x = x_coord,
         y = y_coord)

dim(study_vineyards_DOH) 
dim(study_vineyards) 

study_vineyards_DOH
plot(study_vineyards)


names(study_vineyards)

# load the modlled climate data as tif

## list in the climate data
list.files("V:/Marlborough regional/climate/climate_data_2022_vineyards_R/")
            

climate_type <-"DOH"     #"rain""GST""GDD" "DOH"DOF""DOV"

climate_files <- list.files("V:/Marlborough regional/climate/climate_data_2022_vineyards_R/",
                            pattern = climate_type)
#View(climate_files)

climate_files <- as.data.frame(climate_files)

climate_tiff <- climate_files %>% 
  dplyr::filter(str_detect(climate_files,".tiff")) %>% 
  dplyr::filter(str_detect(climate_files,"aux.xml", negate = TRUE))

climate_tiff$climate_files <- as.character(climate_tiff$climate_files)
#Turn into a list

file_list <- climate_tiff[["climate_files"]] 

#file_list
file_list <- file_list[1]#2013
#file_list <- file_list[2]
#file_list <- file_list[3]
#file_list <- file_list[4] #16
#file_list <- file_list[5] #17
#file_list <- file_list[6] #18
#file_list <- file_list[7] #19
#file_list <- file_list[8] #20
file_list <- file_list[9] #21
file_list




## this will be the loop
#for (file_list in file_list){
  
  
  ## get the year from the file name
  step1<-sub(".tiff*", "", file_list) # Extract characters before pattern # for the rainfall files AND doh
  bit_to_extract <- paste0(".*",climate_type,"_proj_resample_mask_" )
  year2 <-sub(bit_to_extract, "", step1)# Extract characters after pattern
  year <- str_sub(year2,5,8)
  rm(step1, year2, bit_to_extract)
  
  ## bring in the climate raster 
  climate <- rast(paste0("V:/Marlborough regional/climate/climate_data_2022_vineyards_R/",
                         file_list)) 
  ## extract the points for the cliamte raster
  climate <- terra::extract(  x = climate,
                              y = study_vineyards_DOH,
                              xy=TRUE)
  #names(climate)
  names(study_vineyards_DOH_df)
  #colnames(study_vineyardss_df) <- c("ID",climate_type,"x", "y" ) 
  
  climate <- climate %>% dplyr::mutate(year = year,
                                       for_join = paste0(x, "_", y))
  study_vineyards_DOH_df <- study_vineyards_DOH_df %>% dplyr::mutate(for_join = paste0(x, "_", y))
  
  str(climate)
  str(study_vineyards_DOH_df)
  study_vineyards_DOH_df <- study_vineyards_DOH_df %>% 
    dplyr::rename(year_vineyard_data = year) %>% 
    dplyr::select(-x, - y)
  
  climate <- left_join(climate, study_vineyards_DOH_df, by = "for_join")
  
  name <- paste0(climate_type,"_", year)
  assign(name,climate)  
  rm(climate, name,year)
  
#} 

#Tidy up what I have for each year
str(DOH_2013)
DOH_2013 <- DOH_2013 %>% 
  rename( DOH = Dateof200g20122013cor_SB_1.csv_B_gissmall_Ad)

DOH_2013 <- DOH_2013 %>% 
  dplyr::filter(DOH > 0)
DOH_2013 <- DOH_2013 %>% 
  dplyr::filter(julian > 0)
DOH_2013 <- DOH_2013 %>% 
  dplyr::filter(year_vineyard_data == 2013)
DOH_2013 <- DOH_2013 %>% 
  dplyr::select(ID, x,y, year, DOH, julian )
str(DOH_2013)
DOH_2013$julian <- as.double(DOH_2013$julian)


DOH_2013 %>%  
  ggplot( mapping = aes(julian, DOH)) +
  geom_point() +
  geom_smooth(method = lm, se = FALSE, na.rm = TRUE) +
  stat_regline_equation(
    #aes(label =  paste(..eq.label.., ..rr.label.., sep = "~~~~")),
    aes(label =  paste( ..rr.label..)),
    formula = (y ~ x)
  ) +
  theme_bw()+
  theme(legend.position = "none")+
  labs(title = "Modelled climate data vs collected data",
       x = "Observed Day of harvest", y = "Modlled day of harvest")+
  facet_wrap(.~ year)



###########################################################################################
### extract the kriged vineyard data vs modelled data.
  