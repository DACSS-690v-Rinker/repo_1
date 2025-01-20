# rm(list = ls()) # start fresh
# rm(list=ls())
library(tidyverse)
library(tidyr)
library(dplyr)
library(readr)
library(lubridate)
library(REDCapR)
library(haven)
library(sf) 
library(ggplot2)
library (readxl)

# _________________________________________________________________


clinics<- as_tibble (read_excel("C:\\Users\\RinkerD\\OneDrive - UMass Chan Medical School\\FMCH\\1. ARC - PBRN\\UMMS clinical sites database\\ARC-\\outputs and reports - excel only\\Clinics\\ARC.site.description.xlsx" )) 
zip_pop_codebook<- as_tibble (read_excel("C:\\Users\\RinkerD\\OneDrive - UMass Chan Medical School\\FMCH\\1. ARC - PBRN\\UMMS clinical sites database\\ARC-\\source files\\MA_ZIP_population.xlsx" , sheet = "code_book"))
names(clinics)
names(zip_pop_codebook)
# 
# population<- as_tibble (read_excel("C:\\Users\\RinkerD\\OneDrive - UMass Chan Medical School\\FMCH\\1. ARC - PBRN\\UMMS clinical sites database\\ARC-\\source files\\MA_ZIP_population.xlsx" , sheet = "MA_ZIP_population")) 
#   select (GEO_ID,NAME ,DP05_0001E)
  
  population_raw<- as_tibble (read_csv("C:\\Users\\RinkerD\\OneDrive - UMass Chan Medical School\\FMCH\\1. ARC - PBRN\\UMMS clinical sites database\\ARC-\\source files\\census data\\DECENNIALDP2020.DP1-Data.csv" ))
    population <- population_raw %>%
    select (GEO_ID,NAME ,DP1_0001C)  
str(population)

population <- population %>%
  mutate(zip_code =  factor(gsub("ZCTA5 ", "", NAME))
         , population = DP1_0001C ) %>%
filter(zip_code != "Geographic Area Name",zip_code != "Massachusetts" )

unique_zip_count <- length(unique(population$zip_code)) # has to be 538 physical codes 


# Getting a map for base layer: -------------------------------------------
library (ggmap)
library(maps)
library(tigris)
install.packages("tidycensus")
library(tidycensus )
ma_zips <- zctas(cb = F, state = "MA", year = 2010)
select(lon = long, lat, group, id = subregion)
base=ggplot(data = ma_zips) # map to use

length(ma_zips$ZCTA5CE10)


base + geom_sf(fill='black') # plot the geometry


# ma_zips<-ma_zips %>% 
#   mutate (ma_zips =factor(ZCTA5CE10) )
#  Merging Ma map and  data  ---------
names(ma_zips)
names(zip_population_data)
names(clinics)
str(merged_data)

merged_data=merge(ma_zips,population,
                     by.x='ZCTA5CE10', # 
                     by.y='zip_code') %>%
  mutate (population = as.numeric(population))
str(merged_data)
# Calculating population  density -------------------------
library(units)
merged_data$area_km2=drop_units(set_units(st_area(st_make_valid(merged_data)),'km2'))      # setting units? 
merged_data$density=merged_data$population/merged_data$area_km2

MAP=ggplot(data = merged_data) +
  geom_sf(
    aes(fill=density), #variable for coloring geometry
    color=NA)+ # no borders
  scale_fill_viridis_c(direction = -1) # color map
MAP
range (merged_data$density)
summary(merged_data$density)

# The range of density is too great to visualize thedifferences within rural areas of MA.
 # I  am going to compress the upper levels of density to bring the entire scale down.
merged_data<-merged_data %>%
mutate (density_cut = ifelse(density>500, 500,density ))
summary(merged_data$density_cut)

titleText<- "Rurality map of Massachussetts"
sub_titleText<- "by zip code"
sourceText <- "Source: US Census Bureau (https://data.census.gov)"

MAP_cut=ggplot(data = merged_data) +
  geom_sf(
    aes(fill=density_cut), #variable for coloring geometry
    color=NA)+ # no borders
  scale_fill_viridis_c(direction = -1)+
  labs(title=titleText,
       subtitle = sub_titleText,
       fill = "Density Category\n (people/km2)",  # Title for the legend
       caption = sourceText)+
  theme(
    axis.ticks = element_blank(), # Remove axis ticks
    axis.text.x = element_blank(), # Remove x-axis text
    axis.text.y = element_blank(), # Remove y-axis text
    axis.title.x = element_blank(), # Remove x-axis title
    axis.title.y = element_blank()  # Remove y-axis title 
  )
MAP_cut


saveRDS(MAP_cut, file = "HW_3_Rinker.rds")
