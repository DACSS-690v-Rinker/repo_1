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
library(sf)

# _________________________________________________________________


zip_pop_codebook<- as_tibble (read_excel("C:\\Users\\RinkerD\\OneDrive - UMass Chan Medical School\\FMCH\\1. ARC - PBRN\\UMMS clinical sites database\\ARC-\\source files\\MA_ZIP_population.xlsx" , sheet = "code_book"))
names(clinics)

# population<- as_tibble (read_excel("C:\\Users\\RinkerD\\OneDrive - UMass Chan Medical School\\FMCH\\1. ARC - PBRN\\UMMS clinical sites database\\ARC-\\source files\\MA_ZIP_population.xlsx" , sheet = "MA_ZIP_population")) 
#   select (GEO_ID,NAME ,DP05_0001E)
  
  population_raw<- as_tibble (read_csv("C:\\Users\\RinkerD\\OneDrive - UMass Chan Medical School\\FMCH\\1. ARC - PBRN\\UMMS clinical sites database\\ARC-\\source files\\census data\\DECENNIALDP2020.DP1-Data.csv" ))
    population <- population_raw %>%
    select (GEO_ID,NAME ,DP1_0001C)  
str(population)

population <- population %>%
  mutate(zip_code =  factor(gsub("ZCTA5 ", "", NAME))
         , population = as.numeric(DP1_0001C ) )%>%
filter(zip_code != "Geographic Area Name",zip_code != "Massachusetts" ) %>%
  filter (population  != 0)

unique_zip_count <- length(unique(population$zip_code)) # has to be 538 physical codes 


summary(population$population)
class(population$population)
# Getting a map for base layer: -------------------------------------------
library (ggmap)
library(maps)
library(tigris)
# install.packages("tidycensus")
library(tidycensus )
ma_zips <- zctas(cb = F, state = "MA", year = 2010)

base=ggplot(data = ma_zips) # map to use


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
summary(merged_data$population)
 
summary(merged_data$ZCTA5CE10)


# Calculating density -------------------------
library(units)
library(sf)
merged_data$area_km2=drop_units(set_units(st_area(st_make_valid(merged_data$geometry)),'km2')) 

area_boxplot <- ggplot(merged_data, aes(x=area_km2))+
  geom_boxplot()+
  labs(title ="Area boxplot")
summary(merged_data$area_km2)
area_boxplot
popul_boxplot <- ggplot(merged_data, aes(x=area_km2))+
  geom_boxplot()+
  labs(title ="Population boxplot")
popul_boxplot
summary(merged_data$population)

merged_data<-merged_data %>%
  mutate(density= population/area_km2)

density_boxplot <- ggplot(merged_data, aes(x=density))+
  geom_boxplot()+
  labs(title ="density boxplot")
density_boxplot
summary(merged_data$density)

row_for_02108 <- merged_data[merged_data$density  >8527710, ]
merged_data<-merged_data %>%
  filter(density  <8527710)

MAP=ggplot(data = merged_data) +
  geom_sf(
    aes(fill=density), #variable for coloring geometry
    color=NA)+ # no borders
  scale_fill_viridis_c(direction = -1) # color map
MAP

# Discretizing density  -------------------------------

# The range of density is too great to visualize differences within rural areas of MA.


 # I  am going to discretize the upper levels of density to bring the entire scale down.

# merged_data<-merged_data %>%
# mutate (density_cat = ifelse(density>500, 500,density ))
# summary(merged_data$density_cat)
# vector of data breaks
customCuts=c(0,10,100,1000,10000, 100000)

merged_data$density_cat=cut(merged_data$density,
                                     breaks=customCuts,
                                     include.lowest = T,
                                     dig.lab = 5)
summary(merged_data$density_cat)
#Labels 
# labels
labels =c("up to 10",">10 to 100", ">100 to 1000", ">1000 to 10K" , '10K to 100K'  )


table(merged_data$density_cat)
levels(merged_data$density_cat)=labels



titleText<- "Rurality map of Massachussetts"
sub_titleText<- "by zip code"
sourceText <- "Source: US Census Bureau (https://data.census.gov)"

MAP_cat<- ggplot(data = merged_data) +
  geom_sf(
    aes(fill=density_cat), #variable for coloring geometry
    color=NA)+ # no borders
  scale_fill_brewer(palette = "YlOrRd") +
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
MAP_cat

### Annotating: --------------

ZipOk=c("up to 10")
zip_to_annotate <- merged_data[merged_data$density_cat%in%ZipOk,]
zip_to_annotate

MAP_cat<- ggplot(data = merged_data) +
  geom_sf(
    aes(fill=density_cat), #variable for coloring geometry
    color=NA)+ # no borders
  scale_fill_brewer(palette = "YlOrRd") +
  labs(title=titleText,
       subtitle = sub_titleText,
       fill = "Density Category\n (people/km2)",  # Title for the legend
       caption = sourceText)+
  theme(
    axis.ticks = element_blank(), # Remove axis ticks
    axis.text.x = element_blank(), # Remove x-axis text
    axis.text.y = element_blank(), # Remove y-axis text
    axis.title.x = element_blank(), # Remove x-axis title
    axis.title.y = element_blank())+  # Remove y-axis title 
    geom_sf_text(data=zip_to_annotate,
                 aes(label=round(density,0),
                     color=density_cat),
                 check_overlap = T,
                 size=1.89,
                 nudge_y = 0.01) +
      scale_colour_manual(values="black") +
      guides(color=F)

    MAP_cat

# Plotting clinics' coordinates ------------------------------

clinics<- as_tibble (read_csv("C:\\Users\\RinkerD\\OneDrive - UMass Chan Medical School\\FMCH\\1. ARC - PBRN\\UMMS clinical sites database\\ARC-\\outputs and reports - excel only\\power_bi\\practices_geo_list.csv" )) 
# str(clinics)  

library(raster)
mapCRS=crs(merged_data) # projection of our map of zipcodes  

data_for_Points <- st_as_sf(clinics,# data frame
                  coords = c("lon","lat"), # form the data frame
                  remove = TRUE, # remove lon/lat
                  crs=mapCRS) # projection for spatial object
class(data_for_Points)
clinics_map <- MAP_cat + 
            geom_sf(data=data_for_Points, 
                    size=0.9,
                    alpha=0.8) 
clinics_map  

# Interactive map  ------------------------------

library(leaflet)
#subsetting - not needed : 

merged_data
paletteFun = colorFactor(palette = "YlOrRd", 
                       domain = merged_data$density_cat)
#popup labels when using your cursor
popUp_labels <- sprintf("<strong>%s</strong>",
                        merged_data$density_cat) %>% lapply(htmltools::HTML)

# theMAP subset: republicans 2012

# the base map: the WA boundaries (optional)
base_map = leaflet() %>% 
  addTiles()%>%
  addPolygons(data = merged_data,
              fillColor = "white",
              weight = 0,
              fillOpacity = 0.5)
base_map

# adding clinics 
interactive_map = base_map %>%
  addPolygons(data=merged_data,
              stroke = F, # borders of polygon?
              opacity =  0.5, # # the closer to 0 the more transparent
              fillOpacity = 0.3, # color brigthness
              fillColor = ~paletteFun(density_cat),# coloring
              label = popUp_labels, 
              labelOptions = labelOptions(
                style = list("font-weight" = "normal"),
                textsize = "15px",
                direction = "auto"))%>%
  addCircleMarkers(
                  data = clinics, # Your dataset with lat/lon
                  lat = ~lat, # Column name for latitude
                  lng = ~lon, # Column name for longitude
                  radius = 1, # Marker size
                  color = "blue", # Marker border color
                  # fillColor = "red", # Marker fill color
                  fillOpacity = 0.7# Transparency of the marker
                 # , popup = ~location_name # Column name for popup text
                )

interactive_map
#adding a legend:

interactive_map <- interactive_map  %>% 
  addLegend(data=merged_data,
                      position = "bottomright",
                      pal = paletteFun,
                      values = ~density_cat,
                      title = "MA rurality map and ARC clinics",
                      opacity = 1) 
interactive_map

# Creating layers for filter

map10=merged_data[merged_data$density_cat==labels[1],]
map100=merged_data[merged_data$density_cat==labels[2],]
map1000=merged_data[merged_data$density_cat==labels[3],]
map10000=merged_data[merged_data$density_cat==labels[4],]
map100000=merged_data[merged_data$density_cat==labels[5],]


PuOr=c("#FFA501","#FFD700", "#E4D69C",'#b2abd2','#5e3c99')

range(merged_data$density  )
# one layer per group
layer1 <- leaflet() %>% 
  addTiles() %>%
  addPolygons(data=map10,
              color=PuOr[1],
              fillOpacity = 1, 
              stroke = F,
              group = labels[1]) # LAYER as GROUP

layer1_2= layer1%>%addPolygons(data=map100,
                               color=PuOr[2],
                               fillOpacity = 0.5,
                               stroke = F,
                               group = labels[2])


layer1_2_3= layer1_2%>%addPolygons(data=map1000,
                                   color=PuOr[3],fillOpacity = 0.5,stroke = F,
                                   group = labels[3])

layer1_2_3_4= layer1_2_3%>%addPolygons(data=map10000,
                                       color=PuOr[4],fillOpacity = 1,stroke = F,
                                       group = labels[4])


final_MAP= layer1_2_3_4%>%addPolygons(data=map100000,
                                       color=PuOr[5],fillOpacity = 1,stroke = F,
                                       group = labels[5])

final_MAP
# textFun="function(btn, map){map.setView([42.2896832, -71.7877932],9)}"
# 
# final_MAP= final_MAP %>%
#   addEasyButton(
#     easyButton(icon="fa-home", # a symbol
#                title="Zoom to Level 1",
#                onClick=JS(textFun)))
# 
# final_MAP

# row_for_01605 <- merged_data[merged_data$ZCTA5CE10  == "01605", ]


# Adding filtered legend
final_MAP=final_MAP %>% addLayersControl(
  overlayGroups = labels,
  options = layersControlOptions(collapsed = FALSE))

final_MAP<- final_MAP %>%
  addCircleMarkers(
    data = clinics, # Your dataset with lat/lon
    lat = ~lat, # Column name for latitude
    lng = ~lon, # Column name for longitude
    radius = 1, # Marker size
    color = "blue", # Marker border color
    # fillColor = "red", # Marker fill color
    fillOpacity = 0.7# Transparency of the marker
    # , popup = ~location_name # Column name for popup text
  ) %>% addTiles() %>%
  addControl("<h3 style='text-align:center;'> MA rurality and ARC clinics locations <br> Source: U.S. Census Bureau </h3>", position = "bottomleft")


  # addLegend(data=merged_data,
  #           position = "bottomright",
  #           pal = paletteFun,
  #           values = ~density_cat,
  #           title = "MA rurality map and ARC clinics",
  #           opacity = 1) 

final_MAP


# Final file  ------------------------------
saveRDS(final_MAP, file = "HW_3_Rinker.rds")





















