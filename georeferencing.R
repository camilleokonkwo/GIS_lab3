#### Lab 3 Part 4: Georeferencing and symbolizing points in  R ####

# Three steps to minimize errors

# Step 1: install the packages (need to install only once)
install.packages("tidyverse")    # data management/visualization (several packages)
install.packages("sf")           # GIS package
install.packages("tmap")         # mapping package
install.packages("tmaptools")    # additional tools for tmap 


# Step 2: load packages 
library(tidyverse)
library(sf)
library(tmap)
library(tmaptools)


## Projection with the sf package 

states <- st_read("US_states.shp")
st_crs(states)

qtm(states)

tm_shape(states) +
  tm_polygons()

# Project to EPSG: 3857 Pseudo Mercator 
states_3857 <- st_transform(states,
                            crs = 3857)
states_3857

qtm(states_3857)


# Project to EPSG:5070 - Albers Conic
states_5070 <- st_transform(states,
                            crs = 5070)
states_5070

qtm(states_5070)



##TRI sites 

# Read NJ county-level shp

NJ <- st_read("County_Boundaries_of_NJ.shp")

tm_shape (NJ) +
  tm_polygons()


# Read-in cvs
NJ_TRI <- read_csv("tri_2020_nj.csv")

# Plotting coordinates

TRI <- st_as_sf(NJ_TRI, 
                coords = c("long", "lat"), 
                crs = 4269)

tm_shape(TRI) +
  tm_dots()


# Plotting them together

tm_shape(NJ) +
  tm_polygons(col = "grey90",
              border.col = "white") + 
tm_shape(TRI) +
  tm_dots(col = "brown",
          size = "total_release",
          alpha = .7)



# Spatial Join

NJ$total <- st_join(NJ, left = TRUE, TRI)
  
  
# Need to have the same projection
NJ_prj <- st_transform(NJ, crs = st_crs(TRI))

# Let's try it again 
NJ_prj_join<- st_join(NJ_prj, TRI, left = TRUE) 

NJ_prj_join |> 
group_by(COUNTY) |> 
  summarize(total_emissions = sum(total_release)) -> NJ_TRI_total


# Map it
tm_shape(NJ_TRI_total) +
  tm_polygons(col = "total_emissions",
              style = "jenks",
              n = 4,
              palette = "YlOrBr",
              border.col = "black",
              title = "Total Emissions")


# Replace any null values with zero

NJ_TRI_total[is.na(NJ_TRI_total)] <- 0

# Map it again

tm_shape(NJ_TRI_total) +
  tm_polygons(col = "total_emissions",
              style = "jenks",
              n = 4,
              palette = "YlOrBr",
              border.col = "black",
              title = "Total Emissions (lbs)")
