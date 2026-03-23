
#
# Identify river plains (fluvial and glacial plains)
#  

# Map data downloaded:
# Løsmasser
# - https://kartkatalog.geonorge.no/metadata/loesmasser/3de4ddf6-d6b8-4398-8222-f5c47791a757  


# Packages ----
library(sf)
library(dplyr)
library(leaflet)
library(ggplot2)
library(purrr)
library(ggeasy)
# library(RColorBrewer)

# Create a temporary folder and unzip the file there
unzip_dir <- tempdir()


#
# Fluvial deposit areas ----
# 
# from "Løsmasser" (land deposits) data  
#

# unzip the file there
unzip("../../Mapdata/Geologi_0000_Norge_25833_Losmasser_FGDB.zip", exdir = unzip_dir)

# List the files in the unzipped directory
dir(unzip_dir)

# Select file and list layers  
file <- list.files(unzip_dir, pattern = "Losmasse.gdb", full.names = TRUE)
st_layers(file)

# Select and read GML file  
polygons <- st_read(file, layer = "LosmasseFlate")

head(polygons, 2)
# head(polygons, 2) %>% 
tab_all <- polygons %>% 
  as.data.frame() %>% 
  count(losmassetype, losmassetype_navn)
# tab_all

#
# Fluvial deposit areas ----
# 

if (FALSE){
  # run this to check 'losmassetype' manually
  polygons %>% 
    as.data.frame() %>% 
    count(losmassetype, losmassetype_navn) %>% 
    View("losmassetype")
}

#
# select areas
#

polygons %>% 
  as.data.frame() %>% 
  filter(losmassetype %in% c(50,51,52,56,57)) %>% 
  count(losmassetype, losmassetype_navn)

# define fluvial 
# (drop flood deposits, as they are extremely few)
fluvial <- polygons %>% 
  filter(losmassetype_navn %in% "Elve- og bekkeavsetning (Fluvial avsetning)")

nrow(fluvial)  # 12958

# transform to long-lat
fluvial_ll <- fluvial %>% 
  st_transform(mapdata, crs = 4326)

# . all polygons, map and plots ----  

# leaflet map (a bit slow)
leaflet(fluvial_ll) %>% 
  addTiles() %>% 
  addPolygons(weight = 1)  

# plots of sizes  
fluvial_ll %>% 
  as.data.frame() %>%
  ggplot(aes(x = SHAPE_Area/1E6, y = SHAPE_Length/1E3)) +
  geom_point() +
  labs(x = "Area (km2)", y = "Circumference (km)")

fluvial_ll %>% 
  as.data.frame() %>%
  mutate(area_length_ratio = SHAPE_Area/SHAPE_Length) %>% 
  ggplot(aes(x = SHAPE_Area/1E6, y = area_length_ratio)) +
  geom_point() +
  labs(x = "Area (km2)", y = "Area/length ratio")


#
# . fluvial polygons > 8 km2 ----
#
fluvial_ll_sel <- fluvial_ll%>% 
  filter(SHAPE_Area >= 8*1E6)
nrow(fluvial_ll_sel)
sum(fluvial_ll_sel$SHAPE_Area)/1E6 

fluvial_ll_sel %>% 
  filter(SHAPE_Area >= 5*1E6) %>% 
  leaflet() %>% 
  addTiles() %>% 
  addPolygons(weight = 1, color = "red")  

fluvial_ll_sel %>% 
  filter(SHAPE_Area >= 5*1E6) %>% 
  leaflet() %>% 
  addTiles() %>% 
  addPolygons(weight = 1)  

#
# Glacial deposit areas ----
#

polygons %>% 
  as.data.frame() %>% 
  filter(grepl("bre", losmassetype_navn, ignore.case = TRUE)) %>% 
  count(losmassetype, losmassetype_navn)

# define fluvial 
# (drop flood deposits, as they are extremely few)
glacial <- polygons %>% 
  filter(grepl("bre", losmassetype_navn, ignore.case = TRUE)) %>% 
  filter(SHAPE_Area >= 8*1E6)

glacial_ll <- glacial %>% 
  st_transform(mapdata, crs = 4326)

#
# . fluvial polygons > 1 km2 ----
#
glacial_ll_sel <- glacial_ll%>% 
  filter(SHAPE_Area >= 1*1E6)

leaf_fluvial <- fluvial_ll_sel %>% 
  leaflet()

#
# Fluvial and glacial deposits in same leaflet map ----
#
glacial_ll_sel %>% 
  leaflet() %>% 
  addTiles() %>% 
  addPolygons(weight = 1, color = "blue") %>% 
  addPolygons(weight = 1, color = "red", data = fluvial_ll_sel)  


