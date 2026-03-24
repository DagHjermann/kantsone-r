
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
  st_transform(crs = 4326)

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
fluvial_ll_sel <- fluvial_ll %>% 
  filter(SHAPE_Area >= 8*1E6)
nrow(fluvial_ll_sel)
sum(fluvial_ll_sel$SHAPE_Area)/1E6 

fluvial_ll_sel %>% 
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

#
# Combine fluvial and glacial deposits in same leaflet map ----
#
glacial_ll_sel %>% 
  leaflet() %>% 
  addTiles() %>% 
  addPolygons(weight = 1, color = "blue") %>% 
  addPolygons(weight = 1, color = "red", data = fluvial_ll_sel)  

#
# ELVIS Elvenett ----
#

# . data ----

file <- "../../Mapdata/NVE_ELVIS_elvenett/NVEData/Elv_Hovedelv.geojson"
st_layers(file)

# Select and read GML file  
elvis <- st_read(file, layer = "Elv_Hovedelv")
str(elvis)
nrow(elvis)
table(addNA(elvis$nivaa))

elvis_ll <- st_transform(elvis, crs = 4326)

elvis_ll_sel1 <- elvis_ll %>% 
  filter(nivaa %in% "1a")
elvis_ll_sel2 <- elvis_ll %>% 
  filter(nivaa %in% "2a")
elvis_ll_sel12 <- elvis_ll %>% 
  filter(nivaa %in% c("1a","2a"))
elvis_ll_sel123 <- elvis_ll %>% 
  filter(nivaa %in% c("1a","2a","3a"))

leaflet() %>% 
  addTiles() %>% 
  addPolylines(
    weight = 4, color = "red", data = elvis_ll_sel1) %>% 
  addPolylines(
    weight = 2, color = "red", data = elvis_ll_sel2)

# 
# check
#
url <- "https://cache.kartverket.no/v1/wmts/1.0.0/topograatone/default/webmercator/{z}/{y}/{x}.png"
elvis_ll_sel123 %>% 
  filter(elvenavn == "Åsta") %>% 
  leaflet() %>% 
  addTiles() %>% 
  # addTiles(url, attribution = "Kartverket") %>% 
  addPolylines(weight = 3, color = "red")

#
# . overlap analysis ----  
#

elvis_ll_sel123 <- st_make_valid(elvis_ll_sel123)

# overlap1 (fluvial)
fluvial_ll_sel <- st_make_valid(fluvial_ll_sel)      # fix topology
lst1 <- st_intersects(elvis_ll_sel123, fluvial_ll_sel)

# overlap2 (glacial)
glacial_ll_sel <- st_make_valid(glacial_ll_sel)      # fix topology
lst2 <- st_intersects(elvis_ll_sel123, glacial_ll_sel)

sel1 <- lengths(lst1) > 0 & lengths(lst2) == 0
sel2 <- lengths(lst1) == 0 & lengths(lst2) > 0
sel12 <- lengths(lst1) > 0 & lengths(lst2) > 0

elvis_ll_sel123_overlap1 <- elvis_ll_sel123[sel1,] %>% 
  mutate(popup = paste0(elvenavn, "<br>", nedborfeltVassdragNr))
elvis_ll_sel123_overlap2 <- elvis_ll_sel123[sel2,] %>% 
  mutate(popup = paste0(elvenavn, "<br>", nedborfeltVassdragNr))
elvis_ll_sel123_overlap12 <- elvis_ll_sel123[sel12,] %>% 
  mutate(popup = paste0(elvenavn, "<br>", nedborfeltVassdragNr))

#
# Combine leaflet maps ----  
#

# toporaster
url <- "https://cache.kartverket.no/v1/wmts/1.0.0/toporaster/default/webmercator/{z}/{y}/{x}.png"
# greyscale
url <- "https://cache.kartverket.no/v1/wmts/1.0.0/topograatone/default/webmercator/{z}/{y}/{x}.png"

glacial_ll_sel %>% 
  leaflet() %>% 
  # addTiles() %>% 
  addTiles(url, attribution = "Kartverket") %>% 
  addPolygons(weight = 1, color = "blue") %>% 
  addPolygons(weight = 1, color = "red", data = fluvial_ll_sel)%>% 
  addPolylines(
    weight = 3, color = "red", 
    data = elvis_ll_sel123_overlap1, popup = ~popup) %>% 
  addPolylines(
    weight = 3, color = "blue", 
    data = elvis_ll_sel123_overlap2, popup = ~popup) %>% 
  addPolylines(
    weight = 3, color = "magenta", 
    data = elvis_ll_sel123_overlap12, popup = ~popup)  

# Rivers overlapping with deposits (red=fluvial, blue=glacial, purple=both)
# Major rivers (nivaa 1a+2a+3a) overlapping with fluvial and glacial deposits
# rivers_overlapping_deposits2


#
# Make dataset ----
# 

#
# . find length of overlap ----    
# 


# .. extract overlapping river parts ----
# extract the parts of rivers that overlap with fluvial (intersect1) and glacial (intersect2) polygons

fn <- "data/010_elvis_sel123_intersect1.rds"
if (file.exists(fn)){
  elvis_sel123_intersect1 <- readRDS(fn)
} else {
  sel1_all <- lengths(lst1) > 0
  # a bit slow (1 min)
  elvis_sel123_intersect1 <- st_intersection(elvis_ll_sel123[sel1_all,], fluvial_ll_sel)
  saveRDS(elvis_sel123_intersect1, fn)
}

fn <- "data/010_elvis_sel123_intersect2.rds"
if (file.exists(fn)){
  elvis_sel123_intersect2 <- readRDS(fn)
} else {
  sel2_all <- lengths(lst2) > 0
  # a bit slow (1 min)
  elvis_sel123_intersect2 <- st_intersection(elvis_ll_sel123[sel2_all,], glacial_ll_sel[sel_glacial,])
  saveRDS(elvis_sel123_intersect2, fn)
}

# .. extract length of overlap ----
# for some rivers, multiple overlaps  
df_fluvial_all <- elvis_sel123_intersect1 %>% 
  st_drop_geometry() %>%
  select(elvenavn, nedborfeltVassdragNr, nivaa)
nrow(df_fluvial_all)  
df_fluvial_all$fluvial_length <- st_length(elvis_sel123_intersect1)

df_glacial_all <- elvis_sel123_intersect2 %>% 
  st_drop_geometry() %>%
  select(elvenavn, nedborfeltVassdragNr, nivaa)
nrow(df_glacial_all)  
df_glacial_all$glacial_length <- st_length(elvis_sel123_intersect2)

# .. summarise length of overlap ----
# summarise by river

df_fluvial <- df_fluvial_all %>% 
  group_by(elvenavn, nedborfeltVassdragNr, nivaa) %>% 
  summarise(
    fluvial_length = sum(fluvial_length),
    fluvial_n = n())

df_glacial <- df_glacial_all %>% 
  group_by(elvenavn, nedborfeltVassdragNr, nivaa) %>% 
  summarise(
    glacial_length = sum(glacial_length),
    glacial_n = n())

#
# . extract river data
#
# 'elvis_sel123_overlap_feature'  
#

elvis_sel123_overlap_feature <- elvis_ll_sel123 %>%
  select(elvenavn, nedborfeltVassdragNr, nivaa, elvelengde, vassdragsomrade) %>%
  left_join(df_fluvial) %>%
  left_join(df_glacial) %>%
  filter(fluvial_n > 0 | glacial_n > 0) %>% 
  mutate(
    fluvial_perc = round(fluvial_length/elvelengde*100, 1),
    glacial_perc = round(glacial_length/elvelengde*100, 1),
  )
  

# . add centroid to data ----
df_centroid <- elvis_sel123_overlap_feature %>% 
  st_centroid() %>% 
  st_coordinates()
elvis_sel123_overlap_feature$centroid_x <- df_centroid[,1]
elvis_sel123_overlap_feature$centroid_y <- df_centroid[,2]



# . fylke (county) data ----

file <- "../../Mapdata/Basisdata_0000_Norge_25833_NorskeFylkerKommunerIllustrasjonsdata2024_GeoJSON/GeoJSON/Fylker_simple1000.geojson"
st_layers(file)

# Select and read GML file  
fylker <- st_read(file, layer = "Fylker_simple1000")
str(elvis)
nrow(elvis)

fylker_ll <- st_transform(fylker, crs = 4326)

# leaflet() %>% 
#   addTiles() %>% 
#   addPolylines(weight = 4, color = "red", data = fylker_ll)

# elvis_ll_sel123 %>% View("elvis")

# leaflet() %>% 
#   addTiles() %>% 
#   addPolylines(weight = 4, color = "red", data = elvis_ll_sel123[138,])

# rivers overlapping either fluvial or glacuial
sel_either <- lengths(lst1) > 0 | lengths(lst2) > 0
river_fylke_list <- 1:nrow(fylker_ll) %>% 
  map(\(i) st_intersection(elvis_ll_sel123[sel_either,], fylker_ll[i,]))
river_fylkelength_list <- river_fylke_list %>% 
  map(\(obj) {
    result <- obj %>% 
      st_drop_geometry() %>%
      # navn = name of fylke, elvenavn = name of river
      select(navn, elvenavn, nedborfeltVassdragNr)
    result$length <- st_length(obj)
    result
    })
# combine to one tall data frame
river_fylkelength_tall <- bind_rows(river_fylkelength_list) %>% 
  mutate(length = as.numeric(length)/1000)
head(river_fylkelength_tall, 3)
# change to one wide data frame
river_fylkelength_wide <- tidyr::pivot_wider(
  river_fylkelength_tall, names_from = navn, values_from = length, values_fill = 0
)
head(river_fylkelength_wide, 3)

# use function for finding the 3 most covered fylke for river number i:
river_fylkelength_wide2 <- river_fylkelength_wide %>% select(Troms:Oslo) %>% as.data.frame() 
get_max_fylke <- function(i){
  vect <- river_fylkelength_wide2 %>% as.matrix() %>%  .[i,] %>% sort(decreasing = TRUE)
  fylke1 <- names(vect)[1]
  fylke1_km <- vect[1]
  fylke2 <- ifelse(vect[2]>0, names(vect)[2], "-")
  fylke2_km <- vect[2]
  fylke3 <- ifelse(vect[3]>0, names(vect)[3], "-")
  fylke3_km <- vect[3]
  data.frame(
    i=i,
    fylke1=fylke1, fylke1_km=round(fylke1_km,0),
    fylke2=fylke2, fylke2_km=round(fylke2_km,0),
    fylke3=fylke3, fylke3_km=round(fylke3_km,0))
}

# Get all results;:
river_fylkelength_max <- 1:nrow(river_fylkelength_wide) %>% 
  map_dfr(get_max_fylke) 
rownames(river_fylkelength_max) <- NULL

# Final results:
river_fylkelength <- river_fylkelength_wide %>% 
  select(elvenavn, nedborfeltVassdragNr) %>% 
  bind_cols(river_fylkelength_max)

# backup <- elvis_sel123_overlap_feature
elvis_sel123_overlap_feature <- elvis_sel123_overlap_feature %>% 
  left_join(river_fylkelength,
            by = join_by(elvenavn, nedborfeltVassdragNr))


# . dataset without geometry -----

elvis_sel123_overlap <- elvis_sel123_overlap_feature %>% 
  st_drop_geometry()


# . plot length of fluvial_length per river -----

breaks_fluv <- quantile(elvis_sel123_overlap$fluvial_length, 
                        probs = c(0,0.1,0.2,0.4,0.6,0.8,0.9,1), na.rm = TRUE,)
breaks_glac <- quantile(elvis_sel123_overlap$glacial_length, 
                        probs = c(0,0.1,0.2,0.4,0.6,0.8,0.9,1), na.rm = TRUE,)
elvis_sel123_overlap_feature %>% 
  filter(!is.na(fluvial_length)) %>% 
  mutate(
    radius = cut(as.numeric(fluvial_length), breaks=breaks_fluv, labels = FALSE),
    popup = paste0(
      elvenavn, " (", nedborfeltVassdragNr, ")<br>", 
      "elvelengde = ", round(elvelengde,0)/1000, " km<br>",
      "fluvial_length = ", round(fluvial_length,0)/1000, " km")
    ) %>% 
  leaflet() %>% 
  addTiles(url, attribution = "Kartverket") %>% 
  addPolylines() %>% 
  addCircleMarkers(lng = ~centroid_x, lat = ~centroid_y, 
                   radius = ~radius, popup = ~popup, color = "red")
  # addMarkers(lng = ~centroid_x, lat = ~centroid_y)



