rm(list=ls())

library(maptiles)
#install.packages("maptiles")
library(tidyterra)
#install.packages(c("sf","terra","tidyterra","maptiles"))
library(sf)
library(ggplot2)
library(openxlsx)
library(maptiles)
library(ggspatial)

#Rversion 4.5.2.

#Enisey
bb <- c(left = 92.7, bottom = 55.95, right = 92.9, top = 56.04)

matrix(bb, 2, byrow = TRUE) |>
  st_multipoint()       |> 
  st_sfc(crs = 4326)    |>
  st_transform(3857) -> baikal

baikal_tiles <- get_tiles(x = baikal, zoom = 11, crop = TRUE, forceDownload = TRUE,
                          provider = 'CartoDB.VoyagerNoLabels') #Esri.WorldImagery is also not bad but too dark
get_credit(provide = 'Esri.OceanBasemap')
#"Tiles © Esri - Sources: GEBCO, NOAA, CHS, OSU, UNH, CSUMB, National Geographic, DeLorme, NAVTEQ, and Esri"


source_map <- ggplot() +
  geom_spatraster_rgb(data = baikal_tiles) + 
  coord_sf() + 
  scale_x_continuous(expand = c(0.0, 0)) +
  scale_y_continuous(expand = c(0.0, 0)) +
  theme_minimal() + 
  theme(axis.ticks = element_line(colour = "black")) + 
  #coord_sf(crs = 3857, expand = FALSE, ylim = st_bbox(baikal)[c(2, 4)])  + 
  ggspatial::annotation_scale(location = "br", height = unit(0.1, "cm")) 

source_map

ggsave(dpi = 300, filename = "Enisey.svg")


#Angara Irkutsk
bb <- c(left = 104.2, bottom = 52.2, right = 104.4, top = 52.3)

matrix(bb, 2, byrow = TRUE) |>
  st_multipoint()       |> 
  st_sfc(crs = 4326)    |>
  st_transform(3857) -> baikal

baikal_tiles <- get_tiles(x = baikal, zoom = 12, crop = TRUE, forceDownload = TRUE,
                          provider = 'CartoDB.VoyagerNoLabels') #Esri.WorldImagery is also not bad but too dark
get_credit(provide = 'Esri.OceanBasemap')
#"Tiles © Esri - Sources: GEBCO, NOAA, CHS, OSU, UNH, CSUMB, National Geographic, DeLorme, NAVTEQ, and Esri"


source_map <- ggplot() +
  geom_spatraster_rgb(data = baikal_tiles) + 
  coord_sf() + 
  scale_x_continuous(expand = c(0.0, 0)) +
  scale_y_continuous(expand = c(0.0, 0)) +
  theme_minimal() + 
  theme(axis.ticks = element_line(colour = "black")) + 
  #coord_sf(crs = 3857, expand = FALSE, ylim = st_bbox(baikal)[c(2, 4)])  + 
  ggspatial::annotation_scale(location = "bl", height = unit(0.1, "cm")) 

source_map

ggsave(dpi = 300, filename = "Irkutsk.svg")


#Baikal Enisey
bb <- c(left = 92.2, bottom = 51.0, right = 110.0, top = 56.3)

matrix(bb, 2, byrow = TRUE) |>
  st_multipoint()       |> 
  st_sfc(crs = 4326)    |>
  st_transform(3857) -> baikal

baikal_tiles <- get_tiles(x = baikal, zoom = 9, crop = TRUE, forceDownload = TRUE,
                          provider = 'CartoDB.VoyagerNoLabels') #Esri.WorldImagery is also not bad but too dark
get_credit(provide = 'Esri.OceanBasemap')
#"Tiles © Esri - Sources: GEBCO, NOAA, CHS, OSU, UNH, CSUMB, National Geographic, DeLorme, NAVTEQ, and Esri"


source_map <- ggplot() +
  geom_spatraster_rgb(data = baikal_tiles) + 
  coord_sf() + 
  scale_x_continuous(expand = c(0.0, 0)) +
  scale_y_continuous(expand = c(0.0, 0)) +
  theme_minimal() + 
  theme(axis.ticks = element_line(colour = "black")) + 
  #coord_sf(crs = 3857, expand = FALSE, ylim = st_bbox(baikal)[c(2, 4)])  + 
  ggspatial::annotation_scale(location = "bl", height = unit(0.1, "cm")) 
source_map

ggsave(dpi = 300, filename = "Baikal Enisey.svg")

## add rectangles
coords_to_rect <- function(bb) {
  matrix(bb, nrow = 2, byrow = TRUE) |>
    st_multipoint()       |> 
    st_sfc(crs = 4326)    |>
    st_transform(3857) -> bb_sf
  boundbox <- st_bbox(bb_sf)
  rect <- c(boundbox[1], boundbox[3], boundbox[2], boundbox[4])
  rect.df <- data.frame(as.list(rect))
  return(rect.df)
}

bbE <- c(left = 92.7, bottom = 55.95, right = 92.9, top = 56.04)
bbA <- c(left = 104.2, bottom = 52.2, right = 104.4, top = 52.3)
#bbBE <- c(left = 92.2, bottom = 51.0, right = 110.0, top = 56.3)
bbC <- c(left = 104.25, bottom = 51.5, right = 108.0, top = 53.5)
bbN <- c(left = 106.5, bottom = 52.7, right = 110.0, top = 55.0)
bbO <- c(left = 106.4, bottom = 53.0, right = 107.8, top = 53.4)
#bbB <- c(left = 103.6, bottom = 51.4, right = 110.6, top = 55.9)
bbS <- c(left = 103.6, bottom = 51.4, right = 105.0, top = 52.0)

coords_to_rect(bb = c(left = 92.7, bottom = 55.95, right = 92.9, top = 56.04))


st_bbox(bbE_sf, crs = st_crs(4326))

upd_geom_rect <- function(data) {
  return(geom_rect(data=data, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), fill=NA, color="red"))
}

source_map + 
  geom_rect(data = coords_to_rect(bbE), aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), fill=NA, color="red") + 
  geom_rect(data = coords_to_rect(bbA), aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), fill=NA, color="red") + 
  geom_rect(data = coords_to_rect(bbC), aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), fill=NA, color="red") + 
  geom_rect(data = coords_to_rect(bbN), aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), fill=NA, color="red") + 
  geom_rect(data = coords_to_rect(bbS), aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), fill=NA, color="red") + 
  geom_rect(data = coords_to_rect(bbO), aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), fill=NA, color="red")

ggsave(dpi = 300, filename = "Baikal Enisey w rectangles.svg")


## finished adding rectangles



#Central Baikal
bb <- c(left = 104.25, bottom = 51.5, right = 108.0, top = 53.5)

matrix(bb, 2, byrow = TRUE) |>
  st_multipoint()       |> 
  st_sfc(crs = 4326)    |>
  st_transform(3857) -> baikal

baikal_tiles <- get_tiles(x = baikal, zoom = 10, crop = TRUE, forceDownload = TRUE,
                          provider = 'CartoDB.VoyagerNoLabels') #Esri.WorldImagery is also not bad but too dark
get_credit(provide = 'Esri.OceanBasemap')
#"Tiles © Esri - Sources: GEBCO, NOAA, CHS, OSU, UNH, CSUMB, National Geographic, DeLorme, NAVTEQ, and Esri"


source_map <- ggplot() +
  geom_spatraster_rgb(data = baikal_tiles) + 
  coord_sf() + 
  scale_x_continuous(expand = c(0.0, 0)) +
  scale_y_continuous(expand = c(0.0, 0)) +
  theme_minimal() + 
  theme(axis.ticks = element_line(colour = "black")) + 
  #coord_sf(crs = 3857, expand = FALSE, ylim = st_bbox(baikal)[c(2, 4)])  + 
  ggspatial::annotation_scale(location = "br", height = unit(0.1, "cm")) 
source_map

ggsave(dpi = 300, filename = "Central Baikal.svg")


#South Baikal
bb <- c(left = 103.6, bottom = 51.4, right = 105.0, top = 52.0)

matrix(bb, 2, byrow = TRUE) |>
  st_multipoint()       |> 
  st_sfc(crs = 4326)    |>
  st_transform(3857) -> baikal

baikal_tiles <- get_tiles(x = baikal, zoom = 10, crop = TRUE, forceDownload = TRUE,
                          provider = 'CartoDB.VoyagerNoLabels') #Esri.WorldImagery is also not bad but too dark
get_credit(provide = 'Esri.OceanBasemap')
#"Tiles © Esri - Sources: GEBCO, NOAA, CHS, OSU, UNH, CSUMB, National Geographic, DeLorme, NAVTEQ, and Esri"


source_map <- ggplot() +
  geom_spatraster_rgb(data = baikal_tiles) + 
  coord_sf() + 
  scale_x_continuous(expand = c(0.0, 0)) +
  scale_y_continuous(expand = c(0.0, 0)) +
  theme_minimal() + 
  theme(axis.ticks = element_line(colour = "black")) + 
  #coord_sf(crs = 3857, expand = FALSE, ylim = st_bbox(baikal)[c(2, 4)])  + 
  ggspatial::annotation_scale(location = "bl", height = unit(0.1, "cm")) 
source_map

ggsave(dpi = 300, filename = "South Baikal.svg")

#South Baikal for genotyping
bb <- c(left = 104.6, bottom = 51.7, right = 105.1, top = 52.0)

matrix(bb, 2, byrow = TRUE) |>
  st_multipoint()       |> 
  st_sfc(crs = 4326)    |>
  st_transform(3857) -> baikal

baikal_tiles <- get_tiles(x = baikal, zoom = 10, crop = TRUE, forceDownload = TRUE,
                          provider = 'CartoDB.VoyagerNoLabels') #Esri.WorldImagery is also not bad but too dark
get_credit(provide = 'Esri.OceanBasemap')
#"Tiles © Esri - Sources: GEBCO, NOAA, CHS, OSU, UNH, CSUMB, National Geographic, DeLorme, NAVTEQ, and Esri"


source_map <- ggplot() +
  geom_spatraster_rgb(data = baikal_tiles) + 
  coord_sf() + 
  scale_x_continuous(expand = c(0.0, 0)) +
  scale_y_continuous(expand = c(0.0, 0)) +
  theme_minimal() + 
  theme(axis.ticks = element_line(colour = "black")) + 
  #coord_sf(crs = 3857, expand = FALSE, ylim = st_bbox(baikal)[c(2, 4)])  + 
  ggspatial::annotation_scale(location = "bl", height = unit(0.1, "cm")) 
source_map

ggsave(dpi = 300, filename = "South Baikal genotuping.svg")

#North Baikal
bb <- c(left = 106.5, bottom = 52.7, right = 110.0, top = 55.0)

matrix(bb, 2, byrow = TRUE) |>
  st_multipoint()       |> 
  st_sfc(crs = 4326)    |>
  st_transform(3857) -> baikal

baikal_tiles <- get_tiles(x = baikal, zoom = 10, crop = TRUE, forceDownload = TRUE,
                          provider = 'CartoDB.VoyagerNoLabels') #Esri.WorldImagery is also not bad but too dark
get_credit(provide = 'Esri.OceanBasemap')
#"Tiles © Esri - Sources: GEBCO, NOAA, CHS, OSU, UNH, CSUMB, National Geographic, DeLorme, NAVTEQ, and Esri"


source_map <- ggplot() +
  geom_spatraster_rgb(data = baikal_tiles) + 
  coord_sf() + 
  scale_x_continuous(expand = c(0.0, 0)) +
  scale_y_continuous(expand = c(0.0, 0)) +
  theme_minimal() + 
  theme(axis.ticks = element_line(colour = "black")) + 
  #coord_sf(crs = 3857, expand = FALSE, ylim = st_bbox(baikal)[c(2, 4)])  + 
  ggspatial::annotation_scale(location = "br", height = unit(0.1, "cm")) 
source_map

ggsave(dpi = 300, filename = "North Baikal.svg")


#Olkhon Baikal
bb <- c(left = 106.4, bottom = 53.0, right = 107.8, top = 53.4)

matrix(bb, 2, byrow = TRUE) |>
  st_multipoint()       |> 
  st_sfc(crs = 4326)    |>
  st_transform(3857) -> baikal

baikal_tiles <- get_tiles(x = baikal, zoom = 10, crop = TRUE, forceDownload = TRUE,
                          provider = 'CartoDB.VoyagerNoLabels') #Esri.WorldImagery is also not bad but too dark
get_credit(provide = 'Esri.OceanBasemap')
#"Tiles © Esri - Sources: GEBCO, NOAA, CHS, OSU, UNH, CSUMB, National Geographic, DeLorme, NAVTEQ, and Esri"


source_map <- ggplot() +
  geom_spatraster_rgb(data = baikal_tiles) + 
  coord_sf() + 
  scale_x_continuous(expand = c(0.0, 0)) +
  scale_y_continuous(expand = c(0.0, 0)) +
  theme_minimal() + 
  theme(axis.ticks = element_line(colour = "black")) + 
  #coord_sf(crs = 3857, expand = FALSE, ylim = st_bbox(baikal)[c(2, 4)])  + 
  ggspatial::annotation_scale(location = "br", height = unit(0.1, "cm")) 
source_map

ggsave(dpi = 300, filename = "Baikal Olkhon.svg")


#Baikal
bb <- c(left = 103.6, bottom = 51.4, right = 110.6, top = 55.9)

matrix(bb, 2, byrow = TRUE) |>
  st_multipoint()       |> 
  st_sfc(crs = 4326)    |>
  st_transform(3857) -> baikal

baikal_tiles <- get_tiles(x = baikal, zoom = 9, crop = TRUE, forceDownload = TRUE,
                          provider = 'CartoDB.VoyagerNoLabels') #Esri.WorldImagery is also not bad but too dark
get_credit(provide = 'CartoDB.VoyagerNoLabels')
#"Tiles © Esri - Sources: GEBCO, NOAA, CHS, OSU, UNH, CSUMB, National Geographic, DeLorme, NAVTEQ, and Esri"


source_map <- ggplot() +
  geom_spatraster_rgb(data = baikal_tiles) + 
  coord_sf() + 
  scale_x_continuous(expand = c(0.0, 0)) +
  scale_y_continuous(expand = c(0.0, 0)) +
  theme_minimal() + 
  theme(axis.ticks = element_line(colour = "black")) + 
  #coord_sf(crs = 3857, expand = FALSE, ylim = st_bbox(baikal)[c(2, 4)])  + 
  ggspatial::annotation_scale(location = "br", height = unit(0.1, "cm")) 
source_map


ggsave(dpi = 300, filename = "Baikal.svg")


#waffle
WSdistrib <- read.xlsx("Evi.xlsx")
WSdistrib$lat <- as.numeric(sapply(X = WSdistrib$Coordinate, FUN = function(X) {unlist(strsplit(X, split = " "))[1]}))
WSdistrib$lon <- as.numeric(sapply(X = WSdistrib$Coordinate, FUN = function(X) {unlist(strsplit(X, split = " "))[3]}))

WSdistrib_sf <- st_as_sf(WSdistrib, coords = c("lon", "lat"), crs=4326)

source_map + 
  geom_sf(data = WSdistrib_sf) + 
  theme_minimal()
ggsave("map.svg")

#devtools::install_github("liamgilbey/ggwaffle")
library(ggwaffle)

waffle_data <- waffle_iron(WSdistrib, aes_d(group = haplogroup_S))

ggplot(waffle_data, aes(x, y, fill = group)) + 
  geom_waffle()

#install.packages("waffle")
remotes::install_github("hrbrmstr/waffle")

library(waffle)
waffle(parts = data.frame(names=c("W", "S"), vals=c(93, 7)), rows = 20, flip = T) + 
  scale_fill_manual(values = c("blue", "yellow"))  + 
  ggtitle("Li, new")
ggsave("Li_waffle.svg")

waffle(parts = data.frame(names=c("W"), vals=c(11)), rows = 20, flip = T) + 
  scale_fill_manual(values = c(W))  + 
  ggtitle("Li, prev")


