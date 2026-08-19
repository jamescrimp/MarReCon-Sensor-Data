library (tidyverse)
library (cowplot)

library(usethis)

library(devtools)

library (rnaturalearthhires) #to install, 
devtools::install_github("ropensci/rnaturalearthhires")


remotes::install_github("ropensci/rnaturalearthhires")

install.packages("rnaturalearthhires", repos = "https://ropensci.r-universe.dev", type = "source")

library (maps)
library (mapdata)
library (RColorBrewer)
library (sf)
library (ggspatial) #requires that you have installed "sf"
library (patchwork)
library (terra) #for dealing with spat raster objects
library (tidyterra) #ditto

library (rnaturalearth)
library (rnaturalearthdata)
library (ggmap)
library(ggplot2)
library (viridis)
library (stringr)
library (magick)
library (sp)



#devtools::install_github("ropensci/rnaturalearthhires")

setwd("C:/MarRecon_code/thesis_work/Fig1Map")

w2h2 <- sf::st_as_sf(maps::map("world2Hires", regions = c("Canada", "USA"), 
                               plot = FALSE, fill = TRUE))

ggplot (data = w2h2) + geom_sf() +
  coord_sf (xlim = c(170, 235), ylim = c(50, 75))

world <- rnaturalearth::ne_countries (scale = "medium", returnclass = "sf")
ggplot (data = world) + geom_sf()
ggplot (data = world) + geom_sf(fill = "darkolivegreen3") 


GOA <- ggplot (data = world) + geom_sf(fill = "grey34") + 
  coord_sf (xlim = c(-160, -140), ylim = c(56, 62)) 

#map of GOA and AK coast
GOA

#Map of GOA and AK
world <- ne_countries (scale = "medium", returnclass = "sf")
ggplot (data = world) + geom_sf() + coord_sf(xlim = c(-125, -175), ylim = c(50, 75)) + theme_bw()

ne50rivers <- rnaturalearth::ne_download(scale = "medium", type = "rivers_lake_centerlines", category = "physical", returnclass = "sf")
ggplot () + geom_sf(data = world) + geom_sf(data = ne50rivers) + coord_sf(xlim = c(-125, -175), ylim = c(50, 75)) + theme_bw()

ne10rivers <- ne_download(scale = "large", type = "rivers_lake_centerlines", category = "physical", returnclass = "sf") 
ne10lakes <- ne_download (scale = "large", type = "lakes", category = "physical", returnclass = "sf")

ggplot () + geom_sf(data = world, fill = "grey70") + geom_sf(data = ne10rivers, color = "#538AB1") + geom_sf(data = ne10lakes, fill = "#538AB1") + coord_sf(xlim = c(-150, -170), ylim = c(57, 62))

ne.10.rivers.na <- ne_download (scale = 10, type = "rivers_north_america", category = "physical", returnclass = "sf")
ggplot () + geom_sf(data = world, fill = "grey70") + geom_sf(data = ne.10.rivers.na, color = "#538AB1") + geom_sf(data = ne10lakes, fill = "#538AB1") + coord_sf(xlim = c(-150, -170), ylim = c(57, 62))

#Use Stadia map
register_stadiamaps (key = "ed673762-e806-460c-a3bd-b5d574810874") 

alaskabox <- c(left = -160, bottom = 55, right = -142, top = 63)


alaska.terrain <- ggmap::get_stadiamap (bbox = alaskabox, zoom = 5, maptype = "stamen_terrain", where = "cache")
ggmap (alaska.terrain)

ggmap (alaska.terrain)  + geom_sf (data = ne50rivers, inherit.aes = FALSE)


GOA_sites <- ggmap::get_stadiamap (bbox = alaskabox, zoom = 5, maptype = "stamen_terrain", where = "cache")

ggmap (GOA_sites)


ggmap_bbox <- function(map) {
  if (!inherits(map, "ggmap")) stop("map must be a ggmap object")
  # Extract the bounding box (in lat/lon) from the ggmap to a numeric vector, 
  # and set the names to what sf::st_bbox expects:
map_bbox <- setNames(unlist(attr(map, "bb")), 
                       c("ymin", "xmin", "ymax", "xmax"))
  
  
  # Convert the bbox to an sf polygon, transform it to 3857, 
  # and convert back to a bbox (convoluted, but it works)
bbox_3857 <- st_bbox(st_transform(st_as_sfc(st_bbox(map_bbox, crs = 4326)), 3857))}

#Tidy GOA and AK map  
world <- ne_countries (scale = "medium", returnclass = "sf")

GOA_AK <- ggplot (data = world) + geom_sf() + coord_sf(xlim = c(-125, -175), ylim = c(50, 75)) + theme_bw()
#----------------------------------------------------------------
ne50rivers <- ne_download(scale = "medium", type = "rivers_lake_centerlines", category = "physical", returnclass = "sf")
  
ggplot () + geom_sf(data = world) + geom_sf(data = ne50rivers) + coord_sf(xlim = c(-125, -175), ylim = c(50, 75)) + theme_bw()
  
ne.10.rivers.na <- ne_download (scale = 10, type = "rivers_north_america", category = "physical", returnclass = "sf")
    
ne50rivers <- rnaturalearth::ne_download(scale = "medium", type = "rivers_lake_centerlines", category = "physical", returnclass = "sf")
  
ggplot () + geom_sf(data = world) + geom_sf(data = ne50rivers) + coord_sf(xlim = c(-125, -175), ylim = c(50, 75)) + theme_bw()

ne50lakes <- rnaturalearth::ne_download(scale = "medium", type = "lakes", category = "physical", returnclass = "sf") 

ggplot () + geom_sf(data = world, fill = "grey70") + geom_sf(data = ne50rivers, color = "#538AB1") + geom_sf(data = ne50lakes) + coord_sf(xlim = c(-125, -175), ylim = c(50, 75))
  
ggplot () + geom_sf(data = world, fill = "grey70") + geom_sf(data = ne50rivers, color = "#538AB1") + geom_sf(data = ne50lakes, fill = "#538AB1") + coord_sf(xlim = c(-150, -170), ylim = c(57, 62))
  
alaskabox <- c(left = -170, bottom = 50, right = -135, top = 70)
  alaska.terrain <- ggmap::get_stadiamap (bbox = alaskabox, zoom = 5, maptype = "outdoors", where = "cache")
  
  ggmap (alaska.terrain)
  
#Largest plot (ak.base)---------------------
#Get stamen/stadia map background and check to see it looks OK:
 #   ```{r}
alaska.box <- c(left = -170, bottom = 54, right = -125, top = 71)
ak.base <- ggmap::get_stadiamap (bbox = alaska.box, zoom = 6, maptype = "stamen_terrain_background", where = "cache")
ggmap (ak.base)

# project map of EVOS sites
alaska.box.evos <- c(left = -154, bottom = 57, right = -144, top = 61)
ak.base.evos <- ggmap::get_stadiamap (bbox = alaska.box.evos, zoom = 6, maptype = "stamen_terrain_background", where = "cache")
ggmap (ak.base.evos)


#2oija

ggmap_bbox <- function(map) {
  if (!inherits(map, "ggmap")) stop("map must be a ggmap object")
  # Extract the bounding box (in lat/lon) from the ggmap to a numeric vector, 
  # and set the names to what sf::st_bbox expects:
map_bbox <- setNames(unlist(attr(map, "bb")), 
                       c("ymin", "xmin", "ymax", "xmax"))
  
  # Convert the bbox to an sf polygon, transform it to 3857, 
  # and convert back to a bbox (convoluted, but it works)
bbox_3857 <- st_bbox(st_transform(st_as_sfc(st_bbox(map_bbox, crs = 4326)), 3857))
  
  # Overwrite the bbox of the ggmap object with the transformed coordinates 
attr(map, "bb")$ll.lat <- bbox_3857["ymin"]
attr(map, "bb")$ll.lon <- bbox_3857["xmin"]
attr(map, "bb")$ur.lat <- bbox_3857["ymax"]
attr(map, "bb")$ur.lon <- bbox_3857["xmax"]
map
}


#Apply the "ggmap_bbox" function to ak.base:
 # ```{r}
ak.base.sf <- ggmap_bbox(ak.base)

alaska.base.map <- ggmap(ak.base.sf) +
  coord_sf(crs = st_crs(3857))  # force the ggplot2 map to be in 3857

#good!
alaska.base.map

alaska.inset <- alaska.base.map + geom_rect(xmin = -16920900, xmax = -16475284, ymin = 7967317, ymax = 8625823,
                                            fill = "transparent", color = "grey20", linewidth = 0.5) + theme_void() + 
  theme (panel.border = element_rect (fill = "transparent", color = "grey20", linewidth = 1.5),
         plot.margin = unit(c (0,0,0,0), "cm"))

#use this for final fig
alaska.inset

KBY.box <- c(left = -152.5, bottom = 59, right = -150, top = 60.5) #using WSG84
KBY.base <- ggmap::get_stadiamap (bbox = KBY.box, zoom = 9, maptype = "stamen_terrain_background", where = "cache")


KBY.sf <- ggmap_bbox(KBY.base)

KBY.map<- ggmap(KBY.sf) +
  coord_sf(crs = st_crs(3857))  # force the ggplot2 map to be in 3857

#Map figure of KBY
KBY.map

KBY.farms <- data.frame (
  lon = c(-151.51795, -151.27173, -151.5175),
  lat = c(59.46067, 59.5719, 59.46815),
  names = c("A", "B", "C"))

as.numeric(as.character(KBY.farms$lon))
as.numeric(as.character(KBY.farms$lat))

spoints.kby <- sp::SpatialPoints(coords = KBY.farms[,c("lon","lat")], proj4string = CRS("+proj=longlat +datum=WGS84")) #telling sp that these are coordinates in decimal degrees based on the WGS 84 datum
spoints.kby2 <-spTransform(spoints.kby, CRS("+init=EPSG:3857")) #convert to meters; EPSG 3857 - used for web maps (Open street map, google, bing, etc.)

KBY.points <- data.frame(spoints.kby2@coords, KBY.farms)%>% select (-c(lon, lat)) #remove the WGS84 coordinates
names (KBY.points)[1] <- "lon" #rename the 3857 coordinates as lat/lon
names(KBY.points)[2] <- "lat"

KBY.map + geom_point(data = KBY.points, aes (x = lon, y = lat), size = 0.5) + xlab (NULL) + ylab (NULL) +
  theme (plot.margin = unit (c(0,0,0,0), "cm")) + scale_y_continuous (breaks = c(59.0, 59.5, 60), expand = c(0,0)) +
  scale_x_continuous(breaks = c(-152, -151, -150), expand = c(0,0))

Kbay <- KBY.map + geom_point(data = KBY.points, aes (x = lon, y = lat), size = 0.5) + geom_rect (xmin = -16870468, xmax = -16828946, ymin = 8278291, ymax = 8310775, fill = "transparent", color = "grey20", linewidth = 0.75) +
  theme (panel.border = element_rect (fill = "transparent", color = "grey20", linewidth = 2)) + xlab (NULL) + ylab (NULL)

Kbay <- Kbay + scale_y_continuous (breaks = c(59.2, 59.7, 60.2), expand = c(0,0)) + scale_x_continuous(breaks = c(-152, -151, -150), expand = c(0,0))
#Kbay figure
Kbay


farm.box <- c(left = -151.55, bottom = 59.43, right = -151.19, top = 59.598)
Kby.terrain <- ggmap::get_stadiamap (bbox = farm.box, zoom = 12, maptype = "stamen_terrain_background", where = "cache")


Kby.terrain.fix <- ggmap_bbox(Kby.terrain) #apply the function

farms.map <- ggmap(Kby.terrain.fix) +
  coord_sf(crs = st_crs(3857))  # force the ggplot2 map to be in 3857

farms.map

farms.inset <- ggmap (Kby.terrain.fix) + coord_sf(crs = st_crs(3857)) + 
  geom_point (data = KBY.points, aes (x = lon, y = lat), size = 0.7, color = "grey20") +
  geom_text (data = KBY.points, aes (x = lon, y = lat, label = NA), size = 4, color = "grey20", vjust = 1, nudge_y = -1500) 

farms.inset <- farms.inset + scale_x_continuous (expand = c(0,0)) + scale_y_continuous (expand = c(0,0)) + 
  theme_void() + theme (panel.border = element_rect (fill = "transparent", color = "grey20", linewidth = 1.5), plot.margin = unit(c (0,0,0,0), "cm"))

farms.inset

farms.inset <- farms.inset + ggspatial::annotation_scale(location = "br", pad_y = unit(0.8, "cm"), style = "ticks")

Kbay + (alaska.inset/farms.inset)


Kbyfarms.layout <- "
AAAABB
AAAABB
AAAABB
AAAACC
AAAACC
AAAACC
AAAACC"

figure1 <- Kbay + alaska.inset + farms.inset +
  plot_layout(design = Kbyfarms.layout)

figure1

pA <- Kbay + annotation_north_arrow (style = north_arrow_nautical)
pB <- alaska.inset + theme (plot.margin = unit (c(0,0,2,0), unit = "mm"))
pC <- farms.inset + theme (plot.margin = unit (c(2,0,0,0), unit = "mm"))

pA + pB + pC + plot_layout(design = Kbyfarms.layout)