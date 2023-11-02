# Title: Individual variation in habitat selection
# Subtitle: 12 - Map for paper
# Author: Nathan D. Hooven
# Email: nathan.hooven@uky.edu
# Affiliation: Department of Forestry and Natural Resources, University of Kentucky
# Date began: 25 May 2022
# Date completed: 26 May 2022
# Date modified: 01 Nov 2023
# R version: 3.6.2

#_____________________________________________________________________________________________________________
# 1. Load in required packages ----
#_____________________________________________________________________________________________________________

library(tidyverse)
library(raster)        # rasters
library(sf)            # simple features
library(ggspatial)     # scale bar and north arrow
library(spData)        # data for inset map
library(cowplot)       # draw inset map

#_____________________________________________________________________________________________________________
# 2. Read in data ----
#_____________________________________________________________________________________________________________

# relocation data
WS.data <- read.csv("WS_sampled.csv")
SU.data <- read.csv("SU_sampled.csv")
UA.data <- read.csv("UA_sampled.csv")
AW.data <- read.csv("AW_sampled.csv")

# bind all together
all.data <- rbind(WS.data, SU.data, UA.data, AW.data)

# filter out used only
all.data.1 <- all.data %>% filter(Case == 1)

# read in shapefile as sf
elkzone <- st_read("E:/Elk project/Elk Zone rasters (7-20-21)/elkzone_proj2.shp")

# us states from spData
data("us_states", package = "spData")

ky.state <- us_states[us_states$NAME == "Kentucky", ]

plot(ky.state)

#_____________________________________________________________________________________________________________
# 3. Convert relocations to sf ----
#_____________________________________________________________________________________________________________

all.data.sf <- st_as_sf(x = all.data.1,
                        coords = c("x", "y"),
                        crs = "+proj=utm +zone=17 +ellps=GRS80 +units=m +no_defs")

plot(all.data.sf)

#_____________________________________________________________________________________________________________
# 4. Plot in ggplot ----
#_____________________________________________________________________________________________________________

# KY inset map
# extract bounding box of elkzone
elkzone.bb <- st_as_sfc(st_bbox(elkzone))

map.inset <- ggplot() +
  theme_bw() +
  geom_sf(data = ky.state,
          fill = "lightgray",
          alpha = 0.15) +
  geom_sf(data = elkzone,
          fill = "white") +
  theme(panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        plot.margin = margin(c(0, 0, 0, 0))) +
  annotate(geom = "text",
           x = -86.3,
           y = 37.4,
           size = 3.5,
           label = "Kentucky")

# main map with lower alpha
map.main <- ggplot() +
  theme_bw() +
  geom_sf(data = elkzone,
          fill = "white") +
  geom_sf(data = all.data.sf,
          alpha = 0.01,
          size = 0.3) +
  geom_sf(data = contours.WS) +
  coord_sf(crs = "+proj=merc +lat_ts=0 +lon_0=0 +k=1.000000 +x_0=0 +y_0=0 +ellps=GRS80 +datum=WGS84 +units=m +no_defs no_defs") +
  annotation_scale(location = "br", 
                   bar_cols = c("gray", "white"),
                   style = "ticks") +
  annotation_north_arrow(style = north_arrow_fancy_orienteering,
                         location = "br",
                         pad_y = unit(0.25, "in"))

# draw map together
ggdraw() +
  draw_plot(map.main) +
  draw_plot(map.inset,
            scale = 0.35,
            x = -0.209,
            y = 0.245)

#_____________________________________________________________________________________________________________
# 5. Add in HR contours ----
#_____________________________________________________________________________________________________________
# 5a. Read in files ----
#_____________________________________________________________________________________________________________

# CRS
utm.crs <- "+proj=utm +zone=17 +ellps=GRS80 +units=m +no_defs"
tpeqd.crs <- "+proj=tpeqd +lat_1=37.6050690054435 +lat_2=37.6255087558209 +lon_1=-82.6695167948626 +lon_2=-82.6257909049089"

# general directory
shp.dir <- "E:/Elk project/Data analysis/Home ranges/ADKE/Shapefiles/"

# create lists of file names
contours.list <- list()

contours.list[[1]] <- list.files(paste0(shp.dir, "1 - WS"), pattern = "est.*.shp", full.names = TRUE)
contours.list[[2]] <- list.files(paste0(shp.dir, "2 - SU"), pattern = "est.*.shp", full.names = TRUE)
contours.list[[3]] <- list.files(paste0(shp.dir, "3 - UA"), pattern = "est.*.shp", full.names = TRUE)
contours.list[[4]] <- list.files(paste0(shp.dir, "4 - AW"), pattern = "est.*.shp", full.names = TRUE)

# loop through each season and read in shapefiles
contours.files <- list()

contours.files[[1]] <- list()
contours.files[[2]] <- list()
contours.files[[3]] <- list()
contours.files[[4]] <- list()

# by season
for (i in c("WS", "SU", "UA", "AW")) {
  
  # correct list index
  focal.index <- case_when(i == "WS" ~ 1,
                           i == "SU" ~ 2,
                           i == "UA" ~ 3,
                           i == "AW" ~ 4)
  
  focal.list <- contours.list[[focal.index]]
  
  # by entry
  for (j in 1:length(focal.list)) {
    
    # read in file
    focal.file <- st_read(focal.list[[j]])
    
    # convert to sf
    focal.file.1 <- focal.file %>% st_transform(crs = utm.crs)
    
    # bind into large sf
    contours.files[[focal.index]][[j]] <- focal.file.1
    
    
  }
  
}

#_____________________________________________________________________________________________________________
# 5b. Bind all polygons by season ----
#_____________________________________________________________________________________________________________

contours.WS <- do.call(bind_rows, contours.files[[1]])
contours.SU <- do.call(bind_rows, contours.files[[2]])
contours.UA <- do.call(bind_rows, contours.files[[3]])
contours.AW <- do.call(bind_rows, contours.files[[4]])

# add "Season" variable
contours.WS <- contours.WS %>% mutate(Season = "Feb-Apr")
contours.SU <- contours.SU %>% mutate(Season = "May-Jul")
contours.UA <- contours.UA %>% mutate(Season = "Aug-Oct")
contours.AW <- contours.AW %>% mutate(Season = "Nov-Jan")

# bind all together
contours.all <- bind_rows(contours.WS,
                          contours.SU,
                          contours.UA,
                          contours.AW)

# reorder factor
contours.all$Season <- factor(contours.all$Season,
                              levels = c("Feb-Apr",
                                         "May-Jul",
                                         "Aug-Oct",
                                         "Nov-Jan"))

#_____________________________________________________________________________________________________________
# 6. Plot in ggplot ----
#_____________________________________________________________________________________________________________

# main map
map.main.contours <- ggplot() +
  theme_bw() +
  geom_sf(data = elkzone,
          fill = "white") +
  geom_sf(data = contours.all,
          color = scales::alpha(colour = "black",
                                alpha = 0.2),
          fill = NA,
          alpha = 0.01) +
  coord_sf(crs = "+proj=merc +lat_ts=0 +lon_0=0 +k=1.000000 +x_0=0 +y_0=0 +ellps=GRS80 +datum=WGS84 +units=m +no_defs no_defs") +
  annotation_scale(location = "br", 
                   bar_cols = c("gray", "white"),
                   style = "ticks") +
  annotation_north_arrow(style = north_arrow_fancy_orienteering,
                         location = "br",
                         pad_y = unit(0.25, "in"))

# draw map together
ggdraw() +
  draw_plot(map.main.contours) +
  draw_plot(map.inset,
            scale = 0.35,
            x = -0.209,
            y = 0.245)

