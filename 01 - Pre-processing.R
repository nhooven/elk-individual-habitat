# Title: Individual variation in habitat selection
# Subtitle: Data pre-processing
# Author: Nathan D. Hooven
# Email: nathan.d.hooven@gmail.com
# Affiliation: Department of Forestry and Natural Resources, University of Kentucky
# Date began: 17 Nov 2020
# Date completed: 17 Nov 2020
# Date modified: 8 Feb 2022
# R version: 3.6.2

#_____________________________________________________________________________________________________________
# 1. Load in required packages ----
#_____________________________________________________________________________________________________________

library(tidyverse)
library(sp)             # work with spatial objects
library(rgdal)          # read in shapefiles
library(stringr)        # work with strings
library(raster)         # work with rasters
library(lubridate)      # work with dates
library(rgeos)          # gContains

#_____________________________________________________________________________________________________________
# 2. Read in data ----
#_____________________________________________________________________________________________________________

vectronic.data <- read.csv("G:/Elk project/Data analysis/Raw data processing/Relocations_vectronic_1.csv")
lotek.data <- read.csv("G:/Elk project/Data analysis/Raw data processing/Relocations_lotek_1.csv")

# get dates into correct formats
vectronic.data$t <- as.POSIXct(mdy_hm(vectronic.data$t),  tz = "America/New_York")
lotek.data$t <- as.POSIXct(mdy_hm(lotek.data$t), tz = "America/New_York")

# filter out relocations from after Jan 2021 for 2020 collars (retained originally for movement project)
vectronic.data.2020 <- vectronic.data %>% filter(Year == 2020) %>%
                                          filter(t < as.POSIXct("2021-02-01", tz = "America/New_York"))

vectronic.data.2021 <- vectronic.data %>% filter(Year == 2021)

# bind together
vectronic.data.all <- rbind(vectronic.data.2020, vectronic.data.2021)

# replace year "1" with year "2020"
lotek.data$Year[lotek.data$Year == "1"] <- "2020"

# rbind both data frames together
elk.data <- rbind(vectronic.data.all, lotek.data)

# select only columns we need 
elk.data <- elk.data %>% dplyr::select(x, y, t, burst, DOP, Animal, Year, Season)

# define projection
projection <- CRS("+proj=utm +zone=17 +ellps=GRS80 +units=m +no_defs")

# shapefile directory
shp.dir <- "G:/Elk project/Data analysis/Home ranges/ADKE/Shapefiles"

#_____________________________________________________________________________________________________________
# 3. Read in raster data ----
#_____________________________________________________________________________________________________________

# Raster directory
raster.dir <- "G:/Elk project/Elk Zone rasters (7-20-21)"

# Read in each raster
canopy        <- raster(paste0(raster.dir, "/", "canopy_proj.tif"))

dDeveloped    <- raster(paste0(raster.dir, "/", "dDeveloped.tif"))

dEdge         <- raster(paste0(raster.dir, "/", "dEdge.tif"))

TRI           <- raster(paste0(raster.dir, "/", "TRI_10.tif"))

TPI           <- raster(paste0(raster.dir, "/", "TPI_10.tif"))

dRoad         <- raster(paste0(raster.dir, "/", "dRoad.tif"))

dYoungForest  <- raster(paste0(raster.dir, "/", "dYoungForest.tif"))

dMatureForest <- raster(paste0(raster.dir, "/", "dMatureForest.tif"))

dOpen         <- raster(paste0(raster.dir, "/", "dOpen.tif"))

#_____________________________________________________________________________________________________________
# 4. Sample points and extract covariates ---- 
#_____________________________________________________________________________________________________________
# 4a. W/S ----
#_____________________________________________________________________________________________________________

# collars to include
WS.collars <- c(37703, 37704, 37705, 37707, 37708, 37709, 37710, 37711, 37712,
                37713, 37714, 37715, 37716, 37717, 37718, 37719, 37720, 37721, 37722,
                37723, 37724, 37725, 37726, 37727, 
                45469, 45470, 45492, 45493, 45494, 45495, 45496, 45497, 45498, 45499,
                45500, 45501, 45502, 45503, 45504, 45505, 45506, 45507, 45508, 45509,
                45510, 45511, 46391, 46392, 46393, 46394, 46395, 46396, 46397, 46399,
                46400, 46401, 46402,
                101940, 101946, 101948, 101954, 101955, 101957, 101962, 101965, 101966,
                101968, 101972, 101978, 102489, 102491, 102492, 102493,
                102494, 102495, 102497, 102535, 102536, 103172, 103174, 103175, 103176,
                103177, 103178, 103179, 103181, 103182, 103249,
                101956, 101974, 101980, 101985, 103173, 103180,
                103184, 103185, 103186, 103239, 103244, 103245, 103248, 103250)

# filter collars during the WS season and with > 30 relocations only 
WS.data <- elk.data %>% filter(Season == "WS" & Animal %in% WS.collars)

# blank data frame
WS.sampled <- data.frame()

# for loop to sample from each group
for (i in WS.collars) {
  
  animal.id <- i
  
  animal.data <- WS.data %>% filter(Animal == animal.id)
  
  # create SpatialPoints to sample data
  animal.sp <- SpatialPoints(coords = animal.data[ ,c("x", "y")],
                             proj4string = projection)
  
  # read in shapefile
  animal.hr <- readOGR(dsn = paste0(shp.dir, "/1 - WS"), layer = paste0("est_", animal.id, "_WS"))
  
  # transform HR to UTM projection
  animal.hr.proj <- spTransform(animal.hr, projection)
  
  # plot both
  sp:::plot(animal.hr.proj)
  sp:::plot(animal.sp, add = TRUE)
  
  # discard points that fall outside of the 99% contour
  contain.byid <- gContains(animal.hr.proj, animal.sp, byid = TRUE)
  
  if (FALSE %in% contain.byid) {
    
    animal.sp.1 <- animal.sp[-which(contain.byid == FALSE)]
    
  } else {
    
    animal.sp.1 <- animal.sp
    
  }
  
  if (FALSE %in% contain.byid) {
    
    animal.data.1 <- animal.data[-which(contain.byid == FALSE),]
    
  } else {
    
    animal.data.1 <- animal.data
    
  }
  
  # create Case variable for analysis
  animal.data.1$Case <- 1
  
  # sample random points (n = 20 per used)
  random.sp <- spsample(animal.hr.proj, 
                        n = nrow(animal.data.1)*20,
                        type = "regular")
  
  # reproject SpatialPoints layers
  animal.sp.2 <- spTransform(animal.sp.1, crs(dDeveloped@crs))
  random.sp.2 <- spTransform(random.sp, crs(dDeveloped@crs))
  
  # create random.data data frame
  random.data.1 <- data.frame(x = random.sp@coords[ ,1],
                              y = random.sp@coords[ ,2],
                              t = NA,
                              burst = NA, 
                              DOP = NA,
                              Animal = animal.id,
                              Year = NA,
                              Season = "WS",
                              Case = 0)
  
  # extract raster values for Case = 1
  animal.data.1$dDeveloped    <- raster:::extract(dDeveloped, animal.sp.2, method = "simple")
  animal.data.1$dOpen         <- raster:::extract(dOpen, animal.sp.2, method = "simple")
  animal.data.1$dYoungForest  <- raster:::extract(dYoungForest, animal.sp.2, method = "simple")
  animal.data.1$dMatureForest <- raster:::extract(dMatureForest, animal.sp.2, method = "simple")
  animal.data.1$dEdge         <- raster:::extract(dEdge, animal.sp.2, method = "simple")
  animal.data.1$TRI           <- raster:::extract(TRI, animal.sp.2, method = "simple")
  animal.data.1$TPI           <- raster:::extract(TPI, animal.sp.2, method = "simple")
  animal.data.1$dRoad         <- raster:::extract(dRoad, animal.sp.2, method = "simple")
  animal.data.1$canopy        <- raster:::extract(canopy, animal.sp.2, method = "simple")
  
  # extract raster values for Case = 0
  random.data.1$dDeveloped    <- raster:::extract(dDeveloped, random.sp.2, method = "simple")
  random.data.1$dOpen         <- raster:::extract(dOpen, random.sp.2, method = "simple")
  random.data.1$dYoungForest  <- raster:::extract(dYoungForest, random.sp.2, method = "simple")
  random.data.1$dMatureForest <- raster:::extract(dMatureForest, random.sp.2, method = "simple")
  random.data.1$dEdge         <- raster:::extract(dEdge, random.sp.2, method = "simple")
  random.data.1$TRI           <- raster:::extract(TRI, random.sp.2, method = "simple")
  random.data.1$TPI           <- raster:::extract(TPI, random.sp.2, method = "simple")
  random.data.1$dRoad         <- raster:::extract(dRoad, random.sp.2, method = "simple")
  random.data.1$canopy        <- raster:::extract(canopy, random.sp.2, method = "simple")
  
  # bind together
  all.data.1 <- rbind(animal.data.1, random.data.1)
  
  # bind to master data frame
  WS.sampled <- rbind(WS.sampled, all.data.1)
  
}

#_____________________________________________________________________________________________________________
# 4b. S/U ----
#_____________________________________________________________________________________________________________

# collars to include
SU.collars <- c(37703, 37704, 37705, 37706, 37707, 37708, 37709, 37710, 37711, 37712,
                37714, 37715, 37716, 37717, 37718, 37719, 37720, 37722,
                37723, 37724, 37725, 37726, 37727,
                45469, 45470, 45492, 45493, 45494, 45495, 45496, 45497, 45498, 45499,
                45500, 45501, 45502, 45504, 45505, 45506, 45507, 45508, 45509,
                45510, 45511, 46391, 46392, 46393, 46394, 46396, 46397, 46399,
                46400, 46401, 46402,
                101940, 101948, 101954, 101955, 101962, 101965,
                101968, 101972, 101978, 102489, 102491, 102492, 102493,
                102495, 102497, 102535, 102536, 103172, 103174, 103175, 103176,
                103177, 103178, 103179, 103181, 103182, 103249,
                101973, 101974, 101980, 103173, 103180,
                103184, 103185, 103186, 103239, 103244, 103245, 103248, 103250, 103251)

# filter collars during the WS season and with > 30 relocations only 
SU.data <- elk.data %>% filter(Season == "SU" & Animal %in% SU.collars)

# blank data frame
SU.sampled <- data.frame()

# for loop to sample from each group
for (i in SU.collars) {
  
  animal.id <- i
  
  animal.data <- SU.data %>% filter(Animal == animal.id)
  
  # create SpatialPoints to sample data
  animal.sp <- SpatialPoints(coords = animal.data[ ,c("x", "y")],
                             proj4string = projection)
  
  # read in shapefile
  animal.hr <- readOGR(dsn = paste0(shp.dir, "/2 - SU"), layer = paste0("est_", animal.id, "_SU"))
  
  # transform HR to UTM projection
  animal.hr.proj <- spTransform(animal.hr, projection)
  
  # plot both
  sp:::plot(animal.hr.proj)
  sp:::plot(animal.sp, add = TRUE)
  
  # discard points that fall outside of the 99% contour
  contain.byid <- gContains(animal.hr.proj, animal.sp, byid = TRUE)
  
  if (FALSE %in% contain.byid) {
    
    animal.sp.1 <- animal.sp[-which(contain.byid == FALSE)]
    
  } else {
    
    animal.sp.1 <- animal.sp
    
  }
  
  if (FALSE %in% contain.byid) {
    
    animal.data.1 <- animal.data[-which(contain.byid == FALSE),]
    
  } else {
    
    animal.data.1 <- animal.data
    
  }
  
  # create Case variable for analysis
  animal.data.1$Case <- 1
  
  # sample random points (n = 20 per used)
  random.sp <- spsample(animal.hr.proj, 
                        n = nrow(animal.data.1)*20,
                        type = "regular")
  
  # reproject SpatialPoints layers
  animal.sp.2 <- spTransform(animal.sp.1, crs(dDeveloped@crs))
  random.sp.2 <- spTransform(random.sp, crs(dDeveloped@crs))
  
  # create random.data data frame
  random.data.1 <- data.frame(x = random.sp@coords[ ,1],
                              y = random.sp@coords[ ,2],
                              t = NA,
                              burst = NA, 
                              DOP = NA,
                              Animal = animal.id,
                              Year = NA,
                              Season = "SU",
                              Case = 0)
  
  # extract raster values for Case = 1
  animal.data.1$dDeveloped    <- raster:::extract(dDeveloped, animal.sp.2, method = "simple")
  animal.data.1$dOpen         <- raster:::extract(dOpen, animal.sp.2, method = "simple")
  animal.data.1$dYoungForest  <- raster:::extract(dYoungForest, animal.sp.2, method = "simple")
  animal.data.1$dMatureForest <- raster:::extract(dMatureForest, animal.sp.2, method = "simple")
  animal.data.1$dEdge         <- raster:::extract(dEdge, animal.sp.2, method = "simple")
  animal.data.1$TRI           <- raster:::extract(TRI, animal.sp.2, method = "simple")
  animal.data.1$TPI           <- raster:::extract(TPI, animal.sp.2, method = "simple")
  animal.data.1$dRoad         <- raster:::extract(dRoad, animal.sp.2, method = "simple")
  animal.data.1$canopy        <- raster:::extract(canopy, animal.sp.2, method = "simple")
  
  # extract raster values for Case = 0
  random.data.1$dDeveloped    <- raster:::extract(dDeveloped, random.sp.2, method = "simple")
  random.data.1$dOpen         <- raster:::extract(dOpen, random.sp.2, method = "simple")
  random.data.1$dYoungForest  <- raster:::extract(dYoungForest, random.sp.2, method = "simple")
  random.data.1$dMatureForest <- raster:::extract(dMatureForest, random.sp.2, method = "simple")
  random.data.1$dEdge         <- raster:::extract(dEdge, random.sp.2, method = "simple")
  random.data.1$TRI           <- raster:::extract(TRI, random.sp.2, method = "simple")
  random.data.1$TPI           <- raster:::extract(TPI, random.sp.2, method = "simple")
  random.data.1$dRoad         <- raster:::extract(dRoad, random.sp.2, method = "simple")
  random.data.1$canopy        <- raster:::extract(canopy, random.sp.2, method = "simple")
  
  # bind together
  all.data.1 <- rbind(animal.data.1, random.data.1)
  
  # bind to master data frame
  SU.sampled <- rbind(SU.sampled, all.data.1)
  
}

#_____________________________________________________________________________________________________________
# 4c. U/A ----
#_____________________________________________________________________________________________________________

# collars to include
UA.collars <- c(37703, 37704, 37705, 37706, 37707, 37708, 37709, 37710, 37711, 37712,
                37714, 37715, 37716, 37717, 37718, 37719, 37720, 37722,
                37723, 37724, 37725, 37726, 37727,
                45469, 45470, 45492, 45493, 45494, 45496, 45497, 45498, 45499,
                45500, 45501, 45504, 45505, 45506, 45507, 45508, 45509,
                45510, 45511, 46391, 46392, 46393, 46394, 46396, 46397, 46399,
                46400, 46401, 46402,
                101978, 102489, 102492, 
                102495, 103172, 103174, 103175, 103176,
                103177, 103178, 103179, 103181, 103182,
                101973, 101974, 101980, 101985, 103173, 103180,
                103184, 103185, 103186, 103239, 103244, 103245, 103248, 103250, 103251)

# filter collars during the WS season and with > 30 relocations only 
UA.data <- elk.data %>% filter(Season == "UA" & Animal %in% UA.collars)

# blank data frame
UA.sampled <- data.frame()

# for loop to sample from each group
for (i in UA.collars) {
  
  animal.id <- i
  
  animal.data <- UA.data %>% filter(Animal == animal.id)
  
  # create SpatialPoints to sample data
  animal.sp <- SpatialPoints(coords = animal.data[ ,c("x", "y")],
                             proj4string = projection)
  
  # read in shapefile
  animal.hr <- readOGR(dsn = paste0(shp.dir, "/3 - UA"), layer = paste0("est_", animal.id, "_UA"))
  
  # transform HR to UTM projection
  animal.hr.proj <- spTransform(animal.hr, projection)
  
  # plot both
  sp:::plot(animal.hr.proj)
  sp:::plot(animal.sp, add = TRUE)
  
  # discard points that fall outside of the 99% contour
  contain.byid <- gContains(animal.hr.proj, animal.sp, byid = TRUE)
  
  if (FALSE %in% contain.byid) {
    
    animal.sp.1 <- animal.sp[-which(contain.byid == FALSE)]
    
  } else {
    
    animal.sp.1 <- animal.sp
    
  }
  
  if (FALSE %in% contain.byid) {
    
    animal.data.1 <- animal.data[-which(contain.byid == FALSE),]
    
  } else {
    
    animal.data.1 <- animal.data
    
  }
  
  # create Case variable for analysis
  animal.data.1$Case <- 1
  
  # sample random points (n = 20 per used)
  random.sp <- spsample(animal.hr.proj, 
                        n = nrow(animal.data.1)*20,
                        type = "regular")
  
  # reproject SpatialPoints layers
  animal.sp.2 <- spTransform(animal.sp.1, crs(dDeveloped@crs))
  random.sp.2 <- spTransform(random.sp, crs(dDeveloped@crs))
  
  # create random.data data frame
  random.data.1 <- data.frame(x = random.sp@coords[ ,1],
                              y = random.sp@coords[ ,2],
                              t = NA,
                              burst = NA, 
                              DOP = NA,
                              Animal = animal.id,
                              Year = NA,
                              Season = "UA",
                              Case = 0)
  
  # extract raster values for Case = 1
  animal.data.1$dDeveloped    <- raster:::extract(dDeveloped, animal.sp.2, method = "simple")
  animal.data.1$dOpen         <- raster:::extract(dOpen, animal.sp.2, method = "simple")
  animal.data.1$dYoungForest  <- raster:::extract(dYoungForest, animal.sp.2, method = "simple")
  animal.data.1$dMatureForest <- raster:::extract(dMatureForest, animal.sp.2, method = "simple")
  animal.data.1$dEdge         <- raster:::extract(dEdge, animal.sp.2, method = "simple")
  animal.data.1$TRI           <- raster:::extract(TRI, animal.sp.2, method = "simple")
  animal.data.1$TPI           <- raster:::extract(TPI, animal.sp.2, method = "simple")
  animal.data.1$dRoad         <- raster:::extract(dRoad, animal.sp.2, method = "simple")
  animal.data.1$canopy        <- raster:::extract(canopy, animal.sp.2, method = "simple")
  
  # extract raster values for Case = 0
  random.data.1$dDeveloped    <- raster:::extract(dDeveloped, random.sp.2, method = "simple")
  random.data.1$dOpen         <- raster:::extract(dOpen, random.sp.2, method = "simple")
  random.data.1$dYoungForest  <- raster:::extract(dYoungForest, random.sp.2, method = "simple")
  random.data.1$dMatureForest <- raster:::extract(dMatureForest, random.sp.2, method = "simple")
  random.data.1$dEdge         <- raster:::extract(dEdge, random.sp.2, method = "simple")
  random.data.1$TRI           <- raster:::extract(TRI, random.sp.2, method = "simple")
  random.data.1$TPI           <- raster:::extract(TPI, random.sp.2, method = "simple")
  random.data.1$dRoad         <- raster:::extract(dRoad, random.sp.2, method = "simple")
  random.data.1$canopy        <- raster:::extract(canopy, random.sp.2, method = "simple")
  
  # bind together
  all.data.1 <- rbind(animal.data.1, random.data.1)
  
  # bind to master data frame
  UA.sampled <- rbind(UA.sampled, all.data.1)
  
}

#_____________________________________________________________________________________________________________
# 4d. A/W ----
#_____________________________________________________________________________________________________________

# collars to include
AW.collars <- c(37703, 37704, 37705, 37706, 37707, 37708, 37709, 37710, 37711, 37712,
                37714, 37715, 37716, 37718, 37719, 37720, 37722,
                37723, 37724, 37725, 37726, 37727,
                45469, 45470, 45492, 45493, 45494, 45496, 45497, 45498,
                45500, 45501, 45504, 45505, 45506, 45507, 45508, 
                45510, 45511, 46391, 46392, 46393, 46394, 46396, 46397, 46399,
                46400, 46401, 46402,
                101940, 101946, 101948, 101954, 101962, 101978, 102491, 102493,
                102497, 102535, 102536, 103172, 103174, 103175, 103176, 103177, 103178,
                103179, 103181, 103182, 103249, 101974, 101980, 101985, 103173,
                103180, 103184, 103239, 103244, 103245, 103248, 103250, 103251)

# filter collars during the WS season and with > 30 relocations only 
AW.data <- elk.data %>% filter(Season == "AW" & Animal %in% AW.collars)

# blank data frame
AW.sampled <- data.frame()

# for loop to sample from each group
for (i in AW.collars) {
  
  animal.id <- i
  
  animal.data <- AW.data %>% filter(Animal == animal.id)
  
  # create SpatialPoints to sample data
  animal.sp <- SpatialPoints(coords = animal.data[ ,c("x", "y")],
                             proj4string = projection)
  
  # read in shapefile
  animal.hr <- readOGR(dsn = paste0(shp.dir, "/4 - AW"), layer = paste0("est_", animal.id, "_AW"))
  
  # transform HR to UTM projection
  animal.hr.proj <- spTransform(animal.hr, projection)
  
  # plot both
  sp:::plot(animal.hr.proj)
  sp:::plot(animal.sp, add = TRUE)
  
  # discard points that fall outside of the 99% contour
  contain.byid <- gContains(animal.hr.proj, animal.sp, byid = TRUE)
  
  if (FALSE %in% contain.byid) {
    
    animal.sp.1 <- animal.sp[-which(contain.byid == FALSE)]
    
  } else {
    
    animal.sp.1 <- animal.sp
    
  }
  
  if (FALSE %in% contain.byid) {
    
    animal.data.1 <- animal.data[-which(contain.byid == FALSE),]
    
  } else {
    
    animal.data.1 <- animal.data
    
  }
  
  # create Case variable for analysis
  animal.data.1$Case <- 1
  
  # sample random points (n = 20 per used)
  random.sp <- spsample(animal.hr.proj, 
                        n = nrow(animal.data.1)*20,
                        type = "regular")
  
  # reproject SpatialPoints layers
  animal.sp.2 <- spTransform(animal.sp.1, crs(dDeveloped@crs))
  random.sp.2 <- spTransform(random.sp, crs(dDeveloped@crs))
  
  # create random.data data frame
  random.data.1 <- data.frame(x = random.sp@coords[ ,1],
                              y = random.sp@coords[ ,2],
                              t = NA,
                              burst = NA, 
                              DOP = NA,
                              Animal = animal.id,
                              Year = NA,
                              Season = "AW",
                              Case = 0)
  
  # extract raster values for Case = 1
  animal.data.1$dDeveloped    <- raster:::extract(dDeveloped, animal.sp.2, method = "simple")
  animal.data.1$dOpen         <- raster:::extract(dOpen, animal.sp.2, method = "simple")
  animal.data.1$dYoungForest  <- raster:::extract(dYoungForest, animal.sp.2, method = "simple")
  animal.data.1$dMatureForest <- raster:::extract(dMatureForest, animal.sp.2, method = "simple")
  animal.data.1$dEdge         <- raster:::extract(dEdge, animal.sp.2, method = "simple")
  animal.data.1$TRI           <- raster:::extract(TRI, animal.sp.2, method = "simple")
  animal.data.1$TPI           <- raster:::extract(TPI, animal.sp.2, method = "simple")
  animal.data.1$dRoad         <- raster:::extract(dRoad, animal.sp.2, method = "simple")
  animal.data.1$canopy        <- raster:::extract(canopy, animal.sp.2, method = "simple")
  
  # extract raster values for Case = 0
  random.data.1$dDeveloped    <- raster:::extract(dDeveloped, random.sp.2, method = "simple")
  random.data.1$dOpen         <- raster:::extract(dOpen, random.sp.2, method = "simple")
  random.data.1$dYoungForest  <- raster:::extract(dYoungForest, random.sp.2, method = "simple")
  random.data.1$dMatureForest <- raster:::extract(dMatureForest, random.sp.2, method = "simple")
  random.data.1$dEdge         <- raster:::extract(dEdge, random.sp.2, method = "simple")
  random.data.1$TRI           <- raster:::extract(TRI, random.sp.2, method = "simple")
  random.data.1$TPI           <- raster:::extract(TPI, random.sp.2, method = "simple")
  random.data.1$dRoad         <- raster:::extract(dRoad, random.sp.2, method = "simple")
  random.data.1$canopy        <- raster:::extract(canopy, random.sp.2, method = "simple")
  
  # bind together
  all.data.1 <- rbind(animal.data.1, random.data.1)
  
  # bind to master data frame
  AW.sampled <- rbind(AW.sampled, all.data.1)
  
}

#_____________________________________________________________________________________________________________
# 5. Assign groups ---- 
#_____________________________________________________________________________________________________________

# read in group key csv
group.key <- read.csv("Group key.csv")

#_____________________________________________________________________________________________________________
# 5a. W/S ---- 
#_____________________________________________________________________________________________________________

# blank variables
WS.sampled$GroupID <- NA

# fill variable with correct GroupID
for (i in 1:nrow(WS.sampled)) {
  
  row <- i
  
  CollarID <- as.character(WS.sampled$Animal[row])
  
  WS.sampled$GroupID[row] <- as.character(group.key$WS[group.key$CollarID == CollarID])
                                  
}

#_____________________________________________________________________________________________________________
# 5b. S/U ---- 
#_____________________________________________________________________________________________________________

# blank variables
SU.sampled$GroupID <- NA

# fill variable with correct GroupID
for (i in 1:nrow(SU.sampled)) {
  
  row <- i
  
  CollarID <- as.character(SU.sampled$Animal[row])
  
  SU.sampled$GroupID[row] <- as.character(group.key$SU[group.key$CollarID == CollarID])
  
}

#_____________________________________________________________________________________________________________
# 5c. U/A ---- 
#_____________________________________________________________________________________________________________

# blank variables
UA.sampled$GroupID <- NA

# fill variable with correct GroupID
for (i in 1:nrow(UA.sampled)) {
  
  row <- i
  
  CollarID <- as.character(UA.sampled$Animal[row])
  
  UA.sampled$GroupID[row] <- as.character(group.key$UA[group.key$CollarID == CollarID])
  
}

#_____________________________________________________________________________________________________________
# 5d. A/W ---- 
#_____________________________________________________________________________________________________________

# blank variables
AW.sampled$GroupID <- NA

# fill variable with correct GroupID
for (i in 1:nrow(AW.sampled)) {
  
  row <- i
  
  CollarID <- as.character(AW.sampled$Animal[row])
  
  AW.sampled$GroupID[row] <- as.character(group.key$AW[group.key$CollarID == CollarID])
  
}

#_____________________________________________________________________________________________________________
# 6. Write to csvs ---- 
#_____________________________________________________________________________________________________________

write.csv(WS.sampled, "WS_sampled.csv")
write.csv(SU.sampled, "SU_sampled.csv")
write.csv(UA.sampled, "UA_sampled.csv")
write.csv(AW.sampled, "AW_sampled.csv")
