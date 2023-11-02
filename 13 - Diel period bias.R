# Title: Individual variation in habitat selection
# Subtitle: 13 - Diel period bias
# Author: Nathan D. Hooven
# Email: nathan.hooven@uky.edu
# Affiliation: Department of Forestry and Natural Resources, University of Kentucky
# Date began: 17 Mar 2022
# Date completed: 24 Mar 2022
# Date modified: 
# R version: 3.6.2

#_____________________________________________________________________________________________________________
# 1. Load in required packages ----
#_____________________________________________________________________________________________________________

library(tidyverse)
library(lubridate)
library(sp)
library(suncalc)     # get sun times
library(mefa4)
library(rstatix)     # Wilcoxon test

#_____________________________________________________________________________________________________________
# 2. Read in data ----
#_____________________________________________________________________________________________________________

WS.data <- read.csv("WS_sampled.csv")
SU.data <- read.csv("SU_sampled.csv")
UA.data <- read.csv("UA_sampled.csv")
AW.data <- read.csv("AW_sampled.csv")

# UTM projection
projection <- CRS("+proj=utm +zone=17 +ellps=GRS80 +units=m +no_defs")

#_____________________________________________________________________________________________________________
# 3. WS ----
#_____________________________________________________________________________________________________________

WS.data <- WS.data %>% filter(GroupID %notin% c(101955, 101965))

WS.sun <- WS.data %>% filter(Case == 1) %>% 
                      dplyr::select(x, y, t, GroupID)

WS.sun$t <- as.POSIXct(mdy_hm(WS.sun$t, tz = "America/New_York"))

# add date column
WS.sun$date <- as.Date(WS.sun$t)

# convert to lat/long
WS.sun.sp <- SpatialPoints(coords = WS.sun[ ,c("x", "y")],
                             proj4string = projection)

WS.sun.latlong <- spTransform(WS.sun.sp, CRS("+proj=longlat +datum=WGS84"))

WS.sun$lon <- WS.sun.latlong@coords[,1]
WS.sun$lat <- WS.sun.latlong@coords[,2]

# calculate sunrises and sunsets
WS.sunrise <- getSunlightTimes(data = WS.sun, keep = "sunrise", tz = "America/New_York")
WS.sunset <- getSunlightTimes(data = WS.sun, keep = "sunset", tz = "America/New_York")

# bind to df
WS.sun$sunrise <- WS.sunrise$sunrise
WS.sun$sunset<- WS.sunset$sunset

# create diel period column
WS.sun$diel <- ifelse((WS.sun$t > WS.sun$sunrise & WS.sun$t < WS.sun$sunset),
                                 "Day",
                                 "Night")

# summary 
WS.sun.summary <- WS.sun %>% group_by(GroupID) %>%
                             summarize(n.day = sum(diel == "Day"),
                                       n.night = sum(diel == "Night"))

# column sums
colSums(WS.sun.summary[ ,c(2:3)])

# differences and percent differences
WS.sun.summary <- WS.sun.summary %>% mutate(diff = n.night - n.day,
                                            p.diff = (diff / (n.night + n.day)*100))
mean(WS.sun.summary$diff)
mean(WS.sun.summary$p.diff)

# plot of percent difference
WS.sun.p.diff.plot <- ggplot(data = WS.sun.summary, aes(x = p.diff)) +
       theme_bw() +
       geom_vline(xintercept = 0,
                  linetype = "dashed") +
       geom_histogram(bins = 30,
                      color = "black",
                      fill = "#009900") +
       theme(panel.grid = element_blank(),
             axis.title.y = element_blank(),
             axis.title.x = element_blank())

# bind to fr df
WS.fr <- read.csv("WS_pergroup.csv")
WS.fr.2 <- merge(WS.fr, WS.sun.summary)

# does it affect the habitat selection inference?
ggplot(data = WS.fr.2, aes(x = p.diff, y = dMatureForest)) +
       geom_point() +
       geom_smooth(method = "lm")

cor(WS.fr.2[ ,c(38, 4:10)], method = "spearman")

#_____________________________________________________________________________________________________________
# 4. SU ----
#_____________________________________________________________________________________________________________

SU.sun <- SU.data %>% filter(Case == 1) %>% 
                      dplyr::select(x, y, t, GroupID)

SU.sun$t <- as.POSIXct(ymd_hms(SU.sun$t, tz = "America/New_York"))

# add date column
SU.sun$date <- as.Date(SU.sun$t)

# convert to lat/long
SU.sun.sp <- SpatialPoints(coords = SU.sun[ ,c("x", "y")],
                             proj4string = projection)

SU.sun.latlong <- spTransform(SU.sun.sp, CRS("+proj=longlat +datum=WGS84"))

SU.sun$lon <- SU.sun.latlong@coords[,1]
SU.sun$lat <- SU.sun.latlong@coords[,2]

# calculate sunrises and sunsets
SU.sunrise <- getSunlightTimes(data = SU.sun, keep = "sunrise", tz = "America/New_York")
SU.sunset <- getSunlightTimes(data = SU.sun, keep = "sunset", tz = "America/New_York")

# bind to df
SU.sun$sunrise <- SU.sunrise$sunrise
SU.sun$sunset<- SU.sunset$sunset

# create diel period column
SU.sun$diel <- ifelse((SU.sun$t > SU.sun$sunrise & SU.sun$t < SU.sun$sunset),
                                 "Day",
                                 "Night")

# summary 
SU.sun.summary <- SU.sun %>% group_by(GroupID) %>%
                             summarize(n.day = sum(diel == "Day"),
                                       n.night = sum(diel == "Night"))

# column sums
colSums(SU.sun.summary[ ,c(2:3)])

# differences and percent differences
SU.sun.summary <- SU.sun.summary %>% mutate(diff = n.night - n.day,
                                            p.diff = (diff / (n.night + n.day)*100))
mean(SU.sun.summary$diff)
mean(SU.sun.summary$p.diff)

# plot of percent difference
SU.sun.p.diff.plot <- ggplot(data = SU.sun.summary, aes(x = p.diff)) +
       theme_bw() +
       geom_vline(xintercept = 0,
                  linetype = "dashed") +
       geom_histogram(bins = 30,
                      color = "black",
                      fill = "#FF9900") +
       theme(panel.grid = element_blank(),
             axis.title.y = element_blank(),
             axis.title.x = element_blank())

# bind to fr df
SU.fr <- read.csv("SU_pergroup.csv")
SU.fr.2 <- merge(SU.fr, SU.sun.summary)

ggplot(data = SU.fr.2, aes(x = p.diff, y = dMatureForest)) +
       geom_point() +
       geom_smooth(method = "lm")

cor(SU.fr.2[ ,c(38, 4:10)], method = "spearman")

#_____________________________________________________________________________________________________________
# 5. UA ----
#_____________________________________________________________________________________________________________

UA.sun <- UA.data %>% filter(Case == 1) %>% 
                      dplyr::select(x, y, t, GroupID)

UA.sun$t <- as.POSIXct(ymd_hms(UA.sun$t, tz = "America/New_York"))

# add date column
UA.sun$date <- as.Date(UA.sun$t)

# convert to lat/long
UA.sun.sp <- SpatialPoints(coords = UA.sun[ ,c("x", "y")],
                             proj4string = projection)

UA.sun.latlong <- spTransform(UA.sun.sp, CRS("+proj=longlat +datum=WGS84"))

UA.sun$lon <- UA.sun.latlong@coords[,1]
UA.sun$lat <- UA.sun.latlong@coords[,2]

# calculate sunrises and sunsets
UA.sunrise <- getSunlightTimes(data = UA.sun, keep = "sunrise", tz = "America/New_York")
UA.sunset <- getSunlightTimes(data = UA.sun, keep = "sunset", tz = "America/New_York")

# bind to df
UA.sun$sunrise <- UA.sunrise$sunrise
UA.sun$sunset<- UA.sunset$sunset

# create diel period column
UA.sun$diel <- ifelse((UA.sun$t > UA.sun$sunrise & UA.sun$t < UA.sun$sunset),
                                 "Day",
                                 "Night")

# summary 
UA.sun.summary <- UA.sun %>% group_by(GroupID) %>%
                             summarize(n.day = sum(diel == "Day"),
                                       n.night = sum(diel == "Night"))

# column sums
colSums(UA.sun.summary[ ,c(2:3)])

# differences and percent differences
UA.sun.summary <- UA.sun.summary %>% mutate(diff = n.night - n.day,
                                            p.diff = (diff / (n.night + n.day)*100))
mean(UA.sun.summary$diff)
mean(UA.sun.summary$p.diff)

# plot of percent difference
UA.sun.p.diff.plot <- ggplot(data = UA.sun.summary, aes(x = p.diff)) +
       theme_bw() +
       geom_vline(xintercept = 0,
                  linetype = "dashed") +
       geom_histogram(bins = 30,
                      color = "black",
                      fill = "#FF9900") +
       theme(panel.grid = element_blank(),
             axis.title.y = element_blank(),
             axis.title.x = element_blank())

# bind to fr df
UA.fr <- read.csv("UA_pergroup.csv")
UA.fr.2 <- merge(UA.fr, UA.sun.summary)

ggplot(data = UA.fr.2, aes(x = p.diff, y = dMatureForest)) +
       geom_point() +
       geom_smooth(method = "lm")

cor(UA.fr.2[ ,c(38, 4:10)], method = "spearman")

#_____________________________________________________________________________________________________________
# 5. AW ----
#_____________________________________________________________________________________________________________

AW.sun <- AW.data %>% filter(Case == 1) %>% 
                      dplyr::select(x, y, t, GroupID)

AW.sun$t <- as.POSIXct(ymd_hms(AW.sun$t, tz = "America/New_York"))

# add date column
AW.sun$date <- as.Date(AW.sun$t)

# convert to lat/long
AW.sun.sp <- SpatialPoints(coords = AW.sun[ ,c("x", "y")],
                             proj4string = projection)

AW.sun.latlong <- spTransform(AW.sun.sp, CRS("+proj=longlat +datum=WGS84"))

AW.sun$lon <- AW.sun.latlong@coords[,1]
AW.sun$lat <- AW.sun.latlong@coords[,2]

# calculate sunrises and sunsets
AW.sunrise <- getSunlightTimes(data = AW.sun, keep = "sunrise", tz = "America/New_York")
AW.sunset <- getSunlightTimes(data = AW.sun, keep = "sunset", tz = "America/New_York")

# bind to df
AW.sun$sunrise <- AW.sunrise$sunrise
AW.sun$sunset<- AW.sunset$sunset

# create diel period column
AW.sun$diel <- ifelse((AW.sun$t > AW.sun$sunrise & AW.sun$t < AW.sun$sunset),
                                 "Day",
                                 "Night")

# summary 
AW.sun.summary <- AW.sun %>% group_by(GroupID) %>%
                             summarize(n.day = sum(diel == "Day"),
                                       n.night = sum(diel == "Night"))

# column sums
colSums(AW.sun.summary[ ,c(2:3)])

# differences and percent differences
AW.sun.summary <- AW.sun.summary %>% mutate(diff = n.night - n.day,
                                            p.diff = (diff / (n.night + n.day)*100))
mean(AW.sun.summary$diff)
mean(AW.sun.summary$p.diff)

# plot of percent difference
AW.sun.p.diff.plot <- ggplot(data = AW.sun.summary, aes(x = p.diff)) +
       theme_bw() +
       geom_vline(xintercept = 0,
                  linetype = "dashed") +
       geom_histogram(bins = 30,
                      color = "black",
                      fill = "#FF9900") +
       theme(panel.grid = element_blank(),
             axis.title.y = element_blank(),
             axis.title.x = element_blank())

# bind to fr df
AW.fr <- read.csv("AW_pergroup.csv")
AW.fr.2 <- merge(AW.fr, AW.sun.summary)

cor(AW.fr.2[ ,c(38, 4:10)], method = "spearman")

#_____________________________________________________________________________________________________________
# 6. Save image and csvs ----
#_____________________________________________________________________________________________________________

save.image("suncalc.RData")

write.csv(WS.fr.2, "WS_pergroup2.csv")
write.csv(SU.fr.2, "WS_pergroup2.csv")
write.csv(UA.fr.2, "WS_pergroup2.csv")
write.csv(AW.fr.2, "WS_pergroup2.csv")
