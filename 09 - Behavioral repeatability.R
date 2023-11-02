# Title: Individual variation in habitat selection
# Subtitle: 9 - Repeatability in behavioral metrics
# Author: Nathan D. Hooven
# Email: nathan.d.hooven@gmail.com
# Affiliation: Department of Forestry and Natural Resources, University of Kentucky
# Date began: 21 Feb 2022
# Date completed: 21 Feb 2022
# Date modified: 22 Jul 2023
# R version: 3.6.2

#_____________________________________________________________________________________________________________
# 1. Load in required packages ----
#_____________________________________________________________________________________________________________

library(tidyverse)
library(mefa4)         # %notin%
library(rptR)          # repeatability
library(rgdal)         # read in shapefiles
library(rgeos)         # compute GOI

#_____________________________________________________________________________________________________________
# 2. Read in data ----
#_____________________________________________________________________________________________________________

WS.data <- read.csv("WS_pergroup.csv")
SU.data <- read.csv("SU_pergroup.csv")
UA.data <- read.csv("UA_pergroup.csv")
AW.data <- read.csv("AW_pergroup.csv")

#_____________________________________________________________________________________________________________
# 3. Create indiv.id columns ----
#_____________________________________________________________________________________________________________
# 3a. WS ----
#_____________________________________________________________________________________________________________

WS.data$indiv.id <- WS.data$GroupID

# create extra rows
WS.group.1 <- WS.data %>% filter(GroupID == "AL1")
WS.group.1 <- rbind(WS.group.1, WS.group.1)
WS.group.1$indiv.id <- as.factor(c(37708, 37727))

WS.group.2 <- WS.data %>% filter(GroupID == "FT1")
WS.group.2 <- rbind(WS.group.2, WS.group.2, WS.group.2, WS.group.2)
WS.group.2$indiv.id <- as.factor(c(37706, 37717, 37718, 37723))

WS.group.3 <- WS.data %>% filter(GroupID == "SF1")
WS.group.3 <- rbind(WS.group.3, WS.group.3)
WS.group.3$indiv.id <- as.factor(c(45505, 46394))

WS.data.1 <- WS.data %>% filter(GroupID %notin% c("AL1", "FT1", "SF1")) %>%
                         bind_rows(WS.group.1, WS.group.2, WS.group.3)

#_____________________________________________________________________________________________________________
# 3b. SU ----
#_____________________________________________________________________________________________________________

SU.data$indiv.id <- SU.data$GroupID

#_____________________________________________________________________________________________________________
# 3c. UA ----
#_____________________________________________________________________________________________________________

UA.data$indiv.id <- UA.data$GroupID

# create extra rows
UA.group.1 <- UA.data %>% filter(GroupID == "FT1")
UA.group.1 <- rbind(UA.group.1, UA.group.1)
UA.group.1$indiv.id <- as.factor(c(37706, 37709))

UA.group.2 <- UA.data %>% filter(GroupID == "CA1")
UA.group.2 <- rbind(UA.group.2, UA.group.2)
UA.group.2$indiv.id <- as.factor(c(37710, 37714))

UA.data.1 <- UA.data %>% filter(GroupID %notin% c("FT1", "CA1")) %>%
                         bind_rows(UA.group.1, UA.group.2)

#_____________________________________________________________________________________________________________
# 3d. AW ----
#_____________________________________________________________________________________________________________

AW.data$indiv.id <- AW.data$GroupID

# create extra rows
AW.group.1 <- AW.data %>% filter(GroupID == "CA1")
AW.group.1 <- rbind(AW.group.1, AW.group.1, AW.group.1)
AW.group.1$indiv.id <- as.factor(c(37710, 37719, 37722))

AW.group.2 <- AW.data %>% filter(GroupID == "FT1")
AW.group.2 <- rbind(AW.group.2, AW.group.2, AW.group.2, AW.group.2)
AW.group.2$indiv.id <- as.factor(c(37706, 37709, 37718, 37723))

AW.group.3 <- AW.data %>% filter(GroupID == "SF1")
AW.group.3 <- rbind(AW.group.3, AW.group.3, AW.group.3)
AW.group.3$indiv.id <- as.factor(c(45505, 45508, 46393))

AW.group.4 <- AW.data %>% filter(GroupID == "TR1")
AW.group.4 <- rbind(AW.group.4, AW.group.4)
AW.group.4$indiv.id <- as.factor(c(45507, 46400))

AW.data.1 <- AW.data %>% filter(GroupID %notin% c("CA1", "FT1", "SF1", "TR1")) %>%
                         bind_rows(AW.group.1, AW.group.2, AW.group.3, AW.group.4)

#_____________________________________________________________________________________________________________
# 4. Combine dfs across seasons to compare behavioral consistency ----
#_____________________________________________________________________________________________________________

WS.data.2 <- WS.data.1 %>% dplyr::select(dDeveloped, dRoad.P, TRI.x, TPI.x, dOpen, dMatureForest, dYoungForest, area.est.topo, 
                                         dev, dRoad, TRI.y, TPI.y, open, mf, yf, indiv.id)
WS.data.2$Season <- "WS"

SU.data.2 <- SU.data %>% dplyr::select(dDeveloped, dRoad.P, TRI.x, TPI.x, dOpen, dMatureForest, dYoungForest, area.est.topo, 
                                       dev, dRoad, TRI.y, TPI.y, open, mf, yf, indiv.id)
SU.data.2$Season <- "SU"

UA.data.2 <- UA.data.1 %>% dplyr::select(dDeveloped, dRoad.P, TRI.x, TPI.x, dOpen, dMatureForest, dYoungForest, area.est.topo,
                                         dev, dRoad, TRI.y, TPI.y, open, mf, yf, indiv.id)
UA.data.2$Season <- "UA"

AW.data.2 <- AW.data.1 %>% dplyr::select(dDeveloped, dRoad.P, TRI.x, TPI.x, dOpen, dMatureForest, dYoungForest, area.est.topo, 
                                         dev, dRoad, TRI.y, TPI.y, open, mf, yf, indiv.id)
AW.data.2$Season <- "AW"

# convert all indiv.id columns to characters
WS.data.2$indiv.id <- as.character(WS.data.2$indiv.id)
SU.data.2$indiv.id <- as.character(SU.data.2$indiv.id)
UA.data.2$indiv.id <- as.character(UA.data.2$indiv.id)
AW.data.2$indiv.id <- as.character(AW.data.2$indiv.id)

# bind dfs
all.data.1 <- rbind(WS.data.2, SU.data.2, UA.data.2, AW.data.2)

# pivot_longer for plotting
all.data.2 <- pivot_longer(all.data.1, cols = c(dDeveloped, dRoad.P, TRI.x, TPI.x, dOpen, dMatureForest, dYoungForest, area.est.topo))

#_____________________________________________________________________________________________________________
# 5. Repeatability estimates ----
#_____________________________________________________________________________________________________________

# dDeveloped
rpt(dDeveloped ~ (1 | indiv.id), grname = "indiv.id", data = all.data.1, nboot = 1000, npermut = 0)

# dRoad.P
rpt(dRoad.P ~ (1 | indiv.id), grname = "indiv.id", data = all.data.1, nboot = 1000, npermut = 0)

# TRI
rpt(TRI.x ~ (1 | indiv.id), grname = "indiv.id", data = all.data.1, nboot = 1000, npermut = 0)

# TPI
rpt(TPI.x ~ (1 | indiv.id), grname = "indiv.id", data = all.data.1, nboot = 1000, npermut = 0)

# dOpen
rpt(dOpen ~ (1 | indiv.id), grname = "indiv.id", data = all.data.1, nboot = 1000, npermut = 0)

# dMatureForest
rpt(dMatureForest ~ (1 | indiv.id), grname = "indiv.id", data = all.data.1, nboot = 1000, npermut = 0)

# dYoungForest
rpt(dYoungForest ~ (1 | indiv.id), grname = "indiv.id", data = all.data.1, nboot = 1000, npermut = 0)

# HR area
rpt(area.est.topo ~ (1 | indiv.id), grname = "indiv.id", data = all.data.1, nboot = 1000, npermut = 0)

#_____________________________________________________________________________________________________________
# 6. Repeatability plots ----
#_____________________________________________________________________________________________________________

all.data.2$Season <- factor(all.data.2$Season, 
                            levels = c("WS", "SU", "UA", "AW"))

all.data.2$name <- factor(all.data.2$name, 
                          levels = c("dDeveloped", "dRoad.P", "TRI.x", "TPI.x",
                                     "dOpen", "dMatureForest", "dYoungForest", "area.est.topo"),
                          labels = c("dDeveloped", "ln(dRoad)", "TRI", "TPI",
                                     "dOpen", "dMatureForest", "dYoungForest", "HR area"))

ggplot(all.data.2, aes(x = as.factor(Season), y = value, group = indiv.id)) +
       theme_bw() +
       facet_wrap(~name, scales = "free") +
       geom_line(alpha = 0.2, color = "red") +
       xlab("") +
       ylab("") + 
       theme(panel.grid = element_blank())

#_____________________________________________________________________________________________________________
# 7. Coefficient of variation ----
#_____________________________________________________________________________________________________________

all.data.1.sum <- all.data.1 %>% group_by(indiv.id) %>% 
                                 summarize(sd.dDeveloped = sd(dDeveloped),
                                           sd.dRoad.P = sd(dRoad.P),
                                           sd.TRI.x = sd(TRI.x),
                                           sd.TPI.x = sd(TPI.x),
                                           sd.dOpen = sd(dOpen),
                                           sd.dMatureForest = sd(dMatureForest),
                                           sd.dYoungForest = sd(dYoungForest),
                                           sd.dev = sd(dev),
                                           sd.dRoad = sd(dRoad),
                                           sd.TRI.y = sd(TRI.y),
                                           sd.TPI.y = sd(TPI.y),
                                           sd.open = sd(open),
                                           sd.mf = sd(mf),
                                           sd.yf = sd(yf))

#_____________________________________________________________________________________________________________
# 8. Home range overlap using the Ferrarini index (GOI) ----
#_____________________________________________________________________________________________________________

# shapefile directory
shp.dir <- "D:/Elk project/Data analysis/Home ranges/ADKE/Shapefiles"

# UTM CRS
UTM.crs <- CRS("+proj=utm +zone=17 +ellps=GRS80 +units=m +no_defs")

# create a function that computes the general overlap index (GOI) from Ferrarini et al. (2021)
GOI <- function(n.hrs = n.seasons) {

        # common area factor (to convert to km2)
        area.fac <- 1000000
        
        # sum areas iteratively
        area.sum <- 0
        
        for (x in 1:length(shp.list)) {
                
              hr.area <- gArea(shp.list[[x]]) / area.fac
              
              area.sum <- area.sum + hr.area
                
        }
        
        # union of all home ranges 
        if (n.hrs == 2) {
                
            all.union <- gUnion(shp.list[[1]], shp.list[[2]])
                
        } else {
                
                if (n.hrs == 3) {
                        
                     all.union.1 <- gUnion(shp.list[[1]], shp.list[[2]])  
                     all.union <- gUnion(all.union.1, shp.list[[3]])
                        
                } else {
                        
                        if (n.hrs == 4) {
                                
                                all.union.1 <- gUnion(shp.list[[1]], shp.list[[2]])
                                all.union.2 <- gUnion(shp.list[[3]], shp.list[[4]])
                                all.union <- gUnion(all.union.1, all.union.2)
                                
                        } 
                }
                
        }
        
        # area of union
        area.union <- gArea(all.union) / area.fac
        
        # max area
        all.areas <- c()
        
        for (x in 1:length(shp.list)) {
                
              hr.area <- gArea(shp.list[[x]]) / area.fac
              
              all.areas <- c(all.areas, hr.area)
              
              area.max <- max(all.areas)
                
        }
        
        # calculate GOI
        goi.indiv <- 100*((area.sum - area.union) / (area.sum - area.max))
        
        return(goi.indiv)
}

# for loop to iterate over all individuals
all.data.2.sum <- all.data.1.sum[complete.cases(all.data.1.sum), ]

all.data.2.sum$goi <- NA

for (i in unique(all.data.1$indiv.id)) {
        
        # subset by indiv.id
        indiv <- i 
        
        indiv.data <- all.data.1 %>% filter(indiv.id == indiv)
        
        # determine how many seasons we need HRs for
        n.seasons <- nrow(indiv.data)
        
        # read in shapefiles only for seasons we need, transform to UTM 
        shp.list <- list()
        
        # WS
        if ("WS" %in% indiv.data$Season) {
                
                shp.WS <- readOGR(dsn = paste0(shp.dir, "/1 - WS"),
                                  layer = paste0("est_", indiv, "_WS"))
                
                shp.WS.proj <- spTransform(shp.WS, UTM.crs)
                
                shp.list <- c(shp.list, shp.WS.proj)
                
        } else {
                
                NULL
        }
        
        # SU
        if ("SU" %in% indiv.data$Season) {
                
                shp.SU <- readOGR(dsn = paste0(shp.dir, "/2 - SU"),
                                  layer = paste0("est_", indiv, "_SU"))
                
                shp.SU.proj <- spTransform(shp.SU, UTM.crs)
                
                shp.list <- c(shp.list, shp.SU.proj)
                
        } else {
                
                NULL
        }
        
        # UA
        if ("UA" %in% indiv.data$Season) {
                
                shp.UA <- readOGR(dsn = paste0(shp.dir, "/3 - UA"),
                                  layer = paste0("est_", indiv, "_UA"))
                
                shp.UA.proj <- spTransform(shp.UA, UTM.crs)
                
                shp.list <- c(shp.list, shp.UA.proj)
                
        } else {
                
                NULL
        }
        
        # AW
        if ("AW" %in% indiv.data$Season) {
                
                shp.AW <- readOGR(dsn = paste0(shp.dir, "/4 - AW"),
                                  layer = paste0("est_", indiv, "_AW"))
                
                shp.AW.proj <- spTransform(shp.AW, UTM.crs)
                
                shp.list <- c(shp.list, shp.AW.proj)
                
        } else {
                
                NULL
        }
        
        # calculate GOI
        goi.indiv <- GOI(n.hrs = n.seasons)
         
        # bind to correct row
        all.data.2.sum$goi[all.data.2.sum$indiv.id == indiv] <- goi.indiv
        
}

# look at distribution of GOI
hist(all.data.2.sum$goi)

all.data.3.sum <- all.data.1.sum[complete.cases(all.data.1.sum), ]

#_____________________________________________________________________________________________________________
# 9. Vizualize HR overlap and variability in selection ----
#_____________________________________________________________________________________________________________

all.data.3.sum <- all.data.3.sum %>% mutate(goi = all.data.2.sum$goi) 

all.data.3.sum.longer <- all.data.3.sum %>% pivot_longer(cols = 2:8)

all.data.3.sum.longer$name <- factor(all.data.3.sum.longer$name,
                                     levels = colnames(all.data.3.sum)[2:8],
                                     labels = c("dDeveloped", "ln(dRoad)", "TRI", "TPI",
                                                "dOpen", "dMatureForest", "dYoungForest"))

# plot
ggplot(data = all.data.3.sum.longer, aes(x = goi, y = value)) +
       theme_bw() +
        facet_wrap(~name, scales = "free_y", nrow = 2) +
        geom_point(shape = 21) +
        geom_smooth(method = "lm",
                    color = "darkgreen",
                    fill = "darkgreen",
                    alpha = 0.2) +
        ylab("SD of selection coefficients") +
        xlab("GOI") +
        theme(panel.grid = element_blank())

#_____________________________________________________________________________________________________________
# 10. Regressions ----
#_____________________________________________________________________________________________________________

summary(lm(sd.dDeveloped ~ goi, data = all.data.3.sum))
summary(lm(sd.dRoad.P ~ goi, data = all.data.3.sum))
summary(lm(sd.TRI.x ~ goi, data = all.data.3.sum))
summary(lm(sd.TPI.x ~ goi, data = all.data.3.sum))
summary(lm(sd.dOpen ~ goi, data = all.data.3.sum))
summary(lm(sd.dMatureForest ~ goi, data = all.data.3.sum))
summary(lm(sd.dYoungForest ~ goi, data = all.data.3.sum))

save.image("goi.RData")
