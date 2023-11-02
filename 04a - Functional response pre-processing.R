# Title: Individual variation in habitat selection
# Subtitle: 4a - Functional response pre-processing
# Author: Nathan D. Hooven
# Email: nathan.d.hooven@gmail.com
# Affiliation: Department of Forestry and Natural Resources, University of Kentucky
# Date began: 5 Jan 2022
# Date completed: 6 Jan 2022
# Date modified: 8 Feb 2022
# R version: 3.6.2

#_____________________________________________________________________________________________________________
# 1. Load in required packages ----
#_____________________________________________________________________________________________________________

library(tidyverse)
library(factoextra)       # PCA visualization

#_____________________________________________________________________________________________________________
# 2. Read in data ----
#_____________________________________________________________________________________________________________
# 2a. Random slope data ----
#_____________________________________________________________________________________________________________

# WS
WS.slopes <- read.csv("randomslopes_WS_analysis.csv")

WS.slopes <- WS.slopes %>% dplyr::select(rownames.coefs.df., dDeveloped, dRoad.P, TRI, TPI, 
                                         dOpen, dMatureForest, dYoungForest, Season)

# SU
SU.slopes <- read.csv("randomslopes_SU_analysis.csv")

SU.slopes <- SU.slopes %>% dplyr::select(rownames.coefs.df., dDeveloped, dRoad.P, TRI, TPI, 
                                         dOpen, dMatureForest, dYoungForest, Season)

# UA
UA.slopes <- read.csv("randomslopes_UA_analysis.csv")

UA.slopes <- UA.slopes %>% dplyr::select(rownames.coefs.df., dDeveloped, dRoad.P, TRI, TPI, 
                                         dOpen, dMatureForest, dYoungForest, Season)

# AW
AW.slopes <- read.csv("randomslopes_AW_analysis.csv")

AW.slopes <- AW.slopes %>% dplyr::select(rownames.coefs.df., dDeveloped, dRoad.P, TRI, TPI, 
                                         dOpen, dMatureForest, dYoungForest, Season)

#_____________________________________________________________________________________________________________
# 2b. Availability data ----
#_____________________________________________________________________________________________________________

# WS
WS.avail.indiv <- read.csv("G:/Elk project/Data analysis/Home ranges/ADKE/WS_HRs.csv")
WS.avail.group <- read.csv("G:/Elk project/Data analysis/Home ranges/ADKE/WS_groups.csv")

WS.avail.indiv.1 <- WS.avail.indiv %>% dplyr::select(CollarID, dev, dRoad, TRI, TPI, open, mf, yf, iji, ed, area.est.topo)
WS.avail.group.1 <- WS.avail.group %>% dplyr::select(CollarID, dev, dRoad, TRI, TPI, open, mf, yf, iji, ed)

# add in mean area.est.topo for groups
WS.avail.group.1$area.est.topo <- NA

WS.avail.group.1$area.est.topo[1] <- mean(c(WS.avail.indiv.1$area.est.topo[WS.avail.indiv.1$CollarID == 37706],
                                            WS.avail.indiv.1$area.est.topo[WS.avail.indiv.1$CollarID == 37717],
                                            WS.avail.indiv.1$area.est.topo[WS.avail.indiv.1$CollarID == 37718],
                                            WS.avail.indiv.1$area.est.topo[WS.avail.indiv.1$CollarID == 37723]))

WS.avail.group.1$area.est.topo[2] <- mean(c(WS.avail.indiv.1$area.est.topo[WS.avail.indiv.1$CollarID == 37708],
                                            WS.avail.indiv.1$area.est.topo[WS.avail.indiv.1$CollarID == 37727]))

WS.avail.group.1$area.est.topo[3] <- mean(c(WS.avail.indiv.1$area.est.topo[WS.avail.indiv.1$CollarID == 45505],
                                            WS.avail.indiv.1$area.est.topo[WS.avail.indiv.1$CollarID == 46394]))

WS.avail.all <- rbind(WS.avail.indiv.1, WS.avail.group.1)

names(WS.avail.all)[1] <- "GroupID"

# SU
SU.avail.indiv <- read.csv("G:/Elk project/Data analysis/Home ranges/ADKE/SU_HRs.csv")

SU.avail.indiv.1 <- SU.avail.indiv %>% dplyr::select(CollarID, dev, dRoad, TRI, TPI, open, mf, yf, iji, ed, area.est.topo)

SU.avail.all <- SU.avail.indiv.1

names(SU.avail.all)[1] <- "GroupID"

# UA
UA.avail.indiv <- read.csv("G:/Elk project/Data analysis/Home ranges/ADKE/UA_HRs.csv")
UA.avail.group <- read.csv("G:/Elk project/Data analysis/Home ranges/ADKE/UA_groups.csv")

UA.avail.indiv.1 <- UA.avail.indiv %>% dplyr::select(CollarID, dev, dRoad, TRI, TPI, open, mf, yf, iji, ed, area.est.topo)
UA.avail.group.1 <- UA.avail.group %>% dplyr::select(CollarID, dev, dRoad, TRI, TPI, open, mf, yf, iji, ed)

# add in mean area.est.topo for groups
UA.avail.group.1$area.est.topo <- NA

UA.avail.group.1$area.est.topo[1] <- mean(c(UA.avail.indiv.1$area.est.topo[UA.avail.indiv.1$CollarID == 37706],
                                            UA.avail.indiv.1$area.est.topo[UA.avail.indiv.1$CollarID == 37709]))

UA.avail.group.1$area.est.topo[2] <- mean(c(UA.avail.indiv.1$area.est.topo[UA.avail.indiv.1$CollarID == 37710],
                                            UA.avail.indiv.1$area.est.topo[UA.avail.indiv.1$CollarID == 37714]))

UA.avail.all <- rbind(UA.avail.indiv.1, UA.avail.group.1)

names(UA.avail.all)[1] <- "GroupID"

# AW
AW.avail.indiv <- read.csv("G:/Elk project/Data analysis/Home ranges/ADKE/AW_HRs.csv")
AW.avail.group <- read.csv("G:/Elk project/Data analysis/Home ranges/ADKE/AW_groups.csv")

AW.avail.indiv.1 <- AW.avail.indiv %>% dplyr::select(CollarID, dev, dRoad, TRI, TPI, open, mf, yf, iji, ed, area.est.topo)
AW.avail.group.1 <- AW.avail.group %>% dplyr::select(CollarID, dev, dRoad, TRI, TPI, open, mf, yf, iji, ed)

# add in mean area.est.topo for groups
AW.avail.group.1$area.est.topo <- NA

AW.avail.group.1$area.est.topo[1] <- mean(c(AW.avail.indiv.1$area.est.topo[AW.avail.indiv.1$CollarID == 37706],
                                            AW.avail.indiv.1$area.est.topo[AW.avail.indiv.1$CollarID == 37709],
                                            AW.avail.indiv.1$area.est.topo[AW.avail.indiv.1$CollarID == 37718],
                                            AW.avail.indiv.1$area.est.topo[AW.avail.indiv.1$CollarID == 37723]))

AW.avail.group.1$area.est.topo[2] <- mean(c(AW.avail.indiv.1$area.est.topo[AW.avail.indiv.1$CollarID == 37710],
                                            AW.avail.indiv.1$area.est.topo[AW.avail.indiv.1$CollarID == 37719],
                                            AW.avail.indiv.1$area.est.topo[AW.avail.indiv.1$CollarID == 37722]))

AW.avail.group.1$area.est.topo[3] <- mean(c(AW.avail.indiv.1$area.est.topo[AW.avail.indiv.1$CollarID == 45505],
                                            AW.avail.indiv.1$area.est.topo[AW.avail.indiv.1$CollarID == 45508],
                                            AW.avail.indiv.1$area.est.topo[AW.avail.indiv.1$CollarID == 46393]))

AW.avail.group.1$area.est.topo[4] <- mean(c(AW.avail.indiv.1$area.est.topo[AW.avail.indiv.1$CollarID == 45507],
                                            AW.avail.indiv.1$area.est.topo[AW.avail.indiv.1$CollarID == 46400]))

AW.avail.all <- rbind(AW.avail.indiv.1, AW.avail.group.1)

names(AW.avail.all)[1] <- "GroupID"

#_____________________________________________________________________________________________________________
# 3. Join slopes and availabilities together ----
#_____________________________________________________________________________________________________________

# WS
names(WS.slopes)[1] <- "GroupID"

WS.join <- merge(WS.slopes, WS.avail.all, by = "GroupID", all.x = TRUE)

# SU
names(SU.slopes)[1] <- "GroupID"

SU.join <- merge(SU.slopes, SU.avail.all, by = "GroupID", all.x = TRUE)

# UA
names(UA.slopes)[1] <- "GroupID"

UA.join <- merge(UA.slopes, UA.avail.all, by = "GroupID", all.x = TRUE)

# AW
names(AW.slopes)[1] <- "GroupID"

AW.join <- merge(AW.slopes, AW.avail.all, by = "GroupID", all.x = TRUE)

#_____________________________________________________________________________________________________________
# 4. Home range characteristics PCA per season ----
#_____________________________________________________________________________________________________________
# 4a. WS ----
#_____________________________________________________________________________________________________________

# add row labels
row.names(WS.join) <- WS.join$GroupID

WS.pca <- prcomp(WS.join[ ,c(10:19)], scale = TRUE)

WS.pca$rotation

WS.pca.var <- get_pca_var(WS.pca)

# scree plot
fviz_eig(WS.pca)

# variable biplot
fviz_pca_var(WS.pca, repel = TRUE)

# individuals
WS.ind <- get_pca_ind(WS.pca)

fviz_pca_ind(WS.pca)

# add first two PCs to FR sheet
WS.join$PC1 <- WS.ind$coord[ ,1]
WS.join$PC2 <- WS.ind$coord[ ,2]

#_____________________________________________________________________________________________________________
# 4b. SU ----
#_____________________________________________________________________________________________________________

# add row labels
row.names(SU.join) <- SU.join$GroupID

SU.pca <- prcomp(SU.join[ ,c(10:19)], scale = TRUE)

SU.pca$rotation

SU.pca.var <- get_pca_var(SU.pca)

# scree plot
fviz_eig(SU.pca)

# variable biplot
fviz_pca_var(SU.pca, repel = TRUE)

# individuals
SU.ind <- get_pca_ind(SU.pca)

fviz_pca_ind(SU.pca)

# add first two PCs to FR sheet
SU.join$PC1 <- SU.ind$coord[ ,1]
SU.join$PC2 <- SU.ind$coord[ ,2]

#_____________________________________________________________________________________________________________
# 4c. UA ----
#_____________________________________________________________________________________________________________

# add row labels
row.names(UA.join) <- UA.join$GroupID

UA.pca <- prcomp(UA.join[ ,c(10:19)], scale = TRUE)

UA.pca$rotation

UA.pca.var <- get_pca_var(UA.pca)

# scree plot
fviz_eig(UA.pca)

# variable biplot
fviz_pca_var(UA.pca, repel = TRUE)

# individuals
UA.ind <- get_pca_ind(UA.pca)

fviz_pca_ind(UA.pca)

# add first two PCs to FR sheet
UA.join$PC1 <- UA.ind$coord[ ,1]
UA.join$PC2 <- UA.ind$coord[ ,2]

#_____________________________________________________________________________________________________________
# 4a. AW ----
#_____________________________________________________________________________________________________________

# add row labels
row.names(AW.join) <- AW.join$GroupID

AW.pca <- prcomp(AW.join[ ,c(10:19)], scale = TRUE)

AW.pca$rotation

AW.pca.var <- get_pca_var(AW.pca)

# scree plot
fviz_eig(AW.pca)

# variable biplot
fviz_pca_var(AW.pca, repel = TRUE)

# individuals
AW.ind <- get_pca_ind(AW.pca)

fviz_pca_ind(AW.pca)

# add first two PCs to FR sheet
AW.join$PC1 <- AW.ind$coord[ ,1]
AW.join$PC2 <- AW.ind$coord[ ,2]

#_____________________________________________________________________________________________________________
# 7. Write joined df to csvs ----
#_____________________________________________________________________________________________________________

write.csv(WS.join, "WS_fr.csv")
write.csv(SU.join, "SU_fr.csv")
write.csv(UA.join, "UA_fr.csv")
write.csv(AW.join, "AW_fr.csv")
