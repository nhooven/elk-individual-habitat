# Title: Individual variation in habitat selection
# Subtitle: 4b - Functional response models
# Author: Nathan D. Hooven
# Email: nathan.d.hooven@gmail.com
# Affiliation: Department of Forestry and Natural Resources, University of Kentucky
# Date began: 5 Jul 2022
# Date completed: 6 Jan 2022
# Date modified: 22 Mar 2022
# R version: 3.6.2

#_____________________________________________________________________________________________________________
# 1. Load in required packages ----
#_____________________________________________________________________________________________________________

library(tidyverse)
library(AICcmodavg)    # model selection
library(MuMIn)
library(mgcv)          # GAMs
library(splines)       # splines
library(tidymv)
library(cowplot)       # multiple plots
library(mefa4)

#_____________________________________________________________________________________________________________
# 2. Read in data ----
#_____________________________________________________________________________________________________________

WS.fr <- read.csv("WS_fr.csv")
SU.fr <- read.csv("SU_fr.csv")
UA.fr <- read.csv("UA_fr.csv")
AW.fr <- read.csv("AW_fr.csv")

#_____________________________________________________________________________________________________________
# 3. WS functional response model selection ----

# remove 101955 and 101965
WS.fr <- WS.fr %>% filter(GroupID %notin% c(101955, 101965))

# list to hold top models
WS.models <- list()

#_____________________________________________________________________________________________________________
# 3a. dDeveloped ----
#_____________________________________________________________________________________________________________

WS.dDeveloped <- list()

WS.dDeveloped[[1]] <- gam(dDeveloped ~ s(dev, bs = "cs"), data = WS.fr, method = "REML")

WS.dDeveloped[[2]] <- gam(dDeveloped ~ s(iji, bs = "cs"), data = WS.fr, method = "REML")

WS.dDeveloped[[3]] <- gam(dDeveloped ~ s(ed, bs = "cs"), data = WS.fr, method = "REML")

gam.check(WS.dDeveloped[[1]])
gam.check(WS.dDeveloped[[2]])
gam.check(WS.dDeveloped[[3]])

model.sel(WS.dDeveloped)

write.table(model.sel(WS.dDeveloped), "clipboard", sep = "\t")

ggplot(WS.fr, aes(x = ed, y = dDeveloped)) +
  theme_bw() +
  stat_smooth(method = "gam") +
  geom_point() +
  geom_hline(yintercept = 0)

summary(WS.dDeveloped[[3]])

WS.models[[1]] <- WS.dDeveloped[[3]]

#_____________________________________________________________________________________________________________
# 3b. dRoad.P ----
#_____________________________________________________________________________________________________________

WS.dRoad <- list()

WS.dRoad[[1]] <- gam(dRoad.P ~ s(dRoad, bs = "cs"), data = WS.fr, method = "REML")

WS.dRoad[[2]] <- gam(dRoad.P ~ s(iji, bs = "cs"), data = WS.fr, method = "REML")

WS.dRoad[[3]] <- gam(dRoad.P ~ s(ed, bs = "cs"), data = WS.fr, method = "REML")

gam.check(WS.dRoad[[1]])
gam.check(WS.dRoad[[2]])
gam.check(WS.dRoad[[3]])

model.sel(WS.dRoad)

write.table(model.sel(WS.dRoad), "clipboard", sep = "\t")

ggplot(WS.fr, aes(x = dRoad, y = dRoad.P)) +
  theme_bw() +
  stat_smooth(method = "gam") +
  geom_point() +
  geom_hline(yintercept = 0)

summary(WS.dRoad[[1]])

WS.models[[2]] <- WS.dRoad[[1]]

#_____________________________________________________________________________________________________________
# 3c. TRI ----
#_____________________________________________________________________________________________________________

WS.TRI <- list()

WS.TRI[[1]] <- gam(TRI.x ~ s(TRI.y, bs = "cs"), data = WS.fr, method = "REML")

WS.TRI[[2]] <- gam(TRI.x ~ s(iji, bs = "cs"), data = WS.fr, method = "REML")

WS.TRI[[3]] <- gam(TRI.x ~ s(ed, bs = "cs"), data = WS.fr, method = "REML")

gam.check(WS.TRI[[1]])
gam.check(WS.TRI[[2]])
gam.check(WS.TRI[[3]])

model.sel(WS.TRI)

write.table(model.sel(WS.TRI), "clipboard", sep = "\t")

ggplot(WS.fr, aes(x = ed, y = TRI.x)) +
  theme_bw() +
  stat_smooth(method = "gam") +
  geom_point() +
  geom_hline(yintercept = 0)

summary(WS.TRI[[3]])

WS.models[[3]] <- WS.TRI[[3]]

#_____________________________________________________________________________________________________________
# 3d. TPI ----
#_____________________________________________________________________________________________________________

WS.TPI <- list()

WS.TPI[[1]] <- gam(TPI.x ~ s(TPI.y, bs = "cs"), data = WS.fr, method = "REML")

WS.TPI[[2]] <- gam(TPI.x ~ s(iji, bs = "cs"), data = WS.fr, method = "REML")

WS.TPI[[3]] <- gam(TPI.x ~ s(ed, bs = "cs"), data = WS.fr, method = "REML")

gam.check(WS.TPI[[1]])
gam.check(WS.TPI[[2]])
gam.check(WS.TPI[[3]])

model.sel(WS.TPI)

write.table(model.sel(WS.TPI), "clipboard", sep = "\t")

ggplot(WS.fr, aes(x = ed, y = TPI.x)) +
  theme_bw() +
  stat_smooth(method = "gam") +
  geom_point() +
  geom_hline(yintercept = 0)

summary(WS.TPI[[3]])

WS.models[[4]] <- WS.TPI[[3]]

#_____________________________________________________________________________________________________________
# 3e. dOpen ----
#_____________________________________________________________________________________________________________

WS.dOpen <- list()

WS.dOpen[[1]] <- gam(dOpen ~ s(open, bs = "cs"), data = WS.fr, method = "REML")

WS.dOpen[[2]] <- gam(dOpen ~ s(iji, bs = "cs"), data = WS.fr, method = "REML")

WS.dOpen[[3]] <- gam(dOpen ~ s(ed, bs = "cs"), data = WS.fr, method = "REML")

gam.check(WS.dOpen[[1]])
gam.check(WS.dOpen[[2]])
gam.check(WS.dOpen[[3]])

model.sel(WS.dOpen)

write.table(model.sel(WS.dOpen), "clipboard", sep = "\t")

ggplot(WS.fr, aes(x = ed, y = dOpen)) +
  theme_bw() +
  stat_smooth(method = "gam") +
  geom_point() +
  geom_hline(yintercept = 0)

summary(WS.dOpen[[3]])

WS.models[[5]] <- WS.dOpen[[3]]

#_____________________________________________________________________________________________________________
# 3f. dMatureForest ----
#_____________________________________________________________________________________________________________

WS.dMatureForest <- list()

WS.dMatureForest[[1]] <- gam(dMatureForest ~ s(mf, bs = "cs"), data = WS.fr, method = "REML")

WS.dMatureForest[[2]] <- gam(dMatureForest ~ s(iji, bs = "cs"), data = WS.fr, method = "REML")

WS.dMatureForest[[3]] <- gam(dMatureForest ~ s(ed, bs = "cs"), data = WS.fr, method = "REML")

gam.check(WS.dMatureForest[[1]])
gam.check(WS.dMatureForest[[2]])
gam.check(WS.dMatureForest[[3]])

model.sel(WS.dMatureForest)

write.table(model.sel(WS.dMatureForest), "clipboard", sep = "\t")

ggplot(WS.fr, aes(x = mf, y = dMatureForest)) +
  theme_bw() +
  stat_smooth(method = "gam") +
  geom_point() +
  geom_hline(yintercept = 0)

summary(WS.dMatureForest[[1]])

WS.models[[6]] <- WS.dMatureForest[[1]]

#_____________________________________________________________________________________________________________
# 3g. dYoungForest ----
#_____________________________________________________________________________________________________________

WS.dYoungForest <- list()

WS.dYoungForest[[1]] <- gam(dYoungForest ~ s(yf, bs = "cs"), data = WS.fr, method = "REML")

WS.dYoungForest[[2]] <- gam(dYoungForest ~ s(iji, bs = "cs"), data = WS.fr, method = "REML")

WS.dYoungForest[[3]] <- gam(dYoungForest ~ s(ed, bs = "cs"), data = WS.fr, method = "REML")

gam.check(WS.dYoungForest[[1]])
gam.check(WS.dYoungForest[[2]])
gam.check(WS.dYoungForest[[3]])

model.sel(WS.dYoungForest)

write.table(model.sel(WS.dYoungForest), "clipboard", sep = "\t")

ggplot(WS.fr, aes(x = yf, y = dYoungForest)) +
  theme_bw() +
  stat_smooth(method = "gam") +
  geom_point() +
  geom_hline(yintercept = 0)

summary(WS.dYoungForest[[3]])

WS.models[[7]] <- WS.dYoungForest[[1]]

#_____________________________________________________________________________________________________________
# 4. SU functional response model selection ----

# list to hold top models
SU.models <- list()

#_____________________________________________________________________________________________________________
# 4a. dDeveloped ----
#_____________________________________________________________________________________________________________

SU.dDeveloped <- list()

SU.dDeveloped[[1]] <- gam(dDeveloped ~ s(dev, bs = "cs"), data = SU.fr, method = "REML")

SU.dDeveloped[[2]] <- gam(dDeveloped ~ s(iji, bs = "cs"), data = SU.fr, method = "REML")

SU.dDeveloped[[3]] <- gam(dDeveloped ~ s(ed, bs = "cs"), data = SU.fr, method = "REML")

gam.check(SU.dDeveloped[[1]])
gam.check(SU.dDeveloped[[2]])
gam.check(SU.dDeveloped[[3]])

model.sel(SU.dDeveloped)

write.table(model.sel(SU.dDeveloped), "clipboard", sep = "\t")

ggplot(SU.fr, aes(x = iji, y = dDeveloped)) +
  theme_bw() +
  stat_smooth(method = "gam") +
  geom_point() +
  geom_hline(yintercept = 0)

summary(SU.dDeveloped[[2]])

SU.models[[1]] <- SU.dDeveloped[[2]]

#_____________________________________________________________________________________________________________
# 4b. dRoad.P ----
#_____________________________________________________________________________________________________________

SU.dRoad <- list()

SU.dRoad[[1]] <- gam(dRoad.P ~ s(dRoad, bs = "cs"), data = SU.fr, method = "REML")

SU.dRoad[[2]] <- gam(dRoad.P ~ s(iji, bs = "cs"), data = SU.fr, method = "REML")

SU.dRoad[[3]] <- gam(dRoad.P ~ s(ed, bs = "cs"), data = SU.fr, method = "REML")

gam.check(SU.dRoad[[1]])
gam.check(SU.dRoad[[2]])
gam.check(SU.dRoad[[3]])

model.sel(SU.dRoad)

write.table(model.sel(SU.dRoad), "clipboard", sep = "\t")

ggplot(SU.fr, aes(x = ed, y = dRoad.P)) +
  theme_bw() +
  stat_smooth(method = "gam") +
  geom_point() +
  geom_hline(yintercept = 0)

summary(SU.dRoad[[2]])

SU.models[[2]] <- SU.dRoad[[3]]

#_____________________________________________________________________________________________________________
# 4c. TRI ----
#_____________________________________________________________________________________________________________

SU.TRI <- list()

SU.TRI[[1]] <- gam(TRI.x ~ s(TRI.y, bs = "cs"), data = SU.fr, method = "REML")

SU.TRI[[2]] <- gam(TRI.x ~ s(iji, bs = "cs"), data = SU.fr, method = "REML")

SU.TRI[[3]] <- gam(TRI.x ~ s(ed, bs = "cs"), data = SU.fr, method = "REML")

gam.check(SU.TRI[[1]])
gam.check(SU.TRI[[2]])
gam.check(SU.TRI[[3]])

model.sel(SU.TRI)

write.table(model.sel(SU.TRI), "clipboard", sep = "\t")

ggplot(SU.fr, aes(x = ed, y = TRI.x)) +
  theme_bw() +
  stat_smooth(method = "gam") +
  geom_point() +
  geom_hline(yintercept = 0)

summary(SU.TRI[[3]])

SU.models[[3]] <- SU.TRI[[3]]

#_____________________________________________________________________________________________________________
# 4d. TPI ----
#_____________________________________________________________________________________________________________

SU.TPI <- list()

SU.TPI[[1]] <- gam(TPI.x ~ s(TPI.y, bs = "cs"), data = SU.fr, method = "REML")

SU.TPI[[2]] <- gam(TPI.x ~ s(iji, bs = "cs"), data = SU.fr, method = "REML")

SU.TPI[[3]] <- gam(TPI.x ~ s(ed, bs = "cs"), data = SU.fr, method = "REML")

gam.check(SU.TPI[[1]])
gam.check(SU.TPI[[2]])
gam.check(SU.TPI[[3]])

model.sel(SU.TPI)

write.table(model.sel(SU.TPI), "clipboard", sep = "\t")

ggplot(SU.fr, aes(x = TPI.y, y = TPI.x)) +
  theme_bw() +
  stat_smooth(method = "gam") +
  geom_point() +
  geom_hline(yintercept = 0)

summary(SU.TPI[[1]])

SU.models[[4]] <- SU.TPI[[1]]

#_____________________________________________________________________________________________________________
# 4e. dOpen ----
#_____________________________________________________________________________________________________________

SU.dOpen <- list()

SU.dOpen[[1]] <- gam(dOpen ~ s(open, bs = "cs"), data = SU.fr, method = "REML")

SU.dOpen[[2]] <- gam(dOpen ~ s(iji, bs = "cs"), data = SU.fr, method = "REML")

SU.dOpen[[3]] <- gam(dOpen ~ s(ed, bs = "cs"), data = SU.fr, method = "REML")

gam.check(SU.dOpen[[1]])
gam.check(SU.dOpen[[2]])
gam.check(SU.dOpen[[3]])

model.sel(SU.dOpen)

write.table(model.sel(SU.dOpen), "clipboard", sep = "\t")

ggplot(SU.fr, aes(x = open, y = dOpen)) +
  theme_bw() +
  stat_smooth(method = "gam") +
  geom_point() +
  geom_hline(yintercept = 0)

summary(SU.dOpen[[2]])

SU.models[[5]] <- SU.dOpen[[1]]

#_____________________________________________________________________________________________________________
# 4f. dMatureForest ----
#_____________________________________________________________________________________________________________

SU.dMatureForest <- list()

SU.dMatureForest[[1]] <- gam(dMatureForest ~ s(mf, bs = "cs"), data = SU.fr, method = "REML")

SU.dMatureForest[[2]] <- gam(dMatureForest ~ s(iji, bs = "cs"), data = SU.fr, method = "REML")

SU.dMatureForest[[3]] <- gam(dMatureForest ~ s(ed, bs = "cs"), data = SU.fr, method = "REML")

gam.check(SU.dMatureForest[[1]])
gam.check(SU.dMatureForest[[2]])
gam.check(SU.dMatureForest[[3]])

model.sel(SU.dMatureForest)

write.table(model.sel(SU.dMatureForest), "clipboard", sep = "\t")

ggplot(SU.fr, aes(x = mf, y = dMatureForest)) +
  theme_bw() +
  stat_smooth(method = "gam") +
  geom_point() +
  geom_hline(yintercept = 0)

summary(SU.dMatureForest[[3]])

SU.models[[6]] <- SU.dMatureForest[[1]]

#_____________________________________________________________________________________________________________
# 4g. dYoungForest ----
#_____________________________________________________________________________________________________________

SU.dYoungForest <- list()

SU.dYoungForest[[1]] <- gam(dYoungForest ~ s(yf, bs = "cs"), data = SU.fr, method = "REML")

SU.dYoungForest[[2]] <- gam(dYoungForest ~ s(iji, bs = "cs"), data = SU.fr, method = "REML")

SU.dYoungForest[[3]] <- gam(dYoungForest ~ s(ed, bs = "cs"), data = SU.fr, method = "REML")

gam.check(SU.dYoungForest[[1]])
gam.check(SU.dYoungForest[[2]])
gam.check(SU.dYoungForest[[3]])

model.sel(SU.dYoungForest)

write.table(model.sel(SU.dYoungForest), "clipboard", sep = "\t")

ggplot(SU.fr, aes(x = yf, y = dYoungForest)) +
  theme_bw() +
  stat_smooth(method = "gam") +
  geom_point() +
  geom_hline(yintercept = 0)

summary(SU.dYoungForest[[2]])

SU.models[[7]] <- SU.dYoungForest[[1]]

#_____________________________________________________________________________________________________________
# 5. UA functional response model selection ----

# list to hold top models
UA.models <- list()

#_____________________________________________________________________________________________________________
# 5a. dDeveloped ----
#_____________________________________________________________________________________________________________

UA.dDeveloped <- list()

UA.dDeveloped[[1]] <- gam(dDeveloped ~ s(dev, bs = "cs"), data = UA.fr, method = "REML")

UA.dDeveloped[[2]] <- gam(dDeveloped ~ s(iji, bs = "cs"), data = UA.fr, method = "REML")

UA.dDeveloped[[3]] <- gam(dDeveloped ~ s(ed, bs = "cs"), data = UA.fr, method = "REML")

gam.check(UA.dDeveloped[[1]])
gam.check(UA.dDeveloped[[2]])
gam.check(UA.dDeveloped[[3]])

model.sel(UA.dDeveloped)

write.table(model.sel(UA.dDeveloped), "clipboard", sep = "\t")

ggplot(UA.fr, aes(x = dev, y = dDeveloped)) +
  theme_bw() +
  stat_smooth(method = "gam") +
  geom_point() +
  geom_hline(yintercept = 0)

summary(UA.dDeveloped[[1]])

UA.models[[1]] <- UA.dDeveloped[[1]]

#_____________________________________________________________________________________________________________
# 5b. dRoad.P ----
#_____________________________________________________________________________________________________________

UA.dRoad <- list()

UA.dRoad[[1]] <- gam(dRoad.P ~ s(dRoad, bs = "cs"), data = UA.fr, method = "REML")

UA.dRoad[[2]] <- gam(dRoad.P ~ s(iji, bs = "cs"), data = UA.fr, method = "REML")

UA.dRoad[[3]] <- gam(dRoad.P ~ s(ed, bs = "cs"), data = UA.fr, method = "REML")

gam.check(UA.dRoad[[1]])
gam.check(UA.dRoad[[2]])
gam.check(UA.dRoad[[3]])

model.sel(UA.dRoad)

write.table(model.sel(UA.dRoad), "clipboard", sep = "\t")

ggplot(UA.fr, aes(x = iji, y = dRoad.P)) +
  theme_bw() +
  stat_smooth(method = "gam") +
  geom_point() +
  geom_hline(yintercept = 0)

summary(UA.dRoad[[2]])

UA.models[[2]] <- UA.dRoad[[2]]

#_____________________________________________________________________________________________________________
# 5c. TRI ----
#_____________________________________________________________________________________________________________

UA.TRI <- list()

UA.TRI[[1]] <- gam(TRI.x ~ s(TRI.y, bs = "cs"), data = UA.fr, method = "REML")

UA.TRI[[2]] <- gam(TRI.x ~ s(iji, bs = "cs"), data = UA.fr, method = "REML")

UA.TRI[[3]] <- gam(TRI.x ~ s(ed, bs = "cs"), data = UA.fr, method = "REML")

gam.check(UA.TRI[[1]])
gam.check(UA.TRI[[2]])
gam.check(UA.TRI[[3]])

model.sel(UA.TRI)

write.table(model.sel(UA.TRI), "clipboard", sep = "\t")

ggplot(UA.fr, aes(x = ed, y = TRI.x)) +
  theme_bw() +
  stat_smooth(method = "gam") +
  geom_point() +
  geom_hline(yintercept = 0)

summary(UA.TRI[[3]])

UA.models[[3]] <- UA.TRI[[3]]

#_____________________________________________________________________________________________________________
# 5d. TPI ----
#_____________________________________________________________________________________________________________

UA.TPI <- list()

UA.TPI[[1]] <- gam(TPI.x ~ s(TPI.y, bs = "cs"), data = UA.fr, method = "REML")

UA.TPI[[2]] <- gam(TPI.x ~ s(iji, bs = "cs"), data = UA.fr, method = "REML")

UA.TPI[[3]] <- gam(TPI.x ~ s(ed, bs = "cs"), data = UA.fr, method = "REML")

gam.check(UA.TPI[[1]])
gam.check(UA.TPI[[2]])
gam.check(UA.TPI[[3]])

model.sel(UA.TPI)

write.table(model.sel(UA.TPI), "clipboard", sep = "\t")

ggplot(UA.fr, aes(x = iji, y = TPI.x)) +
  theme_bw() +
  stat_smooth(method = "gam") +
  geom_point() +
  geom_hline(yintercept = 0)

summary(UA.TPI[[2]])

UA.models[[4]] <- UA.TPI[[2]]

#_____________________________________________________________________________________________________________
# 5e. dOpen ----
#_____________________________________________________________________________________________________________

UA.dOpen <- list()

UA.dOpen[[1]] <- gam(dOpen ~ s(open, bs = "cs"), data = UA.fr, method = "REML")

UA.dOpen[[2]] <- gam(dOpen ~ s(iji, bs = "cs"), data = UA.fr, method = "REML")

UA.dOpen[[3]] <- gam(dOpen ~ s(ed, bs = "cs"), data = UA.fr, method = "REML")

gam.check(UA.dOpen[[1]])
gam.check(UA.dOpen[[2]])
gam.check(UA.dOpen[[3]])

model.sel(UA.dOpen)

write.table(model.sel(UA.dOpen), "clipboard", sep = "\t")

ggplot(UA.fr, aes(x = ed, y = dOpen)) +
  theme_bw() +
  stat_smooth(method = "gam") +
  geom_point() +
  geom_hline(yintercept = 0)

summary(UA.dOpen[[3]])

UA.models[[5]] <- UA.dOpen[[3]]

#_____________________________________________________________________________________________________________
# 5f. dMatureForest ----
#_____________________________________________________________________________________________________________

UA.dMatureForest <- list()

UA.dMatureForest[[1]] <- gam(dMatureForest ~ s(mf, bs = "cs"), data = UA.fr, method = "REML")

UA.dMatureForest[[2]] <- gam(dMatureForest ~ s(iji, bs = "cs"), data = UA.fr, method = "REML")

UA.dMatureForest[[3]] <- gam(dMatureForest ~ s(ed, bs = "cs"), data = UA.fr, method = "REML")

gam.check(UA.dMatureForest[[1]])
gam.check(UA.dMatureForest[[2]])
gam.check(UA.dMatureForest[[3]])

model.sel(UA.dMatureForest)

write.table(model.sel(UA.dMatureForest), "clipboard", sep = "\t")

ggplot(UA.fr, aes(x = ed, y = dMatureForest)) +
  theme_bw() +
  stat_smooth(method = "gam") +
  geom_point() +
  geom_hline(yintercept = 0)

summary(UA.dMatureForest[[3]])

UA.models[[6]] <- UA.dMatureForest[[3]]

#_____________________________________________________________________________________________________________
# 5g. dYoungForest ----
#_____________________________________________________________________________________________________________

UA.dYoungForest <- list()

UA.dYoungForest[[1]] <- gam(dYoungForest ~ s(yf, bs = "cs"), data = UA.fr, method = "REML")

UA.dYoungForest[[2]] <- gam(dYoungForest ~ s(iji, bs = "cs"), data = UA.fr, method = "REML")

UA.dYoungForest[[3]] <- gam(dYoungForest ~ s(ed, bs = "cs"), data = UA.fr, method = "REML")

gam.check(UA.dYoungForest[[1]])
gam.check(UA.dYoungForest[[2]])
gam.check(UA.dYoungForest[[3]])

model.sel(UA.dYoungForest)

write.table(model.sel(UA.dYoungForest), "clipboard", sep = "\t")

ggplot(UA.fr, aes(x = yf, y = dYoungForest)) +
  theme_bw() +
  stat_smooth(method = "gam") +
  geom_point() +
  geom_hline(yintercept = 0)

summary(UA.dYoungForest[[1]])

UA.models[[7]] <- UA.dYoungForest[[1]]

#_____________________________________________________________________________________________________________
# 6. AW functional response model selection ----

# list to hold top models
AW.models <- list()

#_____________________________________________________________________________________________________________
# 6a. dDeveloped ----
#_____________________________________________________________________________________________________________

AW.dDeveloped <- list()

AW.dDeveloped[[1]] <- gam(dDeveloped ~ s(dev, bs = "cs"), data = AW.fr, method = "REML")

AW.dDeveloped[[2]] <- gam(dDeveloped ~ s(iji, bs = "cs"), data = AW.fr, method = "REML")

AW.dDeveloped[[3]] <- gam(dDeveloped ~ s(ed, bs = "cs"), data = AW.fr, method = "REML")

gam.check(AW.dDeveloped[[1]])
gam.check(AW.dDeveloped[[2]])
gam.check(AW.dDeveloped[[3]])

model.sel(AW.dDeveloped)

write.table(model.sel(AW.dDeveloped), "clipboard", sep = "\t")

ggplot(AW.fr, aes(x = dev, y = dDeveloped)) +
  theme_bw() +
  stat_smooth(method = "gam") +
  geom_point() +
  geom_hline(yintercept = 0)

summary(AW.dDeveloped[[1]])

AW.models[[1]] <- AW.dDeveloped[[1]]

#_____________________________________________________________________________________________________________
# 6b. dRoad.P ----
#_____________________________________________________________________________________________________________

AW.dRoad <- list()

AW.dRoad[[1]] <- gam(dRoad.P ~ s(dRoad, bs = "cs"), data = AW.fr, method = "REML")

AW.dRoad[[2]] <- gam(dRoad.P ~ s(iji, bs = "cs"), data = AW.fr, method = "REML")

AW.dRoad[[3]] <- gam(dRoad.P ~ s(ed, bs = "cs"), data = AW.fr, method = "REML")

gam.check(AW.dRoad[[1]])
gam.check(AW.dRoad[[2]])
gam.check(AW.dRoad[[3]])

model.sel(AW.dRoad)

write.table(model.sel(AW.dRoad), "clipboard", sep = "\t")

ggplot(AW.fr, aes(x = dRoad, y = dRoad.P)) +
  theme_bw() +
  stat_smooth(method = "gam") +
  geom_point() +
  geom_hline(yintercept = 0)

summary(AW.dRoad[[1]])

AW.models[[2]] <- AW.dRoad[[1]]

#_____________________________________________________________________________________________________________
# 6c. TRI ----
#_____________________________________________________________________________________________________________

AW.TRI <- list()

AW.TRI[[1]] <- gam(TRI.x ~ s(TRI.y, bs = "cs"), data = AW.fr, method = "REML")

AW.TRI[[2]] <- gam(TRI.x ~ s(iji, bs = "cs"), data = AW.fr, method = "REML")

AW.TRI[[3]] <- gam(TRI.x ~ s(ed, bs = "cs"), data = AW.fr, method = "REML")

gam.check(AW.TRI[[1]])
gam.check(AW.TRI[[2]])
gam.check(AW.TRI[[3]])

model.sel(AW.TRI)

write.table(model.sel(AW.TRI), "clipboard", sep = "\t")

ggplot(AW.fr, aes(x = ed, y = TRI.x)) +
  theme_bw() +
  stat_smooth(method = "gam") +
  geom_point() +
  geom_hline(yintercept = 0)

summary(AW.TRI[[3]])

AW.models[[3]] <- AW.TRI[[3]]

#_____________________________________________________________________________________________________________
# 6d. TPI ----
#_____________________________________________________________________________________________________________

AW.TPI <- list()

AW.TPI[[1]] <- gam(TPI.x ~ s(TPI.y, bs = "cs"), data = AW.fr, method = "REML")

AW.TPI[[2]] <- gam(TPI.x ~ s(iji, bs = "cs"), data = AW.fr, method = "REML")

AW.TPI[[3]] <- gam(TPI.x ~ s(ed, bs = "cs"), data = AW.fr, method = "REML")

gam.check(AW.TPI[[1]])
gam.check(AW.TPI[[2]])
gam.check(AW.TPI[[3]])

model.sel(AW.TPI)

write.table(model.sel(AW.TPI), "clipboard", sep = "\t")

ggplot(AW.fr, aes(x = ed, y = TPI.x)) +
  theme_bw() +
  stat_smooth(method = "gam") +
  geom_point() +
  geom_hline(yintercept = 0)

summary(AW.TPI[[3]])

AW.models[[4]] <- AW.TPI[[3]]

#_____________________________________________________________________________________________________________
# 6e. dOpen ----
#_____________________________________________________________________________________________________________

AW.dOpen <- list()

AW.dOpen[[1]] <- gam(dOpen ~ s(open, bs = "cs"), data = AW.fr, method = "REML")

AW.dOpen[[2]] <- gam(dOpen ~ s(iji, bs = "cs"), data = AW.fr, method = "REML")

AW.dOpen[[3]] <- gam(dOpen ~ s(ed, bs = "cs"), data = AW.fr, method = "REML")

gam.check(AW.dOpen[[1]])
gam.check(AW.dOpen[[2]])
gam.check(AW.dOpen[[3]])

model.sel(AW.dOpen)

write.table(model.sel(AW.dOpen), "clipboard", sep = "\t")

ggplot(AW.fr, aes(x = ed, y = dOpen)) +
  theme_bw() +
  stat_smooth(method = "gam") +
  geom_point() +
  geom_hline(yintercept = 0)

summary(AW.dOpen[[3]])

AW.models[[5]] <- AW.dOpen[[3]]

#_____________________________________________________________________________________________________________
# 6f. dMatureForest ----
#_____________________________________________________________________________________________________________

AW.dMatureForest <- list()

AW.dMatureForest[[1]] <- gam(dMatureForest ~ s(mf, bs = "cs"), data = AW.fr, method = "REML")

AW.dMatureForest[[2]] <- gam(dMatureForest ~ s(iji, bs = "cs"), data = AW.fr, method = "REML")

AW.dMatureForest[[3]] <- gam(dMatureForest ~ s(ed, bs = "cs"), data = AW.fr, method = "REML")

gam.check(AW.dMatureForest[[1]])
gam.check(AW.dMatureForest[[2]])
gam.check(AW.dMatureForest[[3]])

model.sel(AW.dMatureForest)

write.table(model.sel(AW.dMatureForest), "clipboard", sep = "\t")

ggplot(AW.fr, aes(x = mf, y = dMatureForest)) +
  theme_bw() +
  stat_smooth(method = "gam") +
  geom_point() +
  geom_hline(yintercept = 0)

summary(AW.dMatureForest[[1]])

AW.models[[6]] <- AW.dMatureForest[[1]]

#_____________________________________________________________________________________________________________
# 6g. dYoungForest ----
#_____________________________________________________________________________________________________________

AW.dYoungForest <- list()

AW.dYoungForest[[1]] <- gam(dYoungForest ~ s(yf, bs = "cs"), data = AW.fr, method = "REML")

AW.dYoungForest[[2]] <- gam(dYoungForest ~ s(iji, bs = "cs"), data = AW.fr, method = "REML")

AW.dYoungForest[[3]] <- gam(dYoungForest ~ s(ed, bs = "cs"), data = AW.fr, method = "REML")

gam.check(AW.dYoungForest[[1]])
gam.check(AW.dYoungForest[[2]])
gam.check(AW.dYoungForest[[3]])

model.sel(AW.dYoungForest)

write.table(model.sel(AW.dYoungForest), "clipboard", sep = "\t")

ggplot(AW.fr, aes(x = yf, y = dYoungForest)) +
  theme_bw() +
  stat_smooth(method = "gam") +
  geom_point() +
  geom_hline(yintercept = 0)

summary(AW.dYoungForest[[2]])

AW.models[[7]] <- AW.dYoungForest[[1]]


plot_grid(WS.grid, SU.grid, AW.grid, ncol = 3)

#_____________________________________________________________________________________________________________
# 7. Save models to file ----
#_____________________________________________________________________________________________________________

save.image(file = "fr_models.RData")
