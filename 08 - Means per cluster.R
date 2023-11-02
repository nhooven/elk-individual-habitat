# Title: Individual variation in habitat selection
# Subtitle: 8 - Means per cluster
# Author: Nathan D. Hooven
# Email: nathan.d.hooven@gmail.com
# Affiliation: Department of Forestry and Natural Resources, University of Kentucky
# Date began: 18 Feb 2022
# Date completed: 21 Feb 2022
# Date modified: 24 Mar 2022
# R version: 3.6.2

#_____________________________________________________________________________________________________________
# 1. Load in required packages ----
#_____________________________________________________________________________________________________________

library(tidyverse)

#_____________________________________________________________________________________________________________
# 2. Read in data ----
#_____________________________________________________________________________________________________________

load("distance.RData")

WS.cv <- read.csv("WS_cv_compare.csv")
SU.cv <- read.csv("SU_cv_compare.csv")
UA.cv <- read.csv("UA_cv_compare.csv")
AW.cv <- read.csv("AW_cv_compare.csv")

#_____________________________________________________________________________________________________________
# 3. Bind together ----
#_____________________________________________________________________________________________________________

WS.fr.1 <- inner_join(WS.fr, WS.cv, by = "GroupID")
SU.fr.1 <- inner_join(SU.fr, SU.cv, by = "GroupID")
UA.fr.1 <- inner_join(UA.fr, UA.cv, by = "GroupID")
AW.fr.1 <- inner_join(AW.fr, AW.cv, by = "GroupID")

# create 95% confidence intervals for the mean
CI_95 <- function(x) {
  
  estimate <- mean(x)
  
  # compute upper and lower confidence limits
  # upper
  CI.upp <- round(estimate + 1.96*(sd(x)/sqrt(length(x))), digits = 3)
  
  # lower
  CI.low <- round(estimate - 1.96*(sd(x)/sqrt(length(x))), digits = 3)
  
  # bind together
  CI.both <- paste0("(", CI.low, ", ", CI.upp, ")")
  
  return(CI.both)
  
}

#_____________________________________________________________________________________________________________
# 4. WS mean tables ----
#_____________________________________________________________________________________________________________
# 4a. Selection coefficients ----
#_____________________________________________________________________________________________________________

WS.mean.table.1 <- WS.fr.1 %>% group_by(clust) %>%
                             summarize(number = n(),
                                       dDeveloped.mean = round(mean(dDeveloped), digits = 3),
                                       dDeveloped.ci = CI_95(dDeveloped),
                                       dRoad.P.mean = round(mean(dRoad.P), digits = 3),
                                       dRoad.P.ci = CI_95(dRoad.P),
                                       TRI.x.mean = round(mean(TRI.x), digits = 3),
                                       TRI.x.ci = CI_95(TRI.x),
                                       TPI.x.mean = round(mean(TPI.x), digits = 3),
                                       TPI.x.ci = CI_95(TPI.x),
                                       dOpen.mean = round(mean(dOpen), digits = 3),
                                       dOpen.ci = CI_95(dOpen),
                                       dMatureForest.mean = round(mean(dMatureForest), digits = 3),
                                       dMatureForest.ci = CI_95(dMatureForest),
                                       dYoungForest.mean = round(mean(dYoungForest), digits = 3),
                                       dYoungForest.ci = CI_95(dYoungForest))

write.table(WS.mean.table.1, "clipboard", sep = "\t")

#_____________________________________________________________________________________________________________
# 4b. Home range coefficients ----
#_____________________________________________________________________________________________________________

WS.mean.table.2 <- WS.fr.1 %>% group_by(clust) %>%
                             summarize(number = n(),
                                       dev.mean = round(mean(dev), digits = 3),
                                       dev.ci = CI_95(dev),
                                       dRoad.mean = round(mean(dRoad), digits = 3),
                                       dRoad.ci = CI_95(dRoad),
                                       TRI.y.mean = round(mean(TRI.y), digits = 3),
                                       TRI.y.ci = CI_95(TRI.y),
                                       TPI.y.mean = round(mean(TPI.y), digits = 3),
                                       TPI.y.ci = CI_95(TPI.y),
                                       open.mean = round(mean(open), digits = 3),
                                       open.ci = CI_95(open),
                                       mf.mean = round(mean(mf), digits = 3),
                                       mf.ci = CI_95(mf),
                                       yf.mean = round(mean(yf), digits = 3),
                                       yf.ci = CI_95(yf),
                                       iji.mean = round(mean(iji), digits = 3),
                                       iji.ci = CI_95(iji),
                                       ed.mean = round(mean(ed), digits = 3),
                                       ed.ci = CI_95(ed),
                                       area.est.topo.mean = round(mean(area.est.topo), digits = 3),
                                       area.est.topo.ci = CI_95(area.est.topo))

write.table(WS.mean.table.2, "clipboard", sep = "\t")

#_____________________________________________________________________________________________________________
# 5. SU mean table ----
#_____________________________________________________________________________________________________________
# 5a. Selection coefficients ----
#_____________________________________________________________________________________________________________

SU.mean.table.1 <- SU.fr.1 %>% group_by(clust) %>%
                             summarize(number = n(),
                                       dDeveloped.mean = round(mean(dDeveloped), digits = 3),
                                       dDeveloped.ci = CI_95(dDeveloped),
                                       dRoad.P.mean = round(mean(dRoad.P), digits = 3),
                                       dRoad.P.ci = CI_95(dRoad.P),
                                       TRI.x.mean = round(mean(TRI.x), digits = 3),
                                       TRI.x.ci = CI_95(TRI.x),
                                       TPI.x.mean = round(mean(TPI.x), digits = 3),
                                       TPI.x.ci = CI_95(TPI.x),
                                       dOpen.mean = round(mean(dOpen), digits = 3),
                                       dOpen.ci = CI_95(dOpen),
                                       dMatureForest.mean = round(mean(dMatureForest), digits = 3),
                                       dMatureForest.ci = CI_95(dMatureForest),
                                       dYoungForest.mean = round(mean(dYoungForest), digits = 3),
                                       dYoungForest.ci = CI_95(dYoungForest))

write.table(SU.mean.table.1, "clipboard", sep = "\t")

#_____________________________________________________________________________________________________________
# 5b. Home range coefficients ----
#_____________________________________________________________________________________________________________

SU.mean.table.2 <- SU.fr.1 %>% group_by(clust) %>%
                             summarize(number = n(),
                                       dev.mean = round(mean(dev), digits = 3),
                                       dev.ci = CI_95(dev),
                                       dRoad.mean = round(mean(dRoad), digits = 3),
                                       dRoad.ci = CI_95(dRoad),
                                       TRI.y.mean = round(mean(TRI.y), digits = 3),
                                       TRI.y.ci = CI_95(TRI.y),
                                       TPI.y.mean = round(mean(TPI.y), digits = 3),
                                       TPI.y.ci = CI_95(TPI.y),
                                       open.mean = round(mean(open), digits = 3),
                                       open.ci = CI_95(open),
                                       mf.mean = round(mean(mf), digits = 3),
                                       mf.ci = CI_95(mf),
                                       yf.mean = round(mean(yf), digits = 3),
                                       yf.ci = CI_95(yf),
                                       iji.mean = round(mean(iji), digits = 3),
                                       iji.ci = CI_95(iji),
                                       ed.mean = round(mean(ed), digits = 3),
                                       ed.ci = CI_95(ed),
                                       area.est.topo.mean = round(mean(area.est.topo), digits = 3),
                                       area.est.topo.ci = CI_95(area.est.topo))

write.table(SU.mean.table.2, "clipboard", sep = "\t")

#_____________________________________________________________________________________________________________
# 6. UA mean table ----
#_____________________________________________________________________________________________________________
# 6a. Selection coefficients ----
#_____________________________________________________________________________________________________________

UA.mean.table.1 <- UA.fr.1 %>% group_by(clust) %>%
                             summarize(number = n(),
                                       dDeveloped.mean = round(mean(dDeveloped), digits = 3),
                                       dDeveloped.ci = CI_95(dDeveloped),
                                       dRoad.P.mean = round(mean(dRoad.P), digits = 3),
                                       dRoad.P.ci = CI_95(dRoad.P),
                                       TRI.x.mean = round(mean(TRI.x), digits = 3),
                                       TRI.x.ci = CI_95(TRI.x),
                                       TPI.x.mean = round(mean(TPI.x), digits = 3),
                                       TPI.x.ci = CI_95(TPI.x),
                                       dOpen.mean = round(mean(dOpen), digits = 3),
                                       dOpen.ci = CI_95(dOpen),
                                       dMatureForest.mean = round(mean(dMatureForest), digits = 3),
                                       dMatureForest.ci = CI_95(dMatureForest),
                                       dYoungForest.mean = round(mean(dYoungForest), digits = 3),
                                       dYoungForest.ci = CI_95(dYoungForest))

write.table(UA.mean.table.1, "clipboard", sep = "\t")

#_____________________________________________________________________________________________________________
# 6b. Home range coefficients ----
#_____________________________________________________________________________________________________________

UA.mean.table.2 <- UA.fr.1 %>% group_by(clust) %>%
                             summarize(number = n(),
                                       dev.mean = round(mean(dev), digits = 3),
                                       dev.ci = CI_95(dev),
                                       dRoad.mean = round(mean(dRoad), digits = 3),
                                       dRoad.ci = CI_95(dRoad),
                                       TRI.y.mean = round(mean(TRI.y), digits = 3),
                                       TRI.y.ci = CI_95(TRI.y),
                                       TPI.y.mean = round(mean(TPI.y), digits = 3),
                                       TPI.y.ci = CI_95(TPI.y),
                                       open.mean = round(mean(open), digits = 3),
                                       open.ci = CI_95(open),
                                       mf.mean = round(mean(mf), digits = 3),
                                       mf.ci = CI_95(mf),
                                       yf.mean = round(mean(yf), digits = 3),
                                       yf.ci = CI_95(yf),
                                       iji.mean = round(mean(iji), digits = 3),
                                       iji.ci = CI_95(iji),
                                       ed.mean = round(mean(ed), digits = 3),
                                       ed.ci = CI_95(ed),
                                       area.est.topo.mean = round(mean(area.est.topo), digits = 3),
                                       area.est.topo.ci = CI_95(area.est.topo))

write.table(UA.mean.table.2, "clipboard", sep = "\t")

#_____________________________________________________________________________________________________________
# 7. AW mean table ----
#_____________________________________________________________________________________________________________
# 7a. Selection coefficients ----
#_____________________________________________________________________________________________________________

AW.mean.table.1 <- AW.fr.1 %>% group_by(clust) %>%
                             summarize(number = n(),
                                       dDeveloped.mean = round(mean(dDeveloped), digits = 3),
                                       dDeveloped.ci = CI_95(dDeveloped),
                                       dRoad.P.mean = round(mean(dRoad.P), digits = 3),
                                       dRoad.P.ci = CI_95(dRoad.P),
                                       TRI.x.mean = round(mean(TRI.x), digits = 3),
                                       TRI.x.ci = CI_95(TRI.x),
                                       TPI.x.mean = round(mean(TPI.x), digits = 3),
                                       TPI.x.ci = CI_95(TPI.x),
                                       dOpen.mean = round(mean(dOpen), digits = 3),
                                       dOpen.ci = CI_95(dOpen),
                                       dMatureForest.mean = round(mean(dMatureForest), digits = 3),
                                       dMatureForest.ci = CI_95(dMatureForest),
                                       dYoungForest.mean = round(mean(dYoungForest), digits = 3),
                                       dYoungForest.ci = CI_95(dYoungForest))

write.table(AW.mean.table.1, "clipboard", sep = "\t")

#_____________________________________________________________________________________________________________
# 7b. Home range coefficients ----
#_____________________________________________________________________________________________________________

AW.mean.table.2 <- AW.fr.1 %>% group_by(clust) %>%
                             summarize(number = n(),
                                       dev.mean = round(mean(dev), digits = 3),
                                       dev.ci = CI_95(dev),
                                       dRoad.mean = round(mean(dRoad), digits = 3),
                                       dRoad.ci = CI_95(dRoad),
                                       TRI.y.mean = round(mean(TRI.y), digits = 3),
                                       TRI.y.ci = CI_95(TRI.y),
                                       TPI.y.mean = round(mean(TPI.y), digits = 3),
                                       TPI.y.ci = CI_95(TPI.y),
                                       open.mean = round(mean(open), digits = 3),
                                       open.ci = CI_95(open),
                                       mf.mean = round(mean(mf), digits = 3),
                                       mf.ci = CI_95(mf),
                                       yf.mean = round(mean(yf), digits = 3),
                                       yf.ci = CI_95(yf),
                                       iji.mean = round(mean(iji), digits = 3),
                                       iji.ci = CI_95(iji),
                                       ed.mean = round(mean(ed), digits = 3),
                                       ed.ci = CI_95(ed),
                                       area.est.topo.mean = round(mean(area.est.topo), digits = 3),
                                       area.est.topo.ci = CI_95(area.est.topo))

write.table(AW.mean.table.2, "clipboard", sep = "\t")

#_____________________________________________________________________________________________________________
# 8. How well do cluster assignments predict probability of use? ----
#_____________________________________________________________________________________________________________
# 8a. WS ----
#_____________________________________________________________________________________________________________

load("WS_models.RData")

# elk names
elknames.WS <- unique(elk.data.scaled$GroupID)

# data frame to hold all data
WS.clust.cv <- data.frame()

for (x in elknames.WS) {
  
  group.id <- x
  
  clust.id <- WS.fr.1$clust[WS.fr.1$GroupID == group.id]
  
  clust.coef <- WS.mean.table.1 %>% filter(clust == clust.id)
  
  # subset test and training data
  test.data <- elk.data.scaled %>% filter(GroupID == group.id)
  
  # calculate RSF scores on test data
  test.data <- test.data %>% mutate(RSF.score.clust = exp(clust.coef$dDeveloped.mean*dDeveloped +
                                                          clust.coef$dRoad.P.mean*dRoad.P +
                                                          clust.coef$TRI.x.mean*TRI +
                                                          clust.coef$TPI.x.mean*TPI +
                                                          clust.coef$dOpen.mean*dOpen +
                                                          clust.coef$dMatureForest.mean*dMatureForest +
                                                          clust.coef$dYoungForest.mean*dYoungForest))
  
  # create scores data frame
  test.scores <- test.data %>% dplyr::select(Animal, GroupID, Case, RSF.score.clust)
  
  # create 10 quantiles of RSF scores
  cv.quant <- quantile(test.scores$RSF.score.clust, probs = seq(0, 1, 0.1))
  
  # create a bin variable
  test.scores$bin <- rep(NA, length(test.scores$RSF.score.clust))
  
  # for loop to classify RSF scores in 1 of 10 bins
  for (j in 1:10) {
    
    test.scores$bin[test.scores$RSF.score.clust >= cv.quant[j] & test.scores$RSF.score.clust < cv.quant[j + 1]] = j
    
  }
  
  # if there's an NA because of a high RSF score, change the bin to 10
  test.scores$bin[is.na(test.scores$bin)] <- 10
  
  # area-adjusted frequency from Roberts et al.
  cv.aa <- table(test.scores$Case, test.scores$bin)
  cv.aa <- t(cv.aa)
  cv.aa <- as.data.frame.matrix(cv.aa)
  cv.aa$areaadjusted <- rep(NA, length(10))
  cv.aa$areaadjusted <- (cv.aa[ ,2] / sum(cv.aa[ ,2])) / (cv.aa[ ,1] / sum(cv.aa[ ,1]))
  cv.aa$bins <- seq(1, 10, 1)
  
  # create sum-friendly df
  cv.indiv <- as.data.frame(t(cv.aa[ ,c(3)]))
  cv.indiv$GroupID <- group.id
  
  # Spearman's rank correlation between area-adjusted freq. and bins
  cv.cor <- cor.test(cv.aa$areaadjusted, cv.aa$bins)
  
  cv.indiv$rho <- cv.cor$estimate
  cv.indiv$p.value <- cv.cor$p.value
  
  # bind to master df
  WS.clust.cv <- rbind(WS.clust.cv, cv.indiv)
  
}

# bind to main df
WS.clust.cv <- WS.clust.cv %>% dplyr::select(GroupID, rho) %>%
                                      rename(rho.clust = rho)

WS.fr.2 <- merge(WS.fr.1, WS.clust.cv)

# calculate differences
WS.fr.2 <- WS.fr.2 %>% mutate(rho.diff.global.clust = rho.clust - rho.global,
                              rho.diff.fr.clust = rho.clust - rho.fr)

# mean table 3
WS.mean.table.3 <- WS.fr.2 %>% group_by(clust) %>%
                             summarize(number = n(),
                                       dist.mean = round(mean(dist), digits = 3),
                                       dist.ci = CI_95(dist),
                                       rho.global.mean = round(mean(rho.global), digits = 3),
                                       rho.global.ci = CI_95(rho.global),
                                       rho.fr.mean = round(mean(rho.fr), digits = 3),
                                       rho.fr.ci = CI_95(rho.fr),
                                       rho.clust.mean = round(mean(rho.clust), digits = 3),
                                       rho.clust.ci = CI_95(rho.clust),
                                       rho.diff.mean = round(mean(rho.diff), digits = 3),
                                       rho.diff.ci = CI_95(rho.diff),
                                       rho.diff.global.clust.mean = round(mean(rho.diff.global.clust), digits = 3),
                                       rho.diff.global.clust.ci = CI_95(rho.diff.global.clust),
                                       rho.diff.fr.clust.mean = round(mean(rho.diff.fr.clust), digits = 3),
                                       rho.diff.fr.clust.ci = CI_95(rho.diff.fr.clust))

write.table(WS.mean.table.3, "clipboard", sep = "\t")

#_____________________________________________________________________________________________________________
# 8b. SU ----
#_____________________________________________________________________________________________________________

load("SU_models.RData")

# elk names
elknames.SU <- unique(elk.data.scaled$GroupID)

# data frame to hold all data
SU.clust.cv <- data.frame()

for (x in elknames.SU) {
  
  group.id <- x
  
  clust.id <- SU.fr.1$clust[SU.fr.1$GroupID == group.id]
  
  clust.coef <- SU.mean.table.1 %>% filter(clust == clust.id)
  
  # subset test and training data
  test.data <- elk.data.scaled %>% filter(GroupID == group.id)
  
  # calculate RSF scores on test data
  test.data <- test.data %>% mutate(RSF.score.clust = exp(clust.coef$dDeveloped.mean*dDeveloped +
                                                          clust.coef$dRoad.P.mean*dRoad.P +
                                                          clust.coef$TRI.x.mean*TRI +
                                                          clust.coef$TPI.x.mean*TPI +
                                                          clust.coef$dOpen.mean*dOpen +
                                                          clust.coef$dMatureForest.mean*dMatureForest +
                                                          clust.coef$dYoungForest.mean*dYoungForest))
  
  # create scores data frame
  test.scores <- test.data %>% dplyr::select(Animal, GroupID, Case, RSF.score.clust)
  
  # create 10 quantiles of RSF scores
  cv.quant <- quantile(test.scores$RSF.score.clust, probs = seq(0, 1, 0.1), na.rm = TRUE)
  
  # create a bin variable
  test.scores$bin <- rep(NA, length(test.scores$RSF.score.clust))
  
  # for loop to classify RSF scores in 1 of 10 bins
  for (j in 1:10) {
    
    test.scores$bin[test.scores$RSF.score.clust >= cv.quant[j] & test.scores$RSF.score.clust < cv.quant[j + 1]] = j
    
  }
  
  # if there's an NA because of a high RSF score, change the bin to 10
  test.scores$bin[is.na(test.scores$bin)] <- 10
  
  # area-adjusted frequency from Roberts et al.
  cv.aa <- table(test.scores$Case, test.scores$bin)
  cv.aa <- t(cv.aa)
  cv.aa <- as.data.frame.matrix(cv.aa)
  cv.aa$areaadjusted <- rep(NA, length(10))
  cv.aa$areaadjusted <- (cv.aa[ ,2] / sum(cv.aa[ ,2])) / (cv.aa[ ,1] / sum(cv.aa[ ,1]))
  cv.aa$bins <- seq(1, 10, 1)
  
  # create sum-friendly df
  cv.indiv <- as.data.frame(t(cv.aa[ ,c(3)]))
  cv.indiv$GroupID <- group.id
  
  # Spearman's rank correlation between area-adjusted freq. and bins
  cv.cor <- cor.test(cv.aa$areaadjusted, cv.aa$bins)
  
  cv.indiv$rho <- cv.cor$estimate
  cv.indiv$p.value <- cv.cor$p.value
  
  # bind to master df
  SU.clust.cv <- rbind(SU.clust.cv, cv.indiv)
  
}

# bind to main df
SU.clust.cv <- SU.clust.cv %>% dplyr::select(GroupID, rho) %>%
                                      rename(rho.clust = rho)

SU.fr.2 <- merge(SU.fr.1, SU.clust.cv)

# calculate differences
SU.fr.2 <- SU.fr.2 %>% mutate(rho.diff.global.clust = rho.clust - rho.global,
                              rho.diff.fr.clust = rho.clust - rho.fr)

# mean table 3
SU.mean.table.3 <- SU.fr.2 %>% group_by(clust) %>%
                             summarize(number = n(),
                                       rho.global.mean = round(mean(rho.global), digits = 3),
                                       rho.global.ci = CI_95(rho.global),
                                       rho.fr.mean = round(mean(rho.fr), digits = 3),
                                       rho.fr.ci = CI_95(rho.fr),
                                       rho.clust.mean = round(mean(rho.clust), digits = 3),
                                       rho.clust.ci = CI_95(rho.clust),
                                       rho.diff.mean = round(mean(rho.diff), digits = 3),
                                       rho.diff.ci = CI_95(rho.diff),
                                       rho.diff.global.clust.mean = round(mean(rho.diff.global.clust), digits = 3),
                                       rho.diff.global.clust.ci = CI_95(rho.diff.global.clust),
                                       rho.diff.fr.clust.mean = round(mean(rho.diff.fr.clust), digits = 3),
                                       rho.diff.fr.clust.ci = CI_95(rho.diff.fr.clust))

write.table(SU.mean.table.3, "clipboard", sep = "\t")

#_____________________________________________________________________________________________________________
# 8c. UA ----
#_____________________________________________________________________________________________________________

load("UA_models.RData")

# elk names
elknames.UA <- unique(elk.data.scaled$GroupID)

# data frame to hold all data
UA.clust.cv <- data.frame()

for (x in elknames.UA) {
  
  group.id <- x
  
  clust.id <- UA.fr.1$clust[UA.fr.1$GroupID == group.id]
  
  clust.coef <- UA.mean.table.1 %>% filter(clust == clust.id)
  
  # subset test and training data
  test.data <- elk.data.scaled %>% filter(GroupID == group.id)
  
  # calculate RSF scores on test data
  test.data <- test.data %>% mutate(RSF.score.clust = exp(clust.coef$dDeveloped.mean*dDeveloped +
                                                          clust.coef$dRoad.P.mean*dRoad.P +
                                                          clust.coef$TRI.x.mean*TRI +
                                                          clust.coef$TPI.x.mean*TPI +
                                                          clust.coef$dOpen.mean*dOpen +
                                                          clust.coef$dMatureForest.mean*dMatureForest +
                                                          clust.coef$dYoungForest.mean*dYoungForest))
  
  # create scores data frame
  test.scores <- test.data %>% dplyr::select(Animal, GroupID, Case, RSF.score.clust)
  
  # create 10 quantiles of RSF scores
  cv.quant <- quantile(test.scores$RSF.score.clust, probs = seq(0, 1, 0.1), na.rm = TRUE)
  
  # create a bin variable
  test.scores$bin <- rep(NA, length(test.scores$RSF.score.clust))
  
  # for loop to classify RSF scores in 1 of 10 bins
  for (j in 1:10) {
    
    test.scores$bin[test.scores$RSF.score.clust >= cv.quant[j] & test.scores$RSF.score.clust < cv.quant[j + 1]] = j
    
  }
  
  # if there's an NA because of a high RSF score, change the bin to 10
  test.scores$bin[is.na(test.scores$bin)] <- 10
  
  # area-adjusted frequency from Roberts et al.
  cv.aa <- table(test.scores$Case, test.scores$bin)
  cv.aa <- t(cv.aa)
  cv.aa <- as.data.frame.matrix(cv.aa)
  cv.aa$areaadjusted <- rep(NA, length(10))
  cv.aa$areaadjusted <- (cv.aa[ ,2] / sum(cv.aa[ ,2])) / (cv.aa[ ,1] / sum(cv.aa[ ,1]))
  cv.aa$bins <- seq(1, 10, 1)
  
  # create sum-friendly df
  cv.indiv <- as.data.frame(t(cv.aa[ ,c(3)]))
  cv.indiv$GroupID <- group.id
  
  # Spearman's rank correlation between area-adjusted freq. and bins
  cv.cor <- cor.test(cv.aa$areaadjusted, cv.aa$bins)
  
  cv.indiv$rho <- cv.cor$estimate
  cv.indiv$p.value <- cv.cor$p.value
  
  # bind to master df
  UA.clust.cv <- rbind(UA.clust.cv, cv.indiv)
  
}

# bind to main df
UA.clust.cv <- UA.clust.cv %>% dplyr::select(GroupID, rho) %>%
                                      rename(rho.clust = rho)

UA.fr.2 <- merge(UA.fr.1, UA.clust.cv)

# calculate differences
UA.fr.2 <- UA.fr.2 %>% mutate(rho.diff.global.clust = rho.clust - rho.global,
                              rho.diff.fr.clust = rho.clust - rho.fr)

# mean table 3
UA.mean.table.3 <- UA.fr.2 %>% group_by(clust) %>%
                             summarize(number = n(),
                                       dist.mean = round(mean(dist), digits = 3),
                                       dist.ci = CI_95(dist),
                                       rho.global.mean = round(mean(rho.global), digits = 3),
                                       rho.global.ci = CI_95(rho.global),
                                       rho.fr.mean = round(mean(rho.fr), digits = 3),
                                       rho.fr.ci = CI_95(rho.fr),
                                       rho.clust.mean = round(mean(rho.clust), digits = 3),
                                       rho.clust.ci = CI_95(rho.clust),
                                       rho.diff.mean = round(mean(rho.diff), digits = 3),
                                       rho.diff.ci = CI_95(rho.diff),
                                       rho.diff.global.clust.mean = round(mean(rho.diff.global.clust), digits = 3),
                                       rho.diff.global.clust.ci = CI_95(rho.diff.global.clust),
                                       rho.diff.fr.clust.mean = round(mean(rho.diff.fr.clust), digits = 3),
                                       rho.diff.fr.clust.ci = CI_95(rho.diff.fr.clust))

write.table(UA.mean.table.3, "clipboard", sep = "\t")

#_____________________________________________________________________________________________________________
# 8d. AW ----
#_____________________________________________________________________________________________________________

load("AW_models.RData")

# elk names
elknames.AW <- unique(elk.data.scaled$GroupID)

# data frame to hold all data
AW.clust.cv <- data.frame()

for (x in elknames.AW) {
  
  group.id <- x
  
  clust.id <- AW.fr.1$clust[AW.fr.1$GroupID == group.id]
  
  clust.coef <- AW.mean.table.1 %>% filter(clust == clust.id)
  
  # subset test and training data
  test.data <- elk.data.scaled %>% filter(GroupID == group.id)
  
  # calculate RSF scores on test data
  test.data <- test.data %>% mutate(RSF.score.clust = exp(clust.coef$dDeveloped.mean*dDeveloped +
                                                          clust.coef$dRoad.P.mean*dRoad.P +
                                                          clust.coef$TRI.x.mean*TRI +
                                                          clust.coef$TPI.x.mean*TPI +
                                                          clust.coef$dOpen.mean*dOpen +
                                                          clust.coef$dMatureForest.mean*dMatureForest +
                                                          clust.coef$dYoungForest.mean*dYoungForest))
  
  # create scores data frame
  test.scores <- test.data %>% dplyr::select(Animal, GroupID, Case, RSF.score.clust)
  
  # create 10 quantiles of RSF scores
  cv.quant <- quantile(test.scores$RSF.score.clust, probs = seq(0, 1, 0.1), na.rm = TRUE)
  
  # create a bin variable
  test.scores$bin <- rep(NA, length(test.scores$RSF.score.clust))
  
  # for loop to classify RSF scores in 1 of 10 bins
  for (j in 1:10) {
    
    test.scores$bin[test.scores$RSF.score.clust >= cv.quant[j] & test.scores$RSF.score.clust < cv.quant[j + 1]] = j
    
  }
  
  # if there's an NA because of a high RSF score, change the bin to 10
  test.scores$bin[is.na(test.scores$bin)] <- 10
  
  # area-adjusted frequency from Roberts et al.
  cv.aa <- table(test.scores$Case, test.scores$bin)
  cv.aa <- t(cv.aa)
  cv.aa <- as.data.frame.matrix(cv.aa)
  cv.aa$areaadjusted <- rep(NA, length(10))
  cv.aa$areaadjusted <- (cv.aa[ ,2] / sum(cv.aa[ ,2])) / (cv.aa[ ,1] / sum(cv.aa[ ,1]))
  cv.aa$bins <- seq(1, 10, 1)
  
  # create sum-friendly df
  cv.indiv <- as.data.frame(t(cv.aa[ ,c(3)]))
  cv.indiv$GroupID <- group.id
  
  # Spearman's rank correlation between area-adjusted freq. and bins
  cv.cor <- cor.test(cv.aa$areaadjusted, cv.aa$bins)
  
  cv.indiv$rho <- cv.cor$estimate
  cv.indiv$p.value <- cv.cor$p.value
  
  # bind to master df
  AW.clust.cv <- rbind(AW.clust.cv, cv.indiv)
  
}

# bind to main df
AW.clust.cv <- AW.clust.cv %>% dplyr::select(GroupID, rho) %>%
                                      rename(rho.clust = rho)

AW.fr.2 <- merge(AW.fr.1, AW.clust.cv)

# calculate differences
AW.fr.2 <- AW.fr.2 %>% mutate(rho.diff.global.clust = rho.clust - rho.global,
                              rho.diff.fr.clust = rho.clust - rho.fr)

# mean table 3
AW.mean.table.3 <- AW.fr.2 %>% group_by(clust) %>%
                             summarize(number = n(),
                                       dist.mean = round(mean(dist), digits = 3),
                                       dist.ci = CI_95(dist),
                                       rho.global.mean = round(mean(rho.global), digits = 3),
                                       rho.global.ci = CI_95(rho.global),
                                       rho.fr.mean = round(mean(rho.fr), digits = 3),
                                       rho.fr.ci = CI_95(rho.fr),
                                       rho.clust.mean = round(mean(rho.clust), digits = 3),
                                       rho.clust.ci = CI_95(rho.clust),
                                       rho.diff.mean = round(mean(rho.diff), digits = 3),
                                       rho.diff.ci = CI_95(rho.diff),
                                       rho.diff.global.clust.mean = round(mean(rho.diff.global.clust), digits = 3),
                                       rho.diff.global.clust.ci = CI_95(rho.diff.global.clust),
                                       rho.diff.fr.clust.mean = round(mean(rho.diff.fr.clust), digits = 3),
                                       rho.diff.fr.clust.ci = CI_95(rho.diff.fr.clust))

write.table(AW.mean.table.3, "clipboard", sep = "\t")

#_____________________________________________________________________________________________________________
# 9. Relationships between pred/actual distance and functional response CV score ----
#_____________________________________________________________________________________________________________
# 9a. WS ----
#_____________________________________________________________________________________________________________

ggplot(WS.fr.2, aes(x = dist, y = rho.fr)) +
       theme_bw() +
       geom_point() +
       stat_smooth(method = "gam")

#_____________________________________________________________________________________________________________
# 10. Save csvs  ----
#_____________________________________________________________________________________________________________

write.csv(WS.fr.2, "WS_pergroup.csv")
write.csv(SU.fr.2, "SU_pergroup.csv")
write.csv(UA.fr.2, "UA_pergroup.csv")
write.csv(AW.fr.2, "AW_pergroup.csv")
