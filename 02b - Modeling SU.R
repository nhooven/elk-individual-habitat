# Title: Individual variation in habitat selection
# Subtitle: 2b - Modeling SU
# Author: Nathan D. Hooven
# Email: nathan.d.hooven@gmail.com
# Affiliation: Department of Forestry and Natural Resources, University of Kentucky
# Date began: 17 Nov 2020
# Date completed: 17 Nov 2020
# Date modified: 5 Jan 2022
# R version: 3.6.2

#_____________________________________________________________________________________________________________
# 1. Load in required packages ----
#_____________________________________________________________________________________________________________

library(tidyverse)       
library(glmmTMB)          # modeling
library(ggplot2)          # visualization
library(AICcmodavg)       # model selection
library(reshape)          # melt random slopes
library(performance)      # VIFs

#_____________________________________________________________________________________________________________
# 2. Read in data and check for correlations ----
#_____________________________________________________________________________________________________________

elk.data <- read.csv("SU_sampled.csv")

# select only columns we need
elk.data <- elk.data %>% dplyr::select(Animal, Year, GroupID, Case, 
                                       dDeveloped, dOpen, dYoungForest, dMatureForest, dEdge,
                                       TRI, TPI, dRoad, canopy)

# convert GroupID to factor for mixed modeling
elk.data$GroupID <- as.factor(elk.data$GroupID)

# check for correlations
cor(elk.data[ ,5:13], method = "spearman", use = "complete.obs")
cor(elk.data[ ,5:13], method = "pearson", use = "complete.obs")

#_____________________________________________________________________________________________________________
# 3. Density plots ----
#_____________________________________________________________________________________________________________

# dDeveloped
ggplot(elk.data, aes(x = dDeveloped, group = as.factor(Case), color = as.factor(Case))) +
  theme_bw() +
  geom_density(size = 1.5)

# dOpen
ggplot(elk.data, aes(x = dOpen, group = as.factor(Case), color = as.factor(Case))) +
  theme_bw() +
  geom_density(size = 1.5) +
  coord_cartesian(xlim = c(0, 1500))

# dYoungForest
ggplot(elk.data, aes(x = dYoungForest, group = as.factor(Case), color = as.factor(Case))) +
  theme_bw() +
  geom_density(size = 1.5)

# dMatureForest
ggplot(elk.data, aes(x = dMatureForest, group = as.factor(Case), color = as.factor(Case))) +
  theme_bw() +
  geom_density(size = 1.5)

# TRI
ggplot(elk.data, aes(x = TRI, group = as.factor(Case), color = as.factor(Case))) +
  theme_bw() +
  geom_density(size = 1.5)

# TPI
ggplot(elk.data, aes(x = TPI, group = as.factor(Case), color = as.factor(Case))) +
  theme_bw() +
  geom_density(size = 1.5)

# dRoad
ggplot(elk.data, aes(x = dRoad, group = as.factor(Case), color = as.factor(Case))) +
  theme_bw() +
  geom_density(size = 1.5)

#_____________________________________________________________________________________________________________
# 4. Standardize variables ----
#_____________________________________________________________________________________________________________

# remove highly correlated variables
elk.data <- elk.data %>% dplyr::select(Animal, Year, GroupID, Case,
                                       dDeveloped, dOpen, dYoungForest, dMatureForest, 
                                       TRI, TPI, dRoad)

# add pseudothreshold variable for roads
elk.data$dRoad.P <- log(elk.data$dRoad + 1)

# center and scale all variables
elk.data.scaled <- elk.data %>% mutate_at(c(5:12), scale)

# create weights column
elk.data.scaled <- elk.data.scaled %>% mutate(weights = ifelse(Case == 0, 5000, 1))

#_____________________________________________________________________________________________________________
# 5. Fit model ----
#_____________________________________________________________________________________________________________

models.struc <- glmmTMB(Case ~
                          dDeveloped +
                          dRoad.P +
                          TRI + 
                          TPI +
                          dOpen +
                          dMatureForest +
                          dYoungForest +
                          (1 | GroupID) +
                          (0 + dDeveloped | GroupID) +
                          (0 + dRoad.P | GroupID) +
                          (0 + TRI | GroupID) +
                          (0 + TPI | GroupID) +
                          (0 + dOpen | GroupID) +
                          (0 + dMatureForest | GroupID) +
                          (0 + dYoungForest | GroupID),
                        data = elk.data.scaled,
                        weights = weights,
                        family = binomial,
                        doFit = FALSE)

models.struc$parameters$theta[1] = log(1e3)
models.struc$mapArg = list(theta = factor(c(NA, 1:7)))

global.model <- glmmTMB:::fitTMB(models.struc)

summary(global.model)

check_collinearity(global.model)

#_____________________________________________________________________________________________________________
# 7. Extract random slopes ----
#_____________________________________________________________________________________________________________

# pull slope coefficients
coefs <- coef(global.model)

coefs.df <- data.frame(coefs$cond$GroupID)
coefs.df <- cbind(rownames(coefs.df), coefs.df)

# melt data into tidy format
coefs.df.melt <- melt(coefs.df, id = "rownames(coefs.df)")
colnames(coefs.df.melt) <- c("GroupID", "variable", "estimate")

# write melted random slopes file for plotting
write.csv(coefs.df.melt, "randomslopes_SU_plot.csv")

# write rndom slopes file for analysis
coefs.df$Season <- "SU"

write.csv(coefs.df, "randomslopes_SU_analysis.csv")

#_____________________________________________________________________________________________________________
# 8. Save image for predictions ----
#_____________________________________________________________________________________________________________

save.image(file = "SU_models.RData")
