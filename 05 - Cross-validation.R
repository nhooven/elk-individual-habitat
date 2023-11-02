# Title: Individual variation in habitat selection
# Subtitle: 5 - Cross-validation
# Author: Nathan D. Hooven
# Email: nathan.d.hooven@gmail.com
# Affiliation: Department of Forestry and Natural Resources, University of Kentucky
# Date began: 11 Jan 2022
# Date completed: 14 Jan 2022
# Date modified: 24 Mar 2022
# R version: 3.6.2

#_____________________________________________________________________________________________________________
# 1. Load in required packages ----
#_____________________________________________________________________________________________________________

library(tidyverse)
library(glmmTMB)    # modeling

#_____________________________________________________________________________________________________________
# 2. Load in functional response models ----
#_____________________________________________________________________________________________________________

load("fr_models.RData")

#_____________________________________________________________________________________________________________
# 3. WS cross-validation ----
#_____________________________________________________________________________________________________________

load("WS_models.RData")

summary(global.model)

#_____________________________________________________________________________________________________________
# 3a. For loop - global model ----
#_____________________________________________________________________________________________________________

# elk names
elknames.WS <- unique(elk.data.scaled$GroupID)

# data frame to hold all data
WS.global.cv <- data.frame()

for (x in elknames.WS) {
  
  group.id <- x
  
  # subset test and training data
  test.data <- elk.data.scaled %>% filter(GroupID == group.id)
  
  train.data <- elk.data.scaled %>% filter(GroupID != group.id)
  
  # fit global model to training data
  train.model.struc <- glmmTMB(Case ~
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
                             data = train.data,
                             weights = weights,
                             family = binomial,
                             doFit = FALSE)
  
  train.model.struc$parameters$theta[1] = log(1e3)
  train.model.struc$mapArg = list(theta = factor(c(NA, 1:7)))
  
  train.model <- glmmTMB:::fitTMB(train.model.struc)
  
  # calculate RSF scores on test data
  test.data <- test.data %>% mutate(RSF.score.global = exp(train.model$fit$par[2]*dDeveloped +
                                                           train.model$fit$par[3]*dRoad.P +
                                                           train.model$fit$par[4]*TRI +
                                                           train.model$fit$par[5]*TPI +
                                                           train.model$fit$par[6]*dOpen +
                                                           train.model$fit$par[7]*dMatureForest +
                                                           train.model$fit$par[8]*dYoungForest))
  
  # create scores data frame
  test.scores <- test.data %>% dplyr::select(Animal, GroupID, Case, RSF.score.global)
  
  # create 10 quantiles of RSF scores
  cv.quant <- quantile(test.scores$RSF.score.global, probs = seq(0, 1, 0.1))
  
  # create a bin variable
  test.scores$bin <- rep(NA, length(test.scores$RSF.score.global))
  
  # for loop to classify RSF scores in 1 of 10 bins
  for (j in 1:10) {
    
    test.scores$bin[test.scores$RSF.score.global >= cv.quant[j] & test.scores$RSF.score.global < cv.quant[j + 1]] = j
    
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
  WS.global.cv <- rbind(WS.global.cv, cv.indiv)
  
}

# density plot of rho values
ggplot(WS.global.cv, aes(x = rho)) +
       theme_bw() +
       geom_density() +
       geom_vline(xintercept = mean(WS.global.cv$rho))

#_____________________________________________________________________________________________________________
# 3b. For loop - functional response model ----
#_____________________________________________________________________________________________________________

# data frame to hold all data
WS.fr.cv <- data.frame()

for (x in elknames.WS) {
  
  group.id <- x
  
  # subset availability data
  group.avail <- WS.fr %>% filter(GroupID == group.id)
  
  # subset test and training data
  test.data <- elk.data.scaled %>% filter(GroupID == group.id)
  
  # calculate RSF scores on test data
  test.data <- test.data %>% mutate(RSF.score.fr = exp(as.numeric(predict(WS.models[[1]], newdata = group.avail))*dDeveloped +
                                                       as.numeric(predict(WS.models[[2]], newdata = group.avail))*dRoad.P +
                                                       as.numeric(predict(WS.models[[3]], newdata = group.avail))*TRI +
                                                       as.numeric(predict(WS.models[[4]], newdata = group.avail))*TPI +
                                                       as.numeric(predict(WS.models[[5]], newdata = group.avail))*dOpen +
                                                       as.numeric(predict(WS.models[[6]], newdata = group.avail))*dMatureForest +
                                                       as.numeric(predict(WS.models[[7]], newdata = group.avail))*dYoungForest))
  
  # create scores data frame
  test.scores <- test.data %>% dplyr::select(Animal, GroupID, Case, RSF.score.fr)
  
  # create 10 quantiles of RSF scores
  cv.quant <- quantile(test.scores$RSF.score.fr, probs = seq(0, 1, 0.1))
  
  # create a bin variable
  test.scores$bin <- rep(NA, length(test.scores$RSF.score.fr))
  
  # for loop to classify RSF scores in 1 of 10 bins
  for (j in 1:10) {
    
    test.scores$bin[test.scores$RSF.score.fr >= cv.quant[j] & test.scores$RSF.score.fr < cv.quant[j + 1]] = j
    
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
  WS.fr.cv <- rbind(WS.fr.cv, cv.indiv)
  
}

# density plot of rho values
ggplot(WS.fr.cv, aes(x = rho)) +
  theme_bw() +
  geom_density() +
  geom_vline(xintercept = mean(WS.fr.cv$rho))

#_____________________________________________________________________________________________________________
# 3c. Increase in model performance with the addition of the functional response ----
#_____________________________________________________________________________________________________________

WS.cv.compare <- data.frame(GroupID = WS.global.cv$GroupID,
                            rho.global = WS.global.cv$rho,
                            rho.fr = WS.fr.cv$rho)

WS.cv.compare <- WS.cv.compare %>% mutate(rho.diff = rho.fr - rho.global)

# longer table for graphing
WS.cv.compare.long <- pivot_longer(WS.cv.compare, cols = c("rho.global", "rho.fr"))

# make sure factor levels are in the right order
WS.cv.compare.long$name <- factor(WS.cv.compare.long$name,
                                  levels = c("rho.global", "rho.fr"))

# plot of differences
ggplot(data = WS.cv.compare.long, aes(x = name, y = value, group = GroupID)) +
       theme_bw() +
       geom_line(aes(color = rho.diff),
                 alpha = 0.5) +
       geom_point(aes(shape = name),
                  alpha = 0.5)

mean(WS.cv.compare$rho.diff)
max(WS.cv.compare$rho.diff)
min(WS.cv.compare$rho.diff)


mean(WS.cv.compare$rho.global)
mean(WS.cv.compare$rho.fr)

# density plot
ggplot(data = WS.cv.compare.long, aes(x = value, group = name)) +
       theme_bw() +
       geom_density(aes(color = name, fill = name),
                    alpha = 0.25,
                    size = 1.15)

#_____________________________________________________________________________________________________________
# 3d. Plot of all aaf/bin relationships ----
#_____________________________________________________________________________________________________________

WS.global.cv.longer <- WS.global.cv %>% pivot_longer(cols = c(1:10), names_prefix = "V") %>%
                                        rename(bin = name) %>%
                                        dplyr::select(GroupID, bin, value)

# df for plotting of mean value
WS.global.mean <- WS.global.cv.longer %>% group_by(bin) %>%
                                          summarize(mean.value = mean(value))

ggplot(data = WS.global.cv.longer, aes(x = as.numeric(bin), y = value)) +
       theme_bw() +
       theme(panel.grid = element_blank()) +
       geom_line(aes(group = GroupID), 
                 alpha = 0.5, color = "gray") +
       ylab("Frequency of used locations") +
       xlab("RSF score bin") + 
       scale_x_continuous(breaks = seq(0, 10, 1)) +
       geom_line(data = WS.global.mean, aes(x = as.numeric(bin), y = mean.value),
                 color = "#009900",
                 size = 1.25) +
       geom_errorbar(aes(x = 1, 
                     ymin = mean(WS.global.cv$V1) - sd(WS.global.cv$V1)),
                     ymax = mean(WS.global.cv$V1) + sd(WS.global.cv$V1),
                     width = 0) +
       geom_point(aes(x = 1, y = mean(WS.global.cv$V1)),
                  shape = 21,
                  fill = "#009900") +
       geom_errorbar(aes(x = 2, 
                         ymin = mean(WS.global.cv$V2) - sd(WS.global.cv$V2)),
                     ymax = mean(WS.global.cv$V2) + sd(WS.global.cv$V2),
                     width = 0) +
       geom_point(aes(x = 2, y = mean(WS.global.cv$V2)),
                  shape = 21,
                  fill = "#009900") +
       geom_errorbar(aes(x = 3, 
                         ymin = mean(WS.global.cv$V3) - sd(WS.global.cv$V3)),
                     ymax = mean(WS.global.cv$V3) + sd(WS.global.cv$V3),
                     width = 0) +
       geom_point(aes(x = 3, y = mean(WS.global.cv$V3)),
                  shape = 21,
                  fill = "#009900") +
       geom_errorbar(aes(x = 4, 
                         ymin = mean(WS.global.cv$V4) - sd(WS.global.cv$V4)),
                     ymax = mean(WS.global.cv$V4) + sd(WS.global.cv$V4),
                     width = 0) +
       geom_point(aes(x = 4, y = mean(WS.global.cv$V4)),
                  shape = 21,
                  fill = "#009900") +
       geom_errorbar(aes(x = 5, 
                         ymin = mean(WS.global.cv$V5) - sd(WS.global.cv$V5)),
                     ymax = mean(WS.global.cv$V5) + sd(WS.global.cv$V5),
                     width = 0) +
       geom_point(aes(x = 5, y = mean(WS.global.cv$V5)),
                  shape = 21,
                  fill = "#009900") +
       geom_errorbar(aes(x = 6, 
                         ymin = mean(WS.global.cv$V6) - sd(WS.global.cv$V6)),
                     ymax = mean(WS.global.cv$V6) + sd(WS.global.cv$V6),
                     width = 0) +
       geom_point(aes(x = 6, y = mean(WS.global.cv$V6)),
                  shape = 21,
                  fill = "#009900") +
       geom_errorbar(aes(x = 7, 
                         ymin = mean(WS.global.cv$V7) - sd(WS.global.cv$V7)),
                     ymax = mean(WS.global.cv$V7) + sd(WS.global.cv$V7),
                     width = 0) +
       geom_point(aes(x = 7, y = mean(WS.global.cv$V7)),
                  shape = 21,
                  fill = "#009900") +
       geom_errorbar(aes(x = 8, 
                         ymin = mean(WS.global.cv$V8) - sd(WS.global.cv$V8)),
                     ymax = mean(WS.global.cv$V8) + sd(WS.global.cv$V8),
                     width = 0) +
       geom_point(aes(x = 8, y = mean(WS.global.cv$V8)),
                  shape = 21,
                  fill = "#009900") +
       geom_errorbar(aes(x = 9, 
                         ymin = mean(WS.global.cv$V9) - sd(WS.global.cv$V9)),
                     ymax = mean(WS.global.cv$V9) + sd(WS.global.cv$V9),
                     width = 0) +
       geom_point(aes(x = 9, y = mean(WS.global.cv$V9)),
                  shape = 21,
                  fill = "#009900") +
       geom_errorbar(aes(x = 10, 
                         ymin = mean(WS.global.cv$V10) - sd(WS.global.cv$V10)),
                     ymax = mean(WS.global.cv$V10) + sd(WS.global.cv$V10),
                     width = 0) +
       geom_point(aes(x = 10, y = mean(WS.global.cv$V10)),
                  shape = 21,
                  fill = "#009900")

#_____________________________________________________________________________________________________________
# 3e. Write to csv ----
#_____________________________________________________________________________________________________________

write.csv(WS.cv.compare, "WS_cv_compare.csv")

#_____________________________________________________________________________________________________________
# 4. SU cross-validation ----
#_____________________________________________________________________________________________________________

load("SU_models.RData")

summary(global.model)

#_____________________________________________________________________________________________________________
# 4a. For loop - global model ----
#_____________________________________________________________________________________________________________

# elk names
elknames.SU <- unique(elk.data.scaled$GroupID)

# data frame to hold all data
SU.global.cv <- data.frame()

for (x in elknames.SU) {
  
  group.id <- x
  
  # subset test and training data
  test.data <- elk.data.scaled %>% filter(GroupID == group.id)
  
  train.data <- elk.data.scaled %>% filter(GroupID != group.id)
  
  # fit global model to training data
  train.model.struc <- glmmTMB(Case ~
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
                               data = train.data,
                               weights = weights,
                               family = binomial,
                               doFit = FALSE)
  
  train.model.struc$parameters$theta[1] = log(1e3)
  train.model.struc$mapArg = list(theta = factor(c(NA, 1:7)))
  
  train.model <- glmmTMB:::fitTMB(train.model.struc)
  
  # calculate RSF scores on test data
  test.data <- test.data %>% mutate(RSF.score.global = exp(train.model$fit$par[2]*dDeveloped +
                                                           train.model$fit$par[3]*dRoad.P +
                                                           train.model$fit$par[4]*TRI +
                                                           train.model$fit$par[5]*TPI +
                                                           train.model$fit$par[6]*dOpen +
                                                           train.model$fit$par[7]*dMatureForest +
                                                           train.model$fit$par[8]*dYoungForest))
  
  # create scores data frame
  test.scores <- test.data %>% dplyr::select(Animal, GroupID, Case, RSF.score.global)
  
  # remove rows with NAs for RSF.score
  test.scores <- test.scores[complete.cases(test.scores$RSF.score.global), ]
  
  # create 10 quantiles of RSF scores
  cv.quant <- quantile(test.scores$RSF.score.global, probs = seq(0, 1, 0.1))
  
  # create a bin variable
  test.scores$bin <- rep(NA, length(test.scores$RSF.score.global))
  
  # for loop to classify RSF scores in 1 of 10 bins
  for (j in 1:10) {
    
    test.scores$bin[test.scores$RSF.score.global >= cv.quant[j] & test.scores$RSF.score.global < cv.quant[j + 1]] = j
    
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
  SU.global.cv <- rbind(SU.global.cv, cv.indiv)
  
}

# density plot of rho values
ggplot(SU.global.cv, aes(x = rho)) +
  theme_bw() +
  geom_density() +
  geom_vline(xintercept = mean(SU.global.cv$rho))

#_____________________________________________________________________________________________________________
# 4b. For loop - functional response model ----
#_____________________________________________________________________________________________________________

# data frame to hold all data
SU.fr.cv <- data.frame()

for (x in elknames.SU) {
  
  group.id <- x
  
  # subset availability data
  group.avail <- SU.fr %>% filter(GroupID == group.id)
  
  # subset test and training data
  test.data <- elk.data.scaled %>% filter(GroupID == group.id)
  
  # calculate RSF scores on test data
  test.data <- test.data %>% mutate(RSF.score.fr = exp(mean(SU.fr$dDeveloped)*dDeveloped +
                                                       as.numeric(predict(SU.models[[2]], newdata = group.avail))*dRoad.P +
                                                       as.numeric(predict(SU.models[[3]], newdata = group.avail))*TRI +
                                                       mean(SU.fr$TPI.x)*TPI +
                                                       as.numeric(predict(SU.models[[5]], newdata = group.avail))*dOpen +
                                                       as.numeric(predict(SU.models[[6]], newdata = group.avail))*dMatureForest +
                                                       as.numeric(predict(SU.models[[7]], newdata = group.avail))*dYoungForest))
  
  # create scores data frame
  test.scores <- test.data %>% dplyr::select(Animal, GroupID, Case, RSF.score.fr)
  
  # remove rows with NAs for RSF.score
  test.scores <- test.scores[complete.cases(test.scores$RSF.score.fr), ]
  
  # create 10 quantiles of RSF scores
  cv.quant <- quantile(test.scores$RSF.score.fr, probs = seq(0, 1, 0.1))
  
  # create a bin variable
  test.scores$bin <- rep(NA, length(test.scores$RSF.score.fr))
  
  # for loop to classify RSF scores in 1 of 10 bins
  for (j in 1:10) {
    
    test.scores$bin[test.scores$RSF.score.fr >= cv.quant[j] & test.scores$RSF.score.fr < cv.quant[j + 1]] = j
    
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
  SU.fr.cv <- rbind(SU.fr.cv, cv.indiv)
  
}

# density plot of rho values
ggplot(SU.fr.cv, aes(x = rho)) +
  theme_bw() +
  geom_density() +
  geom_vline(xintercept = mean(SU.fr.cv$rho))

#_____________________________________________________________________________________________________________
# 4c. Increase in model performance with the addition of the functional response ----
#_____________________________________________________________________________________________________________

SU.cv.compare <- data.frame(GroupID = SU.global.cv$GroupID,
                            rho.global = SU.global.cv$rho,
                            rho.fr = SU.fr.cv$rho)

SU.cv.compare <- SU.cv.compare %>% mutate(rho.diff = rho.fr - rho.global)

# longer table for graphing
SU.cv.compare.long <- pivot_longer(SU.cv.compare, cols = c("rho.global", "rho.fr"))

# make sure factor levels are in the right order
SU.cv.compare.long$name <- factor(SU.cv.compare.long$name,
                                  levels = c("rho.global", "rho.fr"))

# plot of differences
ggplot(data = SU.cv.compare.long, aes(x = name, y = value, group = GroupID)) +
  theme_bw() +
  geom_line(aes(color = rho.diff),
            alpha = 0.5) +
  geom_point(aes(shape = name),
             alpha = 0.5)

mean(SU.cv.compare$rho.diff)
max(SU.cv.compare$rho.diff)

mean(SU.cv.compare$rho.global)
mean(SU.cv.compare$rho.fr)

# density plot
ggplot(data = SU.cv.compare.long, aes(x = value, group = name)) +
  theme_bw() +
  geom_density(aes(color = name, fill = name),
               alpha = 0.25,
               size = 1.15)

#_____________________________________________________________________________________________________________
# 4e. Write to csv ----
#_____________________________________________________________________________________________________________

write.csv(SU.cv.compare, "SU_cv_compare.csv")

#_____________________________________________________________________________________________________________
# 4d. Plot of all aaf/bin relationships ----
#_____________________________________________________________________________________________________________

SU.global.cv.longer <- SU.global.cv %>% pivot_longer(cols = c(1:10), names_prefix = "V") %>%
                                        rename(bin = name) %>%
                                        dplyr::select(GroupID, bin, value)

# df for plotting of mean value
SU.global.mean <- SU.global.cv.longer %>% group_by(bin) %>%
                                          summarize(mean.value = mean(value))

ggplot(data = SU.global.cv.longer, aes(x = as.numeric(bin), y = value)) +
       theme_bw() +
       theme(panel.grid = element_blank()) +
       geom_line(aes(group = GroupID), 
                 alpha = 0.5, color = "gray") +
       ylab("Frequency of used locations") +
       xlab("RSF score bin") + 
       scale_x_continuous(breaks = seq(0, 10, 1)) +
       geom_line(data = SU.global.mean, aes(x = as.numeric(bin), y = mean.value),
                 color = "#FF9900",
                 size = 1.25) +
       geom_errorbar(aes(x = 1, 
                         ymin = mean(SU.global.cv$V1) - sd(SU.global.cv$V1)),
                     ymax = mean(SU.global.cv$V1) + sd(SU.global.cv$V1),
                     width = 0) +
       geom_point(aes(x = 1, y = mean(SU.global.cv$V1)),
                  shape = 21,
                  fill = "#FF9900") +
       geom_errorbar(aes(x = 2, 
                         ymin = mean(SU.global.cv$V2) - sd(SU.global.cv$V2)),
                     ymax = mean(SU.global.cv$V2) + sd(SU.global.cv$V2),
                     width = 0) +
       geom_point(aes(x = 2, y = mean(SU.global.cv$V2)),
                  shape = 21,
                  fill = "#FF9900") +
       geom_errorbar(aes(x = 3, 
                         ymin = mean(SU.global.cv$V3) - sd(SU.global.cv$V3)),
                     ymax = mean(SU.global.cv$V3) + sd(SU.global.cv$V3),
                     width = 0) +
       geom_point(aes(x = 3, y = mean(SU.global.cv$V3)),
                  shape = 21,
                  fill = "#FF9900") +
       geom_errorbar(aes(x = 4, 
                         ymin = mean(SU.global.cv$V4) - sd(SU.global.cv$V4)),
                     ymax = mean(SU.global.cv$V4) + sd(SU.global.cv$V4),
                     width = 0) +
       geom_point(aes(x = 4, y = mean(SU.global.cv$V4)),
                  shape = 21,
                  fill = "#FF9900") +
       geom_errorbar(aes(x = 5, 
                         ymin = mean(SU.global.cv$V5) - sd(SU.global.cv$V5)),
                     ymax = mean(SU.global.cv$V5) + sd(SU.global.cv$V5),
                     width = 0) +
       geom_point(aes(x = 5, y = mean(SU.global.cv$V5)),
                  shape = 21,
                  fill = "#FF9900") +
       geom_errorbar(aes(x = 6, 
                         ymin = mean(SU.global.cv$V6) - sd(SU.global.cv$V6)),
                     ymax = mean(SU.global.cv$V6) + sd(SU.global.cv$V6),
                     width = 0) +
       geom_point(aes(x = 6, y = mean(SU.global.cv$V6)),
                  shape = 21,
                  fill = "#FF9900") +
       geom_errorbar(aes(x = 7, 
                         ymin = mean(SU.global.cv$V7) - sd(SU.global.cv$V7)),
                     ymax = mean(SU.global.cv$V7) + sd(SU.global.cv$V7),
                     width = 0) +
       geom_point(aes(x = 7, y = mean(SU.global.cv$V7)),
                  shape = 21,
                  fill = "#FF9900") +
       geom_errorbar(aes(x = 8, 
                         ymin = mean(SU.global.cv$V8) - sd(SU.global.cv$V8)),
                     ymax = mean(SU.global.cv$V8) + sd(SU.global.cv$V8),
                     width = 0) +
       geom_point(aes(x = 8, y = mean(SU.global.cv$V8)),
                  shape = 21,
                  fill = "#FF9900") +
       geom_errorbar(aes(x = 9, 
                         ymin = mean(SU.global.cv$V9) - sd(SU.global.cv$V9)),
                     ymax = mean(SU.global.cv$V9) + sd(SU.global.cv$V9),
                     width = 0) +
       geom_point(aes(x = 9, y = mean(SU.global.cv$V9)),
                  shape = 21,
                  fill = "#FF9900") +
       geom_errorbar(aes(x = 10, 
                         ymin = mean(SU.global.cv$V10) - sd(SU.global.cv$V10)),
                     ymax = mean(SU.global.cv$V10) + sd(SU.global.cv$V10),
                     width = 0) +
       geom_point(aes(x = 10, y = mean(SU.global.cv$V10)),
                  shape = 21,
                  fill = "#FF9900")

#_____________________________________________________________________________________________________________
# 5. UA cross-validation ----
#_____________________________________________________________________________________________________________

load("UA_models.RData")

summary(global.model)

#_____________________________________________________________________________________________________________
# 5a. For loop - global model ----
#_____________________________________________________________________________________________________________

# elk names
elknames.UA <- unique(elk.data.scaled$GroupID)

# data frame to hold all data
UA.global.cv <- data.frame()

for (x in elknames.UA) {
  
  group.id <- x
  
  # subset test and training data
  test.data <- elk.data.scaled %>% filter(GroupID == group.id)
  
  train.data <- elk.data.scaled %>% filter(GroupID != group.id)
  
  # fit global model to training data
  train.model.struc <- glmmTMB(Case ~
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
                               data = train.data,
                               weights = weights,
                               family = binomial,
                               doFit = FALSE)
  
  train.model.struc$parameters$theta[1] = log(1e3)
  train.model.struc$mapArg = list(theta = factor(c(NA, 1:7)))
  
  train.model <- glmmTMB:::fitTMB(train.model.struc)
  
  # calculate RSF scores on test data
  test.data <- test.data %>% mutate(RSF.score.global = exp(train.model$fit$par[2]*dDeveloped +
                                                             train.model$fit$par[3]*dRoad.P +
                                                             train.model$fit$par[4]*TRI +
                                                             train.model$fit$par[5]*TPI +
                                                             train.model$fit$par[6]*dOpen +
                                                             train.model$fit$par[7]*dMatureForest +
                                                             train.model$fit$par[8]*dYoungForest))
  
  # create scores data frame
  test.scores <- test.data %>% dplyr::select(Animal, GroupID, Case, RSF.score.global)
  
  # remove rows with NAs for RSF.score
  test.scores <- test.scores[complete.cases(test.scores$RSF.score.global), ]
  
  # create 10 quantiles of RSF scores
  cv.quant <- quantile(test.scores$RSF.score.global, probs = seq(0, 1, 0.1))
  
  # create a bin variable
  test.scores$bin <- rep(NA, length(test.scores$RSF.score.global))
  
  # for loop to classify RSF scores in 1 of 10 bins
  for (j in 1:10) {
    
    test.scores$bin[test.scores$RSF.score.global >= cv.quant[j] & test.scores$RSF.score.global < cv.quant[j + 1]] = j
    
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
  UA.global.cv <- rbind(UA.global.cv, cv.indiv)
  
}

# density plot of rho values
ggplot(UA.global.cv, aes(x = rho)) +
  theme_bw() +
  geom_density() +
  geom_vline(xintercept = mean(UA.global.cv$rho))

#_____________________________________________________________________________________________________________
# 5b. For loop - functional response model ----
#_____________________________________________________________________________________________________________

# data frame to hold all data
UA.fr.cv <- data.frame()

for (x in elknames.UA) {
  
  group.id <- x
  
  # subset availability data
  group.avail <- UA.fr %>% filter(GroupID == group.id)
  
  # subset test and training data
  test.data <- elk.data.scaled %>% filter(GroupID == group.id)
  
  # calculate RSF scores on test data
  test.data <- test.data %>% mutate(RSF.score.fr = exp(as.numeric(predict(UA.models[[1]], newdata = group.avail))*dDeveloped +
                                                       mean(UA.fr$dRoad.P)*dRoad.P +
                                                       as.numeric(predict(UA.models[[3]], newdata = group.avail))*TRI +
                                                       mean(UA.fr$TPI.x)*TPI +
                                                       as.numeric(predict(UA.models[[5]], newdata = group.avail))*dOpen +
                                                       mean(UA.fr$dMatureForest)*dMatureForest +
                                                       as.numeric(predict(UA.models[[7]], newdata = group.avail))*dYoungForest))
  
  # create scores data frame
  test.scores <- test.data %>% dplyr::select(Animal, GroupID, Case, RSF.score.fr)
  
  # remove rows with NAs for RSF.score
  test.scores <- test.scores[complete.cases(test.scores$RSF.score.fr), ]
  
  # create 10 quantiles of RSF scores
  cv.quant <- quantile(test.scores$RSF.score.fr, probs = seq(0, 1, 0.1))
  
  # create a bin variable
  test.scores$bin <- rep(NA, length(test.scores$RSF.score.fr))
  
  # for loop to classify RSF scores in 1 of 10 bins
  for (j in 1:10) {
    
    test.scores$bin[test.scores$RSF.score.fr >= cv.quant[j] & test.scores$RSF.score.fr < cv.quant[j + 1]] = j
    
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
  UA.fr.cv <- rbind(UA.fr.cv, cv.indiv)
  
}

# density plot of rho values
ggplot(UA.fr.cv, aes(x = rho)) +
  theme_bw() +
  geom_density() +
  geom_vline(xintercept = mean(UA.fr.cv$rho))

#_____________________________________________________________________________________________________________
# 5c. Increase in model performance with the addition of the functional response ----
#_____________________________________________________________________________________________________________

UA.cv.compare <- data.frame(GroupID = UA.global.cv$GroupID,
                            rho.global = UA.global.cv$rho,
                            rho.fr = UA.fr.cv$rho)

UA.cv.compare <- UA.cv.compare %>% mutate(rho.diff = rho.fr - rho.global)

# longer table for graphing
UA.cv.compare.long <- pivot_longer(UA.cv.compare, cols = c("rho.global", "rho.fr"))

# make sure factor levels are in the right order
UA.cv.compare.long$name <- factor(UA.cv.compare.long$name,
                                  levels = c("rho.global", "rho.fr"))

# plot of differences
ggplot(data = UA.cv.compare.long, aes(x = name, y = value, group = GroupID)) +
  theme_bw() +
  geom_line(aes(color = rho.diff),
            alpha = 0.5) +
  geom_point(aes(shape = name),
             alpha = 0.5)

mean(UA.cv.compare$rho.diff)
max(UA.cv.compare$rho.diff)

mean(UA.cv.compare$rho.global)
mean(UA.cv.compare$rho.fr)

# density plot
ggplot(data = UA.cv.compare.long, aes(x = value, group = name)) +
  theme_bw() +
  geom_density(aes(color = name, fill = name),
               alpha = 0.25,
               size = 1.15)

#_____________________________________________________________________________________________________________
# 5d. Plot of all aaf/bin relationships ----
#_____________________________________________________________________________________________________________

UA.global.cv.longer <- UA.global.cv %>% pivot_longer(cols = c(1:10), names_prefix = "V") %>%
                                        rename(bin = name) %>%
                                        dplyr::select(GroupID, bin, value)

# df for plotting of mean value
UA.global.mean <- UA.global.cv.longer %>% group_by(bin) %>%
                                          summarize(mean.value = mean(value))

ggplot(data = UA.global.cv.longer, aes(x = as.numeric(bin), y = value)) +
       theme_bw() +
       theme(panel.grid = element_blank()) +
       geom_line(aes(group = GroupID), 
                 alpha = 0.5, color = "gray") +
       ylab("Frequency of used locations") +
       xlab("RSF score bin") + 
       scale_x_continuous(breaks = seq(0, 10, 1)) +
       geom_line(data = UA.global.mean, aes(x = as.numeric(bin), y = mean.value),
                 color = "#FF3300",
                 size = 1.25) +
       geom_errorbar(aes(x = 1, 
                         ymin = mean(UA.global.cv$V1) - sd(UA.global.cv$V1)),
                     ymax = mean(UA.global.cv$V1) + sd(UA.global.cv$V1),
                     width = 0) +
       geom_point(aes(x = 1, y = mean(UA.global.cv$V1)),
                  shape = 21,
                  fill = "#FF3300") +
       geom_errorbar(aes(x = 2, 
                         ymin = mean(UA.global.cv$V2) - sd(UA.global.cv$V2)),
                     ymax = mean(UA.global.cv$V2) + sd(UA.global.cv$V2),
                     width = 0) +
       geom_point(aes(x = 2, y = mean(UA.global.cv$V2)),
                  shape = 21,
                  fill = "#FF3300") +
       geom_errorbar(aes(x = 3, 
                         ymin = mean(UA.global.cv$V3) - sd(UA.global.cv$V3)),
                     ymax = mean(UA.global.cv$V3) + sd(UA.global.cv$V3),
                     width = 0) +
       geom_point(aes(x = 3, y = mean(UA.global.cv$V3)),
                  shape = 21,
                  fill = "#FF3300") +
       geom_errorbar(aes(x = 4, 
                         ymin = mean(UA.global.cv$V4) - sd(UA.global.cv$V4)),
                     ymax = mean(UA.global.cv$V4) + sd(UA.global.cv$V4),
                     width = 0) +
       geom_point(aes(x = 4, y = mean(UA.global.cv$V4)),
                  shape = 21,
                  fill = "#FF3300") +
       geom_errorbar(aes(x = 5, 
                         ymin = mean(UA.global.cv$V5) - sd(UA.global.cv$V5)),
                     ymax = mean(UA.global.cv$V5) + sd(UA.global.cv$V5),
                     width = 0) +
       geom_point(aes(x = 5, y = mean(UA.global.cv$V5)),
                  shape = 21,
                  fill = "#FF3300") +
       geom_errorbar(aes(x = 6, 
                         ymin = mean(UA.global.cv$V6) - sd(UA.global.cv$V6)),
                     ymax = mean(UA.global.cv$V6) + sd(UA.global.cv$V6),
                     width = 0) +
       geom_point(aes(x = 6, y = mean(UA.global.cv$V6)),
                  shape = 21,
                  fill = "#FF3300") +
       geom_errorbar(aes(x = 7, 
                         ymin = mean(UA.global.cv$V7) - sd(UA.global.cv$V7)),
                     ymax = mean(UA.global.cv$V7) + sd(UA.global.cv$V7),
                     width = 0) +
       geom_point(aes(x = 7, y = mean(UA.global.cv$V7)),
                  shape = 21,
                  fill = "#FF3300") +
       geom_errorbar(aes(x = 8, 
                         ymin = mean(UA.global.cv$V8) - sd(UA.global.cv$V8)),
                     ymax = mean(UA.global.cv$V8) + sd(UA.global.cv$V8),
                     width = 0) +
       geom_point(aes(x = 8, y = mean(UA.global.cv$V8)),
                  shape = 21,
                  fill = "#FF3300") +
       geom_errorbar(aes(x = 9, 
                         ymin = mean(UA.global.cv$V9) - sd(UA.global.cv$V9)),
                     ymax = mean(UA.global.cv$V9) + sd(UA.global.cv$V9),
                     width = 0) +
       geom_point(aes(x = 9, y = mean(UA.global.cv$V9)),
                  shape = 21,
                  fill = "#FF3300") +
       geom_errorbar(aes(x = 10, 
                         ymin = mean(UA.global.cv$V10) - sd(UA.global.cv$V10)),
                     ymax = mean(UA.global.cv$V10) + sd(UA.global.cv$V10),
                     width = 0) +
       geom_point(aes(x = 10, y = mean(UA.global.cv$V10)),
                  shape = 21,
                  fill = "#FF3300")

#_____________________________________________________________________________________________________________
# 5e. Write to csv ----
#_____________________________________________________________________________________________________________

write.csv(UA.cv.compare, "UA_cv_compare.csv")

#_____________________________________________________________________________________________________________
# 6. AW cross-validation ----
#_____________________________________________________________________________________________________________

load("AW_models.RData")

summary(global.model)

#_____________________________________________________________________________________________________________
# 6a. For loop - global model ----
#_____________________________________________________________________________________________________________

# elk names
elknames.AW <- unique(elk.data.scaled$GroupID)

# data frame to hold all data
AW.global.cv <- data.frame()

for (x in elknames.AW) {
  
  group.id <- x
  
  # subset test and training data
  test.data <- elk.data.scaled %>% filter(GroupID == group.id)
  
  train.data <- elk.data.scaled %>% filter(GroupID != group.id)
  
  # fit global model to training data
  train.model.struc <- glmmTMB(Case ~
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
                               data = train.data,
                               weights = weights,
                               family = binomial,
                               doFit = FALSE)
  
  train.model.struc$parameters$theta[1] = log(1e3)
  train.model.struc$mapArg = list(theta = factor(c(NA, 1:7)))
  
  train.model <- glmmTMB:::fitTMB(train.model.struc)
  
  # calculate RSF scores on test data
  test.data <- test.data %>% mutate(RSF.score.global = exp(train.model$fit$par[2]*dDeveloped +
                                                             train.model$fit$par[3]*dRoad.P +
                                                             train.model$fit$par[4]*TRI +
                                                             train.model$fit$par[5]*TPI +
                                                             train.model$fit$par[6]*dOpen +
                                                             train.model$fit$par[7]*dMatureForest +
                                                             train.model$fit$par[8]*dYoungForest))
  
  # create scores data frame
  test.scores <- test.data %>% dplyr::select(Animal, GroupID, Case, RSF.score.global)
  
  # remove rows with NAs for RSF.score
  test.scores <- test.scores[complete.cases(test.scores$RSF.score.global), ]
  
  # create 10 quantiles of RSF scores
  cv.quant <- quantile(test.scores$RSF.score.global, probs = seq(0, 1, 0.1))
  
  # create a bin variable
  test.scores$bin <- rep(NA, length(test.scores$RSF.score.global))
  
  # for loop to classify RSF scores in 1 of 10 bins
  for (j in 1:10) {
    
    test.scores$bin[test.scores$RSF.score.global >= cv.quant[j] & test.scores$RSF.score.global < cv.quant[j + 1]] = j
    
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
  AW.global.cv <- rbind(AW.global.cv, cv.indiv)
  
}

# density plot of rho values
ggplot(AW.global.cv, aes(x = rho)) +
  theme_bw() +
  geom_density() +
  geom_vline(xintercept = mean(AW.global.cv$rho))

#_____________________________________________________________________________________________________________
# 6b. For loop - functional response model ----
#_____________________________________________________________________________________________________________

# data frame to hold all data
AW.fr.cv <- data.frame()

for (x in elknames.AW) {
  
  group.id <- x
  
  # subset availability data
  group.avail <- AW.fr %>% filter(GroupID == group.id)
  
  # subset test and training data
  test.data <- elk.data.scaled %>% filter(GroupID == group.id)
  
  # calculate RSF scores on test data
  test.data <- test.data %>% mutate(RSF.score.fr = exp(as.numeric(predict(AW.models[[1]], newdata = group.avail))*dDeveloped +
                                                       as.numeric(predict(AW.models[[2]], newdata = group.avail))*dRoad.P +
                                                       as.numeric(predict(AW.models[[3]], newdata = group.avail))*TRI +
                                                       as.numeric(predict(AW.models[[4]], newdata = group.avail))*TPI +
                                                       as.numeric(predict(AW.models[[5]], newdata = group.avail))*dOpen +
                                                       as.numeric(predict(AW.models[[6]], newdata = group.avail))*dMatureForest +
                                                       as.numeric(predict(AW.models[[7]], newdata = group.avail))*dYoungForest))
  
  # create scores data frame
  test.scores <- test.data %>% dplyr::select(Animal, GroupID, Case, RSF.score.fr)
  
  # remove rows with NAs for RSF.score
  test.scores <- test.scores[complete.cases(test.scores$RSF.score.fr), ]
  
  # create 10 quantiles of RSF scores
  cv.quant <- quantile(test.scores$RSF.score.fr, probs = seq(0, 1, 0.1))
  
  # create a bin variable
  test.scores$bin <- rep(NA, length(test.scores$RSF.score.fr))
  
  # for loop to classify RSF scores in 1 of 10 bins
  for (j in 1:10) {
    
    test.scores$bin[test.scores$RSF.score.fr >= cv.quant[j] & test.scores$RSF.score.fr < cv.quant[j + 1]] = j
    
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
  AW.fr.cv <- rbind(AW.fr.cv, cv.indiv)
  
}

# density plot of rho values
ggplot(AW.fr.cv, aes(x = rho)) +
  theme_bw() +
  geom_density() +
  geom_vline(xintercept = mean(AW.fr.cv$rho))

#_____________________________________________________________________________________________________________
# 6c. Increase in model performance with the addition of the functional response ----
#_____________________________________________________________________________________________________________

AW.cv.compare <- data.frame(GroupID = AW.global.cv$GroupID,
                            rho.global = AW.global.cv$rho,
                            rho.fr = AW.fr.cv$rho)

AW.cv.compare <- AW.cv.compare %>% mutate(rho.diff = rho.fr - rho.global)

# longer table for graphing
AW.cv.compare.long <- pivot_longer(AW.cv.compare, cols = c("rho.global", "rho.fr"))

# make sure factor levels are in the right order
AW.cv.compare.long$name <- factor(AW.cv.compare.long$name,
                                  levels = c("rho.global", "rho.fr"))

# plot of differences
ggplot(data = AW.cv.compare.long, aes(x = name, y = value, group = GroupID)) +
  theme_bw() +
  geom_line(aes(color = rho.diff),
            alpha = 0.5) +
  geom_point(aes(shape = name),
             alpha = 0.5)

mean(AW.cv.compare$rho.diff)
max(AW.cv.compare$rho.diff)

mean(AW.cv.compare$rho.global)
mean(AW.cv.compare$rho.fr)

# density plot
ggplot(data = AW.cv.compare.long, aes(x = value, group = name)) +
  theme_bw() +
  geom_density(aes(color = name, fill = name),
               alpha = 0.25,
               size = 1.15)

#_____________________________________________________________________________________________________________
# 6d. Plot of all aaf/bin relationships ----
#_____________________________________________________________________________________________________________

AW.global.cv.longer <- AW.global.cv %>% pivot_longer(cols = c(1:10), names_prefix = "V") %>%
                                        rename(bin = name) %>%
                                        dplyr::select(GroupID, bin, value)

# df for plotting of mean value
AW.global.mean <- AW.global.cv.longer %>% group_by(bin) %>%
                                          summarize(mean.value = mean(value))

ggplot(data = AW.global.cv.longer, aes(x = as.numeric(bin), y = value)) +
       theme_bw() +
       theme(panel.grid = element_blank()) +
       geom_line(aes(group = GroupID), 
                 alpha = 0.5, color = "gray") +
       ylab("Frequency of used locations") +
       xlab("RSF score bin") + 
       scale_x_continuous(breaks = seq(0, 10, 1)) +
       geom_line(data = AW.global.mean, aes(x = as.numeric(bin), y = mean.value),
                 color = "#003399",
                 size = 1.25) +
       geom_errorbar(aes(x = 1, 
                         ymin = mean(AW.global.cv$V1) - sd(AW.global.cv$V1)),
                     ymax = mean(AW.global.cv$V1) + sd(AW.global.cv$V1),
                     width = 0) +
       geom_point(aes(x = 1, y = mean(AW.global.cv$V1)),
                  shape = 21,
                  fill = "#003399") +
       geom_errorbar(aes(x = 2, 
                         ymin = mean(AW.global.cv$V2) - sd(AW.global.cv$V2)),
                     ymax = mean(AW.global.cv$V2) + sd(AW.global.cv$V2),
                     width = 0) +
       geom_point(aes(x = 2, y = mean(AW.global.cv$V2)),
                  shape = 21,
                  fill = "#003399") +
       geom_errorbar(aes(x = 3, 
                         ymin = mean(AW.global.cv$V3) - sd(AW.global.cv$V3)),
                     ymax = mean(AW.global.cv$V3) + sd(AW.global.cv$V3),
                     width = 0) +
       geom_point(aes(x = 3, y = mean(AW.global.cv$V3)),
                  shape = 21,
                  fill = "#003399") +
       geom_errorbar(aes(x = 4, 
                         ymin = mean(AW.global.cv$V4) - sd(AW.global.cv$V4)),
                     ymax = mean(AW.global.cv$V4) + sd(AW.global.cv$V4),
                     width = 0) +
       geom_point(aes(x = 4, y = mean(AW.global.cv$V4)),
                  shape = 21,
                  fill = "#003399") +
       geom_errorbar(aes(x = 5, 
                         ymin = mean(AW.global.cv$V5) - sd(AW.global.cv$V5)),
                     ymax = mean(AW.global.cv$V5) + sd(AW.global.cv$V5),
                     width = 0) +
       geom_point(aes(x = 5, y = mean(AW.global.cv$V5)),
                  shape = 21,
                  fill = "#003399") +
       geom_errorbar(aes(x = 6, 
                         ymin = mean(AW.global.cv$V6) - sd(AW.global.cv$V6)),
                     ymax = mean(AW.global.cv$V6) + sd(AW.global.cv$V6),
                     width = 0) +
       geom_point(aes(x = 6, y = mean(AW.global.cv$V6)),
                  shape = 21,
                  fill = "#003399") +
       geom_errorbar(aes(x = 7, 
                         ymin = mean(AW.global.cv$V7) - sd(AW.global.cv$V7)),
                     ymax = mean(AW.global.cv$V7) + sd(AW.global.cv$V7),
                     width = 0) +
       geom_point(aes(x = 7, y = mean(AW.global.cv$V7)),
                  shape = 21,
                  fill = "#003399") +
       geom_errorbar(aes(x = 8, 
                         ymin = mean(AW.global.cv$V8) - sd(AW.global.cv$V8)),
                     ymax = mean(AW.global.cv$V8) + sd(AW.global.cv$V8),
                     width = 0) +
       geom_point(aes(x = 8, y = mean(AW.global.cv$V8)),
                  shape = 21,
                  fill = "#003399") +
       geom_errorbar(aes(x = 9, 
                         ymin = mean(AW.global.cv$V9) - sd(AW.global.cv$V9)),
                     ymax = mean(AW.global.cv$V9) + sd(AW.global.cv$V9),
                     width = 0) +
       geom_point(aes(x = 9, y = mean(AW.global.cv$V9)),
                  shape = 21,
                  fill = "#003399") +
       geom_errorbar(aes(x = 10, 
                         ymin = mean(AW.global.cv$V10) - sd(AW.global.cv$V10)),
                     ymax = mean(AW.global.cv$V10) + sd(AW.global.cv$V10),
                     width = 0) +
       geom_point(aes(x = 10, y = mean(AW.global.cv$V10)),
                  shape = 21,
                  fill = "#003399")

#_____________________________________________________________________________________________________________
# 6e. Write to csv ----
#_____________________________________________________________________________________________________________

write.csv(AW.cv.compare, "AW_cv_compare.csv")

#_____________________________________________________________________________________________________________
# 7. CV plots ----
#_____________________________________________________________________________________________________________

# WS
WS.cv.plot <- ggplot(data = WS.global.cv.longer, aes(x = as.numeric(bin), y = value)) +
       theme_bw() +
       theme(panel.grid = element_blank(),
             plot.margin = margin(-10, 0, -10, 0),
             plot.title = element_text(vjust = -8, hjust = 0.05)) +
       geom_line(aes(group = GroupID), 
                 alpha = 0.5, color = "gray") +
       ggtitle("a") +
       ylab("Frequency of used locations") +
       xlab("") + 
       scale_x_continuous(breaks = seq(0, 10, 2)) +
       scale_y_continuous(breaks = seq(0, 5, 1)) +
       coord_cartesian(ylim = c(0, 5)) +
       geom_line(data = WS.global.mean, aes(x = as.numeric(bin), y = mean.value),
                 color = "#009900",
                 size = 1.25) +
       geom_errorbar(aes(x = 1, 
                     ymin = mean(WS.global.cv$V1) - sd(WS.global.cv$V1)),
                     ymax = mean(WS.global.cv$V1) + sd(WS.global.cv$V1),
                     width = 0) +
       geom_point(aes(x = 1, y = mean(WS.global.cv$V1)),
                  shape = 21,
                  fill = "#009900") +
       geom_errorbar(aes(x = 2, 
                         ymin = mean(WS.global.cv$V2) - sd(WS.global.cv$V2)),
                     ymax = mean(WS.global.cv$V2) + sd(WS.global.cv$V2),
                     width = 0) +
       geom_point(aes(x = 2, y = mean(WS.global.cv$V2)),
                  shape = 21,
                  fill = "#009900") +
       geom_errorbar(aes(x = 3, 
                         ymin = mean(WS.global.cv$V3) - sd(WS.global.cv$V3)),
                     ymax = mean(WS.global.cv$V3) + sd(WS.global.cv$V3),
                     width = 0) +
       geom_point(aes(x = 3, y = mean(WS.global.cv$V3)),
                  shape = 21,
                  fill = "#009900") +
       geom_errorbar(aes(x = 4, 
                         ymin = mean(WS.global.cv$V4) - sd(WS.global.cv$V4)),
                     ymax = mean(WS.global.cv$V4) + sd(WS.global.cv$V4),
                     width = 0) +
       geom_point(aes(x = 4, y = mean(WS.global.cv$V4)),
                  shape = 21,
                  fill = "#009900") +
       geom_errorbar(aes(x = 5, 
                         ymin = mean(WS.global.cv$V5) - sd(WS.global.cv$V5)),
                     ymax = mean(WS.global.cv$V5) + sd(WS.global.cv$V5),
                     width = 0) +
       geom_point(aes(x = 5, y = mean(WS.global.cv$V5)),
                  shape = 21,
                  fill = "#009900") +
       geom_errorbar(aes(x = 6, 
                         ymin = mean(WS.global.cv$V6) - sd(WS.global.cv$V6)),
                     ymax = mean(WS.global.cv$V6) + sd(WS.global.cv$V6),
                     width = 0) +
       geom_point(aes(x = 6, y = mean(WS.global.cv$V6)),
                  shape = 21,
                  fill = "#009900") +
       geom_errorbar(aes(x = 7, 
                         ymin = mean(WS.global.cv$V7) - sd(WS.global.cv$V7)),
                     ymax = mean(WS.global.cv$V7) + sd(WS.global.cv$V7),
                     width = 0) +
       geom_point(aes(x = 7, y = mean(WS.global.cv$V7)),
                  shape = 21,
                  fill = "#009900") +
       geom_errorbar(aes(x = 8, 
                         ymin = mean(WS.global.cv$V8) - sd(WS.global.cv$V8)),
                     ymax = mean(WS.global.cv$V8) + sd(WS.global.cv$V8),
                     width = 0) +
       geom_point(aes(x = 8, y = mean(WS.global.cv$V8)),
                  shape = 21,
                  fill = "#009900") +
       geom_errorbar(aes(x = 9, 
                         ymin = mean(WS.global.cv$V9) - sd(WS.global.cv$V9)),
                     ymax = mean(WS.global.cv$V9) + sd(WS.global.cv$V9),
                     width = 0) +
       geom_point(aes(x = 9, y = mean(WS.global.cv$V9)),
                  shape = 21,
                  fill = "#009900") +
       geom_errorbar(aes(x = 10, 
                         ymin = mean(WS.global.cv$V10) - sd(WS.global.cv$V10)),
                     ymax = mean(WS.global.cv$V10) + sd(WS.global.cv$V10),
                     width = 0) +
       geom_point(aes(x = 10, y = mean(WS.global.cv$V10)),
                  shape = 21,
                  fill = "#009900")

# SU
SU.cv.plot <- ggplot(data = SU.global.cv.longer, aes(x = as.numeric(bin), y = value)) +
       theme_bw() +
       theme(panel.grid = element_blank(),
             plot.margin = margin(-10, 0, -10, -5),
             plot.title = element_text(vjust = -8, hjust = 0.05)) +
       geom_line(aes(group = GroupID), 
                 alpha = 0.5, color = "gray") +
       ggtitle("b") +
       ylab("") +
       xlab("") + 
       scale_x_continuous(breaks = seq(0, 10, 2)) +
       scale_y_continuous(breaks = seq(0, 5, 1)) +
       coord_cartesian(ylim = c(0, 5)) +
       geom_line(data = SU.global.mean, aes(x = as.numeric(bin), y = mean.value),
                 color = "#FF9900",
                 size = 1.25) +
       geom_errorbar(aes(x = 1, 
                         ymin = mean(SU.global.cv$V1) - sd(SU.global.cv$V1)),
                     ymax = mean(SU.global.cv$V1) + sd(SU.global.cv$V1),
                     width = 0) +
       geom_point(aes(x = 1, y = mean(SU.global.cv$V1)),
                  shape = 21,
                  fill = "#FF9900") +
       geom_errorbar(aes(x = 2, 
                         ymin = mean(SU.global.cv$V2) - sd(SU.global.cv$V2)),
                     ymax = mean(SU.global.cv$V2) + sd(SU.global.cv$V2),
                     width = 0) +
       geom_point(aes(x = 2, y = mean(SU.global.cv$V2)),
                  shape = 21,
                  fill = "#FF9900") +
       geom_errorbar(aes(x = 3, 
                         ymin = mean(SU.global.cv$V3) - sd(SU.global.cv$V3)),
                     ymax = mean(SU.global.cv$V3) + sd(SU.global.cv$V3),
                     width = 0) +
       geom_point(aes(x = 3, y = mean(SU.global.cv$V3)),
                  shape = 21,
                  fill = "#FF9900") +
       geom_errorbar(aes(x = 4, 
                         ymin = mean(SU.global.cv$V4) - sd(SU.global.cv$V4)),
                     ymax = mean(SU.global.cv$V4) + sd(SU.global.cv$V4),
                     width = 0) +
       geom_point(aes(x = 4, y = mean(SU.global.cv$V4)),
                  shape = 21,
                  fill = "#FF9900") +
       geom_errorbar(aes(x = 5, 
                         ymin = mean(SU.global.cv$V5) - sd(SU.global.cv$V5)),
                     ymax = mean(SU.global.cv$V5) + sd(SU.global.cv$V5),
                     width = 0) +
       geom_point(aes(x = 5, y = mean(SU.global.cv$V5)),
                  shape = 21,
                  fill = "#FF9900") +
       geom_errorbar(aes(x = 6, 
                         ymin = mean(SU.global.cv$V6) - sd(SU.global.cv$V6)),
                     ymax = mean(SU.global.cv$V6) + sd(SU.global.cv$V6),
                     width = 0) +
       geom_point(aes(x = 6, y = mean(SU.global.cv$V6)),
                  shape = 21,
                  fill = "#FF9900") +
       geom_errorbar(aes(x = 7, 
                         ymin = mean(SU.global.cv$V7) - sd(SU.global.cv$V7)),
                     ymax = mean(SU.global.cv$V7) + sd(SU.global.cv$V7),
                     width = 0) +
       geom_point(aes(x = 7, y = mean(SU.global.cv$V7)),
                  shape = 21,
                  fill = "#FF9900") +
       geom_errorbar(aes(x = 8, 
                         ymin = mean(SU.global.cv$V8) - sd(SU.global.cv$V8)),
                     ymax = mean(SU.global.cv$V8) + sd(SU.global.cv$V8),
                     width = 0) +
       geom_point(aes(x = 8, y = mean(SU.global.cv$V8)),
                  shape = 21,
                  fill = "#FF9900") +
       geom_errorbar(aes(x = 9, 
                         ymin = mean(SU.global.cv$V9) - sd(SU.global.cv$V9)),
                     ymax = mean(SU.global.cv$V9) + sd(SU.global.cv$V9),
                     width = 0) +
       geom_point(aes(x = 9, y = mean(SU.global.cv$V9)),
                  shape = 21,
                  fill = "#FF9900") +
       geom_errorbar(aes(x = 10, 
                         ymin = mean(SU.global.cv$V10) - sd(SU.global.cv$V10)),
                     ymax = mean(SU.global.cv$V10) + sd(SU.global.cv$V10),
                     width = 0) +
       geom_point(aes(x = 10, y = mean(SU.global.cv$V10)),
                  shape = 21,
                  fill = "#FF9900")

UA.cv.plot <- ggplot(data = UA.global.cv.longer, aes(x = as.numeric(bin), y = value)) +
       theme_bw() +
       theme(panel.grid = element_blank(),
             plot.margin = margin(-15, 0, 0, 0),
             plot.title = element_text(vjust = -8, hjust = 0.05)) +
       geom_line(aes(group = GroupID), 
                 alpha = 0.5, color = "gray") +
       ggtitle("c") +
       ylab("Frequency of used locations") +
       xlab("RSF score bin") + 
       scale_x_continuous(breaks = seq(0, 10, 2)) +
       scale_y_continuous(breaks = seq(0, 5, 1)) +
       coord_cartesian(ylim = c(0, 5)) +
       geom_line(data = UA.global.mean, aes(x = as.numeric(bin), y = mean.value),
                 color = "#FF3300",
                 size = 1.25) +
       geom_errorbar(aes(x = 1, 
                         ymin = mean(UA.global.cv$V1) - sd(UA.global.cv$V1)),
                     ymax = mean(UA.global.cv$V1) + sd(UA.global.cv$V1),
                     width = 0) +
       geom_point(aes(x = 1, y = mean(UA.global.cv$V1)),
                  shape = 21,
                  fill = "#FF3300") +
       geom_errorbar(aes(x = 2, 
                         ymin = mean(UA.global.cv$V2) - sd(UA.global.cv$V2)),
                     ymax = mean(UA.global.cv$V2) + sd(UA.global.cv$V2),
                     width = 0) +
       geom_point(aes(x = 2, y = mean(UA.global.cv$V2)),
                  shape = 21,
                  fill = "#FF3300") +
       geom_errorbar(aes(x = 3, 
                         ymin = mean(UA.global.cv$V3) - sd(UA.global.cv$V3)),
                     ymax = mean(UA.global.cv$V3) + sd(UA.global.cv$V3),
                     width = 0) +
       geom_point(aes(x = 3, y = mean(UA.global.cv$V3)),
                  shape = 21,
                  fill = "#FF3300") +
       geom_errorbar(aes(x = 4, 
                         ymin = mean(UA.global.cv$V4) - sd(UA.global.cv$V4)),
                     ymax = mean(UA.global.cv$V4) + sd(UA.global.cv$V4),
                     width = 0) +
       geom_point(aes(x = 4, y = mean(UA.global.cv$V4)),
                  shape = 21,
                  fill = "#FF3300") +
       geom_errorbar(aes(x = 5, 
                         ymin = mean(UA.global.cv$V5) - sd(UA.global.cv$V5)),
                     ymax = mean(UA.global.cv$V5) + sd(UA.global.cv$V5),
                     width = 0) +
       geom_point(aes(x = 5, y = mean(UA.global.cv$V5)),
                  shape = 21,
                  fill = "#FF3300") +
       geom_errorbar(aes(x = 6, 
                         ymin = mean(UA.global.cv$V6) - sd(UA.global.cv$V6)),
                     ymax = mean(UA.global.cv$V6) + sd(UA.global.cv$V6),
                     width = 0) +
       geom_point(aes(x = 6, y = mean(UA.global.cv$V6)),
                  shape = 21,
                  fill = "#FF3300") +
       geom_errorbar(aes(x = 7, 
                         ymin = mean(UA.global.cv$V7) - sd(UA.global.cv$V7)),
                     ymax = mean(UA.global.cv$V7) + sd(UA.global.cv$V7),
                     width = 0) +
       geom_point(aes(x = 7, y = mean(UA.global.cv$V7)),
                  shape = 21,
                  fill = "#FF3300") +
       geom_errorbar(aes(x = 8, 
                         ymin = mean(UA.global.cv$V8) - sd(UA.global.cv$V8)),
                     ymax = mean(UA.global.cv$V8) + sd(UA.global.cv$V8),
                     width = 0) +
       geom_point(aes(x = 8, y = mean(UA.global.cv$V8)),
                  shape = 21,
                  fill = "#FF3300") +
       geom_errorbar(aes(x = 9, 
                         ymin = mean(UA.global.cv$V9) - sd(UA.global.cv$V9)),
                     ymax = mean(UA.global.cv$V9) + sd(UA.global.cv$V9),
                     width = 0) +
       geom_point(aes(x = 9, y = mean(UA.global.cv$V9)),
                  shape = 21,
                  fill = "#FF3300") +
       geom_errorbar(aes(x = 10, 
                         ymin = mean(UA.global.cv$V10) - sd(UA.global.cv$V10)),
                     ymax = mean(UA.global.cv$V10) + sd(UA.global.cv$V10),
                     width = 0) +
       geom_point(aes(x = 10, y = mean(UA.global.cv$V10)),
                  shape = 21,
                  fill = "#FF3300")

# AW
AW.cv.plot <- ggplot(data = AW.global.cv.longer, aes(x = as.numeric(bin), y = value)) +
               theme_bw() +
               theme(panel.grid = element_blank(),
                     plot.margin = margin(-15, 0, 0, -5),
                     plot.title = element_text(vjust = -8, hjust = 0.05)) +
               geom_line(aes(group = GroupID), 
                         alpha = 0.5, color = "gray") +
               ggtitle("d") +
               ylab("") +
               xlab("RSF score bin") + 
               scale_x_continuous(breaks = seq(0, 10, 2)) +
               scale_y_continuous(breaks = seq(0, 5, 1)) +
               coord_cartesian(ylim = c(0, 5)) +
               geom_line(data = AW.global.mean, aes(x = as.numeric(bin), y = mean.value),
                         color = "#003399",
                         size = 1.25) +
               geom_errorbar(aes(x = 1, 
                                 ymin = mean(AW.global.cv$V1) - sd(AW.global.cv$V1)),
                             ymax = mean(AW.global.cv$V1) + sd(AW.global.cv$V1),
                             width = 0) +
               geom_point(aes(x = 1, y = mean(AW.global.cv$V1)),
                          shape = 21,
                          fill = "#003399") +
               geom_errorbar(aes(x = 2, 
                                 ymin = mean(AW.global.cv$V2) - sd(AW.global.cv$V2)),
                             ymax = mean(AW.global.cv$V2) + sd(AW.global.cv$V2),
                             width = 0) +
               geom_point(aes(x = 2, y = mean(AW.global.cv$V2)),
                          shape = 21,
                          fill = "#003399") +
               geom_errorbar(aes(x = 3, 
                                 ymin = mean(AW.global.cv$V3) - sd(AW.global.cv$V3)),
                             ymax = mean(AW.global.cv$V3) + sd(AW.global.cv$V3),
                             width = 0) +
               geom_point(aes(x = 3, y = mean(AW.global.cv$V3)),
                          shape = 21,
                          fill = "#003399") +
               geom_errorbar(aes(x = 4, 
                                 ymin = mean(AW.global.cv$V4) - sd(AW.global.cv$V4)),
                             ymax = mean(AW.global.cv$V4) + sd(AW.global.cv$V4),
                             width = 0) +
               geom_point(aes(x = 4, y = mean(AW.global.cv$V4)),
                          shape = 21,
                          fill = "#003399") +
               geom_errorbar(aes(x = 5, 
                                 ymin = mean(AW.global.cv$V5) - sd(AW.global.cv$V5)),
                             ymax = mean(AW.global.cv$V5) + sd(AW.global.cv$V5),
                             width = 0) +
               geom_point(aes(x = 5, y = mean(AW.global.cv$V5)),
                          shape = 21,
                          fill = "#003399") +
               geom_errorbar(aes(x = 6, 
                                 ymin = mean(AW.global.cv$V6) - sd(AW.global.cv$V6)),
                             ymax = mean(AW.global.cv$V6) + sd(AW.global.cv$V6),
                             width = 0) +
               geom_point(aes(x = 6, y = mean(AW.global.cv$V6)),
                          shape = 21,
                          fill = "#003399") +
               geom_errorbar(aes(x = 7, 
                                 ymin = mean(AW.global.cv$V7) - sd(AW.global.cv$V7)),
                             ymax = mean(AW.global.cv$V7) + sd(AW.global.cv$V7),
                             width = 0) +
               geom_point(aes(x = 7, y = mean(AW.global.cv$V7)),
                          shape = 21,
                          fill = "#003399") +
               geom_errorbar(aes(x = 8, 
                                 ymin = mean(AW.global.cv$V8) - sd(AW.global.cv$V8)),
                             ymax = mean(AW.global.cv$V8) + sd(AW.global.cv$V8),
                             width = 0) +
               geom_point(aes(x = 8, y = mean(AW.global.cv$V8)),
                          shape = 21,
                          fill = "#003399") +
               geom_errorbar(aes(x = 9, 
                                 ymin = mean(AW.global.cv$V9) - sd(AW.global.cv$V9)),
                             ymax = mean(AW.global.cv$V9) + sd(AW.global.cv$V9),
                             width = 0) +
               geom_point(aes(x = 9, y = mean(AW.global.cv$V9)),
                          shape = 21,
                          fill = "#003399") +
               geom_errorbar(aes(x = 10, 
                                 ymin = mean(AW.global.cv$V10) - sd(AW.global.cv$V10)),
                             ymax = mean(AW.global.cv$V10) + sd(AW.global.cv$V10),
                             width = 0) +
               geom_point(aes(x = 10, y = mean(AW.global.cv$V10)),
                          shape = 21,
                          fill = "#003399")

# plot together
plot_grid(WS.cv.plot, SU.cv.plot, UA.cv.plot, AW.cv.plot, nrow = 2, ncol = 2, rel_widths = c(1, 1, 1, 1))

#_____________________________________________________________________________________________________________
# 9. Distributions of CV scores ----
#_____________________________________________________________________________________________________________

# add season column
WS.cv.compare$Season <- "WS"
SU.cv.compare$Season <- "SU"
UA.cv.compare$Season <- "UA"
AW.cv.compare$Season <- "AW"

# bind together
all.cv.compare <- rbind(WS.cv.compare, SU.cv.compare, UA.cv.compare, AW.cv.compare)

# order factor levels correctly
all.cv.compare$Season <- factor(all.cv.compare$Season,
                                levels = c("WS", "SU", "UA", "AW"),
                                labels = c("Feb-Apr", "May-Jul", "Aug-Oct", "Nov-Jan"))

rho.mean <- plyr::ddply(all.cv.compare, "Season", summarize, grp.mean = mean(rho.global))

# facetted plot - global model CV
ggplot(data = all.cv.compare, aes(x = rho.global, fill = Season)) +
       theme_bw() +
       theme(panel.grid = element_blank(),
             legend.position = "none",
             legend.title = element_blank(),
             axis.text.x = element_text(angle = 270, vjust = 0.12)) +
       facet_wrap(~Season) + 
       geom_histogram(binwidth = 0.1, color = "white") +
       scale_fill_manual(values = c("#009900", "#FF9900", "#FF3300", "#003399")) +
       ylab("") +
       xlab(expression("Cross-validation score " (r[s]))) +
       scale_x_continuous(breaks = seq(-0.5, 1.0, 0.3)) +
       geom_vline(data = rho.mean, 
                  aes(xintercept = grp.mean), 
                  linetype = "dashed")

#_____________________________________________________________________________________________________________
# 8. Save to file ----
#_____________________________________________________________________________________________________________

save.image("cross_val.RData")
