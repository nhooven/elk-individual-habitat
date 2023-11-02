# Title: Individual variation in habitat selection
# Subtitle: Random slope visualization
# Author: Nathan D. Hooven
# Email: nathan.d.hooven@gmail.com
# Affiliation: Department of Forestry and Natural Resources, University of Kentucky
# Date began: 19 Nov 2020
# Date completed: 19 Nov 2020
# Date modified: 22 Mar 2022
# R version: 3.6.2

#_____________________________________________________________________________________________________________
# 1. Load in required packages ----
#_____________________________________________________________________________________________________________

library(tidyverse)
library(ggplot2)          # visualization

#_____________________________________________________________________________________________________________
# 2. Read in data ----
#_____________________________________________________________________________________________________________

WS.slopes <- read.csv("randomslopes_WS_plot.csv")
SU.slopes <- read.csv("randomslopes_SU_plot.csv")
UA.slopes <- read.csv("randomslopes_UA_plot.csv")
AW.slopes <- read.csv("randomslopes_AW_plot.csv")

#_____________________________________________________________________________________________________________
# 3. Remove intercept variable and reorder factor levels ----
#_____________________________________________________________________________________________________________

# WS
WS.slopes <- WS.slopes[WS.slopes$variable != "X.Intercept.",]

WS.slopes$variable <- factor(WS.slopes$variable, levels = c("dYoungForest",
                                                            "dMatureForest",
                                                            "dOpen",
                                                            "TPI",
                                                            "TRI",
                                                            "dRoad.P",
                                                            "dDeveloped"))

# SU
SU.slopes <- SU.slopes[SU.slopes$variable != "X.Intercept.",]

SU.slopes$variable <- factor(SU.slopes$variable, levels = c("dYoungForest",
                                                            "dMatureForest",
                                                            "dOpen",
                                                            "TPI",
                                                            "TRI",
                                                            "dRoad.P",
                                                            "dDeveloped"))

# UA
UA.slopes <- UA.slopes[UA.slopes$variable != "X.Intercept.",]

UA.slopes$variable <- factor(UA.slopes$variable, levels = c("dYoungForest",
                                                            "dMatureForest",
                                                            "dOpen",
                                                            "TPI",
                                                            "TRI",
                                                            "dRoad.P",
                                                            "dDeveloped"))

# AW
AW.slopes <- AW.slopes[AW.slopes$variable != "X.Intercept.",]

AW.slopes$variable <- factor(AW.slopes$variable, levels = c("dYoungForest",
                                                            "dMatureForest",
                                                            "dOpen",
                                                            "TPI",
                                                            "TRI",
                                                            "dRoad.P",
                                                            "dDeveloped"))

#_____________________________________________________________________________________________________________
# 4. Make plots ----
#_____________________________________________________________________________________________________________
# 4a. WS ----
#_____________________________________________________________________________________________________________

ggplot(data = WS.slopes, aes(x = estimate, y = variable)) +
       theme_bw() +
       geom_vline(xintercept = 0) +
       geom_point(aes(group = GroupID),
                  color = "#009900",
                  alpha = 0.25,
                  size = 4) +
       xlab("Standardized selection coefficient") +
       ylab("") +
       scale_x_continuous(breaks = c(seq(-5, 3, 1))) +
       coord_cartesian(xlim = c(-5, 3)) +
       theme(panel.grid = element_blank()) +
       geom_errorbarh(aes(xmin = 0.058 - 1.960*0.046, 
                          xmax = 0.058 + 1.960*0.046, 
                          y = 7),
                      height = 0, 
                      inherit.aes = FALSE) +
       geom_point(aes(x = 0.058, y = 7), 
                  size = 4, 
                  shape = 21, 
                  fill = "white", 
                  inherit.aes = FALSE) +
       geom_errorbarh(aes(xmin = 0.521 - 1.960*0.052, 
                          xmax = 0.521 + 1.960*0.052, 
                          y = 6),
                      height = 0, 
                      inherit.aes = FALSE) +
       geom_point(aes(x = 0.521, y = 6), 
                  size = 4, 
                  shape = 21, 
                  fill = "white", 
                  inherit.aes = FALSE) +
      geom_errorbarh(aes(xmin = -0.279 - 1.960*0.033, 
                         xmax = -0.279 + 1.960*0.033, 
                         y = 5),
                     height = 0, 
                     inherit.aes = FALSE) +
      geom_point(aes(x = -0.279, y = 5), 
                 size = 4, 
                 shape = 21, 
                 fill = "white", 
                 inherit.aes = FALSE) +
      geom_errorbarh(aes(xmin = 0.466 - 1.960*0.030, 
                         xmax = 0.466 + 1.960*0.030, 
                         y = 4),
                     height = 0, 
                     inherit.aes = FALSE) +
      geom_point(aes(x = 0.466, y = 4), 
                 size = 4, 
                 shape = 21, 
                 fill = "white", 
                 inherit.aes = FALSE) +
      geom_errorbarh(aes(xmin = -0.809 - 1.960*0.109, 
                         xmax = -0.809 + 1.960*0.109, 
                         y = 3),
                     height = 0, 
                     inherit.aes = FALSE) +
      geom_point(aes(x = -0.809, y = 3), 
                 size = 4, 
                 shape = 21, 
                 fill = "white", 
                 inherit.aes = FALSE) +
      geom_errorbarh(aes(xmin = 0.063 - 1.960*0.028, 
                         xmax = 0.063 + 1.960*0.028, 
                         y = 2),
                     height = 0, 
                     inherit.aes = FALSE) +
      geom_point(aes(x = 0.063, y = 2), 
                 size = 4, 
                 shape = 21, 
                 fill = "white", 
                 inherit.aes = FALSE) +
      geom_errorbarh(aes(xmin = -0.836 - 1.960*0.103, 
                         xmax = -0.836 + 1.960*0.103, 
                         y = 1),
                     height = 0, 
                     inherit.aes = FALSE) +
      geom_point(aes(x = -0.836, y = 1), 
                 size = 4, 
                 shape = 21, 
                 fill = "white", 
                 inherit.aes = FALSE)
     
#_____________________________________________________________________________________________________________
# 4b. SU ----
#_____________________________________________________________________________________________________________

ggplot(data = SU.slopes, aes(x = estimate, y = variable)) +
       theme_bw() +
       geom_vline(xintercept = 0) +
       geom_point(aes(group = GroupID),
                  color = "#FF9900",
                  alpha = 0.25,
                  size = 4) +
       xlab("Standardized selection coefficient") +
       ylab("") +
       scale_x_continuous(breaks = c(seq(-5, 3, 1))) +
       coord_cartesian(xlim = c(-5, 3)) +
       theme(panel.grid = element_blank()) +
       geom_errorbarh(aes(xmin = 0.029 - 1.960*0.049, 
                          xmax = 0.029 + 1.960*0.049, 
                          y = 7),
                      height = 0, 
                      inherit.aes = FALSE) +
       geom_point(aes(x = 0.029, y = 7), 
                  size = 4, 
                  shape = 21, 
                  fill = "white", 
                  inherit.aes = FALSE) +
       geom_errorbarh(aes(xmin = 0.630 - 1.960*0.074, 
                          xmax = 0.630 + 1.960*0.074, 
                          y = 6),
                      height = 0, 
                      inherit.aes = FALSE) +
       geom_point(aes(x = 0.630, y = 6), 
                  size = 4, 
                  shape = 21, 
                  fill = "white", 
                  inherit.aes = FALSE) +
       geom_errorbarh(aes(xmin = -0.218 - 1.960*0.027, 
                          xmax = -0.218 + 1.960*0.027, 
                          y = 5),
                      height = 0, 
                      inherit.aes = FALSE) +
       geom_point(aes(x = -0.218, y = 5), 
                  size = 4, 
                  shape = 21, 
                  fill = "white", 
                  inherit.aes = FALSE) +
       geom_errorbarh(aes(xmin = 0.257 - 1.960*0.028, 
                          xmax = 0.257 + 1.960*0.028, 
                          y = 4),
                      height = 0, 
                      inherit.aes = FALSE) +
       geom_point(aes(x = 0.257, y = 4), 
                  size = 4, 
                  shape = 21, 
                  fill = "white", 
                  inherit.aes = FALSE) +
       geom_errorbarh(aes(xmin = -0.776 - 1.960*0.082, 
                          xmax = -0.776 + 1.960*0.082, 
                          y = 3),
                      height = 0, 
                      inherit.aes = FALSE) +
       geom_point(aes(x = -0.776, y = 3), 
                  size = 4, 
                  shape = 21, 
                  fill = "white", 
                  inherit.aes = FALSE) +
       geom_errorbarh(aes(xmin = 0.099 - 1.960*0.027, 
                          xmax = 0.099 + 1.960*0.027, 
                          y = 2),
                      height = 0, 
                      inherit.aes = FALSE) +
       geom_point(aes(x = 0.099, y = 2), 
                  size = 4, 
                  shape = 21, 
                  fill = "white", 
                  inherit.aes = FALSE) +
       geom_errorbarh(aes(xmin = -0.574 - 1.960*0.071, 
                          xmax = -0.574 + 1.960*0.071, 
                          y = 1),
                      height = 0, 
                      inherit.aes = FALSE) +
       geom_point(aes(x = -0.574, y = 1), 
                  size = 4, 
                  shape = 21, 
                  fill = "white", 
                  inherit.aes = FALSE)

#_____________________________________________________________________________________________________________
# 4c. UA ----
#_____________________________________________________________________________________________________________

ggplot(data = UA.slopes, aes(x = estimate, y = variable)) +
       theme_bw() +
       geom_vline(xintercept = 0) +
       geom_point(aes(group = GroupID),
                  color = "#FF3300",
                  alpha = 0.25,
                  size = 4) +
       xlab("Standardized selection coefficient") +
       ylab("") +
       scale_x_continuous(breaks = c(seq(-5, 3, 1))) +
       coord_cartesian(xlim = c(-5, 3)) +
       theme(panel.grid = element_blank()) +
       geom_errorbarh(aes(xmin = -0.047 - 1.960*0.048, 
                          xmax = -0.047 + 1.960*0.048, 
                          y = 7),
                      height = 0, 
                      inherit.aes = FALSE) +
       geom_point(aes(x = -0.047, y = 7), 
                  size = 4, 
                  shape = 21, 
                  fill = "white", 
                  inherit.aes = FALSE) +
       geom_errorbarh(aes(xmin = 0.717 - 1.960*0.086, 
                          xmax = 0.717 + 1.960*0.086, 
                          y = 6),
                      height = 0, 
                      inherit.aes = FALSE) +
       geom_point(aes(x = 0.717, y = 6), 
                  size = 4, 
                  shape = 21, 
                  fill = "white", 
                  inherit.aes = FALSE) +
       geom_errorbarh(aes(xmin = -0.347 - 1.960*0.039, 
                          xmax = -0.347 + 1.960*0.039, 
                          y = 5),
                      height = 0, 
                      inherit.aes = FALSE) +
       geom_point(aes(x = -0.347, y = 5), 
                  size = 4, 
                  shape = 21, 
                  fill = "white", 
                  inherit.aes = FALSE) +
       geom_errorbarh(aes(xmin = 0.377 - 1.960*0.030, 
                          xmax = 0.377 + 1.960*0.030, 
                          y = 4),
                      height = 0, 
                      inherit.aes = FALSE) +
       geom_point(aes(x = 0.377, y = 4), 
                  size = 4, 
                  shape = 21, 
                  fill = "white", 
                  inherit.aes = FALSE) +
       geom_errorbarh(aes(xmin = -0.619 - 1.960*0.096, 
                          xmax = -0.619 + 1.960*0.096, 
                          y = 3),
                      height = 0, 
                      inherit.aes = FALSE) +
       geom_point(aes(x = -0.619, y = 3), 
                  size = 4, 
                  shape = 21, 
                  fill = "white", 
                  inherit.aes = FALSE) +
       geom_errorbarh(aes(xmin = 0.018 - 1.960*0.035, 
                          xmax = 0.018 + 1.960*0.035, 
                          y = 2),
                      height = 0, 
                      inherit.aes = FALSE) +
       geom_point(aes(x = 0.018, y = 2), 
                  size = 4, 
                  shape = 21, 
                  fill = "white", 
                  inherit.aes = FALSE) +
       geom_errorbarh(aes(xmin = -1.110 - 1.960*0.160, 
                          xmax = -1.110 + 1.960*0.160, 
                          y = 1),
                      height = 0, 
                      inherit.aes = FALSE) +
       geom_point(aes(x = -1.110, y = 1), 
                  size = 4, 
                  shape = 21, 
                  fill = "white", 
                  inherit.aes = FALSE)

#_____________________________________________________________________________________________________________
# 5. Facetted plot ----
#_____________________________________________________________________________________________________________

# bind data frames together
WS.slopes$season <- "WS"
SU.slopes$season <- "SU"
UA.slopes$season <- "UA"
AW.slopes$season <- "AW"

WS.slopes$GroupID <- as.character(WS.slopes$GroupID)

all.slopes <- rbind(WS.slopes, SU.slopes, UA.slopes, AW.slopes)

# reorder factor levels
all.slopes$season <- factor(all.slopes$season,
                            levels = c("WS", "SU", "UA", "AW"),
                            labels = c("Feb-Apr", "May-Jul",
                                       "Aug-Oct", "Nov-Dec"))

all.slopes$variable <- factor(all.slopes$variable,
                              levels = c("dDeveloped", "dRoad.P", "TRI",
                                         "TPI", "dOpen", "dMatureForest", "dYoungForest"),
                              labels = c("dDeveloped", "ln(dRoad)", "TRI",
                                         "TPI", "dOpen", "dMatureForest", "dYoungForest"))

# horizontal
ggplot(data = all.slopes, aes(y = season, x = estimate)) +
       geom_vline(xintercept = 0) +
       geom_point(aes(color = season), size = 3, alpha = 0.25) +
       theme_bw() +
       theme(panel.grid = element_blank(),
             axis.text.y = element_blank(),
             axis.ticks.y = element_blank(),
             legend.position = "top") +
       facet_wrap(~ variable, ncol = 2) +
       scale_y_discrete(limits = c("UA", "SU", "WS")) +
       scale_x_continuous(limits = c(-4.5, 3)) +
       xlab("Standardized selection coefficient") +
       ylab("") +
       scale_color_manual(values = c("#009900", "#FF9900", "#FF3300", "#003399"))

# vertical
ggplot(data = all.slopes, aes(y = estimate, x = season)) +
   geom_hline(yintercept = 0) +
   geom_point(aes(color = season), size = 3, alpha = 0.25) +
   theme_bw() +
   theme(panel.grid = element_blank(),
         axis.text.x = element_blank(),
         axis.ticks.x = element_blank(),
         legend.position = "top") +
   facet_wrap(~ variable, ncol = 4) +
   scale_x_discrete(limits = c("WS", "SU", "UA", "AW")) +
   scale_y_continuous(limits = c(-4.5, 3)) +
   ylab("Standardized selection coefficient") +
   xlab("") +
   scale_color_manual(values = c("#009900", "#FF9900", "#FF3300", "#003399"))

#_____________________________________________________________________________________________________________
# 6. Grouped plot ----
#_____________________________________________________________________________________________________________

ggplot(data = all.slopes, aes(y = estimate, 
                              x = variable, 
                              group = season)) +
       geom_hline(yintercept = 0) +
       geom_point(aes(color = season, 
                      shape = season, 
                      size = season), alpha = 0.25,
                  position = position_dodge(width = 0.5)) +
       scale_shape_manual(values = c(16, 15, 17, 18)) +
       scale_size_manual(values = c(3, 3, 3, 3.5)) +
       theme_bw() +
       theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
             legend.position = "top") +
       scale_y_continuous(limits = c(-4.5, 3)) +
       ylab("Standardized selection coefficient") +
       xlab("") +
       scale_color_manual(values = c("#009900", "#FF9900", "#FF3300", "#003399")) +
       # WS fixed effects
       geom_errorbar(aes(ymin = 0.055 - 1.960*0.046, 
                          ymax = 0.055 + 1.960*0.046, 
                          x = 0.80),
                      width = 0, 
                      inherit.aes = FALSE) +
       geom_point(aes(y = 0.055, x = 0.80), 
                  size = 2, 
                  shape = 21, 
                  fill = "white", 
                  inherit.aes = FALSE) +
       geom_errorbar(aes(ymin = 0.530 - 1.960*0.053, 
                          ymax = 0.530 + 1.960*0.053, 
                          x = 1.8),
                      width = 0, 
                      inherit.aes = FALSE) +
       geom_point(aes(y = 0.530, x = 1.8), 
                  size = 2, 
                  shape = 21, 
                  fill = "white", 
                  inherit.aes = FALSE) +
       geom_errorbar(aes(ymin = -0.284 - 1.960*0.033, 
                          ymax = -0.284 + 1.960*0.033, 
                          x = 2.8),
                      width = 0, 
                      inherit.aes = FALSE) +
       geom_point(aes(y = -0.284, x = 2.8), 
                  size = 2, 
                  shape = 21, 
                  fill = "white", 
                  inherit.aes = FALSE) +
       geom_errorbar(aes(ymin = 0.466 - 1.960*0.031, 
                          ymax = 0.466 + 1.960*0.031, 
                          x = 3.8),
                      width = 0, 
                      inherit.aes = FALSE) +
       geom_point(aes(y = 0.466, x = 3.8), 
                  size = 2, 
                  shape = 21, 
                  fill = "white", 
                  inherit.aes = FALSE) +
       geom_errorbar(aes(ymin = -0.816 - 1.960*0.112, 
                          ymax = -0.816 + 1.960*0.112, 
                          x = 4.8),
                      width = 0, 
                      inherit.aes = FALSE) +
       geom_point(aes(y = -0.816, x = 4.8), 
                  size = 2, 
                  shape = 21, 
                  fill = "white", 
                  inherit.aes = FALSE) +
       geom_errorbar(aes(ymin = 0.063 - 1.960*0.029, 
                          ymax = 0.063 + 1.960*0.029, 
                          x = 5.8),
                      width = 0, 
                      inherit.aes = FALSE) +
       geom_point(aes(y = 0.063, x = 5.8), 
                  size = 2, 
                  shape = 21, 
                  fill = "white", 
                  inherit.aes = FALSE) +
       geom_errorbar(aes(ymin = -0.836 - 1.960*0.106, 
                          ymax = -0.836 + 1.960*0.106, 
                          x = 6.8),
                      width = 0, 
                      inherit.aes = FALSE) +
       geom_point(aes(y = -0.836, x = 6.8), 
                  size = 2, 
                  shape = 21, 
                  fill = "white", 
                  inherit.aes = FALSE) +
      # SU fixed effects
      geom_errorbar(aes(ymin = 0.029 - 1.960*0.049, 
                         ymax = 0.029 + 1.960*0.049, 
                         x = 0.93),
                     width = 0, 
                     inherit.aes = FALSE) +
      geom_point(aes(y = 0.029, x = 0.93), 
                 size = 2, 
                 shape = 22, 
                 fill = "white", 
                 inherit.aes = FALSE) +
      geom_errorbar(aes(ymin = 0.630 - 1.960*0.074, 
                         ymax = 0.630 + 1.960*0.074, 
                         x = 1.93),
                     width = 0, 
                     inherit.aes = FALSE) +
      geom_point(aes(y = 0.630, x = 1.93), 
                 size = 2, 
                 shape = 22, 
                 fill = "white", 
                 inherit.aes = FALSE) +
      geom_errorbar(aes(ymin = -0.218 - 1.960*0.027, 
                         ymax = -0.218 + 1.960*0.027, 
                         x = 2.93),
                     width = 0, 
                     inherit.aes = FALSE) +
      geom_point(aes(y = -0.218, x = 2.93), 
                 size = 2, 
                 shape = 22, 
                 fill = "white", 
                 inherit.aes = FALSE) +
      geom_errorbar(aes(ymin = 0.257 - 1.960*0.028, 
                         ymax = 0.257 + 1.960*0.028, 
                         x = 3.93),
                     width = 0, 
                     inherit.aes = FALSE) +
      geom_point(aes(y = 0.257, x = 3.93), 
                 size = 2, 
                 shape = 22, 
                 fill = "white", 
                 inherit.aes = FALSE) +
      geom_errorbar(aes(ymin = -0.776 - 1.960*0.082, 
                         ymax = -0.776 + 1.960*0.082, 
                         x = 4.93),
                     width = 0, 
                     inherit.aes = FALSE) +
      geom_point(aes(y = -0.776, x = 4.93), 
                 size = 2, 
                 shape = 22, 
                 fill = "white", 
                 inherit.aes = FALSE) +
      geom_errorbar(aes(ymin = 0.099 - 1.960*0.027, 
                         ymax = 0.099 + 1.960*0.027, 
                         x = 5.93),
                     width = 0, 
                     inherit.aes = FALSE) +
      geom_point(aes(y = 0.099, x = 5.93), 
                 size = 2, 
                 shape = 22, 
                 fill = "white", 
                 inherit.aes = FALSE) +
      geom_errorbar(aes(ymin = -0.574 - 1.960*0.071, 
                         ymax = -0.574 + 1.960*0.071, 
                         x = 6.93),
                     width = 0, 
                     inherit.aes = FALSE) +
      geom_point(aes(y = -0.574, x = 6.93), 
                 size = 2, 
                 shape = 22, 
                 fill = "white", 
                 inherit.aes = FALSE) +
      # UA fixed effects
      geom_errorbar(aes(ymin = -0.047 - 1.960*0.048, 
                         ymax = -0.047 + 1.960*0.048, 
                         x = 1.07),
                     width = 0, 
                     inherit.aes = FALSE) +
      geom_point(aes(y = -0.047, x = 1.07), 
                 size = 2, 
                 shape = 24, 
                 fill = "white", 
                 inherit.aes = FALSE) +
      geom_errorbar(aes(ymin = 0.717 - 1.960*0.086, 
                         ymax = 0.717 + 1.960*0.086, 
                         x = 2.07),
                     width = 0, 
                     inherit.aes = FALSE) +
      geom_point(aes(y = 0.717, x = 2.07), 
                 size = 2, 
                 shape = 24, 
                 fill = "white", 
                 inherit.aes = FALSE) +
      geom_errorbar(aes(ymin = -0.347 - 1.960*0.039, 
                         ymax = -0.347 + 1.960*0.039, 
                         x = 3.07),
                     width = 0, 
                     inherit.aes = FALSE) +
      geom_point(aes(y = -0.347, x = 3.07), 
                 size = 2, 
                 shape = 24, 
                 fill = "white", 
                 inherit.aes = FALSE) +
      geom_errorbar(aes(ymin = 0.377 - 1.960*0.030, 
                         ymax = 0.377 + 1.960*0.030, 
                         x = 4.07),
                     width = 0, 
                     inherit.aes = FALSE) +
      geom_point(aes(y = 0.377, x = 4.07), 
                 size = 2, 
                 shape = 24, 
                 fill = "white", 
                 inherit.aes = FALSE) +
      geom_errorbar(aes(ymin = -0.619 - 1.960*0.096, 
                         ymax = -0.619 + 1.960*0.096, 
                         x = 5.07),
                     width = 0, 
                     inherit.aes = FALSE) +
      geom_point(aes(y = -0.619, x = 5.07), 
                 size = 2, 
                 shape = 24, 
                 fill = "white", 
                 inherit.aes = FALSE) +
      geom_errorbar(aes(ymin = 0.018 - 1.960*0.035, 
                         ymax = 0.018 + 1.960*0.035, 
                         x = 6.07),
                     width = 0, 
                     inherit.aes = FALSE) +
      geom_point(aes(y = 0.018, x = 6.07), 
                 size = 2, 
                 shape = 24, 
                 fill = "white", 
                 inherit.aes = FALSE) +
      geom_errorbar(aes(ymin = -1.110 - 1.960*0.160, 
                         ymax = -1.110 + 1.960*0.160, 
                         x = 7.07),
                     width = 0, 
                     inherit.aes = FALSE) +
      geom_point(aes(y = -1.110, x = 7.07), 
                 size = 2, 
                 shape = 24, 
                 fill = "white", 
                 inherit.aes = FALSE) +
      # AW fixed effects
      geom_errorbar(aes(ymin = -0.081 - 1.960*0.047, 
                         ymax = -0.081 + 1.960*0.047, 
                         x = 1.19),
                     width = 0, 
                     inherit.aes = FALSE) +
      geom_point(aes(y = -0.081, x = 1.19), 
                 size = 2, 
                 shape = 23, 
                 fill = "white", 
                 inherit.aes = FALSE) +
      geom_errorbar(aes(ymin = 0.368 - 1.960*0.071, 
                         ymax = 0.368 + 1.960*0.071, 
                         x = 2.19),
                     width = 0, 
                     inherit.aes = FALSE) +
      geom_point(aes(y = 0.368, x = 2.19), 
                 size = 2, 
                 shape = 23, 
                 fill = "white", 
                 inherit.aes = FALSE) +
      geom_errorbar(aes(ymin = -0.323 - 1.960*0.038, 
                         ymax = -0.323 + 1.960*0.038, 
                         x = 3.19),
                     width = 0, 
                     inherit.aes = FALSE) +
      geom_point(aes(y = -0.323, x = 3.19), 
                 size = 2, 
                 shape = 23, 
                 fill = "white", 
                 inherit.aes = FALSE) +
      geom_errorbar(aes(ymin = 0.508 - 1.960*0.035, 
                         ymax = 0.508 + 1.960*0.035, 
                         x = 4.19),
                     width = 0, 
                     inherit.aes = FALSE) +
      geom_point(aes(y = 0.508, x = 4.19), 
                 size = 2, 
                 shape = 23, 
                 fill = "white", 
                 inherit.aes = FALSE) +
      geom_errorbar(aes(ymin = -0.761 - 1.960*0.098, 
                         ymax = -0.761 + 1.960*0.098, 
                         x = 5.19),
                     width = 0, 
                     inherit.aes = FALSE) +
      geom_point(aes(y = -0.761, x = 5.19), 
                 size = 2, 
                 shape = 23, 
                 fill = "white", 
                 inherit.aes = FALSE) +
      geom_errorbar(aes(ymin = 0.101 - 1.960*0.03, 
                         ymax = 0.101 + 1.960*0.03, 
                         x = 6.19),
                     width = 0, 
                     inherit.aes = FALSE) +
      geom_point(aes(y = 0.101, x = 6.19), 
                 size = 2, 
                 shape = 23, 
                 fill = "white", 
                 inherit.aes = FALSE) +
      geom_errorbar(aes(ymin = -0.406 - 1.960*0.070, 
                         ymax = -0.406 + 1.960*0.070, 
                         x = 7.19),
                     width = 0, 
                     inherit.aes = FALSE) +
      geom_point(aes(y = -0.406, x = 7.19), 
                 size = 2, 
                 shape = 23, 
                 fill = "white", 
                 inherit.aes = FALSE)

# w 644 h 467

ggplot(data = all.slopes, aes(y = estimate, x = variable, group = season)) +
       geom_hline(yintercept = 0) +
       geom_point(aes(color = season, 
                      shape = season, 
                      size = season),
                  position = position_dodge(width = 0.5)) +
       scale_shape_manual(values = c(16, 15, 17, 18)) +
       scale_size_manual(values = c(3, 3, 3, 3.5)) +
       theme_bw() +
       theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
             legend.position = "top",
             legend.title = element_blank()) +
       scale_y_continuous(limits = c(-4.5, 3)) +
       ylab("Standardized selection coefficient") +
       xlab("") +
       scale_color_manual(values = c("#009900", "#FF9900", "#FF3300", "#003399"))