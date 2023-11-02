# Title: Individual variation in habitat selection
# Subtitle: 07 - Cluster visualization
# Author: Nathan D. Hooven
# Email: nathan.hooven@uky.edu
# Affiliation: Department of Forestry and Natural Resources, University of Kentucky
# Date began: 30 Jun 2022
# Date completed: 12 Jul 2023
# Date modified: 12 Jul 2023
# R version: 3.6.2

#_____________________________________________________________________________________________________________
# 1. Load in required packages ----
#_____________________________________________________________________________________________________________

library(tidyverse)
library(ggridges)      # ridgeline plots

#_____________________________________________________________________________________________________________
# 2. Read in data ----
#_____________________________________________________________________________________________________________

WS.data <- read.csv("WS_sampled_RS_IM_4.csv")
SU.data <- read.csv("SU_sampled_RS_IM_4.csv")
UA.data <- read.csv("UA_sampled_RS_IM_4.csv")
AW.data <- read.csv("AW_sampled_RS_IM_4.csv")

# add "Season" column
WS.data$Season <- "WS"
SU.data$Season <- "SU"
UA.data$Season <- "UA"
AW.data$Season <- "AW"

#_____________________________________________________________________________________________________________
# 3. RS ----
#_____________________________________________________________________________________________________________

# pivot_longer
WS.data.RS.longer <- WS.data %>% dplyr::select(GroupID, dDeveloped, dRoad.P, TRI.x, TPI.x,
                                               dOpen, dMatureForest, dYoungForest, clust) %>%
  mutate(Season = "WS") %>%
  pivot_longer(cols = 2:8)

SU.data.RS.longer <- SU.data %>% dplyr::select(GroupID, dDeveloped, dRoad.P, TRI.x, TPI.x,
                                               dOpen, dMatureForest, dYoungForest, clust) %>%
  mutate(Season = "SU") %>%
  pivot_longer(cols = 2:8)

UA.data.RS.longer <- UA.data %>% dplyr::select(GroupID, dDeveloped, dRoad.P, TRI.x, TPI.x,
                                               dOpen, dMatureForest, dYoungForest, clust) %>%
  mutate(Season = "UA") %>%
  pivot_longer(cols = 2:8)

AW.data.RS.longer <- AW.data %>% dplyr::select(GroupID, dDeveloped, dRoad.P, TRI.x, TPI.x,
                                               dOpen, dMatureForest, dYoungForest, clust) %>%
  mutate(Season = "AW") %>%
  pivot_longer(cols = 2:8)

# bind together
data.RS.longer <- rbind(WS.data.RS.longer, SU.data.RS.longer, UA.data.RS.longer, AW.data.RS.longer)

# reorder factors
data.RS.longer$name <- factor(data.RS.longer$name,
                              levels = c("dDeveloped", "dRoad.P", "TRI.x", "TPI.x",
                                         "dOpen", "dMatureForest", "dYoungForest"),
                              labels = c("dDeveloped", "ln(dRoad)", "TRI", "TPI",
                                         "dOpen", "dMatureForest", "dYoungForest"))

data.RS.longer$Season <- factor(data.RS.longer$Season,
                                levels = c("WS", "SU", "UA", "AW"),
                                labels = c("Feb-Apr", "May-Jul", "Aug-Oct", "Nov-Jan"))

# points
ggplot(data = data.RS.longer,
       aes(x = as.factor(clust),
           y = value,
           color = Season,
           shape = as.factor(clust))) +
  theme_bw() +
  geom_hline(yintercept = 0) +
  facet_grid(rows = vars(Season),
             cols = vars(name),
             scales = "free_y") +
  geom_point(alpha = 0.25) +
  theme(panel.grid = element_blank(),
        legend.position = "none",
        strip.background = element_rect(fill = "white")) +
  xlab("Behavioral cluster") +
  ylab("Standardized selection coefficient") +
  scale_color_manual(values = c("#009900", "#FF9900", "#FF3300", "#003399"))

# ridges
ggplot(data = data.RS.longer,
       aes(y = as.factor(clust),
           x = value,
           color = Season,
           fill = Season,
           shape = as.factor(clust),
           linetype = as.factor(clust))) +
  theme_bw() +
  annotate("rect",
           xmin = -Inf,
           xmax = Inf,
           ymin = 2,
           ymax = 3,
           alpha = 0.10) +
  geom_vline(xintercept = 0) +
  facet_grid(rows = vars(Season),
             cols = vars(name),
             scales = "free") +
  geom_density_ridges(size = 0.75,
                      alpha = 0.25,
                      scale = 0.7) +
  theme(panel.grid = element_blank(),
        legend.position = "none",
        strip.background = element_rect(fill = "white"),
        axis.text.y = element_text(vjust = -1),
        axis.ticks.y = element_blank()) +
  ylab("Cluster") +
  xlab("Standardized selection coefficient") +
  scale_color_manual(values = c("#009900", "#FF9900", "#FF3300", "#003399")) +
  scale_fill_manual(values = c("#009900", "#FF9900", "#FF3300", "#003399")) +
  scale_linetype_manual(values = c("solid", "dashed", "dotted")) +
  scale_y_discrete(limits = rev)
