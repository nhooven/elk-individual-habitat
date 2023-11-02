# Title: Individual variation in habitat selection
# Subtitle: 11 - CV score histograms
# Author: Nathan D. Hooven
# Email: nathan.hooven@uky.edu
# Affiliation: Department of Forestry and Natural Resources, University of Kentucky
# Date began: 22 Jul 2023
# Date completed: 22 Jul 2023
# Date modified: 
# R version: 3.6.2

#_____________________________________________________________________________________________________________
# 1. Load in required packages ----
#_____________________________________________________________________________________________________________

library(tidyverse)

#_____________________________________________________________________________________________________________
# 2. Read in data ----
#_____________________________________________________________________________________________________________

WS <- read.csv("WS_pergroup.csv")
SU <- read.csv("SU_pergroup.csv")
UA <- read.csv("UA_pergroup.csv")
AW <- read.csv("AW_pergroup.csv")

#_____________________________________________________________________________________________________________
# 3. Manipulate and bind together ----
#_____________________________________________________________________________________________________________

# select only columns we need
WS.1 <- WS %>% dplyr::select(GroupID, Season, rho.global, rho.fr, rho.clust)
SU.1 <- SU %>% dplyr::select(GroupID, Season, rho.global, rho.fr, rho.clust)
UA.1 <- UA %>% dplyr::select(GroupID, Season, rho.global, rho.fr, rho.clust)
AW.1 <- AW %>% dplyr::select(GroupID, Season, rho.global, rho.fr, rho.clust)

# make sure GroupID is a character
WS.1$GroupID <- as.character(WS.1$GroupID)
SU.1$GroupID <- as.character(SU.1$GroupID)
UA.1$GroupID <- as.character(UA.1$GroupID)
AW.1$GroupID <- as.character(AW.1$GroupID)

all.data <- bind_rows(WS.1, SU.1, UA.1, AW.1)

# pivot
all.data.pivot <- all.data %>% pivot_longer(cols = 3:5)

#_____________________________________________________________________________________________________________
# 4. Facetted plot ----
#_____________________________________________________________________________________________________________

# rearrange and name factors
all.data.pivot$Season <- factor(all.data.pivot$Season,
                                levels = c("WS", "SU", "UA", "AW"),
                                labels = c("Feb-Apr", "May-Jul", "Aug-Oct", "Nov-Jan"))

all.data.pivot$name <- factor(all.data.pivot$name,
                              levels = c("rho.global", "rho.fr", "rho.clust"),
                              labels = c("Global", "Functional", "Cluster"))

ggplot(data = all.data.pivot,
       aes(x = value,
           fill = Season)) +
  theme_bw() +
  facet_grid(Season ~ name,
             scales = "free_y") +
  geom_histogram(binwidth = 0.1,
                 color = "white",
                 boundary = 1) +
  theme(panel.grid = element_blank(),
        legend.position = "none") +
  scale_fill_manual(values = c("#009900", 
                               "#FF9900", 
                               "#FF3300", 
                               "#003399")) +
  xlab(expression("Cross-validation score " (r[s]))) +
  ylab("Count")
