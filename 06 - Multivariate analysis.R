# Title: Individual variation in habitat selection
# Subtitle: 6 - Multivariate analysis
# Author: Nathan D. Hooven
# Email: nathan.d.hooven@gmail.com
# Affiliation: Department of Forestry and Natural Resources, University of Kentucky
# Date began: 14 Jan 2022
# Date completed: 15 Feb 2022
# Date modified: 22 Mar 2022
# R version: 3.6.2

#_____________________________________________________________________________________________________________
# 1. Load in required packages ----
#_____________________________________________________________________________________________________________

library(tidyverse)
library(factoextra)       # PCA visualization
library(NbClust)          # determine optimal number of clusters
library(cowplot)          # multiple plots

set.seed(123)

#_____________________________________________________________________________________________________________
# 2. Read in data ----
#_____________________________________________________________________________________________________________

WS.fr <- read.csv("WS_fr.csv")
SU.fr <- read.csv("SU_fr.csv")
UA.fr <- read.csv("UA_fr.csv")
AW.fr <- read.csv("AW_fr.csv")

#_____________________________________________________________________________________________________________
# 3. W/S ----

# remove 101955 and 101965
WS.fr <- WS.fr %>% filter(GroupID %notin% c(101955, 101965))

#_____________________________________________________________________________________________________________
# 3a. PCA ----
#_____________________________________________________________________________________________________________

# simple correlations
cor(x = as.matrix(WS.fr[ ,c(3:9)]), method = "pearson")

# fit PCA without scaling (variables are already on the same scale)
WS.pca <- prcomp(WS.fr[ ,c(3:9)], scale = FALSE)

write.table(WS.pca$rotation, "clipboard", sep = "\t")

# variance explained
WS.pca.eig <- get_eig(WS.pca)

write.table(WS.pca.eig$variance.percent, "clipboard", sep = "\t")

# scree plot
fviz_eig(WS.pca)

# variable plot
fviz_pca_var(WS.pca, repel = TRUE)

#_____________________________________________________________________________________________________________
# 3b. Determine optimal number of clusters via k-means clustering ----
#_____________________________________________________________________________________________________________

WS.nb.clust.opt <- NbClust(WS.fr[ ,c(3:9)], distance = "euclidean", method = "kmeans", max.nc = 15)

WS.opt.clust <- 3

# run k-means clustering
WS.kmeans <- kmeans(x = WS.fr[ ,c(3:9)], centers = WS.opt.clust)

#_____________________________________________________________________________________________________________
# # 3c. Visualize in lower dimensional space
#_____________________________________________________________________________________________________________

# bind PCs 1 and 2
WS.fr$PC1.slope <- get_pca_ind(WS.pca)$coord[ ,1]
WS.fr$PC2.slope <- get_pca_ind(WS.pca)$coord[ ,2]

# bind kmeans clusters
WS.fr$clust <- WS.kmeans$cluster

# visualize
WS.clust.plot <- ggplot(WS.fr, aes(PC1.slope, PC2.slope)) +
                        theme_bw() +
                        geom_vline(xintercept = 0, linetype = "dashed") +
                        geom_hline(yintercept = 0, linetype = "dashed") +
                        geom_point(aes(shape = as.factor(clust),
                                       fill = as.factor(clust)),
                                   size = 4) +
                         scale_shape_manual(values = c(21, 22, 24)) +
                         scale_fill_manual(values = c("#009900", "palegreen1", "white")) +
                         xlab("PC1 (42.3%)") +
                         ylab("PC2 (35.1%)") +
                         theme(legend.title = element_blank(),
                               legend.position = c(0.83, 0.93),
                               legend.margin = margin(c(1, 5, 1, 1)),
                               legend.spacing.x = unit(0, "pt"),
                               panel.grid = element_blank(),
                               legend.box.background = element_rect(colour = "black"),
                               legend.direction = "horizontal") +
                         guides(fill = guide_legend(override.aes = list(size = 3))) +
                         ggtitle("a")

# distance matrix
WS.dist <- as.data.frame(as.matrix(dist(x = WS.fr[ ,c(3:9)], method = "euclidean")))

# add col and rownames
rownames(WS.dist) <- WS.fr$GroupID
colnames(WS.dist) <- WS.fr$GroupID

#_____________________________________________________________________________________________________________
# 4. S/U ----
#_____________________________________________________________________________________________________________
# 4a. PCA ----
#_____________________________________________________________________________________________________________

# simple correlations
cor(x = as.matrix(SU.fr[ ,c(3:9)]), method = "pearson")

# fit PCA without scaling (variables are already on the same scale)
SU.pca <- prcomp(SU.fr[ ,c(3:9)], scale = FALSE)

write.table(SU.pca$rotation, "clipboard", sep = "\t")

# variance explained
SU.pca.eig <- get_eig(SU.pca)

write.table(SU.pca.eig$variance.percent, "clipboard", sep = "\t")

# scree plot
fviz_eig(SU.pca)

# variable plot
fviz_pca_var(SU.pca, repel = TRUE)

#_____________________________________________________________________________________________________________
# 4b. Determine optimal number of clusters via k-means clustering ----
#_____________________________________________________________________________________________________________

SU.nb.clust.opt <- NbClust(SU.fr[ ,c(3:9)], distance = "euclidean", method = "kmeans", max.nc = 15)

SU.opt.clust <- 2

# run k-means clustering
SU.kmeans <- kmeans(x = SU.fr[ ,c(3:9)], centers = SU.opt.clust)

#_____________________________________________________________________________________________________________
# # 4c. Visualize in lower dimensional space
#_____________________________________________________________________________________________________________

# bind PCs 1 and 2
SU.fr$PC1.slope <- get_pca_ind(SU.pca)$coord[ ,1]
SU.fr$PC2.slope <- get_pca_ind(SU.pca)$coord[ ,2]

# bind best clusters
SU.fr$clust <- SU.kmeans$cluster

# visualize
SU.clust.plot <- ggplot(SU.fr, aes(PC1.slope, PC2.slope)) +
                        theme_bw() +
                        geom_vline(xintercept = 0, linetype = "dashed") +
                        geom_hline(yintercept = 0, linetype = "dashed") +
                        geom_point(aes(shape = as.factor(clust),
                                       fill = as.factor(clust)),
                                   size = 4) +
                         scale_shape_manual(values = c(21, 22)) +
                         scale_fill_manual(values = c("#FF9900", "white")) +
                         xlab("PC1 (36.4%)") +
                         ylab("PC2 (24.0%)") +
                         theme(legend.title = element_blank(),
                               legend.position = c(0.87, 0.93),
                               legend.margin = margin(c(1, 5, 1, 1)),
                               legend.spacing.x = unit(0, "pt"),
                               legend.direction = "horizontal",
                               panel.grid = element_blank(),
                               legend.box.background = element_rect(colour = "black")) +
                         guides(fill = guide_legend(override.aes = list(size = 3))) +
                         ggtitle("b")

# distance matrix
SU.dist <- as.data.frame(as.matrix(dist(x = SU.fr[ ,c(3:9)], method = "euclidean")))

# add col and rownames
rownames(SU.dist) <- SU.fr$GroupID
colnames(SU.dist) <- SU.fr$GroupID

#_____________________________________________________________________________________________________________
# 5. U/A ----
#_____________________________________________________________________________________________________________
# 5a. PCA ----
#_____________________________________________________________________________________________________________

# simple correlations
cor(x = as.matrix(UA.fr[ ,c(3:9)]), method = "pearson")

# fit PCA without scaling (variables are already on the same scale)
UA.pca <- prcomp(UA.fr[ ,c(3:9)], scale = FALSE)

write.table(UA.pca$rotation, "clipboard", sep = "\t")

# variance explained
UA.pca.eig <- get_eig(UA.pca)

write.table(UA.pca.eig$variance.percent, "clipboard", sep = "\t")

# scree plot
fviz_eig(UA.pca)

# variable plot
fviz_pca_var(UA.pca, repel = TRUE)

#_____________________________________________________________________________________________________________
# 5b. Determine optimal number of clusters via k-means clustering ----
#_____________________________________________________________________________________________________________

UA.nb.clust.opt <- NbClust(UA.fr[ ,c(3:9)], distance = "euclidean", method = "kmeans", max.nc = 15)

UA.opt.clust <- 2

# run k-means clustering
UA.kmeans <- kmeans(x = UA.fr[ ,c(3:9)], centers = UA.opt.clust)

#_____________________________________________________________________________________________________________
# 5c. Visualize in lower dimensional space
#_____________________________________________________________________________________________________________

# bind PCs 1 and 2
UA.fr$PC1.slope <- get_pca_ind(UA.pca)$coord[ ,1]
UA.fr$PC2.slope <- get_pca_ind(UA.pca)$coord[ ,2]

# bind best clusters
UA.fr$clust <- UA.kmeans$cluster

# visualize
UA.clust.plot <- ggplot(UA.fr, aes(PC1.slope, PC2.slope)) +
                        theme_bw() +
                        geom_vline(xintercept = 0, linetype = "dashed") +
                        geom_hline(yintercept = 0, linetype = "dashed") +
                        geom_point(aes(shape = as.factor(clust),
                                       fill = as.factor(clust)),
                                   size = 4) +
                         scale_shape_manual(values = c(21, 22)) +
                         scale_fill_manual(values = c("#FF3300", "white")) +
                         xlab("PC1 (59.3%)") +
                         ylab("PC2 (18.0%)") +
                         theme(legend.title = element_blank(),
                               legend.position = c(0.87, 0.93),
                               legend.margin = margin(c(1, 5, 1, 1)),
                               legend.spacing.x = unit(0, "pt"),
                               legend.direction = "horizontal",
                               panel.grid = element_blank(),
                               legend.box.background = element_rect(colour = "black")) +
                         guides(fill = guide_legend(override.aes = list(size = 3))) +
                         ggtitle("c")

# distance matrix
UA.dist <- as.data.frame(as.matrix(dist(x = UA.fr[ ,c(3:9)], method = "euclidean")))

# add col and rownames
rownames(UA.dist) <- UA.fr$GroupID
colnames(UA.dist) <- UA.fr$GroupID

#_____________________________________________________________________________________________________________
# 6. A/W ----
#_____________________________________________________________________________________________________________
# 6a. PCA ----
#_____________________________________________________________________________________________________________

# simple correlations
cor(x = as.matrix(AW.fr[ ,c(3:9)]), method = "pearson")

# fit PCA without scaling (variables are already on the same scale)
AW.pca <- prcomp(AW.fr[ ,c(3:9)], scale = FALSE)

write.table(AW.pca$rotation, "clipboard", sep = "\t")

# variance explained
AW.pca.eig <- get_eig(AW.pca)

write.table(AW.pca.eig$variance.percent, "clipboard", sep = "\t")

# scree plot
fviz_eig(AW.pca)

# variable plot
fviz_pca_var(AW.pca, repel = TRUE)

#_____________________________________________________________________________________________________________
# 6b. Determine optimal number of clusters via k-means clustering ----
#_____________________________________________________________________________________________________________

AW.nb.clust.opt <- NbClust(AW.fr[ ,c(3:9)], distance = "euclidean", method = "kmeans", max.nc = 15)

AW.opt.clust <- 3

# run k-means clustering
AW.kmeans <- kmeans(x = AW.fr[ ,c(3:9)], centers = AW.opt.clust)

#_____________________________________________________________________________________________________________
# 6c. Visualize in lower dimensional space ----
#_____________________________________________________________________________________________________________

# bind PCs 1 and 2
AW.fr$PC1.slope <- get_pca_ind(AW.pca)$coord[ ,1]
AW.fr$PC2.slope <- get_pca_ind(AW.pca)$coord[ ,2]

# bind best clusters
AW.fr$clust <- AW.kmeans$cluster

# visualize
AW.clust.plot <- ggplot(AW.fr, aes(PC1.slope, PC2.slope)) +
                        theme_bw() +
                        geom_vline(xintercept = 0, linetype = "dashed") +
                        geom_hline(yintercept = 0, linetype = "dashed") +
                        geom_point(aes(shape = as.factor(clust),
                                       fill = as.factor(clust)),
                                   size = 4) +
                         scale_shape_manual(values = c(21, 22, 24)) +
                         scale_fill_manual(values = c("#003399", "deepskyblue", "white")) +
                         xlab("PC1 (41.2%)") +
                         ylab("PC2 (25.0%)") +
                         theme(legend.title = element_blank(),
                               legend.position = c(0.83, 0.93),
                               legend.margin = margin(c(1, 5, 1, 1)),
                               legend.spacing.x = unit(0, "pt"),
                               legend.direction = "horizontal",
                               panel.grid = element_blank(),
                               legend.box.background = element_rect(colour = "black")) +
                         guides(fill = guide_legend(override.aes = list(size = 3))) +
                         ggtitle("d")
 
# distance matrix
AW.dist <- as.data.frame(as.matrix(dist(x = AW.fr[ ,c(3:9)], method = "euclidean")))

# add col and rownames
rownames(AW.dist) <- AW.fr$GroupID
colnames(AW.dist) <- AW.fr$GroupID

#_____________________________________________________________________________________________________________
# 7. All plots together ----
#_____________________________________________________________________________________________________________

plot_grid(WS.clust.plot, SU.clust.plot, UA.clust.plot, AW.clust.plot,
          nrow = 2, ncol = 2)

# w 760 h 780

#_____________________________________________________________________________________________________________
# 8. Save image ----
#_____________________________________________________________________________________________________________

save.image(file = "clusters.RData")
