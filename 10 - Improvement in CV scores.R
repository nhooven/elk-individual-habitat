# Title: Individual variation in habitat selection
# Subtitle: 10 - Improvement in CV scores
# Author: Nathan D. Hooven
# Email: nathan.d.hooven@gmail.com
# Affiliation: Department of Forestry and Natural Resources, University of Kentucky
# Date began: 28 Feb 2022
# Date completed: 2 Mar 2022
# Date modified: 28 Mar 2022
# R version: 3.6.2

#_____________________________________________________________________________________________________________
# 1. Load in required packages ----
#_____________________________________________________________________________________________________________

library(tidyverse)
library(rstatix)     # Friedman test
library(cowplot)     # multiple plots

#_____________________________________________________________________________________________________________
# 2. Read in data ----
#_____________________________________________________________________________________________________________

WS.fr <- read.csv("WS_pergroup.csv")
SU.fr <- read.csv("SU_pergroup.csv")
UA.fr <- read.csv("UA_pergroup.csv")
AW.fr <- read.csv("AW_pergroup.csv")

#_____________________________________________________________________________________________________________
# 3. Group differences - Wilcoxon signed-rank test ----
#_____________________________________________________________________________________________________________
# 3a. WS ----
#_____________________________________________________________________________________________________________

# test of normality
shapiro.test(WS.fr$rho.global)
shapiro.test(WS.fr$rho.fr)
shapiro.test(WS.fr$rho.clust)

# means
mean(WS.fr$rho.global)
mean(WS.fr$rho.fr)
mean(WS.fr$rho.clust)

# get data into "longer" format
WS.fr.longer <- WS.fr %>% pivot_longer(cols = c(rho.global, rho.fr, rho.clust))

# make sure factor is in the correct rank
WS.fr.longer$name <- factor(WS.fr.longer$name,
                            levels = c("rho.global", "rho.fr", "rho.clust"))

# Friedman test for repeated measures
WS.fried <- WS.fr.longer %>% friedman_test(value ~ name | GroupID)

WS.fried

# effect size
WS.fr.longer %>% friedman_effsize(value ~ name | GroupID)

# paired wilcoxon test
WS.wilcox <- WS.fr.longer %>% wilcox_test(value ~ name, 
                                          paired = TRUE, 
                                          p.adjust.method = "BH",
                                          alternative = "less")

WS.wilcox

WS.fr.longer %>% wilcox_effsize(value ~ name, 
                                          paired = TRUE, 
                                          p.adjust.method = "BH",
                                          alternative = "less")

#_____________________________________________________________________________________________________________
# 3b. SU ----
#_____________________________________________________________________________________________________________

# test of normality
shapiro.test(SU.fr$rho.global)
shapiro.test(SU.fr$rho.fr)
shapiro.test(SU.fr$rho.clust)

# means
mean(SU.fr$rho.global)
mean(SU.fr$rho.fr)
mean(SU.fr$rho.clust)

# get data into "longer" format
SU.fr.longer <- SU.fr %>% pivot_longer(cols = c(rho.global, rho.fr, rho.clust))

# make sure factor is in the correct rank
SU.fr.longer$name <- factor(SU.fr.longer$name,
                            levels = c("rho.global", "rho.fr", "rho.clust"))

# Friedman test for repeated measures
SU.fried <- SU.fr.longer %>% friedman_test(value ~ name | GroupID)

SU.fried

# effect size
SU.fr.longer %>% friedman_effsize(value ~ name | GroupID)

# paired wilcoxon test
SU.wilcox <- SU.fr.longer %>% wilcox_test(value ~ name, 
                                          paired = TRUE, 
                                          p.adjust.method = "BH",
                                          alternative = "less")

SU.wilcox

SU.fr.longer %>% wilcox_effsize(value ~ name, 
                                          paired = TRUE, 
                                          p.adjust.method = "BH",
                                          alternative = "less")

#_____________________________________________________________________________________________________________
# 3c. UA ----
#_____________________________________________________________________________________________________________

# test of normality
shapiro.test(UA.fr$rho.global)
shapiro.test(UA.fr$rho.fr)
shapiro.test(UA.fr$rho.clust)

# means
mean(UA.fr$rho.global)
mean(UA.fr$rho.fr)
mean(UA.fr$rho.clust)

# get data into "longer" format
UA.fr.longer <- UA.fr %>% pivot_longer(cols = c(rho.global, rho.fr, rho.clust))

# make sure factor is in the correct rank
UA.fr.longer$name <- factor(UA.fr.longer$name,
                            levels = c("rho.global", "rho.fr", "rho.clust"))

# Friedman test for repeated measures
UA.fried <- UA.fr.longer %>% friedman_test(value ~ name | GroupID)

UA.fried

# effect size
UA.fr.longer %>% friedman_effsize(value ~ name | GroupID)

# paired wilcoxon test
UA.wilcox <- UA.fr.longer %>% wilcox_test(value ~ name, 
                                          paired = TRUE, 
                                          p.adjust.method = "BH",
                                          alternative = "less")

UA.wilcox

UA.fr.longer %>% wilcox_effsize(value ~ name, 
                                          paired = TRUE, 
                                          p.adjust.method = "BH",
                                          alternative = "less")

#_____________________________________________________________________________________________________________
# 3d. AW ----
#_____________________________________________________________________________________________________________

# test of normality
shapiro.test(AW.fr$rho.global)
shapiro.test(AW.fr$rho.fr)
shapiro.test(AW.fr$rho.clust)

# means
mean(AW.fr$rho.global)
mean(AW.fr$rho.fr)
mean(AW.fr$rho.clust)

# get data into "longer" format
AW.fr.longer <- AW.fr %>% pivot_longer(cols = c(rho.global, rho.fr, rho.clust))

# make sure factor is in the correct rank
AW.fr.longer$name <- factor(AW.fr.longer$name,
                            levels = c("rho.global", "rho.fr", "rho.clust"))

# Friedman test for repeated measures
AW.fried <- AW.fr.longer %>% friedman_test(value ~ name | GroupID)

AW.fried

# effect size
AW.fr.longer %>% friedman_effsize(value ~ name | GroupID)

# paired wilcoxon test
AW.wilcox <- AW.fr.longer %>% wilcox_test(value ~ name, 
                                          paired = TRUE, 
                                          p.adjust.method = "BH",
                                          alternative = "less")

AW.wilcox

AW.fr.longer %>% wilcox_effsize(value ~ name, 
                                          paired = TRUE, 
                                          p.adjust.method = "BH",
                                          alternative = "less")

#_____________________________________________________________________________________________________________
# 4. Group-level improvement ----
#_____________________________________________________________________________________________________________
# 4a. WS ----
#_____________________________________________________________________________________________________________

WS.fr.diff <- WS.fr %>% pivot_longer(cols = c(rho.diff, rho.diff.global.clust, rho.diff.fr.clust))

WS.fr.diff$name <- factor(WS.fr.diff$name,
                          levels = c("rho.diff", "rho.diff.fr.clust", "rho.diff.global.clust"),
                          labels = c("Functional - global", "Cluster - functional", "Cluster - global"))

WS.diff.plot <- ggplot(data = WS.fr.diff, aes(x = value, y = name)) +
       theme_bw() +
       geom_vline(xintercept = 0) +
       ggridges::geom_density_ridges(aes(scale = 1),
                                     alpha = 0.4,
                                     fill = "#009900") +
       ggtitle("a") +
       theme(panel.grid = element_blank(),
             axis.title.y = element_blank(),
             axis.title.x = element_blank(),
             axis.text.y = element_blank(),
             axis.ticks.y = element_blank(),
             plot.title = element_text(vjust = -7, hjust = 0.025),
             plot.margin = margin(0, 5, 0, 5)) +
       coord_cartesian(ylim = c(1.5, 3.4),
                       xlim = c(-0.25, 0.5)) +
       geom_segment(aes(x = mean(WS.fr$rho.diff),
                        xend = mean(WS.fr$rho.diff),
                        y = 1, yend = 2),
                  linetype = "dashed") +
       geom_segment(aes(x = mean(WS.fr$rho.diff.global.clust),
                        xend = mean(WS.fr$rho.diff.global.clust),
                        y = 3, yend = 4),
                  linetype = "dashed") +
       geom_segment(aes(x = mean(WS.fr$rho.diff.fr.clust),
                        xend = mean(WS.fr$rho.diff.fr.clust),
                        y = 2, yend = 3),
                  linetype = "dashed")

#_____________________________________________________________________________________________________________
# 4b. SU ----
#_____________________________________________________________________________________________________________

SU.fr.diff <- SU.fr %>% pivot_longer(cols = c(rho.diff, rho.diff.global.clust, rho.diff.fr.clust))

SU.fr.diff$name <- factor(SU.fr.diff$name,
                          levels = c("rho.diff", "rho.diff.fr.clust", "rho.diff.global.clust"),
                          labels = c("Functional - global", "Cluster - functional", "Cluster - global"))

SU.diff.plot <- ggplot(data = SU.fr.diff, aes(x = value, y = name)) +
       theme_bw() +
       geom_vline(xintercept = 0) +
       ggridges::geom_density_ridges(aes(scale = 1),
                                     alpha = 0.4,
                                     fill = "#FF9900") +
       ggtitle("b") +
       theme(panel.grid = element_blank(),
             axis.title.y = element_blank(),
             axis.title.x = element_blank(),
             axis.text.y = element_blank(),
             axis.ticks.y = element_blank(),
             plot.title = element_text(vjust = -7, hjust = 0.025),
             plot.margin = margin(0, 5, 0, 5)) +
       coord_cartesian(ylim = c(1.5, 3.4),
                       xlim = c(-0.25, 0.5)) +
       geom_segment(aes(x = mean(SU.fr$rho.diff),
                        xend = mean(SU.fr$rho.diff),
                        y = 1, yend = 2),
                  linetype = "dashed") +
       geom_segment(aes(x = mean(SU.fr$rho.diff.global.clust),
                        xend = mean(SU.fr$rho.diff.global.clust),
                        y = 3, yend = 4),
                  linetype = "dashed") +
       geom_segment(aes(x = mean(SU.fr$rho.diff.fr.clust),
                        xend = mean(SU.fr$rho.diff.fr.clust),
                        y = 2, yend = 3),
                  linetype = "dashed")

#_____________________________________________________________________________________________________________
# 4c. UA ----
#_____________________________________________________________________________________________________________

UA.fr.diff <- UA.fr %>% pivot_longer(cols = c(rho.diff, rho.diff.global.clust, rho.diff.fr.clust))

UA.fr.diff$name <- factor(UA.fr.diff$name,
                          levels = c("rho.diff", "rho.diff.fr.clust", "rho.diff.global.clust"),
                          labels = c("Functional - global", "Cluster - functional", "Cluster - global"))

UA.diff.plot <- ggplot(data = UA.fr.diff, aes(x = value, y = name)) +
       theme_bw() +
       geom_vline(xintercept = 0) +
       ggridges::geom_density_ridges(aes(scale = 1),
                                     alpha = 0.4,
                                     fill = "#FF3300") +
       ggtitle("c") +
       theme(panel.grid = element_blank(),
             axis.title.y = element_blank(),
             axis.title.x = element_blank(),
             axis.text.y = element_blank(),
             axis.ticks.y = element_blank(),
             plot.title = element_text(vjust = -7, hjust = 0.025),
             plot.margin = margin(0, 5, 0, 5)) +
       coord_cartesian(ylim = c(1.5, 3.4),
                       xlim = c(-0.25, 0.5)) +
       geom_segment(aes(x = mean(UA.fr$rho.diff),
                        xend = mean(UA.fr$rho.diff),
                        y = 1, yend = 2),
                  linetype = "dashed") +
       geom_segment(aes(x = mean(UA.fr$rho.diff.global.clust),
                        xend = mean(UA.fr$rho.diff.global.clust),
                        y = 3, yend = 4),
                  linetype = "dashed") +
       geom_segment(aes(x = mean(UA.fr$rho.diff.fr.clust),
                        xend = mean(UA.fr$rho.diff.fr.clust),
                        y = 2, yend = 3),
                  linetype = "dashed")

#_____________________________________________________________________________________________________________
# 4d. AW ----
#_____________________________________________________________________________________________________________

AW.fr.diff <- AW.fr %>% pivot_longer(cols = c(rho.diff, rho.diff.global.clust, rho.diff.fr.clust))

AW.fr.diff$name <- factor(AW.fr.diff$name,
                          levels = c("rho.diff", "rho.diff.fr.clust", "rho.diff.global.clust"),
                          labels = c("Functional - global", "Cluster - functional", "Cluster - global"))

AW.diff.plot <- ggplot(data = AW.fr.diff, aes(x = value, y = name)) +
       theme_bw() +
       geom_vline(xintercept = 0) +
       ggridges::geom_density_ridges(aes(scale = 1),
                                     alpha = 0.4,
                                     fill = "#003399") +
       ggtitle("d") +
       theme(panel.grid = element_blank(),
             axis.title.y = element_blank(),
             axis.title.x = element_blank(),
             axis.text.y = element_blank(),
             axis.ticks.y = element_blank(),
             plot.title = element_text(vjust = -7, hjust = 0.025),
             plot.margin = margin(0, 5, 0, 5)) +
       coord_cartesian(ylim = c(1.5, 3.4),
                       xlim = c(-0.25, 0.5)) +
       geom_segment(aes(x = mean(AW.fr$rho.diff),
                        xend = mean(AW.fr$rho.diff),
                        y = 1, yend = 2),
                  linetype = "dashed") +
       geom_segment(aes(x = mean(AW.fr$rho.diff.global.clust),
                        xend = mean(AW.fr$rho.diff.global.clust),
                        y = 3, yend = 4),
                  linetype = "dashed") +
       geom_segment(aes(x = mean(AW.fr$rho.diff.fr.clust),
                        xend = mean(AW.fr$rho.diff.fr.clust),
                        y = 2, yend = 3),
                  linetype = "dashed")

#_____________________________________________________________________________________________________________
# 4e. Combine plots ----
#_____________________________________________________________________________________________________________

plot_grid(WS.diff.plot, SU.diff.plot, UA.diff.plot, AW.diff.plot, ncol = 2, nrow = 2)

"Cluster - global"
"Cluster - functional"
"Functional - global"

#_____________________________________________________________________________________________________________
# 5. Explore Euclidean distance from functional predictions ----
#_____________________________________________________________________________________________________________
# 5a. WS ----
#_____________________________________________________________________________________________________________

ggplot(data = WS.fr, aes(x = dist)) +
        geom_density()+
        theme_bw()

WS.dist.plot <- ggplot(data = WS.fr, aes(x = dist, y = abs(PC1.slope))) +
        theme_bw() +
        geom_point(shape = 21) + 
        geom_smooth(method = "lm", 
                    color = "#009900", 
                    fill = "#009900", 
                    alpha = 0.2) +
        ggtitle("a") +
        theme(panel.grid = element_blank(),
              axis.title.x = element_blank(),
              plot.title = element_text(vjust = -7, hjust = 0.025),
              plot.margin = margin(0, 5, 0, 5)) +
        ylab("|PC1|") +
        xlab("Functional response predicted vs. actual distance")

#_____________________________________________________________________________________________________________
# 5b. SU ----
#_____________________________________________________________________________________________________________

ggplot(data = SU.fr, aes(x = dist)) +
        geom_density()+
        theme_bw()

SU.dist.plot <- ggplot(data = SU.fr, aes(x = dist, y = abs(PC1.slope))) +
        theme_bw() +
        geom_point(shape = 22) + 
        geom_smooth(method = "lm", 
                    color = "#FF9900", 
                    fill = "#FF9900", 
                    alpha = 0.2) +
        ggtitle("b") +
        theme(panel.grid = element_blank(),
              axis.title.y = element_blank(),
              axis.title.x = element_blank(),
              plot.title = element_text(vjust = -7, hjust = 0.025),
              plot.margin = margin(0, 5, 0, 5)) +
        ylab("|PC1|") +
        xlab("Functional response predicted vs. actual distance")

#_____________________________________________________________________________________________________________
# 5c. UA ----
#_____________________________________________________________________________________________________________

ggplot(data = UA.fr, aes(x = dist)) +
        geom_density()+
        theme_bw()

UA.dist.plot <- ggplot(data = UA.fr, aes(x = dist, y = abs(PC1.slope))) +
        theme_bw() +
        geom_point(shape = 24) + 
        geom_smooth(method = "lm", 
                    color = "#FF3300", 
                    fill = "#FF3300", 
                    alpha = 0.2) +
        ggtitle("c") +
        theme(panel.grid = element_blank(),
              plot.title = element_text(vjust = -7, hjust = 0.025),
              plot.margin = margin(-5, 5, 0, 5)) +
        ylab("|PC1|") +
        xlab("Predicted vs. actual distance")

#_____________________________________________________________________________________________________________
# 5d. AW ----
#_____________________________________________________________________________________________________________

ggplot(data = AW.fr, aes(x = dist)) +
        geom_density()+
        theme_bw()

AW.dist.plot <- ggplot(data = AW.fr, aes(x = dist, y = abs(PC1.slope))) +
        theme_bw() +
        geom_point(shape = 23) + 
        geom_smooth(method = "lm", 
                    color = "#003399", 
                    fill = "#003399", 
                    alpha = 0.2) +
        ggtitle("d") +
        theme(panel.grid = element_blank(),
              axis.title.y = element_blank(),
              plot.title = element_text(vjust = -7, hjust = 0.025),
              plot.margin = margin(-5, 5, 0, 10)) +
        ylab("|PC1|") +
        xlab("Predicted vs. actual distance")
        
#_____________________________________________________________________________________________________________
# 5e. Combine plots ----
#_____________________________________________________________________________________________________________

plot_grid(WS.dist.plot, SU.dist.plot, UA.dist.plot, AW.dist.plot, ncol = 2, nrow = 2)

#_____________________________________________________________________________________________________________
# 6. Distance from functional predictions in multivariate space ----
#_____________________________________________________________________________________________________________
# 6a. WS ----
#_____________________________________________________________________________________________________________

WS.dist.plot2 <- ggplot(data = WS.fr, aes(x = PC1.slope, y = PC2.slope, fill = dist)) +
        theme_bw() +
        geom_hline(yintercept = 0, linetype = "dashed") +
        geom_vline(xintercept = 0, linetype = "dashed") +
        geom_point(shape = 21,
                   size = 3) + 
        ggtitle("a") +
        theme(panel.grid = element_blank(),
              axis.title.x = element_blank(),
              plot.title = element_text(vjust = -7, hjust = 0.025),
              plot.margin = margin(0, 5, 0, 5)) +
        scale_fill_viridis_c(option = "plasma") +
        ylab("PC2") +
        xlab("PC1")

#_____________________________________________________________________________________________________________
# 6b. All ----
#_____________________________________________________________________________________________________________

fr.all <- rbind(WS.fr, SU.fr, UA.fr, AW.fr)

fr.all$Season <- factor(fr.all$Season, labels = c("Feb-Apr", "May-Jul", "Aug-Oct", "Nov-Jan"))

ggplot(data = fr.all, aes(x = PC1.slope, y = PC2.slope, shape = as.factor(Season), fill = dist)) +
        facet_wrap(~Season, scales = "free") +
        theme_bw() +
        geom_hline(yintercept = 0, linetype = "dashed") +
        geom_vline(xintercept = 0, linetype = "dashed") +
        geom_point(size = 3) + 
        scale_shape_manual(values = c(21, 22, 24, 23)) +
        theme(panel.grid = element_blank()) +
        scale_fill_viridis_c(option = "plasma", direction = -1) +
        ylab("PC2") +
        xlab("PC1")

#_____________________________________________________________________________________________________________
# 7. Global vs improvement ----
#_____________________________________________________________________________________________________________

# WS
ggplot(data = WS.fr, aes(x = rho.global, y = rho.diff)) +
        theme_bw() +
        geom_hline(yintercept = 0) +
        geom_point() +
        geom_smooth(method = "lm", formula = y ~ poly(x, 2))

ggplot(data = WS.fr, aes(x = rho.global, y = rho.diff.global.clust)) +
        theme_bw() +
        geom_hline(yintercept = 0) +
        geom_point() +
        geom_smooth(method = "lm", formula = y ~ poly(x, 2))

# SU
ggplot(data = SU.fr, aes(x = rho.global, y = rho.diff)) +
        theme_bw() +
        geom_hline(yintercept = 0) +
        geom_point() +
        geom_smooth(method = "lm", formula = y ~ poly(x, 2))

ggplot(data = SU.fr, aes(x = rho.global, y = rho.diff.global.clust)) +
        theme_bw() +
        geom_hline(yintercept = 0) +
        geom_point() +
        geom_smooth(method = "lm", formula = y ~ poly(x, 2))

# UA
ggplot(data = UA.fr, aes(x = rho.global, y = rho.diff)) +
        theme_bw() +
        geom_hline(yintercept = 0) +
        geom_point() +
        geom_smooth(method = "lm", formula = y ~ poly(x, 2))

ggplot(data = UA.fr, aes(x = rho.global, y = rho.diff.global.clust)) +
        theme_bw() +
        geom_hline(yintercept = 0) +
        geom_point() +
        geom_smooth(method = "lm", formula = y ~ poly(x, 2))

# AW
ggplot(data = AW.fr, aes(x = rho.global, y = rho.diff)) +
        theme_bw() +
        geom_hline(yintercept = 0) +
        geom_point() +
        geom_smooth(method = "lm", formula = y ~ poly(x, 2))

ggplot(data = AW.fr, aes(x = rho.global, y = rho.diff.global.clust)) +
        theme_bw() +
        geom_hline(yintercept = 0) +
        geom_point() +
        geom_smooth(method = "lm", formula = y ~ poly(x, 2))

# bind together for facetting
WS.fr.1 <- WS.fr %>% dplyr::select(-dist)
UA.fr.1 <- UA.fr %>% dplyr::select(-dist)
AW.fr.1 <- AW.fr %>% dplyr::select(-dist)

all.fr <- rbind(WS.fr.1, SU.fr, UA.fr.1, AW.fr.1)

all.fr.longer <- all.fr %>% pivot_longer(cols = c(29, 30))

all.fr.longer$Season <- factor(all.fr.longer$Season,
                               labels = c("Feb-Apr", "May-Jul", "Aug-Oct", "Nov-Jan"))

all.fr.longer$name <- factor(all.fr.longer$name,
                               labels = c("Functional", "Cluster"))

ggplot(data = all.fr.longer, aes(x = rho.global, y = value, shape = Season, fill = Season, color = Season)) +
        theme_bw() +
        facet_grid(name ~ Season,
                   scales = "free_y") +
        geom_hline(yintercept = 0) +
        geom_point(color = "black", fill = NA, alpha = 0.5) +
        geom_smooth(method = "gam",
                    alpha = 0.2) +
        theme(panel.grid = element_blank(),
              legend.position = "none") +
        scale_color_manual(values = c("#009900", "#FF9900", "#FF3300", "#003399")) +
        scale_fill_manual(values = c("#009900", "#FF9900", "#FF3300", "#003399")) +
        scale_shape_manual(values = c(21, 22, 24, 23)) +
        ylab(expression(paste("Improvement from global ", r[s]))) +
        xlab(expression(paste("Global ", r[s])))

ggplot(data = all.fr.longer, aes(x = rho.global, y = dist)) +
        theme_bw() +
        facet_wrap(~Season) +
        geom_point() +
        geom_smooth(method = "lm")
             
#_____________________________________________________________________________________________________________
# 8. Which approach led to the most best predictions? ----
#_____________________________________________________________________________________________________________

all.fr$approach <- NA

for (x in 1:nrow(all.fr)) {
        
        indiv.row <- all.fr[x, ]
        
        best.approach <- ifelse(indiv.row$rho.global > indiv.row$rho.fr & indiv.row$rho.global > indiv.row$rho.clust,
                                "rho.global",
                                ifelse(indiv.row$rho.fr > indiv.row$rho.global & indiv.row$rho.fr > indiv.row$rho.clust,
                                       "rho.fr",
                                       "rho.clust"))
        
        all.fr$approach[x] <- best.approach
        
}

all.fr.1 <- all.fr

all.fr.1$Season <- factor(all.fr.1$Season,
                               labels = c("Feb-Apr", "May-Jul", "Aug-Oct", "Nov-Jan"))

all.fr.1$approach <- factor(all.fr.1$approach,
                               levels = c("rho.global", "rho.fr", "rho.clust"),
                               labels = c("Global", "Functional", "Cluster"))

ggplot(data = all.fr.1, aes(x = Season, fill = approach)) +
        theme_bw() +
        geom_bar(color = "black") +
        theme(panel.grid = element_blank(),
              axis.title.x = element_blank(),
              legend.title = element_blank(),
              legend.position = c(0.85, 0.9),
              legend.background = element_blank()) +
        scale_fill_viridis_d(option = "cividis") +
        ylab("Number of groups")

table(all.fr.1$approach, all.fr.1$Season)

