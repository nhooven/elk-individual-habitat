# Title: Individual variation in habitat selection
# Subtitle: IM10 - Functional response visualization
# Author: Nathan D. Hooven
# Email: nathan.d.hooven@gmail.com
# Affiliation: Department of Forestry and Natural Resources, University of Kentucky
# Date began: 20 Jun 2022
# Date completed: 21 Jun 2022
# Date modified: 11 Jul 2023
# R version: 3.6.2

#_____________________________________________________________________________________________________________
# 1. Load in required packages ----
#_____________________________________________________________________________________________________________

library(tidyverse)
library(cowplot)

#_____________________________________________________________________________________________________________
# 2. Load in data ----
#_____________________________________________________________________________________________________________

load("fr_models.RData")

#_____________________________________________________________________________________________________________
# 3. dDeveloped ----
#_____________________________________________________________________________________________________________
# 3a. WS ----
#_____________________________________________________________________________________________________________

# RS predictions
WS.x1.pred.RS <- as.data.frame(predict(WS.models[[1]], newdata = WS.fr, se.fit = TRUE))

WS.x1.plot <- ggplot() +
              theme_bw() +
              geom_hline(yintercept = 0) +
              geom_ribbon(data = WS.x1.pred.RS,
                          aes(x = WS.fr$ed,
                              y = fit,
                              ymin = fit - se.fit,
                              ymax = fit + se.fit),
                          alpha = 0.2,
                          fill = "#009900") +
              geom_line(data = WS.x1.pred.RS,
                          aes(WS.fr$ed, fit),
                        size = 1.5,
                        color = "#009900") +
              xlab(expression(paste("Edge density", " ", (m/km^2)))) +
              ylab(expression(paste(beta, " for dDeveloped"))) +
              theme(panel.grid = element_blank(),
                    plot.margin = margin(5, 0, 5, 0)) +
              scale_x_continuous(breaks = seq(1000, 5000, 2000))

WS.x1.plot

#_____________________________________________________________________________________________________________
# 3b. SU ----
#_____________________________________________________________________________________________________________

# RS predictions
SU.x1.pred.RS <- as.data.frame(predict(SU.dDeveloped[[1]], newdata = SU.fr, se.fit = TRUE))

SU.x1.plot <- ggplot() +
              theme_bw() +
              geom_hline(yintercept = 0) +
              geom_ribbon(data = SU.x1.pred.RS,
                          aes(x = SU.fr$dev,
                              y = fit,
                              ymin = fit - se.fit,
                              ymax = fit + se.fit),
                          alpha = 0.2,
                          fill = "#FF9900") +
              geom_line(data = SU.x1.pred.RS,
                          aes(SU.fr$dev, fit),
                        size = 1.5,
                        color = "#FF9900") +
              xlab("% developed") +
              theme(panel.grid = element_blank(),
                    plot.margin = margin(5, 0, 11, 0),
                    axis.title.y = element_blank())

SU.x1.plot

#_____________________________________________________________________________________________________________
# 3c. UA ----
#_____________________________________________________________________________________________________________

# RS predictions
UA.x1.pred.RS <- as.data.frame(predict(UA.models[[1]], newdata = UA.fr, se.fit = TRUE))

UA.x1.plot <- ggplot() +
              theme_bw() +
              geom_hline(yintercept = 0) +
              geom_ribbon(data = UA.x1.pred.RS,
                          aes(x = UA.fr$dev,
                              y = fit,
                              ymin = fit - se.fit,
                              ymax = fit + se.fit),
                          alpha = 0.2,
                          fill = "#FF3300") +
              geom_line(data = UA.x1.pred.RS,
                          aes(UA.fr$dev, fit),
                        size = 1.5,
                        color = "#FF3300") +
              xlab("% developed") +
              theme(panel.grid = element_blank(),
                    plot.margin = margin(5, 0, 11, 0),
                    axis.title.y = element_blank()) +
              scale_y_continuous(breaks = c(-0.4, -0.2, 0, 0.2))

UA.x1.plot

#_____________________________________________________________________________________________________________
# 3d. AW ----
#_____________________________________________________________________________________________________________

# RS predictions
AW.x1.pred.RS <- as.data.frame(predict(AW.models[[1]], newdata = AW.fr, se.fit = TRUE))

AW.x1.plot <- ggplot() +
              theme_bw() +
              geom_hline(yintercept = 0) +
              geom_ribbon(data = AW.x1.pred.RS,
                          aes(x = AW.fr$dev,
                              y = fit,
                              ymin = fit - se.fit,
                              ymax = fit + se.fit),
                          alpha = 0.2,
                          fill = "#003399") +
              geom_line(data = AW.x1.pred.RS,
                          aes(AW.fr$dev, fit),
                        size = 1.5,
                        color = "#003399") +
              xlab("% developed") +
              theme(panel.grid = element_blank(),
                    plot.margin = margin(5, 0, 11, 0),
                    axis.title.y = element_blank()) +
              scale_x_continuous(breaks = c(1, 3, 5, 7, 9, 11))

AW.x1.plot

#_____________________________________________________________________________________________________________
# 3e. Multiple plots ----
#_____________________________________________________________________________________________________________

dDeveloped.grid <- plot_grid(WS.x1.plot, SU.x1.plot, UA.x1.plot, AW.x1.plot,
                             nrow = 1, ncol = 4,
                             rel_widths = c(1.1, 1, 1, 1))

dDeveloped.grid

#_____________________________________________________________________________________________________________
# 4. dRoad.P ----
#_____________________________________________________________________________________________________________
# 4a. WS ----
#_____________________________________________________________________________________________________________

# RS predictions
WS.x2.pred.RS <- as.data.frame(predict(WS.models[[2]], newdata = WS.fr, se.fit = TRUE))

WS.x2.plot <- ggplot() +
              theme_bw() +
              geom_hline(yintercept = 0) +
              geom_ribbon(data = WS.x2.pred.RS,
                          aes(x = WS.fr$dRoad,
                              y = fit,
                              ymin = fit - se.fit,
                              ymax = fit + se.fit),
                          alpha = 0.2,
                          fill = "#009900") +
              geom_line(data = WS.x2.pred.RS,
                          aes(WS.fr$dRoad, fit),
                        size = 1.5,
                        color = "#009900") +
              xlab("Mean distance to roads (m)") +
              ylab(expression(paste(beta, " for ln(dRoad)"))) +
              theme(panel.grid = element_blank(),
                    plot.margin = margin(5, 0, 11, 0))

WS.x2.plot

#_____________________________________________________________________________________________________________
# 4b. SU ----
#_____________________________________________________________________________________________________________

# RS predictions
SU.x2.pred.RS <- as.data.frame(predict(SU.models[[2]], newdata = SU.fr, se.fit = TRUE))

SU.x2.plot <- ggplot() +
              theme_bw() +
              geom_hline(yintercept = 0) +
              geom_ribbon(data = SU.x2.pred.RS,
                          aes(x = SU.fr$ed,
                              y = fit,
                              ymin = fit - se.fit,
                              ymax = fit + se.fit),
                          alpha = 0.2,
                          fill = "#FF9900") +
              geom_line(data = SU.x2.pred.RS,
                          aes(SU.fr$ed, fit),
                        size = 1.5,
                        color = "#FF9900") +
              xlab(expression(paste("Edge density", " ", (m/km^2)))) +
              theme(panel.grid = element_blank(),
                    plot.margin = margin(5, 0, 5, 0),
                    axis.title.y = element_blank()) +
              scale_y_continuous(breaks = c(-0.2, 0, 0.2, 0.4, 0.6, 0.8)) +
              scale_x_continuous(breaks = c(1000, 2000, 3000, 4000))

SU.x2.plot

#_____________________________________________________________________________________________________________
# 4c. UA ----
#_____________________________________________________________________________________________________________

# RS predictions
UA.x2.pred.RS <- as.data.frame(predict(UA.dRoad[[3]], newdata = UA.fr, se.fit = TRUE))

UA.x2.plot <- ggplot() +
              theme_bw() +
              geom_hline(yintercept = 0) +
              geom_ribbon(data = UA.x2.pred.RS,
                          aes(x = UA.fr$ed,
                              y = fit,
                              ymin = fit - se.fit,
                              ymax = fit + se.fit),
                          alpha = 0.2,
                          fill = "#FF3300") +
              geom_line(data = UA.x2.pred.RS,
                          aes(UA.fr$ed, fit),
                        size = 1.5,
                        color = "#FF3300") +
              xlab(expression(paste("Edge density", " ", (m/km^2)))) +
              theme(panel.grid = element_blank(),
                    plot.margin = margin(5, 0, 5, 0),
                    axis.title.y = element_blank()) +
              scale_y_continuous(breaks = c(-0.2, 0, 0.2, 0.4, 0.6, 0.8)) +
              scale_x_continuous(breaks = c(1000, 2000, 3000))

UA.x2.plot

#_____________________________________________________________________________________________________________
# 4d. AW ----
#_____________________________________________________________________________________________________________

# RS predictions
AW.x2.pred.RS <- as.data.frame(predict(AW.models[[2]], newdata = AW.fr, se.fit = TRUE))

AW.x2.plot <- ggplot() +
              theme_bw() +
              geom_hline(yintercept = 0) +
              geom_ribbon(data = AW.x2.pred.RS,
                          aes(x = AW.fr$dRoad,
                              y = fit,
                              ymin = fit - se.fit,
                              ymax = fit + se.fit),
                          alpha = 0.2,
                          fill = "#003399") +
              geom_line(data = AW.x2.pred.RS,
                          aes(AW.fr$dRoad, fit),
                        size = 1.5,
                        color = "#003399") +
              xlab("Mean distance to roads (m)") +
              theme(panel.grid = element_blank(),
                    plot.margin = margin(5, 0, 11, 0),
                    axis.title.y = element_blank())

AW.x2.plot

#_____________________________________________________________________________________________________________
# 4e. Multiple plots ----
#_____________________________________________________________________________________________________________

dRoad.grid <- plot_grid(WS.x2.plot, SU.x2.plot, UA.x2.plot, AW.x2.plot,
                        nrow = 1, ncol = 4,
                        rel_widths = c(1.1, 1, 1, 1))

dRoad.grid

#_____________________________________________________________________________________________________________
# 5. TRI ----
#_____________________________________________________________________________________________________________
# 5a. WS ----
#_____________________________________________________________________________________________________________

# RS predictions
WS.x3.pred.RS <- as.data.frame(predict(WS.models[[3]], newdata = WS.fr, se.fit = TRUE))

WS.x3.plot <- ggplot() +
              theme_bw() +
              geom_hline(yintercept = 0) +
              geom_ribbon(data = WS.x3.pred.RS,
                          aes(x = WS.fr$ed,
                              y = fit,
                              ymin = fit - se.fit,
                              ymax = fit + se.fit),
                          alpha = 0.2,
                          fill = "#009900") +
              geom_line(data = WS.x3.pred.RS,
                          aes(WS.fr$ed, fit),
                        size = 1.5,
                        color = "#009900") +
               xlab(expression(paste("Edge density", " ", (m/km^2)))) +
              ylab(expression(paste(beta, " for TRI"))) +
              theme(panel.grid = element_blank(),
                    plot.margin = margin(5, 0, 5, 0)) +
              scale_y_continuous(breaks = c(-0.4, -0.2, 0.0, 0.2)) +
              scale_x_continuous(breaks = c(1000, 3000, 5000))

WS.x3.plot

#_____________________________________________________________________________________________________________
# 5b. SU ----
#_____________________________________________________________________________________________________________

# RS predictions
SU.x3.pred.RS <- as.data.frame(predict(SU.models[[3]], newdata = SU.fr, se.fit = TRUE))

SU.x3.plot <- ggplot() +
              theme_bw() +
              geom_hline(yintercept = 0) +
              geom_ribbon(data = SU.x3.pred.RS,
                          aes(x = SU.fr$ed,
                              y = fit,
                              ymin = fit - se.fit,
                              ymax = fit + se.fit),
                          alpha = 0.2,
                          fill = "#FF9900") +
              geom_line(data = SU.x3.pred.RS,
                          aes(SU.fr$ed, fit),
                        size = 1.5,
                        color = "#FF9900") +
              xlab(expression(paste("Edge density", " ", (m/km^2)))) +
              theme(panel.grid = element_blank(),
                    plot.margin = margin(5, 0, 5, 0),
                    axis.title.y = element_blank()) +
              scale_x_continuous(breaks = c(1000, 2000, 3000, 4000))

SU.x3.plot

#_____________________________________________________________________________________________________________
# 5c. UA ----
#_____________________________________________________________________________________________________________

# RS predictions
UA.x3.pred.RS <- as.data.frame(predict(UA.models[[3]], newdata = UA.fr, se.fit = TRUE))

UA.x3.plot <- ggplot() +
              theme_bw() +
              geom_hline(yintercept = 0) +
              geom_ribbon(data = UA.x3.pred.RS,
                          aes(x = UA.fr$ed,
                              y = fit,
                              ymin = fit - se.fit,
                              ymax = fit + se.fit),
                          alpha = 0.2,
                          fill = "#FF3300") +
              geom_line(data = UA.x3.pred.RS,
                          aes(UA.fr$ed, fit),
                        size = 1.5,
                        color = "#FF3300") +
              xlab(expression(paste("Edge density", " ", (m/km^2)))) +
              theme(panel.grid = element_blank(),
                    plot.margin = margin(5, 0, 5, 0),
                    axis.title.y = element_blank()) +
              scale_x_continuous(breaks = c(1000, 2000, 3000))

UA.x3.plot

#_____________________________________________________________________________________________________________
# 5d. AW ----
#_____________________________________________________________________________________________________________

# RS predictions
AW.x3.pred.RS <- as.data.frame(predict(AW.models[[3]], newdata = AW.fr, se.fit = TRUE))

AW.x3.plot <- ggplot() +
              theme_bw() +
              geom_hline(yintercept = 0) +
              geom_ribbon(data = AW.x3.pred.RS,
                          aes(x = AW.fr$ed,
                              y = fit,
                              ymin = fit - se.fit,
                              ymax = fit + se.fit),
                          alpha = 0.2,
                          fill = "#003399") +
              geom_line(data = AW.x3.pred.RS,
                          aes(AW.fr$ed, fit),
                        size = 1.5,
                        color = "#003399") +
               xlab(expression(paste("Edge density", " ", (m/km^2)))) +
              theme(panel.grid = element_blank(),
                    plot.margin = margin(5, 0, 5, 0),
                    axis.title.y = element_blank()) +
              scale_x_continuous(breaks = c(1000, 2000, 3000))

AW.x3.plot

#_____________________________________________________________________________________________________________
# 5e. Multiple plots ----
#_____________________________________________________________________________________________________________

TRI.grid <- plot_grid(WS.x3.plot, SU.x3.plot, UA.x3.plot, AW.x3.plot,
                        nrow = 1, ncol = 4,
                        rel_widths = c(1.1, 1, 1, 1))

TRI.grid

#_____________________________________________________________________________________________________________
# 6. TPI ----
#_____________________________________________________________________________________________________________
# 6a. WS ----
#_____________________________________________________________________________________________________________

# RS predictions
WS.x4.pred.RS <- as.data.frame(predict(WS.models[[4]], newdata = WS.fr, se.fit = TRUE))

WS.x4.plot <- ggplot() +
              theme_bw() +
              geom_hline(yintercept = 0) +
              geom_ribbon(data = WS.x4.pred.RS,
                          aes(x = WS.fr$ed,
                              y = fit,
                              ymin = fit - se.fit,
                              ymax = fit + se.fit),
                          alpha = 0.2,
                          fill = "#009900") +
              geom_line(data = WS.x4.pred.RS,
                          aes(WS.fr$ed, fit),
                        size = 1.5,
                        color = "#009900") +
               xlab(expression(paste("Edge density", " ", (m/km^2)))) +
              ylab(expression(paste(beta, " for TPI"))) +
              theme(panel.grid = element_blank(),
                    plot.margin = margin(5, 0, 5, 0)) +
              scale_x_continuous(breaks = c(1000, 3000, 5000))

WS.x4.plot

#_____________________________________________________________________________________________________________
# 6b. SU ----
#_____________________________________________________________________________________________________________

# RS predictions
SU.x4.pred.RS <- as.data.frame(predict(SU.models[[4]], newdata = SU.fr, se.fit = TRUE))

SU.x4.plot <- ggplot() +
              theme_bw() +
              geom_hline(yintercept = 0) +
              geom_ribbon(data = SU.x4.pred.RS,
                          aes(x = SU.fr$TPI.y,
                              y = fit,
                              ymin = fit - se.fit,
                              ymax = fit + se.fit),
                          alpha = 0.2,
                          fill = "#FF9900") +
              geom_line(data = SU.x4.pred.RS,
                          aes(SU.fr$TPI.y, fit),
                        size = 1.5,
                        color = "#FF9900") +
              xlab("Mean TPI") +
              theme(panel.grid = element_blank(),
                    plot.margin = margin(5, 0, 11, 0),
                    axis.title.y = element_blank())

SU.x4.plot

#_____________________________________________________________________________________________________________
# 6c. UA ----
#_____________________________________________________________________________________________________________

# RS predictions
UA.x4.pred.RS <- as.data.frame(predict(UA.models[[4]], newdata = UA.fr, se.fit = TRUE))

UA.x4.plot <- ggplot() +
              theme_bw() +
              geom_hline(yintercept = 0) +
              geom_ribbon(data = UA.x4.pred.RS,
                          aes(x = UA.fr$TPI.y,
                              y = fit,
                              ymin = fit - se.fit,
                              ymax = fit + se.fit),
                          alpha = 0.2,
                          fill = "#FF3300") +
              geom_line(data = UA.x4.pred.RS,
                          aes(UA.fr$TPI.y, fit),
                        size = 1.5,
                        color = "#FF3300") +
              xlab("Mean TPI") +
              theme(panel.grid = element_blank(),
                    plot.margin = margin(5, 0, 11, 0),
                    axis.title.y = element_blank())

UA.x4.plot

#_____________________________________________________________________________________________________________
# 6d. AW ----
#_____________________________________________________________________________________________________________

# RS predictions
AW.x4.pred.RS <- as.data.frame(predict(AW.models[[4]], newdata = AW.fr, se.fit = TRUE))

AW.x4.plot <- ggplot() +
              theme_bw() +
              geom_hline(yintercept = 0) +
              geom_ribbon(data = AW.x4.pred.RS,
                          aes(x = AW.fr$ed,
                              y = fit,
                              ymin = fit - se.fit,
                              ymax = fit + se.fit),
                          alpha = 0.2,
                          fill = "#003399") +
              geom_line(data = AW.x4.pred.RS,
                          aes(AW.fr$ed, fit),
                        size = 1.5,
                        color = "#003399") +
               xlab(expression(paste("Edge density", " ", (m/km^2)))) +
              theme(panel.grid = element_blank(),
                    plot.margin = margin(5, 0, 5, 0),
                    axis.title.y = element_blank()) +
              scale_x_continuous(breaks = c(1000, 2000, 3000))

AW.x4.plot

#_____________________________________________________________________________________________________________
# 6e. Multiple plots ----
#_____________________________________________________________________________________________________________

TPI.grid <- plot_grid(WS.x4.plot, SU.x4.plot, UA.x4.plot, AW.x4.plot,
                        nrow = 1, ncol = 4,
                        rel_widths = c(1.1, 1, 1, 1))

TPI.grid

#_____________________________________________________________________________________________________________
# 7. dOpen ----
#_____________________________________________________________________________________________________________
# 7a. WS ----
#_____________________________________________________________________________________________________________

# RS predictions
WS.x5.pred.RS <- as.data.frame(predict(WS.models[[5]], newdata = WS.fr, se.fit = TRUE))

WS.x5.plot <- ggplot() +
              theme_bw() +
              geom_hline(yintercept = 0) +
              geom_ribbon(data = WS.x5.pred.RS,
                          aes(x = WS.fr$ed,
                              y = fit,
                              ymin = fit - se.fit,
                              ymax = fit + se.fit),
                          alpha = 0.2,
                          fill = "#009900") +
              geom_line(data = WS.x5.pred.RS,
                          aes(WS.fr$ed, fit),
                        size = 1.5,
                        color = "#009900") +
               xlab(expression(paste("Edge density", " ", (m/km^2)))) +
              ylab(expression(paste(beta, " for dOpen"))) +
              theme(panel.grid = element_blank(),
                    plot.margin = margin(5, 0, 5, 0)) +
              scale_x_continuous(breaks = c(1000, 3000, 5000))

WS.x5.plot

#_____________________________________________________________________________________________________________
# 7b. SU ----
#_____________________________________________________________________________________________________________

# RS predictions
SU.x5.pred.RS <- as.data.frame(predict(SU.models[[5]], newdata = SU.fr, se.fit = TRUE))

SU.x5.plot <- ggplot() +
              theme_bw() +
              geom_hline(yintercept = 0) +
              geom_ribbon(data = SU.x5.pred.RS,
                          aes(x = SU.fr$open,
                              y = fit,
                              ymin = fit - se.fit,
                              ymax = fit + se.fit),
                          alpha = 0.2,
                          fill = "#FF9900") +
              geom_line(data = SU.x5.pred.RS,
                          aes(SU.fr$open, fit),
                        size = 1.5,
                        color = "#FF9900") +
              xlab("% open") +
              theme(panel.grid = element_blank(),
                    plot.margin = margin(5, 0, 11, 0),
                    axis.title.y = element_blank())

SU.x5.plot

#_____________________________________________________________________________________________________________
# 7c. UA ----
#_____________________________________________________________________________________________________________

# RS predictions
UA.x5.pred.RS <- as.data.frame(predict(UA.models[[5]], newdata = UA.fr, se.fit = TRUE))

UA.x5.plot <- ggplot() +
              theme_bw() +
              geom_hline(yintercept = 0) +
              geom_ribbon(data = UA.x5.pred.RS,
                          aes(x = UA.fr$ed,
                              y = fit,
                              ymin = fit - se.fit,
                              ymax = fit + se.fit),
                          alpha = 0.2,
                          fill = "#FF3300") +
              geom_line(data = UA.x5.pred.RS,
                          aes(UA.fr$ed, fit),
                        size = 1.5,
                        color = "#FF3300") +
              xlab(expression(paste("Edge density", " ", (m/km^2)))) +
              theme(panel.grid = element_blank(),
                    plot.margin = margin(5, 0, 5, 0)) +
              scale_x_continuous(breaks = c(1000, 2000, 3000))

UA.x5.plot

#_____________________________________________________________________________________________________________
# 7d. AW ----
#_____________________________________________________________________________________________________________

# RS predictions
AW.x5.pred.RS <- as.data.frame(predict(AW.models[[5]], newdata = AW.fr, se.fit = TRUE))

AW.x5.plot <- ggplot() +
              theme_bw() +
              geom_hline(yintercept = 0) +
              geom_ribbon(data = AW.x5.pred.RS,
                          aes(x = AW.fr$ed,
                              y = fit,
                              ymin = fit - se.fit,
                              ymax = fit + se.fit),
                          alpha = 0.2,
                          fill = "#003399") +
              geom_line(data = AW.x5.pred.RS,
                          aes(AW.fr$ed, fit),
                        size = 1.5,
                        color = "#003399") +
               xlab(expression(paste("Edge density", " ", (m/km^2)))) +
              theme(panel.grid = element_blank(),
                    plot.margin = margin(5, 0, 5, 0),
                    axis.title.y = element_blank()) +
              scale_x_continuous(breaks = c(1000, 2000, 3000))

AW.x5.plot

#_____________________________________________________________________________________________________________
# 7e. Multiple plots ----
#_____________________________________________________________________________________________________________

dOpen.grid <- plot_grid(WS.x5.plot, SU.x5.plot, UA.x5.plot, AW.x5.plot,
                        nrow = 1, ncol = 4,
                        rel_widths = c(1.1, 1, 1, 1))

dOpen.grid

#_____________________________________________________________________________________________________________
# 8. dMatureForest ----
#_____________________________________________________________________________________________________________
# 8a. WS ----
#_____________________________________________________________________________________________________________

# RS predictions
WS.x6.pred.RS <- as.data.frame(predict(WS.models[[6]], newdata = WS.fr, se.fit = TRUE))

WS.x6.plot <- ggplot() +
              theme_bw() +
              geom_hline(yintercept = 0) +
              geom_ribbon(data = WS.x6.pred.RS,
                          aes(x = WS.fr$mf,
                              y = fit,
                              ymin = fit - se.fit,
                              ymax = fit + se.fit),
                          alpha = 0.2,
                          fill = "#009900") +
              geom_line(data = WS.x6.pred.RS,
                          aes(WS.fr$mf, fit),
                        size = 1.5,
                        color = "#009900") +
               xlab("% mature forest") +
              ylab(expression(paste(beta, " for dMatureForest"))) +
              theme(panel.grid = element_blank(),
                    plot.margin = margin(5, 0, 11, 0))

WS.x6.plot

#_____________________________________________________________________________________________________________
# 8b. SU ----
#_____________________________________________________________________________________________________________

# RS predictions
SU.x6.pred.RS <- as.data.frame(predict(SU.models[[6]], newdata = SU.fr, se.fit = TRUE))

SU.x6.plot <- ggplot() +
              theme_bw() +
              geom_hline(yintercept = 0) +
              geom_ribbon(data = SU.x6.pred.RS,
                          aes(x = SU.fr$mf,
                              y = fit,
                              ymin = fit - se.fit,
                              ymax = fit + se.fit),
                          alpha = 0.2,
                          fill = "#FF9900") +
              geom_line(data = SU.x6.pred.RS,
                          aes(SU.fr$mf, fit),
                        size = 1.5,
                        color = "#FF9900") +
              xlab("% mature forest") +
              theme(panel.grid = element_blank(),
                    plot.margin = margin(5, 0, 11, 0),
                    axis.title.y = element_blank())

SU.x6.plot

#_____________________________________________________________________________________________________________
# 8c. UA ----
#_____________________________________________________________________________________________________________

# RS predictions
UA.x6.pred.RS <- as.data.frame(predict(UA.models[[6]], newdata = UA.fr, se.fit = TRUE))

UA.x6.plot <- ggplot() +
              theme_bw() +
              geom_hline(yintercept = 0) +
              geom_ribbon(data = UA.x6.pred.RS,
                          aes(x = UA.fr$mf,
                              y = fit,
                              ymin = fit - se.fit,
                              ymax = fit + se.fit),
                          alpha = 0.2,
                          fill = "#FF3300") +
              geom_line(data = UA.x6.pred.RS,
                          aes(UA.fr$mf, fit),
                        size = 1.5,
                        color = "#FF3300") +
              xlab("% mature forest") +
              theme(panel.grid = element_blank(),
                    plot.margin = margin(5, 0, 11, 0),
                    axis.title.y = element_blank())

UA.x6.plot

#_____________________________________________________________________________________________________________
# 8d. AW ----
#_____________________________________________________________________________________________________________

# RS predictions
AW.x6.pred.RS <- as.data.frame(predict(AW.models[[6]], newdata = AW.fr, se.fit = TRUE))

AW.x6.plot <- ggplot() +
              theme_bw() +
              geom_hline(yintercept = 0) +
              geom_ribbon(data = AW.x6.pred.RS,
                          aes(x = AW.fr$mf,
                              y = fit,
                              ymin = fit - se.fit,
                              ymax = fit + se.fit),
                          alpha = 0.2,
                          fill = "#003399") +
              geom_line(data = AW.x6.pred.RS,
                          aes(AW.fr$mf, fit),
                        size = 1.5,
                        color = "#003399") +
               xlab("% mature forest") +
              theme(panel.grid = element_blank(),
                    plot.margin = margin(5, 0, 11, 0),
                    axis.title.y = element_blank())

AW.x6.plot

#_____________________________________________________________________________________________________________
# 8e. Multiple plots ----
#_____________________________________________________________________________________________________________

dMatureForest.grid <- plot_grid(WS.x6.plot, SU.x6.plot, UA.x6.plot, AW.x6.plot,
                        nrow = 1, ncol = 4,
                        rel_widths = c(1.1, 1, 1, 1))

dMatureForest.grid

#_____________________________________________________________________________________________________________
# 9. dYoungForest ----
#_____________________________________________________________________________________________________________
# 9a. WS ----
#_____________________________________________________________________________________________________________

# RS predictions
WS.x7.pred.RS <- as.data.frame(predict(WS.models[[7]], newdata = WS.fr, se.fit = TRUE))

WS.x7.plot <- ggplot() +
              theme_bw() +
              geom_hline(yintercept = 0) +
              geom_ribbon(data = WS.x7.pred.RS,
                          aes(x = WS.fr$yf,
                              y = fit,
                              ymin = fit - se.fit,
                              ymax = fit + se.fit),
                          alpha = 0.2,
                          fill = "#009900") +
              geom_line(data = WS.x7.pred.RS,
                          aes(WS.fr$yf, fit),
                        size = 1.5,
                        color = "#009900") +
               xlab("% young forest") +
              ylab(expression(paste(beta, " for dYoungForest"))) +
              theme(panel.grid = element_blank(),
                    plot.margin = margin(5, 0, 11, 0))

WS.x7.plot

#_____________________________________________________________________________________________________________
# 9b. SU ----
#_____________________________________________________________________________________________________________

# RS predictions
SU.x7.pred.RS <- as.data.frame(predict(SU.models[[7]], newdata = SU.fr, se.fit = TRUE))

SU.x7.plot <- ggplot() +
              theme_bw() +
              geom_hline(yintercept = 0) +
              geom_ribbon(data = SU.x7.pred.RS,
                          aes(x = SU.fr$yf,
                              y = fit,
                              ymin = fit - se.fit,
                              ymax = fit + se.fit),
                          alpha = 0.2,
                          fill = "#FF9900") +
              geom_line(data = SU.x7.pred.RS,
                          aes(SU.fr$yf, fit),
                        size = 1.5,
                        color = "#FF9900") +
              xlab("% young forest") +
              theme(panel.grid = element_blank(),
                    plot.margin = margin(5, 0, 11, 0),
                    axis.title.y = element_blank())

SU.x7.plot

#_____________________________________________________________________________________________________________
# 9c. UA ----
#_____________________________________________________________________________________________________________

# RS predictions
UA.x7.pred.RS <- as.data.frame(predict(UA.models[[7]], newdata = UA.fr, se.fit = TRUE))

UA.x7.plot <- ggplot() +
              theme_bw() +
              geom_hline(yintercept = 0) +
              geom_ribbon(data = UA.x7.pred.RS,
                          aes(x = UA.fr$yf,
                              y = fit,
                              ymin = fit - se.fit,
                              ymax = fit + se.fit),
                          alpha = 0.2,
                          fill = "#FF3300") +
              geom_line(data = UA.x7.pred.RS,
                          aes(UA.fr$yf, fit),
                        size = 1.5,
                        color = "#FF3300") +
              xlab("% young forest") +
              theme(panel.grid = element_blank(),
                    plot.margin = margin(5, 0, 11, 0),
                    axis.title.y = element_blank())

UA.x7.plot

#_____________________________________________________________________________________________________________
# 9d. AW ----
#_____________________________________________________________________________________________________________

# RS predictions
AW.x7.pred.RS <- as.data.frame(predict(AW.models[[7]], newdata = AW.fr, se.fit = TRUE))

AW.x7.plot <- ggplot() +
              theme_bw() +
              geom_hline(yintercept = 0) +
              geom_ribbon(data = AW.x7.pred.RS,
                          aes(x = AW.fr$yf,
                              y = fit,
                              ymin = fit - se.fit,
                              ymax = fit + se.fit),
                          alpha = 0.2,
                          fill = "#003399") +
              geom_line(data = AW.x7.pred.RS,
                          aes(AW.fr$yf, fit),
                        size = 1.5,
                        color = "#003399") +
               xlab("% young forest") +
              theme(panel.grid = element_blank(),
                    plot.margin = margin(5, 0, 11, 0),
                    axis.title.y = element_blank()) +
              scale_y_continuous(breaks = c(-1, -0.5, 0)) +
              scale_x_continuous(breaks = c(4, 9, 14))

AW.x7.plot

#_____________________________________________________________________________________________________________
# 9e. Multiple plots ----
#_____________________________________________________________________________________________________________

dYoungForest.grid <- plot_grid(WS.x7.plot, SU.x7.plot, UA.x7.plot, AW.x7.plot,
                        nrow = 1, ncol = 4,
                        rel_widths = c(1.1, 1, 1, 1))

dYoungForest.grid

#_____________________________________________________________________________________________________________
# 10. All plots ----
#_____________________________________________________________________________________________________________

plot_grid(dDeveloped.grid, dRoad.grid, TRI.grid, TPI.grid, dOpen.grid, dMatureForest.grid, dYoungForest.grid,
          nrow = 7, ncol = 1)

# w 950 h 1700

#_____________________________________________________________________________________________________________
# 11. Grouped plots for TRI and young forest ----
#_____________________________________________________________________________________________________________
# 11a. TRI
#_____________________________________________________________________________________________________________

# add identifier variable
TRI.pred.WS <- WS.x3.pred.RS %>% mutate(Season = "WS",
                                        x = WS.fr$ed)
TRI.pred.SU <- SU.x3.pred.RS %>% mutate(Season = "SU",
                                        x = SU.fr$ed)
TRI.pred.UA <- UA.x3.pred.RS %>% mutate(Season = "UA",
                                        x = UA.fr$ed)
TRI.pred.AW <- AW.x3.pred.RS %>% mutate(Season = "AW",
                                        x = AW.fr$ed)

# bind together
TRI.pred <- bind_rows(TRI.pred.WS, TRI.pred.SU, TRI.pred.UA, TRI.pred.AW)

# change factor labels
TRI.pred$Season <- factor(TRI.pred$Season,
                          levels = c("WS", "SU", "UA", "AW"),
                          labels = c("Feb-Apr", "May-Jul", "Aug-Oct", "Nov-Jan"))

# plot
ggplot() +
  theme_bw() +
  geom_hline(yintercept = 0) +
  facet_wrap(~Season,
             ncol = 4) +
  geom_ribbon(data = TRI.pred,
              aes(x = x,
                  y = fit,
                  ymin = fit - se.fit,
                  ymax = fit + se.fit,
                  fill = Season),
              alpha = 0.2) +
  geom_line(data = TRI.pred,
            aes(x = x,
                y = fit,
                color = Season),
            linewidth = 1.5) +
  xlab(expression(paste("Edge density", " ", (m/km^2)))) +
  ylab(expression(paste(beta, " for TRI"))) +
  theme(legend.position = "none",
        strip.text = element_text(face = "bold",
                                  hjust = 0),
        strip.background = element_rect(fill = "white")) +
  scale_color_manual(values = c("#009900", "#FF9900", "#FF3300", "#003399")) +
  scale_fill_manual(values = c("#009900", "#FF9900", "#FF3300", "#003399")) +
  theme(panel.grid.minor = element_blank(),
        plot.margin = margin(5, 0, 11, 0)) +
  scale_y_continuous(breaks = c(0, -0.5, -1.0)) +
  scale_x_continuous(breaks = c(1000, 3000, 5000))

#_____________________________________________________________________________________________________________
# 11b. young forest
#_____________________________________________________________________________________________________________

# add identifier variable
yf.pred.WS <- WS.x7.pred.RS %>% mutate(Season = "WS",
                                        x = WS.fr$yf)
yf.pred.SU <- SU.x7.pred.RS %>% mutate(Season = "SU",
                                        x = SU.fr$yf)
yf.pred.UA <- UA.x7.pred.RS %>% mutate(Season = "UA",
                                        x = UA.fr$yf)
yf.pred.AW <- AW.x7.pred.RS %>% mutate(Season = "AW",
                                        x = AW.fr$yf)

# bind together
yf.pred <- bind_rows(yf.pred.WS, yf.pred.SU, yf.pred.UA, yf.pred.AW)

# change factor labels
yf.pred$Season <- factor(yf.pred$Season,
                          levels = c("WS", "SU", "UA", "AW"),
                          labels = c("Feb-Apr", "May-Jul", "Aug-Oct", "Nov-Jan"))

# plot
ggplot() +
  theme_bw() +
  geom_hline(yintercept = 0) +
  facet_wrap(~Season,
             ncol = 4) +
  geom_ribbon(data = yf.pred,
              aes(x = x,
                  y = fit,
                  ymin = fit - se.fit,
                  ymax = fit + se.fit,
                  fill = Season),
              alpha = 0.2) +
  geom_line(data = yf.pred,
            aes(x = x,
                y = fit,
                color = Season),
            linewidth = 1.5) +
  xlab("% young forest") +
  ylab(expression(paste(beta, " for young forest"))) +
  theme(legend.position = "none",
        strip.text = element_text(face = "bold",
                                  hjust = 0),
        strip.background = element_rect(fill = "white")) +
  scale_color_manual(values = c("#009900", "#FF9900", "#FF3300", "#003399")) +
  scale_fill_manual(values = c("#009900", "#FF9900", "#FF3300", "#003399")) +
  theme(panel.grid.minor = element_blank(),
        plot.margin = margin(5, 0, 11, 0)) +
  scale_x_continuous(breaks = c(5, 15, 25))

# 791 x 263
