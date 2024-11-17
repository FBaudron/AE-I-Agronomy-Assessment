#' ---
#' title: "Predicted values Kenya"
#' author: "Frédéric Baudron"
#' date: "October 30th, 2024"
#' ---


# CLEAR ENVIRONMENT-------------------------------------------------------------

rm(list = ls())


# LOADING NECESSARY PACKAGES----------------------------------------------------

library(openxlsx)
library(ggplot2)
library(ggthemes)
library(jcolors)
library(scales)
library(tidyverse)
library(egg)
library(cowplot)
library(ggpubr)


# SET WORKING DIRECTORY---------------------------------------------------------

setwd("D:\\Mes Donnees\\1. Cirad\\AE-I\\Evaluation of AE practices\\Kenya\\")

# file.choose()

pred = read.xlsx("Prediction Kenya.xlsx")

pred$treatment = ifelse(pred$treatment == "Control", "control", pred$treatment)
pred$treatment = ifelse(pred$treatment == "Test", "test", pred$treatment)

maize =  subset(pred, crop == "maize")
bean =  subset(pred, crop == "bean")
cabbage =  subset(pred, crop == "cabbage")
spinach =  subset(pred, crop == "spinach")

maize_ipm =  subset(maize, technology == "ipm")
maize_manure =  subset(maize, technology == "manure")
maize_terrace =  subset(maize, technology == "terrace")

bean_ipm =  subset(bean, technology == "ipm")
bean_manure =  subset(bean, technology == "manure")
bean_terrace =  subset(bean, technology == "terrace")

cabbage_ipm = subset(cabbage, technology == "ipm")

spinach_compost = subset(spinach, technology == "compost")
spinach_mulch = subset(spinach, technology == "mulch")

maize_ipm_yld =  subset(maize_ipm, variable == "yield")
maize_ipm_pest =  subset(maize_ipm, variable == "pest")
maize_manure_yld =  subset(maize_manure, variable == "yield")
maize_manure_pest =  subset(maize_manure, variable == "pest")
maize_terrace_yld =  subset(maize_terrace, variable == "yield")
maize_terrace_pest =  subset(maize_terrace, variable == "pest")
bean_ipm_yld =  subset(bean_ipm, variable == "yield")
bean_ipm_pest =  subset(bean_ipm, variable == "pest")
bean_manure_yld =  subset(bean_manure, variable == "yield")
bean_manure_pest =  subset(bean_manure, variable == "pest")
bean_terrace_yld =  subset(bean_terrace, variable == "yield")
bean_terrace_pest =  subset(bean_terrace, variable == "pest")
cabbage_ipm_yld =  subset(cabbage_ipm, variable == "yield")
cabbage_ipm_pest =  subset(cabbage_ipm, variable == "pest")
spinach_compost_yld =  subset(spinach_compost, variable == "yield")
spinach_compost_color =  subset(spinach_compost, variable == "color")
spinach_mulch_yld =  subset(spinach_mulch, variable == "yield")
spinach_mulch_color =  subset(spinach_mulch, variable == "color")


# show_col(jcolors(palette = "pal6"))


# MAIZE-------------------------------------------------------------------------

p1 = ggplot(maize_ipm_yld, aes(x = treatment, y = predicted)) + 
  ggtitle("Maize - IPM - grain yield") +
  geom_point(size  = 8, color = "#9449d2") + 
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high),
                width = 0.2, linewidth  = 1.5, color = "#9449d2") +
  theme_bw() + 
  ylab("Predicted yield (t/ha)") + xlab("") +
  scale_x_discrete(labels = c('Control','Test')) +
  theme(plot.title = element_text(hjust = 0, size = 17, face = "bold", margin = margin(0,0,20,0)),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 15,  face = "bold", margin = margin(0,10,0,0)),
        axis.text.y = element_text(size = 13,  face = "bold"),
        axis.text.x = element_text(size = 15,  face = "bold"),
        plot.margin = margin(10, 10, 10, 10))

p2 = ggplot(maize_ipm_pest, aes(x = treatment, y = predicted)) + 
  ggtitle("Maize - IPM - pest attack") +
  geom_point(size  = 8, color = c("#9449d2", "#D5006A")) + 
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high),
                width = 0.2, linewidth  = 1.5, color = c("#9449d2", "#D5006A")) +
  theme_bw() + 
  ylab("Pest incidence") + xlab("") +
  scale_x_discrete(labels = c('Control','Test')) +
  theme(plot.title = element_text(hjust = 0, size = 17, face = "bold", margin = margin(0,0,20,0)),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 15,  face = "bold", margin = margin(0,10,0,0)),
        axis.text.y = element_text(size = 13,  face = "bold"),
        axis.text.x = element_text(size = 15,  face = "bold"),
        plot.margin = margin(10, 10, 10, 10))

p3 = ggplot(maize_manure_yld, aes(x = treatment, y = predicted)) + 
  ggtitle("Maize - manure - grain yield") +
  geom_point(size  = 8, color = c("#9449d2", "#D5006A")) + 
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high),
                width = 0.2, linewidth  = 1.5, color = c("#9449d2", "#D5006A")) +
  theme_bw() + 
  ylab("Predicted yield (t/ha)") + xlab("") +
  scale_x_discrete(labels = c('Control','Test')) +
  theme(plot.title = element_text(hjust = 0, size = 17, face = "bold", margin = margin(0,0,20,0)),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 15,  face = "bold", margin = margin(0,10,0,0)),
        axis.text.y = element_text(size = 13,  face = "bold"),
        axis.text.x = element_text(size = 15,  face = "bold"),
        plot.margin = margin(10, 10, 10, 10))

p4 = ggplot(maize_manure_pest, aes(x = treatment, y = predicted)) + 
  ggtitle("Maize - manure - pest attack") +
  geom_point(size  = 8, color = "#9449d2") + 
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high),
                width = 0.2, linewidth  = 1.5, color = "#9449d2") +
  theme_bw() + 
  ylab("Pest incidence") + xlab("") +
  scale_x_discrete(labels = c('Control','Test')) +
  theme(plot.title = element_text(hjust = 0, size = 17, face = "bold", margin = margin(0,0,20,0)),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 15,  face = "bold", margin = margin(0,10,0,0)),
        axis.text.y = element_text(size = 13,  face = "bold"),
        axis.text.x = element_text(size = 15,  face = "bold"),
        plot.margin = margin(10, 10, 10, 10))

p5 = ggplot(maize_terrace_yld, aes(x = treatment, y = predicted)) + 
  ggtitle("Maize - terrace - grain yield") +
  geom_point(size  = 8, color = "#9449d2") + 
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high),
                width = 0.2, linewidth  = 1.5, color = "#9449d2") +
  theme_bw() + 
  ylab("Predicted yield (t/ha)") + xlab("") +
  scale_x_discrete(labels = c('Control','Test')) +
  theme(plot.title = element_text(hjust = 0, size = 17, face = "bold", margin = margin(0,0,20,0)),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 15,  face = "bold", margin = margin(0,10,0,0)),
        axis.text.y = element_text(size = 13,  face = "bold"),
        axis.text.x = element_text(size = 15,  face = "bold"),
        plot.margin = margin(10, 10, 10, 10))

p6 = ggplot(maize_terrace_pest, aes(x = treatment, y = predicted)) + 
  ggtitle("Maize - terrace - pest attack") +
  geom_point(size  = 8, color = "#9449d2") + 
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high),
                width = 0.2, linewidth  = 1.5, color = "#9449d2") +
  theme_bw() + 
  ylab("Pest incidence") + xlab("") +
  scale_x_discrete(labels = c('Control','Test')) +
  theme(plot.title = element_text(hjust = 0, size = 17, face = "bold", margin = margin(0,0,20,0)),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 15,  face = "bold", margin = margin(0,10,0,0)),
        axis.text.y = element_text(size = 13,  face = "bold"),
        axis.text.x = element_text(size = 15,  face = "bold"),
        plot.margin = margin(10, 10, 10, 10))

fig1 = ggarrange(p1, p3, p5, p2, p4, p6, ncol = 3, nrow = 2, widths=c(1,1,1), heights=c(1,1))

ggdraw(fig1) +
  theme(plot.background = element_rect(fill = "white", color = NA),
        plot.margin = margin(10, 10, 10, 10))

# ggsave("Maize.png", units = "cm", width = 45, height = 25, dpi = 320)


# BEAN--------------------------------------------------------------------------

p7 = ggplot(bean_ipm_yld, aes(x = treatment, y = predicted)) + 
  ggtitle("Bean - IPM - grain yield") +
  geom_point(size  = 8, color = c("#9449d2", "#D5006A")) + 
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high),
                width = 0.2, linewidth  = 1.5, color = c("#9449d2", "#D5006A")) +
  theme_bw() + 
  ylab("Predicted yield (t/ha)") + xlab("") +
  scale_x_discrete(labels = c('Control','Test')) +
  theme(plot.title = element_text(hjust = 0, size = 17, face = "bold", margin = margin(0,0,20,0)),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 15,  face = "bold", margin = margin(0,10,0,0)),
        axis.text.y = element_text(size = 13,  face = "bold"),
        axis.text.x = element_text(size = 15,  face = "bold"),
        plot.margin = margin(10, 10, 10, 10))

p8 = ggplot(bean_ipm_pest, aes(x = treatment, y = predicted)) + 
  ggtitle("Bean - IPM - pest attack") +
  geom_point(size  = 8, color = c("#9449d2", "#D5006A")) + 
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high),
                width = 0.2, linewidth  = 1.5, color = c("#9449d2", "#D5006A")) +
  theme_bw() + 
  ylab("Pest incidence") + xlab("") +
  scale_x_discrete(labels = c('Control','Test')) +
  theme(plot.title = element_text(hjust = 0, size = 17, face = "bold", margin = margin(0,0,20,0)),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 15,  face = "bold", margin = margin(0,10,0,0)),
        axis.text.y = element_text(size = 13,  face = "bold"),
        axis.text.x = element_text(size = 15,  face = "bold"),
        plot.margin = margin(10, 10, 10, 10))

p9 = ggplot(bean_manure_yld, aes(x = treatment, y = predicted)) + 
  ggtitle("Bean - manure - grain yield") +
  geom_point(size  = 8, color = c("#9449d2", "#D5006A")) + 
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high),
                width = 0.2, linewidth  = 1.5, color = c("#9449d2", "#D5006A")) +
  theme_bw() + 
  ylab("Predicted yield (t/ha)") + xlab("") +
  scale_x_discrete(labels = c('Control','Test')) +
  theme(plot.title = element_text(hjust = 0, size = 17, face = "bold", margin = margin(0,0,20,0)),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 15,  face = "bold", margin = margin(0,10,0,0)),
        axis.text.y = element_text(size = 13,  face = "bold"),
        axis.text.x = element_text(size = 15,  face = "bold"),
        plot.margin = margin(10, 10, 10, 10))

p10 = ggplot(bean_manure_pest, aes(x = treatment, y = predicted)) + 
  ggtitle("Bean - manure - pest attack") +
  geom_point(size  = 8, color = c("#9449d2", "#D5006A")) + 
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high),
                width = 0.2, linewidth  = 1.5, color = c("#9449d2", "#D5006A")) +
  theme_bw() + 
  ylab("Pest incidence") + xlab("") +
  scale_x_discrete(labels = c('Control','Test')) +
  theme(plot.title = element_text(hjust = 0, size = 17, face = "bold", margin = margin(0,0,20,0)),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 15,  face = "bold", margin = margin(0,10,0,0)),
        axis.text.y = element_text(size = 13,  face = "bold"),
        axis.text.x = element_text(size = 15,  face = "bold"),
        plot.margin = margin(10, 10, 10, 10))

p11 = ggplot(bean_terrace_yld, aes(x = treatment, y = predicted)) + 
  ggtitle("Bean - terrace - grain yield") +
  geom_point(size  = 8, color = c("#9449d2", "#D5006A")) + 
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high),
                width = 0.2, linewidth  = 1.5, color = c("#9449d2", "#D5006A")) +
  theme_bw() + 
  ylab("Predicted yield (t/ha)") + xlab("") +
  scale_x_discrete(labels = c('Control','Test')) +
  theme(plot.title = element_text(hjust = 0, size = 17, face = "bold", margin = margin(0,0,20,0)),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 15,  face = "bold", margin = margin(0,10,0,0)),
        axis.text.y = element_text(size = 13,  face = "bold"),
        axis.text.x = element_text(size = 15,  face = "bold"),
        plot.margin = margin(10, 10, 10, 10))

p12 = ggplot(bean_terrace_pest, aes(x = treatment, y = predicted)) + 
  ggtitle("Bean - terrace - pest attack") +
  geom_point(size  = 8, color = "#9449d2") + 
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high),
                width = 0.2, linewidth  = 1.5, color = "#9449d2") +
  theme_bw() + 
  ylab("Pest incidence") + xlab("") +
  scale_x_discrete(labels = c('Control','Test')) +
  theme(plot.title = element_text(hjust = 0, size = 17, face = "bold", margin = margin(0,0,20,0)),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 15,  face = "bold", margin = margin(0,10,0,0)),
        axis.text.y = element_text(size = 13,  face = "bold"),
        axis.text.x = element_text(size = 15,  face = "bold"),
        plot.margin = margin(10, 10, 10, 10))

fig2 = ggarrange(p7, p9, p11, p8, p10, p12, ncol = 3, nrow = 2, widths=c(1,1,1), heights=c(1,1))

ggdraw(fig2) +
  theme(plot.background = element_rect(fill = "white", color = NA),
        plot.margin = margin(10, 10, 10, 10))

# ggsave("Bean.png", units = "cm", width = 45, height = 25, dpi = 320)


# CABBAGE-----------------------------------------------------------------------

p13 = ggplot(cabbage_ipm_yld, aes(x = treatment, y = predicted)) + 
  ggtitle("Cabbage - IPM - yield") +
  geom_point(size  = 8, color = "#9449d2") + 
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high),
                width = 0.2, linewidth  = 1.5, color = "#9449d2") +
  theme_bw() + 
  ylab("Predicted yield (t/ha)") + xlab("") +
  scale_x_discrete(labels = c('Control','Test')) +
  theme(plot.title = element_text(hjust = 0, size = 17, face = "bold", margin = margin(0,0,20,0)),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 15,  face = "bold", margin = margin(0,10,0,0)),
        axis.text.y = element_text(size = 13,  face = "bold"),
        axis.text.x = element_text(size = 15,  face = "bold"),
        plot.margin = margin(10, 10, 10, 10))

p14 = ggplot(cabbage_ipm_pest, aes(x = treatment, y = predicted)) + 
  ggtitle("Cabbage - IPM - pest attack") +
  geom_point(size  = 8, color = c("#9449d2", "#D5006A")) + 
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high),
                width = 0.2, linewidth  = 1.5, color = c("#9449d2", "#D5006A")) +
  theme_bw() + 
  ylab("Pest incidence") + xlab("") +
  scale_x_discrete(labels = c('Control','Test')) +
  theme(plot.title = element_text(hjust = 0, size = 17, face = "bold", margin = margin(0,0,20,0)),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 15,  face = "bold", margin = margin(0,10,0,0)),
        axis.text.y = element_text(size = 13,  face = "bold"),
        axis.text.x = element_text(size = 15,  face = "bold"),
        plot.margin = margin(10, 10, 10, 10))

fig3 = ggarrange(p13, p14, ncol = 2, nrow = 1, widths=c(1,1), heights=c(1))

ggdraw(fig3) +
  theme(plot.background = element_rect(fill = "white", color = NA),
        plot.margin = margin(10, 10, 10, 10))

# ggsave("Cabbage.png", units = "cm", width = 30, height = 13, dpi = 320)


# SPINACH-----------------------------------------------------------------------

p15 = ggplot(spinach_compost_yld, aes(x = treatment, y = predicted)) + 
  ggtitle("Spinach - compost - yield") +
  geom_point(size  = 8, color = "#9449d2") + 
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high),
                width = 0.2, linewidth  = 1.5, color = "#9449d2") +
  theme_bw() + 
  ylab("Predicted yield (t/ha)") + xlab("") +
  scale_x_discrete(labels = c('Control','Test')) +
  theme(plot.title = element_text(hjust = 0, size = 17, face = "bold", margin = margin(0,0,20,0)),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 15,  face = "bold", margin = margin(0,10,0,0)),
        axis.text.y = element_text(size = 13,  face = "bold"),
        axis.text.x = element_text(size = 15,  face = "bold"),
        plot.margin = margin(10, 10, 10, 10))

p16 = ggplot(spinach_compost_color, aes(x = treatment, y = predicted)) + 
  ggtitle("Spinach - compost - color") +
  geom_point(size  = 8, color = "#9449d2") + 
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high),
                width = 0.2, linewidth  = 1.5, color = "#9449d2") +
  theme_bw() + 
  ylab("Color") + xlab("") +
  scale_x_discrete(labels = c('Control','Test')) +
  theme(plot.title = element_text(hjust = 0, size = 17, face = "bold", margin = margin(0,0,20,0)),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 15,  face = "bold", margin = margin(0,10,0,0)),
        axis.text.y = element_text(size = 13,  face = "bold"),
        axis.text.x = element_text(size = 15,  face = "bold"),
        plot.margin = margin(10, 10, 10, 10))

p17 = ggplot(spinach_mulch_yld, aes(x = treatment, y = predicted)) + 
  ggtitle("Spinach - mulch - yield") +
  geom_point(size  = 8, color = "#9449d2") + 
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high),
                width = 0.2, linewidth  = 1.5, color = "#9449d2") +
  theme_bw() + 
  ylab("Predicted yield (t/ha)") + xlab("") +
  scale_x_discrete(labels = c('Control','Test')) +
  theme(plot.title = element_text(hjust = 0, size = 17, face = "bold", margin = margin(0,0,20,0)),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 15,  face = "bold", margin = margin(0,10,0,0)),
        axis.text.y = element_text(size = 13,  face = "bold"),
        axis.text.x = element_text(size = 15,  face = "bold"),
        plot.margin = margin(10, 10, 10, 10))

p18 = ggplot(spinach_mulch_color, aes(x = treatment, y = predicted)) + 
  ggtitle("Spinach - mulch - color") +
  geom_point(size  = 8, color = "#9449d2") + 
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high),
                width = 0.2, linewidth  = 1.5, color = "#9449d2") +
  theme_bw() + 
  ylab("Color") + xlab("") +
  scale_x_discrete(labels = c('Control','Test')) +
  theme(plot.title = element_text(hjust = 0, size = 17, face = "bold", margin = margin(0,0,20,0)),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 15,  face = "bold", margin = margin(0,10,0,0)),
        axis.text.y = element_text(size = 13,  face = "bold"),
        axis.text.x = element_text(size = 15,  face = "bold"),
        plot.margin = margin(10, 10, 10, 10))

fig4 = ggarrange(p15, p16, p17, p18, ncol = 2, nrow = 2, widths=c(1,1), heights=c(1,1))

ggdraw(fig4) +
  theme(plot.background = element_rect(fill = "white", color = NA),
        plot.margin = margin(10, 10, 10, 10))

# ggsave("Spinach.png", units = "cm", width = 30, height = 25, dpi = 320)






