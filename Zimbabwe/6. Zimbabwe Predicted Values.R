#' ---
#' title: "Predicted values Zimbabwe"
#' author: "Frédéric Baudron"
#' date: "October 27th, 2024"
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
library(patchwork)
library(cowplot)
library(ggpubr)


# SET WORKING DIRECTORY---------------------------------------------------------

setwd("D:\\Mes Donnees\\1. Cirad\\AE-I\\Evaluation of AE practices\\Zimbabwe\\")

# file.choose()

pred_yld_mother = read.xlsx("Prediction Zimbabwe Mother Yield Stover.xlsx")
pred_pest_mother = read.xlsx("Prediction Zimbabwe Mother Pests.xlsx")
pred_rat_mother = read.xlsx("Prediction Zimbabwe Mother Rating.xlsx")
pred_baby = read.xlsx("Prediction Zimbabwe Baby.xlsx")


names(pred_rat_mother)[7] = "treatment"

pred_rat_mother$Age.group = as.factor(pred_rat_mother$Age.group)
pred_rat_mother$treatment = as.factor(pred_rat_mother$treatment)

yld =  subset(pred_yld_mother, variable == "yield")
stv =  subset(pred_yld_mother, variable == "stover")

yld_both =  subset(yld, season == "Both seasons")
yld_seas1 =  subset(yld, season == "Season 1")
yld_seas2 =  subset(yld, season == "Season 2")

stv_both =  subset(stv, season == "Both seasons")
stv_seas1 =  subset(stv, season == "Season 1")
stv_seas2 =  subset(stv, season == "Season 2")

pest_both =  subset(pred_pest_mother, season == "Both seasons")
pest_seas1 =  subset(pred_pest_mother, season == "Season 1")
pest_seas2 =  subset(pred_pest_mother, season == "Season 2")

yield =  subset(pred_rat_mother, Indicator == "Grain yield")
biom =  subset(pred_rat_mother, Indicator == "Biomass yield")
inp =  subset(pred_rat_mother, Indicator == "Input")
lab =  subset(pred_rat_mother, Indicator == "Labour")
pest =  subset(pred_rat_mother, Indicator == "Pests and diseases")
drgt =  subset(pred_rat_mother, Indicator == "Drought")
soil =  subset(pred_rat_mother, Indicator == "Soil conservation")

baby_faw = subset(pred_baby, variable == "faw")
baby_stv = subset(pred_baby, variable == "stover")


# show_col(jcolors(palette = "pal6"))


# GRAIN YIELD--------------------------------------------------------------------

p1 = ggplot(yld_both, aes(x = treatment, y = predicted)) + 
  ggtitle("Mother trials - grain yield - both seasons") +
  geom_point(size  = 8, color = "#9449d2") + 
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high),
                width = 0.2, linewidth  = 1.5, color = "#9449d2") +
  theme_bw() + 
  ylab("Predicted yield (t/ha)") + xlab("") +
  scale_x_discrete(labels = ~ str_wrap(as.character(c('Conventional practice','Conservation agriculture', "Push-pull")), 10)) +
  theme(plot.title = element_text(hjust = 0, size = 17, face = "bold", margin = margin(0,0,20,0)),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 15,  face = "bold", margin = margin(0,10,0,0)),
        axis.text.y = element_text(size = 13,  face = "bold"),
        axis.text.x = element_text(size = 15,  face = "bold"),
        plot.margin = margin(10, 10, 10, 10))

p2 = ggplot(yld_seas1, aes(x = treatment, y = predicted)) + 
  ggtitle("Mother trials - grain yield - Season 1") +
  geom_point(size  = 8, color = "#9449d2") + 
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high),
                width = 0.2, linewidth  = 1.5, color = "#9449d2") +
  theme_bw() + 
  ylab("Predicted yield (t/ha)") + xlab("") +
  scale_x_discrete(labels = ~ str_wrap(as.character(c('Conventional practice','Conservation agriculture', "Push-pull")), 10)) +
  theme(plot.title = element_text(hjust = 0, size = 17, face = "bold", margin = margin(0,0,20,0)),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 15,  face = "bold", margin = margin(0,10,0,0)),
        axis.text.y = element_text(size = 13,  face = "bold"),
        axis.text.x = element_text(size = 15,  face = "bold"),
        plot.margin = margin(10, 10, 10, 10))

p3 = ggplot(yld_seas2, aes(x = treatment, y = predicted)) + 
  ggtitle("Mother trials - grain yield - Season 2") +
  geom_point(size  = 8, color = c("#9449d2", "#D5006A", "#9449d2", "#D5006A","#D5006A","#D5006A" )) + 
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high),
                width = 0.2, linewidth  = 1.5, color = c("#9449d2", "#D5006A", "#9449d2", "#D5006A","#D5006A","#D5006A")) +
  theme_bw() + 
  ylab("Predicted yield (t/ha)") + xlab("") +
  scale_x_discrete(labels = ~ str_wrap(as.character(c('Conventional practice', 'Biochar', 'Conservation agriculture', 'Conservation agriculture + Mucuna', 'Push-pull', 'Traditional practices')), 10)) +
  theme(plot.title = element_text(hjust = 0, size = 17, face = "bold", margin = margin(0,0,20,0)),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 15,  face = "bold", margin = margin(0,10,0,0)),
        axis.text.y = element_text(size = 13,  face = "bold"),
        axis.text.x = element_text(size = 15,  face = "bold"),
        plot.margin = margin(10, 10, 10, 10))

fig1 = (p1 + p2) / p3

ggdraw(fig1) +
  theme(plot.background = element_rect(fill = "white", color = NA),
        plot.margin = margin(10, 10, 10, 10))

# ggsave("Mother trials - grain yield.png", units = "cm", width = 30, height = 25, dpi = 320)


# STOVER YIELD------------------------------------------------------------------

p4 = ggplot(stv_both, aes(x = treatment, y = predicted)) + 
  ggtitle("Mother trials - stover yield - both seasons") +
  geom_point(size  = 8, color = "#9449d2") + 
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high),
                width = 0.2, linewidth  = 1.5, color = "#9449d2") +
  theme_bw() + 
  ylab("Predicted yield (t/ha)") + xlab("") +
  scale_x_discrete(labels = ~ str_wrap(as.character(c('Conventional practice','Conservation agriculture', "Push-pull")), 10)) +
  theme(plot.title = element_text(hjust = 0, size = 17, face = "bold", margin = margin(0,0,20,0)),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 15,  face = "bold", margin = margin(0,10,0,0)),
        axis.text.y = element_text(size = 13,  face = "bold"),
        axis.text.x = element_text(size = 15,  face = "bold"),
        plot.margin = margin(10, 10, 10, 10))

p5 = ggplot(stv_seas1, aes(x = treatment, y = predicted)) + 
  ggtitle("Mother trials - stover yield - Season 1") +
  geom_point(size  = 8, color = "#9449d2") + 
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high),
                width = 0.2, linewidth  = 1.5, color = "#9449d2") +
  theme_bw() + 
  ylab("Predicted yield (t/ha)") + xlab("") +
  scale_x_discrete(labels = ~ str_wrap(as.character(c('Conventional practice','Conservation agriculture', "Push-pull")), 10)) +
  theme(plot.title = element_text(hjust = 0, size = 17, face = "bold", margin = margin(0,0,20,0)),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 15,  face = "bold", margin = margin(0,10,0,0)),
        axis.text.y = element_text(size = 13,  face = "bold"),
        axis.text.x = element_text(size = 15,  face = "bold"),
        plot.margin = margin(10, 10, 10, 10))

p6 = ggplot(stv_seas2, aes(x = treatment, y = predicted)) + 
  ggtitle("Mother trials - stover yield - Season 2") +
  geom_point(size  = 8, color = c("#9449d2", "#D5006A", "#9449d2", "#D5006A","#D5006A","#D5006A" )) + 
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high),
                width = 0.2, linewidth  = 1.5, color = c("#9449d2", "#D5006A", "#9449d2", "#D5006A","#D5006A","#D5006A")) +
  theme_bw() + 
  ylab("Predicted yield (t/ha)") + xlab("") +
  scale_x_discrete(labels = ~ str_wrap(as.character(c('Conventional practice', 'Biochar', 'Conservation agriculture', 'Conservation agriculture + Mucuna', 'Push-pull', 'Traditional practices')), 10)) +
  theme(plot.title = element_text(hjust = 0, size = 17, face = "bold", margin = margin(0,0,20,0)),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 15,  face = "bold", margin = margin(0,10,0,0)),
        axis.text.y = element_text(size = 13,  face = "bold"),
        axis.text.x = element_text(size = 15,  face = "bold"),
        plot.margin = margin(10, 10, 10, 10))

fig2 = (p4 + p5) / p6

ggdraw(fig2) +
  theme(plot.background = element_rect(fill = "white", color = NA),
        plot.margin = margin(10, 10, 10, 10))

# ggsave("Mother trials - stover yield.png", units = "cm", width = 30, height = 25, dpi = 320)


# SEVERITY OF PEST ATTACK-------------------------------------------------------

p7 = ggplot(pest_both, aes(x = treatment, y = predicted)) + 
  ggtitle("Mother trials - pest attack - both seasons") +
  geom_point(size  = 8, color = c("#9449d2", "#9449d2", "#D5006A")) + 
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high),
                width = 0.2, linewidth  = 1.5, color = c("#9449d2", "#9449d2", "#D5006A")) +
  theme_bw() + 
  ylab("Severity FAW damage (1-9)") + xlab("") +
  scale_x_discrete(labels = ~ str_wrap(as.character(c('Conventional practice','Conservation agriculture', "Push-pull")), 10)) +
  theme(plot.title = element_text(hjust = 0, size = 17, face = "bold", margin = margin(0,0,20,0)),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 15,  face = "bold", margin = margin(0,10,0,0)),
        axis.text.y = element_text(size = 13,  face = "bold"),
        axis.text.x = element_text(size = 15,  face = "bold"),
        plot.margin = margin(10, 10, 10, 10))

p8 = ggplot(pest_seas1, aes(x = treatment, y = predicted)) + 
  ggtitle("Mother trials - pest attack - Season 1") +
  geom_point(size  = 8, color = c("#9449d2", "#9449d2", "#D5006A")) + 
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high),
                width = 0.2, linewidth  = 1.5, color = c("#9449d2", "#9449d2", "#D5006A")) +
  theme_bw() + 
  ylab("Severity FAW damage (1-9)") + xlab("") +
  scale_x_discrete(labels = ~ str_wrap(as.character(c('Conventional practice','Conservation agriculture', "Push-pull")), 10)) +
  theme(plot.title = element_text(hjust = 0, size = 17, face = "bold", margin = margin(0,0,20,0)),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 15,  face = "bold", margin = margin(0,10,0,0)),
        axis.text.y = element_text(size = 13,  face = "bold"),
        axis.text.x = element_text(size = 15,  face = "bold"),
        plot.margin = margin(10, 10, 10, 10))

p9 = ggplot(pest_seas2, aes(x = treatment, y = predicted)) + 
  ggtitle("Mother trials - pest attack - Season 2") +
  geom_point(size  = 8, color = "#9449d2") + 
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high),
                width = 0.2, linewidth  = 1.5, color = "#9449d2") +
  theme_bw() + 
  ylab("Severity FAW damage (1-9)") + xlab("") +
  scale_x_discrete(labels = ~ str_wrap(as.character(c('Conventional practice', 'Biochar', 'Conservation agriculture', 'Conservation agriculture + Mucuna', 'Push-pull', 'Traditional practices')), 10)) +
  theme(plot.title = element_text(hjust = 0, size = 17, face = "bold", margin = margin(0,0,20,0)),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 15,  face = "bold", margin = margin(0,10,0,0)),
        axis.text.y = element_text(size = 13,  face = "bold"),
        axis.text.x = element_text(size = 15,  face = "bold"),
        plot.margin = margin(10, 10, 10, 10))

fig3 = (p7 + p8) / p9

ggdraw(fig3) +
  theme(plot.background = element_rect(fill = "white", color = NA),
        plot.margin = margin(10, 10, 10, 10))

# ggsave("Mother trials - severity of pest attack.png", units = "cm", width = 30, height = 25, dpi = 320)


# RATING------------------------------------------------------------------------

p10 = ggplot(yield, aes(x = treatment, y = predicted)) + 
  ggtitle("Rating - grain yield") +
  geom_point(aes(color = Age.group), size = 8, position = position_dodge(0.5)) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high, color = Age.group),
                width = 0.4, linewidth  = 1.5, position = position_dodge(0.5)) +
  theme_bw() + 
  ylab("Rating (1-10)") + xlab("") + 
  scale_x_discrete(labels = ~ str_wrap(as.character(c('Conventional practice','Conservation agriculture', "Push-pull")), 10)) +
  scale_colour_jcolors(palette = "pal6") +
  theme(plot.title = element_text(hjust = 0, size = 17, face = "bold", margin = margin(0,0,20,0)),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 15,  face = "bold", margin = margin(0,10,0,0)),
        axis.text.y = element_text(size = 13,  face = "bold"),
        axis.text.x = element_text(size = 15,  face = "bold"),
        legend.title = element_blank(),
        legend.text = element_text(size = 15,  face = "bold"),
        plot.margin = margin(10, 10, 10, 10),
        legend.position = "none")

p11 = ggplot(biom, aes(x = treatment, y = predicted)) + 
  ggtitle("Mother trials - biomass yield") +
  geom_point(aes(color = Age.group), size = 8, position = position_dodge(0.5)) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high, color = Age.group),
                width = 0.4, linewidth  = 1.5, position = position_dodge(0.5)) +
  theme_bw() + 
  ylab("Rating (1-10)") + xlab("") + 
  scale_x_discrete(labels = ~ str_wrap(as.character(c('Conventional practice','Conservation agriculture', "Push-pull")), 10)) +
  scale_colour_jcolors(palette = "pal6") +
  theme(plot.title = element_text(hjust = 0, size = 17, face = "bold", margin = margin(0,0,20,0)),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 15,  face = "bold", margin = margin(0,10,0,0)),
        axis.text.y = element_text(size = 13,  face = "bold"),
        axis.text.x = element_text(size = 15,  face = "bold"),
        legend.title = element_blank(),
        legend.text = element_text(size = 15,  face = "bold"),
        plot.margin = margin(10, 10, 10, 10),
        legend.position = "none")

p12 = ggplot(inp, aes(x = treatment, y = predicted)) + 
  ggtitle("Mother trials - input reduction") +
  geom_point(aes(color = Age.group), size = 8, position = position_dodge(0.5)) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high, color = Age.group),
                width = 0.4, linewidth  = 1.5, position = position_dodge(0.5)) +
  theme_bw() + 
  ylab("Rating (1-10)") + xlab("") + 
  scale_x_discrete(labels = ~ str_wrap(as.character(c('Conventional practice','Conservation agriculture', "Push-pull")), 10)) +
  scale_colour_jcolors(palette = "pal6") +
  theme(plot.title = element_text(hjust = 0, size = 17, face = "bold", margin = margin(0,0,20,0)),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 15,  face = "bold", margin = margin(0,10,0,0)),
        axis.text.y = element_text(size = 13,  face = "bold"),
        axis.text.x = element_text(size = 15,  face = "bold"),
        legend.title = element_blank(),
        legend.text = element_text(size = 15,  face = "bold"),
        plot.margin = margin(10, 10, 10, 10),
        legend.position = "none")


p13 = ggplot(lab, aes(x = treatment, y = predicted)) + 
  ggtitle("Mother trials - labour") +
  geom_point(aes(color = Age.group), size = 8, position = position_dodge(0.5)) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high, color = Age.group),
                width = 0.4, linewidth  = 1.5, position = position_dodge(0.5)) +
  theme_bw() + 
  ylab("Rating (1-10)") + xlab("") + 
  scale_x_discrete(labels = ~ str_wrap(as.character(c('Conventional practice','Conservation agriculture', "Push-pull")), 10)) +
  scale_colour_jcolors(palette = "pal6") +
  theme(plot.title = element_text(hjust = 0, size = 17, face = "bold", margin = margin(0,0,20,0)),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 15,  face = "bold", margin = margin(0,10,0,0)),
        axis.text.y = element_text(size = 13,  face = "bold"),
        axis.text.x = element_text(size = 15,  face = "bold"),
        legend.title = element_blank(),
        legend.text = element_text(size = 15,  face = "bold"),
        plot.margin = margin(10, 10, 10, 10),
        legend.position = "none")

p14 = ggplot(pest, aes(x = treatment, y = predicted)) + 
  ggtitle("Mother trials - pest biocontrol") +
  geom_point(aes(color = Age.group), size = 8, position = position_dodge(0.5)) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high, color = Age.group),
                width = 0.4, linewidth  = 1.5, position = position_dodge(0.5)) +
  theme_bw() + 
  ylab("Rating (1-10)") + xlab("") + 
  scale_x_discrete(labels = ~ str_wrap(as.character(c('Conventional practice','Conservation agriculture', "Push-pull")), 10)) +
  scale_colour_jcolors(palette = "pal6") +
  theme(plot.title = element_text(hjust = 0, size = 17, face = "bold", margin = margin(0,0,20,0)),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 15,  face = "bold", margin = margin(0,10,0,0)),
        axis.text.y = element_text(size = 13,  face = "bold"),
        axis.text.x = element_text(size = 15,  face = "bold"),
        legend.title = element_blank(),
        legend.text = element_text(size = 15,  face = "bold"),
        plot.margin = margin(10, 10, 10, 10),
        legend.position = "none")

p15 = ggplot(drgt, aes(x = treatment, y = predicted)) + 
  ggtitle("Mother trials - drought tolerance") +
  geom_point(aes(color = Age.group), size = 8, position = position_dodge(0.5)) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high, color = Age.group),
                width = 0.4, linewidth  = 1.5, position = position_dodge(0.5)) +
  theme_bw() + 
  ylab("Rating (1-10)") + xlab("") + 
  scale_x_discrete(labels = ~ str_wrap(as.character(c('Conventional practice','Conservation agriculture', "Push-pull")), 10)) +
  scale_colour_jcolors(palette = "pal6") +
  theme(plot.title = element_text(hjust = 0, size = 17, face = "bold", margin = margin(0,0,20,0)),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 15,  face = "bold", margin = margin(0,10,0,0)),
        axis.text.y = element_text(size = 13,  face = "bold"),
        axis.text.x = element_text(size = 15,  face = "bold"),
        legend.title = element_blank(),
        legend.text = element_text(size = 15,  face = "bold"),
        plot.margin = margin(10, 10, 10, 10),
        legend.position = "none")

p16 = ggplot(soil, aes(x = treatment, y = predicted)) + 
  ggtitle("Mother trials - soil conservation") +
  geom_point(aes(color = Age.group), size = 8, position = position_dodge(0.5)) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high, color = Age.group),
                width = 0.4, linewidth  = 1.5, position = position_dodge(0.5)) +
  theme_bw() + 
  ylab("Rating (1-10)") + xlab("") + 
  scale_x_discrete(labels = ~ str_wrap(as.character(c('Conventional practice','Conservation agriculture', "Push-pull")), 10)) +
  scale_colour_jcolors(palette = "pal6") +
  theme(plot.title = element_text(hjust = 0, size = 17, face = "bold", margin = margin(0,0,20,0)),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 15,  face = "bold", margin = margin(0,10,0,0)),
        axis.text.y = element_text(size = 13,  face = "bold"),
        axis.text.x = element_text(size = 15,  face = "bold"),
        legend.title = element_blank(),
        legend.text = element_text(size = 15,  face = "bold"),
        plot.margin = margin(10, 10, 10, 10),
        legend.position = "right")

legend = get_legend(p16)

fig4 = ggarrange(p10, p11, p12, p13, p14, p15, p16 + theme(legend.position = "none"), as_ggplot(legend), ncol=4, nrow=2, widths=c(1,1,1,1), heights=c(1,1))

ggdraw(fig4) +
  theme(plot.background = element_rect(fill = "white", color = NA),
        plot.margin = margin(10, 10, 10, 10))

# ggsave("Mother trials - rating.png", units = "cm", width = 55, height = 25, dpi = 320)


# BABY TRIALS-------------------------------------------------------------------

p17 = ggplot(baby_stv, aes(x = treatment, y = predicted)) + 
  ggtitle("Baby trials - stover yield - Season 2") +
  geom_point(size  = 8, color = c("#9449d2", "#D5006A", "#D5006A")) + 
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high),
                width = 0.2, linewidth  = 1.5, color = c("#9449d2", "#D5006A", "#D5006A")) +
  theme_bw() + 
  ylab("Predicted yield (t/ha)") + xlab("") +
  scale_x_discrete(labels = ~ str_wrap(as.character(c('Conventional practice','Conservation agriculture', "Push-pull")), 10)) +
  theme(plot.title = element_text(hjust = 0, size = 17, face = "bold", margin = margin(0,0,20,0)),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 15,  face = "bold", margin = margin(0,10,0,0)),
        axis.text.y = element_text(size = 13,  face = "bold"),
        axis.text.x = element_text(size = 15,  face = "bold"),
        plot.margin = margin(10, 10, 10, 10))

p18 = ggplot(baby_faw, aes(x = treatment, y = predicted)) + 
  ggtitle("Baby trials - pest attack - Season 2") +
  geom_point(size  = 8, color = c("#9449d2", "#D5006A", "#9449d2")) + 
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high),
                width = 0.2, linewidth  = 1.5, color = c("#9449d2", "#D5006A", "#9449d2")) +
  theme_bw() + 
  ylab("Severity FAW damage (1-9)") + xlab("") +
  scale_x_discrete(labels = ~ str_wrap(as.character(c('Conventional practice','Conservation agriculture', "Push-pull")), 10)) +
  theme(plot.title = element_text(hjust = 0, size = 17, face = "bold", margin = margin(0,0,20,0)),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 15,  face = "bold", margin = margin(0,10,0,0)),
        axis.text.y = element_text(size = 13,  face = "bold"),
        axis.text.x = element_text(size = 15,  face = "bold"),
        plot.margin = margin(10, 10, 10, 10))

fig5 = ggarrange(p17, p18, ncol = 2, nrow = 1, widths=c(1,1), heights=c(1))

ggdraw(fig5) +
  theme(plot.background = element_rect(fill = "white", color = NA),
        plot.margin = margin(10, 10, 10, 10))

# ggsave("Baby trials.png", units = "cm", width = 30, height = 15, dpi = 320)


