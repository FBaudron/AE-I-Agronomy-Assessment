#' ---
#' title: "TAPE radar plots Zimbabwe"
#' author: "Frédéric Baudron"
#' date: "November 8th, 2024"
#' ---


# CLEAR ENVIRONMENT-------------------------------------------------------------

rm(list = ls())


# LOADING NECESSARY PACKAGES----------------------------------------------------

if (!require('ggradar')) remotes::install_github("ricardo-bion/ggradar")

library(openxlsx)
library(janitor)
library(tidyr)
library(ggradar)
library(egg)
library(cowplot)
library(jcolors)
library(scales)
library(stringr)


# SETTING UP THE DIRECTORY & LOADING DATA---------------------------------------

setwd("D:\\Mes Donnees\\1. Cirad\\AE-I\\Evaluation of AE practices\\Zimbabwe\\")

# file.choose()

data = read.xlsx("TAPE Zimbabwe for analysis.xlsx", sheet = 1)

show_col(jcolors(palette = "pal6"))


data_mean = aggregate(Score ~ Element + LL + Treatment, FUN = mean, na.rm = TRUE, data = data)

data_mean$Score = data_mean$Score * 100 / 4

data_mean = spread(data_mean, Element, Score)

murehwa = subset(data_mean, LL == "Murehwa")
mbire = subset(data_mean, LL == "Mbire")

murehwa$Treatment = factor(murehwa$Treatment, levels = c('Traditional', 'Biochar', 'CA live mulch', 'Push-pull', 'CA dead mulch', 'Conventional'))

ggradar(
  murehwa[, c(2:12)], 
  axis.labels = c("Diversity", "Synergies", "Efficiency", "Recycling", "Resilience",
                  "Food", "Co-creation", "Social values", "Economy", "Governance"),
  plot.title = "Murehwa",
  values.radar = c("0", "50", "100"),
  grid.min = 0, grid.mid = 50, grid.max = 100,
  group.line.width = 1, 
  group.point.size = 3,
  group.colours = c("#558aa6", "#B1740F", "#D5006A", "#08585A", "#9449d2", "#BBBE64"),
  fill = TRUE, fill.alpha = 0.1,
  background.circle.colour = "white",
  gridline.mid.colour = "grey",
  legend.position = "bottom"
) +
  theme(plot.title = element_text(size = 18, face = "bold", hjust = 0.5))

ggsave("Murehwa.jpeg", units = "cm", width = 20, height = 20, dpi = 320)


mbire$Treatment = factor(mbire$Treatment, levels = c('Traditional', 'Biochar', 'CA live mulch', 'Push-pull', 'CA dead mulch', 'Conventional'))

ggradar(
  mbire[, c(2:12)], 
  axis.labels = c("Diversity", "Synergies", "Efficiency", "Recycling", "Resilience",
                  "Food", "Co-creation", "Social values", "Economy", "Governance"),
  plot.title = "Mbire",
  values.radar = c("0", "50", "100"),
  grid.min = 0, grid.mid = 50, grid.max = 100,
  group.line.width = 1, 
  group.point.size = 3,
  group.colours = c("#558aa6", "#B1740F", "#D5006A", "#08585A", "#9449d2", "#BBBE64"),
  fill = TRUE, fill.alpha = 0.1,
  background.circle.colour = "white",
  gridline.mid.colour = "grey",
  legend.position = "bottom"
) +
  theme(plot.title = element_text(size = 18, face = "bold", hjust = 0.5))

ggsave("Mbire.jpeg", units = "cm", width = 20, height = 20, dpi = 320)


