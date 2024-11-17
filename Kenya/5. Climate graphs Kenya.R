#' ---
#' title: "Climate data"
#' author: "Frédéric Baudron"
#' date: "October 22nd, 2024"
#' ---


# CLEAR ENVIRONMENT-------------------------------------------------------------

rm(list = ls())


# LOADING NECESSARY PACKAGES----------------------------------------------------

library(openxlsx)
library(tidyverse)
library(magrittr)
library(ggplot2)
library(ggthemes)

# SETTING UP THE DIRECTORY------------------------------------------------------

setwd("D:\\Mes Donnees\\1. Cirad\\AE-I\\Evaluation of AE practices\\Climate\\")


# KIAMBU------------------------------------------------------------------------

data = read.xlsx("ClimateEngine_Kiambu.xlsx", sheet = 1)

data = data[-c(1:2),]

names(data) = c("date", "precipitation", "temperature")

data$precipitation = gsub(",", ".", data$precipitation)
data$temperature = gsub(",", ".", data$temperature)

data$date = as.Date(data$date, format = "%Y-%m-%d")

data$precipitation = as.numeric(data$precipitation)
data$temperature = as.numeric(data$temperature)

data = 
  data %>%
  mutate(date = floor_date(as_date(date), "month"))


prec = data[, c(1, 2)]
temp = data[, c(1, 3)]

prec = na.omit(prec)
temp = na.omit(temp)

prec_month = aggregate(precipitation ~ date, FUN = sum, na.rm = TRUE, data = prec)
temp_month = aggregate(temperature ~ date, FUN = mean, na.rm = TRUE, data = temp)

prec_month$month = month(prec_month$date)
prec_month$year = year(prec_month$date)

temp_month$month = month(temp_month$date)
temp_month$year = year(temp_month$date)

prec_month = prec_month[, c(4, 3, 2)]
temp_month = temp_month[, c(4, 3, 2)]


prec_mean_1991_2020 = aggregate(precipitation ~ month, FUN = mean, na.rm = TRUE, data = prec_month[which(prec_month$year < 2021), ])
temp_mean_1991_2020 = aggregate(temperature ~ month, FUN = mean, na.rm = TRUE, data = temp_month[which(temp_month$year < 2021), ])

prec_mean_1991_2020$year = rep("Average (1991-2000)", nrow(prec_mean_1991_2020))
temp_mean_1991_2020$year = rep("Average (1991-2000)", nrow(temp_mean_1991_2020))


prec_2022_2024 = subset(prec_month, year > 2021)
temp_2022_2024 = subset(temp_month, year > 2021)

prec_graph = rbind(prec_mean_1991_2020, prec_2022_2024)
temp_graph = rbind(temp_mean_1991_2020, temp_2022_2024)

prec_graph = spread(prec_graph, month, precipitation)
prec_graph[is.na(prec_graph)] = 0
prec_graph = gather(prec_graph, month, precipitation, "1":"12")

temp_graph = spread(temp_graph, month, temperature)
temp_graph = gather(temp_graph, month, temperature, "1":"12")


graph = merge(prec_graph, temp_graph, by = c("year", "month"))


graph$month = as.numeric(graph$month)

graph$year = factor(graph$year, levels = c("Average (1991-2000)", "2022", "2023", "2024"))

ggplot(graph, aes(color = year, fill = year)) + ggtitle("Kiambu - Kenya") +
  geom_bar(mapping = aes(x = month, y = precipitation * max(na.omit(temperature))/max(precipitation)),
                         stat = "identity", position = "dodge", alpha = 0.7) +
  geom_point(mapping = aes(x = month, y = temperature), size = 4, alpha = 0.7) + 
  geom_line(mapping = aes(x = month, y = temperature), size = 1) + 
  scale_y_continuous(name = expression("Temperature ("~degree~"C)"),
                     sec.axis = sec_axis(~ . * max(graph$precipitation)/max(na.omit(graph$temperature)),
                                         name = "Precipitation (mm)"), limits = c(0, max(na.omit(graph$temperature))),
                     expand = c(0.01, 0.01)) +
  scale_color_manual(values = c("black", "#558aa6", "#B1740F", "#D5006A")) +
  scale_fill_manual(values = c("black", "#558aa6", "#B1740F", "#D5006A")) +
  scale_x_continuous(n.breaks = 12, expand = c(0.01, 0.01)) +
  theme_minimal() +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(size = 15),
        axis.title.y = element_text(size = 16),
        axis.title.x = element_blank(),
        axis.text = element_text(size = 14),
        plot.title = element_text(hjust = 0.5, size = 18, face = "bold", margin = margin(0, 0, 20, 0)),
        plot.margin = margin(15, 15, 15, 15))

# ggsave("Climate Kiambu - Kenya.jpeg", units = "cm", width = 30, height = 20, dpi = 320)


# MAKUENI ----------------------------------------------------------------------

data = read.xlsx("ClimateEngine_Makueni.xlsx", sheet = 1)

data = data[-c(1:2),]

names(data) = c("date", "precipitation", "temperature")

data$precipitation = gsub(",", ".", data$precipitation)
data$temperature = gsub(",", ".", data$temperature)

data$date = as.Date(data$date, format = "%Y-%m-%d")

data$precipitation = as.numeric(data$precipitation)
data$temperature = as.numeric(data$temperature)

data = 
  data %>%
  mutate(date = floor_date(as_date(date), "month"))


prec = data[, c(1, 2)]
temp = data[, c(1, 3)]

prec = na.omit(prec)
temp = na.omit(temp)

prec_month = aggregate(precipitation ~ date, FUN = sum, na.rm = TRUE, data = prec)
temp_month = aggregate(temperature ~ date, FUN = mean, na.rm = TRUE, data = temp)

prec_month$month = month(prec_month$date)
prec_month$year = year(prec_month$date)

temp_month$month = month(temp_month$date)
temp_month$year = year(temp_month$date)

prec_month = prec_month[, c(4, 3, 2)]
temp_month = temp_month[, c(4, 3, 2)]


prec_mean_1991_2020 = aggregate(precipitation ~ month, FUN = mean, na.rm = TRUE, data = prec_month[which(prec_month$year < 2021), ])
temp_mean_1991_2020 = aggregate(temperature ~ month, FUN = mean, na.rm = TRUE, data = temp_month[which(temp_month$year < 2021), ])

prec_mean_1991_2020$year = rep("Average (1991-2000)", nrow(prec_mean_1991_2020))
temp_mean_1991_2020$year = rep("Average (1991-2000)", nrow(temp_mean_1991_2020))


prec_2022_2024 = subset(prec_month, year > 2021)
temp_2022_2024 = subset(temp_month, year > 2021)

prec_graph = rbind(prec_mean_1991_2020, prec_2022_2024)
temp_graph = rbind(temp_mean_1991_2020, temp_2022_2024)

prec_graph = spread(prec_graph, month, precipitation)
prec_graph[is.na(prec_graph)] = 0
prec_graph = gather(prec_graph, month, precipitation, "1":"12")

temp_graph = spread(temp_graph, month, temperature)
temp_graph = gather(temp_graph, month, temperature, "1":"12")


graph = merge(prec_graph, temp_graph, by = c("year", "month"))


graph$month = as.numeric(graph$month)

graph$year = factor(graph$year, levels = c("Average (1991-2000)", "2022", "2023", "2024"))

ggplot(graph, aes(color = year, fill = year)) + ggtitle("Makueni - Kenya") +
  geom_bar(mapping = aes(x = month, y = precipitation * max(na.omit(temperature))/max(precipitation)),
           stat = "identity", position = "dodge", alpha = 0.7) +
  geom_point(mapping = aes(x = month, y = temperature), size = 4, alpha = 0.7) + 
  geom_line(mapping = aes(x = month, y = temperature), size = 1) + 
  scale_y_continuous(name = expression("Temperature ("~degree~"C)"),
                     sec.axis = sec_axis(~ . * max(graph$precipitation)/max(na.omit(graph$temperature)),
                                         name = "Precipitation (mm)"), limits = c(0, max(na.omit(graph$temperature))),
                     expand = c(0.01, 0.01)) +
  scale_color_manual(values = c("black", "#558aa6", "#B1740F", "#D5006A")) +
  scale_fill_manual(values = c("black", "#558aa6", "#B1740F", "#D5006A")) +
  scale_x_continuous(n.breaks = 12, expand = c(0.01, 0.01)) +
  theme_minimal() +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(size = 15),
        axis.title.y = element_text(size = 16),
        axis.title.x = element_blank(),
        axis.text = element_text(size = 14),
        plot.title = element_text(hjust = 0.5, size = 18, face = "bold", margin = margin(0, 0, 20, 0)),
        plot.margin = margin(15, 15, 15, 15))

# ggsave("Climate Makueni - Kenya.jpeg", units = "cm", width = 30, height = 20, dpi = 320)

