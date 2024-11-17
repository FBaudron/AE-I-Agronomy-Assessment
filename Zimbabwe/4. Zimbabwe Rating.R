#' ---
#' title: "Zimbabwe Rating"
#' author: "Frédéric Baudron"
#' date: "October 24th, 2024"
#' ---


# CLEAR ENVIRONMENT-------------------------------------------------------------

rm(list = ls())


# LOADING NECESSARY PACKAGES----------------------------------------------------

library(ggplot2)  
library(dplyr)    
library(tidyr)    
library(sjPlot)
library(ggeffects)
library(ggstats)
library(openxlsx)


# SETTING UP THE DIRECTORY------------------------------------------------------

setwd("D:\\Mes Donnees\\1. Cirad\\AE-I\\Evaluation of AE practices\\Zimbabwe\\")


# Load the dataset

data = read.csv("Data//Rating_assessments_2023_24.csv")


data$Treatment = ifelse(data$Treatment == "Conservation Agriculture", "Conservation Agriculture Dead Mulch",
                        data$Treatment)

data$Treatment = ifelse(data$Treatment == "Conventional Practice", "0.Conventional Practice",
                        data$Treatment)


table(data$Age_group)

data$Age_group = ifelse(data$Age_group == "18 -35 (Females)", "18 - 35  (Females)", data$Age_group)
data$Age_group = ifelse(data$Age_group == "18 -35 (Males)", "18 - 35  (Males)", data$Age_group)

# Converting variables to factors

data$Indicator = as.factor(data$Indicator)
data$Age_group = as.factor(data$Age_group)
data$Year = as.factor(data$Year)
data$Treatment = as.factor(data$Treatment)


data = aggregate(. ~ Year + District + Ward + Age_group + Treatment + Indicator,
                   FUN = mean, na.rm = TRUE, data = data)

data$Rating = round(data$Rating)

data = spread(data, Indicator, Rating)

data_both_seasons = data

# Subset for common treatments
data_both_seasons = subset(data_both_seasons, Treatment %in% c("Conservation Agriculture Dead Mulch",
                                                               "0.Conventional Practice", "Push-Pull"))

# Remove columns that have some NAs
data_both_seasons = data_both_seasons[ , colSums(is.na(data_both_seasons)) == 0]

data_2023 = subset(data, Year == "2023")

data_2023 = data_2023[ , colSums(is.na(data_2023)) == 0]

data_2024 = subset(data, Year == "2024")


mod_gy_both_seasons = glm(Grain_yield ~ Year + District + (District/Ward) + Age_group + Treatment, data = data_both_seasons, family = poisson)
# summary(mod_gy_both_seasons)
ggcoef_model(mod_gy_both_seasons)
plot_model(mod_gy_both_seasons, type = "pred", pred.type = 'fe', terms = c("Age_group", "Treatment"))
predict_response(mod_gy_both_seasons, terms = c("Age_group", "Treatment"))
gy_both_seasons = as.data.frame(predict_response(mod_gy_both_seasons, terms = c("Age_group", "Treatment")))
gy_both_seasons$Indicator = rep("Grain yield")

mod_by_both_seasons = glm(Biomass_yield ~ Year + District + (District/Ward) + Age_group + Treatment, data = data_both_seasons, family = poisson)
# summary(mod_by_both_seasons)
ggcoef_model(mod_by_both_seasons)
plot_model(mod_by_both_seasons, type = "pred", pred.type = 'fe', terms = c("Age_group", "Treatment"))
predict_response(mod_by_both_seasons, terms = c("Age_group", "Treatment"))
by_both_seasons = as.data.frame(predict_response(mod_by_both_seasons, terms = c("Age_group", "Treatment")))
by_both_seasons$Indicator = rep("Biomass yield")

mod_in_both_seasons = glm(Inputs ~ Year + District + (District/Ward) + Age_group + Treatment, data = data_both_seasons, family = poisson)
# summary(mod_in_both_seasons)
ggcoef_model(mod_in_both_seasons)
plot_model(mod_in_both_seasons, type = "pred", pred.type = 'fe', terms = c("Age_group", "Treatment"))
predict_response(mod_in_both_seasons, terms = c("Age_group", "Treatment"))
in_both_seasons = as.data.frame(predict_response(mod_in_both_seasons, terms = c("Age_group", "Treatment")))
in_both_seasons$Indicator = rep("Input")

mod_lb_both_seasons = glm(Labour ~ Year + District + (District/Ward) + Age_group + Treatment, data = data_both_seasons, family = poisson)
# summary(mod_lb_both_seasons)
ggcoef_model(mod_lb_both_seasons)
plot_model(mod_lb_both_seasons, type = "pred", pred.type = 'fe', terms = c("Age_group", "Treatment"))
predict_response(mod_lb_both_seasons, terms = c("Age_group", "Treatment"))
lb_both_seasons = as.data.frame(predict_response(mod_lb_both_seasons, terms = c("Age_group", "Treatment")))
lb_both_seasons$Indicator = rep("Labour")

mod_pd_both_seasons = glm(`Pests and diseases` ~ Year + District + (District/Ward) + Age_group + Treatment, data = data_both_seasons, family = poisson)
# summary(mod_pd_both_seasons)
ggcoef_model(mod_pd_both_seasons)
plot_model(mod_pd_both_seasons, type = "pred", pred.type = 'fe', terms = c("Age_group", "Treatment"))
predict_response(mod_pd_both_seasons, terms = c("Age_group", "Treatment"))
pd_both_seasons = as.data.frame(predict_response(mod_pd_both_seasons, terms = c("Age_group", "Treatment")))
pd_both_seasons$Indicator = rep("Pests and diseases")

mod_dr_both_seasons = glm(Drought ~ Year + District + (District/Ward) + Age_group + Treatment, data = data_both_seasons, family = poisson)
# summary(mod_dr_both_seasons)
ggcoef_model(mod_dr_both_seasons)
plot_model(mod_dr_both_seasons, type = "pred", pred.type = 'fe', terms = c("Age_group", "Treatment"))
predict_response(mod_dr_both_seasons, terms = c("Age_group", "Treatment"))
dr_both_seasons = as.data.frame(predict_response(mod_dr_both_seasons, terms = c("Age_group", "Treatment")))
dr_both_seasons$Indicator = rep("Drought")

mod_sl_both_seasons = glm(Soil_conservation ~ Year + District + (District/Ward) + Age_group + Treatment, data = data_both_seasons, family = poisson)
# summary(mod_sl_both_seasons)
ggcoef_model(mod_sl_both_seasons)
plot_model(mod_sl_both_seasons, type = "pred", pred.type = 'fe', terms = c("Age_group", "Treatment"))
predict_response(mod_sl_both_seasons, terms = c("Age_group", "Treatment"))
sl_both_seasons = as.data.frame(predict_response(mod_sl_both_seasons, terms = c("Age_group", "Treatment")))
sl_both_seasons$Indicator = rep("Soil conservation")

# pred_rating_both_seasons = rbind(gy_both_seasons, by_both_seasons, in_both_seasons,
#                                  lb_both_seasons, pd_both_seasons, dr_both_seasons,
#                                  sl_both_seasons)
# 
# names(pred_rating_both_seasons)[1] = "Age group"
# 
# pred_rating_both_seasons = pred_rating_both_seasons[, c(7, 1:6)]
# 
# write.xlsx(pred_rating_both_seasons, "Prediction Zimbabwe Mother Rating.xlsx")

