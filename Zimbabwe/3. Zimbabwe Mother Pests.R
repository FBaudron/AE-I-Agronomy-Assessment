#' ---
#' title: "Zimbabwe Mothers Pests"
#' author: "Frédéric Baudron"
#' date: "October 23rd, 2024"
#' ---


# CLEAR ENVIRONMENT-------------------------------------------------------------

rm(list = ls())


# LOADING NECESSARY PACKAGES----------------------------------------------------

library(openxlsx)
library(lmerTest)
library(sjPlot)
library(ggeffects)
library(tidyr)
library(dplyr)
library(glmmTMB)
library(ggstats)


# SETTING UP THE DIRECTORY------------------------------------------------------

setwd("D:\\Mes Donnees\\1. Cirad\\AE-I\\Evaluation of AE practices\\Zimbabwe\\")


# FUNCTION FOR MODEL PERFORMANCE------------------------------------------------

resid.inspect = function(mod, col = "black"){
  resid = resid(mod)
  fitd = fitted(mod)
  par(mfrow = c(1, 3))
  hist(resid / sd(resid, na.rm = T), 30, main = "")
  plot(fitd, resid / sd(resid, na.rm = T), col = col)
  qqnorm(resid / sd(resid, na.rm = T))
  abline(coef = c(0, 1)) }

# LOADING DATASETS--------------------------------------------------------------

data = read.csv("Data//Merged_Pest_Damage_Assessment.csv")
cor = read.csv("Data//Correspondance demos baseline.csv")

# DATA MANIPULATION-------------------------------------------------------------

names(cor)[5] = "Farmer"
names(data)[5] = "Treatment"
names(data)[7] = "Crip"
names(data)[8] = "Cris"
names(data)[9] = "Fawp"
names(data)[11] = "Faws"
names(data)[12] = "Stlkp"
names(data)[13] = "Stlks"

data = merge(data, cor, by = "Farmer")

data = data[, c(15, 16, 1, 3:13)]

data = data %>%
  mutate(Ward = case_when(
    Ward == "Wd2" ~ "Ward 2",
    Ward == "Wd3" ~ "Ward 3",
    Ward == "Wd27" ~ "Ward 27",
    Ward == "Wd4" ~ "Ward 4",
    TRUE ~ as.character(Ward)
  ))

data$Farmer = as.factor(data$Farmer)
data$District = as.factor(data$District)
data$Ward = as.factor(data$Ward)
data$Season = as.factor(data$Season)
data$Treatment = as.factor(data$Treatment)

data = data %>%
  mutate(Treatment = case_when(
    Treatment == "CA" ~ "CABIO",
    # Add more cases if needed
    TRUE ~ as.character(Treatment) # Keep other values unchanged
  ))


data$Treatment = ifelse(data$Treatment == "CONV", "0.CONV", data$Treatment)

data$Treatment = as.factor(data$Treatment)

data_1 = subset(data, Period == "First")
data_1 = subset(data, Treatment %in% c("0.CONV", "CABIO", "PPULL"))

data_2 = subset(data, Season == "2022/23")
data_3 = subset(data, Season == "2023/24")


# CRICKET BOTH SEASONS----------------------------------------------------------

# mod_crip = glmmTMB(Crip/10 ~ Season * Treatment + Vstage + District + (1|Farmer), data = data_1, family = binomial(link = "logit"))
# summary(mod_crip)
# 
# plot_model(mod_crip, type = "pred", pred.type = 'fe', terms = "Treatment")
# predict_response(mod_crip, terms = "Treatment")
# 
# mod_cris = glmer(Cris ~ Season * Treatment + Vstage + District + (1|Farmer), data = data_1, family = poisson())
# summary(mod_cris)
# 
# plot_model(mod_cris, type = "pred", pred.type = 'fe', terms = "Treatment")
# predict_response(mod_cris, terms = "Treatment")


# FALL ARMYWORM BOTH SEASONS----------------------------------------------------

# mod_fawp = glmmTMB(Fawp/10 ~ Season * Treatment + Vstage + District + (1|Farmer), data = data_1, family = binomial(link = "logit"))
# summary(mod_fawp)
# 
# ggcoef_model(mod_fawp)
#
# plot_model(mod_fawp, type = "pred", pred.type = 'fe', terms = "Treatment")
# predict_response(mod_fawp, terms = "Treatment")


mod_faws = glmer(Faws ~ Season * Treatment + Vstage + District + (1|Farmer), data = data_1, family = poisson())
# summary(mod_faws)

ggcoef_model(mod_faws)

plot_model(mod_faws, type = "pred", pred.type = 'fe', terms = "Treatment")
predict_response(mod_faws, terms = "Treatment")

pred_mother_season_both_pest = as.data.frame(predict_response(mod_faws, terms = "Treatment"))


# MAIZE STALKBORER BOTH SEASONS-------------------------------------------------

# mod_Stlkp = glmmTMB(Stlkp/10 ~ Season * Treatment + Vstage + District + (1|Farmer), data = data_1, family = binomial(link = "logit"))
# summary(mod_Stlkp)
# 
# ggcoef_model(mod_Stlkp)
# 
# plot_model(mod_Stlkp, type = "pred", pred.type = 'fe', terms = "Treatment")
# predict_response(mod_Stlkp, terms = "Treatment")
# 
# 
# mod_stlks = glmer(Stlks ~ Season * Treatment + Vstage + District + (1|Farmer), data = data_1, family = poisson())
# summary(mod_stlks)
# 
# ggcoef_model(mod_stlks)
# 
# plot_model(mod_stlks, type = "pred", pred.type = 'fe', terms = "Treatment")
# predict_response(mod_stlks, terms = "Treatment")


# CRICKET SEASON 1----------------------------------------------------------

# mod_crip = glmmTMB(Crip/10 ~ Treatment + Period + Vstage + District + (1|Farmer), data = data_2, family = binomial(link = "logit"))
# summary(mod_crip)
# 
# ggcoef_model(mod_crip)
# 
# plot_model(mod_crip, type = "pred", pred.type = 'fe', terms = "Treatment")
# predict_response(mod_crip, terms = "Treatment")
# 
# 
# mod_cris = glmer(Cris ~ Treatment + Period + Vstage + District + (1|Farmer), data = data_2, family = poisson())
# summary(mod_cris)
# 
# ggcoef_model(mod_cris)
# 
# plot_model(mod_cris, type = "pred", pred.type = 'fe', terms = "Treatment")
# predict_response(mod_cris, terms = "Treatment")


# FALL ARMYWORM SEASON 1--------------------------------------------------------

# mod_fawp = glmmTMB(Fawp/10 ~ Treatment + Period + Vstage + District + (1|Farmer), data = data_2, family = binomial(link = "logit"))
# summary(mod_fawp)
# 
# ggcoef_model(mod_fawp)
# 
# plot_model(mod_fawp, type = "pred", pred.type = 'fe', terms = "Treatment")
# predict_response(mod_fawp, terms = "Treatment")


mod_faws = glmer(Faws ~ Treatment + Period + Vstage + District + (1|Farmer), data = data_2, family = poisson())
# summary(mod_faws)

ggcoef_model(mod_faws)

plot_model(mod_faws, type = "pred", pred.type = 'fe', terms = "Treatment")
predict_response(mod_faws, terms = "Treatment")

pred_mother_season1_pest = as.data.frame(predict_response(mod_faws, terms = "Treatment"))


# MAIZE STALK BORER SEASON 1----------------------------------------------------

# mod_Stlkp = glmmTMB(Stlkp/10 ~ Treatment + Period + Vstage + District + (1|Farmer), data = data_2, family = binomial(link = "logit"))
# summary(mod_Stlkp)
# 
# ggcoef_model(mod_Stlkp)
# 
# plot_model(mod_Stlkp, type = "pred", pred.type = 'fe', terms = "Treatment")
# predict_response(mod_Stlkp, terms = "Treatment")
# 
# 
# mod_stlks = glmer(Stlks ~ Treatment + Period + Vstage + District + (1|Farmer), data = data_2, family = poisson())
# summary(mod_stlks)
# 
# ggcoef_model(mod_stlks)
# 
# plot_model(mod_stlks, type = "pred", pred.type = 'fe', terms = "Treatment")
# predict_response(mod_stlks, terms = "Treatment")


# CRICKET SEASON 2----------------------------------------------------------

# mod_crip = glmmTMB(Crip/10 ~ Treatment + Vstage + District + (1|Farmer), data = data_3, family = binomial(link = "logit"))
# summary(mod_crip)
# 
# ggcoef_model(mod_crip)
# 
# plot_model(mod_crip, type = "pred", pred.type = 'fe', terms = "Treatment")
# predict_response(mod_crip, terms = "Treatment")
# 
# 
# mod_cris = glmer(Cris ~ Treatment + Vstage + District + (1|Farmer), data = data_3, family = poisson())
# summary(mod_cris)
# 
# ggcoef_model(mod_cris)
# 
# plot_model(mod_cris, type = "pred", pred.type = 'fe', terms = "Treatment")
# predict_response(mod_cris, terms = "Treatment")


# FALL ARMYWORM SEASON 2--------------------------------------------------------

# mod_fawp = glmmTMB(Fawp/10 ~ Treatment + Vstage + District + (1|Farmer), data = data_3, family = binomial(link = "logit"))
# summary(mod_fawp)
# 
# ggcoef_model(mod_fawp)
# 
# plot_model(mod_fawp, type = "pred", pred.type = 'fe', terms = "Treatment")
# predict_response(mod_fawp, terms = "Treatment")


mod_faws = glmer(Faws ~ Treatment + Vstage + District + (1|Farmer), data = data_3, family = poisson())
# summary(mod_faws)

ggcoef_model(mod_faws)

plot_model(mod_faws, type = "pred", pred.type = 'fe', terms = "Treatment")
predict_response(mod_faws, terms = "Treatment")

pred_mother_season2_pest = as.data.frame(predict_response(mod_faws, terms = "Treatment"))


# MAIZE STALKBORER SEASON 2-----------------------------------------------------

# mod_stlkp = glmmTMB(Stlkp/10 ~ Treatment + Vstage + District + (1|Farmer), data = data_3, family = binomial(link = "logit"))
# summary(mod_Stlkp)
# 
# ggcoef_model(mod_faws)
# 
# plot_model(mod_Stlkp, type = "pred", pred.type = 'fe', terms = "Treatment")
# predict_response(mod_Stlkp, terms = "Treatment")
# 
# 
# mod_stlks = glmer(Stlks ~ Treatment + Vstage + District + (1|Farmer), data = data_3, family = poisson())
# summary(mod_stlks)
# 
# ggcoef_model(mod_stlks)
# 
# plot_model(mod_stlks, type = "pred", pred.type = 'fe', terms = "Treatment")
# predict_response(mod_stlks, terms = "Treatment")


# pred_mother_season_both_pest$season = rep("Both seasons", nrow(pred_mother_season_both_pest))
# pred_mother_season1_pest$season = rep("Season 1", nrow(pred_mother_season1_pest))
# pred_mother_season2_pest$season = rep("Season 2", nrow(pred_mother_season2_pest))
# 
# pred_mother_pest = rbind(pred_mother_season_both_pest, pred_mother_season1_pest, pred_mother_season2_pest)
# 
# pred_mother_pest$variable = rep("FAW severity", nrow(pred_mother_pest))
# 
# names(pred_mother_pest)[1] = "treatment"
# 
# write.xlsx(pred_mother_pest, "Prediction Zimbabwe Mother Pests.xlsx")
