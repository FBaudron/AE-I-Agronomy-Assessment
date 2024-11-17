#' ---
#' title: "Zimbabwe Mothers Harvest"
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


# Load the dataset

data = read.csv("Data//Final_Merged_2023_2024_modified.csv")

#Renaming the wards
data = data %>%
  mutate(Ward = case_when(
    Ward == "2" ~ "Ward 2",
    Ward == "3" ~ "Ward 3",
    Ward == "27" ~ "Ward 27",
    Ward == "4" ~ "Ward 4",
    TRUE ~ as.character(Ward) # Keep other values unchanged
  ))

# Subset data for the Mother trials and specific treatments
mother = subset(data, Source == "Mother" & Treatment %in% c("CA", "CONV", "CABIO", "PPULL"))

mother$Treatment = ifelse(mother$Treatment == "CA", "CABIO", mother$Treatment)

# Remove rows with zero values in the YLD and BIO columns
# mother = mother %>%
#   filter(YLD != 0, BIO != 0)

mother$Treatment = ifelse(mother$Treatment == "CONV", "0.CONV", mother$Treatment)

y_mother = mother
b_mother = mother

data = subset(data, Source == "Mother")

data$Treatment = ifelse(data$Treatment == "CA", "CABIO", data$Treatment)
data$Treatment = ifelse(data$Treatment == "CONV", "0.CONV", data$Treatment)


seas1 = subset(data, Season == "2022_2023")
seas2 = subset(data, Season == "2023_2024")

y_seas1 = seas1
b_seas1 = seas1

y_seas2 = seas2
b_seas2 = seas2


# GRAIN BOTH SEASONS------------------------------------------------------------

par(mfrow = c(1, 1))
# boxplot(y_mother$YLD)
y_mother =  y_mother[y_mother$YLD < 5.5,]
# boxplot(y_mother$YLD)

table(y_mother$Treatment)

mod_hrvt = lmer(YLD ~ Season * Treatment + District + (1|Farmer), data = y_mother, REML = TRUE)
resid.inspect(mod_hrvt)
# summary(mod_hrvt)

ggcoef_model(mod_hrvt)

plot_model(mod_hrvt, type = "pred", pred.type = 'fe', terms = "Treatment")
predict_response(mod_hrvt, terms = "Treatment")

# pred_mother_season_both_yld = as.data.frame(predict_response(mod_hrvt, terms = "Treatment"))


# BIOMASS BOTH SEASONS----------------------------------------------------------

par(mfrow = c(1, 1))
# boxplot(b_mother$BIO)
b_mother =  b_mother[b_mother$BIO < 12,]
# boxplot(b_mother$BIO)

mod_hrvt_b = lmer(BIO ~ Season * Treatment + District + (1|Farmer), data = b_mother, REML = TRUE)
resid.inspect(mod_hrvt_b)
# summary(mod_hrvt_b)

ggcoef_model(mod_hrvt_b)

plot_model(mod_hrvt_b, type = "pred", pred.type = 'fe', terms = "Treatment")
predict_response(mod_hrvt_b, terms = "Treatment")

# pred_mother_season_both_res = as.data.frame(predict_response(mod_hrvt_b, terms = "Treatment"))


# GRAIN SEASON 1----------------------------------------------------------------

par(mfrow = c(1, 1))
# boxplot(y_seas1$YLD)
y_seas1 =  y_seas1[y_seas1$YLD < 6.5,]
# boxplot(y_seas1$YLD)

mod_hrvt_y_seas1 = lmer(YLD ~ Treatment + District + (1|Farmer), data = y_seas1, REML = TRUE)
resid.inspect(mod_hrvt_y_seas1)
# summary(mod_hrvt_y_seas1)

ggcoef_model(mod_hrvt_y_seas1)

plot_model(mod_hrvt_y_seas1, type = "pred", pred.type = 'fe', terms = "Treatment")
predict_response(mod_hrvt_y_seas1, terms = "Treatment")

# pred_mother_season1_yld = as.data.frame(predict_response(mod_hrvt_y_seas1, terms = "Treatment"))


# BIOMASS SEASON 1--------------------------------------------------------------

par(mfrow = c(1, 1))
# boxplot(b_seas1$BIO)
b_seas1 =  b_seas1[b_seas1$BIO < 12.5,]
# boxplot(b_seas1$BIO)

mod_hrvt_b_seas1 = lmer(BIO ~ Treatment + District + (1|Farmer), data = b_seas1, REML = TRUE)
resid.inspect(mod_hrvt_b_seas1)
# summary(mod_hrvt_b_seas1)

ggcoef_model(mod_hrvt_b_seas1)

plot_model(mod_hrvt_b_seas1, type = "pred", pred.type = 'fe', terms = "Treatment")
predict_response(mod_hrvt_b_seas1, terms = "Treatment")

# pred_mother_season1_res = as.data.frame(predict_response(mod_hrvt_b_seas1, terms = "Treatment"))


# GRAIN SEASON 2----------------------------------------------------------------

par(mfrow = c(1, 1))
# boxplot(y_seas2$YLD)

mod_hrvt_y_seas2 = lmer(YLD ~ Treatment + District + (1|Farmer), data = y_seas2, REML = TRUE)
resid.inspect(mod_hrvt_y_seas2)
# summary(mod_hrvt_y_seas2)

ggcoef_model(mod_hrvt_y_seas2)

plot_model(mod_hrvt_y_seas2, type = "pred", pred.type = 'fe', terms = "Treatment")
predict_response(mod_hrvt_y_seas2, terms = "Treatment")

# pred_mother_season2_yld = as.data.frame(predict_response(mod_hrvt_y_seas2, terms = "Treatment"))


# BIOMASS SEASON 2--------------------------------------------------------------

par(mfrow = c(1, 1))
# boxplot(b_seas2$BIO)
b_seas2 =  b_seas2[b_seas2$BIO < 13,]
# boxplot(b_seas2$BIO)

mod_hrvt_b_seas2 = lmer(BIO ~ Treatment + District + (District/Ward) + (1|Farmer), data = b_seas2, REML = TRUE)
resid.inspect(mod_hrvt_b_seas2)
# summary(mod_hrvt_b_seas2)

ggcoef_model(mod_hrvt_b_seas2)

plot_model(mod_hrvt_b_seas2, type = "pred", pred.type = 'fe', terms = "Treatment")
predict_response(mod_hrvt_b_seas2, terms = "Treatment")

# pred_mother_season2_res = as.data.frame(predict_response(mod_hrvt_b_seas2, terms = "Treatment"))


# pred_mother_season_both_yld$season = rep("Both seasons", nrow(pred_mother_season_both_yld))
# pred_mother_season1_yld$season = rep("Season 1", nrow(pred_mother_season1_yld))
# pred_mother_season2_yld$season = rep("Season 2", nrow(pred_mother_season2_yld))
# 
# pred_mother_yld = rbind(pred_mother_season_both_yld, pred_mother_season1_yld, pred_mother_season2_yld)
# 
# pred_mother_yld$variable = rep("yield", nrow(pred_mother_yld))
# 
# pred_mother_season_both_res$season = rep("Both seasons", nrow(pred_mother_season_both_res))
# pred_mother_season1_res$season = rep("Season 1", nrow(pred_mother_season1_res))
# pred_mother_season2_res$season = rep("Season 2", nrow(pred_mother_season2_res))
# 
# pred_mother_res = rbind(pred_mother_season_both_res, pred_mother_season1_res, pred_mother_season2_res)
# 
# pred_mother_res$variable = rep("stover", nrow(pred_mother_res))
# 
# pred_mother_yld_res = rbind(pred_mother_yld, pred_mother_res)
# 
# names(pred_mother_yld_res)[1] = "treatment"
# 
# write.xlsx(pred_mother_yld_res, "Prediction Zimbabwe Mother Yield Stover.xlsx")

