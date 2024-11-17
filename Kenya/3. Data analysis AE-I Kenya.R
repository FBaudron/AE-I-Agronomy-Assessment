#' ---
#' title: "Data analysis AE-I Kenya"
#' author: "Frédéric Baudron"
#' date: "October 23rd, 2024"
#' ---


# CLEAR ENVIRONMENT-------------------------------------------------------------

rm(list = ls())


# LOADING NECESSARY PACKAGES----------------------------------------------------

library(openxlsx)
library(lmerTest)
library(ggstats)
library(sjPlot)
library(ggeffects)
library(tidyr)


# SETTING UP THE DIRECTORY------------------------------------------------------

setwd("D:\\Mes Donnees\\1. Cirad\\AE-I\\Evaluation of AE practices\\Kenya\\")


# FUNCTION FOR MODEL PERFORMANCE------------------------------------------------

resid.inspect = function(mod, col = "black"){
  resid = resid(mod)
  fitd = fitted(mod)
  par(mfrow = c(1, 3))
  hist(resid / sd(resid, na.rm = T), 30, main = "")
  plot(fitd, resid / sd(resid, na.rm = T), col = col)
  qqnorm(resid / sd(resid, na.rm = T))
  abline(coef = c(0, 1)) }


# MAIZE IPM---------------------------------------------------------------------

# Harvest

# file.choose()

maize_ipm_hrvt = read.xlsx("Input data cleared\\maize_ipm_hrvt.xlsx", sheet = 1)

maize_ipm_hrvt$treatment = ifelse(maize_ipm_hrvt$treatment == "Control", "control", "test")

mod_maize_ipm_hrvt = lmer(yield_kg_per_hectacre ~ season + treatment + (1 | farmer_i_d_and_name), data = maize_ipm_hrvt)
# summary(mod_maize_ipm_hrvt)

resid.inspect(mod_maize_ipm_hrvt)

ggcoef_model(mod_maize_ipm_hrvt)

plot_model(mod_maize_ipm_hrvt, type = "pred", pred.type = 'fe', terms = "treatment")
predict_response(mod_maize_ipm_hrvt, terms = "treatment")


# Pest

# file.choose()

maize_ipm_pest = read.xlsx("Input data cleared\\maize_ipm_pest.xlsx", sheet = 1)

maize_ipm_pest$is_the_plant_attacked_by_any_pest = as.numeric(maize_ipm_pest$is_the_plant_attacked_by_any_pest == "Yes")

mod_maize_ipm_pest = glmer(is_the_plant_attacked_by_any_pest ~ season + treatment + (1 | farmer_i_d_and_name), data = maize_ipm_pest[which(maize_ipm_pest$the_measurement_time == "Grain filling stage"), ], family = binomial)
# summary(mod_maize_ipm_pest)

ggcoef_model(mod_maize_ipm_pest)

plot_model(mod_maize_ipm_pest, type = "pred", pred.type = 'fe', terms = "treatment")
predict_response(mod_maize_ipm_pest, terms = "treatment")


# MAIZE MANURE------------------------------------------------------------------

# Harvest

# file.choose()

maize_manure_hrvt = read.xlsx("Input data cleared\\maize_manure_hrvt.xlsx", sheet = 1)

names(maize_manure_hrvt)[22] = "yield_kg_per_hectacre"

par(mfrow = c(1, 1))

# boxplot(maize_manure_hrvt$yield_kg_per_hectacre)
maize_manure_hrvt =  maize_manure_hrvt[maize_manure_hrvt$yield_kg_per_hectacre < 12000,]
# boxplot(maize_manure_hrvt$yield_kg_per_hectacre)

mod_maize_manure_hrvt = lmer(yield_kg_per_hectacre ~ season + treatment + (1 | farmer_i_d_and_name), data = maize_manure_hrvt)
# summary(mod_maize_manure_hrvt)

resid.inspect(mod_maize_manure_hrvt)

ggcoef_model(mod_maize_manure_hrvt)

plot_model(mod_maize_manure_hrvt, type = "pred", pred.type = 'fe', terms = "treatment")
predict_response(mod_maize_manure_hrvt, terms = "treatment")


# Pest

# file.choose()

maize_manure_pest = read.xlsx("Input data cleared\\maize_manure_pest.xlsx", sheet = 1)

maize_manure_pest$is_the_plant_attacked_by_any_pest = as.numeric(maize_manure_pest$is_the_plant_attacked_by_any_pest == "Yes")

mod_maize_manure_pest = glmer(is_the_plant_attacked_by_any_pest ~ season + treatment + (1 | farmer_i_d_and_name), data = maize_manure_pest[which(maize_manure_pest$the_measurement_time == "Grain filling stage"), ], family = binomial)
# summary(mod_maize_manure_pest)

ggcoef_model(mod_maize_manure_pest)

plot_model(mod_maize_manure_pest, type = "pred", pred.type = 'fe', terms = "treatment")
predict_response(mod_maize_manure_pest, terms = "treatment")


# MAIZE TERRACES----------------------------------------------------------------

# Harvest

# file.choose()

maize_terrace_hrvt = read.xlsx("Input data cleared\\maize_terrace_hrvt.xlsx", sheet = 1)

maize_terrace_hrvt$treatment = ifelse(maize_terrace_hrvt$treatment == "Control", "control", "test")

par(mfrow = c(1, 1))

# boxplot(maize_terrace_hrvt$yield_kg_per_hectacre)
maize_terrace_hrvt =  maize_terrace_hrvt[maize_terrace_hrvt$yield_kg_per_hectacre < 10000,]
# boxplot(maize_terrace_hrvt$yield_kg_per_hectacre)

mod_maize_terrace_hrvt = lmer(yield_kg_per_hectacre ~ season + treatment + (1 | farmer_i_d_and_name), data = maize_terrace_hrvt)
# summary(mod_maize_terrace_hrvt)

resid.inspect(mod_maize_terrace_hrvt)

ggcoef_model(mod_maize_terrace_hrvt)

plot_model(mod_maize_terrace_hrvt, type = "pred", pred.type = 'fe', terms = "treatment")
predict_response(mod_maize_terrace_hrvt, terms = "treatment")


# Pest

# file.choose()

maize_terrace_pest = read.xlsx("Input data cleared\\maize_terrace_pest.xlsx", sheet = 1)

maize_terrace_pest$is_the_plant_attacked_by_any_pest = as.numeric(maize_terrace_pest$is_the_plant_attacked_by_any_pest == "Yes")

mod_maize_terrace_pest = glmer(is_the_plant_attacked_by_any_pest ~ season + treatment + (1 | farmer_i_d_and_name), data = maize_terrace_pest[which(maize_terrace_pest$the_measurement_time == "Grain filling stage"), ], family = binomial)
# summary(mod_maize_terrace_pest)

ggcoef_model(mod_maize_terrace_pest)

plot_model(mod_maize_terrace_pest, type = "pred", pred.type = 'fe', terms = "treatment")
predict_response(mod_maize_terrace_pest, terms = "treatment")


# BEAN IPM---------------------------------------------------------------------

# Harvest

# file.choose()

bean_ipm_hrvt = read.xlsx("Input data cleared\\bean_ipm_hrvt.xlsx", sheet = 1)

names(bean_ipm_hrvt)[15] = "grain_yield_kg_per_ha"

mod_bean_ipm_hrvt = lmer(grain_yield_kg_per_ha ~ season + treatment + (1 | farmer_i_d_and_name), data = bean_ipm_hrvt)
# summary(mod_bean_ipm_hrvt)

resid.inspect(mod_bean_ipm_hrvt)

ggcoef_model(mod_bean_ipm_hrvt)

plot_model(mod_bean_ipm_hrvt, type = "pred", pred.type = 'fe', terms = "treatment")
predict_response(mod_bean_ipm_hrvt, terms = "treatment")


# Pest

# file.choose()

bean_ipm_pest = read.xlsx("Input data cleared\\bean_ipm_pest.xlsx", sheet = 1)

bean_ipm_pest$is_the_plant_attacked_by_any_pest = as.numeric(bean_ipm_pest$is_the_plant_attacked_by_any_pest == "Yes")

mod_bean_ipm_pest = glmer(is_the_plant_attacked_by_any_pest ~ treatment + (1 | farmer_i_d_and_name), data = bean_ipm_pest, family = binomial)
# summary(mod_bean_ipm_pest)

ggcoef_model(mod_bean_ipm_pest)

plot_model(mod_bean_ipm_pest, type = "pred", pred.type = 'fe', terms = "treatment")
predict_response(mod_bean_ipm_pest, terms = "treatment")


# BEAN MANURE-------------------------------------------------------------------

# Harvest

# file.choose()

bean_manure_hrvt = read.xlsx("Input data cleared\\bean_manure_hrvt.xlsx", sheet = 1)

names(bean_manure_hrvt)[17] = "grain_yield_kg_per_ha"

par(mfrow = c(1, 1))
# boxplot(bean_manure_hrvt$grain_yield_kg_per_ha)
bean_manure_hrvt =  bean_manure_hrvt[bean_manure_hrvt$grain_yield_kg_per_ha < 2000,]
# boxplot(bean_manure_hrvt$grain_yield_kg_per_ha)

mod_bean_manure_hrvt = lmer(grain_yield_kg_per_ha ~ season + treatment + (1 | farmer_i_d_and_name), data = bean_manure_hrvt)
# summary(mod_bean_manure_hrvt)

resid.inspect(mod_bean_manure_hrvt)

ggcoef_model(mod_bean_manure_hrvt)

plot_model(mod_bean_manure_hrvt, type = "pred", pred.type = 'fe', terms = "treatment")
predict_response(mod_bean_manure_hrvt, terms = "treatment")


# Pest

# file.choose()

bean_manure_pest = read.xlsx("Input data cleared\\bean_manure_pest.xlsx", sheet = 1)

bean_manure_pest$is_the_plant_attacked_by_any_pest = as.numeric(bean_manure_pest$is_the_plant_attacked_by_any_pest == "Yes")

names(bean_manure_pest)

mod_bean_manure_pest = glmer(is_the_plant_attacked_by_any_pest ~ treatment + (1 | farmer_i_d_and_name), data = bean_manure_pest, family = binomial)
# summary(mod_bean_manure_pest)

ggcoef_model(mod_bean_manure_pest)

plot_model(mod_bean_manure_pest, type = "pred", pred.type = 'fe', terms = "treatment")
predict_response(mod_bean_manure_pest, terms = "treatment")


# BEAN TERRACES-----------------------------------------------------------------

# Harvest

# file.choose()

bean_terrace_hrvt = read.xlsx("Input data cleared\\bean_terrace_hrvt.xlsx", sheet = 1)

names(bean_terrace_hrvt)[15] = "grain_yield_kg_per_ha"

mod_bean_terrace_hrvt = lmer(grain_yield_kg_per_ha ~ season + treatment + (1 | farmer_i_d_and_name), data = bean_terrace_hrvt)
# summary(mod_bean_terrace_hrvt)

resid.inspect(mod_bean_terrace_hrvt)

ggcoef_model(mod_bean_terrace_hrvt)

plot_model(mod_bean_terrace_hrvt, type = "pred", pred.type = 'fe', terms = "treatment")
predict_response(mod_bean_terrace_hrvt, terms = "treatment")


# Pest

# file.choose()

bean_terrace_pest = read.xlsx("Input data cleared\\bean_terrace_pest.xlsx", sheet = 1)

bean_terrace_pest$is_the_plant_attacked_by_any_pest = as.numeric(bean_terrace_pest$is_the_plant_attacked_by_any_pest == "Yes")

mod_bean_terrace_pest = glmer(is_the_plant_attacked_by_any_pest ~ treatment + (1 | farmer_i_d_and_name), data = bean_terrace_pest, family = binomial)
# summary(mod_bean_terrace_pest)

ggcoef_model(mod_bean_terrace_pest)

plot_model(mod_bean_terrace_pest, type = "pred", pred.type = 'fe', terms = "treatment")
predict_response(mod_bean_terrace_pest, terms = "treatment")


# CABBAGE IPM-------------------------------------------------------------------

# Harvest

# file.choose()

cabbage_ipm_hrvt = read.xlsx("Input data cleared\\cabbage_ipm_hrvt.xlsx", sheet = 1)

names(cabbage_ipm_hrvt) 

par(mfrow = c(1, 1))
# boxplot(cabbage_ipm_hrvt$weight_kg)
cabbage_ipm_hrvt =  cabbage_ipm_hrvt[cabbage_ipm_hrvt$weight_kg < 6,]
# boxplot(cabbage_ipm_hrvt$weight_kg)

mod_cabbage_ipm_hrvt = lmer(weight_kg ~ season + treatment + (1 | farmer_i_d_and_name), data = cabbage_ipm_hrvt)
# summary(mod_cabbage_ipm_hrvt)

resid.inspect(mod_cabbage_ipm_hrvt)

ggcoef_model(mod_cabbage_ipm_hrvt)

plot_model(mod_cabbage_ipm_hrvt, type = "pred", pred.type = 'fe', terms = "treatment")
predict_response(mod_cabbage_ipm_hrvt, terms = "treatment")


# Pest

# file.choose()

cabbage_ipm_pest = read.xlsx("Input data cleared\\cabbage_ipm_pest.xlsx", sheet = 1)

names(cabbage_ipm_pest)

cabbage_ipm_pest$is_the_plant_attacked_by_aphids = as.numeric(cabbage_ipm_pest$is_the_plant_attacked_by_aphids == "Yes")

mod_cabbage_ipm_pest = glmer(is_the_plant_attacked_by_aphids ~ season + treatment + (1 | farmer_i_d_and_name), data = cabbage_ipm_pest, family = binomial)
# summary(mod_cabbage_ipm_pest)

ggcoef_model(mod_cabbage_ipm_pest)

plot_model(mod_cabbage_ipm_pest, type = "pred", pred.type = 'fe', terms = "treatment")
predict_response(mod_cabbage_ipm_pest, terms = "treatment")


# SPINACH COMPOST---------------------------------------------------------------

# Harvest

# file.choose()

spinash_compost = read.xlsx("Input data cleared\\spinash_compost.xlsx", sheet = 1)

spinash_compost$the_measurement_time = as.factor(spinash_compost$the_measurement_time)

par(mfrow = c(1, 1))
# boxplot(spinash_compost$vegetable_yield_kg_per_ha)
spinash_compost =  spinash_compost[spinash_compost$vegetable_yield_kg_per_ha < 25000,]
# boxplot(spinash_compost$vegetable_yield_kg_per_ha)

mod_spinash_compost_hrvt = lmer(vegetable_yield_kg_per_ha ~ season + treatment + the_measurement_time + (1 | farmer_i_d_and_name), data = spinash_compost)
# summary(mod_spinash_compost_hrvt)

resid.inspect(mod_spinash_compost_hrvt)

ggcoef_model(mod_spinash_compost_hrvt)

plot_model(mod_spinash_compost_hrvt, type = "pred", pred.type = 'fe', terms = "treatment")
predict_response(mod_spinash_compost_hrvt, terms = "treatment")


# Pest

# NOT RELIABLE MODEL: TOO MANY ZEROS

# spinash_compost$is_the_plant_attacked_by_any_pest = ifelse(spinash_compost$is_the_plant_attacked_by_any_pest == "Yes", 1, spinash_compost$is_the_plant_attacked_by_any_pest)
# spinash_compost$is_the_plant_attacked_by_any_pest = ifelse(spinash_compost$is_the_plant_attacked_by_any_pest == "No", 0, spinash_compost$is_the_plant_attacked_by_any_pest)
# 
# spinash_compost$is_the_plant_attacked_by_any_pest = as.numeric(spinash_compost$is_the_plant_attacked_by_any_pest)
# 
# mod_spinash_compost_pest = glmer(is_the_plant_attacked_by_any_pest ~ season + treatment + the_measurement_time + (1 | farmer_i_d_and_name), data = spinash_compost, family = binomial)
# summary(mod_spinash_compost_pest)
# 
# ggcoef_model(mod_spinash_compost_pest)
# 
# plot_model(mod_spinash_compost_pest, type = "pred", pred.type = 'fe', terms = "treatment")
# predict_response(mod_spinash_compost_pest, terms = "treatment")


# Color

spinash_compost$leaf_color = round(spinash_compost$leaf_color)

mod_spinash_compost_col = glmer(leaf_color ~ season + treatment + the_measurement_time + (1 | farmer_i_d_and_name), data = spinash_compost, family = poisson)
# summary(mod_spinash_compost_col)

ggcoef_model(mod_spinash_compost_col)

plot_model(mod_spinash_compost_col, type = "pred", pred.type = 'fe', terms = "treatment")
predict_response(mod_spinash_compost_col, terms = "treatment")


# SPINACH MULCH-----------------------------------------------------------------

# Harvest

# file.choose()

spinash_mulch = read.xlsx("Input data cleared\\spinash_mulch.xlsx", sheet = 1)

spinash_mulch$the_measurement_time = as.factor(spinash_mulch$the_measurement_time)

mod_spinash_mulch_hrvt = lmer(vegetable_yield_kg_per_ha ~ season + treatment + the_measurement_time + (1 | farmer_i_d_and_name), data = spinash_mulch)
# summary(mod_spinash_mulch_hrvt)

resid.inspect(mod_spinash_mulch_hrvt)

ggcoef_model(mod_spinash_mulch_hrvt)

plot_model(mod_spinash_mulch_hrvt, type = "pred", pred.type = 'fe', terms = "treatment")
predict_response(mod_spinash_mulch_hrvt, terms = "treatment")


# Pest

# file.choose()

# NOT RELIABLE MODEL: TOO MANY ZEROS

# spinash_mulch$is_the_plant_attacked_by_any_pest = as.numeric(spinash_mulch$is_the_plant_attacked_by_any_pest)
# 
# mod_spinash_mulch_pest = glmer(is_the_plant_attacked_by_any_pest ~ season + treatment + the_measurement_time + (1 | farmer_i_d_and_name), data = spinash_mulch, family = binomial)
# summary(mod_spinash_mulch_pest)
# 
# ggcoef_model(mod_spinash_mulch_pest)
# 
# plot_model(mod_spinash_mulch_pest, type = "pred", pred.type = 'fe', terms = "treatment")
# predict_response(mod_spinash_mulch_pest, terms = "treatment")


# Color

spinash_mulch$leaf_color = round(spinash_mulch$leaf_color)

mod_spinash_mulch_col = glmer(leaf_color ~ season + treatment + the_measurement_time + (1 | farmer_i_d_and_name), data = spinash_mulch, family = poisson)
# summary(mod_spinash_mulch_col)

ggcoef_model(mod_spinash_mulch_col)

plot_model(mod_spinash_mulch_col, type = "pred", pred.type = 'fe', terms = "treatment")
predict_response(mod_spinash_mulch_col, terms = "treatment")


# pred_maize_ipm_hrvt = as.data.frame(predict_response(mod_maize_ipm_hrvt, terms = "treatment"))
# pred_maize_ipm_pest = as.data.frame(predict_response(mod_maize_ipm_pest, terms = "treatment"))
# pred_maize_manure_hrvt = as.data.frame(predict_response(mod_maize_manure_hrvt, terms = "treatment"))
# pred_maize_manure_pest = as.data.frame(predict_response(mod_maize_manure_pest, terms = "treatment"))
# pred_maize_terrace_hrvt = as.data.frame(predict_response(mod_maize_terrace_hrvt, terms = "treatment"))
# pred_maize_terrace_pest = as.data.frame(predict_response(mod_maize_terrace_pest, terms = "treatment"))
# pred_bean_ipm_hrvt = as.data.frame(predict_response(mod_bean_ipm_hrvt, terms = "treatment"))
# pred_bean_ipm_pest = as.data.frame(predict_response(mod_bean_ipm_pest, terms = "treatment"))
# pred_bean_manure_hrvt = as.data.frame(predict_response(mod_bean_manure_hrvt, terms = "treatment"))
# pred_bean_manure_pest = as.data.frame(predict_response(mod_bean_manure_pest, terms = "treatment"))
# pred_bean_terrace_hrvt = as.data.frame(predict_response(mod_bean_terrace_hrvt, terms = "treatment"))
# pred_bean_terrace_pest = as.data.frame(predict_response(mod_bean_terrace_pest, terms = "treatment"))
# pred_cabbage_ipm_hrvt = as.data.frame(predict_response(mod_cabbage_ipm_hrvt, terms = "treatment"))
# pred_cabbage_ipm_pest = as.data.frame(predict_response(mod_cabbage_ipm_pest, terms = "treatment"))
# pred_spinash_compost_hrvt = as.data.frame(predict_response(mod_spinash_compost_hrvt, terms = "treatment"))
# pred_spinash_compost_col = as.data.frame(predict_response(mod_spinash_compost_col, terms = "treatment"))
# pred_spinash_mulch_hrvt = as.data.frame(predict_response(mod_spinash_mulch_hrvt, terms = "treatment"))
# pred_spinash_mulch_col = as.data.frame(predict_response(mod_spinash_mulch_col, terms = "treatment"))
# 
# pred_maize_ipm_hrvt$variable = rep("yield", nrow(pred_maize_ipm_hrvt))
# pred_maize_ipm_pest$variable = rep("pest", nrow(pred_maize_ipm_pest))
# pred_maize_ipm = rbind(pred_maize_ipm_hrvt, pred_maize_ipm_pest)
# pred_maize_ipm$technology = rep("ipm", nrow(pred_maize_ipm))
# 
# pred_maize_manure_hrvt$variable = rep("yield", nrow(pred_maize_manure_hrvt))
# pred_maize_manure_pest$variable = rep("pest", nrow(pred_maize_manure_pest))
# pred_maize_manure = rbind(pred_maize_manure_hrvt, pred_maize_manure_pest)
# pred_maize_manure$technology = rep("manure", nrow(pred_maize_manure))
# 
# pred_maize_terrace_hrvt$variable = rep("yield", nrow(pred_maize_terrace_hrvt))
# pred_maize_terrace_pest$variable = rep("pest", nrow(pred_maize_terrace_pest))
# pred_maize_terrace = rbind(pred_maize_terrace_hrvt, pred_maize_terrace_pest)
# pred_maize_terrace$technology = rep("terrace", nrow(pred_maize_terrace))
# 
# pred_maize = rbind(pred_maize_ipm, pred_maize_manure, pred_maize_terrace)
# pred_maize$crop = rep("maize", nrow(pred_maize))
# 
# 
# pred_bean_ipm_hrvt$variable = rep("yield", nrow(pred_bean_ipm_hrvt))
# pred_bean_ipm_pest$variable = rep("pest", nrow(pred_bean_ipm_pest))
# pred_bean_ipm = rbind(pred_bean_ipm_hrvt, pred_bean_ipm_pest)
# pred_bean_ipm$technology = rep("ipm", nrow(pred_bean_ipm))
# 
# pred_bean_manure_hrvt$variable = rep("yield", nrow(pred_bean_manure_hrvt))
# pred_bean_manure_pest$variable = rep("pest", nrow(pred_bean_manure_pest))
# pred_bean_manure = rbind(pred_bean_manure_hrvt, pred_bean_manure_pest)
# pred_bean_manure$technology = rep("manure", nrow(pred_bean_manure))
# 
# pred_bean_terrace_hrvt$variable = rep("yield", nrow(pred_bean_terrace_hrvt))
# pred_bean_terrace_pest$variable = rep("pest", nrow(pred_bean_terrace_pest))
# pred_bean_terrace = rbind(pred_bean_terrace_hrvt, pred_bean_terrace_pest)
# pred_bean_terrace$technology = rep("terrace", nrow(pred_bean_terrace))
# 
# pred_bean = rbind(pred_bean_ipm, pred_bean_manure, pred_bean_terrace)
# pred_bean$crop = rep("bean", nrow(pred_bean))
# 
# 
# pred_cabbage_ipm_hrvt$variable = rep("yield", nrow(pred_cabbage_ipm_hrvt))
# pred_cabbage_ipm_pest$variable = rep("pest", nrow(pred_cabbage_ipm_pest))
# pred_cabbage = rbind(pred_cabbage_ipm_hrvt, pred_cabbage_ipm_pest)
# pred_cabbage$technology = rep("ipm", nrow(pred_cabbage))
# pred_cabbage$crop = rep("cabbage", nrow(pred_cabbage))
# 
# 
# pred_spinash_compost_hrvt$variable = rep("yield", nrow(pred_maize_ipm_hrvt))
# pred_spinash_compost_col$variable = rep("color", nrow(pred_spinash_compost_col))
# pred_spinash_compost = rbind(pred_spinash_compost_hrvt, pred_spinash_compost_col)
# pred_spinash_compost$technology = rep("compost", nrow(pred_spinash_compost))
# 
# pred_spinash_mulch_hrvt$variable = rep("yield", nrow(pred_spinash_mulch_hrvt))
# pred_spinash_mulch_col$variable = rep("color", nrow(pred_spinash_mulch_col))
# pred_spinash_mulch = rbind(pred_spinash_mulch_hrvt, pred_spinash_mulch_col)
# pred_spinash_mulch$technology = rep("mulch", nrow(pred_spinash_mulch))
# 
# pred_spinash = rbind(pred_spinash_compost, pred_spinash_mulch)
# pred_spinash$crop = rep("spinach", nrow(pred_spinash))
# 
# 
# pred = rbind(pred_maize, pred_bean, pred_cabbage, pred_spinash)
# 
# names(pred)[1] = "treatment"
# 
# pred = pred[, c(9, 8, 7, 1:5)]
# 
# write.xlsx(pred, "Prediction Kenya.xlsx")
