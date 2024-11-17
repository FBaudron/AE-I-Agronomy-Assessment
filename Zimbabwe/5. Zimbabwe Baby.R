#' ---
#' title: "Zimbabwe Baby"
#' author: "Frédéric Baudron"
#' date: "October 24th, 2024"
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

# file.choose()

data = read.csv("Data//baby_plots_data.csv")

names(data)[6:17] = c("Crip", "Cris", "Fawp", "Fawf", "Faws", "Stlkp", "Stlks",
                      "loc", "lon", "lat", "elev", "prec")

data = data[, c(1:8, 10:16, 19:26)]

mur = subset(data, District == "Murehwa")

mur$Treatment = ifelse(mur$Treatment == "Conventional practise", "0.Conventional", mur$Treatment)


# MODELS CRICKET----------------------------------------------------------------

# mod_crip = glmmTMB(Crip/10 ~ Treatment + Vstage + Ashes + Chillies + Hand.Picking +
#                      River.Sand + (1|Farmer), data = mur, family = binomial(link = "logit"))
# summary(mod_crip)
# 
# ggcoef_model(mod_crip)
# 
# plot_model(mod_crip, type = "pred", pred.type = 'fe', terms = "Treatment")
# predict_response(mod_crip, terms = "Treatment")
# 
# 
# mod_cris = glmer(Cris ~ Treatment + Vstage + Ashes + Chillies + Hand.Picking +
#                    River.Sand + (1|Farmer), data = mur, family = poisson())
# summary(mod_cris)
# 
# ggcoef_model(mod_cris)
# 
# plot_model(mod_cris, type = "pred", pred.type = 'fe', terms = "Treatment")
# predict_response(mod_cris, terms = "Treatment")


# MODELS FALL ARMYWORM----------------------------------------------------------

# mod_fawp = glmmTMB(Fawp/10 ~ Treatment + Vstage + Ashes + Chillies + Hand.Picking +
#                      River.Sand + (1|Farmer), data = mur, family = binomial(link = "logit"))
# summary(mod_fawp)
# 
# ggcoef_model(mod_fawp)
# 
# plot_model(mod_fawp, type = "pred", pred.type = 'fe', terms = "Treatment")
# predict_response(mod_fawp, terms = "Treatment")


mod_faws = glmer(Faws ~ Treatment + Vstage + Ashes + Chillies + Hand.Picking +
                   River.Sand + (1|Farmer), data = mur, family = poisson())
# summary(mod_faws)

ggcoef_model(mod_faws)

plot_model(mod_faws, type = "pred", pred.type = 'fe', terms = "Treatment")
predict_response(mod_faws, terms = "Treatment")

# pred_baby_faw = as.data.frame(predict_response(mod_faws, terms = "Treatment"))


# MODELS STALK BORER------------------------------------------------------------

# NO CONVERGENCE

# mod_Stlkp = glmmTMB(Stlkp/10 ~ Treatment + Vstage + Ashes + Chillies + Hand.Picking +
#                       River.Sand + (1|Farmer), data = mur, family = binomial(link = "logit"))
# summary(mod_Stlkp)

# NO CONVERGENCE

# mod_stlks = glmer(Stlks ~ Treatment + Vstage + Ashes + Chillies + Hand.Picking +
#                     River.Sand + (1|Farmer), data = mur, family = poisson())
# summary(mod_stlks)


# MODELS BIOMASS----------------------------------------------------------------

par(mfrow = c(1, 1))
# boxplot(mur$BIO)
mur =  mur[mur$BIO < 5.5,]
mur =  mur[mur$BIO > 0,]
# boxplot(mur$BIO)

mod_hrvt = lmer(BIO ~ Treatment + Vstage + Ashes + Chillies + Hand.Picking +
                  River.Sand + (1|Farmer), data = mur, REML = TRUE)
resid.inspect(mod_hrvt)
# summary(mod_hrvt)

ggcoef_model(mod_hrvt)

plot_model(mod_hrvt, type = "pred", pred.type = 'fe', terms = "Treatment")
predict_response(mod_hrvt, terms = "Treatment")

# pred_baby_res = as.data.frame(predict_response(mod_hrvt, terms = "Treatment"))

# pred_baby_faw$variable = rep("faw", nrow(pred_baby_faw))
# pred_baby_res$variable = rep("stover", nrow(pred_baby_res))
# 
# pred_baby = rbind(pred_baby_faw, pred_baby_res)
# 
# names(pred_baby)[1] = "treatment"
# 
# write.xlsx(pred_baby, "Prediction Zimbabwe Baby.xlsx")
# 


          