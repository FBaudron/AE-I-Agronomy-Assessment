#' ---
#' title: "Data cleaning AE-I Kenya"
#' author: "Frédéric Baudron"
#' date: "October 21st, 2024"
#' ---


# CLEAR ENVIRONMENT-------------------------------------------------------------

rm(list = ls())


# LOADING NECESSARY PACKAGES----------------------------------------------------

library(janitor)
library(openxlsx)


# SETTING UP THE DIRECTORY------------------------------------------------------

setwd("D:\\Mes Donnees\\1. Cirad\\AE-I\\Evaluation of AE practices\\Kenya\\")


# BEANS IPM---------------------------------------------------------------------

# file.choose()

data_b_ipm = read.xlsx("Kenya Trial Data\\Kenya Trial Data\\Beans_IPM_Trial_Season 1&2.xlsx", sheet = 1)
pest_test_b_ipm = read.xlsx("Kenya Trial Data\\Kenya Trial Data\\Beans_IPM_Trial_Season 1&2.xlsx", sheet = 2)
pest_cont_b_ipm = read.xlsx("Kenya Trial Data\\Kenya Trial Data\\Beans_IPM_Trial_Season 1&2.xlsx", sheet = 3)
hrvt_test_b_ipm = read.xlsx("Kenya Trial Data\\Kenya Trial Data\\Beans_IPM_Trial_Season 1&2.xlsx", sheet = 4)
hrvt_cont_b_ipm = read.xlsx("Kenya Trial Data\\Kenya Trial Data\\Beans_IPM_Trial_Season 1&2.xlsx", sheet = 5)


data_b_ipm = clean_names(data_b_ipm)
pest_test_b_ipm = clean_names(pest_test_b_ipm)
pest_cont_b_ipm = clean_names(pest_cont_b_ipm)
hrvt_test_b_ipm = clean_names(hrvt_test_b_ipm)
hrvt_cont_b_ipm = clean_names(hrvt_cont_b_ipm)


data_b_ipm = data_b_ipm[, c(1, 38, 3, 6:9)]
names(data_b_ipm)[1] = "season"
# 45352 is 01/03/2024 as julian date
data_b_ipm$season = ifelse(data_b_ipm$season < 45352, "Season1", "Season2")
names(data_b_ipm)[2] = "id"


pest_test_b_ipm = pest_test_b_ipm[, c(8, 1:4)]
names(pest_test_b_ipm)[1] = "id"
pest_test_b_ipm$treatment = rep("test", nrow(pest_test_b_ipm))

pest_cont_b_ipm = pest_cont_b_ipm[, c(9, 1, 3, 5:6)]
names(pest_cont_b_ipm)[1] = "id"
pest_cont_b_ipm$treatment = rep("control", nrow(pest_cont_b_ipm))

pest_b_ipm = rbind(pest_test_b_ipm, pest_cont_b_ipm)

pest_b_ipm = merge(data_b_ipm, pest_b_ipm, by = "id", all.y = TRUE)


hrvt_test_b_ipm = hrvt_test_b_ipm[, c(13, 1:2, 5:10)]
names(hrvt_test_b_ipm)[1] = "id"
hrvt_test_b_ipm$treatment = rep("test", nrow(hrvt_test_b_ipm))

hrvt_cont_b_ipm = hrvt_cont_b_ipm[, c(13, 1:2, 5:10)]
names(hrvt_cont_b_ipm)[1] = "id"
hrvt_cont_b_ipm$treatment = rep("control", nrow(hrvt_cont_b_ipm))

hrvt_b_ipm = rbind(hrvt_test_b_ipm, hrvt_cont_b_ipm)

hrvt_b_ipm = merge(data_b_ipm, hrvt_b_ipm, by = "id", all.y = TRUE)


# BEANS MANURE------------------------------------------------------------------

# file.choose()

data_b_man = read.xlsx("Kenya Trial Data\\Kenya Trial Data\\Beans_Manure__Trial_Season 1&2.xlsx", sheet = 1)
pest_test_b_man1 = read.xlsx("Kenya Trial Data\\Kenya Trial Data\\Beans_Manure__Trial_Season 1&2.xlsx", sheet = 2)
pest_cont_b_man1 = read.xlsx("Kenya Trial Data\\Kenya Trial Data\\Beans_Manure__Trial_Season 1&2.xlsx", sheet = 3)
pest_test_b_man2 = read.xlsx("Kenya Trial Data\\Kenya Trial Data\\Beans_Manure__Trial_Season 1&2.xlsx", sheet = 4)
pest_cont_b_man2 = read.xlsx("Kenya Trial Data\\Kenya Trial Data\\Beans_Manure__Trial_Season 1&2.xlsx", sheet = 5)
hrvt_test_b_man = read.xlsx("Kenya Trial Data\\Kenya Trial Data\\Beans_Manure__Trial_Season 1&2.xlsx", sheet = 6)
hrvt_cont_b_man = read.xlsx("Kenya Trial Data\\Kenya Trial Data\\Beans_Manure__Trial_Season 1&2.xlsx", sheet = 7)


data_b_man = clean_names(data_b_man)
pest_test_b_man1 = clean_names(pest_test_b_man1)
pest_cont_b_man1 = clean_names(pest_cont_b_man1)
pest_test_b_man2 = clean_names(pest_test_b_man2)
pest_cont_b_man2 = clean_names(pest_cont_b_man2)
hrvt_test_b_man = clean_names(hrvt_test_b_man)
hrvt_cont_b_man = clean_names(hrvt_cont_b_man)


data_b_man = data_b_man[, c(1, 37, 3, 6:9)]
names(data_b_man)[1] = "season"
data_b_man$season = ifelse(data_b_man$season < 45352, "Season1", "Season2")
names(data_b_man)[2] = "id"


pest_test_b_man1 = pest_test_b_man1[, c(7, 1:4)]
names(pest_test_b_man1)[1] = "id"
pest_test_b_man1$treatment = rep("test", nrow(pest_test_b_man1))

pest_cont_b_man1 = pest_cont_b_man1[, c(9, 1, 3:5)]
names(pest_cont_b_man1)[1] = "id"
pest_cont_b_man1$treatment = rep("control", nrow(pest_cont_b_man1))

pest_test_b_man2 = pest_test_b_man2[, c(7, 1:2, 5:6)]
names(pest_test_b_man2)[1] = "id"
pest_test_b_man2$treatment = rep("test", nrow(pest_test_b_man2))

pest_cont_b_man2 = pest_cont_b_man2[, c(9, 1, 3, 6:7)]
names(pest_cont_b_man2)[1] = "id"
pest_cont_b_man2$treatment = rep("control", nrow(pest_cont_b_man2))

pest_b_man = rbind(pest_test_b_man1, pest_cont_b_man1, pest_test_b_man2, pest_cont_b_man2)

pest_b_man = merge(data_b_man, pest_b_man, by = "id", all.y = TRUE)


hrvt_test_b_man = hrvt_test_b_man[, c(12, 1:2, 5:9)]
names(hrvt_test_b_man)[1] = "id"
hrvt_test_b_man$treatment = rep("test", nrow(hrvt_test_b_man))

hrvt_cont_b_man = hrvt_cont_b_man[, c(12, 1:2, 5:9)]
names(hrvt_cont_b_man)[1] = "id"
hrvt_cont_b_man$treatment = rep("control", nrow(hrvt_cont_b_man))

hrvt_b_man = rbind(hrvt_test_b_man, hrvt_cont_b_man)

hrvt_b_man = merge(data_b_man, hrvt_b_man, by = "id", all.y = TRUE)


# BEANS TERRACES----------------------------------------------------------------

# file.choose()

data_b_ter = read.xlsx("Kenya Trial Data\\Kenya Trial Data\\Beans_Terraces__Trial_Season 1&2.xlsx", sheet = 1)
pest_test_b_ter = read.xlsx("Kenya Trial Data\\Kenya Trial Data\\Beans_Terraces__Trial_Season 1&2.xlsx", sheet = 2)
pest_cont_b_ter = read.xlsx("Kenya Trial Data\\Kenya Trial Data\\Beans_Terraces__Trial_Season 1&2.xlsx", sheet = 3)
hrvt_test_b_ter = read.xlsx("Kenya Trial Data\\Kenya Trial Data\\Beans_Terraces__Trial_Season 1&2.xlsx", sheet = 4)
hrvt_cont_b_ter = read.xlsx("Kenya Trial Data\\Kenya Trial Data\\Beans_Terraces__Trial_Season 1&2.xlsx", sheet = 5)


data_b_ter = clean_names(data_b_ter)
pest_test_b_ter = clean_names(pest_test_b_ter)
pest_cont_b_ter = clean_names(pest_cont_b_ter)
hrvt_test_b_ter = clean_names(hrvt_test_b_ter)
hrvt_cont_b_ter = clean_names(hrvt_cont_b_ter)


data_b_ter = data_b_ter[, c(1, 41, 3, 6:9)]
names(data_b_ter)[1] = "season"
data_b_ter$season = ifelse(data_b_ter$season < 45352, "Season1", "Season2")
names(data_b_ter)[2] = "id"

pest_test_b_ter = pest_test_b_ter[, c(7, 1:4)]
names(pest_test_b_ter)[1] = "id"
pest_test_b_ter$treatment = rep("test", nrow(pest_test_b_ter))

pest_cont_b_ter = pest_cont_b_ter[, c(9, 1, 3, 5:6)]
names(pest_cont_b_ter)[1] = "id"
pest_cont_b_ter$treatment = rep("control", nrow(pest_cont_b_ter))

pest_b_ter = rbind(pest_test_b_ter, pest_cont_b_ter)

pest_b_ter = merge(data_b_ter, pest_b_ter, by = "id", all.y = TRUE)


hrvt_test_b_ter = hrvt_test_b_ter[, c(12, 1:2, 5:9)]
names(hrvt_test_b_ter)[1] = "id"
hrvt_test_b_ter$treatment = rep("test", nrow(hrvt_test_b_ter))


hrvt_cont_b_ter = hrvt_cont_b_ter[, c(12, 1:2, 5:9)]
names(hrvt_cont_b_ter)[1] = "id"
hrvt_cont_b_ter$treatment = rep("control", nrow(hrvt_cont_b_ter))

hrvt_b_ter = rbind(hrvt_test_b_ter, hrvt_cont_b_ter)

hrvt_b_ter = merge(data_b_ter, hrvt_b_ter, by = "id", all.y = TRUE)


# MAIZE IPM---------------------------------------------------------------------

# file.choose()

data_m_ipm = read.xlsx("Kenya Trial Data\\Kenya Trial Data\\Maize_IPM_Trial_Season 1&2.xlsx", sheet = 1)
pest_test_m_ipm1 = read.xlsx("Kenya Trial Data\\Kenya Trial Data\\Maize_IPM_Trial_Season 1&2.xlsx", sheet = 2)
pest_cont_m_ipm1 = read.xlsx("Kenya Trial Data\\Kenya Trial Data\\Maize_IPM_Trial_Season 1&2.xlsx", sheet = 3)
pest_test_m_ipm2 = read.xlsx("Kenya Trial Data\\Kenya Trial Data\\Maize_IPM_Trial_Season 1&2.xlsx", sheet = 6)
pest_cont_m_ipm2 = read.xlsx("Kenya Trial Data\\Kenya Trial Data\\Maize_IPM_Trial_Season 1&2.xlsx", sheet = 7)
hrvt_test_m_ipm = read.xlsx("Kenya Trial Data\\Kenya Trial Data\\Maize_IPM_Trial_Season 1&2.xlsx", sheet = 8)
hrvt_cont_m_ipm = read.xlsx("Kenya Trial Data\\Kenya Trial Data\\Maize_IPM_Trial_Season 1&2.xlsx", sheet = 9)
pest_test_m_ipm3 = read.xlsx("Kenya Trial Data\\Kenya Trial Data\\Maize_IPM_Trial_Season 1&2.xlsx", sheet = 10)
pest_cont_m_ipm3 = read.xlsx("Kenya Trial Data\\Kenya Trial Data\\Maize_IPM_Trial_Season 1&2.xlsx", sheet = 11)


data_m_ipm = clean_names(data_m_ipm)
pest_test_m_ipm1 = clean_names(pest_test_m_ipm1)
pest_cont_m_ipm1 = clean_names(pest_cont_m_ipm1)
pest_test_m_ipm2 = clean_names(pest_test_m_ipm2)
pest_cont_m_ipm2 = clean_names(pest_cont_m_ipm2)
pest_test_m_ipm3 = clean_names(pest_test_m_ipm3)
pest_cont_m_ipm3 = clean_names(pest_cont_m_ipm3)
hrvt_test_m_ipm = clean_names(hrvt_test_m_ipm)
hrvt_cont_m_ipm = clean_names(hrvt_cont_m_ipm)


data_m_ipm = data_m_ipm[, c(1, 42, 3, 6:9)]
names(data_m_ipm)[1] = "season"
data_m_ipm$season = ifelse(data_m_ipm$season < 45352, "Season1", "Season2")
names(data_m_ipm)[2] = "id"


pest_test_m_ipm1 = pest_test_m_ipm1[, c(8, 1:4)]
names(pest_test_m_ipm1)[1] = "id"
pest_test_m_ipm1$treatment = rep("test", nrow(pest_test_m_ipm1))

pest_cont_m_ipm1 = pest_cont_m_ipm1[, c(7, 1, 3, 5:6)]
names(pest_cont_m_ipm1)[1] = "id"
pest_cont_m_ipm1$treatment = rep("control", nrow(pest_cont_m_ipm1))

pest_test_m_ipm2 = pest_test_m_ipm2[, c(8, 1:4)]
names(pest_test_m_ipm2)[1] = "id"
pest_test_m_ipm2$treatment = rep("test", nrow(pest_test_m_ipm2))

pest_cont_m_ipm2 = pest_cont_m_ipm2[, c(9, 1, 3, 5:6)]
names(pest_cont_m_ipm2)[1] = "id"
pest_cont_m_ipm2$treatment = rep("control", nrow(pest_cont_m_ipm2))

pest_test_m_ipm3 = pest_test_m_ipm3[, c(8, 1:2, 5:6)]
names(pest_test_m_ipm3)[1] = "id"
pest_test_m_ipm3$treatment = rep("test", nrow(pest_test_m_ipm3))

pest_cont_m_ipm3 = pest_cont_m_ipm3[, c(11, 1, 3, 7:8)]
names(pest_cont_m_ipm3)[1] = "id"
pest_cont_m_ipm3$treatment = rep("control", nrow(pest_cont_m_ipm3))

pest_m_ipm = rbind(pest_test_m_ipm1, pest_cont_m_ipm1, pest_test_m_ipm2, pest_cont_m_ipm2,
                   pest_test_m_ipm3, pest_cont_m_ipm3)

pest_m_ipm = merge(data_m_ipm, pest_m_ipm, by = "id", all.y = TRUE)


hrvt_test_m_ipm = hrvt_test_m_ipm[, c(14, 1:11)]
names(hrvt_test_m_ipm)[1] = "id"
hrvt_test_m_ipm$treatment = rep("test", nrow(hrvt_test_m_ipm))


hrvt_cont_m_ipm = hrvt_cont_m_ipm[, c(14, 1:11)]
names(hrvt_cont_m_ipm)[1] = "id"
hrvt_cont_m_ipm$treatment = rep("control", nrow(hrvt_cont_m_ipm))
names(hrvt_cont_m_ipm)[8] = "moisture_content"

hrvt_m_ipm = rbind(hrvt_test_m_ipm, hrvt_cont_m_ipm)

hrvt_m_ipm = merge(data_m_ipm, hrvt_m_ipm, by = "id", all.y = TRUE)


# MAIZE MANURE------------------------------------------------------------------

# file.choose()

data_m_man = read.xlsx("Kenya Trial Data\\Kenya Trial Data\\Maize_Manure_Trial_ Season 1&2.xlsx", sheet = 1)
pest_test_m_man1 = read.xlsx("Kenya Trial Data\\Kenya Trial Data\\Maize_Manure_Trial_ Season 1&2.xlsx", sheet = 2)
pest_cont_m_man1 = read.xlsx("Kenya Trial Data\\Kenya Trial Data\\Maize_Manure_Trial_ Season 1&2.xlsx", sheet = 3)
pest_test_m_man2 = read.xlsx("Kenya Trial Data\\Kenya Trial Data\\Maize_Manure_Trial_ Season 1&2.xlsx", sheet = 6)
pest_cont_m_man2 = read.xlsx("Kenya Trial Data\\Kenya Trial Data\\Maize_Manure_Trial_ Season 1&2.xlsx", sheet = 7)
hrvt_test_m_man = read.xlsx("Kenya Trial Data\\Kenya Trial Data\\Maize_Manure_Trial_ Season 1&2.xlsx", sheet = 8)
hrvt_cont_m_man = read.xlsx("Kenya Trial Data\\Kenya Trial Data\\Maize_Manure_Trial_ Season 1&2.xlsx", sheet = 9)
pest_test_m_man3 = read.xlsx("Kenya Trial Data\\Kenya Trial Data\\Maize_Manure_Trial_ Season 1&2.xlsx", sheet = 10)
pest_cont_m_man3 = read.xlsx("Kenya Trial Data\\Kenya Trial Data\\Maize_Manure_Trial_ Season 1&2.xlsx", sheet = 11)


data_m_man = clean_names(data_m_man)
pest_test_m_man1 = clean_names(pest_test_m_man1)
pest_cont_m_man1 = clean_names(pest_cont_m_man1)
pest_test_m_man2 = clean_names(pest_test_m_man2)
pest_cont_m_man2 = clean_names(pest_cont_m_man2)
pest_test_m_man3 = clean_names(pest_test_m_man3)
pest_cont_m_man3 = clean_names(pest_cont_m_man3)
hrvt_test_m_man = clean_names(hrvt_test_m_man)
hrvt_cont_m_man = clean_names(hrvt_cont_m_man)


data_m_man = data_m_man[, c(1, 53, 3, 6:9)]
names(data_m_man)[1] = "season"
data_m_man$season = ifelse(data_m_man$season < 45352, "Season1", "Season2")
names(data_m_man)[2] = "id"


pest_test_m_man1 = pest_test_m_man1[, c(14, 1:4)]
names(pest_test_m_man1)[1] = "id"
pest_test_m_man1$treatment = rep("test", nrow(pest_test_m_man1))

pest_cont_m_man1 = pest_cont_m_man1[, c(9, 1, 3, 5:6)]
names(pest_cont_m_man1)[1] = "id"
pest_cont_m_man1$treatment = rep("control", nrow(pest_cont_m_man1))

pest_test_m_man2 = pest_test_m_man2[, c(14, 1:4)]
names(pest_test_m_man2)[1] = "id"
pest_test_m_man2$treatment = rep("test", nrow(pest_test_m_man2))

pest_cont_m_man2 = pest_cont_m_man2[, c(9, 1, 3, 5:6)]
names(pest_cont_m_man2)[1] = "id"
pest_cont_m_man2$treatment = rep("control", nrow(pest_cont_m_man2))

pest_test_m_man3 = pest_test_m_man3[, c(16, 1:2, 5:6)]
names(pest_test_m_man3)[1] = "id"
pest_test_m_man3$treatment = rep("test", nrow(pest_test_m_man3))

pest_cont_m_man3 = pest_cont_m_man3[, c(11, 1, 3, 7:8)]
names(pest_cont_m_man3)[1] = "id"
pest_cont_m_man3$treatment = rep("control", nrow(pest_cont_m_man3))

pest_m_man = rbind(pest_test_m_man1, pest_cont_m_man1, pest_test_m_man2, pest_cont_m_man2,
                   pest_test_m_man3, pest_cont_m_man3)

pest_m_man = merge(data_m_man, pest_m_man, by = "id", all.y = TRUE)


hrvt_test_m_man = hrvt_test_m_man[, c(14, 1:11)]
names(hrvt_test_m_man)[1] = "id"
hrvt_test_m_man$treatment = rep("test", nrow(hrvt_test_m_man))


hrvt_cont_m_man = hrvt_cont_m_man[, c(14, 1:11)]
names(hrvt_cont_m_man)[1] = "id"
hrvt_cont_m_man$treatment = rep("control", nrow(hrvt_cont_m_man))
names(hrvt_cont_m_man)[8] = "moisture_content"

hrvt_m_man = rbind(hrvt_test_m_man, hrvt_cont_m_man)

hrvt_m_man = merge(data_m_man, hrvt_m_man, by = "id", all.y = TRUE)


# MAIZE TERRACES------------------------------------------------------------------

# file.choose()

data_m_ter = read.xlsx("Kenya Trial Data\\Kenya Trial Data\\Maize_Terraces_Trial_Season 1&2.xlsx", sheet = 1)
pest_test_m_ter1 = read.xlsx("Kenya Trial Data\\Kenya Trial Data\\Maize_Terraces_Trial_Season 1&2.xlsx", sheet = 2)
pest_cont_m_ter1 = read.xlsx("Kenya Trial Data\\Kenya Trial Data\\Maize_Terraces_Trial_Season 1&2.xlsx", sheet = 3)
pest_test_m_ter2 = read.xlsx("Kenya Trial Data\\Kenya Trial Data\\Maize_Terraces_Trial_Season 1&2.xlsx", sheet = 6)
pest_cont_m_ter2 = read.xlsx("Kenya Trial Data\\Kenya Trial Data\\Maize_Terraces_Trial_Season 1&2.xlsx", sheet = 7)
hrvt_test_m_ter = read.xlsx("Kenya Trial Data\\Kenya Trial Data\\Maize_Terraces_Trial_Season 1&2.xlsx", sheet = 8)
hrvt_cont_m_ter = read.xlsx("Kenya Trial Data\\Kenya Trial Data\\Maize_Terraces_Trial_Season 1&2.xlsx", sheet = 9)
pest_test_m_ter3 = read.xlsx("Kenya Trial Data\\Kenya Trial Data\\Maize_Terraces_Trial_Season 1&2.xlsx", sheet = 10)
pest_cont_m_ter3 = read.xlsx("Kenya Trial Data\\Kenya Trial Data\\Maize_Terraces_Trial_Season 1&2.xlsx", sheet = 11)


data_m_ter = clean_names(data_m_ter)
pest_test_m_ter1 = clean_names(pest_test_m_ter1)
pest_cont_m_ter1 = clean_names(pest_cont_m_ter1)
pest_test_m_ter2 = clean_names(pest_test_m_ter2)
pest_cont_m_ter2 = clean_names(pest_cont_m_ter2)
pest_test_m_ter3 = clean_names(pest_test_m_ter3)
pest_cont_m_ter3 = clean_names(pest_cont_m_ter3)
hrvt_test_m_ter = clean_names(hrvt_test_m_ter)
hrvt_cont_m_ter = clean_names(hrvt_cont_m_ter)


data_m_ter = data_m_ter[, c(1, 50, 3, 6:9)]
names(data_m_ter)[1] = "season"
data_m_ter$season = ifelse(data_m_ter$season < 45352, "Season1", "Season2")
names(data_m_ter)[2] = "id"


pest_test_m_ter1 = pest_test_m_ter1[, c(8, 1:4)]
names(pest_test_m_ter1)[1] = "id"
pest_test_m_ter1$treatment = rep("test", nrow(pest_test_m_ter1))

pest_cont_m_ter1 = pest_cont_m_ter1[, c(9, 1, 3, 5:6)]
names(pest_cont_m_ter1)[1] = "id"
pest_cont_m_ter1$treatment = rep("control", nrow(pest_cont_m_ter1))

pest_test_m_ter2 = pest_test_m_ter2[, c(8, 1:4)]
names(pest_test_m_ter2)[1] = "id"
pest_test_m_ter2$treatment = rep("test", nrow(pest_test_m_ter2))

pest_cont_m_ter2 = pest_cont_m_ter2[, c(9, 1, 3, 5:6)]
names(pest_cont_m_ter2)[1] = "id"
pest_cont_m_ter2$treatment = rep("control", nrow(pest_cont_m_ter2))

pest_test_m_ter3 = pest_test_m_ter3[, c(10, 1:2, 5:6)]
names(pest_test_m_ter3)[1] = "id"
pest_test_m_ter3$treatment = rep("test", nrow(pest_test_m_ter3))

pest_cont_m_ter3 = pest_cont_m_ter3[, c(11, 1, 3, 7:8)]
names(pest_cont_m_ter3)[1] = "id"
pest_cont_m_ter3$treatment = rep("control", nrow(pest_cont_m_ter3))

pest_m_ter = rbind(pest_test_m_ter1, pest_cont_m_ter1, pest_test_m_ter2, pest_cont_m_ter2,
                   pest_test_m_ter3, pest_cont_m_ter3)

pest_m_ter = merge(data_m_ter, pest_m_ter, by = "id", all.y = TRUE)


hrvt_test_m_ter = hrvt_test_m_ter[, c(14, 1:11)]
names(hrvt_test_m_ter)[1] = "id"
hrvt_test_m_ter$treatment = rep("test", nrow(hrvt_test_m_ter))

hrvt_cont_m_ter = hrvt_cont_m_ter[, c(14, 1:11)]
names(hrvt_cont_m_ter)[1] = "id"
hrvt_cont_m_ter$treatment = rep("control", nrow(hrvt_cont_m_ter))
names(hrvt_cont_m_ter)[8] = "moisture_content"

hrvt_m_ter = rbind(hrvt_test_m_ter, hrvt_cont_m_ter)

hrvt_m_ter = merge(data_m_ter, hrvt_m_ter, by = "id", all.y = TRUE)


# CABBAGE IPM-------------------------------------------------------------------

# file.choose()

data_c_ipm1 = read.xlsx("Kenya Trial Data\\Kenya Trial Data\\Cabbage_IPM_Trial_Season 1.xlsx", sheet = 1)
pest_test_c_ipm11 = read.xlsx("Kenya Trial Data\\Kenya Trial Data\\Cabbage_IPM_Trial_Season 1.xlsx", sheet = 2)
pest_cont_c_ipm11 = read.xlsx("Kenya Trial Data\\Kenya Trial Data\\Cabbage_IPM_Trial_Season 1.xlsx", sheet = 3)
pest_test_c_ipm12 = read.xlsx("Kenya Trial Data\\Kenya Trial Data\\Cabbage_IPM_Trial_Season 1.xlsx", sheet = 4)
pest_cont_c_ipm12 = read.xlsx("Kenya Trial Data\\Kenya Trial Data\\Cabbage_IPM_Trial_Season 1.xlsx", sheet = 5)
pest_test_c_ipm13 = read.xlsx("Kenya Trial Data\\Kenya Trial Data\\Cabbage_IPM_Trial_Season 1.xlsx", sheet = 6)
pest_cont_c_ipm13 = read.xlsx("Kenya Trial Data\\Kenya Trial Data\\Cabbage_IPM_Trial_Season 1.xlsx", sheet = 7)
pest_test_c_ipm14 = read.xlsx("Kenya Trial Data\\Kenya Trial Data\\Cabbage_IPM_Trial_Season 1.xlsx", sheet = 8)
pest_cont_c_ipm14 = read.xlsx("Kenya Trial Data\\Kenya Trial Data\\Cabbage_IPM_Trial_Season 1.xlsx", sheet = 9)
hrvt_test_c_ipm1 = read.xlsx("Kenya Trial Data\\Kenya Trial Data\\Cabbage_IPM_Trial_Season 1.xlsx", sheet = 10)
hrvt_cont_c_ipm1 = read.xlsx("Kenya Trial Data\\Kenya Trial Data\\Cabbage_IPM_Trial_Season 1.xlsx", sheet = 11)

data_c_ipm2 = read.xlsx("Kenya Trial Data\\Kenya Trial Data\\Cabbage_IPM_Trial_Season_2.xlsx", sheet = 1)
pest_test_c_ipm21 = read.xlsx("Kenya Trial Data\\Kenya Trial Data\\Cabbage_IPM_Trial_Season_2.xlsx", sheet = 2)
pest_cont_c_ipm21 = read.xlsx("Kenya Trial Data\\Kenya Trial Data\\Cabbage_IPM_Trial_Season_2.xlsx", sheet = 3)
pest_test_c_ipm22 = read.xlsx("Kenya Trial Data\\Kenya Trial Data\\Cabbage_IPM_Trial_Season_2.xlsx", sheet = 4)
pest_cont_c_ipm22 = read.xlsx("Kenya Trial Data\\Kenya Trial Data\\Cabbage_IPM_Trial_Season_2.xlsx", sheet = 5)
pest_test_c_ipm23 = read.xlsx("Kenya Trial Data\\Kenya Trial Data\\Cabbage_IPM_Trial_Season_2.xlsx", sheet = 6)
pest_cont_c_ipm23 = read.xlsx("Kenya Trial Data\\Kenya Trial Data\\Cabbage_IPM_Trial_Season_2.xlsx", sheet = 7)
pest_test_c_ipm24 = read.xlsx("Kenya Trial Data\\Kenya Trial Data\\Cabbage_IPM_Trial_Season_2.xlsx", sheet = 8)
pest_cont_c_ipm24 = read.xlsx("Kenya Trial Data\\Kenya Trial Data\\Cabbage_IPM_Trial_Season_2.xlsx", sheet = 9)
hrvt_test_c_ipm2 = read.xlsx("Kenya Trial Data\\Kenya Trial Data\\Cabbage_IPM_Trial_Season_2.xlsx", sheet = 10)
hrvt_cont_c_ipm2 = read.xlsx("Kenya Trial Data\\Kenya Trial Data\\Cabbage_IPM_Trial_Season_2.xlsx", sheet = 11)


data_c_ipm1 = clean_names(data_c_ipm1)
pest_test_c_ipm11 = clean_names(pest_test_c_ipm11)
pest_cont_c_ipm11 = clean_names(pest_cont_c_ipm11)
pest_test_c_ipm12 = clean_names(pest_test_c_ipm12)
pest_cont_c_ipm12 = clean_names(pest_cont_c_ipm12)
pest_test_c_ipm13 = clean_names(pest_test_c_ipm13)
pest_cont_c_ipm13 = clean_names(pest_cont_c_ipm13)
pest_test_c_ipm14 = clean_names(pest_test_c_ipm14)
pest_cont_c_ipm14 = clean_names(pest_cont_c_ipm14)
hrvt_test_c_ipm1 = clean_names(hrvt_test_c_ipm1)
hrvt_cont_c_ipm1 = clean_names(hrvt_cont_c_ipm1)


data_c_ipm2 = clean_names(data_c_ipm2)
pest_test_c_ipm21 = clean_names(pest_test_c_ipm21)
pest_cont_c_ipm21 = clean_names(pest_cont_c_ipm21)
pest_test_c_ipm22 = clean_names(pest_test_c_ipm22)
pest_cont_c_ipm22 = clean_names(pest_cont_c_ipm22)
pest_test_c_ipm23 = clean_names(pest_test_c_ipm23)
pest_cont_c_ipm23 = clean_names(pest_cont_c_ipm23)
pest_test_c_ipm24 = clean_names(pest_test_c_ipm24)
pest_cont_c_ipm24 = clean_names(pest_cont_c_ipm24)
hrvt_test_c_ipm2 = clean_names(hrvt_test_c_ipm2)
hrvt_cont_c_ipm2 = clean_names(hrvt_cont_c_ipm2)


data_c_ipm1 = data_c_ipm1[, c(61, 3, 6:9)]
names(data_c_ipm1)[1] = "id"


pest_test_c_ipm11 = pest_test_c_ipm11[, c(13, 1:2, 4:9)]
names(pest_test_c_ipm11)[1] = "id"
pest_test_c_ipm11$treatment = rep("test", nrow(pest_test_c_ipm11))

pest_cont_c_ipm11 = pest_cont_c_ipm11[, c(13, 1:8)]
names(pest_cont_c_ipm11)[1] = "id"
pest_cont_c_ipm11$treatment = rep("control", nrow(pest_cont_c_ipm11))
names(pest_cont_c_ipm11)[4] = "leaf_damage"

pest_test_c_ipm12 = pest_test_c_ipm12[, c(13, 1:2, 4:9)]
names(pest_test_c_ipm12)[1] = "id"
pest_test_c_ipm12$treatment = rep("test", nrow(pest_test_c_ipm12))

pest_cont_c_ipm12 = pest_cont_c_ipm12[, c(18, 1:8)]
names(pest_cont_c_ipm12)[1] = "id"
pest_cont_c_ipm12$treatment = rep("control", nrow(pest_cont_c_ipm12))
names(pest_cont_c_ipm12)[4] = "leaf_damage"

pest_test_c_ipm13 = pest_test_c_ipm13[, c(13, 1:2, 4:9)]
names(pest_test_c_ipm13)[1] = "id"
pest_test_c_ipm13$treatment = rep("test", nrow(pest_test_c_ipm13))

pest_cont_c_ipm13 = pest_cont_c_ipm13[, c(18, 1:8)]
names(pest_cont_c_ipm13)[1] = "id"
pest_cont_c_ipm13$treatment = rep("control", nrow(pest_cont_c_ipm13))
names(pest_cont_c_ipm13)[4] = "leaf_damage"

pest_test_c_ipm14 = pest_test_c_ipm14[, c(13, 1:2, 4:9)]
names(pest_test_c_ipm14)[1] = "id"
pest_test_c_ipm14$treatment = rep("test", nrow(pest_test_c_ipm14))

pest_cont_c_ipm14 = pest_cont_c_ipm14[, c(18, 1:8)]
names(pest_cont_c_ipm14)[1] = "id"
pest_cont_c_ipm14$treatment = rep("control", nrow(pest_cont_c_ipm14))
names(pest_cont_c_ipm14)[4] = "leaf_damage"


pest_c_ipm1 = rbind(pest_test_c_ipm11, pest_cont_c_ipm11, pest_test_c_ipm12, pest_cont_c_ipm12,
                   pest_test_c_ipm13, pest_cont_c_ipm13, pest_test_c_ipm14, pest_cont_c_ipm14)

pest_c_ipm1 = merge(data_c_ipm1, pest_c_ipm1, by = "id", all.y = TRUE)


hrvt_test_c_ipm1 = hrvt_test_c_ipm1[, c(22, 1:3)]
names(hrvt_test_c_ipm1)[1] = "id"
hrvt_test_c_ipm1$treatment = rep("test", nrow(hrvt_test_c_ipm1))

hrvt_cont_c_ipm1 = hrvt_cont_c_ipm1[, c(24, 1:3)]
names(hrvt_cont_c_ipm1)[1] = "id"
hrvt_cont_c_ipm1$treatment = rep("control", nrow(hrvt_cont_c_ipm1))

hrvt_c_ipm1 = rbind(hrvt_test_c_ipm1, hrvt_cont_c_ipm1)

hrvt_c_ipm1 = merge(data_c_ipm1, hrvt_c_ipm1, by = "id", all.y = TRUE)


data_c_ipm2 = data_c_ipm2[, c(34, 3, 6:9)]
names(data_c_ipm2)[1] = "id"


pest_test_c_ipm21 = pest_test_c_ipm21[, c(12, 1:2, 4:9)]
names(pest_test_c_ipm21)[1] = "id"
pest_test_c_ipm21$treatment = rep("test", nrow(pest_test_c_ipm21))

pest_cont_c_ipm21 = pest_cont_c_ipm21[, c(11, 1:8)]
names(pest_cont_c_ipm21)[1] = "id"
pest_cont_c_ipm21$treatment = rep("control", nrow(pest_cont_c_ipm21))
names(pest_cont_c_ipm21)[4] = "leaf_damage"

pest_test_c_ipm22 = pest_test_c_ipm22[, c(12, 1:2, 4:9)]
names(pest_test_c_ipm22)[1] = "id"
pest_test_c_ipm22$treatment = rep("test", nrow(pest_test_c_ipm22))

pest_cont_c_ipm22 = pest_cont_c_ipm22[, c(11, 1:8)]
names(pest_cont_c_ipm22)[1] = "id"
pest_cont_c_ipm22$treatment = rep("control", nrow(pest_cont_c_ipm22))
names(pest_cont_c_ipm22)[4] = "leaf_damage"

pest_test_c_ipm23 = pest_test_c_ipm23[, c(12, 1:2, 4:9)]
names(pest_test_c_ipm23)[1] = "id"
pest_test_c_ipm23$treatment = rep("test", nrow(pest_test_c_ipm23))

pest_cont_c_ipm23 = pest_cont_c_ipm23[, c(11, 1:8)]
names(pest_cont_c_ipm23)[1] = "id"
pest_cont_c_ipm23$treatment = rep("control", nrow(pest_cont_c_ipm23))
names(pest_cont_c_ipm23)[4] = "leaf_damage"

pest_test_c_ipm24 = pest_test_c_ipm24[, c(12, 1:2, 4:9)]
names(pest_test_c_ipm24)[1] = "id"
pest_test_c_ipm24$treatment = rep("test", nrow(pest_test_c_ipm24))

pest_cont_c_ipm24 = pest_cont_c_ipm24[, c(11, 1:8)]
names(pest_cont_c_ipm24)[1] = "id"
pest_cont_c_ipm24$treatment = rep("control", nrow(pest_cont_c_ipm24))
names(pest_cont_c_ipm24)[4] = "leaf_damage"


pest_c_ipm2 = rbind(pest_test_c_ipm21, pest_cont_c_ipm21, pest_test_c_ipm22, pest_cont_c_ipm22,
                    pest_test_c_ipm23, pest_cont_c_ipm23, pest_test_c_ipm24, pest_cont_c_ipm24)

pest_c_ipm2 = merge(data_c_ipm2, pest_c_ipm2, by = "id", all.y = TRUE)


hrvt_test_c_ipm2 = hrvt_test_c_ipm2[, c(6, 1:3)]
names(hrvt_test_c_ipm2)[1] = "id"
hrvt_test_c_ipm2$treatment = rep("test", nrow(hrvt_test_c_ipm2))

hrvt_cont_c_ipm2 = hrvt_cont_c_ipm2[, c(6, 1:3)]
names(hrvt_cont_c_ipm2)[1] = "id"
hrvt_cont_c_ipm2$treatment = rep("control", nrow(hrvt_cont_c_ipm2))

hrvt_c_ipm2 = rbind(hrvt_test_c_ipm2, hrvt_cont_c_ipm2)

hrvt_c_ipm2 = merge(data_c_ipm2, hrvt_c_ipm2, by = "id", all.y = TRUE)

pest_c_ipm1$season = rep("Season1", nrow(pest_c_ipm1))
pest_c_ipm1 = pest_c_ipm1[, c(16, 1:15)]

pest_c_ipm2$season = rep("Season2", nrow(pest_c_ipm2))
pest_c_ipm2 = pest_c_ipm2[, c(16, 1:15)]

hrvt_c_ipm1$season = rep("Season1", nrow(hrvt_c_ipm1))
hrvt_c_ipm1 = hrvt_c_ipm1[, c(11, 1:10)]

hrvt_c_ipm2$season = rep("Season2", nrow(hrvt_c_ipm2))
hrvt_c_ipm2 = hrvt_c_ipm2[, c(11, 1:10)]

pest_c_ipm = rbind(pest_c_ipm1, pest_c_ipm2)

hrvt_c_ipm = rbind(hrvt_c_ipm1, hrvt_c_ipm2)


# SPINACH COMPOST---------------------------------------------------------------

# file.choose()

data_s_com1 = read.xlsx("Kenya Trial Data\\Kenya Trial Data\\Compost_Trial_Spinach_Season_1.xlsx", sheet = 1)

data_s_com2 = read.xlsx("Kenya Trial Data\\Kenya Trial Data\\Spinach_Compost_Trial_Season_2.xlsx", sheet = 1)
pest_test_s_com21 = read.xlsx("Kenya Trial Data\\Kenya Trial Data\\Spinach_Compost_Trial_Season_2.xlsx", sheet = 2)
pest_cont_s_com21 = read.xlsx("Kenya Trial Data\\Kenya Trial Data\\Spinach_Compost_Trial_Season_2.xlsx", sheet = 3)
pest_test_s_com22 = read.xlsx("Kenya Trial Data\\Kenya Trial Data\\Spinach_Compost_Trial_Season_2.xlsx", sheet = 4)
pest_cont_s_com22 = read.xlsx("Kenya Trial Data\\Kenya Trial Data\\Spinach_Compost_Trial_Season_2.xlsx", sheet = 5)
pest_test_s_com23 = read.xlsx("Kenya Trial Data\\Kenya Trial Data\\Spinach_Compost_Trial_Season_2.xlsx", sheet = 6)
pest_cont_s_com23 = read.xlsx("Kenya Trial Data\\Kenya Trial Data\\Spinach_Compost_Trial_Season_2.xlsx", sheet = 7)
pest_test_s_com24 = read.xlsx("Kenya Trial Data\\Kenya Trial Data\\Spinach_Compost_Trial_Season_2.xlsx", sheet = 8)
pest_cont_s_com24 = read.xlsx("Kenya Trial Data\\Kenya Trial Data\\Spinach_Compost_Trial_Season_2.xlsx", sheet = 9)
pest_test_s_com25 = read.xlsx("Kenya Trial Data\\Kenya Trial Data\\Spinach_Compost_Trial_Season_2.xlsx", sheet = 10)
pest_cont_s_com25 = read.xlsx("Kenya Trial Data\\Kenya Trial Data\\Spinach_Compost_Trial_Season_2.xlsx", sheet = 11)
pest_test_s_com26 = read.xlsx("Kenya Trial Data\\Kenya Trial Data\\Spinach_Compost_Trial_Season_2.xlsx", sheet = 12)
pest_cont_s_com26 = read.xlsx("Kenya Trial Data\\Kenya Trial Data\\Spinach_Compost_Trial_Season_2.xlsx", sheet = 13)


data_s_com1 = clean_names(data_s_com1)


data_s_com2 = clean_names(data_s_com2)
pest_test_s_com21 = clean_names(pest_test_s_com21)
pest_cont_s_com21 = clean_names(pest_cont_s_com21)
pest_test_s_com22 = clean_names(pest_test_s_com22)
pest_cont_s_com22 = clean_names(pest_cont_s_com22)
pest_test_s_com23 = clean_names(pest_test_s_com23)
pest_cont_s_com23 = clean_names(pest_cont_s_com23)
pest_test_s_com24 = clean_names(pest_test_s_com24)
pest_cont_s_com24 = clean_names(pest_cont_s_com24)
pest_test_s_com25 = clean_names(pest_test_s_com25)
pest_cont_s_com25 = clean_names(pest_cont_s_com25)
pest_test_s_com26 = clean_names(pest_test_s_com26)
pest_cont_s_com26 = clean_names(pest_cont_s_com26)


test_s_com11 = data_s_com1[, c(169, 14:15, 18:21)]
names(test_s_com11)[1] = "id"

cont_s_com11 = data_s_com1[, c(169, 14:15, 25:28)]
names(cont_s_com11) = names(test_s_com11)

test_s_com12 = data_s_com1[, c(169, 14:15, 32:35)]
names(test_s_com12) = names(test_s_com11)

cont_s_com12 = data_s_com1[, c(169, 14:15, 39:42)]
names(cont_s_com12) = names(test_s_com11)

test_s_com13 = data_s_com1[, c(169, 14:15, 46:49)]
names(test_s_com13) = names(test_s_com11)

cont_s_com13 = data_s_com1[, c(169, 14:15, 53:56)]
names(cont_s_com13) = names(test_s_com11)

test_s_com14 = data_s_com1[, c(169, 14:15, 60:63)]
names(test_s_com14) = names(test_s_com11)

cont_s_com14 = data_s_com1[, c(169, 14:15, 67:70)]
names(cont_s_com14) = names(test_s_com11)

test_s_com15 = data_s_com1[, c(169, 14:15, 74:77)]
names(test_s_com15) = names(test_s_com11)

cont_s_com15 = data_s_com1[, c(169, 14:15, 81:84)]
names(cont_s_com15) = names(test_s_com11)

test_s_com16 = data_s_com1[, c(169, 14:15, 88:90)]
test_s_com16$is_the_plant_attacked_by_any_pest = rep(NA, nrow(test_s_com16))
names(test_s_com16) = names(test_s_com11)

cont_s_com16 = data_s_com1[, c(169, 14:15, 94:96)]
cont_s_com16$is_the_plant_attacked_by_any_pest = rep(NA, nrow(cont_s_com16))
names(cont_s_com16) = names(test_s_com11)


test_s_com1 = rbind(test_s_com11, test_s_com12, test_s_com13, test_s_com14,
                    test_s_com15, test_s_com16)

cont_s_com1 = rbind(cont_s_com11, cont_s_com12, cont_s_com13, cont_s_com14,
                    cont_s_com15, cont_s_com16)


test_s_com1$treatment = rep("test", nrow(test_s_com1))

cont_s_com1$treatment = rep("control", nrow(cont_s_com1))

s_com1 = rbind(test_s_com1, cont_s_com1)

s_com1 = s_com1[!is.na(s_com1$number_of_leaves_per_plot),]

s_com1$season = rep("Season1", nrow(s_com1))


hrvt_test_s_com21 = data_s_com2[, c(81, 8:9, 12)]
names(hrvt_test_s_com21) = c("id", "farmer_i_d_and_name", "the_measurement_time", "vegetable_yield_kg_per_plot")
hrvt_test_s_com21 = na.omit(hrvt_test_s_com21)

hrvt_cont_s_com21 = data_s_com2[, c(81, 8:9, 15)]
names(hrvt_cont_s_com21) = c("id", "farmer_i_d_and_name", "the_measurement_time", "vegetable_yield_kg_per_plot")
hrvt_cont_s_com21 = na.omit(hrvt_cont_s_com21)

hrvt_test_s_com22 = data_s_com2[, c(81, 8:9, 18)]
names(hrvt_test_s_com22) = c("id", "farmer_i_d_and_name", "the_measurement_time", "vegetable_yield_kg_per_plot")
hrvt_test_s_com22 = na.omit(hrvt_test_s_com22)

hrvt_cont_s_com22 = data_s_com2[, c(81, 8:9, 21)]
names(hrvt_cont_s_com22) = c("id", "farmer_i_d_and_name", "the_measurement_time", "vegetable_yield_kg_per_plot")
hrvt_cont_s_com22 = na.omit(hrvt_cont_s_com22)

hrvt_test_s_com23 = data_s_com2[, c(81, 8:9, 24)]
names(hrvt_test_s_com23) = c("id", "farmer_i_d_and_name", "the_measurement_time", "vegetable_yield_kg_per_plot")
hrvt_test_s_com23 = na.omit(hrvt_test_s_com23)

hrvt_cont_s_com23 = data_s_com2[, c(81, 8:9, 27)]
names(hrvt_cont_s_com23) = c("id", "farmer_i_d_and_name", "the_measurement_time", "vegetable_yield_kg_per_plot")
hrvt_cont_s_com23 = na.omit(hrvt_cont_s_com23)

hrvt_test_s_com24 = data_s_com2[, c(81, 8:9, 30)]
names(hrvt_test_s_com24) = c("id", "farmer_i_d_and_name", "the_measurement_time", "vegetable_yield_kg_per_plot")
hrvt_test_s_com24 = na.omit(hrvt_test_s_com24)

hrvt_cont_s_com24 = data_s_com2[, c(81, 8:9, 33)]
names(hrvt_cont_s_com24) = c("id", "farmer_i_d_and_name", "the_measurement_time", "vegetable_yield_kg_per_plot")
hrvt_cont_s_com24 = na.omit(hrvt_cont_s_com24)

hrvt_test_s_com25 = data_s_com2[, c(81, 8:9, 36)]
names(hrvt_test_s_com25) = c("id", "farmer_i_d_and_name", "the_measurement_time", "vegetable_yield_kg_per_plot")
hrvt_test_s_com25 = na.omit(hrvt_test_s_com25)

hrvt_cont_s_com25 = data_s_com2[, c(81, 8:9, 39)]
names(hrvt_cont_s_com25) = c("id", "farmer_i_d_and_name", "the_measurement_time", "vegetable_yield_kg_per_plot")
hrvt_cont_s_com25 = na.omit(hrvt_cont_s_com25)

hrvt_test_s_com26 = data_s_com2[, c(81, 8:9, 42)]
names(hrvt_test_s_com26) = c("id", "farmer_i_d_and_name", "the_measurement_time", "vegetable_yield_kg_per_plot")
hrvt_test_s_com26 = na.omit(hrvt_test_s_com26)

hrvt_cont_s_com26 = data_s_com2[, c(81, 8:9, 45)]
names(hrvt_cont_s_com26) = c("id", "farmer_i_d_and_name", "the_measurement_time", "vegetable_yield_kg_per_plot")
hrvt_cont_s_com26 = na.omit(hrvt_cont_s_com26)


pest_test_s_com21 = pest_test_s_com21[, c(11, 1:3)]
names(pest_test_s_com21) = c("id", "number_of_leaves_per_plot", "leaf_color", "is_the_plant_attacked_by_any_pest")
pest_test_s_com21$the_measurement_time = rep(1, nrow(pest_test_s_com21))

pest_cont_s_com21 = pest_cont_s_com21[, c(11, 1:3)]
names(pest_cont_s_com21) = c("id", "number_of_leaves_per_plot", "leaf_color", "is_the_plant_attacked_by_any_pest")
pest_cont_s_com21$the_measurement_time = rep(1, nrow(pest_cont_s_com21))

pest_test_s_com22 = pest_test_s_com22[, c(12, 1:3)]
names(pest_test_s_com22) = c("id", "number_of_leaves_per_plot", "leaf_color", "is_the_plant_attacked_by_any_pest")
pest_test_s_com22$the_measurement_time = rep(2, nrow(pest_test_s_com22))

pest_cont_s_com22 = pest_cont_s_com22[, c(12, 1:3)]
names(pest_cont_s_com22) = c("id", "number_of_leaves_per_plot", "leaf_color", "is_the_plant_attacked_by_any_pest")
pest_cont_s_com22$the_measurement_time = rep(2, nrow(pest_cont_s_com22))

pest_test_s_com23 = pest_test_s_com23[, c(11, 1:3)]
names(pest_test_s_com23) = c("id", "number_of_leaves_per_plot", "leaf_color", "is_the_plant_attacked_by_any_pest")
pest_test_s_com23$the_measurement_time = rep(3, nrow(pest_test_s_com23))

pest_cont_s_com23 = pest_cont_s_com23[, c(11, 1:3)]
names(pest_cont_s_com23) = c("id", "number_of_leaves_per_plot", "leaf_color", "is_the_plant_attacked_by_any_pest")
pest_cont_s_com23$the_measurement_time = rep(3, nrow(pest_cont_s_com23))

pest_test_s_com24 = pest_test_s_com24[, c(11, 1:3)]
names(pest_test_s_com24) = c("id", "number_of_leaves_per_plot", "leaf_color", "is_the_plant_attacked_by_any_pest")
pest_test_s_com24$the_measurement_time = rep(4, nrow(pest_test_s_com24))

pest_cont_s_com24 = pest_cont_s_com24[, c(11, 1:3)]
names(pest_cont_s_com24) = c("id", "number_of_leaves_per_plot", "leaf_color", "is_the_plant_attacked_by_any_pest")
pest_cont_s_com24$the_measurement_time = rep(4, nrow(pest_cont_s_com24))

pest_test_s_com25 = pest_test_s_com25[, c(11, 1:3)]
names(pest_test_s_com25) = c("id", "number_of_leaves_per_plot", "leaf_color", "is_the_plant_attacked_by_any_pest")
pest_test_s_com25$the_measurement_time = rep(5, nrow(pest_test_s_com25))

pest_cont_s_com25 = pest_cont_s_com25[, c(11, 1:3)]
names(pest_cont_s_com25) = c("id", "number_of_leaves_per_plot", "leaf_color", "is_the_plant_attacked_by_any_pest")
pest_cont_s_com25$the_measurement_time = rep(5, nrow(pest_cont_s_com25))

pest_test_s_com26 = pest_test_s_com26[, c(7, 1:2, 4)]
names(pest_test_s_com26) = c("id", "number_of_leaves_per_plot", "leaf_color", "is_the_plant_attacked_by_any_pest")
pest_test_s_com26$is_the_plant_attacked_by_any_pest = ifelse(pest_test_s_com26$is_the_plant_attacked_by_any_pest > 0, 1, 0)
pest_test_s_com26$the_measurement_time = rep(6, nrow(pest_test_s_com26))

pest_cont_s_com26 = pest_cont_s_com26[, c(7, 1:2, 4)]
names(pest_cont_s_com26) = c("id", "number_of_leaves_per_plot", "leaf_color", "is_the_plant_attacked_by_any_pest")
pest_cont_s_com26$is_the_plant_attacked_by_any_pest = ifelse(pest_cont_s_com26$is_the_plant_attacked_by_any_pest > 0, 1, 0)
pest_cont_s_com26$the_measurement_time = rep(6, nrow(pest_cont_s_com26))


hrvt_test_s_com2 = rbind(hrvt_test_s_com21, hrvt_test_s_com22, hrvt_test_s_com23,
                         hrvt_test_s_com24, hrvt_test_s_com25, hrvt_test_s_com26)

hrvt_cont_s_com2 = rbind(hrvt_cont_s_com21, hrvt_cont_s_com22, hrvt_cont_s_com23,
                         hrvt_cont_s_com24, hrvt_cont_s_com25, hrvt_cont_s_com26)

hrvt_test_s_com2$treatment = rep("test", nrow(hrvt_test_s_com2))

hrvt_cont_s_com2$treatment = rep("control", nrow(hrvt_cont_s_com2))

hrvt_s_com2 = rbind(hrvt_test_s_com2, hrvt_cont_s_com2)


pest_test_s_com2 = rbind(pest_test_s_com21, pest_test_s_com22, pest_test_s_com23,
                         pest_test_s_com24, pest_test_s_com25, pest_test_s_com26)

pest_cont_s_com2 = rbind(pest_cont_s_com21, pest_cont_s_com22, pest_cont_s_com23,
                         pest_cont_s_com24, pest_cont_s_com25, pest_cont_s_com26)

pest_test_s_com2$treatment = rep("test", nrow(pest_test_s_com2))

pest_cont_s_com2$treatment = rep("control", nrow(pest_cont_s_com2))

pest_s_com2 = rbind(pest_test_s_com2, pest_cont_s_com2)

pest_s_com2$is_the_plant_attacked_by_any_pest = ifelse(pest_s_com2$is_the_plant_attacked_by_any_pest == "Yes", 1, pest_s_com2$is_the_plant_attacked_by_any_pest)
pest_s_com2$is_the_plant_attacked_by_any_pest = ifelse(pest_s_com2$is_the_plant_attacked_by_any_pest == "No", 0, pest_s_com2$is_the_plant_attacked_by_any_pest)

pest_s_com2$is_the_plant_attacked_by_any_pest = as.numeric(pest_s_com2$is_the_plant_attacked_by_any_pest)

pest_s_com2 = aggregate(. ~ id + treatment + the_measurement_time, FUN = mean,
                        na.rm = TRUE, data = pest_s_com2)

pest_s_com2$is_the_plant_attacked_by_any_pest = ifelse(pest_s_com2$is_the_plant_attacked_by_any_pest > 0, 1, 0)

s_com2 = merge(hrvt_s_com2, pest_s_com2, by = c("id", "treatment", "the_measurement_time"), all = TRUE)

s_com2$season = rep("Season2", nrow(s_com2))

s_com2 = s_com2[, c(1, 4, 3, 6:7, 5, 8, 2, 9)]


s_com = rbind(s_com1, s_com2)


# SPINACH MULCHING--------------------------------------------------------------

# file.choose()

data_s_mul1 = read.xlsx("Kenya Trial Data\\Kenya Trial Data\\Spinach_Mulching_Trial_Season1.xlsx", sheet = 1)

data_s_mul2 = read.xlsx("Kenya Trial Data\\Kenya Trial Data\\Spinach_Mulching_Trial_Season_2.xlsx", sheet = 1)
pest_test_s_mul21 = read.xlsx("Kenya Trial Data\\Kenya Trial Data\\Spinach_Mulching_Trial_Season_2.xlsx", sheet = 2)
pest_cont_s_mul21 = read.xlsx("Kenya Trial Data\\Kenya Trial Data\\Spinach_Mulching_Trial_Season_2.xlsx", sheet = 3)
pest_test_s_mul22 = read.xlsx("Kenya Trial Data\\Kenya Trial Data\\Spinach_Mulching_Trial_Season_2.xlsx", sheet = 4)
pest_cont_s_mul22 = read.xlsx("Kenya Trial Data\\Kenya Trial Data\\Spinach_Mulching_Trial_Season_2.xlsx", sheet = 5)
pest_test_s_mul23 = read.xlsx("Kenya Trial Data\\Kenya Trial Data\\Spinach_Mulching_Trial_Season_2.xlsx", sheet = 6)
pest_cont_s_mul23 = read.xlsx("Kenya Trial Data\\Kenya Trial Data\\Spinach_Mulching_Trial_Season_2.xlsx", sheet = 7)
pest_test_s_mul24 = read.xlsx("Kenya Trial Data\\Kenya Trial Data\\Spinach_Mulching_Trial_Season_2.xlsx", sheet = 8)
pest_cont_s_mul24 = read.xlsx("Kenya Trial Data\\Kenya Trial Data\\Spinach_Mulching_Trial_Season_2.xlsx", sheet = 9)
pest_test_s_mul25 = read.xlsx("Kenya Trial Data\\Kenya Trial Data\\Spinach_Mulching_Trial_Season_2.xlsx", sheet = 10)
pest_cont_s_mul25 = read.xlsx("Kenya Trial Data\\Kenya Trial Data\\Spinach_Mulching_Trial_Season_2.xlsx", sheet = 11)
pest_test_s_mul26 = read.xlsx("Kenya Trial Data\\Kenya Trial Data\\Spinach_Mulching_Trial_Season_2.xlsx", sheet = 12)
pest_cont_s_mul26 = read.xlsx("Kenya Trial Data\\Kenya Trial Data\\Spinach_Mulching_Trial_Season_2.xlsx", sheet = 13)


data_s_mul1 = clean_names(data_s_mul1)


data_s_mul2 = clean_names(data_s_mul2)
pest_test_s_mul21 = clean_names(pest_test_s_mul21)
pest_cont_s_mul21 = clean_names(pest_cont_s_mul21)
pest_test_s_mul22 = clean_names(pest_test_s_mul22)
pest_cont_s_mul22 = clean_names(pest_cont_s_mul22)
pest_test_s_mul23 = clean_names(pest_test_s_mul23)
pest_cont_s_mul23 = clean_names(pest_cont_s_mul23)
pest_test_s_mul24 = clean_names(pest_test_s_mul24)
pest_cont_s_mul24 = clean_names(pest_cont_s_mul24)
pest_test_s_mul25 = clean_names(pest_test_s_mul25)
pest_cont_s_mul25 = clean_names(pest_cont_s_mul25)
pest_test_s_mul26 = clean_names(pest_test_s_mul26)
pest_cont_s_mul26 = clean_names(pest_cont_s_mul26)


test_s_mul11 = data_s_mul1[, c(191, 14:15, 18:21)]
names(test_s_mul11)[1] = "id"

cont_s_mul11 = data_s_mul1[, c(191, 14:15, 25:28)]
names(cont_s_mul11) = names(test_s_mul11)

test_s_mul12 = data_s_mul1[, c(191, 14:15, 32:35)]
names(test_s_mul12) = names(test_s_mul11)

cont_s_mul12 = data_s_mul1[, c(191, 14:15, 39:42)]
names(cont_s_mul12) = names(test_s_mul11)

test_s_mul13 = data_s_mul1[, c(191, 14:15, 46:49)]
names(test_s_mul13) = names(test_s_mul11)

cont_s_mul13 = data_s_mul1[, c(191, 14:15, 53:56)]
names(cont_s_mul13) = names(test_s_mul11)

test_s_mul14 = data_s_mul1[, c(191, 14:15, 60:63)]
names(test_s_mul14) = names(test_s_mul11)

cont_s_mul14 = data_s_mul1[, c(191, 14:15, 67:70)]
names(cont_s_mul14) = names(test_s_mul11)

test_s_mul15 = data_s_mul1[, c(191, 14:15, 74:76)]
test_s_mul15$is_the_plant_attacked_by_any_pest = rep(NA, nrow(test_s_mul15))
names(test_s_mul15) = names(test_s_mul11)

cont_s_mul15 = data_s_mul1[, c(191, 14:15, 79:81)]
cont_s_mul15$is_the_plant_attacked_by_any_pest = rep(NA, nrow(cont_s_mul15))
names(cont_s_mul15) = names(test_s_mul11)


test_s_mul1 = rbind(test_s_mul11, test_s_mul12, test_s_mul13, test_s_mul14,
                    test_s_mul15)

cont_s_mul1 = rbind(cont_s_mul11, cont_s_mul12, cont_s_mul13, cont_s_mul14,
                    cont_s_mul15)


test_s_mul1$treatment = rep("test", nrow(test_s_mul1))

cont_s_mul1$treatment = rep("control", nrow(cont_s_mul1))

s_mul1 = rbind(test_s_mul1, cont_s_mul1)

s_mul1 = s_mul1[!is.na(s_mul1$number_of_leaves_per_plot),]

s_mul1$season = rep("Season1", nrow(s_mul1))


hrvt_test_s_mul21 = data_s_mul2[, c(186, 8:9, 12)]
names(hrvt_test_s_mul21) = c("id", "farmer_i_d_and_name", "the_measurement_time", "vegetable_yield_kg_per_plot")
hrvt_test_s_mul21 = na.omit(hrvt_test_s_mul21)

hrvt_cont_s_mul21 = data_s_mul2[, c(186, 8:9, 15)]
names(hrvt_cont_s_mul21) = c("id", "farmer_i_d_and_name", "the_measurement_time", "vegetable_yield_kg_per_plot")
hrvt_cont_s_mul21 = na.omit(hrvt_cont_s_mul21)

hrvt_test_s_mul22 = data_s_mul2[, c(186, 8:9, 18)]
names(hrvt_test_s_mul22) = c("id", "farmer_i_d_and_name", "the_measurement_time", "vegetable_yield_kg_per_plot")
hrvt_test_s_mul22 = na.omit(hrvt_test_s_mul22)

hrvt_cont_s_mul22 = data_s_mul2[, c(186, 8:9, 21)]
names(hrvt_cont_s_mul22) = c("id", "farmer_i_d_and_name", "the_measurement_time", "vegetable_yield_kg_per_plot")
hrvt_cont_s_mul22 = na.omit(hrvt_cont_s_mul22)

hrvt_test_s_mul23 = data_s_mul2[, c(186, 8:9, 24)]
names(hrvt_test_s_mul23) = c("id", "farmer_i_d_and_name", "the_measurement_time", "vegetable_yield_kg_per_plot")
hrvt_test_s_mul23 = na.omit(hrvt_test_s_mul23)

hrvt_cont_s_mul23 = data_s_mul2[, c(186, 8:9, 27)]
names(hrvt_cont_s_mul23) = c("id", "farmer_i_d_and_name", "the_measurement_time", "vegetable_yield_kg_per_plot")
hrvt_cont_s_mul23 = na.omit(hrvt_cont_s_mul23)

hrvt_test_s_mul24 = data_s_mul2[, c(186, 8:9, 30)]
names(hrvt_test_s_mul24) = c("id", "farmer_i_d_and_name", "the_measurement_time", "vegetable_yield_kg_per_plot")
hrvt_test_s_mul24 = na.omit(hrvt_test_s_mul24)

hrvt_cont_s_mul24 = data_s_mul2[, c(186, 8:9, 33)]
names(hrvt_cont_s_mul24) = c("id", "farmer_i_d_and_name", "the_measurement_time", "vegetable_yield_kg_per_plot")
hrvt_cont_s_mul24 = na.omit(hrvt_cont_s_mul24)

hrvt_test_s_mul25 = data_s_mul2[, c(186, 8:9, 36)]
names(hrvt_test_s_mul25) = c("id", "farmer_i_d_and_name", "the_measurement_time", "vegetable_yield_kg_per_plot")
hrvt_test_s_mul25 = na.omit(hrvt_test_s_mul25)

hrvt_cont_s_mul25 = data_s_mul2[, c(186, 8:9, 39)]
names(hrvt_cont_s_mul25) = c("id", "farmer_i_d_and_name", "the_measurement_time", "vegetable_yield_kg_per_plot")
hrvt_cont_s_mul25 = na.omit(hrvt_cont_s_mul25)

hrvt_test_s_mul26 = data_s_mul2[, c(186, 8:9, 42)]
names(hrvt_test_s_mul26) = c("id", "farmer_i_d_and_name", "the_measurement_time", "vegetable_yield_kg_per_plot")
hrvt_test_s_mul26 = na.omit(hrvt_test_s_mul26)

hrvt_cont_s_mul26 = data_s_mul2[, c(186, 8:9, 45)]
names(hrvt_cont_s_mul26) = c("id", "farmer_i_d_and_name", "the_measurement_time", "vegetable_yield_kg_per_plot")
hrvt_cont_s_mul26 = na.omit(hrvt_cont_s_mul26)


pest_test_s_mul21 = pest_test_s_mul21[, c(11, 1:3)]
names(pest_test_s_mul21) = c("id", "number_of_leaves_per_plot", "leaf_color", "is_the_plant_attacked_by_any_pest")
pest_test_s_mul21$the_measurement_time = rep(1, nrow(pest_test_s_mul21))

pest_cont_s_mul21 = pest_cont_s_mul21[, c(11, 1:3)]
names(pest_cont_s_mul21) = c("id", "number_of_leaves_per_plot", "leaf_color", "is_the_plant_attacked_by_any_pest")
pest_cont_s_mul21$the_measurement_time = rep(1, nrow(pest_cont_s_mul21))

pest_test_s_mul22 = pest_test_s_mul22[, c(12, 1:3)]
names(pest_test_s_mul22) = c("id", "number_of_leaves_per_plot", "leaf_color", "is_the_plant_attacked_by_any_pest")
pest_test_s_mul22$the_measurement_time = rep(2, nrow(pest_test_s_mul22))

pest_cont_s_mul22 = pest_cont_s_mul22[, c(12, 1:3)]
names(pest_cont_s_mul22) = c("id", "number_of_leaves_per_plot", "leaf_color", "is_the_plant_attacked_by_any_pest")
pest_cont_s_mul22$the_measurement_time = rep(2, nrow(pest_cont_s_mul22))

pest_test_s_mul23 = pest_test_s_mul23[, c(11, 1:3)]
names(pest_test_s_mul23) = c("id", "number_of_leaves_per_plot", "leaf_color", "is_the_plant_attacked_by_any_pest")
pest_test_s_mul23$the_measurement_time = rep(3, nrow(pest_test_s_mul23))

pest_cont_s_mul23 = pest_cont_s_mul23[, c(11, 1:3)]
names(pest_cont_s_mul23) = c("id", "number_of_leaves_per_plot", "leaf_color", "is_the_plant_attacked_by_any_pest")
pest_cont_s_mul23$the_measurement_time = rep(3, nrow(pest_cont_s_mul23))

pest_test_s_mul24 = pest_test_s_mul24[, c(11, 1:3)]
names(pest_test_s_mul24) = c("id", "number_of_leaves_per_plot", "leaf_color", "is_the_plant_attacked_by_any_pest")
pest_test_s_mul24$the_measurement_time = rep(4, nrow(pest_test_s_mul24))

pest_cont_s_mul24 = pest_cont_s_mul24[, c(11, 1:3)]
names(pest_cont_s_mul24) = c("id", "number_of_leaves_per_plot", "leaf_color", "is_the_plant_attacked_by_any_pest")
pest_cont_s_mul24$the_measurement_time = rep(4, nrow(pest_cont_s_mul24))

pest_test_s_mul25 = pest_test_s_mul25[, c(11, 1:3)]
names(pest_test_s_mul25) = c("id", "number_of_leaves_per_plot", "leaf_color", "is_the_plant_attacked_by_any_pest")
pest_test_s_mul25$the_measurement_time = rep(5, nrow(pest_test_s_mul25))

pest_cont_s_mul25 = pest_cont_s_mul25[, c(11, 1:3)]
names(pest_cont_s_mul25) = c("id", "number_of_leaves_per_plot", "leaf_color", "is_the_plant_attacked_by_any_pest")
pest_cont_s_mul25$the_measurement_time = rep(5, nrow(pest_cont_s_mul25))

pest_test_s_mul26 = pest_test_s_mul26[, c(7, 1:2, 4)]
names(pest_test_s_mul26) = c("id", "number_of_leaves_per_plot", "leaf_color", "is_the_plant_attacked_by_any_pest")
pest_test_s_mul26$is_the_plant_attacked_by_any_pest = ifelse(pest_test_s_mul26$is_the_plant_attacked_by_any_pest > 0, 1, 0)
pest_test_s_mul26$the_measurement_time = rep(6, nrow(pest_test_s_mul26))

pest_cont_s_mul26 = pest_cont_s_mul26[, c(7, 1:2, 4)]
names(pest_cont_s_mul26) = c("id", "number_of_leaves_per_plot", "leaf_color", "is_the_plant_attacked_by_any_pest")
pest_cont_s_mul26$is_the_plant_attacked_by_any_pest = ifelse(pest_cont_s_mul26$is_the_plant_attacked_by_any_pest > 0, 1, 0)
pest_cont_s_mul26$the_measurement_time = rep(6, nrow(pest_cont_s_mul26))


hrvt_test_s_mul2 = rbind(hrvt_test_s_mul21, hrvt_test_s_mul22, hrvt_test_s_mul23,
                         hrvt_test_s_mul24, hrvt_test_s_mul25, hrvt_test_s_mul26)

hrvt_cont_s_mul2 = rbind(hrvt_cont_s_mul21, hrvt_cont_s_mul22, hrvt_cont_s_mul23,
                         hrvt_cont_s_mul24, hrvt_cont_s_mul25, hrvt_cont_s_mul26)

hrvt_test_s_mul2$treatment = rep("test", nrow(hrvt_test_s_mul2))

hrvt_cont_s_mul2$treatment = rep("control", nrow(hrvt_cont_s_mul2))

hrvt_s_mul2 = rbind(hrvt_test_s_mul2, hrvt_cont_s_mul2)


pest_test_s_mul2 = rbind(pest_test_s_mul21, pest_test_s_mul22, pest_test_s_mul23,
                         pest_test_s_mul24, pest_test_s_mul25, pest_test_s_mul26)

pest_cont_s_mul2 = rbind(pest_cont_s_mul21, pest_cont_s_mul22, pest_cont_s_mul23,
                         pest_cont_s_mul24, pest_cont_s_mul25, pest_cont_s_mul26)

pest_test_s_mul2$treatment = rep("test", nrow(pest_test_s_mul2))

pest_cont_s_mul2$treatment = rep("control", nrow(pest_cont_s_mul2))

pest_s_mul2 = rbind(pest_test_s_mul2, pest_cont_s_mul2)

pest_s_mul2$is_the_plant_attacked_by_any_pest = ifelse(pest_s_mul2$is_the_plant_attacked_by_any_pest == "Yes", 1, pest_s_mul2$is_the_plant_attacked_by_any_pest)
pest_s_mul2$is_the_plant_attacked_by_any_pest = ifelse(pest_s_mul2$is_the_plant_attacked_by_any_pest == "No", 0, pest_s_mul2$is_the_plant_attacked_by_any_pest)

pest_s_mul2$is_the_plant_attacked_by_any_pest = as.numeric(pest_s_mul2$is_the_plant_attacked_by_any_pest)

pest_s_mul2 = aggregate(. ~ id + treatment + the_measurement_time, FUN = mean,
                        na.rm = TRUE, data = pest_s_mul2)

pest_s_mul2$is_the_plant_attacked_by_any_pest = ifelse(pest_s_mul2$is_the_plant_attacked_by_any_pest > 0, 1, 0)


s_mul2 = merge(hrvt_s_mul2, pest_s_mul2, by = c("id", "treatment", "the_measurement_time"), all.x = TRUE)

s_mul2$leaf_color = round(s_mul2$leaf_color)

s_mul2$season = rep("Season2", nrow(s_mul2))

s_mul2 = s_mul2[, c(1, 4, 3, 6:7, 5, 8, 2, 9)]

s_mul = rbind(s_mul1, s_mul2)

s_mul = s_mul[, -c(4)]

s_mul$is_the_plant_attacked_by_any_pest = ifelse(s_mul$is_the_plant_attacked_by_any_pest == "Yes", 1, pest_s_mul2$is_the_plant_attacked_by_any_pest)
s_mul$is_the_plant_attacked_by_any_pest = ifelse(s_mul$is_the_plant_attacked_by_any_pest == "No", 0, pest_s_mul2$is_the_plant_attacked_by_any_pest)


write.xlsx(pest_b_ipm, "Input data//bean_ipm_pest.xlsx")
write.xlsx(hrvt_b_ipm, "Input data//bean_ipm_hrvt.xlsx")
write.xlsx(pest_b_man, "Input data//bean_manure_pest.xlsx")
write.xlsx(hrvt_b_man, "Input data//bean_manure_hrvt.xlsx")
write.xlsx(pest_b_ter, "Input data//bean_terrace_pest.xlsx")
write.xlsx(hrvt_b_ter, "Input data//bean_terrace_hrvt.xlsx")
write.xlsx(pest_m_ipm, "Input data//maize_ipm_pest.xlsx")
write.xlsx(hrvt_m_ipm, "Input data//maize_ipm_hrvt.xlsx")
write.xlsx(pest_m_man, "Input data//maize_manure_pest.xlsx")
write.xlsx(hrvt_m_man, "Input data//maize_manure_hrvt.xlsx")
write.xlsx(pest_m_ter, "Input data//maize_terrace_pest.xlsx")
write.xlsx(hrvt_m_ter, "Input data//maize_terrace_hrvt.xlsx")
write.xlsx(pest_c_ipm, "Input data//cabbage_ipm_pest.xlsx")
write.xlsx(hrvt_c_ipm, "Input data//cabbage_ipm_hrvt.xlsx")
write.xlsx(s_com, "Input data//spinash_compost.xlsx")
write.xlsx(s_mul, "Input data//spinash_mulch.xlsx")

