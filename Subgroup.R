#------------------------------Subgroup Analyses-----------------------------#
rm(list=ls())
library(survival)
library(tableone)
source("Function.R")
load("/data/nhanes.rda")

fullvar <- c("age", "sex", "race", "educ", "hhinc", "hei", "tcal", "bmi_cat",
             "drink", "smoke", "pa_cat", "sleep_times", "diabetes", "hypten",
             "hypchol", "cvd", "cancer", "sleepdis")


nhanesdf_sub <- nhanesdf %>%
  mutate(age_group = ifelse(age < 50, "0. <50", "1. >=50"),
         educ_group = ifelse(educ == "3. College or above", "2. College or above", "1. Below college"),
         pir_group = ifelse(hhinc == "3. High", "2. High", "1. Low or middle"),
         hei_group = cut(hei, breaks = quantile(HEI2015_TOTAL_SCORE, probs = 0:2/2, na.rm = TRUE),
                         labels = c("1. Below average", "2. Average or above"),
                         include.lowest = TRUE),
         bmi_group = ifelse(bmi_cat == "Normal", "1. <25", "2. >=25"),
         smoke_group = ifelse(smoke == "2. Current", "1. Yes", "0. No"),
         drink_group = ifelse(drink == "0. Non", "0. No", "1. Yes"),
         chronic_group = ifelse(diabetes == "1. Yes" | hypten == "1. Yes"
                                | hypten == "1. Yes" | cvd == "1. Yes"
                                | cancer == "1. Yes" | sleepdis == "1. Yes", "1. Yes", "0. No")
  )


exposures <- c("nabsejl_mid", "nabsejl_bf", "nabsejl_dn", "nabsejl_tcal_per400", "nabsejl_am11_per8", "nabsejl_pm5_per10")
subgroups <- c("age_group", "sex","educ_group", "pir_group", "hei_group", "bmi_group", "smoke_group", "drink_group", "chronic_group")
combinations <- expand_grid(exposure = exposures, subgroup = subgroups)
sg_coef_alldth <- pmap_df(combinations, function(exposure, subgroup) {
  cox_subgroup(
    dataframe = nhanesdf_sub,
    outcome = "allcause_dth",
    followyear = "followyear",
    exposure = exposure,
    variable_cox = fullvar,
    subgroup = subgroup,
    cont = TRUE
  )}) %>% 
  mutate(exposure = recode(exposure,
                           nabsejl_mid = "1. Eating midpoint per hour",
                           nabsejl_bf = "2. First meal per hour",
                           nabsejl_dn = "3. Last meal per hour",
                           nabsejl_tcal_per400 = "4. Total energy intake per 400kcal",
                           nabsejl_am11_per8 = "5. Morning energy intake per 8%",
                           nabsejl_pm5_per10 = "6. Evening energy intake per 10%")) %>% 
  arrange(exposure, subgroup, strata)
write.csv(sg_coef_alldth, file = "/results/sg_coef_alldth.csv")

sg_coef_cvddth <- pmap_df(combinations, function(exposure, subgroup) {
  cox_subgroup(
    dataframe = nhanesdf_sub,
    outcome = "cvd_dth",
    followyear = "followyear",
    exposure = exposure,
    variable_cox = fullvar,
    subgroup = subgroup,
    cont = TRUE
  )}) %>% 
  mutate(exposure = recode(exposure,
                           nabsejl_mid = "1. Eating midpoint per hour",
                           nabsejl_bf = "2. First meal per hour",
                           nabsejl_dn = "3. Last meal per hour",
                           nabsejl_tcal_per400 = "4. Total energy intake per 400kcal",
                           nabsejl_am11_per8 = "5. Morning energy intake per 8%",
                           nabsejl_pm5_per10 = "6. Evening energy intake per 10%")) %>% 
  arrange(exposure, subgroup, strata)
write.csv(sg_coef_cvddth, file = "/results/sg_coef_cvddth.csv")