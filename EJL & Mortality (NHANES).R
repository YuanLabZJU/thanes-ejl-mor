#----------------------------------------------------------------------------#
#                                                                            #
#                           Script Description                               #
#                                                                            #
#----------------------------------------------------------------------------#
#   Topic: Eating jet lag & Mortality                                        #
#   Title: Weekday-weekend eating jet lag and mortality risk                 #
#   Updated on: 2024/01/28 by Yihong Ding                                    #
#   Database: the National Health and Nutrition Examination Survey (NHANES)  #
#   Exposure: Weekday-weekend eating jet lag                                 #
#   Outcome: All-cause and cause-specific mortality risk                     #
#   Covariates: Age, Sex, Race, Education, HHIncome, HEI-2015,               #
#               Total Energy, BMI, Smoke, Drink, PA, Sleep Times,            #
#               Diabetes, Hyperten, Hyperchol, CVD, Cancer, Sleep dis        #
#----------------------------------------------------------------------------#





#-------------------------------Data Cleaning--------------------------------#
## Setting up environment
rm(list=ls())
library(tidyverse)
library(haven)
library(hms)
library(survey)
library(rlang)
source("Function.R")
load("/data/nhanes_cov_1999_2018.rda")   ## NHANES covariates (after multiple imputations)
load("/data/HEI_2Day_2003_2018.rda")   ## 2 days Healthy Eating Index
load("/data/nhanes_diet_2003_2018.rda")   ## 2 days dietary recall



## Covariates (NHANES 2003-2018)
nhanes_cov <- subset(nhanes_cov, SEQN >= 21005)
### Initial non-pregnant adult participants in NHANES 2003-2018
### N = 43849



## Exposure: Weekday-weekend eating jet lag
HEI2D2003_2018 <- HEI_2Day_2003_2018 %>%
  group_by(SEQN) %>%
  dplyr::summarize(HEI_Day1 = ifelse(min(DAYREC) == 1, HEI2015_TOTAL_SCORE[DAYREC == 1], NA),
                   HEI_Day2 = ifelse(max(DAYREC) == 2, HEI2015_TOTAL_SCORE[DAYREC == 2], NA))

diet_d1 <- dietFF2003_2018_d1 %>%
  subset(., DR1DRSTZ == 1 & DRDINT == 2) %>%
  group_by(SEQN) %>%
  mutate(occasion_d1 = case_when(DR1_030Z %in% c(1,5,10,11) ~ 1,   ## Eating occasion
                                 DR1_030Z %in% c(2,5,12) ~ 2,
                                 DR1_030Z %in% c(3,4,14) ~ 3),
         tkcal_d1 = sum(DR1IKCAL, na.rm = TRUE),   ## Total energy intake
         tkcal_am11_d1 = sum(DR1IKCAL[DR1_020<=as_hms("11:00:00")], na.rm = TRUE),   ## Energy intake before AM11
         tkcal_pm5_d1 = sum(DR1IKCAL[DR1_020>=as_hms("17:00:00")], na.rm = TRUE),   ## Energy intake after PM5
         am11_d1 = round(tkcal_am11_d1 / tkcal_d1 * 100, 2),   ## Energy intake % after AM11
         pm5_d1 = round(tkcal_pm5_d1 / tkcal_d1 * 100, 2),   ## Energy intake % after PM5
         noca_d1 = length(unique(occasion_d1)),   ## Number of eating occasion
         bf_d1 = as_hms(min(DR1_020[occasion_d1%in%c(1:3)], na.rm = TRUE)),   ## First eating occasion
         dn_d1 = as_hms(max(DR1_020[occasion_d1%in%c(1:3)], na.rm = TRUE)),   ## Last eating occasion
         mid_d1 = (dn_d1 + bf_d1) / 2,   ## Eating midpoint
         nfd_d1 = max(DR1_020[DR1IKCAL>=5], na.rm = TRUE) - min(DR1_020[DR1IKCAL>=5], na.rm = TRUE)) %>%   ## Nightly fasting duration
  mutate(bf_d1 = ifelse(bf_d1 == Inf | bf_d1 == -Inf, NA, bf_d1),
         dn_d1 = ifelse(dn_d1 == Inf | dn_d1 == -Inf, NA, dn_d1),
         mid_d1 = ifelse(mid_d1 == Inf | mid_d1 == -Inf, NA, mid_d1),
         nfd_d1 = ifelse(nfd_d1 == Inf | nfd_d1 == -Inf, NA, nfd_d1)) %>%
  mutate(occasion_min_d1 = min(occasion_d1, na.rm = TRUE)) %>% 
  ungroup() %>% 
  distinct(SEQN, .keep_all = TRUE)

diet_d2 <- dietFF2003_2018_d2 %>%
  subset(., DR2DRSTZ == 1) %>%
  group_by(SEQN) %>%
  mutate(occasion_d2 = case_when(DR2_030Z %in% c(1,5,10,11) ~ 1,   ## Eating occasion
                                 DR2_030Z %in% c(2,5,12) ~ 2,
                                 DR2_030Z %in% c(3,4,14) ~ 3),
         tkcal_d2 = sum(DR2IKCAL, na.rm = TRUE),   ## Total energy intake
         tkcal_am11_d2 = sum(DR2IKCAL[DR2_020<=as_hms("11:00:00")], na.rm = TRUE),   ## Energy intake before AM11
         tkcal_pm5_d2 = sum(DR2IKCAL[DR2_020>=as_hms("17:00:00")], na.rm = TRUE),   ## Energy intake after PM5
         am11_d2 = round(tkcal_am11_d2 / tkcal_d2 * 100, 2),   ## Energy intake % after AM11
         pm5_d2 = round(tkcal_pm5_d2 / tkcal_d2 * 100, 2),   ## Energy intake % after PM5
         noca_d2 = length(unique(occasion_d2)),   ## Number of eating occasion
         bf_d2 = as_hms(min(DR2_020[occasion_d2%in%c(1:3)], na.rm = TRUE)),   ## First eating occasion
         dn_d2 = as_hms(max(DR2_020[occasion_d2%in%c(1:3)], na.rm = TRUE)),   ## Last eating occasion
         mid_d2 = (dn_d2 + bf_d2) / 2,   ## Eating midpoint
         nfd_d2 = max(DR2_020[DR2IKCAL>=5], na.rm = TRUE) - min(DR2_020[DR2IKCAL>=5], na.rm = TRUE)) %>%   ## Nightly fasting duration
  mutate(bf_d2 = ifelse(bf_d2 == Inf | bf_d2 == -Inf, NA, bf_d2),
         dn_d2 = ifelse(dn_d2 == Inf | dn_d2 == -Inf, NA, dn_d2),
         mid_d2 = ifelse(mid_d2 == Inf | mid_d2 == -Inf, NA, mid_d2),
         nfd_d2 = ifelse(nfd_d2 == Inf | nfd_d2 == -Inf, NA, nfd_d2)) %>%
  mutate(occasion_min_d2 = min(occasion_d2, na.rm = TRUE)) %>% 
  ungroup() %>% 
  distinct(SEQN, .keep_all = TRUE)

diet_d12 <- left_join(diet_d1, diet_d2, by = "SEQN") %>%
  left_join(., HEI2D2003_2018, by = "SEQN")

nhanes <- left_join(nhanes_cov, diet_d12, by = "SEQN") %>%
  mutate(week_d1 = case_when(DR1DAY %in% c(1,7) ~ "Weekend",
                             DR1DAY %in% c(2:6) ~ "Weekday"),
         week_d2 = case_when(DR2DAY %in% c(1,7) ~ "Weekend",
                             DR2DAY %in% c(2:6) ~ "Weekday"),
         nfd = (nfd_d1 + nfd_d2) / 2)

ejl_calculate <- function(d1var, d2var, wkdayvar, wkendvar, ejlvar) {
  nhanes <- nhanes %>%
    mutate({{wkdayvar}} := ifelse(week_d1 == "Weekday", {{d1var}}, {{d2var}}),
           {{wkendvar}} := ifelse(week_d1 == "Weekend", {{d1var}}, {{d2var}}),
           {{ejlvar}} := {{wkendvar}}-{{wkdayvar}}) %>%
    ungroup()
  return(nhanes)
}
nhanes <- ejl_calculate(mid_d1, mid_d2, wkday_mid, wkend_mid, ejl_mid)   ## Weekend-weekday eating midpoint jet lag
nhanes <- ejl_calculate(bf_d1, bf_d2, wkday_bf, wkend_bf, ejl_bf)   ## Weekend-weekday breakfast jet lag
nhanes <- ejl_calculate(dn_d1, dn_d2, wkday_dn, wkend_dn, ejl_dn)   ## Weekend-weekday dinner jet lag
nhanes <- ejl_calculate(tkcal_d1, tkcal_d2, wkday_tcal, wkend_tcal, ejl_tcal)   ## Weekend-weekday total energy jet lag
nhanes <- ejl_calculate(am11_d1, am11_d2, wkday_am11, wkend_am11, ejl_am11)   ## Weekend-weekday AM11 energy jet lag
nhanes <- ejl_calculate(pm5_d1, pm5_d2, wkday_pm5, wkend_pm5, ejl_pm5)   ## Weekend-weekday PM5 energy jet lag
nhanes <- nhanes %>% 
  mutate_at(vars(ejl_bf, ejl_dn, ejl_mid), ~ ./3600) %>%
  mutate(absejl_bf = abs(ejl_bf),
         absejl_dn = abs(ejl_dn),
         absejl_mid = abs(ejl_mid),
         absejl_am11 = abs(ejl_am11),
         absejl_pm5 = abs(ejl_pm5),
         absejl_tcal = abs(ejl_tcal))



## Primary outcome: All-cause and cause-specific mortality
nhanes <- nhanes %>%
  mutate(allcause_dth = case_when(mortstat == 0 ~ 0,   ## All-cause death
                                  mortstat == 1 ~ 1),
         cvd_dth = case_when(mortstat == 0 | ucod_leading %in% c(2:4,6:10) ~ 0,   ## CVD death
                             ucod_leading %in% c(1,5) ~ 1),
         cmd_dth = case_when(mortstat == 0 | ucod_leading %in% c(2:4,6,8:10) ~ 0,   ## CMD death
                             ucod_leading %in% c(1,5,7) ~ 1),
         cancer_dth = case_when(mortstat == 0 | ucod_leading %in% c(1,3:10) ~ 0,   ## Cancer death
                                ucod_leading == 2 ~ 1),
         followyear = permth_int / 12)   ## Follow-up year



## Flowchart for primary analyses
nhanes <- nhanes %>% 
  mutate(tcal_exclu = ifelse((tcal>=800 & tcal<=4200 & sex=="1. Male")
                             | (tcal>=600 & tcal<=3500 & sex=="2. Female"), FALSE, TRUE))
alldata <- multiple_exclude(data = nhanes, 
                            conditions = c("occu == '2. Unemployed'",
                                           "tcal_exclu",
                                           "wkday_bf>=54000 | is.na(ejl_bf)",
                                           "week_d1 == week_d2",
                                           "is.na(allcause_dth)"))
exclude <- alldata[[2]]
exclude
nhanesdf <- alldata[[1]]



## Creating categorical variables for eating jet lag
nhanesdf <- nhanesdf %>%
  mutate(nabsejl_bf = absejl_bf,
         nabsejl_dn = absejl_dn,
         nabsejl_mid = absejl_mid,
         nabsejl_am11 = absejl_am11,
         nabsejl_am11_per8 = absejl_am11 / 8,
         nabsejl_pm5 = absejl_pm5,
         nabsejl_pm5_per10 = absejl_pm5 / 10,
         nabsejl_tcal = absejl_tcal,
         nabsejl_tcal_per400 = absejl_tcal / 400) %>% 
  mutate(across(.cols = c("absejl_bf", "absejl_dn", "absejl_mid"),
                .fns = ~ case_when(. < 1 ~ "1. EJL <1",
                                   . < 2 ~ "2. EJL 1-<2",
                                   . >= 2 ~ "3. EJL ≥2")),
         across(.cols = c("absejl_am11"),
                .fns = ~ case_when(. < 8 ~ "1. EJL <8",
                                   . < 16 ~ "2. EJL 8-<16",
                                   . >= 16 ~ "3. EJL ≥16")),
         across(.cols = c("absejl_pm5"),
                .fns = ~ case_when(. < 10 ~ "1. EJL <10",
                                   . < 20 ~ "2. EJL 10-<20",
                                   . >= 20 ~ "3. EJL ≥20")),
         across(.cols = c("absejl_tcal"),
                .fns = ~ case_when(. < 400 ~ "1. EJL <400",
                                   . < 800 ~ "2. EJL 400-<800",
                                   . >= 800 ~ "3. EJL ≥800"))) %>% 
  mutate(across(.cols = c("ejl_bf", "ejl_dn", "ejl_mid"),
                .fns = ~ case_when(. <= -2 ~ "2. Earlier ≥2h",
                                   . <= -1 ~ "3. Earlier 1-<2h",
                                   . < 1 ~ "1. Within 1h",
                                   . < 2 ~ "4. Later 1-<2h",
                                   . >= 2 ~ "5. Later ≥2h")),
         across(.cols = c("ejl_am11"),
                .fns = ~ case_when(. <= -16 ~ "2. Less ≥16",
                                   . <= -8 ~ "3. Less 8-<16",
                                   . < 8 ~ "1. Within 8",
                                   . < 16 ~ "4. More 8-<16",
                                   . >= 16 ~ "5. More ≥16")),
         across(.cols = c("ejl_pm5"),
                .fns = ~ case_when(. <= -20 ~ "2. Less ≥20",
                                   . <= -10 ~ "3. Less 10-<20",
                                   . < 10 ~ "1. Within 10",
                                   . < 20 ~ "4. More 10-<20",
                                   . >= 20 ~ "5. More ≥20")),
         across(.cols = c("ejl_tcal"),
                .fns = ~ case_when(. <= -800 ~ "2. Less ≥800",
                                   . <= -400 ~ "3. Less 400-<800",
                                   . < 400 ~ "1. Within 400",
                                   . < 800 ~ "4. More 400-<800",
                                   . >= 800 ~ "5. More ≥800")))
factorVars <- c("absejl_bf", "absejl_dn", "absejl_mid", "absejl_am11", "absejl_pm5", "absejl_tcal",
                "ejl_bf", "ejl_dn", "ejl_mid", "ejl_am11", "ejl_pm5", "ejl_tcal")
nhanesdf[ ,factorVars] <- lapply(nhanesdf[ ,factorVars], factor)





#-----------------------------Primary Analyses-------------------------------#
library(survival)
library(tableone)
source("Function.R")

model1 <- c("age", "sex")
model2 <- c("age", "sex", "race", "educ", "hhinc")
model3 <- c("age", "sex", "race", "educ", "hhinc", "hei", "tcal", "bmi_cat",
            "drink", "smoke", "pa_cat", "sleep_times", "diabetes", "hypten",
            "hypchol", "cvd", "cancer", "sleepdis")

### Weekend-weekday jet lag & All-cause death
cat_mv1 <- map_df(c("absejl_mid", "absejl_bf", "absejl_dn", "absejl_tcal", "absejl_am11", "absejl_pm5"), 
                  ~ cox_model(dataframe = nhanesdf, outcome = "allcause_dth", followyear = "followyear",
                              exposure = .x, variable_cox = model1, cont = FALSE)) %>%
  data.frame() %>% 
  mutate(model = "mv1")
cont_mv1 <- map_df(c("nabsejl_mid", "nabsejl_bf", "nabsejl_dn", "nabsejl_tcal_per400", "nabsejl_am11_per8", "nabsejl_pm5_per10"), 
                   ~ cox_model(dataframe = nhanesdf, outcome = "allcause_dth", followyear = "followyear",
                               exposure = .x, variable_cox = model1, cont = TRUE)) %>%
  data.frame() %>% 
  mutate(model = "mv1")

cat_mv2 <- map_df(c("absejl_mid", "absejl_bf", "absejl_dn", "absejl_tcal", "absejl_am11", "absejl_pm5"), 
                  ~ cox_model(dataframe = nhanesdf, outcome = "allcause_dth", followyear = "followyear",
                              exposure = .x, variable_cox = model2, cont = FALSE)) %>%
  data.frame() %>% 
  mutate(model = "mv2")
cont_mv2 <- map_df(c("nabsejl_mid", "nabsejl_bf", "nabsejl_dn", "nabsejl_tcal_per400", "nabsejl_am11_per8", "nabsejl_pm5_per10"), 
                   ~ cox_model(dataframe = nhanesdf, outcome = "allcause_dth", followyear = "followyear",
                               exposure = .x, variable_cox = model2, cont = TRUE)) %>%
  data.frame() %>% 
  mutate(model = "mv2")

cat_mv3 <- map_df(c("absejl_mid", "absejl_bf", "absejl_dn", "absejl_tcal", "absejl_am11", "absejl_pm5"), 
                  ~ cox_model(dataframe = nhanesdf, outcome = "allcause_dth", followyear = "followyear",
                              exposure = .x, variable_cox = model3, cont = FALSE)) %>%
  data.frame() %>% 
  mutate(model = "mv3")
cont_mv3 <- map_df(c("nabsejl_mid", "nabsejl_bf", "nabsejl_dn", "nabsejl_tcal_per400", "nabsejl_am11_per8", "nabsejl_pm5_per10"), 
                   ~ cox_model(dataframe = nhanesdf, outcome = "allcause_dth", followyear = "followyear",
                               exposure = .x, variable_cox = model3, cont = TRUE)) %>%
  data.frame() %>% 
  mutate(model = "mv3")

alldth_coef <- rbind(cat_mv1, cont_mv1,
                     cat_mv2, cont_mv2,
                     cat_mv3, cont_mv3) %>% 
  mutate(exposure = recode(exposure,
                           absejl_mid = "1. Eating midpoint cat",
                           absejl_bf = "2. First meal cat",
                           absejl_dn = "3. Last meal cat",
                           absejl_tcal = "4. Total energy intake cat",
                           absejl_am11 = "5. Morning energy intake cat",
                           absejl_pm5 = "6. Evening energy intake cat",
                           nabsejl_mid = "1. Eating midpoint per hour",
                           nabsejl_bf = "2. First meal per hour",
                           nabsejl_dn = "3. Last meal per hour",
                           nabsejl_tcal_per400 = "4. Total energy intake per 400kcal",
                           nabsejl_am11_per8 = "5. Morning energy intake per 8%",
                           nabsejl_pm5_per10 = "6. Evening energy intake per 10%")) %>% 
  arrange(model, exposure)
write_csv(alldth_coef, "/results/alldth_coef.csv")


### Weekend-weekday jet lag & CVD death
cat_mv1 <- map_df(c("absejl_mid", "absejl_bf", "absejl_dn", "absejl_tcal", "absejl_am11", "absejl_pm5"), 
                  ~ cox_model(dataframe = nhanesdf, outcome = "cvd_dth", followyear = "followyear",
                              exposure = .x, variable_cox = model1, cont = FALSE)) %>%
  data.frame() %>% 
  mutate(model = "mv1")
cont_mv1 <- map_df(c("nabsejl_mid", "nabsejl_bf", "nabsejl_dn", "nabsejl_tcal_per400", "nabsejl_am11_per8", "nabsejl_pm5_per10"), 
                   ~ cox_model(dataframe = nhanesdf, outcome = "cvd_dth", followyear = "followyear",
                               exposure = .x, variable_cox = model1, cont = TRUE)) %>%
  data.frame() %>% 
  mutate(model = "mv1")

cat_mv2 <- map_df(c("absejl_mid", "absejl_bf", "absejl_dn", "absejl_tcal", "absejl_am11", "absejl_pm5"), 
                  ~ cox_model(dataframe = nhanesdf, outcome = "cvd_dth", followyear = "followyear",
                              exposure = .x, variable_cox = model2, cont = FALSE)) %>%
  data.frame() %>% 
  mutate(model = "mv2")
cont_mv2 <- map_df(c("nabsejl_mid", "nabsejl_bf", "nabsejl_dn", "nabsejl_tcal_per400", "nabsejl_am11_per8", "nabsejl_pm5_per10"), 
                   ~ cox_model(dataframe = nhanesdf, outcome = "cvd_dth", followyear = "followyear",
                               exposure = .x, variable_cox = model2, cont = TRUE)) %>%
  data.frame() %>% 
  mutate(model = "mv2")

cat_mv3 <- map_df(c("absejl_mid", "absejl_bf", "absejl_dn", "absejl_tcal", "absejl_am11", "absejl_pm5"), 
                  ~ cox_model(dataframe = nhanesdf, outcome = "cvd_dth", followyear = "followyear",
                              exposure = .x, variable_cox = model3, cont = FALSE)) %>%
  data.frame() %>% 
  mutate(model = "mv3")
cont_mv3 <- map_df(c("nabsejl_mid", "nabsejl_bf", "nabsejl_dn", "nabsejl_tcal_per400", "nabsejl_am11_per8", "nabsejl_pm5_per10"), 
                   ~ cox_model(dataframe = nhanesdf, outcome = "cvd_dth", followyear = "followyear",
                               exposure = .x, variable_cox = model3, cont = TRUE)) %>%
  data.frame() %>% 
  mutate(model = "mv3")

cvdth_coef <- rbind(cat_mv1, cont_mv1,
                    cat_mv2, cont_mv2,
                    cat_mv3, cont_mv3) %>% 
  mutate(exposure = recode(exposure,
                           absejl_mid = "1. Eating midpoint cat",
                           absejl_bf = "2. First meal cat",
                           absejl_dn = "3. Last meal cat",
                           absejl_tcal = "4. Total energy intake cat",
                           absejl_am11 = "5. Morning energy intake cat",
                           absejl_pm5 = "6. Evening energy intake cat",
                           nabsejl_mid = "1. Eating midpoint per hour",
                           nabsejl_bf = "2. First meal per hour",
                           nabsejl_dn = "3. Last meal per hour",
                           nabsejl_tcal_per400 = "4. Total energy intake per 400kcal",
                           nabsejl_am11_per8 = "5. Morning energy intake per 8%",
                           nabsejl_pm5_per10 = "6. Evening energy intake per 10%")) %>% 
  arrange(model, exposure)
write_csv(cvdth_coef, "/results/cvdth_coef.csv")

### Weekend-weekday jet lag & Cancer death
cat_mv1 <- map_df(c("absejl_mid", "absejl_bf", "absejl_dn", "absejl_tcal", "absejl_am11", "absejl_pm5"), 
                  ~ cox_model(dataframe = nhanesdf, outcome = "cancer_dth", followyear = "followyear",
                              exposure = .x, variable_cox = model1, cont = FALSE)) %>%
  data.frame() %>% 
  mutate(model = "mv1")
cont_mv1 <- map_df(c("nabsejl_mid", "nabsejl_bf", "nabsejl_dn", "nabsejl_tcal_per400", "nabsejl_am11_per8", "nabsejl_pm5_per10"), 
                   ~ cox_model(dataframe = nhanesdf, outcome = "cancer_dth", followyear = "followyear",
                               exposure = .x, variable_cox = model1, cont = TRUE)) %>%
  data.frame() %>% 
  mutate(model = "mv1")

cat_mv2 <- map_df(c("absejl_mid", "absejl_bf", "absejl_dn", "absejl_tcal", "absejl_am11", "absejl_pm5"), 
                  ~ cox_model(dataframe = nhanesdf, outcome = "cancer_dth", followyear = "followyear",
                              exposure = .x, variable_cox = model2, cont = FALSE)) %>%
  data.frame() %>% 
  mutate(model = "mv2")
cont_mv2 <- map_df(c("nabsejl_mid", "nabsejl_bf", "nabsejl_dn", "nabsejl_tcal_per400", "nabsejl_am11_per8", "nabsejl_pm5_per10"), 
                   ~ cox_model(dataframe = nhanesdf, outcome = "cancer_dth", followyear = "followyear",
                               exposure = .x, variable_cox = model2, cont = TRUE)) %>%
  data.frame() %>% 
  mutate(model = "mv2")

cat_mv3 <- map_df(c("absejl_mid", "absejl_bf", "absejl_dn", "absejl_tcal", "absejl_am11", "absejl_pm5"), 
                  ~ cox_model(dataframe = nhanesdf, outcome = "cancer_dth", followyear = "followyear",
                              exposure = .x, variable_cox = model3, cont = FALSE)) %>%
  data.frame() %>% 
  mutate(model = "mv3")
cont_mv3 <- map_df(c("nabsejl_mid", "nabsejl_bf", "nabsejl_dn", "nabsejl_tcal_per400", "nabsejl_am11_per8", "nabsejl_pm5_per10"), 
                   ~ cox_model(dataframe = nhanesdf, outcome = "cancer_dth", followyear = "followyear",
                               exposure = .x, variable_cox = model3, cont = TRUE)) %>%
  data.frame() %>% 
  mutate(model = "mv3")

cancerth_coef <- rbind(cat_mv1, cont_mv1,
                       cat_mv2, cont_mv2,
                       cat_mv3, cont_mv3) %>% 
  mutate(exposure = recode(exposure,
                           absejl_mid = "1. Eating midpoint cat",
                           absejl_bf = "2. First meal cat",
                           absejl_dn = "3. Last meal cat",
                           absejl_tcal = "4. Total energy intake cat",
                           absejl_am11 = "5. Morning energy intake cat",
                           absejl_pm5 = "6. Evening energy intake cat",
                           nabsejl_mid = "1. Eating midpoint per hour",
                           nabsejl_bf = "2. First meal per hour",
                           nabsejl_dn = "3. Last meal per hour",
                           nabsejl_tcal_per400 = "4. Total energy intake per 400kcal",
                           nabsejl_am11_per8 = "5. Morning energy intake per 8%",
                           nabsejl_pm5_per10 = "6. Evening energy intake per 10%")) %>% 
  arrange(model, exposure)
write_csv(cancerth_coef, "/results/cancerth_coef.csv")