#----------------------------Sensitivity Analyses----------------------------#
library(survival)
library(tableone)
library(tidyverse)
source("Function.R")
load("/data/nhanes.rda")

fullvar <- c("age", "sex", "race", "educ", "hhinc", "hei", "tcal", "bmi_cat",
             "drink", "smoke", "pa_cat", "sleep_times", "diabetes", "hypten",
             "hypchol", "cvd", "cancer", "sleepdis")

## 1. Excluded participants with prevalent diabetes, CVD, or cancer
nhanesdf_sensi <- subset(nhanesdf, diabetes == "0. No" & cvd == "0. No" & cancer == "0. No")
cat_alldth_sen1 <- map_df(c("absejl_mid", "absejl_bf", "absejl_dn", "absejl_tcal", "absejl_am11", "absejl_pm5"), 
                          ~ cox_model(dataframe = nhanesdf_sensi, outcome = "allcause_dth", followyear = "followyear",
                                      exposure = .x, variable_cox = fullvar, cont = FALSE)) %>%
  data.frame() %>% 
  mutate(model = "sensitivity 1")
cont_alldth_sen1 <- map_df(c("nabsejl_mid", "nabsejl_bf", "nabsejl_dn", "nabsejl_tcal_per400", "nabsejl_am11_per8", "nabsejl_pm5_per10"), 
                           ~ cox_model(dataframe = nhanesdf_sensi, outcome = "allcause_dth", followyear = "followyear",
                                       exposure = .x, variable_cox = fullvar, cont = TRUE)) %>%
  data.frame() %>% 
  mutate(model = "sensitivity 1")

cat_cvddth_sen1 <- map_df(c("absejl_mid", "absejl_bf", "absejl_dn", "absejl_tcal", "absejl_am11", "absejl_pm5"), 
                          ~ cox_model(dataframe = nhanesdf_sensi, outcome = "cvd_dth", followyear = "followyear",
                                      exposure = .x, variable_cox = fullvar, cont = FALSE)) %>%
  data.frame() %>% 
  mutate(model = "sensitivity 1")
cont_cvddth_sen1 <- map_df(c("nabsejl_mid", "nabsejl_bf", "nabsejl_dn", "nabsejl_tcal_per400", "nabsejl_am11_per8", "nabsejl_pm5_per10"), 
                           ~ cox_model(dataframe = nhanesdf_sensi, outcome = "cvd_dth", followyear = "followyear",
                                       exposure = .x, variable_cox = fullvar, cont = TRUE)) %>%
  data.frame() %>% 
  mutate(model = "sensitivity 1")



## 2. Excluding events that occurred within the first two years of follow-up
nhanesdf_sensi_allcause <- subset(nhanesdf, !(allcause_dth == 1 & followyear < 2) | SEQN < 93703)
nhanesdf_sensi_cvd <- subset(nhanesdf, !(cvd_dth == 1 & followyear < 2) | SEQN < 93703)
nhanesdf_sensi_cancer <- subset(nhanesdf, !(cancer_dth == 1 & followyear < 2) | SEQN < 93703)
nhanesdf_sensi <- subset(nhanesdf, diabetes == "0. No" & cvd == "0. No" & cancer == "0. No")
cat_alldth_sen2 <- map_df(c("absejl_mid", "absejl_bf", "absejl_dn", "absejl_tcal", "absejl_am11", "absejl_pm5"), 
                          ~ cox_model(dataframe = nhanesdf_sensi_allcause, outcome = "allcause_dth", followyear = "followyear",
                                      exposure = .x, variable_cox = fullvar, cont = FALSE)) %>%
  data.frame() %>% 
  mutate(model = "sensitivity 2")
cont_alldth_sen2 <- map_df(c("nabsejl_mid", "nabsejl_bf", "nabsejl_dn", "nabsejl_tcal_per400", "nabsejl_am11_per8", "nabsejl_pm5_per10"), 
                           ~ cox_model(dataframe = nhanesdf_sensi_allcause, outcome = "allcause_dth", followyear = "followyear",
                                       exposure = .x, variable_cox = fullvar, cont = TRUE)) %>%
  data.frame() %>% 
  mutate(model = "sensitivity 2")

cat_cvddth_sen2 <- map_df(c("absejl_mid", "absejl_bf", "absejl_dn", "absejl_tcal", "absejl_am11", "absejl_pm5"), 
                          ~ cox_model(dataframe = nhanesdf_sensi_cvd, outcome = "cvd_dth", followyear = "followyear",
                                      exposure = .x, variable_cox = fullvar, cont = FALSE)) %>%
  data.frame() %>% 
  mutate(model = "sensitivity 2")
cont_cvddth_sen2 <- map_df(c("nabsejl_mid", "nabsejl_bf", "nabsejl_dn", "nabsejl_tcal_per400", "nabsejl_am11_per8", "nabsejl_pm5_per10"), 
                           ~ cox_model(dataframe = nhanesdf_sensi_cvd, outcome = "cvd_dth", followyear = "followyear",
                                       exposure = .x, variable_cox = fullvar, cont = TRUE)) %>%
  data.frame() %>% 
  mutate(model = "sensitivity 2")



## 3. Among individuals aged 40 years or older
nhanesdf_sensi <- subset(nhanesdf, age >= 40)
cat_alldth_sen3 <- map_df(c("absejl_mid", "absejl_bf", "absejl_dn", "absejl_tcal", "absejl_am11", "absejl_pm5"), 
                          ~ cox_model(dataframe = nhanesdf_sensi, outcome = "allcause_dth", followyear = "followyear",
                                      exposure = .x, variable_cox = fullvar, cont = FALSE)) %>%
  data.frame() %>% 
  mutate(model = "sensitivity 3")
cont_alldth_sen3 <- map_df(c("nabsejl_mid", "nabsejl_bf", "nabsejl_dn", "nabsejl_tcal_per400", "nabsejl_am11_per8", "nabsejl_pm5_per10"), 
                           ~ cox_model(dataframe = nhanesdf_sensi, outcome = "allcause_dth", followyear = "followyear",
                                       exposure = .x, variable_cox = fullvar, cont = TRUE)) %>%
  data.frame() %>% 
  mutate(model = "sensitivity 3")

cat_cvddth_sen3 <- map_df(c("absejl_mid", "absejl_bf", "absejl_dn", "absejl_tcal", "absejl_am11", "absejl_pm5"), 
                          ~ cox_model(dataframe = nhanesdf_sensi, outcome = "cvd_dth", followyear = "followyear",
                                      exposure = .x, variable_cox = fullvar, cont = FALSE)) %>%
  data.frame() %>% 
  mutate(model = "sensitivity 3")
cont_cvddth_sen3 <- map_df(c("nabsejl_mid", "nabsejl_bf", "nabsejl_dn", "nabsejl_tcal_per400", "nabsejl_am11_per8", "nabsejl_pm5_per10"), 
                           ~ cox_model(dataframe = nhanesdf_sensi, outcome = "cvd_dth", followyear = "followyear",
                                       exposure = .x, variable_cox = fullvar, cont = TRUE)) %>%
  data.frame() %>% 
  mutate(model = "sensitivity 3")



## 4. Additional inclusion of quadratic terms of age in the model
nhanesdf <- mutate(nhanesdf, age_quadratic = age^2)
cat_alldth_sen4 <- map_df(c("absejl_mid", "absejl_bf", "absejl_dn", "absejl_tcal", "absejl_am11", "absejl_pm5"), 
                          ~ cox_model(dataframe = nhanesdf, outcome = "allcause_dth", followyear = "followyear",
                                      exposure = .x, variable_cox = append(fullvar,"age_quadratic"), cont = FALSE)) %>%
  data.frame() %>% 
  mutate(model = "sensitivity 4")
cont_alldth_sen4 <- map_df(c("nabsejl_mid", "nabsejl_bf", "nabsejl_dn", "nabsejl_tcal_per400", "nabsejl_am11_per8", "nabsejl_pm5_per10"), 
                           ~ cox_model(dataframe = nhanesdf, outcome = "allcause_dth", followyear = "followyear",
                                       exposure = .x, variable_cox = append(fullvar,"age_quadratic"), cont = TRUE)) %>%
  data.frame() %>% 
  mutate(model = "sensitivity 4")

cat_cvddth_sen4 <- map_df(c("absejl_mid", "absejl_bf", "absejl_dn", "absejl_tcal", "absejl_am11", "absejl_pm5"), 
                          ~ cox_model(dataframe = nhanesdf, outcome = "cvd_dth", followyear = "followyear",
                                      exposure = .x, variable_cox = append(fullvar,"age_quadratic"), cont = FALSE)) %>%
  data.frame() %>% 
  mutate(model = "sensitivity 4")
cont_cvddth_sen4 <- map_df(c("nabsejl_mid", "nabsejl_bf", "nabsejl_dn", "nabsejl_tcal_per400", "nabsejl_am11_per8", "nabsejl_pm5_per10"), 
                           ~ cox_model(dataframe = nhanesdf, outcome = "cvd_dth", followyear = "followyear",
                                       exposure = .x, variable_cox = append(fullvar,"age_quadratic"), cont = TRUE)) %>%
  data.frame() %>% 
  mutate(model = "sensitivity 4")



## 5. Mutually adjusted
cat_alldth_sen5_dn <- map_df(c("absejl_dn"), 
                             ~ cox_model(dataframe = nhanesdf, outcome = "allcause_dth", followyear = "followyear",
                                         exposure = .x, variable_cox = append(fullvar, "absejl_tcal"), cont = FALSE)) %>%
  data.frame() %>% 
  mutate(model = "sensitivity 5")
cat_alldth_sen5_tcal <- map_df(c("absejl_tcal"), 
                               ~ cox_model(dataframe = nhanesdf, outcome = "allcause_dth", followyear = "followyear",
                                           exposure = .x, variable_cox = append(fullvar, "absejl_dn"), cont = FALSE)) %>%
  data.frame() %>% 
  mutate(model = "sensitivity 5")
cont_alldth_sen5_dn <- map_df(c("nabsejl_dn"), 
                              ~ cox_model(dataframe = nhanesdf, outcome = "allcause_dth", followyear = "followyear",
                                          exposure = .x, variable_cox = append(fullvar, "nabsejl_tcal_per400"), cont = TRUE)) %>%
  data.frame() %>% 
  mutate(model = "sensitivity 5")
cont_alldth_sen5_tcal <- map_df(c("nabsejl_tcal_per400"), 
                                ~ cox_model(dataframe = nhanesdf, outcome = "allcause_dth", followyear = "followyear",
                                            exposure = .x, variable_cox = append(fullvar, "nabsejl_dn"), cont = TRUE)) %>%
  data.frame() %>% 
  mutate(model = "sensitivity 5")

cat_cvddth_sen5_dn <- map_df(c("absejl_mid", "absejl_dn"), 
                             ~ cox_model(dataframe = nhanesdf, outcome = "cvd_dth", followyear = "followyear",
                                         exposure = .x, variable_cox = append(fullvar, "absejl_pm5"), cont = FALSE)) %>%
  data.frame() %>% 
  mutate(model = "sensitivity 5")
cat_cvddth_sen5_pm5 <- map_df(c("absejl_pm5"), 
                              ~ cox_model(dataframe = nhanesdf, outcome = "cvd_dth", followyear = "followyear",
                                          exposure = .x, variable_cox = append(fullvar, "absejl_dn"), cont = FALSE)) %>%
  data.frame() %>% 
  mutate(model = "sensitivity 5")
cont_cvddth_sen5_mid <- map_df(c("nabsejl_mid", "nabsejl_dn"), 
                               ~ cox_model(dataframe = nhanesdf, outcome = "cvd_dth", followyear = "followyear",
                                           exposure = .x, variable_cox = append(fullvar, "nabsejl_pm5_per10"), cont = TRUE)) %>%
  data.frame() %>% 
  mutate(model = "sensitivity 5")
cont_cvddth_sen5_pm5 <- map_df(c("nabsejl_pm5_per10"), 
                               ~ cox_model(dataframe = nhanesdf, outcome = "cvd_dth", followyear = "followyear",
                                           exposure = .x, variable_cox = append(fullvar, "nabsejl_dn"), cont = TRUE)) %>%
  data.frame() %>% 
  mutate(model = "sensitivity 5")



## 6. Adjusting BMI and PA as continuous variable
fullvar_cont <- c("age", "sex", "race", "educ", "hhinc", "hei", "tcal", "bmi",
                  "drink", "smoke", "LTPA_TIME", "sleep_times", "diabetes", "hypten",
                  "hypchol", "cvd", "cancer", "sleepdis")
cat_alldth_sen6 <- map_df(c("absejl_mid", "absejl_bf", "absejl_dn", "absejl_tcal", "absejl_am11", "absejl_pm5"), 
                          ~ cox_model(dataframe = nhanesdf, outcome = "allcause_dth", followyear = "followyear",
                                      exposure = .x, variable_cox = fullvar_cont, cont = FALSE)) %>%
  data.frame() %>% 
  mutate(model = "sensitivity 6")
cont_alldth_sen6 <- map_df(c("nabsejl_mid", "nabsejl_bf", "nabsejl_dn", "nabsejl_tcal_per400", "nabsejl_am11_per8", "nabsejl_pm5_per10"), 
                           ~ cox_model(dataframe = nhanesdf, outcome = "allcause_dth", followyear = "followyear",
                                       exposure = .x, variable_cox = fullvar_cont, cont = TRUE)) %>%
  data.frame() %>% 
  mutate(model = "sensitivity 6")

cat_cvddth_sen6 <- map_df(c("absejl_mid", "absejl_bf", "absejl_dn", "absejl_tcal", "absejl_am11", "absejl_pm5"), 
                          ~ cox_model(dataframe = nhanesdf, outcome = "cvd_dth", followyear = "followyear",
                                      exposure = .x, variable_cox = fullvar_cont, cont = FALSE)) %>%
  data.frame() %>% 
  mutate(model = "sensitivity 6")
cont_cvddth_sen6 <- map_df(c("nabsejl_mid", "nabsejl_bf", "nabsejl_dn", "nabsejl_tcal_per400", "nabsejl_am11_per8", "nabsejl_pm5_per10"), 
                           ~ cox_model(dataframe = nhanesdf, outcome = "cvd_dth", followyear = "followyear",
                                       exposure = .x, variable_cox = fullvar_cont, cont = TRUE)) %>%
  data.frame() %>% 
  mutate(model = "sensitivity 6")



## 7. Excluded participants who skip breakfast
summary(nhanesdf$occasion_d1)
nhanesdf_sensi <- subset(nhanesdf, occasion_min_d1 == "1" & occasion_min_d2 == "1")
cat_alldth_sen7 <- map_df(c("absejl_mid", "absejl_bf", "absejl_dn", "absejl_tcal", "absejl_am11", "absejl_pm5"), 
                          ~ cox_model(dataframe = nhanesdf_sensi, outcome = "allcause_dth", followyear = "followyear",
                                      exposure = .x, variable_cox = fullvar, cont = FALSE)) %>%
  data.frame() %>% 
  mutate(model = "sensitivity 7")
cont_alldth_sen7 <- map_df(c("nabsejl_mid", "nabsejl_bf", "nabsejl_dn", "nabsejl_tcal_per400", "nabsejl_am11_per8", "nabsejl_pm5_per10"), 
                           ~ cox_model(dataframe = nhanesdf_sensi, outcome = "allcause_dth", followyear = "followyear",
                                       exposure = .x, variable_cox = fullvar, cont = TRUE)) %>%
  data.frame() %>% 
  mutate(model = "sensitivity 7")

cat_cvddth_sen7 <- map_df(c("absejl_mid", "absejl_bf", "absejl_dn", "absejl_tcal", "absejl_am11", "absejl_pm5"), 
                          ~ cox_model(dataframe = nhanesdf_sensi, outcome = "cvd_dth", followyear = "followyear",
                                      exposure = .x, variable_cox = fullvar, cont = FALSE)) %>%
  data.frame() %>% 
  mutate(model = "sensitivity 7")
cont_cvddth_sen7 <- map_df(c("nabsejl_mid", "nabsejl_bf", "nabsejl_dn", "nabsejl_tcal_per400", "nabsejl_am11_per8", "nabsejl_pm5_per10"), 
                           ~ cox_model(dataframe = nhanesdf_sensi, outcome = "cvd_dth", followyear = "followyear",
                                       exposure = .x, variable_cox = fullvar, cont = TRUE)) %>%
  data.frame() %>% 
  mutate(model = "sensitivity 7")

sen_coef <- rbind(cat_alldth_sen1, cont_alldth_sen1,
                  cat_cvddth_sen1, cont_cvddth_sen1,
                  cat_alldth_sen2, cont_alldth_sen2,
                  cat_cvddth_sen2, cont_cvddth_sen2,
                  cat_alldth_sen3, cont_alldth_sen3,
                  cat_cvddth_sen3, cont_cvddth_sen3,
                  cat_alldth_sen4, cont_alldth_sen4,
                  cat_cvddth_sen4, cont_cvddth_sen4,
                  cat_alldth_sen5_dn, cat_alldth_sen5_tcal,
                  cont_alldth_sen5_dn, cont_alldth_sen5_tcal,
                  cat_cvddth_sen5_dn, cat_cvddth_sen5_pm5,
                  cont_cvddth_sen5_mid, cont_cvddth_sen5_pm5,
                  cat_alldth_sen6, cont_alldth_sen6,
                  cat_cvddth_sen6, cont_cvddth_sen6,
                  cat_alldth_sen7, cont_alldth_sen7,
                  cat_cvddth_sen7, cont_cvddth_sen7) %>% 
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
  arrange(outcome, model, exposure)
write_csv(sen_coef, "/results/sen_coef.csv")



