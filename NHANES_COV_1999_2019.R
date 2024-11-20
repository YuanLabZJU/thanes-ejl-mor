# Covariates in NHANES 1999-2018
# Programmer: Yihong Ding
# Updated on: 2024/01/27



## Setting up environment
rm(list=ls())
library(readr)
library(dplyr)
library(survey)
library(haven)
library(sqldf)
library(readxl)
options(survey.lonely.psu='adjust')
cat("R package versions:\n")
for (p in c("base", "survey","dplyr","haven")) { 
  cat(p, ": ", as.character(packageVersion(p)), "\n")
}
## base :  4.3.0 
## survey :  4.2.1 
## dplyr :  1.1.4
## haven :  2.5.2



## Death data
file_path <- ("/Users/dingyihong/Documents/Database/NHANES/death/")
lmf <- data.frame()
file_name <- c("NHANES_1999_2000", "NHANES_2001_2002", "NHANES_2003_2004", "NHANES_2005_2006",
               "NHANES_2007_2008", "NHANES_2009_2010", "NHANES_2011_2012", "NHANES_2013_2014",
               "NHANES_2015_2016", "NHANES_2017_2018")
for (i in 1:10) {
  x <- file_name[i]
  srvyin <- paste0(file_path, x, "_MORT_2019_PUBLIC.dat", collapse = NULL, recycle0 = FALSE)
  srvyout <- x
  dsn <- read_fwf(file=srvyin,
                  col_types = "iiiiiiii",
                  fwf_cols(seqn = c(1,6),
                           eligstat = c(15,15),
                           mortstat = c(16,16),
                           ucod_leading = c(17,19),
                           diabetes = c(20,20),
                           hyperten = c(21,21),
                           permth_int = c(43,45),
                           permth_exm = c(46,48)
                  ),
                  na = c("", ".")
  )
  lmf <- rbind(lmf, dsn)
}
str(lmf)
table(lmf$eligstat)   #ELIGSTAT: Eligibility Status for Mortality Follow-up
# 1 = "Eligible" (59064)
# 2 = "Under age 18, not available for public release" (42112)
# 3 = "Ineligible" (140)

table(lmf$mortstat, useNA="ifany")   #MORTSTAT: Final Mortality Status
# 0 = Assumed alive (49815)
# 1 = Assumed deceased (9249)
# <NA> = Ineligible or under age 18 (42252)

table(lmf$ucod_leading, useNA="ifany")   #UCOD_LEADING: Underlying Cause of Death: Recode
# 1 = Diseases of heart (I00-I09, I11, I13, I20-I51)
# 2 = Malignant neoplasms (C00-C97)
# 3 = Chronic lower respiratory diseases (J40-J47)
# 4 = Accidents (unintentional injuries) (V01-X59, Y85-Y86)
# 5 = Cerebrovascular diseases (I60-I69)
# 6 = Alzheimer's disease (G30)
# 7 = Diabetes mellitus (E10-E14)
# 8 = Influenza and pneumonia (J09-J18)
# 9 = Nephritis, nephrotic syndrome and nephrosis (N00-N07, N17-N19, N25-N27)
# 10 = All other causes (residual)
# <NA> = Ineligible, under age 18, assumed alive, or no cause of death data available

table(dsn$diabetes, useNA="ifany")   #DIABETES: Diabetes Flag from Multiple Cause of Death (MCOD)
# 0 = No - Condition not listed as a multiple cause of death
# 1 = Yes - Condition listed as a multiple cause of death
# <NA> = Assumed alive, under age 18, ineligible for mortality follow-up, or MCOD not available

table(dsn$hyperten, useNA="ifany")   #HYPERTEN: Hypertension Flag from Multiple Cause of Death (MCOD)
# 0 = No - Condition not listed as a multiple cause of death
# 1 = Yes - Condition listed as a multiple cause of death
# <NA> = Assumed alive, under age 18, ineligible for mortality follow-up, or MCOD not available'



## Demographics Data
file_path <- ("/Users/dingyihong/Documents/Database/NHANES/Demographics/")
filename <- c("", "_B", "_C", "_D", "_E", "_F", "_G", "_H", "_I", "_J")
demo1999_2018 <- data.frame()
for (i in 1:10) {
  x <- filename[i]
  pathway <- paste0(file_path, "DEMO", x, ".XPT", collapse = NULL, recycle0 = FALSE)
  data <- read_xpt(pathway)
  data <- data[,c("SEQN","RIAGENDR","RIDAGEYR","RIDRETH1","DMDEDUC2",
                  "DMDMARTL","INDFMPIR","RIDEXPRG","WTINT2YR")]
  demo1999_2018 <- rbind(demo1999_2018, data)
}

DEMO1999_2018 <- mutate(demo1999_2018,
                        one = 1,
                        age = RIDAGEYR,   ## Age
                        age2 = age**2,
                        sex = case_when(RIAGENDR == 1 ~ "1. Male",   ## Sex
                                        RIAGENDR == 2 ~ "2. Female"),
                        race = case_when(RIDRETH1 == 3 ~ "1. Non-Hispanic White",   ## Race
                                         RIDRETH1 == 4 ~ "2. Non-Hispanic White",
                                         RIDRETH1 %in% c(1,2) ~ "3. Hispanic",
                                         RIDRETH1 == 5 ~ "4. Other"),
                        marriage = case_when(DMDMARTL %in% c(1,6) ~ "1. Married",   ## Marriage status
                                             DMDMARTL %in% c(2:4) ~ "2. Separated",
                                             DMDMARTL == 5 ~ "3. Never married"),
                        educ = case_when(DMDEDUC2 %in% c(1,2) ~ "1. Below high school",   ## Education level
                                         DMDEDUC2 == 3 ~ "2. High school or equivalent",
                                         DMDEDUC2 %in% c(4,5) ~ "3. College or above"),
                        hhinc = case_when(INDFMPIR <= 1 ~ "1. Low",   ## Household income
                                          INDFMPIR > 1 & INDFMPIR < 4 ~ "2. Middle",
                                          INDFMPIR >= 4 ~ "3. High"))
DEMO <- mutate(DEMO1999_2018, preg = ifelse(RIDEXPRG == 1 & is.na(RIDEXPRG) == FALSE, 1, 0))
## Initial participants in NHANES 1999-2018
## N = 101316



## Merged with death data
names(lmf)[1] <- "SEQN"
NHANESDATA <- left_join(DEMO, lmf, by = "SEQN")



## Healthy Eating Index
file_path <- ("/Users/dingyihong/Documents/Database/NHANES/HEI/")
df <- NHANESDATA
HEI1999_2018 <- data.frame()
HEIfilename <- c("1999_2000","2001_2002","2003_2004","2005_2006","2007_2008",
                 "2009_2010","2011_2012","2013_2014","2015_2016","2017_2018")
for (i in 1:10) {
  x <- HEIfilename[i]
  pathway <- paste0(file_path, x, ".xlsx", collapse = NULL, recycle0 = FALSE)
  data <- read_excel(pathway)
  HEI1999_2018 <- rbind(HEI1999_2018, data)
}
NHANESDATA <- left_join(df, HEI1999_2018, by = "SEQN") %>%
  mutate(tcal = KCAL,   ## Total energy intake
         hei = HEI2015_TOTAL_SCORE,   ## Healthy Eating Index 2015
         hei_quantiles = cut(HEI2015_TOTAL_SCORE,   ## HEI score as quartiles
                             breaks = quantile(HEI2015_TOTAL_SCORE, probs = 0:4/4, na.rm = TRUE),
                             labels = c("Q1", "Q2", "Q3", "Q4"),
                             include.lowest = TRUE))
summary(NHANESDATA)



## Alcohol Use Data
file_path <- ("/Users/dingyihong/Documents/Database/NHANES/Questionnaire/")
df <- NHANESDATA
alco1999_2016 <- data.frame()
for (i in 1:9) {
  x <- filename[i]
  pathway <- paste0(file_path, "ALQ", x, ".XPT", collapse = NULL, recycle0 = FALSE)
  data <- read_xpt(pathway)
  data <- data[,c("SEQN","ALQ110","ALQ120Q","ALQ120U","ALQ130")]
  alco1999_2016 <- rbind(alco1999_2016, data)
}
alco2017_2018 <- read_xpt(paste0(file_path, "ALQ_J.XPT"))[, c("SEQN","ALQ111","ALQ121","ALQ130")]
alco1999_2016 <- mutate_at(alco1999_2016, vars(ALQ120Q, ALQ130), ~ ifelse(. >= 777, NA, .)) %>%
  mutate(DRINK_FREQUENCY = case_when(ALQ120U == 1 ~ ALQ120Q*365/7,
                                     ALQ120U == 2 ~ ALQ120Q*12,
                                     ALQ120U == 3 ~ ALQ120Q),
         DRINK_CONSUMPTION = DRINK_FREQUENCY * ALQ130 / 365)
alco2017_2018 <- mutate_at(alco2017_2018, vars(ALQ130), ~ ifelse(. >= 777, NA, .)) %>%
  mutate(DRINK_FREQUENCY = case_when(ALQ121 == 0 ~ 0,
                                     ALQ121 %in% c(1:2) ~ 365,
                                     ALQ121 == 3 ~ 365/2,
                                     ALQ121 == 4 ~ 365/7*2,
                                     ALQ121 == 5 ~ 365/7,
                                     ALQ121 == 6 ~ 12*2.5,
                                     ALQ121 == 7 ~ 12,
                                     ALQ121 == 8 ~ 9,
                                     ALQ121 == 9 ~ 4.5,
                                     ALQ121 == 10 ~ 1.5),
         DRINK_CONSUMPTION = DRINK_FREQUENCY * ALQ130 / 365,
         ALQ110 = ALQ111,
         ALQ120Q = ALQ121)
ALCO <- rbind(alco1999_2016[, c("SEQN","ALQ110","ALQ120Q","DRINK_CONSUMPTION")],
              alco2017_2018[, c("SEQN","ALQ110","ALQ120Q","DRINK_CONSUMPTION")])
NHANESDATA <- left_join(df, ALCO, by = "SEQN") %>%
  mutate(drink_times = DRINK_CONSUMPTION * 7,   ## Alcohol intake times/week
         drink = case_when(ALQ110 == 2 ~ "0. Non",   ## Alcohol consumption
                           (sex == "1. Male" & DRINK_CONSUMPTION < 2)
                           | (sex == "2. Female" & DRINK_CONSUMPTION < 1)
                           | ALQ120Q == 0 ~ "1. L2M",
                           (sex == "1. Male" & DRINK_CONSUMPTION >= 2)
                           | (sex == "2. Female" & DRINK_CONSUMPTION >= 1) ~ "2. Heavy"))



## Cigarette Use Data
df <- NHANESDATA
smoke1999_2018 <- data.frame()
for (i in 1:10) {
  x <- filename[i]
  pathway <- paste0(file_path, "SMQ", x, ".XPT", collapse = NULL, recycle0 = FALSE)
  data <- read_xpt(pathway)
  data <- data[,c("SEQN","SMQ020","SMQ040")]
  smoke1999_2018 <- rbind(smoke1999_2018, data)
}
SMO <- mutate(smoke1999_2018, smoke = case_when(SMQ020 == 2 ~ "0. Never",   ## Smoking status
                                                SMQ020 == 1 & SMQ040 == 3 ~ "1. Former",
                                                SMQ040 == 1 | SMQ040 == 2 ~ "2. Current"))
NHANESDATA <- left_join(df, SMO, by = "SEQN")



## Occupation Data
df <- NHANESDATA
occu <- data.frame()
occu <- read_xpt(paste0(file_path, "OCQ.XPT"))[,c("SEQN","OCQ150","OCQ380")]
names(occu)[2] <- "OCD150"
for (i in 2:10) {
  x <- filename[i]
  pathway <- paste0(file_path, "OCQ", x, ".XPT", collapse = NULL, recycle0 = FALSE)
  data <- read_xpt(pathway)
  data <- data[,c("SEQN","OCD150","OCQ380")]
  occu <- rbind(occu, data)
}
occu <- mutate(occu, occu = case_when(OCD150 %in% c(1,2) ~ "1. Employed",
                                      OCD150 %in% c(3,4) ~ "2. Unemployed"))
NHANESDATA <- left_join(df, occu, by = "SEQN")



## Physical Activity Data
df <- NHANESDATA
phsy1999_2006_IAF <- data.frame()
for (i in 1:4) {
  x <- filename[i]
  pathway <- paste0(file_path, "PAQ", x, ".XPT", collapse = NULL, recycle0 = FALSE)
  pathway_iaf <- paste0(file_path, "PAQIAF", x, ".XPT", collapse = NULL, recycle0 = FALSE)
  data <- read_xpt(pathway)[,c("SEQN","PAD200","PAD320")]
  data_iaf <- read_xpt(pathway_iaf)[, c("SEQN","PADLEVEL","PADTIMES","PADDURAT")] %>%
    mutate(LTPA_TIME = PADLEVEL * PADTIMES * PADDURAT / 30 * 7)  %>%
    group_by(SEQN) %>%
    summarise(LTPA_TIME = round(sum(LTPA_TIME), 2)) %>%
    left_join(data, ., by = "SEQN") %>%
    mutate(LTPA_TIME = case_when(PAD200 == 1 | PAD320 == 1 ~ LTPA_TIME,
                                 PAD200 == 2 & PAD320 == 2 ~ 0)) %>%
    select("SEQN", "LTPA_TIME")
  phsy1999_2006_IAF <- rbind(phsy1999_2006_IAF, data_iaf)
}
phsy2007_2018 <- data.frame()
for (i in 5:10) {
  x <- filename[i]
  pathway <- paste0(file_path, "PAQ", x, ".XPT", collapse = NULL, recycle0 = FALSE)
  data <- read_xpt(pathway)
  data <- data[,c("SEQN","PAQ650","PAQ655","PAD660","PAQ665","PAQ670","PAD675")]
  phsy2007_2018 <- rbind(phsy2007_2018, data)
}
PHSY <- phsy2007_2018 %>%
  mutate(across(.cols = c("PAQ655", "PAQ670"), .fns = ~ ifelse(. >= 77, NA, .)),
         across(.cols = c("PAD660", "PAD675"), .fns = ~ ifelse(. >= 7777, NA, .))) %>%
  mutate(VIGOROUS_TIME = case_when(PAQ650 == 2 ~ 0,
                                   PAQ650 == 1 ~ PAQ655 * PAD660 * 2),
         MODERATE_TIME = case_when(PAQ665 == 2 ~ 0,
                                   PAQ665 == 1 ~ PAQ670 * PAD675),
         LTPA_TIME = round(rowSums(data.frame(VIGOROUS_TIME,MODERATE_TIME)), 2)) %>%
  select("SEQN", "LTPA_TIME") %>%
  rbind(phsy1999_2006_IAF, .)
NHANESDATA <- left_join(df, PHSY, by = "SEQN") %>%
  mutate(pa_cat = case_when(LTPA_TIME == 0 ~ "0. 0 min",   ## MEI score
                            LTPA_TIME > 0 & LTPA_TIME < 150 ~ "1. 0-150 min",
                            LTPA_TIME >= 150 ~ "2. >=150 min"))



## Medical Conditions Data
df <- NHANESDATA
medi1999_2018 <- data.frame()
for (i in 1:10) {
  x <- filename[i]
  pathway <- paste0(file_path, "MCQ", x, ".XPT", collapse = NULL, recycle0 = FALSE)
  data <- read_xpt(pathway)
  data <- data[,c("SEQN","MCQ160B","MCQ160C","MCQ160D","MCQ160E","MCQ160F", "MCQ160G", "MCQ160K", "MCQ220")]
  medi1999_2018 <- rbind(medi1999_2018, data)
}
MEDI <- mutate_at(medi1999_2018, vars(starts_with("MCQ")), ~ifelse(. >=7, NA, .)) %>%
  mutate(cvd = case_when(MCQ160B == 1 | MCQ160C == 1 | MCQ160D == 1 | MCQ160E == 1 | MCQ160F == 1 ~ "1. Yes",   ## CVD
                         MCQ160B == 2 & MCQ160C == 2 & MCQ160D == 2 & MCQ160E == 2 & MCQ160F == 2 ~ "0. No"),
         cancer = ifelse(MCQ220 == 1, "1. Yes", "0. No"),   ## Cancer
         cb = ifelse(MCQ160K == 1, "1. Yes", "0. No"),   ## Chronic bronchitis
         emphy = ifelse(MCQ160G == 1, "1. Yes", "0. No"))   ## Emphysema
NHANESDATA <- left_join(df, MEDI, by = "SEQN")



## Sleep Disorders Data
df <- NHANESDATA
slpd1999_2014 <- data.frame()
for (i in 4:8) {
  x <- filename[i]
  pathway <- paste0(file_path, "SLQ", x, ".XPT", collapse = NULL, recycle0 = FALSE)
  data <- read_xpt(pathway)
  data <- data[,c("SEQN","SLD010H","SLQ050")]
  slpd1999_2014 <- rbind(slpd1999_2014, data)
}
names(slpd1999_2014)[2] <- "sleep_times"
slpd2015_2018 <- data.frame()
for (i in 9:10) {
  x <- filename[i]
  pathway <- paste0(file_path, "SLQ", x, ".XPT", collapse = NULL, recycle0 = FALSE)
  data <- read_xpt(pathway)
  data <- data[,c("SEQN","SLD012","SLQ050")]
  slpd2015_2018 <- rbind(slpd2015_2018, data)
}
names(slpd2015_2018)[2] <- "sleep_times"
slpd1999_2018 <- rbind(slpd1999_2014, slpd2015_2018)
NHANESDATA <- left_join(df, slpd1999_2018, by = "SEQN") %>%
  mutate_at(vars(sleep_times), ~ ifelse(. >= 77, NA, .)) %>%
  mutate(sleepdis = case_when(SLQ050 == 1 ~ "1. Yes",
                              SLQ050 == 2 ~ "0. No"))



# Hospital Utilization Data
df <- NHANESDATA
hosp1999_2018 <- data.frame()
for (i in 1:10) {
  x <- filename[i]
  pathway <- paste0(file_path, "HUQ", x, ".XPT", collapse = NULL, recycle0 = FALSE)
  data <- read_xpt(pathway)
  data <- data[,c("SEQN","HUQ010")]
  hosp1999_2018 <- rbind(hosp1999_2018, data)
}
NHANESDATA <- left_join(df, hosp1999_2018, by = "SEQN") %>%
  mutate(self_health = case_when(HUQ010 %in% c(1:2) ~ "1. Excellent",
                                 HUQ010 == 3 ~ "2. Good",
                                 HUQ010 %in% c(4:5) ~ "3. Fair/Poor"))



# Blood Pressure & Cholesterol & Diabetes & BMI Data
df <- NHANESDATA
bpch1999_2018 <- data.frame()
for (i in 1:10) {
  x <- filename[i]
  pathway <- paste0(file_path, "BPQ", x, ".XPT", collapse = NULL, recycle0 = FALSE)
  data <- read_xpt(pathway)
  data <- data[,c("SEQN","BPQ020","BPQ080")]
  bpch1999_2018 <- rbind(bpch1999_2018, data)
}
diab1999_2018 <- data.frame()
for (i in 1:10) {
  x <- filename[i]
  pathway <- paste0(file_path, "DIQ", x, ".XPT", collapse = NULL, recycle0 = FALSE)
  data <- read_xpt(pathway)
  data <- data[,c("SEQN","DIQ010")]
  diab1999_2018 <- rbind(diab1999_2018, data)
}


file_path <- ("/Users/dingyihong/Documents/Database/NHANES/Examination/")
bpx1999_2018 <- data.frame()
for (i in 1:10) {
  x <- filename[i]
  pathway <- paste0(file_path, "BPX", x, ".XPT", collapse = NULL, recycle0 = FALSE)
  data <- read_xpt(pathway)
  data <- select(data, "SEQN", starts_with("BPXSY"), starts_with("BPXDI"))
  bpx1999_2018 <- rbind(bpx1999_2018, data)
}
bmx1999_2018 <- data.frame()
for (i in 1:10) {
  x <- filename[i]
  pathway <- paste0(file_path, "BMX", x, ".XPT", collapse = NULL, recycle0 = FALSE)
  data <- read_xpt(pathway)
  data <- select(data, "SEQN", "BMXWT", "BMXHT")
  bmx1999_2018 <- rbind(bmx1999_2018, data)
}

file_path <- ("/Users/dingyihong/Documents/Database/NHANES/Laboratory/")
tchol1999_2008 <- data.frame()
for (i in 1:10) {
  x <- filename[i]
  pathway <- paste0(file_path, "TCHOL", x, ".XPT", collapse = NULL, recycle0 = FALSE)
  data <- read_xpt(pathway)
  data <- select(data, "SEQN", "LBDTCSI")
  tchol1999_2008 <- rbind(tchol1999_2008, data)
}
glu1999_2018 <- data.frame()
for (i in 1:10) {
  x <- filename[i]
  pathway <- paste0(file_path, "GLU", x, ".XPT", collapse = NULL, recycle0 = FALSE)
  data <- read_xpt(pathway)
  data <- select(data, "SEQN", "LBXGLU")
  glu1999_2018 <- rbind(glu1999_2018, data)
}
NHANESDATA <- left_join(df, bpch1999_2018, by = "SEQN") %>%
  left_join(., diab1999_2018, by = "SEQN") %>%
  left_join(., bpx1999_2018, by = "SEQN") %>%
  left_join(., bmx1999_2018, by = "SEQN") %>%
  left_join(., tchol1999_2008, by = "SEQN") %>%
  left_join(., glu1999_2018, by = "SEQN") %>%
  mutate(n_sbp = rowSums(!is.na(select(., starts_with("BPXSY")))),
         n_dbp = rowSums(!is.na(select(., starts_with("BPXDI"))))) %>%
  mutate_at(vars(starts_with("BPXDI")), list(~na_if(., 0))) %>%
  mutate(mean_sbp = rowMeans(select(., starts_with("BPXSY")), na.rm=TRUE),
         mean_dbp = rowMeans(select(., starts_with("BPXDI")), na.rm=TRUE),
         hypten = case_when(mean_sbp >= 140 | mean_dbp >= 90 | BPQ020 == 1 ~ "1. Yes",   ## Hypertension
                            (n_sbp > 0 & n_dbp > 0) | BPQ020 == 2 ~ "0. No"),
         hypchol = case_when(LBDTCSI >= 6.2 | BPQ080 == 1 ~ "1. Yes",   ## High blood cholesterol
                             LBDTCSI > 0 | BPQ080 == 2 ~ "0. No"),
         diabetes = case_when(LBXGLU > 125 | DIQ010 == 1 ~ "1. Yes",   ## Diabetes
                              LBXGLU > 0 | DIQ010 %in% c(2:3) ~ "0. No"),
         bmi = round(BMXWT / BMXHT**2 * 10000, 2),
         bmi_cat = cut(bmi,   ## BMI
                       breaks = c(-Inf, 25, 30, Inf),
                       right = FALSE,
                       labels = c("Normal", "Overweight", "Obesity")))



## Impute missing data (multiple imputations)
library(mice)
library(VIM)
factorVars <- c("sex", "race", "marriage", "educ", "hhinc",
                "hei_quantiles", "drink", "smoke", "pa_cat", "bmi_cat", "occu",
                "diabetes", "hypten", "hypchol", "cvd", "cancer", "cb", "emphy", "sleepdis", "self_health")
NHANESDATA[ ,factorVars] <- lapply(NHANESDATA[ ,factorVars], factor)
NHANESDATA <- filter(NHANESDATA, age >= 20)   ## 55081
NHANESDATA <- filter(NHANESDATA, preg == 0)   ## 53540
nhanes_cov_ori <- NHANESDATA
nhanes_cov <- NHANESDATA

corVars <- c("age", "sex", "race", "marriage", "educ", "hhinc",
             "hei", "hei_quantiles", "tcal", "drink", "smoke", "pa_cat", "bmi_cat", "occu", "sleep_times",
             "diabetes", "hypten", "hypchol", "cvd", "cancer", "cb", "emphy", "sleepdis", "self_health")
md.pattern(nhanes_cov[, corVars])
mice_plot <- aggr(nhanes_cov[, corVars], col = c('navyblue','yellow'),
                  numbers = TRUE, sortVars = TRUE, prop = FALSE,
                  labels = names(nhanes_cov[, corVars]), cex.axis = .7, cex.numbers = .3,
                  gap = 3, ylab = c("Missing data","Pattern"))

imputed_Data <- mice(nhanes_cov[, corVars], m = 50, maxit = 10, 
                     method = c("", "", "", "polr", "polr", "polr",
                                "pmm", "polr", "pmm", "polr", "polr", "polr", "polr", "logreg", "pmm",
                                "logreg", "logreg", "logreg", "logreg", "logreg", "logreg", "logreg", "logreg", "polr"),
                     seed = 2024)
nhanes_cov[, corVars] <- complete(imputed_Data)
summary(nhanes_cov[, corVars])

file_path <- "/Users/dingyihong/Documents/Database/RDA/"
save(nhanes_cov_ori, nhanes_cov, file = paste0(file_path, "nhanes_cov_1999_2018.rda"))
save(nhanes_cov_ori, nhanes_cov, file = "Rda/nhanes_cov_1999_2018.rda")
