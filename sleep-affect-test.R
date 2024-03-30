source("sleep-affect-data.R")

# Sleep stages including TWT (D = 4) ----------------
## predicting outcomes at wake* ---------------------
cilrw <- compilr(d[Survey == "Wake"], sbp = sbp4, 
                 parts = c("SleepLight", "SleepDeep", "SleepREM", "WAKE"), total = 448)

m_sleep_hapa_adj <- brmcoda(compilr = cilrw,
                            formula = PosAffHALead ~ 
                              bilr1 + bilr2 + bilr3 + 
                              wilr1 + wilr2 + wilr3 + 
                              WPosAffHALeadLag1 +
                              WeekDay +
                              Age + Sex + RACE3G + BMI + SES_1 + 
                              CurrentWork + SmokingStatus + AUDITCat + (1 | ID),
                            cores = 4, backend = "cmdstanr"
)
m_sleep_lapa_adj <- brmcoda(compilr = cilrw,
                            formula = PosAffLALead ~ 
                              bilr1 + bilr2 + bilr3 + 
                              wilr1 + wilr2 + wilr3 + 
                              WPosAffLALeadLag1 +
                              WeekDay +
                              Age + Sex + RACE3G + BMI + SES_1 + 
                              CurrentWork + SmokingStatus + AUDITCat + (1 | ID),
                            cores = 4, backend = "cmdstanr"
)
m_sleep_hana_adj <- brmcoda(compilr = cilrw,
                            formula = NegAffHALead ~ 
                              bilr1 + bilr2 + bilr3 + 
                              wilr1 + wilr2 + wilr3 + 
                              WNegAffHALeadLag1 +
                              WeekDay +
                              Age + Sex + RACE3G + BMI + SES_1 + 
                              CurrentWork + SmokingStatus + AUDITCat + (1 | ID),
                            cores = 4, backend = "cmdstanr"
)
m_sleep_lana_adj <- brmcoda(compilr = cilrw,
                            formula = NegAffLALead ~ 
                              bilr1 + bilr2 + bilr3 + 
                              wilr1 + wilr2 + wilr3 + 
                              WNegAffLALeadLag1 +
                              WeekDay +
                              Age + Sex + RACE3G + BMI + SES_1 + 
                              CurrentWork + SmokingStatus + AUDITCat + (1 | ID),
                            cores = 4, backend = "cmdstanr"
)

plan(multisession, workers = 5)
m_hapa_sub_adj <- substitution(
  m_sleep_hapa_adj,
  delta = c(60),
  level = c("between", "within"),
  ref = "grandmean")
m_lapa_sub_adj <- substitution(
  m_sleep_lapa_adj,
  delta = c(60),
  level = c("between", "within"),
  ref = "grandmean")
m_hana_sub_adj <- substitution(
  m_sleep_hana_adj,
  delta = c(60),
  level = c("between", "within"),
  ref = "grandmean")
m_lana_sub_adj <- substitution(
  m_sleep_lana_adj,
  delta = c(60),
  level = c("between", "within"),
  ref = "grandmean")
plan(sequential)

summary(m_hapa_sub_adj, delta = 60)
summary(m_lapa_sub_adj, delta = 60)
summary(m_hana_sub_adj, delta = 60) #
summary(m_lana_sub_adj, delta = 60) #

## predicting outcomes at morning ---------------------
cilrm <- compilr(d[Survey == "Morning"], sbp = sbp4, 
                 parts = c("SleepLight", "SleepDeep", "SleepREM", "WAKE"), total = 448)

m_sleep_hapa_adj <- brmcoda(compilr = cilrm,
                            formula = PosAffHALead ~ 
                              bilr1 + bilr2 + bilr3 + 
                              wilr1 + wilr2 + wilr3 + 
                              WPosAffHALeadLag2 +
                              WeekDay +
                              Age + Sex + RACE3G + BMI + SES_1 + 
                              CurrentWork + SmokingStatus + AUDITCat + (1 | ID),
                            cores = 4, backend = "cmdstanr"
)
m_sleep_lapa_adj <- brmcoda(compilr = cilrm,
                            formula = PosAffLALead ~ 
                              bilr1 + bilr2 + bilr3 + 
                              wilr1 + wilr2 + wilr3 + 
                              WPosAffLALeadLag2 +
                              WeekDay +
                              Age + Sex + RACE3G + BMI + SES_1 + 
                              CurrentWork + SmokingStatus + AUDITCat + (1 | ID),
                            cores = 4, backend = "cmdstanr"
)
m_sleep_hana_adj <- brmcoda(compilr = cilrm,
                            formula = NegAffHALead ~ 
                              bilr1 + bilr2 + bilr3 + 
                              wilr1 + wilr2 + wilr3 + 
                              WNegAffHALeadLag2 +
                              WeekDay +
                              Age + Sex + RACE3G + BMI + SES_1 + 
                              CurrentWork + SmokingStatus + AUDITCat + (1 | ID),
                            cores = 4, backend = "cmdstanr"
)
m_sleep_lana_adj <- brmcoda(compilr = cilrm,
                            formula = NegAffLALead ~ 
                              bilr1 + bilr2 + bilr3 + 
                              wilr1 + wilr2 + wilr3 + 
                              WNegAffLALeadLag2 +
                              WeekDay +
                              Age + Sex + RACE3G + BMI + SES_1 + 
                              CurrentWork + SmokingStatus + AUDITCat + (1 | ID),
                            cores = 4, backend = "cmdstanr"
)

plan(multisession, workers = 5)
m_hapa_sub_adj <- substitution(
  m_sleep_hapa_adj,
  delta = c(60),
  level = c("between", "within"),
  ref = "grandmean")
m_lapa_sub_adj <- substitution(
  m_sleep_lapa_adj,
  delta = c(60),
  level = c("between", "within"),
  ref = "grandmean")
m_hana_sub_adj <- substitution(
  m_sleep_hana_adj,
  delta = c(60),
  level = c("between", "within"),
  ref = "grandmean")
m_lana_sub_adj <- substitution(
  m_sleep_lana_adj,
  delta = c(60),
  level = c("between", "within"),
  ref = "grandmean")
plan(sequential)

summary(m_hapa_sub_adj, delta = 60) #
summary(m_lapa_sub_adj, delta = 60)
summary(m_hana_sub_adj, delta = 60) #
summary(m_lana_sub_adj, delta = 60) #

## predicting outcomes at afternoon ---------------------
cilra <- compilr(d[Survey == "Afternoon"], sbp = sbp4, 
                 parts = c("SleepLight", "SleepDeep", "SleepREM", "WAKE"), total = 448)

m_sleep_hapa_adj <- brmcoda(compilr = cilra,
                            formula = PosAffHALead ~ 
                              bilr1 + bilr2 + bilr3 + 
                              wilr1 + wilr2 + wilr3 + 
                              # WPosAffHALeadLag2 +
                              WeekDay +
                              Age + Sex + RACE3G + BMI + SES_1 + 
                              CurrentWork + SmokingStatus + AUDITCat + (1 | ID),
                            cores = 4, backend = "cmdstanr"
)
m_sleep_lapa_adj <- brmcoda(compilr = cilra,
                            formula = PosAffLALead ~ 
                              bilr1 + bilr2 + bilr3 + 
                              wilr1 + wilr2 + wilr3 + 
                              # WPosAffLALeadLag2 +
                              WeekDay +
                              Age + Sex + RACE3G + BMI + SES_1 + 
                              CurrentWork + SmokingStatus + AUDITCat + (1 | ID),
                            cores = 4, backend = "cmdstanr"
)
m_sleep_hana_adj <- brmcoda(compilr = cilra,
                            formula = NegAffHALead ~ 
                              bilr1 + bilr2 + bilr3 + 
                              wilr1 + wilr2 + wilr3 + 
                              # WNegAffHALeadLag2 +
                              WeekDay +
                              Age + Sex + RACE3G + BMI + SES_1 + 
                              CurrentWork + SmokingStatus + AUDITCat + (1 | ID),
                            cores = 4, backend = "cmdstanr"
)
m_sleep_lana_adj <- brmcoda(compilr = cilra,
                            formula = NegAffLALead ~ 
                              bilr1 + bilr2 + bilr3 + 
                              wilr1 + wilr2 + wilr3 + 
                              # WNegAffLALeadLag2 +
                              WeekDay +
                              Age + Sex + RACE3G + BMI + SES_1 + 
                              CurrentWork + SmokingStatus + AUDITCat + (1 | ID),
                            cores = 4, backend = "cmdstanr"
)

plan(multisession, workers = 5)
m_hapa_sub_adj <- substitution(
  m_sleep_hapa_adj,
  delta = c(60),
  level = c("between", "within"),
  ref = "grandmean")
m_lapa_sub_adj <- substitution(
  m_sleep_lapa_adj,
  delta = c(60),
  level = c("between", "within"),
  ref = "grandmean")
m_hana_sub_adj <- substitution(
  m_sleep_hana_adj,
  delta = c(60),
  level = c("between", "within"),
  ref = "grandmean")
m_lana_sub_adj <- substitution(
  m_sleep_lana_adj,
  delta = c(60),
  level = c("between", "within"),
  ref = "grandmean")
plan(sequential)

summary(m_hapa_sub_adj, delta = 60)
summary(m_lapa_sub_adj, delta = 60)
summary(m_hana_sub_adj, delta = 60)
summary(m_lana_sub_adj, delta = 60)

## predicting outcomes at evening ---------------------
cilre <- compilr(d[Survey == "Evening"], sbp = sbp4, 
                 parts = c("SleepLight", "SleepDeep", "SleepREM", "WAKE"), total = 448)

m_sleep_hapa_adj <- brmcoda(compilr = cilre,
                            formula = PosAffHALead ~ 
                              bilr1 + bilr2 + bilr3 + 
                              wilr1 + wilr2 + wilr3 + 
                              # WPosAffHALeadLag2 +
                              WeekDay +
                              Age + Sex + RACE3G + BMI + SES_1 + 
                              CurrentWork + SmokingStatus + AUDITCat + (1 | ID),
                            cores = 4, backend = "cmdstanr"
)
m_sleep_lapa_adj <- brmcoda(compilr = cilre,
                            formula = PosAffLALead ~ 
                              bilr1 + bilr2 + bilr3 + 
                              wilr1 + wilr2 + wilr3 + 
                              # WPosAffLALeadLag2 +
                              WeekDay +
                              Age + Sex + RACE3G + BMI + SES_1 + 
                              CurrentWork + SmokingStatus + AUDITCat + (1 | ID),
                            cores = 4, backend = "cmdstanr"
)
m_sleep_hana_adj <- brmcoda(compilr = cilre,
                            formula = NegAffHALead ~ 
                              bilr1 + bilr2 + bilr3 + 
                              wilr1 + wilr2 + wilr3 + 
                              # WNegAffHALeadLag2 +
                              WeekDay +
                              Age + Sex + RACE3G + BMI + SES_1 + 
                              CurrentWork + SmokingStatus + AUDITCat + (1 | ID),
                            cores = 4, backend = "cmdstanr"
)
m_sleep_lana_adj <- brmcoda(compilr = cilre,
                            formula = NegAffLALead ~ 
                              bilr1 + bilr2 + bilr3 + 
                              wilr1 + wilr2 + wilr3 + 
                              # WNegAffLALeadLag2 +
                              WeekDay +
                              Age + Sex + RACE3G + BMI + SES_1 + 
                              CurrentWork + SmokingStatus + AUDITCat + (1 | ID),
                            cores = 4, backend = "cmdstanr"
)

plan(multisession, workers = 5)
m_hapa_sub_adj <- substitution(
  m_sleep_hapa_adj,
  delta = c(60),
  level = c("between", "within"),
  ref = "grandmean")
m_lapa_sub_adj <- substitution(
  m_sleep_lapa_adj,
  delta = c(60),
  level = c("between", "within"),
  ref = "grandmean")
m_hana_sub_adj <- substitution(
  m_sleep_hana_adj,
  delta = c(60),
  level = c("between", "within"),
  ref = "grandmean")
m_lana_sub_adj <- substitution(
  m_sleep_lana_adj,
  delta = c(60),
  level = c("between", "within"),
  ref = "grandmean")
plan(sequential)

summary(m_hapa_sub_adj, delta = 60)
summary(m_lapa_sub_adj, delta = 60)
summary(m_hana_sub_adj, delta = 60)
summary(m_lana_sub_adj, delta = 60)

## predicting outcomes at average next day ---------------------
cilr <- compilr(d, sbp = sbp4, parts = c("SleepLight", "SleepDeep", "SleepREM", "WAKE"), total = 448)

m_sleep_hapa_adj <- brmcoda(compilr = cilr,
                            formula = PosAffHADayLead ~ 
                              bilr1 + bilr2 + bilr3 + 
                              wilr1 + wilr2 + wilr3 + 
                              WPosAffHADay +
                              WeekDay +
                              Age + Sex + RACE3G + BMI + SES_1 + 
                              CurrentWork + SmokingStatus + AUDITCat + (1 | ID),
                            cores = 4, backend = "cmdstanr"
)
m_sleep_lapa_adj <- brmcoda(compilr = cilr,
                            formula = PosAffLADayLead ~ 
                              bilr1 + bilr2 + bilr3 + 
                              wilr1 + wilr2 + wilr3 + 
                              WPosAffLADay +
                              WeekDay +
                              Age + Sex + RACE3G + BMI + SES_1 + 
                              CurrentWork + SmokingStatus + AUDITCat + (1 | ID),
                            cores = 4, backend = "cmdstanr"
)
m_sleep_hana_adj <- brmcoda(compilr = cilr,
                            formula = NegAffHADayLead ~ 
                              bilr1 + bilr2 + bilr3 + 
                              wilr1 + wilr2 + wilr3 + 
                              WNegAffHADay +
                              WeekDay +
                              Age + Sex + RACE3G + BMI + SES_1 + 
                              CurrentWork + SmokingStatus + AUDITCat + (1 | ID),
                            cores = 4, backend = "cmdstanr"
)
m_sleep_lana_adj <- brmcoda(compilr = cilr,
                            formula = NegAffLADayLead ~ 
                              bilr1 + bilr2 + bilr3 + 
                              wilr1 + wilr2 + wilr3 + 
                              WNegAffLADay +
                              WeekDay +
                              Age + Sex + RACE3G + BMI + SES_1 + 
                              CurrentWork + SmokingStatus + AUDITCat + (1 | ID),
                            cores = 4, backend = "cmdstanr"
)

plan(multisession, workers = 5)
m_hapa_sub_adj <- substitution(
  m_sleep_hapa_adj,
  delta = c(60),
  level = c("between", "within"),
  ref = "grandmean")
m_lapa_sub_adj <- substitution(
  m_sleep_lapa_adj,
  delta = c(60),
  level = c("between", "within"),
  ref = "grandmean")
m_hana_sub_adj <- substitution(
  m_sleep_hana_adj,
  delta = c(60),
  level = c("between", "within"),
  ref = "grandmean")
m_lana_sub_adj <- substitution(
  m_sleep_lana_adj,
  delta = c(60),
  level = c("between", "within"),
  ref = "grandmean")
plan(sequential)

summary(m_hapa_sub_adj, delta = 60)
summary(m_lapa_sub_adj, delta = 60)
summary(m_hana_sub_adj, delta = 60)
summary(m_lana_sub_adj, delta = 60)


# Sleep stage excluding TWT (D = 3)  ---------------------
## predicting outcomes at wake ---------------------
cilrw <- compilr(d[Survey == "Wake"], sbp = sbp3, 
                 parts = c("SleepLight", "SleepDeep", "SleepREM"), total = 375)

m_sleep_hapa_adj <- brmcoda(compilr = cilrw,
                            formula = PosAffHALead ~ 
                              bilr1 + bilr2 + 
                              wilr1 + wilr2 + 
                              WPosAffHALeadLag1 +
                              WeekDay +
                              Age + Sex + RACE3G + BMI + SES_1 + 
                              CurrentWork + SmokingStatus + AUDITCat + (1 | ID),
                            cores = 4, backend = "cmdstanr"
)
m_sleep_lapa_adj <- brmcoda(compilr = cilrw,
                            formula = PosAffLALead ~ 
                              bilr1 + bilr2 + 
                              wilr1 + wilr2 + 
                              WPosAffLALeadLag1 +
                              WeekDay +
                              Age + Sex + RACE3G + BMI + SES_1 + 
                              CurrentWork + SmokingStatus + AUDITCat + (1 | ID),
                            cores = 4, backend = "cmdstanr"
)
m_sleep_hana_adj <- brmcoda(compilr = cilrw,
                            formula = NegAffHALead ~ 
                              bilr1 + bilr2 + 
                              wilr1 + wilr2 + 
                              WNegAffHALeadLag1 +
                              WeekDay +
                              Age + Sex + RACE3G + BMI + SES_1 + 
                              CurrentWork + SmokingStatus + AUDITCat + (1 | ID),
                            cores = 4, backend = "cmdstanr"
)
m_sleep_lana_adj <- brmcoda(compilr = cilrw,
                            formula = NegAffLALead ~ 
                              bilr1 + bilr2 + 
                              wilr1 + wilr2 + 
                              WNegAffLALeadLag1 +
                              WeekDay +
                              Age + Sex + RACE3G + BMI + SES_1 + 
                              CurrentWork + SmokingStatus + AUDITCat + (1 | ID),
                            cores = 4, backend = "cmdstanr"
)

plan(multisession, workers = 5)
m_hapa_sub_adj <- substitution(
  m_sleep_hapa_adj,
  delta = c(60),
  level = c("between", "within"),
  ref = "grandmean")
m_lapa_sub_adj <- substitution(
  m_sleep_lapa_adj,
  delta = c(60),
  level = c("between", "within"),
  ref = "grandmean")
m_hana_sub_adj <- substitution(
  m_sleep_hana_adj,
  delta = c(60),
  level = c("between", "within"),
  ref = "grandmean")
m_lana_sub_adj <- substitution(
  m_sleep_lana_adj,
  delta = c(60),
  level = c("between", "within"),
  ref = "grandmean")
plan(sequential)

summary(m_hapa_sub_adj, delta = 60)
summary(m_lapa_sub_adj, delta = 60)
summary(m_hana_sub_adj, delta = 60) #
summary(m_lana_sub_adj, delta = 60) #

## predicting outcomes at morning ---------------------
cilrm <- compilr(d[Survey == "Morning"], sbp = sbp3, 
                 parts = c("SleepLight", "SleepDeep", "SleepREM"), total = 375)

m_sleep_hapa_adj <- brmcoda(compilr = cilrm,
                            formula = PosAffHALead ~ 
                              bilr1 + bilr2 + 
                              wilr1 + wilr2 + 
                              WPosAffHALeadLag2 +
                              WeekDay +
                              Age + Sex + RACE3G + BMI + SES_1 + 
                              CurrentWork + SmokingStatus + AUDITCat + (1 | ID),
                            cores = 4, backend = "cmdstanr"
)
m_sleep_lapa_adj <- brmcoda(compilr = cilrm,
                            formula = PosAffLALead ~ 
                              bilr1 + bilr2 + 
                              wilr1 + wilr2 + 
                              WPosAffLALeadLag2 +
                              WeekDay +
                              Age + Sex + RACE3G + BMI + SES_1 + 
                              CurrentWork + SmokingStatus + AUDITCat + (1 | ID),
                            cores = 4, backend = "cmdstanr"
)
m_sleep_hana_adj <- brmcoda(compilr = cilrm,
                            formula = NegAffHALead ~ 
                              bilr1 + bilr2 + 
                              wilr1 + wilr2 + 
                              WNegAffHALeadLag2 +
                              WeekDay +
                              Age + Sex + RACE3G + BMI + SES_1 + 
                              CurrentWork + SmokingStatus + AUDITCat + (1 | ID),
                            cores = 4, backend = "cmdstanr"
)
m_sleep_lana_adj <- brmcoda(compilr = cilrm,
                            formula = NegAffLALead ~ 
                              bilr1 + bilr2 + 
                              wilr1 + wilr2 + 
                              WNegAffLALeadLag2 +
                              WeekDay +
                              Age + Sex + RACE3G + BMI + SES_1 + 
                              CurrentWork + SmokingStatus + AUDITCat + (1 | ID),
                            cores = 4, backend = "cmdstanr"
)

plan(multisession, workers = 5)
m_hapa_sub_adj <- substitution(
  m_sleep_hapa_adj,
  delta = c(60),
  level = c("between", "within"),
  ref = "grandmean")
m_lapa_sub_adj <- substitution(
  m_sleep_lapa_adj,
  delta = c(60),
  level = c("between", "within"),
  ref = "grandmean")
m_hana_sub_adj <- substitution(
  m_sleep_hana_adj,
  delta = c(60),
  level = c("between", "within"),
  ref = "grandmean")
m_lana_sub_adj <- substitution(
  m_sleep_lana_adj,
  delta = c(60),
  level = c("between", "within"),
  ref = "grandmean")
plan(sequential)

summary(m_hapa_sub_adj, delta = 60) #
summary(m_lapa_sub_adj, delta = 60)
summary(m_hana_sub_adj, delta = 60)
summary(m_lana_sub_adj, delta = 60)

## predicting outcomes at afternoon ---------------------
cilra <- compilr(d[Survey == "Afternoon"], sbp = sbp3, 
                 parts = c("SleepLight", "SleepDeep", "SleepREM"), total = 375)

m_sleep_hapa_adj <- brmcoda(compilr = cilra,
                            formula = PosAffHALead ~ 
                              bilr1 + bilr2 + 
                              wilr1 + wilr2 + 
                              WPosAffHALeadLag2 +
                              WeekDay +
                              Age + Sex + RACE3G + BMI + SES_1 + 
                              CurrentWork + SmokingStatus + AUDITCat + (1 | ID),
                            cores = 4, backend = "cmdstanr"
)
m_sleep_lapa_adj <- brmcoda(compilr = cilra,
                            formula = PosAffLALead ~ 
                              bilr1 + bilr2 + 
                              wilr1 + wilr2 + 
                              WPosAffLALeadLag2 +
                              WeekDay +
                              Age + Sex + RACE3G + BMI + SES_1 + 
                              CurrentWork + SmokingStatus + AUDITCat + (1 | ID),
                            cores = 4, backend = "cmdstanr"
)
m_sleep_hana_adj <- brmcoda(compilr = cilra,
                            formula = NegAffHALead ~ 
                              bilr1 + bilr2 + 
                              wilr1 + wilr2 + 
                              WNegAffHALeadLag2 +
                              WeekDay +
                              Age + Sex + RACE3G + BMI + SES_1 + 
                              CurrentWork + SmokingStatus + AUDITCat + (1 | ID),
                            cores = 4, backend = "cmdstanr"
)
m_sleep_lana_adj <- brmcoda(compilr = cilra,
                            formula = NegAffLALead ~ 
                              bilr1 + bilr2 + 
                              wilr1 + wilr2 + 
                              WNegAffLALeadLag2 +
                              WeekDay +
                              Age + Sex + RACE3G + BMI + SES_1 + 
                              CurrentWork + SmokingStatus + AUDITCat + (1 | ID),
                            cores = 4, backend = "cmdstanr"
)

plan(multisession, workers = 5)
m_hapa_sub_adj <- substitution(
  m_sleep_hapa_adj,
  delta = c(60),
  level = c("between", "within"),
  ref = "grandmean")
m_lapa_sub_adj <- substitution(
  m_sleep_lapa_adj,
  delta = c(60),
  level = c("between", "within"),
  ref = "grandmean")
m_hana_sub_adj <- substitution(
  m_sleep_hana_adj,
  delta = c(60),
  level = c("between", "within"),
  ref = "grandmean")
m_lana_sub_adj <- substitution(
  m_sleep_lana_adj,
  delta = c(60),
  level = c("between", "within"),
  ref = "grandmean")
plan(sequential)

summary(m_hapa_sub_adj, delta = 60) #
summary(m_lapa_sub_adj, delta = 60) #
summary(m_hana_sub_adj, delta = 60)
summary(m_lana_sub_adj, delta = 60)

## predicting outcomes at evening ---------------------
cilre <- compilr(d[Survey == "Evening"], sbp = sbp3, 
                 parts = c("SleepLight", "SleepDeep", "SleepREM"), total = 375)

m_sleep_hapa_adj <- brmcoda(compilr = cilre,
                            formula = PosAffHALead ~ 
                              bilr1 + bilr2 + 
                              wilr1 + wilr2 + 
                              WPosAffHALeadLag2 +
                              WeekDay +
                              Age + Sex + RACE3G + BMI + SES_1 + 
                              CurrentWork + SmokingStatus + AUDITCat + (1 | ID),
                            cores = 4, backend = "cmdstanr"
)
m_sleep_lapa_adj <- brmcoda(compilr = cilre,
                            formula = PosAffLALead ~ 
                              bilr1 + bilr2 + 
                              wilr1 + wilr2 + 
                              WPosAffLALeadLag2 +
                              WeekDay +
                              Age + Sex + RACE3G + BMI + SES_1 + 
                              CurrentWork + SmokingStatus + AUDITCat + (1 | ID),
                            cores = 4, backend = "cmdstanr"
)
m_sleep_hana_adj <- brmcoda(compilr = cilre,
                            formula = NegAffHALead ~ 
                              bilr1 + bilr2 + 
                              wilr1 + wilr2 + 
                              WNegAffHALeadLag2 +
                              WeekDay +
                              Age + Sex + RACE3G + BMI + SES_1 + 
                              CurrentWork + SmokingStatus + AUDITCat + (1 | ID),
                            cores = 4, backend = "cmdstanr"
)
m_sleep_lana_adj <- brmcoda(compilr = cilre,
                            formula = NegAffLALead ~ 
                              bilr1 + bilr2 + 
                              wilr1 + wilr2 + 
                              WNegAffLALeadLag2 +
                              WeekDay +
                              Age + Sex + RACE3G + BMI + SES_1 + 
                              CurrentWork + SmokingStatus + AUDITCat + (1 | ID),
                            cores = 4, backend = "cmdstanr"
)

plan(multisession, workers = 5)
m_hapa_sub_adj <- substitution(
  m_sleep_hapa_adj,
  delta = c(60),
  level = c("between", "within"),
  ref = "grandmean")
m_lapa_sub_adj <- substitution(
  m_sleep_lapa_adj,
  delta = c(60),
  level = c("between", "within"),
  ref = "grandmean")
m_hana_sub_adj <- substitution(
  m_sleep_hana_adj,
  delta = c(60),
  level = c("between", "within"),
  ref = "grandmean")
m_lana_sub_adj <- substitution(
  m_sleep_lana_adj,
  delta = c(60),
  level = c("between", "within"),
  ref = "grandmean")
plan(sequential)

summary(m_hapa_sub_adj, delta = 60)
summary(m_lapa_sub_adj, delta = 60)
summary(m_hana_sub_adj, delta = 60) #
summary(m_lana_sub_adj, delta = 60)

## predicting outcomes at average next day ---------------------
cilr <- compilr(d, sbp = sbp3, 
                parts = c("SleepLight", "SleepDeep", "SleepREM"), total = 375)

m_sleep_hapa_adj <- brmcoda(compilr = cilr,
                            formula = PosAffHADayLead ~ 
                              bilr1 + bilr2 + 
                              wilr1 + wilr2 + 
                              WPosAffHADay +
                              WeekDay +
                              Age + Sex + RACE3G + BMI + SES_1 + 
                              CurrentWork + SmokingStatus + AUDITCat + (1 | ID),
                            cores = 4, backend = "cmdstanr"
)
m_sleep_lapa_adj <- brmcoda(compilr = cilr,
                            formula = PosAffLADayLead ~ 
                              bilr1 + bilr2 + 
                              wilr1 + wilr2 + 
                              WPosAffLADay +
                              WeekDay +
                              Age + Sex + RACE3G + BMI + SES_1 + 
                              CurrentWork + SmokingStatus + AUDITCat + (1 | ID),
                            cores = 4, backend = "cmdstanr"
)
m_sleep_hana_adj <- brmcoda(compilr = cilr,
                            formula = NegAffHADayLead ~ 
                              bilr1 + bilr2 + 
                              wilr1 + wilr2 + 
                              WNegAffHADay +
                              WeekDay +
                              Age + Sex + RACE3G + BMI + SES_1 + 
                              CurrentWork + SmokingStatus + AUDITCat + (1 | ID),
                            cores = 4, backend = "cmdstanr"
)
m_sleep_lana_adj <- brmcoda(compilr = cilr,
                            formula = NegAffLADayLead ~ 
                              bilr1 + bilr2 + 
                              wilr1 + wilr2 + 
                              WNegAffLADay +
                              WeekDay +
                              Age + Sex + RACE3G + BMI + SES_1 + 
                              CurrentWork + SmokingStatus + AUDITCat + (1 | ID),
                            cores = 4, backend = "cmdstanr"
)

plan(multisession, workers = 5)
m_hapa_sub_adj <- substitution(
  m_sleep_hapa_adj,
  delta = c(60),
  level = c("between", "within"),
  ref = "grandmean")
m_lapa_sub_adj <- substitution(
  m_sleep_lapa_adj,
  delta = c(60),
  level = c("between", "within"),
  ref = "grandmean")
m_hana_sub_adj <- substitution(
  m_sleep_hana_adj,
  delta = c(60),
  level = c("between", "within"),
  ref = "grandmean")
m_lana_sub_adj <- substitution(
  m_sleep_lana_adj,
  delta = c(60),
  level = c("between", "within"),
  ref = "grandmean")
plan(sequential)

summary(m_hapa_sub_adj, delta = 60)
summary(m_lapa_sub_adj, delta = 60) #
summary(m_hana_sub_adj, delta = 60)
summary(m_lana_sub_adj, delta = 60)

# TST - TWT only (D = 2) -----------------
## predicting outcomes at wake --------------------
cilrw <- compilr(d[Survey == "Wake"], sbp = sbp2, 
                parts = c("Sleep", "WAKE"), total = 448)

m_sleep_hapa_adj <- brmcoda(compilr = cilrw,
                            formula = PosAffHALead ~ 
                              bilr1 +
                              wilr1 +
                              WPosAffHALeadLag1 +
                              WeekDay +
                              Age + Sex + RACE3G + BMI + SES_1 + 
                              CurrentWork + SmokingStatus + AUDITCat + (1 | ID),
                            cores = 4, backend = "cmdstanr"
)
m_sleep_lapa_adj <- brmcoda(compilr = cilrw,
                            formula = PosAffLALead ~ 
                              bilr1 +
                              wilr1 +
                              WPosAffLALeadLag1 +
                              WeekDay +
                              Age + Sex + RACE3G + BMI + SES_1 + 
                              CurrentWork + SmokingStatus + AUDITCat + (1 | ID),
                            cores = 4, backend = "cmdstanr"
)
m_sleep_hana_adj <- brmcoda(compilr = cilrw,
                            formula = NegAffHALead ~ 
                              bilr1 +
                              wilr1 +
                              WNegAffHALeadLag1 +
                              WeekDay +
                              Age + Sex + RACE3G + BMI + SES_1 + 
                              CurrentWork + SmokingStatus + AUDITCat + (1 | ID),
                            cores = 4, backend = "cmdstanr"
)
m_sleep_lana_adj <- brmcoda(compilr = cilrw,
                            formula = NegAffLALead ~ 
                              bilr1 +
                              wilr1 +
                              WNegAffLALeadLag1 +
                              WeekDay +
                              Age + Sex + RACE3G + BMI + SES_1 + 
                              CurrentWork + SmokingStatus + AUDITCat + (1 | ID),
                            cores = 4, backend = "cmdstanr"
)

plan(multisession, workers = 2)
m_hapa_sub_adj <- substitution(
  m_sleep_hapa_adj,
  delta = c(60),
  level = c("between", "within"),
  ref = "grandmean")
m_lapa_sub_adj <- substitution(
  m_sleep_lapa_adj,
  delta = c(60),
  level = c("between", "within"),
  ref = "grandmean")
m_hana_sub_adj <- substitution(
  m_sleep_hana_adj,
  delta = c(60),
  level = c("between", "within"),
  ref = "grandmean")
m_lana_sub_adj <- substitution(
  m_sleep_lana_adj,
  delta = c(60),
  level = c("between", "within"),
  ref = "grandmean")
plan(sequential)

summary(m_hapa_sub_adj, delta = 60)
summary(m_lapa_sub_adj, delta = 60)
summary(m_hana_sub_adj, delta = 60) #
summary(m_lana_sub_adj, delta = 60) #

## predicting outcomes at morning ---------------------
cilrm <- compilr(d[Survey == "Morning"], sbp = sbp2, 
                 parts = c("Sleep", "WAKE"), total = 448)

m_sleep_hapa_adj <- brmcoda(compilr = cilrm,
                            formula = PosAffHALead ~ 
                              bilr1 +
                              wilr1 +
                              WPosAffHALeadLag2 +
                              WeekDay +
                              Age + Sex + RACE3G + BMI + SES_1 + 
                              CurrentWork + SmokingStatus + AUDITCat + (1 | ID),
                            cores = 4, backend = "cmdstanr"
)
m_sleep_lapa_adj <- brmcoda(compilr = cilrm,
                            formula = PosAffLALead ~ 
                              bilr1 +
                              wilr1 +
                              WPosAffLALeadLag2 +
                              WeekDay +
                              Age + Sex + RACE3G + BMI + SES_1 + 
                              CurrentWork + SmokingStatus + AUDITCat + (1 | ID),
                            cores = 4, backend = "cmdstanr"
)
m_sleep_hana_adj <- brmcoda(compilr = cilrm,
                            formula = NegAffHALead ~ 
                              bilr1 +
                              wilr1 +
                              WNegAffHALeadLag2 +
                              WeekDay +
                              Age + Sex + RACE3G + BMI + SES_1 + 
                              CurrentWork + SmokingStatus + AUDITCat + (1 | ID),
                            cores = 4, backend = "cmdstanr"
)
m_sleep_lana_adj <- brmcoda(compilr = cilrm,
                            formula = NegAffLALead ~ 
                              bilr1 +
                              wilr1 +
                              WNegAffLALeadLag2 +
                              WeekDay +
                              Age + Sex + RACE3G + BMI + SES_1 + 
                              CurrentWork + SmokingStatus + AUDITCat + (1 | ID),
                            cores = 4, backend = "cmdstanr"
)

plan(multisession, workers = 2)
m_hapa_sub_adj <- substitution(
  m_sleep_hapa_adj,
  delta = c(60),
  level = c("between", "within"),
  ref = "grandmean")
m_lapa_sub_adj <- substitution(
  m_sleep_lapa_adj,
  delta = c(60),
  level = c("between", "within"),
  ref = "grandmean")
m_hana_sub_adj <- substitution(
  m_sleep_hana_adj,
  delta = c(60),
  level = c("between", "within"),
  ref = "grandmean")
m_lana_sub_adj <- substitution(
  m_sleep_lana_adj,
  delta = c(60),
  level = c("between", "within"),
  ref = "grandmean")
plan(sequential)

summary(m_hapa_sub_adj, delta = 60)
summary(m_lapa_sub_adj, delta = 60)
summary(m_hana_sub_adj, delta = 60) #
summary(m_lana_sub_adj, delta = 60) #

## predicting outcomes at afternoon ---------------------
cilra <- compilr(d[Survey == "Afternoon"], sbp = sbp2, 
                 parts = c("Sleep", "WAKE"), total = 448)

m_sleep_hapa_adj <- brmcoda(compilr = cilra,
                            formula = PosAffHALead ~ 
                              bilr1 +
                              wilr1 +
                              WPosAffHALeadLag2 +
                              WeekDay +
                              Age + Sex + RACE3G + BMI + SES_1 + 
                              CurrentWork + SmokingStatus + AUDITCat + (1 | ID),
                            cores = 4, backend = "cmdstanr"
)
m_sleep_lapa_adj <- brmcoda(compilr = cilra,
                            formula = PosAffLALead ~ 
                              bilr1 +
                              wilr1 +
                              WPosAffLALeadLag2 +
                              WeekDay +
                              Age + Sex + RACE3G + BMI + SES_1 + 
                              CurrentWork + SmokingStatus + AUDITCat + (1 | ID),
                            cores = 4, backend = "cmdstanr"
)
m_sleep_hana_adj <- brmcoda(compilr = cilra,
                            formula = NegAffHALead ~ 
                              bilr1 +
                              wilr1 +
                              WNegAffHALeadLag2 +
                              WeekDay +
                              Age + Sex + RACE3G + BMI + SES_1 + 
                              CurrentWork + SmokingStatus + AUDITCat + (1 | ID),
                            cores = 4, backend = "cmdstanr"
)
m_sleep_lana_adj <- brmcoda(compilr = cilra,
                            formula = NegAffLALead ~ 
                              bilr1 +
                              wilr1 +
                              WNegAffLALeadLag2 +
                              WeekDay +
                              Age + Sex + RACE3G + BMI + SES_1 + 
                              CurrentWork + SmokingStatus + AUDITCat + (1 | ID),
                            cores = 4, backend = "cmdstanr"
)

plan(multisession, workers = 5)
m_hapa_sub_adj <- substitution(
  m_sleep_hapa_adj,
  delta = c(60),
  level = c("between", "within"),
  ref = "grandmean")
m_lapa_sub_adj <- substitution(
  m_sleep_lapa_adj,
  delta = c(60),
  level = c("between", "within"),
  ref = "grandmean")
m_hana_sub_adj <- substitution(
  m_sleep_hana_adj,
  delta = c(60),
  level = c("between", "within"),
  ref = "grandmean")
m_lana_sub_adj <- substitution(
  m_sleep_lana_adj,
  delta = c(60),
  level = c("between", "within"),
  ref = "grandmean")
plan(sequential)

summary(m_hapa_sub_adj, delta = 60)
summary(m_lapa_sub_adj, delta = 60)
summary(m_hana_sub_adj, delta = 60) #
summary(m_lana_sub_adj, delta = 60) 

## predicting outcomes at evening ---------------------
cilre <- compilr(d[Survey == "Evening"], sbp = sbp2, 
                 parts = c("Sleep", "WAKE"), total = 448)

m_sleep_hapa_adj <- brmcoda(compilr = cilre,
                            formula = PosAffHALead ~ 
                              bilr1 +
                              wilr1 +
                              WPosAffHALeadLag2 +
                              WeekDay +
                              Age + Sex + RACE3G + BMI + SES_1 + 
                              CurrentWork + SmokingStatus + AUDITCat + (1 | ID),
                            cores = 4, backend = "cmdstanr"
)
m_sleep_lapa_adj <- brmcoda(compilr = cilre,
                            formula = PosAffLALead ~ 
                              bilr1 +
                              wilr1 +
                              WPosAffLALeadLag2 +
                              WeekDay +
                              Age + Sex + RACE3G + BMI + SES_1 + 
                              CurrentWork + SmokingStatus + AUDITCat + (1 | ID),
                            cores = 4, backend = "cmdstanr"
)
m_sleep_hana_adj <- brmcoda(compilr = cilre,
                            formula = NegAffHALead ~ 
                              bilr1 +
                              wilr1 +
                              WNegAffHALeadLag2 +
                              WeekDay +
                              Age + Sex + RACE3G + BMI + SES_1 + 
                              CurrentWork + SmokingStatus + AUDITCat + (1 | ID),
                            cores = 4, backend = "cmdstanr"
)
m_sleep_lana_adj <- brmcoda(compilr = cilre,
                            formula = NegAffLALead ~ 
                              bilr1 +
                              wilr1 +
                              WNegAffLALeadLag2 +
                              WeekDay +
                              Age + Sex + RACE3G + BMI + SES_1 + 
                              CurrentWork + SmokingStatus + AUDITCat + (1 | ID),
                            cores = 4, backend = "cmdstanr"
)

plan(multisession, workers = 5)
m_hapa_sub_adj <- substitution(
  m_sleep_hapa_adj,
  delta = c(60),
  level = c("between", "within"),
  ref = "grandmean")
m_lapa_sub_adj <- substitution(
  m_sleep_lapa_adj,
  delta = c(60),
  level = c("between", "within"),
  ref = "grandmean")
m_hana_sub_adj <- substitution(
  m_sleep_hana_adj,
  delta = c(60),
  level = c("between", "within"),
  ref = "grandmean")
m_lana_sub_adj <- substitution(
  m_sleep_lana_adj,
  delta = c(60),
  level = c("between", "within"),
  ref = "grandmean")
plan(sequential)

summary(m_hapa_sub_adj, delta = 60)
summary(m_lapa_sub_adj, delta = 60)
summary(m_hana_sub_adj, delta = 60) 
summary(m_lana_sub_adj, delta = 60) 

## predicting outcomes at average next day ---------------------
cilr <- compilr(d, sbp = sbp2, 
                parts = c("Sleep", "WAKE"), total = 448)

m_sleep_hapa_adj <- brmcoda(compilr = cilr,
                            formula = PosAffHADayLead ~ 
                              bilr1 + 
                              wilr1 + 
                              WPosAffHADay +
                              WeekDay +
                              Age + Sex + RACE3G + BMI + SES_1 + 
                              CurrentWork + SmokingStatus + AUDITCat + (1 | ID),
                            cores = 4, backend = "cmdstanr"
)
m_sleep_lapa_adj <- brmcoda(compilr = cilr,
                            formula = PosAffLADayLead ~ 
                              bilr1 + 
                              wilr1 + 
                              WPosAffLADay +
                              WeekDay +
                              Age + Sex + RACE3G + BMI + SES_1 + 
                              CurrentWork + SmokingStatus + AUDITCat + (1 | ID),
                            cores = 4, backend = "cmdstanr"
)
m_sleep_hana_adj <- brmcoda(compilr = cilr,
                            formula = NegAffHADayLead ~ 
                              bilr1 + 
                              wilr1 + 
                              WNegAffHADay +
                              WeekDay +
                              Age + Sex + RACE3G + BMI + SES_1 + 
                              CurrentWork + SmokingStatus + AUDITCat + (1 | ID),
                            cores = 4, backend = "cmdstanr"
)
m_sleep_lana_adj <- brmcoda(compilr = cilr,
                            formula = NegAffLADayLead ~ 
                              bilr1 + 
                              wilr1 +
                              WNegAffLADay +
                              WeekDay +
                              Age + Sex + RACE3G + BMI + SES_1 + 
                              CurrentWork + SmokingStatus + AUDITCat + (1 | ID),
                            cores = 4, backend = "cmdstanr"
)

plan(multisession, workers = 5)
m_hapa_sub_adj <- substitution(
  m_sleep_hapa_adj,
  delta = c(60),
  level = c("between", "within"),
  ref = "grandmean")
m_lapa_sub_adj <- substitution(
  m_sleep_lapa_adj,
  delta = c(60),
  level = c("between", "within"),
  ref = "grandmean")
m_hana_sub_adj <- substitution(
  m_sleep_hana_adj,
  delta = c(60),
  level = c("between", "within"),
  ref = "grandmean")
m_lana_sub_adj <- substitution(
  m_sleep_lana_adj,
  delta = c(60),
  level = c("between", "within"),
  ref = "grandmean")
plan(sequential)

summary(m_hapa_sub_adj, delta = 60)
summary(m_lapa_sub_adj, delta = 60)
summary(m_hana_sub_adj, delta = 60) 
summary(m_lana_sub_adj, delta = 60) 

# test if time of the day is a moderator ----------------
cilr <- compilr(d, sbp = sbp3, parts = c("SleepLight", "SleepDeep", "SleepREM"), total = 448)

m1 <- brmcoda(compilr = cilr,
              formula = PosAffHALead ~ 
                (bilr1 + bilr2 + 
                   wilr1 + wilr2)*Survey + 
                WPosAffHALeadLag2 +
                WeekDay +
                Age + Sex + RACE3G + BMI + SES_1 + 
                CurrentWork + SmokingStatus + AUDITCat + (1 | ID),
              cores = 4, backend = "cmdstanr",
              save_pars = save_pars(all = TRUE)
)
m2 <- brmcoda(compilr = cilr,
              formula = PosAffLALead ~ 
                (bilr1 + bilr2 + 
                   wilr1 + wilr2)*Survey + 
                WPosAffLALeadLag2 +
                WeekDay +
                Age + Sex + RACE3G + BMI + SES_1 + 
                CurrentWork + SmokingStatus + AUDITCat + (1 | ID),
              cores = 4, backend = "cmdstanr",
              save_pars = save_pars(all = TRUE)
)
m3 <- brmcoda(compilr = cilr,
              formula = NegAffHALead ~ 
                (bilr1 + bilr2 + 
                   wilr1 + wilr2)*Survey + 
                WNegAffHALeadLag2 +
                WeekDay +
                Age + Sex + RACE3G + BMI + SES_1 + 
                CurrentWork + SmokingStatus + AUDITCat + (1 | ID),
              cores = 4, backend = "cmdstanr",
              save_pars = save_pars(all = TRUE)
)
m4 <- brmcoda(compilr = cilr,
              formula = NegAffLALead ~ 
                (bilr1 + bilr2 +
                   wilr1 + wilr2)*Survey + 
                WNegAffLALeadLag2 +
                WeekDay +
                Age + Sex + RACE3G + BMI + SES_1 + 
                CurrentWork + SmokingStatus + AUDITCat + (1 | ID),
              cores = 4, backend = "cmdstanr",
              save_pars = save_pars(all = TRUE)
)

m5 <- brmcoda(compilr = cilr,
              formula = PosAffHALead ~ 
                bilr1 + bilr2 + 
                wilr1 + wilr2 + Survey + 
                WPosAffHALeadLag2 +
                WeekDay +
                Age + Sex + RACE3G + BMI + SES_1 + 
                CurrentWork + SmokingStatus + AUDITCat + (1 | ID),
              cores = 4, backend = "cmdstanr",
              save_pars = save_pars(all = TRUE)
)
m6 <- brmcoda(compilr = cilr,
              formula = PosAffLALead ~ 
                bilr1 + bilr2 + 
                wilr1 + wilr2 +  Survey + 
                WPosAffLALeadLag2 +
                WeekDay +
                Age + Sex + RACE3G + BMI + SES_1 + 
                CurrentWork + SmokingStatus + AUDITCat + (1 | ID),
              cores = 4, backend = "cmdstanr",
              save_pars = save_pars(all = TRUE)
)
m7 <- brmcoda(compilr = cilr,
              formula = NegAffHALead ~ 
                bilr1 + bilr2 +
                wilr1 + wilr2 + Survey + 
                WNegAffHALeadLag2 +
                WeekDay +
                Age + Sex + RACE3G + BMI + SES_1 + 
                CurrentWork + SmokingStatus + AUDITCat + (1 | ID),
              cores = 4, backend = "cmdstanr",
              save_pars = save_pars(all = TRUE)
)
m8 <- brmcoda(compilr = cilr,
              formula = NegAffLALead ~ 
                bilr1 + bilr2 + 
                wilr1 + wilr2 + Survey + 
                WNegAffLALeadLag2 +
                WeekDay +
                Age + Sex + RACE3G + BMI + SES_1 + 
                CurrentWork + SmokingStatus + AUDITCat + (1 | ID),
              cores = 4, backend = "cmdstanr",
              save_pars = save_pars(all = TRUE)
)
summary(m1)
bayes_factor(m1, m5) #Estimated Bayes factor in favor of m1 over m5 : 0.0006677345
bayes_factor(m2, m6) #Estimated Bayes factor in favor of m2 over m6 : 0.02254922
bayes_factor(m3, m7) #Estimated Bayes factor in favor of m3 over m7 : 5.059045e-07
bayes_factor(m4, m8) #Estimated Bayes factor in favor of m4 over m8 : 0.0006315389

plan(multisession, workers = 5)
subm1 <- substitution(
  m1,
  delta = c(60),
  level = c("between", "within"),
  ref = "grandmean",
  summary = FALSE)
subm2 <- substitution(
  m2,
  delta = c(60),
  level = c("between", "within"),
  ref = "grandmean",
  summary = FALSE)
subm3 <- substitution(
  m3,
  delta = c(60),
  level = c("between", "within"),
  ref = "grandmean",
  summary = FALSE)
subm4 <- substitution(
  m4,
  delta = c(60),
  level = c("between", "within"),
  ref = "grandmean",
  summary = FALSE)
plan(sequential)

summary(subm1, delta = 60)
summary(subm2, delta = 60)
summary(subm3, delta = 60)
summary(subm4, delta = 60)






# Predicting change in outcome overnight

# predicting average wake and morning
cilr <- compilr(d[Survey == "Morning"], sbp = sbp4, parts = c("SleepLight", "SleepDeep", "SleepREM", "WAKE"), total = 448)

m_sleep_hapa_adj <- brmcoda(compilr = cilr,
                            formula = PosAffHA_HalfDayLead ~ 
                              bilr1 + bilr2 + bilr3 + 
                              wilr1 + wilr2 + wilr3 + 
                              WPosAffHA_HalfDayLeadLag +
                              WeekDay +
                              Age + Sex + RACE3G + BMI + SES_1 + 
                              CurrentWork + SmokingStatus + AUDITCat + (1 | ID),
                            cores = 4, backend = "cmdstanr"
)
m_sleep_lapa_adj <- brmcoda(compilr = cilr,
                            formula = PosAffLA_HalfDayLead ~ 
                              bilr1 + bilr2 + bilr3 + 
                              wilr1 + wilr2 + wilr3 + 
                              WPosAffLA_HalfDayLeadLag +
                              WeekDay +
                              Age + Sex + RACE3G + BMI + SES_1 + 
                              CurrentWork + SmokingStatus + AUDITCat + (1 | ID),
                            cores = 4, backend = "cmdstanr"
)
m_sleep_hana_adj <- brmcoda(compilr = cilr,
                            formula = NegAffHA_HalfDayLead ~ 
                              bilr1 + bilr2 + bilr3 + 
                              wilr1 + wilr2 + wilr3 + 
                              WNegAffHA_HalfDayLeadLag +
                              WeekDay +
                              Age + Sex + RACE3G + BMI + SES_1 + 
                              CurrentWork + SmokingStatus + AUDITCat + (1 | ID),
                            cores = 4, backend = "cmdstanr"
)
m_sleep_lana_adj <- brmcoda(compilr = cilr,
                            formula = NegAffLA_HalfDayLead ~ 
                              bilr1 + bilr2 + bilr3 + 
                              wilr1 + wilr2 + wilr3 + 
                              WNegAffLA_HalfDayLeadLag +
                              WeekDay +
                              Age + Sex + RACE3G + BMI + SES_1 + 
                              CurrentWork + SmokingStatus + AUDITCat + (1 | ID),
                            cores = 4, backend = "cmdstanr"
)

plan(multisession, workers = 5)
m_hapa_sub_adj <- substitution(
  m_sleep_hapa_adj,
  delta = c(60),
  level = c("between", "within"),
  ref = "grandmean")
m_lapa_sub_adj <- substitution(
  m_sleep_lapa_adj,
  delta = c(60),
  level = c("between", "within"),
  ref = "grandmean")
m_hana_sub_adj <- substitution(
  m_sleep_hana_adj,
  delta = c(60),
  level = c("between", "within"),
  ref = "grandmean")
m_lana_sub_adj <- substitution(
  m_sleep_lana_adj,
  delta = c(60),
  level = c("between", "within"),
  ref = "grandmean")
plan(sequential)

summary(m_hapa_sub_adj, delta = 60)
summary(m_lapa_sub_adj, delta = 60)
summary(m_hana_sub_adj, delta = 60)
summary(m_lana_sub_adj, delta = 60)


## D4 predicting outcomes at wake modereated by TIB ---------------------
cilrw <- compilr(d[Survey == "Wake"], sbp = sbp4, 
                 parts = c("SleepLight", "SleepDeep", "SleepREM", "WAKE"), total = 448)

m_sleep_hapa_adj <- brmcoda(compilr = cilrw_hapa,
                            formula = PosAffHALead ~ 
                              (bilr1 + bilr2 + bilr3)*BTIBz +
                              (wilr1 + wilr2 + wilr3)*WTIBz + 
                              WPosAffHALeadLag1 +
                              WeekDay + CPDzc18 +
                              Age + Sex + RACE3G + BMI + SES_1 +
                              # CurrentWork + SmokingStatus + AUDITCat + 
                              (1 + WPosAffHALeadLag1 + wilr1 + wilr2 + wilr3 + WTIBz| ID),
                            iter = 6000, chains = 8, cores = 8, seed = 123, warmup = 1000,
                            backend = "cmdstanr", save_pars = save_pars(all = TRUE))

m_sleep_lapa_adj <- brmcoda(compilr = cilrw,
                            formula = PosAffLALead ~ 
                              (bilr1 + bilr2 + bilr3)*BTIBz +
                              (wilr1 + wilr2 + wilr3)*WTIBz + 
                              WPosAffLALeadLag1 +
                              WeekDay + CPDzc18 +
                              Age + Sex + RACE3G + BMI + SES_1 +
                              # CurrentWork + SmokingStatus + AUDITCat + 
                              (1 + WPosAffLALeadLag1 + wilr1 + wilr2 + wilr3 + WTIBz| ID),
                            iter = 6000, chains = 8, cores = 8, seed = 123, warmup = 1000,
                            backend = "cmdstanr", save_pars = save_pars(all = TRUE))

m_sleep_hana_adj <- brmcoda(compilr = cilrw,
                            formula = NegAffHALead ~ 
                              (bilr1 + bilr2 + bilr3)*BTIBz +
                              (wilr1 + wilr2 + wilr3)*WTIBz + 
                              WNegAffHALeadLag1 +
                              WeekDay + CPDzc18 +
                              Age + Sex + RACE3G + BMI + SES_1 +
                              # CurrentWork + SmokingStatus + AUDITCat + 
                              (1 + WNegAffHALeadLag1 + wilr1 + wilr2 + wilr3 + WTIBz| ID),
                            iter = 6000, chains = 8, cores = 8, seed = 123, warmup = 1000,
                            backend = "cmdstanr", save_pars = save_pars(all = TRUE))

m_sleep_lana_adj <- brmcoda(compilr = cilrw,
                            formula = NegAffLALead ~ 
                              (bilr1 + bilr2 + bilr3)*BTIBz +
                              (wilr1 + wilr2 + wilr3)*WTIBz + 
                              WNegAffLALeadLag1 +
                              WeekDay + CPDzc18 +
                              Age + Sex + RACE3G + BMI + SES_1 +
                              # CurrentWork + SmokingStatus + AUDITCat + 
                              (1 + WNegAffLALeadLag1 + wilr1 + wilr2 + wilr3 + WTIBz| ID),
                            iter = 6000, chains = 8, cores = 8, seed = 123, warmup = 1000,
                            backend = "cmdstanr", save_pars = save_pars(all = TRUE))

object <- m_sleep_hapa_adj
object <- m_sleep_lapa_adj
object <- m_sleep_hana_adj
object <- m_sleep_lana_adj

comp0 <- object$CompILR$BetweenComp
d0 <- cbind(object$CompILR$BetweenComp, object$CompILR$data)
d0 <- d0[, head(.SD, 1), by = eval(object$CompILR$idvar)]
comp0 <- acomp(d0[, colnames(object$CompILR$BetweenComp), with = FALSE], total = object$CompILR$total)
mcomp <- mean.acomp(comp0, robust = TRUE)
mcomp <- acomp(mcomp, total = object$CompILR$total)
mcomp <- as.data.table(t(mcomp))
bilr0 <- ilr(mcomp, V = object$CompILR$psi)
bilr0 <- as.data.table(t(bilr0))
colnames(bilr0)  <- colnames(object$CompILR$BetweenILR)

wilr0 <- as.data.table(matrix(0, nrow = nrow(bilr0), ncol = ncol(bilr0)))
colnames(wilr0)  <- colnames(object$CompILR$WithinILR)

BTIBz <- c(mean(model.frame(object)$BTIBz) - sd(model.frame(object)$BTIBz), 
           mean(model.frame(object)$BTIBz), 
           mean(model.frame(object)$BTIBz) + sd(model.frame(object)$BTIBz))
WTIBz <- c(mean(model.frame(object)$WTIBz) - sd(model.frame(object)$WTIBz), 
           mean(model.frame(object)$WTIBz), 
           mean(model.frame(object)$WTIBz) + sd(model.frame(object)$WTIBz))

WTIBz0 <- c(mean(model.frame(object)$WTIBz))
BTIBz0 <- c(mean(model.frame(object)$BTIBz))

WPosAffHALeadLag1 <- mean(model.frame(object)$WPosAffHALeadLag1)
# WPosAffLALeadLag1 <- mean(model.frame(object)$WPosAffLALeadLag1)
# WNegAffHALeadLag1 <- mean(model.frame(object)$WNegAffHALeadLag1)
# WNegAffLALeadLag1 <- mean(model.frame(object)$WNegAffLALeadLag1)

CPDzc18 <- mean(model.frame(object)$CPDzc18)
Age <- mean(model.frame(object)$Age)
BMI <- mean(model.frame(object)$BMI)
SES_1 <- mean(model.frame(object)$SES_1)

WeekDay <- 1
Sex <- 1
RACE3G <- "Asian"

d0b <- as.data.table(
  expand.grid.df(mcomp, bilr0, wilr0, 
                 BTIBz, WTIBz0,
                 WPosAffHALeadLag1, CPDzc18,
                 Age, BMI, SES_1,
                 WeekDay, Sex, RACE3G
  ))
d0w <- as.data.table(
  expand.grid.df(mcomp, bilr0, wilr0, 
                 BTIBz0, WTIBz,
                 WPosAffHALeadLag1, CPDzc18,
                 Age, BMI, SES_1,
                 WeekDay, Sex, RACE3G
  ))

colnames(d0b)  <- c(colnames(object$CompILR$TotalComp), 
                    colnames(object$CompILR$BetweenILR), 
                    colnames(object$CompILR$WithinILR), 
                    "BTIBz", "WTIBz",
                    "WPosAffHALeadLag1", "CPDzc18",
                    "Age", "BMI", "SES_1",
                    "WeekDay", "Sex", "RACE3G"
)
colnames(d0w)  <- c(colnames(object$CompILR$TotalComp), 
                    colnames(object$CompILR$BetweenILR), 
                    colnames(object$CompILR$WithinILR), 
                    "BTIBz", "WTIBz",
                    "WPosAffHALeadLag1", "CPDzc18",
                    "Age", "BMI", "SES_1",
                    "WeekDay", "Sex", "RACE3G"
)
y0b <- fitted(
  object,
  newdata = d0b,
  re_formula = NA,
  summary = FALSE
)
y0w <- fitted(
  object,
  newdata = d0w,
  re_formula = NA,
  summary = FALSE
)
# between susbtitution ------------
# pairwise sub
psub <- basesub(parts)
delta <- 30
kout <- vector("list")
jout <- vector("list")

for (j in seq_len(delta)) {
  sub <- psub * j
  
  for (k in seq_len(nrow(psub))) {
    subk <- sub[k, ]
    subk <- subk[rep(seq_len(nrow(subk)), nrow(mcomp)), ]
    newcomp <- mcomp + subk
    colnames(newcomp) <- paste0(parts)
    Delta <- j
    To <- colnames(subk)[(which(subk > 0))]
    From <- colnames(subk)[(which(subk < 0))]
    kout[[k]] <- cbind(mcomp, newcomp, Delta, From, To)
  }
  
  jout[[j]] <- do.call(rbind, kout)
}
dnew <- as.data.table(do.call(rbind, jout))

# compositions and ilrs for predictions
bcomp0 <- acomp(dnew[, colnames(object$CompILR$BetweenComp), with = FALSE], total = object$CompILR$total)
bcompsub  <- acomp(dnew[, object$CompILR$parts, with = FALSE], total = object$CompILR$total)

bilrsub <- ilr(bcompsub, V = object$CompILR$psi)
wilr0 <- as.data.table(matrix(0, nrow = nrow(bilrsub), ncol = ncol(bilrsub)))

colnames(bilrsub) <- colnames(object$CompILR$BetweenILR)
colnames(wilr0) <- colnames(object$CompILR$WithinILR)

dnormal <- cbind(bilrsub, wilr0, dnew, d0b[, -c(colnames(bilrsub), colnames(wilr0)), with = FALSE][BTIBz == mean(model.frame(object)$BTIBz)])
dshort <- cbind(bilrsub, wilr0, dnew, d0b[, -c(colnames(bilrsub), colnames(wilr0)), with = FALSE][BTIBz == mean(model.frame(object)$BTIBz) - sd(model.frame(object)$BTIBz)])
dlong <- cbind(bilrsub, wilr0, dnew, d0b[, -c(colnames(bilrsub), colnames(wilr0)), with = FALSE][BTIBz == mean(model.frame(object)$BTIBz) + sd(model.frame(object)$BTIBz)])

ysub_normal <-
  fitted(
    object,
    newdata = dnormal,
    re_formula = NA,
    summary = FALSE
  )
delta_y_normal <- ysub_normal - y0b[,2]
PD_delta_y_normal <- apply(delta_y_normal, 2, function(x) {
  describe_posterior(x, centrality = "mean")
})
PD_delta_y_normal <- rbindlist(PD_delta_y_normal)
PD_delta_y_normal[, TIB := "Normal Sleeper"]

ysub_short <-
  fitted(
    object,
    newdata = dshort,
    re_formula = NA,
    summary = FALSE
  )
delta_y_short <- ysub_short - y0b[,1]
PD_delta_y_short <- apply(delta_y_short, 2, function(x) {
  describe_posterior(x, centrality = "mean")
})
PD_delta_y_short <- rbindlist(PD_delta_y_short)
PD_delta_y_short[, TIB := "Short Sleeper"]

ysub_long <-
  fitted(
    object,
    newdata = dlong,
    re_formula = NA,
    summary = FALSE
  )
delta_y_long <- ysub_long - y0b[,3]
PD_delta_y_long <- apply(delta_y_long, 2, function(x) {
  describe_posterior(x, centrality = "mean")
})
PD_delta_y_long <- rbindlist(PD_delta_y_long)
PD_delta_y_long[, TIB := "Long Sleeper"]

PD_delta_y <- rbind(cbind(dnew, PD_delta_y_normal[, .(Mean, CI_low, CI_high, TIB)]), 
                    cbind(dnew, PD_delta_y_short[, .(Mean, CI_low, CI_high, TIB)]), 
                    cbind(dnew, PD_delta_y_long[, .(Mean, CI_low, CI_high, TIB)]))

# plot
ggplot(PD_delta_y[ To == "WAKE"], 
       aes(x = Delta, y = Mean)) +
  geom_line(aes(colour = From), linewidth = 1) +
  geom_ribbon(
    aes(ymin = CI_low,
        ymax = CI_high, fill = From),
    alpha = 1 / 10,
    linewidth = 1 / 10) +
  geom_hline(yintercept = 0,
             linewidth = 0.2,
             linetype = 2) +
  geom_vline(xintercept = 0,
             linewidth = 0.2,
             linetype = 2) +
  facet_wrap(ggplot2::vars(From, TIB))

ggplot(PD_delta_y[ To == "SleepLight"],
       aes(x = Delta, y = Mean)) +
  geom_line(aes(colour = From), linewidth = 1) +
  geom_ribbon(
    aes(ymin = CI_low,
        ymax = CI_high, fill = From),
    alpha = 1 / 10,
    linewidth = 1 / 10) +
  geom_hline(yintercept = 0,
             linewidth = 0.2,
             linetype = 2) +
  geom_vline(xintercept = 0,
             linewidth = 0.2,
             linetype = 2) +
  facet_wrap(ggplot2::vars(From, TIB))

ggplot(PD_delta_y[ To == "SleepREM"],
       aes(x = Delta, y = Mean)) +
  geom_line(aes(colour = From), linewidth = 1) +
  geom_ribbon(
    aes(ymin = CI_low,
        ymax = CI_high, fill = From),
    alpha = 1 / 10,
    linewidth = 1 / 10) +
  geom_hline(yintercept = 0,
             linewidth = 0.2,
             linetype = 2) +
  geom_vline(xintercept = 0,
             linewidth = 0.2,
             linetype = 2) +
  facet_wrap(ggplot2::vars(From, TIB))

ggplot(PD_delta_y[ To == "SleepDeep"],
       aes(x = Delta, y = Mean)) +
  geom_line(aes(colour = From), linewidth = 1) +
  geom_ribbon(
    aes(ymin = CI_low,
        ymax = CI_high, fill = From),
    alpha = 1 / 10,
    linewidth = 1 / 10) +
  geom_hline(yintercept = 0,
             linewidth = 0.2,
             linetype = 2) +
  geom_vline(xintercept = 0,
             linewidth = 0.2,
             linetype = 2) +
  facet_wrap(ggplot2::vars(From, TIB))

 # within substitution -----------------
psub <- basesub(parts)
delta <- 30
kout <- vector("list")
jout <- vector("list")

for (j in seq_len(delta)) {
  sub <- psub * j
  
  for (k in seq_len(nrow(psub))) {
    subk <- sub[k, ]
    subk <- subk[rep(seq_len(nrow(subk)), nrow(mcomp)), ]
    newcomp <- mcomp + subk
    colnames(newcomp) <- paste0(parts)
    Delta <- j
    To <- colnames(subk)[(which(subk > 0))]
    From <- colnames(subk)[(which(subk < 0))]
    kout[[k]] <- cbind(mcomp, newcomp, Delta, From, To)
  }
  
  jout[[j]] <- do.call(rbind, kout)
}
dnew <- as.data.table(do.call(rbind, jout))

# compositions and ilrs for predictions
bcomp0 <- acomp(dnew[, colnames(object$CompILR$BetweenComp), with = FALSE], total = object$CompILR$total)
bcompsub  <- acomp(dnew[, object$CompILR$parts, with = FALSE], total = object$CompILR$total)

bilrsub <- ilr(bcompsub, V = object$CompILR$psi)
wilr0 <- as.data.table(matrix(0, nrow = nrow(bilrsub), ncol = ncol(bilrsub)))

bilr0   <- ilr(bcomp0, V = object$CompILR$psi)
wilrsub <- bilrsub - bilr0

colnames(bilr0) <- colnames(object$CompILR$BetweenILR)
colnames(bilrsub) <- colnames(object$CompILR$BetweenILR)
colnames(wilr0) <- colnames(object$CompILR$WithinILR)
colnames(wilrsub) <- colnames(object$CompILR$WithinILR)

dnormal <- cbind(bilr0, wilrsub, dnew, d0w[, -c(colnames(bilr0), colnames(wilrsub)), with = FALSE][WTIBz == mean(model.frame(object)$WTIBz)])
dshort <- cbind(bilr0, wilrsub, dnew, d0w[, -c(colnames(bilr0), colnames(wilrsub)), with = FALSE][WTIBz == mean(model.frame(object)$WTIBz) - sd(model.frame(object)$WTIBz)])
dlong <- cbind(bilr0, wilrsub, dnew, d0w[, -c(colnames(bilr0), colnames(wilrsub)), with = FALSE][WTIBz == mean(model.frame(object)$WTIBz) + sd(model.frame(object)$WTIBz)])

ysub_normal <-
  fitted(
    object,
    newdata = dnormal,
    re_formula = NA,
    summary = FALSE
  )
delta_y_normal <- ysub_normal - y0w[,2]
PD_delta_y_normal <- apply(delta_y_normal, 2, function(x) {
  describe_posterior(x, centrality = "mean")
})
PD_delta_y_normal <- rbindlist(PD_delta_y_normal)
PD_delta_y_normal[, TIB := "Normal Sleeper"]

ysub_short <-
  fitted(
    object,
    newdata = dshort,
    re_formula = NA,
    summary = FALSE
  )
delta_y_short <- ysub_short - y0w[,1]
PD_delta_y_short <- apply(delta_y_short, 2, function(x) {
  describe_posterior(x, centrality = "mean")
})
PD_delta_y_short <- rbindlist(PD_delta_y_short)
PD_delta_y_short[, TIB := "Short Sleeper"]

ysub_long <-
  fitted(
    object,
    newdata = dlong,
    re_formula = NA,
    summary = FALSE
  )
delta_y_long <- ysub_long - y0w[,3]
PD_delta_y_long <- apply(delta_y_long, 2, function(x) {
  describe_posterior(x, centrality = "mean")
})
PD_delta_y_long <- rbindlist(PD_delta_y_long)
PD_delta_y_long[, TIB := "Long Sleeper"]

PD_delta_y <- rbind(cbind(dnew, PD_delta_y_normal[, .(Mean, CI_low, CI_high, TIB)]), 
                    cbind(dnew, PD_delta_y_short[, .(Mean, CI_low, CI_high, TIB)]), 
                    cbind(dnew, PD_delta_y_long[, .(Mean, CI_low, CI_high, TIB)]))

# plot
ggplot(PD_delta_y[ To == "WAKE"], 
       aes(x = Delta, y = Mean)) +
  geom_line(aes(colour = From), linewidth = 1) +
  geom_ribbon(
    aes(ymin = CI_low,
        ymax = CI_high, fill = From),
    alpha = 1 / 10,
    linewidth = 1 / 10) +
  geom_hline(yintercept = 0,
             linewidth = 0.2,
             linetype = 2) +
  geom_vline(xintercept = 0,
             linewidth = 0.2,
             linetype = 2) +
  facet_wrap(ggplot2::vars(From, TIB))

ggplot(PD_delta_y[ To == "SleepLight"],
       aes(x = Delta, y = Mean)) +
  geom_line(aes(colour = From), linewidth = 1) +
  geom_ribbon(
    aes(ymin = CI_low,
        ymax = CI_high, fill = From),
    alpha = 1 / 10,
    linewidth = 1 / 10) +
  geom_hline(yintercept = 0,
             linewidth = 0.2,
             linetype = 2) +
  geom_vline(xintercept = 0,
             linewidth = 0.2,
             linetype = 2) +
  facet_wrap(ggplot2::vars(From, TIB))

ggplot(PD_delta_y[ To == "SleepREM"],
       aes(x = Delta, y = Mean)) +
  geom_line(aes(colour = From), linewidth = 1) +
  geom_ribbon(
    aes(ymin = CI_low,
        ymax = CI_high, fill = From),
    alpha = 1 / 10,
    linewidth = 1 / 10) +
  geom_hline(yintercept = 0,
             linewidth = 0.2,
             linetype = 2) +
  geom_vline(xintercept = 0,
             linewidth = 0.2,
             linetype = 2) +
  facet_wrap(ggplot2::vars(From, TIB))

ggplot(PD_delta_y[ To == "SleepDeep"],
       aes(x = Delta, y = Mean)) +
  geom_line(aes(colour = From), linewidth = 1) +
  geom_ribbon(
    aes(ymin = CI_low,
        ymax = CI_high, fill = From),
    alpha = 1 / 10,
    linewidth = 1 / 10) +
  geom_hline(yintercept = 0,
             linewidth = 0.2,
             linetype = 2) +
  geom_vline(xintercept = 0,
             linewidth = 0.2,
             linetype = 2) +
  facet_wrap(ggplot2::vars(From, TIB))

## BF
m_sleep_hapa <- brmcoda(compilr = cilrw_hapa,
                        formula = PosAffHALead ~ 
                          (bilr1 + bilr2 + bilr3) + BTIBz +
                          (wilr1 + wilr2 + wilr3) + WTIBz + 
                          WPosAffHALeadLag1 +
                          WeekDay + CPDzc18 +
                          Age + Sex + RACE3G + BMI + SES_1 +
                          # CurrentWork + SmokingStatus + AUDITCat + 
                          (1 + WPosAffHALeadLag1 + wilr1 + wilr2 + wilr3 + WTIBz | ID),
                        iter = 6000, chains = 8, cores = 8, seed = 123, warmup = 1000,
                        backend = "cmdstanr", save_pars = save_pars(all = TRUE))
m_sleep_lapa <- brmcoda(compilr = cilrw,
                        formula = PosAffLALead ~ 
                          (bilr1 + bilr2 + bilr3) + BTIBz +
                          (wilr1 + wilr2 + wilr3) + WTIBz + 
                          WPosAffLALeadLag1 +
                          WeekDay + CPDzc18 +
                          Age + Sex + RACE3G + BMI + SES_1 +
                          # CurrentWork + SmokingStatus + AUDITCat + 
                          (1 + WPosAffLALeadLag1 + wilr1 + wilr2 + wilr3 + WTIBz | ID),
                        iter = 6000, chains = 8, cores = 8, seed = 123, warmup = 1000,
                        backend = "cmdstanr", save_pars = save_pars(all = TRUE))
m_sleep_hana <- brmcoda(compilr = cilrw,
                        formula = NegAffHALead ~ 
                          (bilr1 + bilr2 + bilr3) + BTIBz +
                          (wilr1 + wilr2 + wilr3) + WTIBz + 
                          WNegAffHALeadLag1 +
                          WeekDay + CPDzc18 +
                          Age + Sex + RACE3G + BMI + SES_1 +
                          # CurrentWork + SmokingStatus + AUDITCat + 
                          (1 + WNegAffHALeadLag1 + wilr1 + wilr2 + wilr3 + WTIBz | ID),
                        iter = 6000, chains = 8, cores = 8, seed = 123, warmup = 1000,
                        backend = "cmdstanr", save_pars = save_pars(all = TRUE))
m_sleep_lana <- brmcoda(compilr = cilrw,
                        formula = NegAffLALead ~ 
                          (bilr1 + bilr2 + bilr3) + BTIBz +
                          (wilr1 + wilr2 + wilr3) + WTIBz + 
                          WNegAffLALeadLag1 +
                          WeekDay + CPDzc18 +
                          Age + Sex + RACE3G + BMI + SES_1 +
                          # CurrentWork + SmokingStatus + AUDITCat + 
                          (1 + WNegAffLALeadLag1 + wilr1 + wilr2 + wilr3 + WTIBz | ID),
                        iter = 6000, chains = 8, cores = 8, seed = 123, warmup = 1000,
                        backend = "cmdstanr", save_pars = save_pars(all = TRUE))

bayes_factor(m_sleep_hapa_adj, m_sleep_hapa)
bayes_factor(m_sleep_lapa_adj, m_sleep_lapa)
bayes_factor(m_sleep_hana_adj, m_sleep_hana)
bayes_factor(m_sleep_lana_adj, m_sleep_lana)


m0_sleep_hapa <- brmcoda(compilr = cilrw_hapa,
                        formula = PosAffHALead ~ 
                          BTIBz +
                          WTIBz + 
                          WPosAffHALeadLag1 +
                          WeekDay + CPDzc18 +
                          Age + Sex + RACE3G + BMI + SES_1 +
                          # CurrentWork + SmokingStatus + AUDITCat + 
                          (1 + WPosAffHALeadLag1 + wilr1 + wilr2 + wilr3 + WTIBz | ID),
                        iter = 6000, chains = 8, cores = 8, seed = 123, warmup = 1000,
                        backend = "cmdstanr", save_pars = save_pars(all = TRUE))
m0_sleep_lapa <- brmcoda(compilr = cilrw,
                        formula = PosAffLALead ~ 
                          BTIBz +
                          WTIBz + 
                          WPosAffLALeadLag1 +
                          WeekDay + CPDzc18 +
                          Age + Sex + RACE3G + BMI + SES_1 +
                          # CurrentWork + SmokingStatus + AUDITCat + 
                          (1 + WPosAffLALeadLag1 + wilr1 + wilr2 + wilr3 + WTIBz | ID),
                        iter = 6000, chains = 8, cores = 8, seed = 123, warmup = 1000,
                        backend = "cmdstanr", save_pars = save_pars(all = TRUE))
m0_sleep_hana <- brmcoda(compilr = cilrw,
                        formula = NegAffHALead ~ 
                          BTIBz +
                          WTIBz + 
                          WNegAffHALeadLag1 +
                          WeekDay + CPDzc18 +
                          Age + Sex + RACE3G + BMI + SES_1 +
                          # CurrentWork + SmokingStatus + AUDITCat + 
                          (1 + WNegAffHALeadLag1 + wilr1 + wilr2 + wilr3 + WTIBz | ID),
                        iter = 6000, chains = 8, cores = 8, seed = 123, warmup = 1000,
                        backend = "cmdstanr", save_pars = save_pars(all = TRUE))
m0_sleep_lana <- brmcoda(compilr = cilrw,
                        formula = NegAffLALead ~ 
                          BTIBz +
                          WTIBz + 
                          WNegAffLALeadLag1 +
                          WeekDay + CPDzc18 +
                          Age + Sex + RACE3G + BMI + SES_1 +
                          # CurrentWork + SmokingStatus + AUDITCat + 
                          (1 + WNegAffLALeadLag1 + wilr1 + wilr2 + wilr3 + WTIBz | ID),
                        iter = 6000, chains = 8, cores = 8, seed = 123, warmup = 1000,
                        backend = "cmdstanr", save_pars = save_pars(all = TRUE))


loo_hapax <- loo(m_sleep_hapa_adj$Model)
loo_hapa <- loo(m_sleep_hapa$Model)
loo_hapa0 <- loo(m0_sleep_hapa$Model)

comp <- loo_compare(loo_hapax, loo_hapa, loo_hapa0)
print(comp, digits = 2)
