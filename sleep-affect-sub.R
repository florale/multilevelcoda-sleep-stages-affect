source("data.R")

## unadj predicting outcomes at wake ---------------------
cilrw <- compilr(d[Survey == "Wake"], sbp = sbp4, 
                 parts = c("SleepLight", "SleepDeep", "SleepREM", "WAKE"), total = 448)

m_hapa_unadj <- brmcoda(compilr = cilrw,
                      formula = PosAffHALead ~ 
                        bilr1 + bilr2 + bilr3 + 
                        wilr1 + wilr2 + wilr3 + 
                        # WPosAffHALeadLag1 +
                        # WeekDay +
                        # Age + Sex + RACE3G + BMI + SES_1 + 
                        # CurrentWork + SmokingStatus + AUDITCat + 
                        (1 | ID),
                      cores = 4, backend = "cmdstanr"
)
m_lapa_unadj <- brmcoda(compilr = cilrw,
                      formula = PosAffLALead ~ 
                        bilr1 + bilr2 + bilr3 + 
                        wilr1 + wilr2 + wilr3 + 
                        # WPosAffLALeadLag1 +
                        # WeekDay +
                        # Age + Sex + RACE3G + BMI + SES_1 + 
                        # CurrentWork + SmokingStatus + AUDITCat + 
                        (1 | ID),
                      cores = 4, backend = "cmdstanr"
)
m_hana_unadj <- brmcoda(compilr = cilrw,
                      formula = NegAffHALead ~ 
                        bilr1 + bilr2 + bilr3 + 
                        wilr1 + wilr2 + wilr3 + 
                        # WNegAffHALeadLag1 +
                        # WeekDay +
                        # Age + Sex + RACE3G + BMI + SES_1 + 
                        # CurrentWork + SmokingStatus + AUDITCat +
                        (1 | ID),
                      cores = 4, backend = "cmdstanr"
)
m_lana_unadj <- brmcoda(compilr = cilrw,
                      formula = NegAffLALead ~ 
                        bilr1 + bilr2 + bilr3 + 
                        wilr1 + wilr2 + wilr3 + 
                        # WNegAffLALeadLag1 +
                        # WeekDay +
                        # Age + Sex + RACE3G + BMI + SES_1 + 
                        # CurrentWork + SmokingStatus + AUDITCat + 
                        (1 | ID),
                      cores = 4, backend = "cmdstanr"
)

plan(multisession, workers = 5)
m_hapa_sub_unadj <- substitution(
  m_hapa_unadj,
  delta = c(60),
  level = c("between", "within"),
  ref = "grandmean")
m_lapa_sub_unadj <- substitution(
  m_lapa_unadj,
  delta = c(60),
  level = c("between", "within"),
  ref = "grandmean")
m_hana_sub_unadj <- substitution(
  m_hana_unadj,
  delta = c(60),
  level = c("between", "within"),
  ref = "grandmean")
m_lana_sub_unadj <- substitution(
  m_lana_unadj,
  delta = c(60),
  level = c("between", "within"),
  ref = "grandmean")
plan(sequential)

summary(m_hapa_sub_unadj, delta = 60) #
summary(m_lapa_sub_unadj, delta = 60)
summary(m_hana_sub_unadj, delta = 60) #
summary(m_lana_sub_unadj, delta = 60) #

## predicting outcomes at wake  adj for all  ---------------------
m_hapa <- readRDS(paste0(outputdir, "m_hapa", ".RDS"))
m_lapa <- readRDS(paste0(outputdir, "m_lapa", ".RDS"))
m_hana <- readRDS(paste0(outputdir, "m_hana", ".RDS"))
m_lana <- readRDS(paste0(outputdir, "m_lana", ".RDS"))

plan(multisession, workers = 5)
m_hapa_sub_adj <- substitution(
  m_hapa,
  delta = c(1:60),
  # delta = 30,
  level = c("between", "within"),
  ref = "grandmean")
m_lapa_sub_adj <- substitution(
  m_lapa,
  delta = c(1:60),
  # delta = 30,
  level = c("between", "within"),
  ref = "grandmean")
m_hana_sub_adj <- substitution(
  m_hana,
  delta = c(1:60),
  # delta = 30,
  level = c("between", "within"),
  ref = "grandmean")
m_lana_sub_adj <- substitution(
  m_lana,
  delta = c(1:60),
  # delta = 30,
  level = c("between", "within"),
  ref = "grandmean")
plan(sequential)

summary(m_hapa_sub_adj, delta = 30)
summary(m_lapa_sub_adj, delta = 30)
summary(m_hana_sub_adj, delta = 30)
summary(m_lana_sub_adj, delta = 30)

saveRDS(m_hapa_sub_adj, paste0(outputdir, "m_hapa_sub_adj", ".RDS"))
saveRDS(m_lapa_sub_adj, paste0(outputdir, "m_lapa_sub_adj", ".RDS"))
saveRDS(m_hana_sub_adj, paste0(outputdir, "m_hana_sub_adj", ".RDS"))
saveRDS(m_lana_sub_adj, paste0(outputdir, "m_lana_sub_adj", ".RDS"))
