source("sleep-affect-data.R")

# full ------------------

m_hapa <- brmcoda(complr = cilrw_hapa,
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
saveRDS(m_hapa, paste0(outputdir, "m_hapa", ".RDS"))

m_lapa <- brmcoda(complr = cilrw_lapa,
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
saveRDS(m_lapa, paste0(outputdir, "m_lapa", ".RDS"))

m_hana <- brmcoda(complr = cilrw_hana,
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
saveRDS(m_hana, paste0(outputdir, "m_hana", ".RDS"))

m_lana <- brmcoda(complr = cilrw_lana,
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
saveRDS(m_lana, paste0(outputdir, "m_lana", ".RDS"))

# between --------------------
mb_hapa <- brmcoda(complr = cilrw_hapa,
                   formula = PosAffHALead ~ 
                     bilr1 + bilr2 + bilr3 +
                     BTIBz +
                     WTIBz + 
                     WPosAffHALeadLag1 +
                     WeekDay + CPDzc18 + 
                     Age + Sex + RACE3G + BMI + SES_1 + 
                     # CurrentWork + SmokingStatus + AUDITCat + 
                     (1 + WPosAffHALeadLag1 + WTIBz | ID),
                   iter = 6000, chains = 8, cores = 8, seed = 123, warmup = 1000,
                   backend = "cmdstanr", save_pars = save_pars(all = TRUE))
saveRDS(mb_hapa, paste0(outputdir, "mb_hapa", ".RDS"))

mb_lapa <- brmcoda(complr = cilrw_lapa,
                   formula = PosAffLALead ~ 
                     bilr1 + bilr2 + bilr3 +
                     BTIBz +
                     WTIBz + 
                     WPosAffLALeadLag1 +
                     WeekDay + CPDzc18 + 
                     Age + Sex + RACE3G + BMI + SES_1 + 
                     # CurrentWork + SmokingStatus + AUDITCat + 
                     (1 + WPosAffLALeadLag1 + WTIBz | ID),
                   iter = 6000, chains = 8, cores = 8, seed = 123, warmup = 1000,
                   backend = "cmdstanr", save_pars = save_pars(all = TRUE))
saveRDS(mb_lapa, paste0(outputdir, "mb_lapa", ".RDS"))

mb_hana <- brmcoda(complr = cilrw_hana,
                   formula = NegAffHALead ~ 
                     bilr1 + bilr2 + bilr3 +
                     BTIBz +
                     WTIBz + 
                     WNegAffHALeadLag1 +
                     WeekDay + CPDzc18 +
                     Age + Sex + RACE3G + BMI + SES_1 + 
                     # CurrentWork + SmokingStatus + AUDITCat + 
                     (1 + WNegAffHALeadLag1 + WTIBz | ID),
                   iter = 6000, chains = 8, cores = 8, seed = 123, warmup = 1000,
                   backend = "cmdstanr", save_pars = save_pars(all = TRUE))
saveRDS(mb_hana, paste0(outputdir, "mb_hana", ".RDS"))

mb_lana <- brmcoda(complr = cilrw_lana,
                   formula = NegAffLALead ~ 
                     bilr1 + bilr2 + bilr3 +
                     BTIBz +
                     WTIBz + 
                     WNegAffLALeadLag1 +
                     WeekDay + CPDzc18 + 
                     Age + Sex + RACE3G + BMI + SES_1 + 
                     # CurrentWork + SmokingStatus + AUDITCat + 
                     (1 + WNegAffLALeadLag1 + WTIBz | ID),
                   iter = 6000, chains = 8, cores = 8, seed = 123, warmup = 1000,
                   backend = "cmdstanr", save_pars = save_pars(all = TRUE))
saveRDS(mb_lana, paste0(outputdir, "mb_lana", ".RDS"))

# within ---------------------
mw_hapa <- brmcoda(complr = cilrw_hapa,
                   formula = PosAffHALead ~ 
                     wilr1 + wilr2 + wilr3 +
                     BTIBz +
                     WTIBz + 
                     WPosAffHALeadLag1 +
                     WeekDay + CPDzc18 + 
                     Age + Sex + RACE3G + BMI + SES_1 + 
                     # CurrentWork + SmokingStatus + AUDITCat + 
                     (1 + WPosAffHALeadLag1 + wilr1 + wilr2 + wilr3 + WTIBz | ID),
                   iter = 6000, chains = 8, cores = 8, seed = 123, warmup = 1000,
                   backend = "cmdstanr", save_pars = save_pars(all = TRUE))
saveRDS(mw_hapa, paste0(outputdir, "mw_hapa", ".RDS"))

mw_lapa <- brmcoda(complr = cilrw_lapa,
                   formula = PosAffLALead ~ 
                     wilr1 + wilr2 + wilr3 +
                     BTIBz +
                     WTIBz + 
                     WPosAffLALeadLag1 +
                     WeekDay + CPDzc18 + 
                     Age + Sex + RACE3G + BMI + SES_1 + 
                     # CurrentWork + SmokingStatus + AUDITCat + 
                     (1 + WPosAffLALeadLag1 + wilr1 + wilr2 + wilr3 + WTIBz | ID),
                   iter = 6000, chains = 8, cores = 8, seed = 123, warmup = 1000,
                   backend = "cmdstanr", save_pars = save_pars(all = TRUE))
saveRDS(mw_lapa, paste0(outputdir, "mw_lapa", ".RDS"))

mw_hana <- brmcoda(complr = cilrw_hana,
                   formula = NegAffHALead ~ 
                     wilr1 + wilr2 + wilr3 +
                     BTIBz +
                     WTIBz + 
                     WNegAffHALeadLag1 +
                     WeekDay + CPDzc18 + 
                     Age + Sex + RACE3G + BMI + SES_1 + 
                     # CurrentWork + SmokingStatus + AUDITCat + 
                     (1 + WNegAffHALeadLag1 + wilr1 + wilr2 + wilr3 + WTIBz | ID),
                   iter = 6000, chains = 8, cores = 8, seed = 123, warmup = 1000,
                   backend = "cmdstanr", save_pars = save_pars(all = TRUE))
saveRDS(mw_hana, paste0(outputdir, "mw_hana", ".RDS"))

mw_lana <- brmcoda(complr = cilrw_lana,
                   formula = NegAffLALead ~ 
                     wilr1 + wilr2 + wilr3 +
                     BTIBz +
                     WTIBz + 
                     WNegAffLALeadLag1 +
                     WeekDay + CPDzc18 + 
                     Age + Sex + RACE3G + BMI + SES_1 + 
                     # CurrentWork + SmokingStatus + AUDITCat + 
                     (1 + WNegAffLALeadLag1 + wilr1 + wilr2 + wilr3 + WTIBz | ID),
                   iter = 6000, chains = 8, cores = 8, seed = 123, warmup = 1000,
                   backend = "cmdstanr", save_pars = save_pars(all = TRUE))
saveRDS(mw_lana, paste0(outputdir, "mw_lana", ".RDS"))

# null -----------------
m0_hapa <- brmcoda(complr = cilrw_hapa,
                   formula = PosAffHALead ~ 
                     BTIBz +
                     WTIBz + 
                     WPosAffHALeadLag1 +
                     WeekDay + CPDzc18 + 
                     Age + Sex + RACE3G + BMI + SES_1 + 
                     # CurrentWork + SmokingStatus + AUDITCat + 
                     (1 + WPosAffHALeadLag1 + WTIBz | ID),
                   iter = 6000, chains = 8, cores = 8, seed = 123, warmup = 1000,
                   backend = "cmdstanr", save_pars = save_pars(all = TRUE))
saveRDS(m0_hapa, paste0(outputdir, "m0_hapa", ".RDS"))

m0_lapa <- brmcoda(complr = cilrw_lapa,
                   formula = PosAffLALead ~ 
                     BTIBz +
                     WTIBz + 
                     WPosAffLALeadLag1 +
                     WeekDay + CPDzc18 + 
                     Age + Sex + RACE3G + BMI + SES_1 + 
                     # CurrentWork + SmokingStatus + AUDITCat + 
                     (1 + WPosAffLALeadLag1 + WTIBz | ID),
                   iter = 6000, chains = 8, cores = 8, seed = 123, warmup = 1000,
                   backend = "cmdstanr", save_pars = save_pars(all = TRUE))
saveRDS(m0_lapa, paste0(outputdir, "m0_lapa", ".RDS"))

m0_hana <- brmcoda(complr = cilrw_hana,
                   formula = NegAffHALead ~ 
                     BTIBz +
                     WTIBz + 
                     WNegAffHALeadLag1 +
                     WeekDay + CPDzc18 + 
                     Age + Sex + RACE3G + BMI + SES_1 + 
                     # CurrentWork + SmokingStatus + AUDITCat + 
                     (1 + WNegAffHALeadLag1 + WTIBz | ID),
                   iter = 6000, chains = 8, cores = 8, seed = 123, warmup = 1000,
                   backend = "cmdstanr", save_pars = save_pars(all = TRUE))
saveRDS(m0_hana, paste0(outputdir, "m0_hana", ".RDS"))

m0_lana <- brmcoda(complr = cilrw_lana,
                   formula = NegAffLALead ~ 
                     BTIBz +
                     WTIBz + 
                     WNegAffLALeadLag1 +
                     WeekDay + CPDzc18 + 
                     Age + Sex + RACE3G + BMI + SES_1 + 
                     # CurrentWork + SmokingStatus + AUDITCat + 
                     (1 + WNegAffLALeadLag1 + WTIBz | ID),
                   iter = 6000, chains = 8, cores = 8, seed = 123, warmup = 1000,
                   backend = "cmdstanr", save_pars = save_pars(all = TRUE))
saveRDS(m0_lana, paste0(outputdir, "m0_lana", ".RDS"))

# interaction ---------------
# with between and within TIB
mx1_hapa <- brmcoda(complr = cilrw_hapa,
                    formula = PosAffHALead ~ 
                      (bilr1 + bilr2 + bilr3)*BTIBz +
                      (wilr1 + wilr2 + wilr3)*WTIBz + 
                      WPosAffHALeadLag1 +
                      WeekDay + CPDzc18 +
                      Age + Sex + RACE3G + BMI + SES_1 + 
                      # CurrentWork + SmokingStatus + AUDITCat + 
                      (1 + WPosAffHALeadLag1 + wilr1 + wilr2 + wilr3 + WTIBz | ID),
                    iter = 6000, chains = 8, cores = 8, seed = 123, warmup = 1000,
                    backend = "cmdstanr", save_pars = save_pars(all = TRUE))
saveRDS(mx1_hapa, paste0(outputdir, "mx1_hapa", ".RDS"))

mx1_lapa <- brmcoda(complr = cilrw_lapa,
                    formula = PosAffLALead ~ 
                      (bilr1 + bilr2 + bilr3)*BTIBz +
                      (wilr1 + wilr2 + wilr3)*WTIBz + 
                      WPosAffLALeadLag1 +
                      WeekDay + CPDzc18 +
                      Age + Sex + RACE3G + BMI + SES_1 + 
                      # CurrentWork + SmokingStatus + AUDITCat + 
                      (1 + WPosAffLALeadLag1 + wilr1 + wilr2 + wilr3 + WTIBz | ID),
                    iter = 6000, chains = 8, cores = 8, seed = 123, warmup = 1000,
                    backend = "cmdstanr", save_pars = save_pars(all = TRUE))
saveRDS(mx1_lapa, paste0(outputdir, "mx1_lapa", ".RDS"))

mx1_hana <- brmcoda(complr = cilrw_hana,
                    formula = NegAffHALead ~ 
                      (bilr1 + bilr2 + bilr3)*BTIBz +
                      (wilr1 + wilr2 + wilr3)*WTIBz + 
                      WNegAffHALeadLag1 +
                      WeekDay + CPDzc18 +
                      Age + Sex + RACE3G + BMI + SES_1 + 
                      # CurrentWork + SmokingStatus + AUDITCat + 
                      (1 + WNegAffHALeadLag1 + wilr1 + wilr2 + wilr3 + WTIBz | ID),
                    iter = 6000, chains = 8, cores = 8, seed = 123, warmup = 1000,
                    backend = "cmdstanr", save_pars = save_pars(all = TRUE))
saveRDS(mx1_hana, paste0(outputdir, "mx1_hana", ".RDS"))

mx1_lana <- brmcoda(complr = cilrw_lana,
                    formula = NegAffLALead ~ 
                      (bilr1 + bilr2 + bilr3)*BTIBz +
                      (wilr1 + wilr2 + wilr3)*TIBz +
                      WNegAffLALeadLag1 +
                      WeekDay + CPDzc18 +
                      Age + Sex + RACE3G + BMI + SES_1 + 
                      # CurrentWork + SmokingStatus + AUDITCat + 
                      (1 + WNegAffLALeadLag1 + wilr1 + wilr2 + wilr3 + WTIBz | ID),
                    iter = 6000, chains = 8, cores = 8, seed = 123, warmup = 1000,
                    backend = "cmdstanr", save_pars = save_pars(all = TRUE))
saveRDS(mx1_lana, paste0(outputdir, "mx1_lana", ".RDS"))

# interaction with between and total TIB
mx2_hapa <- brmcoda(complr = cilrw_hapa,
                    formula = PosAffHALead ~ 
                      (bilr1 + bilr2 + bilr3)*BTIBz +
                      (wilr1 + wilr2 + wilr3)*TIBz + 
                      WPosAffHALeadLag1 +
                      WeekDay + CPDzc18 +
                      Age + Sex + RACE3G + BMI + SES_1 + 
                      # CurrentWork + SmokingStatus + AUDITCat + 
                      (1 + WPosAffHALeadLag1 + wilr1 + wilr2 + wilr3 + TIBz | ID),
                    iter = 6000, chains = 8, cores = 8, seed = 123, warmup = 1000,
                    backend = "cmdstanr", save_pars = save_pars(all = TRUE))
saveRDS(mx2_hapa, paste0(outputdir, "mx2_hapa", ".RDS"))

mx2_lapa <- brmcoda(complr = cilrw_lapa,
                    formula = PosAffLALead ~ 
                      (bilr1 + bilr2 + bilr3)*BTIBz +
                      (wilr1 + wilr2 + wilr3)*TIBz + 
                      WPosAffLALeadLag1 +
                      WeekDay + CPDzc18 +
                      Age + Sex + RACE3G + BMI + SES_1 + 
                      # CurrentWork + SmokingStatus + AUDITCat + 
                      (1 + WPosAffLALeadLag1 + wilr1 + wilr2 + wilr3 + TIBz | ID),
                    iter = 6000, chains = 8, cores = 8, seed = 123, warmup = 1000,
                    backend = "cmdstanr", save_pars = save_pars(all = TRUE))
saveRDS(mx2_lapa, paste0(outputdir, "mx2_lapa", ".RDS"))

mx2_hana <- brmcoda(complr = cilrw_hana,
                    formula = NegAffHALead ~ 
                      (bilr1 + bilr2 + bilr3)*BTIBz +
                      (wilr1 + wilr2 + wilr3)*TIBz + 
                      WNegAffHALeadLag1 +
                      WeekDay + CPDzc18 +
                      Age + Sex + RACE3G + BMI + SES_1 + 
                      # CurrentWork + SmokingStatus + AUDITCat + 
                      (1 + WNegAffHALeadLag1 + wilr1 + wilr2 + wilr3 + TIBz | ID),
                    iter = 6000, chains = 8, cores = 8, seed = 123, warmup = 1000,
                    backend = "cmdstanr", save_pars = save_pars(all = TRUE))
saveRDS(mx2_hana, paste0(outputdir, "mx2_hana", ".RDS"))

mx2_lana <- brmcoda(complr = cilrw_lana,
                    formula = NegAffLALead ~ 
                      (bilr1 + bilr2 + bilr3)*BTIBz +
                      (wilr1 + wilr2 + wilr3)*TIBz +
                      WNegAffLALeadLag1 +
                      WeekDay + CPDzc18 +
                      Age + Sex + RACE3G + BMI + SES_1 + 
                      # CurrentWork + SmokingStatus + AUDITCat + 
                      (1 + WNegAffLALeadLag1 + wilr1 + wilr2 + wilr3 + TIBz | ID),
                    iter = 6000, chains = 8, cores = 8, seed = 123, warmup = 1000,
                    backend = "cmdstanr", save_pars = save_pars(all = TRUE))
saveRDS(mx2_lana, paste0(outputdir, "mx2_lana", ".RDS"))

# total composition with total TIB 
mx3_hapa <- brmcoda(complr = cilrw_hapa,
                    formula = PosAffHALead ~ 
                      (ilr1 + ilr2 + ilr3)*TIBz +
                      WPosAffHALeadLag1 +
                      WeekDay + CPDzc18 +
                      Age + Sex + RACE3G + BMI + SES_1 + 
                      # CurrentWork + SmokingStatus + AUDITCat + 
                      (1 + WPosAffHALeadLag1 + ilr1 + ilr2 + ilr3 + TIBz | ID),
                    iter = 6000, chains = 8, cores = 8, seed = 123, warmup = 1000,
                    backend = "cmdstanr", save_pars = save_pars(all = TRUE))
saveRDS(mx3_hapa, paste0(outputdir, "mx3_hapa", ".RDS"))

mx3_lapa <- brmcoda(complr = cilrw_lapa,
                    formula = PosAffLALead ~ 
                      (ilr1 + ilr2 + ilr3)*TIBz +
                      WPosAffLALeadLag1 +
                      WeekDay + CPDzc18 +
                      Age + Sex + RACE3G + BMI + SES_1 + 
                      # CurrentWork + SmokingStatus + AUDITCat + 
                      (1 + WPosAffLALeadLag1 + ilr1 + ilr2 + ilr3 + TIBz | ID),
                    iter = 6000, chains = 8, cores = 8, seed = 123, warmup = 1000,
                    backend = "cmdstanr", save_pars = save_pars(all = TRUE))
saveRDS(mx3_lapa, paste0(outputdir, "mx3_lapa", ".RDS"))

mx3_hana <- brmcoda(complr = cilrw_hana,
                    formula = NegAffHALead ~ 
                      (ilr1 + ilr2 + ilr3)*TIBz +
                      WNegAffHALeadLag1 +
                      WeekDay + CPDzc18 +
                      Age + Sex + RACE3G + BMI + SES_1 + 
                      # CurrentWork + SmokingStatus + AUDITCat + 
                      (1 + WNegAffHALeadLag1 + ilr1 + ilr2 + ilr3 + TIBz | ID),
                    iter = 6000, chains = 8, cores = 8, seed = 123, warmup = 1000,
                    backend = "cmdstanr", save_pars = save_pars(all = TRUE))
saveRDS(mx3_hana, paste0(outputdir, "mx3_hana", ".RDS"))

mx3_lana <- brmcoda(complr = cilrw_lana,
                    formula = NegAffLALead ~ 
                      (ilr1 + ilr2 + ilr3)*TIBz +
                      WNegAffLALeadLag1 +
                      WeekDay + CPDzc18 +
                      Age + Sex + RACE3G + BMI + SES_1 + 
                      # CurrentWork + SmokingStatus + AUDITCat + 
                      (1 + WNegAffLALeadLag1 + ilr1 + ilr2 + ilr3 + TIBz | ID),
                    iter = 6000, chains = 8, cores = 8, seed = 123, warmup = 1000,
                    backend = "cmdstanr", save_pars = save_pars(all = TRUE))
saveRDS(mx3_lana, paste0(outputdir, "mx3_lana", ".RDS"))

# bw composition with total TIB 
mx4_hapa <- brmcoda(complr = cilrw_hapa,
                    formula = PosAffHALead ~ 
                      (bilr1 + bilr2 + bilr3 + wilr1 + wilr2 + wilr3)*TIBz +
                      WPosAffHALeadLag1 +
                      WeekDay + CPDzc18 +
                      Age + Sex + RACE3G + BMI + SES_1 + 
                      # CurrentWork + SmokingStatus + AUDITCat + 
                      (1 + WPosAffHALeadLag1 + ilr1 + ilr2 + ilr3 + TIBz | ID),
                    iter = 6000, chains = 8, cores = 8, seed = 123, warmup = 1000,
                    backend = "cmdstanr", save_pars = save_pars(all = TRUE))
saveRDS(mx4_hapa, paste0(outputdir, "mx4_hapa", ".RDS"))

mx4_lapa <- brmcoda(complr = cilrw_lapa,
                    formula = PosAffLALead ~ 
                      (bilr1 + bilr2 + bilr3 + wilr1 + wilr2 + wilr3)*TIBz +
                      WPosAffLALeadLag1 +
                      WeekDay + CPDzc18 +
                      Age + Sex + RACE3G + BMI + SES_1 + 
                      # CurrentWork + SmokingStatus + AUDITCat + 
                      (1 + WPosAffLALeadLag1 + ilr1 + ilr2 + ilr3 + TIBz | ID),
                    iter = 6000, chains = 8, cores = 8, seed = 123, warmup = 1000,
                    backend = "cmdstanr", save_pars = save_pars(all = TRUE))
saveRDS(mx4_lapa, paste0(outputdir, "mx4_lapa", ".RDS"))

mx4_hana <- brmcoda(complr = cilrw_hana,
                    formula = NegAffHALead ~ 
                      (bilr1 + bilr2 + bilr3 + wilr1 + wilr2 + wilr3)*TIBz +
                      WNegAffHALeadLag1 +
                      WeekDay + CPDzc18 +
                      Age + Sex + RACE3G + BMI + SES_1 + 
                      # CurrentWork + SmokingStatus + AUDITCat + 
                      (1 + WNegAffHALeadLag1 + ilr1 + ilr2 + ilr3 + TIBz | ID),
                    iter = 6000, chains = 8, cores = 8, seed = 123, warmup = 1000,
                    backend = "cmdstanr", save_pars = save_pars(all = TRUE))
saveRDS(mx4_hana, paste0(outputdir, "mx4_hana", ".RDS"))

mx4_lana <- brmcoda(complr = cilrw_lana,
                    formula = NegAffLALead ~ 
                      (bilr1 + bilr2 + bilr3 + wilr1 + wilr2 + wilr3)*TIBz +
                      WNegAffLALeadLag1 +
                      WeekDay + CPDzc18 +
                      Age + Sex + RACE3G + BMI + SES_1 + 
                      # CurrentWork + SmokingStatus + AUDITCat + 
                      (1 + WNegAffLALeadLag1 + ilr1 + ilr2 + ilr3 + TIBz | ID),
                    iter = 6000, chains = 8, cores = 8, seed = 123, warmup = 1000,
                    backend = "cmdstanr", save_pars = save_pars(all = TRUE))
saveRDS(mx4_lana, paste0(outputdir, "mx4_lana", ".RDS"))

## pivot coordinates
m_hapa_pc <- pivot_coord(m_hapa, method = "rotate")
m_lapa_pc <- pivot_coord(m_lapa, method = "rotate")
m_hana_pc <- pivot_coord(m_hana, method = "rotate")
m_lana_pc <- pivot_coord(m_lana, method = "rotate")

summary(m_hapa_pc)
summary(m_lapa_pc)
summary(m_hana_pc)
summary(m_lana_pc)
