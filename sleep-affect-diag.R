
library(multilevelcoda)
library(brms)

m_hapa <- readRDS(paste0(outputdir, "m_hapa", ".RDS"))
m_lapa <- readRDS(paste0(outputdir, "m_lapa", ".RDS"))
m_hana <- readRDS(paste0(outputdir, "m_hana", ".RDS"))
m_lana <- readRDS(paste0(outputdir, "m_lana", ".RDS"))

plot(m_hapa, variable = c("b_Intercept", "b_bilr1", "b_bilr2", "sd_ID__Intercept"), regex = TRUE)
pp_check(m_hapa$Model, ndraws = 100)
pp_check(m_lapa$Model, ndraws = 100)
pp_check(m_hana$Model, ndraws = 100)
pp_check(m_lana$Model, ndraws = 100)
