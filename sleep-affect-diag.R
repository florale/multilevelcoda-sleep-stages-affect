source("sleep-affect-utils.R")

library(multilevelcoda)
library(brms)

mb_hapa <- readRDS(paste0(outputdir, "mb_hapa", ".RDS"))
mb_lapa <- readRDS(paste0(outputdir, "mb_lapa", ".RDS"))
m_hana <- readRDS(paste0(outputdir, "m_hana", ".RDS"))
mw_lana <- readRDS(paste0(outputdir, "mw_lana", ".RDS"))

plot(mb_hapa, regex = TRUE)
plot(mb_lapa, regex = TRUE)
plot(m_hana, regex = TRUE)
plot(mw_lana, regex = TRUE)

pp_check(mb_hapa$Model, ndraws = 100, type = "stat")
pp_check(mb_lapa$Model, ndraws = 100, type = "stat")
pp_check(m_hana$model, ndraws = 100, type = "stat")
pp_check(mw_lana$Model, ndraws = 100, type = "stat")
