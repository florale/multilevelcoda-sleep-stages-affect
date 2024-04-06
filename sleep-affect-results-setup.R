source("sleep-affect-utils.R")

# brmcoda
m_hapa <- readRDS(paste0(outputdir, "m_hapa", ".RDS"))
m_lapa <- readRDS(paste0(outputdir, "m_lapa", ".RDS"))
m_hana <- readRDS(paste0(outputdir, "m_hana", ".RDS"))
m_lana <- readRDS(paste0(outputdir, "m_lana", ".RDS"))

# substitution
m_hapa_sub_adj <- readRDS(paste0(outputdir, "m_hapa_sub_adj", ".RDS"))
m_lapa_sub_adj <- readRDS(paste0(outputdir, "m_lapa_sub_adj", ".RDS"))
m_hana_sub_adj <- readRDS(paste0(outputdir, "m_hana_sub_adj", ".RDS"))
m_lana_sub_adj <- readRDS(paste0(outputdir, "m_lana_sub_adj", ".RDS"))

affect_sub_list <- lapply(list(
  m_hapa_sub_adj, m_lapa_sub_adj, m_hana_sub_adj, m_lana_sub_adj
), function(X) {
  
  X <- summary(X, delta = 30)
  X[, From := ifelse(From == "SleepDeep", "Slow Wave Sleep", From)]
  X[, From := ifelse(From == "SleepREM", "REM Sleep", From)]
  X[, From := ifelse(From == "SleepLight", "Light Sleep", From)]
  X[, From := ifelse(From == "WAKE", "Total Wake Time", From)]
  
  X[, To := ifelse(To == "SleepDeep", "Slow Wave Sleep", To)]
  X[, To := ifelse(To == "SleepREM", "REM Sleep", To)]
  X[, To := ifelse(To == "SleepLight", "Light Sleep", To)]
  X[, To := ifelse(To == "WAKE", "Total Wake Time", To)]
})

# graphs
sleep_affect_plot_b <- readRDS(paste0(outputdir, "sleep_affect_plot_b", ".RDS"))
sleep_affect_plot_w <- readRDS(paste0(outputdir, "sleep_affect_plot_w", ".RDS"))

names <- c(`WAKE` = "Total Wake Time",
           `SleepLight` = "Light Sleep",
           `SleepDeep` = "Slow Wave Sleep",
           `SleepREM` = "REM Sleep")
labeller <- function(variable,value){
  return(names[value])
}

# loo
loo_m_hapa <- readRDS(paste0(outputdir, "loo_m_hapa", ".RDS"))
loo_m_lapa <- readRDS(paste0(outputdir, "loo_m_lapa", ".RDS"))
loo_m_hana <- readRDS(paste0(outputdir, "loo_m_hana", ".RDS"))
loo_m_lana <- readRDS(paste0(outputdir, "loo_m_lana", ".RDS"))

loo_mb_hapa <- readRDS(paste0(outputdir, "loo_mb_hapa", ".RDS"))
loo_mb_lapa <- readRDS(paste0(outputdir, "loo_mb_lapa", ".RDS"))
loo_mb_hana <- readRDS(paste0(outputdir, "loo_mb_hana", ".RDS"))
loo_mb_lana <- readRDS(paste0(outputdir, "loo_mb_lana", ".RDS"))

loo_mw_hapa <- readRDS(paste0(outputdir, "loo_mw_hapa", ".RDS"))
loo_mw_lapa <- readRDS(paste0(outputdir, "loo_mw_lapa", ".RDS"))
loo_mw_hana <- readRDS(paste0(outputdir, "loo_mw_hana", ".RDS"))
loo_mw_lana <- readRDS(paste0(outputdir, "loo_mw_lana", ".RDS"))

loo_m0_hapa <- readRDS(paste0(outputdir, "loo_m0_hapa", ".RDS"))
loo_m0_lapa <- readRDS(paste0(outputdir, "loo_m0_lapa", ".RDS"))
loo_m0_hana <- readRDS(paste0(outputdir, "loo_m0_hana", ".RDS"))
loo_m0_lana <- readRDS(paste0(outputdir, "loo_m0_lana", ".RDS"))

loo_mx1_hapa <- readRDS(paste0(outputdir, "loo_mx1_hapa", ".RDS"))
loo_mx1_lapa <- readRDS(paste0(outputdir, "loo_mx1_lapa", ".RDS"))
loo_mx1_hana <- readRDS(paste0(outputdir, "loo_mx1_hana", ".RDS"))
loo_mx1_lana <- readRDS(paste0(outputdir, "loo_mx1_lana", ".RDS"))

loo_mx2_hapa <- readRDS(paste0(outputdir, "loo_mx2_hapa", ".RDS"))
loo_mx2_lapa <- readRDS(paste0(outputdir, "loo_mx2_lapa", ".RDS"))
loo_mx2_hana <- readRDS(paste0(outputdir, "loo_mx2_hana", ".RDS"))
loo_mx2_lana <- readRDS(paste0(outputdir, "loo_mx2_lana", ".RDS"))

loo_mx3_hapa <- readRDS(paste0(outputdir, "loo_mx3_hapa", ".RDS"))
loo_mx3_lapa <- readRDS(paste0(outputdir, "loo_mx3_lapa", ".RDS"))
loo_mx3_hana <- readRDS(paste0(outputdir, "loo_mx3_hana", ".RDS"))
loo_mx3_lana <- readRDS(paste0(outputdir, "loo_mx3_lana", ".RDS"))

loo_mx4_hapa <- readRDS(paste0(outputdir, "loo_mx4_hapa", ".RDS"))
loo_mx4_lapa <- readRDS(paste0(outputdir, "loo_mx4_lapa", ".RDS"))
loo_mx4_hana <- readRDS(paste0(outputdir, "loo_mx4_hana", ".RDS"))
loo_mx4_lana <- readRDS(paste0(outputdir, "loo_mx4_lana", ".RDS"))

lpd_point_hapa <- cbind(loo_m0_hapa$pointwise[,"elpd_loo"],
                        loo_mb_hapa$pointwise[,"elpd_loo"],
                        loo_mw_hapa$pointwise[,"elpd_loo"],
                        loo_m_hapa$pointwise[,"elpd_loo"])

lpd_point_lapa <- cbind(loo_m0_lapa$pointwise[,"elpd_loo"],
                        loo_mb_lapa$pointwise[,"elpd_loo"],
                        loo_mw_lapa$pointwise[,"elpd_loo"],
                        loo_m_lapa$pointwise[,"elpd_loo"])

lpd_point_hana <- cbind(loo_m0_hana$pointwise[,"elpd_loo"],
                        loo_mb_hana$pointwise[,"elpd_loo"],
                        loo_mw_hana$pointwise[,"elpd_loo"],
                        loo_m_hana$pointwise[,"elpd_loo"])

lpd_point_lana <- cbind(loo_m0_lana$pointwise[,"elpd_loo"],
                        loo_mb_lana$pointwise[,"elpd_loo"],
                        loo_mw_lana$pointwise[,"elpd_loo"],
                        loo_m_lana$pointwise[,"elpd_loo"])

stacking_hapa <- loo::stacking_weights(lpd_point_hapa)
stacking_lapa <- loo::stacking_weights(lpd_point_lapa)
stacking_hana <- loo::stacking_weights(lpd_point_hana)
stacking_lana <- loo::stacking_weights(lpd_point_lana)

loo_compare_hapa <- loo::loo_compare(loo_m0_hapa, loo_mb_hapa, loo_mw_hapa, loo_m_hapa)
loo_compare_lapa <- loo::loo_compare(loo_m0_lapa, loo_mb_lapa, loo_mw_lapa, loo_m_lapa)
loo_compare_hana <- loo::loo_compare(loo_m0_hana, loo_mb_hana, loo_mw_hana, loo_m_hana)
loo_compare_lana <- loo::loo_compare(loo_m0_lana, loo_mb_lana, loo_mw_lana, loo_m_lana)
