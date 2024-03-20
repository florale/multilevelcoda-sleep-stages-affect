# MODELS --------------------
m_hapa <- readRDS(paste0(outputdir, "m_hapa", ".RDS"))
m_lapa <- readRDS(paste0(outputdir, "m_lapa", ".RDS"))
m_hana <- readRDS(paste0(outputdir, "m_hana", ".RDS"))
m_lana <- readRDS(paste0(outputdir, "m_lana", ".RDS"))

mb_hapa <- readRDS(paste0(outputdir, "mb_hapa", ".RDS"))
mb_lapa <- readRDS(paste0(outputdir, "mb_lapa", ".RDS"))
mb_hana <- readRDS(paste0(outputdir, "mb_hana", ".RDS"))
mb_lana <- readRDS(paste0(outputdir, "mb_lana", ".RDS"))

mw_hapa <- readRDS(paste0(outputdir, "mw_hapa", ".RDS"))
mw_lapa <- readRDS(paste0(outputdir, "mw_lapa", ".RDS"))
mw_hana <- readRDS(paste0(outputdir, "mw_hana", ".RDS"))
mw_lana <- readRDS(paste0(outputdir, "mw_lana", ".RDS"))

m0_hapa <- readRDS(paste0(outputdir, "m0_hapa", ".RDS"))
m0_lapa <- readRDS(paste0(outputdir, "m0_lapa", ".RDS"))
m0_hana <- readRDS(paste0(outputdir, "m0_hana", ".RDS"))
m0_lana <- readRDS(paste0(outputdir, "m0_lana", ".RDS"))

mx1_hapa <- readRDS(paste0(outputdir, "mx1_hapa", ".RDS"))
mx1_lapa <- readRDS(paste0(outputdir, "mx1_lapa", ".RDS"))
mx1_hana <- readRDS(paste0(outputdir, "mx1_hana", ".RDS"))
mx1_lana <- readRDS(paste0(outputdir, "mx1_lana", ".RDS"))

mx2_hapa <- readRDS(paste0(outputdir, "mx2_hapa", ".RDS"))
mx2_lapa <- readRDS(paste0(outputdir, "mx2_lapa", ".RDS"))
mx2_hana <- readRDS(paste0(outputdir, "mx2_hana", ".RDS"))
mx2_lana <- readRDS(paste0(outputdir, "mx2_lana", ".RDS"))

mx3_hapa <- readRDS(paste0(outputdir, "mx3_hapa", ".RDS"))
mx3_lapa <- readRDS(paste0(outputdir, "mx3_lapa", ".RDS"))
mx3_hana <- readRDS(paste0(outputdir, "mx3_hana", ".RDS"))
mx3_lana <- readRDS(paste0(outputdir, "mx3_lana", ".RDS"))

mx4_hapa <- readRDS(paste0(outputdir, "mx4_hapa", ".RDS"))
mx4_lapa <- readRDS(paste0(outputdir, "mx4_lapa", ".RDS"))
mx4_hana <- readRDS(paste0(outputdir, "mx4_hana", ".RDS"))
mx4_lana <- readRDS(paste0(outputdir, "mx4_lana", ".RDS"))

# compute LOO ----------------------
loo_m_hapa <- loo(m_hapa$Model, cores = 8, reloo = TRUE)
loo_m_lapa <- loo(m_lapa$Model, cores = 8, reloo = TRUE)
loo_m_hana <- loo(m_hana$Model, cores = 8, reloo = TRUE)
loo_m_lana <- loo(m_lana$Model, cores = 8, reloo = TRUE)

loo_mb_hapa <- loo(mb_hapa$Model, cores = 8, reloo = TRUE)
loo_mb_lapa <- loo(mb_lapa$Model, cores = 8, reloo = TRUE)
loo_mb_hana <- loo(mb_hana$Model, cores = 8, reloo = TRUE)
loo_mb_lana <- loo(mb_lana$Model, cores = 8, reloo = TRUE)

loo_mw_hapa <- loo(mw_hapa$Model, cores = 8, reloo = TRUE)
loo_mw_lapa <- loo(mw_lapa$Model, cores = 8, reloo = TRUE)
loo_mw_hana <- loo(mw_hana$Model, cores = 8, reloo = TRUE)
loo_mw_lana <- loo(mw_lana$Model, cores = 8, reloo = TRUE)

loo_m0_hapa <- loo(m0_hapa$Model, cores = 8, reloo = TRUE)
loo_m0_lapa <- loo(m0_lapa$Model, cores = 8, reloo = TRUE)
loo_m0_hana <- loo(m0_hana$Model, cores = 8, reloo = TRUE)
loo_m0_lana <- loo(m0_lana$Model, cores = 8, reloo = TRUE)

loo_mx1_hapa <- loo(mx1_hapa$Model, cores = 8, reloo = TRUE)
loo_mx1_lapa <- loo(mx1_lapa$Model, cores = 8, reloo = TRUE)
loo_mx1_hana <- loo(mx1_hana$Model, cores = 8, reloo = TRUE)
loo_mx1_lana <- loo(mx1_lana$Model, cores = 8, reloo = TRUE)

loo_mx2_hapa <- loo(mx2_hapa$Model, cores = 8, reloo = TRUE)
loo_mx2_lapa <- loo(mx2_lapa$Model, cores = 8, reloo = TRUE)
loo_mx2_hana <- loo(mx2_hana$Model, cores = 8, reloo = TRUE)
loo_mx2_lana <- loo(mx2_lana$Model, cores = 8, reloo = TRUE)

loo_mx3_hapa <- loo(mx3_hapa$Model, cores = 8, reloo = TRUE)
loo_mx3_lapa <- loo(mx3_lapa$Model, cores = 8, reloo = TRUE)
loo_mx3_hana <- loo(mx3_hana$Model, cores = 8, reloo = TRUE)
loo_mx3_lana <- loo(mx3_lana$Model, cores = 8, reloo = TRUE)

loo_mx4_hapa <- loo(mx4_hapa$Model, cores = 8, reloo = TRUE)
loo_mx4_lapa <- loo(mx4_lapa$Model, cores = 8, reloo = TRUE)
loo_mx4_hana <- loo(mx4_hana$Model, cores = 8, reloo = TRUE)
loo_mx4_lana <- loo(mx4_lana$Model, cores = 8, reloo = TRUE)

# saveRDS(loo_m_hapa, paste0(outputdir, "loo_m_hapa", ".RDS"))
# saveRDS(loo_m_lapa, paste0(outputdir, "loo_m_lapa", ".RDS"))
# saveRDS(loo_m_hana, paste0(outputdir, "loo_m_hana", ".RDS"))
# saveRDS(loo_m_lana, paste0(outputdir, "loo_m_lana", ".RDS"))
# 
# saveRDS(loo_mb_hapa, paste0(outputdir, "loo_mb_hapa", ".RDS"))
# saveRDS(loo_mb_lapa, paste0(outputdir, "loo_mb_lapa", ".RDS"))
# saveRDS(loo_mb_hana, paste0(outputdir, "loo_mb_hana", ".RDS"))
# saveRDS(loo_mb_lana, paste0(outputdir, "loo_mb_lana", ".RDS"))
# 
# saveRDS(loo_mw_hapa, paste0(outputdir, "loo_mw_hapa", ".RDS"))
# saveRDS(loo_mw_lapa, paste0(outputdir, "loo_mw_lapa", ".RDS"))
# saveRDS(loo_mw_hana, paste0(outputdir, "loo_mw_hana", ".RDS"))
# saveRDS(loo_mw_lana, paste0(outputdir, "loo_mw_lana", ".RDS"))
# 
# saveRDS(loo_m0_hapa, paste0(outputdir, "loo_m0_hapa", ".RDS"))
# saveRDS(loo_m0_lapa, paste0(outputdir, "loo_m0_lapa", ".RDS"))
# saveRDS(loo_m0_hana, paste0(outputdir, "loo_m0_hana", ".RDS"))
# saveRDS(loo_m0_lana, paste0(outputdir, "loo_m0_lana", ".RDS"))
# 
# saveRDS(loo_mx1_hapa, paste0(outputdir, "loo_mx1_hapa", ".RDS"))
# saveRDS(loo_mx1_lapa, paste0(outputdir, "loo_mx1_lapa", ".RDS"))
# saveRDS(loo_mx1_hana, paste0(outputdir, "loo_mx1_hana", ".RDS"))
# saveRDS(loo_mx1_lana, paste0(outputdir, "loo_mx1_lana", ".RDS"))
# 
# saveRDS(loo_mx2_hapa, paste0(outputdir, "loo_mx2_hapa", ".RDS"))
# saveRDS(loo_mx2_lapa, paste0(outputdir, "loo_mx2_lapa", ".RDS"))
# saveRDS(loo_mx2_hana, paste0(outputdir, "loo_mx2_hana", ".RDS"))
# saveRDS(loo_mx2_lana, paste0(outputdir, "loo_mx2_lana", ".RDS"))
# 
# saveRDS(loo_mx3_hapa, paste0(outputdir, "loo_mx3_hapa", ".RDS"))
# saveRDS(loo_mx3_lapa, paste0(outputdir, "loo_mx3_lapa", ".RDS"))
# saveRDS(loo_mx3_hana, paste0(outputdir, "loo_mx3_hana", ".RDS"))
# saveRDS(loo_mx3_lana, paste0(outputdir, "loo_mx3_lana", ".RDS"))
# 
# saveRDS(loo_mx4_hapa, paste0(outputdir, "loo_mx4_hapa", ".RDS"))
# saveRDS(loo_mx4_lapa, paste0(outputdir, "loo_mx4_lapa", ".RDS"))
# saveRDS(loo_mx4_hana, paste0(outputdir, "loo_mx4_hana", ".RDS"))
# saveRDS(loo_mx4_lana, paste0(outputdir, "loo_mx4_lana", ".RDS"))

# MODEL COMPARE ------------------
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

# interaction -----------------
loo_comp_hapa_x <- cbind(loo_mx1_hapa$pointwise[,"elpd_loo"],
                         loo_mx2_hapa$pointwise[,"elpd_loo"],
                         # loo_mx3_hapa$pointwise[,"elpd_loo"],
                         loo_mx4_hapa$pointwise[,"elpd_loo"],
                         loo_m_hapa$pointwise[,"elpd_loo"])

loo_comp_lapa_x <- cbind(loo_mx1_lapa$pointwise[,"elpd_loo"],
                         loo_mx2_lapa$pointwise[,"elpd_loo"],
                         # loo_mx3_lapa$pointwise[,"elpd_loo"],
                         loo_mx4_lapa$pointwise[,"elpd_loo"],
                         loo_m_lapa$pointwise[,"elpd_loo"])

loo_comp_hana_x <- cbind(loo_mx1_hana$pointwise[,"elpd_loo"],
                         loo_mx2_hana$pointwise[,"elpd_loo"],
                         # loo_mx3_hana$pointwise[,"elpd_loo"],
                         loo_mx4_hana$pointwise[,"elpd_loo"],
                         loo_m_hana$pointwise[,"elpd_loo"])

loo_comp_lana_x <- cbind(loo_mx1_lana$pointwise[,"elpd_loo"],
                         loo_mx2_lana$pointwise[,"elpd_loo"],
                         # loo_mx3_lana$pointwise[,"elpd_loo"],
                         loo_mx4_lana$pointwise[,"elpd_loo"],
                         loo_m_lana$pointwise[,"elpd_loo"])

loo::stacking_weights(loo_comp_hapa_x)
loo::stacking_weights(loo_comp_lapa_x)
loo::stacking_weights(loo_comp_hana_x)
loo::stacking_weights(loo_comp_lana_x)

loo::loo_compare(loo_m_hapa,
                 loo_mx1_hapa,
                 loo_mx2_hapa,
                 # loo_mx3_hapa,
                 loo_mx4_hapa)
loo::loo_compare(loo_m_lapa,
                 loo_mx1_lapa,
                 loo_mx2_lapa,
                 # loo_mx3_lapa,
                 loo_mx4_lapa)
loo::loo_compare(loo_m_hana,
                 loo_mx1_hana,
                 loo_mx2_hana,
                 # loo_mx3_hana,
                 loo_mx4_hana)
loo::loo_compare(loo_m_lana,
                 loo_mx1_lana,
                 loo_mx2_lana,
                 # loo_mx3_lana,
                 loo_mx4_lana)

# full vs between vs within vs null -----------------
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

loo::stacking_weights(lpd_point_hapa)
loo::stacking_weights(lpd_point_lapa)
loo::stacking_weights(lpd_point_hana)
loo::stacking_weights(lpd_point_lana)

loo_compare_hapa <- loo::loo_compare(loo_m0_hapa, loo_mb_hapa, loo_mw_hapa, loo_m_hapa)
loo_compare_lapa <- loo::loo_compare(loo_m0_lapa, loo_mb_lapa, loo_mw_lapa, loo_m_lapa)
loo_compare_hana <- loo::loo_compare(loo_m0_hana, loo_mb_hana, loo_mw_hana, loo_m_hana)
loo_compare_lana <- loo::loo_compare(loo_m0_lana, loo_mb_lana, loo_mw_lana, loo_m_lana)

print(loo_compare_hapa, digits = 3)
print(loo_compare_lapa, digits = 3)
print(loo_compare_hana, digits = 3)
print(loo_compare_lana, digits = 3)


# bayes factor --------------------
## full vs between vs within vs null -----------------

comparison_hapa <- bayesfactor_models(m_hapa$Model, mb_hapa$Model, mw_hapa$Model, denominator = m0_hapa$Model)
comparison_lapa <- bayesfactor_models(m_lapa$Model, mb_lapa$Model, mw_lapa$Model, denominator = m0_lapa$Model)
comparison_hana <- bayesfactor_models(m_hana$Model, mb_hana$Model, mw_hana$Model, denominator = m0_hana$Model)
comparison_lana <- bayesfactor_models(m_lana$Model, mb_lana$Model, mw_lana$Model, denominator = m0_lana$Model)

comparison_hapa
comparison_lapa
comparison_hana
comparison_lana

update(comparison_hapa, reference = 1)
update(comparison_lapa, reference = 1)
update(comparison_hana, reference = 1)
update(comparison_lana, reference = 1)

bayes_factor(m0_hana, mw_hana)

## interaction ----------
comparison_hapax <- bayesfactor_models(mx4_hapa,
                                       # mx1_hapa,
                                       # mx2_hapa,
                                       # loo_mx3_hapa,
                                       denominator = m_hapa)
comparison_lapax <- bayesfactor_models(mx4_lapa,
                                       mx1_lapa,
                                       mx2_lapa,
                                       # loo_mx3_lapa,
                                       denominator = m_lapa)
comparison_hanax <- bayesfactor_models(mx4_hana,
                                       mx1_hana,
                                       mx2_hana,
                                       # loo_mx3_hana,
                                       denominator = m_hana)
comparison_lanax <- bayesfactor_models(mx4_lana,
                                       mx1_lana,
                                       mx2_lana,
                                       # loo_mx3_lana,
                                       denominator = m_lana)

comparison_hapax
comparison_lapax
comparison_hanax
comparison_lanax

comparison_hapax4 <- bayes_factor(mx4_hapa, m_hapa)
comparison_hapax1 <- bayes_factor(mx1_hapa, m_hapa)
comparison_hapax2 <- bayes_factor(mx2_hapa, m_hapa)

comparison_lapax4 <- bayes_factor(mx4_lapa, m_lapa)
comparison_lapax1 <- bayes_factor(mx1_lapa, m_lapa)
comparison_lapax2 <- bayes_factor(mx2_lapa, m_lapa)

comparison_hanax4 <- bayes_factor(mx4_hana, m_hana)
comparison_hanax1 <- bayes_factor(mx1_hana, m_hana, recompile = TRUE)
comparison_hanax2 <- bayes_factor(mx2_hana, m_hana)

comparison_lanax4 <- bayes_factor(mx4_lana, m_lana)
comparison_lanax1 <- bayes_factor(mx1_lana, m_lana)
comparison_lanax2 <- bayes_factor(mx2_lana, m_lana)

comparison_hapax4
comparison_hapax1
comparison_hapax2

comparison_lapax4
comparison_lapax1
comparison_lapax2

comparison_hanax4
comparison_hanax1
comparison_hanax2

comparison_lanax4
comparison_lanax1
comparison_lanax2
