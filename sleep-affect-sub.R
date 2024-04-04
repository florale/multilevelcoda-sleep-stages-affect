source("sleep-affect-utils.R")

## predicting outcomes at wake  adj for all  ---------------------
m_hapa <- readRDS(paste0(outputdir, "m_hapa", ".RDS"))
m_lapa <- readRDS(paste0(outputdir, "m_lapa", ".RDS"))
m_hana <- readRDS(paste0(outputdir, "m_hana", ".RDS"))
m_lana <- readRDS(paste0(outputdir, "m_lana", ".RDS"))

m_hapa_sub_adj <- substitution(
  m_hapa,
  delta = c(1:60),
  # delta = 30,
  level = c("between", "within"),
  ref = "grandmean",
  cores = 5
)
m_lapa_sub_adj <- substitution(
  m_lapa,
  delta = c(1:60),
  # delta = 30,
  level = c("between", "within"),
  ref = "grandmean",
  cores = 5
)
m_hana_sub_adj <- substitution(
  m_hana,
  delta = c(1:60),
  # delta = 30,
  level = c("between", "within"),
  ref = "grandmean",
  cores = 5
)
m_lana_sub_adj <- substitution(
  m_lana,
  delta = c(1:60),
  # delta = 30,
  level = c("between", "within"),
  ref = "grandmean",
  cores = 5
)

summary(m_hapa_sub_adj, delta = 30)
summary(m_lapa_sub_adj, delta = 30)
summary(m_hana_sub_adj, delta = 30)
summary(m_lana_sub_adj, delta = 30)

saveRDS(m_hapa_sub_adj, paste0(outputdir, "m_hapa_sub_adj", ".RDS"))
saveRDS(m_lapa_sub_adj, paste0(outputdir, "m_lapa_sub_adj", ".RDS"))
saveRDS(m_hana_sub_adj, paste0(outputdir, "m_hana_sub_adj", ".RDS"))
saveRDS(m_lana_sub_adj, paste0(outputdir, "m_lana_sub_adj", ".RDS"))
