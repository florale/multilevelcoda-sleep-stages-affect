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


# standardised coefs ------------------
m_hapa_sub_adj <- readRDS(paste0(outputdir, "m_hapa_sub_adj", ".RDS"))
m_lapa_sub_adj <- readRDS(paste0(outputdir, "m_lapa_sub_adj", ".RDS"))
m_hana_sub_adj <- readRDS(paste0(outputdir, "m_hana_sub_adj", ".RDS"))
m_lana_sub_adj <- readRDS(paste0(outputdir, "m_lana_sub_adj", ".RDS"))


egltable(c(
  "PosAffHALead", "PosAffLALead", "NegAffHALead", "NegAffLALead",
  "BPosAffHALead", "BPosAffLALead", "BNegAffHALead", "BNegAffLALead",
  "WPosAffHALead", "WPosAffLALead", "WNegAffHALead", "WNegAffLALead"
),
idvar = "ID",
data = d[Survey == "Wake"][, (fvars) := lapply(.SD, as.factor), .SDcols = fvars]
)

d_sub_hapa <- as.data.table(summary(m_hapa_sub_adj, delta = 30, digits = "asis"))
d_sub_lapa <- as.data.table(summary(m_lapa_sub_adj, delta = 30, digits = "asis"))
d_sub_hana <- as.data.table(summary(m_hana_sub_adj, delta = 30, digits = "asis"))
d_sub_lana <- as.data.table(summary(m_lana_sub_adj, delta = 30, digits = "asis"))

d_sub_hapa[Level == "between", SMean := round(Mean / sd(d[Survey == "Wake"]$BPosAffHALead, na.rm = T), 2)]
d_sub_hapa[Level == "within", SMean := round(Mean / sd(d[Survey == "Wake"]$WPosAffHALead, na.rm = T), 2)]

d_sub_lapa[Level == "between", SMean := round(Mean / sd(d[Survey == "Wake"]$BPosAffLALead, na.rm = T), 2)]
d_sub_lapa[Level == "within", SMean := round(Mean / sd(d[Survey == "Wake"]$WPosAffLALead, na.rm = T), 2)]

d_sub_hana[Level == "between", SMean := round(Mean / sd(d[Survey == "Wake"]$BNegAffHALead, na.rm = T), 2)]
d_sub_hana[Level == "within", SMean := round(Mean / sd(d[Survey == "Wake"]$WNegAffHALead, na.rm = T), 2)]

d_sub_lana[Level == "between", SMean := round(Mean / sd(d[Survey == "Wake"]$BNegAffLALead, na.rm = T), 2)]
d_sub_lana[Level == "within", SMean := round(Mean / sd(d[Survey == "Wake"]$WNegAffLALead, na.rm = T), 2)]

d_sub_hapa
d_sub_lapa
d_sub_hana
d_sub_lana
