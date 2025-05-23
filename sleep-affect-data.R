source("sleep-affect-utils.R")
shs <- readRDS("/Volumes/shared/Behavioral-med-lab/StressHealthStudy/SHS Research Interns/Data/shs_all_ggir.RDS")
d <- as.data.table(shs)

# lead day average
d[, c("PosAffHADayLead", "PosAffLADayLead", "NegAffHADayLead", "NegAffLADayLead", 
      "PosAffDayLead", "NegAffDayLead", "STRESSDayLead") :=
    .SD[.(ID = ID, Survey = Survey, SurveyDay = SurveyDay + 1),
        .(PosAffHADay, PosAffLADay, NegAffHADay, NegAffLADay, 
          PosAffDay, NegAffDay, STRESSDay),
        on = c("ID", "SurveyDay", "Survey")]]

# lag day average
d[, c("PosAffHADayLag", "PosAffLADayLag", "NegAffHADayLag", "NegAffLADayLag", 
      "PosAffDayLag", "NegAffDayLag", "STRESSDayLag") :=
    .SD[.(ID = ID, Survey = Survey, SurveyDay = SurveyDay - 1),
        .(PosAffHADay, PosAffLADay, NegAffHADay, NegAffLADay, 
          PosAffDay, NegAffDay, STRESSDay),
        on = c("ID", "SurveyDay", "Survey")]]

# next(lead) day by survey - used variables
d[, c("PosAffHALead", "PosAffLALead", "NegAffHALead", "NegAffLALead", 
      "PosAffLead", "NegAffLead", "STRESSLead") :=
    .SD[.(ID = ID, Survey = Survey, SurveyDay = SurveyDay + 1),
        .(PosAffHA, PosAffLA, NegAffHA, NegAffLA, 
          PosAff, NegAff, STRESS),
        on = c("ID", "SurveyDay", "Survey")]]

# lag of lead day by survey
d[, c("PosAffHALeadLag1", "PosAffLALeadLag1", "NegAffHALeadLag1", "NegAffLALeadLag1", 
      "PosAffLeadLag1", "NegAffLeadLag1", "STRESSLeadLag1") :=
    .SD[.(ID = ID, USURVEYID = USURVEYID - 1),
        .(PosAffHALead, PosAffLALead, NegAffHALead, NegAffLALead, 
          PosAffLead, NegAffLead, STRESSLead),
        on = c("ID", "USURVEYID")]]

d[, c("PosAffHALeadLag2", "PosAffLALeadLag2", "NegAffHALeadLag2", "NegAffLALeadLag2", 
      "PosAffLeadLag2", "NegAffLeadLag2", "STRESSLeadLag2") :=
    .SD[.(ID = ID, USURVEYID = USURVEYID - 2),
        .(PosAffHALead, PosAffLALead, NegAffHALead, NegAffLALead, 
          PosAffLead, NegAffLead, STRESSLead),
        on = c("ID", "USURVEYID")]]

d[, IDxDay := paste(ID, StudyDay, sep = "-")]

# create between within affect
# d[, c("BPosAffHALeadLag", "WPosAffHALeadLag") := meanDeviations(PosAffHALeadLag), by = ID]
# d[, c("BPosAffLALeadLag", "WPosAffLALeadLag") := meanDeviations(PosAffLALeadLag), by = ID]
# d[, c("BNegAffHALeadLag", "WNegAffHALeadLag") := meanDeviations(NegAffHALeadLag), by = ID]
# d[, c("BNegAffLALeadLag", "WNegAffLALeadLag") := meanDeviations(NegAffLALeadLag), by = ID]
# d[, c("BSTRESSLeadLag", "WSTRESSLeadLag") := meanDeviations(STRESSLeadLag), by = ID]

d[, c("BPosAffHALead", "WPosAffHALead") := meanDeviations(PosAffHALead), by = ID]
d[, c("BPosAffLALead", "WPosAffLALead") := meanDeviations(PosAffLALead), by = ID]
d[, c("BNegAffHALead", "WNegAffHALead") := meanDeviations(NegAffHALead), by = ID]
d[, c("BNegAffLALead", "WNegAffLALead") := meanDeviations(NegAffLALead), by = ID]
d[, c("BPosAffLead", "WPosAffLead") := meanDeviations(PosAffLead), by = ID]
d[, c("BNegAffLead", "WNegAffLead") := meanDeviations(NegAffLead), by = ID]
d[, c("BSTRESSLead", "WSTRESSLead") := meanDeviations(STRESSLead), by = ID]

# # lag of lead day by survey decomposed
# d[, c("WPosAffHALeadLag1", "WPosAffLALeadLag1", "WNegAffHALeadLag1", "WNegAffLALeadLag1", 
#       "WPosAfLeadLag1", "WNegAffLeadLag1", "WSTRESSLeadLag1",
#       "BPosAffHALeadLag1", "BPosAffLALeadLag1", "BNegAffHALeadLag1", "BNegAffLALeadLag1", 
#       "BPosAfLeadLag1", "BNegAffLeadLag1", "BSTRESSLeadLag1") :=
#     .SD[.(ID = ID, USURVEYID = USURVEYID - 1),
#         .(WPosAffHALead, WPosAffLALead, WNegAffHALead, WNegAffLALead, 
#           WPosAffLead, WNegAffLead, WSTRESSLead,
#           BPosAffHALead, BPosAffLALead, BNegAffHALead, BNegAffLALead, 
#           BPosAffLead, BNegAffLead, BSTRESSLead
#         ),
#         on = c("ID", "USURVEYID")]]

d[, c("BPosAffHALeadLag1", "WPosAffHALeadLag1") := meanDeviations(PosAffHALeadLag1), by = ID]
d[, c("BPosAffLALeadLag1", "WPosAffLALeadLag1") := meanDeviations(PosAffLALeadLag1), by = ID]
d[, c("BNegAffHALeadLag1", "WNegAffHALeadLag1") := meanDeviations(NegAffHALeadLag1), by = ID]
d[, c("BNegAffLALeadLag1", "WNegAffLALeadLag1") := meanDeviations(NegAffLALeadLag1), by = ID]
d[, c("BPosAffLeadLag1", "WPosAffLeadLag1") := meanDeviations(PosAffLeadLag1), by = ID]
d[, c("BNegAffLeadLag1", "WNegAffLeadLag1") := meanDeviations(NegAffLeadLag1), by = ID]
d[, c("BSTRESSLeadLag1", "WSTRESSLeadLag1") := meanDeviations(STRESSLeadLag1), by = ID]


d[, c("BPosAffHALeadLag2", "WPosAffHALeadLag2") := meanDeviations(PosAffHALeadLag2), by = ID]
d[, c("BPosAffLALeadLag2", "WPosAffLALeadLag2") := meanDeviations(PosAffLALeadLag2), by = ID]
d[, c("BNegAffHALeadLag2", "WNegAffHALeadLag2") := meanDeviations(NegAffHALeadLag2), by = ID]
d[, c("BNegAffLALeadLag2", "WNegAffLALeadLag2") := meanDeviations(NegAffLALeadLag2), by = ID]
d[, c("BPosAffLeadLag2", "WPosAffLeadLag2") := meanDeviations(PosAffLeadLag2), by = ID]
d[, c("BNegAffLeadLag2", "WNegAffLeadLag2") := meanDeviations(NegAffLeadLag2), by = ID]
d[, c("BSTRESSLeadLag2", "WSTRESSLeadLag2") := meanDeviations(STRESSLeadLag2), by = ID]

d[, c("BPosAffHADayLag", "WPosAffHADayLag") := meanDeviations(PosAffHADayLag), by = ID]
d[, c("BPosAffLADayLag", "WPosAffLADayLag") := meanDeviations(PosAffLADayLag), by = ID]
d[, c("BNegAffHADayLag", "WNegAffHADayLag") := meanDeviations(NegAffHADayLag), by = ID]
d[, c("BNegAffLADayLag", "WNegAffLADayLag") := meanDeviations(NegAffLADayLag), by = ID]
d[, c("BPosAffDayLag", "WPosAffDayLag") := meanDeviations(PosAffDayLag), by = ID]
d[, c("BNegAffDayLag", "WNegAffDayLag") := meanDeviations(NegAffDayLag), by = ID]
d[, c("BSTRESSDayLag", "WSTRESSDayLag") := meanDeviations(STRESSDayLag), by = ID]

# prepare sleep data 
d[, SleepLight := first(na.omit(SleepLight)), c("ID", "SurveyDay")]
d[, SleepDeep := first(na.omit(SleepDeep)), c("ID", "SurveyDay")]
d[, SleepREM := first(na.omit(SleepREM)), c("ID", "SurveyDay")]
d[, WASORAWz := first(na.omit(WASORAWz)), c("ID", "SurveyDay")]
d[, SOLRAWz := first(na.omit(SOLRAWz)), c("ID", "SurveyDay")]
d[, TIBz := first(na.omit(TIBz)), c("ID", "SurveyDay")]

d[, TIBz := TIBz*60]
d[, SleepLight := SleepLight*60]
d[, SleepDeep := SleepDeep*60]
d[, SleepREM := SleepREM*60]
d[, Sleep := SleepLight + SleepDeep + SleepREM]

d[, TotalSleepz := SleepLight + SleepDeep + SleepREM + WASORAWz + SOLRAWz]
d[, WAKE := WASORAWz + SOLRAWz]
d[, c("BTIBz", "WTIBz") := meanDeviations(TIBz), by = ID]

d[, RACE3G := as.factor(RACE3G)]
# d[, CurrentWork := as.factor(CurrentWork)]
# d[, SmokingStatus := as.factor(SmokingStatus)]
# d[, CurrentSchool := as.factor(CurrentSchool)]
d[, AUDITCat := as.factor(AUDITCat)]

d[, CurrentWork := as.integer(CurrentWork == "Working")] # all is not working so removed from model
d[, SmokingStatus := as.integer(SmokingStatus == "Never")] # all is not working so removed from model
d[, CurrentSchool := as.integer(CurrentSchool == "In School")]

d[, WeekDay := NA]
d[, WeekDay := ifelse(DayofWeek %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"), 1, WeekDay)]
d[, WeekDay := ifelse(DayofWeek %in% c("Saturday", "Sunday"), 0, WeekDay)]

# Composite Phase Deviation
d[, CPDzc18 := first(na.omit(CPDzc18)), c("ID", "SurveyDay")]
# View(d[, .(ID, CPDzc18, Survey, SurveyDay, PosAffHALead)])

# check missing data
# 1086 is all
nrow(d[Survey == "Wake"])
nrow(d[Survey == "Wake"][complete.cases(PosAffHALead)])
nrow(d[Survey == "Wake"][complete.cases(PosAffLALead)])
nrow(d[Survey == "Wake"][complete.cases(NegAffHALead)])
nrow(d[Survey == "Wake"][complete.cases(NegAffLALead)])

nrow(d[Survey == "Wake"][complete.cases(SleepLight)])
nrow(d[Survey == "Wake"][complete.cases(SleepDeep)])
nrow(d[Survey == "Wake"][complete.cases(SleepREM)])
nrow(d[Survey == "Wake"][complete.cases(WAKE)])
nrow(d[Survey == "Wake"][complete.cases(TIBz)])

nrow(d[Survey == "Wake"][complete.cases(CPDzc18)])

nrow(d[Survey == "Wake"][complete.cases(Age)][!duplicated(ID)])

nrow(d[Survey == "Wake"][complete.cases(PosAffHA)])
nrow(d[Survey == "Wake"][complete.cases(PosAffLA)])
nrow(d[Survey == "Wake"][complete.cases(NegAffHA)])
nrow(d[Survey == "Wake"][complete.cases(NegAffLA)])

parts <- c("SleepLight", "SleepDeep", "SleepREM", "WAKE")

min(d$BTDATETIMEz, na.rm = TRUE)
max(d$BTDATETIMEz, na.rm = TRUE)

d <- d[complete.cases(d[, .(SleepLight, SleepDeep, SleepREM, WAKE)])]
composition_imp <- lrEM(d[, parts, with = FALSE], label = 0,dl = rep(1, 4), ini.cov = "multRepl")
d <- cbind(d[, -parts, with = FALSE], composition_imp)

hist(d$PosAffHALead)
hist(d$PosAffLALead)
hist(d$NegAffHALead)
hist(d$NegAffLALead)
# d <- d[, .(
#   ID, SurveyDay, Survey, 
#   PosAff, NegAff,
#   PosAffHA,  PosAffLA, NegAffHA, NegAffLA, 
#   PosAffLead, WPosAffLeadLag1,
#   NegAffLead, WNegAffLeadLag1,
#   PosAffHALead, WPosAffHALeadLag1, 
#   PosAffLALead, WPosAffLALeadLag1, 
#   NegAffHALead, WNegAffHALeadLag1, 
#   NegAffLALead, WNegAffLALeadLag1, 
#   STRESSLead, WSTRESSLeadLag1,
#   SleepLight, SleepDeep, SleepREM, WAKE, 
#   TIBz, BTIBz, WTIBz, CPDzc18,
#   DayofWeek, WeekDay,
#   Age, Sex, RACE3G, BMI, SES_1, CurrentWork, SmokingStatus, AUDITCat
# )]

# sequential binary partition
sbp4 <- matrix(c(
  1, -1, -1,-1,
  0, 1, -1, -1,
  0, 0, 1, -1), ncol = 4, byrow = TRUE)

sbp3 <- matrix(c(
  1, -1, -1,
  0, 1, -1), ncol = 3, byrow = TRUE)

# make composition and ilr
cilrw_hapa <- complr(d[Survey == "Wake"], sbp = sbp4, 
                     parts = c("SleepLight", "SleepDeep", "SleepREM", "WAKE"), total = 448)
cilrw_lapa <- complr(d[Survey == "Wake"], sbp = sbp4, 
                     parts = c("SleepLight", "SleepDeep", "SleepREM", "WAKE"), total = 448)
cilrw_hana <- complr(d[Survey == "Wake"], sbp = sbp4, 
                     parts = c("SleepLight", "SleepDeep", "SleepREM", "WAKE"), total = 448)
cilrw_lana <- complr(d[Survey == "Wake"], sbp = sbp4, 
                     parts = c("SleepLight", "SleepDeep", "SleepREM", "WAKE"), total = 448)

summary(cilrw_hapa)
summary(cilrw_lapa)
summary(cilrw_hana)
summary(cilrw_lana)


## Descriptive stats ---------------------------

fvars <- c("Sex", "SmokingStatus", "CurrentWork", "AUDITCat", "DEDUUniPlus", "WeekDay", "BornAUS")
egltable(c("PosAffHALead", "PosAffLALead", "NegAffHALead", "NegAffLALead",
           "PosAffHALeadLag1", "PosAffLALeadLag1", "NegAffHALeadLag1", "NegAffLALeadLag1",
           "SleepLight", "SleepDeep", "SleepREM", "WAKE", "TIBz",
           "WeekDay", "CPDzc18",
           "SmokingStatus", "CurrentWork", "AUDITCat", "DEDUUniPlus", "BornAUS",
           "Age", "Sex", "RACE3G", "BMI", "SES_1" ),
         idvar = "ID",
         data = d[Survey == "Wake"][, (fvars) := lapply(.SD, as.factor), .SDcols = fvars]
)

# range
psych::describe(d$Age)
psych::describe(d$SES_1)
psych::describe(d$BMI)

psych::describe(d$SleepLight)

# ICC
multilevelTools::iccMixed(c("PosAffHALead"), id = "ID", data = d[Survey == "Wake"])
multilevelTools::iccMixed(c("PosAffLALead"), id = "ID", data = d[Survey == "Wake"])
multilevelTools::iccMixed(c("NegAffHALead"), id = "ID", data = d[Survey == "Wake"])
multilevelTools::iccMixed(c("NegAffLALead"), id = "ID", data = d[Survey == "Wake"])

multilevelTools::iccMixed(c("SleepLight"), id = "ID", data = d[Survey == "Wake"])
multilevelTools::iccMixed(c("SleepDeep"), id = "ID", data = d[Survey == "Wake"])
multilevelTools::iccMixed(c("SleepREM"), id = "ID", data = d[Survey == "Wake"])
multilevelTools::iccMixed(c("WAKE"), id = "ID", data = d[Survey == "Wake"])
multilevelTools::iccMixed(c("TIBz"), id = "ID", data = d[Survey == "Wake"])

multilevelTools::iccMixed(c("CPDzc18"), id = "ID", data = d[Survey == "Wake"])

# nobs
nrow(d[Survey == "Wake"][complete.cases(PosAffHALead)])
nrow(d[Survey == "Wake"][complete.cases(PosAffLALead)])
nrow(d[Survey == "Wake"][complete.cases(NegAffHALead)])
nrow(d[Survey == "Wake"][complete.cases(NegAffLALead)])

nrow(d[Survey == "Wake"][complete.cases(SleepLight)])
nrow(d[Survey == "Wake"][complete.cases(SleepDeep)])
nrow(d[Survey == "Wake"][complete.cases(SleepREM)])
nrow(d[Survey == "Wake"][complete.cases(WAKE)])
nrow(d[Survey == "Wake"][complete.cases(TIBz)])

nrow(d[Survey == "Wake"][complete.cases(CPDzc18)])

nrow(d[Survey == "Wake"][complete.cases(Age)][!duplicated(ID)])

nrow(d[Survey == "Wake"][complete.cases(PosAffHA)])
nrow(d[Survey == "Wake"][complete.cases(PosAffLA)])
nrow(d[Survey == "Wake"][complete.cases(NegAffHA)])
nrow(d[Survey == "Wake"][complete.cases(NegAffLA)])


# plot observed data
d_plot_by_sleep <- melt(d[Survey == "Wake"], id.vars = c("ID", "PosAffHALead", "PosAffLALead", "NegAffHALead", "NegAffLALead"), 
                        measure.vars = c("SleepLight", "SleepDeep", "SleepREM", "WAKE"),
                        variable.name = "Sleep",
                        value.name = "Minutes")

d_plot_by_affect <- melt(d[Survey == "Wake"], id.vars = c("SleepLight", "SleepDeep", "SleepREM", "WAKE"), 
                         measure.vars = c("PosAffHALead", "PosAffLALead", "NegAffHALead", "NegAffLALead"),
                         variable.name = "Affect",
                         value.name = "Rating")

d_plot_by_sleep[Sleep == "SleepLight", Sleep := "Light"]
d_plot_by_sleep[Sleep == "SleepDeep", Sleep := "SWS"]
d_plot_by_sleep[Sleep == "SleepREM", Sleep := "REM"]
d_plot_by_sleep[Sleep == "WAKE", Sleep := "TWT"]

col <- c(`TWT` = "#BBA9A7",
         `Light` = "#83A192",
         `SWS` = "#465A3D",
         `REM` = "#FAD899") #bb847a

colf <- c(`TWT` = "#DDCBB7",
          `Light` = "#9DB3A8",
          `SWS` = "#83A192",
          `REM` = "#FAD899")

sleep_hapa <- ggplot(d_plot_by_sleep, aes(x = Minutes, y = PosAffHALead, colour = Sleep, fill = Sleep)) +
  geom_point(alpha = 0.1) +
  labs(y = "High arousal positive affect rating", x = "Minutes in sleep architecture component") +
  geom_smooth(alpha = 0.3) +
  scale_y_continuous(limits = c(1, 5)) +
  scale_colour_manual(values = col) +
  scale_fill_manual(values = colf) +
  facet_wrap(~ Sleep, ncol = 2, scale = "free") +
  theme_minimal() +
  theme(
    panel.background  = element_blank(),
    plot.background   = element_blank(),
    panel.border      = element_rect(fill = "transparent", colour = "black"),
    panel.grid.major  = element_blank(),
    panel.grid.minor  = element_blank(),
    text = element_text(size = 11, family = "Arial Narrow"),
    legend.position   = "none",
  )

ggsave(paste0(outputdir, "sleep_hapa.png"), width = 5, height = 5, dpi = 300)
saveRDS(sleep_hapa, paste0(outputdir, "sleep_hapa", ".RDS"))

sleep_lapa <- ggplot(d_plot_by_sleep, aes(x = Minutes, y = PosAffLALead, colour = Sleep, fill = Sleep)) +
  geom_point(alpha = 0.1) +
  labs(y = "Low arousal positive affect rating", x = "Minutes in sleep architecture component") +
  geom_smooth(alpha = 0.3) +
  scale_y_continuous(limits = c(1, 5)) +
  scale_colour_manual(values = col) +
  scale_fill_manual(values = colf) +
  facet_wrap(~ Sleep, ncol = 2, scale = "free") +
  theme_minimal() +
  theme(
    panel.background  = element_blank(),
    plot.background   = element_blank(),
    panel.border      = element_rect(fill = "transparent", colour = "black"),
    panel.grid.major  = element_blank(),
    panel.grid.minor  = element_blank(),
    text = element_text(size = 11, family = "Arial Narrow"),
    legend.position   = "none",
  )

ggsave(paste0(outputdir, "sleep_lapa.png"), width = 5, height = 5, dpi = 300)
saveRDS(sleep_lapa, paste0(outputdir, "sleep_lapa", ".RDS"))

sleep_hana <- ggplot(d_plot_by_sleep, aes(x = Minutes, y = NegAffHALead, colour = Sleep, fill = Sleep)) +
  geom_point(alpha = 0.1) +
  labs(y = "High arousal negative affect rating", x = "Minutes in sleep architecture component") +
  geom_smooth(alpha = 0.3) +
  scale_y_continuous(limits = c(1, 5)) +
  scale_colour_manual(values = col) +
  scale_fill_manual(values = colf) +
  facet_wrap(~ Sleep, ncol = 2, scale = "free") +
  theme_minimal() +
  theme(
    panel.background  = element_blank(),
    plot.background   = element_blank(),
    panel.border      = element_rect(fill = "transparent", colour = "black"),
    panel.grid.major  = element_blank(),
    panel.grid.minor  = element_blank(),
    text = element_text(size = 11, family = "Arial Narrow"),
    legend.position   = "none",
  )

ggsave(paste0(outputdir, "sleep_hana.png"), width = 5, height = 5, dpi = 300)
saveRDS(sleep_hana, paste0(outputdir, "sleep_hana", ".RDS"))

sleep_lana <- ggplot(d_plot_by_sleep, aes(x = Minutes, y = NegAffLALead, colour = Sleep, fill = Sleep)) +
  geom_point(alpha = 0.1) +
  labs(y = "Low arousal negative affect rating", x = "Minutes in sleep architecture component") +
  geom_smooth(alpha = 0.3) +
  # scale_y_continuous(limits = c(0, 5)) +
  scale_colour_manual(values = col) +
  scale_fill_manual(values = colf) +
  facet_wrap(~ Sleep, ncol = 2, scale = "free") +
  theme_minimal() +
  theme(
    panel.background  = element_blank(),
    plot.background   = element_blank(),
    panel.border      = element_rect(fill = "transparent", colour = "black"),
    panel.grid.major  = element_blank(),
    panel.grid.minor  = element_blank(),
    text = element_text(size = 11, family = "Arial Narrow"),
    legend.position   = "none",
  )

ggsave(paste0(outputdir, "sleep_lana.png"), width = 5, height = 5, dpi = 300)
saveRDS(sleep_lana, paste0(outputdir, "sleep_lana", ".RDS"))



