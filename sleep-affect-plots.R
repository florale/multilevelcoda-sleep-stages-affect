source("sleep-affect-utils.R")

col <- c("#978787", "#C99696", "#C6BFB9", "#A1B2C2", "#647F9A") #lpa #mvpa #sb #wake #tst
colf <- c("#B2ABA6", "#B2ABA6", "#B2ABA6", "#B2ABA6", "#B2ABA6")
colf <- c("#978787", "#C99696", "#B2ABA6", "#A1B2C2", "#647F9A") #lpa #mvpa #sb #wake #tst

# col <- c("#7F7F7F", "#C99696", "#41765a", "#82a794", "#7F7F7F", "#C99696", "#41765a", "#82a794")
# colf <- c("#BFBFBF", "#D1ACA5", "#7A9F8B", "#91B2A1", "#BFBFBF", "#D1ACA5", "#7A9F8B", "#91B2A1")

col <- c(`Total Wake Time` = "#83A192",
         `Light Sleep` = "#FAD899",
         `Slow Wave Sleep` = "#978787",
         `REM Sleep` = "#41765a")

colf <- c(`Total Wake Time` = "#AFC7BB",
          `Light Sleep` = "#FAD899",
          `Slow Wave Sleep` = "#DCD5CE",
          `REM Sleep` = "#7A9F8B")

names <- c(`WAKE` = "Total Wake Time",
           `SleepLight` = "Light Sleep",
           `SleepDeep` = "Slow Wave Sleep",
           `SleepREM` = "REM Sleep")
labeller <- function(variable,value){
  return(names[value])
}

# read models ----------------
# m_hapa <- readRDS(paste0(outputdir, "m_hapa", ".RDS"))
# m_lapa <- readRDS(paste0(outputdir, "m_lapa", ".RDS"))
# m_hana <- readRDS(paste0(outputdir, "m_hana", ".RDS"))
# m_lana <- readRDS(paste0(outputdir, "m_lana", ".RDS"))

m_hapa_sub_adj <- readRDS(paste0(outputdir, "m_hapa_sub_adj", ".RDS"))
m_lapa_sub_adj <- readRDS(paste0(outputdir, "m_lapa_sub_adj", ".RDS"))
m_hana_sub_adj <- readRDS(paste0(outputdir, "m_hana_sub_adj", ".RDS"))
m_lana_sub_adj <- readRDS(paste0(outputdir, "m_lana_sub_adj", ".RDS"))

summary(m_hapa_sub_adj, delta = 30)
summary(m_lapa_sub_adj, delta = 30)
summary(m_hana_sub_adj, delta = 30)
summary(m_lana_sub_adj, delta = 30)

# between hapa -------------------
(plotb_hapa_wake <- 
   plot(m_hapa_sub_adj, ref = "grandmean", level = "between", to = "WAKE") +
   scale_colour_manual(values = col) +
   scale_fill_manual(values = colf) +
   labs(x = "Change in Minutes in Awake in Bed at Between-person level",
        y = "Change in High Arousal Positive Affect") +
   facet_grid(~From, labeller = labeller) +
   scale_x_continuous(breaks = c(-60, 0, 60)) +
   scale_y_continuous(limits = c(-2.25, 2.25),
                      breaks = c(-2, -1, 0, 1, 2)) +
   hrbrthemes::theme_ipsum() +
   theme(
     axis.ticks        = element_blank(),
     panel.background  = element_blank(),
     panel.border      = element_blank(),
     panel.grid.major  = element_blank(),
     panel.grid.minor  = element_blank(),
     plot.background   = element_rect(fill = "transparent", colour = NA),
     # strip.text = element_blank(),
     axis.title.x      = element_text(size = 14),
     axis.title.y      = element_text(size = 14, hjust = .5),
     plot.margin = margin(.5, .5, .5, .5, "cm"),
     legend.position = "none"
   ))

(plotb_hapa_light <- 
    plot(m_hapa_sub_adj, ref = "grandmean", level = "between", to = "SleepLight") +
    scale_colour_manual(values = col) +
    scale_fill_manual(values = colf) +
    labs(x = "Change in Minutes in Light Sleep at Between-person level",
         y = "HChange in igh Arousal Positive Affect") +
    facet_grid(~From, labeller = labeller) +
    scale_x_continuous(breaks = c(-60, 0, 60)) +
    scale_y_continuous(limits = c(-2.5, 2.25),
                       breaks = c(-2, -1, 0, 1, 2)) +
    hrbrthemes::theme_ipsum() +
    theme(
      axis.ticks        = element_blank(),
      panel.background  = element_blank(),
      panel.border      = element_blank(),
      panel.grid.major  = element_blank(),
      panel.grid.minor  = element_blank(),
      plot.background   = element_rect(fill = "transparent", colour = NA),
      # strip.text = element_blank(),
      axis.title.x      = element_text(size = 14),
      axis.title.y      = element_text(size = 14, hjust = .5),
      plot.margin = margin(.5, .5, .5, .5, "cm"),
      legend.position = "none"
    ))

(plotb_hapa_deep <- 
    plot(m_hapa_sub_adj, ref = "grandmean", level = "between", to = "SleepDeep") +
    scale_colour_manual(values = col) +
    scale_fill_manual(values = colf) +
    labs(x = "Change in Minutes in Slow Wave Sleep at Between-person level",
         y = "Change in High Arousal Positive Affect") +
    facet_grid(~From, labeller = labeller) +
    scale_x_continuous(breaks = c(-60, 0, 60)) +
    scale_y_continuous(limits = c(-2.5, 2.25),
                       breaks = c(-2, -1, 0, 1, 2)) +
    hrbrthemes::theme_ipsum() +
    theme(
      axis.ticks        = element_blank(),
      panel.background  = element_blank(),
      panel.border      = element_blank(),
      panel.grid.major  = element_blank(),
      panel.grid.minor  = element_blank(),
      plot.background   = element_rect(fill = "transparent", colour = NA),
      # strip.text = element_blank(),
      axis.title.x      = element_text(size = 14),
      axis.title.y      = element_text(size = 14, hjust = .5),
      plot.margin = margin(.5, .5, .5, .5, "cm"),
      legend.position = "none"
    ))

(plotb_hapa_rem <- 
    plot(m_hapa_sub_adj, ref = "grandmean", level = "between", to = "SleepREM") +
    scale_colour_manual(values = col) +
    scale_fill_manual(values = colf) +
    labs(x = "Change in Minutes REM Sleep at Between-person level",
         y = "Change in High Arousal Positive Affect") +
    facet_grid(~From, labeller = labeller) +
    scale_x_continuous(breaks = c(-60, 0, 60)) +
    scale_y_continuous(limits = c(-2.5, 2.25),
                       breaks = c(-2, -1, 0, 1, 2)) +
    hrbrthemes::theme_ipsum() +
    theme(
      axis.ticks        = element_blank(),
      panel.background  = element_blank(),
      panel.border      = element_blank(),
      panel.grid.major  = element_blank(),
      panel.grid.minor  = element_blank(),
      plot.background   = element_rect(fill = "transparent", colour = NA),
      # strip.text = element_blank(),
      axis.title.x      = element_text(size = 14),
      axis.title.y      = element_text(size = 14, hjust = .5),
      plot.margin = margin(.5, .5, .5, .5, "cm"),
      legend.position = "none"
    ))

# between lapa -------------------
(plotb_lapa_wake <- 
   plot(m_lapa_sub_adj, ref = "grandmean", level = "between", to = "WAKE") +
   scale_colour_manual(values = col) +
   scale_fill_manual(values = colf) +
   labs(x = "Change in Minutes in  Awake in Bed at Between-person level",
        y = "Change in Low Arousal Positive Affect") +
   facet_grid(~From, labeller = labeller) +
   scale_x_continuous(breaks = c(-60, 0, 60)) +
   scale_y_continuous(limits = c(-2.25, 2.25),
                      breaks = c(-2, -1, 0, 1, 2)) +
   hrbrthemes::theme_ipsum() +
   theme(
     axis.ticks        = element_blank(),
     panel.background  = element_blank(),
     panel.border      = element_blank(),
     panel.grid.major  = element_blank(),
     panel.grid.minor  = element_blank(),
     plot.background   = element_rect(fill = "transparent", colour = NA),
     # strip.text = element_blank(),
     axis.title.x      = element_text(size = 14),
     axis.title.y      = element_text(size = 14, hjust = .5),
     plot.margin = margin(.5, .5, .5, .5, "cm"),
     legend.position = "none"
   ))

(plotb_lapa_light <- 
    plot(m_lapa_sub_adj, ref = "grandmean", level = "between", to = "SleepLight") +
    scale_colour_manual(values = col) +
    scale_fill_manual(values = colf) +
    labs(x = "Change in Minutes in Light Sleep at Between-person level",
         y = "Low Arousal Positive Affect") +
    facet_grid(~From, labeller = labeller) +
    scale_x_continuous(breaks = c(-60, 0, 60)) +
    scale_y_continuous(limits = c(-2.5, 2.25),
                       breaks = c(-2, -1, 0, 1, 2)) +
    hrbrthemes::theme_ipsum() +
    theme(
      axis.ticks        = element_blank(),
      panel.background  = element_blank(),
      panel.border      = element_blank(),
      panel.grid.major  = element_blank(),
      panel.grid.minor  = element_blank(),
      plot.background   = element_rect(fill = "transparent", colour = NA),
      # strip.text = element_blank(),
      axis.title.x      = element_text(size = 14),
      axis.title.y      = element_text(size = 14, hjust = .5),
      plot.margin = margin(.5, .5, .5, .5, "cm"),
      legend.position = "none"
    ))

(plotb_lapa_deep <- 
    plot(m_lapa_sub_adj, ref = "grandmean", level = "between", to = "SleepDeep") +
    scale_colour_manual(values = col) +
    scale_fill_manual(values = colf) +
    labs(x = "Change in Minutes in Slow Wave Sleep at Between-person level",
         y = "Change in Low Arousal Positive Affect") +
    facet_grid(~From, labeller = labeller) +
    scale_x_continuous(breaks = c(-60, 0, 60)) +
    scale_y_continuous(limits = c(-2.5, 2.25),
                       breaks = c(-2, -1, 0, 1, 2)) +
    hrbrthemes::theme_ipsum() +
    theme(
      axis.ticks        = element_blank(),
      panel.background  = element_blank(),
      panel.border      = element_blank(),
      panel.grid.major  = element_blank(),
      panel.grid.minor  = element_blank(),
      plot.background   = element_rect(fill = "transparent", colour = NA),
      # strip.text = element_blank(),
      axis.title.x      = element_text(size = 14),
      axis.title.y      = element_text(size = 14, hjust = .5),
      plot.margin = margin(.5, .5, .5, .5, "cm"),
      legend.position = "none"
    ))


(plotb_lapa_rem <- 
    plot(m_lapa_sub_adj, ref = "grandmean", level = "between", to = "SleepREM") +
    scale_colour_manual(values = col) +
    scale_fill_manual(values = colf) +
    labs(x = "Change in Minutes in REM Sleep at Between-person level",
         y = "Change in Low Arousal Positive Affect") +
    facet_grid(~From, labeller = labeller) +
    scale_x_continuous(breaks = c(-60, 0, 60)) +
    scale_y_continuous(limits = c(-2.5, 2.25),
                       breaks = c(-2, -1, 0, 1, 2)) +
    hrbrthemes::theme_ipsum() +
    theme(
      axis.ticks        = element_blank(),
      panel.background  = element_blank(),
      panel.border      = element_blank(),
      panel.grid.major  = element_blank(),
      panel.grid.minor  = element_blank(),
      plot.background   = element_rect(fill = "transparent", colour = NA),
      # strip.text = element_blank(),
      axis.title.x      = element_text(size = 14),
      axis.title.y      = element_text(size = 14, hjust = .5),
      plot.margin = margin(.5, .5, .5, .5, "cm"),
      legend.position = "none"
    ))

# between hana -------------------
(plotb_hana_wake <- 
   plot(m_hana_sub_adj, ref = "grandmean", level = "between", to = "WAKE") +
   scale_colour_manual(values = col) +
   scale_fill_manual(values = colf) +
   labs(x = "Change in Minutes in Awake in Bed at Between-person level",
        y = "Change in High Arousal Negative Affect") +
   facet_grid(~From, labeller = labeller) +
   scale_x_continuous(breaks = c(-60, 0, 60)) +
   scale_y_continuous(limits = c(-2.25, 2.25),
                      breaks = c(-2, -1, 0, 1, 2)) +
   hrbrthemes::theme_ipsum() +
   theme(
     axis.ticks        = element_blank(),
     panel.background  = element_blank(),
     panel.border      = element_blank(),
     panel.grid.major  = element_blank(),
     panel.grid.minor  = element_blank(),
     plot.background   = element_rect(fill = "transparent", colour = NA),
     # strip.text = element_blank(),
     axis.title.x      = element_text(size = 14),
     axis.title.y      = element_text(size = 14, hjust = .5),
     plot.margin = margin(.5, .5, .5, .5, "cm"),
     legend.position = "none"
   ))

(plotb_hana_light <- 
    plot(m_hana_sub_adj, ref = "grandmean", level = "between", to = "SleepLight") +
    scale_colour_manual(values = col) +
    scale_fill_manual(values = colf) +
    labs(x = "Change in Minutes in Light Sleep at Between-person level",
         y = "Change in High Arousal Negative Affect") +
    facet_grid(~From, labeller = labeller) +
    scale_x_continuous(breaks = c(-60, 0, 60)) +
    scale_y_continuous(limits = c(-2.25, 2.25),
                       breaks = c(-2, -1, 0, 1, 2)) +
    hrbrthemes::theme_ipsum() +
    theme(
      axis.ticks        = element_blank(),
      panel.background  = element_blank(),
      panel.border      = element_blank(),
      panel.grid.major  = element_blank(),
      panel.grid.minor  = element_blank(),
      plot.background   = element_rect(fill = "transparent", colour = NA),
      # strip.text = element_blank(),
      axis.title.x      = element_text(size = 14),
      axis.title.y      = element_text(size = 14, hjust = .5),
      plot.margin = margin(.5, .5, .5, .5, "cm"),
      legend.position = "none"
    ))

(plotb_hana_deep <- 
    plot(m_hana_sub_adj, ref = "grandmean", level = "between", to = "SleepDeep") +
    scale_colour_manual(values = col) +
    scale_fill_manual(values = colf) +
    labs(x = "Change in Minutes in Slow Wave Sleep at Between-person level",
         y = "Change in High Arousal Negative Affect") +
    facet_grid(~From, labeller = labeller) +
    scale_x_continuous(breaks = c(-60, 0, 60)) +
    scale_y_continuous(limits = c(-2.25, 2.25),
                       breaks = c(-2, -1, 0, 1, 2)) +
    hrbrthemes::theme_ipsum() +
    theme(
      axis.ticks        = element_blank(),
      panel.background  = element_blank(),
      panel.border      = element_blank(),
      panel.grid.major  = element_blank(),
      panel.grid.minor  = element_blank(),
      plot.background   = element_rect(fill = "transparent", colour = NA),
      # strip.text = element_blank(),
      axis.title.x      = element_text(size = 14),
      axis.title.y      = element_text(size = 14, hjust = .5),
      plot.margin = margin(.5, .5, .5, .5, "cm"),
      legend.position = "none"
    ))

(plotb_hana_rem <- 
    plot(m_hana_sub_adj, ref = "grandmean", level = "between", to = "SleepREM") +
    scale_colour_manual(values = col) +
    scale_fill_manual(values = colf) +
    labs(x = "Change in Minutes in REM Sleep at Between-person level",
         y = "Change in High Arousal Negative Affect") +
    facet_grid(~From, labeller = labeller) +
    scale_x_continuous(breaks = c(-60, 0, 60)) +
    scale_y_continuous(limits = c(-2.25, 2.25),
                       breaks = c(-2, -1, 0, 1, 2)) +
    hrbrthemes::theme_ipsum() +
    theme(
      axis.ticks        = element_blank(),
      panel.background  = element_blank(),
      panel.border      = element_blank(),
      panel.grid.major  = element_blank(),
      panel.grid.minor  = element_blank(),
      plot.background   = element_rect(fill = "transparent", colour = NA),
      # strip.text = element_blank(),
      axis.title.x      = element_text(size = 14),
      axis.title.y      = element_text(size = 14, hjust = .5),
      plot.margin = margin(.5, .5, .5, .5, "cm"),
      legend.position = "none"
    ))

# between lana -------------------
(plotb_lana_wake <- 
   plot(m_lana_sub_adj, ref = "grandmean", level = "between", to = "WAKE") +
   scale_colour_manual(values = col) +
   scale_fill_manual(values = colf) +
   labs(x = "Change in Minutes in Awake in Bed at Between-person level",
        y = "Change in Low Arousal Negative Affect") +
   facet_grid(~From, labeller = labeller) +
   scale_x_continuous(breaks = c(-60, 0, 60)) +
   scale_y_continuous(limits = c(-2.25, 2.25),
                      breaks = c(-2, -1, 0, 1, 2)) +
   hrbrthemes::theme_ipsum() +
   theme(
     axis.ticks        = element_blank(),
     panel.background  = element_blank(),
     panel.border      = element_blank(),
     panel.grid.major  = element_blank(),
     panel.grid.minor  = element_blank(),
     plot.background   = element_rect(fill = "transparent", colour = NA),
     # strip.text = element_blank(),
     axis.title.x      = element_text(size = 14, hjust = .5),
     axis.title.y      = element_text(size = 14, hjust = .5),
     plot.margin = margin(.5, .5, .5, .5, "cm"),
     legend.position = "none"
   ))

(plotb_lana_light <- 
    plot(m_lana_sub_adj, ref = "grandmean", level = "between", to = "SleepLight") +
    scale_colour_manual(values = col) +
    scale_fill_manual(values = colf) +
    labs(x = "Change in Minutes in Light Sleep at Between-person level",
         y = "Change in Low Arousal Negative Affect") +
    facet_grid(~From, labeller = labeller) +
    scale_x_continuous(breaks = c(-60, 0, 60)) +
    scale_y_continuous(limits = c(-2.25, 2.25),
                       breaks = c(-2, -1, 0, 1, 2)) +
    hrbrthemes::theme_ipsum() +
    theme(
      axis.ticks        = element_blank(),
      panel.background  = element_blank(),
      panel.border      = element_blank(),
      panel.grid.major  = element_blank(),
      panel.grid.minor  = element_blank(),
      plot.background   = element_rect(fill = "transparent", colour = NA),
      # strip.text = element_blank(),
      axis.title.x      = element_text(size = 14),
      axis.title.y      = element_text(size = 14, hjust = .5),
      plot.margin = margin(.5, .5, .5, .5, "cm"),
      legend.position = "none"
    ))

(plotb_lana_deep <- 
    plot(m_lana_sub_adj, ref = "grandmean", level = "between", to = "SleepDeep") +
    scale_colour_manual(values = col) +
    scale_fill_manual(values = colf) +
    labs(x = "Change in Minutes in Slow Wave Sleep at Between-person level",
         y = "Change in Low Arousal Negative Affect") +
    facet_grid(~From, labeller = labeller) +
    scale_x_continuous(breaks = c(-60, 0, 60)) +
    scale_y_continuous(limits = c(-2.5, 2.25),
                       breaks = c(-2, -1, 0, 1, 2)) +
    hrbrthemes::theme_ipsum() +
    theme(
      axis.ticks        = element_blank(),
      panel.background  = element_blank(),
      panel.border      = element_blank(),
      panel.grid.major  = element_blank(),
      panel.grid.minor  = element_blank(),
      plot.background   = element_rect(fill = "transparent", colour = NA),
      # strip.text = element_blank(),
      axis.title.x      = element_text(size = 14),
      axis.title.y      = element_text(size = 14, hjust = .5),
      plot.margin = margin(.5, .5, .5, .5, "cm"),
      legend.position = "none"
    ))

(plotb_lana_rem <- 
    plot(m_lana_sub_adj, ref = "grandmean", level = "between", to = "SleepREM") +
    scale_colour_manual(values = col) +
    scale_fill_manual(values = colf) +
    labs(x = "Change in Minutes in REM Sleep at Between-person level",
         y = "Change in Low Arousal Negative Affect") +
    facet_grid(~From, labeller = labeller) +
    scale_x_continuous(breaks = c(-60, 0, 60)) +
    scale_y_continuous(limits = c(-2.75, 2.25),
                       breaks = c(-2, -1, 0, 1, 2)) +
    hrbrthemes::theme_ipsum() +
    theme(
      axis.ticks        = element_blank(),
      panel.background  = element_blank(),
      panel.border      = element_blank(),
      panel.grid.major  = element_blank(),
      panel.grid.minor  = element_blank(),
      plot.background   = element_rect(fill = "transparent", colour = NA),
      # strip.text = element_blank(),
      axis.title.x      = element_text(size = 14),
      axis.title.y      = element_text(size = 14, hjust = .5),
      plot.margin = margin(.5, .5, .5, .5, "cm"),
      legend.position = "none"
    ))

# within hapa -------------------
(plotw_hapa_wake <- 
   plot(m_hapa_sub_adj, ref = "grandmean", level = "within", to = "WAKE") +
   scale_colour_manual(values = col) +
   scale_fill_manual(values = colf) +
   labs(x = "Change in Minutes in Awake in Bed at Within-person level",
        y = "Change in Change in High Arousal Positive Affect") +
   facet_grid(~From, labeller = labeller) +
   scale_x_continuous(breaks = c(-60, 0, 60)) +
   scale_y_continuous(limits = c(-0.5, 0.5),
                      breaks = c(-0.5, -0.25, 0, 0.25, 0.5)) +
   hrbrthemes::theme_ipsum() +
   theme(
     axis.ticks        = element_blank(),
     panel.background  = element_blank(),
     panel.border      = element_blank(),
     panel.grid.major  = element_blank(),
     panel.grid.minor  = element_blank(),
     plot.background   = element_rect(fill = "transparent", colour = NA),
     # strip.text = element_blank(),
     axis.title.x      = element_text(size = 14),
     axis.title.y      = element_text(size = 14, hjust = .5),
     plot.margin = margin(.5, .5, .5, .5, "cm"),
     legend.position = "none"
   ))

(plotw_hapa_light <- 
    plot(m_hapa_sub_adj, ref = "grandmean", level = "within", to = "SleepLight") +
    scale_colour_manual(values = col) +
    scale_fill_manual(values = colf) +
    labs(x = "Change in Minutes in Light Sleep at Within-person level",
         y = "Change in High Arousal Positive Affect") +
    facet_grid(~From, labeller = labeller) +
    scale_x_continuous(breaks = c(-60, 0, 60)) +
    scale_y_continuous(limits = c(-0.5, 0.5),
                       breaks = c(-0.5, -0.25, 0, 0.25, 0.5)) +
    hrbrthemes::theme_ipsum() +
    theme(
      axis.ticks        = element_blank(),
      panel.background  = element_blank(),
      panel.border      = element_blank(),
      panel.grid.major  = element_blank(),
      panel.grid.minor  = element_blank(),
      plot.background   = element_rect(fill = "transparent", colour = NA),
      # strip.text = element_blank(),
      axis.title.x      = element_text(size = 14),
      axis.title.y      = element_text(size = 14, hjust = .5),
      plot.margin = margin(.5, .5, .5, .5, "cm"),
      legend.position = "none"
    ))

(plotw_hapa_deep <- 
    plot(m_hapa_sub_adj, ref = "grandmean", level = "within", to = "SleepDeep") +
    scale_colour_manual(values = col) +
    scale_fill_manual(values = colf) +
    labs(x = "Change in Minutes in Slow Wave Sleep at Within-person level",
         y = "Change in High Arousal Positive Affect") +
    facet_grid(~From, labeller = labeller) +
    scale_x_continuous(breaks = c(-60, 0, 60)) +
    scale_y_continuous(limits = c(-0.5, 0.5),
                       breaks = c(-0.5, -0.25, 0, 0.25, 0.5)) +
    hrbrthemes::theme_ipsum() +
    theme(
      axis.ticks        = element_blank(),
      panel.background  = element_blank(),
      panel.border      = element_blank(),
      panel.grid.major  = element_blank(),
      panel.grid.minor  = element_blank(),
      plot.background   = element_rect(fill = "transparent", colour = NA),
      # strip.text = element_blank(),
      axis.title.x      = element_text(size = 14),
      axis.title.y      = element_text(size = 14, hjust = .5),
      plot.margin = margin(.5, .5, .5, .5, "cm"),
      legend.position = "none"
    ))

(plotw_hapa_rem <- 
    plot(m_hapa_sub_adj, ref = "grandmean", level = "within", to = "SleepREM") +
    scale_colour_manual(values = col) +
    scale_fill_manual(values = colf) +
    labs(x = "Change in Minutes in REM Sleep at Within-person level",
         y = "Change in High Arousal Positive Affect") +
    facet_grid(~From, labeller = labeller) +
    scale_x_continuous(breaks = c(-60, 0, 60)) +
    scale_y_continuous(limits = c(-0.5, 0.5),
                       breaks = c(-0.5, -0.25, 0, 0.25, 0.5)) +
    hrbrthemes::theme_ipsum() +
    theme(
      axis.ticks        = element_blank(),
      panel.background  = element_blank(),
      panel.border      = element_blank(),
      panel.grid.major  = element_blank(),
      panel.grid.minor  = element_blank(),
      plot.background   = element_rect(fill = "transparent", colour = NA),
      # strip.text = element_blank(),
      axis.title.x      = element_text(size = 14),
      axis.title.y      = element_text(size = 14, hjust = .5),
      plot.margin = margin(.5, .5, .5, .5, "cm"),
      legend.position = "none"
    ))

# within lapa -------------------
(plotw_lapa_wake <- 
   plot(m_lapa_sub_adj, ref = "grandmean", level = "within", to = "WAKE") +
   scale_colour_manual(values = col) +
   scale_fill_manual(values = colf) +
   labs(x = "Change in Minutes in Awake in Bed at Within-person level",
        y = "Change in Low Arousal Positive Affect") +
   facet_grid(~From, labeller = labeller) +
   scale_x_continuous(breaks = c(-60, 0, 60)) +
   scale_y_continuous(limits = c(-0.5, 0.5),
                      breaks = c(-0.5, -0.25, 0, 0.25, 0.5)) +
   hrbrthemes::theme_ipsum() +
   theme(
     axis.ticks        = element_blank(),
     panel.background  = element_blank(),
     panel.border      = element_blank(),
     panel.grid.major  = element_blank(),
     panel.grid.minor  = element_blank(),
     plot.background   = element_rect(fill = "transparent", colour = NA),
     # strip.text = element_blank(),
     axis.title.x      = element_text(size = 14),
     axis.title.y      = element_text(size = 14, hjust = .5),
     plot.margin = margin(.5, .5, .5, .5, "cm"),
     legend.position = "none"
   ))

(plotw_lapa_light <- 
    plot(m_lapa_sub_adj, ref = "grandmean", level = "within", to = "SleepLight") +
    scale_colour_manual(values = col) +
    scale_fill_manual(values = colf) +
    labs(x = "Change in Minutes in Light Sleep at Within-person level",
         y = "Change in Low Arousal Positive Affect") +
    facet_grid(~From, labeller = labeller) +
    scale_x_continuous(breaks = c(-60, 0, 60)) +
    scale_y_continuous(limits = c(-0.5, 0.5),
                       breaks = c(-0.5, -0.25, 0, 0.25, 0.5)) +
    hrbrthemes::theme_ipsum() +
    theme(
      axis.ticks        = element_blank(),
      panel.background  = element_blank(),
      panel.border      = element_blank(),
      panel.grid.major  = element_blank(),
      panel.grid.minor  = element_blank(),
      plot.background   = element_rect(fill = "transparent", colour = NA),
      # strip.text = element_blank(),
      axis.title.x      = element_text(size = 14),
      axis.title.y      = element_text(size = 14, hjust = .5),
      plot.margin = margin(.5, .5, .5, .5, "cm"),
      legend.position = "none"
    ))

(plotw_lapa_deep <- 
    plot(m_lapa_sub_adj, ref = "grandmean", level = "within", to = "SleepDeep") +
    scale_colour_manual(values = col) +
    scale_fill_manual(values = colf) +
    labs(x = "Change in Minutes in Slow Wave Sleep at Within-person level",
         y = "Change in Low Arousal Positive Affect") +
    facet_grid(~From, labeller = labeller) +
    scale_x_continuous(breaks = c(-60, 0, 60)) +
    scale_y_continuous(limits = c(-0.5, 0.5),
                       breaks = c(-0.5, -0.25, 0, 0.25, 0.5)) +
    hrbrthemes::theme_ipsum() +
    theme(
      axis.ticks        = element_blank(),
      panel.background  = element_blank(),
      panel.border      = element_blank(),
      panel.grid.major  = element_blank(),
      panel.grid.minor  = element_blank(),
      plot.background   = element_rect(fill = "transparent", colour = NA),
      # strip.text = element_blank(),
      axis.title.x      = element_text(size = 14),
      axis.title.y      = element_text(size = 14, hjust = .5),
      plot.margin = margin(.5, .5, .5, .5, "cm"),
      legend.position = "none"
    ))

(plotw_lapa_rem <- 
    plot(m_lapa_sub_adj, ref = "grandmean", level = "within", to = "SleepREM") +
    scale_colour_manual(values = col) +
    scale_fill_manual(values = colf) +
    labs(x = "Change in Minutes in REM Sleep at Within-person level",
         y = "Change in Low Arousal Positive Affect") +
    facet_grid(~From, labeller = labeller) +
    scale_x_continuous(breaks = c(-60, 0, 60)) +
    scale_y_continuous(limits = c(-0.5, 0.5),
                       breaks = c(-0.5, -0.25, 0, 0.25, 0.5)) +
    hrbrthemes::theme_ipsum() +
    theme(
      axis.ticks        = element_blank(),
      panel.background  = element_blank(),
      panel.border      = element_blank(),
      panel.grid.major  = element_blank(),
      panel.grid.minor  = element_blank(),
      plot.background   = element_rect(fill = "transparent", colour = NA),
      # strip.text = element_blank(),
      axis.title.x      = element_text(size = 14),
      axis.title.y      = element_text(size = 14, hjust = .5),
      plot.margin = margin(.5, .5, .5, .5, "cm"),
      legend.position = "none"
    ))

# within hana -------------------
(plotw_hana_wake <- 
   plot(m_hana_sub_adj, ref = "grandmean", level = "within", to = "WAKE") +
   scale_colour_manual(values = col) +
   scale_fill_manual(values = colf) +
   labs(x = "Change in Minutes in Awake in Bed at Within-person level",
        y = "Change in High Arousal Negative Affect") +
   facet_grid(~From, labeller = labeller) +
   scale_x_continuous(breaks = c(-60, 0, 60)) +
   scale_y_continuous(limits = c(-0.5, 0.5),
                      breaks = c(-0.5, -0.25, 0, 0.25, 0.5)) +
   hrbrthemes::theme_ipsum() +
   theme(
     axis.ticks        = element_blank(),
     panel.background  = element_blank(),
     panel.border      = element_blank(),
     panel.grid.major  = element_blank(),
     panel.grid.minor  = element_blank(),
     plot.background   = element_rect(fill = "transparent", colour = NA),
     # strip.text = element_blank(),
     axis.title.x      = element_text(size = 14),
     axis.title.y      = element_text(size = 14, hjust = .5),
     plot.margin = margin(.5, .5, .5, .5, "cm"),
     legend.position = "none"
   ))

(plotw_hana_light <- 
    plot(m_hana_sub_adj, ref = "grandmean", level = "within", to = "SleepLight") +
    scale_colour_manual(values = col) +
    scale_fill_manual(values = colf) +
    labs(x = "Change in Minutes in Light Sleep at Within-person level",
         y = "Change in High Arousal Negative Affect") +
    facet_grid(~From, labeller = labeller) +
    scale_x_continuous(breaks = c(-60, 0, 60)) +
    scale_y_continuous(limits = c(-0.5, 0.5),
                       breaks = c(-0.5, -0.25, 0, 0.25, 0.5)) +
    hrbrthemes::theme_ipsum() +
    theme(
      axis.ticks        = element_blank(),
      panel.background  = element_blank(),
      panel.border      = element_blank(),
      panel.grid.major  = element_blank(),
      panel.grid.minor  = element_blank(),
      plot.background   = element_rect(fill = "transparent", colour = NA),
      # strip.text = element_blank(),
      axis.title.x      = element_text(size = 14),
      axis.title.y      = element_text(size = 14, hjust = .5),
      plot.margin = margin(.5, .5, .5, .5, "cm"),
      legend.position = "none"
    ))

(plotw_hana_deep <- 
    plot(m_hana_sub_adj, ref = "grandmean", level = "within", to = "SleepDeep") +
    scale_colour_manual(values = col) +
    scale_fill_manual(values = colf) +
    labs(x = "Change in Minutes in Slow Wave Sleep at Within-person level",
         y = "Change in High Arousal Negative Affect") +
    facet_grid(~From, labeller = labeller) +
    scale_x_continuous(breaks = c(-60, 0, 60)) +
    scale_y_continuous(limits = c(-0.5, 0.5),
                       breaks = c(-0.5, -0.25, 0, 0.25, 0.5)) +
    hrbrthemes::theme_ipsum() +
    theme(
      axis.ticks        = element_blank(),
      panel.background  = element_blank(),
      panel.border      = element_blank(),
      panel.grid.major  = element_blank(),
      panel.grid.minor  = element_blank(),
      plot.background   = element_rect(fill = "transparent", colour = NA),
      # strip.text = element_blank(),
      axis.title.x      = element_text(size = 14),
      axis.title.y      = element_text(size = 14, hjust = .5),
      plot.margin = margin(.5, .5, .5, .5, "cm"),
      legend.position = "none"
    ))

(plotw_hana_rem <- 
    plot(m_hana_sub_adj, ref = "grandmean", level = "within", to = "SleepREM") +
    scale_colour_manual(values = col) +
    scale_fill_manual(values = colf) +
    labs(x = "Change in Minutes in REM Sleep at Within-person level",
         y = "Change in High Arousal Negative Affect") +
    facet_grid(~From, labeller = labeller) +
    scale_x_continuous(breaks = c(-60, 0, 60)) +
    scale_y_continuous(limits = c(-0.5, 0.5),
                       breaks = c(-0.5, -0.25, 0, 0.25, 0.5)) +
    hrbrthemes::theme_ipsum() +
    theme(
      axis.ticks        = element_blank(),
      panel.background  = element_blank(),
      panel.border      = element_blank(),
      panel.grid.major  = element_blank(),
      panel.grid.minor  = element_blank(),
      plot.background   = element_rect(fill = "transparent", colour = NA),
      # strip.text = element_blank(),
      axis.title.x      = element_text(size = 14),
      axis.title.y      = element_text(size = 14, hjust = .5),
      plot.margin = margin(.5, .5, .5, .5, "cm"),
      legend.position = "none"
    ))

# within lana -------------------
(plotw_lana_wake <- 
   plot(m_lana_sub_adj, ref = "grandmean", level = "within", to = "WAKE") +
   scale_colour_manual(values = col) +
   scale_fill_manual(values = colf) +
   labs(x = "Change in Minutes in Awake in Bed at Within-person level",
        y = "Change in Low Arousal Negative Affect") +
   facet_grid(~From, labeller = labeller) +
   scale_x_continuous(breaks = c(-60, 0, 60)) +
   scale_y_continuous(limits = c(-0.5, 0.5),
                      breaks = c(-0.5, -0.25, 0, 0.25, 0.5)) +
   hrbrthemes::theme_ipsum() +
   theme(
     axis.ticks        = element_blank(),
     panel.background  = element_blank(),
     panel.border      = element_blank(),
     panel.grid.major  = element_blank(),
     panel.grid.minor  = element_blank(),
     plot.background   = element_rect(fill = "transparent", colour = NA),
     # strip.text = element_blank(),
     axis.title.x      = element_text(size = 14),
     axis.title.y      = element_text(size = 14, hjust = .5),
     plot.margin = margin(.5, .5, .5, .5, "cm"),
     legend.position = "none"
   ))

(plotw_lana_light <- 
    plot(m_lana_sub_adj, ref = "grandmean", level = "within", to = "SleepLight") +
    scale_colour_manual(values = col) +
    scale_fill_manual(values = colf) +
    labs(x = "Change in Minutes in Light Sleep at Within-person level",
         y = "Change in Low Arousal Negative Affect") +
    facet_grid(~From, labeller = labeller) +
    scale_x_continuous(breaks = c(-60, 0, 60)) +
    scale_y_continuous(limits = c(-0.5, 0.5),
                       breaks = c(-0.5, -0.25, 0, 0.25, 0.5)) +
    hrbrthemes::theme_ipsum() +
    theme(
      axis.ticks        = element_blank(),
      panel.background  = element_blank(),
      panel.border      = element_blank(),
      panel.grid.major  = element_blank(),
      panel.grid.minor  = element_blank(),
      plot.background   = element_rect(fill = "transparent", colour = NA),
      # strip.text = element_blank(),
      axis.title.x      = element_text(size = 14),
      axis.title.y      = element_text(size = 14, hjust = .5),
      plot.margin = margin(.5, .5, .5, .5, "cm"),
      legend.position = "none"
    ))

(plotw_lana_deep <- 
    plot(m_lana_sub_adj, ref = "grandmean", level = "within", to = "SleepDeep") +
    scale_colour_manual(values = col) +
    scale_fill_manual(values = colf) +
    labs(x = "Change in Minutes in Slow Wave Sleep at Within-person level",
         y = "Change in Low Arousal Negative Affect") +
    facet_grid(~From, labeller = labeller) +
    scale_x_continuous(breaks = c(-60, 0, 60)) +
    scale_y_continuous(limits = c(-0.5, 0.5),
                       breaks = c(-0.5, -0.25, 0, 0.25, 0.5)) +
    hrbrthemes::theme_ipsum() +
    theme(
      axis.ticks        = element_blank(),
      panel.background  = element_blank(),
      panel.border      = element_blank(),
      panel.grid.major  = element_blank(),
      panel.grid.minor  = element_blank(),
      plot.background   = element_rect(fill = "transparent", colour = NA),
      # strip.text = element_blank(),
      axis.title.x      = element_text(size = 14),
      axis.title.y      = element_text(size = 14, hjust = .5),
      plot.margin = margin(.5, .5, .5, .5, "cm"),
      legend.position = "none"
    ))

(plotw_lana_rem <- 
    plot(m_lana_sub_adj, ref = "grandmean", level = "within", to = "SleepREM") +
    scale_colour_manual(values = col) +
    scale_fill_manual(values = colf) +
    labs(x = "Change in Minutes in REM Sleep at Within-person level",
         y = "Change in Low Arousal Negative Affect") +
    facet_grid(~From, labeller = labeller) +
    scale_x_continuous(breaks = c(-60, 0, 60)) +
    scale_y_continuous(limits = c(-0.5, 0.5),
                       breaks = c(-0.5, -0.25, 0, 0.25, 0.5)) +
    hrbrthemes::theme_ipsum() +
    theme(
      axis.ticks        = element_blank(),
      panel.background  = element_blank(),
      panel.border      = element_blank(),
      panel.grid.major  = element_blank(),
      panel.grid.minor  = element_blank(),
      plot.background   = element_rect(fill = "transparent", colour = NA),
      # strip.text = element_blank(),
      axis.title.x      = element_text(size = 14),
      axis.title.y      = element_text(size = 14, hjust = .5),
      plot.margin = margin(.5, .5, .5, .5, "cm"),
      legend.position = "none"
    ))

# print plots --------------------------
extrafont::loadfonts()

#sig
grDevices::cairo_pdf(
  file = paste0(outputdir, "plotb_hapa_deep", ".pdf"),
  width = 8,
  height = 4,
)
plotb_hapa_deep
dev.off()

grDevices::cairo_pdf(
  file = paste0(outputdir, "plotb_lapa_deep", ".pdf"),
  width = 8,
  height = 4,
)
plotb_lapa_deep
dev.off()

grDevices::cairo_pdf(
  file = paste0(outputdir, "plotb_hana_deep", ".pdf"),
  width = 8,
  height = 4,
)
plotb_hana_deep
dev.off()

grDevices::cairo_pdf(
  file = paste0(outputdir, "plotw_hana_rem", ".pdf"),
  width = 8,
  height = 4,
)
plotw_hana_rem
dev.off()


grDevices::cairo_pdf(
  file = paste0(outputdir, "plotw_lana_wake", ".pdf"),
  width = 8,
  height = 4,
)
plotw_lana_wake
dev.off()

grDevices::cairo_pdf(
  file = paste0(outputdir, "plot_pa", ".pdf"),
  width = 8,
  height = 7.5,
)
ggarrange(plotb_hapa_deep, plotb_lapa_deep, nrow = 2, legend = "none")
dev.off()

grDevices::cairo_pdf(
  file = paste0(outputdir, "plot_na", ".pdf"),
  width = 8,
  height = 11,
)
ggarrange(plotb_hana_deep, plotw_hana_rem, plotw_lana_wake, nrow = 3, legend = "none")
dev.off()

# plotb_hana_deep / plotw_hana_rem / plotw_lana_wake

# rest
plotw_hapa_deep
plotw_hapa_light

plotw_lapa_deep
plotw_lapa_light
plotw_lapa_rem

plotw_hana_deep
plotw_hana_light
plotw_hana_wake

