library(hrbrthemes)
library(wesanderson)
library(ggplot2)
library(ggsci)
library(ggpubr)
library(patchwork)
library(latex2exp)

col <- c("#978787", "#C99696", "#C6BFB9", "#A1B2C2", "#647F9A") #lpa #mvpa #sb #wake #tst
colf <- c("#B2ABA6", "#B2ABA6", "#B2ABA6", "#B2ABA6", "#B2ABA6")
colf <- c("#978787", "#C99696", "#B2ABA6", "#A1B2C2", "#647F9A") #lpa #mvpa #sb #wake #tst

col <- c("#FAD899", "#B2ABA6", "#84a98c", "#52796f", "#FAD899", "#B2ABA6", "#84a98c", "#52796f") 
colf <- c("#FAD899", "#B2ABA6", "#90B197", "#829E97", "#FAD899", "#B2ABA6", "#90B197", "#829E97")

colour <- c("#EFE3E0", "#BEACA2", "#708885", "#5A6367")

names <- c(`WAKE` = "Awake in Bed",
           `SleepLight` = "Light Sleep",
           `SleepDeep` = "Slow Wave Sleep",
           `SleepREM` = "REM Sleep")
labeller <- function(variable,value){
  return(names[value])
}

# read models
m_hapa <- readRDS(paste0(outputdir, "m_hapa", ".RDS"))
m_lapa <- readRDS(paste0(outputdir, "m_lapa", ".RDS"))
m_hana <- readRDS(paste0(outputdir, "m_hana", ".RDS"))
m_lana <- readRDS(paste0(outputdir, "m_lana", ".RDS"))

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
   scale_colour_manual(values = col[1:3]) +
   scale_fill_manual(values = colf[1:3]) +
   labs(x = "Minutes Reallocation of Awake in Bed at Between-person level",
        y = "High Arousal Positive Affect") +
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
    scale_colour_manual(values = col[2:4]) +
    scale_fill_manual(values = colf[2:4]) +
    labs(x = "Minutes Reallocation of Light Sleep at Between-person level",
         y = "High Arousal Positive Affect") +
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
    scale_colour_manual(values = col[3:5]) +
    scale_fill_manual(values = colf[3:5]) +
    labs(x = "Minutes Reallocation of Slow Wave Sleep at Between-person level",
         y = "High Arousal Positive Affect") +
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
    scale_colour_manual(values = col[4:6]) +
    scale_fill_manual(values = colf[4:6]) +
    labs(x = "Minutes Reallocation of REM Sleep at Between-person level",
         y = "High Arousal Positive Affect") +
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
   scale_colour_manual(values = col[1:3]) +
   scale_fill_manual(values = colf[1:3]) +
   labs(x = "Minutes Reallocation of Awake in Bed at Between-person level",
        y = "Low Arousal Positive Affect") +
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
    scale_colour_manual(values = col[2:4]) +
    scale_fill_manual(values = colf[2:4]) +
    labs(x = "Minutes Reallocation of Light Sleep at Between-person level",
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
    scale_colour_manual(values = col[3:5]) +
    scale_fill_manual(values = colf[3:5]) +
    labs(x = "Minutes Reallocation of Slow Wave Sleep at Between-person level",
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


(plotb_lapa_rem <- 
    plot(m_lapa_sub_adj, ref = "grandmean", level = "between", to = "SleepREM") +
    scale_colour_manual(values = col[4:6]) +
    scale_fill_manual(values = colf[4:6]) +
    labs(x = "Minutes Reallocation of REM Sleep at Between-person level",
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

# between hana -------------------
(plotb_hana_wake <- 
   plot(m_hana_sub_adj, ref = "grandmean", level = "between", to = "WAKE") +
   scale_colour_manual(values = col[1:3]) +
   scale_fill_manual(values = colf[1:3]) +
   labs(x = "Minutes Reallocation of Awake in Bed at Between-person level",
        y = "High Arousal Negative Affect") +
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
    scale_colour_manual(values = col[2:4]) +
    scale_fill_manual(values = colf[2:4]) +
    labs(x = "Minutes Reallocation of Light Sleep at Between-person level",
         y = "High Arousal Negative Affect") +
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
    scale_colour_manual(values = col[3:5]) +
    scale_fill_manual(values = colf[3:5]) +
    labs(x = "Minutes Reallocation of Slow Wave Sleep at Between-person level",
         y = "High Arousal Negative Affect") +
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
    scale_colour_manual(values = col[4:6]) +
    scale_fill_manual(values = colf[4:6]) +
    labs(x = "Minutes Reallocation of REM Sleep at Between-person level",
         y = "High Arousal Negative Affect") +
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
   scale_colour_manual(values = col[1:3]) +
   scale_fill_manual(values = colf[1:3]) +
   labs(x = "Minutes Reallocation of Awake in Bed at Between-person level",
        y = "Low Arousal Negative Affect") +
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
    scale_colour_manual(values = col[2:4]) +
    scale_fill_manual(values = colf[2:4]) +
    labs(x = "Minutes Reallocation of Light Sleep at Between-person level",
         y = "Low Arousal Negative Affect") +
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
    scale_colour_manual(values = col[3:5]) +
    scale_fill_manual(values = colf[3:5]) +
    labs(x = "Minutes Reallocation of Slow Wave Sleep at Between-person level",
         y = "Low Arousal Negative Affect") +
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
    scale_colour_manual(values = col[4:6]) +
    scale_fill_manual(values = colf[4:6]) +
    labs(x = "Minutes Reallocation of REM Sleep at Between-person level",
         y = "Low Arousal Negative Affect") +
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
   scale_colour_manual(values = col[1:3]) +
   scale_fill_manual(values = colf[1:3]) +
   labs(x = "Minutes Reallocation of Awake in Bed at Within-person level",
        y = "High Arousal Positive Affect") +
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
    scale_colour_manual(values = col[2:4]) +
    scale_fill_manual(values = colf[2:4]) +
    labs(x = "Minutes Reallocation of Light Sleep at Within-person level",
         y = "High Arousal Positive Affect") +
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
    scale_colour_manual(values = col[3:5]) +
    scale_fill_manual(values = colf[3:5]) +
    labs(x = "Minutes Reallocation of Slow Wave Sleep at Within-person level",
         y = "High Arousal Positive Affect") +
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
    scale_colour_manual(values = col[4:6]) +
    scale_fill_manual(values = colf[4:6]) +
    labs(x = "Minutes Reallocation of REM Sleep at Within-person level",
         y = "High Arousal Positive Affect") +
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
   scale_colour_manual(values = col[1:3]) +
   scale_fill_manual(values = colf[1:3]) +
   labs(x = "Minutes Reallocation of Awake in Bed at Within-person level",
        y = "Low Arousal Positive Affect") +
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
    scale_colour_manual(values = col[2:4]) +
    scale_fill_manual(values = colf[2:4]) +
    labs(x = "Minutes Reallocation of Light Sleep at Within-person level",
         y = "Low Arousal Positive Affect") +
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
    scale_colour_manual(values = col[3:5]) +
    scale_fill_manual(values = colf[3:5]) +
    labs(x = "Minutes Reallocation of Slow Wave Sleep at Within-person level",
         y = "Low Arousal Positive Affect") +
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
    scale_colour_manual(values = col[4:6]) +
    scale_fill_manual(values = colf[4:6]) +
    labs(x = "Minutes Reallocation of REM Sleep at Within-person level",
         y = "Low Arousal Positive Affect") +
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
   scale_colour_manual(values = col[1:3]) +
   scale_fill_manual(values = colf[1:3]) +
   labs(x = "Minutes Reallocation of Awake in Bed at Within-person level",
        y = "High Arousal Negative Affect") +
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
    scale_colour_manual(values = col[2:4]) +
    scale_fill_manual(values = colf[2:4]) +
    labs(x = "Minutes Reallocation of Light Sleep at Within-person level",
         y = "High Arousal Negative Affect") +
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
    scale_colour_manual(values = col[3:5]) +
    scale_fill_manual(values = colf[3:5]) +
    labs(x = "Minutes Reallocation of Slow Wave Sleep at Within-person level",
         y = "High Arousal Negative Affect") +
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
    scale_colour_manual(values = col[4:6]) +
    scale_fill_manual(values = colf[4:6]) +
    labs(x = "Minutes Reallocation of REM Sleep at Within-person level",
         y = "High Arousal Negative Affect") +
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
   scale_colour_manual(values = col[1:3]) +
   scale_fill_manual(values = colf[1:3]) +
   labs(x = "Minutes Reallocation of Awake in Bed at Within-person level",
        y = "Low Arousal Negative Affect") +
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
    scale_colour_manual(values = col[2:4]) +
    scale_fill_manual(values = colf[2:4]) +
    labs(x = "Minutes Reallocation of Light Sleep at Within-person level",
         y = "Low Arousal Negative Affect") +
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
    scale_colour_manual(values = col[3:5]) +
    scale_fill_manual(values = colf[3:5]) +
    labs(x = "Minutes Reallocation of Slow Wave Sleep at Within-person level",
         y = "Low Arousal Negative Affect") +
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
    scale_colour_manual(values = col[4:6]) +
    scale_fill_manual(values = colf[4:6]) +
    labs(x = "Minutes Reallocation of REM Sleep at Within-person level",
         y = "Low Arousal Negative Affect") +
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
# save as 8x4

#sig
plotb_hapa_deep
plotb_hapa_light

plotb_lapa_deep
plotb_hana_deep

plotw_hana_rem
plotw_lana_wake

# rest
plotw_hapa_deep
plotw_hapa_light

plotw_lapa_deep
plotw_lapa_light
plotw_lapa_rem

plotw_hana_deep
plotw_hana_light
plotw_hana_wake

