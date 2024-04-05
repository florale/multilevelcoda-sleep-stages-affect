source("sleep-affect-utils.R")

col <- c(`Total Wake Time` = "#83A192",
         `Light Sleep` = "#FAD899",
         `Slow Wave Sleep` = "#978787",
         `REM Sleep` = "#41765a")

colf <- c(`Total Wake Time` = "#AFC7BB",
          `Light Sleep` = "#FAD899",
          `Slow Wave Sleep` = "#DCD5CE",
          `REM Sleep` = "#7A9F8B")

col <- c(`WAKE` = "#83A192",
         `SleepLight` = "#C99696",
         `SleepDeep` = "#978787",
         `SleepREM` = "#52796f")

colf <- c(`WAKE` = "#AFC7BB",
          `SleepLight` = "#D1ACA5",
          `SleepDeep` = "#DCD5CE",
          `SleepREM` = "#829E97")

names <- c(`WAKE` = "Total Wake Time",
           `SleepLight` = "Light Sleep",
           `SleepDeep` = "Slow Wave Sleep",
           `SleepREM` = "REM Sleep")
labeller <- function(variable,value){
  return(names[value])
}

# read models
m_hapa_sub_adj <- readRDS(paste0(outputdir, "m_hapa_sub_adj", ".RDS"))
m_lapa_sub_adj <- readRDS(paste0(outputdir, "m_lapa_sub_adj", ".RDS"))
m_hana_sub_adj <- readRDS(paste0(outputdir, "m_hana_sub_adj", ".RDS"))
m_lana_sub_adj <- readRDS(paste0(outputdir, "m_lana_sub_adj", ".RDS"))

# make a grid to loop plots
sub_models   <- c("m_hapa_sub_adj", "m_lapa_sub_adj", "m_hana_sub_adj",  "m_lana_sub_adj")
resp         <- c("High Arousal Positive Affect", "Low Arousal Positive Affect", "High Arousal Negative Affect", "Low Arousal Negative Affect")
parts        <- c("WAKE" , "SleepLight", "SleepDeep", "SleepREM")
part_labels  <- c("Total Wake Time", "Light Sleep", "Slow Wave Sleep", "REM Sleep")

rg <- expand.grid.df(data.frame(sub_models, affect), 
                     data.frame(parts, sleep_labels))

# between -------------------
sleep_affect_plot_b <- foreach(i = seq_len(nrow(rg)),
                               .packages = "multilevelcoda") %dopar% {
                                 plot(get(rg[i, "sub_models"]), to = rg[i, "parts"], ref = "grandmean", level = "between") +
                                   scale_colour_manual(values = col) +
                                   scale_fill_manual(values = colf) +
                                   labs(x = paste0("Difference in ", rg[i, "part_labels"]),
                                        y = paste0("Difference in ", rg[i, "resp"])) +
                                   facet_grid( ~ From, labeller = labeller) +
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
                                   )
                               }
saveRDS(sleep_affect_plot_b, paste0(outputdir, "sleep_affect_plot_b", ".RDS"))

# within  -------------------
sleep_affect_plot_w <- foreach(i = seq_len(nrow(rg)),
                               .packages = "multilevelcoda") %dopar% {
                                 plot(get(rg[i, "sub_models"]), to = rg[i, "parts"], ref = "grandmean", level = "within") +
                                   scale_colour_manual(values = col) +
                                   scale_fill_manual(values = colf) +
                                   labs(x = paste0("Difference in ", rg[i, "part_labels"]),
                                        y = paste0("Difference in ", rg[i, "resp"])) +
                                   facet_grid( ~ From, labeller = labeller) +
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
                                   )
                               }
saveRDS(sleep_affect_plot_w, paste0(outputdir, "sleep_affect_plot_w", ".RDS"))

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

