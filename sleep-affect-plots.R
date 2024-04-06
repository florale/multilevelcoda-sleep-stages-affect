source("sleep-affect-utils.R")

# set up ----------

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
          `SleepLight` = "#E3C9C9",
          `SleepDeep` = "#DCD5CE",
          `SleepREM` = "#829E97")

names <- c(`WAKE` = "Total Wake Time",
           `SleepLight` = "Light Sleep",
           `SleepDeep` = "Slow Wave Sleep",
           `SleepREM` = "REM Sleep")
labeller <- function(variable,value){
  return(names[value])
}
alpha <- 2/10

# read models
m_hapa_sub_adj <- readRDS(paste0(outputdir, "m_hapa_sub_adj", ".RDS"))
m_lapa_sub_adj <- readRDS(paste0(outputdir, "m_lapa_sub_adj", ".RDS"))
m_hana_sub_adj <- readRDS(paste0(outputdir, "m_hana_sub_adj", ".RDS"))
m_lana_sub_adj <- readRDS(paste0(outputdir, "m_lana_sub_adj", ".RDS"))

m_hapa_sub_adj <- as.data.table(summary(m_hapa_sub_adj, delta = c(-60:60), digits = "asis"))
m_lapa_sub_adj <- as.data.table(summary(m_lapa_sub_adj, delta = c(-60:60), digits = "asis"))
m_hana_sub_adj <- as.data.table(summary(m_hana_sub_adj, delta = c(-60:60), digits = "asis"))
m_lana_sub_adj <- as.data.table(summary(m_lana_sub_adj, delta = c(-60:60), digits = "asis"))

m_hapa_sub_adj$sig <- between(0, m_hapa_sub_adj$CI_low, m_hapa_sub_adj$CI_high)
m_lapa_sub_adj$sig <- between(0, m_lapa_sub_adj$CI_low, m_lapa_sub_adj$CI_high)
m_hana_sub_adj$sig <- between(0, m_hana_sub_adj$CI_low, m_hana_sub_adj$CI_high)
m_lana_sub_adj$sig <- between(0, m_lana_sub_adj$CI_low, m_lana_sub_adj$CI_high)

m_hapa_sub_adj[, Sig := NA]
m_lapa_sub_adj[, Sig := NA]
m_hana_sub_adj[, Sig := NA]
m_lana_sub_adj[, Sig := NA]

m_hapa_sub_adj[, Sig := ifelse(sig == FALSE & Delta %in% c(-55, 55), "*", "")]
m_lapa_sub_adj[, Sig := ifelse(sig == FALSE & Delta %in% c(-55, 55), "*", "")]
m_hana_sub_adj[, Sig := ifelse(sig == FALSE & Delta %in% c(-55, 55), "*", "")]
m_lana_sub_adj[, Sig := ifelse(sig == FALSE & Delta %in% c(-55, 55), "*", "")]

# make a grid to loop plots
sub_models   <- c("m_hapa_sub_adj", "m_lapa_sub_adj", "m_hana_sub_adj",  "m_lana_sub_adj")
resp         <- c("High Arousal Positive Affect", "Low Arousal Positive Affect", 
                  "High Arousal Negative Affect", "Low Arousal Negative Affect")
level        <- c("between", "within")
level_labels <- c("Between-person ", "Within-person ")
parts        <- c("WAKE" , "SleepLight", "SleepDeep", "SleepREM")
part_labels  <- c("Total Wake Time", "Light Sleep", "Slow Wave Sleep", "REM Sleep")

rg <- expand.grid.df(data.frame(sub_models, resp), 
                     data.frame(level, level_labels),
                     data.frame(parts, part_labels))
rg <- rg[order(rg$level, rg$sub_models), ]

# between -------------------
sleep_affect_plot_b <- foreach(i = 1:16,
                               .packages = "multilevelcoda") %dopar% {
                                 
                                 ggplot(get(rg[i, "sub_models"])[To == eval(rg[i, "parts"]) & Level == eval(rg[i, "level"])], 
                                        aes(x = Delta, y = Mean)) +
                                   geom_hline(yintercept = 0, linewidth = 0.2, linetype = 2) +
                                   geom_vline(xintercept = 0, linewidth = 0.2, linetype = 2) +    
                                   geom_ribbon(aes(ymin = CI_low,
                                                   ymax = CI_high, fill = From),
                                               alpha = alpha) +
                                   geom_line(aes(colour = From), linewidth = 1) +
                                   geom_text(aes(label = Sig), 
                                             size = 6, nudge_x = 0.05, nudge_y = 0.2, 
                                             show.legend = FALSE) +
                                   scale_colour_manual(values = col) +
                                   scale_fill_manual(values = colf) +
                                   labs(x = paste0("Difference in ", rg[i, "part_labels"], " at Between-person Level"),
                                        y = paste0("Difference in ", rg[i, "resp"])) +
                                   
                                   facet_grid( ~ From, labeller = labeller) +
                                   scale_x_continuous(limits = c(-60, 60),
                                                      breaks = c(-60, 0, 60)) +
                                   scale_y_continuous(limits = c(-2.4, 2.4),
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
                                     axis.title.x      = element_text(size = 14, face = "bold"),
                                     axis.title.y      = element_text(size = 14, face = "bold", hjust = .5),
                                     plot.margin = margin(.5, .5, .5, .5, "cm"),
                                     legend.position = "none"
                                   )
                               }

names(sleep_affect_plot_b) <- foreach(i = 1:16) %dopar% {
  paste0(rg[i, "level_labels"], "Reallocation of ", rg[i, "part_labels"], " and ", rg[i, "resp"])
}
  
saveRDS(sleep_affect_plot_b, paste0(outputdir, "sleep_affect_plot_b", ".RDS"))

# within  -------------------
sleep_affect_plot_w <- foreach(i = 17:32,
                               .packages = "multilevelcoda") %dopar% {
                                 
                                 ggplot(get(rg[i, "sub_models"])[To == eval(rg[i, "parts"]) & Level == eval(rg[i, "level"])], 
                                        aes(x = Delta, y = Mean)) +
                                   geom_hline(yintercept = 0, linewidth = 0.2, linetype = 2) +
                                   geom_vline(xintercept = 0, linewidth = 0.2, linetype = 2) +    
                                   geom_ribbon(aes(ymin = CI_low,
                                                   ymax = CI_high, fill = From),
                                               alpha = alpha) +
                                   geom_line(aes(colour = From), linewidth = 1) +
                                   geom_text(aes(label = Sig), 
                                             size = 6, nudge_x = 0.05, nudge_y = 0.1, 
                                             show.legend = FALSE) +
                                   scale_colour_manual(values = col) +
                                   scale_fill_manual(values = colf) +
                                   labs(x = paste0("Difference in ", rg[i, "part_labels"], " at Within-person Level"),
                                        y = paste0("Difference in ", rg[i, "resp"])) +
                                   facet_grid( ~ From, labeller = labeller) +
                                   scale_x_continuous(limits = c(-60, 60),
                                                      breaks = c(-60, 0, 60)) +
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
                                     axis.title.x      = element_text(size = 14, face = "bold"),
                                     axis.title.y      = element_text(size = 14, face = "bold", hjust = .5),
                                     plot.margin = margin(.5, .5, .5, .5, "cm"),
                                     legend.position = "none"
                                   )
                               }


names(sleep_affect_plot_w) <- foreach(i = 17:32) %dopar% {
  paste0(rg[i, "level_labels"], "Reallocation of ", rg[i, "part_labels"], " and ", rg[i, "resp"])
}

saveRDS(sleep_affect_plot_w, paste0(outputdir, "sleep_affect_plot_w", ".RDS"))

# print plots --------------------------
extrafont::loadfonts()

#sig
sleep_affect_plot_b[[7]]
sleep_affect_plot_b[[15]]

sleep_affect_plot_b[[3]]

sleep_affect_plot_w[[4]]
sleep_affect_plot_w[[9]]

grDevices::cairo_pdf(
  file = paste0(outputdir, "plot_pa", ".pdf"),
  width = 8,
  height = 7,
)
ggarrange(sleep_affect_plot_b[[7]], sleep_affect_plot_b[[15]], nrow = 2, legend = "none")
dev.off()

grDevices::cairo_pdf(
  file = paste0(outputdir, "plot_na", ".pdf"),
  width = 8,
  height = 10,
)
ggarrange(sleep_affect_plot_b[[3]], sleep_affect_plot_w[[4]], sleep_affect_plot_w[[9]], nrow = 3, legend = "none")
dev.off()
