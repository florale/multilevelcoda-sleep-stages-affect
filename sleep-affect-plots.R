source("sleep-affect-utils.R")
# source("sleep-affect-data.R") # load for d to work

# sub results ---------------
m_hapa_sub_adj <- readRDS(paste0(outputdir, "m_hapa_sub_adj", ".RDS"))
m_lapa_sub_adj <- readRDS(paste0(outputdir, "m_lapa_sub_adj", ".RDS"))
m_hana_sub_adj <- readRDS(paste0(outputdir, "m_hana_sub_adj", ".RDS"))
m_lana_sub_adj <- readRDS(paste0(outputdir, "m_lana_sub_adj", ".RDS"))

# set up ----------

col <- c(`TWT` = "#BBA9A7",
         `Light` = "#83A192",
         `SWS` = "#465A3D",
         `REM` = "#FAD899") #bb847a

colf <- c(`TWT` = "#DDCBB7",
          `Light` = "#9DB3A8",
          `SWS` = "#83A192",
          `REM` = "#FAD899")

# col <- c(`TWT` = "#8AAFCA",
#          `Light` = "#BBA9A7",
#          `SWS` = "#456691",
#          `REM` = "#BA7B6B") #bb847a
# 
# colf <- c(`TWT` = "#A1B2C2",
#           `Light` = "#DCD5CE",
#           `SWS` = "#8399AE",
#           `REM` = "#CB968B")

# check colourblind
# convert to the three dichromacy approximations
protan <- dichromat::dichromat(col, type = "protan")
deutan <- dichromat::dichromat(col, type = "deutan")
tritan <- dichromat::dichromat(col, type = "tritan")

# plot for comparison
layout(matrix(1:4, nrow = 4)); par(mar = rep(1, 4))
recolorize::plotColorPalette(col, main = "Trichromacy")
recolorize::plotColorPalette(protan, main = "Protanopia")
recolorize::plotColorPalette(deutan, main = "Deutanopia")
recolorize::plotColorPalette(tritan, main = "Tritanopia")


names <- c(`WAKE` = "TWT",
           `SleepLight` = "Light",
           `SleepDeep` = "SWS",
           `SleepREM` = "REM")

# names <- c(`WAKE` = "At the expense of \nTWT",
#            `SleepLight` = "At the expense of \nLight",
#            `SleepDeep` = "At the expense of \nSWS",
#            `SleepREM` = "At the expense of \nREM")

labeller <- function(variable,value){
  return(names[value])
}
alpha <- 2/10

# make a grid to loop plots
sub_models   <- c("m_hapa_sub_adj", "m_lapa_sub_adj", "m_hana_sub_adj",  "m_lana_sub_adj")
resp         <- c("High Arousal Positive Affect", "Low Arousal Positive Affect", 
                  "High Arousal Negative Affect", "Low Arousal Negative Affect")
level        <- c("between", "within")
level_labels <- c("Between-person ", "Within-person ")
parts        <- c("TWT", "Light", "SWS", "REM")

rg <- expand.grid.df(data.frame(sub_models, resp), 
                     data.frame(level, level_labels),
                     data.frame(parts))
rg <- rg[order(rg$level, rg$sub_models), ]

# standarised coefs
rg$smean <- NA
rg$smean <- ifelse(level == "between" & sub_models == "m_hapa_sub_adj", sd(d[Survey == "Wake"]$BPosAffHALead, na.rm = T), rg$smean)
rg$smean <- ifelse(level == "between" & sub_models == "m_lapa_sub_adj", sd(d[Survey == "Wake"]$BPosAffLALead, na.rm = T), rg$smean)
rg$smean <- ifelse(level == "between" & sub_models == "m_hana_sub_adj", sd(d[Survey == "Wake"]$BNegAffHALead, na.rm = T), rg$smean)
rg$smean <- ifelse(level == "between" & sub_models == "m_lana_sub_adj", sd(d[Survey == "Wake"]$BNegAffLALead, na.rm = T), rg$smean)

rg$smean <- ifelse(level == "within" & sub_models == "m_hapa_sub_adj", sd(d[Survey == "Wake"]$WPosAffHALead, na.rm = T), rg$smean)
rg$smean <- ifelse(level == "within" & sub_models == "m_lapa_sub_adj", sd(d[Survey == "Wake"]$WPosAffLALead, na.rm = T), rg$smean)
rg$smean <- ifelse(level == "within" & sub_models == "m_hana_sub_adj", sd(d[Survey == "Wake"]$WNegAffHALead, na.rm = T), rg$smean)
rg$smean <- ifelse(level == "within" & sub_models == "m_lana_sub_adj", sd(d[Survey == "Wake"]$WNegAffLALead, na.rm = T), rg$smean)

saveRDS(rg, paste0(outputdir, "sleep_affect_rg", ".RDS"))

# between -------------------
sub_models_b <- list()

for(i in seq_along(sub_models)) {
  
  model_tmp <- get(sub_models[[i]])
  
  model_tmp <- as.data.table(summary(model_tmp, delta = c(-60:60), level = "between", digits = "asis"))
  
  model_tmp[, From := ifelse(From == "WAKE", "TWT", From)]
  model_tmp[, From := ifelse(From == "SleepLight", "Light", From)]
  model_tmp[, From := ifelse(From == "SleepDeep", "SWS", From)]
  model_tmp[, From := ifelse(From == "SleepREM", "REM", From)]
  model_tmp[, From := factor(From, ordered = TRUE,
                             levels = c("TWT",
                                        "Light",
                                        "SWS",
                                        "REM"))]
  
  model_tmp[, To := ifelse(To == "WAKE", "TWT", To)]
  model_tmp[, To := ifelse(To == "SleepLight", "Light", To)]
  model_tmp[, To := ifelse(To == "SleepDeep", "SWS", To)]
  model_tmp[, To := ifelse(To == "SleepREM", "REM", To)]
  model_tmp[, To := factor(To, ordered = TRUE,
                           levels = c("TWT",
                                      "Light",
                                      "SWS",
                                      "REM"))]
  
  model_tmp$sig <- between(0, model_tmp$CI_low, model_tmp$CI_high)
  model_tmp[, Sig := NA]
  model_tmp[, Sig := ifelse(sig == FALSE & Delta %in% c(-55, 55), "*", "")]
  
  sub_models_b[[i]] <- model_tmp
}
names(sub_models_b) <- (sub_models)

sleep_affect_plot_b <- foreach(i = 1:16,
                               .packages = "multilevelcoda") %dopar% {
                                 
                                 ggplot(sub_models_b[[rg[i, "sub_models"]]][To == eval(rg[i, "parts"]) & Level == eval(rg[i, "level"])], 
                                        aes(x = Delta, y = Mean)) +
                                   geom_hline(yintercept = 0, linewidth = 0.2, linetype = 2) +
                                   geom_vline(xintercept = 0, linewidth = 0.2, linetype = 2) +    
                                   geom_ribbon(aes(ymin = CI_low,
                                                   ymax = CI_high, fill = From),
                                               alpha = alpha) +
                                   geom_line(aes(colour = From), linewidth = 1) +
                                   geom_text(aes(label = Sig, colour = From), 
                                             size = 7, nudge_x = 0.05, nudge_y = 0.2, 
                                             show.legend = FALSE) +
                                   scale_colour_manual(values = col) +
                                   scale_fill_manual(values = colf) +
                                   labs(x = paste0("Difference in ", rg[i, "parts"], " at Between-person Level"),
                                        y = paste0("Estimated Difference")) +
                                   facet_wrap(ggplot2::vars(From, To),
                                              labeller = label_bquote(cols = .(as.character(From)) %<-% phantom() ~ minutes ~ phantom() %->% .(as.character(To))),
                                              strip.position = "bottom") +
                                   scale_x_continuous(limits = c(-60, 60),
                                                      breaks = c(-60, 0, 60)) +
                                   scale_y_continuous(limits = c(-2.4, 2.4),
                                                      breaks = c(-2, -1, 0, 1, 2),
                                                      sec.axis = sec_axis(~ . / rg[i, "smean"], name = paste0("Standardised Estimated Difference"))
                                   ) +
                                   hrbrthemes::theme_ipsum() +
                                   theme(
                                     axis.ticks         = element_blank(),
                                     panel.background   = element_blank(),
                                     panel.border       = element_blank(),
                                     panel.grid.major   = element_blank(),
                                     panel.grid.minor   = element_blank(),
                                     plot.background    = element_rect(fill = "transparent", colour = NA),
                                     strip.background   = element_rect(fill = "transparent", colour = NA),
                                     strip.text         = element_text(size = 11, hjust = .5),
                                     strip.placement    = "outside",
                                     axis.title.x       = element_blank(),
                                     # axis.title.y       = element_text(size = 12, hjust = .5),
                                     axis.title.y       = element_blank(),
                                     # axis.title.y.right = element_text(size = 12, hjust = .5, angle = 270),
                                     axis.title.y.right = element_blank(),
                                     plot.margin        = margin(.5, .5, .5, .5, "cm"),
                                     legend.position    = "none"
                                   )
                               }

names(sleep_affect_plot_b) <- foreach(i = 1:16) %dopar% {
  paste0(rg[i, "level_labels"], "Reallocation of ", rg[i, "parts"], " and ", rg[i, "resp"])
}
saveRDS(sleep_affect_plot_b, paste0(outputdir, "sleep_affect_plot_b", ".RDS"))

# within -------------------
sub_models_w <- list()

for(i in seq_along(sub_models)) {
  
  model_tmp <- get(sub_models[[i]])
  
  model_tmp <- as.data.table(summary(model_tmp, delta = c(-60:60), level = "within", digits = "asis"))
  
  model_tmp[, From := ifelse(From == "WAKE", "TWT", From)]
  model_tmp[, From := ifelse(From == "SleepLight", "Light", From)]
  model_tmp[, From := ifelse(From == "SleepDeep", "SWS", From)]
  model_tmp[, From := ifelse(From == "SleepREM", "REM", From)]
  model_tmp[, From := factor(From, ordered = TRUE,
                             levels = c("TWT",
                                        "Light",
                                        "SWS",
                                        "REM"))]
  
  model_tmp[, To := ifelse(To == "WAKE", "TWT", To)]
  model_tmp[, To := ifelse(To == "SleepLight", "Light", To)]
  model_tmp[, To := ifelse(To == "SleepDeep", "SWS", To)]
  model_tmp[, To := ifelse(To == "SleepREM", "REM", To)]
  model_tmp[, To := factor(To, ordered = TRUE,
                           levels = c("TWT",
                                      "Light",
                                      "SWS",
                                      "REM"))]
  
  model_tmp$sig <- between(0, model_tmp$CI_low, model_tmp$CI_high)
  model_tmp[, Sig := NA]
  model_tmp[, Sig := ifelse(sig == FALSE & Delta %in% c(-55, 55), "*", "")]
  
  sub_models_w[[i]] <- model_tmp
}
names(sub_models_w) <- (sub_models)

sleep_affect_plot_w <- foreach(i = 17:32,
                               .packages = "multilevelcoda") %dopar% {
                                 
                                 ggplot(sub_models_w[[rg[i, "sub_models"]]][To == eval(rg[i, "parts"]) & Level == eval(rg[i, "level"])], 
                                        aes(x = Delta, y = Mean)) +
                                   geom_hline(yintercept = 0, linewidth = 0.2, linetype = 2) +
                                   geom_vline(xintercept = 0, linewidth = 0.2, linetype = 2) +    
                                   geom_ribbon(aes(ymin = CI_low,
                                                   ymax = CI_high, fill = From),
                                               alpha = alpha) +
                                   geom_line(aes(colour = From), linewidth = 1) +
                                   geom_text(aes(label = Sig, colour = From), 
                                             size = 7, nudge_x = 0.05, nudge_y = 0.1, 
                                             show.legend = FALSE) +
                                   scale_colour_manual(values = col) +
                                   scale_fill_manual(values = colf) +
                                   labs(x = paste0("Difference in ", rg[i, "parts"], " at Within-person Level"),
                                        y = paste0("Estimated Difference")) +
                                   facet_wrap(ggplot2::vars(From, To),
                                              labeller = label_bquote(cols = .(as.character(From)) %<-% phantom() ~ minutes ~ phantom() %->% .(as.character(To))),
                                              strip.position = "bottom") +
                                   scale_x_continuous(limits = c(-60, 60),
                                                      breaks = c(-60, 0, 60)) +
                                   scale_y_continuous(limits = c(-0.5, 0.5),
                                                      breaks = c(-0.5, -0.25, 0, 0.25, 0.5),
                                                      sec.axis = sec_axis(~ . / rg[i, "smean"], name = paste0("Standardised Estimated Difference"))
                                   ) +
                                   hrbrthemes::theme_ipsum() +
                                   theme(
                                     axis.ticks         = element_blank(),
                                     panel.background   = element_blank(),
                                     panel.border       = element_blank(),
                                     panel.grid.major   = element_blank(),
                                     panel.grid.minor   = element_blank(),
                                     plot.background    = element_rect(fill = "transparent", colour = NA),
                                     strip.background   = element_rect(fill = "transparent", colour = NA),
                                     strip.text         = element_text(size = 11, hjust = .5),
                                     strip.placement    = "outside",
                                     axis.title.x       = element_blank(),
                                     # axis.title.y       = element_text(size = 12, hjust = .5),
                                     axis.title.y       = element_blank(),
                                     # axis.title.y.right = element_text(size = 12, hjust = .5, angle = 270),
                                     axis.title.y.right = element_blank(),
                                     plot.margin        = margin(.5, .5, .5, .5, "cm"),
                                     legend.position    = "none"
                                   )
                               }

names(sleep_affect_plot_w) <- foreach(i = 17:32) %dopar% {
  paste0(rg[i, "level_labels"], "Reallocation of ", rg[i, "parts"], " and ", rg[i, "resp"])
}
saveRDS(sleep_affect_plot_w, paste0(outputdir, "sleep_affect_plot_w", ".RDS"))

# print plots --------------------------
#sig
sleep_affect_plot_b[[7]]
sleep_affect_plot_b[[15]]

sleep_affect_plot_b[[3]]

sleep_affect_plot_w[[4]]
sleep_affect_plot_w[[9]]

grDevices::cairo_pdf(
  file = paste0(outputdir, "plot_sleep_pa", ".pdf"),
  width = 9,
  height = 8,
)
ggarrange(sleep_affect_plot_b[[7]], sleep_affect_plot_b[[15]], 
          nrow = 2, legend = "none", 
          labels = c("A. High Arousal Positive Affect at Between-person level", 
                     "B. Low Arousal Positive Affect at Between-person level"),
          hjust = 0,
          font.label = list(size = 13, color = "black", family = "Arial Narrow")
)
dev.off()

grDevices::cairo_pdf(
  file = paste0(outputdir, "plot_sleep_na", ".pdf"),
  width = 9,
  height = 12,
)
ggarrange(sleep_affect_plot_b[[3]], sleep_affect_plot_w[[4]], sleep_affect_plot_w[[9]], 
          nrow = 3, legend = "none",
          labels = c("A. High Arousal Negative Affect at Between-person level", 
                     "B. High Arousal Negative Affect at Within-person level",
                     "C. Low Arousal Negative Affect at Within-person level"),
          hjust = 0,
          font.label = list(size = 13, color = "black", family = "Arial Narrow")
)
dev.off()

# all
grDevices::cairo_pdf(
  file = paste0(outputdir, "plot_sleep_affect", ".pdf"),
  width = 9,
  height = 13,
)
fig <- ggarrange(sleep_affect_plot_b[[7]], sleep_affect_plot_b[[15]], 
          sleep_affect_plot_b[[3]], sleep_affect_plot_w[[4]], sleep_affect_plot_w[[9]], 
          nrow = 5, legend = "none",
          labels = c("A. High Arousal Positive Affect at Between-person level", 
                     "B. Low Arousal Positive Affect at Between-person level",
                     "C. High Arousal Negative Affect at Between-person level", 
                     "D. High Arousal Negative Affect at Within-person level",
                     "E. Low Arousal Negative Affect at Within-person level")
          ,
          hjust = 0,
          font.label = list(size = 11, color = "black", family = "Arial Narrow", face = "italic")
)
annotate_figure(fig, 
                left = text_grob("Estimated Difference", size = 13, rot = 90, family = "Arial Narrow", face = "bold"),
                right = text_grob("Standardised Estimated Difference", size = 13, rot = 270, family = "Arial Narrow", face = "bold"))

dev.off()


# for supplementary -------------------
sleep_affect_plot_b_supp <- foreach(i = 1:16,
                                    .packages = "multilevelcoda") %dopar% {
                                      
                                      ggplot(sub_models_b[[rg[i, "sub_models"]]][To == eval(rg[i, "parts"]) & Level == eval(rg[i, "level"])], 
                                             aes(x = Delta, y = Mean)) +
                                        geom_hline(yintercept = 0, linewidth = 0.2, linetype = 2) +
                                        geom_vline(xintercept = 0, linewidth = 0.2, linetype = 2) +    
                                        geom_ribbon(aes(ymin = CI_low,
                                                        ymax = CI_high, fill = From),
                                                    alpha = alpha) +
                                        geom_line(aes(colour = From), linewidth = 1) +
                                        geom_text(aes(label = Sig, colour = From), 
                                                  size = 7, nudge_x = 0.05, nudge_y = 0.2, 
                                                  show.legend = FALSE) +
                                        scale_colour_manual(values = col) +
                                        scale_fill_manual(values = colf) +
                                        labs(x = paste0("Difference in ", rg[i, "parts"], " at Between-person Level"),
                                             y = paste0("Estimated Difference"),
                                             title =  paste0("Reallocation of ", rg[i, "parts"], " and ", rg[i, "resp"], " at Between-person Level")) +
                                        facet_wrap(ggplot2::vars(From, To),
                                                   labeller = label_bquote(cols = .(as.character(From)) %<-% phantom() ~ minutes ~ phantom() %->% .(as.character(To))),
                                                   strip.position = "bottom") +
                                        scale_x_continuous(limits = c(-60, 60),
                                                           breaks = c(-60, 0, 60)) +
                                        scale_y_continuous(limits = c(-2.4, 2.4),
                                                           breaks = c(-2, -1, 0, 1, 2),
                                                           sec.axis = sec_axis(~ . / rg[i, "smean"], name = paste0("Standardised Estimated Difference"))
                                        ) +
                                        hrbrthemes::theme_ipsum() +
                                        theme(
                                          axis.ticks          = element_blank(),
                                          panel.background    = element_blank(),
                                          panel.border        = element_blank(),
                                          panel.grid.major    = element_blank(),
                                          panel.grid.minor    = element_blank(),
                                          plot.background     = element_rect(fill = "transparent", colour = NA),
                                          strip.background    = element_rect(fill = "transparent", colour = NA),
                                          strip.text          = element_text(size = 13, hjust = .5),
                                          strip.placement     = "outside",
                                          axis.title.x        = element_blank(),
                                          axis.title.y        = element_text(size = 12, hjust = .5),
                                          axis.title.y.right  = element_text(size = 12, hjust = .5, angle = 270),
                                          plot.title          = element_text(size = 13, hjust = .5, face = "italic"),
                                          plot.title.position = "plot",
                                          plot.margin         = margin(.5, .5, 1.5, .5, "cm"),
                                          legend.position     = "none"
                                        )
                                    }

names(sleep_affect_plot_b_supp) <- foreach(i = 1:16) %dopar% {
  paste0(rg[i, "level_labels"], "Reallocation of ", rg[i, "parts"], " and ", rg[i, "resp"], " at between-person level")
}
saveRDS(sleep_affect_plot_b_supp, paste0(outputdir, "sleep_affect_plot_b_supp", ".RDS"))

sleep_affect_plot_w_supp <- foreach(i = 17:32,
                                    .packages = "multilevelcoda") %dopar% {
                                      
                                      ggplot(sub_models_w[[rg[i, "sub_models"]]][To == eval(rg[i, "parts"]) & Level == eval(rg[i, "level"])], 
                                             aes(x = Delta, y = Mean)) +
                                        geom_hline(yintercept = 0, linewidth = 0.2, linetype = 2) +
                                        geom_vline(xintercept = 0, linewidth = 0.2, linetype = 2) +    
                                        geom_ribbon(aes(ymin = CI_low,
                                                        ymax = CI_high, fill = From),
                                                    alpha = alpha) +
                                        geom_line(aes(colour = From), linewidth = 1) +
                                        geom_text(aes(label = Sig, colour = From), 
                                                  size = 7, nudge_x = 0.05, nudge_y = 0.1, 
                                                  show.legend = FALSE) +
                                        scale_colour_manual(values = col) +
                                        scale_fill_manual(values = colf) +
                                        labs(x = paste0("Difference in ", rg[i, "parts"], " at Within-person Level"),
                                             y = paste0("Estimated Difference"),
                                             title =  paste0("Reallocation of ", rg[i, "parts"], " and ", rg[i, "resp"], " at Within-person Level")) +
                                        facet_wrap(ggplot2::vars(From, To),
                                                   labeller = label_bquote(cols = .(as.character(From)) %<-% phantom() ~ minutes ~ phantom() %->% .(as.character(To))),
                                                   strip.position = "bottom") +
                                        scale_x_continuous(limits = c(-60, 60),
                                                           breaks = c(-60, 0, 60)) +
                                        scale_y_continuous(limits = c(-0.5, 0.5),
                                                           breaks = c(-0.5, -0.25, 0, 0.25, 0.5),
                                                           sec.axis = sec_axis(~ . / rg[i, "smean"], name = paste0("Standardised Estimated Difference"))
                                        ) +
                                        hrbrthemes::theme_ipsum() +
                                        theme(
                                          axis.ticks          = element_blank(),
                                          panel.background    = element_blank(),
                                          panel.border        = element_blank(),
                                          panel.grid.major    = element_blank(),
                                          panel.grid.minor    = element_blank(),
                                          plot.background     = element_rect(fill = "transparent", colour = NA),
                                          strip.background    = element_rect(fill = "transparent", colour = NA),
                                          strip.text          = element_text(size = 13, hjust = .5),
                                          strip.placement     = "outside",
                                          axis.title.x        = element_blank(),
                                          axis.title.y        = element_text(size = 12, hjust = .5),
                                          axis.title.y.right  = element_text(size = 12, hjust = .5, angle = 270),
                                          plot.title          = element_text(size = 13, hjust = .5, face = "italic"),
                                          plot.title.position = "plot",
                                          plot.margin         = margin(.5, .5, 1.5, .5, "cm"),
                                          legend.position     = "none"
                                        )
                                    }

names(sleep_affect_plot_w_supp) <- foreach(i = 17:32) %dopar% {
  paste0(rg[i, "level_labels"], "Reallocation of ", rg[i, "parts"], " and ", rg[i, "resp"])
}
saveRDS(sleep_affect_plot_w_supp, paste0(outputdir, "sleep_affect_plot_w_supp", ".RDS"))

