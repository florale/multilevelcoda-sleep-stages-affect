source("sleep-affect-utils.R")

sleep <- c("Total Wake Time (TWT)", "Light Sleep", "Slow Wave Sleep (SWS)", "Rapid Eye Movement (REM) Sleep")
sleep_composition_a <- data.frame("sleep" = sleep,
                                  "prop" = c(60/480, 180/480, 90/480, 150/480), 
                                  "prop_labels" = c("60m\nTWT", "180m\nLight", "90m\nSWS", "150m\nREM"),
                                  label = "A")
sleep_composition_b <- data.frame("sleep" = sleep,
                                  "prop" = c(60/480, 180/480, 60/480, 180/480), 
                                  "prop_labels" = c("60m\nTWT", "180m\nLight", "60m\nSWS", "180m\nREM"),
                                  label = "B")
sleep_composition_c <- data.frame("sleep" = sleep,
                                  "prop" = c(30/480, 180/480, 90/480, 180/480), 
                                  "prop_labels" = c("30m\nTWT", "180m\nLight", "90m\nSWS", "180m\nREM"),
                                  label = "C")
sleep_composition <- rbind(sleep_composition_a,
                           sleep_composition_b,
                           sleep_composition_c)

sleep_composition$sleep <- factor(sleep_composition$sleep, ordered = TRUE,
                                  levels = c("Total Wake Time (TWT)",
                                             "Light Sleep",
                                             "Slow Wave Sleep (SWS)",
                                             "Rapid Eye Movement (REM) Sleep"))

col <- c(`Total Wake Time (TWT)` = "#F3EBE1",
         `Light Sleep` = "#9DB3A8",
         `Slow Wave Sleep (SWS)` = "#708885",
         `Rapid Eye Movement (REM) Sleep` = "#FADBA3")
family <- "Arial Narrow"

grDevices::cairo_pdf(
  file = paste0(outputdir, "sleep_composition_media", ".pdf"),
  width = 9,
  height = 4.5,
)

ggplot(sleep_composition, aes(x = "", y = prop, fill = sleep)) +
  geom_bar(stat = "identity", width = 1.3, size = 1.3) +
  coord_polar("y", start = 0) +
  geom_text(
    aes(label = prop_labels),
    position = position_stack(vjust = 0.5),
    size = 3.5,
    family = family
  ) +
  facet_wrap(~ label) +
  scale_fill_manual(values = alpha(col, 0.8), name = "Minutes in") +
  hrbrthemes::theme_ipsum() +
  theme(
    panel.background   = element_blank(),
    panel.border       = element_blank(),
    panel.grid.major   = element_blank(),
    panel.grid.minor   = element_blank(),
    plot.background    = element_rect(fill = "transparent", colour = NA),
    strip.background   = element_rect(fill = "transparent", colour = NA),
    axis.title.x       = element_blank(),
    axis.title.y       = element_blank(),
    axis.text.x        = element_blank(),
    axis.text.y        = element_blank(),
    strip.text         = element_text(size = 13, hjust = 0, face = "bold"),
    legend.title       = element_text(size = 12, face = "bold"),
    legend.text       = element_text(size = 12),
    plot.margin        = margin(.5, .5, .5, .5, "cm"),
    legend.position    = "bottom"
  )

dev.off()

ggplot(sleep_composition, aes(x = "", y = prop, fill = sleep)) +
  geom_bar(stat = "identity", width = 1.3, size = 1.3) +
  coord_polar("y", start = 180) +
  geom_text(
    aes(label = prop_labels),
    position = position_stack(vjust = 0.5),
    size = 3.5,
    family = family
  ) +
  facet_wrap(~ label) +
  scale_fill_manual(values = alpha(col, 0.8), name = "Minutes in") +
  hrbrthemes::theme_ipsum() +
  theme(
    panel.background   = element_blank(),
    panel.border       = element_blank(),
    panel.grid.major   = element_blank(),
    panel.grid.minor   = element_blank(),
    plot.background    = element_rect(fill = "transparent", colour = NA),
    strip.background   = element_rect(fill = "transparent", colour = NA),
    axis.title.x       = element_blank(),
    axis.title.y       = element_blank(),
    axis.text.x        = element_blank(),
    axis.text.y        = element_blank(),
    strip.text         = element_text(size = 13, hjust = 0, face = "bold"),
    legend.title       = element_text(size = 12, face = "bold"),
    legend.text       = element_text(size = 12),
    plot.margin        = margin(.5, .5, .5, .5, "cm"),
    legend.position    = "bottom"
  )
