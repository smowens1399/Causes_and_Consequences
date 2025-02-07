################## Create a figure for ocean entry ##############
fig_color=c("#FFAD48", "#024B79")

fig_oe<-ggplot(oe, aes(x = strat, y = doy, fill = strat)) +
  geom_boxplot(position = position_dodge(width = 0.8), alpha = 0.7) +
  labs(x = "", y = "Ocean Entry (DOY)", fill = "Strategy") +
  theme_bw() +
  theme(
    axis.text.x = element_blank(),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.text = element_text(size = 12),
    legend.margin = margin(-15, 0, 0, 0),
    text = element_text(family = "serif")
  ) +
  scale_fill_manual(values = fig_color)
