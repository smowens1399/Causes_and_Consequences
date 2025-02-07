################## Create a figure for winter growth ##############
fig_color=c("#FFAD48", "#024B79")

fig_growth<-ggplot(smolt_size, aes(x = strategy, y = growth, fill = strategy)) +
  geom_boxplot(position = position_dodge(width = 0.8), alpha = 0.7) +
  labs(x = "", y = " Winter Growth (mm)", fill = "Strategy") +
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
