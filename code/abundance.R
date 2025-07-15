################ Estimate DSR & NRR Abundance at the BCST ###################
library(tidyverse)
library(scales)
### Manually write in IDFG Data
abundance <- data.frame(
  BY = c(2006,2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 
         2017, 2019, 2020, 2021, 2006,2007, 2008, 2009, 2010, 2011, 2012, 
         2013, 2014, 2015, 2016, 2017, 2019, 2020, 2021),
  LH = c("DSR","DSR", "DSR", "DSR", "DSR", "DSR", "DSR", "DSR", "DSR", "DSR", 
         "DSR", "DSR", "DSR", "DSR", "DSR", "NRR", "NRR", "NRR", "NRR", 
         "NRR", "NRR", "NRR", "NRR", "NRR", "NRR", "NRR", "NRR", "NRR", "NRR","NRR"),
  Estimate = c(44461,46555, 115920, 165406, 237540, 188168, 117007, 117458, 
               308155, 194874, 205450, 48156, 49298, 42206, 35973, 18981,9331, 
               15820, 17862, 10371, 23036, 12128, 10204, 15495, 10319, 
               9895, 8320, 18029, 13339, 8265)
)


### Plot 
abundance_plot=ggplot(abundance, aes(x = as.factor(BY), y = Estimate, fill = LH)) +
  geom_bar(stat = "identity", position = "dodge", colour = "black") +
  labs(x = "Brood Year", y = "Estimated Abundance") +
  scale_fill_manual(values = c("DSR" = "#FFAD48", "NRR" = "#024B79")) +
  scale_y_continuous(labels = label_comma()) +  # Format y-axis with commas
  theme_bw() +
  theme(
    text = element_text(family = "serif"),
    axis.text = element_text(size = 12),
    axis.text.x = element_text(angle = 70, hjust = 1, vjust = 0.95),  # Rotates x-axis labels
    legend.text = element_text(size = 14),
    axis.title = element_text(size = 14),
    axis.title.x = element_text(vjust = -.5),  # Moves x-axis label down
    legend.title = element_blank(),
    plot.margin = margin(t = 5, r = 5, b = 20, l = 5)  # Adds more space at the bottom
  )


### Save Figure
ggsave("figures/abundance.png", abundance_plot, width = 6.25, height = 3, dpi=600, units = "in")

