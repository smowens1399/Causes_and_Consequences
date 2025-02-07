################ Combine COnsequences Plots ##################
library(cowplot)
library(tidyverse)

### Combine
consequences_figs = plot_grid(fig_growth,fig_ss,fig_oe, ncol = 3, align = "v")
print(consequences_figs)

### Save
ggsave("figures/consequences_fig.png",consequences_figs,width=6.5,height=2.4,dpi=600,units="in")
