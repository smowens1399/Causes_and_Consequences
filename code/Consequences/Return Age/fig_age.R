################# Create figure for return age #######################
library(tidyverse)


### Prepare data for ggplot
# Calculate percentages for each strategy
age <- transform(age,
                  percent = ave(age, strat, FUN = function(x) x / sum(x) * 100))
# Reorder the age groups from lowest to highest
age$age <- factor(age$age, levels = unique(sort(age$age)))
# Select colors
color=c("#FFAD48", "#024B79", "#43B7C2")

### Plot
age_fig<-ggplot(age, aes(x = strat, y = percent, fill = age)) +
  geom_col(position = "stack", stat = "identity") +
  labs(x = "", y = "Return Age Probability (%)", fill = "Adult Age") + # Adjusting axis and legend titles
  scale_fill_manual(values = color) +  # Specify custom fill colors
  theme_bw() +
  theme(legend.position = "top",  # Placing legend at the top
        text=element_text(size = 20, family = "serif"),
        axis.title = element_text(size = 20, family = "serif"),  # Adjusting axis title properties
        axis.text = element_text(size = 18, family = "serif"))  # Adjusting axis text properties
### Save
ggsave("figures/age_fig.png",age_fig,width=4,height=6.25,dpi=600,units="in")
