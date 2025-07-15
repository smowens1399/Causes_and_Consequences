######################## Prepare Database for Modeling #######################
library(tidyverse)
### Prep independent variables
## Load Processed data for independent variables
body_mass <- read_csv("processed_data/causes/body mass.csv")
degree_days <- read_csv("processed_data/causes/degreedays.csv")
density <- read_csv("processed_data/causes/density_reddcounts.csv")
discharge <- read_csv("processed_data/causes/discharge.csv")

## Merge dependent variables into a single database
iv <- body_mass %>%
  right_join(density, by = "Brood Year") %>%
  right_join(degree_days, by = "Brood Year") %>%
  right_join(discharge, by = "Brood Year")

### Prep dependent variable
## Reference abundance DB
dv<-abundance
## Find Total NRR and DSR for each year
dv <- dv %>%
  group_by(BY) %>%
  summarise(
    Total_DSR= sum(Estimate[LH == "DSR"]),  
    Total_NRR= sum(Estimate[LH == "NRR"])  
  )

## Rename BY column
## Remove BY 2021
dv <- dv %>%
  filter(BY != 2021)
dv <- dv %>%
  rename(`Brood Year` = BY)


### Merge IV and DV into single database
model_inputs<-iv%>%
  right_join(dv)
