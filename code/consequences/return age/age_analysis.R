################### Analysis for age at ocean return ######################
library(tidyverse)
### Read in Data
age<-read.csv("processed_data/consequences/age.csv")
### filter for  brood year 2006-2019
age <- age %>% filter(!by %in% c(2018, 2020))


age_summary <- age %>%
  group_by(strat,by ) %>%
  summarise(
    count = n(),  
    .groups = "drop"
  )

### Perform t-test
t.test(age~strat,data=age)
