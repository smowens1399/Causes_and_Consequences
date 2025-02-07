################ Analysis for SAR #####################
library(tidyverse)
### Read in processed data
sar<-read.csv("processed_data/consequences/sar.csv")
### Prep for two-proportion z-test
sar_ztest <- sar %>%
  group_by(strategy) %>%
  summarise(
    Smolt_Sum = sum(smolts),
    Adult_Sum = sum(adults),
  )
### Run two-proportion z-test

sar_test<- prop.test(c(sar_ztest$Adult_Sum[1], sar_ztest$Adult_Sum[2]), c(sar_ztest$Smolt_Sum[1], sar_ztest$Smolt_Sum[2]), alternative="two.sided")

## Calculate 95% CI
# DSR
dsr_ci <- prop.test(sar_ztest$Adult_Sum[1], sar_ztest$Smolt_Sum[1], conf.level = 0.95)
#NRR
nrr_ci <- prop.test(sar_ztest$Adult_Sum[2], sar_ztest$Smolt_Sum[2], conf.level = 0.95)
# Print
dsr_ci$conf.int
nrr_ci$conf.int
