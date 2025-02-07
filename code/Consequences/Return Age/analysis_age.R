################### Analysis for age at ocean return ######################
library(tidyverse)
### Read in Data
age<-read.csv("processed_data/consequences/age.csv")

### Perform t-test
t.test(age~strat,data=age)
