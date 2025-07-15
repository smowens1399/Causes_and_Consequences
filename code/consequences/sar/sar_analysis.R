################ model sar #####################
library(tidyverse)
library(lme4)
####glmm
### prep data for modeling
## create a column "failures" that count smolts that did not return as adults
sar_modelinput<-sar_annual%>%
  mutate(failures=round(total_smoltsabundance-total_adults))
## change brood year to a factor
sar_modelinput$Brood_Year<-as.factor(sar_modelinput$Brood_Year)

### model
## glmm with random effect for brood year and fixed effect for strategy
sar_model<-glmer(cbind(total_adults,failures)~Strategy+(1|Brood_Year), data=sar_modelinput ,family="binomial")

#summary of model
summary(sar_model)



