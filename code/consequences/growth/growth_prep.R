#### Size and Growth ####
library(tidyverse)
### load raw data 
## noaa marks in upper big creek
noaa<-read.csv("raw_data/ptagis/rawptagis_noaamark.csv")
## juvenile passive detections at lgr and lgs
lgrlgs_recaps<-read.csv("raw_data/ptagis/rawptagis_lgrlgs_recaps.csv")
###prep noaa data
## recognize date column as dates w/ lubridate
noaa$Mark.Date.MMDDYYYY<-mdy(noaa$Mark.Date.MMDDYYYY)
## cleanup
#rename columns
noaa <- noaa %>%
  rename(Tag_Code = Tag.Code)
noaa <- noaa %>%
  rename(mark_date = Mark.Date.MMDDYYYY)
# remove when length wasn't recorded
noaa <- noaa %>%
  filter(!is.na(Length.mm))
### prep lgr and lgs data
## date/time editing
# recognize date/time value w/lubridate
lgrlgs_recaps$Mark.Date.MMDDYYYY<-mdy(lgrlgs_recaps$Mark.Date.MMDDYYYY)
lgrlgs_recaps$Recap.Date.MMDDYYYY<-mdy(lgrlgs_recaps$Recap.Date.MMDDYYYY)
## rename columns
lgrlgs_recaps <- lgrlgs_recaps %>%
  rename(Tag_Code = Tag.Code)

## remove rows where length wasnt recorded at recapture
lgrlgs_recaps <- lgrlgs_recaps %>%
  filter(!is.na(Recap.Length.mm))

#### merge databases
### merge noaa data with strategy categorization database
## only matching tag ids are kept
noaa_bc_recaps <- inner_join(bc_fish, noaa, by = "Tag_Code")
# remove rows where recap length wasnt recorded (largely detections at tay)
noaa_bc_recaps <- noaa_bc_recaps %>%
  filter(!is.na(BCST_Length_mm))
## cleanup new database
#remove unnecessary columns
noaa_bc_recaps <- noaa_bc_recaps %>% select(c(1,3,4,13,9,6,7))
# rename columns
noaa_bc_recaps <- noaa_bc_recaps %>%
  rename(recap_date = Classification_Date)
noaa_bc_recaps <- noaa_bc_recaps %>%
  rename(recap_length = BCST_Length_mm)
noaa_bc_recaps <- noaa_bc_recaps %>%
  rename(mark_length = UBC_Length_mm)
### merge lgrlgs data w/ strategy categorization database
## only matching tag ids are kept
noaa_lgrlgs_recaps <- inner_join(bc_fish, lgrlgs_recaps, by = "Tag_Code")
## cleanup database
# remove unnecessary columns
noaa_lgrlgs_recaps <- noaa_lgrlgs_recaps %>% select(c(1,3,4,13,14,17,15))
# rename columns
noaa_lgrlgs_recaps <- noaa_lgrlgs_recaps %>%
  rename(recap_date = Recap.Date.MMDDYYYY)
noaa_lgrlgs_recaps <- noaa_lgrlgs_recaps %>%
  rename(recap_length = Recap.Length.mm)
noaa_lgrlgs_recaps <- noaa_lgrlgs_recaps %>%
  rename(mark_length = Mark.Length.mm)
noaa_lgrlgs_recaps <- noaa_lgrlgs_recaps %>%
  rename(mark_date = Mark.Date.MMDDYYYY)

#### filter so that nrr and dsr are only in proper databases
### nrr assessed by recap events at the bcst
nrr_growth<-noaa_bc_recaps%>%
  filter(Strategy=="NRR")
### dsr assessed by recap events at lgr/lgs
dsr_growth<-noaa_lgrlgs_recaps%>%
  filter(Strategy=="DSR")
#### merge into one growth database
growth_db<- bind_rows(dsr_growth,nrr_growth)
## filter out na lengths
growth_db <- growth_db %>%
  filter(!is.na(mark_length))
## recognize as dates
growth_db$recap_date <- as.Date(growth_db$recap_date)
growth_db$mark_date <- as.Date(growth_db$mark_date)
#### calcualte growth metrics
### total growth
growth_db$total_growth<-growth_db$recap_length-growth_db$mark_length
### growth rate (mm/day)
## calcaulte number of days between mark and recap
growth_db$growth_days<-growth_db$recap_date-growth_db$mark_date
# make numeric
growth_db$growth_days <- as.numeric(growth_db$growth_days)
## caclulate growth per day
growth_db$growth_rate<-growth_db$total_growth/growth_db$growth_days

#### summarize
## summary by brood year and strategy
growth_summary_annual<-growth_db%>%
  group_by(Strategy, Brood_Year) %>%
  summarise(
    mean_growth = mean(total_growth),
    mean_days = mean(growth_days),
    mean_growth_rate=mean(growth_rate),
    mean_smoltlength=mean(recap_length),
    count = n(),  
    .groups = "drop"
  )
## summary by just strategy (aggregated)
growth_summary_total<-growth_db%>%
  group_by(Strategy) %>%
  summarise(
    mean_growth = mean(total_growth),
    mean_days = mean(growth_days),
    mean_growth_rate=mean(growth_rate),
    mean_smoltlength=mean(recap_length),
    count = n(),  
    .groups = "drop"
  )
