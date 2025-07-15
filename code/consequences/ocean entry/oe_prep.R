#### Caclulate Ocean Entry ####
library(tidyverse)
### load raw data from bonnevile juvenile bypasses
bon_smolts<-read.csv("raw_data/ptagis/rawptagis_bon_smolts.csv")
### prep data
## modify date and time
# recognize date/time value w/lubridate
bon_smolts$First.Time.Value<-mdy_hms(bon_smolts$First.Time.Value)
# extract just date (time value isn't needed)
bon_smolts$Detection_Date <- as_date(bon_smolts$First.Time.Value)
# cleanup by removing date/time column
bon_smolts <- bon_smolts %>% select(-First.Time.Value)
## rename columns
bon_smolts <- bon_smolts %>%
  rename(Tag_Code = Tag.Code)
### merge bon juvenile detections with strategy categorization database
## only matching tag IDs are merged, everything else is filtered out
ocean_entry <- inner_join(bc_fish, bon_smolts, by = "Tag_Code")
## cleanup by removing columns that aren't important for select analysis
ocean_entry <- ocean_entry %>% select(c(1,3,4,15))
### create new column for day of the year detected at bon
ocean_entry$detection_doy<-yday(ocean_entry$Detection_Date)
### summarize by doy of detection based on strategy and brood year
## summary by brood year and strategy
ocean_entry_summary_annual <- ocean_entry %>%
  group_by(Strategy, Brood_Year) %>%
  summarise(
    mean_doy = mean(detection_doy), 
    count = n(),  
    .groups = "drop"
  )

## summary by strategy
ocean_entry_summary_total <- ocean_entry %>%
  group_by(Strategy, ) %>%
  summarise(
    mean_doy = mean(detection_doy), 
    count = n(),  
    .groups = "drop"
  )
