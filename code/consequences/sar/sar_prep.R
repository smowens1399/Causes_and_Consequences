#### prepare and summarize SAR data ####
library(tidyverse)
### read in raw data
## lgr smolt detections
lgr_smolts<-read.csv("raw_data/ptagis/rawptagis_lgr_smolts_passive.csv")
## bon adult detections
bon_adults<-read.csv("raw_data/ptagis/rawptagis_bon_adult_detections.csv")
#### prep
### lgr
## recognize date/time using lubridate
lgr_smolts$First.Time.Value<-mdy_hms(lgr_smolts$First.Time.Value)
# extract just date (time value isn't needed)
lgr_smolts$Detection_Date <- as_date(lgr_smolts$First.Time.Value)
# cleanup by removing date/time column
lgr_smolts <- lgr_smolts %>% select(-First.Time.Value)
# remove duplicates
lgr_smolts <- lgr_smolts %>%
  arrange(Tag.Code, Detection_Date) %>%  # Sort by Tag_Code and date
  distinct(Tag.Code, .keep_all = TRUE)
# rename columns
lgr_smolts <- lgr_smolts %>%
  rename(Tag_Code = Tag.Code)
### bon
## recognize date/time using lubridate
bon_adults$First.Time.Value<-mdy_hms(bon_adults$First.Time.Value)
# extract just date (time value isn't needed)
bon_adults$Detection_Date <- as_date(bon_adults$First.Time.Value)
# cleanup by removing date/time column
bon_adults <- bon_adults %>% select(-First.Time.Value)
# remove duplicates
bon_adults <- bon_adults %>%
  arrange(Tag.Code, Detection_Date) %>%  # Sort by Tag_Code and date
  distinct(Tag.Code, .keep_all = TRUE)
# rename columns
bon_adults <- bon_adults %>%
  rename(Tag_Code = Tag.Code)
#### link datasets with strategy catgorization db to associate tag code's with strategy
### lgr
## only matching tag IDs are merged, everything else is filtered out
lgr_smolts <- inner_join(bc_fish, lgr_smolts, by = "Tag_Code")
## cleanup by removing unnecessary columns
lgr_smolts <- lgr_smolts %>% select(c(1,3,4,15))
### bon
## only matching tag IDs are merged, everything else is filtered out
bon_adults <- inner_join(bc_fish, bon_adults, by = "Tag_Code")
## cleanup by removing unnecessary columns
bon_adults <- bon_adults %>% select(c(1,3,4,15))
#### calculate smolt abundance
### summarize lgr smolt detections by detection data and strategy
lgr_smolts_adundance <- lgr_smolts %>%
  group_by(Strategy, Detection_Date) %>%
  summarise(
    total_individuals = n(),
    .groups = "drop"
  )
## add a column for brood year
lgr_smolts_adundance$Brood_Year<-year(lgr_smolts_adundance$Detection_Date)-2
### merge with lgr detection efficiency db
lgr_smolts_adundance <- inner_join(lgr_smolts_adundance, lgr_efficiency, by = "Detection_Date")
## cleanup by removing unnecessary columns
lgr_smolts_adundance <- lgr_smolts_adundance %>% select(c(1,2,4,3,8))
### estimate abundance
lgr_smolts_adundance$abundance<-lgr_smolts_adundance$total_individuals/lgr_smolts_adundance$daily_efficiency

#### calculate SAR
### summarize lgr smolts by brood year
lgr_smolts_summary <- lgr_smolts_adundance %>%
  group_by(Brood_Year, Strategy) %>%
  summarise(
    total_smolts = sum(total_individuals, na.rm = TRUE),
    total_smoltsabundance = sum(abundance, na.rm = TRUE),
    .groups = "drop"
  )
### summarize adults by brood year
bon_adults_summary <-bon_adults %>%
  group_by(Brood_Year, Strategy) %>%
  summarise(total_adults = n(), .groups = "drop")

### combine into one dataset showing by brood year 
sar_annual <- left_join(bon_adults_summary, lgr_smolts_summary, by = c("Brood_Year", "Strategy"))
## calculate sar 
sar_annual$sar<-sar_annual$total_adults/sar_annual$total_smoltsabundance
## filter to only show brood year 2006 - 2019
sar_annual <- sar_annual %>%
  filter(Brood_Year >= 2006, Brood_Year <= 2019, Brood_Year != 2018)
### combine into one dataset showing aggregated totals
sar_total <- sar_annual%>%
  group_by(Strategy)%>%
  summarise(
    total_adults = sum(total_adults,na.rm=TRUE),
    total_smolts = sum(total_smolts, na.rm = TRUE),
    total_smoltsabundance = sum(total_smoltsabundance, na.rm = TRUE),
  )
## calculate sar
sar_total$sar<-sar_total$total_adults/sar_total$total_smoltsabundance
