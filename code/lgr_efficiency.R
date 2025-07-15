#### lgr detection efficiency ####
library(tidyverse)
#### load raw data
lgr_allsmolts<-read.csv("raw_data/ptagis/rawptagis_lgr_smolts_salmonbasin.csv")
lgs_allsmolts<-read.csv("raw_data/ptagis/rawptagis_lgs_smolts_salmonbasin.csv")
#### prep data
### lgr
## recognize dates with lubridate
lgr_allsmolts$First.Time.Value<-mdy_hms(lgr_allsmolts$First.Time.Value)
# extract just date (time value isn't needed)
lgr_allsmolts$Detection_Date <- as_date(lgr_allsmolts$First.Time.Value)
# cleanup by removing date/time column
lgr_allsmolts <- lgr_allsmolts %>% select(-First.Time.Value)
## create a new site column and name all as lgr
lgr_allsmolts$site<-"lgr"
# remove other site name rows
lgr_allsmolts <- lgr_allsmolts %>% select(-Mark.Site.Name)
lgr_allsmolts <- lgr_allsmolts %>% select(-Site.Name)
## extract when smolts were barged
lgr_allsmolts <- lgr_allsmolts %>%
  mutate(barged = if_else(Last.Antenna.Group.Name == "BARGE LOAD RACEWAY", "y", "n"))
## remove duplicates
lgr_allsmolts <- lgr_allsmolts %>%
  arrange(Tag.Code, Detection_Date) %>%  # Sort by Tag_Code and date
  distinct(Tag.Code, .keep_all = TRUE)
## remove last antenna group column
lgr_allsmolts <- lgr_allsmolts %>% select(-Last.Antenna.Group.Name)
### lgs
## recognize dates with lubridate
lgs_allsmolts$First.Time.Value<-mdy_hms(lgs_allsmolts$First.Time.Value)
# extract just date (time value isn't needed)
lgs_allsmolts$Detection_Date <- as_date(lgs_allsmolts$First.Time.Value)
# cleanup by removing date/time column
lgs_allsmolts <- lgs_allsmolts %>% select(-First.Time.Value)
## create a new site column and name all as lgs
lgs_allsmolts$site<-"lgs"
# remove other site name rows
lgs_allsmolts <- lgs_allsmolts %>% select(-Mark.Site.Name)
lgs_allsmolts <- lgs_allsmolts %>% select(-Site.Name)
lgs_allsmolts <- lgs_allsmolts %>% select(-Last.Antenna.Group.Name)

## remove duplicates
lgs_allsmolts <- lgs_allsmolts %>%
  arrange(Tag.Code, Detection_Date) %>%  # Sort by Tag_Code and date
  distinct(Tag.Code, .keep_all = TRUE)

#### merge lgr and lgs databases into one
lgr_efficiency<-bind_rows(lgr_allsmolts,lgs_allsmolts)

#### create a column for number of times a tag id is detected (2=detected at both dams, 1=missed at 1 dam or barged )
lgr_efficiency <- lgr_efficiency %>%
  add_count(Tag.Code, name = "tag_count")

#### summarize data to prep for efficiency
lgr_efficiency <- lgr_efficiency %>%
  mutate(Detection_Date = as.Date(Detection_Date)) %>%
  group_by(Detection_Date) %>%
  summarise(
    unique_tags_lgr = n_distinct(Tag.Code[site == "lgr"]),
    tag_count_1_lgs = sum(tag_count == 1 & site == "lgs"),
    barged_y_count = sum(barged == "y", na.rm = TRUE)
  ) %>%
  arrange(Detection_Date)
#### calculate daily efficiency
lgr_efficiency <- lgr_efficiency %>%
  mutate(
    daily_efficiency = unique_tags_lgr / 
      (unique_tags_lgr + tag_count_1_lgs * (1 - (barged_y_count / unique_tags_lgr)))
  )

