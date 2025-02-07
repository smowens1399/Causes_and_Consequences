library(lubridate)
library(dplyr)


### Read in raw data
bcstmark<-read.csv("raw_data/ptagis/rawptagis_bcstmark.csv")
bcstrecap<-read.csv("raw_data/ptagis/rawptagis_bcstrecap.csv")
tay<-read.csv("raw_data/ptagis/rawptagis_tay.csv")


### Merge all data sets
## Add event type identifier for each dataset
bcstmark <- bcstmark %>%
  mutate(Event_Type = "mark")

bcstrecap <- bcstrecap %>%
  mutate(Event_Type = "recap")

tay <- tay %>%
  mutate(Event_Type = "detection")

##Rename Columns
bcstmark <- bcstmark %>%
  rename(
    Tag_Code = Tag.Code,
    Classification_Site= Mark.Site.Name,
    Classification_Date = Mark.Date.MMDDYYYY,
    BCST_Length_mm = Length.mm,
    BCST_Weight_g = Weight.g
  )

bcstrecap <- bcstrecap %>%
  rename(
    Tag_Code = Tag.Code,
    Classification_Site= Mark.Site.Name,
    Classification_Date = Recap.Date.MMDDYYYY,
    Mark_Date=Mark.Date.MMDDYYYY,
    Recap_Site=Recap.Site.Name,
    UBC_Length_mm = Mark.Length.mm,
    UBC_Weight_g = Mark.Weight.g,
    BCST_Length_mm = Recap.Length.mm,
    BCST_Weight_g = Recap.Weight.g,
    
  )

tay <- tay %>%
  rename(
    Tag_Code = Tag.Code,
    Classification_Site= Site.Name,
    Classification_Date = First.Time.Value,
    UBC_Length_mm = Mark.Length.mm,
    UBC_Weight_g = Mark.Weight.g
  )

## Merge data sets
bc_fish=bind_rows(bcstmark,bcstrecap,tay)

### Modify Dates/Time
## Recognize date values as dates
bc_fish$Classification_Date <- parse_date_time(bc_fish$Classification_Date, orders = c("mdy", "mdy HMS"))
bc_fish$Mark_Date<-mdy(bc_fish$Mark_Date)

## Create a classification year column
bc_fish$Classification_Year<-year(bc_fish$Classification_Date)

## Create a tag month column
bc_fish$Classification_Month<-month(bc_fish$Classification_Date)

### Classify fish behavior
## Classify as NRR or DSR
bc_fish$Strategy <- ifelse(bc_fish$Classification_Month >= 3 & bc_fish$Classification_Month <= 5, "NRR", "DSR")

## Classify Brood Year
bc_fish$Brood_Year <- ifelse(bc_fish$Strategy == "DSR", bc_fish$Classification_Year - 1, bc_fish$Classification_Year - 2)


### Cleanup Dataset
## Filter out data outside of brood years 2007-2021
bc_fish<-bc_fish%>%
  filter(Brood_Year>=2007 & Brood_Year <= 2021)
## Remove un-needed columns and organize 
bc_fish <- bc_fish %>%
  select(Tag_Code,Event_Type,Brood_Year,Strategy,Classification_Site,Classification_Date,BCST_Length_mm,BCST_Weight_g,UBC_Length_mm,UBC_Weight_g,Mark_Date,Recap_Site)

### Save to processed data folder
write.csv(bc_fish, "processed_data/bc_fish.csv", row.names = FALSE)
