library("tidyverse"); theme_set(theme_bw())
library("data.table")
library("stringr")
library("lubridate")

options(scipen=999999)

add_dates <- function(d) {
  data.frame(date_time = seq(as.POSIXct(d$eventStart, format='%m/%d/%Y %H:%M'),
                             as.POSIXct(d$eventEnd,   format='%m/%d/%Y %H:%M'),
                             by = 'mins'))
}

### Data wrangling
load(file="./data/raw/flume/clippedrainandflowdataallyears.rds")
df <- d
df$SiteID <- toupper(str_sub(df$watershed,1,3))
df$Treatment <- str_sub(df$treatment)

event <- df %>%
  mutate(SiteID = recode(site,'arm' = 'ARM',
                         'spirit' = 'HOE', 
                         'eia' = 'EIA', 
                         'rhodes' = 'RHO',
                         'worle' = 'WOR', 
                         'white' = 'WHI', 
                         'mcnay' = 'MCN', 
                         'marsh' = 'MAR',
                         .default = 'site'),
         date_time = as.POSIXct(date_time, format = "%m/%d/%Y %H:%M"),
         Year = as.numeric(format(date_time, "%Y"))) %>%
  subset(Treatment != "rain") %>%
  ungroup() %>%
  select('SiteID','Treatment','date_time', 'rain', 'Year','level','flow','watershed','sampleID')

df2 <- subset(sed2, analyte == "TSS (mg/L)")

load <- df2 %>%
  mutate(SiteID = recode(site,'arm' = 'ARM',
                         'spirit' = 'HOE', 
                         'eia' = 'EIA', 
                         'rhodes' = 'RHO',
                         'worle' = 'WOR', 
                         'white' = 'WHI', 
                         'mcnay' = 'MCN', 
                         'marsh' = 'MAR',
                         .default = 'site'),
         date_time = as.POSIXct(date_time, format = "%m/%d/%Y %H:%M"),
         Year = as.numeric(format(date_time, "%Y")),
         tss_kg_ha = valueload*1.12,
         Treatment = recode(treatment, 
                            'treatment' = 'strips', 
                            .default= 'control'),
         area_ha = acres*0.404686) %>%
  ungroup() %>%
  select('SiteID','Treatment','date_time', 'Year','sampleID', 'tss_kg_ha', 'area_ha')

load_cumulative <- load %>%
  group_by(Year, SiteID, Treatment, sampleID) %>% 
  summarize(tss_sum = sum(tss_kg_ha)) %>%
  subset(tss_sum > 0.000003977139) # sample 3010 is below detecable limit https://beta-static.fishersci.com/content/dam/fishersci/en_US/documents/programs/scientific/technical-documents/white-papers/apha-total-suspended-solids-procedure-white-paper.pdf



### Rain event calculation
rf_flags <- event %>%
  group_by(SiteID, Year) %>%
  mutate(rainflag = ifelse(rain > 0,1,0)) %>%
  mutate(rainlength = rep(rle(rainflag)$lengths, rle(rainflag)$lengths)) %>%
  #mutate(eventflag = ifelse(rainflag == 1,1, 
  #      ifelse(rainflag == 0 & rainlength < 72, 1,0))) %>% #72 entries or at least 6 hours (Osterholz et al. 2021) of dry between wet events
  #  mutate(eventflag = ifelse(row_number() < 72 & rainflag == 0, 0, eventflag)) %>%
  #  mutate(eventid = rep(seq(1,length(rle(eventflag)$lengths)), rle(eventflag)$lengths)) 
  mutate(eventflag = ifelse(rainflag == 1,1, 
                            ifelse(rainflag == 0 & rainlength < 72, 1,0))) %>% #72 entries or at least 12 hours (Virginia) of dry between wet events
  mutate(eventflag = ifelse(row_number() < 72 & rainflag == 0, 0, eventflag)) %>%
  mutate(eventid = rep(seq(1,length(rle(eventflag)$lengths)), rle(eventflag)$lengths)) 

rain_pivot <- rf_flags %>% 
  # Select only the rain events
  filter(eventflag == 1) %>% 
  # Group by id
  group_by(SiteID, Year, eventid) %>% 
  summarize(
    precipitation = sum(rain),
    eventStart = first(date_time),
    eventEnd = last(date_time)
  ) %>% 
  # Compute time difference as duration of event, add 1 hour, knowing that the timestamp is the time when the rain record ends
  mutate(rain_time = as.numeric(difftime(eventEnd,eventStart, units = 'm')) + 1) #https://www.codegrepper.com/code-examples/python/select+rainfall+events+and+calculate+rainfall+event+total+from+time-series+data

rain_event <- rain_pivot[rain_pivot$precipitation > 0.00635, ] #>6.35 mm of rain (Osterholz et al. 2021)

write.csv(rain_event, "./data/tidy/rain_event12.csv",  row.names = FALSE)

## Sample+Rain event calculation
rf_times <- rain_event

rf_newtimes <- rf_times %>%
  group_by(SiteID, Year, eventid, precipitation, rain_time) %>%
  do(add_dates(.)) %>%
  ungroup()


rf_joined <- rf_newtimes %>% 
  left_join(load, by = c("SiteID", "date_time")) %>%
  group_by(SiteID, Treatment, Year.x, eventid, precipitation, rain_time) %>% 
  ungroup() %>%
  mutate(rf_event = eventid,
         Year = Year.x) %>%
  drop_na(sampleID) %>%
  select(SiteID, Year, precipitation, rain_time, date_time, sampleID, rf_event)

rf_cumulative <- rf_joined %>%
  group_by(Year, SiteID, rf_event, rain_time, sampleID) %>% 
  summarize(rain = sum(precipitation)) %>%
  distinct(Year, SiteID, rain_time, sampleID, rain)

write.csv(rf_cumulative, "./data/tidy/rfSed2_event12.csv",  row.names = FALSE)


### Runoff event calculation
ro_flags <- event %>%
  group_by(SiteID, Treatment, Year) %>%
  mutate(roflag = ifelse(flow > 0,1,0)) %>%
  mutate(rolength = rep(rle(roflag)$lengths, rle(roflag)$lengths)) %>%
  #mutate(eventflag = ifelse(roflag == 1,1, 
  #      ifelse(roflag == 0 & rolength < 72, 1,0))) %>% #72 entries or at least 6 hours (Osterholz et al. 2021) of dry between wet events
  #  mutate(eventflag = ifelse(row_number() < 72 & roflag == 0, 0, eventflag)) %>%
  #  mutate(eventid = rep(seq(1,length(rle(eventflag)$lengths)), rle(eventflag)$lengths)) 
  mutate(eventflag = ifelse(roflag == 1,1, 
                            ifelse(roflag == 0 & rolength < 144, 1,0))) %>% #72 entries or at least 12 hours (Virginia) of dry between wet events
  mutate(eventflag = ifelse(row_number() < 144 & roflag == 0, 0, eventflag)) %>%
  mutate(eventid = rep(seq(1,length(rle(eventflag)$lengths)), rle(eventflag)$lengths)) ## **manual edit WOR 2019 RO 4)

ro_event <- ro_flags %>% 
  # Select only the rain events
  filter(eventflag == 1) %>% 
  # Group by id
  group_by(SiteID, Treatment, Year, eventid) %>% 
  summarize(
    flow = sum(flow),
    eventStart = first(date_time),
    eventEnd = last(date_time)
  ) %>% 
  # Compute time difference as duration of event, add 1 hour, knowing that the timestamp is the time when the rain record ends
  mutate(flow_time = as.numeric(difftime(eventEnd,eventStart, units = 'm')) + 1)

write.csv(ro_event, "./data/tidy/runoff_event12.csv",  row.names = FALSE)


## Sample+Runoff event calculation
ro_times <- ro_event

ro_newtimes <- ro_times %>%
  group_by(SiteID, Year, Treatment, eventid, flow, flow_time) %>%
  do(add_dates(.)) %>%
  ungroup()


ro_joined <- ro_newtimes %>% 
  left_join(load, by = c("SiteID","Treatment", "date_time")) %>%
  group_by(SiteID, Treatment, Year.x, eventid, flow_time) %>% 
  ungroup() %>%
  mutate(ro_event = eventid,
         Year = Year.x) %>%
  drop_na(sampleID) %>%
  select(SiteID, Year, Treatment, flow, flow_time, date_time, sampleID, tss_kg_ha, area_ha, ro_event)

ro_cumulative <- ro_joined %>%
  group_by(Year, SiteID, ro_event, Treatment, flow, flow_time, sampleID) %>% 
  summarize(tss_sum = sum(tss_kg_ha)) %>%
  subset(tss_sum > 0.000003977139) # sample 3010 is below detecable limit https://beta-static.fishersci.com/content/dam/fishersci/en_US/documents/programs/scientific/technical-documents/white-papers/apha-total-suspended-solids-procedure-white-paper.pdf


write.csv(ro_cumulative, "./data/tidy/roSed2_event12.csv",  row.names = FALSE)
