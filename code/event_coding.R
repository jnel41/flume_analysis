library("tidyverse"); theme_set(theme_bw())
library("data.table")
library("stringr")
library("lubridate")

options(scipen=999999)

add_dates <- function(d) {
  data.frame(date_time = seq(as.POSIXct(d$eventStart, format="%Y/%m/%d %H:%M"),
                             as.POSIXct(d$eventEnd,   format="%Y/%m/%d %H:%M"),
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
         Year = as.numeric(format(date_time, "%Y")),
         Treatment = recode(treatment, 
                            'treatment' = 'strips',
                            'rain' = 'rain',
                            .default= 'control')) %>%
  subset(Treatment != "rain") %>%
  ungroup() %>%
  dplyr::select('SiteID','Treatment','date_time', 'rain', 'Year','level','flow','watershed','sampleID')

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
         date_time = as.POSIXct(date_time, format = "%Y/%m/%d %H:%M"),
         Year = as.numeric(format(date_time, "%Y")),
         tss_kg_ha = valueload*1.12,
         Treatment = recode(treatment, 
                            'treatment' = 'strips', 
                            .default= 'control'),
         area_ha = acres*0.404686) %>%
  ungroup() %>%
  dplyr::select('SiteID','Treatment','date_time', 'Year','sampleID', 'tss_kg_ha', 'area_ha')

load_cumulative <- load %>%
  group_by(Year, SiteID, Treatment, sampleID) %>% 
  summarize(tss_sum = sum(tss_kg_ha)) %>%
  subset(tss_sum > 0.000003977139) # sample 3010 is below detecable limit https://beta-static.fishersci.com/content/dam/fishersci/en_US/documents/programs/scientific/technical-documents/white-papers/apha-total-suspended-solids-procedure-white-paper.pdf

write.csv(load_cumulative, "./data/tidy/load_eventUPDATE.csv",  row.names = FALSE)


### Rain event calculation
rf_flags <- event %>%
  group_by(SiteID, Year) %>%
  mutate(rainflag = ifelse(rain > 0,1,0)) %>%
  mutate(rainlength = rep(rle(rainflag)$lengths, rle(rainflag)$lengths)) %>%
  #mutate(eventflag = ifelse(rainflag == 1,1, 
  #                          ifelse(rainflag == 0 & rainlength < 288, 1,0))) %>% #288 entries or at least 24 hours (Levia and Herwitz 2002) of dry between wet events
  #mutate(eventflag = ifelse(row_number() < 288 & rainflag == 0, 0, eventflag)) %>%
  #mutate(eventid = rep(seq(1,length(rle(eventflag)$lengths)), rle(eventflag)$lengths)) 
#mutate(eventflag = ifelse(rainflag == 1,1, 
  #                          ifelse(rainflag == 0 & rainlength < 36, 1,0))) %>% #3 entries or at least 3 hours (Dunkerly 2008) of dry between wet events
  #mutate(eventflag = ifelse(row_number() < 36 & rainflag == 0, 0, eventflag)) %>%
  #mutate(eventid = rep(seq(1,length(rle(eventflag)$lengths)), rle(eventflag)$lengths))
  mutate(eventflag = ifelse(rainflag == 1,1, 
        ifelse(rainflag == 0 & rainlength < 72, 1,0))) %>% #72 entries or at least 6 hours (Osterholz et al. 2021) of dry between wet events
  mutate(eventflag = ifelse(row_number() < 72 & rainflag == 0, 0, eventflag)) %>%
  mutate(eventid = rep(seq(1,length(rle(eventflag)$lengths)), rle(eventflag)$lengths)) 
  #mutate(eventflag = ifelse(rainflag == 1,1, 
  #                          ifelse(rainflag == 0 & rainlength < 144, 1,0))) %>% #144 entries or at least 12 hours (Virginia 2018; Bracken 2008) of dry between wet events
  #mutate(eventflag = ifelse(row_number() < 144 & rainflag == 0, 0, eventflag)) %>%
  #mutate(eventid = rep(seq(1,length(rle(eventflag)$lengths)), rle(eventflag)$lengths)) 
  
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
  mutate(rain_time = as.numeric(difftime(eventEnd,eventStart, units = 'mins')) + 1) #https://www.codegrepper.com/code-examples/python/select+rainfall+events+and+calculate+rainfall+event+total+from+time-series+data

rain_unique <- rain_pivot %>%
  subset(SiteID != 'MAR') %>%
  subset(subset=!(SiteID=="MCN" & Year == 2016)) %>%
  subset(subset=!(SiteID=="MCN" & Year == 2017)) %>%
  subset(subset=!(SiteID=="MCN" & Year == 2018)) %>%
  subset(subset=!(SiteID=="MCN" & Year == 2019)) %>%
  subset(subset=!(SiteID=="MCN" & Year == 2020)) %>%
  subset(subset=!(SiteID=="RHO" & Year == 2016)) %>%
  subset(subset=!(SiteID=="RHO" & Year == 2017)) %>%
  dplyr::select(Year, SiteID, precipitation, eventStart, eventEnd, rain_time) %>%
  distinct()

rain_event <- rain_unique[rain_unique$precipitation > 0.00635, ] #>6.35 mm of rain (Osterholz et al. 2021)
rain_event <- rain_unique # no minimum amount (Virginia 2018)
rain_event <- rain_unique[rain_unique$precipitation > 0.00254, ] # >0.254 mm of rain (Levia and Herwitz 2002; unspecified amount Levia 2004)

rain_event <- rain_event %>%
  group_by(SiteID, Year) %>%
  mutate(rf_event = 1:n()) %>%
  ungroup() %>%
  mutate(rain_mm = precipitation * 1000,
         rain_hr = rain_time/60,
         rf_rate = rain_mm/rain_hr,
         eventStart = as.POSIXct(eventStart, format = "%Y/%m/%d %H:%M"),
         eventEnd = as.POSIXct(eventEnd, format = "%Y/%m/%d %H:%M")) %>%
  dplyr::select(Year, SiteID, rf_event, rain_mm, rain_hr, rf_rate, eventStart, eventEnd)

write.csv(rain_event, "./data/tidy/rain_event3UPDATE.csv",  row.names = FALSE)

## Sample+Rain event calculation
rf_newtimes <- rain_event %>%
  group_by(Year, SiteID, rf_event, rain_mm, rain_hr, rf_rate) %>%
  do(add_dates(.)) %>%
  ungroup()

rf_joined <- rf_newtimes %>% 
  left_join(load, by = c("Year", "SiteID", "date_time")) %>%
  group_by(Year, SiteID, rf_event, rain_mm, rain_hr, rf_rate, sampleID) %>% 
  ungroup() %>%
  #mutate(rf_event = eventid,
  #       Year = Year.x) %>%
  drop_na(sampleID) %>%
  dplyr::select(Year, SiteID, date_time, rf_event, rain_mm, rain_hr, rf_rate, sampleID)

rf_cumulative <- rf_joined %>%
  distinct(Year, SiteID, rf_event, sampleID, rain_mm, rain_hr, rf_rate) #%>%
  #group_by(Year, SiteID, rf_event) %>% 
  #summarize(rain_mm = sum(rain_mm),
  #          rain_hr = sum(rain_he),
  #          rf_count = n()) %>%
  #ungroup() #%>%

write.csv(rf_cumulative, "./data/tidy/rfSed2_event6UPDATE.csv",  row.names = FALSE)


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
                            ifelse(roflag == 0 & rolength < 144, 1,0))) %>% #144 entries or at least 12 hours (Virginia) of dry between wet events
  mutate(eventflag = ifelse(row_number() < 144 & roflag == 0, 0, eventflag)) %>%
  mutate(eventid = rep(seq(1,length(rle(eventflag)$lengths)), rle(eventflag)$lengths)) 

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
  mutate(flow_time = as.numeric(difftime(eventEnd,eventStart, units = 'm')) + 1) %>%
  #subset(subset=!(SiteID=="WOR" & Treatment == "control" & Year == 2018 & eventid == 4))

ro_unique <- ro_event %>%
subset(SiteID != 'MAR') %>%
  subset(subset=!(SiteID=="MCN" & Year == 2016)) %>%
  subset(subset=!(SiteID=="MCN" & Year == 2017)) %>%
  subset(subset=!(SiteID=="MCN" & Year == 2018)) %>%
  subset(subset=!(SiteID=="MCN" & Year == 2019)) %>%
  subset(subset=!(SiteID=="MCN" & Year == 2020)) %>%
  subset(subset=!(SiteID=="RHO" & Year == 2016)) %>%
  subset(subset=!(SiteID=="RHO" & Year == 2017)) %>%
  dplyr::select(Year, SiteID,Treatment, eventid, flow, flow_time, eventStart, eventEnd) %>%
  distinct()
  
  
## **manual edit WOR 2018 RO 4 (WQ samples: 3010, 3016, 3019, 3027, 3030, 3035, 3039, 3043, 3044)**
write.csv(ro_unique, "./data/tidy/runoff_event12UPDATE.csv",  row.names = FALSE) 


## Sample+Runoff event calculation
#ro_times <- read.csv("./data/tidy/runoff_event12UPDATE.csv")

ro_newtimes <- ro_unique %>%
  group_by(SiteID, Year, Treatment, eventid, flow, flow_time) %>%
  do(add_dates(.)) %>%
  ungroup()


ro_joined <- ro_newtimes %>% 
  left_join(load, by = c("SiteID", "Treatment", "date_time")) %>%
  group_by(SiteID, Treatment, eventid, flow_time) %>% 
  mutate(Year = Year.x,
         ro_event = eventid) %>%
  drop_na(sampleID) %>%
  ungroup() %>%
  dplyr::select(SiteID, Year, Treatment, flow, flow_time, date_time, sampleID, tss_kg_ha, area_ha, ro_event, sampleID)

ro_cumulative <- ro_joined %>%
  group_by(Year, SiteID, Treatment, flow_time, flow, ro_event) %>% 
  summarize(tss_sum = sum(tss_kg_ha)) %>%
  ungroup() %>%
  #group_by(Year, SiteID, Treatment, sampleID) %>%
  #summarize(tss_sum = sum(tss_sum),
  #          flow_sum = sum(flow),
  #          flow_min = sum(flow_time),
  #          ro_count = n()) %>%
  subset(tss_sum > 0.000003977139) # sample 3010 is below detecable limit https://beta-static.fishersci.com/content/dam/fishersci/en_US/documents/programs/scientific/technical-documents/white-papers/apha-total-suspended-solids-procedure-white-paper.pdf


write.csv(ro_cumulative, "./data/tidy/roSed2_event12UPDATE.csv",  row.names = FALSE)

##Merging Sample+Runoff+Rain events into flume_event
rfro_joined <- ro_cumulative %>% 
  left_join(rf_cumulative, by = c("SiteID", "sampleID")) %>%
  drop_na(rain_mm) %>%
  mutate(Year = Year.x) %>%
  select(SiteID, Year, Treatment, sampleID, tss_sum, ro_count, flow_sum, flow_min, 
         rf_event, rain_mm, rain_hr, rf_rate)

write.csv(rfro_joined, "./data/tidy/rf12ro12event_UPDATE.csv",  row.names = FALSE)
  
  
