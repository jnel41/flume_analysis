library("tidyverse"); theme_set(theme_bw())
library("lme4")
library("lmerTest")
library("emmeans")
library("ggResidpanel")
library("data.table")
library("stringr")
library("lubridate")

#runoff$SiteID <- toupper(str_sub(runoff$watershed,1,3))
d$SiteID <- toupper(str_sub(d$watershed,1,3))

#runoff$Treatment <- str_sub(runoff$watershed,-3)
d$Treatment <- str_sub(d$watershed,-3)

ro <- d %>%
  mutate(Treatment = recode(Treatment,'ctl' = "control", 
                                    'trt' ="strips", 
                                   .default = "control"),
         date = as.POSIXct(date_time, format = "%Y-%m-%dT%H:%M"),
         Year = as.numeric(format(date_time, "%Y"))) %>%
  select('SiteID','Treatment','date','Year','level','flow','watershed','sampleID')
## can remove quotes

e <- ro %>%
  filter(flow != "NA") %>%                                        #removes lines with no observation
  filter(flow != 0) %>%                                           #removes lines with 0 flow, but you would use rain variable
  select(date, SiteID, Treatment, Year, flow) %>%              #you can ignore, this just gets variables I am interested in
  group_by(SiteID, Year) %>%                                        #grouping
  spread(Treatment, flow) %>%                                     #spreads flow data into wide form. This is how I figured out to make unique events within a year               
  ungroup %>%                                                     #for some reason I had to ungroup and regroup here
  group_by(SiteID, Year) %>%
  mutate(event = (c(0, cumsum(diff(date) > 720))),
         julian = yday(date)) %>%       #makes a column assigning an event number,
  #new event starts when difference between
  #date_time is greater than 12 hours re: helmers paper
  arrange(SiteID, Year, date)                                  #sorting


runoff_events <- e %>%
  gather(Treatment, flow, control:strips) %>%                   #changing data back into long form
  arrange(Treatment, date) %>%                              #sorting
  filter(flow != "NA")                                                                                        #removing NA’s

write_csv(runoff_events,"./data/tidy/clipped_runoff_events.csv")

sample <- ro %>%
  filter(sampleID != "NA") %>%                                      
  select(date, SiteID, Treatment, Year, sampleID) %>%
  mutate(julian = yday(date))

#write.csv(runoff,"./data/tidy/runoff.csv", row.names = FALSE)

