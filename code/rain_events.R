library("tidyverse"); theme_set(theme_bw())
library("data.table")
library("stringr")
library("lubridate")


rain$Treatment <- str_sub(rain$watershed,-3)
rf <- rain %>%
  mutate(Treatment = recode(Treatment,'ctl' = "control", 
                            'trt' ="strips", 
                            .default = "control"),
         date = as.POSIXct(date_time, format = "%Y-%m-%d %H:%M:%S"),
         Year = as.numeric(format(date_time, "%Y")),
         SiteID = toupper(str_sub(watershed,1,3))) %>% 
  ungroup() %>%
  select('SiteID','Treatment','date','Year','rain','cumulative_rain')


f <- rf %>%
  filter(rain != "NA") %>%                                        #removes lines with no observation
  filter(rain > 0.00635) %>%                                           #removes lines with 0 flow, but you would use rain variable
  select(date, SiteID, Treatment, Year, rain) %>%              #you can ignore, this just gets variables I am interested in
  group_by(SiteID, Year) %>%                                        #grouping
  spread(Treatment, rain) %>%                                     #spreads flow data into wide form. This is how I figured out to make unique events within a year               
  ungroup %>%                                                     #for some reason I had to ungroup and regroup here
  group_by(SiteID, Year) %>%
  mutate(event = (c(0, cumsum(diff(date) > 360))), #https://www.sciencedirect.com/science/article/pii/S0022169414005526?casa_token=eZ6-8YQE7mUAAAAA:NWH1jtbcAb0rhiv7DasZizRMrdedI1yzm073r1C2IR4pO3SOU7LZTPAL2AiSH_I0s2ntb-jyGA, https://www.sciencedirect.com/science/article/pii/S0048969720355765?via%3Dihub#bb0295,
                                                  #https://acsess.onlinelibrary.wiley.com/doi/epdf/10.1002/jeq2.20308
         julian = yday(date)) %>%       #makes a column assigning an event number,
  #new event starts when difference between
  arrange(SiteID, Year, date)                                         #removes lines with 0 flow, but you would use rain variable



rf_events <- f %>%
  gather(Treatment, rain, control:strips) %>%                   #changing data back into long form
  arrange(Treatment, date) %>%                              #sorting
  filter(rain != "NA") %>%
  select(date, SiteID, Treatment, Year, rain, event)

write.csv(rain,"./data/tidy/rain.csv", row.names = FALSE)
write.csv(sed2,"./data/tidy/sed2.csv")
