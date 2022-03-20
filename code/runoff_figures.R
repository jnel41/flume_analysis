library("tidyverse"); theme_set(theme_bw())
library("lme4")
library("lmerTest")
library("emmeans")
library("ggResidpanel")
library("data.table")
library("stringr")
library("lubridate")

rain_2016 <- rain_events[rain_events$Year == 2016,]


g16 <- ggplot(rain_events %>%
              group_by(SiteID, julian) %>%
              filter(Year == 2016) %>%
              summarize(rain_total = sum(rain),
                        .groups = "drop"), 
            aes(x = julian, 
                y = rain_total)) + 
  geom_line() +
  geom_point() +
  facet_grid(SiteID~., scales = "free_y") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  ggtitle("2016 Rainfall Accumulation by Julian Day") +
  xlab("Julian Day") + 
  ylab("Rainfall Accumulation (m)")
  #geom_vline(xintercept= julian, color='red',alpha=.3)

g17 <- ggplot(rain_events %>%
              group_by(SiteID, julian) %>%
              filter(Year == 2017) %>%
              summarize(rain_total = sum(rain),
                        .groups = "drop"), 
            aes(x = julian, 
                y = rain_total)) + 
  geom_line() +
  geom_point() +
  facet_grid(SiteID~., scales = "free_y") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  ggtitle("2017 Rainfall Accumulation by Julian Day") +
  xlab("Julian Day") + 
  ylab("Rainfall Accumulation (m)")
#geom_vline(xintercept= julian, color='red',alpha=.3)

g18 <- ggplot(rain_events %>%
                group_by(SiteID, julian) %>%
                filter(Year == 2018) %>%
                summarize(rain_total = sum(rain),
                          .groups = "drop"), 
              aes(x = julian, 
                  y = rain_total)) + 
  geom_line() +
  geom_point() +
  facet_grid(SiteID~., scales = "free_y") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  ggtitle("2018 Rainfall Accumulation by Julian Day") +
  xlab("Julian Day") + 
  ylab("Rainfall Accumulation (m)")

g19 <- ggplot(rain_events %>%
                group_by(SiteID, julian) %>%
                filter(Year == 2019) %>%
                summarize(rain_total = sum(rain),
                          .groups = "drop"), 
              aes(x = julian, 
                  y = rain_total)) + 
  geom_line() +
  geom_point() +
  facet_grid(SiteID~., scales = "free_y") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  ggtitle("2019 Rainfall Accumulation by Julian Day") +
  xlab("Julian Day") + 
  ylab("Rainfall Accumulation (m)")

g20 <- ggplot(rain_events %>%
                group_by(SiteID, julian) %>%
                filter(Year == 2020) %>%
                summarize(rain_total = sum(rain),
                          .groups = "drop"), 
              aes(x = julian, 
                  y = rain_total)) + 
  geom_line() +
  geom_point() +
  facet_grid(SiteID~., scales = "free_y") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  ggtitle("2020 Rainfall Accumulation by Julian Day") +
  xlab("Julian Day") + 
  ylab("Rainfall Accumulation (m)")



rain_2016 %>%
  select(Year,SiteID,event,julian,rain) %>%
  group_by(Year,SiteID, event) %>%
  mutate(rainfall=sum(rain), 
         event = factor(event)) %>%
  ggplot()+
  geom_col(mapping = aes(x=julian,y=rainfall, fill=event),
           position = position_dodge(width=0.1))+
  #geom_line(mapping = aes(x=julian,y=rainfall, color=site)) +
  facet_wrap(SiteID~.,ncol = 1,scales = 'free_y') +
  scale_color_gradientn(colours = rainbow(5))
