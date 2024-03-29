sedTSS_pivot <- sedTSS %>% 
  # Group by id
  group_by(sampleID, site, year, treatment) %>% 
  summarize(
    precipitation = sum(rain),
    sampStart = first(date_time),
    sampEnd = last(date_time)
  ) %>% 
  # Compute time difference as duration of event, add 1 hour, knowing that the timestamp is the time when the rain record ends
  mutate(rain_time = as.numeric(difftime(sampEnd,sampStart, units = 'm')) + 1) %>% #https://www.codegrepper.com/code-examples/python/select+rainfall+events+and+calculate+rainfall+event+total+from+time-series+data
  subset(subset = sampleID %in% c("1000","1001", "1002", "1006", "1007", "1008", "1009", "1010", "1011",
         "1012", "1013", "1014", "1015", "1016", "1017", "1018", "1019", "1020", 
         "1021", "1023", "1024", "1025", "1026", "1027", "1028", "1030", "1031",
         "1032", "1033", "1034", "1035", "1036", "1037", "1038", "1039", "2000", 
         "2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009", 
         "2010", "2011", "2012", "2013", "2014", "2016", "2017", "2018", "2019",
         "2020", "2021", "2022", "2023", "2024", "2025", "2026", "2027", "3000", 
         "3001", "3002", "3003", "3004", "3005", "3006", "3007", "3008", "3009", 
         "3010", "3011", "3012", "3013", "3014", "3015", "3016", "3017", "3019",
         "3020", "3021", "3022", "3024", "3025", "3026", "3027", "3028", "3029", 
         "3030", "3031", "3032", "3033", "3034", "3035", "3036", "3037", "3038",
         "3039", "3041", "3042", "3043", "3044", "3045", "3046", "3048", "3049",
         "3050", "3051", "3052", "3053", "3054", "3055", "3056", "3057", "3058", 
         "3059", "3060", "3061", "3062", "3063", "3064", "3065", "3066", "3067", 
         "3068", "3069", "3070", "3071", "3072", "3073", "3074", "3075", "3076",
         "3077", "3078", "3079", "3080", "3081", "3200", "3201", "3202", "3205", 
         "3206", "3207", "3208", "3209", "3210", "3211", "3212", "3213", "4000",
         "4001", "4004", "4005", "4006", "4007", "4008", "4009", "4010", "4011",
         "4012", "4013", "4014", "4015", "4016", "4017", "4019", "4020", "4021", 
         "4022", "4023", "4024", "4025", "4026", "4027", "4028", "4029", "4031", 
         "4032", "4033", "4034", "4035", "4036", "4037", "4038", "4039", "4040", 
         "4041", "4043", "4044", "4045", "4046", "4047", "4048", "4049", "4050",
         "4051", "4052", "4053", "4056", "4058", "4059", "4060", "4061", "4062", 
         "4063", "4064", "4065", "4066", "4068", "4069", "4070", "4071", "4072", 
         "5000", "5001", "5002", "5003", "5005", "5006", "5007", "5008", "5009", 
         "5010", "5011", "5012", "5015", "5016", "5017", "5019", "5020", "6000",
         "6001", "6002", "6003", "6004", "6005", "6006", "6007", "6008", "6009", 
         "6010", "6011", "SPL1050", "SPL1051", "SPL1052", "SPL1053", "SPL2050",
         "SPL2051", "SPL2052", "SPL2053", "SPL2054", "SPL3300", "SPL3301", "SPL3302",
         "SPL3303", "SPL3304", "SPL3305", "SPL3306", "SPL3307", "SPL3309", "SPL3310", 
         "SPL4300", "SPL4301")) %>%
  mutate(SiteID = recode(site,'arm' = 'ARM',
                         'spirit' = 'HOE', 
                         'eia' = 'EIA', 
                         'rhodes' = 'RHO',
                         'worle' = 'WOR', 
                         'white' = 'WHI', 
                         'mcnay' = 'MCN', 
                         'marsh' = 'MAR',
                         .default = 'site'),
         Year = as.numeric(format(sampStart, "%Y")),
         Treatment = recode(treatment, 
                            'treatment' = 'strips',
                            .default= 'control')) %>%
  ungroup() %>%
  dplyr::select('sampleID', 'Year', 'SiteID', 'Treatment', 'sampStart', 'sampEnd', 'rain_time', 'precipitation')

  
write.csv(sedTSS_pivot, "./data/tidy/sample_dates.csv",  row.names = FALSE)
