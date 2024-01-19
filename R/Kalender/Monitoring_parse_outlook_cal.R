library(dplyr)
library(ical)
library(lubridate)
library(tidyverse)
library(stringr)

a <- "C:/Users/Alexander Wedel/Downloads/awcal2.ics" %>% ical_parse_df() %>%
  filter(str_detect(summary, 'LiGa')) %>% as_tibble() %>%
  mutate(Minuten = end - start,
         Datum = floor_date(start, unit = "day")) %>%
  select(c(summary,Datum,Minuten)) %>%
  arrange(desc(Datum))

write.csv(a, "C:/Users/Alexander Wedel/OneDrive - Deutsche Kinder- und Jugendstiftung/Alex/R/Kalender/testoutput.csv")

