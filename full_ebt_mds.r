library(tidyverse)
library(magrittr)
library(lubridate)

# Einlesen des minimal Dataset
source("H:/00_Burkhardt/DT/ebt_mds.txt")

ebt_mds_full <-
  ebt_mds$Date %>%
  full_seq(1) %>%
  tibble(Date = .) %>%
  left_join(ebt_mds, by = "Date") %>%
  mutate(nLoc = map(Loc, length) %>% unlist(),
         Day = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")[wday(Date, week_start = 1)] %>%
           parse_factor(levels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"), ordered = TRUE)) %>% 
  replace_na(replace = list("Count" = 0L, Value = 0L, Hits = 0L)) %>% 
  select(Date, Day, Count, Value, Hits, nLoc, Loc)
