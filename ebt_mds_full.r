library(tidyverse)

source("ebt_mds.txt") # Einlesen des minimal Dataset
source("ebt_mds_grpd.r") # Funktion zum Gruppieren

ebt_mds_full <-
  ebt_mds_grpd(per = "day") %>% 
  rename(Date = Period, Day = Days) %>% # Da jeweils nur ein Tag Datum stat Periode und Wochentag statt Anzahl Tage
  select(-EntRt, -LocRt) %>% # Beide Ableitungen entstehen durch Division durch 1
  mutate(Day = lubridate::wday(x = Date, week_start = 1, label = TRUE) %>% ordered(labels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")))
# Immer englische Abkürzungen für die Wochentage verwenden [wday() nimmt aus locale()]



# Kennzahlen
library(tidyverse)
bind_cols(
  ebt_mds_full %>% summarise_at(.vars = vars(Hits, Count, Value), .funs = ~sum(., na.rm = TRUE)),
  ebt_mds_full %>% summarise(Days = n()),
  ebt_mds_full %>% summarise(nLoc = map_int(.x = list(Reduce(union, Loc)), .f = ~ length(.)))
) %>%
  print()



### Expandieren der Denos
# ebt_mds_full %>% 
#   bind_cols(
#     ebt_mds_full$Deno %>%
#       do.call(rbind, .) %>%
#       as_tibble(.name_repair = "minimal") %>%
#       setNames(., paste0("E", c(5, 10, 20, 50, 100, 200, 500) %>% sprintf("%03d", .)))
#   )



### Wo war ich am
# pseudo %>%
#   filter(Loc %in% (ebt_mds_full %>%
#            filter(Date %in% lubridate::make_date(year = 2004:2019, month = 3, day = 21)) %>%
#            pull(Loc) %>% unlist() %>% unique())) %>%
#   select(Coords, Country:City) %>%
#   arrange(Country, ZIP)



### Wann war ich in
# pseudo %>%
#   filter(City == "Oldenburg") %>%
#   pull(Loc) -> tmp
# ebt_mds_full %>%
#   filter(purrr::map_lgl(.x = ebt_mds_full$Loc, .f = ~ (intersect(tmp, .) %>% length()) > 0)) %>%
#   pull(Date)
# rm(tmp)
