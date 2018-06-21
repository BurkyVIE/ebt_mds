ebt_mds_grpd <- function(period = FALSE, mds_data = ebt_mds) {

  # Notwendige libraries
  library(tidyverse)
  library(lubridate)
  
  # Auswahl der Periode 
  period_list = c("week", "month", "3 month", "6 month", "year")
  if(!period %in% period_list) period <- period_list[menu(period_list, title = "choose periode")]
  if(identical(period, character(0))) return(NULL)
  
  # Erstelle volleständige Liste der möglichen Daten und ergänze Periode
  mds_data$Date %>%
    range() %>%
    full_seq(1) %>%
    tibble(Date = .) %>%
    transmute(Date = Date,
              Period = floor_date(Date, unit = period, week_start = 1)) %>%
    # Zusammenführen mit den mds-Daten und Zusammenfassen
    left_join(ebt_mds, "Date") %>% 
    replace_na(replace = list("Count" = 0L, Value = 0L, Hits = 0L)) %>%
    group_by(Period) %>% 
    summarise(Count = sum(Count, na.rm = TRUE),
              Value = sum(Value, na.rm = TRUE),
              Hits = sum(Hits, na.rm = TRUE),
              nLoc = Loc %>% unlist() %>% c() %>% unique() %>% length()) %>%
    mutate(Avg = Value / Count,
           HitRt = Count / Hits) %>% 
    select(Period, Count, Value, Avg, Hits, HitRt, nLoc) %>% 
    return()
  }
