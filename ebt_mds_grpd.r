source("ebt_mds.txt")

ebt_mds_grpd <- function(period = FALSE, mds_data = ebt_mds_full) {

  # Notwendige libraries
  library(tidyverse)
  library(lubridate)
  
  # Auswahl der Periode 
  period_list = c("day", "week", "month", "3 month", "6 month", "year")
  if(!period %in% period_list) period <- period_list[menu(period_list, title = "choose periode")]
  if(identical(period, character(0))) return(NULL)
  
  # Erstelle vollständige Liste der möglichen Daten
  mds_data$Date %>%                      # aus den Eingabezeitpunkten ...
    full_seq(1) %>%                     # einen Vektor aller möglichen Zeitpunkte erzeugen ...
    tibble(Date = .) %>%                # in einen Tibble umwandeln ...
    left_join(mds_data, by = "Date") %>% # und die Daten zu den Eingabezeitpunkten einfügen.
    # Ergänze Periode
#    mutate(Period = ceiling_date(Date, unit = period, week_start = 1) - days(1)) %>% # Führt leider zu ungewohnten Effekten in GRafiken
    mutate(Period = floor_date(Date, unit = period, week_start = 1)) %>%
    group_by(Period) %>% 
    summarise(Days = n(),
              Deno = list(Reduce(`+`, Deno)),
              Count = sum(Count, na.rm = TRUE),
              Value = sum(Value, na.rm = TRUE),
              Hits = sum(Hits, na.rm = TRUE),
              Loc = list(Reduce(union, Loc)),
              nLoc = map_int(.x = Loc, .f = ~ length(.))) %>%
  mutate(EntRt = Count / Days,
           Avg = Value / Count,
           HitRt = Count / Hits,
           LocRt = nLoc / Days) %>% 
    return()
  }
