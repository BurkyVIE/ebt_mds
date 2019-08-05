ebt_mds_grpd <- function(period = FALSE, mds_data = ebt_mds) {

  # Notwendige libraries
  library(tidyverse)
  library(lubridate)
  
  # Auswahl der Periode 
  period_list = c("day", "week", "month", "3 month", "6 month", "year")
  if(!period %in% period_list) period <- period_list[menu(period_list, title = "choose periode")]
  if(identical(period, character(0))) return(NULL)
  
  # Erstelle vollständige Liste der möglichen Daten
  ebt_mds$Date %>%                      # aus den Eingabezeitpunkten ...
    full_seq(1) %>%                     # einen Vektor aller möglichen Zeitpunkte erzeugen ...
    tibble(Date = .) %>%                # in einen Tibble umwandeln ...
    left_join(ebt_mds, by = "Date") %>% # und die Daten zu den Eingabezeitpunkten einfügen.
    # Fülle Deno auf und ergänze Count und Value
    mutate(Deno = map(.x = Deno, .f = function(x = .) c(as.integer(x), integer(7 - length(x)))),
           Count = map_int(.x = Deno, .f = ~ sum(.)),
           Value = map_int(.x = Deno, .f = ~ (t(.) %*% c(5, 10, 20, 50, 100, 200, 500)) %>% as.integer())) %>% 
    # Ergänze Periode
#    mutate(Period = ceiling_date(Date, unit = period, week_start = 1) - days(1)) %>% # Führt leider zu ungewohnten Effekten in GRafiken
    mutate(Period = floor_date(Date, unit = period, week_start = 1)) %>%
    group_by(Period) %>% 
    summarise(Days = n(),
              Deno = list(Reduce(`+`, Deno)),
              Loc = list(Reduce(union, Loc)),
              Count = sum(Count, na.rm = TRUE),
              Value = sum(Value, na.rm = TRUE),
              Hits = sum(Hits, na.rm = TRUE),
              nLoc = map_int(.x = Loc, .f = ~ length(.))) %>%
    mutate(EntRt = Count / Days,
           Avg = Value / Count,
           HitRt = Count / Hits,
           LocRt = nLoc / Days) %>% 
    return()
  }
