ebt_mds_grpd <- function(period = FALSE, mds_data = ebt_mds) {

  # Notwendige libraries
  library(tidyverse)
  library(lubridate)
  
  # Auswahl der Periode 
  period_list = c("day", "week", "month", "3 month", "6 month", "year")
  if(!period %in% period_list) period <- period_list[menu(period_list, title = "choose periode")]
  if(identical(period, character(0))) return(NULL)
  
  # Erstelle vollständige Liste der möglichen Daten
  mds_data$Date %>%                          # aus den Eingabezeitpunkten ...
    full_seq(1) %>%                          # einen Vektor aller möglichen Zeitpunkte erzeugen ...
    enframe(name = NULL, value = "Date") %>% # in einen Tibble umwandeln ...
    left_join(mds_data, by = "Date") %>%     # und die Daten zu den Eingabezeitpunkten einfügen.
    # Fülle Deno auf 7 Stellen auf und wandle Hits in integer um
    mutate(Deno = map(.x = Deno, .f = function(x = .) c(as.integer(x), integer(7 - length(x)))),
           Hits = as.integer(Hits)) %>% 
    # Ergänze Periode
#    mutate(Period = ceiling_date(Date, unit = period, week_start = 1) - days(1)) %>% # Führt leider zu ungewohnten Effekten in GRafiken
    mutate(Period = floor_date(Date, unit = period, week_start = 1)) %>%
    group_by(Period) %>% 
    # Zusammenfassen entsprechend Periode
    summarise(Deno = list(Reduce(`+`, Deno)),
              Loc = list(Reduce(union, Loc)),
              Days = n(),
              Hits = sum(Hits, na.rm = TRUE)) %>%
    # Diverse Ableitungen
    mutate(Count = map_int(.x = Deno, .f = ~ sum(.)),
           Value = map_int(.x = Deno, .f = ~ (t(.) %*% c(5, 10, 20, 50, 100, 200, 500)) %>% as.integer()),
           nLoc = map_int(.x = Loc, .f = ~ length(.)),
           Avg = Value / Count,
           HitRt = Count / Hits,
           EntRt = Count / Days,
           LocRt = nLoc / Days) %>% 
    return()
  }
