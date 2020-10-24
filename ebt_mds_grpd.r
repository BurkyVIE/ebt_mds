ebt_mds_grpd <- function(mds_data = ebt_mds, period = NULL, invert = FALSE) {
  
  # Notwendige libraries
  library(tidyverse)
  library(lubridate)
  
  # Auswahl der Periode 
  period_list = c("overall", "day", "week", "month", "quarter", "halfyear", "year")
  if(is.null(period)) period <- period_list[menu(period_list, title = "choose periode")]
  if(identical(period, character(0))) return(NULL)
  
  # Erstelle vollständige Liste der möglichen Daten
 mds_data %>% 
    pull(Date) %>%                       # aus den Eingabezeitpunkten ...
    full_seq(1) %>%                      # einen Vektor aller möglichen Zeitpunkte erzeugen ...
    tibble("Date" = .) %>%               # in einen Tibble umwandeln ...
    left_join(mds_data, by = "Date") %>% # und die Daten zu den Eingabezeitpunkten einfügen.
    # Fülle Deno auf 7 Stellen auf und wandle Hits in integer um
    mutate(Deno = map(.x = Deno, .f = ~ as.integer(c(., numeric(7))[1:7])),
           Hits = as.integer(Hits)) %>%
    replace_na(list(Hits = 0)) -> tmp
  # Wenn tageweise, dann kein gruppieren notwendig
  if(period == "day") tmp <- tmp %>%
    mutate(Days = 1) %>%
    select(Date, Deno, Loc, Days, Hits) %>%
    rename(Period = Date)
  else {
    # Ergänze Periode
    if(period == "overall") tmp <- tmp %>% mutate(Period = "overall")
    else tmp <- tmp %>% mutate(Period = floor_date(Date, unit = period, week_start = 1)) # ceiling_date(Date, unit = period, week_start = 1) - days(1) ... führt leider zu ungewohnten Effekten in Grafiken
    tmp <- tmp %>% 
      group_by(Period) %>% 
      # Zusammenfassen entsprechend Periode
      summarise(Deno = list(Reduce(`+`, Deno)),
                Loc = list(Reduce(union, Loc)),
                Days = n(),
                Hits = sum(Hits, na.rm = TRUE),
                .groups = "drop")
  }
  # Diverse Ableitungen
  tmp <- tmp %>% 
    mutate(Count = map_int(.x = Deno, .f = ~ sum(.)),
           Value = map_int(.x = Deno, .f = ~ (t(.) %*% c(5, 10, 20, 50, 100, 200, 500)) %>% as.integer()),
           nLoc = map_int(.x = Loc, .f = ~ length(.)),
           Avg = Value / Count,
           HitRt = Count / Hits,
           EntRt = Count / Days,
           LocRt = nLoc / Days,
           EntRtPerc = ecdf(EntRt)(EntRt))
  # Umkehren der Reihenfolge (letzte oben)
  if(invert) tmp <- tmp %>%
    arrange(desc(Period))
  # Rückgabe
  return(tmp)
}
