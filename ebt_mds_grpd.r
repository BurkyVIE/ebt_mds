ebt_mds_grpd <- function(mds_data = ebt_mds, period = NULL, grouping = Period, reverse = FALSE) {
  
  grouping = enquo(grouping)
  grouping_nm = quo_name(grouping)
  
  # Notwendige libraries
  library(tidyverse)
  library(lubridate)
  
  # Auswahl der Periode 
  period_list = c("overall", "weekday", "day", "week", "month", "quarter", "halfyear", "year")
  if(is.null(period)) period <- period_list[menu(period_list, title = "choose period")]
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
    replace_na(list(Hits = 0L)) -> tmp

  # Spezialfälle 
  skip <- FALSE
  if(period == "day") {
    tmp <- tmp %>% rename(!!grouping_nm := Date)
    skip <- TRUE
  }
  if(period == "weekday") {
    tmp <- tmp %>% mutate(!!grouping_nm := lubridate::wday(x = Date, week_start = 1, label = TRUE) %>% ordered(labels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")))
    skip <- TRUE
  }
  if(period == "overall") {
    tmp <- tmp %>% mutate(!!grouping_nm := "overall")
    skip <- TRUE
  }
  # Quasi: else-Zweig der Spezialfälle
  if(!skip) tmp <- tmp %>% mutate(!!grouping_nm := floor_date(Date, unit = period, week_start = 1))
  # ceiling_date(Date, unit = period, week_start = 1) - days(1) ... führt leider zu ungewohnten Effekten in Grafiken

  tmp <- tmp %>% 
    group_by(!!grouping) %>% 
    # Zusammenfassen entsprechend Periode
    summarise(Deno = list(Reduce(`+`, Deno)),
              Loc = list(Reduce(union, Loc)),
              Days = n(),
              Hits = sum(Hits, na.rm = TRUE),
              .groups = "drop")
  # Diverse Ableitungen
  tmp <- tmp %>% 
    mutate(Count = map_int(.x = Deno, .f = ~ sum(.)),
           Value = map_int(.x = Deno, .f = ~ (t(.) %*% c(5, 10, 20, 50, 100, 200, 500)) %>% as.integer()),
           nLoc = map_int(.x = Loc, .f = ~ length(.)),
           Avg = Value / Count,
           HitRt = Count / Hits,
           EntRt = Count / Days,
           LocRt = nLoc / Days,
           AvPctl = ecdf(Avg)(Avg),
           HRPctl = ecdf(HitRt)(HitRt),
           ERPctl = ecdf(EntRt)(EntRt),
           LRPctl = ecdf(LocRt)(LocRt))

  # Umkehren der Reihenfolge (letzte oben)
  if(reverse) tmp <- tmp %>%
    arrange(desc(!!grouping))

  # Rückgabe
  return(tmp)
}
