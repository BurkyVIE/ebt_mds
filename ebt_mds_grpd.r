ebt_mds_grpd <- function(mds_data = ebt_mds, ytd = FALSE, ytd_day = NULL, ytd_month = NULL, period = NULL, grp_nm = "Period", reverse = FALSE) {
  # mds_data    mds_data-artige Daten
  # ytd         year-to-date Auswertung; Parameter 'period' und 'grp_nm' werden ignoriert
  # ytd_day     year-to-date Tag
  # ytd_month   year-to-date Monat
  # period      gruppierende (Zeit-)Variable
  # grp_nm      Bezeichnung im Ergebnis
  # reverse     Umkehren der Reihenfolge im Ergebnis (= jüngster Zeitraum zuerst)
  
  # Notwendige libraries
  library(tidyverse)
  library(lubridate)
  
  # Evaluation der Gruppe/des Gruppennamens
  grouping = sym(grp_nm)
  grouping_nm = as_label(grouping)
  
  # Auswahl der Periode 
  period_list = c("overall", "weekday", "day", "week", "month", "quarter", "halfyear", "year")
  
  if(ytd) period <- "year" # für year-to-date
  if(is.null(period)) period <- period_list[menu(period_list, title = "choose period")] # wenn keine Auswahl im Funktionsaufruf
  if(identical(period, character(0))) return(NULL) # beende Funktion wenn keine Auswahl (= 0)
  
  # Erstelle vollständige Liste der möglichen Daten (= Datümer)
  c(today(tzone = "CET"),                # bis inkl heute ... --- CET weil in Arbeit keine std-tz gesetzt
    pull(mds_data, Date)) |>             # aus den Eingabezeitpunkten ...
    full_seq(1) |>                       # einen Vektor aller möglichen Zeitpunkte erzeugen ...
    (\(d) tibble("Date" = d))() |>       # in einen Tibble umwandeln (anonymous function) ...
    left_join(mds_data, by = "Date") |>  # und die Daten zu den Eingabezeitpunkten einfügen.
    # Fülle Deno auf 7 Stellen auf und wandle Hits in integer (inkl NA in 0) um
    mutate(Deno = map(.x = Deno, .f = ~ as.integer(c(., numeric(7))[1:7])),
           Hits = as.integer(Hits)) %>%
    replace_na(list(Hits = 0L)) -> tmp
  
  # year-to-date
  if(ytd) {
    if(is.null(ytd_day)) ytd_day <- as.numeric(format(today(tzone = "CET"), "%d"))
    if(is.null(ytd_month)) ytd_month <- as.numeric(format(today(tzone = "CET"), "%m"))
    d <- ytd_day; m <- ytd_month
    tmp <- tmp %>% 
      filter(month(Date) < m | (month(Date) == m & day(Date) <= d))
  }
  
  # Spezialfälle 
  skip <- FALSE
  if(period == "day") {
    tmp <- tmp %>% rename(!!grouping_nm := Date)
    skip <- TRUE
  }
  if(period == "weekday") {
    tmp <- tmp %>% mutate(!!grouping_nm := lubridate::wday(x = Date, week_start = 1, label = TRUE) %>%
                            ordered(labels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")))
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
    mutate(map2_df(                                                   # map2 auf ...
      map(.x = Deno, .f = ~ rep(c(5, 10, 20, 50, 100, 200, 500), .)), # 1) expandierte Denos (eigenes map) und ...
      Loc,                                                            # 2) Locations ...
      ~ data.frame(Count = length(.x),                                # eine Reihe von Funktionen gleichzeitig
                   Value = as.integer(sum(.x)),
                   nLoc = length(.y),
                   Avg = mean(.x),
                   Med = median(.x),
                   SD = sd(.x))),
      HitRt = Count / Hits, # Ableitungen
      EntRt = Count / Days,
      LocRt = nLoc / Days,
      AvPctl = Avg %>% ecdf(.)(.),   # empirische Verteilungsfunktionen
      HRPctl = HitRt %>% ecdf(.)(.),
      ERPctl = EntRt %>% ecdf(.)(.),
      LRPctl = LocRt %>% ecdf(.)(.))
  
  # Labels
  if(period == "weekday") {
    tmp <- tmp %>% mutate(Label = str_sub(!!grouping, 1, 3)) %>% relocate(Label, .after = Loc)
  }
  if(period == "week") {
    tmp <- tmp %>% mutate(Label = strftime(!!grouping, "%Y W%V")) %>% relocate(Label, .after = Loc)
  }
  if(period == "month") {
    tmp <- tmp %>% mutate(Label = strftime(!!grouping, "%Y-%m")) %>% relocate(Label, .after = Loc)
  }
  if(period %in% c("2 month", "2 months")) {
    tmp <- tmp %>% mutate(Label = paste0(year(!!grouping), " ", (month(!!grouping) - 1) %/% 2 + 1, "/6")) %>% relocate(Label, .after = Loc)
  }
  if(period %in% c("quarter", "3 month")) {
    tmp <- tmp %>% mutate(Label = paste0(year(!!grouping), " Q", quarter(!!grouping))) %>% relocate(Label, .after = Loc)
  }
  if(period %in% c("4 month", "4 months")) {
    tmp <- tmp %>% mutate(Label = paste0(year(!!grouping), " ", (month(!!grouping) - 1) %/% 4 + 1, "/3")) %>% relocate(Label, .after = Loc)
  }
  if(period %in% c("halfyear", "6 month", "6 months")) {
    tmp <- tmp %>% mutate(Label = paste0(year(!!grouping), " / ", c("I", "II")[semester(!!grouping)])) %>% relocate(Label, .after = Loc)
  }
  if(period == "year") {
    tmp <- tmp %>% mutate(Label = year(!!grouping)) %>% relocate(Label, .after = Loc)
  }
  if(period %in% c("10 year", "10 years")) {
    tmp <- tmp %>% mutate(Label = paste0(year(!!grouping) %/% 10, "0s")) %>% relocate(Label, .after = Loc)
  }
  if(ytd) {
    tmp <- tmp %>% mutate(Label = paste0("YTD ", Label, "-", str_pad(ytd_month, 2, pad = "0"), "-", str_pad(ytd_day, 2, pad = "0")))
  }
  
  # Umkehren der Reihenfolge (letzte oben)
  if(reverse) tmp <- tmp %>%
    arrange(desc(!!grouping))
  
  # Rückgabe
  return(tmp)
  rm(tmp)
}
