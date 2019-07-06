library(tidyverse)

source("ebt_mds.txt") # Einlesen des minimal Dataset

ebt_mds_full <-
  ebt_mds$Date %>%                    # aus den Eingabezeitpunkten ...
  full_seq(1) %>%                     # einen Vektor aller möglichen Zeitpunkte erzeugen ...
  tibble(Date = .) %>%                # in einen Tibble umwandeln ...
  left_join(ebt_mds, by = "Date") %>% # und die Daten zu den Eingabezeitpunkten einfügen.
  mutate(Day = lubridate::wday(x = Date, week_start = 1, label = TRUE),
         #Deno = purrr::map(.x = Deno, .f = ~ if(is.null(.)) integer(7) else as.integer(.)), # ersetze die aus dem Joinen (auch schon bei der Erstellung) entstandenen NULL
         Deno = purrr::map(.x = Deno, .f = function(x = .) c(x, double(7 - length(x))) %>% as.integer()),
         Count = map_int(.x = Deno, .f = ~ sum(.)),
         Value = map_int(.x = Deno, .f = ~ (t(.) %*% c(5, 10, 20, 50, 100, 200, 500)) %>% as.integer()),
         Hits = as.integer(Hits),
         nLoc = map_int(.x = Loc, .f = ~ length(.))) %>%
  select(Date, Day, Deno, Count, Value, Hits, Loc, nLoc)
# Immer englische Abkürzungen für die Wochentage verwenden [wday() nimmt aus locale()]
levels(ebt_mds_full$Day) <- c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")



### Expandieren der Denos
ebt_mds_full %>% 
  bind_cols(
    ebt_mds_full$Deno %>%
      do.call(rbind, .) %>%
      as_tibble(.name_repair = "minimal") %>%
      setNames(., paste0("E", c(5, 10, 20, 50, 100, 200, 500) %>% sprintf("%03d", .)))
  )
