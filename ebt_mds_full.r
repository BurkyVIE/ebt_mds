# Initialisierung ----
## Libraries ----
library(tidyverse)

## notwendige Daten/Funktionen ----
source("ebt_mds.txt") # Einlesen des minimal Dataset
source("ebt_mds_grpd.r") # Funktion zum Gruppieren

## Funktion zur Erstellung eines streak ----
# (x ... welche Variable, a ... Anzahl Stellen No, b ... Anzahl Stellen Cons)
streak <- function(x, a, b){
  No <- rle(x > 0)[[1]] %>% rep(1:length(.), .) %>% `/`(2) %>% ceiling() # Immer Paare mit gleicher Nummer
  Type <- rep("active", length(x))
  Type[x == 0] <- "inactv"
  Cons <- rle(x == 0)[[1]] %>% sapply(., seq) %>% unlist()
  
  a <- paste0("%0", a, "d") # Definition der Formats (fmt) fuer nachfolgendes sprintf
  b <- paste0("%0", b, "d")
  
  cbind(Type, "_", sprintf(a, No), " #", sprintf(b, Cons)) %>% apply(., 1, paste, collapse = "") %>%
    return()
}

# Daten ----
## Erstelle Daten mittels Funktion ----
ebt_mds_full <-
  ebt_mds_grpd(per = "day", grp_nm = "Date") %>% 
  select(!c(Days, EntRt, LocRt, ERPctl, LRPctl)) %>% # Hier days immer 1, daher weg mit Ableitungen und entsprechenden ecdf
  mutate(Day = lubridate::wday(x = Date, week_start = 1, label = TRUE) %>% ordered(labels = date_names_lang("en")$day_ab[c(2:7, 1)])) %>% # Immer englische Abkürzungen für die Wochentage verwenden [wday() nimmt aus locale()]
  relocate(Day, .after = Loc)

## Ergänzen der Streaks ----
ebt_mds_full <- ebt_mds_full %>% 
  mutate(EntryStreak = streak(Count, 2, 4),
         HitStreak = streak(Hits, 4, 2)) %>% 
  arrange(desc(Date))

# Aufräumen ----
rm(streak)

# Kennzahlen ----
## Werte aus dem Internet ----
download.file("https://de.eurobilltracker.com/profile/?user=32954", "EBT.html")
tmp <-
  read_file("EBT.html") %>% 
  gsub("<span style=\"font-size: 7px; line-height: normal\">&nbsp;</span>", "", .) %>% 
  sub(".*Interessante Treffer: ([0-9]+).*Eingegebene Geldscheine: ([0-9]+).*Gesamtwert der eingegebenen Scheine: ([0-9]+).*",
      "\\1, \\2, \\3", .) %>% 
  strsplit(., split = ", ") %>% 
  unlist() %>% 
  as.integer()
tmp <- 
  tibble(Where = "Net", Hits = tmp[1], Count = tmp[2], Value = tmp[3])
file.remove("EBT.html")

## Zusätzlich Werte aus lokalen Daten ----
tmp <- bind_cols(
  Where = "Local",
  ebt_mds_full %>% summarise_at(.vars = vars(Hits, Count, Value), .funs = ~sum(., na.rm = TRUE)),
  ebt_mds_full %>% summarise(Days = n()),
  ebt_mds_full %>% summarise(nLoc = map_int(.x = list(Reduce(union, Loc)), .f = ~ length(.)))
) %>%
  bind_rows(., tmp)

print(list(Summary = tmp,
           Check = identical(tmp[1, 2:4], tmp[2, 2:4]))
)

rm(tmp)
