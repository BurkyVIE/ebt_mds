library(dplyr)
library(lubridate)

# Zahlensystem der Pseudonyme
c("i", "j", "l", "o", "q", "s", "z") %>% # ohne diese Buchstaben
  setdiff(letters, .) %>%
  c(1:9, ., toupper(.)) %>% # Gültig sind 1:9 und die nicht ausgeschlossenen Buchstaben sowohl klein als auch groß
  factor(x = ., levels = .) %>% # fixiere Reihenfolge, da 'crossing' sortiert
  crossing(., ., .) %>%
  apply(., 1, paste, collapse = "") -> he

# Pseudonyme
pseudo <- notes %>%
  mutate(Coords = paste(Long, Lat, sep = "~")) %>%
  group_by(Coords) %>% 
  summarise(First = min(DateStamp),
            Country = first(EntryCountry),
            ZIP = first(EntryZIP),
            City = first(EntryCity)) %>%
  arrange(First) %>% 
  bind_cols(Loc = he[1:length(.$First)])

# Minimal-Data-Set
ebt_mds <- notes %>%
  mutate(Date = DateStamp %>% date(),
         Coords = paste(Long, Lat, sep = "~")) %>% 
  left_join(pseudo, by = "Coords") %>% # Anfügen der Pseudonyme
  group_by(Date) %>%
  summarise(Count = n(),
            Value = sum(Value),
            Loc = Loc %>% unique() %>% list()) %>% 
  full_join(hits %>%
              mutate(Date = DateStamp %>% date()) %>% 
              group_by(Date) %>% 
              summarise(Hits = n()),
            by = c("Date" = "Date")) %>% 
  arrange(Date)

# Schreiben 'ebt_mds' gesamt
dump("ebt_mds")

rm(he, pseudo, ebt_mds)

