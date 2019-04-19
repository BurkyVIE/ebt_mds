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
  summarise(#Count = n(),
            Deno = factor(Value, levels = c(5, 10, 20, 50, 100, 200 ,500)) %>% table() %>% as.double() %>% list(), # NEU
            #Value = sum(Value),
            Loc = Loc %>% unique() %>% list()) %>% 
  full_join(hits %>%
              mutate(Date = DateStamp %>% date()) %>% 
              group_by(Date) %>% 
              summarise(Hits = n()),
            by = c("Date" = "Date")) %>% 
  arrange(Date) %>% 
  replace_na(replace = list(Hits = 0))

ebt_mds_seven <- ebt_mds %>% tail(7)

# Schreiben 'ebt_mds' gesamt
dump("ebt_mds", "ebt_mds.txt")
#write_csv(ebt_mds, "ebt_mds.csv")

# Schreiben der letzten sieben Einträge plus Länge 
dump("ebt_mds_seven", file = "ebt_mds_seven.txt")
write(paste0("\n",dim(ebt_mds)[1]), file = "ebt_mds_seven.txt", append = TRUE)

#Schreiben der Pseudonyme
dump("pseudo", file = "pseudo.txt")
rm(he, ebt_mds_seven)
