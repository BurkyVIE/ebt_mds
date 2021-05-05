library(tidyverse)

source("ebt_mds.txt") # Einlesen des minimal Dataset
source("ebt_mds_grpd.r") # Funktion zum Gruppieren

ebt_mds_full <-
  ebt_mds_grpd(per = "day", grp_nm = "Date", reverse = FALSE) %>% 
  select(-Days, -EntRt, -LocRt, -ERPctl, -LRPctl) %>% # Beide Ableitungen entstehen durch Division durch 1
  mutate(Day = lubridate::wday(x = Date, week_start = 1, label = TRUE) %>% ordered(labels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"))) %>% # Immer englische Abkürzungen für die Wochentage verwenden [wday() nimmt aus locale()]
  relocate(Day, .after = Loc)

# Funktion zur Erstellung eines streak (x ... welche Variable, a ... Stellen No, b ... Stellen Cons)
streak <- function(x, a, b){
  No <- rle(x > 0) %>% `[[`(1) %>% rep(1:length(.), .) %>% `/`(2) %>% ceiling() # Immer Paare mit gleicher Nummer
  Type <- rep("active", length(x))
  Type[x == 0] <- "inactv"
  Cons <- rle(x == 0) %>% `[[`(1) %>% sapply(., seq) %>% unlist()
  
  a <- paste0("%0", a, "d") # Definition der Formats (fmt) fuer nachfolgendes sprintf
  b <- paste0("%0", b, "d")
  
  cbind(Type, "_", sprintf(a, No), " #", sprintf(b, Cons)) %>% apply(., 1, paste, collapse = "") %>%
    return()
}

# Ergaenzen der Streaks
ebt_mds_full <- ebt_mds_full %>% 
  mutate(EntryStreak = streak(Count, 2, 4),
         HitStreak = streak(Hits, 4, 2)) %>% 
  arrange(desc(Date))

rm(streak)



# Kennzahlen
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



### Expandieren der Denos
# ebt_mds %>%
#   mutate(map_dfr(Deno, ~c(., integer(7))[1:7] %>%
#                    set_names(paste0("E", sprintf("%03d", c(5, 10, 20, 50, 100, 200, 500)))) %>%   # or (c(1, 2, 5) * 10 ** rep(0:2, each = 3))[3:9]
#                    as_tibble_row()))
#
# OR
#
# vec2tib <- function(x){
#   x <- c(x, integer(7))[1:7]
#   names(x) <- paste0("E", sprintf("%03d", c(5, 10, 20, 50, 100, 200, 500)))
#   return(as_tibble_row(x))
# }
# ebt_mds %>% mutate(map_dfr(Deno, ~vec2tib(.)))
# rm(vec2tib)
#
# OR
#
# ebt_mds %>% 
#   bind_cols(
#     ebt_mds_full$Deno %>%
#       do.call(rbind, .) %>%
#       as_tibble(.name_repair = "minimal") %>%
#       setNames(., paste0("E", c(5, 10, 20, 50, 100, 200, 500) %>% sprintf("%03d", .)))
#   )



### Wo war ich am
# pseudo %>%
#   filter(Loc %in% (ebt_mds_full %>%
#            filter(Date %in% lubridate::make_date(year = 2004:2019, month = 3, day = 21)) %>%
#            pull(Loc) %>% unlist() %>% unique())) %>%
#   select(Coords, Country:City) %>%
#   arrange(Country, ZIP)



### Wann war ich in
# pseudo %>%
#   filter(City == "Oldenburg") %>%
#   pull(Loc) -> tmp
# ebt_mds_full %>%
#   filter(purrr::map_lgl(.x = ebt_mds_full$Loc, .f = ~ (intersect(tmp, .) %>% length()) > 0)) %>%
#   pull(Date)
# rm(tmp)



### Geburtstagsscheine
# ebt_mds_full %>% filter(str_ends(Date, "3-21"))
