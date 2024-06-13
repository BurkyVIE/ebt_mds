# LIBRARIES ----
library(tidyverse)

# GLOBAL ----
## Definition der Gruppengrößen pro Kennzahl ----
divisor <- c(Count = 1, Value = 25, Hits = 1/100) * 25e3

# DATA ----
## Erste Aufbereitung ----
he <- ebt_mds_grpd(per = "day", grp_nm = "Date") |> 
  select(Date, Count, Value, Hits) |> 
  mutate(across(.cols = Count:Hits, .fns = ~ cumsum(.), .names = "C_{.col}"), # Kumuliere die Kennzahlen
         across(.cols = starts_with("C_"), .fns = ~ c(0, diff(                 # Durch diff zum Vorwert der Ganzzahligen Division Gruppengrenzen finden
           . %/% .GlobalEnv$divisor[str_remove(deparse(substitute(.)), "C_")]) # deparse(substitute) gibt Variablennamen aus across als string; Verwende Wert aus divisor
           ) == 1, .names = "L_{str_remove(.col, 'C_')}")) |>                  # Verweandle in Wahrheitswert
  filter(if_any(starts_with("L_"))) |> # Behalte nur ZEilen wo eine neue Gruppe beginnt
  add_row(Date = lubridate::ymd("2004-8-3"), L_Count = TRUE, L_Value = TRUE, L_Hits = TRUE, .before = 1) # Füge Nullpunkt an. -> Für späteres diff(Date)

## Aufbereitung für Grafik ----
select(he, Date, starts_with("L_")) |>
  pivot_longer(-Date, names_to = "Cat", values_to = "filter") |>
  filter(filter) |> # Behalte nur die Kategorien wo Grenze überschritten
  mutate(DDiff = c(NA, diff(Date)), .by = Cat, .keep = "used") |>  # Berechne pro Kategorie die Zeitdauern (NA wegen identer Länge)
  mutate(Cat = factor(str_remove(Cat, "L_"), levels = names(divisor), # Erstelle Kategoriebeschriftungen für Grafik
                      labels = c(paste0(divisor[1] / 1e3, "k Bills"), # Spalten sind fix wegen Verwendung names(divisor) in Zeile davor
                                 paste0(divisor[2] / 1e3, "k Euro"),
                                 paste0(divisor[3], " Hits")))) |> 
  filter(!is.na(DDiff)) |> # Entferne NA-Zeilen
  ggplot(mapping = aes(x = DDiff, fill = Cat)) +
  geom_histogram(binwidth = 30, color = "white", show.legend = FALSE) +
  geom_boxplot(mapping = aes(y = 1.5), width = 1, color = "white", fill = "white", size = 2, alpha = .75) +
  geom_boxplot(mapping = aes(y = 1.5), width = 1, fill = NA, size = 1.25) +
  scale_fill_brewer(palette = "Set1") +
  scale_x_continuous(breaks = function(lim) seq(0, lim[2], by = 360)) +
  labs(title = paste0("EuroBillTracker - Time to get Another ..."),
       subtitle = "by Burky",
       caption = paste0("as: ",max(ebt_mds$Date) ," (https://www.eurobilltracker.com)"),
       x = "Days",
       y = "Count") +
  facet_wrap(~ Cat, ncol = 1) +
  theme_ebt() -> p

# GRAPH ----
windows(16, 9)
plot(p)

# AUFRÄUMEN ----
rm(divisor, he, p)
