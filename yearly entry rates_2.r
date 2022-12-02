# Initialisierung ----
## Libraries ----
library(tidyverse)
library(gghighlight)
library(lubridate)

## Parameter für Betrachtungsweise ----
param <- list(list(period = "day", xmax = 365, y_fn = function(x) year(x), t_fn = function(x) yday(x), by = 60),
              list(period = "week", xmax = 52, y_fn = function(x) year(x + days(3)), t_fn = function(x) isoweek(x), by = 8), # 3 wg Mittwoch der Woche; dieser entscheidet ob Woche 1 im Jahr+1 oder Woche 53
              list(period = "month", xmax = 12, y_fn = function(x) year(x), t_fn = function(x) month(x), by = 2)
              )[[1]] # <- HIER Schalter!
# period    ... Für die ebt_mds_grp- Abfrage und für die Beschriftung x-Achse
# slope_fac ... Korrektur für Steigung der Hilfslinien i.e. Tage pro Abschnitt
# xmax      ... x-Achsen-Korrektur für Labels Hilfslinien
# y_fn      ... Funktion zur Berechnung des Jahres (Gruppierung)
# t_fn      ... Berechnung der x-Abschnitte
# by        ... Beschriftungsabstände auf der x-Achse

## Highlighten der letzten ... ----
last <- 3

# Berechnungen ----
## Bereitstellung Daten ----
dat <- ebt_mds_grpd(period = param$period) |> 
  select(Period, Days, Count) |> 
  mutate(Year = param$y_fn(Period),
         Time = param$t_fn(Period))

## Ableitungen ----
dat |> 
  group_by(Year) |> 
  mutate(cumDays = cumsum(Days),
         cumCount = cumsum(Count),
         cumEntRt = cumCount / cumDays) |>
  ungroup() |>
  ggplot() +
  aes(x = Time, y = cumEntRt) +
  geom_line(mapping = aes(color = factor(Year), group = Year), linewidth = 1.5,  alpha = .67) +
  scale_color_brewer(name = "", palette = "Set1", direction = -1) +
  gghighlight(max(Year), max_highlight = 3, label_key = Year,
              unhighlighted_params = list(linewidth = .5, color = "grey")) +
  scale_x_continuous(name = paste0("Time Elapsed (", str_to_title(param$period), "s)"), breaks = function(x)seq(0, x[2], by = param$by), expand = expansion(mult = c(0, .01))) +
  scale_y_continuous(name = "Cumulative Entry Rate [k]", labels = function(x) x / 1000, expand = expansion(mult = .02)) +
  labs(title = "EuroBillTracker - Aggregation of Entries over the Years",
       subtitle = "by Burky",
       caption = paste0("as: ",max(ebt_mds$Date), " (https://www.eurobilltracker.com)")) +
  theme_ebt() -> p 

# Grafik ----
windows(16, 9)
plot(p)

# Aufräumen ----
rm(param, last, dat, p)
