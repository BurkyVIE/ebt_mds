# Initialisierung ----
## Libraries ----
library(tidyverse)
library(gghighlight)
library(lubridate)

## Parameter für Betrachtungsweise ----
param <- list(list(period = "day", slope_fac = 1, xmax = 365, y_fn = function(x) year(x), t_fn = function(x) yday(x), by = 60),
              list(period = "week", slope_fac = 7, xmax = 52, y_fn = function(x) year(x + days(3)), t_fn = function(x) isoweek(x), by = 8), # 3 wg Mittwoch der Woche; dieser entscheidet ob Woche 1 im Jahr+1 oder Woche 53
              list(period = "month", slope_fac = (365.2475 / 12), xmax = 12, y_fn = function(x) year(x), t_fn = function(x) month(x), by = 2)
              )[[1]] # <- HIER Schalter!
# period    ... Für die ebt_mds_grpd-Abfrage und für die Beschriftung x-Achse
# slope_fac ... Korrektur für Steigung der Hilfslinien i.e. Tage pro Abschnitt
# xmax      ... x-Achsen-Korrektur für Labels Hilfslinien
# y_fn      ... Funktion zur Berechnung des Jahres (Gruppierung)
# t_fn      ... Berechnung der x-Abschnitte
# by        ... Beschriftungsabstände auf der x-Achse

## Highlighten der letzten ... ----
last <- 3

slopes <- tibble(slope0 = c(50, 100, 200, 300)) %>%
  mutate(slope = slope0 * param$slope_fac,
         x = sqrt(80000**2 / (slope**2 + (80000**2/param$xmax**2))),
         y = x * slope,
         label = paste0(slope0,"/d"))

# Berechnungen ----
## Bereitstellung Daten ----
dat <- ebt_mds_grpd(period = param$period) |> 
  select(Period, Count) |> 
  mutate(Year = param$y_fn(Period),
         Time = param$t_fn(Period))

## Ableitungen ----
dat |> 
  group_by(Year) |> 
  summarise(Year = first(Year),
            Time = min(Time)-1,
            Count = 0,
            .groups = "drop") |> 
  (\(x) bind_rows(dat, x))() |> 
  arrange(Year, Time) |>
  group_by(Year) |> 
  mutate(cumCount = cumsum(Count)) |> 
  ungroup() |>  
  ggplot() +
  aes(x = Time, y = cumCount) +
  geom_abline(slope = slopes$slope, color = "white", linetype = "dashed", linewidth = 1) +
  geom_label(data = slopes, mapping = aes(x = x, y = y, label = label), color = "white", fill = "grey", alpha = .6) +
  geom_line(mapping = aes(color = factor(Year), group = Year), linewidth = 2.5, alpha = .6) +
  scale_color_brewer(name = "", palette = "Set1", direction = -1) +
  gghighlight(max(Year), max_highlight = last, n = 1, unhighlighted_params = list(linewidth = 1, color = "grey"),
              use_direct_label = TRUE, label_key = Year) +
  scale_x_continuous(name = paste0("Time Elapsed (", str_to_title(param$period), "s)"), breaks = function(x)seq(0, x[2], by = param$by), expand = expansion(mult = c(0, .01))) +
  scale_y_continuous(name = "Cumulative Count [k]", labels = function(x) x / 1000, expand = expansion(mult = c(0, .02))) +
  labs(title = "EuroBillTracker - Aggregation of Entries over the Years",
       subtitle = "by Burky",
       caption = paste0("as: ",max(ebt_mds$Date), " (https://www.eurobilltracker.com)")) +
  theme_ebt() -> p 

# Grafik ----
windows(16, 9)
plot(p)

# Aufräumen ----
rm(param, last, slopes, dat, p)
