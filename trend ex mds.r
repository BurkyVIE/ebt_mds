library(tidyverse)
library(lubridate)

# Einlesen des minimal Dataset
source("ebt_mds_full.r")

# Gruppieren der Daten nach definierter Zeispanne (zB Quartal = 3 month)
period = "Month" # Großbuchstaben sind möglich; Im Filter wird 'tolower' verwendet

ebt_grpd <- ebt_mds_full %>%
  # Zusammenfassen der Daten pro Monat
  group_by(Period = floor_date(Date, unit = tolower(period))) %>% 
  summarise(first = first(Date),
            last = last(Date),
            Count = sum(Count),
            Value = sum(Value),
            Hits = sum(Hits),
            Loc = Loc %>% unlist() %>% unique() %>% length()) %>%
  # Einfügen der Anzahl Tage des Monats; Sonderfälle erstes und letztes (= Aktuelles) Monat -> händisches Zusammensetzen des Vektors
  mutate(Days = c(first[1] %--% Period[2] / ddays(1), # ab der ersten Eingabe
                  Period[2:(n()-1)] %--% Period[3:n()] / ddays(1), # Zeitraum zwischen den Perioden
                  Period[n()] %--% last[n()] / ddays(1) + 1) %>% as.integer()) %>% # bis zur letzten Eingabe
  select(-first, -last)

# Korrigieren der Daten mit den Tagen, abgeleitete Variablen und Farben aktuelle Periode
p <- ebt_grpd  %>%
  mutate_at(vars(Count, Value, Hits), .funs = ~(. / Days)) %>%
  mutate(AvgValue = Value / Count,
         HitRt = Count / Hits,
         Color = c(rep("old", n() - 1), "recent")) %>%
  # Überführen der Spalten in eine Variable
  gather(-Period, -Days, -Color, key = "What", value = "Count") %>%
  # Festlegen der Reihenfolge der Plots
  mutate(What = What %>%
           factor(levels = c("Count", "Value", "AvgValue", "Hits", "HitRt", "Loc"),
                  labels = c("Notes per Day", "Value per Day", "Avg Value", "Hits per Day", "Hit-Ratio (1 : ..)", "Locations"))) %>%
  # Definieren des Plots
  ggplot(mapping = aes(x = Period, y = Count)) + 
#  geom_line(color = "grey75") +
  geom_point(mapping = aes(color = Color), shape = 4, size = 0.9, stroke = 1.25, alpha = 0.67, show.legend = FALSE) +
  geom_smooth(color = "orangered", method = "loess", span = 1.5 / log(ebt_grpd$Period %>% length()), se = FALSE) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  scale_color_manual(values = c("old" = "mediumblue", "recent" = "mediumseagreen")) +
  labs(title = paste0(period,"ly entries to EBT by Burky"),
       caption = paste0("by Burky as: ",max(ebt_mds$Date), " (http://www.eurobilltracker.com)"),
       y = "Value") +
  facet_wrap(~ What, scales = "free_y", ncol = 2) +
  theme_ebt()

png(paste0(EBT_global$whereis, "/spec/trend.png"), width = 34, height = 22 ,units = "cm", res = 300)
plot(p)
dev.off()

# Aufräumen
rm(period, ebt_grpd, p, ebt_mds)

