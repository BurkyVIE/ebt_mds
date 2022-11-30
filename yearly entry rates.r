# Täglich V2----

library(tidyverse)
library(gghighlight)
library(lubridate)

last <- 3
slopes <- tibble(slope0 = c(50, 100, 200, 300)) %>%
  mutate(slope = slope0 * 1,
         x = sqrt(80000**2 / (slope**2 + (80000**2/365**2))), #xmax = 52, ymax = 80000
         y = x * slope,
         label = paste0(slope0,"/d"))

he <- ebt_mds_grpd(period = "day") |> 
  select(Period, Count) |> 
  mutate(Year = year(Period),
         Time = yday(Period))
he |> 
  group_by(Year) |> 
  summarise(Year = first(Year),
            Time = min(Time)-1,
            Count = 0,
            .groups = "drop") |> 
  (\(x) bind_rows(he, x))() |> 
  arrange(Year, Time) |>
  group_by(Year) |> 
  mutate(cumCount = cumsum(Count)) |> 
  ungroup() |>  
  ggplot() +
  aes(x = Time, y = cumCount) +
  geom_abline(slope = slopes$slope, color = "white", linetype = "dashed") +
  geom_label(data = slopes, mapping = aes(x = x, y = y, label = label), color = "white", fill = "grey", alpha = .6) +
  geom_line(mapping = aes(color = factor(Year), group = Year), linewidth = 2.5, alpha = .6) +
  scale_color_brewer(name = "", palette = "Set1", direction = -1) +
  gghighlight(max(Year), max_highlight = last, n = 1, unhighlighted_params = list(linewidth = 1, color = "grey"),
              use_direct_label = TRUE, label_key = Year) +
  scale_x_continuous(name = "Time Elapsed (Days)", breaks = function(x)seq(0, x[2], by = 60), expand = c(.01, .01)) +
  scale_y_continuous(name = "Cumulative Count [k]", labels = function(x) x / 1000, expand = c(.02, .02)) +
  labs(title = "EuroBillTracker - Aggregation of Entries over the Years",
       subtitle = "by Burky",
       caption = paste0("as: ",max(ebt_mds$Date), " (https://www.eurobilltracker.com)")) +
  theme_ebt() -> p 

windows(16, 9, restoreConsole = TRUE)
plot(p)

rm(last, slopes, he, p)



# Wöchentlich V2----

library(tidyverse)
library(gghighlight)
library(lubridate)

last <- 3
slopes <- tibble(slope0 = c(50, 100, 200, 300)) %>%
  mutate(slope = slope0 * 7,
         x = sqrt(80000**2 / (slope**2 + (80000**2/52**2))), #xmax = 52, ymax = 80000
         y = x * slope,
         label = paste0(slope0,"/d"))

he <- ebt_mds_grpd(period = "week") %>%
  select(Period, Count) %>% 
  mutate(Year = year(Period + days(3)), # 3 wg Mittwoch der Woche; dieser entscheidet ob Woche 1 im Jahr+1 oder Woche 53
         Time = isoweek(Period))
he |> 
  group_by(Year) |> 
  summarise(Year = first(Year),
            Time = min(Time)-1,
            Count = 0,
            .groups = "drop") |> 
  (\(x) bind_rows(he, x))() |> 
  arrange(Year, Time) |>
  group_by(Year) |> 
  mutate(cumCount = cumsum(Count)) |> 
  ungroup() |>  
  ggplot() +
  aes(x = Time, y = cumCount) +
  geom_abline(slope = slopes$slope, color = "white", linetype = "dashed") +
  geom_label(data = slopes, mapping = aes(x = x, y = y, label = label), color = "white", fill = "grey", alpha = .6) +
  geom_line(mapping = aes(color = factor(Year), group = Year), linewidth = 2.5, alpha = .6) +
  scale_color_brewer(name = "", palette = "Set1", direction = -1) +
  gghighlight(max(Year), max_highlight = last, n = 1, unhighlighted_params = list(linewidth = 1, color = "grey"),
              use_direct_label = TRUE, label_key = Year) +
  scale_x_continuous(name = "Time Elapsed (Weeks)", breaks = function(x)seq(0, x[2], by = 8), expand = c(.01, .01)) +
  scale_y_continuous(name = "Cumulative Count [k]", labels = function(x) x / 1000, expand = c(.02, .02)) +
  labs(title = "EuroBillTracker - Aggregation of Entries over the Years",
       subtitle = "by Burky",
       caption = paste0("as: ",max(ebt_mds$Date), " (https://www.eurobilltracker.com)")) +
  theme_ebt() -> p 

windows(16, 9, restoreConsole = TRUE)
plot(p)

rm(last, slopes, he, p)



# Monatlich V2----

library(tidyverse)
library(gghighlight)
library(lubridate)

last <- 3
slopes <- tibble(slope0 = c(50, 100, 200, 300)) %>%
  mutate(slope = slope0 * (365.2475 / 12),
         x = sqrt(80000**2 / (slope**2 + (80000**2/12**2))), #xmax = 12, ymax = 80000
         y = x * slope,
         label = paste0(slope0,"/d"))

he <- ebt_mds_grpd(period = "month") %>%
  select(Period, Count) %>% 
  mutate(Year = year(Period),
         Time = month(Period))
he |> 
  group_by(Year) |> 
  summarise(Year = first(Year),
            Time = min(Time)-1,
            Count = 0,
            .groups = "drop") |> 
  (\(x) bind_rows(he, x))() |> 
  arrange(Year, Time) |>
  group_by(Year) |> 
  mutate(cumCount = cumsum(Count)) |> 
  ungroup() |>  
  ggplot() +
  aes(x = Time, y = cumCount) +
  geom_abline(slope = slopes$slope, color = "white", linetype = "dashed") +
  geom_label(data = slopes, mapping = aes(x = x, y = y, label = label), color = "white", fill = "grey", alpha = .6) +
  geom_line(mapping = aes(color = factor(Year), group = Year), linewidth = 2.5, alpha = .6) +
  scale_color_brewer(name = "", palette = "Set1", direction = -1) +
  gghighlight(max(Year), max_highlight = last, n = 1, unhighlighted_params = list(linewidth = 1, color = "grey"),
              use_direct_label = TRUE, label_key = Year) +
  scale_x_continuous(name = "Time Elapsed (Months)", breaks = function(x)seq(0, x[2], by = 2), expand = c(.01, .01)) +
  scale_y_continuous(name = "Cumulative Count [k]", labels = function(x) x / 1000, expand = c(.02, .02)) +
  labs(title = "EuroBillTracker - Aggregation of Entries over the Years",
       subtitle = "by Burky",
       caption = paste0("as: ",max(ebt_mds$Date), " (https://www.eurobilltracker.com)")) +
  theme_ebt() -> p 

windows(16, 9, restoreConsole = TRUE)
plot(p)

rm(last, slopes, he, p)



# Täglich ----

library(tidyverse)
library(lubridate)

last <- 3
slopes <- tibble(slope = c(50, 100, 200, 300)) %>%
  mutate(x = sqrt(80000**2 / (slope**2 + (80000**2/365**2))), #xmax = 365, ymax = 80000
         y = x * slope,
         label = paste0(slope,"/d"))

he <- ebt_mds_grpd(period = "day") %>%
  select(Period, Count) %>% 
  mutate(Year = year(Period),
         Time = yday(Period))
he %>% 
  group_by(Year) %>%
  summarise(Year = first(Year),
            Time = min(Time)-1,
            Count = 0,
            .groups = "drop") %>% 
  bind_rows(he, .) %>%
  arrange(Year, Time) %>% 
  group_by(Year) %>% 
  mutate(cumCount = cumsum(Count)) %>% 
  ungroup() %>% 
  mutate(Year2 = case_when(Year < (max(Year) - last + 1) ~ "older",
                           TRUE ~ as.character(Year))) %>% 
  ggplot(data = ., mapping = aes(x = Time, y = cumCount)) +
  geom_abline(slope = slopes$slope, color = "white", linetype = "dashed") +
  geom_label(data = slopes, mapping = aes(x = x, y = y, label = label), color = "white", fill = "grey", alpha = .6) +
  geom_line(mapping = aes(group = Year, color = Year2, size = Year2), alpha = .6) +
  scale_color_manual(name = "", values = c(rev(RColorBrewer::brewer.pal(last, "Set1")), "grey")) +
  scale_size_manual(name = "", values = c(rep(2.5, last), 1)) +
  scale_x_continuous(name = "Time Elapsed (Days)", breaks = function(x)seq(0, x[2], by = 60), expand = c(.01, .01)) +
  scale_y_continuous(name = "Cumulative Count [k]", labels = function(x) x / 1000, expand = c(.02, .02)) +
  labs(title = "EuroBillTracker - Aggregation of Entries over the Years",
       subtitle = "by Burky",
       caption = paste0("as: ",max(ebt_mds$Date), " (https://www.eurobilltracker.com)")) +
  theme_ebt() -> p 

windows(16, 9, restoreConsole = TRUE)
plot(p)

rm(last, slopes, he, p)



# Wöchentlich ----

library(tidyverse)
library(lubridate)

last <- 3
slopes <- tibble(slope0 = c(50, 100, 200, 300)) %>%
  mutate(slope = slope0 * 7,
         x = sqrt(80000**2 / (slope**2 + (80000**2/52**2))), #xmax = 52, ymax = 80000
         y = x * slope,
         label = paste0(slope0,"/d"))

he <- ebt_mds_grpd(period = "week") %>%
  select(Period, Count) %>% 
  mutate(Year = year(Period + days(3)), # 3 wg Mittwoch der Woche; dieser entscheidet ob Woche 1 im Jahr+1 oder Woche 53
         Time = isoweek(Period))
he %>% 
  group_by(Year) %>%
  summarise(Year = first(Year),
            Time = min(Time)-1,
            Count = 0,
            .groups = "drop") %>% 
  bind_rows(he, .) %>%
  arrange(Year, Time) %>% 
  group_by(Year) %>% 
  mutate(cumCount = cumsum(Count)) %>% 
  ungroup() %>% 
  mutate(Year2 = case_when(Year < (max(Year) - last + 1) ~ "older",
                           TRUE ~ as.character(Year))) %>% 
  ggplot(data = ., mapping = aes(x = Time, y = cumCount)) +
  geom_abline(slope = slopes$slope, color = "white", linetype = "dashed", lwd = 1) +
  geom_label(data = slopes, mapping = aes(x = x, y = y, label = label), color = "white", fill = "grey", alpha = .6) +
  geom_line(mapping = aes(group = Year, color = Year2, lwd = Year2), alpha = .6) +
  scale_color_manual(name = "", values = c(rev(RColorBrewer::brewer.pal(last, "Set1")), "grey")) +
  scale_size_manual(name = "", values = c(rep(2.5, last), 1)) +
  scale_x_continuous(name = "Time Elapsed (Weeks)", breaks = function(x)seq(0, x[2], by = 8), expand = c(.01, .01)) +
  scale_y_continuous(name = "Cumulative Count [k]", labels = function(x) x / 1000, expand = c(.02, .02)) +
  labs(title = "EuroBillTracker - Aggregation of Entries over the Years",
       subtitle = "by Burky",
       caption = paste0("as: ",max(ebt_mds$Date), " (https://www.eurobilltracker.com)")) +
  theme_ebt() -> p

windows(16, 9, restoreConsole = TRUE)
plot(p)

rm(last, slopes, he, p)



# Monatlich ----

library(tidyverse)
library(lubridate)

last <- 3
slopes <- tibble(slope0 = c(50, 100, 200, 300)) %>%
  mutate(slope = slope0 * (365.2475 / 12),
         x = sqrt(80000**2 / (slope**2 + (80000**2/12**2))), #xmax = 12, ymax = 80000
         y = x * slope,
         label = paste0(slope0,"/d"))

he <- ebt_mds_grpd(period = "month") %>%
  select(Period, Count) %>% 
  mutate(Year = year(Period),
         Time = month(Period))
he %>% 
  group_by(Year) %>%
  summarise(Year = first(Year),
            Time = min(Time)-1,
            Count = 0,
            .groups = "drop") %>% 
  bind_rows(he, .) %>%
  arrange(Year, Time) %>% 
  group_by(Year) %>% 
  mutate(cumCount = cumsum(Count)) %>% 
  ungroup() %>% 
  mutate(Year2 = case_when(Year < (max(Year) - last + 1) ~ "older",
                           TRUE ~ as.character(Year))) %>% 
  ggplot(data = ., mapping = aes(x = Time, y = cumCount)) +
  geom_abline(slope = slopes$slope, color = "white", linetype = "dashed", lwd = 1) +
  geom_label(data = slopes, mapping = aes(x = x, y = y, label = label), color = "white", fill = "grey", alpha = .6) +
  geom_line(mapping = aes(group = Year, color = Year2, lwd = Year2), alpha = .6) +
  scale_color_manual(name = "", values = c(rev(RColorBrewer::brewer.pal(last, "Set1")), "grey")) +
  scale_size_manual(name = "", values = c(rep(2.5, last), 1)) +
  scale_x_continuous(name = "Time Elapsed (Months)", breaks = function(x)seq(0, x[2], by = 2), expand = c(.01, .01)) +
  scale_y_continuous(name = "Cumulative Count [k]", labels = function(x) x / 1000, expand = c(.02, .02)) +
  labs(title = "EuroBillTracker - Aggregation of Entries over the Years",
       subtitle = "by Burky",
       caption = paste0("as: ",max(ebt_mds$Date), " (https://www.eurobilltracker.com)")) +
  theme_ebt() -> p

windows(16, 9, restoreConsole = TRUE)
plot(p)

rm(last, slopes, he, p)
