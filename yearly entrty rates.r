# Täglich

library(tidyverse)
library(lubridate)

top <- 3

ebt_mds_full %>%
  arrange(Date) %>% 
  mutate(Year = year(Date),
         Year2 = case_when(Year < (max(Year) - top + 1) ~ "older",
                           TRUE ~ as.character(Year)),
         Course = yday(Date)) %>% 
  group_by(Year) %>% 
  mutate(cumCount = cumsum(Count)) %>% 
  ungroup() %>% 
  ggplot(data = ., mapping = aes(x = Course, y = cumCount)) +
  geom_abline(slope = c(100, 200, 300), color = "white", linetype = "dashed", size = 1) +
  geom_line(mapping = aes(group = Year, color = Year2, size = Year2), alpha = .6) +
  scale_color_manual(name = "", values = c(rev(RColorBrewer::brewer.pal(top, "Set1")), "grey")) +
  scale_size_manual(name = "", values = c(rep(2.5, top), 1.5)) +
  scale_x_continuous(name = "Course (day of year)") +
  scale_y_continuous(name = "Cumulative Count [k]", labels = function(x) x / 1000) +
  labs(title = "Entries Throughout the Year") +
  theme_ebt() -> p 

windows(16, 9)
plot(p)

rm(top, p)

# Wöchentlich

library(tidyverse)
library(lubridate)

top <- 3

ebt_mds_grpd(per = "week") %>% 
  mutate(Year = year(Period + days(3)), # 3 wg Mittwoch der Woch; dieser entscheidet ob Woche 1 im Jahr+1 oder Woche 53
         Year2 = case_when(Year < (max(Year) - top + 1) ~ "older",
                           TRUE ~ as.character(Year)),
         Course = week(Period)) %>% 
  group_by(Year) %>% 
  mutate(cumCount = cumsum(Count)) %>% 
  ungroup() %>% 
  ggplot(data = ., mapping = aes(x = Course, y = cumCount)) +
  geom_abline(slope = c(100, 200, 300) * 7, color = "white", linetype = "dashed", size = 1) +
  geom_line(mapping = aes(group = Year, color = Year2, size = Year2), alpha = .6) +
  scale_color_manual(name = "", values = c(rev(RColorBrewer::brewer.pal(top, "Set1")), "grey")) +
  scale_size_manual(name = "", values = c(rep(2.5, top), 1.5)) +
  scale_x_continuous(name = "Course (week = 7 day periods)") +
  scale_y_continuous(name = "Cumulative Count [k]", labels = function(x) x / 1000) +
  labs(title = "Entries Throughout the Year") +
  theme_ebt() -> p 

windows(16, 9)
plot(p)

rm(top, p)
