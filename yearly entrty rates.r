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
  geom_abline(slope = c(100, 200), color = "white", linetype = "dashed", size = 1) +
  geom_line(mapping = aes(group = Year, color = Year2, size = Year2), alpha = 1/2) +
  scale_color_manual(name = "", values = c(RColorBrewer::brewer.pal(top, "Set1"), "grey")) +
  scale_size_manual(name = "", values = c(rep(2.5, top), 1.5)) +
  scale_x_continuous(name = "Course (day of year)") +
  scale_y_continuous(name = "Cumulative Count [k]", labels = function(x) x / 1000) +
  labs(title = "Entries Throughout the Year") +
  theme_ebt() -> p 
  plot(p)

rm(top, p)