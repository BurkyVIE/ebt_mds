library(tidyverse)
library(lubridate)

colors = colorRampPalette(c("purple", "white"))(6)[c(1, 3:5)] %>% 
  setNames(., c("Median", "30 %", "60 %", "90 %"))

ebt_mds_grpd(per = "day", grp_nm = "Date") %>%
  mutate(MoDa = str_sub(Date, 6, 10)) %>%
  select(MoDa, nLoc, Count, Value, Hits) %>%
  nest(data = -MoDa) %>%
  mutate(map_dfr(data, ~ with(., c(
    Q = quantile(Count, .05),
    Q = quantile(Count, .2),
    Q = quantile(Count, .35),
    Median = median(Count),
    Q = quantile(Count, .65),
    Q = quantile(Count, .8),
    Q = quantile(Count, .95)
  )))) %>% 
  mutate(Date0 = ymd(paste(2000, MoDa, sep = "-"))) %>% 
  ggplot(aes(x = Date0, y = Median)) +
  geom_ribbon(aes(ymin = `Q.5%`, ymax = `Q.95%`, fill = "90 %")) +
  geom_ribbon(aes(ymin = `Q.20%`, ymax = `Q.80%`, fill = "60 %")) +
  geom_ribbon(aes(ymin = `Q.35%`, ymax = `Q.65%`, fill = "30 %")) +
  geom_line(aes(color = "Median"), size = 1.25) +
  scale_x_date(name = "Course of Year",
               breaks = paste(2000, seq(1, 12, by = 2), 1, sep = "-") %>% as.Date(),
               date_labels = "%b",
               expand = c(0, 0)) +
 scale_y_continuous(name = "",
                     expand = c(.01, .01)) +
  scale_fill_manual(name = "", values = colors) +
  scale_color_manual(name = "", values = colors) +
  labs(title = "EuroBillTracker - Daily Entries",
       subtitle = "by Burky") +
  theme_ebt() -> p

windows(16, 9)
plot(p)
rm(colors, p)
