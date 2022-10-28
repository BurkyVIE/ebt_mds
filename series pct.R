# LIBRARIES ----
library(tidyverse)
library(lubridate)

# PLOT ----
p <- notes %>%
  transmute(Semi = floor_date(DateStamp, unit = "6 month"), Series) %>%
  ggplot(data = ., mapping = aes(x = Semi)) +
  geom_bar(mapping = aes(fill = Series), position = "fill") +
#  geom_density(mapping = aes(fill = Series), alpha = .5) +
  scale_fill_viridis_d(begin = .15, end = .85) +
  labs(x = "",
       y = "Share",
       title = "Series percentage by Burky",
       subtitle = "by Burky",
       caption = paste0("as: ",max(ebt_mds$Date) ," (https://www.eurobilltracker.com)")) +
  theme_ebt()

windows(16, 9, restoreConsole = TRUE)
plot(p)

# CLEAN UP ----
rm(p)
