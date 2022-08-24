library(tidyverse)

param <- c(360, 30) # period, lag
vari <- quote(Hits) # switch variables for representation

ebt_mds_grpd(per = "day", grp_nm = "Date") |>
  tail(sum(param)) |>
  select(Date, vari) |>
  mutate(Cum = cumsum(!!vari),
         Lag = lag(Cum, n = param[2]),
         Avg = (Cum - Lag) / param[2]) |>
  tail(param[1]) %>%
  ggplot() +
  aes(x = Date) +
  geom_col(mapping = aes(y = !!vari), fill = "beige", color = "gold") +
  geom_line(mapping = aes(y = Avg), color = "firebrick", lwd = 1.5) +
  scale_x_date(date_labels = "%m/%Y") +
  labs(title = paste0("EuroBillTracker - Activity Over the Past ", param[1], " Days, including a ", param[2], "-day Moving Average"),
       subtitle = "by Burky",
       caption = paste0("as: ",max(ebt_mds$Date) ," (https://www.eurobilltracker.com)")) +
  theme_ebt() -> p

windows(16, 9)
plot(p)
rm(param, vari, p)
