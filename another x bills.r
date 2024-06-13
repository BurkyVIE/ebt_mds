library(tidyverse)

divisor <- c(Cnt = 1, Val = 25, Hts = 1/100) * 25e3

he <- ebt_mds_grpd(per = "day", grp_nm = "Date") |> 
  select(Date, Cnt = Count, Val = Value, Hts = Hits) |> 
  mutate(across(.cols = Cnt:Hts, .fns = ~ cumsum(.), .names = "C_{.col}"),
         across(.cols = starts_with("C_"), .fns = ~ c(0, diff(
           . %/% .GlobalEnv$divisor[substr(deparse(substitute(.)), 3, 5)]) # deparse(substitute) gibt variablen namen aus across als string
           ) == 1, .names = "{.col}_mod")) |>
  filter(if_any(ends_with("_mod"))) |> 
  add_row(Date = lubridate::ymd("2004-8-3"), C_Cnt_mod = TRUE, C_Val_mod = TRUE, C_Hts_mod = TRUE, .before = 1)

select(he, Date, ends_with("_mod")) |>
  pivot_longer(-Date, names_to = "Cat", values_to = "filter") |>
  filter(filter) |>
  mutate(DDiff = c(NA, diff(Date)), .by = Cat,
         Cat = factor(substr(Cat, 3, 5), levels = c("Cnt", "Val", "Hts"),
                      labels = c(paste0(divisor["Cnt"] / 1e3, "k Bills"),
                                 paste0(divisor["Val"] / 1e3, "k Euro"),
                                 paste0(divisor["Hts"], " Hits")))) |> 
  filter(!is.na(DDiff)) |> 
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

windows(16, 9)
plot(p)
rm(divisor, he, p)
