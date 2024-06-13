library(tidyverse)

divisor <- c(Count = 1, Value = 25, Hits = 1/100) * 25e3

he <- ebt_mds_grpd(per = "day", grp_nm = "Date") |> 
  select(Date, Count, Value, Hits) |> 
  mutate(across(.cols = Count:Hits, .fns = ~ cumsum(.), .names = "C_{.col}"),
         across(.cols = starts_with("C_"), .fns = ~ c(0, diff(
           . %/% .GlobalEnv$divisor[str_replace(deparse(substitute(.)), ".*_(.*)","\\1")]) # deparse(substitute) gibt variablen namen aus across als string
           ) == 1, .names = "{.col}_ns")) |>
  filter(if_any(ends_with("ns"))) |> 
  add_row(Date = lubridate::ymd("2004-8-3"), C_Count_ns = TRUE, C_Value_ns = TRUE, C_Hits_ns = TRUE, .before = 1)

select(he, Date, ends_with("ns")) |>
  pivot_longer(-Date, names_to = "Cat", values_to = "filter") |>
  filter(filter) |>
  mutate(DDiff = c(NA, diff(Date)), .by = Cat, .keep = "used") |> 
  mutate(Cat = factor(str_replace(Cat, ".*_(.*)_.*", "\\1"), levels = names(divisor),
                      labels = c(paste0(divisor["Count"] / 1e3, "k Bills"),
                                 paste0(divisor["Value"] / 1e3, "k Euro"),
                                 paste0(divisor["Hits"], " Hits")))) |> 
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
