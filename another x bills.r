library(tidyverse)

x <- c(Cnt = 1, Val = 20, Hts = 1/100) * 25e3

he <- ebt_mds_grpd(per = "day", grp_nm = "Date") %>%
  select(Date, Cnt = Count, Val = Value, Hts = Hits) %>%
  mutate(L_Cnt = cumsum(Cnt) %/% x["Cnt"],
         L_Val = cumsum(Val) %/% x["Val"],
         L_Hts = cumsum(Hts) %/% x["Hts"]) %>%
  mutate(across(.cols = starts_with("L_"), .fns = ~ (c(0, diff(.))) == 1)) %>%
  filter(L_Hts | L_Cnt | L_Val) %>% 
  add_row(Date = lubridate::ymd("2004-8-3"), L_Cnt = TRUE, L_Val = TRUE, L_Hts = TRUE, .before = 1)

bind_rows(he %>% filter(L_Cnt) %>% pull(Date) %>% as.numeric() %>% diff() %>% tibble(DDiff = ., Cat = "Cnt"),
          he %>% filter(L_Val) %>% pull(Date) %>% as.numeric() %>% diff() %>% tibble(DDiff = ., Cat = "Val"),
          he %>% filter(L_Hts) %>% pull(Date) %>% as.numeric() %>% diff() %>% tibble(DDiff = ., Cat = "Hts")) %>% 
  mutate(Cat = factor(Cat, levels = c("Cnt", "Val", "Hts"), labels = c(paste0(x[1] / 1e3, "k Bills"), paste0(x[2] / 1e3, "k Euro"), paste0(x[3], " Hits")))) %>% 
  ggplot(mapping = aes(x = DDiff, fill = Cat)) +
  geom_histogram(binwidth = 30, color = "white", show.legend = FALSE) +
  geom_boxplot(mapping = aes(y = 1), width = .75, color = "white", fill = "white", size = 2, alpha = .75) +
  geom_boxplot(mapping = aes(y = 1), width = .75, fill = NA, size = 1.25) +
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
rm(x, he, p)
