library(tidyverse)

x <- 5e4  # another x bills

with(ebt_mds_grpd(per = "day", grp_nm = "Date"),
     rep(Date, Count)) %>% 
  `[`(0:(length(.) %/% (x - 1)) * x + 1) %>% 
  diff() %>% 
  as.numeric() %>% 
  as_tibble_col(., column_name = "Diff") %>% 
  ggplot(mapping = aes(x = Diff)) +
  geom_histogram(fill = "lightblue", color = "navy", binwidth = x/2e3*1.5, size = 1.25) +
  geom_boxplot(mapping = aes(y = .25), width = .2, color = "white", fill = "white", size = 2.5, alpha = .75) +
  geom_boxplot(mapping = aes(y = .25), width = .2, color = "purple3", fill = NA, size = 1.25) +
  labs(title = paste0("EuroBillTracker - Time to Enter Another ", format(x, big.mark = ","), " Bills"),
       subtitle = "by Burky",
       x = "Days",
       y = "Count") +
  theme_ebt() -> p

windows(16, 9)
plot(p)
rm(x, p)

#---

library(tidyverse)

x <- 25e3
x <- x * c(1, 30, 1/100)

he <- ebt_mds_full %>%
  select(Date, Hits, Count, Value) %>%
  arrange(Date) %>%
  mutate(across(.cols = Hits:Value, .fns = ~ cumsum(.))) %>%
  mutate(D_Cnt = Count %% x[1],
         D_Val = Value %% x[2],
         D_Hts = Hits %% x[3]) %>%
  mutate(across(.cols = starts_with("D_"), .fns = ~ . - lag(., 1))) %>%
  filter(D_Hts < 0 | D_Cnt < 0 | D_Val < 0) %>% 
  add_row(Date = lubridate::ymd("2004-8-3"), D_Cnt = -1, D_Val = -1, D_Hts = -1, .before = 1)

Cnt <- he %>% filter(D_Cnt < 0) %>% pull(Date) %>% as.numeric() %>% diff() %>% tibble(DDiff = ., Cat = "Cnt")
Val <- he %>% filter(D_Val < 0) %>% pull(Date) %>% as.numeric() %>% diff() %>% tibble(DDiff = ., Cat = "Val")
Hts <- he %>% filter(D_Hts < 0) %>% pull(Date) %>% as.numeric() %>% diff() %>% tibble(DDiff = ., Cat = "Hts")

x[1:2] <- paste0(x[1:2] / 1000, "k")

bind_rows(Cnt, Val, Hts) %>% 
  mutate(Cat = factor(Cat, levels = c("Cnt", "Val", "Hts"), labels = c(paste0(x[1], " Bills"), paste0(x[2], " Euro"), paste0(x[3], " Hits")))) %>% 
  ggplot(mapping = aes(x = DDiff, fill = Cat)) +
  geom_histogram(binwidth = 30, show.legend = FALSE) +
  geom_boxplot(mapping = aes(y = 1), width = .5, color = "white", fill = "white", size = 2.5, alpha = .75) +
  geom_boxplot(mapping = aes(y = 1), width = .5, fill = NA, size = 1.25) +
  labs(title = paste0("EuroBillTracker - Time to Enter Another ..."),
       subtitle = "by Burky",
       x = "Days",
       y = "Count") +
  facet_wrap(~ Cat, ncol = 1) +
  theme_ebt() -> p

windows(16, 9)
plot(p)
rm(x, he, Cnt, Val, Hts, p)
