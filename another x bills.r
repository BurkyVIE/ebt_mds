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
