library(tidyverse)

with(ebt_mds_grpd(per = "day", grp_nm = "Date"),
     rep(Date, Count)) %>% 
  `[`(1:(length(.) %/% 5e4) * 5e4) %>% 
  diff() %>% 
  as.numeric() %>% 
  as_tibble_col(., column_name = "Diff") %>% 
  ggplot(mapping = aes(x = Diff)) +
  geom_histogram(fill = "purple3", color = "purple", binwidth = 5e4/2e3*1.5, size = 1.25) +
  geom_boxplot(mapping = aes(y = 1/2), color = "navy", fill = "lightblue", size = 1.25, alpha = .75) +
  labs(title = paste0("Time to Enter Another ", format(5e4, big.mark = ","), " Bills")) +
  xlab("Days") +
  ylab("Count") +
  theme_ebt() -> p

windows(16, 9)
plot(p)
rm(p)
