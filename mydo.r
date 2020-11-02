library(tidyverse)
library(magrittr)

mydo <- function()
{
  per_list <- tribble(~Period, ~Int_length, ~Period2, ~Title,
                      "day", 49, "Daily", "seven Weeks",
                      "week", 52, "Weekly", "one Year",
                      "month", 36, "Monthly", "three Years",
                      "6 month", 30, "Semianually", "fifteen Years",
                      "year", 20, "Yearly", "twenty Years")
  
  selection <- menu(per_list$Period)
  
  p <- ebt_mds_grpd(period = per_list[selection, 1]) %>% 
    arrange(Period) %>% 
    tail(per_list[selection, 2] + 1) %>% head(-1) %>% 
    ggplot(mapping = aes(x = Period, y = Count)) +
    geom_ribbon(mapping = aes(ymax = Days * 75, fill = "000 - 075"), ymin = 0, alpha = 1/8) +
    geom_ribbon(mapping = aes(ymax = Days * 150, ymin = Days * 75, fill = "076 - 150"), alpha = 1/8) +
    geom_ribbon(mapping = aes(ymax = Days * 225, ymin = Days * 150, fill = "151 - 225"), alpha = 1/8) +
    geom_ribbon(mapping = aes(ymax = Inf, ymin = Days * 225, fill = "226 - Inf"), alpha = 1/8) +
    scale_fill_manual(name = "Bills per Day", values = c("firebrick", "gold", "yellowgreen", "forestgreen")) +
    geom_smooth(se = FALSE, color = "orangered") +
    geom_path(size = 1.5, alpha = 1/4) +
    geom_point(size = 4) +
    labs(title = paste0(per_list[selection, 3], " entered Bills for ", per_list[selection, 4])) +
    theme_ebt()
  
  plot(p)
  
  rm(per_list, selection)
}