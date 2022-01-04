library(tidyverse)
library(lubridate)

precision <- list(12/3, "Third") # list(12/2, "Half") list(12/3, "Third") list(12/4, "Quarter") list(12/6, "Sixth")

ggplot() +
  geom_rect(data = ebt_mds_grpd(per = paste(precision[[1]], "month")), mapping = aes(xmin = Period, xmax = Period + months(precision[[1]]), ymin = 0, ymax = EntRt, fill = EntRt)) +
  scale_x_date(date_minor_breaks = "1 year", expand = c(.01, .01)) +
  scale_y_continuous(expand = c(.02, .02)) +
  scale_fill_fermenter(name = "Entry Rate", palette = "BuPu", direction = 1,
                       breaks = c(0, 25, 75, 175, 225), 
                       guide = guide_coloursteps(barwidth = 25,
                                                 barheight = .75,
                                                 even.steps = FALSE)) +
  labs(title = "EuroBillTracker - Entry Rates",
       subtitle = "by Burky",
       x = paste0(precision[[2]], "s of Year"),
       y = "Daily Entry Rate") +
  theme_ebt() -> p

windows(16, 9)
plot(p)

rm(precision, p)
