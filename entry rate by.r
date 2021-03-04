library(tidyverse)
library(lubridate)

ggplot() +
  geom_rect(data = ebt_mds_grpd(per = "4 month"), mapping = aes(xmin = Period, xmax = Period + months(4), ymin = 0, ymax = EntRt, fill = EntRt)) +
  scale_x_date(date_minor_breaks = "1 year", expand = c(.01, .01)) +
  scale_y_continuous(expand = c(.02, .02)) +
  scale_fill_fermenter(palette = "BuPu", direction = 1,
                       breaks = c((0, 25, 75, 175, 225), 
                       guide = guide_coloursteps(barwidth = 25,
                                                 barheight = .75,
                                                 even.steps = FALSE)) +
  labs(title = "EuroBillTracker - Daily Entries",
       subtitle = "by Burky",
       x = "Thirds of Year",
       y = "Daily Entry Rate") +
  theme_ebt() -> p

windows(16, 9)
plot(p)

rm(p)
