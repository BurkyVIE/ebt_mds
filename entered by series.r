library(tidyverse)

denominator <- 2 # 2, 3, 4, 6, 12
name = "Halfyear" # "Halfyear", "Third of Year", "Quarter of Year", "Bimonth", "Month"

notes %>% 
  group_by(Series, Time = (lubridate::month(DateStamp) - 1) %/% (12 / denominator) * (1 / denominator) + lubridate::year(DateStamp)) %>%
  tally() %>%
  ggplot(mapping = aes(x = Time, y = n)) +
  geom_col(fill = "grey95", alpha = .5) +
  geom_point(mapping = aes(color = Series), shape = 17, size = 3, show.legend = FALSE) +
  geom_smooth(mapping = aes(color = Series), se = F, size = 2) +
#  geom_freqpoly(mapping = aes(color = Series), stat = "identity", size = 2) +
  scale_x_continuous(expand = c(.01, .01)) +
  scale_y_continuous(name = "Count [k]", labels = function(x) x/1000, expand = c(.01, .01)) +
  scale_color_brewer(palette = "Set1") +
  labs(title = "EuroBillTracker - Entered Bills by Series", subtitle = "by Burky",
       x = name) +
  theme_ebt() -> p

windows(16,9)
plot(p)

rm(denominator, name, p)