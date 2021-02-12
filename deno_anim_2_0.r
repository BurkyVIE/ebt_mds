library(tidyverse)
library(lubridate)
library(gganimate)

source("ebt_mds_grpd.r")

dat <- ebt_mds_grpd(period = "quarter")
dat %>% select(Period, Deno) %>%
  transmute(Quarter = Period,
            map_dfr(Deno, ~set_names(., paste0("EUR_", sprintf("%03d", c(5, 10, 20, 50, 100, 200, 500))))),
            Q4 = case_when(month(Period) == 10 ~ TRUE,
                           TRUE ~ FALSE)) %>%
  mutate(across(starts_with("EUR_"), cumsum)) %>% 
  pivot_longer(starts_with("EUR_"), names_to = "Denomination", values_to = "Count") %>%
  ggplot(mapping = aes(x = Denomination, y = Count, group = Denomination)) +
  geom_col(mapping = aes(fill = Denomination), show.legend = FALSE) +
  geom_point(mapping = aes(shape = Q4), size = 8, show.legend = FALSE) +
  scale_y_continuous(name = "Count [k]", labels = function(x) x / 1000) +
  scale_fill_manual(values = c("#CCCCCC","#FF9999","#99CCFF","#FFCC99","#66CC66","#FFCC33","#B299CC")) +
  scale_shape_manual(values = c("", "-")) +
  labs(title = "Denominations entered by Burky",
       subtitle = "as Quarter {quarter(frame_time, with_year = TRUE)} (cumulated)") +
  theme_ebt() +
  transition_time(Quarter) +
  ease_aes("quartic-in") +
  shadow_mark() -> p

animate(p, nframes = pull(tally(dat), n) * 10, duration = 10, end_pause = 100, width = 9 * 55, height = 16 * 55,
        renderer = gifski_renderer("spec/deno_anim.gif"))

rm(dat, p)
