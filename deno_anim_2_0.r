library(tidyverse)
library(lubridate)
library(gganimate)

source("ebt_mds_grpd.r")

dat <- ebt_mds_grpd(period = "halfyear", grp_nm = "Halfyear")
dat %>% select(Halfyear, Deno) %>%
  transmute(Halfyear,
            map_dfr(Deno, ~set_names(., paste0("EUR_", sprintf("%03d", c(5, 10, 20, 50, 100, 200, 500))))),
            H2 = case_when(semester(Halfyear) == 2 ~ TRUE,
                           TRUE ~ NA)) %>%
  mutate(across(starts_with("EUR_"), cumsum)) %>% 
  pivot_longer(starts_with("EUR_"), names_to = "Denomination", values_to = "Count") %>%
  ggplot(mapping = aes(x = Denomination, y = Count, group = Denomination)) +
  geom_col(mapping = aes(fill = Denomination), show.legend = FALSE) +
  geom_point(mapping = aes(shape = H2), color = "grey25", na.rm = TRUE, show.legend = FALSE) +
  scale_x_discrete(labels = paste0(c(5, 10, 20, 50, 100, 200, 500), " EUR")) +
  scale_y_continuous(name = "Count [k]", labels = function(x) x / 1000) +
  scale_fill_manual(values = c("#CCCCCC","#FF9999","#99CCFF","#FFCC99","#66CC66","#FFCC33","#B299CC")) +
  scale_shape_manual(values = c(1)) +
  labs(title = "Denominations entered by Burky",
       subtitle = "as Halfyear {semester(frame_time, with_year = TRUE)} (cumulated; o = End of Year)") +
  theme_ebt() +
  transition_time(Halfyear) +
  ease_aes("quartic-in") +
  shadow_mark() -> p

animate(p, nframes = pull(tally(dat), n) * 12, detail = 10, end_pause = 100, width = 12 * 50, height = 17 * 50,
        renderer = gifski_renderer("deno_anim.gif")) #spec/

rm(dat, p)
