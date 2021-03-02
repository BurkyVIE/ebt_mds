ebt_mds_grpd(per = "halfyear") %>%
  ggplot() +
  geom_rect(mapping = aes(xmin = Period, xmax = Period + months(6), ymin = 0, ymax = EntRt, fill = EntRt)) +
  scale_x_date(expand = c(.01, .01)) +
  scale_fill_fermenter(palette = "BuPu", direction = 1,
                       breaks = c(100, 150, 200, 250),
                       guide = guide_coloursteps(even.steps = FALSE)) +
  labs(title = "EuroBillTracker - Entries",
       subtitle = "by Burky",
       x = "Halfyear",
       y = "Entry Rate") +
  theme_ebt()
