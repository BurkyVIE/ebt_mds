library(tidyverse)

p <- ebt_mds_full %>%
  mutate(MoDa = str_sub(Date, 6, 10)) %>%
  group_by(MoDa) %>%
  summarise(n = n(),
            Total = sum(Count),
            Avg = mean(Count),
            SD = sd(Count)) %>% 
  mutate(V = SD / Avg,
         teco = case_when(Avg <= 100 ~ "white", # text color; for dark backgrounds (tiles) use lighter colors
                          TRUE ~ "black")) %>% 
  separate(MoDa, into = c("Month", "Day")) %>% 
  ggplot(mapping = aes (x = Day, y = Month)) +
  geom_tile(mapping = aes(fill = Avg), color = "black") +
  geom_text(mapping = aes(color = teco,
                          label = paste0(format(Total, big.mark = " "), " / ", n, "\n= ",
                                         format(round(Avg, 1), nsmall = 1), "\n(± ",
                                         format(round(SD, 1), nsmall = 1), ")")), size = 2) +
  # scale_fill_fermenter(name = "Average Value",
  #                      palette = "Purples",
  #                      breaks = c(0, 100, 125, 150, 175, 200, Inf), 
  #                      guide = guide_coloursteps(barwidth = 25,
  #                                                barheight = .75,
  #                                                even.steps = FALSE)) +
  scale_fill_viridis_b(name = "Average Count",
                       option = "viridis",
                       breaks = c(50, 100, 125, 150, 175, 255), 
                       guide = guide_coloursteps(barwidth = 25,
                                                 barheight = .75,
                                                 even.steps = FALSE)) +
  scale_color_identity() +
  scale_x_discrete(position = "top") +
  scale_y_discrete(limits = rev) +
  labs(title = "EuroBillTracker - Daily Entries",
       subtitle = "by Burky") +
  theme_ebt()

windows(16, 9)
plot(p)
rm(p)
