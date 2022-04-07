library(tidyverse)

vari <- expr(Count) # switches variables for representation

breaks <- switch(quo_name(vari),
                 Count = list(c(100, 125, 150, 175, 200), 100), #list contains [[1]] breaks and [[2]] split value for text color
                 Hits = list(c(.5, 1, 1.5, 2, 2.5), 1))

p <- ebt_mds_full %>%
  mutate(MoDa = str_sub(Date, 6, 10)) %>%
  group_by(MoDa) %>%
  summarise(n = n(),
            Total = sum(!!vari),
            Avg = mean(!!vari),
            SD = sd(!!vari)) %>% 
  mutate(V = SD / Avg,
         teco = case_when(Avg <= breaks[[2]] ~ "white", # text color; for dark backgrounds (tiles) use lighter colors
                          TRUE ~ "black")) %>% 
  separate(MoDa, into = c("Month", "Day")) %>% 
  ggplot(mapping = aes (x = Day, y = Month)) +
  geom_tile(mapping = aes(fill = Avg), color = "black") +
  geom_text(mapping = aes(color = teco,
                          label = paste0(format(Total, big.mark = " "), " / ", n, "\n= ",
                                         format(round(Avg, 1), nsmall = 1), "\n(± ",
                                         format(round(SD, 1), nsmall = 1), ")")), size = 2) +
  scale_fill_viridis_b(name = "Average Value",
                       option = "viridis", # Purples
                       breaks = breaks[[1]], 
                       guide = guide_coloursteps(barwidth = 25,
                                                 barheight = .75,
                                                 even.steps = FALSE)) +
  scale_color_identity() +
  scale_x_discrete(position = "top") +
  scale_y_discrete(limits = rev) +
  labs(title = "EuroBillTracker - Daily Entries",
       subtitle = "by Burky",
       caption = paste0("as: ",max(ebt_mds$Date), " (http://www.eurobilltracker.com)")) +
  theme_ebt()

windows(16, 9)
plot(p)
rm(breaks, wte, p)
