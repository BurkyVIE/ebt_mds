library(tidyverse)

ebt_grpd <- ebt_mds_grpd(period = "month") # Daten für den Plot

p <- ebt_grpd  %>%
  mutate(across(c(Count, Value, Hits), .fns = ~(. / Days))) %>% # Berechnungen für pro Tag
  mutate(Color = c(rep("old", n() - 1), "recent")) %>%
  select(-Deno, -Loc, -(EntRt:LRPctl)) %>% 
  # Überführen der Spalten in eine Variable
  pivot_longer(Hits:HitRt, names_to = "What", values_to = "Count") %>% 
#  gather(-Period, -Days, -Color, key = "What", value = "Count") %>%
  # Festlegen der Reihenfolge der Plots
  mutate(What = What %>%
           factor(levels = c("Count", "Value", "Avg", "Hits", "HitRt", "nLoc"),
                  labels = c("Notes per Day", "Value per Day", "Avg Value", "Hits per Day", "Hit-Ratio (1 : ..)", "Locations"))) %>%
  # Definieren des Plots
  ggplot(mapping = aes(x = Period, y = Count)) + 
  #  geom_line(color = "grey75") +
  geom_point(mapping = aes(color = Color), shape = 4, size = 0.9, stroke = 1.25, alpha = 0.67, show.legend = FALSE) +
  geom_smooth(color = "orangered", method = "loess", span = 1.5 / log(ebt_grpd$Period %>% length()), se = FALSE) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  scale_color_manual(values = c("old" = "mediumblue", "recent" = "mediumseagreen")) +
  labs(title = "Monthly entries to EBT by Burky",
       caption = paste0("by Burky as: ",max(ebt_mds$Date), " (http://www.eurobilltracker.com)"),
       y = "Value") +
  facet_wrap(~ What, scales = "free_y", ncol = 2) +
  theme_ebt()

png(paste0(EBT_global$whereis, "/spec/trend.png"), width = 34, height = 22 ,units = "cm", res = 300)
plot(p)
dev.off()

# Aufräumen
rm(ebt_grpd, p)
