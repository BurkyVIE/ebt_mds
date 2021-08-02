library(tidyverse)

ebt_mds_full %>%
  mutate(MoDa = str_sub(Date, 6, 10)) %>%
  group_by(MoDa) %>%
  summarise(n = n(),
            Total = sum(Count),
            Avg = mean(Count),
            SD = sd(Count)) %>% 
  mutate(V = SD / Avg,
         Month = as.numeric(str_sub(MoDa, 1, 2))) -> dat

normpar <- with(dat, c(mean(Avg), sd(Avg)))
xseq <- seq(floor(normpar[1] - 3.25 * normpar[2]), ceiling(normpar[1] + 3.25 * normpar[2]))
norm <- dnorm(xseq, normpar[1], normpar[2])
norm <- tibble(x = xseq, y = norm / max(norm))

ggplot(data = dat, mapping = aes(x = Avg)) +
  geom_line(data = norm, aes(x = x, y = y), size = 1.25, linetype = "dashed") + 
  geom_text(x = normpar[1], y = 1.025, aes(label = paste0("N(", format(normpar[1], digits = 1, nsmall = 1), ", ", format(normpar[2], digits = 1, nsmall = 1), ")"))) +
  geom_density(mapping = aes(y = after_stat(scaled)), fill = "lightblue", color = "navy", alpha = .9) +
  geom_jitter(mapping = aes(y = -.2), height = .11, pch = 21, color = "navy", fill = "lightblue") +
  geom_boxplot(mapping = aes(y = -.04), width = .06, color = "navy", size = 1.25) +
  labs(title = "EuroBillTracker - On Average Entered Bills per Date (n = 366)",
       subtitle = "by Burky",
       x = "Average") +
  theme_ebt() +
  theme(panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank()) -> p

windows(16, 9)
plot(p)
rm(dat, normpar, xseq, norm, p)
