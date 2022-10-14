# LIBRARIES ----
library(tidyverse)

# GLOBAL ----
cuts <- list(
  c(0, 66, 100, 150, 500, 1000, Inf),
  c("≤ 66", "≤ 100", "≤ 150", "≤ 500", "≤ 1,000", "> 1,000"),
  right_closed = TRUE)

# DATA ----
dat <- ebt_mds_grpd(period = "day", grp_nm = "Date") |>
  transmute(Date, cCount = cumsum(Count), cHits = cumsum(Hits)) |>
  mutate(cHitRt = cCount / cHits, Set = cut(cHitRt, right = cuts[[3]], breaks = cuts[[1]], labels = cuts[[2]]))

spec0 <- filter(dat, cHitRt != Inf)
spec <- bind_rows(
  filter(spec0, row_number() == 1),
  filter(spec0, cHitRt == min(cHitRt)),
  filter(spec0, cHitRt == max(cHitRt)),
  filter(spec0, row_number() == n())) |>
  mutate(Label = format(cHitRt, trim = TRUE, digits = 2, nsmall = 2, big.mark = ","),
         Label = paste(c("first", "lowest", "highest", "latest"), Label, sep = ": "))
  
# PLOT ----
p <- ggplot(data = dat) +
  aes(x = Date, y = cHitRt, fill = Set) +
  geom_col(width = 1) +
  geom_point(data = spec, shape = 1, size = 5, show.legend = FALSE) +
  ggrepel::geom_label_repel(data = spec, mapping = aes(label = Label),
                            box.padding = 1.5, nudge_y = -0.25, direction = "x",
                            size = 2.75, fill = rgb(221, 226, 233, maxColorValue = 255), alpha = 3/4, show.legend = FALSE) +
  scale_x_date(name = "Time", date_minor_breaks = "year", expand = c(1/100, 0)) +
  scale_y_log10(name = "Cumulative Hit Ratio [log10]", labels = function(x)format(x, big.mark = ",")) +
  scale_fill_brewer(name = "Hit Ratio", palette = "RdYlGn", direction = -1, na.value = "grey50") +
  labs(title = "EuroBillTracker - Hit Ratio over Time",
       subtitle = "by Burky",
       caption = paste0("as: ",max(ebt_mds$Date) ," (https://www.eurobilltracker.com)")) +
  guides(fill = guide_legend(nrow = 1)) +
  theme_ebt()

windows(16, 9, restoreConsole = TRUE)
plot(p)

# CLEAN UP ----
rm(cuts, dat, spec0, spec, p)
