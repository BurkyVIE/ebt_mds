# LIBRARIES ----
library(tidyverse)

# GLOBAL ----
cuts <- list(
  c(0, 80, 100, 125, 500, 1000, Inf),
  c("≤ 80", "≤ 100", "≤ 125", "≤ 500", "≤ 1,000", "> 1,000"),
  right_closed = TRUE)

# DATA ----
dat <- ebt_mds_grpd(period = "day", grp_nm = "Date") |>
  select(Date, Count, Hits, HitRt) |>
  mutate(cCount = cumsum(Count),
         cHits = cumsum(Hits),
         cHitRt = cCount / cHits,
         cHitRtLong = num(cHitRt, digits = 3),
         Change = c(NaN, sign(diff(round(cHitRt, 3)))),  # Differenz auf drei Nachkommestellen
         Change_lit = c("lower", "equal", "higher")[Change + 2])
dat <- mutate(dat, Set = cut(cHitRt, right = cuts[[3]], breaks = cuts[[1]], labels = cuts[[2]]))

spec0 <- filter(dat, is.finite(cHitRt))

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
  scale_x_date(name = "Time", date_minor_breaks = "year", expand = expansion(add = c(30, 90))) +
  scale_y_log10(name = "Cumulative Hit Ratio [log10]", labels = function(x)format(x, big.mark = ","), expand = expansion(mult = .01)) +
  scale_fill_brewer(name = "Hit Ratio", palette = "RdYlGn", direction = -1, na.value = "grey50") +
  labs(title = "EuroBillTracker - Hit Ratio over Time",
       subtitle = "by Burky",
       caption = paste0("as: ",max(ebt_mds$Date) ," (https://www.eurobilltracker.com)")) +
  guides(fill = guide_legend(nrow = 1)) +
  theme_ebt()

windows(16, 9, restoreConsole = TRUE)
plot(p)

# CLEAN UP ----
rm(cuts, dat, n_extr, extreme, spec0, spec, p)



# Graph max and min
# dat |>
#   select(Date, cHitRt) |>
#   mutate(cMin = cummin(cHitRt),
#          cMax = rev(cummax(rev(cHitRt)))) |>
#   ggplot() +
#   aes(x = Date) +
#   geom_line(aes(y = cHitRt), linewidth = 2, col = "gold") +
#   geom_line(aes(y = cMin), color = "forestgreen") +
#   geom_line(aes(y = cMax), color = "firebrick") +
#   scale_y_log10()
