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
         cHitRtLong = num(cHitRt, digits = 5),
         Change = c(NaN, sign(diff(cHitRt))),
         Change_lit = case_when(Change < 0 ~ "lower",
                            Change > 0 ~ "higher",
                            Change == 0 ~ "equal"))
dat <- mutate(dat, Set = cut(cHitRt, right = cuts[[3]], breaks = cuts[[1]], labels = cuts[[2]]))

# Loakale Extrema
n_extr <- 7
extreme <- mutate(dat,
                  Extreme = case_when(Change == "lower" ~ "-",
                                      Change == "higher" ~ "+",
                                      TRUE ~ "0"),
                  # Extreme = paste0(lag(Extreme, 5), lag(Extreme, 4), lag(Extreme, 3), lag(Extreme, 2), lag(Extreme, 1),
                  Extreme = paste0(map_dfc(set_names(n_extr:1, letters[1:n_extr]), .f = ~ lag(Extreme, .), ) |> (\(x) apply(x, 1, paste, collapse = ""))(),
                                   Extreme,
                                   map_dfc(set_names(1:n_extr, letters[1:n_extr]), .f = ~ lead(Extreme, .)) |> (\(x) apply(x, 1, paste, collapse = ""))())) |> 
  filter(Extreme %in% c(paste(c(rep("-", n_extr + 1), rep("+", n_extr)), collapse = ""), paste(c(rep("+", n_extr + 1), rep("-", n_extr)), collapse = "")))

spec0 <- filter(dat, is.finite(cHitRt))

spec <- bind_rows(
  filter(spec0, row_number() == 1),
  filter(spec0, cHitRt == min(cHitRt)),
  filter(spec0, cHitRt == max(cHitRt)),
  filter(spec0, row_number() == n()),
  extreme) |>
  mutate(Label = format(cHitRt, trim = TRUE, digits = 2, nsmall = 2, big.mark = ","),
         Label = paste(c("first", "lowest", "highest", "latest", rep("local extreme", tally(extreme))), Label, sep = ": "))
  
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
