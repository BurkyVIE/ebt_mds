# LIBRARIES ----
library(tidyverse)

# GLOBAL ----
cuts <- c(0, 75, 100, 125, 250, 1000, Inf)

# DATA ----
dat <- ebt_mds_grpd(period = "day", grp_nm = "Date") |>
  transmute(Date, cCount = cumsum(Count), cHits = cumsum(Hits)) |>
  mutate(cHitRt = cCount / cHits, Set = cut(cHitRt, breaks = cuts))
  
# PLOT ----
p <- ggplot(data = dat) +
  aes(x = Date, y =cHitRt, fill = Set) +
  geom_col(width = 1) +
  scale_y_log10() +
  scale_fill_brewer(palette = "RdYlGn", direction = -1)

windows(16, 4, restoreConsole = TRUE)
plot(p)

# CLEAN UP ----
rm(cuts, dat, p)
