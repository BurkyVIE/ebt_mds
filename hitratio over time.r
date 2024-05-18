# LIBRARIES ----
library(tidyverse)

# GLOBAL ----
cuts <- list( # Definition der Gruppen für die farblich unterschiedenen Hit Ratio Gruppen
  c(0, 80, 100, 125, 500, 1000, Inf),
  c("≤ 80", "≤ 100", "≤ 125", "≤ 500", "≤ 1,000", "> 1,000"),
  right_closed = TRUE)

# DATA ----
dat <- # Grundsätzliche Erstellung des idF verwendeten Datensatzes 
  (\(lag = 365, digi_lon = 3) { # last x Days Hit Ratio wird hier - mittels anonymer Funktion - eingeführt, ebenso Anzahl Nachkommastellen für Long-Repräsentation
   vnm = sym(paste0("l", lag, "HR")) # Erstellen des Variablennamens für last x Days
   ebt_mds_grpd(period = "day", grp_nm = "Date") |>
     select(Date, Count, Hits, HitRt) |>
     mutate(across(c(Count, Hits), ~cumsum(.), .names = "c{.col}"), # Kumulieren von Count und Hits
            !!vnm := (cCount - lag(head(c(0, cCount), -1), lag-1)) / (cHits - lag(head(c(0, cHits), -1), lag-1)), # last x Days Hit Ratio
            cHitRt = cCount / cHits,
            smlr = rank(cHitRt, ties.method = "first") - 1, # Anzahl Tage mit niedrigerer cHitRt
            across(c(!!vnm, cHitRt), ~num(., digits = digi_lon), .names = "{.col}Long"), # Langversionenen der Hit Ratios (Nachkommastellen hier regelbar)
            DcHitRt = c(NaN, sign(diff(round(cHitRt, digi_lon)))),  # Differenz auf 'digi_lon' Nachkommastellen (siehe Definition der anonymen Funktion)
            DcHitRt_lit = c("lower", "equal", "higher")[DcHitRt + 2])})() # Verbale Beschreibung der Veränderung

dat <- mutate(dat, Set = cut(cHitRt, right = cuts[[3]], breaks = cuts[[1]], labels = cuts[[2]])) # Ergänzung der Gruppen im Datensatz

spec0 <- filter(dat, is.finite(cHitRt)) # Auswahl der Daten für die Beschriftungen

spec <- bind_rows(
  filter(spec0, row_number() == 1),      # first,
  filter(spec0, cHitRt == min(cHitRt)),  # lowest,
  filter(spec0, cHitRt == max(cHitRt)),  # highest und
  filter(spec0, row_number() == n())) |> # latest Hit Ratio
  mutate(Label = format(cHitRt, trim = TRUE, digits = 2, nsmall = 2, big.mark = ","),
         Label = paste(c("first", "lowest", "highest", "latest"), Label, sep = ": "))
  
# PLOT ----
he <- names(dat)[str_detect(names(dat), "l\\d+HR$")] # Auslesen des Variablennamen für last x Days Hit Ratio
he <- list(sym(he), as.numeric(str_extract(he, "\\d+"))) # Auslesen des Zeitfensters

p <- ggplot(data = dat) +
  aes(x = Date, y = cHitRt) +
  geom_col(aes(fill = Set), width = 1) +
  geom_line(aes(y = !!(he[[1]]), lty = "l..HR"), linejoin = "bevel", color = "black", linewidth = 1.5, alpha = .6) +
  geom_point(data = spec, shape = 1, size = 5) +
  ggrepel::geom_label_repel(data = spec, mapping = aes(label = Label),
                            box.padding = 1.5, nudge_y = -0.25, direction = "x",
                            size = 2.75, fill = rgb(221, 226, 233, maxColorValue = 255), alpha = 3/4, show.legend = FALSE) +
  scale_x_date(name = "Time", date_minor_breaks = "year", expand = expansion(add = c(30, 90))) +
  scale_y_log10(name = "Cumulative Hit Ratio [log10]", labels = function(x)format(x, big.mark = ","), expand = expansion(mult = .01)) +
  scale_fill_brewer(name = "Hit Ratio", palette = "RdYlGn", direction = -1, na.value = "grey50") +
  scale_linetype_manual(name = "Moving Average", values = 1, labels = paste("Last", format(he[[2]], big.mark = ","), "Days Hit Ratio")) +
  labs(title = "EuroBillTracker - Hit Ratio over Time",
       subtitle = "by Burky",
       caption = paste0("as: ",max(ebt_mds$Date) ," (https://www.eurobilltracker.com)")) +
  guides(fill = guide_legend(nrow = 1),
         col = guide_legend(nrow = 1)) +
  theme_ebt()

windows(16, 9, restoreConsole = TRUE)
plot(p)

# CLEAN UP ----
rm(cuts, dat, spec0, spec, he, p)



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
