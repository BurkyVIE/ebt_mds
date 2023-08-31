ebt_mds_grpd(per = "year", grp_nm = "Date") |> 
  select(Date, Deno) |> 
  transmute(Date, map_dfr(Deno, ~set_names(., paste0("E", c(5, 10, 20, 50, 100, 200, 500) %>% sprintf("%03d", .)))))

# - - - - -

ebt_mds_grpd(per = "year") |> 
  select(Label, Deno) |> 
  transmute(Label, map_dfr(Deno, ~ set_names(., paste0("E", c(5, 10, 20, 50, 100, 200, 500) %>% sprintf("%03d", .))))) |> 
  pivot_longer(-Label, names_to = "Denomination", values_to = "Count") |> 
  mutate(Denomination = factor(Denomination) |>  fct_rev()) |> 
  ggplot(mapping = aes(x = Label, y = Count, fill = Denomination)) +
  geom_col() +
  scale_fill_manual(values = c("E005" = "#9aa79c", "E010" = "#f9bcc1", "E020" = "#70a0b5",
                               "E050" = "#f9bb7f", "E100" = "#8ec67d", "E200" = "#fae966",
                               "E500" = "#c59fb4")) +
  # scale_fill_viridis_d() +
  scale_x_continuous(name = "Year") +
  scale_y_continuous(name = "Count [k]", labels = function(x) x / 1000) +
  theme_ebt()

# - - - - -

ebt_mds_grpd(per = "year")|> 
  select(Label, Deno) |> 
  transmute(Label, map_dfr(Deno, ~ set_names(., paste0("E", c(5, 10, 20, 50, 100, 200, 500) %>% sprintf("%03d", .)))))|> 
  pivot_longer(-Label, names_to = "Denomination", values_to = "Count") |> 
  mutate(Year = factor(Label),
         Denomination = fct_rev(factor(Denomination))) |> 
  ggplot() +
  ggmosaic::geom_mosaic(aes(x = ggmosaic::product(Year), fill = Denomination, weight = Count), show.legend = FALSE) +
  scale_fill_manual(values = c("E005" = "#9aa79c", "E010" = "#f9bcc1", "E020" = "#70a0b5",
                               "E050" = "#f9bb7f", "E100" = "#8ec67d", "E200" = "#fae966",
                               "E500" = "#c59fb4")) +
  ggmosaic::scale_x_productlist(position = "top", expand = c(0, 0)) +
  ggmosaic::scale_y_productlist(expand = c(0, 0)) +
  theme_minimal() +
  theme(panel.grid = element_blank())

# - - - - -

  ebt_mds_grpd(per = "year", grp_nm = "Date") |> 
  select(Label, Deno) |> 
  transmute(Year = Label, map_dfr(Deno, ~set_names(., c(5, 10, 20, 50, 100, 200, 500)))) |> 
  gt::gt(rowname_col = "Year") |>
  gt::tab_spanner(label = "Denomination", columns = "5":"500") |>
  gt::tab_stubhead(label = "Year") |> 
  gt::fmt_number("5":"500", decimals = 0) |>
  gt::data_color("5":"500", palette = "viridis", reverse = FALSE) |> 
  gt::tab_header(title = "Number of Bills Entered", subtitle = "at EuroBillTracker by Burky") |> 
  gt::tab_source_note(source_note = "Visit: https://www.eurobiltracker.com") |> 
  gt::tab_footnote(footnote = "values are subject to change", placement = "right", locations = gt::cells_stub(Year == max(Year))) |> 
  gtExtras::gt_theme_538() #|> gt::gtsave("test.html")

# - - - - -

map_df(2004:format(Sys.Date(), "%Y"), ~ebd(.)) |>
  rename(Year = Selection, EqualBillsDay = EBDay) |>
  gt::gt() |>
  gt::fmt_integer(Count, sep_mark = ",") |>
  gt::fmt_date(EqualBillsDay, date_style = "day_month_year") |> 
  gt::fmt_percent(StdYears, decimals = 0) |>
  gt::data_color(columns = Days, target_columns = EqualBillsDay:StdYears, palette = "viridis", reverse = TRUE, domain = c(1, 700)) |> 
  gt::tab_header(title = "Equal Bills Day", subtitle = "at EuroBillTracker by Burky") |> 
  gt::tab_source_note(source_note = "Visit: https://www.eurobiltracker.com") |> 
  gt::tab_footnote(footnote = "no Equal Bills Day yet", placement = "right", locations = gt::cells_body(1, is.na(EqualBillsDay))) |> 
  gt::tab_footnote(footnote = "subject to change", placement = "right", locations = gt::cells_body(2, Year == max(Year))) |> 
  gt::tab_options(footnotes.multiline = FALSE, footnotes.sep = "; ") |> 
  gtExtras::gt_theme_538() #|> gt::gtsave("test.html")

# - - - - -

ebt_mds_full |>
  mutate(Streak = str_extract(HitStreak, "active_\\d*")) |>
  filter(!is.na(Streak)) |>
  group_by(Streak) |>
  summarise(Deno = list(Reduce(`+`, Deno)),
            Loc = list(Reduce(union, Loc)),
            From = min(Date),
            To = max(Date),
            Days = n(),
            Hits = sum(Hits, na.rm = TRUE),
            .groups = "drop") |> 
  mutate(map2_df(                                                   # map2 auf ...
    map(.x = Deno, .f = ~ rep(c(5, 10, 20, 50, 100, 200, 500), .)), # 1) expandierte Denos (eigenes map) und ...
    .y = Loc,                                                       # 2) Locations ...
    ~ data.frame(Count = length(.x),                                # eine Reihe von Funktionen gleichzeitig
                 Value = as.integer(sum(.x)),
                 nLoc = length(.y),
                 Avg = mean(.x),
                 Med = median(.x))),
    HitRt = Count / Hits, # Ableitungen
    EntRt = Count / Days,
    LocRt = nLoc / Days) |> 
  select(-(Streak:Loc)) |> 
  arrange(desc(Days)) |>
  filter(Days >= 14) |> 
  gt::gt() |>
  gt::fmt_integer(c(Days, Hits, Count, Value, nLoc, Med), sep_mark = ",") |> 
  gt::fmt_number(c(Avg, HitRt, EntRt, LocRt), decimals = 2) |>
  gt::data_color(Avg:Med, palette = "YlGn", reverse = F) |> 
  gt::data_color(HitRt:LocRt, palette = "YlOrRd", reverse = F) |> 
  gt::data_color(EntRt:LocRt, palette = "YlOrRd", reverse = T) |> 
  gt::tab_header(title = "Hit Streaks (= 14 Days)", subtitle = "at EuroBillTracker by Burky") |> 
  gt::tab_source_note(source_note = "Visit: https://www.eurobiltracker.com") |> 
  gt::tab_footnote(footnote = "distinct entry locations", placement = "right", locations = gt::cells_column_labels("nLoc")) |> 
  gt::tab_footnote(footnote = "value/count", placement = "right", locations = gt::cells_column_labels("Avg")) |> 
  gt::tab_footnote(footnote = "median(denomination)", placement = "right", locations = gt::cells_column_labels("Med")) |> 
  gt::tab_footnote(footnote = "count/hits", placement = "right", locations = gt::cells_column_labels("HitRt")) |> 
  gt::tab_footnote(footnote = "count/days", placement = "right", locations = gt::cells_column_labels("EntRt")) |> 
  gt::tab_footnote(footnote = "nloc/days", placement = "right", locations = gt::cells_column_labels("LocRt")) |> 
  gt::tab_options(footnotes.multiline = FALSE, footnotes.sep = "; ") |> 
  gtExtras::gt_theme_538() #|> gt::gtsave("test.html")

