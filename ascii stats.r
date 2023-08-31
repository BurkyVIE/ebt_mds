# denovec <- c(30, 53, 50, 0, 25, 0, 0)                                               # <- Input here
## ebt_mds_grpd(per = "overall")[[2]] %>% paste()
denovec <- c(293889, 251408, 193989, 137280, 66165, 10848, 5296)

list(
  denovec |>                                                                                 # ZUSAMMENFASSUNG ---
    (\(x) c(x, numeric(7))[1:7])() |>                                                        # ergänze auf sieben Stellen
    (\(x, y = c(5, 10, 20, 50, 100, 200, 500)) x %*% cbind(rep(1, 7), y, y ** 2))() |>       # berechne n, sum(x) und sum(x²); Wertstufen im Funktionsaufruf
    (\(x) c(x[2], x[1], x[2] / x[1], sqrt((x[3] - x[2] ** 2 / x[1]) / x[1])))() |>           # übernehme sum(x) und n, bzw leite mean(x) und sd(x) ab
    (\(x) c(formatC(x[1:2], digits = 0, width = 1, format = "f", big.mark = ","),            # formatiere die Summen-Größen
            formatC(x[3:4], digits = 2, width = 4, format = "f", big.mark = ",")))() |>      # formatiere die abgeleiteten Größen
    (\(x) paste0(x[1], " Euro in ", x[2], " Bills = ", x[3], " (avg) ± ", x[4], " (sd)"))(), # füge in Rückgabe zusammen  
  
  denovec |>                                                                                 # AUFSTELLUNG NACH DENOMINATION ---
    (\(x) c(x, numeric(7))[1:7])() |>                                                        # ergänze auf sieben Stellen
    (\(x, y = c(5, 10, 20, 50, 100, 200, 500))                                               # basierend auf denovec und Scheinwerten ...
     map2_dfr(x, y, ~tibble(Denomination = paste(formatC(.y, width = 3), "Euro"),            # erzeuge Denomination ...
                            Count = .x,                                                      # Count und ...
                            Value = .x  * .y)))() |>                                         # Value
    mutate(across(c(C = Count, V = Value), ~. / sum(.) * 100, .names = "{.col}_Pct"),        # erzeuge Pct (neue Namen wegen Benamsung Ergebnis)
           across(c(C = C_Pct, V = V_Pct),                                                   # erzeuge Graphen  (neue Namen wegen Benamsung Ergebnis)
                  ~map_chr(., ~paste(rep("#", ceiling(. / 1.5)), collapse = "")),            # !!! hier wird definiert wievile Prozentpunkte ein Zeichen ergeben
                  .names = "{.col}_Graph")) |>                                               #                                                |
    mutate(across(c("Count", "Value"), ~format(., big.mark = ",")),                          # formatieren Tausender (Trennung)               |
           across(ends_with("Pct"), ~round(., 2)),                                           # runde Pct auf zwei Nachkommastellen            v
           across(ends_with("Graph"), ~str_pad(., width = 40 / 1.5, side = "right"))) |>     # formatieren Graph auf fixe Breite    (max_% / ...)
    relocate(starts_with("V"), .after = C_Graph) |>                                          # umsortieren der Reihenfolge der Variablen
    as.data.frame()                                                                          # konvertiere in data.frame
); rm(denovec)
