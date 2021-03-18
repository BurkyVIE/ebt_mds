library(tidyverse)

streak <- function(data, varname){
  
  varname = enquo(varname)
  retKind = paste0("Cons", quo_name(varname))
  retNo = paste0(quo_name(varname), "Str")

  tmp <- tibble(Data = pull(data, !!varname),
                No = (Data > 0) %>%
                 rle() %>%
                 `[[`(1) %>%
                 rep(1:length(.), .)
               ) %>% 
    mutate(No = ceiling(No / 2),
           Kind = case_when(Data == 0 ~ "inactive",
                            TRUE ~ "active"))

  return(tmp %>% select(!!retKind := Kind, !!retNo := No))
}

ebt_mds_grpd(per = "day") %>% bind_cols(., streak(., Count)) %>% bind_cols(., streak(., Hits))
