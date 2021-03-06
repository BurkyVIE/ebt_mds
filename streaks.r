library(tidyverse)

streak <- function(data, varname){
  
  retKind = paste0("Cons", ensym(varname))
  retNo = paste0(ensym(varname), "Str")

  tmp <- tibble(Data = pull(data, {{varname}}),
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

# examle
# ebt_mds_grpd(per = "day") %>% bind_cols(., streak(., Count)) %>% bind_cols(., streak(., Hits))
