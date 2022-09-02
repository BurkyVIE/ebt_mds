# Function to calculate the date on which the same number of notes were entered as in the given 'year'

ebd <- function(year = NULL){

  # Libraries ----
  library(tidyverse)
  library(lubridate)
  
  #Calculate ----
  # Get Data
  dat <- ebt_mds %>%
    transmute(Date, Count = map_int(.x = Deno, .f = ~ as.integer(sum(.x))))
  
  # Get 'count' to look for in the future
  count <- dat |> 
    filter(year(Date) == year) |> 
    pull(Count) |> 
    sum()
  
  # Find EBD
  ebd <- dat |> 
    filter(year(Date) > year) |> 
    mutate(CumCount = cumsum(Count)) |> 
    filter(CumCount >= count) |> 
    head(1)
  # Handle empty result
  if(tally(ebd) == 0) ebd <- NA else
    ebd <- ebd$Date
  
  # Calculate duration to reach EBD
  dur <- as.numeric(
    difftime(ebd, date(
      paste(year, "12", "31", sep = "-")),
    units = "days"))
  
  # Result ----
  # Build result df
  res <- data.frame(
    Selection = year,
    Count = count,
    EBDay = ebd,
    Days = dur,
    StdYear = dur / 365)
  
  # Return Result
  return(res)
  
  # Clean up ----
  rm(year, count, ebd, dur)
}

# map_df(.x = 2005:2021, .f = ~ebd(.x))