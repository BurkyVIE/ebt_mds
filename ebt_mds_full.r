library(tidyverse)

source("ebt_mds.txt") # Einlesen des minimal Dataset
source("ebt_mds_grpd.r") # Funktion zum Gruppieren

ebt_mds_full <- ebt_mds_grpd(per = "day")
