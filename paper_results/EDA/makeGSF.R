library(deuce)
library(dplyr)

data("point_by_point")
pbp = tbl_df(point_by_point)
pbp$wim = grepl("Wimbledon", pbp$tny_name)
pbp$french = grepl("FrenchOpen", pbp$tny_name)
pbp$aus = grepl("AustralianOpen", pbp$tny_name)
pbp$us = grepl("USOpen", pbp$tny_name)
pbp$qual = grepl("Qualifying", pbp$tny_name)

# Create data for Grand Slam finals
gsf = select(pbp, c("tny_name", "wim", "french", "us", "aus", "qual")) %>%
  filter(., wim | french | us | aus & !qual)

save(gsf, file = "GrandSlamFinals.RData")