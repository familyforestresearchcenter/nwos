library(dplyr)
library(tidyr)

SAMPLE <- readRDS("../../SAMPLE/DATA/PPS_SAMPLE_20180831.rds")

wi <- SAMPLE %>%
  filter(SAMPLE==1) %>%
  select(-SAMPLE) %>%
  add_count(OWN_ID, LAND_USE) %>%
  rename(POINT_COUNT = n) %>%
  distinct()

write.csv(wi, "data/wi.csv", row.names = FALSE)
