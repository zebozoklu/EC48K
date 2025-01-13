# Load necessary libraries
library(readr)
library(readxl)
library(dplyr)
library(ggplot2)
library(lubridate)
library(tidyr)

cross_20_h <- read_csv("GYKA-Kesit/GYKA_Kesit_2020/english/silc20_h.csv")
cross_20_pr <- read_csv("GYKA-Kesit/GYKA_Kesit_2020/english/silc20_pr.csv")
cross_20_p <- read_csv("GYKA-Kesit/GYKA_Kesit_2020/english/silc20_p.csv")

cross_20_hh <- cross_20_h %>%
  select(HH_ID, HB010, HB031, HB040, HB050, HG110)

cross_20_pp <- cross_20_p %>%
  select(HH_ID, PERS_ID, FB010, FB030, FB100, FE030, FI010, FI040, 
         FI120, FI130, FI140, FI150, FI160, FI190,
         FI240, FI250, FG140)

cross_20_prp <- cross_20_pp %>%
  full_join(cross_20_pr, by = c("HH_ID", "PERS_ID"))

# Merge household data with the merged personal data
cross_20 <- cross_20_prp %>%
  left_join(cross_20_hh, by = "HH_ID")

write_csv(cross_20, "cross_20.csv")


