# Load necessary libraries
library(readr)
library(readxl)
library(dplyr)
library(ggplot2)
library(lubridate)
library(tidyr)

cross_21_h <- read_csv("GYKA-Kesit/GYKA_Kesit_2021/english/silc21_h.csv")
cross_21_pr <- read_csv("GYKA-Kesit/GYKA_Kesit_2021/english/silc21_pr.csv")
cross_21_p <- read_csv("GYKA-Kesit/GYKA_Kesit_2021/english/silc21_p.csv")

cross_21_hh <- cross_21_h %>%
  select(HH_ID, HB010, HB031, HB040, HB050, HG110)

cross_21_pp <- cross_21_p %>%
  select(HH_ID, PERS_ID, FB010, FB030, FB100, FE030, FI010, FI040, 
         FI120, FI130, FI140, FI150, FI160, FI190,
         FI240, FI250, FG140)

cross_21_prp <- cross_21_pp %>%
  full_join(cross_21_pr, by = c("HH_ID", "PERS_ID"))

# Merge household data with the merged personal data
cross_21 <- cross_21_prp %>%
  left_join(cross_21_hh, by = "HH_ID")

write_csv(cross_21, "cross_21.csv")


