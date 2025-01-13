# Load necessary libraries
library(readr)
library(readxl)
library(dplyr)
library(ggplot2)
library(lubridate)
library(tidyr)

cross_19_h <- read_delim("GYKA-Kesit/GYKA_Kesit_2019/english/silc19_h.csv", delim = ";")
cross_19_pr <- read_delim("GYKA-Kesit/GYKA_Kesit_2019/english/silc19_pr.csv", delim = ";")
cross_19_p <- read_delim("GYKA-Kesit/GYKA_Kesit_2019/english/silc19_p.csv", delim = ";")

cross_19_hh <- cross_19_h %>%
  select(HH_ID, HB010, HB031, HB040, HB050, HG110)

cross_19_pp <- cross_19_p %>%
  select(HH_ID, PERS_ID, FB010, FB030, FB100, FE030, FI010, FI040, 
         FI120, FI130, FI140, FI150, FI160, FI190,
         FI240, FI250, FG140)

cross_19_prp <- cross_19_pp %>%
  full_join(cross_19_pr, by = c("HH_ID", "PERS_ID"))

# Merge household data with the merged personal data
cross_19 <- cross_19_prp %>%
  left_join(cross_19_hh, by = "HH_ID")

write_csv(cross_19, "cross_19.csv")


