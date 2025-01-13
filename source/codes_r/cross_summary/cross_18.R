# Load necessary libraries
library(readr)
library(readxl)
library(dplyr)
library(ggplot2)
library(lubridate)
library(tidyr)

cross_18_h <- read_delim("GYKA-Kesit/GYKA_Kesit_2018/english/silc18_h.csv", delim = ";")
cross_18_pr <- read_delim("GYKA-Kesit/GYKA_Kesit_2018/english/silc18_pr.csv", delim = ";")
cross_18_p <- read_delim("GYKA-Kesit/GYKA_Kesit_2018/english/silc18_p.csv", delim = ";")

cross_18_hh <- cross_18_h %>%
  select(HH_ID, HB010, HB031, HB040, HB050, HG110)

cross_18_pp <- cross_18_p %>%
  select(HH_ID, PERS_ID, FB010, FB030, FB100, FE030, FI010, FI040, 
         FI120, FI130, FI140, FI150, FI160, FI190,
         FI240, FI250, FG140)

cross_18_prp <- cross_18_pp %>%
  full_join(cross_18_pr, by = c("HH_ID", "PERS_ID"))

# Merge household data with the merged personal data
cross_18 <- cross_18_prp %>%
  left_join(cross_18_hh, by = "HH_ID")

write_csv(cross_18, "cross_18.csv")
            
            
            