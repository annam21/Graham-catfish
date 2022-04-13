# Processing encounter histories 
# Anna Moeller
# 2/2/2021

# packages
library(tidyverse)
library(lubridate)

# Bring in the data
blackwell <- readxl::read_excel(
  "Data/Final datasheet.xlsx", 
  sheet = "Blackwell", 
  col_types = c("date",
                "text", "text", "numeric" , "numeric", "text", 
                "numeric", "date", "numeric", "numeric", 
                "numeric", "numeric", "numeric", 
                "numeric", "numeric", "numeric", 
                "numeric", "text", "numeric", "date", 
                "text", "text", "text", "text", "text", 
                "text", "numeric", "text", "numeric", 
                "text", "numeric", "numeric", "text", 
                "numeric", "numeric", "numeric", 
                "text", "text", "date", "text", "text", 
                "numeric", "numeric")
  )

# Unique ID of the fish is tag. 
# Capture occasion is month (1-19)

# Make a look-up table of our occasions 
# May 2019 to Feb 2021
dts <- seq(ymd("2019-05-01"), ymd("2021-02-01"), by = "month")
lu_occ <- data.frame(
  dt = dts,
  occ = 1:length(dts),
  effort = c(1,1,1,0,1,1,1,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1)
  ) 

# Clean up data
bw <- blackwell %>%
  filter(!is.na(tag),
         tag != "fake") %>%
  # Get rid of any duplicates
  distinct(tag, Date, .keep_all = T) %>%
  # Make it prettier for us to see
  arrange(Date, tag)

# Create a target dataframe
# May 2019 to Feb 2021
target <- expand.grid(
  dt = dts,
  tag = unique(bw$tag)
) %>%
  left_join(., lu_occ)

eh <- bw %>% 
  # Pare down to the important columns 
  select(tag, Date) %>% 
  # Let's initialize an encounter history column (all 1s)
  mutate(eh = 1) %>%
  # Round our dates down so we can join to the lookup table
  mutate(dt_round = floor_date(Date, unit = "month")) %>%
  # Join to the lookup table to find which occasion it is
  left_join(., lu_occ, by = c("dt_round" = "dt")) %>%
  # Join to the target dataframe
  left_join(target, ., by = c("tag", "occ", "effort", "dt" = "dt_round")) %>% 
  # Fill in the EH where it's NA.
  # If EH is NA and effort = 1, EH should be 0. 
  # If EH is NA and effort = 0, EH  should be "."
  mutate(eh = replace(eh, is.na(eh) & effort == 1, 0)) %>%
  mutate(eh = as.character(eh)) %>%
  mutate(eh = replace(eh, is.na(eh), ".")) %>%
  # Now make it wide instead of long 
  # First, select only the columns we need 
  select(tag, occ, eh) %>%
  # Make sure there aren't any duplicates, which cause trouble
  distinct() %>% 
  pivot_wider(names_from = occ, values_from = eh) %>% 
  # Now smush eh into a single column
  unite(col = "EH", 2:23, sep = "")
# Done! 

# Now to think about individual covariates
cov1 <- bw %>% 
  arrange(tag, Date) %>% 
  group_by(tag) %>% 
  summarize(firstlength = first(TL)) %>% 
  # center and scale
  ungroup %>% 
  mutate(
    # If no lengths ever recorded, give them the mean
    firstlength = replace(firstlength, 
                          is.na(firstlength), 
                          mean(firstlength, na.rm = T)),
    firstlen_cs = 
      (firstlength - mean(firstlength, na.rm = T))/
      sd(firstlength, na.rm = T)
  )

# Then join to encounter history
eh_cov1 <- left_join(eh, cov1, by = "tag") 
saveRDS(eh_cov1, "Data/encounter_histories.rds")

# Or a time-varying individual covariate 
# Built the same way as the encounter history
# It would look like: 
tibble::tibble(
  tag = 1:3,
  # occ1 = c(0,1,1),
  # occ2 = c(1, 0,1),
  # occ3 = c(0,0,1),
  eh = c("010", "100", "111"),
  len_occ1 = c(NA,40,50),
  len_occ2 = c(31,NA,51),
  len_occ3 = c(NA,NA,52)
)

# Or to think about occasion covariates 
# In RMARK 
# Imagine... 
lu_occ %>% 
  select(-effort) %>%
  mutate(season = c("spring", 
                    rep(c("summer", "fall", "winter", "spring", 
                          "summer", "fall", "winter"), 
                        each = 3)
                    )
         ) %>% 
  mutate(spring = if_else(season == "spring", 1, 0),
         summer = if_else(season == "summer", 1, 0),
         fall = if_else(season == "fall", 1, 0),
         winter = if_else(season == "winter", 1, 0))
