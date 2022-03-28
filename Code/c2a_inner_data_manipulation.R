# =========================================================================
#
#                           DATA MANIPULATION
#
# =========================================================================

# ===============================================
# DATA DROP
#
# Do I have zip codes missing for some Years?
# Keep Years 2000-2016 since it is the maximum b/ced sample
# Also I seem to be missing a lot of zips in 2016.
# ===============================================

# Drop if Year > 2016
data <- data %>% filter(as.numeric(Year) <= 2016)


data <- data %>%
  group_by(zip) %>%
  mutate(Count = n()) %>%
  ungroup()


# Drop Year > 2016 and Count < 17
data <- data %>% filter(Count == 17)


# #######

row.names(data) <- NULL

# #######


# ===============================================
# DATA DROP 2
#
# Non comparable observations
# If nX_Y = N then either non-comparable or available.
# Drop the zipcodes wiht at least one N!
#
# Also, there are some missing values. Thes are unmatched
# observations from the merge in the file c1d!
#
# For now focus the analysis to n_total and n1_4
# ===============================================


# Drop if zip code contains an "N"
data <- data %>%
  mutate(
    n_total = if_else(n_total == "N", "-1000", n_total),
    n1_4 = if_else(n1_4 == "N", "-1000", n1_4),
    n5_9 = if_else(n5_9 == "N", "-1000", n5_9),
    n10_19 = if_else(n10_19 == "N", "-1000", n10_19),
    n20_49 = if_else(n20_49 == "N", "-1000", n20_49),
    n50_99 = if_else(n50_99 == "N", "-1000", n50_99),
    n100_249 = if_else(n100_249 == "N", "-1000", n100_249),
    n250_499 = if_else(n250_499 == "N", "-1000", n250_499),
    n500_999 = if_else(n500_999 == "N", "-1000", n500_999),
    n1000 = if_else(n1000 == "N", "-1000", n1000),
  ) %>%
  mutate(
    n_total = as.numeric(n_total),
    n1_4 = as.numeric(n1_4),
    n5_9 = as.numeric(n5_9),
    n10_19 = as.numeric(n10_19),
    n20_49 = as.numeric(n20_49),
    n50_99 = as.numeric(n50_99),
    n100_249 = as.numeric(n100_249),
    n250_499 = as.numeric(n250_499),
    n500_999 = as.numeric(n500_999),
    n1000 = as.numeric(n1000),
  ) %>%
  rowwise() %>%
  mutate(Min_n = min(c_across(n_total:n1_4)))

data <- data %>%
  group_by(zip) %>%
  mutate(Min_n = min(Min_n)) %>%
  ungroup()


data <- data %>% filter(Min_n >= 0)

## Drop Count, Min_n and _merge var
data <- data %>% select(-c("Count", "_merge", "Min_n"))

# ================================================
# Numeric: make some variables numeric
# ================================================


data <- data %>%
  mutate(
    zip = as.numeric(zip),
    Year = as.numeric(Year),
    Census_Year = as.numeric(Year),
    Short_Dist_MA = as.numeric(Short_Dist_MA),
    Zone_Band = as.numeric(Zone_Band),
    State_Population = as.numeric(State_Population),
    Population = as.numeric(Population),
    Population_Share = as.numeric(Population_Share),
    Inc_percapita = as.numeric(Inc_percapita),
    allmv_pvt = as.numeric(allmv_pvt),
    allmv_pub = as.numeric(allmv_pub),
    allmv_total = as.numeric(allmv_total),
    vehicle_miles = as.numeric(vehicle_miles)
  )

# ================================================
# Make FHA data into county equivalents since now
# they are the state ones.
# Assume uniform distribution ==> Pop_Share x FHA
# ================================================

data <- data %>%
    mutate(allmv_pvt = Population_Share * allmv_pvt,
           allmv_pub = Population_Share * allmv_pub,
           allmv_total = Population_Share * allmv_total,
           vehicle_miles = Population_Share * vehicle_miles)





# ===============================================
# Get Min and Max distance from MA for counties
# in neighboring states.
# Recall that distance is missing by default for
# some zip codes. In such case, replace with the max
# distance among available distances.
# If distance is missing for all zip codes in county
# then definitely not treated.
# ===============================================

data <- data %>%
    group_by(State_Code, County) %>%
    mutate(distma_min = min(Short_Dist_MA),
           distma_max = max(Short_Dist_MA)) %>%
    rename(distma = Short_Dist_MA) %>%
    ungroup()


# =============================================
# Get County data
# =============================================


cty_data <- data %>%
    group_by(State_Code, County) %>%
    mutate(n_total_cty = sum(n_total),
           n1_4_cty = sum(n1_4)) %>%
    select(-c("zip", "Lat_Long", "distma", "Zone_Band", "Lat_nom", "Long_nom",
              "City",
              "n_total", "n1_4", "n5_9", "n10_19", "n20_49", "n50_99",
              "n100_249", "n250_499", "n500_999", "n1000")) %>%
    distinct()


# ========================================
# Retention rate
# ========================================

print(dim(cty_data)[1] / 17 / 3210)
