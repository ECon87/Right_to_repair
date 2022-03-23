library(dplyr)
library(ggplot2)
library(janitor)
library(fixest)
library(sjmisc)
library(broom)
library(tidysynth)
library(readr)

`%notin%` <- Negate(`%in%`)

# ===============================================
#   * directories *
# ===============================================

rootdir <- "/home/econ87/Research/Papers/Right_to_repair/Note/"
datadir <- paste0(rootdir, "Data/")

# ===============================================
#   * load data *
# ===============================================

file <- paste0(datadir, "fha_censuswgts_2000_2020.csv")

data <- read_csv(file, col_types = cols(.default = "c"))

data <- data %>% select(-c("...1", "index"))


# Drop if missing n1_4
data <- data %>% filter(is.na(n1_4) == F)


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



# =========================================================================
#
#                           DiD: GRAPHS
#
# =========================================================================

zone_cut <- 25
treated_states <- c("MA", "RI", "VT", "NY", "CT", "NH")


# ========================================================
# Graphical inspection:
#
# Zip Codes: Border vs Inner
# Zip Codes: Border vs Rest of US
# ========================================================


# ----------------------------
# Zip Codes: Border v Inner
# One graph for each border state
# ----------------------------

### Average Number of Shops in zip code
data %>%
  filter(is.na(distma) == F) %>%
  # filter(Year >= 2000 & Year <= 2016) %>%
  mutate(Zone = case_when(
    distma <= zone_cut ~ "Border",
    distma > zone_cut &
      distma <= 2 * zone_cut ~ "Next to Border",
    distma > 2 * zone_cut ~ "Inner"
  )) %>%
  group_by(State_Code, Zone, Year) %>%
  # mutate(Shops = mean(as.numeric(n1_4), na.rm = T)) %>%
  mutate(Shops = sum(as.numeric(n1_4), na.rm = T)) %>%
  ungroup() %>%
  select(State_Code, Zone, Year, Shops) %>%
  distinct() %>%
  # ggplot(aes(x = Year, y = Sum_n1_4, color = factor(Zone))) +
  ggplot(aes(x = Year, y = Shops, color = Zone)) +
  geom_line() +
  geom_vline(xintercept = 2012) +
  facet_wrap(~State_Code, scales = "free")


data %>%
  filter(is.na(distma) == F) %>%
  mutate(Zone = case_when(
    distma <= zone_cut ~ "Border",
    distma > zone_cut &
      distma <= 2 * zone_cut ~ "Next to Border",
    distma > 2 * zone_cut ~ "Inner"),
         T_Year = if_else(Year >= 2012, 1, 0),
         T = if_else(Zone == "Border" & T_Year == 1, 1, 0)) %>%
  filter(Zone != "Inner") %>%
  filter(Year <= 2014) %>%
  feols(n1_4 ~ T | to_factor(Year) + to_factor(zip),
        data = .)

data %>%
  filter(is.na(distma) == F) %>%
  mutate(Zone = case_when(
    distma <= zone_cut ~ "Border",
    distma > zone_cut &
      distma <= 2 * zone_cut ~ "Next to Border",
    distma > 2 * zone_cut ~ "Inner"),
         T_Year = if_else(Year >= 2012, 1, 0),
         T = if_else(Zone == "Border" & T_Year == 1, 1, 0)) %>%
  filter(Zone != "Next to Border") %>%
  filter(Year <= 2014) %>%
  filter(State_Code == "RI") %>%
  feols(n1_4 ~ T | to_factor(Year) + to_factor(zip),
        data = .)


### Total (Sum) Number of Shops in zip code
### Exclude inner
data %>%
  filter(is.na(distma) == F) %>%
  # filter(Year >= 2000 & Year <= 2016) %>%
  mutate(Zone = case_when(
    distma <= zone_cut ~ "Border",
    distma > zone_cut &
      distma <= 2 * zone_cut ~ "Next to Border",
    distma > 2 * zone_cut ~ "Inner"
  )) %>%
  filter(Zone != "Inner") %>%
  group_by(State_Code, Zone, Year) %>%
  # mutate(Shops = mean(as.numeric(n1_4), na.rm = T)) %>%
  mutate(Shops = sum(as.numeric(n1_4), na.rm = T)) %>%
  ungroup() %>%
  select(State_Code, Zone, Year, Shops) %>%
  distinct() %>%
  # ggplot(aes(x = Year, y = Sum_n1_4, color = factor(Zone))) +
  ggplot(aes(x = Year, y = Shops, color = Zone)) +
  geom_line() +
  geom_vline(xintercept = 2012) +
  facet_wrap(~State_Code, scales = "free")


# ----------------------------
# Zip Codes: Border v Inner
# One graph for all border state
# ----------------------------

### Average Number of Shops in zip code

rest_inner <- data %>%
  filter(State_Code %in% treated_states & distma > zone_cut & distma <= 2 * zone_cut) %>%
  filter(State_Code != "MA") %>%
  filter(is.na(State_Code) == F) %>%
  mutate(Zone = 0) %>%
  group_by(Year) %>%
  mutate(Shops = mean(n1_4, na.rm = T)) %>%
  ungroup() %>%
  select(Year, Zone, Shops) %>%
  distinct()


treat_sts <- data %>%
  filter(State_Code %in% treated_states & distma <= zone_cut) %>%
  filter(State_Code != "MA") %>%
  mutate(Zone = 1) %>%
  group_by(Zone, Year) %>%
  mutate(Shops = mean(n1_4, na.rm = T)) %>%
  ungroup() %>%
  select(Year, Zone, Shops) %>%
  distinct()


treat_sts <- rbind(treat_sts, rest_inner)


treat_sts %>%
  ggplot(aes(x = Year, y = Shops, color = factor(Zone))) +
  geom_line() +
  geom_vline(xintercept = 2012)



### Total (Sum) Number of Shops in zip code

rest_inner <- data %>%
  filter(State_Code %in% treated_states &
         distma > zone_cut &
         distma <= 2 * zone_cut) %>%
  filter(State_Code != "MA") %>%
  filter(is.na(State_Code) == F) %>%
  mutate(Zone = 0) %>%
  group_by(Year) %>%
  mutate(Shops = sum(n1_4, na.rm = T)) %>%
  ungroup() %>%
  select(Year, Zone, Shops) %>%
  distinct()


treat_sts <- data %>%
  filter(State_Code %in% treated_states & distma <= zone_cut) %>%
  filter(State_Code != "MA") %>%
  mutate(Zone = 1) %>%
  group_by(Zone, Year) %>%
  mutate(Shops = sum(n1_4, na.rm = T)) %>%
  ungroup() %>%
  select(Year, Zone, Shops) %>%
  distinct()


treat_sts <- rbind(treat_sts, rest_inner)


treat_sts %>%
  ggplot(aes(x = Year, y = Shops, color = factor(Zone))) +
  geom_line() +
  geom_vline(xintercept = 2012)



# ----------------------------------
# Zip Codes: Border vs Rest of US
# One graph for each border state
#
### Total (Sum) Number of Shops in zip code
### SKIP
### the rest of the US will have an insane number of shops
# ----------------------------------

### Average Number of Shops in zip code
rest_us <- data %>%
  filter(State_Code %notin% treated_states |
    (State_Code %in% treated_states & distma > zone_cut)) %>%
  filter(State_Code != "MA") %>%
  filter(is.na(State_Code) == F) %>%
  mutate(Zone = 0, State_Code = "US") %>%
  group_by(Year) %>%
  mutate(Shops = mean(n1_4, na.rm = T)) %>%
  ungroup() %>%
  select(Year, State_Code, Zone, Shops) %>%
  distinct()


treat_sts <- data %>%
  filter(State_Code %in% treated_states & distma <= zone_cut) %>%
  filter(State_Code != "MA") %>%
  mutate(Zone = 1) %>%
  group_by(State_Code, Zone, Year) %>%
  mutate(Shops = mean(n1_4, na.rm = T)) %>%
  ungroup() %>%
  select(Year, State_Code, Zone, Shops) %>%
  distinct()

# One graph for each state

for (st in treated_states) {
  if (st == "MA") {
    print("")
  } else {
    rest_us <- rest_us %>% mutate(State_Code = st)
    treat_sts <- rbind(treat_sts, rest_us)
  }
}


treat_sts %>%
  ggplot(aes(x = Year, y = Shops, color = factor(Zone))) +
  geom_line() +
  geom_vline(xintercept = 2012) +
  facet_wrap(~State_Code, scales = "free")




# ----------------------------------
# Zip Codes: Border vs Rest of US
# One graph for all border state
# ----------------------------------

rest_us <- data %>%
  filter(State_Code %notin% treated_states |
    (State_Code %in% treated_states & distma > zone_cut)) %>%
  filter(State_Code != "MA") %>% 
  filter(is.na(State_Code) == F) %>%
  mutate(Zone = 0) %>%
  group_by(Year) %>%
  mutate(Shops = mean(n1_4, na.rm = T)) %>%
  ungroup() %>%
  select(Year, Zone, Shops) %>%
  distinct()

treat_sts <- data %>%
  filter(State_Code %in% treated_states & distma <= zone_cut) %>%
  filter(State_Code != "MA") %>%
  mutate(Zone = 1) %>%
  group_by(Zone, Year) %>%
  mutate(Shops = mean(n1_4, na.rm = T)) %>%
  ungroup() %>%
  select(Year, Zone, Shops) %>%
  distinct()


treat_sts <- rbind(treat_sts, rest_us)

treat_sts %>%
  ggplot(aes(x = Year, y = Shops, color = factor(Zone))) +
  geom_line() +
  geom_vline(xintercept = 2012)



# =============================================================






# ======================================================
# Synthetic Control: County level data
#
#
# I think I will do the states separately!
# ======================================================


end_yr <- 2016
start_yr <- 2000

### Define Treatment to be within cutoff
cty_data <- cty_data %>%
    # mutate(Treat = if_else(distma_min + distma_max < 2 * cut, 1, 0))
    mutate(Treat = if_else(distma_min <= cut, 1, 0))


### Drop MA
cty_data <- cty_data %>%
    filter(State_Code != "MA")


# ==============
# RI
# ==============

cty_data %>%
    filter(Treat == 1) %>%
    tabyl(State_Code)

cty_data %>%
    filter(State_Code %notin% c("VT", "NY", "CT", "NH") &
           is.na(State) == F) %>%
    mutate(State_Syn = case_when(State_Code == "RI" & Treat == 1 ~ "RI-Border",
                                 State_Code == "RI" &
                                     (Treat != 1 | is.na(Treat)) ~ "RI-Inner"),
           State_Syn = if_else(is.na(State_Syn), State_Code, State_Syn)
           ) %>%
    tabyl(State_Syn)




# ==============
# VT
# ==============

# ==============
# NY
# ==============

# ==============
# CT
# ==============

# ==============
# NH
# ==============



rest_us_syn <- data %>%
  filter(State %notin% treated_states |
    (State %in% treated_states & Short_Dist_MA > cut)) %>%
  filter(is.na(State) == F) %>%
  mutate(y = as.numeric(n1_4)) %>%
  group_by(Year, State) %>%
  mutate(y_agg = sum(y, na.rm = T)) %>%
  ungroup() %>%
  select(Year, State, y_agg) %>%
  distinct()

treat_sts_syn <- data %>%
  filter(State %in% treated_states & Short_Dist_MA <= cut) %>%
  filter(State != "MA") %>%
  mutate(State = "Border") %>%
  mutate(y = as.numeric(n1_4)) %>%
  group_by(Year) %>%
  mutate(y_agg = sum(y, na.rm = T)) %>%
  ungroup() %>%
  select(Year, State, y_agg) %>%
  distinct()

treat_sts_syn <- rbind(treat_sts_syn, rest_us_syn)
treat_sts_syn <- treat_sts_syn %>% filter(Year >= start_yr & Year <= end_yr)

smoking_out <- smoking %>%
  # initialize the synthetic control object
  synthetic_control(
    outcome = cigsale,
    unit = state,
    time = Year,
    i_unit = "California", # where intervention happened
    i_time = 1988, # Year of intervention
    generate_placebos = T
  )
treat_sts_syn

smoking_out <- treat_sts_syn %>%
  filter(is.na(treat_sts_syn) == F) %>%
  # initialize the synthetic control object
  synthetic_control(
    outcome = y_agg,
    unit = State,
    time = Year,
    i_unit = "Border", # where intervention happened
    i_time = 2012 # Year of intervention
  )


## Inference
smoking_out %>% plot_placebos()
smoking_out %>% plot_placebos(prune = F)

smoking_out %>% plot_mspe_ratio()


# ========================================================
# REG
# =======================================================

cut <- 15

for (cut in c(5, 10, 15, 25, 35, 50, 75)) {
  print(cut)
  data %>%
    filter(Year > 2005 & Year < 2015) %>%
    mutate(
      Zone = if_else(Zone_Band > cut |
        is.na(Zone_Band), 0, 1),
      Yr = if_else(Year >= 2012, 1, 0)
    ) %>%
    feols(as.numeric(n1_4) ~ -1 + (to_factor(Yr) * to_factor(Zone)),
      data = .
    ) %>%
    tidy() %>%
    adorn_rounding(3) %>%
    print()
  print("")
}


data %>%
  filter(Year > 2007 & Year < 2015) %>%
  mutate(
    Zone = if_else(Zone_Band > 30 |
      is.na(Zone_Band), 0, 1),
    Yr = if_else(Year >= 2012, 1, 0)
  ) %>%
  feols(as.numeric(n1_4) ~ -1 + (to_factor(Yr) * to_factor(Zone) * to_factor(State_Code)),
    data = .
  ) %>%
  tidy() %>%
  adorn_rounding(3)
