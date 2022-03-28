library(dplyr)
library(ggplot2)
library(janitor)
library(fixest)
library(sjmisc)
library(broom)
library(tidysynth)
library(readr)
library(kableExtra)

`%notin%` <- Negate(`%in%`)

# ===============================================
#   * directories *
# ===============================================

rootdir <- "/home/econ87/Research/Papers/Right_to_repair/Note/"
datadir <- paste0(rootdir, "Data/")
codedir <- paste0(rootdir, "Code/")

# ===============================================
#   * load data *
# ===============================================

file <- paste0(datadir, "fha_censuswgts_2000_2020.csv")

data <- read_csv(file, col_types = cols(.default = "c"))

data <- data %>% select(-c("...1", "index"))


# Drop if missing n1_4 or n_total
data <- data %>% filter(is.na(n1_4) == F)
data <- data %>% filter(is.na(n_total) == F)

row.names(data) <- NULL

# =========================================================================
#
#                           DATA MANIPULATION
#
# =========================================================================

source(paste0(codedir, "c2a_inner_data_manipulation.R"))



####### Drop if in treated state and missing distance ###########
### missing Zone_Band -> distma > 200

treated_states <- c("MA", "RI", "VT", "NY", "CT", "NH")

data %>%
  filter(State_Code %in% treated_states) %>%
  filter(State_Code != "MA") %>%
  filter(is.na(Zone_Band)) %>%
  select(zip, State_Code, Lat_Long, Zone_Band, distma) %>%
  distinct() %>%
  filter(is.na(Zone_Band)) %>%
  summary()


### Fix Zone Band

data <- data %>%
    mutate(Zone_Band = if_else(
            State_Code %in% treated_states &
                is.na(Zone_Band), 201, Zone_Band))
 



# =========================================================================
#
#                           DiD: GRAPHS
#
#
# From the cross-tabulation of Zone and State, it is best to limit the analysis
# that compares border zip codes and inner zip codes to CT & NH!!!
# These are the only states with (relative) uniform distribution of zip codes.
#
# For the analysis for which I compare with rest of the US, I can include
# the rest of the states.

# =========================================================================


zone_cut <- 25

treated_states <- c("MA", "RI", "VT", "NY", "CT", "NH")


# Cross-tabulation between Zone and State
data %>%
  filter(State_Code %in% treated_states) %>%
  filter(State_Code != "MA") %>%
  select(zip, Zone_Band, State_Code) %>%
  distinct() %>%
  tabyl(Zone_Band, State_Code) %>%
  adorn_percentages("col") %>%
  adorn_rounding(2) %>%
  adorn_ns() %>%
  kable("markdown")







# ========================================================
# Graphical inspection:
#
# A. Zip Codes: Border vs Rest of US
# B. Zip Codes: Border vs Inner
# ========================================================

# ---------------------------------------
#
# A.1. Zip Codes: Border vs Rest of US
# One graph for *ALL* border state {{{
# ----------------------------------

# Y: n1_4
rest_us <- data %>%
  filter(State_Code %notin% treated_states |
    (State_Code %in% treated_states & distma > zone_cut) |
    (State_Code %in% treated_states & is.na(distma))) %>%
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
  # filter(Year >= 2006) %>%
  ggplot(aes(x = Year, y = Shops, color = factor(Zone))) +
  geom_line() +
  geom_vline(xintercept = 2012)



# ----------------
# Regression

regdata <- data %>%
  filter(is.na(State_Code) == F) %>%
  filter(State_Code != "MA") %>%
  mutate(Zone = if_else(State_Code %notin% treated_states |
                        (State_Code %in% treated_states & distma > zone_cut) |
                        (State_Code %in% treated_states & is.na(distma)), 0, 1),
         log_n1_4 = log(1 + n1_4),
         log_ntotal = log(1 + n_total),
         Yt = if_else(Year >= 2012, 1, 0),
         DID = Zone * Yt)


# regdata <- panel(regdata, ~ zip + Year)

names(regdata)

est0_1_4 <- feols(n1_4 ~ DID +
              distma + Population + Inc_percapita +
              allmv_total + vehicle_miles |
              to_factor(Yt) + to_factor(Zone), data = regdata)

est0_ntotal <- feols(n_total ~ DID +
              distma + Population + Inc_percapita +
              allmv_total + vehicle_miles |
              to_factor(Yt) + to_factor(Zone), data = regdata)

etable(est0_1_4, est0_ntotal)



est1 <- regdata %>% feols(n_total ~ l(DID, -2:6), data = .)

feols(n_total ~ l(DID, -2:4), panel.id = ~zip + Year, data = regdata)

regdata %>%
    arrange(zip, Year) %>%
    group_by(zip) %>%
    mutate(
           lead1 = dplyr::lead(DID, n = 1),
           lead2 = dplyr::lead(DID, n = 2),
           lag1 = dplyr::lag(DID, n = 1),
           lag2 = dplyr::lag(DID, n = 2),
           lag3 = dplyr::lag(DID, n = 3),
           lag4 = dplyr::lag(DID, n = 4),
    ) %>%
    tabyl(lag4)
    feols(n_total ~ lead1 + lead2 + DID + lag1 + lag2 + lag3 + lag4,
          data = .)


etable(est0, est1)

names(cty_data)




# }}}
#
# ---------------------------------------


# -------------------------------------
#
# A.2. Zip Codes: Border vs Rest of US
# One graph for *EACH* border state {{{
#
# 1. Average
### SKIP: Sum of Shops in zip code
# -------------------------------------
# ----------------------------------

### Average Number of Shops in zip code
rest_us <- data %>%
  filter(State_Code %notin% treated_states |
    (State_Code %in% treated_states & distma > zone_cut) |
    (State_Code %in% treated_states & is.na(distma))) %>%
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


# }}}
#
# -------------------------------------


# -------------------------------------
#
# B.1 Zip Codes: Border v Inner
# One graph for *EACH* border state {{{
# -------------------------------------
#
# 1. Average
# 2. Sum

### 1. Average Number of Shops in zip code
data %>%
  filter(is.na(distma) == F) %>%
  # filter(Year >= 2000 & Year <= 2014) %>%
  mutate(Zone = case_when(
    distma <= zone_cut ~ "Border",
    distma > zone_cut &
      distma <= 2 * zone_cut ~ "Next to Border",
    (distma > 2 * zone_cut) | (is.na(distma)) ~ "Inner"
  )) %>%
  group_by(State_Code, Zone_Band, Year) %>%
  mutate(Shops = mean(as.numeric(n1_4), na.rm = T)) %>%
  ungroup() %>%
  select(State_Code, Zone_Band, Year, Shops) %>%
  distinct() %>%
  # ggplot(aes(x = Year, y = Sum_n1_4, color = factor(Zone))) +
  ggplot(aes(x = Year, y = Shops, color = factor(Zone_Band))) +
  geom_line() +
  geom_vline(xintercept = 2012) +
  facet_wrap(~State_Code, scales = "free")


data %>%
  filter(is.na(distma) == F) %>%
  mutate(Zone = case_when(
    distma <= zone_cut ~ "Border",
    distma > zone_cut &
      distma <= 2 * zone_cut ~ "Next to Border",
    (distma > 2 * zone_cut) | (is.na(distma)) ~ "Inner",
         T_Year = if_else(Year >= 2012, 1, 0),
         T = if_else(Zone == "Border" & T_Year == 1, 1, 0)) %>%
  filter(Zone != "Inner") %>%
  filter(Year <= 2014) %>%
  feols(n1_4 ~ T | to_factor(Year) + to_factor(zip),
        data = .)



### 2. Sum of Shops in zip code
### Exclude inner
data %>%
  filter(is.na(distma) == F) %>%
  # filter(Year >= 2000 & Year <= 2016) %>%
  mutate(Zone = case_when(
    distma <= zone_cut ~ "Border",
    distma > zone_cut &
      distma <= 2 * zone_cut ~ "Next to Border",
    (distma > 2 * zone_cut) | (is.na(distma)) ~ "Inner"
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

# }}}



# B.1b Zip Codes CT and NH *only*: Border v Inner
# One graph for *EACH* border state {{{
# -------------------------------------
#
# 1. Average
# 2. Sum

print(zone_cut)
### 1. Average Number of Shops in zip code
data %>%
  filter(State_Code %in% c("NH", "CT")) %>%
  # filter(Year >= 2000 & Year <= 2014) %>%
  mutate(Zone = case_when(
    distma <= zone_cut ~ "Border",
    distma > zone_cut &
      distma <= 2 * zone_cut ~ "Next to Border",
    (distma > 2 * zone_cut) | (is.na(distma)) ~ "Inner"
  )) %>%
  group_by(State_Code, Zone, Year) %>%
  # mutate(Shops = mean(as.numeric(n1_4), na.rm = T)) %>%
  mutate(Shops = mean(as.numeric(n_total), na.rm = T)) %>%
  ungroup() %>%
  select(State_Code, Zone, Year, Shops) %>%
  distinct() %>%
  ggplot(aes(x = Year, y = Shops, color = factor(Zone))) +
  geom_line() +
  geom_vline(xintercept = 2012) +
  facet_wrap(~State_Code, scales = "free")


data %>% filter(is.na(distma) == F) %>%
  mutate(Zone = case_when(
    distma <= zone_cut ~ "Border",
    distma > zone_cut &
      distma <= 2 * zone_cut ~ "Next to Border",
    (distma > 2 * zone_cut) | (is.na(distma)) ~ "Inner",
         T_Year = if_else(Year >= 2012, 1, 0),
         T = if_else(Zone == "Border" & T_Year == 1, 1, 0)) %>%
  filter(Zone != "Inner") %>%
  filter(Year <= 2014) %>%
  feols(n1_4 ~ T | to_factor(Year) + to_factor(zip),
        data = .)



### 2. Sum of Shops in zip code
### Exclude inner
data %>%
  filter(is.na(distma) == F) %>%
  # filter(Year >= 2000 & Year <= 2016) %>%
  mutate(Zone = case_when(
    distma <= zone_cut ~ "Border",
    distma > zone_cut &
      distma <= 2 * zone_cut ~ "Next to Border",
    (distma > 2 * zone_cut) | (is.na(distma)) ~ "Inner"
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

# }}}
#

# -------------------------------------


# -------------------------------------
#
# B.2 Zip Codes: Border v Inner
# One graph for *ALL* border state {{{
#
# 1. Average
# 2. Sum
# -------------------------------------



### 1. Average Number of Shops in zip code

rest_inner <- data %>%
  filter(State_Code %in% treated_states &
         ((distma > zone_cut & distma <= 2 * zone_cut) | (is.na(distma))) %>%
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



### 2. Sum of Shops in zip code

rest_inner <- data %>%
  filter(State_Code %in% treated_states &
         ((distma > zone_cut & distma <= 2 * zone_cut) | (is.na(distma)))) %>%
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


# }}}
#
# -------------------------------------


# ------------------
# Practice DiD regs
# ------------------

for (i in treated_states) {
    print(i)
    mod_next <- data %>%
        filter(State_Code == i) %>%
        mutate(Zone = case_when(
                    distma <= zone_cut ~ "Border",
                    distma > zone_cut &
                        distma <= 2 * zone_cut ~ "Next",
                    (distma > 2 * zone_cut) | (is.na(distma)) ~ "Inner"),
               Tz = if_else(Zone == "Border", 1, 0),
               Ty = if_else(Year >= 2012, 1, 0)) %>%
        filter(Year >= 2006 & Zone != "Inner") %>%
        feols(n_total ~ (Tz * Ty) | to_factor(Year) + to_factor(zip),
              data = .)
    mod_inner <- data %>%
        filter(State_Code == i) %>%
        mutate(Zone = case_when(
                    distma <= zone_cut ~ "Border",
                    distma > zone_cut &
                        distma <= 2 * zone_cut ~ "Next",
                    (distma > 2 * zone_cut) | (is.na(distma)) ~ "Inner"),
               Tz = if_else(Zone == "Border", 1, 0),
               Ty = if_else(Year >= 2012, 1, 0)) %>%
        filter(Year >= 2006 & Zone != "Next") %>%
        feols(n_total ~ (Tz * Ty) | to_factor(Year) + to_factor(zip),
              data = .)
        print(paste("State:", i))
        print("\\nNext to Border")
        print(mod_next)
        print("\\nInner")
        print(mod_inner)
}


data %>%
    filter(State_Code == "CT") %>%
    mutate(Zone = case_when(
                distma <= zone_cut ~ "Border",
                distma > zone_cut &
                    distma <= 2 * zone_cut ~ "Next",
                (distma > 2 * zone_cut) | (is.na(distma)) ~ "Inner"),
           Tz = if_else(Zone == "Border", 1, 0),
           Ty = if_else(Year >= 2012, 1, 0)) %>%
    filter(Year >= 2006 & Zone != "Inner") %>%
    feols(n_total ~ (Tz * to_factor(Year)) | to_factor(zip),
          data = .)



# =============================================================






# ======================================================
# Synthetic Control: County level data
#
#
# I think I will do the states separately!
# ======================================================


end_yr <- 2016
start_yr <- 2000
cut <- 15


cty_data %>%
    select(distma_min, distma_max) %>%
    head()

### Define Treatment to be within cutoff
cty_data <- cty_data %>%
    mutate(Treat0 = if_else(distma_min + distma_max < 1.5 * cut, 1, 0),
           Treat1 = if_else(distma_min <= cut, 1, 0))


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
