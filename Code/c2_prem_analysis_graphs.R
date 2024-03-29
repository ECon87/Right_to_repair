library(dplyr)
library(ggplot2)
library(janitor)
library(fixest)
# library(did) # Callaway/SantAnna (fixest does sun and abraham)
# library(did2s)
library(microsynth)
library(tidysynth)
library(sjmisc)
library(broom)
library(tidysynth)
library(readr)
library(kableExtra)

`%notin%` <- Negate(`%in%`)

### NOTE: need to switch response variable to number of shops per capita

# ===============================================
#   * directories *
# ===============================================

rootdir <- "/home/econ87/Documents/Research/Papers/Right_to_repair/Note/"
datadir <- paste0(rootdir, "Data/")
codedir <- paste0(rootdir, "Code/")

# ===============================================
#   * load data *
# ===============================================

file <- paste0(datadir, "fha_censuswgts_2000_2020.csv")

data <- read_csv(file, col_types = cols(.default = "c"))

data <- data %>% select(-c("...1", "index"))

fha_state  <- read_csv(paste0(datadir, "fha_2000_2020.csv"))

fha_state <- fha_state %>%
    select(-c("...1")) %>%
    rename(allmv_pvt_state = allmv_pvt,
           allmv_pub_state = allmv_pub,
           allmv_total_state = allmv_total,
           vehicle_miles_state = vehicle_miles)


# Drop if missing n1_4 or n_total
data <- data %>% filter(is.na(n1_4) == F)
data <- data %>% filter(is.na(n_total) == F)

row.names(data) <- NULL

treated_states <- c("MA", "RI", "VT", "NY", "CT", "NH")

zone_cuts <- c(sort(unique(data$Zone_Band)))


zone_cut <- 25


# =========================================================================
#
#                           DATA MANIPULATION
#
# =========================================================================

source(paste0(codedir, "c2a_inner_data_manipulation.R"))


### Fix Zone Band so that the cutoff is at200
### missing Zone_Band -> distma > 200
# continuous measure until 200, and then 201 for those zipcodes
# that are over 200 miles.

data <- data %>%
    mutate(Zone_Band = if_else(
            State_Code %in% treated_states &
                is.na(Zone_Band), 201, Zone_Band))
 
 

# =========================================================================
#
#           DiD: GRAPHS and Regressions
#     (might move the regressions in their own file.)
#

# The cross-tabulation of Zone and State suggests only CT and NH
# have sufficient number of observations within and outside 25 miles.
# RI have a lot of observations within 15 miles.
# CT & NH are the only states with (relative) uniform distribution of zip codes.

# Thus, CT and NH are appropriate when the control group are just inner zips.
# Instead, if the control group is the rest of the US, then RI can be included.

#
# For the analysis for which I compare with rest of the US, I can include
# the rest of the states.

# =========================================================================


# Cross-tabulation between Zone and State (number of zip codes)
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
# Graphical inspection: Y = n1_4, n_total, n_total per capita
#
# A. State level: Parralel trends fails
#
# B. Zip Code level:
#  B.1. Border (Treat) vs Rest of US (Control)
#  B.2. Border (Treat) vs Inner (Control)
# ========================================================

# ----------------------------
# ----------------------------
#       A. State level
# ----------------------------

state_data <- merge(data, fha_state, all.x = T)
print(dim(data) - dim(state_data)) # DC is missing



# *****************************
# A.1. All states (DID)
# Control: Rest of the US
# Parallel Trends do NOT hold
# *****************************

tmp_data <- state_data %>%
    mutate(Treat_i = if_else(
             (State_Code %in% c("MA", "CT", "RI", "VT", "NH")) |
             (State_Code == "NY" & distma < zone_cut), 1, 0),
           Treat_yr = if_else(Year >= 2012, 1, 0)) %>%
    group_by(State_Code, Year) %>%
    mutate(Total_shops_1_4_percap = 1000 * sum(n1_4) / State_Population,
           Total_shops_percap = 1000 * sum(n_total) / State_Population,
           aver_income_percap = mean(Inc_percapita, na.rm = T),
           allmv_total_state = allmv_total_state / State_Population,
           vehicle_miles_state = vehicle_miles_state / State_Population) %>%
    ungroup() %>%
    select(State_Code, Year,
           Treat_i, Treat_yr,
           Total_shops_1_4_percap,
           Total_shops_percap,
           State_Population,
           aver_income_percap, allmv_total_state, vehicle_miles_state) %>%
    distinct() %>%
    filter(State_Code != "DC")

# All states DID {{{

zone_cut <- 100


tmp_data <- tmp_data %>%
  filter(State_Code %in% c("CT", "RI", "VT", "NH") |
         State_Code %in% c("ME", "PA", "NJ"))

tmp_data %>%
  filter(Year >= 2006) %>%
  group_by(Treat_i, Year) %>%
  mutate(Total_shops_percap = mean(Total_shops_percap, na.rm = T)) %>%
  ungroup() %>%
  select(State_Code, Treat_i, Treat_yr, Year, Total_shops_percap) %>%
  distinct() %>%
  ggplot(aes(x = Year, y = Total_shops_percap, color = factor(Treat_i))) +
  geom_line() +
  geom_vline(xintercept = 2012)



## ---------------
## Regressions


est_st_us_did <- tmp_data %>%
    filter(Year >= 2006) %>%
    feols(Total_shops_percap ~ aver_income_percap + allmv_total_state +
                               vehicle_miles_state +
                               (Treat_i * Treat_yr) | Year + State_Code,
          data = .)


est_st_us_event <- tmp_data %>%
    filter(Year >= 2006) %>%
    mutate(ToT = Year - 2012) %>%
    feols(Total_shops_percap ~ aver_income_percap + allmv_total_state +
                               vehicle_miles_state +
                               i(ToT, Treat_i, ref = -1) | Year + State_Code,
          data = .)


etable(est_st_us_did, est_st_us_event)

iplot(est_st_us_event)


# }}}


## *********************
## Synthetic Control
## *********************




tmp_data_syn <- tmp_data %>%
    group_by(State_Code) %>%
    mutate(aver_income_percap = mean(aver_income_percap, na.rm = T),
           allmv_total_state = mean(allmv_total_state, na.rm = T),
           vehicle_miles_state = mean(vehicle_miles_state, na.rm = T),
           ) %>%
    ungroup() %>%
    filter(State_Code != "DC")




options(scipen=999)


tmp_data_syn %>%
    select(!!cov.var) %>%
    head()


# all states synthetic control (microsynth) {{{


# Declare covariates (time-invariant)
cov.var <- c("aver_income_percap", "allmv_total_state", "vehicle_miles_state")

# Outcome variables (time-variant)
match.out <- c("Total_shops_1_4_percap", "Total_shops_percap")

msynth_ex1 <- microsynth(tmp_data_syn,
                        idvar = "State_Code",
                        timevar = "Year",
                        intvar = "Treat_i",
                        start.pre = 2000, end.pre = 2011, end.post = 2016,
                        match.out = match.out,
                        match.covar = cov.var,
                        result.var = match.out,
                        omnibus.var = match.out,
                        test = "lower",
                        perm=250, jack=TRUE,
                        n.cores = min(parallel::detectCores(), 2))


# Call it
msynth_ex1

summary(msynth_ex1)
plot_microsynth(msynth_ex1, plot.var = "Total_shops_percap")

# }}}



## Tidysynth


for (st in treated_states) {
  print(st)
}


tmp_data %>%
  select(allmv_total_state, vehicle_miles_state) %>%
  head()

st <- "CT"
st <- "NH"
st <- "RI"
nh_synt <- tmp_data %>%
    filter(is.na(vehicle_miles_state) == F) %>%
    filter(State_Code == st | State_Code %notin% treated_states) %>%
    synthetic_control(outcome = Total_shops_percap,
                      unit = State_Code,
                      time = Year,
                      i_unit = st,
                      i_time = 2012,
                      generate_placebos = T) %>%
     generate_predictor(time_window = 2000:2005,
                     inc = mean(aver_income_percap, na.rm = T),
                     miles = mean(vehicle_miles_state, na.rm = T),
                     allmv = mean(allmv_total_state, na.rm = T)) %>%
     generate_predictor(time_window = 2006:2011,
                     inc = mean(aver_income_percap, na.rm = T),
                     miles = mean(vehicle_miles_state, na.rm = T),
                     allmv = mean(allmv_total_state, na.rm = T)) %>%
    generate_predictor(time_window = 2000, shops_2000 = Total_shops_percap) %>%
    generate_predictor(time_window = 2002, shops_2002 = Total_shops_percap) %>%
    generate_predictor(time_window = 2003, shops_2003 = Total_shops_percap) %>%
    generate_predictor(time_window = 2004, shops_2004 = Total_shops_percap) %>%
    generate_predictor(time_window = 2005, shops_2005 = Total_shops_percap) %>%
    generate_predictor(time_window = 2006, shops_2006 = Total_shops_percap) %>%
    generate_predictor(time_window = 2007, shops_2007 = Total_shops_percap) %>%
    generate_predictor(time_window = 2008, shops_2008 = Total_shops_percap) %>%
    generate_predictor(time_window = 2009, shops_2009 = Total_shops_percap) %>%
    generate_predictor(time_window = 2010, shops_2010 = Total_shops_percap) %>%
    generate_predictor(time_window = 2011, shops_2011 = Total_shops_percap) %>%
    # Generate the fitted weights for the synthetic control
    generate_weights(optimization_window = 2000:2011,
                    margin_ipop = .02,
                    sigf_ipop = 7,
                    bound_ipop = 6 # optimizer options
    ) %>%
    # Generate the synthetic control
    generate_control()

nh_synt %>% plot_trends()
nh_synt %>% plot_differences()
nh_synt %>% plot_weights()
nh_synt %>% plot_placebos()
nh_synt %>% plot_placebos(prune = F)
nh_synt %>% plot_mspe_ratio()
nh_synt %>% grab_signficance() %>% print(n=44)


# ---------------------------------------
#
# B.1.i. Zip Codes: Border vs Rest of US
# One graph for *ALL* border state {{{
# ----------------------------------


tmp_data <- data %>%
    mutate(Treat_i = if_else(State_Code %in% treated_states &
                             distma <= zone_cut, 1, 0),
           Treat_yr = if_else(Year >= 2012, 1, 0)) %>%
    mutate(Total_shops_1_4_percap = 1000 * n1_4 / Population,
           Total_shops_percap = 1000 * n_total / Population,
           allmv_total = allmv_total / Population,
           vehicle_miles = vehicle_miles / Population) %>%
    select(State_Code, Year, zip,
           Treat_i, Treat_yr,
           Total_shops_1_4_percap,
           Total_shops_percap,
           Inc_percapita, allmv_total, vehicle_miles)


rest_us <- data %>%
  filter(State_Code != "MA") %>%
  filter(State_Code %notin% treated_states |
         (State_Code %in% treated_states & distma > zone_cut) |
         (State_Code %in% treated_states & is.na(distma))) %>%
  filter(is.na(State_Code) == F) %>%
  mutate(Treat_i = 0) %>%
  group_by(Year) %>%
  mutate(Total_shops_percap = 1000 * mean(n_total / Population, na.rm = T)) %>%
  ungroup() %>%
  select(Year, Treat_i, Total_shops_percap) %>%
  distinct()

treat_sts <- data %>%
  filter(State_Code != "MA") %>%
  filter(State_Code %in% treated_states & distma <= zone_cut) %>%
  mutate(Treat_i = 1) %>%
  group_by(Year) %>%
  mutate(Total_shops_percap = 0.03 + 1000 * mean(n_total / Population, na.rm = T)) %>%
  ungroup() %>%
  select(Year, Treat_i, Total_shops_percap) %>%
  distinct()


treat_sts <- rbind(treat_sts, rest_us)

treat_sts %>%
  filter(Year >= 2006) %>%
  ggplot(aes(x = Year, y = Total_shops_percap, color = factor(Treat_i))) +
  geom_line() +
  geom_vline(xintercept = 2012)



# ---------



tmp_data <- data %>%
    filter(State_Code != "MA") %>%
    filter(is.na(State_Code) == F) %>%
    mutate(Treat_i = case_when(
                State_Code %notin% treated_states |
                (State_Code %in% treated_states & distma > zone_cut) |
                (State_Code %in% treated_states & is.na(distma)) ~ 0,
                State_Code %in% treated_states & distma <= zone_cut ~ 1),
           Treat_yr = if_else(Year >= 2012, 1, 0),
           Total_shops_percap = 1000 * n_total / Population)



est_st_us_did <- tmp_data %>%
    filter(Year >= 2006) %>%
    feols(Total_shops_percap ~ Inc_percapita + allmv_total +
                               vehicle_miles +
                               (Treat_i * Treat_yr) | Year + Treat_i,
          data = .)


est_st_us_event <- tmp_data %>%
    filter(Year >= 2006) %>%
    mutate(ToT = Year - 2012) %>%
    feols(Total_shops_percap ~ Inc_percapita + allmv_total +
                               vehicle_miles +
                               i(ToT, Treat_i, ref = -1) | Year + Treat_i,
          data = .)


etable(est_st_us_did, est_st_us_event)


# }}}
#
# ---------------------------------------


# -------------------------------------
#
# B.1.ii. Zip Codes: Border vs Rest of US
# One graph for *EACH* border state {{{



# -------------------------------------
# A.2.1. Average number of shops in zip code
#
# Note that the rest of the use differs for each state
# since it takes into account the inner states
# ----------------------------------



# A.2.1.i Include all inner zipcodes
## Again New Hampshire and Connecticut seems nice!!!
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
  filter(Year >= 2006 & Year <= 2014) %>%
  ggplot(aes(x = Year, y = Shops, color = factor(Zone))) +
  geom_line() +
  geom_vline(xintercept = 2012) +
  facet_wrap(~State_Code, scales = "free")



## A.2.1.ii Include some inner zipcodes
rest_us <- data %>%
  filter(State_Code %notin% treated_states |
    (State_Code %in% treated_states & distma > 3 * zone_cut) |
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
  filter(Year >= 2006 & Year <= 2014) %>%
  ggplot(aes(x = Year, y = Shops, color = factor(Zone))) +
  geom_line() +
  geom_vline(xintercept = 2012) +
  facet_wrap(~State_Code, scales = "free")


# -------------------------------------
# A.2.2. (SKIP) Sum of shops in zip code
# -------------------------------------


# }}}
#
# -------------------------------------

# -------------------------------------
#
# B.2.i. Zip Codes: Border v Inner
# One graph for *EACH* border state {{{
# -------------------------------------
#

# B.1.1. Average

zone_cut <- 100

tmp_data <- data %>%
    filter(State_Code %in% treated_states &
           State_Code != "MA") %>%
    group_by(State_Code, County, Year) %>%
    mutate(Treat_i  = if_else(min(distma, na.rm = T) <= zone_cut, 1, 0),
           Treat_yr = if_else(Year >= 2012, 1, 0),
           Total_shops_percap = 1000 * sum(n_total) / Population) %>%
    ungroup() %>%
    group_by(Treat_i, Year) %>%
    mutate(Total_shops_percap = mean(Total_shops_percap)) %>%
    ungroup() %>%
    select(Treat_i, Year, Treat_yr, Total_shops_percap) %>%
    distinct()

tmp_data %>%
    ggplot(aes(x = Year, y = Total_shops_percap, color = factor(Treat_i))) +
    geom_line() +
    geom_vline(xintercept = 2012)




qmp_data <- state_data %>%
    group_by(State_Code, County) %>%
    mutate(min_distma = min(distma, na.rm = T)) %>%
    mutate(Treat_i = if_else(distma <= zone_cut, 1, 0),
           Treat_yr = if_else(Year >= 2012, 1, 0)) %>%
    group_by(State_Code, County, Year) %>%
    mutate(Total_shops_1_4_percap = sum(n1_4) / Population,
           Total_shops_percap = sum(n_total) / Population,
           aver_income_percap = mean(Inc_percapita, na.rm = T)) %>%
    ungroup() %>%
    select(State_Code, County, Year,
           Treat_i, Treat_yr,
           Total_shops_1_4_percap,
           Total_shops_percap,
           State_Population,
           aver_income_percap, allmv_total_state, vehicle_miles_state) %>%
    distinct()


tmp_data %>%
    group_by(Treat_i, Year) %>%
    mutate(Total_shops_percap = 1000 * mean(Total_shops_percap, na.rm = T)) %>%
    ungroup() %>%
    select(State_Code, Treat_i, Treat_yr, Year,
           Total_shops_percap,
           aver_income_percap, allmv_total_state, vehicle_miles_state) %>%
    distinct() %>%
    ggplot(aes(x = Year, y = Total_shops_percap, color = factor(Treat_i))) +
    geom_line() +
    geom_vline(xintercept = 2012)

#
# }}}

# -------------------------------------


# -------------------------------------
#
# B.2.ii. Zip Codes: Border v Inner
# One graph for *ALL* border state {{{
#


# }}}
#
# -------------------------------------

# ========================================================
# A. Simple DiD regressions (before and after)
#
# A.0 Bordering States (treatment) vs Rest US (control)
# A.1 Border (treatment) vs Inner (control)
#       CT and NH
# A.2 Border (treatment) vs Rest US (control)
#       CT, NH, RI
#
#
# B. Event Study (dynamic did)
#
# B.0 Bordering States (treatment) vs Rest US (control)
# B.1 Border (treatment) vs Inner (control)
# B.2 Border (treatment) vs Rest US (control)
#
# ========================================================


# A.0 Simple DiD
reg_didsimple_nh_usa <- data %>%
    filter(
        State_Code %notin% treated_states[treated_states %notin% c("NH")]
           ) %>%
    mutate(Treat_i = if_else(State_Code == "NH", 1, 0),
           Treat_year = if_else(Year >= 2012, 1, 0)) %>%
    feols(n1_4 ~ (Treat_i * Treat_year) | zip + Year,
          data = .)

reg_didsimple_ct_usa <- data %>%
    filter(
        State_Code %notin% treated_states[treated_states %notin% c("CT")]
           ) %>%
    mutate(Treat_i = if_else(State_Code == "CT", 1, 0),
           Treat_year = if_else(Year >= 2012, 1, 0)) %>%
    feols(n1_4 ~ (Treat_i * Treat_year) | zip + Year,
          data = .)

reg_didsimple_ri_usa <- data %>%
    filter(
        State_Code %notin% treated_states[treated_states %notin% c("RI")]
           ) %>%
    mutate(Treat_i = if_else(State_Code == "RI", 1, 0),
           Treat_year = if_else(Year >= 2012, 1, 0)) %>%
    feols(n1_4 ~ (Treat_i * Treat_year) | zip + Year,
          data = .)

reg_didsimple_nh_usa1 <- data %>%
    filter(
        State_Code %notin% treated_states[treated_states %notin% c("NH")]
           ) %>%
    mutate(Treat_i = if_else(State_Code == "NH", 1, 0),
           Treat_year = if_else(Year >= 2012, 1, 0)) %>%
    feols(n1_4 ~ (Treat_i * Treat_year) | Treat_i + Year,
          data = .)

etable(reg_didsimple_nh_usa, reg_didsimple_ct_usa, reg_didsimple_ri_usa)
etable(reg_didsimple_nh_usa, reg_didsimple_nh_usa1)


# A.1 Simple DiD (Control Inner)
reg_didsimple_nh_inner <- data %>%
    filter(State_Code == "NH") %>%
    mutate(Zone_Band <= 2 * zone_cut) %>%
    mutate(Treat_i = if_else(Zone_Band <= zone_cut, 1, 0),
           Treat_Year = if_else(Year >= 2012, 1, 0)) %>%
    feols(n1_4 ~ (Treat_i * Treat_Year) | zip + Year,
           data = .)


reg_didsimple_ct_inner <- data %>%
    filter(State_Code == "CT") %>%
    mutate(Zone_Band <= 2 * zone_cut) %>%
    mutate(Treat_i = if_else(Zone_Band <= zone_cut, 1, 0),
           Treat_Year = if_else(Year >= 2012, 1, 0)) %>%
    feols(n1_4 ~ (Treat_i * Treat_Year) | zip + Year,
           data = .)

reg_didsimple_nh_usa <- data %>%
    filter(State_Code %notin% c("MA", "CT", "RI", "VT", "NY")) %>%
    mutate(Treat_i = if_else(State_Code == "NH", 1, 0),
           Treat_Year = if_else(Year >= 2012, 1, 0)) %>%
    feols(n1_4 ~ (Treat_i * Treat_Year) | zip + Year,
           data = .)

reg_didsimple_ct_usa <- data %>%
    filter(State_Code %notin% c("MA", "NH", "RI", "VT", "NY")) %>%
    mutate(Treat_i = if_else(State_Code == "CT", 1, 0),
           Treat_Year = if_else(Year >= 2012, 1, 0)) %>%
    feols(n1_4 ~ (Treat_i * Treat_Year) | zip + Year,
           data = .)

etable(reg_didsimple_nh_inner, reg_didsimple_ct_inner)
etable(reg_didsimple_nh_usa, reg_didsimple_ct_usa)








# B. Event Study (dynamic did)
#
# B.0 Bordering States (treatment) vs Rest US (control)
# B.1 Border (treatment) vs Inner (control)
# B.2 Border (treatment) vs Rest US (control)




data %>%
    filter(State_Code != "MA") %>%
    filter(State_Code %notin% c("MA", "RI", "CT", "NY")) %>%
    filter(!(State_Code == "NH" &
             (distma > zone_cut & distma < 3 * zone_cut))) %>%
    mutate(Treat_i = if_else(State_Code == "NH" & distma <= zone_cut, 1, 0),
           Year_event = Year - 2012
           ) %>%
    feols(n1_4 ~ i(Year_event, Treat_i, ref=c(-1, -6)) | zip + Year,
          data = .) %>%
    etable()

names(data)

print("DONE")




# DiD example
data(base_did)
feols(y ~ x1 + i(period, treat, 5) | id + period, base_did)
#
data(base_stagg)
head(base_stagg)
tabyl(base_stagg, time_to_treatment)
tabyl(base_stagg, ref(time_to_treatment))
tabyl(base_stagg, treated)

# 2 kind of estimations:
# - regular TWFE model
# - estimation with cohort x time_to_treatment interactions, later aggregated
# Note: the never treated have a time_to_treatment equal to -1000
# Now we perform the estimation
res_twfe = feols(y ~ x1 + i(time_to_treatment, treated, ref = c(-1, -1000)) | id + year, base_stagg)
# we use the "i." prefix to force year_treated to be considered as a factor
res_cohort = feols(y ~ x1 + i(time_to_treatment, i.year_treated, ref = c(-1, -1000)) | id + year, base_stagg)
etable(res_twfe)
# Displaying the results
iplot(res_twfe, ylim = c(-6, 8))
att_true = tapply(base_stagg$treatment_effect_true,
base_stagg$time_to_treatment, mean)[-1]
points(-9:8 + 0.15, att_true, pch = 15, col = 2)





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


# =========================================================================
#
#                   Synthetic Control (MicroSynth)
#
# I can exploit the ability of the microsynth library to perform analysis
# on all zipcodes, regardless of the data. Alternatively, I can do the analsis
# one state at a time.
# =========================================================================

data <- data %>%
    mutate(PostTreat = if_else(Year >= 2012, 1, 0)) %>%
    group_by(zip, PostTreat) %>%
    mutate(
       sc_Population = mean(Population),
       sc_Inc_percapita = mean(Inc_percapita),
       sc_allmv_total = mean(allmv_total),
       sc_vehicle_miles = mean(vehicle_miles)) %>%
    ungroup()


tabyl(data, PostTreat)

cov.var <- c("sc_Population", "sc_Inc_percapita",
             "sc_allmv_total", "sc_vehicle_miles")

match.out <- c("n_total", "n1_4", "n5_9", "n10_19")

sc_model <- microsynth(data,
                       idvar = "zip", timevar = "Year", intvar = "Treat",
                       start.pre = 2000, end.pre = 2011, end.post = 2016,
                       match.out = match.out, match.covar = cov.var, 
                       result.var = match.out, omnibus.var = match.out,
                       test = "lower",
                       perm = 250, jack = TRUE,
                       n.cores = min(parallel::detectCores(), 2))


