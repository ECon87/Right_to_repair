# NOTE: To load data, you must download both the extract's data and the DDI
# and also set the working directory to the folder with these files (or change the path below).

if (!require("ipumsr")) stop("Reading IPUMS data into R requires the ipumsr package. It can be installed using the following command: install.packages('ipumsr')")

library(dplyr)
library(janitor)
library(skimr)

ipumsdir <- "/home/econ87/Research/Papers/Right_to_repair/Note/Data/Census/CPS/"

# ======================
# must set to ipum dir
# ======================

setwd(ipumsdir)


# ======================
# load files
# ======================

ddi <- read_ipums_ddi("cps_00001.xml")
data <- read_ipums_micro(ddi)



# ======================
# Aggregate over county
# ======================

summary(data$FTOTVAL)
summary(data$INCTOT)


df <- data %>%
    filter(is.na(COUNTY) == F & COUNTY != 0) %>%
    filter(INCTOT != 999999999 & INCTOT >= 0) %>%
    select(YEAR, COUNTY, INCTOT) %>%
    group_by(YEAR, COUNTY) %>%
    mutate(averinc_county = mean(INCTOT)) %>%
    ungroup() %>%
    select(YEAR, COUNTY, averinc_county) %>%
    distinct() %>%
    rename(Year = YEAR)


skim(df)

# ======================
# save
# ======================


write.csv(df, file = paste0(ipumsdir, "county_demos_1996_2019.csv"))
