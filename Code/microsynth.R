library(microsynth)
library(dplyr)
library(janitor)
library(knitr)
library(parallel)

set.seed(99199)

colnames(seattledmi)

# Declare covariates (time-invariant)
cov.var <- c("TotalPop", "BLACK", "HISPANIC", "Males_1521", "HOUSEHOLDS",
             "FAMILYHOUS", "FEMALE_HOU", "RENTER_HOU", "VACANT_HOU")

# Outcome variables (time-variant)
match.out <- c("i_felony", "i_misdemea", "i_drugs", "any_crime")


# =========================================
# Example 1: Barebones results
#
# What we do here:
# Calc weights to match treatment to synthetic control on vars specified!
# Calc a variance estimate based on the linearization method only,
# and based on that, run one-sided lower sign test (test=lower)!
# Calc an omnibus statistic to test joing sign on all outcome vars
# (result.var = match.out)!
# Create a plot and display it as output (plot_microsynth())!
# Estimate results but not save it to file (result.file=NULL)!


msynth_ex1 <- microsynth(seattledmi,
                        idvar = "ID",
                        timevar = "time",
                        intvar = "Intervention",
                        start.pre = 1, end.pre = 12, end.post = 16,
                        match.out = match.out,
                        match.covar = cov.var,
                        result.var = match.out,
                        omnibus.var = match.out,
                        test = "lower",
                        n.cores = min(parallel::detectCores(), 2))

# Call it
msynth_ex1

summary(msynth_ex1)

plot_microsynth(msynth_ex1, plot.var = "i_felony")



# =========================================
# Example 2: Adding permutations and jacknife
#
# In addition to using linearization to calculate a variance estimate,
# microsynth can approximate the estimator's sampling distribution by
# by generating permuted placebo groups.
# Default perm = 250 is somewhat computationly intensive.
# We will also generate jacknife replicationg groups, using as many
# groups as the lesser of the number of cases in the treatment group and the
# number of cases in the control group (jack = TRUE).

msynth_ex2 <- microsynth(seattledmi,
                         idvar = "ID",
                         timevar = "time",
                         intvar = "Intervention",
                         start.pre = 1, end.pre = 12, end.post = 16,
                         match.out = match.out,
                         match.covar = cov.var,
                         result.var = match.out,
                         omnibus.var = match.out,
                         test = "lower",
                         perm = 250, jack = TRUE,
                         n.cores = min(parallel::detectCores(), 2))

# Call it
msynth_ex2

summary(msynth_ex2)

plot_microsynth(msynth_ex2)
