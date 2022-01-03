# Initial / Default Parameters
n <- 25000
nUNP <- 0.02  ### 2 % unprotected (previous U0)
# there are about 500 non-vaccinated + 2500 without boosters; consider these non-vaccinated
nVAXP <- (1-0.02-0.15-0.125) #0.8*n  # vaccine protected (previous UVAXP0)

nEVP <- 0.15 # prior infection with earlier variants (previous UEVP0)
nCVP <- 0.125  #0.125*n # prior infection with current variant (Omicron) (previous UCVP0)
# # We did not have that many infections; based on 2000/100K weekly peak rate in the DC community
AUNP0 <- 0.2 * nUNP # asymptomatic, unprotected (previous A0)
AVAXP0 <- 0.025 * nVAXP # asymptomatic, vaccine protected
AEVP0 <- 0.025 * nEVP # asynptomatic, prior infection with earlier variants
ACVP0 <- 0 # asymptomatic, prior infection with current variant (Omicron)

# E0 <- 1/1000 * n # 0 #exposed, asymptomatic # JS: used 1/3 the DC prevalence
# A0 <- 25 # infected, asymptomatic
# M0 <- 0 # infected, symptomatic, isolated
# TP0 <- 0 # infected, true positive, isolated
# FP0 <- 0 # uninfected, false positive, isolated
# UVAXP0 <- 3/25 * n # 0.8*n # uninfected, susceptible, vaccine protected
#   # there are about 500 non-vaccinated + 2500 without boosters; consider these non-vaccinated
# EVAXP0 <- 0 # exposed, asymptomatic, vaccine protected
# AVAXP0 <- 0 # infected, asymptomatic, vaccine protected
# TPVAXP0 <- 0 # infected, true positive, vaccine protected, isolated
# FPVAXP0 <- 0 # uninfected, false positive, vaccine protected, isolated
# UEVP0 <- 0.05*n # uninfected, susceptible, prior infection with earlier variants
# EEVP0 <- 0 # exposed, asymptomatic, prior infection with earlier variants
# AEVP0 <- 0 # infected, asymptomatic, prior infection with earlier variants
# TPEVP0 <- 0 # infected, true positive, prior infection with earlier variants, isolated
# FPEVP0 <- 0 # uninfected, false positive, prior infection with earlier variants, isolated
# UCVP0 <-  20/1000 * n #0.125*n # uninfected, susceptible, prior infection with current variant (Omicron)
# # We did not have that many infections; based on 2000/100K weekly peak rate in the DC community
# ECVP0 <- 0 # exposed, asymptomatic, prior infection with current variant (Omicron)
# ACVP0 <- 0 # infected, asymptomatic, prior infection with current variant (Omicron)
# TPCVP0 <- 0 # infected, true positive, prior infection with current variant (Omicron), isolated
# FPCVP0 <- 0 # uninfected, false positive, prior infection with current variant (Omicron), isolated


daystoincubation <- 3
daystorecovery <- 8
percenttosymptoms <- 0.3
fptouninfpool <- 1
percentfatality <- 0.0001
R0 <- 3.7   ### reproduction rate # is 1.6 more realistic with masks, etc?
ncycles <- 120

epsilon_VAXi0 <- 0.7 # 0.85 # assuming boosted (55-80 against symptomatic - reduced by 10% for actual infection)
epsilon_VAXi6m <- 0.4 # 0.66 # (taking the low range of booster; get true number from 10 weeks study)
epsilon_VAXt0 <- 0.5 # 0.25 (assuming another 20% reduction for transmissibility)
epsilon_VAXt6m <- 0.2 #0.25

epsilon_EVi0 <-  0.4 # 0.85 # protection for Omicron is rather low (20% for symptomatic at 6M)
epsilon_EVi6m <- 0.1 # 0.66
epsilon_EVt0 <- 0.2 # 0.25
epsilon_EVt6m <- 0.0 # 0.25

epsilon_CVi0 <- 1 
epsilon_CVi6m <- 0.6 # 1 # it may wane faster than 6M
epsilon_CVt0 <- 1
epsilon_CVt6m <- 0.6 # 1

freqShock <- 1
Xshock <- 0.005*n # JS: This is more realistic than 5 for 25,000
testfreq_UNP <- 7.0
testfreq_VAXP <- 14.0
testfreq_EVP <- 14.0
testfreq_CVP <- 14.0

Se <- 0.99 # JS
Sp <- 0.9999 # JS
mu <- 1
source("model.R", local = TRUE)
results <- covidpred(n, nUNP *n, nVAXP*n, nEVP*n, nCVP*n, AUNP0*n, AVAXP0*n, AEVP0*n, ACVP0*n, 
            ncycles, daystoincubation, daystorecovery, percenttosymptoms,
            fptouninfpool, percentfatality, R0, 
            epsilon_VAXt0, epsilon_VAXt6m, epsilon_VAXi0, epsilon_VAXi6m,
            epsilon_EVt0, epsilon_EVt6m, epsilon_EVi0, epsilon_EVi6m,
            epsilon_CVt0, epsilon_CVt6m, epsilon_CVi0, epsilon_CVi6m,
            freqShock, Xshock, testfreq_UNP, testfreq_VAXP, testfreq_EVP, testfreq_CVP,
            Se, Sp)
results <- data.table(results)
results[,plot(.I,newinf,type='l')]

#plot(results$newinf, results$TP)
