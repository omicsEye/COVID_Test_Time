# Initial / Default Parameters
n <- 25000
E0 <- 1/1000 * n # 0 #exposed, asymptomatic # JS: used 1/3 the DC prevalence
A0 <- 25 # infected, asymptomatic
U0 <- 0.025*n - A0 # uninfected, susceptible
M0 <- 0 # infected, symptomatic, isolated
TP0 <- 0 # infected, true positive, isolated
FP0 <- 0 # uninfected, false positive, isolated
UVAXP0 <- 3/25 * n # 0.8*n # uninfected, susceptible, vaccine protected
  # there are about 500 non-vaccinated + 2500 without boosters; consider these non-vaccinated
EVAXP0 <- 0 # exposed, asymptomatic, vaccine protected
AVAXP0 <- 0 # infected, asymptomatic, vaccine protected
TPVAXP0 <- 0 # infected, true positive, vaccine protected, isolated
FPVAXP0 <- 0 # uninfected, false positive, vaccine protected, isolated
UEVP0 <- 0.05*n # uninfected, susceptible, prior infection with earlier variants
EEVP0 <- 0 # exposed, asymptomatic, prior infection with earlier variants
AEVP0 <- 0 # infected, asymptomatic, prior infection with earlier variants
TPEVP0 <- 0 # infected, true positive, prior infection with earlier variants, isolated
FPEVP0 <- 0 # uninfected, false positive, prior infection with earlier variants, isolated
UCVP0 <-  20/1000 * n #0.125*n # uninfected, susceptible, prior infection with current variant (Omicron)
# We did not have that many infections; based on 2000/100K weekly peak rate in the DC community
ECVP0 <- 0 # exposed, asymptomatic, prior infection with current variant (Omicron)
ACVP0 <- 0 # infected, asymptomatic, prior infection with current variant (Omicron)
TPCVP0 <- 0 # infected, true positive, prior infection with current variant (Omicron), isolated
FPCVP0 <- 0 # uninfected, false positive, prior infection with current variant (Omicron), isolated

ncycles <- 120
theta <- 1.0/3.0
sigma <- 0.031
rho <- 0.071
Rstar <- 1.6 # 3 ### reproduction rate # is 1.6 more realistic with masks, etc?
beta <- Rstar*(sigma+rho)
delta <- 0.0000357
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

freqShock <- 7
Xshock <- 25 # JS: This is more realistic than 5 for 25,000
test_frequency <- 7.0
tau <- 1.0/test_frequency ### weekly testing
tau_VAXP <- 1.0/test_frequency
tau_EVP <- 1.0/test_frequency
tau_CVP <- 1.0/test_frequency
Se <- 0.99 # JS
Sp <- 0.9999 # JS
mu <- 1
source("model.R", local = TRUE)
results <- covidpred(U0, E0, A0, M0, TP0, FP0,
                     UVAXP0, EVAXP0, AVAXP0, TPVAXP0, FPVAXP0,
                     UEVP0, EEVP0, AEVP0, TPEVP0, FPEVP0,
                     UCVP0, ECVP0, ACVP0, TPCVP0, FPCVP0,
                     ncycles,beta,
                     epsilon_VAXt0, epsilon_VAXt6m, epsilon_VAXi0, epsilon_VAXi6m,
                     epsilon_EVt0, epsilon_EVt6m, epsilon_EVi0, epsilon_EVi6m,
                     epsilon_CVt0, epsilon_CVt6m, epsilon_CVi0, epsilon_CVi6m,
                     freqShock, Xshock,
                     tau, tau_VAXP, tau_EVP, tau_CVP, Se, Sp, mu,
                     theta, sigma, rho, delta)
results <- data.table(results)
#plot(results$newinf, results$TP)
