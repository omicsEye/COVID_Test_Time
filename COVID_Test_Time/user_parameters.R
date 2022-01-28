# Initial / Default Parameters
n <- 15000
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




n <- 25000
nUNP <- 0.025*n ### 2.5% unprotected
nVAXP <- 0.8*n
nEVP <- 0.05*n
nCVP <- 0.125*n
AUNP0 <- 25
AVAXP0 <- 0.05*nVAXP
AEVP0 <- 0.04*nEVP
ACVP0 <- 0

daystoincubation <- 3
daystorecovery <- 10
percenttosymptoms <- 0.3
fptouninfpool <- 1
percentfatality <- 0.0005
R0 <- 3
ncycles <- 120

epsilon_VAXi0 <- 0.5
epsilon_VAXi6m <- 0.2
epsilon_VAXt0 <- 0.1
epsilon_VAXt6m <- 0.1

epsilon_EVi0 <- 0.5
epsilon_EVi6m <- 0.2
epsilon_EVt0 <- 0.1
epsilon_EVt6m <- 0.1

epsilon_CVi0 <- 1
epsilon_CVi6m <- 1
epsilon_CVt0 <- 1
epsilon_CVt6m <- 1

freqShock <- 1
DynamicShock <- FALSE
Xshock <- 25

testfreq_UNP <- 7 ### weekly testing
testfreq_VAXP <- 7
testfreq_EVP <- 7
testfreq_CVP <- 7
Se <- 0.99
Sp <- 0.99

test <- covidpred(n, nUNP, nVAXP, nEVP, nCVP, AUNP0, AVAXP0, AEVP0, ACVP0,
                  ncycles, daystoincubation, daystorecovery, percenttosymptoms,
                  fptouninfpool, percentfatality, R0,
                  epsilon_VAXt0, epsilon_VAXt6m, epsilon_VAXi0, epsilon_VAXi6m,
                  epsilon_EVt0, epsilon_EVt6m, epsilon_EVi0, epsilon_EVi6m,
                  epsilon_CVt0, epsilon_CVt6m, epsilon_CVi0, epsilon_CVi6m,
                  freqShock, DynamicShock, Xshock, testfreq_UNP, testfreq_VAXP, testfreq_EVP, testfreq_CVP,
                  Se, Sp)


pdf("Covid_Pred_DynamicShock_Figure.pdf")
plot(c(1:121)-1, test$newinf,type="l", xlab="Days", ylab="Daily new infections")

plot(c(1:121)-1, test$cumnewinf,type="l", xlab="Days", ylab="Cumulative number of new infections")

### subjects in the isolation pool
plot(c(1:121)-1, test$totiso,type="l", xlab="Days", ylab="Daily Number of Isolations")
dev.off()
