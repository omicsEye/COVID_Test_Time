#### COVID Prediction
####
####
####
#### Multiple (22) states
# UUNP: uninfected, susceptible, unprotected
# EUNP: exposed, asymptomatic, unprotected
# AUNP: infected, asymptomatic, unprotected
# M: infected, symptomatic, isolated
# TPUNP: infected, true positive, isolated, unprotected
# FPUNP: uninfected, false positive, isolated, unprotected

# UVAXP: uninfected, susceptible, vaccine protected
# EVAXP: exposed, asymptomatic, vaccine protected
# AVAXP: infected, asymptomatic, vaccine protected
# TPVAXP: infected, true positive, vaccine protected, isolated
# FPVAXP: uninfected, false positive, vaccine protected, isolated

# UEVP: uninfected, susceptible, prior infection with earlier variants
# EEVP: exposed, asymptomatic, prior infection with earlier variants
# AEVP: infected, asymptomatic, prior infection with earlier variants
# TPEVP: infected, true positive, prior infection with earlier variants, isolated
# FPEVP: uninfected, false positive, prior infection with earlier variants, isolated

# UCVP: uninfected, susceptible, prior infection with current variant (Omicron)
# ECVP: exposed, asymptomatic, prior infection with current variant (Omicron)
# ACVP: infected, asymptomatic, prior infection with current variant (Omicron)
# TPCVP: infected, true positive, prior infection with current variant (Omicron), isolated
# FPCVP: uninfected, false positive, prior infection with current variant (Omicron), isolated

# D: dead

#### Model Parameters
#
# beta: rate at which infected individuals contact and infect susceptible
#       individuals; applies to transmission to persons in states E and EP
# theta: incubation, the rates at which exposed individuals in states E and EP
#         advance to  asymptomatic, infectious compartments A and AP, respectively
# sigma: the symptom onset rate from states A and TP to state M
# rho: rate at which individuals in state i recover from disease
# delta: the symptom-case fatality rate for individuals in state M

# epsilon_VAXi0: antibody effectiveness in reducing susceptibility to infection
#                 at baseline, vaccine protected
# epsilon_VAXi6m: antibody effectiveness in reducing susceptibility to infection
#                 at 6 month, vaccine protected
# epsilon_VAXt0: antibody effectiveness in reducing transmission
#                 at baseline, vaccine protected
# epsilon_VAXt6m: antibody effectiveness in reducing transmission
#                 at 6 month, vaccine protected
# Exponential decay is assumed
# epsilon_VAXi(t) = epsilon_VAXi0 * exp(-kappa_VAXi * t)
# where kappa_VAXi = log(epsilon_VAXi0/epsilon_VAXi6m)/182.5

# epsilon_EVi0: antibody effectiveness in reducing susceptibility to infection
#                 at baseline, prior infection with earlier variants
# epsilon_EVi6m: antibody effectiveness in reducing susceptibility to infection
#                 at 6 month, prior infection with earlier variants
# epsilon_EVt0: antibody effectiveness in reducing transmission
#                 at baseline, prior infection with earlier variants
# epsilon_EVt6m: antibody effectiveness in reducing transmission
#                 at 6 month, prior infection with earlier variants
# Exponential decay is assumed
# epsilon_EVi(t) = epsilon_EVi0 * exp(-kappa_EVi * t)
# where kappa_EVi = log(epsilon_EVi0/epsilon_EVi6m)/182.5

# epsilon_CVi0: antibody effectiveness in reducing susceptibility to infection
#                 at baseline, prior infection with current variant
# epsilon_CVi6m: antibody effectiveness in reducing susceptibility to infection
#                 at 6 month, prior infection with current variant
# epsilon_CVt0: antibody effectiveness in reducing transmission
#                 at baseline, prior infection with current variant
# epsilon_CVt6m: antibody effectiveness in reducing transmission
#                 at 6 month, prior infection with current variant
# Exponential decay is assumed
# epsilon_CVi(t) = epsilon_CVi0 * exp(-kappa_CVi * t)
# where kappa_CVi = log(epsilon_EVi0/epsilon_CVi6m)/182.5

## a cycle time of one day

### Testing
## tau_UNP: constant rate for persons who are not antibody-protected
## tau_VAXp: constant rate for persons who are vaccine protected
## tau_EVp: constant rate for persons who had prior infection with earlier variants
## tau_CVp: constant rate for persons who had prior infection with current variant

## individuals are screened, at random, on average once every 1/tau cycles
## a lag of one cycle (one day) was introduced to account for the time between testing and
## receiving a positive test result
## Se: sensitivity of the screening test
## Sp: specificity of the screening test
## mu: rate at which false positives are returned from FP and FPP to U and UP

#### Imported infections
## I(t): an indicator function which assumes value 1 if one or more imported
###   infections are assumed to take place in cycle t and 0 otherwise
### freqShock: freuqncy of shock (days); for example, 7 days
### then I(t) = rep(rep(0,freqShock-1), 1)
### Xshock: number of new infections in a cycle where imported infections are assumed
### to take place (i.e., the magnitude of the shock) assuming that nobody is
### antibody-protected.

#### User Specified Parameters

### Input arguments:
### n = population size
### Population sizes of four groups (nUNP, nVAXP, nEVP, nCVP): 
###           (1) unprotected; (2) vaccine protected; 
###           (3) prior infection with earlier variants;
###           (4) prior infection with current variant (Omicron)
### Asymptomatic infections at time 0 in each group: AUNP0, AVAXP0, AEVP0, ACVP0

### freqShcok: Frequency of exogeneous shocks (days)
### Xshock: number of new infections in a cycle where imported infections are assumed to take
###   place (i.e., the magnitude of the shock), assuming that nobody is antibody-protected.
###   (This value is adjusted to account for antibody protection in the susceptible population.)

### R0: Reproduction number 

### Se: Sensitivity of COVID tests
### Sp: pecificity of COVID tests

### daystoincubation: Days to incubation (Exposed --> Asymptomatic)
### daystorecovery: Time to recovery (days)
### percenttosymptoms: %asymptomatics advancing to symptoms
### fptouninfpool: Return of FPs to the Uninfected pool (days)
### percentfatality: Symptoms case fatality ratio

### testfreq: Test frequencies (days) for each group (use 999999 or larger if no testing) 
### For each protected group, four parameters corresponding to 
###         (1) preventive efficacy at time 0; (epsilon_iVAXP0, epsilon_iEVP0, epsilon_iCVP0)
###         (2) preventive efficacy at 6 months; (epsilon_iVAXP6m, epsilon_iEVP6m, epsilon_iCVP6m)
###         (3) transmission efficacy at time 0; (epsilon_tVAXP0, epsilon_tEVP0, epsilon_tCVP0)
###         (4) transmission efficacy at 6 months (epsilon_tVAXP6m, epsilon_tEVP6m, epsilon_tCVP6m)

### ncycles: number of cycles (days)#### Equations

covidpred <- function(n, nUNP, nVAXP, nEVP, nCVP, AUNP0, AVAXP0, AEVP0, ACVP0, 
                      ncycles, daystoincubation, daystorecovery, percenttosymptoms,
                      fptouninfpool, percentfatality, R0, 
                      epsilon_VAXt0, epsilon_VAXt6m, epsilon_VAXi0, epsilon_VAXi6m,
                      epsilon_EVt0, epsilon_EVt6m, epsilon_EVi0, epsilon_EVi6m,
                      epsilon_CVt0, epsilon_CVt6m, epsilon_CVi0, epsilon_CVi6m,
                      freqShock, Xshock, testfreq_UNP, testfreq_VAXP, testfreq_EVP, testfreq_CVP,
                      Se, Sp)
{
  nUNP <- nUNP * n
  nVAXP <- nVAXP * n
  nEVP <- nEVP * n
  nCVP <- nCVP * n
  AUNP0 <- AUNP0 * n
  AVAXP0 <- AVAXP0 * n
  AEVP0 <- AEVP0 * n
  ACVP0 <- ACVP0 * n
  UUNP0 <- nUNP - AUNP0  # uninfected, susceptible, unprotected
  EUNP0 <- 0 # exposed, asymptomatic, unprotected
  M0 <- 0   # infected, symptomatic, isolated
  TPUNP0 <- 0 # infected, true positive, isolated, unprotected
  FPUNP0 <- 0 #  uninfected, false positive, isolated, unprotected
  
  UVAXP0 <- nVAXP - AVAXP0
  EVAXP0 <- 0
  TPVAXP0 <- 0
  FPVAXP0 <- 0
  
  UEVP0 <- nEVP - AEVP0
  EEVP0 <- 0
  TPEVP0 <- 0
  FPEVP0 <- 0
  
  UCVP0 <- nCVP - ACVP0
  ECVP0 <- 0
  TPCVP0 <- 0
  FPCVP0 <- 0
  D0 <- 0
  
  
  theta <- 1.0/daystoincubation
  rho <- 1.0/daystorecovery
  sigma <- rho*percenttosymptoms/(1 - percenttosymptoms)
  delta <- rho*percentfatality/(1-percentfatality)
  beta <- R0*(sigma+rho)
  mu <- 1.0/fptouninfpool 
  
  tau_UNP <- 1.0/testfreq_UNP
  tau_VAXP <- 1.0/testfreq_VAXP
  tau_EVP <- 1.0/testfreq_EVP
  tau_CVP <- 1.0/testfreq_CVP
  ### calculate decay parameters
  kappa_VAXt <- log(epsilon_VAXt0/epsilon_VAXt6m)/182.5
  kappa_VAXi <- log(epsilon_VAXi0/epsilon_VAXi6m)/182.5
  kappa_EVt <- log(epsilon_EVt0/epsilon_EVt6m)/182.5
  kappa_EVi <- log(epsilon_EVi0/epsilon_EVi6m)/182.5
  kappa_CVt <- log(epsilon_CVt0/epsilon_CVt6m)/182.5
  kappa_CVi <- log(epsilon_CVi0/epsilon_CVi6m)/182.5
  
  ### determine I(t)
  It <- c(0, rep(c(rep(0, freqShock-1),1),ncycles))
  It <- It[1:(ncycles+1)]   
  
  N <- rep(0, ncycles+1)
  totalN <- N
  UUNP <- N
  EUNP <- N
  AUNP <- N
  M <- N
  TPUNP <- N
  FPUNP <- N
  
  
  UVAXP <- N
  EVAXP <- N
  AVAXP <- N
  TPVAXP <- N
  FPVAXP <- N
  
  UEVP <- N
  EEVP <- N
  AEVP <- N
  TPEVP <- N
  FPEVP <- N
  
  UCVP <- N
  ECVP <- N
  ACVP <- N
  TPCVP <- N
  FPCVP <- N
  
  D <- N
  inisolation <- N
  ### Baseline
  UUNP[1] <- UUNP0
  EUNP[1] <- EUNP0
  AUNP[1] <- AUNP0
  M[1] <- M0
  TPUNP[1] <- TPUNP0
  FPUNP[1] <- FPUNP0
  
  UVAXP[1] <- UVAXP0
  EVAXP[1] <- EVAXP0
  AVAXP[1] <- AVAXP0
  TPVAXP[1] <- TPVAXP0
  FPVAXP[1] <- FPVAXP0
  
  UEVP[1] <- UEVP0
  EEVP[1] <- EEVP0
  AEVP[1] <- AEVP0
  TPEVP[1] <- TPEVP0
  FPEVP[1] <- FPEVP0
  
  UCVP[1] <- UCVP0
  ECVP[1] <- ECVP0
  ACVP[1] <- ACVP0
  TPCVP[1] <- TPCVP0
  FPCVP[1] <- FPCVP0
  
  D[1] <- D0
  
  ### non-isolated transmission pool at baseline
  N[1] <- UUNP[1] + EUNP[1] + AUNP[1] + 
    UVAXP[1] + EVAXP[1] + AVAXP[1] + 
    UEVP[1] + EEVP[1] + AEVP[1] + 
    UCVP[1] + ECVP[1] + ACVP[1]
  totalN[1] <- N[1] + M[1] + FPUNP[1] + TPUNP[1] + FPVAXP[1] + TPVAXP[1] + 
    FPEVP[1] + TPEVP[1] + FPCVP[1] + TPCVP[1] + D[1]
  
  ### transmissions to person in UUNP, UVAXP, UEVP, and UCVP
  ZUUNP <- N
  ZUVAXP <- N
  ZUEVP <- N
  ZUCVP <- N
  
  ### vaccination/antibody- adjusted magnitude of imported infections
  X <- N
  ### imported transmissions to persons in U, UVAXP, UEVP, UCVP
  YUUNP <- N
  YUVAXP <- N
  YUEVP <- N
  YUCVP <- N
  
  ### new infections 
  newinf <- N
  for (i in 1:ncycles)
  {
    ### vaccine/EV/CV effectiveness at time t
    epsilon_VAXi_t <- epsilon_VAXi0*exp(-kappa_VAXi*i)
    epsilon_VAXt_t <- epsilon_VAXt0*exp(-kappa_VAXt*i)
    
    epsilon_EVi_t <- epsilon_EVi0*exp(-kappa_EVi*i)
    epsilon_EVt_t <- epsilon_EVt0*exp(-kappa_EVt*i)
    
    epsilon_CVi_t <- epsilon_CVi0*exp(-kappa_CVi*i)
    epsilon_CVt_t <- epsilon_CVt0*exp(-kappa_CVt*i)
    
    
    
    ### transmissions to persons in U, UVAXP, UEVP, UCVP
    tmp <- AUNP[i] + (1- epsilon_VAXt_t)*AVAXP[i] +
      (1- epsilon_EVt_t)*AEVP[i] +
      (1- epsilon_CVt_t)*ACVP[i] 
    
    ZUUNP[i+1] <- beta*UUNP[i]*tmp/N[i]
    ZUVAXP[i+1] <- beta*(1- epsilon_VAXi_t)*UVAXP[i]*tmp/N[i]
    ZUEVP[i+1] <- beta*(1- epsilon_EVi_t)*UEVP[i]*tmp/N[i]
    ZUCVP[i+1] <- beta*(1- epsilon_CVi_t)*UCVP[i]*tmp/N[i]
    
    ### antibody-adjusted magnitude of imported infections
    
    sumUt <- UUNP[i] + UVAXP[i] + UEVP[i] + UCVP[i]
    sumUt_wt <- UUNP[i] + (1- epsilon_VAXi_t)*UVAXP[i] + 
      (1- epsilon_EVi_t)*UEVP[i] +
      (1- epsilon_CVi_t)*UCVP[i]
    
    X[i+1] <- It[i+1] * Xshock * sumUt_wt/sumUt
    
    ### imported transmissions to persons in U, UVAXP, UEVP, UCVP 
    YUUNP[i+1] <- X[i+1]*UUNP[i]/sumUt_wt
    YUVAXP[i+1] <- X[i+1]*(1- epsilon_VAXi_t)*UVAXP[i]/sumUt_wt
    YUEVP[i+1] <- X[i+1]*(1- epsilon_EVi_t)*UEVP[i]/sumUt_wt
    YUCVP[i+1] <- X[i+1]*(1- epsilon_CVi_t)*UCVP[i]/sumUt_wt
    
    ### Uninfected
    if (i == 1)
    {
      UUNP[i+1] <- UUNP[i] - 0*tau_UNP*(1-Sp) - ZUUNP[i+1] - YUUNP[i+1] + mu*FPUNP[i]  
    }
    else 
    {
      UUNP[i+1] <- UUNP[i] - UUNP[i-1]*tau_UNP*(1-Sp) - ZUUNP[i+1] - YUUNP[i+1] + mu*FPUNP[i]  
    }
    UUNP[i+1] <- max(UUNP[i+1], 0) ### U cannot be below 0
    ### Exposed
    EUNP[i+1] <- EUNP[i]*(1-theta) + ZUUNP[i+1] + YUUNP[i+1]
    ### Asymptomatic
    if (i == 1)
      AUNP[i+1] <- AUNP[i]*(1 - sigma - rho) - 0*tau_UNP*Se + EUNP[i]*theta # why is there a term with 0 ?
    else
      AUNP[i+1] <- AUNP[i]*(1 - sigma - rho) - AUNP[i-1]*tau_UNP*Se + EUNP[i]*theta
    ### Symptomatic
    ### Paltiel and Schwartz didn't include AP--> M or TPP -->M
    M[i+1] <- M[i]*(1 - rho - delta) + sigma*(AUNP[i] + TPUNP[i] + 
                                                AVAXP[i] + TPVAXP[i] +
                                                AEVP[i] + TPEVP[i] +
                                                ACVP[i] + TPCVP[i])
    ### false positives    
    if (i == 1)
      FPUNP[i+1] <- FPUNP[i]*(1-mu) + 0*tau_UNP*(1-Sp) # why is there a term with 0 ?
    else
      FPUNP[i+1] <- FPUNP[i]*(1-mu) + UUNP[i-1]*tau_UNP*(1-Sp)
    ### true positives
    if (i == 1)
      TPUNP[i+1] <- TPUNP[i]*(1 - sigma - rho) + 0*tau_UNP*Se # why is there a term with 0 ?
    else 
      TPUNP[i+1] <- TPUNP[i]*(1 - sigma - rho) + AUNP[i-1]*tau_UNP*Se
    
    ### uninfected UVAXP, UEVP, UCVP
    ### recoveries will be added to UCVP
    if (i == 1)
    {
      UVAXP[i+1] <- UVAXP[i] - 0*tau_VAXP*(1-Sp) - ZUVAXP[i+1] - YUVAXP[i+1] + # why is there a term with 0 ?
        mu*FPVAXP[i] ##
      UEVP[i+1] <- UEVP[i] - 0*tau_EVP*(1-Sp) - ZUEVP[i+1] - YUEVP[i+1] + # why is there a term with 0 ?
        mu*FPEVP[i]
      UCVP[i+1] <- UCVP[i] - 0*tau_CVP*(1-Sp) - ZUCVP[i+1] - YUCVP[i+1] + # why is there a term with 0 ?
        mu*FPCVP[i] + rho*(AUNP[i]+M[i]+TPUNP[i]+AVAXP[i]+TPVAXP[i]+AEVP[i]+TPEVP[i] +
                             ACVP[i]+TPCVP[i])
    }
    else 
    {
      UVAXP[i+1] <- UVAXP[i] - UVAXP[i-1]*tau_VAXP*(1-Sp) - ZUVAXP[i+1] - YUVAXP[i+1] + 
        mu*FPVAXP[i]
      UEVP[i+1] <- UEVP[i] - UEVP[i-1]*tau_EVP*(1-Sp) - ZUEVP[i+1] - YUEVP[i+1] + 
        mu*FPEVP[i]
      UCVP[i+1] <- UCVP[i] - UCVP[i-1]*tau_CVP*(1-Sp) - ZUCVP[i+1] - YUCVP[i+1] + 
        mu*FPCVP[i] + rho*(AUNP[i]+M[i]+TPUNP[i]+AVAXP[i]+TPVAXP[i]+AEVP[i]+TPEVP[i] +
                             ACVP[i]+TPCVP[i])
    }
    ### Exposed
    EVAXP[i+1] <- EVAXP[i]*(1-theta) + ZUVAXP[i+1] + YUVAXP[i+1]
    EEVP[i+1] <- EEVP[i]*(1-theta) + ZUEVP[i+1] + YUEVP[i+1]
    ECVP[i+1] <- ECVP[i]*(1-theta) + ZUCVP[i+1] + YUCVP[i+1]
    
    ### Asymptomatic
    if (i == 1)
    {
      AVAXP[i+1] <- AVAXP[i]*(1-sigma-rho) - 0*tau_VAXP*Se + EVAXP[i]*theta # why is there a term with 0 ?
      AEVP[i+1] <- AEVP[i]*(1-sigma-rho) - 0*tau_EVP*Se + EEVP[i]*theta # why is there a term with 0 ?
      ACVP[i+1] <- ACVP[i]*(1-sigma-rho) - 0*tau_CVP*Se + ECVP[i]*theta # why is there a term with 0 ?
    }
    else 
    {
      AVAXP[i+1] <- AVAXP[i]*(1-sigma-rho) - AVAXP[i-1]*tau_VAXP*Se + EVAXP[i]*theta
      AEVP[i+1] <- AEVP[i]*(1-sigma-rho) - AEVP[i-1]*tau_EVP*Se + EEVP[i]*theta
      ACVP[i+1] <- ACVP[i]*(1-sigma-rho) - ACVP[i-1]*tau_CVP*Se + ECVP[i]*theta
    }
    
    ### False positives
    if (i==1)
    {
      FPVAXP[i+1] <- FPVAXP[i]*(1-mu) + 0*tau_VAXP*(1-Sp) # why is there a term with 0 ?
      FPEVP[i+1] <- FPEVP[i]*(1-mu) + 0*tau_EVP*(1-Sp) # why is there a term with 0 ?
      FPCVP[i+1] <- FPCVP[i]*(1-mu) + 0*tau_CVP*(1-Sp) # why is there a term with 0 ?
    }
    else
    {
      FPVAXP[i+1] <- FPVAXP[i]*(1-mu) + UVAXP[i-1]*tau_VAXP*(1-Sp)
      FPEVP[i+1] <- FPEVP[i]*(1-mu) + UEVP[i-1]*tau_EVP*(1-Sp)
      FPCVP[i+1] <- FPCVP[i]*(1-mu) + UCVP[i-1]*tau_CVP*(1-Sp)
    }
    
    ### True positives
    if (i == 1)
    {
      TPVAXP[i+1] <- TPVAXP[i]*(1-sigma-rho) + 0*tau_VAXP*Se # why is there a term with 0 ?
      TPEVP[i+1] <- TPEVP[i]*(1-sigma-rho) + 0*tau_EVP*Se # why is there a term with 0 ?
      TPCVP[i+1] <- TPCVP[i]*(1-sigma-rho) + 0*tau_CVP*Se # why is there a term with 0 ?
    }
    else
    {
      TPVAXP[i+1] <- TPVAXP[i]*(1-sigma-rho) + AVAXP[i-1]*tau_VAXP*Se
      TPEVP[i+1] <- TPEVP[i]*(1-sigma-rho) + AEVP[i-1]*tau_EVP*Se
      TPCVP[i+1] <- TPCVP[i]*(1-sigma-rho) + ACVP[i-1]*tau_CVP*Se
    }
    
    D[i+1] <- D[i] + delta*M[i]    
    
    N[i+1] <- UUNP[i+1] + EUNP[i+1] + AUNP[i+1] + 
      UVAXP[i+1] + EVAXP[i+1] + AVAXP[i+1] +
      UEVP[i+1] + EEVP[i+1] + AEVP[i+1] +
      UCVP[i+1] + ECVP[i+1] + ACVP[i+1] 
    
    totalN[i+1] <- N[i+1] + M[i+1] + FPUNP[i+1] + TPUNP[i+1] + FPVAXP[i+1] + TPVAXP[i+1] + 
      FPEVP[i+1] + TPEVP[i+1] + FPCVP[i+1] + TPCVP[i+1] + D[i+1]
    
  }
  
  newinf <- (EUNP + EVAXP + EEVP + ECVP)*theta
  cumnewinf <- cumsum(EUNP + EVAXP + EEVP + ECVP)*theta
  r <- data.table(cbind(N, UUNP, EUNP, AUNP, M, FPUNP, TPUNP, UVAXP, EVAXP, AVAXP, FPVAXP, TPVAXP, 
        UEVP, EEVP, AEVP, FPEVP, TPEVP, UCVP, ECVP, ACVP, FPCVP, TPCVP, D, newinf, cumnewinf,totalN))
  r[,inisolation:=mapply(sum, M ,FPVAXP , TPVAXP , TPCVP , TPEVP , FPEVP , FPCVP , TPUNP , FPUNP)]
  return(r)
}

