n <- 25000
E0 <- 0
A0 <- 25
U0 <- 0.025*n - A0
M0 <- 0
TP0 <- 0
FP0 <- 0
UVAXP0 <- 0.8*n
EVAXP0 <- 0
AVAXP0 <- 0
TPVAXP0 <- 0
FPVAXP0 <- 0
UEVP0 <- 0.05*n
EEVP0 <- 0
AEVP0 <- 0
TPEVP0 <- 0
FPEVP0 <- 0
UCVP0 <- 0.125*n
ECVP0 <- 0
ACVP0 <- 0
TPCVP0 <- 0
FPCVP0 <- 0

ncycles <- 120
theta <- 1.0/3.0
sigma <- 0.031
rho <- 0.071
Rstar <- 3 ### reproduction rate
beta <- Rstar*(sigma+rho)
delta <- 0.0000357
epsilon_VAXi0 <- 0.85
epsilon_VAXi6m <- 0.66
epsilon_VAXt0 <- 0.25
epsilon_VAXt6m <- 0.25

epsilon_EVi0 <- 0.85
epsilon_EVi6m <- 0.66
epsilon_EVt0 <- 0.50
epsilon_EVt6m <- 0.25

epsilon_CVi0 <- 1
epsilon_CVi6m <- 1
epsilon_CVt0 <- 1
epsilon_CVt6m <- 1

freqShock <- 7
Xshock <- 5
test_frequency <- 7.0
tau <- 1.0/test_frequency ### weekly testing
tau_VAXP <- 1.0/test_frequency
tau_EVP <- 1.0/test_frequency
tau_CVP <- 1.0/test_frequency
Se <- 0.9
Sp <- 0.95
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
results <- as.data.frame(results)
#plot(results$newinf, results$TP)
