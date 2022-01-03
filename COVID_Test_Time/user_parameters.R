
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

freqShock <- 7
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
                  freqShock, Xshock, testfreq_UNP, testfreq_VAXP, testfreq_EVP, testfreq_CVP,
                  Se, Sp)

test <- data.frame(test)

par(mfrow=c(2,1))
plot(c(1:121)-1, test$newinf,type="l", xlab="Days", ylab="Daily new infections")

plot(c(1:121)-1, test$cumnewinf,type="l", xlab="Days", ylab="Cumulative number of new infections")
