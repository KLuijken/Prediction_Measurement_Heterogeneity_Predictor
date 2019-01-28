#############################################################
# Specification scenarios
#############################################################

rho       <- c(0,0.5,0.9)
psiA      <- 0
thetaA    <- 1
sigmaUA   <- c(0.5,1,2)
psiB      <- c(0,0.25)
thetaB    <- c(0.5,1,2)
sigmaUB   <- c(0.5,1,2)

Scenarios1 <- expand.grid(psiA=psiA,thetaA=thetaA,sigmaUA=sigmaUA, psiB=psiB, thetaB=thetaB, sigmaUB=sigmaUB)
Scenarios1 <- as.matrix(Scenarios1)
Scenarios <- expand.grid(rho=rho,psiA=psiA,thetaA=thetaA,sigmaUA=sigmaUA, psiB=psiB, thetaB=thetaB, sigmaUB=sigmaUB)
Scenarios <- as.matrix(Scenarios)
#------------------------------------------------------------
# Differential
#------------------------------------------------------------

psiA0      <- 0
thetaA0    <- 1
sigmaUA0   <- 1
psiA1      <- c(0,0.25)
thetaA1    <- c(0.5,1,2)
sigmaUA1   <- c(0.5,1,2)
psiB0      <- 0
thetaB0    <- 1
sigmaUB0   <- 1
psiB1      <- 0
thetaB1    <- 1
sigmaUB1   <- 1

ScenariosdiffA <- expand.grid(psiA0=psiA0,thetaA0=thetaA0,sigmaUA0=sigmaUA0,psiA1=psiA1,thetaA1=thetaA1,sigmaUA1=sigmaUA1, psiB0=psiB0, psiB1=psiB1, thetaB0=thetaB0, thetaB1=thetaB1, sigmaUB0=sigmaUB0, sigmaUB1=sigmaUB1)

psiA0      <- 0
thetaA0    <- 1
sigmaUA0   <- 1
psiA1      <- 0
thetaA1    <- 1
sigmaUA1   <- 1
psiB0      <- 0
thetaB0    <- 1
sigmaUB0   <- 1
psiB1      <- c(0,0.25)
thetaB1    <- c(0.5,1,2)
sigmaUB1   <- c(0.5,1,2)

ScenariosdiffB <- expand.grid(psiA0=psiA0,thetaA0=thetaA0,sigmaUA0=sigmaUA0,psiA1=psiA1,thetaA1=thetaA1,sigmaUA1=sigmaUA1, psiB0=psiB0, psiB1=psiB1, thetaB0=thetaB0, thetaB1=thetaB1, sigmaUB0=sigmaUB0, sigmaUB1=sigmaUB1)

Scenariosdiff <- as.matrix(rbind(ScenariosdiffA,ScenariosdiffB))
