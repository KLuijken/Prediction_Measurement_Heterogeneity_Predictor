#############################################################
# Generate storage objects
#############################################################
#Model 1
outcprevAM1 <- matrix(0,nrow(Scenarios1),nsim)
outcprevBM1 <- matrix(0,nrow(Scenarios1),nsim)
CcheckM1 <- matrix(0,nrow(Scenarios1),nsim)
BrcheckM1 <- matrix(0,nrow(Scenarios1),nsim)
Brcheck_spreadM1 <- matrix(0,nrow(Scenarios1),nsim)
Brcheck_calM1 <- matrix(0,nrow(Scenarios1),nsim)
MAwM1 <- matrix(0,nrow(Scenarios1),nsim)
countSepM1 <- 0
calMwAM1 <- matrix(0,nrow(Scenarios1),nsim)
avprobAM1 <- matrix(0,nrow(Scenarios1),nsim)
interceptAM1 <- matrix(0,nrow(Scenarios1),nsim)
cal.slopeAM1 <- matrix(0,nrow(Scenarios1),nsim)
c.statAM1 <- matrix(0,nrow(Scenarios1),nsim)
BrierAM1 <- matrix(0,nrow(Scenarios1),nsim)
Brier_spreadAM1 <- matrix(0,nrow(Scenarios1),nsim)
Brier_calAM1 <- matrix(0,nrow(Scenarios1),nsim)
calMwBM1 <- matrix(0,nrow(Scenarios1),nsim)
avprobBM1 <- matrix(0,nrow(Scenarios1),nsim)
calLBM1 <- matrix(0,nrow(Scenarios1),nsim)
calLBORM1 <- matrix(0,nrow(Scenarios1),nsim)
OEratioM1 <- matrix(0,nrow(Scenarios1),nsim)
interceptBM1 <- matrix(0,nrow(Scenarios1),nsim)
cal.slopeBM1 <- matrix(0,nrow(Scenarios1),nsim)
c.statBM1 <- matrix(0,nrow(Scenarios1),nsim)
BrierBM1 <- matrix(0,nrow(Scenarios1),nsim)
Brier_spreadBM1 <- matrix(0,nrow(Scenarios1),nsim)
Brier_calBM1 <- matrix(0,nrow(Scenarios1),nsim)

#Model 2
outcprevAM2 <- matrix(0,nrow(Scenarios),nsim)
outcprevBM2 <- matrix(0,nrow(Scenarios),nsim)
CcheckM2 <- matrix(0,nrow(Scenarios),nsim)
BrcheckM2 <- matrix(0,nrow(Scenarios),nsim)
Brcheck_spreadM2 <- matrix(0,nrow(Scenarios),nsim)
Brcheck_calM2 <- matrix(0,nrow(Scenarios),nsim)
MAwM2 <- matrix(0,nrow(Scenarios),nsim)
countSepM2 <- 0
calMwAM2 <- matrix(0,nrow(Scenarios),nsim)
avprobAM2 <- matrix(0,nrow(Scenarios),nsim)
interceptAM2 <- matrix(0,nrow(Scenarios),nsim)
cal.slopeAM2 <- matrix(0,nrow(Scenarios),nsim)
c.statAM2 <- matrix(0,nrow(Scenarios),nsim)
BrierAM2 <- matrix(0,nrow(Scenarios),nsim)
Brier_spreadAM2 <- matrix(0,nrow(Scenarios),nsim)
Brier_calAM2 <- matrix(0,nrow(Scenarios),nsim)
calMwBM2 <- matrix(0,nrow(Scenarios),nsim)
avprobBM2 <- matrix(0,nrow(Scenarios),nsim)
calLBM2 <- matrix(0,nrow(Scenarios),nsim)
calLBORM2 <- matrix(0,nrow(Scenarios),nsim)
OEratioM2 <- matrix(0,nrow(Scenarios),nsim)
interceptBM2 <- matrix(0,nrow(Scenarios),nsim)
cal.slopeBM2 <- matrix(0,nrow(Scenarios),nsim)
c.statBM2 <- matrix(0,nrow(Scenarios),nsim)
BrierBM2 <- matrix(0,nrow(Scenarios),nsim)
Brier_spreadBM2 <- matrix(0,nrow(Scenarios),nsim)
Brier_calBM2 <- matrix(0,nrow(Scenarios),nsim)


#Model 3
outcprevAM3 <- matrix(0,nrow(Scenarios),nsim)
outcprevBM3 <- matrix(0,nrow(Scenarios),nsim)
CcheckM3 <- matrix(0,nrow(Scenarios),nsim)
BrcheckM3 <- matrix(0,nrow(Scenarios),nsim)
Brcheck_spreadM3 <- matrix(0,nrow(Scenarios),nsim)
Brcheck_calM3 <- matrix(0,nrow(Scenarios),nsim)
MAwM3 <- matrix(0,nrow(Scenarios),nsim)
countSepM3 <- 0
calMwAM3 <- matrix(0,nrow(Scenarios),nsim)
avprobAM3 <- matrix(0,nrow(Scenarios),nsim)
interceptAM3 <- matrix(0,nrow(Scenarios),nsim)
cal.slopeAM3 <- matrix(0,nrow(Scenarios),nsim)
c.statAM3 <- matrix(0,nrow(Scenarios),nsim)
BrierAM3 <- matrix(0,nrow(Scenarios),nsim)
Brier_spreadAM3 <- matrix(0,nrow(Scenarios),nsim)
Brier_calAM3 <- matrix(0,nrow(Scenarios),nsim)
calMwBM3 <- matrix(0,nrow(Scenarios),nsim)
avprobBM3 <- matrix(0,nrow(Scenarios),nsim)
calLBM3 <- matrix(0,nrow(Scenarios),nsim)
calLBORM3 <- matrix(0,nrow(Scenarios),nsim)
OEratioM3 <- matrix(0,nrow(Scenarios),nsim)
interceptBM3 <- matrix(0,nrow(Scenarios),nsim)
cal.slopeBM3 <- matrix(0,nrow(Scenarios),nsim)
c.statBM3 <- matrix(0,nrow(Scenarios),nsim)
BrierBM3 <- matrix(0,nrow(Scenarios),nsim)
Brier_spreadBM3 <- matrix(0,nrow(Scenarios),nsim)
Brier_calBM3 <- matrix(0,nrow(Scenarios),nsim)


#Model 1, differential
outcprevAM1diff <- matrix(0,nrow(Scenariosdiff),nsim)
outcprevBM1diff <- matrix(0,nrow(Scenariosdiff),nsim)
CcheckM1diff <- matrix(0,nrow(Scenariosdiff),nsim)
BrcheckM1diff <- matrix(0,nrow(Scenariosdiff),nsim)
Brcheck_spreadM1diff <- matrix(0,nrow(Scenariosdiff),nsim)
Brcheck_calM1diff <- matrix(0,nrow(Scenariosdiff),nsim)
MAwM1diff <- matrix(0,nrow(Scenariosdiff),nsim)
countSepM1diff <- 0
calMwAM1diff <- matrix(0,nrow(Scenariosdiff),nsim)
avprobAM1diff <- matrix(0,nrow(Scenariosdiff),nsim)
interceptAM1diff <- matrix(0,nrow(Scenariosdiff),nsim)
cal.slopeAM1diff <- matrix(0,nrow(Scenariosdiff),nsim)
c.statAM1diff <- matrix(0,nrow(Scenariosdiff),nsim)
BrierAM1diff <- matrix(0,nrow(Scenariosdiff),nsim)
Brier_spreadAM1diff <- matrix(0,nrow(Scenariosdiff),nsim)
Brier_calAM1diff <- matrix(0,nrow(Scenariosdiff),nsim)
calMwBM1diff <- matrix(0,nrow(Scenariosdiff),nsim)
avprobBM1diff <- matrix(0,nrow(Scenariosdiff),nsim)
calLBM1diff <- matrix(0,nrow(Scenariosdiff),nsim)
calLBORM1diff <- matrix(0,nrow(Scenariosdiff),nsim)
OEratioM1diff <- matrix(0,nrow(Scenariosdiff),nsim)
interceptBM1diff <- matrix(0,nrow(Scenariosdiff),nsim)
cal.slopeBM1diff <- matrix(0,nrow(Scenariosdiff),nsim)
c.statBM1diff <- matrix(0,nrow(Scenariosdiff),nsim)
BrierBM1diff <- matrix(0,nrow(Scenariosdiff),nsim)
Brier_spreadBM1diff <- matrix(0,nrow(Scenariosdiff),nsim)
Brier_calBM1diff <- matrix(0,nrow(Scenariosdiff),nsim)

# For reading in output:
Sim.outp.names <- c("outcprevA",
                    "outcprevB",
                    "Ccheck",
                    "Brcheck",
                    "Brcheck_spread",
                    "Brcheck_cal",
                    "MAw",
                    "calMwA",
                    "avprobA",
                    "interceptA",
                    "cal.slopeA", 
                    "c.statA",
                    "BrierA",
                    "Brier_spreadA",
                    "Brier_calA",
                    "calMwB",
                    "avprobB",
                    "calLB",
                    "calLBOR",
                    "OEratio",
                    "interceptB",
                    "cal.slopeB",  
                    "c.statB",
                    "BrierB",
                    "Brier_spreadB",
                    "Brier_calB")


Sim.outp.namesM1 <- c("outcprevAM1",
                    "outcprevBM1",
                    "CcheckM1",
                    "BrcheckM1",
                    "Brcheck_spreadM1",
                    "Brcheck_calM1",
                    "MAwM1",
                    "calMwAM1",
                    "avprobAM1",
                    "interceptAM1",
                    "cal.slopeAM1", 
                    "c.statAM1",
                    "BrierAM1",
                    "Brier_spreadAM1",
                    "Brier_calAM1",
                    "calMwBM1",
                    "avprobBM1",
                    "calLBM1",
                    "calLBORM1",
                    "OEratioM1",
                    "interceptBM1",
                    "cal.slopeBM1",  
                    "c.statBM1",
                    "BrierBM1",
                    "Brier_spreadBM1",
                    "Brier_calBM1")

Sim.outp.namesM2 <- c("outcprevAM2",
                      "outcprevBM2",
                      "CcheckM2",
                      "BrcheckM2",
                      "Brcheck_spreadM2",
                      "Brcheck_calM2",
                      "MAwM2",
                      "calMwAM2",
                      "avprobAM2",
                      "interceptAM2",
                      "cal.slopeAM2", 
                      "c.statAM2",
                      "BrierAM2",
                      "Brier_spreadAM2",
                      "Brier_calAM2",
                      "calMwBM2",
                      "avprobBM2",
                      "calLBM2",
                      "calLBORM2",
                      "OEratioM2",
                      "interceptBM2",
                      "cal.slopeBM2",  
                      "c.statBM2",
                      "BrierBM2",
                      "Brier_spreadBM2",
                      "Brier_calBM2")

Sim.outp.namesM3 <- c("outcprevAM3",
                      "outcprevBM3",
                      "CcheckM3",
                      "BrcheckM3",
                      "Brcheck_spreadM3",
                      "Brcheck_calM3",
                      "MAwM3",
                      "calMwAM3",
                      "avprobAM3",
                      "interceptAM3",
                      "cal.slopeAM3", 
                      "c.statAM3",
                      "BrierAM3",
                      "Brier_spreadAM3",
                      "Brier_calAM3",
                      "calMwBM3",
                      "avprobBM3",
                      "calLBM3",
                      "calLBORM3",
                      "OEratioM3",
                      "interceptBM3",
                      "cal.slopeBM3",  
                      "c.statBM3",
                      "BrierBM3",
                      "Brier_spreadBM3",
                      "Brier_calBM3")

Sim.outp.namesM1diff <- c("outcprevAM1diff",
                      "outcprevBM1diff",
                      "CcheckM1diff",
                      "BrcheckM1diff",
                      "Brcheck_spreadM1diff",
                      "Brcheck_calM1diff",
                      "MAwM1diff",
                      "calMwAM1diff",
                      "avprobAM1diff",
                      "interceptAM1diff",
                      "cal.slopeAM1diff", 
                      "c.statAM1diff",
                      "BrierAM1diff",
                      "Brier_spreadAM1diff",
                      "Brier_calAM1diff",
                      "calMwBM1diff",
                      "avprobBM1diff",
                      "calLBM1diff",
                      "calLBORM1diff",
                      "OEratioM1diff",
                      "interceptBM1diff",
                      "cal.slopeBM1diff",  
                      "c.statBM1diff",
                      "BrierBM1diff",
                      "Brier_spreadBM1diff",
                      "Brier_calBM1diff")
