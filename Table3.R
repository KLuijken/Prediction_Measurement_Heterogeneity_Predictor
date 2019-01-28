#########################################################
# Table 3
#########################################################

# Define rows
Scenarios1 <- as.data.frame(Scenarios1)
scen1 <- with(Scenarios1,{which(psiB == 0     &  thetaB== 0.5 &  sigmaUA< sigmaUB)})
scen2 <- with(Scenarios1,{which(psiB == 0     &  thetaB== 1.0 &  sigmaUA< sigmaUB)})
scen3 <- with(Scenarios1,{which(psiB == 0     &  thetaB== 2.0 &  sigmaUA< sigmaUB)})
scen4 <- with(Scenarios1,{which(psiB == 0.25  &  thetaB== 0.5 &  sigmaUA< sigmaUB)})
scen5 <- with(Scenarios1,{which(psiB == 0.25  &  thetaB== 1.0 &  sigmaUA< sigmaUB)})
scen6 <- with(Scenarios1,{which(psiB == 0.25  &  thetaB== 2.0 &  sigmaUA< sigmaUB)})
scen7  <- with(Scenarios1,{which(psiB == 0     &  thetaB== 0.5 &  sigmaUA == sigmaUB)})
scen8  <- with(Scenarios1,{which(psiB == 0     &  thetaB== 1.0 &  sigmaUA == sigmaUB)})
scen9  <- with(Scenarios1,{which(psiB == 0     &  thetaB== 2.0 &  sigmaUA == sigmaUB)})
scen10 <- with(Scenarios1,{which(psiB == 0.25  &  thetaB== 0.5 &  sigmaUA == sigmaUB)})
scen11  <- with(Scenarios1,{which(psiB == 0.25  &  thetaB== 1.0 &  sigmaUA == sigmaUB)})
scen12  <- with(Scenarios1,{which(psiB == 0.25  &  thetaB== 2.0 &  sigmaUA == sigmaUB)})
scen13  <- with(Scenarios1,{which(psiB == 0     &  thetaB== 0.5 &  sigmaUA> sigmaUB)})
scen14  <- with(Scenarios1,{which(psiB == 0     &  thetaB== 1.0 &  sigmaUA> sigmaUB)})
scen15  <- with(Scenarios1,{which(psiB == 0     &  thetaB== 2.0 &  sigmaUA> sigmaUB)})
scen16 <- with(Scenarios1,{which(psiB == 0.25  &  thetaB== 0.5 &  sigmaUA> sigmaUB)})
scen17 <- with(Scenarios1,{which(psiB == 0.25  &  thetaB== 1.0 &  sigmaUA> sigmaUB)})
scen18 <- with(Scenarios1,{which(psiB == 0.25  &  thetaB== 2.0 &  sigmaUA> sigmaUB)})


all <- list(scen1, scen2, scen3, scen4, scen5, scen6, scen7, scen8,scen9, scen10, scen11, scen12, scen13, scen14, scen15, scen16, scen17,scen18)

# Create table

T3 <- function(scenario){
  Table3 <- matrix(0,18,12)
  colnames(Table3) <- c("c-statistic A","sd", "c-statistic B","sd", "Calibration slope","sd","Cal-in-large","sd","Brier Score A","sd", "Brier Score B","sd")
  for(i in 1:18){
    Table3[i,1] <- mean(apply(c.statAM1[all[[i]],],1,mean))
    Table3[i,2] <- sd(apply(c.statAM1[all[[i]],],1,mean))
    Table3[i,3] <- mean(apply(c.statBM1[all[[i]],],1,mean))
    Table3[i,4] <- sd(apply(c.statBM1[all[[i]],],1,mean))
    Table3[i,5] <- mean(apply(cal.slopeBM1[all[[i]],],1,median))
    Table3[i,6] <- sd(apply(cal.slopeBM1[all[[i]],],1,median))
    Table3[i,7] <- mean(apply(calLBM1[all[[i]],],1,mean))
    Table3[i,8] <- sd(apply(calLBM1[all[[i]],],1,mean))
    Table3[i,9] <- mean(apply(BrierAM1[all[[i]],],1,mean))
    Table3[i,10] <- sd(apply(BrierAM1[all[[i]],],1,mean))
    Table3[i,11] <- mean(apply(BrierBM1[all[[i]],],1,mean))
    Table3[i,12] <- sd(apply(BrierBM1[all[[i]],],1,mean))
  }
  print(Table3)
}

print(xtable(T3(all), digits=4),file=file.path(newpath,"/OutputOverall","Table3.txt"), compress=F)

