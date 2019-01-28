#############################################################
# Data generation and model validation M1, differential error
#############################################################

# Data-generating parameters are equal for set A and B
# Measurement error structure varies between set A and B
# Generate nsim datasets of n observations for each combination

for(i in 1:nrow(Scenariosdiff)){
  
  currentScen <- Scenariosdiff[i,]
  
  for(j in 1:nsim){
    currentSeedA <- SeedsM1diff[(i-1)*nsim+j,1]
    currentSeedB <- SeedsM1diff[(i-1)*nsim+j,2]

    # Set model parameters
    a <- log(4)
    
    ### Derivation setting, denoted set A ###
    
    # Set seed
    set.seed(currentSeedA)
    
    # Generate data Set A
    X1A<- rnorm(n,0,1)
    PA <- (1+exp(-(a*X1A)))^(-1)
    YA <- rbinom(n,1,PA)
    
    #Internal model checks
    outcprevAM1diff[i,j]      <- sum(YA)/n 
    Mcheck                    <- glm(YA ~ X1A, family=binomial())
    Pcheck                    <- plogis(predict(Mcheck))
    CcheckM1diff[i,j]         <- as.numeric(somers2(Pcheck,YA)["C"])
    BrcheckM1diff[i,j]        <- mean((Pcheck-YA)^2)
    Brcheck_spreadM1diff[i,j] <- as.numeric(mean(Pcheck*(1-Pcheck)))
    Brcheck_calM1diff[i,j]    <- as.numeric(mean((YA-Pcheck)*(1-2*Pcheck)))
    
    # Check for degenerate distributions Set A  
    if(var(X1A)!=0 & var(YA)!=0){ 
      
      # Add measurement error to X1A, i.e. create W1A
      etaA0 <- rnorm(n,0,currentScen[3]) #sigmaUA0
      etaA1 <- rnorm(n,0,currentScen[6]) #sigmaUA1
      W1A <- vector()
      for(m in 1:n){
        W1A[m] <- ((currentScen[1] + currentScen[2]*X1A[m]+etaA0[m])^(1-YA[m]))*((currentScen[4]+currentScen[5]*X1A[m] + etaA1[m])^(YA[m]))
        #((psiA0 + thetaA0*X1A[m]+etaA0[m])^(1-YA[m])) * ((psiA1 + thetaA1*X1A[m] + etaA1[m])^(YA[m]))
      }
      
      # Model in Set A
      MA <- tryCatch.W.E(glm(YA~W1A, family=binomial()))
      if(MA$value$coefficients[1] >= sqrt(5000)|MA$value$coefficients[2] >= sqrt(5000)) {countSepM1diff <- countSepM1diff + 1}
      MAwM1diff[i,j] <- class(MA$warning)
      
      # Prediction model in Set A, using W1A
      LPA <- MA$value$coefficients[1] + MA$value$coefficients[2]*W1A
      calMA <- tryCatch.W.E(glm(YA~LPA,family=binomial))
      predA <- (1+exp(-LPA))^(-1)
      calMwAM1diff[i,j] <- class(calMA$warning)
      
      
      # Storing output internal model validation
      avprobAM1diff[i,j]       <- mean(predA)
      interceptAM1diff[i,j]    <- as.numeric(calMA$value$coefficients[1])
      cal.slopeAM1diff[i,j]    <- as.numeric(calMA$value$coefficients[2])
      c.statAM1diff[i,j]       <- as.numeric(somers2(predA,YA)["C"])
      BrierAM1diff[i,j]        <- mean((predA-YA)^2)
      Brier_spreadAM1diff[i,j] <- as.numeric(mean(predA*(1-predA)))
      Brier_calAM1diff[i,j]    <- as.numeric(mean((YA-predA)*(1-2*predA)))
      
      ### Validation setting, denoted set B ###
      
      # Set seed
      set.seed(currentSeedB)
      
      # Generate data Set B
      X1B<- rnorm(n,0,1)
      PB <- (1+exp(-(a*X1B)))^(-1)
      YB <- rbinom(n,1,PB)
      outcprevBM1diff[i,j] <- sum(YB)/n
      
      # Check for degenerate distributions Set B  
      if(var(X1B)!=0 & var(YB)!=0){ 
        
        # Add measurement error to X1B, i.e. create W1B
        etaB0 <- rnorm(n,0,currentScen[11]) #sigmaUB0
        etaB1 <- rnorm(n,0,currentScen[12]) #sigmaUB1
        W1B <- vector()
        for(m in 1:n){
          W1B[m] <- ((currentScen[7] + currentScen[9]*X1B[m]+etaB0[m])^(1-YB[m]))*((currentScen[8]+currentScen[10]*X1B[m] + etaB1[m])^(YB[m]))
          #((psiB0 + thetaB0*X1B[m]+etaB0[m])^(1-YB[m])) * ((psiB1 + thetaB1*X1B[m] + etaB1[m])^(YB[m]))
        }
        
        #Model validation
        LPB <- MA$value$coefficients[1] + MA$value$coefficients[2]*W1B
        calMB <- tryCatch.W.E(glm(YB~LPB,family=binomial))
        predB <- (1+exp(-LPB))^(-1)
        calMwBM1diff[i,j] <- class(calMB$warning)
        
        # Storing output
        CalLmod                  <- glm(YB~offset(LPB),family=binomial)
        calLBM1diff[i,j]         <- as.numeric(CalLmod$coefficients[1])
        calLBORM1diff[i,j]       <- (mean(predB)/(1-mean(predB)))/(mean(YB)/(1-mean(YB)))
        avprobBM1diff[i,j]       <- mean(predB)
        OEratioM1diff[i,j]       <- outcprevBM1diff[i,j]/avprobBM1diff[i,j]
        interceptBM1diff[i,j]    <- as.numeric(calMB$value$coefficients[1])
        cal.slopeBM1diff[i,j]    <- as.numeric(calMB$value$coefficients[2])
        c.statBM1diff[i,j]       <- as.numeric(somers2(predB,YB)["C"])
        BrierBM1diff[i,j]        <- mean((predB-YB)^2)
        Brier_spreadBM1diff[i,j] <- as.numeric(mean(predB*(1-predB)))
        Brier_calBM1diff[i,j]    <- as.numeric(mean((YB-predB)*(1-2*predB)))

      }}
  }
  # Results
  Result <- list(outcprevAM1diff = outcprevAM1diff[i,], outcprevBM1diff = outcprevBM1diff[i,], CcheckM1diff = CcheckM1diff[i,], BrcheckM1diff=BrcheckM1diff[i,], Brcheck_spreadM1diff=Brcheck_spreadM1diff[i,], Brcheck_calM1diff=Brcheck_calM1diff[i,], MAwM1diff = MAwM1diff[i,], 
                 calMwAM1diff =  calMwAM1diff[i,], avprobAM1diff = avprobAM1diff[i,], interceptAM1diff = interceptAM1diff[i,], cal.slopeAM1diff  = cal.slopeAM1diff[i,], 
                 c.statAM1diff = c.statAM1diff[i,], BrierAM1diff = BrierAM1diff[i,], Brier_spreadAM1diff=Brier_spreadAM1diff[i,], Brier_calAM1diff=Brier_calAM1diff[i,],
                 calMwBM1diff = calMwBM1diff[i,], avprobBM1diff = avprobBM1diff[i,],calLBM1diff = calLBM1diff[i,], calLBORM1diff=calLBORM1diff[i,], OEratioM1diff=OEratioM1diff[i,], interceptBM1diff = interceptBM1diff[i,], cal.slopeBM1diff  = cal.slopeBM1diff[i,],  
                 c.statBM1diff = c.statBM1diff[i,], BrierBM1diff = BrierBM1diff[i,], Brier_spreadBM1diff=Brier_spreadBM1diff[i,], Brier_calBM1diff=Brier_calBM1diff[i,])
  
  name <- paste(1,sprintf("%03d",i), sep="_")
  saveRDS(Result, file= file.path(newpath,paste("OutputM1diff/",name, ".rds", sep="")),compress ="xz")
  
    
}






