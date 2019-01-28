#############################################################
# Data generation and model validation Model 3
#############################################################

# Data-generating parameters are equal for set A and B
# Measurement error structure varies between set A and B
# Generate nsim datasets of n observations for each combination

for(i in 1:nrow(Scenarios)){
  
  currentScen <- Scenarios[i,]
  
  for(j in 1:nsim){
    currentSeedA <- SeedsM3[(i-1)*nsim+j,1]
    currentSeedB <- SeedsM3[(i-1)*nsim+j,2]
    
    # Set model parameters
    if(currentScen[1] != 0.9){#rho
      a <- log(2.3)
      b <- log(2.3)
    }
    if(currentScen[1] == 0.9){#rho
      a <- log(2.1)
      b <- log(2.1)
    }


    ### Derivation setting, denoted set A ###
    
    # Set seed
    set.seed(currentSeedA)
    
    # Generate data Set A
    SigmaA <- matrix(c(1,currentScen[1],currentScen[1],1),2,2)
    dA <- mvrnorm(n, rep(0,2),SigmaA)
    X1A <- dA[,1]
    X2A <- dA[,2]
    PA <- (1+exp(-(a*X1A+b*X2A)))^(-1)
    YA <- rbinom(n,1,PA)
  
    #Internal checks
    outcprevAM3[i,j] <- sum(YA)/n 
    Mcheck <- glm(YA~X1A + X2A, family=binomial())
    Pcheck <- predict(Mcheck,newdata=NULL, type="response")
    CcheckM3[i,j] <- somers2(Pcheck,YA)["C"]
    BrcheckM3[i,j]        <- mean((Pcheck-YA)^2)
    Brcheck_spreadM3[i,j] <- as.numeric(mean(Pcheck*(1-Pcheck)))
    Brcheck_calM3[i,j]    <- as.numeric(mean((YA-Pcheck)*(1-2*Pcheck)))
    
    # Check for degenerate distributions Set A  
    if(var(X1A)!=0 & var(X2A) != 0 & var(YA)!=0){ 
      
      # Measurement error
      etaA1 <- rnorm(n,0,currentScen[4]) #sigmaUA
      etaA2 <- rnorm(n,0,currentScen[4]) #sigmaUA
      W1A  <- currentScen[2] + currentScen[3] *X1A + etaA1 #psiA + thetaA*X1A+etaA1
      W2A  <- currentScen[2] + currentScen[3] *X2A + etaA2 #psiA + thetaA*X2A+etaA2
      
      # Prediction model in Set A, using W1A and W2A
      MA <- tryCatch.W.E(glm(YA~W1A+W2A, family=binomial()))
      if(MA$value$coefficients[1] >= sqrt(5000)|MA$value$coefficients[2] >= sqrt(5000)|MA$value$coefficients[3] >= sqrt(5000)) {countSepM3 <- countSepM3 + 1}
      MAwM3[i,j] <- class(MA$warning)
    
      # Internal model validation, using W1A and W2A
      LPA <- MA$value$coefficients[1] + MA$value$coefficients[2]*W1A + MA$value$coefficients[3]*W2A
      calMA <- tryCatch.W.E(glm(YA~LPA,family=binomial))
      calMwAM3[i,j] <- class(calMA$warning)
      predA <- (1+exp(-LPA))^(-1)
      
      # Storing output internal model validation
      avprobAM3[i,j]       <- mean(predA)
      interceptAM3[i,j]    <- as.numeric(calMA$value$coefficients[1])
      cal.slopeAM3[i,j]    <- as.numeric(calMA$value$coefficients[2])
      c.statAM3[i,j]       <- as.numeric(somers2(predA,YA)["C"])
      BrierAM3[i,j]        <- mean((predA-YA)^2)
      Brier_spreadAM3[i,j] <- as.numeric(mean(predA*(1-predA)))
      Brier_calAM3[i,j]    <- as.numeric(mean((YA-predA)*(1-2*predA)))
    
    
      ### Validation setting, denoted set B ###
      
      # Set seed
      set.seed(currentSeedB)
      
      # Generate data Set B
      SigmaB <- matrix(c(1,currentScen[1],currentScen[1],1),2,2)
      dB <- mvrnorm(n, rep(0,2),SigmaB)
      X1B <- dB[,1]
      X2B <- dB[,2]
      PB <- (1+exp(-(a*X1B+b*X2B)))^(-1)
      YB <- rbinom(n,1,PB)
      outcprevBM3[i,j] <- sum(YB)/n
      
      # Check for degenerate distributions Set B  
      if(var(X1B)!=0 & var(X2B) != 0 & var(YB)!=0){
      
        # Add measurement error to X1B, i.e. create W1B and W2B
        etaB1 <- rnorm(n,0,currentScen[7]) #sigmaUB
        etaB2 <- rnorm(n,0,currentScen[7]) #sigmaUB
        W1B <- currentScen[5] + currentScen[6] *X1B + etaB1 #psiB + thetaB*X1B+etaB
        W2B <- currentScen[5] + currentScen[6] *X2B + etaB2 #psiB + thetaB*X2B+etaB
 
        # External model validation, transporting model MA to data W1B
        LPB <- MA$value$coefficients[1] + MA$value$coefficients[2]*W1B + MA$value$coefficients[3]*W2B
        calMB <- tryCatch.W.E(glm(YB~LPB,family=binomial))
        predB <- (1+exp(-LPB))^(-1)
        calMwBM3[i,j] <- class(calMB$warning)
      
        # Storing output
        CalLmod              <- glm(YB~offset(LPB),family=binomial)
        calLBM3[i,j]         <- as.numeric(CalLmod$coefficients[1])
        calLBORM3[i,j]       <- (mean(predB)/(1-mean(predB)))/(mean(YB)/(1-mean(YB)))
        avprobBM3[i,j]       <- mean(predB)
        OEratioM3[i,j]       <- outcprevBM3[i,j]/avprobBM3[i,j]
        interceptBM3[i,j]    <- as.numeric(calMB$value$coefficients[1])
        cal.slopeBM3[i,j]    <- as.numeric(calMB$value$coefficients[2])
        c.statBM3[i,j]       <- as.numeric(somers2(predB,YB)["C"])
        BrierBM3[i,j]        <- mean((predB-YB)^2)
        Brier_spreadBM3[i,j] <- mean(predB*(1-predB))
        Brier_calBM3[i,j]    <- mean((YB-predB)*(1-2*predB))
      
    }}
  } 
  # Results
  Result <- list(outcprevAM3 = outcprevAM3[i,], outcprevBM3 = outcprevBM3[i,], CcheckM3 = CcheckM3[i,], BrcheckM3=BrcheckM3[i,], Brcheck_spreadM3=Brcheck_spreadM3[i,], Brcheck_calM3=Brcheck_calM3[i,], MAwM3 = MAwM3[i,], 
                 calMwAM3 =  calMwAM3[i,], avprobAM3 = avprobAM3[i,], interceptAM3 = interceptAM3[i,], cal.slopeAM3  = cal.slopeAM3[i,], 
                 c.statAM3 = c.statAM3[i,], BrierAM3 = BrierAM3[i,], Brier_spreadAM3=Brier_spreadAM3[i,], Brier_calAM3=Brier_calAM3[i,],
                 calMwBM3 = calMwBM3[i,], avprobBM3 = avprobBM3[i,],calLBM3 = calLBM3[i,], calLBORM3=calLBORM3[i,], OEratioM3=OEratioM3[i,], interceptBM3 = interceptBM3[i,], cal.slopeBM3  = cal.slopeBM3[i,],  
                 c.statBM3 = c.statBM3[i,], BrierBM3 = BrierBM3[i,], Brier_spreadBM3=Brier_spreadBM3[i,], Brier_calBM3=Brier_calBM3[i,])
  
  name <- paste(1,sprintf("%03d",i), sep="_")
  saveRDS(Result, file= file.path(newpath,paste("OutputM3/",name, ".rds", sep="")),compress ="xz")
  


}