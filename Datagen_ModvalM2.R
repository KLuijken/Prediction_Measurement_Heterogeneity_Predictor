#############################################################
# Data generation and model validation Model 2
#############################################################

# Data-generating parameters are equal for set A and B
# Measurement error structure varies between set A and B
# Generate nsim datasets of n observations for each combination

for(i in 1:nrow(Scenarios)){
  
  currentScen <- Scenarios[i,]
  
  for(j in 1:nsim){
    currentSeedA <- SeedsM2[(i-1)*nsim+j,1]
    currentSeedB <- SeedsM2[(i-1)*nsim+j,2]
    
    # Set model parameters
    if(currentScen[1] != 0.9){ #rho
    a <- log(2.3)
    b <- log(2.3)
    }
    if(currentScen[1] == 0.9){ #rho
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
    outcprevAM2[i,j] <- sum(YA)/n 
    Mcheck <- glm(YA ~ X1A+X2A, family=binomial())
    Pcheck <- predict(Mcheck,newdata=NULL, type="response")
    CcheckM2[i,j] <- somers2(Pcheck,YA)["C"]
    BrcheckM2[i,j]        <- mean((Pcheck-YA)^2)
    Brcheck_spreadM2[i,j] <- as.numeric(mean(Pcheck*(1-Pcheck)))
    Brcheck_calM2[i,j]    <- as.numeric(mean((YA-Pcheck)*(1-2*Pcheck)))
    
    # Check for degenerate distributions Set A  
    if(var(X1A)!=0 & var(X2A) != 0 & var(YA)!=0){ 
    
      # Measurement error
      etaA <- rnorm(n,0,currentScen[4]) #sigmaUA
      W1A <- currentScen[2] + currentScen[3] *X1A + etaA #psiA + thetaA*X1A+etaA
    
      # Prediction model in Set A, using W1A
      MA <- tryCatch.W.E(glm(YA~W1A+X2A, family=binomial()))
      if(MA$value$coefficients[1] >= sqrt(5000)|MA$value$coefficients[2] >= sqrt(5000)|MA$value$coefficients[3] >= sqrt(5000)) {countSepM2 <- countSepM2 + 1}
      MAwM2[i,j] <- class(MA$warning)
    
      # Internal model validation, using W1A
      LPA <- MA$value$coefficients[1] + MA$value$coefficients[2]*W1A + MA$value$coefficients[3]*X2A
      calMA <- tryCatch.W.E(glm(YA~LPA,family=binomial))
      calMwAM2[i,j] <- class(calMA$warning)
      predA <- (1+exp(-LPA))^(-1)
    
      # Storing output internal model validation
      avprobAM2[i,j]       <- mean(predA)
      interceptAM2[i,j]    <- as.numeric(calMA$value$coefficients[1])
      cal.slopeAM2[i,j]    <- as.numeric(calMA$value$coefficients[2])
      c.statAM2[i,j]       <- as.numeric(somers2(predA,YA)["C"])
      BrierAM2[i,j]        <- mean((predA-YA)^2)
      Brier_spreadAM2[i,j] <- as.numeric(mean(predA*(1-predA)))
      Brier_calAM2[i,j]    <- as.numeric(mean((YA-predA)*(1-2*predA)))
    
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
      outcprevBM2[i,j] <- sum(YB)/n
      
      # Check for degenerate distributions Set B  
      if(var(X1B)!=0 & var(X2B) != 0 & var(YB)!=0){ 
      
        # Add measurement error to X1B, i.e. create W1B
        etaB <- rnorm(n,0,currentScen[7]) #sigmaUB
        W1B <- currentScen[5] + currentScen[6] *X1B + etaB #psiB + thetaB*X1B+etaB
      
        # External model validation, transporting model MA to data W1B
        LPB <- MA$value$coefficients[1] + MA$value$coefficients[2]*W1B + MA$value$coefficients[3]*X2B
        calMB <- tryCatch.W.E(glm(YB~LPB,family=binomial))
        predB <- (1+exp(-LPB))^(-1)
        calMwBM2[i,j] <- class(calMB$warning)
      
        # Storing output
        CalLmod              <- glm(YB~offset(LPB),family=binomial)
        calLBM2[i,j]         <- as.numeric(CalLmod$coefficients[1])
        calLBORM2[i,j]       <- (mean(predB)/(1-mean(predB)))/(mean(YB)/(1-mean(YB)))
        avprobBM2[i,j]       <- mean(predB)
        OEratioM2[i,j]       <- outcprevBM2[i,j]/avprobBM2[i,j]
        interceptBM2[i,j]    <- as.numeric(calMB$value$coefficients[1])
        cal.slopeBM2[i,j]    <- as.numeric(calMB$value$coefficients[2])
        c.statBM2[i,j]       <- as.numeric(somers2(predB,YB)["C"])
        BrierBM2[i,j]        <- mean((predB-YB)^2)
        Brier_spreadBM2[i,j] <- mean(predB*(1-predB))
        Brier_calBM2[i,j]    <- mean((YB-predB)*(1-2*predB))
      
        }}
    }

    # Results
    Result <- list(outcprevAM2 = outcprevAM2[i,], outcprevBM2 = outcprevBM2[i,], CcheckM2 = CcheckM2[i,], BrcheckM2=BrcheckM2[i,], Brcheck_spreadM2=Brcheck_spreadM2[i,], Brcheck_calM2=Brcheck_calM2[i,], MAwM2 = MAwM2[i,], 
                   calMwAM2 =  calMwAM2[i,], avprobAM2 = avprobAM2[i,], interceptAM2 = interceptAM2[i,], cal.slopeAM2  = cal.slopeAM2[i,], 
                   c.statAM2 = c.statAM2[i,], BrierAM2 = BrierAM2[i,], Brier_spreadAM2=Brier_spreadAM2[i,], Brier_calAM2=Brier_calAM2[i,],
                   calMwBM2 = calMwBM2[i,], avprobBM2 = avprobBM2[i,],calLBM2 = calLBM2[i,], calLBORM2=calLBORM2[i,], OEratioM2=OEratioM2[i,], interceptBM2 = interceptBM2[i,], cal.slopeBM2  = cal.slopeBM2[i,],  
                   c.statBM2 = c.statBM2[i,], BrierBM2 = BrierBM2[i,], Brier_spreadBM2=Brier_spreadBM2[i,], Brier_calBM2=Brier_calBM2[i,])
    
    name <- paste(1,sprintf("%03d",i), sep="_")
    saveRDS(Result, file= file.path(newpath,paste("OutputM2/",name, ".rds", sep="")),compress ="xz")
    
  
  
}





