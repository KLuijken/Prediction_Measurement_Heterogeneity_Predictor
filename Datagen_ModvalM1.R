#############################################################
# Data generation and model validation Model 1
#############################################################

# Data-generating parameters are equal for set A and B
# Measurement error structure varies between set A and B
# Generate nsim datasets of n observations for each combination

for(i in 1:nrow(Scenarios1)){
  
  currentScen <- Scenarios1[i,]
  
  for(j in 1:nsim){
  currentSeedA <- SeedsM1[(i-1)*nsim+j,1]
  currentSeedB <- SeedsM1[(i-1)*nsim+j,2]

    # Set model parameters
    a <- log(4)
      
    ### Derivation setting, denoted set A ###
    
    # Set seed
    set.seed(currentSeedA)
    
    # Generate data Set A
    X1A<- rnorm(n,0,1)
    PA <- 1/(1+exp(-(a*X1A)))
    YA <- rbinom(n,1,PA)
      
    #Internal model checks
    outcprevAM1[i,j]      <- sum(YA)/n 
    Mcheck                <- glm(YA ~ X1A, family=binomial())
    Pcheck                <- plogis(predict(Mcheck))
    CcheckM1[i,j]         <- as.numeric(somers2(Pcheck,YA)["C"])
    BrcheckM1[i,j]        <- mean((Pcheck-YA)^2)
    Brcheck_spreadM1[i,j] <- as.numeric(mean(Pcheck*(1-Pcheck)))
    Brcheck_calM1[i,j]    <- as.numeric(mean((YA-Pcheck)*(1-2*Pcheck)))
    
    # Check for degenerate distributions Set A  
    if(var(X1A)!=0 & var(YA)!=0){ 
        
      # Add measurement error to X1A, i.e. create W1A
      etaA <- rnorm(n,0,currentScen[3]) #sigmaUA
      W1A <- currentScen[1] + currentScen[2] *X1A + etaA #psiA + thetaA*X1A+etaA
        
      # Prediction model in Set A, using W1A
      MA <- tryCatch.W.E(glm(YA~W1A, family=binomial()))
      if(MA$value$coefficients[1] >= sqrt(5000)|MA$value$coefficients[2] >= sqrt(5000)) {countSepM1 <- countSepM1 + 1}
      MAwM1[i,j] <- class(MA$warning)
        
      # Internal model validation, using W1A
      LPA    <- MA$value$coefficients[1] + MA$value$coefficients[2]*W1A
      calMA  <- tryCatch.W.E(glm(YA~LPA,family=binomial))
      calMwAM1[i,j] <- class(calMA$warning)
      predA  <- plogis(LPA)
        
      # Storing output internal model validation
      avprobAM1[i,j]       <- mean(predA)
      interceptAM1[i,j]    <- as.numeric(calMA$value$coefficients[1])
      cal.slopeAM1[i,j]    <- as.numeric(calMA$value$coefficients[2])
      c.statAM1[i,j]       <- as.numeric(somers2(predA,YA)["C"])
      BrierAM1[i,j]        <- mean((predA-YA)^2)
      Brier_spreadAM1[i,j] <- as.numeric(mean(predA*(1-predA)))
      Brier_calAM1[i,j]    <- as.numeric(mean((YA-predA)*(1-2*predA)))
        
      ### Validation setting, denoted set B ###
        
      # Set seed
      set.seed(currentSeedB)
      
      # Generate data Set B
      X1B<- rnorm(n,0,1)
      PB <- (1+exp(-(a*X1B)))^(-1)
      YB <- rbinom(n,1,PB)
      outcprevBM1[i,j] <- sum(YB)/n
      
      # Check for degenerate distributions Set B  
      if(var(X1B)!=0 & var(YB)!=0){ 
          
        # Add measurement error to X1B, i.e. create W1B
        etaB <- rnorm(n,0,currentScen[6]) #sigmaUB
        W1B <- currentScen[4] + currentScen[5] *X1B + etaB #psiB + thetaB*X1B+etaB
          
        # External model validation, transporting model MA to data W1B
        LPB    <- MA$value$coefficients[1] + MA$value$coefficients[2]*W1B
        calMB  <- tryCatch.W.E(glm(YB~LPB,family=binomial))
        predB  <- plogis(LPB)
        calMwBM1[i,j] <- class(calMB$warning)
          
        # Storing output
        CalLmod              <- glm(YB~offset(LPB),family=binomial)
        calLBM1[i,j]         <- as.numeric(CalLmod$coefficients[1])
        calLBORM1[i,j]       <- (mean(predB)/(1-mean(predB)))/(mean(YB)/(1-mean(YB)))
        avprobBM1[i,j]       <- mean(predB)
        OEratioM1[i,j]       <- outcprevBM1[i,j]/avprobBM1[i,j]
        interceptBM1[i,j]    <- as.numeric(calMB$value$coefficients[1])
        cal.slopeBM1[i,j]    <- as.numeric(calMB$value$coefficients[2])
        c.statBM1[i,j]       <- as.numeric(somers2(predB,YB)["C"])
        BrierBM1[i,j]        <- mean((predB-YB)^2)
        Brier_spreadBM1[i,j] <- as.numeric(mean(predB*(1-predB)))
        Brier_calBM1[i,j]    <- as.numeric(mean((YB-predB)*(1-2*predB)))
        
      }}
  }
  
  
    # Results
      Result <- list(outcprevAM1 = outcprevAM1[i,], outcprevBM1 = outcprevBM1[i,], CcheckM1 = CcheckM1[i,], BrcheckM1=BrcheckM1[i,], Brcheck_spreadM1=Brcheck_spreadM1[i,], Brcheck_calM1=Brcheck_calM1[i,], MAwM1 = MAwM1[i,], 
                     calMwAM1 =  calMwAM1[i,], avprobAM1 = avprobAM1[i,], interceptAM1 = interceptAM1[i,], cal.slopeAM1  = cal.slopeAM1[i,], 
                     c.statAM1 = c.statAM1[i,], BrierAM1 = BrierAM1[i,], Brier_spreadAM1=Brier_spreadAM1[i,], Brier_calAM1=Brier_calAM1[i,],
                     calMwBM1 = calMwBM1[i,], avprobBM1 = avprobBM1[i,],calLBM1 = calLBM1[i,], calLBORM1=calLBORM1[i,], OEratioM1=OEratioM1[i,], interceptBM1 = interceptBM1[i,], cal.slopeBM1  = cal.slopeBM1[i,],  
                     c.statBM1 = c.statBM1[i,], BrierBM1 = BrierBM1[i,], Brier_spreadBM1=Brier_spreadBM1[i,], Brier_calBM1=Brier_calBM1[i,])
      
      name <- paste(1,sprintf("%03d",i), sep="_")
      saveRDS(Result, file= file.path(newpath,paste("OutputM1/",name, ".rds", sep="")),compress ="xz")

  
}





