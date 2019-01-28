#############################################################
# Generate and store seeds
#############################################################

Seeds <- matrix(0,((nsim*nrow(Scenarios1))+(2*nsim*nrow(Scenarios))),2)
Seeds[,1] <- floor(runif((nsim*nrow(Scenarios1))+(2*nsim*nrow(Scenarios)),1,1000000000))
Seeds[,2] <- floor(runif((nsim*nrow(Scenarios1))+(2*nsim*nrow(Scenarios)),1,1000000000))

SeedsM1     <- matrix(0,(nsim*nrow(Scenarios1)),2)
SeedsM1[,1] <- Seeds[1:(nsim*nrow(Scenarios1)),1]
SeedsM1[,2] <- Seeds[1:(nsim*nrow(Scenarios1)),2]

SeedsM2     <- matrix(0,(nsim*nrow(Scenarios)),2)
SeedsM2[,1] <- Seeds[(nsim*nrow(Scenarios1)+1):(nsim*nrow(Scenarios1) + nsim*nrow(Scenarios)),1]
SeedsM2[,2] <- Seeds[(nsim*nrow(Scenarios1)+1):(nsim*nrow(Scenarios1) + nsim*nrow(Scenarios)),2]

SeedsM3     <- matrix(0,(nsim*nrow(Scenarios)),2)
SeedsM3[,1] <- Seeds[(nsim*nrow(Scenarios1) + nsim*nrow(Scenarios)+1):(nsim*nrow(Scenarios1) + 2*nsim*nrow(Scenarios)),1]
SeedsM3[,2] <- Seeds[(nsim*nrow(Scenarios1) + nsim*nrow(Scenarios)+1):(nsim*nrow(Scenarios1) + 2*nsim*nrow(Scenarios)),2]

SeedsM1diff     <- matrix(0,(nsim*nrow(Scenariosdiff)),2)
SeedsM1diff[,1] <- floor(runif((nsim*nrow(Scenariosdiff)),1,1000000000))
SeedsM1diff[,2] <- floor(runif((nsim*nrow(Scenariosdiff)),1,1000000000))

# Save seeds
write.table(Seeds,file=file.path(paste(newpath,"/Seeds/Seeds.txt",sep="")))
saveRDS(SeedsM1,file=file.path(paste(newpath,"/Seeds/SeedsM1.rds",sep="")))
saveRDS(SeedsM2,file=file.path(paste(newpath,"/Seeds/SeedsM2.rds",sep="")))
saveRDS(SeedsM3,file=file.path(paste(newpath,"/Seeds/SeedsM3.rds",sep="")))
saveRDS(SeedsM1diff, file=file.path(paste(newpath,"/Seeds/SeedsM1diff.rds",sep="")))



