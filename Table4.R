#########################################################
# Table 4
#########################################################

# Select relevant scenarios
selection1 <- c(1,19)
selection2 <- c(17,35)


# Create table

Tdiff <- function(selection){
  Tablediff <- matrix(0,3,8)
  colnames(Tablediff) <- c("Set A differential","sd", "Set B","sd", "Set A","sd", "Set B differential","sd")
  rownames(Tablediff) <- c("Brier score","calbration slope", "c-statistic")
  Tablediff[1,1] <- mean(BrierAM1diff[selection[1],])
  Tablediff[1,2] <- sd(BrierAM1diff[selection[1],])
  Tablediff[1,3] <- mean(BrierBM1diff[selection[1],])
  Tablediff[1,4] <- sd(BrierBM1diff[selection[1],])
  Tablediff[2,1] <- median(cal.slopeAM1diff[selection[1],])
  Tablediff[2,2] <- sd(cal.slopeAM1diff[selection[1],])
  Tablediff[2,3] <- median(cal.slopeBM1diff[selection[1],])
  Tablediff[2,4] <- sd(cal.slopeBM1diff[selection[1],])
  Tablediff[3,1] <- mean(c.statAM1diff[selection[1],])
  Tablediff[3,2] <- sd(c.statAM1diff[selection[1],])
  Tablediff[3,3] <- mean(c.statBM1diff[selection[1],])
  Tablediff[3,4] <- sd(c.statBM1diff[selection[1],])
  Tablediff[1,5] <- mean(BrierAM1diff[selection[2],])
  Tablediff[1,6] <- sd(BrierAM1diff[selection[2],])
  Tablediff[1,7] <- mean(BrierBM1diff[selection[2],])
  Tablediff[1,8] <- sd(BrierBM1diff[selection[2],])
  Tablediff[2,5] <- median(cal.slopeAM1diff[selection[2],])
  Tablediff[2,6] <- sd(cal.slopeAM1diff[selection[2],])
  Tablediff[2,7] <- median(cal.slopeBM1diff[selection[2],])
  Tablediff[2,8] <- sd(cal.slopeBM1diff[selection[2],])
  Tablediff[3,5] <- mean(c.statAM1diff[selection[2],])
  Tablediff[3,6] <- sd(c.statAM1diff[selection[2],])
  Tablediff[3,7] <- mean(c.statBM1diff[selection[2],])
  Tablediff[3,8] <- sd(c.statBM1diff[selection[2],])
  print(Tablediff)
}

print(xtable(Tdiff(selection1), digits=4),file=file.path(newpath,"/OutputOverall","Table4_1.txt"), compress=F)
print(xtable(Tdiff(selection2), digits=4),file=file.path(newpath,"/OutputOverall","Table4_2.txt"), compress=F)
