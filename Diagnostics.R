#########################################################
# Diagnostic Checks
#########################################################

Diagnostics <- function(model){
  outcprevAinput <- paste('outcprevA',model,sep="")
  outcprevBinput <- paste('outcprevB',model,sep="")
  Ccheckinput <- paste('Ccheck',model,sep="")
  Brcheckinput <- paste('Brcheck',model,sep="")
  Brcheck_calinput <- paste('Brcheck_cal',model,sep="")
  Brcheck_spreadinput <- paste('Brcheck_spread',model,sep="")
  countSepinput <- paste('countSep',model,sep="")
  MAwinput <- paste('MAw',model,sep="")
  calMwAinput <- paste('calMwA',model,sep="")
  calMwBinput <- paste('calMwB',model,sep="")
  
  # Outcome prevalence set A mean+var
  OutcPrevA <- matrix(c(rowMeans(get(outcprevAinput)),apply(get(outcprevAinput),1,sd)),
                      nrow(get(outcprevAinput)),
                      2,
                      dimnames=list(NULL,c("OutcPrevA mean","OutcPrevA sd")))
  OutcPrevB <- matrix(c(rowMeans(get(outcprevBinput)),apply(get(outcprevBinput),1,sd)),
                      nrow(get(outcprevBinput)),
                      2,
                      dimnames=list(NULL,c("OutcPrevB mean","OutcPrevB sd")))
  
  # Check c-stat no ME
  Ccheck <- matrix(c(rowMeans(get(Ccheckinput)),apply(get(Ccheckinput),1,sd)),
                      nrow(get(Ccheckinput)),
                      2,
                      dimnames=list(NULL,c("Ccheck mean","Check sd")))
  
  # Brier Score no ME
  BrierCheck <- matrix(c(rowMeans(get(Brcheckinput)),apply(get(Brcheckinput),1,sd)),
                      nrow(get(Brcheckinput)),
                      2,
                      dimnames=list(NULL,c("BrCheck mean","BrCheck sd")))
  BrierCheck_cal <- matrix(c(rowMeans(get(Brcheck_calinput)),apply(get(Brcheck_calinput),1,sd)),
                      nrow(get(Brcheck_calinput)),
                      2,
                      dimnames=list(NULL,c("BrCheck_cal mean","BrCheck_cal sd")))
BrierCheck_spread <- matrix(c(rowMeans(get(Brcheck_spreadinput)),apply(get(Brcheck_spreadinput),1,sd)),
                      nrow(get(Brcheck_spreadinput)),
                      2,
                      dimnames=list(NULL,c("BrCheck_spread mean","BrCheck_spread sd")))

# Separated datasets
countSep <- get(countSepinput)

# Convergence
Conv1 <- get(MAwinput)[get(MAwinput)!="NULL"]
Conv2 <- get(calMwAinput)[get(calMwAinput)!="NULL"]
Conv3 <- get(calMwBinput)[get(calMwBinput)!="NULL"]

# Save output
print(xtable(cbind(OutcPrevA,
                   OutcPrevB,
                   Ccheck,
                   BrierCheck,
                   BrierCheck_cal,
                   BrierCheck_spread),digits=5),
      file=file.path(newpath,"/OutputOverall",paste("diagnostics",model,"table.txt",sep="")),
      compress=F)

write.table(countSep,
            file=file.path(newpath,"/OutputOverall",paste("diagnostics",model,"counters.txt",sep="")),
            col.names="countSep",
            row.names=F,
            quote=F)
write.table(c(Conv1,Conv2,Conv3),
            file=file.path(newpath,"/OutputOverall",paste("diagnostics",model,"counters.txt",sep="")),
            col.names=T,
            row.names=F,
            quote=F,
            append=T)
}





