#############################################################
# Create Figure 3
#############################################################

# Data preparation
Scenarios <- as.data.frame(Scenarios)

Prepare <- function(perfmA,perfmB,nameA,nameB,perfm,model){
   PerfmA <- apply(perfmA,1,mean)
   PerfmB <- apply(perfmB,1,mean)
   data   <- data.frame(PerfmA,PerfmB)
   
   data$random <- rep(NA,length(data))
   data$random[which(Scenarios$sigmaUA == 0.5 & Scenarios$sigmaUB != 0.5)] <- 1
   data$random[which(Scenarios$sigmaUA == 2.0 & Scenarios$sigmaUB != 2.0)] <- 2
   data$random <- as.factor(data$random)

   data$group <- rep(NA,length(data))
   data$group[which(Scenarios$thetaB == 0.5)] <- "theta = 0.5"
   data$group[which(Scenarios$thetaB == 2.0)] <- "theta = 2.0"
   data$group <- as.factor(data$group)

   derA <- mean(data$PerfmA[which(data$random == 1)])
   data$Z <- rep(NA,nrow(data))
   data$Z[which(data$random == 1)] <- derA
   valA <- mean(data$PerfmA[which(data$random == 2)])
   data$Z[which(data$random == 2)] <- valA

   data$random <- factor(data$random, labels = c(bquote(paste(sigma[D]^2,"<",sigma[V]^2)),bquote(paste(sigma[D]^2,">",sigma[V]^2))))
   
   colnames(data) <- c(nameA,nameB,"random","group","Z")
   assign(paste("dat",perfm,model,sep=""),data, envir = .GlobalEnv)
}

Prepare(c.statAM2,c.statBM2,"cstatAM2","cstatBM2","cstat",2)
Prepare(cal.slopeAM2,cal.slopeBM2,"calslopeAM2","calslopeBM2","cal",2)
Prepare(BrierAM2,BrierBM2,"BrierM2A","BrierM2B","Brier",2)

#############################################################
# Plotting
#############################################################

sit1 <- bquote(paste(theta[V] == "0.5","      "))
sit2 <- bquote(paste(theta[V] == "2.0","      "))

greeks <- list(sit1, sit2)

Palette <- c("grey21","snow2")

p1 <- ggplot(data=subset(datBrier2, !is.na(random) & !is.na(group)),aes(x=group, y=BrierM2B, fill=group)) + 
  geom_boxplot() +
  stat_boxplot(geom = "errorbar") +
  facet_grid(~random, labeller = label_parsed) +
  geom_hline(aes(yintercept=Z), size=1)+
  scale_y_continuous(name = "Mean Brier Score",
                     limits=c(0, 0.35)) +
  ggtitle("Brier Score") +
  scale_fill_manual(NULL,labels=c(greeks), values=Palette) +
  theme(strip.text.x = element_text(size = 15),legend.position = "bottom", 
        legend.text = element_text(size=16),  
        axis.text=element_text(size=rel(1.5)), axis.title.y = element_text(size = rel(1.5)),
        plot.title = element_text(size=rel(1.5)),legend.justification = "center")+
  scale_x_discrete(name = NULL, labels = NULL) 


p2 <- ggplot(data=subset(datcal2, !is.na(random) & !is.na(group)),aes(x=group, y=calslopeBM2, fill=group)) + 
  geom_boxplot() +
  stat_boxplot(geom = "errorbar") +
  facet_grid(~random, labeller = label_parsed) +
  geom_hline(aes(yintercept=Z), size=1)+
  scale_y_continuous(name = "Median calibration slope",
                     limits=c(0, 5.5)) +
  scale_x_discrete(name = NULL, labels = NULL) +
  ggtitle("Calibration slope") +
  scale_fill_manual(values=Palette) +
  theme(strip.text.x = element_text(size = 15),axis.text=element_text(size=rel(1.5)), axis.title.y = element_text(size = rel(1.5)),plot.title = element_text(size=rel(1.5)))


p3 <- ggplot(data=subset(datcstat2, !is.na(random) & !is.na(group)),aes(x=group, y=cstatBM2, fill=group)) + 
  geom_boxplot() +
  stat_boxplot(geom = "errorbar") +
  facet_grid(~random, labeller = label_parsed) +
  geom_hline(aes(yintercept=Z), size=1)+
  scale_y_continuous(name = "Mean c-statistic",
                     limits=c(0.5, 1)) +
  scale_x_discrete(name = NULL, labels=NULL) +
  ggtitle("C-statistic") +
  scale_fill_manual(values=Palette) +
  theme(strip.text.x = element_text(size = 15),axis.text=element_text(size=rel(1.5)), 
        axis.title.y = element_text(size = rel(1.5)),plot.title = element_text(size=rel(1.5)))


# arrange the three plots in a single row
prow2 <- plot_grid( p3 + theme(legend.position="none"),
                    p2 + theme(legend.position="none"),
                    p1 + theme(legend.position="none"),
                    align = 'vh',
                    nrow = 1,
                    ncol = 3
                    
)

legend_b <- get_legend(p1 + theme(legend.position="left"))

# Combine legend and plot
PP2 <- plot_grid(legend_b, prow2, align="h", axis = "t", ncol= 2, rel_widths = c(.15,1))

#save plots
setwd(paste(newpath,"/OutputOverall",sep=""))

pdf("Figure3_TotalM2.pdf", width = 18, height = 6)
PP2
dev.off()


setwd(newpath)
Scenarios <- as.matrix(Scenarios)




