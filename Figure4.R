#############################################################
# Create Figure 4
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

Prepare(c.statAM3,c.statBM3,"cstatAM3","cstatBM3","cstat",3)
Prepare(cal.slopeAM3,cal.slopeBM3,"calslopeAM3","calslopeBM3","cal",3)
Prepare(BrierAM3,BrierBM3,"BrierM3A","BrierM3B","Brier",3)

#############################################################
# Plotting
#############################################################

sit1 <- bquote(paste(theta[V] == "0.5","      "))
sit2 <- bquote(paste(theta[V] == "2.0","      "))

greeks <- list(sit1, sit2)

Palette <- c("grey21","snow2")

p4 <- ggplot(data=subset(datBrier3, !is.na(random) & !is.na(group)),aes(x=group, y=BrierM3B, fill=group)) + 
  geom_boxplot() +
  stat_boxplot(geom = "errorbar") +
  facet_grid(~random, labeller = label_parsed) +
  geom_hline(aes(yintercept=Z), size=1)+
  scale_y_continuous(name = "Mean Brier Score",
                     limits=c(0, 0.35)) +
  scale_x_discrete(name = NULL, labels=NULL) +
  ggtitle("Brier Score") +
  scale_fill_manual(values=Palette) +
  theme(strip.text.x = element_text(size = 15),legend.position = "bottom", 
        legend.text = element_text(size=16),  
        axis.text=element_text(size=rel(1.5)), axis.title.y = element_text(size = rel(1.5)),
        plot.title = element_text(size=rel(1.5)),legend.justification = "center")+
  scale_x_discrete(name = NULL, labels = NULL)

p5 <- ggplot(data=subset(datcal3, !is.na(random) & !is.na(group)),aes(x=group, y=calslopeBM3, fill=group)) + 
  geom_boxplot() +
  stat_boxplot(geom = "errorbar") +
  facet_grid(~random, labeller = label_parsed) +
  geom_hline(aes(yintercept=Z), size=1)+
  scale_y_continuous(name = "Median calibration slope",
                     limits=c(0, 5.5)) +
  scale_x_discrete(name = NULL, labels=NULL) +
  ggtitle("Calibration slope") +
  scale_fill_manual(values=Palette) +
  theme(strip.text.x = element_text(size = 15),axis.text=element_text(size=rel(1.5)), axis.title.y = element_text(size = rel(1.5)),plot.title = element_text(size=rel(1.5)))


p6 <- ggplot(data=subset(datcstat3, !is.na(random) & !is.na(group)),aes(x=group, y=cstatBM3, fill=group)) + 
  geom_boxplot() +
  stat_boxplot(geom = "errorbar") +
  facet_grid(~random, labeller = label_parsed) +
  geom_hline(aes(yintercept=Z), size=1)+
  scale_y_continuous(name = "Mean c-statistic",
                     limits=c(0.5, 1)) +
  scale_x_discrete(name = NULL, labels=NULL) +
  ggtitle("C-statistic") +
  scale_fill_manual(values=Palette) +
  theme(strip.text.x = element_text(size = 15),axis.text=element_text(size=rel(1.5)), axis.title.y = element_text(size = rel(1.5)),plot.title = element_text(size=rel(1.5)))


# arrange the three plots in a single row
prow3 <- plot_grid( p6 + theme(legend.position="none"),
                    p5 + theme(legend.position="none"),
                    p4 + theme(legend.position="none"),
                    align = 'vh',
                    nrow = 1,
                    ncol = 3
                    
)


legend_b <- get_legend(p4 + theme(legend.position="left"))

# Combine legend and plot
PP3 <- plot_grid(legend_b, prow3, align="h", axis = "t", ncol= 2, rel_widths = c(.15,1))

#save plots
setwd(paste(newpath,"/OutputOverall",sep=""))

pdf("Figure4_TotalM3.pdf", width = 18, height = 6)
PP3
dev.off()

setwd(newpath)