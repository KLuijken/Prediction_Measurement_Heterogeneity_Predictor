###############################################################
# Measurement error continuous predictor
#
###############################################################

# Load packages
require(rms)
library(cowplot)
library(CalibrationCurves)

# Settings
set.seed(832)
n<-1000000
bdgm <- log(4)

# Create storage space
etot <- seq(from=0, to=2.15,by=0.05)
Y2 <- list()
f1 <- list()
f2 <- list()
pred.fit    <- list()
pred.reest  <- list()
pred.transp <- list()
TBrier.fit    <- list()
TBrier.reest  <- list()
TBrier.transp <- list()
CBrier.fit    <- list()
CBrier.reest  <- list()
CBrier.transp <- list()
SBrier.fit    <- list()
SBrier.reest  <- list()
SBrier.transp <- list()

# Generate data under random measurement error
for(i in 1:length(etot)){
  # Set A
  V1 <- rnorm(n,0,0.5*sqrt(2))
  eX1 <- rnorm(n,0,0.5*sqrt(2))
  X1 <- V1 + eX1
  P1 <- 1/(1+exp(-(V1*bdgm)))
  Y1 <- rbinom(n,1,P1)
  eW1 <- rnorm(n,0,etot[i])
  W1 <- V1 + eW1
  
  # Set B
  V2 <- rnorm(n,0,0.5*sqrt(2))
  eX2 <- rnorm(n,0,0.5*sqrt(2))
  X2 <- V2 + eX2
  P2 <- 1/(1+exp(-(V2*bdgm)))
  Y2[[i]] <- rbinom(n,1,P2)
  eW2 <- rnorm(n,0,etot[i])
  W2 <- V2 + eW2
  
  # Estimate logistic regression model
  f1[[i]] <- glm(Y1~X1, family=binomial)
  f2[[i]] <- glm(Y2[[i]]~W2, family=binomial)
  
  # Compute predicted scores
  pred.fit[[i]]    <- 1/(1+exp(-(f1[[i]]$coefficients[1] + X1*f1[[i]]$coefficients[2])))
  pred.reest[[i]]  <- 1/(1+exp(-(f2[[i]]$coefficients[1] + W2*f2[[i]]$coefficients[2])))
  pred.transp[[i]] <- 1/(1+exp(-(f1[[i]]$coefficients[1] + W2*f1[[i]]$coefficients[2])))
  
  # Total empirical Brier Score
  TBrier.fit[[i]]    <- mean((Y1-pred.fit[[i]])^2)
  TBrier.reest[[i]]  <- mean((Y2[[i]]-pred.reest[[i]])^2)
  TBrier.transp[[i]] <- mean((Y2[[i]]-pred.transp[[i]])^2)
  
  # Calibration component
  CBrier.fit[[i]]    <- mean((Y1-pred.fit[[i]])*(1-2*pred.fit[[i]]))
  CBrier.reest[[i]]  <- mean((Y2[[i]]-pred.reest[[i]])*(1-2*pred.reest[[i]]))
  CBrier.transp[[i]] <- mean((Y2[[i]]-pred.transp[[i]])*(1-2*pred.transp[[i]]))
  
  # Spread component
  SBrier.fit[[i]]    <- mean(pred.fit[[i]]*(1-pred.fit[[i]]))
  SBrier.reest[[i]]  <- mean(pred.reest[[i]]*(1-pred.reest[[i]]))
  SBrier.transp[[i]] <- mean(pred.transp[[i]]*(1-pred.transp[[i]]))
}

# Recompute error term to percentage
etot100 <- matrix(0,length(etot),1)
for(j in 1:length(etot)){
  etot100[j]<-((0.5*sqrt(2)+etot[j])/(sqrt(2))) * 100
}

## In sample (re-estimated) Brier
TBrier1 <- data.frame(unlist(TBrier.reest),rep(1,times=length(TBrier.reest)), etot100)
colnames(TBrier1) <- c("Brier","colour","etot")

SBrier1 <- data.frame(unlist(SBrier.reest),rep(2,times=length(TBrier.reest)), etot100)
colnames(SBrier1) <- c("Brier","colour","etot")

CBrier1 <- data.frame(unlist(CBrier.reest),rep(3,times=length(TBrier.reest)), etot100)
colnames(CBrier1) <- c("Brier","colour","etot")

Brierplot.reest <- rbind(TBrier1,SBrier1, CBrier1)
Brierplot.reest$colour <- factor(Brierplot.reest$colour, levels=c(1,2,3), labels = c("Empirical Brier Score","Refinement component", "Calibration component"))


InBrier <- ggplot(Brierplot.reest) +
  geom_smooth(aes(x=Brierplot.reest$etot, y=Brierplot.reest$Brier, linetype=colour), color="black", se=F) +
  ylim(c(-0.05,0.40)) + xlab("% MV in validation relative to derivaion") + ylab("Brier Score") + labs(title="A:  Re-estimation") +
  theme(axis.text=element_text(size=rel(1.5)), axis.title.y = element_text(size = rel(1.5)), axis.title.x = element_text(size = rel(1.5)),plot.title = element_text(size=rel(2), hjust=-.2))

## Out-of-sample (transported) Brier
TBrier2 <- data.frame(unlist(TBrier.transp),rep(1,times=length(TBrier.transp)), etot100)
colnames(TBrier2) <- c("Brier","Brier Score","etot")

SBrier2 <- data.frame(unlist(SBrier.transp),rep(2,times=length(TBrier.transp)), etot100)
colnames(SBrier2) <- c("Brier","Brier Score","etot")

CBrier2 <- data.frame(unlist(CBrier.transp),rep(3,times=length(TBrier.transp)), etot100)
colnames(CBrier2) <- c("Brier","Brier Score","etot")

Brierplot.transp <- rbind(TBrier2,SBrier2, CBrier2)
colnames(Brierplot.transp) <- c("Brier","Brier Score","etot")
Brierplot.transp$`Brier Score` <- factor(Brierplot.transp$`Brier Score`, levels=c(1,2,3), labels = c("Empirical Brier Score","Refinement component", "Calibration component"))


OutBrier <- ggplot(Brierplot.transp) +
  geom_smooth(aes(x=Brierplot.transp$etot, y=Brierplot.transp$Brier, linetype=`Brier Score`), color="black", se=F) +
  ylim(c(-0.05,0.40)) + xlab("% MV in validation relative to derivation") + ylab("Brier Score") + labs(title="B:  Transportation") +
  theme(axis.text=element_text(size=rel(1.5)), axis.title.y = element_text(size = rel(1.5)), axis.title.x = element_text(size = rel(1.5)),plot.title = element_text(size=rel(2), hjust=-.2), legend.key.size = unit(1,"cm"), legend.text = element_text(size = rel(1.5)),legend.justification = "center")



# Combine plots into figure
legend <- get_legend(OutBrier+ theme(legend.position="bottom"))
Combo <- plot_grid(InBrier+theme(legend.position = "none"),OutBrier+theme(legend.position = "none"), align = 'vh',
                   nrow = 1,
                   ncol = 2)
Grid <- plot_grid(Combo, legend, ncol = 1, rel_heights = c(1, .2))

#setwd("")
pdf("Figure2_BrierScore1M.pdf", width = 13, height = 7)
Grid
dev.off()

