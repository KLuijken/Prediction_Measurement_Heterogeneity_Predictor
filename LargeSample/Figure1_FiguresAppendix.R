###############################################################
# Measurement error continuous predictor
#
###############################################################

# Load packages
require(rms)
library(cowplot)
require(grid)
require(gridExtra)
library(CalibrationCurves)

# Settings
set.seed(832)
n<-1000000
bdgm <- log(8)

# Create storage space
psi      <- c(0,0.25)
theta    <- c(0.5,1,2)
sigmaw   <- c(0,0.5*sqrt(2),sqrt(2))
Ypsi    <- list()
Ytheta  <- list()
Ysigmaw <- list()
f1psi    <- list()
f1theta  <- list()
f1sigmaw <- list()
f2psi    <- list()
f2theta  <- list()
f2sigmaw <- list()
pred.fitpsi       <- list()
pred.fittheta     <- list()
pred.fitsigmaw    <- list()
pred.reestpsi     <- list()
pred.reesttheta   <- list()
pred.reestsigmaw  <- list()
pred.transppsi    <- list()
pred.transptheta  <- list()
pred.transpsigmaw <- list()

#####################################################
# Generate data under additive systematic measurement error
for(i in 1:length(psi)){
  set.seed(832)
  
  # Data
  X <- rnorm(n,0,0.5*sqrt(2))
  eD <- rnorm(n,0,0.5*sqrt(2))
  WD <- X + eD
  P <- 1/(1+exp(-(X*bdgm)))
  Ypsi[[i]] <- rbinom(n,1,P)
  eV <- rnorm(n,0,0.5*sqrt(2))
  WV <- psi[i]+ X + eV
  
  
  # Estimate logistic regression model
  f1psi[[i]] <- glm(Ypsi[[i]]~WD, family=binomial)
  f2psi[[i]] <- glm(Ypsi[[i]]~WV, family=binomial)
  
  # Compute predicted scores
  pred.fitpsi[[i]]    <- 1/(1+exp(-(f1psi[[i]]$coefficients[1] + WD*f1psi[[i]]$coefficients[2])))
  pred.reestpsi[[i]]  <- 1/(1+exp(-(f2psi[[i]]$coefficients[1] + WV*f2psi[[i]]$coefficients[2])))
  pred.transppsi[[i]] <- 1/(1+exp(-(f1psi[[i]]$coefficients[1] + WV*f1psi[[i]]$coefficients[2])))
}


#####################################################
# Generate data under multiplicative systematic measurement error
for(i in 1:length(theta)){
  set.seed(832)
  
  # Data
  X <- rnorm(n,0,0.5*sqrt(2))
  eD <- rnorm(n,0,0.5*sqrt(2))
  WD <- X + eD
  P <- 1/(1+exp(-(X*bdgm)))
  Ytheta[[i]] <- rbinom(n,1,P)
  eV <- rnorm(n,0,0.5*sqrt(2))
  WV <- theta[i]*X + eV
  
  # Estimate logistic regression model
  f1theta[[i]] <- glm(Ytheta[[i]]~WD, family=binomial)
  f2theta[[i]] <- glm(Ytheta[[i]]~WV, family=binomial)
  
  # Compute predicted scores
  pred.fittheta[[i]]    <- 1/(1+exp(-(f1theta[[i]]$coefficients[1] + WD*f1theta[[i]]$coefficients[2])))
  pred.reesttheta[[i]]  <- 1/(1+exp(-(f2theta[[i]]$coefficients[1] + WV*f2theta[[i]]$coefficients[2])))
  pred.transptheta[[i]] <- 1/(1+exp(-(f1theta[[i]]$coefficients[1] + WV*f1theta[[i]]$coefficients[2])))
}


#####################################################
# Generate data under random measurement error
for(i in 1:length(sigmaw)){
  set.seed(832)
  
  # Set A
  X <- rnorm(n,0,0.5*sqrt(2))
  eD <- rnorm(n,0,0.5*sqrt(2))
  WD <- X + eD
  P <- 1/(1+exp(-(X*bdgm)))
  Ysigmaw[[i]] <- rbinom(n,1,P)
  eV <- rnorm(n,0,sigmaw[[i]])
  WV <- X + eV
  
  
  # Estimate logistic regression model
  f1sigmaw[[i]] <- glm(Ysigmaw[[i]]~WD, family=binomial)
  f2sigmaw[[i]] <- glm(Ysigmaw[[i]]~WV, family=binomial)
  
  # Compute predicted scores
  pred.fitsigmaw[[i]]    <- 1/(1+exp(-(f1sigmaw[[i]]$coefficients[1] + WD*f1sigmaw[[i]]$coefficients[2])))
  pred.reestsigmaw[[i]]  <- 1/(1+exp(-(f2sigmaw[[i]]$coefficients[1] + WV*f2sigmaw[[i]]$coefficients[2])))
  pred.transpsigmaw[[i]] <- 1/(1+exp(-(f1sigmaw[[i]]$coefficients[1] + WV*f1sigmaw[[i]]$coefficients[2])))
}

#####################################################
# Function to write calibration plots:
val.prob.me <- function (p, y, logit, group, weights = rep(1, length(y)), normwt = FALSE, 
                         pl = TRUE, smooth = TRUE, logistic.cal = TRUE, xlab = "Predicted Probability", 
                         ylab = "Actual Probability", lim = c(0, 1), m, g, cuts, emax.lim = c(0, 
                                                                                              1), legendloc = lim[1] + c(0.55 * diff(lim), 0.27 * diff(lim)), 
                         statloc = c(0, 0.99), riskdist = c("predicted", "calibrated"), 
                         cex = 3.0, mkh = 0.02, connect.group = FALSE, connect.smooth = TRUE, 
                         g.group = 4, evaluate = 100, nmin = 0) 
{
  if (missing(p)) 
    p <- plogis(logit)
  else logit <- qlogis(p)
  if (length(p) != length(y)) 
    stop("lengths of p or logit and y do not agree")
  names(p) <- names(y) <- names(logit) <- NULL
  riskdist <- match.arg(riskdist)
  Spi <- function(p, y) {
    z <- sum((y - p) * (1 - 2 * p))/sqrt(sum((1 - 2 * p) * 
                                               (1 - 2 * p) * p * (1 - p)))
    P <- 2 * pnorm(-abs(z))
    c(Z = z, P = P)
  }
  if (!missing(group)) {
    if (length(group) == 1 && is.logical(group) && group) 
      group <- rep("", length(y))
    if (!is.factor(group)) 
      group <- if (is.logical(group) || is.character(group)) 
        as.factor(group)
    else cut2(group, g = g.group)
    names(group) <- NULL
    nma <- !(is.na(p + y + weights) | is.na(group))
    ng <- length(levels(group))
  }
  else {
    nma <- !is.na(p + y + weights)
    ng <- 0
  }
  logit <- logit[nma]
  y <- y[nma]
  p <- p[nma]
  if (ng > 0) {
    group <- group[nma]
    weights <- weights[nma]
    return(val.probg(p, y, group, evaluate, weights, normwt, 
                     nmin))
  }
  if (length(unique(p)) == 1) {
    P <- mean(y)
    Intc <- qlogis(P)
    n <- length(y)
    D <- -1/n
    L01 <- -2 * sum(y * logit - logb(1 + exp(logit)), na.rm = TRUE)
    L.cal <- -2 * sum(y * Intc - logb(1 + exp(Intc)), na.rm = TRUE)
    U.chisq <- L01 - L.cal
    U.p <- 1 - pchisq(U.chisq, 1)
    U <- (U.chisq - 1)/n
    Q <- D - U
    spi <- unname(Spi(p, y))
    stats <- c(0.5,Intc, 0,mean((y - p[1])^2))
    names(stats) <- c("C-stat","Intercept", "Slope","Brier")
    return(stats)
  }
  i <- !is.infinite(logit)
  nm <- sum(!i)
  if (nm > 0) 
    warning(paste(nm, "observations deleted from logistic calibration due to probs. of 0 or 1"))
  f.fixed <- lrm.fit(logit[i], y[i], initial = c(0, 1), maxit = 1L)
  f.recal <- lrm.fit(logit[i], y[i])
  stats <- f.fixed$stats
  n <- stats["Obs"]
  predprob <- seq(emax.lim[1], emax.lim[2], by = 5e-04)
  Sm <- lowess(p, y, iter = 0)
  cal.smooth <- approx(Sm, xout = p, ties = mean)$y
  er <- abs(p - cal.smooth)
  eavg <- mean(er)
  emax <- max(er)
  e90 <- unname(quantile(er, 0.9))
  if (pl) {
    plot(0.5, 0.5, xlim = lim, ylim = lim, type = "n", xlab = xlab, 
         ylab = ylab, cex.lab=2.5, cex.axis=2.5, cex.main=2.5, cex.sub=2.5, cex = 3.0, mar=c(5, 6, 4, 2) + 0.1)
    abline(0, 1, lwd = 6, col = gray(0.85))
    lt <- 1
    leg <- "Ideal"
    marks <- -1
    lwd <- 6
    col <- gray(0.85)
    if (logistic.cal) {
      lt <- c(lt, 1)
      leg <- c(leg, "Logistic calibration")
      lwd <- c(lwd, 1)
      col <- c(col, "black")
      marks <- c(marks, -1)
    }
    if (smooth) {
      if (connect.smooth) {
        lines(Sm, lty = 3)
        lt <- c(lt, 3)
        lwd <- c(lwd, 1)
        col <- c(col, "black")
        marks <- c(marks, -1)
      }
      else {
        points(Sm)
        lt <- c(lt, 0)
        lwd <- c(lwd, 1)
        col <- c(col, "black")
        marks <- c(marks, 1)
      }
      leg <- c(leg, "Nonparametric")
    }
    if (!missing(m) | !missing(g) | !missing(cuts)) {
      if (!missing(m)) 
        q <- cut2(p, m = m, levels.mean = TRUE, digits = 7)
      else if (!missing(g)) 
        q <- cut2(p, g = g, levels.mean = TRUE, digits = 7)
      else if (!missing(cuts)) 
        q <- cut2(p, cuts = cuts, levels.mean = TRUE, 
                  digits = 7)
      means <- as.numeric(levels(q))
      prop <- tapply(y, q, function(x) mean(x, na.rm = TRUE))
      points(means, prop, pch = 2)
      if (connect.group) {
        lines(means, prop)
        lt <- c(lt, 1)
      }
      else lt <- c(lt, 0)
      leg <- c(leg, "Grouped observations")
      col <- c(col, "black")
      lwd <- c(lwd, 1)
      marks <- c(marks, 2)
    }
  }
  C <- stats["C"]
  B <- mean((p - y)^2)
  stats <- c(C, f.recal$coef,B)
  names(stats) <- c("C-stat","Intercept", "Slope","Brier")
  if (pl) {
    logit <- seq(-7, 7, length = 200)
    prob <- plogis(logit)
    pred.prob <- f.recal$coef[1] + f.recal$coef[2] * logit
    pred.prob <- plogis(pred.prob)
    if (logistic.cal) 
      lines(prob, pred.prob, lty = 1)
    lp <- legendloc
    if (!is.logical(lp)) {
      if (!is.list(lp)) 
        lp <- list(x = lp[1], y = lp[2])
      legend(lp, leg, lty = lt, pch = marks, cex = cex, 
             lwd = lwd, col = col, bty = "n")
    }
    if (!is.logical(statloc)) {
      dostats <- c("C-stat","Intercept", "Slope","Brier")
      leg <- paste(format(names(stats)[dostats]), "      ", sep="")
      leg <- paste(leg, ":",format(stats[dostats]), sep = "")
      if (!is.list(statloc)) 
        statloc <- list(x = statloc[1], y = statloc[2])
      text(statloc, paste(format(names(stats[dostats])), 
                          collapse = "\n"), adj = c(0, 1), cex = cex)
      text(statloc$x + 0.225 * diff(lim), statloc$y, paste(format(round(stats[dostats], 
                                                                        2)), collapse = "\n"), adj = c(-1, 1), cex = cex)
    }
    if (is.character(riskdist)) {
      if (riskdist == "calibrated") {
        x <- f.recal$coef[1] + f.recal$coef[2] * qlogis(p)
        x <- plogis(x)
        x[p == 0] <- 0
        x[p == 1] <- 1
      }
      else x <- p
      bins <- seq(lim[1], lim[2], length = 101)
      x <- x[x >= lim[1] & x <= lim[2]]
      f <- table(cut(x, bins))
      j <- f > 0
      bins <- (bins[-101])[j]
      f <- f[j]
      f <- lim[1] + 0.15 * diff(lim) * f/max(f)
      segments(bins, 0, bins, f)
    }
  }
  stats
}


##############################################################
### Recalibration plots


# Additive measurement heterogeneity
pdf("psi1M.pdf", width=21, height = 8)
par(mar=c(6.1, 6.1, 4.1, 2.1),mfrow=c(1,3), oma=c(0,0,5,0))
val.prob.me(pred.fitpsi[[1]],Ypsi[[1]], legendloc="none")
mtext("     Derivation sample          Validation sample", cex=3.7, line=1, side=NORTH<-3, adj=0, outer=TRUE, font=2)
title(main="A", cex.main=3,adj=0)
val.prob.me(pred.transppsi[[1]],Ypsi[[1]],legendloc = "none")
title(main="B", cex.main=3,adj=0)
val.prob.me(pred.transppsi[[2]],Ypsi[[2]],legendloc = "none")
title(main="C", cex.main=3,adj=0)
dev.off()


# Multiplicative measurement heterogeneity
pdf("theta1M.pdf", width=28, height = 8)
par(mar=c(6.1, 6.1, 4.1, 2.1),mfrow=c(1,4), oma=c(0,0,5,0))
val.prob.me(pred.fittheta[[1]],Ytheta[[1]], legendloc="none")
mtext("     Derivation sample          Validation sample", cex=3.7, line=1, side=NORTH<-3, adj=0, outer=TRUE, font=2)
title(main="A", cex.main=3,adj=0)
val.prob.me(pred.transptheta[[1]],Ytheta[[1]],legendloc = "none")
title(main="B", cex.main=3,adj=0)
val.prob.me(pred.transptheta[[2]],Ytheta[[2]],legendloc = "none")
title(main="C", cex.main=3,adj=0)
val.prob.me(pred.transptheta[[3]],Ytheta[[3]],legendloc = "none")
title(main="D", cex.main=3,adj=0)
dev.off()

# Random measurement heterogeneity
pdf("random1M.pdf", width=28, height = 8)
par(mar=c(6.1, 6.1, 4.1, 2.1),mfrow=c(1,4), oma=c(0,0,5,0))
val.prob.me(pred.fitsigmaw[[1]],Ysigmaw[[1]], legendloc="none")
mtext("     Derivation sample          Validation sample", cex=3.7, line=1, side=NORTH<-3, adj=0, outer=TRUE, font=2)
title(main="A", cex.main=3,adj=0)
val.prob.me(pred.transpsigmaw[[3]],Ysigmaw[[3]],legendloc = "none")
title(main="B", cex.main=3,adj=0)
val.prob.me(pred.transpsigmaw[[2]],Ysigmaw[[2]],legendloc = "none")
title(main="C", cex.main=3,adj=0)
val.prob.me(pred.transpsigmaw[[1]],Ysigmaw[[1]],legendloc = "none")
title(main="D", cex.main=3,adj=0)
dev.off()

# InvsOut (Figure 1 of paper 'Impact of predictor 
# measurement heterogeneity across settings on performance
# of prediction models: a measurement error perspective')
png("in1M.png", width=20, height = 7, units = "in", res = 300)
par(mar=c(6.1, 6.1, 4.1, 2.1),mfrow=c(1,3), oma=c(0,0,5,0))
val.prob.me(pred.reestsigmaw[[3]],Ysigmaw[[3]], legendloc="none")
mtext("Re-estimation", cex=3.7, line=1, side=NORTH<-3, adj=0.05, outer=TRUE, font=2)
title(main=bquote(bold(paste("A:  200% MV"))), cex.main=3, adj=0)
val.prob.me(pred.reestsigmaw[[2]],Ysigmaw[[2]],legendloc = "none")
title(main=bquote(bold(paste("B:  100% MV"))), cex.main=3, adj=0)
val.prob.me(pred.reestsigmaw[[1]],Ysigmaw[[1]],legendloc = "none")
title(main=bquote(bold(paste("C:  50% MV"))), cex.main=3, adj=0)
dev.off()

png("out1M.png", width=20, height = 7, units = "in", res = 300)
par(mar=c(6.1, 6.1, 4.1, 2.1),mfrow=c(1,3), oma=c(0,0,5,0))
val.prob.me(pred.transpsigmaw[[3]],Ysigmaw[[3]],legendloc = "none")
mtext("Transportation", cex=3.7, line=1, side=NORTH<-3, adj=0.05, outer=TRUE, font=2)
title(main=bquote(bold(paste("D:  200% MV"))), cex.main=3, adj=0)
val.prob.me(pred.transpsigmaw[[2]],Ysigmaw[[2]],legendloc = "none")
title(main=bquote(bold(paste("E:  100% MV"))), cex.main=3, adj=0)
val.prob.me(pred.transpsigmaw[[1]],Ysigmaw[[1]],legendloc = "none")
title(main=bquote(bold(paste("F:  50% MV"))), cex.main=3, adj=0)
dev.off()

#############################################################
# Differential measurement error

# Set parameters
thetaD0    <- 1
thetaD1    <- c(0.5,1)
sigmawD0   <- 0.5*sqrt(2)
sigmawD1   <- c(0,0.5*sqrt(2),sqrt(2))
thetaV0    <- 1
thetaV1    <- c(0.5,1)
sigmawV0   <- 0.5*sqrt(2)
sigmawV1   <- c(0,0.5*sqrt(2),sqrt(2))

DiffME <- function(thetaD0,thetaD1,sigmawD0,sigmawD1,thetaV0,thetaV1,sigmawV0,sigmawV1){
  set.seed(832)
  
  # Data
  X <- rnorm(n,0,0.5*sqrt(2))
  P <- 1/(1+exp(-(X*bdgm)))
  Y <- rbinom(n,1,P)
  
  etaD0 <- rnorm(n,0,sigmawD0)
  etaD1 <- rnorm(n,0,sigmawD1)
  WD <- vector()
  for(m in 1:n){
    WD[m] <- ((thetaD0*X[m]+etaD0[m])^(1-Y[m])) * ((thetaD1*X[m] + etaD1[m])^(Y[m]))
  }
  
  
  etaV0 <- rnorm(n,0,sigmawV0)
  etaV1 <- rnorm(n,0,sigmawV1)
  WV <- vector()
  for(m in 1:n){
    WV[m] <- ((thetaV0*X[m]+etaV0[m])^(1-Y[m])) * ((thetaV1*X[m] + etaV1[m])^(Y[m]))
  }
  
  # Estimate logistic regression model
  f1psi <- glm(Y~WD, family=binomial)
  f2psi <- glm(Y~WV, family=binomial)
  
  # Compute predicted scores
  pred.fit    <- 1/(1+exp(-(f1psi$coefficients[1] + WD*f1psi$coefficients[2])))
  pred.reest  <- 1/(1+exp(-(f2psi$coefficients[1] + WV*f2psi$coefficients[2])))
  pred.transp <- 1/(1+exp(-(f1psi$coefficients[1] + WV*f1psi$coefficients[2])))
  
  result <- list(Y=Y,pred.fit=pred.fit,pred.reest=pred.reest,pred.transp=pred.transp)
  return(result)
}


scen1 <- DiffME(1,1,0.5*sqrt(2),0,1,1,0.5*sqrt(2),0.5*sqrt(2))
scen2 <- DiffME(1,1,0.5*sqrt(2),0.5*sqrt(2),1,1,0.5*sqrt(2),sqrt(2))
scen3 <- DiffME(1,1,0.5*sqrt(2),sqrt(2),1,1,0.5*sqrt(2),0.5*sqrt(2))
scen4 <- DiffME(1,1,0.5*sqrt(2),0.5*sqrt(2),1,1,0.5*sqrt(2),0)
scen5 <- DiffME(1,0.5,0.5*sqrt(2),0.5*sqrt(2),1,1,0.5*sqrt(2),0.5*sqrt(2))
scen6 <- DiffME(1,1,0.5*sqrt(2),0.5*sqrt(2),1,0.5,0.5*sqrt(2),0.5*sqrt(2))
scen7 <- DiffME(1,2,0.5*sqrt(2),0.5*sqrt(2),1,1,0.5*sqrt(2),0.5*sqrt(2))

# Panel
pdf("diffI1M.pdf", width=14, height = 8)
par(mar=c(6.1, 6.1, 4.1, 2.1),mfrow=c(1,2), oma=c(0,0,5,0))
val.prob.me(scen2$pred.fit,scen2$Y, legendloc="none")
mtext("       Derivation sample          Validation sample", cex=3.7, line=1, side=NORTH<-3, adj=0, outer=TRUE, font=2)
title(main="A", cex.main=3,adj=0)
val.prob.me(scen2$pred.transp,scen2$Y, legendloc="none")
dev.off()

pdf("diffII1M.pdf", width=14, height = 7)
par(mar=c(6.1, 6.1, 4.1, 2.1),mfrow=c(1,2))
val.prob.me(scen4$pred.fit,scen4$Y, legendloc="none")
title(main="B", cex.main=3,adj=0)
val.prob.me(scen4$pred.transp,scen4$Y, legendloc="none")
dev.off()

pdf("diffIII1M.pdf", width=14, height = 7)
par(mar=c(6.1, 6.1, 4.1, 2.1),mfrow=c(1,2))
val.prob.me(scen6$pred.fit,scen6$Y, legendloc="none")
title(main="C", cex.main=3,adj=0)
val.prob.me(scen6$pred.transp,scen6$Y, legendloc="none")
dev.off()

pdf("diffIV.pdf", width=14, height = 7)
par(mar=c(6.1, 6.1, 4.1, 2.1),mfrow=c(1,2))
val.prob.me(scen1$pred.fit,scen1$Y, legendloc="none")
title(main="A", cex.main=3,adj=0)
val.prob.me(scen1$pred.transp,scen1$Y, legendloc="none")
dev.off()

pdf("diffV1M.pdf", width=14, height = 7)
par(mar=c(6.1, 6.1, 4.1, 2.1),mfrow=c(1,2))
val.prob.me(scen3$pred.fit,scen3$Y, legendloc="none")
title(main="B", cex.main=3,adj=0)
val.prob.me(scen3$pred.transp,scen3$Y, legendloc="none")
dev.off()

pdf("diffVI1M.pdf", width=14, height = 7)
par(mar=c(6.1, 6.1, 4.1, 2.1),mfrow=c(1,2))
val.prob.me(scen5$pred.fit,scen5$Y, legendloc="none")
title(main="C", cex.main=3,adj=0)
val.prob.me(scen5$pred.transp,scen5$Y, legendloc="none")
dev.off()


