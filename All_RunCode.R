#########################################################
# Measurement heterogeneity of predictors across settings
# Author: K. Luijken
#
#########################################################

# This code accompanies the article 'Impact of predictor 
# measurement heterogeneity across settings on performance
# of prediction models: a measurement error perspective' 
# by Kim Luijken, Rolf H.H. Groenwold, Ben van Calster, 
# Ewout W. Steyerberg, Maarten van Smeden

# Instruction for running the code:
# All necessary code is sourced from the current file (All_RunCode.R).
# The number of observations per sample and repetitions of the
# simulation can be adjusted in "Parameters.R" if preferred.
# Suggestion: it may be helpful to remove the output from the 
# global environment after simultations for each model (M1, M2, M3, and M1diff)
# are finished, to reduce memory use.


# Terminology:
# We denote all objects in the derivation set with A,
# and all objects in the validation set with B.


#########################################################

# Store the current working directory of the R-project,
# using getwd()
newpath <- getwd()

# Load packages
source("Packages.R")
#install.packages("MASS")
#install.packages("rms")
#install.packages("ggplot2")
#install.packages("simsalapar")
#install.packages("xtable")
#install.packages("plyr")
#install.packages("gridExtra")
#install.packages("grid")
#install.packages("cowplot")
#install.packages("RColorBrewer")

# Define measurement error scenarios 
# (similar to paper)
source("Scenarios.R")

# Set simulation parameters (n, nsim) and create output storage
source("Parameters.R")
source("Output_Storage.R")

#########################################################
# Run simulations for single-predictor model, M1
SeedsM1 <- readRDS(paste(newpath,"/Seeds/","SeedsM1.rds",sep=""))
source("Datagen_ModvalM1.R")

# Perform diagnostic checks and summarize output M1
source("Diagnostics.R")
Diagnostics('M1')
source("Table3.R")

# Remove simulation output M1 from Global Environment for memory management
rm(list=Sim.outp.namesM1)

#########################################################
# Run simulations for two-predictor models M2
SeedsM2 <- readRDS(paste(newpath,"/Seeds/","SeedsM2.rds",sep=""))
source("Datagen_ModvalM2.R")

# Perform diagnostic checks and summarize outputM3
Diagnostics('M2')
source("Figure3.R")

# Remove simulation output M1 from Global Environment for memory management
rm(list=Sim.outp.namesM2)

#########################################################
# Run simulations for two-predictor models M3
SeedsM3 <- readRDS(paste(newpath,"/Seeds/","SeedsM3.rds",sep=""))
source("Datagen_ModvalM3.R")

# Perform diagnostic checks and summarize outputM3
Diagnostics('M3')
source("Figure4.R")

# Remove simulation output M1 from Global Environment for memory management
rm(list=Sim.outp.namesM3)

#########################################################
# Run simulations for single-predictor model under differential measurement error
SeedsM1diff <- readRDS(paste(newpath,"/Seeds/","SeedsM1diff.rds",sep=""))
source("Datagen_ModvalM1diff.R")

#source("Diagnostics.R")
Diagnostics('M1diff')
source("Table4.R")



