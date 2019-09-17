#################################################################################
# Supplentary R code to support Thaxter et al. 2017. Sample Size Required to Characterize Area Use of Tracked Seabirds
#
# Code last modified: 16/01/2017
#
# Description:
# Reads in bootstrap output files from:
# JWMThaxter_et_al_sm_RScript1.Bootstrapping.R
# Fits non-linear curves, in turn calculating back-transformed number of birds from area~f(birds)
#
# NOTE: TESTED UNDER 64-bit R versions and key packages: 
#R version 3.1.2 (2014-10-31); drc_2.5-12; proto_0.3-10; nls2_0.2
#R version 3.3.2 (2016-10-31); drc_3.0-1; proto_1.0.0; nls2_0.2
##########################################################################

# Required libraries
#install.packages('drc')
#install.packages('propagate')
#install.packages('nls2')

library(lattice)
library(foreign)
library(drc)
library(nls2)
library(propagate)

# ********************************************************************** #
# HARD-CODED CHOICES
# CODE EASILY ADAPTABLE TO OTHER COMBINATIONS THAT WERE PRODUCED FOR THE 
# PAPER, SUCH AS INCREASING DAYS ADDED TO SAMPLE, AND PREDICTIONS
# OF AREA AT SPECIFIC POPULATION SIZES FOR Fig 5 (not shown here)
# THIS PROGRAM IS JUST FOR THE MAXIMUM 151 DAY PREDICTION FOR MAX BIRDS
# ********************************************************************** #
Plot <- TRUE		# do we want to plot the lines being fitted (diff. colour per model)
type <- "X100" 		# what occupancy do we want to plot curves for? Here, chosen 100% select that from the boostraps read in

# Start directory
start.dir <- "E:/"

# read in source code for function: "fit.eq.birds.txt" and "build.dir.txt"
setwd(start.dir)
source("fit.eq.birds.txt")
source("build.dir.txt")


# ********************************************************************** #
# START OF CURVE-FITTING ROUTINE 
# ********************************************************************** #

# ----------------- #
# STARTING SAMPLE
# ----------------- #
# In the full analysis, the program was run across all starting samples
# however, here the code is taken out of a for loop and shown for the 15 bird 151 day dataset 

runthru <- data.frame(MaxDay = c(176,173,172,171,170,167,163,160,158,158,151,147,143),birdno = seq(5,17,1))
w <- 11 # 15 birds, 151 days
MaxDay <- runthru[w,]$MaxDay; birdno <- runthru[w,]$birdno

# --------------------------------------- #
# READ IN DATA FOR CHOSEN BIRDS AND DAYS
# --------------------------------------- #
# Boostraps from previous program
subDir <- build.dir(birdno) 
setwd(file.path(start.dir, subDir))
filenames <- list.files(full.names=TRUE) # list of filenames of csvs for individual boots	
All <- lapply(filenames,function(i){read.csv(i,header=TRUE,sep=",")})
maxboot <- length(filenames)
All2 <- data.frame(All)
All2$birdNo <- rep(1,MaxDay)
All3 <- All2[substr(names(All2),1,4)== type]	# Define occupancy on which to fit curves

# vector of 1 to maxno birds, each for the 1-max no days - matching the order of data in the bootstraps - to be added in on the fly to the csv's read in
i<-1; a <- b <- C <- NULL; for(i in 1:birdno){a <- rep(i,MaxDay); C <- c(b,a); b <- C}
D <- rep(seq(1,MaxDay,1),birdno)	# vector of 1- max no. days, repeated for the number of birds we have - matching the order of data in the bootstraps - to be added in on the fly to the csv's read in
All3$birdNo <- C; All3$day <- D 	# add in birds and days as extra columns
maxboot <- dim(All3)[2]-2 		# extracting the max number of boots, although we already really know what this is
Use <- All3 				# in the function "use" is the data.frame, for internal debugging it was renamed, can ignore this detail

# ---------------------------------------------- #
# Set up list of bootstraps to run the function over
# ---------------------------------------------- #
list1 <- list(); for(i in 1:maxboot){list1[i] <- list(i)}

# ---------------------------------------------- #
# Run the equation-fitting function
# ---------------------------------------------- #
setwd(start.dir) 				# output files to main directory
maxplot <- 20000 				# for plotting purposes setting an upper y-axis max value 
ab <- lapply(list1,fit.eq.birds) 	# run through all boots and output results to file

# ---------------------------------------------------------------------- #
# CHECK FINAL OUTPUT - read results back in again
# ---------------------------------------------------------------------- #

ab <- read.csv("X100.151day.predictions.csv",sep=",",header=TRUE)
ab_1 <- ab[ab$PropAsy == "X95",] # predict of n birds for 95% the population value
ab_1 <- ab_1[ab_1$birdNo > 0,]

A <- boxplot(ab_1$birdNo,plot=FALSE)
mytable1 <- A$stats
colnames(mytable1)<-A$names
rownames(mytable1)<-c('min','lower quartile','median','upper quartile','max')
table1 <- data.frame(t(mytable1))
table1 <- round(table1,1)
table1$whisker.range <- paste(table1$min,"-",table1$max,sep="")
table1 <- subset(table1,select=c(-min,-max))
table1$quantile.80 <- round(quantile(ab_1$birdNo,probs = 0.8,na.rm=TRUE),1)
table1$quantile.LCI.UCI <- paste(round(quantile(ab_1$birdNo,probs = 0.025,na.rm=TRUE),1),"-",round(quantile(ab_1$birdNo,probs = 0.975,na.rm=TRUE),1),sep="")
table1$birdNo <- birdno
table1$MaxDay <- MaxDay

# ----------------------------- #
# Visualise within whisker range
# ----------------------------- #
boxplot(ab_1$birdNo,ylim=c(0,100),ylab = "Predicted no. birds")
title("15 birds, 151 days")
table1


# ********************************************************************** #
# END OF PROGRAM
# ********************************************************************** #





