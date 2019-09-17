#################################################################################
# Supplentary R code to support Thaxter et al. 2017. Sample Size Required to Characterize Area Use of Tracked Seabirds.
#
# Code last modified: 07/02/2017
#
# Description:
# Carry out bootstrap sampling of area usage and occupancy for increasing numbers of birds and days. 
#
# This program accepts a csv file ("birddays.csv") of four columns for:
# Animal ID, the day number from 1 to max, the grid square ID, and the 
# mean duration of time spent in the square.
# Note, this is an output from previous specific programs to this project
# based on up to four years of data per bird, that required
# prior data processing - see methods in the paper for details.
# The intention in this example is to demonstrate the bootstrapping algorithm that was used.
#
# The bootrapping code is based on the 15 bird, 151 day starting sample. The following
# steps are then carried out:
# 1. Sampling the birds 1-15 from the original with replacement (that have 151 days tracking available)
# 2. Select birds 1, 1-2, 1-3 etc from the raw data, accounting for the fact birds can be included more than once (i.e. duplicating the bird’s data the correct number of times).
# 3. Amalgamate time spent in squares across those 1, 1-2, 1-3 birds etc for cumulatively increasing numbers of 1, 1-2, 1-3…1-151 days in the sample, to give a 151 by y no. squares matrix - This is done through a cumulative sum across rows of the matrix so each square (rows) is a cumulative value across increasing numbers of days (columns)
# 4. Squares are ranked from largest to smallest for each cumulative day increment for columns of the matrix
# 5. The time spent across all squares is converted into a proportion of the maximum summed value across all squares. Thus, from largest time spent in sq x to least in square y, the proportion of the cumulative sum contribution for each square is assessed.
# 6. Step 5 is the occupancy as taken, and the number of squares meeting the desired level are then counted and summed, and then translated to actual area based on the grid cell size.
#
# This is a version of the progam used to output 1000 bootstrap samples for further analysis - no plots are made in this program
#
# NOTE: TESTED UNDER R 3.3.1 64-bit 
#################################################################################
#install.packages('data.table')
#install.packages('reshape2')

library(reshape2)
library(data.table)
library(utils)

# ********************************************************************** #
# STARTING PARAMETERS
# ********************************************************************** #
no <- 15 			# Selection of no. birds chosen - in this example = 15 birds - number of days worked out from the file

# ********************************************************************** #
# Start directory and source R code
# ********************************************************************** #
start.dir <- "E:/"
start.dir <- "F:/Gull Tracking/DECC01 gulls skuas and windfarms/papers/Sample size paper/SUBMISSION J Wild Man/FINAL EDITOR COMMENTS/Supplementary R code/"
setwd(start.dir)

# read in source code for function: "run.all.txt" and "build.dir.txt"
source("run.all.txt")
source("build.dir.txt")
  

# ********************************************************************** #
# READ IN DATA - define bird list for desired number of birds and days
# ********************************************************************** #
data.in <- read.csv("birddays.csv",sep=",")

# "data.in" can also be used to show the spans of data available for each bird
# although admittedly this could be done in many ways.
# Data spanned from just six days to up to 182, i.e. covering the
# full March to August period (for two individuals)
# In this example, 15 birds can be assessed for a maximum of 151 days

birdlist <- with(data.in,aggregate(day, by=list(Bird), FUN='max'))		# How many days / birds have we got?
totalno <- length(unique(data.in$Bird))						# total number birds in this year
birdlist2 <- birdlist[order(-birdlist$x),]	# get x birds sampled randomly up to the maximum number of birds
birdlist2$Bird <- 1:length(birdlist2$x)	
MaxDay <- birdlist2[birdlist2$Bird == no,]$x		# number of days for the maximum number of birds
Blist <- birdlist2[birdlist2$x >= MaxDay,]$Group.1	# get the list of birds with data up to this MaxDay value

# ********************************************************************** #
# Run through all samples (1:max) and lapply the runall function
# to all birds consecutively adding more to the sample
# ********************************************************************** #

r <- 1
for(r in 1:100){ # 1000 used in paper, 100 tested for this example
 	print(r)

	# ------------------------------------------------------- #	
 	# Randomly select birds for the given number desired at outset ("no")
	# also assess number of days available for this many birds
	# ------------------------------------------------------- #

	Blist <- sample(Blist,no) 				# Randomly select them up to the birds you want in the sample, because you could have some birds with the same amount of days data
	datasub <- data.in[data.in$Bird %in% Blist,]	# select these birds from the dataset 
	datasub <- datasub[datasub$day <= MaxDay,] 	# only days up to this maximum day amount for the birds WITH that amount of data or more
	datasub$NewDay <- datasub$day

	# ------------------------------------------------------- #
	# BUILD THE DIRECTORY BASED ON THESE BIRD/DAY CHOICES AND OUTPUT A NULL FILE
	# ------------------------------------------------------- #

  	subDir <- build.dir(no)
  	if (file.exists(subDir)){
  		setwd(file.path(start.dir, subDir))
		} else {
		dir.create(file.path(start.dir, subDir),showWarnings=FALSE)
		setwd(file.path(start.dir, subDir))
  	}

 	# nullify the output file and add in correct headers
	# X50 to X100, are the occupancy percentages of the cumulative distribution of time spent across all squares
	# Day = max day of the cumulative 1 to n calculation, birds = ***, it = the bootstrap iteration nnumber 

	nn1 <- c("X50","X75","X90","X95","X98","X100","Day","birds","it")
 	write.loc <- "Sim_X.csv"; 
 	write.loc <- gsub("X",r,write.loc)
 	write.table(t(nn1),file=write.loc,append=FALSE,row.names=FALSE,col.names=FALSE,sep=",")	

	# ------------------------------------------------------- #
	# BOOTSTRAP RESAMPLING OF BIRDS WITH REPLACEMENT
	# ------------------------------------------------------- #
	# create list of IDs of birds (to subset from the dataframe datasub) from 1 to "no" birds in the sample
 	data <- NULL; for(w in 1:no){data=(cbind(data,(sample(1:no))))}
 	ls2a <- sample(unique(datasub$Bird),replace = TRUE)
 	birdlist <- split(ls2a,1:length(ls2a))
 	ls <- list(); for(i in 1:no){ls[i] <- list(ls2a[1:i])}
	
	# create list of days needed in cumulative fashion
   	ls2 <- list(); for(i in 1:MaxDay){ls2[i] <- list(1:i)}	#### list of cumulative days for run through (not needed anymore)

	# ------------------------------------------------------- #
 	# run the run.all function for our selected bird list
	# ------------------------------------------------------- #

 	invisible(lapply(ls,run.all))	
}



############################## END OF PROGRAM ############################






