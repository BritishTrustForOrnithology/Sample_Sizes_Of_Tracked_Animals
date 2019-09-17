#################################################################################
# Supplentary R code to support Thaxter et al. 2017. Sample Size Required to Characterize Area Use of Tracked Seabirds
#
# Code last modified: 05/01/2017
#
# Description:
# Carry out bootstrap sampling of area usage and occupancy for increasing numbers of birds and days. 
#
# This is a version of the program: "JWMThaxter_et_al_sm_RScript1.Bootstrapping.R"
# and produces graphical examples for one bootstrap.
# Note: All plots are saved to file.
#
#################################################################################

#install.packages('sp')
#install.packages('shapefiles')
#install.packages('maptools')
#install.packages('PBSmapping')
#install.packages('maps')
#install.packages('RColorBrewer')
#install.packages('proj4')
#install.packages('grid')
#install.packages('raster')
#install.packages('rgeos')

# For data manipulation
library(reshape2); library(data.table); library(utils)
# For mapping and plotting
library(lattice); library(foreign)
library(shapefiles); library(sp)
library(maptools); library(PBSmapping)
library(maps); library(RColorBrewer)
library(proj4); library(grid)
library(raster); library(rgeos)

# ********************************************************************** #
# Start directory and source functions
# ********************************************************************** #
start.dir <- "E:/"

# read in source code for function: "plot.one.sample.txt"
setwd(start.dir)
source("plot.one.sample.txt")


# ********************************************************************** #
# READ IN SHAPEFILE OF UK COASTLINE - UK BNG format 
# ********************************************************************** #
path1 <- "BI_countries approxGBprj_labelled"; 
proj4stringA <- CRS("+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 +ellps=airy +datum=OSGB36 +units=m +no_defs")
UKshp <- readShapeSpatial(path1); proj4string(UKshp) <- proj4stringA

# GRID EXTENT
xx1<-440000; xx2<-810000
yy1<-120000; yy2<-410000

# Create polygon raster for plotting - UK BNG
r <- raster(nrow=40, ncol=80, xmn=xx1, xmx=xx2, ymn=yy1, ymx = yy2, crs=proj4stringA) ### create a raster at the extent you want
r[] <- rep(0,ncell(r))		# add ficticious data to start
res(r) <- 2000			# set a suitable reolution - 2 km
pol <- rasterToPolygons(r)	# create a polygon from the raster
pol@data$layer <- 1:length(pol@data$layer)

# ********************************************************************** #
# STARTING PARAMETERS
# ********************************************************************** #
# For further annotated details of this start point see program "BootstrappingSimple.R"
# This version of the program builds no new directories.

no <- 15 			# Selection of no. birds chosen - in this example = 15 birds - number of days worked out from the file

# ********************************************************************** #
# READ IN DATA - define bird list for desired number of birds and days
# ********************************************************************** #

data.in <- read.csv("birddays.csv",sep=",")
birdlist <- with(data.in,aggregate(day, by=list(Bird), FUN='max'))		# How many days / birds have we got?
totalno <- length(unique(data.in$Bird))						# total number birds in this year
birdlist2 <- birdlist[order(-birdlist$x),]	# get x birds sampled randomly up to the maximum number of birds
birdlist2$Bird <- 1:length(birdlist2$x)	
MaxDay <- birdlist2[birdlist2$Bird == no,]$x		# number of days for the maximum number of birds
Blist <- birdlist2[birdlist2$x >= MaxDay,]$Group.1	# get the list of birds with data up to this MaxDay value

# Get the birds from which we are sampling, select no of days e.g. 15 birds up to 151 days
Blist <- sample(Blist,no) 				# Randomly select them up to the birds you want in the sample, because you could have some birds with the same amount of days data
datasub <- data.in[data.in$Bird %in% Blist,]	# select these birds from the dataset 
datasub <- datasub[datasub$day <= MaxDay,] 	# only days up to this maximum day amount for the birds WITH that amount of data or more
datasub$NewDay <- datasub$day


# ******************************************************************** #
# create list of IDs of birds (to subset from the dataframe datasub) from 1 to "no" birds in the sample
data <- NULL; for(w in 1:no){data=(cbind(data,(sample(1:no))))}
ls2a <- sample(unique(datasub$Bird),replace = TRUE)
birdlist <- split(ls2a,1:length(ls2a))
ls <- list(); for(i in 1:no){ls[i] <- list(ls2a[1:i])}
# ******************************************************************** #

	
# ******************************************************************** #
# run through increasing numbers of birds - cumulative = TRUE
# ******************************************************************** #
# Note: maps output to file
for(i in 1:15){
	plot.one.sample(i,cumul=TRUE) # plots saved to file
}

# ******************************************************************** #
# PLOTTING INDIVIDUAL BIRDS TO SHOW THEIR DISTRIBUTIONS 
# ******************************************************************** #
# Note: maps output to file
# Create new list of individual birds included in this example

ls3 <- list(); for(j in 1:length(unique(ls[[15]]))){ls3[j] <- unique(ls[[15]])[j]}
for(i in 1:length(unique(ls[[15]]))){
	plot.one.sample(i,cumul=FALSE) # plots saved to file
}

# ******************************************************************** #
# Remember the iteration sample of birds
# ******************************************************************** #
sink("sample.txt")
ls
sink()

############################## END OF PROGRAM ############################

