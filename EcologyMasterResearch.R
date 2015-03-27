#Ecology Research
#Scott Brunstein's code
#Master File

######################################################
#Outline of algorithms

# 1. Use the Prism Data to get Bioclim variables associated with each plot, using your latitude and longitude. (Default bioclim to 30 seconds, 1 km data. highest resolution)
# 
# 2. Using variables: slope, [aspect], elevation, Bioclim variables, do a multivariate analysis of the environment space .
# 
# -- aspect should probably be converted to a 0-1 variable, where 1 is north facing and 0 is south facing:  Index = Abs value ([aspect -180]/180) 9make sure that works.
#New key: "S" = 91-270, "N" = 0-90, 271-359
# DONE
# 
# - multivariate analysis should probable use Non-metric Multidimensional Scaling (NMMDS).
# 
# 3. Calculate the hypervolume of that space using the first 2-5 axes such that you capture a very high fraction of the environment space (e.g., 95%).
# 
# 4. Using a multivariate convex hull (much as if you put a rubber band around a bunch of nails scattered on a board), calculate the area of each species. 
# 
# 5. Repeat for Genus and family.
# 
# 6. Generate a distribution of species, genus, and family volumes.
# 
# 7. Generate a concentration of species within family volumes (ie, are species distributed around this volume of is the volume concentrated with some species located near the edges.

#Hypotheses:

# A. Within Family Competition and Niche Partitioning. Are species within a genus or family randomly distributed? This is as if you tossed the hypervolumes of a family into the family environment space. Now we expect a number of species in the center, and toward the margins. When we compare this to reality, do we see (a) what we expect; (b) more centralized species with few marginal ones (adaption radiation sort of answer); (c) less overlap, and hence more marginal species than we expect (intra-group niche partitioning), (PRIORITY)
# 
# B. The nature of rare species. Are species with small climatic hypervolumes (which the rare species will be by default), on average closer to the group-wide climatic margins than we would expect by chance? – (NEXT PRIORITY)
# 
# C. Diversity / specialization. Do groups with many species tend to have species with smaller niche volumes? What is the difference in the distribution of niche volumes  relative to group size? (LOWER PRIORITY)


##############################################################
#Loading in libraries

library(hypervolume)
library(grDevices)
library(rgdal)
library(raster)
library(dismo)
library(xlsx)
library(MASS)
library(ffbase)
library(maps)
library(vegan) #for metaMDS
library(geometry) #For convex hull #convhulln
library(sp)
library(alphahull)

##############################################################

#Setting working directory and loading in data into Master_File

setwd("~/Desktop/Ecology Research Project") #Setting working directory
Master_File <- read.csv("Master_File.csv", header = TRUE) #Loading in master file

##############################################################

#Obtaining variable names:

names <- colnames(Master_File)

##############################################################

#Fixing Aspect (Part two of algorithms)
AspectDegrees <- Master_File$Adj_Aspect_Degrees #1743 NA Values
AspectDegreesNew <- AspectDegrees
AspectDegreesNew[AspectDegreesNew <= 270 & AspectDegreesNew >= 91] <- "S"
AspectDegreesNew[AspectDegreesNew != "S" ] <- "N"

##############################################################

#Part  of Algorithms: Using variables: slope, [aspect], elevation, Bioclim variables, do a multivariate analysis of the environment space. This part is not going to be used for analysis, but rather to give me an idea how some of the variables are related, before performing the multivariate analysis.

#Slope vs. Elevation
Slope <- Master_File$Adjusted_Slope
Elevation <- Master_File$Elevation_ft
Slope <- Slope[!is.na(Elevation)]
Elevation <- Elevation[!is.na(Elevation)]
Elevation <- Elevation[!is.na(Slope)]
Slope <- Slope[!is.na(Slope)]
SlopeElevationCorr <- lm(Elevation~Slope)
summary(SlopeElevationCorr)
par(mfrow = c(1,2))
plot(Slope, Elevation, main = "Slope v. Elevation", xlab = "Slope", ylab = "Elevation", pch = 9, col = "blue", ylim = c(0, 15000), xlim = c(0, 150))
plot(SlopeElevationCorr$resid, ylim = c(-7000,7000), xlim = c(0,70000))
#There is no linear correlation between elevation and slope

#Slope vs Aspect
Slope <- Master_File$Adjusted_Slope
AspectDegreesNew.modified <- AspectDegreesNew[!is.na(Slope)]
Slope <- Slope[!is.na(Slope)]
Slope <- Slope[!is.na(AspectDegreesNew.modified)]
AspectDegreesNew.modified <- AspectDegreesNew.modified[!is.na(AspectDegreesNew.modified)]
AspectDegreesNew.f <- factor(AspectDegreesNew.modified)
plot(AspectDegreesNew.f, Slope)
summary(lm(Slope~AspectDegreesNew.f))
#No linear relationship between slope and aspect

#Aspect vs. Elevation
Elevation <- Master_File$Elevation_ft
AspectDegreesNew.modified <- AspectDegreesNew[!is.na(Elevation)]
Elevation <- Elevation[!is.na(Elevation)]
Elevation <- Elevation[!is.na(AspectDegreesNew.modified)]
AspectDegreesNew.modified <- AspectDegreesNew.modified[!is.na(AspectDegreesNew.modified)]
AspectDegreesNew.f <- factor(AspectDegreesNew.modified)
plot(AspectDegreesNew.f, Elevation)
summary(lm(Elevation~AspectDegreesNew.f))
#No linear relationship between slope and aspect

##############################################################

#Taking out NA values in our variables of interest

Rare <- Master_File$CNPS_Rank
Lat <- Master_File$LAT
Long <- Master_File$LONG
SpeciesCover <- Master_File$Species_Cover
ElevationFeet <- Master_File$Elevation_ft #13 NA values
Slope <- Master_File$Slope #0 NA values
AspectDegreesNew.f <- AspectDegreesNew
Family <- Master_File$Current_Family
Genus <- Master_File$Current_Genus

Rare <- Rare[!is.na(ElevationFeet)]
Lat <- Lat[!is.na(ElevationFeet)]
Long <- Long[!is.na(ElevationFeet)]
SpeciesCover <- SpeciesCover[!is.na(ElevationFeet)]
Slope <- Slope[!is.na(ElevationFeet)]
AspectDegreesNew.f <- AspectDegreesNew.f[!is.na(ElevationFeet)]
Family <- Family[!is.na(ElevationFeet)]
Genus <- Genus[!is.na(ElevationFeet)]
ElevationFeet <- ElevationFeet[!is.na(ElevationFeet)]

Rare <- Rare[!is.na(AspectDegreesNew.f)]
Lat <- Lat[!is.na(AspectDegreesNew.f)]
Long <- Long[!is.na(AspectDegreesNew.f)]
SpeciesCover <- SpeciesCover[!is.na(AspectDegreesNew.f)]
Slope <- Slope[!is.na(AspectDegreesNew.f)]
ElevationFeet <- ElevationFeet[!is.na(AspectDegreesNew.f)]
Family <- Family[!is.na(AspectDegreesNew.f)]
Genus <- Genus[!is.na(AspectDegreesNew.f)]
AspectDegreesNew.f <- AspectDegreesNew.f[!is.na(AspectDegreesNew.f)]

Rare <- Rare[!is.na(Slope)]
Lat <- Lat[!is.na(Slope)]
Long <- Long[!is.na(Slope)]
SpeciesCover <- SpeciesCover[!is.na(Slope)]
AspectDegreesNew.f <- AspectDegreesNew.f[!is.na(Slope)]
ElevationFeet <- ElevationFeet[!is.na(Slope)]
Family <- Family[!is.na(Slope)]
Genus <- Genus[!is.na(Slope)]
Slope <- Slope[!is.na(Slope)]

Rare <- Rare[!is.na(Genus)]
Lat <- Lat[!is.na(Genus)]
Long <- Long[!is.na(Genus)]
SpeciesCover <- SpeciesCover[!is.na(Genus)]
AspectDegreesNew.f <- AspectDegreesNew.f[!is.na(Genus)]
ElevationFeet <- ElevationFeet[!is.na(Genus)]
Slope <- Slope[!is.na(Genus)]
Family <- Family[!is.na(Genus)]
Genus <- Genus[!is.na(Genus)]

Rare <- Rare[!is.na(Family)]
Lat <- Lat[!is.na(Family)]
Long <- Long[!is.na(Family)]
SpeciesCover <- SpeciesCover[!is.na(Family)]
AspectDegreesNew.f <- AspectDegreesNew.f[!is.na(Family)]
ElevationFeet <- ElevationFeet[!is.na(Family)]
Slope <- Slope[!is.na(Family)]
Genus <- Genus[!is.na(Family)]
Family <- Family[!is.na(Family)]

#All variables don't have NA values now
ElevationFeet <- as.integer(ElevationFeet)

##############################################################

#NMMDS analysis
#For Aspect Degrees, South is 0 and North is 1
#Changing Aspect Degrees to numeric

AspectDegreesNew.g <- AspectDegreesNew.f
AspectDegreesNew.g <- gsub("S", 0, AspectDegreesNew.g)
AspectDegreesNew.g <- gsub("N", 1, AspectDegreesNew.g)
AspectDegreesNew.g <- as.numeric(AspectDegreesNew.g)

#Changing ElevationFeet to numeric
ElevationFeet <- as.numeric(ElevationFeet)

#Changing Slope to numeric
Slope <- as.numeric(as.character(Slope))
SpeciesCover <- as.numeric(SpeciesCover)
#5180 NA values, all coming from non-specified values

Rare <- Rare[!is.na(Slope)]
Lat <- Lat[!is.na(Slope)]
Long <- Long[!is.na(Slope)]
SpeciesCover <- SpeciesCover[!is.na(Slope)]
ElevationFeet <- ElevationFeet[!is.na(Slope)]
AspectDegreesNew.g <- AspectDegreesNew.g[!is.na(Slope)]
Family <- Family[!is.na(Slope)]
Genus <- Genus[!is.na(Slope)]
Slope <- Slope[!is.na(Slope)]

##############################################################

#Obtaining bioclim variables:
x <- c(37.73)
y <- c(-119.57)

plots <- SpatialPoints(cbind(x,y), CRS("+init=epsg:3310"))

allFiles <- list.files("~/Desktop/Ecology Research Project/bio1-19_bil", pattern = "\\.bil", full.names = TRUE)
r <- stack(allFiles)

plots_proj <- spTransform(plots, CRS=CRS("+init=epsg:4326"))
test <- extract(r, plots_proj)
1:nrow(test)
test[ , 2]
row <- rep(1:nrow(test), length(Genus))
test[row , ]
bioClimVals <- test[row , ]

##############################################################

1:nrow(test)
row <- rep(1:nrow(test), length(Genus))
bioClimVals <- test[row , ]
bioClimVals[bioClimVals < 0] <- 0
bioClimVals <- data.frame(bioClimVals)
bioClimVals[, c(1:19)] <- sapply(bioClimVals[, c(1:19)], as.numeric)

data <- data.frame(Family, Genus, AspectDegreesNew.g, ElevationFeet, Slope, SpeciesCover, bioClimVals, Lat, Long, Rare)


##############################################################

#Splitting up by family

##############################################################

#Random sample
#Play around with random sample size, see if getting different results, see if there are different effects for numbers you choose
Control <- data[sample(nrow(data), 2000),]
Control <- subset(Control, select = c(AspectDegreesNew.g, ElevationFeet, Slope, bio_1, bio_2, bio_3, bio_4, bio_5, bio_6, bio_7, bio_8, bio_9, bio_10, bio_11, bio_12, bio_13, bio_14, bio_15, bio_16, bio_17, bio_18, bio_19))
ControlMDS <- metaMDS(Control)
plot(ControlMDS, type = "n", main = "NMMDS for Fabaceae Family")
points(ControlMDS, display = c("sites"), choices = c(1,2), pch = 3)
text(ControlMDS, display = c("species"), choices = c(1,2), col = "blue", cex = 0.7)

#MANOVA 
Control <- data[sample(nrow(data), 2000),]
Control <- subset(Control, select = c(AspectDegreesNew.g, ElevationFeet, Slope, bio_1, bio_2, bio_3, bio_4, bio_5, bio_6, bio_7, bio_8, bio_9, bio_10, bio_11, bio_12, bio_13, bio_14, bio_15, bio_16, bio_17, bio_18, bio_19, Lat, Long))
Control.manova1 <- manova(cbind(Lat, Long) ~ Slope*ElevationFeet*AspectDegreesNew.g, data = Control)
Control.manova2 <- manova(cbind(Lat, Long) ~ bio_1*bio_2*bio_3*bio_4*bio_5*bio_6*bio_7*bio_8*bio_9, data = Control)
Control.manova3 <- manova(cbind(Lat, Long) ~ bio_10*bio_11*bio_12*bio_13*bio_14*bio_15*bio_16*bio_17*bio_18*bio_19, data = Control)

#Hypervolume
hvControl <- hypervolume(ControlMDS$points, reps = 1000, bandwidth = estimate_bandwidth(ControlMDS$points))
summary(hvControl)
plot(hvControl)

#ConvexHull to calculate area of  species
Control <- data[sample(nrow(data), 2000),]
chullControl <- subset(Control, select = c(SpeciesCover))
areaahull(ahull(chullControl$SpeciesCover, alpha = 2))

##############################################################

#1) Fabaceae
#Whole population was used here
Fabaceae <- subset(data, Family == "Fabaceae", select = c(AspectDegreesNew.g, ElevationFeet, Slope, bio_1, bio_2, bio_3, bio_4, bio_5, bio_6, bio_7, bio_8, bio_9, bio_10, bio_11, bio_12, bio_13, bio_14, bio_15, bio_16, bio_17, bio_18, bio_19))
FabaceaeMDS <- metaMDS(Fabaceae)
plot(FabaceaeMDS, type = "n", main = "NMMDS for Fabaceae Family")
points(FabaceaeMDS, display = c("sites"), choices = c(1,2), pch = 3)
text(FabaceaeMDS, display = c("species"), choices = c(1,2), col = "blue", cex = 0.7)

#MANOVA 
Fabaceae <- subset(data, Family == "Fabaceae", select = c(AspectDegreesNew.g, ElevationFeet, Slope, bio_1, bio_2, bio_3, bio_4, bio_5, bio_6, bio_7, bio_8, bio_9, bio_10, bio_11, bio_12, bio_13, bio_14, bio_15, bio_16, bio_17, bio_18, bio_19, Lat, Long))
Faba.manova1 <- manova(cbind(Lat, Long) ~ Slope*ElevationFeet*AspectDegreesNew.g, data = Fabaceae)
Faba.manova2 <- manova(cbind(Lat, Long) ~ bio_1*bio_2*bio_3*bio_4*bio_5*bio_6*bio_7*bio_8*bio_9, data = Fabaceae)
Faba.manova3 <- manova(cbind(Lat, Long) ~ bio_10*bio_11*bio_12*bio_13*bio_14*bio_15*bio_16*bio_17*bio_18*bio_19, data = Fabaceae)

#Hypervolume
hvFabaceae <- hypervolume(FabaceaeMDS$points, reps = 1000, bandwidth = estimate_bandwidth(FabaceaeMDS$points))
summary(hvFabaceae)
plot(hvFabaceae)

#ConvexHull to calculate area of  species
chullFaba <- subset(data, Family == "Fabaceae", select = c(SpeciesCover))
areaahull(ahull(chullFaba$SpeciesCover, alpha = 2))

##############################################################

#2) Rhamnaceae
#Whole population was used here
Rhamnaceae <- subset(data, Family == "Rhamnaceae", select = c(AspectDegreesNew.g, ElevationFeet, Slope, bio_1, bio_2, bio_3, bio_4, bio_5, bio_6, bio_7, bio_8, bio_9, bio_10, bio_11, bio_12, bio_13, bio_14, bio_15, bio_16, bio_17, bio_18, bio_19))
RhamnaceaeMDS <- metaMDS(Rhamnaceae)
plot(RhamnaceaeMDS, type = "n", main = "NMMDS for Rhamnaceae Family")
points(RhamnaceaeMDS, display = c("sites"), choices = c(1,2), pch = 3)
text(RhamnaceaeMDS, display = c("species"), choices = c(1,2), col = "blue", cex = 0.7)

#MANOVA 
Rhamnaceae <- subset(data, Family == "Rhamnaceae", select = c(AspectDegreesNew.g, ElevationFeet, Slope, bio_1, bio_2, bio_3, bio_4, bio_5, bio_6, bio_7, bio_8, bio_9, bio_10, bio_11, bio_12, bio_13, bio_14, bio_15, bio_16, bio_17, bio_18, bio_19, Lat, Long))
Rham.manova1 <- manova(cbind(Lat, Long) ~ Slope*ElevationFeet*AspectDegreesNew.g, data = Rhamnaceae)
Rham.manova2 <- manova(cbind(Lat, Long) ~ bio_1*bio_2*bio_3*bio_4*bio_5*bio_6*bio_7*bio_8*bio_9, data = Rhamnaceae)
Rham.manova3 <- manova(cbind(Lat, Long) ~ bio_10*bio_11*bio_12*bio_13*bio_14*bio_15*bio_16*bio_17*bio_18*bio_19, data = Rhamnaceae)

#Hypervolume
#Remember to compute actual bandwidth
hvRhamnaceae <- hypervolume(RhamnaceaeMDS$points, reps = 1000, bandwidth = estimate_bandwidth(RhamnaceaeMDS$points))
summary(hvRhamnaceae)
plot(hvRhamnaceae)

#ConvexHull to calculate area of  species
chullRham <- subset(data, Family == "Rhamnaceae", select = c(SpeciesCover))
areaahull(ahull(chullRham$SpeciesCover, alpha = 2))

##############################################################


#3) Ericaceae
#Whole population was used here
Ericaceae <- subset(data, Family == "Ericaceae", select = c(AspectDegreesNew.g, ElevationFeet, Slope, bio_1, bio_2, bio_3, bio_4, bio_5, bio_6, bio_7, bio_8, bio_9, bio_10, bio_11, bio_12, bio_13, bio_14, bio_15, bio_16, bio_17, bio_18, bio_19))
EricaceaeMDS <- metaMDS(Ericaceae)
plot(EricaceaeMDS, type = "n", main = "NMMDS for Ericaceae Family")
points(EricaceaeMDS, display = c("sites"), choices = c(1,2), pch = 3)
text(EricaceaeMDS, display = c("species"), choices = c(1,2), col = "blue", cex = 0.7)

#MANOVA 
Ericaceae <- subset(data, Family == "Ericaceae", select = c(AspectDegreesNew.g, ElevationFeet, Slope, bio_1, bio_2, bio_3, bio_4, bio_5, bio_6, bio_7, bio_8, bio_9, bio_10, bio_11, bio_12, bio_13, bio_14, bio_15, bio_16, bio_17, bio_18, bio_19, Lat, Long))
Eric.manova1 <- manova(cbind(Lat, Long) ~ Slope*ElevationFeet*AspectDegreesNew.g, data = Ericaceae)
Eric.manova2 <- manova(cbind(Lat, Long) ~ bio_1*bio_2*bio_3*bio_4*bio_5*bio_6*bio_7*bio_8*bio_9, data = Ericaceae)
Eric.manova3 <- manova(cbind(Lat, Long) ~ bio_10*bio_11*bio_12*bio_13*bio_14*bio_15*bio_16*bio_17*bio_18*bio_19, data = Ericaceae)

#Hypervolume
hvEricaceae <- hypervolume(EricaceaeMDS$points, reps = 1000, bandwidth = estimate_bandwidth(EricaceaeMDS$points))
summary(hvEricaceae)
plot(hvEricaceae)

#ConvexHull to calculate area of  species
chullEric <- subset(data, Family == "Ericaceae", select = c(SpeciesCover))
areaahull(ahull(chullEric$SpeciesCover, alpha = 2))

##############################################################

#4) Fagaceae
#Whole population was used here
Fagaceae <- subset(data, Family == "Fagaceae", select = c(AspectDegreesNew.g, ElevationFeet, Slope, bio_1, bio_2, bio_3, bio_4, bio_5, bio_6, bio_7, bio_8, bio_9, bio_10, bio_11, bio_12, bio_13, bio_14, bio_15, bio_16, bio_17, bio_18, bio_19))
FegaceaeMDS <- metaMDS(Fagaceae)
plot(FegaceaeMDS, type = "n", main = "NMMDS for Fagaceae Family")
points(FegaceaeMDS, display = c("sites"), choices = c(1,2), pch = 3)
text(FegaceaeMDS, display = c("species"), choices = c(1,2), col = "blue", cex = 0.7)

#MANOVA 
Fagaceae <- subset(data, Family == "Fagaceae", select = c(AspectDegreesNew.g, ElevationFeet, Slope, bio_1, bio_2, bio_3, bio_4, bio_5, bio_6, bio_7, bio_8, bio_9, bio_10, bio_11, bio_12, bio_13, bio_14, bio_15, bio_16, bio_17, bio_18, bio_19, Lat, Long))
Faga.manova1 <- manova(cbind(Lat, Long) ~ Slope*ElevationFeet*AspectDegreesNew.g, data = Fagaceae)
Faga.manova2 <- manova(cbind(Lat, Long) ~ bio_1*bio_2*bio_3*bio_4*bio_5*bio_6*bio_7*bio_8*bio_9, data = Fagaceae)
Faga.manova3 <- manova(cbind(Lat, Long) ~ bio_10*bio_11*bio_12*bio_13*bio_14*bio_15*bio_16*bio_17*bio_18*bio_19, data = Fagaceae)

hvFegaceae <- hypervolume(FegaceaeMDS$points, reps = 1000, bandwidth = estimate_bandwidth(FegaceaeMDS$points))
summary(hvFegaceae)
plot(hvFegaceae)

#ConvexHull to calculate area of  species
chullFega <- subset(data, Family == "Fagaceae", select = c(SpeciesCover))
areaahull(ahull(chullFega$SpeciesCover, alpha = 2))

##############################################################

#5) Orchidaceae - done
#Whole population was used here
Orchidaceae <- subset(data, Family == "Orchidaceae", select = c(AspectDegreesNew.g, ElevationFeet, Slope, bio_1, bio_2, bio_3, bio_4, bio_5, bio_6, bio_7, bio_8, bio_9, bio_10, bio_11, bio_12, bio_13, bio_14, bio_15, bio_16, bio_17, bio_18, bio_19))
OrchidaceaeMDS <- metaMDS(Orchidaceae)
plot(OrchidaceaeMDS, type = "n", main = "NMMDS for Orchidaceae Family")
points(OrchidaceaeMDS, display = c("sites"), choices = c(1,2), pch = 3)
text(OrchidaceaeMDS, display = c("species"), choices = c(1,2), col = "blue", cex = 0.7)


#MANOVA 
Orchidaceae <- subset(data, Family == "Orchidaceae", select = c(AspectDegreesNew.g, ElevationFeet, Slope, bio_1, bio_2, bio_3, bio_4, bio_5, bio_6, bio_7, bio_8, bio_9, bio_10, bio_11, bio_12, bio_13, bio_14, bio_15, bio_16, bio_17, bio_18, bio_19, Lat, Long))
Orch.manova1 <- manova(cbind(Lat, Long) ~ Slope*ElevationFeet*AspectDegreesNew.g, data = Orchidaceae)
Orch.manova2 <- manova(cbind(Lat, Long) ~ bio_1*bio_2*bio_3*bio_4*bio_5*bio_6*bio_7*bio_8*bio_9, data = Orchidaceae)
Orch.manova3 <- manova(cbind(Lat, Long) ~ bio_10*bio_11*bio_12*bio_13*bio_14*bio_15*bio_16*bio_17*bio_18*bio_19, data = Orchidaceae)

hvOrchidaceae <- hypervolume(OrchidaceaeMDS$points, reps = 1000, bandwidth = estimate_bandwidth(OrchidaceaeMDS$points))
summary(hvOrchidaceae)
plot(hvOrchidaceae)

#ConvexHull to calculate area of  species
chullOrch <- subset(data, Family == "Orchidaceae", select = c(SpeciesCover))
areaahull(ahull(chullOrch$SpeciesCover, alpha = 2))

##############################################################

#6) Rosaceae
Rosaceae <- subset(data, Family == "Rosaceae", select = c(AspectDegreesNew.g, ElevationFeet, Slope, bio_1, bio_2, bio_3, bio_4, bio_5, bio_6, bio_7, bio_8, bio_9, bio_10, bio_11, bio_12, bio_13, bio_14, bio_15, bio_16, bio_17, bio_18, bio_19))
RosaceaeMDS <- metaMDS(Rosaceae)
plot(RosaceaeMDS, type = "n", main = "NMMDS for Rosaceae Family")
points(RosaceaeMDS, display = c("sites"), choices = c(1,2), pch = 3)
text(RosaceaeMDS, display = c("species"), choices = c(1,2), col = "blue", cex = 0.7)

#MANOVA 
Rosaceae <- subset(data, Family == "Rosaceae", select = c(AspectDegreesNew.g, ElevationFeet, Slope, bio_1, bio_2, bio_3, bio_4, bio_5, bio_6, bio_7, bio_8, bio_9, bio_10, bio_11, bio_12, bio_13, bio_14, bio_15, bio_16, bio_17, bio_18, bio_19, Lat, Long))
Rosa.manova1 <- manova(cbind(Lat, Long) ~ Slope*ElevationFeet*AspectDegreesNew.g, data = Rosaceae)
Rosa.manova2 <- manova(cbind(Lat, Long) ~ bio_1*bio_2*bio_3*bio_4*bio_5*bio_6*bio_7*bio_8*bio_9, data = Rosaceae)
Rosa.manova3 <- manova(cbind(Lat, Long) ~ bio_10*bio_11*bio_12*bio_13*bio_14*bio_15*bio_16*bio_17*bio_18*bio_19, data = Rosaceae)


hvRosaceae <- hypervolume(RosaceaeMDS$points, reps = 1000, bandwidth = estimate_bandwidth(RosaceaeMDS$points))
summary(hvRosaceae)
plot(hvRosaceae)

#ConvexHull to calculate area of  species
chullRosa <- subset(data, Family == "Rosaceae", select = c(SpeciesCover))
areaahull(ahull(chullRosa$SpeciesCover, alpha = 2))

##############################################################

#Plotting all MDS values for Families of interest
par(mfrow = c(2,3))

plot(FabaceaeMDS, type = "n", main = "NMMDS for Fabaceae Family")
points(FabaceaeMDS, display = c("sites"), choices = c(1,2), pch = 3)
text(FabaceaeMDS, display = c("species"), choices = c(1,2), col = "blue", cex = 0.7)
plot(RhamnaceaeMDS, type = "n", main = "NMMDS for Rhamnaceae Family")
points(RhamnaceaeMDS, display = c("sites"), choices = c(1,2), pch = 3)
text(RhamnaceaeMDS, display = c("species"), choices = c(1,2), col = "blue", cex = 0.7)
plot(EricaceaeMDS, type = "n", main = "NMMDS for Ericaceae Family")
points(EricaceaeMDS, display = c("sites"), choices = c(1,2), pch = 3)
text(EricaceaeMDS, display = c("species"), choices = c(1,2), col = "blue", cex = 0.7)
plot(FegaceaeMDS, type = "n", main = "NMMDS for Fegaceae Family")
points(FegaceaeMDS, display = c("sites"), choices = c(1,2), pch = 3)
text(FegaceaeMDS, display = c("species"), choices = c(1,2), col = "blue", cex = 0.7)
plot(OrchidaceaeMDS, type = "n", main = "NMMDS for Orchidaceae Family")
points(OrchidaceaeMDS, display = c("sites"), choices = c(1,2), pch = 3)
text(OrchidaceaeMDS, display = c("species"), choices = c(1,2), col = "blue", cex = 0.7)
plot(RosaceaeMDS, type = "n", main = "NMMDS for Rosaceae Family")
points(RosaceaeMDS, display = c("sites"), choices = c(1,2), pch = 3)
text(RosaceaeMDS, display = c("species"), choices = c(1,2), col = "blue", cex = 0.7)
par(mfrow = c(1,1))

##############################################################

#Repeat everything for the following Genus families:

##############################################################

#Quercus

Quercus <- subset(data, Genus == "Quercus", select = c(AspectDegreesNew.g, ElevationFeet, Slope, bio_1, bio_2, bio_3, bio_4, bio_5, bio_6, bio_7, bio_8, bio_9, bio_10, bio_11, bio_12, bio_13, bio_14, bio_15, bio_16, bio_17, bio_18, bio_19))
QuercusMDS <- metaMDS(Quercus)
plot(QuercusMDS, type = "n", main = "NMMDS for Quercus Genus")
points(QuercusMDS, display = c("sites"), choices = c(1,2), pch = 3)
text(QuercusMDS, display = c("species"), choices = c(1,2), col = "blue", cex = 0.7)

#MANOVA 
Quercus <- subset(data, Genus == "Quercus", select = c(AspectDegreesNew.g, ElevationFeet, Slope, bio_1, bio_2, bio_3, bio_4, bio_5, bio_6, bio_7, bio_8, bio_9, bio_10, bio_11, bio_12, bio_13, bio_14, bio_15, bio_16, bio_17, bio_18, bio_19, Lat, Long))
Quer.manova1 <- manova(cbind(Lat, Long) ~ Slope*ElevationFeet*AspectDegreesNew.g, data = Quercus)
Quer.manova2 <- manova(cbind(Lat, Long) ~ bio_1*bio_2*bio_3*bio_4*bio_5*bio_6*bio_7*bio_8*bio_9, data = Quercus)
Quer.manova3 <- manova(cbind(Lat, Long) ~ bio_10*bio_11*bio_12*bio_13*bio_14*bio_15*bio_16*bio_17*bio_18*bio_19, data = Quercus)

hvQuercus <- hypervolume(QuercusMDS$points, reps = 1000, bandwidth = estimate_bandwidth(QuercusMDS$points))
summary(hvQuercus)
plot(hvQuercus)

#ConvexHull to calculate area of  species
chullQuer <- subset(data, Genus == "Quercus", select = c(SpeciesCover))
areaahull(ahull(chullQuer$SpeciesCover, alpha = 2))

##############################################################

#Galium

Galium <- subset(data, Genus == "Galium", select = c(AspectDegreesNew.g, ElevationFeet, Slope, bio_1, bio_2, bio_3, bio_4, bio_5, bio_6, bio_7, bio_8, bio_9, bio_10, bio_11, bio_12, bio_13, bio_14, bio_15, bio_16, bio_17, bio_18, bio_19))
GaliumMDS <- metaMDS(Galium)
plot(GaliumMDS, type = "n", main = "NMMDS for Galium Genus")
points(GaliumMDS, display = c("sites"), choices = c(1,2), pch = 3)
text(GaliumMDS, display = c("species"), choices = c(1,2), col = "blue", cex = 0.7)

#MANOVA 
Galium <- subset(data, Genus == "Galium", select = c(AspectDegreesNew.g, ElevationFeet, Slope, bio_1, bio_2, bio_3, bio_4, bio_5, bio_6, bio_7, bio_8, bio_9, bio_10, bio_11, bio_12, bio_13, bio_14, bio_15, bio_16, bio_17, bio_18, bio_19, Lat, Long))
Gali.manova1 <- manova(cbind(Lat, Long) ~ Slope*ElevationFeet*AspectDegreesNew.g, data = Galium)
Gali.manova2 <- manova(cbind(Lat, Long) ~ bio_1*bio_2*bio_3*bio_4*bio_5*bio_6*bio_7*bio_8*bio_9, data = Galium)
Gali.manova3 <- manova(cbind(Lat, Long) ~ bio_10*bio_11*bio_12*bio_13*bio_14*bio_15*bio_16*bio_17*bio_18*bio_19, data = Galium)

hvGalium <- hypervolume(GaliumMDS$points, reps = 1000, bandwidth = estimate_bandwidth(GaliumMDS$points))
summary(hvGalium)
plot(hvGalium)

#ConvexHull to calculate area of  species
chullGali <- subset(data, Genus == "Galium", select = c(SpeciesCover))
areaahull(ahull(chullGali$SpeciesCover, alpha = 2))
##############################################################

#Ceanothus

Ceanothus <- subset(data, Genus == "Ceanothus", select = c(AspectDegreesNew.g, ElevationFeet, Slope, bio_1, bio_2, bio_3, bio_4, bio_5, bio_6, bio_7, bio_8, bio_9, bio_10, bio_11, bio_12, bio_13, bio_14, bio_15, bio_16, bio_17, bio_18, bio_19))
CeanothusMDS <- metaMDS(Ceanothus)
plot(CeanothusMDS, type = "n", main = "NMMDS for Ceanothus Genus")
points(CeanothusMDS, display = c("sites"), choices = c(1,2), pch = 3)
text(CeanothusMDS, display = c("species"), choices = c(1,2), col = "blue", cex = 0.7)

#MANOVA 
Ceanothus <- subset(data, Genus == "Ceanothus", select = c(AspectDegreesNew.g, ElevationFeet, Slope, bio_1, bio_2, bio_3, bio_4, bio_5, bio_6, bio_7, bio_8, bio_9, bio_10, bio_11, bio_12, bio_13, bio_14, bio_15, bio_16, bio_17, bio_18, bio_19, Lat, Long))
Cean.manova1 <- manova(cbind(Lat, Long) ~ Slope*ElevationFeet*AspectDegreesNew.g, data = Ceanothus)
Cean.manova2 <- manova(cbind(Lat, Long) ~ bio_1*bio_2*bio_3*bio_4*bio_5*bio_6*bio_7*bio_8*bio_9, data = Ceanothus)
Cean.manova3 <- manova(cbind(Lat, Long) ~ bio_10*bio_11*bio_12*bio_13*bio_14*bio_15*bio_16*bio_17*bio_18*bio_19, data = Ceanothus)

hvCeanothus <- hypervolume(CeanothusMDS$points, reps = 1000, bandwidth = estimate_bandwidth(CeanothusMDS$points))
summary(hvCeanothus)
plot(hvCeanothus)

#ConvexHull to calculate area of  species
chullCean <- subset(data, Genus == "Ceanothus", select = c(SpeciesCover))
areaahull(ahull(chullCean$SpeciesCover, alpha = 2))
##############################################################

#Eriophyllum

Eriophyllum <- subset(data, Genus == "Eriophyllum", select = c(AspectDegreesNew.g, ElevationFeet, Slope, bio_1, bio_2, bio_3, bio_4, bio_5, bio_6, bio_7, bio_8, bio_9, bio_10, bio_11, bio_12, bio_13, bio_14, bio_15, bio_16, bio_17, bio_18, bio_19))
EriophyllumMDS <- metaMDS(Eriophyllum)
plot(EriophyllumMDS, type = "n", main = "NMMDS for Eriophyllum Genus")
points(EriophyllumMDS, display = c("sites"), choices = c(1,2), pch = 3)
text(EriophyllumMDS, display = c("species"), choices = c(1,2), col = "blue", cex = 0.7)

#MANOVA 
Eriophyllum <- subset(data, Genus == "Eriophyllum", select = c(AspectDegreesNew.g, ElevationFeet, Slope, bio_1, bio_2, bio_3, bio_4, bio_5, bio_6, bio_7, bio_8, bio_9, bio_10, bio_11, bio_12, bio_13, bio_14, bio_15, bio_16, bio_17, bio_18, bio_19, Lat, Long))
Erio.manova1 <- manova(cbind(Lat, Long) ~ Slope*ElevationFeet*AspectDegreesNew.g, data = Eriophyllum)
Erio.manova2 <- manova(cbind(Lat, Long) ~ bio_1*bio_2*bio_3*bio_4*bio_5*bio_6*bio_7*bio_8*bio_9, data = Eriophyllum)
Erio.manova3 <- manova(cbind(Lat, Long) ~ bio_10*bio_11*bio_12*bio_13*bio_14*bio_15*bio_16*bio_17*bio_18*bio_19, data = Eriophyllum)

hvEriophyllum <- hypervolume(EriophyllumMDS$points, reps = 1000, bandwidth = estimate_bandwidth(EriophyllumMDS$points))
summary(hvEriophyllum)
plot(hvEriophyllum)

#ConvexHull to calculate area of  species
chullErio <- subset(data, Genus == "Eriophyllum", select = c(SpeciesCover))
areaahull(ahull(chullErio$SpeciesCover, alpha = 2))

##############################################################

#Lupinus

Lupinus <- subset(data, Genus == "Lupinus", select = c(AspectDegreesNew.g, ElevationFeet, Slope, bio_1, bio_2, bio_3, bio_4, bio_5, bio_6, bio_7, bio_8, bio_9, bio_10, bio_11, bio_12, bio_13, bio_14, bio_15, bio_16, bio_17, bio_18, bio_19))
LupinusMDS <- metaMDS(Lupinus)
plot(LupinusMDS, type = "n", main = "NMMDS for Lupinus Genus")
points(LupinusMDS, display = c("sites"), choices = c(1,2), pch = 3)
text(LupinusMDS, display = c("species"), choices = c(1,2), col = "blue", cex = 0.7)

#MANOVA 
Lupinus <- subset(data, Genus == "Lupinus", select = c(AspectDegreesNew.g, ElevationFeet, Slope, bio_1, bio_2, bio_3, bio_4, bio_5, bio_6, bio_7, bio_8, bio_9, bio_10, bio_11, bio_12, bio_13, bio_14, bio_15, bio_16, bio_17, bio_18, bio_19, Lat, Long))
Lupi.manova1 <- manova(cbind(Lat, Long) ~ Slope*ElevationFeet*AspectDegreesNew.g, data = Lupinus)
Lupi.manova2 <- manova(cbind(Lat, Long) ~ bio_1*bio_2*bio_3*bio_4*bio_5*bio_6*bio_7*bio_8*bio_9, data = Lupinus)
Lupi.manova3 <- manova(cbind(Lat, Long) ~ bio_10*bio_11*bio_12*bio_13*bio_14*bio_15*bio_16*bio_17*bio_18*bio_19, data = Lupinus)

hvLupinus <- hypervolume(LupinusMDS$points, reps = 1000, bandwidth = estimate_bandwidth(LupinusMDS$points))
summary(hvLupinus)
plot(hvLupinus)

#ConvexHull to calculate area of  species
chullLupi <- subset(data, Genus == "Lupinus", select = c(SpeciesCover))
areaahull(ahull(chullLupi$SpeciesCover, alpha = 2))
##############################################################

#Carex

Carex <- subset(data, Genus == "Carex", select = c(AspectDegreesNew.g, ElevationFeet, Slope, bio_1, bio_2, bio_3, bio_4, bio_5, bio_6, bio_7, bio_8, bio_9, bio_10, bio_11, bio_12, bio_13, bio_14, bio_15, bio_16, bio_17, bio_18, bio_19))
CarexMDS <- metaMDS(Carex)
plot(CarexMDS, type = "n", main = "NMMDS for Carex Genus")
points(CarexMDS, display = c("sites"), choices = c(1,2), pch = 3)
text(CarexMDS, display = c("species"), choices = c(1,2), col = "blue", cex = 0.7)

#MANOVA 
Carex <- subset(data, Genus == "Carex", select = c(AspectDegreesNew.g, ElevationFeet, Slope, bio_1, bio_2, bio_3, bio_4, bio_5, bio_6, bio_7, bio_8, bio_9, bio_10, bio_11, bio_12, bio_13, bio_14, bio_15, bio_16, bio_17, bio_18, bio_19, Lat, Long))
Carex.manova1 <- manova(cbind(Lat, Long) ~ Slope*ElevationFeet*AspectDegreesNew.g, data = Carex)
Carex.manova2 <- manova(cbind(Lat, Long) ~ bio_1*bio_2*bio_3*bio_4*bio_5*bio_6*bio_7*bio_8*bio_9, data = Carex)
Carex.manova3 <- manova(cbind(Lat, Long) ~ bio_10*bio_11*bio_12*bio_13*bio_14*bio_15*bio_16*bio_17*bio_18*bio_19, data = Carex)

hvCarex <- hypervolume(CarexMDS$points, reps = 1000, bandwidth = estimate_bandwidth(CarexDS$points))
summary(hvCarex)
plot(hvCarex)

#ConvexHull to calculate area of  species
chullCarex <- subset(data, Genus == "Carex", select = c(SpeciesCover))
areaahull(ahull(chullCarex$SpeciesCover, alpha = 2))
##############################################################

#Plotting all NMMDS Genuses

par(mfrow = c(2,3))
plot(QuercusMDS, type = "n", main = "NMMDS for Quercus Genus")
points(QuercusMDS, display = c("sites"), choices = c(1,2), pch = 3)
text(QuercusMDS, display = c("species"), choices = c(1,2), col = "blue", cex = 0.7)
plot(GaliumMDS, type = "n", main = "NMMDS for Galium Genus")
points(GaliumMDS, display = c("sites"), choices = c(1,2), pch = 3)
text(GaliumMDS, display = c("species"), choices = c(1,2), col = "blue", cex = 0.7)
plot(CeanothusMDS, type = "n", main = "NMMDS for Ceanothus Genus")
points(CeanothusMDS, display = c("sites"), choices = c(1,2), pch = 3)
text(CeanothusMDS, display = c("species"), choices = c(1,2), col = "blue", cex = 0.7)
plot(EriophyllumMDS, type = "n", main = "NMMDS for Eriophyllum Genus")
points(EriophyllumMDS, display = c("sites"), choices = c(1,2), pch = 3)
text(EriophyllumMDS, display = c("species"), choices = c(1,2), col = "blue", cex = 0.7)
plot(LupinusMDS, type = "n", main = "NMMDS for Lupinus Genus")
points(LupinusMDS, display = c("sites"), choices = c(1,2), pch = 3)
text(LupinusMDS, display = c("species"), choices = c(1,2), col = "blue", cex = 0.7)
plot(CarexMDS, type = "n", main = "NMMDS for Carex Genus")
points(CarexMDS, display = c("sites"), choices = c(1,2), pch = 3)
text(CarexMDS, display = c("species"), choices = c(1,2), col = "blue", cex = 0.7)
par(mfrow = c(1,1))

#################################################################

#Choose CNPS_Rank because there are more observations

Rare <- subset(Master_File, CNPS_Rank != "NA")

Rare1 <- subset(data, Rare != "NA", c(AspectDegreesNew.g, ElevationFeet, Slope, bio_1, bio_2, bio_3, bio_4, bio_5, bio_6, bio_7, bio_8, bio_9, bio_10, bio_11, bio_12, bio_13, bio_14, bio_15, bio_16, bio_17, bio_18, bio_19))
Rare1MDS <- metaMDS(Rare1)
plot(Rare1MDS, type = "n", main = "NMMDS for Rare1 Genus")
points(Rare1MDS, display = c("sites"), choices = c(1,2), pch = 3)
text(Rare1MDS, display = c("species"), choices = c(1,2), col = "blue", cex = 0.7)

#MANOVA 
Rare1 <- subset(data, Rare != "NA", select = c(AspectDegreesNew.g, ElevationFeet, Slope, bio_1, bio_2, bio_3, bio_4, bio_5, bio_6, bio_7, bio_8, bio_9, bio_10, bio_11, bio_12, bio_13, bio_14, bio_15, bio_16, bio_17, bio_18, bio_19, Lat, Long))
Rare1.manova1 <- manova(cbind(Lat, Long) ~ Slope*ElevationFeet*AspectDegreesNew.g, data = Rare1)
Rare1.manova2 <- manova(cbind(Lat, Long) ~ bio_1*bio_2*bio_3*bio_4*bio_5*bio_6*bio_7*bio_8*bio_9, data = Rare1)
Rare1.manova3 <- manova(cbind(Lat, Long) ~ bio_10*bio_11*bio_12*bio_13*bio_14*bio_15*bio_16*bio_17*bio_18*bio_19, data = Rare1)

hvRare1 <- hypervolume(Rare1MDS$points, reps = 1000, bandwidth = estimate_bandwidth(Rare1MDS$points))
summary(hvRare1)
plot(hvRare1)

#ConvexHull to calculate area of  species
chullRare1 <- subset(data, Rare != "NA", select = c(SpeciesCover))
areaahull(ahull(chullRare1$SpeciesCover, alpha = 2))
