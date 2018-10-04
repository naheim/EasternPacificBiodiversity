# set root base for proper file direction
init.dir <- getwd()
if(is.element(Sys.info()["nodename"], c("es-naheim.local","sr12-cf96e71ca1.stanford.edu"))) {
	my.root <- "/Volumes/Blastoid/noelheim_data"
} else {
	my.root <- "/Users/noelheim"
}
Sys.setenv(TZ="America/Los_Angeles")

source(paste(my.root,"/Box Sync/Includes/myFunctions.r", sep=""));
setwd(paste(my.root,"/Box Sync/git/snapshotcalcoast", sep=""));

library(vegan)
library(maps)
require(rgdal)
require(rgeos)

# mapping info 
crswgs84 <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs") # map projection
bayCounties <- c('Sonoma','Marin','Napa','Contra Costa','Solano','San Francisco','Alameda','San Mateo','Santa Clara')
coastCounties <- c('Del Norte','Humboldt','Mendocino',bayCounties,'Santa Cruz','Monterey','San Luis Obispo','Santa Barbara','Ventura','Los Angeles','Orange','San Diego')
focalCounties <- c("San Diego","Orange","Los Angeles","Monterey","Santa Cruz","San Mateo","Marin","Sonoma","Humboldt")


## load maps
cal <- readOGR(dsn = "data files/cb_2017_us_state_500k/", layer = "cb_2017_us_state_500k")
cal <- cal[cal$NAME == 'California',]
cal <- spTransform(cal, crswgs84)

counties <- readOGR(dsn = "data files/CA_Counties/", layer = "CA_Counties_TIGER2016")
counties <- spTransform(counties, crswgs84)

mpas <- readOGR(dsn = "data files/CA_MPA/", layer = "California_Marine_Protected_Areas_[ds582]")
mpas <- spTransform(mpas, crswgs84)

# extract meaningful groups of counties
bayArea <- counties[is.element(counties$NAME, bayCounties),]
coastalCounties <- counties[is.element(counties$NAME, coastCounties),]

# load in observation
load("data files/calCoastObs.RData")
# transform coordinates to spatial objects
coordinates(calCoast) <- ~ lng + lat
proj4string(calCoast) <- CRS("+proj=longlat")
calCoast <- spTransform(calCoast, crswgs84)

# Add Valentine's (1966) bioprovinces
provinces <- data.frame(t(data.frame(
	#oregonian.columbian = c(48, 55),
	oregonian.mendocinian = c(37, 48),
	oregonian.montereyan = c(34.45, 37), # first point is Point Conception
	californian = c(27.8, 34.45)
))); colnames(provinces) <- c('southern','northern')

calCoast$province <- NA
for(i in 1:nrow(provinces)) {
	calCoast$province[calCoast$lat >= provinces$southern[i] & calCoast$lat < provinces$northern[i]] <- rownames(provinces)[i]
}
calCoast$province <- factor(calCoast$province, levels=rev(rownames(provinces)))

# add county
calCoast$county <- over(calCoast, counties)$NAME
calCoast$county <- factor(calCoast$county, levels = rev(coastCounties))

# assign nearest county to observations outside of official county boundaries
##  First project data into a planar coordinate system (here UTM zone 32)
utmStr <- "+proj=utm +zone=%d +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"
crsUTM32 <- CRS(sprintf(utmStr, 32))
countiesUTM <- spTransform(coastalCounties, crsUTM32)

unassigned <- subset(calCoast, is.na(county))
unassignedUTM <- spTransform(unassigned, crsUTM32)
## For each point, find name of nearest polygon (in this case, Belgian cantons)
for (i in 1:nrow(unassigned)) {
    calCoast$county[calCoast$id == unassigned$id[i]] <- countiesUTM$NAME[which.min(gDistance(unassignedUTM[i,], countiesUTM, byid=TRUE))]
}


# subset to species
calCoast <- subset(calCoast, !is.na(species_id))
minDiv <- length(unique(calCoast$name[calCoast$year == 2016]))

sampleSize <- table(calCoast$county, calCoast$year)
sampleSize <- sampleSize[apply(sampleSize,1,min) >= 100,]

rarefiedDiversity <- data.frame(matrix(NA, nrow=nrow(sampleSize), ncol=ncol(sampleSize), dimnames=dimnames(sampleSize)))
rarefiedDiversityEqSamp <- rarefiedDiversity
for(i in 1:nrow(sampleSize)) {
	minSample <- min(sampleSize[i,])
	sampleMatrix <- table(calCoast$year[calCoast$county==rownames(sampleSize)[i]], calCoast$species_id[calCoast$county==rownames(sampleSize)[i]])
	rarefied <- rarefy(sampleMatrix, minSample)	
	rarefiedDiversity[i,] <- as.numeric(rarefied)
	rarefied <- rarefy(sampleMatrix, min(sampleSize))
	rarefiedDiversityEqSamp[i,] <- as.numeric(rarefied)
}

plotColors <- c('blue','lightblue')
pdf(file="figures/countyRarefaction.pdf", height=10, width=8)
layout(matrix(c(rep(nrow(sampleSize)+2,nrow(sampleSize)),(nrow(sampleSize)+1):2,rep(1,nrow(sampleSize))), nrow=nrow(sampleSize), ncol=3, byrow=FALSE), widths=c(0.1, 0.3, 0.6))
plot(coastalCounties, border=NA)
for(i in 1:nrow(sampleSize)) {
	plot(coastalCounties[coastalCounties$NAME == rownames(sampleSize)[i],], col=plotColors[i %% 2 + 1], border=NA, add=TRUE)
}
plot(coastalCounties, add=TRUE)

for(i in 1:nrow(sampleSize)) {
	par(las=1, mar=c(3, 2, 1.5, 0) + 0.1, cex.axis=1.25, cex.lab=1.25)
	if(i == 1) {
		xlabels <- 2016:2018
	} else {
		xlabels <- NA
	}
	barplot(as.numeric(rarefiedDiversityEqSamp[i,]), names.arg=xlabels, col=plotColors[i %% 2 + 1], main=rownames(sampleSize)[i], ylab="", ylim=c(0,max(rarefiedDiversityEqSamp)), yaxt="n")
	axis(side=2, at=c(0,50,100))
}

par(mar=c(0, 0, 0, 0) + 0.1)
plot(1:10, type="n", axes=FALSE, xlab="",ylab="")
text(5,5, "Estimated number of Species", cex=1.25, srt=90)
dev.off()














