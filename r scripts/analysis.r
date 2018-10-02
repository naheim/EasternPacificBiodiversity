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

crswgs84 <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs") # map projection
bayCounties <- c('Marin','Sonoma','San Mateo','Napa','Alameda','Contra Costa','Solano','San Francisco','Santa Clara')
coastCounties <- c(bayCounties, 'Del Norte','Humboldt','Mendocino','Santa Cruz','Monterey','San Luis Obispo','Santa Barbara','Ventura','Los Angeles','Orange','San Diego')

load("data files/calCoastObs.RData")
calCoast$lng <- as.numeric(calCoast$lng)
calCoast$lat <- as.numeric(calCoast$lat)
# transform coordinates to spatial objects
coordinates(calCoast) <- ~ lng + lat
proj4string(calCoast) <- CRS("+proj=longlat")
calCoast <- spTransform(calCoast, proj4string(counties))

calCoast <- subset(calCoast, rank == 'species')
minDiv <- length(unique(calCoast$name[calCoast$year == 2016]))
sampleMatrix <- table(calCoast$year, calCoast$taxon_id)

rarefied <- rarecurve(sampleMatrix, 1)
dev.off()

sampleYears <- 2016:2018
quartz(height=5, width=15)
par(pch=16, mfrow=c(1,3))
for(i in 1:3) {
	map('state','California')
	points(calCoast$lng[calCoast$year==sampleYears[i]], calCoast$lat[calCoast$year==sampleYears[i]], cex=0.5, col='red')
}

## maps
counties <- readOGR(dsn = "data files/CA_Counties/", layer = "CA_Counties_TIGER2016")
counties <- spTransform(counties, crswgs84)

# subset individual counties
bayArea <- counties[is.element(counties$NAME, bayCounties),]
coastalCounties <- counties[is.element(counties$NAME, coastCounties),]

temp <- !is.na(over(calCoast, as(bayArea, "SpatialPolygons")))

plot(coastalCounties)
points(calCoast[temp,], col='blue', pch=16, cex=0.5)