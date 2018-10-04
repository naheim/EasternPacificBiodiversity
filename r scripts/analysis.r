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

# subset individual counties
bayArea <- counties[is.element(counties$NAME, bayCounties),]
coastalCounties <- counties[is.element(counties$NAME, coastCounties),]


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
calCoast$county <- coastalCounties$NAME[over(calCoast, as(counties, "SpatialPolygons"))]
calCoast$county <- factor(calCoast$county, levels = rev(coastCounties))




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




temp <- !is.na(over(calCoast, as(mpas, "SpatialPolygons")))

plot(coastalCounties)
points(calCoast[temp,], col='blue', pch=16, cex=0.5)