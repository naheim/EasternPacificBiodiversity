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

load("data files/calCoastObs.RData")

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

