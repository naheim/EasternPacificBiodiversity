# set root base for proper file direction
init.dir <- getwd()
if(is.element(Sys.info()["nodename"], c("es-naheim.local","sr12-cf96e71ca1.stanford.edu"))) {
	my.root <- "/Volumes/Blastoid/noelheim_data"
} else {
	my.root <- "/Users/noelheim"
}
Sys.setenv(TZ="America/Los_Angeles")

library(jsonlite)

source(paste(my.root,"/Box Sync/Includes/myFunctions.r", sep=""));
setwd(paste(my.root,"/Box Sync/git/snapshotcalcoast", sep=""));


## get list of snapshot cal coast projects
projNames <- c("Snapshot Cal Coast 2016","Snapshot Cal Coast 2017","Snapshot Cal Coast 2018")
projects <- fromJSON(URLencode('http://api.inaturalist.org/v1/projects?q=snapshot cal coast&per_page=200'))$results

# loop through projects
calCoast <- list()
for(j in 1:3) {
	projId <- subset(projects, title==projNames[j])$id

	## get number of observations and pages
	nObs <- fromJSON(URLencode(paste('http://api.inaturalist.org/v1/observations?project_id=',projId,'&rank=tribe,subtribe,genus,genushybrid,species,hybrid,subspecies,variety,form&per_page=1&order=desc&order_by=created_at&quality_grade=research', sep="")))$total_results
	if(nObs %% 200 == 0) {
		nPages <- nObs/200
	} else {
		nPages <- trunc(nObs/200)+1
	}
	print(paste(projNames[j],": There are ",nObs," observations on ",nPages," pages.", sep=""))

	## download data
	for(i in 1:nPages) {
		obs <- fromJSON(URLencode(paste("http://api.inaturalist.org/v1/observations?project_id=",projId,"&rank=tribe,subtribe,genus,genushybrid,species,hybrid,subspecies,variety,form&page=",i,"&per_page=200&order=desc&order_by=created_at&quality_grade=research", sep="")))
		calCoast[[i]] <- obs$results
		if(i %% 2 == 0) print(i)
	}
}
save(calCoast, file="data/calCoastObs.RData")
