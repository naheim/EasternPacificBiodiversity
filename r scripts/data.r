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
projects <- fromJSON(URLencode('http://api.inaturalist.org/v1/projects?q=snapshot cal coast&per_page=200'))$results
projId <- subset(projects, title=="Snapshot Cal Coast")$id

## get number of observations and pages
nObs <- fromJSON(URLencode(paste('http://api.inaturalist.org/v1/observations?project_id=',projId,'&rank=tribe,subtribe,genus,genushybrid,species,hybrid,subspecies,variety,form&per_page=1&order=desc&order_by=created_at', sep="")))$total_results
if(nObs %% 200 == 0) {
	nPages <- nObs/200
} else {
	nPages <- trunc(nObs/200)+1
}
print(paste("There are ",nObs," observations on ",nPages," pages.", sep=""))

## download data
calCoast <- list()
for(i in 1:nPages) {
	obs <- fromJSON(URLencode(paste("http://api.inaturalist.org/v1/observations?project_id=",projId,"&rank=tribe,subtribe,genus,genushybrid,species,hybrid,subspecies,variety,form&page=",i,"&per_page=200&order=desc&order_by=created_at", sep="")))
	calCoast[[i]] <- obs$results
	if(i %% 2 == 0) print(i)
}
save(calCoast, file="data/calCoastObs.RData")
