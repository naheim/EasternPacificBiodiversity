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

library(jsonlite)


## get list of snapshot cal coast projects
projNames <- c("Snapshot Cal Coast 2016","Snapshot Cal Coast 2017","Snapshot Cal Coast 2018")
projects <- fromJSON(URLencode('http://api.inaturalist.org/v1/projects?q=snapshot cal coast&per_page=200'))$results

# columns I don't want to keep
dropColumns <- c('time_observed_at','annotations','photos','created_at_details','tags','created_time_zone','quality_metrics','flags','description','time_zone_offset','project_ids_with_curator_id','observed_o','observed_on_string','outlinks','sounds','ofvs','preferences','faves','non_owner_ids','project_ids_without_curator_id','votes','observation_photos','project_observations','identifications','geojson','reviewed_by','comments','project_ids')

formatColumns <- function(x, drops) {
	x <- x[,!is.element(colnames(x), drops)]
	
	# observed_on
	x$observed_on <- as.Date(x$observed_on, "%Y-%m-%d")
	
	# observed_on_details
	x$year <- x$observed_on_details$year
	
	# taxon
	x$is_active <- x$taxon$is_active
	x$endemic <- x$taxon$endemic
	x$threatened <- x$taxon$threatened
	x$introduced <- x$taxon$introduced
	x$native <- x$taxon$native
	x$name <- x$taxon$name
	x$rank <- x$taxon$rank
	x$extinct <- x$taxon$extinct
	x$taxon_id <- x$taxon$id
	x$observations_count <- x$taxon$observations_count
	x$iconic_taxon_name <- x$taxon$iconic_taxon_name
	x$preferred_common_name <- x$taxon$preferred_common_name
	x$conservation_status.iucn <- x$taxon$conservation_status.iucn
	x$conservation_status.geoprivacy <- x$taxon$conservation_status.geoprivacy
	x$conservation_status.status <- x$taxon$conservation_status.status
	
	# place_ids
	x$place_ids <- sapply(x$place_ids, paste, collapse=",")
	
	# user
	x$user_id <- x$user$id
	x$user_observation_counts <- x$user$observations_count
	x$user_identification_count <- x$user$identifications_count
	
	# lat/lng
	temp <- strsplit(x$location, ",")
	x$lng <- as.numeric(sapply(temp, function(x){return(x[2])}))
	x$lat <- as.numeric(sapply(temp, function(x){return(x[1])}))
	
	x <- x[,!is.element(colnames(x), c('observed_on_details','taxon','place_ids','user','location'))]
	return(x)
}

# loop through projects
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
		obs$results <- formatColumns(obs$results, dropColumns)
		
		if(j == 1 & i == 1) {
			calCoast <- obs$results
			
		} else {
			row.names(obs$results) <- (nrow(calCoast)+1):(nrow(calCoast)+nrow(obs$results))
			calCoast <- rbind(calCoast, obs$results)
		}			
		if(i %% 2 == 0) print(i)
	}
}
nrow(calCoast)
save(calCoast, file="data files/calCoastObs.RData")
