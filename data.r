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
projId <- subset(projects, title=="Snapshot Cal Coast 2016")$id

nObs <- fromJSON(URLencode(paste('http://api.inaturalist.org/v1/observations?project_id=',projId,'&rank=tribe,subtribe,genus,genushybrid,species,hybrid,subspecies,variety,form&per_page=1&order=desc&order_by=created_at', sep="")))$total_results
if(nObs %% 200 == 0) {
	nPages <- nObs/200
} else {
	nPages <- trunc(nObs/200)+1
}

## OBSERVATION COLUMN NAMES AND CLASS
$out_of_range
[1] "logical"

$quality_grade
[1] "character"

#$time_observed_at
#[1] "character"

#$annotations
#[1] "list"

#$uuid
#[1] "character"

#$photos
#[1] "list"

$observed_on_details
[1] "data.frame"

$id
[1] "integer"

$cached_votes_total
[1] "integer"

$identifications_most_agree
[1] "logical"

#$created_at_details
#[1] "data.frame"

$species_guess
[1] "character"

$identifications_most_disagree
[1] "logical"

#$tags
#[1] "list"

$positional_accuracy
[1] "integer"

$comments_count
[1] "integer"

$site_id
[1] "integer"

#$created_time_zone
#[1] "character"

$id_please
[1] "logical"

$license_code
[1] "character"

$observed_time_zone
[1] "character"

#$quality_metrics
#[1] "list"

$public_positional_accuracy
[1] "integer"

#$reviewed_by
#[1] "list"

$oauth_application_id
[1] "integer"

#$flags
#[1] "list"

$$created_at
$[1] "character"

#$description
#[1] "character"

#$time_zone_offset
#[1] "character"

#$project_ids_with_curator_id
#[1] "list"

#$observed_on
#[1] "character"

#$observed_on_string
#[1] "character"

$updated_at
[1] "character"

#$sounds
#[1] "list"

$place_ids
[1] "list"

$captive
[1] "logical"

$taxon
[1] "data.frame"

#$outlinks
#[1] "list"

$faves_count
[1] "integer"

#$ofvs
#[1] "list"

$num_identification_agreements
[1] "integer"

#$preferences
#[1] "data.frame"

$comments
[1] "list"

$map_scale
[1] "integer"

$uri
[1] "character"

$project_ids
[1] "list"

#$identifications
#[1] "list"

$community_taxon_id
[1] "integer"

#$geojson
#[1] "data.frame"

$owners_identification_from_vision
[1] "logical"

$identifications_count
[1] "integer"

$obscured
[1] "logical"

#$project_observations
#[1] "list"

$num_identification_disagreements
[1] "integer"

#$observation_photos
#[1] "list"

$geoprivacy
[1] "logical"

$location
[1] "character"

#$votes
#[1] "list"

$spam
[1] "logical"

$user
[1] "data.frame"

$mappable
[1] "logical"

$identifications_some_agree
[1] "logical"

#$project_ids_without_curator_id
#[1] "list"

$place_guess
[1] "character"

#$faves
#[1] "list"

#$non_owner_ids
#[1] "list"




print(paste("There are ",nObs," observations on ",nPages," pages.", sep=""))
calCoast <- data.frame()
for(i in 1:nPages) {
	obs <- fromJSON(URLencode(paste("http://api.inaturalist.org/v1/observations?project_id=",projId,"&rank=tribe,subtribe,genus,genushybrid,species,hybrid,subspecies,variety,form&page=",i,"&per_page=200&order=desc&order_by=created_at", sep="")))
	calCoast <- rbind(calCoast, obs$results)
	if(i %% 2 == 0) print(i)
}
nrow(calCoast)
