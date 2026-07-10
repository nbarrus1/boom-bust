####this script is dedicated to assigning native and non-native status' to the times series of
####different species found within the BioTIME dataset, focussing on the fish, birds, and molluscs



rm(list = ls())

library(tidyverse)
library(lubridate)
library(patchwork)
library(sf)
library(lwgeom)
library(rnaturalearth)
library(rnaturalearthdata)
library(rfishbase)
library(worrms)
library(mregions2)
library(countrycode)
library(countries)
library(Rtools())
library(here)

#load in biotime2 data

large_data_directory <- "C:/Users/nbarr/OneDrive/Pictures/Documents/Career/Journal Publications/Manuscripts/Dissertation/Large_Data_Files/CH_1-Boom-Bust"

bioTIME_raw <- read_csv(file = paste(large_data_directory,"Input/bioTIME_2.0/biotime_v2_rawdata_2025.csv",sep = "/"))

bioTIME_meta <- read_csv(file = paste(large_data_directory,"Input/bioTIME_2.0/biotime_v2_metadata_2025.csv",sep = "/")) |> 
  select(STUDY_ID,REALM,CLIMATE,CEN_LATITUDE,CEN_LONGITUDE,DATA_POINTS,START_YEAR,END_YEAR)

colnames(bioTIME_meta)
#explore the time series
colnames(bioTIME_raw)
str(bioTIME_raw)


str(unique(bioTIME_raw$ID_ALL_RAW_DATA))
table(is.na(bioTIME_raw$ID_ALL_RAW_DATA))

str(bioTIME_raw$ABUNDANCE)
table(is.na(bioTIME_raw$ABUNDANCE))

str(bioTIME_raw$BIOMASS)
table(is.na(bioTIME_raw$BIOMASS))
#biomass and abundance not mutually exclusive

str(unique(bioTIME_raw$ID_SPECIES))
table(is.na(bioTIME_raw$ID_SPECIES))

str(unique(bioTIME_raw$SAMPLE_DESC))
table(is.na(bioTIME_raw$SAMPLE_DESC))

str(unique(bioTIME_raw$LATITUDE))
table(is.na(bioTIME_raw$LATITUDE))

str(unique(bioTIME_raw$LONGITUDE))
table(is.na(bioTIME_raw$LONGITUDE))

str(unique(bioTIME_raw$DEPTH))
table(is.na(bioTIME_raw$DEPTH))

str(unique(bioTIME_raw$DAY))
table(is.na(bioTIME_raw$DAY))

str(unique(bioTIME_raw$MONTH))
table(is.na(bioTIME_raw$MONTH))

str(unique(bioTIME_raw$YEAR))
table(is.na(bioTIME_raw$YEAR))

str(unique(bioTIME_raw$STUDY_ID))
table(is.na(bioTIME_raw$STUDY_ID))

str(unique(bioTIME_raw$newID))
table(is.na(bioTIME_raw$newID))

str(unique(bioTIME_raw$valid_name))
table(is.na(bioTIME_raw$valid_name))

str(unique(bioTIME_raw$resolution))
table(is.na(bioTIME_raw$resolution))
table(bioTIME_raw$resolution)

str(unique(bioTIME_raw$taxon))
table(is.na(bioTIME_raw$taxon))
table(bioTIME_raw$taxon)

###subset

colnames(bioTIME_raw)

bioTIME <- bioTIME_raw |>
  select(-LATITUDE,-LONGITUDE) |> 
  filter(resolution == "species") |> 
  filter(taxon %in% c("Birds","Molluscs","Fish")) |> 
  group_by(STUDY_ID,taxon,valid_name) |> 
  nest() |> 
  left_join(bioTIME_meta,by = "STUDY_ID") |> 
  filter(!(taxon == "Fish" & REALM == "Terrestrial"))


  
rm(bioTIME_raw, bioTIME_meta)

#--------------------------------
####assign native status####
#--------------------------------

#obtain countries from coordinates


countries <- ne_countries(scale = "medium", returnclass = "sf") |> 
  select(country = name,
         iso_a3,
         continent,
         geometry) |> 
  st_transform(4326)

sf_use_s2(FALSE)

fao <- st_read(
  "https://services.arcgis.com/bDAhvQYMG4WL8O5o/ArcGIS/rest/services/FAO_Fishing_Areas/FeatureServer/0/query?where=1%3D1&outFields=*&f=geojson"
) |> 
  st_transform(4326) |> 
  filter(F_LEVEL == "MAJOR")

###give all species where country is unknown
bioTIME_points <- bioTIME |> 
  st_as_sf(coords = c("CEN_LONGITUDE","CEN_LATITUDE"),
           crs = 4326,
           remove = FALSE) |> 
  st_join(countries,join = st_intersects) |> 
  st_join(fao,join = st_intersects) |> 
  st_drop_geometry() |> 
  distinct(
    country,
    F_CODE,
    iso_a3,
    valid_name,
    taxon,
    REALM,
    CEN_LATITUDE,
    CEN_LONGITUDE,
  ) |> 
  arrange(taxon,REALM,country,F_CODE,iso_a3,valid_name)

saveRDS(bioTIME_points, file = here("output","bioTIME_status.rds"))

#--------------------------------------------------------
#####load in CRIIS checklist for cross-referencing
#--------------------------------------------------------

CRIIS.checklist <- read_csv(paste(large_data_directory,"Input/CRIIS/GRIIS - Country Compendium V1_0.csv",sep = "/")) |> 
  mutate(CRIIS.check = 1,
         CRIIS_status = "non-native") |> 
  select(country,species,CRIIS.check,CRIIS_status)

bioTIME_points <- readRDS(here("output","bioTIME_status.rds")) |> 
  left_join(CRIIS.checklist, by = c("valid_name"="species",
                                    "country" = "country"),relationship = "many-to-many") |> 
  mutate(CRIIS.check = if_else(is.na(CRIIS.check),true = 0, false = CRIIS.check))

saveRDS(bioTIME_points, file = here("output","bioTIME_status_01.rds"))

#------------------------------------------------------------------
####check all fish against FishBASE status data
#------------------------------------------------------------------

#get the fish taxonomy for imputing missing longevities by lowest taxonomic group mean longevities
taxonomy_tbl <- species_names()


#get the best longevity for each species by imputing missing longevities by lowest taxonomic group mean longevities
species_tbl <- fb_tbl("species") |> 
  mutate(sci_name = paste(Genus, Species)) |> 
  select(sci_name, LongevityWild, LongevityCaptive) |> 
  distinct() |> 
  mutate() |> 
  left_join(taxonomy_tbl, by = c("sci_name"="Species")) |> 
  group_by(Genus) |> 
  mutate(Genus_long = mean(LongevityWild, na.rm = T)) |> 
  ungroup() |> 
  group_by(Family) |> 
  mutate(Family_long = mean(LongevityWild, na.rm = T)) |> 
  ungroup() |> 
  group_by(Order) |> 
  mutate(Order_long = mean(LongevityWild, na.rm = T)) |> 
  ungroup() |> 
  group_by(Class) |> 
  mutate(Class_long = mean(LongevityWild, na.rm = T)) |> 
  ungroup() |> 
  group_by(SuperClass) |> 
  mutate(
    SuperClass_long = mean(LongevityWild, na.rm = T), 
    BestLongevity = coalesce(
      LongevityWild,
      Genus_long,
      Family_long,
      Order_long,
      Class_long,
      SuperClass_long
    )
  ) |> 
  ungroup() |> 
  select(sci_name,SpecCode,BestLongevity)


#obtain all the best longevities from FishBase for each species within the biotime subset
bioTIME_fish <- bioTIME |> 
  ungroup() |> 
  filter(taxon == "Fish") |> 
  select(valid_name) |> 
  mutate(valid_name = if_else(valid_name == "Epigonus macrops","Epigonus robustus", false = valid_name)) |> 
  distinct() |> 
  mutate(sci_name = validate_names(valid_name),
         sci_name = if_else(is.na(sci_name), true = valid_name,false = sci_name)) |>
  left_join(species_tbl,by = c("sci_name"), na_matches = "never") 

#### get all the country checklists for each species within the bioTIME subset and add the longevities for each of the species
country_tbl_fishbase <- country(bioTIME_fish$sci_name) |> 
  mutate(iso_a3 = country_name(country))|> 
  left_join(bioTIME_fish, by = c("Species"="sci_name"), na_matches = "never") |> 
  select(iso_a3,valid_name,BestLongevity,Status)


fao_tbl_fishbase <- faoareas(bioTIME_fish$sci_name) |> 
  left_join(species_tbl |> select(sci_name,SpecCode), by = "SpecCode", na_matches = "never") |> 
  left_join(bioTIME_fish, by=c("Species"="sci_name"),na_matches="never") |> 
  mutate(F_CODE = as.character(AreaCode)) |> 
  select(F_CODE,valid_name,BestLongevity,Status)
  

#check the species-country checlist to get status and longevities for fishes in fishbase
bioTIME_points <- readRDS(here("output","bioTIME_status_01.rds")) |> 
  left_join(country_tbl_fishbase, by = c("valid_name","iso_a3"),na_matches = "never") |>
  left_join(fao_tbl_fishbase,by = c("valid_name","F_CODE"), na_matches = "never") |> 
  mutate(Status = if_else(!is.na(Status.x),true = Status.x,false = Status.y),
         BestLongevity = if_else(!is.na(BestLongevity.x),true = BestLongevity.x,false = BestLongevity.y),
         fishbase_status = case_when(Status %in% c("native","endemic","extirpated")~"native",
                                     Status %in% c("introduced","not established","stray") ~ "non-native",
                                     is.na(Status) ~ NA_character_,
                                     .default = "unclear")) |> 
  select(-Status.x,-Status.y,-BestLongevity.x,-BestLongevity.y) |> 
  group_by(country,F_CODE,iso_a3,valid_name,REALM,taxon,CEN_LATITUDE,CEN_LONGITUDE,CRIIS.check,CRIIS_status) |> 
  nest() |> 
  mutate(fishbase_status = map_chr(.x = data,.f = ~.x |> select(fishbase_status) |> distinct() |> pull(fishbase_status)),
         fishbase_best_longevity = map_dbl(.x = data,.f = ~.x |> select(BestLongevity) |> distinct() |> pull(BestLongevity))) |> 
  select(-data)

saveRDS(bioTIME_points, file = here("output","bioTIME_status_02.rds"))

rm(list = setdiff(ls(),c("bioTIME","large_data_directory")))
gc()

#------------------------------------------------------------------
####check all mollusks against worrms status data
#wm_records_names#------------------------------------------------------------------

###the quering of the wrms dataset takes a long time so results will be saved as an rds file than loaded in 
#wms_specieslist <- tibble(spp = unique(bioTIME |> filter(taxon == "Molluscs") |> pull(valid_name))) |> 
#  mutate(id = map_int(.x = spp,possibly(wm_name2id,otherwise = NA_integer_)),
#         taxonomy = map(.x = id, possibly(wm_classification,otherwise = NULL)),
#         distribution = map(.x = id,possibly(wm_distribution,otherwise = NULL)))|> 
#  unnest(taxonomy) |> 
#  select(-AphiaID) |> 
#  pivot_wider(names_from = rank,values_from = scientificname)

#saveRDS(wms_specieslist, file = here("output","wms_specieslist.rds"))
#rm(bioTIME)

wms_specieslist <- readRDS(here("output","wms_specieslist.rds"))

wms_distribution <- wms_specieslist |> 
  select(spp,id,distribution) |> 
  unnest(distribution) |> 
  mutate(mgrid_local = str_extract(locationID, "\\d+$"),
         mgrid_local = as.integer(mgrid_local),
         mgrid_higher = str_extract(higherGeographyID, "\\d+$"),
         mgrid_higher = as.integer(mgrid_higher)) 


####set up a for loop so i can save the distribitions####

higherGeom_withsamestatus <- wms_distribution |> 
  ungroup() |> 
  select(spp,mgrid_higher,mgrid_local,establishmentMeans) |> 
  mutate(mgrid_higher = if_else(is.na(mgrid_higher),true = mgrid_local,false = mgrid_higher),
         status = case_when(str_detect(establishmentMeans,"Native")~"Native",
                            establishmentMeans=="Alien" ~ "Non-Native",
                            str_detect(establishmentMeans,"Origin")~"needs review",
                            .default = "unclassified")) |> 
  group_by(mgrid_higher,mgrid_local,status) |> 
  summarise(n_pop = n()) |> 
  group_by(mgrid_higher,status) |> 
  summarise(n_pop = sum(n_pop)) |> 
  group_by(mgrid_higher) |> 
  summarise(n = n(),
            n_pop = sum(n_pop)) |> 
  ##get all mgrid_highers that have one unique status
  filter(n==1) |> 
  pull(mgrid_higher)


wms_distribution <- wms_distribution |> 
  mutate(mgrid = if_else(mgrid_higher %in% higherGeom_withsamestatus,
                           true = mgrid_higher, false = mgrid_local)) 




###set up the for loops-only run once and save output and comment it out (long loop)

#wms_distribution_mgrid <- wms_distribution |> 
#  ungroup() |> 
#  select(mgrid_higher,mgrid_local) |> 
#  mutate(mgrid = if_else(mgrid_higher %in% higherGeom_withsamestatus,
#                         true = mgrid_higher, false = mgrid_local)) |> 
#  select(mgrid) |> 
#  distinct() |> 
#  mutate(geometry = vector("list",n()),
#         error = NA_character_)



#for (i in 1: length(wms_distribution_mgrid$mgrid)) {
#  
#  message(
#    i, "/", nrow(wms_distribution_mgrid),
#    " MRGID: ", wms_distribution_mgrid$mgrid[i]
#  )
#  
#  
#   result <- tryCatch(
#    
#    gaz_geometry(wms_distribution_mgrid$mgrid[i]),
#    
#    error = function(e) {
#      wms_distribution_mgrid$error[i] <- "error"
#      NULL
#    }
#    
#  )
#  
#  wms_distribution_mgrid$geometry[i] <- list(result)
#  
#  if(i%%10==0) {
#    saveRDS(wms_distribution_mgrid,
#            paste(large_data_directory,"Output/wrms_marine_regions_list.rds", sep = "/"))
#  }
  
#}

#saveRDS(wms_distribution_mgrid,
#        paste(large_data_directory,"Output/wrms_marine_regions_list.rds", sep = "/"))

wrms_distribution_mgrid <- readRDS(paste(large_data_directory,"Output/wrms_marine_regions_list.rds", sep = "/")) |> 
  filter(!map_lgl(geometry,is.null))|>
  mutate(geometry = map(geometry, ~ st_as_sf(.x))) |>
  unnest(geometry) |> 
  st_as_sf(crs = 4326)
  
wms_specieslist_final <- wms_specieslist |> 
  left_join(wms_distribution,by = c("spp","id"),na_matches = "never") |> 
  left_join(wrms_distribution_mgrid, by = "mgrid",na_matches = "never") |> 
  mutate(status = case_when(str_detect(establishmentMeans,"Native")~"Native",
                            establishmentMeans=="Alien" ~ "Non-Native",
                            str_detect(establishmentMeans,"Origin")~"needs review",
                            .default = "unclassified")) |> 
  select(spp,x,status) |> 
  distinct()

rm(list = setdiff(ls(), c("wms_specieslist_final","large_data_directory")))
gc()

###set up for loop for assigning status

bioTIME_points <- readRDS(here("output","bioTIME_status_02.rds")) |> 
  st_as_sf(coords = c("CEN_LONGITUDE","CEN_LATITUDE"),
           crs = 4326,
           remove = FALSE) 


mollusk_points <- bioTIME_points |> 
  filter(taxon == "Molluscs") |> 
  select(valid_name,CEN_LATITUDE,CEN_LONGITUDE,geometry) |> 
  mutate(status.wrms = NA_character_,
         error = NA_character_)
  

for(i in 1:nrow(mollusk_points)) {
  
  
  message(i, "/", nrow(mollusk_points), "  Species: ", mollusk_points$valid_name[i])
  
  result <- tryCatch({
    #grab species name
    biotime.temp <- mollusk_points[i,]
    
    #grab species distribution
    distribution.temp <- wms_specieslist_final |> 
      filter(spp == biotime.temp$valid_name) |> 
      st_as_sf()
    
    #spatial join
    spatial.join.temp <- biotime.temp |> 
      st_join(distribution.temp, join = st_intersects,left=TRUE) |> 
      distinct()
    
    
    mollusk_points$status.wrms[i] <- spatial.join.temp$status
    
  }, error = function(e){
    mullusk_points$error[i]<-"error"
    
    NA_character_
  })
  
  #place results into mollusk_points
  mollusk_points$status.wrms[i] <- if (length(result) == 0) {
    NA_character_
  } else {
    paste(result, collapse = "; ")
  }
  
  #save mollusk points after each 100 iterations
  
  if (i %% 100 == 0) {
    saveRDS(
      mollusk_points,
      here("output", "mollusk_points.rds")
    )
  
  
  }
}

  saveRDS(
    mollusk_points,
    here("output", "mollusk_points_status.rds")
  )

  mollusk_points <- readRDS(here("output", "mollusk_points_status.rds"))
  

#### assign status for the birds###  
  
  
  
bioTIME_points <- readRDS(here("output","bioTIME_status_02.rds")) |> 
  left_join(mollusk_points |> select(-geometry), by = colnames(mollusk_points)[1:3]) |> 
  select(-geometry)

saveRDS(bioTIME_points,here("output","bioTIME_status_03.rds"))

rm(list = setdiff(ls(),c("bioTIME_points","large_data_directory")))
gc()


bioTIME_points <- readRDS(here("output","bioTIME_status_03.rds"))

bird_points <- bioTIME_points |> 
  filter(taxon == "Birds") |> 
  st_as_sf(
    coords = c("CEN_LONGITUDE", "CEN_LATITUDE"),
    crs = 4326,
    remove = FALSE
  ) |> 
  mutate(birdLife_status = NA_integer_,
         birdLife_presence = NA_integer_,
         birdLife_errors = NA_character_)

layer <- "main.all_species"

###query the 1925 unique bird species in bioTime data set from the BirdLife Range Data

bird_spp <- unique(bird_points$valid_name)

bird_spp_sql <- paste0(
  "'",
  gsub("'", "''", bird_spp),
  "'",
  collapse = ","
)

query_all <- paste0(
  "SELECT sci_name, presence, origin, seasonal, geom FROM ",
  layer,
  " WHERE sci_name IN (", bird_spp_sql, ")"
)

bird_ranges <- st_read(
  paste(large_data_directory, "Input/birdLife/BOTW_2025.gpkg", sep = "/"),
  query = query_all,
  quiet = TRUE
) |>
  st_make_valid()

saveRDS(bird_ranges, paste(large_data_directory, "Input/birdLife/BirdLifeRanges_BiotimeSubset.rds",sep ="/"))

bird_ranges <- readRDS(paste(large_data_directory, "Input/birdLife/BirdLifeRanges_BiotimeSubset.rds",sep ="/"))
#### for loop for every point

sf::sf_use_s2(FALSE)

for (i in 12:nrow(bird_points)) {

#track the iteration
message(i,"/", nrow(bird_points)," ", bird_points$valid_name[i])   
  
#create the SQL query for the species at row i


#load in the range data for the species at row i
range_temp <- bird_ranges |> 
  filter(sci_name == bird_points$valid_name[i])

### filter the entire bird points to only include the point for row i
point_temp <-bird_points[i,]

# give a meaningful error flag for row i if loading in the range failed
if(is.null(range_temp)||nrow(range_temp)==0){
  bird_points$birdLife_errors[i] <- "no_range_found"
  bird_points$birdLife_status[i] <- NA_integer_
  bird_points$birdLife_presence[i]<-NA_integer_
  
  next
}


# spatial join the attributes from the range polygon that intersects with the point at row i
join_temp <- tryCatch(point_temp |> 
                        st_join(range_temp , join = st_intersects,left = TRUE),
                      error = function(e) NULL)

#if there are multiple polygons check to see if there is concordance if so save the first origin and presence,
#stop otherwise
if (nrow(join_temp) > 1 & length(unique(join_temp$origin))==1) {
  
  bird_points$birdLife_presence[i] <- join_temp$presence[1]
  bird_points$birdLife_status[i] <- join_temp$origin[1]
   
}


if(nrow(join_temp) > 1 & length(unique(join_temp$origin))>1){
  stop(
  "Point intersects multiple polygons at row ",
  i,
  ": ",
  bird_points$valid_name[i]
)}

### check if the spatial join failed and give a meaningful error if failed at row i
if (is.null(join_temp) || nrow(join_temp) == 0) {
  bird_points$birdLife_errors[i] <- "join_error"
  bird_points$birdLife_status[i] <- NA_integer_
  bird_points$birdLife_presence[i]<-NA_integer_
  next
}


#check if the point was outside any of the polygons and give a meaningful error
if (all(is.na(join_temp$presence))&& all(is.na(join_temp$origin))) {
  bird_points$birdLife_errors[i] <- "outside_range"
}

###save the results of all the good spatial joins
if (nrow(join_temp) == 1) { 
bird_points$birdLife_presence[i] <- join_temp$presence
bird_points$birdLife_status[i] <- join_temp$origin
}

### every 50 iteration save results onto a hard file (so loop only needs ran once)
if(i %% 50 == 0) {
  saveRDS(bird_points,here("output","bird_points.rds"))
}

}

saveRDS(bird_points, here("output","bird_points.rds"))


bird_points <- readRDS(here("output","bird_points.rds"))

bioTIME_points <- readRDS(here("output","bioTIME_status_03.rds")) 

bioTIME_points |> 
  left_join(bird_points, by = colnames(bioTIME_points)) |> 
  mutate(birdLife_status = case_when(birdLife_status %in%  c(1,2)~"Native",
                                     birdLife_status %in%  c(3,4,6)~"Non-native",
                                     birdLife_status %in%  c(5)~"Unkown",
                                     .default = NA_character_)) |> 
  saveRDS(here("output","bioTIME_status_04.rds"))

rm(list = setdiff(ls(),c("bioTIME_points","large_data_directory")))
gc()

####first round of native status's are complete####


####check and fix errors if needed####

##Birds###

####check names of species to make sure there is concordance with the birdLife 
###taxonomy

bioTIME_points <- readRDS(here("output","bioTIME_status_04.rds"))

spps_norange <- bioTIME_points |> 
  filter(birdLife_errors == "no_range_found") |> 
  pull(valid_name) |> 
  unique()

##load in chatGPT cross reference of the species with no ranges to the birdLIFE
##taxonomy table that has known alternative scientific names, and gives 
##the accepatable scientific name used in birdLIFE

matched_names <- read_csv(here("data","birdlife_alternate_name_matches.csv")) |> 
  select(original_name,birdlife_accepted_scientific_name) |> 
  rename(valid_name = original_name)

accepted_names <- matched_names |> 
  pull(birdlife_accepted_scientific_name)

bird_points <- bioTIME_points |> 
  left_join(matched_names, by = c("valid_name")) |> 
  filter(birdlife_accepted_scientific_name %in% accepted_names) |>
  st_as_sf(
    coords = c("CEN_LONGITUDE", "CEN_LATITUDE"),
    crs = 4326,
    remove = FALSE
  )

#rerun the loop

layer <- "main.all_species"

###query the 1925 unique bird species in bioTime data set from the BirdLife Range Data

bird_spp <- unique(accepted_names)

bird_spp_sql <- paste0(
  "'",
  gsub("'", "''", bird_spp),
  "'",
  collapse = ","
)

query_all <- paste0(
  "SELECT sci_name, presence, origin, seasonal, geom FROM ",
  layer,
  " WHERE sci_name IN (", bird_spp_sql, ")"
)

bird_ranges <- st_read(
  here("data", "BOTW_2025.gpkg"),
  query = query_all,
  quiet = TRUE
) |>
  st_make_valid()

#### for loop for every point

sf::sf_use_s2(FALSE)

for (i in 1:nrow(bird_points)) {
  
  #track the iteration
  message(i,"/", nrow(bird_points)," ", bird_points$valid_name[i])   
  
  #create the SQL query for the species at row i
  
  
  #load in the range data for the species at row i
  range_temp <- bird_ranges |> 
    filter(sci_name == bird_points$birdlife_accepted_scientific_name[i])
  
  ### filter the entire bird points to only include the point for row i
  point_temp <-bird_points[i,]
  
  # give a meaningful error flag for row i if loading in the range failed
  if(is.null(range_temp)||nrow(range_temp)==0){
    bird_points$birdLife_errors[i] <- "no_range_found"
    bird_points$birdLife_status[i] <- NA_integer_
    bird_points$birdLife_presence[i]<-NA_integer_
    
    next
  }
  
  
  # spatial join the attributes from the range polygon that intersects with the point at row i
  join_temp <- tryCatch(point_temp |> 
                          st_join(range_temp, join = st_intersects,left = TRUE),
                        error = function(e) NULL)
  
  #if there are multiple polygons check to see if there is concordance if so save the first origin and presence,
  #stop otherwise
  if (nrow(join_temp) > 1 & length(unique(join_temp$origin))==1) {
    
    bird_points$birdLife_presence[i] <- join_temp$presence[1]
    bird_points$birdLife_status[i] <- join_temp$origin[1]
    
  }
  
  
  if(nrow(join_temp) > 1 & length(unique(join_temp$origin))>1){
    stop(
      "Point intersects multiple polygons at row ",
      i,
      ": ",
      bird_points$valid_name[i]
    )}
  
  ### check if the spatial join failed and give a meaningful error if failed at row i
  if (is.null(join_temp) || nrow(join_temp) == 0) {
    bird_points$birdLife_errors[i] <- "join_error"
    bird_points$birdLife_status[i] <- NA_integer_
    bird_points$birdLife_presence[i]<-NA_integer_
    next
  }
  
  
  #check if the point was outside any of the polygons and give a meaningful error
  if (all(is.na(join_temp$presence))&& all(is.na(join_temp$origin))) {
    bird_points$birdLife_errors[i] <- "outside_range"
  }
  
  ###save the results of all the good spatial joins
  if (nrow(join_temp) == 1 && all(!is.na(join_temp$presence))) { 
    bird_points$birdLife_presence[i] <- join_temp$presence
    bird_points$birdLife_status[i] <- join_temp$origin
    bird_points$birdLife_errors[i] <- NA_character_
  }
  
  ### every 50 iteration save results onto a hard file (so loop only needs ran once)
  if(i %% 50 == 0) {
    saveRDS(bird_points,here("output","bird_points_rangeerrors.rds"))
  }
  
}


bird_points <- readRDS(here("output","bird_points_rangeerrors.rds")) |> 
  select(-birdlife_accepted_scientific_name)

bioTIME_points <- readRDS(here("output","bioTIME_status_04.rds")) |> 
  filter(!(valid_name %in% matched_names$valid_name)) |> 
  bind_rows(bird_points) |> 
  mutate(birdLife_status = case_when(birdLife_status %in%  c("1","2")~"Native",
                                     birdLife_status %in%  c("3","4","6")~"Non-native",
                                     birdLife_status %in%  c("5")~"Unkown",
                                     .default = birdLife_status)) |> 
  saveRDS(here("output","bioTIME_status_05.rds"))


bioTIME_points <- readRDS(here("output","bioTIME_status_05.rds")) 


rm(list = setdiff(ls(),c("bioTIME_points","large_data_directory")))
gc()

#####Check for proximity to polygon's for those found outside the range

bioTIME_points <- readRDS(here("output","bioTIME_status_05.rds")) 


###get the list of accepted names###

matched_names <- read_csv(here("data","birdlife_alternate_name_matches.csv")) |> 
  select(original_name,birdlife_accepted_scientific_name) |> 
  rename(valid_name = original_name)

accepted_names <- matched_names |> 
  pull(birdlife_accepted_scientific_name)

bird_points <- bioTIME_points |> 
  filter(birdLife_errors=="outside_range") |> 
  left_join(matched_names, by = c("valid_name")) |> 
  mutate(birdlife_accepted_scientific_name = if_else(is.na(birdlife_accepted_scientific_name),
                                                     true = valid_name, false = birdlife_accepted_scientific_name)) |> 
  st_as_sf(
    coords = c("CEN_LONGITUDE", "CEN_LATITUDE"),
    crs = 4326,
    remove = FALSE
  ) |> 
  mutate(dist_m = NA_integer_,
         dist_method = NA_character_,
         nearest_error = NA_character_)


####load in the range maps

#subset for working on workflow-
bird_spp <- unique(bird_points$birdlife_accepted_scientific_name)

#bird_spp <- unique(bird_points$birdlife_accepted_scientific_name)

layer <- "main.all_species"

bird_spp_sql <- paste0(
  "'",
  gsub("'", "''", bird_spp),
  "'",
  collapse = ","
)

query_all <- paste0(
  "SELECT sci_name, presence, origin, seasonal, geom FROM ",
  layer,
  " WHERE sci_name IN (", bird_spp_sql, ")"
)

bird_ranges <- st_read(
  paste(large_data_directory, "Input/birdLife/BOTW_2025.gpkg", sep = "/"),
  query = query_all,
  quiet = TRUE
) |>
  st_make_valid()

###

for (i in seq_len(nrow(bird_points))) {
  
  message(i, "/", nrow(bird_points), " ", bird_points$valid_name[i])
  
  range_temp <- bird_ranges |> 
    filter(sci_name == bird_points$birdlife_accepted_scientific_name[i])
  
  point_temp <- bird_points[i, ]
  
  result <- tryCatch({
    
    sf_use_s2(TRUE)
    
    point_calc <- st_transform(point_temp, 4326)
    range_calc <- st_transform(range_temp, 4326)
    
    nearest_id <- st_nearest_feature(point_calc, range_calc)
    
    dist_m <- as.numeric(
      st_distance(
        point_calc,
        range_calc[nearest_id, ],
        by_element = TRUE
      )
    )
    
    list(
      nearest_id = nearest_id,
      dist_m = dist_m,
      distance_method = "s2_geodesic_4326",
      error = NA_character_
    )
    
  }, error = function(e) {
    
    err <- conditionMessage(e)
    
    sf_use_s2(FALSE)
    
    point_calc <- st_transform(point_temp, 3857)
    range_calc <- st_transform(range_temp, 3857)
    
    nearest_id <- st_nearest_feature(point_calc, range_calc)
    
    dist_m <- as.numeric(
      st_distance(
        point_calc,
        range_calc[nearest_id, ],
        by_element = TRUE
      )
    )
    
    list(
      nearest_id = nearest_id,
      dist_m = dist_m,
      distance_method = "planar_3857_s2_false",
      error = err
    )
  })
  
  if (!is.na(result$dist_m) && result$dist_m <= 1000) {
    
    bird_points$birdLife_status[i] <- paste0(
      unique(range_temp$origin[result$nearest_id]),
      collapse = ","
    )}
  
  
  bird_points$dist_m[i] <- result$dist_m
  bird_points$dist_method[i] <- result$distance_method
  bird_points$nearest_error[i] <- result$error
  
  if(i %% 50 == 0) {
    saveRDS(bird_points,here("output","bird_points_outsiderange.rds"))
  }
  
}

saveRDS(bird_points, here("output","bird_points_outsiderange.rds"))

bird_points <- readRDS(here("output","bird_points_outsiderange.rds")) |> 
  select(-birdlife_accepted_scientific_name)|> 
  mutate(birdLife_status = case_when(birdLife_status %in%  c("1","2")~"Native",
                                     birdLife_status %in%  c("3","4","6")~"Non-native",
                                     birdLife_status %in%  c("5")~"Unkown",
                                     .default = birdLife_status)) 


bioTIME_points <- readRDS(here("output","bioTIME_status_05.rds")) |> 
  filter(birdLife_errors != "outside_range"|is.na(birdLife_errors)) |> 
  bind_rows(bird_points) |> 
  saveRDS(here("output","bioTIME_status_06.rds"))


bioTIME_points <- readRDS(here("output","bioTIME_status_06.rds"))

rm(list = setdiff(ls(),c("bioTIME_points","large_data_directory")))
gc()


###check and then correct if there are names that are not standardized between CRIIS and bioTIME

CRIIS.spp <- read_csv(paste(large_data_directory,"Input/CRIIS/GRIIS - Country Compendium V1_0.csv",sep = "/")) |> 
  mutate(CRIIS.check = 1,
         CRIIS_status = "non-native") |> 
  select(country,species,CRIIS.check,CRIIS_status) |> 
  select(species) |> 
  distinct(species)

bioTIME_spp <- readRDS(here("output","bioTIME_status_06.rds")) |> 
  ungroup() |> 
  select(valid_name) |> 
  anti_join(CRIIS.spp, by = c("valid_name" = "species")) |> 
  distinct(valid_name)



##unique()###################
####concordance between CRIIS check and taxon specific checks
##################

CRIIS.spp <- read_csv(paste(large_data_directory,"Input/CRIIS/GRIIS - Country Compendium V1_0.csv",sep = "/")) |> 
  mutate(CRIIS.check = 1,
         CRIIS_status = "non-native") |> 
  select(country,species,CRIIS.check,CRIIS_status) |> 
  pull(species) |> 
  unique()


bioTIME_points <- readRDS(here("output","bioTIME_status_06.rds")) |> 
  mutate(status.wrms = case_when(status.wrms == "unclassified; Non-Native"~"Non-Native",
                                 status.wrms == "Non-Native; unclassified"~"Non-Native",
                                 status.wrms == "NA" ~NA_character_,
                                 .default = status.wrms),
    final.status = case_when(CRIIS_status == "non-native" &
                                    fishbase_status == "non-native"~"non-native",
                                  CRIIS_status == "non-native" &
                                    birdLife_status == "Non-native"~"non-native",
                                  CRIIS_status == "non-native" &
                                    status.wrms == "Non-Native"~"non-native",
                                  CRIIS_status == "non-native" &
                                    fishbase_status == "unclear"~"non-native",
                                  CRIIS_status == "non-native" &
                                    status.wrms == "unclassified"~"non-native",
                                  CRIIS_status == "non-native" &
                                    fishbase_status == "native"~"needs manual review",
                                  CRIIS_status == "non-native" &
                                    birdLife_status == "Native"~"needs manual review",
                                  CRIIS_status == "non-native" &
                                    status.wrms == "Native"~"needs manual review",
                             #If in CRIIS checklist but non in taxon databases call it non-native
                                  CRIIS_status == "non-native" &
                                    is.na(fishbase_status)&
                                    is.na(birdLife_status)&
                                    is.na(status.wrms)~"non-native",
                                  is.na(CRIIS_status) & 
                                    fishbase_status == "non-native"~"non-native",
                                  is.na(CRIIS_status) & 
                                    birdLife_status == "Non-native"~"non-native",
                                  is.na(CRIIS_status) & 
                                    status.wrms == "Non-Native"~"non-native",
                                  is.na(CRIIS_status) & 
                                    fishbase_status == "native"~"native",
                                  is.na(CRIIS_status) & 
                                    birdLife_status == "Native"~"native",
                                  is.na(CRIIS_status) & 
                                    status.wrms == "Native"~"native",
                                  is.na(CRIIS_status) & 
                                    fishbase_status == "unclear"~"unclassified",
                                  is.na(CRIIS_status) & 
                                    status.wrms == "unclassified"~"unclassified",
                             #if there is a country meaning it should be in the checklist
                             #but not listed as non-native in CRIIS nor is it found in the 
                             #taxon databases then list as "native"
                                  !(is.na(country)) &
                                    is.na(CRIIS_status) &
                                    is.na(fishbase_status)&
                                    is.na(birdLife_status)&
                                    is.na(status.wrms)~"native",
                             #if there is not country but the species is a species listed in the
                             #non-native species checklist
                                  is.na(country) &
                                    is.na(CRIIS_status) &
                                    is.na(fishbase_status)&
                                    is.na(birdLife_status)&
                                    is.na(status.wrms)&
                                    valid_name %in% CRIIS.spp~"needs manual review",
                                 is.na(country) &
                                    is.na(CRIIS_status) &
                                    is.na(fishbase_status)&
                                    is.na(birdLife_status)&
                                    is.na(status.wrms)&
                                    !(valid_name %in% CRIIS.spp)~"native",
                                  .default = NA_character_)) |> 
  saveRDS(here("output","bioTIME_status_07.rds"))

#add the time series data
bioTIME_points <- readRDS(here("output","bioTIME_status_07.rds"))|> 
  left_join(bioTIME |> select(-STUDY_ID,-CLIMATE), by = c("valid_name","taxon","REALM","CEN_LATITUDE","CEN_LONGITUDE"))


sum(table(bioTIME_points$final.status))

manual_review <- bioTIME_points |> 
  filter(final.status == "needs manual review")



