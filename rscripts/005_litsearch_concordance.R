###this script is dedicated to finding if there are data that overlap between the three main 
###sources of time series: my literature search of boom-bust studies and the BioTIME and 
###Global Population Dynamics Database

rm(list = ls())

###load in the libraries

library(tidyverse)
library(fuzzyjoin)
library(stringdist)
library(here)


###load in the data

large_data_directory <- "C:/Users/nbarr/OneDrive/Pictures/Documents/Career/Journal Publications/Manuscripts/Dissertation/Large_Data_Files/CH_1-Boom-Bust"

BoomBust.metadata <- read_csv(here("data", "BoomBust_Review - TimeSeries_Identification.csv"))
GPD.metadata <- read_csv(paste(large_data_directory,"Input/GlobalPopulationDynamicsDatabase/LPD_2024_public.csv", sep = "/"))
bioTIME.metadata <- read_csv(paste(large_data_directory,"Input/bioTIME_2.0/biotime_v2_metadata_2025.csv", sep = "/"))


###step one extract the title's from the GPD data


fuzzy_title <- function(x){
  x |>
    str_to_lower() |>
    str_replace_all("[[:punct:]]", " ") |>
    str_squish()
}


extract_gpd_title <- function(x) {
  case_when(
    str_detect(x, '"') ~ str_extract(x, '(?<=\\").+?(?=\\")'),
    
    str_detect(x, "\\([0-9]{4}") ~ x |>
      str_replace("^.*?\\([0-9]{4}[^)]*\\)\\.?", "") |>
      str_replace("\\.\\s*[A-Z][A-Za-z &]+\\s+[0-9]+.*$", "") |>
      str_squish(),
    
    str_detect(x, "\\b[0-9]{4}\\.\\s+") ~ x |>
      str_replace("^.*?\\b[0-9]{4}\\.\\s+", "") |>
      str_replace("\\.\\s*[A-Z][A-Za-z &]+\\s+[0-9]+.*$", "") |>
      str_squish(),
    
    TRUE ~ NA_character_
  )
}

#set up the citations to work best for fuzzy matching
gpd <- GPD.metadata |> 
  select(Citation) |> 
  distinct() |> 
  mutate(dataset = "GPD",
    title = extract_gpd_title(Citation),
    fuzzy.title = fuzzy_title(title))
  

boom_bust <- BoomBust.metadata |> 
  select(title,author,journal) |> 
  distinct() |> 
  mutate(fuzzy.title = fuzzy_title(title))


bioTIME <- bioTIME.metadata |> 
  select(TITLE,CONTACT_1,CONTACT_2,WEB_LINK,DATA_SOURCE) |> 
  distinct()  |> 
  mutate(fuzzy.title = fuzzy_title(TITLE))




matches_gpd.bioTIME <- gpd |>
  stringdist_inner_join(
    bioTIME,
    by = "fuzzy.title",
    max_dist = 0.20,
    method = "jw"
  )|> 
  select(fuzzy.title.x,fuzzy.title.y) |>
  mutate(jw_dist = stringdist(fuzzy.title.x, fuzzy.title.y, method = "jw"))|>
  rename(gpd.fuzzy.title = fuzzy.title.x,
         bioTIME.fuzzy.title = fuzzy.title.y) |> 
  arrange(jw_dist)

matches_gpd.boombust <- gpd |>
  stringdist_inner_join(
    boom_bust,
    by = "fuzzy.title",
    max_dist = 0.20,
    method = "jw"
  )|> 
  select(fuzzy.title.x,fuzzy.title.y) |>
  mutate(jw_dist = stringdist(fuzzy.title.x, fuzzy.title.y, method = "jw"))|>
  rename(gpd.fuzzy.title = fuzzy.title.x,
         boombust.fuzzy.title = fuzzy.title.y) |> 
  arrange(jw_dist)


matches_bioTIME.boombust <- bioTIME |>
  stringdist_inner_join(
    boom_bust,
    by = "fuzzy.title",
    max_dist = 0.20,
    method = "jw"
  )|> 
  select(fuzzy.title.x,fuzzy.title.y) |>
  mutate(jw_dist = stringdist(fuzzy.title.x, fuzzy.title.y, method = "jw"))|>
  rename(bioTIME.fuzzy.title = fuzzy.title.x,
         boombust.fuzzy.title = fuzzy.title.y) |> 
  arrange(jw_dist)


##save the most likely matches for manual review

 matches_gpd.bioTIME |> 
  bind_rows(matches_gpd.boombust,matches_bioTIME.boombust) |> 
   write_csv(file = here("output","study_matches_v1.csv"))

#### read in the matches after manual review
 
 
 matches.final <- read_csv(here("output","study_matches_v2.csv"))
