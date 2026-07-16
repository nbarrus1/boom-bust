###this script is dedicated to combining the three sources into one,
###providing the best biogeographic boundaries and the longevity information to the time series.
###It also obtaining timeseries length, completeness, and performs some first summaries
###describing the time series. It finally filters the time series by the criteria that I set
### for describing the population dynamics. Mostly that they are complete and sufficiently long.

#remove all 

rm(list = ls())


#libraries

library(tidyverse)
library(EDIutils)
library(patchwork)
library(fuzzyjoin)
library(here)


theme_set(theme_bw())

## read in the data
large_data_directory <- "C:/Users/nbarr/OneDrive/Pictures/Documents/Career/Journal Publications/Manuscripts/Dissertation/Large_Data_Files/CH_1-Boom-Bust"

#digitized data
load(here("output","literatrure_timeseries.Rdata"))

#dorn lab data
load(here("output","MDW_mayan-jewelfish.Rdata"))

#rehage lab data (accessed through EDI)

#package_id <- "knb-lter-fce.1164.13"

#entity_id <- read_data_entity_names(package_id) |> 
#  pull(entityId)

#entity_name <- read_data_entity_names(package_id) |> 
#  pull(entityName)

#rehage_raw <- read_data_entity(
#  packageId = package_id,
#  entityId = entity_id
#)

#con <- rawConnection(rehage_raw)

#rehage_data <- read_csv(
#  con,
#  na = c("", " ", ".", "NA", "-9999", "-9999.0", "-9999.00"),
#  show_col_types = FALSE
#) 



otherdata <- read_csv(here("data","BoomBust_Review - TimeSeries_Identification.csv"))
bird_longevity <- 
countries <- read_csv(here("data","country_continent_island.csv"))

###

#rehage_data |> 
#  group_by(across(everything())) |> 
#  summarise(n = n()) |> 
#  filter(n >1)|> 
#  write_csv(file = here("output","duplicates_all.csv"))

#rehage_data |> 
#  group_by(ID,Year,Date,Season,River,Creek,Bout,Distance,DOMGL,Salinity,TempC,Catch,CPUE) |> 
#  summarise(n = n()) |> 
#  filter(n >1) |> 
#  write_csv(file = here("output","duplicates_IDtoCPUE.csv"))

#rehage_data |> 
#  pivot_longer(14:76,names_to = 'spp',values_to = 'count') |> 
#  group_by(ID,Year,Date,Season,River,Creek,Bout,Distance,DOMGL,Salinity,TempC,Catch,CPUE) |> 
#  summarise(test.total = sum(count)) |> 
#  filter(Catch != test.total)|> 
#  write_csv(file = here("output","SumofSpecies_MismatchCatch.csv"))
  
###bioTIME data
bioTIME_points <- readRDS(here("output","bioTIME_status_08.rds")) |>
  group_by(STUDY_ID) |> 
  mutate(PLOT_ID = 1:n()) |> 
  ungroup() |> 
  mutate(species.names = valid_name,
         ecosystem = REALM,
         plot = map2_chr(.x = STUDY_ID,.y = PLOT_ID,.f = ~paste("bioTIME",.x,.y, sep = "_")),
         group = 1,
         measure = "Abundance",
         x_variable = "Year",
         title = NA_character_,
         author = NA_character_,
         journal= NA_character_,
         search = "bioTIME",
         population = "Y",
         region = NA_character_,
         kingdom = "Animalia",
         common.name = NA_character_,
         survey.freq = "yearly",
         digitized = "bioTIME",
         time.series = "Y",
         native.species = case_when(final.status == "native"~"Y",
                                    final.status == "non-native"~"N",
                                    .default = NA_character_),
         major.group = case_when(taxon == "Birds"~"Aves",
                                 taxon =="Fish"~"Actinopterygii",
                                 .default = taxon),
         country = case_when(is.na(country)&is.na(F_CODE)~NA_character_,
                            is.na(country)&!is.na(F_CODE)~"International Waters",
                            country == "Fr. Polynesia"~"French Polynesia",
                            country == "Russia"~"Russian Federation",
                            country == "United States of America"~"United States",
                            country == "Trinidad and Tobago"~"Trinidad And Tobago",
                            country == "Taiwan" ~ "Taiwan, Province Of China",
                            country == "Åland" ~ "Åland Islands",
                            .default = country),
         ls = data) |> 
  drop_na(native.species) |> 
  left_join(countries, by = "country") |> 
  select(plot,group,measure,x_variable,ls,title,author,journal,search,population,island,
         region,time.series,native.species,major.group,survey.freq,ecosystem,kingdom,species.names,
         common.name,continent,digitized)

LPI.data <- read_csv(paste(large_data_directory,"Input/LivingPlanetIndex/LPD_2024_public.csv", sep = "/")) |> 
  mutate(across(where(is.character),~na_if(.,"NULL"))) |> 
  filter(Class %in% c("Aves","Actinopterygii","Pteromyzonti","Elasmobranchii","Coelocanthi","Dipneusti",
                      "Holocephali", "Myxini")) |> 
  select(-103) |> 
  group_by(Citation) |> 
  mutate(Citation_ID = 1:n()) |> 
  ungroup() |> 
  mutate(Units = str_to_lower(Units),
         measure = case_when(str_detect(Units,"cpue")|
                             str_detect(Units,"catch per unit effort")~"CPUE",
                             str_detect(Units,"egg")|
                               str_detect(Units,"nests")|
                               str_detect(Units,"oocyte")|
                               str_detect(Units,"fry population estimate")|
                               str_detect(Units,"nest count")|
                               str_detect(Units,"nest estimate")~"Reproduction Proxy",
                             str_detect(Units,"fecal samples")|
                               str_detect(Units,"gannet")|
                               str_detect(Units,"aerial sample count")|
                               str_detect(Units,"sample: abundance (counts)")~"Abundance",
                             str_detect(Units,"biomass")~"Biomass",
                             str_detect(Units,"density")|
                               str_detect(Units,"desnity")~"Density",
                             str_detect(Units,"winetr")|
                               str_detect(Units,"nightingales per")~"Relative Abundance",
                             str_detect(Units,"occup")|
                               str_detect(Units,"% of survey routes reporting presence")~"Occupancy",
                             str_detect(Units,"encounter rate")~"Encounter Rate",
                             str_detect(Units,"hook")|
                               str_detect(Units,"net")|
                               str_detect(Units,"hour")|
                               str_detect(Units,"day")|
                               str_detect(Units,"night")|
                               str_detect(Units,"route")|
                               str_detect(Units,"per set")|
                               str_detect(Units,"trap")|
                               str_detect(Units,"tow")|
                               str_detect(Units,"sample")|
                               str_detect(Units,"minute")|
                               str_detect(Units,"dive")|
                               str_detect(Units,"trawl")|
                               str_detect(Units,"effort")|
                               str_detect(Units,"haul")|
                               str_detect(Units,"seine")|
                               str_detect(Units,"person")|
                               str_detect(Units,"scan")|
                               str_detect(Units,"seconds")|
                               str_detect(Units,"volunteer")|
                               str_detect(Units,"branch line")|
                               str_detect(Units,"patrol")|
                               str_detect(Units,"boxes")|
                               str_detect(Units,"boat")|
                               str_detect(Units,"effor")|
                               str_detect(Units,"landings")|
                               str_detect(Units,"transect and trail survey")|
                               str_detect(Units,"dolphin count per 15 mins")|
                               str_detect(Units,"recording")|
                               str_detect(Units,"100 h")|
                               str_detect(Units,"per fisher")|
                               str_detect(Units,"multi-mesh gang")~"CPUE",
                             str_detect(Units,"square")|
                               str_detect(Units,"per m2")|
                               str_detect(Units,"per m sq")|
                               str_detect(Units,"sqaure")|
                               str_detect(Units,"per msq")|
                               str_detect(Units,"per sq m")|
                               str_detect(Units,"per sq km")|
                               str_detect(Units,"sq. km")|
                               str_detect(Units,"sq.km")|
                               str_detect(Units,"per sqkm")|
                               str_detect(Units,"hectar")|
                               str_detect(Units,"per ha")|
                               str_detect(Units,"per km2")|
                               str_detect(Units,"/m2")|
                               str_detect(Units,"km2")|
                               str_detect(Units,"km-2")|
                               str_detect(Units,"km2")|
                               str_detect(Units,"/ha")|
                               str_detect(Units,"100m2")|
                               str_detect(Units,"100 m2")|
                               str_detect(Units,"/ 890 m2")|
                               str_detect(Units,"per ha")|
                               str_detect(Units,"/ha")|
                               str_detect(Units,"/ ha")|
                               str_detect(Units,"40ha")|
                               str_detect(Units,"10ha")|
                               str_detect(Units,"100ha")|
                               str_detect(Units,"ha-1")|
                               str_detect(Units,"1000ha")|
                               str_detect(Units,"per 10 ha")|
                               str_detect(Units,"per 10ha")|
                               str_detect(Units,"per 0.1 ha")|
                               str_detect(Units,"per 0.1ha")|
                               str_detect(Units,"per 9 ha")|
                               str_detect(Units,"per 9ha")|
                               str_detect(Units,"100ha")|
                               str_detect(Units,"500000m")|
                               str_detect(Units,"1000m2")|
                               str_detect(Units,"1,000 ha")|
                               str_detect(Units,"1000 ha")|
                               str_detect(Units,"m2")|
                               str_detect(Units,"m3")|
                               str_detect(Units,"2000m2")|
                               str_detect(Units,"2000 m2")|
                               str_detect(Units,"500m2")|
                               str_detect(Units,"500 m2")|
                               str_detect(Units,"20m x 5m")|
                               str_detect(Units,"kmâ²")|
                               str_detect(Units,"kmâ¯â²")|
                               str_detect(Units,"mâ²")~"Density",
                             str_detect(Units,"km")|
                               str_detect(Units,"meter")|
                               str_detect(Units,"100m")|
                               str_detect(Units,"100 m")|
                               str_detect(Units,"200 m")|
                               str_detect(Units,"200m")|
                               str_detect(Units,"20 m")|
                               str_detect(Units,"20m")|
                               str_detect(Units,"kilomet")|
                               str_detect(Units,"unit distance")|
                               str_detect(Units,"mile")~"Encounter Rate",
                            str_detect(Units,"% of max")|
                               str_detect(Units,"per 100")|
                              str_detect(Units,"% of total")|
                              str_detect(Units,"relative")~"Relative Abundance",
                            (str_detect(Units,"population abundance")|
                               str_detect(Units,"total population")|
                               str_detect(Units,"entire population")|
                               str_detect(Units,"population number")|
                               str_detect(Units,"total individuals")|
                               str_detect(Units,"population estimat")|
                               str_detect(Units,"population size")|
                               str_detect(Units,"mark")|
                               str_detect(Units,"maximum number")|
                               str_detect(Units,"total abundance")|
                               str_detect(Units,"total count")|
                               str_detect(Units,"escapement")|
                               str_detect(Units,"total number")|
                               str_detect(Units,"estimate"))&
                              !(str_detect(Units,"breeding")|
                                  str_detect(Units, "production")|
                                  str_detect(Units,"age")|
                                  str_detect(Units,"male")|
                                  str_detect(Units,">")|
                                  str_detect(Units,"pairs"))~"N",
                            !is.na(Units)&
                              !(str_detect(Units,"percent")|
                                  str_detect(Units,"%")|
                                  str_detect(Units,"g/")|
                                  str_detect(Units,"kg per")|
                                  str_detect(Units,"gram")|
                                  str_detect(Units,"transect")|
                                  str_detect(Units,"trend")|
                                  str_detect(Units,"N")|
                                  str_detect(Units,"Relative Abundance")|
                                  str_detect(Units,"Occupancy")|
                                  str_detect(Units,"Encounter Rate")|
                                  str_detect(Units,"Density")|
                                  str_detect(Units,"Reproduction Proxy")|
                                  str_detect(Units,"CPUE")|
                                  str_detect(Units,"Biomass"))~"Abundance",
                            (str_detect(Units,"change")|
                               str_detect(Units,"percent")|
                               str_detect(Units,"trend")|
                               str_detect(Units,"%")|
                               str_detect(Units,"index"))&!(
                                 str_detect(Units,"sites")|
                                   str_detect(Units,"transect")|
                                   str_detect(Units,"section")
                               )~"Percent Change",
                            str_detect(Units,"g/h")|
                              str_detect(Units,"g/t")|
                              str_detect(Units,"kg")|
                              str_detect(Units,"gram")~"Biomass",
                            str_detect(Units,"per transect")~"CPUE",
                            str_detect(Units,"with rabbit")|
                              str_detect(Units,"with sign")|
                              str_detect(Units,"positive")~"Occupancy",
                            Units=="maximum observed during spring/autumn"~"Abundance"),
         x_variable = "Year",
         group = 1,
         plot = map2_chr(.x = Citation,.y = Citation_ID, .f = ~paste(.x,.y, sep = "_")),
         title = Citation,
         author = Citation,
         journal = Citation,
         search = "LPI",
         population = "Y",
         ecosystem = System,
         kingdom = "Animalia",
         major.group = if_else(Class == "Actinopteri", true = "Actinopterygii",false = Class),
         species.names = str_replace_all(Binomial, "_", " "),
         common.name = Common_name,
         digitized = "LPI",
         native.species = if_else(Native==1,"Y","N"),
         survey.freq = "yearly",
         region = NA_character_,
         scale = Units,
         time.series = "Y",
         group = 1) |> 
  pivot_longer(31:101,names_to = "x", values_to = "y") |> 
  mutate(x = as.numeric(x),
         y = as.numeric(y)) |> 
  rename(country = Country) |> 
  left_join(countries, by = "country")|> 
  select(plot,group,measure,x_variable,title,author,journal,search,population,region,time.series,native.species,survey.freq,ecosystem,continent,kingdom,major.group,species.names,
         common.name,digitized,scale,x,y) |> 
  group_by(plot,group,measure,x_variable,title,author,journal,search,population,region,time.series,native.species,survey.freq,ecosystem,continent,kingdom,major.group,species.names,
           common.name,digitized) |> 
  nest(.key = "ls")

unique(LPI.data$Class)



#----------------
###part one: combine the data sets together###
#----------------

###boom-bust: search







####combine data###

all_data <- lit_data_tib |> 
  bind_rows(MDW) |> 
  left_join(otherdata, by = c("plot","group"))|> 
  bind_rows(LPI.data) |> 
  bind_rows(bioTIME_points)

####all_data summary

all_data_summ <- all_data |> 
  filter(species.names != "TOTAL") |> 
  filter(native.species != "Y") |> 
  mutate(time.series.length = map_dbl(.x = ls, .f = nrow),
         time.series.period = map_chr(.x = ls, .f = function(df) {
                                                        paste0("(",floor(df |> pull(x) |> min()),", ",
                                                                   floor(df |> pull(x) |> max()), ")")
                                                     }),
         years.surveyed = map_dbl(.x = ls, .f = function(df) {
                                                        df |> drop_na(y) |>
                                                                 mutate(place = 1) |>
                                                                 summarise(place = sum(place)) |> 
                                                                 pull(place)
                                                     }),
         completeness.full = years.surveyed/time.series.length,
         completeness.10yrs = if_else(time.series.length >10, true =  map_dbl(.x = ls, .f = function(df) {
           df |> 
             mutate(temp = if_else(is.na(y),true =0, false = 1),
                    comp10yrs = (temp +lag(temp)+lag(temp,2)+lag(temp,3)+
                                   lag(temp,4)+lag(temp,5)+lag(temp,6)+
                                   lag(temp,7)+lag(temp,8)+lag(temp,9))/
                      10) |> 
             select(-temp) |> 
             ungroup() |> 
             summarise(comp10yrs = max(comp10yrs, na.rm = T)) |> 
             pull(comp10yrs)
         }),false = completeness.full))


final.set <- all_data_summ |> 
  filter(time.series.length<300) |> 
  mutate(tsl.scaled = time.series.length/longevity.yrs) |> 
  filter((tsl.scaled >10| years.surveyed > 10)&years.surveyed > 7) |> 
  #filter(completeness.full >= 0.75) |> 
  filter(completeness.10yrs >= 0.75) |> 
  filter(measure != "Harvest") 

save(final.set,file = here("output","final_set.Rdata"))
save(all_data_summ, file = here("output","all_data.Rdata"))
#


#-------------------------------
###visualizations###
#-------------------------------



#genus lists
species_list <- readRDS(file = here("output","wms_specieslist.rds")) |> 
  select(Class,Order,Family) |> 
  distinct() #|> 
  write_csv(file = here("output","genus_list.csv"))
  
  



