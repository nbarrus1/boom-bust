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
library(here)


theme_set(theme_bw())

## read in the data
large_data_directory <- "C:/Users/nbarr/OneDrive/Pictures/Documents/Career/Journal Publications/Manuscripts/Dissertation/Large_Data_Files/CH_1-Boom-Bust"

#digitized data
load(here("output","literatrure_timeseries.Rdata"))

#dorn lab data
load(here("output","MDW_mayan-jewelfish.Rdata"))

#rehage lab data (accessed through EDI)

package_id <- "knb-lter-fce.1164.13"

entity_id <- read_data_entity_names(package_id) |> 
  pull(entityId)

entity_name <- read_data_entity_names(package_id) |> 
  pull(entityName)

rehage_raw <- read_data_entity(
  packageId = package_id,
  entityId = entity_id
)

con <- rawConnection(rehage_raw)

rehage_data <- read_csv(
  con,
  na = c("", " ", ".", "NA", "-9999", "-9999.0", "-9999.00"),
  show_col_types = FALSE
) 

close(con)


otherdata <- read_csv(here("data","BoomBust_Review - TimeSeries_Identification.csv"))

rehage_data |> 
  group_by(across(everything())) |> 
  summarise(n = n()) |> 
  filter(n >1)|> 
  write_csv(file = here("output","duplicates_all.csv"))

rehage_data |> 
  group_by(ID,Year,Date,Season,River,Creek,Bout,Distance,DOMGL,Salinity,TempC,Catch,CPUE) |> 
  summarise(n = n()) |> 
  filter(n >1) |> 
  write_csv(file = here("output","duplicates_IDtoCPUE.csv"))

rehage_data |> 
  pivot_longer(14:76,names_to = 'spp',values_to = 'count') |> 
  group_by(ID,Year,Date,Season,River,Creek,Bout,Distance,DOMGL,Salinity,TempC,Catch,CPUE) |> 
  summarise(test.total = sum(count)) |> 
  filter(Catch != test.total)|> 
  write_csv(file = here("output","SumofSpecies_MismatchCatch.csv"))
  
###bioTIME data
bioTIME_points <- readRDS(here("output","bioTIME_status_07.rds")) 

GPD.metadata <- read_csv(paste(large_data_directory,"Input/GlobalPopulationDynamicsDatabase/LPD_2024_public.csv", sep = "/"))


#----------------
###part one: combine the data sets together###
#----------------

###boom-bust: search







####combine data###

all_data <- lit_data_tib |> 
  bind_rows(MDW) |> 
  left_join(otherdata, by = c("plot","group"))


#check for combining errors

all_data |> 
  filter(is.na(title)) |> 
  pull(plot,group)

unique(all_data |> 
  pull(species.names))

all_data |> 
  mutate(time.series.length = map_dbl(.x = ls, .f = nrow),
         time.series.period = map_dbl(.x = ls, .f = function(df) {
           floor(df |> pull(x) |> max())- floor(df |> pull(x) |> min())+1
         }),
         time.series.period.chr = map_chr(.x = ls, .f = function(df) {
           paste0("(",floor(df |> pull(x) |> min()),", ",
                  floor(df |> pull(x) |> max()), ")")
         })) |> 
  select(time.series.length, time.series.period,time.series.period.chr) |> 
  filter(time.series.length != time.series.period) 

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







