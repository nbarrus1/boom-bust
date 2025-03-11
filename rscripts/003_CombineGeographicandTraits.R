###this script is dedicated to combining the geographic and trait information to the plots
###and obtaining timeseries length, completeness, and performing some first summaries describing
###the time series

#remove all 

rm(list = ls())


#libraries

library(tidyverse)
library(patchwork)
library(here)

##data
load(here("output","literatrure_timeseries_plots.Rdata"))
load(here("output","literatrure_timeseries.Rdata"))

otherdata <- read_csv(here("data","BoomBust_Review - TimeSeries_Identification.csv")) 

####combine data###

all_data <- lit_data_tib |> 
  left_join(otherdata, by = c("plot","group"))


#check for combining errors

all_data |> 
  filter(is.na(title)) |> 
  pull(plot,group)

unique(all_data |> 
  pull(species.names))

test <- all_data |> 
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
         completeness = years.surveyed/time.series.length)





all_data_summ |> 
  ggplot(aes(x = completeness)) +
  geom_histogram()

