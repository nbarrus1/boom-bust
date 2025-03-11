# this script is for entering data from the long term monitoring programs
# at the aquatic ecology lab at FIU (PI: Nathan J. Dorn) prior to screening 
# the data for boom-bust dynamics in non-native populations

#---------------------------
####packages####
#---------------------------
library(tidyverse)
library(googledrive)
library(googlesheets4)
library(purrr)
library(here)

#---------------------------
####load data####
#---------------------------


# download data from google drive ###
drive_auth()

gd_csv <- drive_find(type = "csv") %>% 
  slice(1:7)
  

for(i in 1:length(gd_csv$name)){

drive_download(as_id(gd_csv$id[i]),
               path = here("data", paste(gd_csv$name[i])),
               verbose = T,
               overwrite = T)
  i
}


#

MDW_TT_fish_raw <- read_csv(here("data", paste(gd_csv$name[6])))
MDW_TT_invt_raw <- read_csv(here("data", paste(gd_csv$name[7])))
MDW_EF_slough_raw <- read_csv(here("data", paste(gd_csv$name[4])),
                              na = ".")
MDW_EF_aghole_raw <- read_csv(here("data", paste(gd_csv$name[5])),
                              na = ".")
CERP_TT_fish_raw <- read_csv(here("data", paste(gd_csv$name[1])))
CERP_TT_invt_raw <- read_csv(here("data", paste(gd_csv$name[3])))
IOP_fish_raw <- read_csv(here("data", paste(gd_csv$name[2])))

#--------------------------
####Data Wrangling####
#--------------------------

nonnative <- c("MONALB","MACSIA",
                    "CICURO","HEMLET")


##################
#-----------------------------
####REGION LEVEL with Montitoring Programs Combined
#------------------------------
##################

###MDW Fish###

MDW_TT_fish_summary <- MDW_TT_fish_raw %>% 
  pivot_longer(cols = 10:95,names_to = "SPP", values_to = "COUNT") %>% 
  filter(SPP %in% nonnative) %>% 
  mutate(th_pl = if_else(is.na(COUNT), true = 0, false = 1)) %>% 
  group_by(REGION, SITE, CUM, YEAR, MONTH,SPP) %>% 
  summarise(COUNT= sum(COUNT, na.rm = T),
            nTT = sum(th_pl, na.rm = T),
            DAY = round(mean(DAY, na.rm = T)))%>% 
  unite(col = "DATE", YEAR, MONTH, DAY, sep = "/") |>  
  mutate(DATE = ymd(DATE),
         DENSITY = COUNT/nTT) 

#without plot as grouping variable

MDW_TT_fish_summary <- MDW_TT_fish_raw %>% 
  pivot_longer(cols = 10:95,names_to = "SPP", values_to = "COUNT") %>% 
  filter(SPP %in% nonnative) %>% 
  mutate(th_pl = if_else(is.na(COUNT), true = 0, false = 1)) %>% 
  group_by(REGION, CUM, YEAR, MONTH,SPP) %>% 
  summarise(COUNT= sum(COUNT, na.rm = T),
            nTT = sum(th_pl, na.rm = T),
            DAY = round(mean(DAY, na.rm = T))) %>% 
  unite(col = "DATE", YEAR, MONTH, DAY, sep = "/") %>% 
  mutate(DATE = ymd(DATE),
         DENSITY = COUNT/nTT) 




#### time sereies plots###


MDW_TT_fish_plots <- MDW_TT_fish_summary |> 
  group_by(SPP, REGION) |> 
  nest() |> 
  mutate(
    timeseries = map(data, ~ggplot(., aes(x = DATE, y = DENSITY))+
                       geom_line()+
                       geom_point()+
                       theme_classic()+
                       labs(title = REGION, subtitle = SPP))
  )

MDW_TT_fish_plots$timeseries


####-------------------------------
####MDW electrofishing slough
#----------------------------------

#not grouped by PLOT

MDW_EF_slough_summary <- MDW_EF_slough_raw %>% 
  select(REGION,CUM, YEAR, PERIOD, MONTH, DAY, SITE, PLOT, TRANSECT,SPECIES) %>%
  group_by(REGION,CUM, YEAR, PERIOD, MONTH, DAY, SITE, PLOT,TRANSECT,SPECIES) %>% 
  summarise(COUNT = n()) %>%
  pivot_wider(names_from = SPECIES, values_from = COUNT, values_fill = 0) %>% 
  pivot_longer(cols = 10:63, names_to = "SPECIES", values_to = "COUNT") %>% 
  filter(SPECIES %in% nonnative) %>% 
  mutate(th_pl = if_else(is.na(COUNT), true = 0, false = 1)) %>% 
  group_by(REGION, YEAR, MONTH,SPECIES) %>% 
  summarise(COUNT= sum(COUNT, na.rm = T),
            nTT = sum(th_pl, na.rm = T),
            DAY = round(mean(DAY, na.rm = T))) %>% 
  unite(col = "DATE", YEAR, MONTH, DAY, sep = "/") %>% 
  mutate(DATE = ymd(DATE),
         CPUE = COUNT/nTT) 


MDW_EF_slough_plots <- MDW_EF_slough_summary |> 
  group_by(SPECIES, REGION) |> 
  nest() |> 
  mutate(
    timeseries = map(data, ~ggplot(., aes(x = DATE, y = CPUE))+
                       geom_line()+
                       geom_point()+
                       theme_classic()+
                       labs(title = REGION, subtitle = SPECIES))
  )

MDW_EF_slough_plots$timeseries

####-------------------------------
####MDW electrofishing alligator hole
#----------------------------------

#not grouped by PLOT

MDW_EF_aghole_summary <- MDW_EF_aghole_raw %>% 
  select(REGION,CUM, YEAR, PERIOD, MONTH, DAY, SITE, TRANSECT,SPECIES) %>%
  group_by(REGION,CUM, YEAR, PERIOD, MONTH, DAY, SITE, TRANSECT,SPECIES) %>% 
  summarise(COUNT = n()) %>%
  pivot_wider(names_from = SPECIES, values_from = COUNT, values_fill = 0) %>% 
  pivot_longer(cols = 9:55, names_to = "SPECIES", values_to = "COUNT") %>% 
  filter(PERIOD < 3) |> 
  filter(SPECIES %in% nonnative) %>% 
  mutate(th_pl = if_else(is.na(COUNT), true = 0, false = 1)) %>% 
  group_by(REGION, YEAR, MONTH,SPECIES) %>% 
  summarise(COUNT= sum(COUNT, na.rm = T),
            nTT = sum(th_pl, na.rm = T),
            DAY = round(mean(DAY, na.rm = T))) %>% 
  unite(col = "DATE", YEAR, MONTH, DAY, sep = "/") %>% 
  mutate(DATE = ymd(DATE),
         CPUE = COUNT/nTT) 

MDW_EF_aghole_plots <- MDW_EF_aghole_summary |> 
  group_by(SPECIES, REGION) |> 
  nest() |> 
  mutate(
    timeseries = map(data, ~ggplot(., aes(x = DATE, y = CPUE))+
                       geom_line()+
                       geom_point()+
                       theme_classic()+
                       labs(title = REGION, subtitle = SPECIES))
  )

MDW_EF_aghole_plots$timeseries
