# this script is for entering data from the long term monitoring programs
# at the aquatic ecology lab at FIU (PI: Nathan J. Dorn) prior to screening 
# the data for boom-bust dynamics in non-native populations

#---------------------------
####packages####
#---------------------------
library(tidyverse)
library(googledrive)
library(googlesheets4)
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

nonnative <- c("BELBEL","MONALB","MACSIA","CLABAT", "HOPLIT","PTEMUL",
                    "CICBIM","CICURO","CICMAN","HEMLET", "OREAUR","TILMAR",
                    "CICOCE","ASTOCE","MELTUB","POMMAC","PTEPAR", "PTEDIS")


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
            DAY = round(mean(DAY, na.rm = T))) %>% 
  unite(col = "DATE", YEAR, MONTH, DAY, sep = "/") %>% 
  mutate(DATE = ymd(DATE),
         DENSITY = COUNT/nTT) 

MDW_TT_fish_regmeans <- MDW_TT_fish_summary %>% 
  ungroup() %>% 
  group_by(REGION,SPP) %>%
  summarise(reg_ave = mean(DENSITY,na.rm = T))
  
MDW_TT_fish_summary <- MDW_TT_fish_summary %>%
  left_join(MDW_TT_fish_regmeans, by = c("REGION","SPP"))
  
MDW_TT_fish_summary <- MDW_TT_fish_summary %>%
  mutate(scale = (DENSITY - reg_ave))

###IOP fish

IOP_fish_summary <- IOP_fish_raw %>% 
  select(-(31:80)) %>% 
  pivot_longer(cols = 8:64,names_to = "SPP", values_to = "COUNT") %>% 
  filter(SPP %in% nonnative) %>% 
  mutate(th_pl = if_else(is.na(COUNT), true = 0, false = 1)) %>% 
  group_by(REGION, SITE, YEAR, MONTH,SPP) %>% 
  summarise(COUNT= sum(COUNT, na.rm = T),
            nTT = sum(th_pl, na.rm = T),
            DAY = round(mean(DAY, na.rm = T))) %>% 
  unite(col = "DATE", YEAR, MONTH, DAY, sep = "/") %>% 
  mutate(DATE = ymd(DATE),
         CPUE = COUNT/nTT) 

IOP_fish_regmeans <- IOP_fish_summary %>% 
  ungroup() %>% 
  group_by(REGION,SPP) %>%
  summarise(reg_ave = mean(CPUE,na.rm = T)) %>% 
  ungroup()

IOP_fish_summary <- IOP_fish_summary %>%
  left_join(IOP_fish_regmeans, by = c("REGION","SPP"))

IOP_fish_summary <- IOP_fish_summary %>%
  mutate(scale = (CPUE - reg_ave))



###plot the data 


IOP_fish_summary %>% 
  ggplot(aes(x = DATE, y = scale))+
  geom_point(shape = 1)+
  theme_classic()+
  facet_grid(SPP ~ REGION)

ggsave(filename = here("output/screeningplots/","IOP_fish.png"),
       device = "png", units = "in", width = 14, height = 24) 

#############
#-------------------------------------
#Separated by SITE and Montioring Program ####
#--------------------------------------
#############

#---------------------------------
###MDW THROWTRAP POINTS####
#----------------------------------


#without plot as grouping variable

MDW_TT_fish_summary <- MDW_TT_fish_raw %>% 
  pivot_longer(cols = 10:95,names_to = "SPP", values_to = "COUNT") %>% 
  filter(SPP %in% nonnative) %>% 
  mutate(th_pl = if_else(is.na(COUNT), true = 0, false = 1)) %>% 
  group_by(REGION, SITE, CUM, YEAR, MONTH,SPP) %>% 
  summarise(COUNT= sum(COUNT, na.rm = T),
            nTT = sum(th_pl, na.rm = T),
            DAY = round(mean(DAY, na.rm = T))) %>% 
  unite(col = "DATE", YEAR, MONTH, DAY, sep = "/") %>% 
  mutate(DATE = ymd(DATE),
         DENSITY = COUNT/nTT) 


###inverts

MDW_TT_invt_summary<- MDW_TT_invt_raw %>% 
  select(REGION, SITE,PLOT, CUM, YEAR, MONTH,DAY, THROW, MELTUB, POMMAC) %>% 
  pivot_longer(cols = 9:10,names_to = "SPP", values_to = "COUNT") %>% 
  mutate(th_pl = if_else(is.na(COUNT), true = 0, false = 1)) %>% 
  group_by(REGION, SITE, CUM, YEAR, MONTH,SPP) %>% 
  summarise(COUNT= sum(COUNT, na.rm = T),
            nTT = sum(th_pl, na.rm = T),
            DAY = round(mean(DAY, na.rm = T))) %>% 
  unite(col = "DATE", YEAR, MONTH, DAY, sep = "/") %>% 
  mutate(DATE = ymd(DATE),
         DENSITY = COUNT/nTT) 

MDW_TT_invt_summary %>% 
  filter(SPP == "MELTUB") %>% 
  ggplot(aes(x = DATE, y = DENSITY)) +
  geom_point(shape = 1)+
  theme_classic()+
  facet_grid(REGION~SITE)

ggsave(filename = here("output/screeningplots/","MDW_TT_fish.png"),
       device = "png", units = "in", width = 8, height = 24) 


#### set up work flow###

spp <- unique(MDW_TT_fish_summary$SPP)

MDW_TT_fish_summary %>% 
  filter(SPP == spp[8]) %>% 
  ggplot(aes(x = DATE, y = DENSITY)) +
  geom_line()+
  geom_point()+
  theme_classic()+
  facet_grid(REGION~SITE) +
  labs(title = spp[8])

ggsave(filename = here("output/screeningplots",paste(spp[8],".png", sep = "")),
       device = "png", units = in, width = 24, height = 8)  


###for loop for plots

for(i in 1:length(spp)) {
  
  MDW_TT_fish_summary %>% 
    filter(SPP == spp[i]) %>% 
    ggplot(aes(x = DATE, y = DENSITY)) +
    geom_line()+
    geom_point()+
    theme_classic()+
    facet_grid(REGION~SITE) +
    labs(title = spp[i])
  
  ggsave(filename = here("output/screeningplots/MDW",paste("MDW",spp[i],".png", sep = "_")),
         device = "png", units = "in", width = 24, height = 8) 
}

#-----------------------------
####IOP FISH DATA
#-----------------------------

#without orientation and array

IOP_fish_summary <- IOP_fish_raw %>% 
  select(-(31:80)) %>% 
  pivot_longer(cols = 8:64,names_to = "SPP", values_to = "COUNT") %>% 
  filter(SPP %in% nonnative) %>% 
  mutate(th_pl = if_else(is.na(COUNT), true = 0, false = 1)) %>% 
  group_by(REGION, SITE, YEAR, MONTH,SPP) %>% 
  summarise(COUNT= sum(COUNT, na.rm = T),
            nTT = sum(th_pl, na.rm = T),
            DAY = round(mean(DAY, na.rm = T))) %>% 
  unite(col = "DATE", YEAR, MONTH, DAY, sep = "/") %>% 
  mutate(DATE = ymd(DATE),
         CPUE = COUNT/nTT) 

##set up work flow

spp <- unique(IOP_fish_summary$SPP)

IOP_fish_summary %>% 
  filter(SPP == spp[8]) %>% 
  ggplot(aes(x = DATE, y = CPUE)) +
  geom_line()+
  geom_point()+
  theme_classic()+
  facet_grid(REGION~SITE) +
  labs(title = spp[8])

#for loop for plots

for(i in 1:length(spp)) {
  
  IOP_fish_summary %>% 
    filter(SPP == spp[i]) %>% 
    ggplot(aes(x = DATE, y = CPUE)) +
    geom_line()+
    geom_point()+
    theme_classic()+
    facet_grid(REGION~SITE) +
    labs(title = spp[i])
  
  ggsave(filename = here("output/screeningplots/IOP",paste("IOP",spp[i],".png", sep = "_")),
         device = "png", units = "in", width = 24, height = 8) 
}

#-----------------------------
####CERP DATA
#-----------------------------


CERP_TT_fish_summary <- CERP_TT_fish_raw %>% 
  select(-(79:104)) %>% 
  pivot_longer(cols = 15:78,names_to = "SPP", values_to = "COUNT")%>% 
  filter(SPP %in% nonnative) %>% 
  mutate(th_pl = if_else(is.na(COUNT), true = 0, false = 1)) %>% 
  group_by(REGION, LSU, PSU, YEAR, MONTH,SPP) %>% 
  summarise(COUNT= sum(COUNT, na.rm = T),
            nTT = sum(th_pl, na.rm = T),
            DAY = round(mean(DAY, na.rm = T))) %>% 
  unite(col = "DATE", YEAR, MONTH, DAY, sep = "/") %>% 
  mutate(DATE = ymd(DATE),
         DENSITY = COUNT/nTT) 

###inverts###

CERP_TT_invt_summary <- CERP_TT_invt_raw %>% 
  select(REGION, LSU, PSU, YEAR, MONTH,DAY, THROW, MELTUB, POMMAC) %>% 
  pivot_longer(cols = 8:9,names_to = "SPP", values_to = "COUNT") %>% 
  mutate(th_pl = if_else(is.na(COUNT), true = 0, false = 1)) %>% 
  group_by(REGION, LSU, PSU, YEAR, MONTH,SPP) %>% 
  summarise(COUNT= sum(COUNT, na.rm = T),
            nTT = sum(th_pl, na.rm = T),
            DAY = round(mean(DAY, na.rm = T))) %>% 
  unite(col = "DATE", YEAR, MONTH, DAY, sep = "/") %>% 
  mutate(DATE = ymd(DATE),
         DENSITY = COUNT/nTT) 

CERP_TT_invt_summary %>% 
  filter(REGION == "2A" & SPP == "MELTUB") %>% 
  ggplot(aes(x = DATE, y = DENSITY)) +
  geom_line()+
  geom_point()+
  theme_classic()+
  facet_wrap(~PSU)

#### set up work flow###

spp <- unique(CERP_TT_fish_summary$SPP)
regions <- unique(CERP_TT_fish_summary$REGION)


temp <- CERP_TT_fish_summary %>% 
  filter(REGION == regions[10])

temp %>% 
  filter(SPP == spp[8]) %>% 
  ggplot(aes(x = DATE, y = DENSITY)) +
  geom_line()+
  geom_point()+
  theme_classic()+
  facet_wrap(~PSU) +
  labs(title = regions[10], subtitle = spp[8])

###for loop for plots

for (j in 1:length(regions)) {
  
  temp <- CERP_TT_fish_summary %>% 
    filter(REGION == regions[j])
  
  for(i in 1:length(spp)) {
    
    temp %>% 
      filter(SPP == spp[i]) %>% 
      ggplot(aes(x = DATE, y = DENSITY)) +
      geom_line()+
      geom_point()+
      theme_classic()+
      facet_wrap(~PSU) +
      labs(title = regions[j], subtitle = spp[i])
    
    ggsave(filename = here("output/screeningplots/CERP",paste("CERP",regions[j],spp[i],".png", sep = "_")),
           device = "png", units = "in") 
  }
}

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
  group_by(REGION, SITE, YEAR, MONTH,SPECIES) %>% 
  summarise(COUNT= sum(COUNT, na.rm = T),
            nTT = sum(th_pl, na.rm = T),
            DAY = round(mean(DAY, na.rm = T))) %>% 
  unite(col = "DATE", YEAR, MONTH, DAY, sep = "/") %>% 
  mutate(DATE = ymd(DATE),
         CPUE = COUNT/nTT) 

spp <- unique(MDW_EF_slough_summary$SPECIES)

MDW_EF_slough_summary %>% 
  filter(SPECIES == spp[8]) %>% 
  ggplot(aes(x = DATE, y = CPUE)) +
  geom_line()+
  geom_point()+
  theme_classic()+
  facet_grid(REGION~SITE) +
  labs(title = spp[8])

ggsave(filename = here("output/screeningplots/MDW_EF_slough",paste("MDW_EF_slough",spp[8],".png", sep = "_")),
       device = "png", units = "in", width = 24, height = 12)

#for loop for plots

for(i in 1:length(spp)) {
  
  MDW_EF_slough_summary %>% 
    filter(SPECIES == spp[i]) %>% 
    ggplot(aes(x = DATE, y = CPUE)) +
    geom_line()+
    geom_point()+
    theme_classic()+
    facet_grid(REGION~SITE) +
    labs(title = spp[i])
  
  ggsave(filename = here("output/screeningplots/MDW_EF_slough",paste("MDW_EF_slough",spp[i],".png", sep = "_")),
         device = "png", units = "in", width = 24, height = 12)
}

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
  filter(SPECIES %in% nonnative) %>% 
  mutate(th_pl = if_else(is.na(COUNT), true = 0, false = 1)) %>% 
  group_by(REGION, SITE, YEAR, MONTH,SPECIES) %>% 
  summarise(COUNT= sum(COUNT, na.rm = T),
            nTT = sum(th_pl, na.rm = T),
            DAY = round(mean(DAY, na.rm = T))) %>% 
  unite(col = "DATE", YEAR, MONTH, DAY, sep = "/") %>% 
  mutate(DATE = ymd(DATE),
         CPUE = COUNT/nTT) 

spp <- unique(MDW_EF_aghole_summary$SPECIES)
regions <- unique(MDW_EF_aghole_summary$REGION)

temp <- MDW_EF_aghole_summary %>% 
  filter(REGION == regions[3] )

temp %>% 
  filter(SPECIES == spp[8]) %>% 
  ggplot(aes(x = DATE, y = CPUE)) +
  geom_line()+
  geom_point()+
  theme_classic()+
  facet_wrap(~SITE) +
  labs(title = spp[8])

ggsave(filename = here("output/screeningplots/MDW_EF_aghole",paste("MDW_EF_aghole",regions[3],spp[8],".png", sep = "_")),
       device = "png", units = "in", width = 12, height = 6)

#for loop for plots

for (j in 1:length(regions)) {
  
  temp <- MDW_EF_aghole_summary %>% 
    filter(REGION == regions[j] )
  
  for(i in 1:length(spp)) {
    
    temp %>% 
      filter(SPECIES == spp[i]) %>% 
      ggplot(aes(x = DATE, y = CPUE)) +
      geom_line()+
      geom_point()+
      theme_classic()+
      facet_wrap(~SITE) +
      labs(title = spp[i])
    
    ggsave(filename = here("output/screeningplots/MDW_EF_aghole",paste("MDW_EF_aghole",regions[j],spp[i],".png", sep = "_")),
           device = "png", units = "in")
  }
}




