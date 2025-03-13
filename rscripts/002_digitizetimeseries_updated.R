###This script is dedicated to digitizing and extracting and processing the data from the time-series
###obtained from the literature search

#remove everything in environment

rm(list = ls())

#load library metaDigitise

library(metaDigitise)
library(tidyverse)
library(purrr)
library(here)
library(magrittr)
library(readxl)

theme_set(theme_bw())

#the function metaDigitise is an interactive function that allows you to 1) digitize,
#and extract data, 2) import data that has already been digitized, or 3) edit alread
#digitized data.

#1) Digitizing and extracting data comes from pictures of plots and uses 
# the scaling of the two axis to estimate the value. of each data point
#after digitizing and extracting the data it saves everything to a folder called caldot
#in the directory named.  When Digitizing the data, you follow the prompt in the r 
#environment below and it walks you through the somewhat user friendly steps, 
#to find more information about the step look up the r documentation online. All data
#are stored as a list of seperate dataframes from each plot named after the file name.

#2)import the data is probably the easiest but requires that the caldot folder in the 
#directory named and also already digitized and extracted data.   All data
#are stored as a list of seperate dataframes from each plot named after the file name.

#3) editing the data, if there is something wrong with the digitizing or the extraction
#this is how you can go back and edit or redo the digitization of plots. following the 
#prompts in the r environment should be enough to figure out what is needed. more info
#is given in the r documentation.


lit_data_ls<- metaDigitise(dir = here("TimeSeriesPictures_Literature/TimeSeriesFigures/"),
                           summary = FALSE)



#------------------------
####Data wrangling: Prep the list data to get in useable format###
#------------------------


######list inspection####

str(lit_data_ls,max.level = 1)

#we have nested list, first level elements 1) barplots = 8 lists and 
#2) scatterplots = 145 lists

str(lit_data_ls[[1]], list.len = 4)
str(lit_data_ls[[2]], list.len = 4)


#at the second level is a list of the data extracted from each plot
#one problem i for see having is that in the barplots the "id" are years
#as character (sometimes these id have both a year-mon, or year_location)

#The year only information will have to be split in these cases, 
#month might also have to be extracted into a different variable
#the locations can be kept in id



#####Get the list into a tibble####

#split the lists into seperate list by bar or scatter plots

box.ls <- lit_data_ls[[1]]
scatter.ls <- lit_data_ls[[2]]

#----------------------------------------------------
#####data wrangling for box plots####
#-----------------------------------------------------

#make them into a tibble with each plot as a figure

###reorganize the box.plot data so the year bars are now x variables
###and nest by plot and group, so I have a single time series for everything
box.tib <-box.ls |> 
  set_names(names(box.ls)) |> 
  enframe(name = "plot",value = "ls") |> 
  unnest(ls) |> 
  mutate(x = str_sub(id,start = 1L, end = 4L),
         temp = str_sub(id, start = 7L, end = str_length(id)-1),
         month = case_when(temp=="Jan"~1,
                           temp=="Feb"~2,
                           temp=="Mar"~3,
                           temp=="Apr"~4,
                           temp=="May"~5,
                           temp=="Jun"~6,
                           temp=="Jul"~7,
                           temp=="Aug"~8,
                           temp=="Sep"~9,
                           temp=="Oct"~10,
                           temp=="Nov"~11,
                           temp=="Dec"~12),
         id = if_else(temp %in% c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"),
                      true = "", false = paste(temp)),
         x_variable = if_else(is.na(month),true ="Year", false = "Mon-Year"))|> 
  select(-temp) |> 
  rename(y = mean,
         y_variable = variable,
         y_error = error) |> 
  group_by(plot) |> 
  mutate(group = match(paste(id),unique(paste(id))),
         x = if_else(is.na(month), true = as.double(x),
                     false = as.double(x)+((month-1)/12))) |> 
  group_by(plot,group) |> 
  nest(.key = "ls")


str(box.tib$ls[1])

#------------------------------------------------
####Data wrangling of scatter plots####
#-----------------------------------------------

#kraemer plot

Kraemer <- scatter.ls |> 
  set_names(names(scatter.ls)) |> 
  enframe(name = "plot", value = "ls") |> 
  filter(str_detect(plot,"354_Kraemer")) |> 
  unnest(ls) |> 
  mutate(x = round(x),
         x_variable = "Year",
         y = y*(0.49*26)) |> 
  group_by(plot, id, x,group, col,pch, y_variable,x_variable) |> 
  summarise(y = sum(y),
            n = n(),
            y = y/(26*0.49*n)) |> 
  ungroup() |> 
  group_by(plot,group) |> 
  nest(.key = "ls")

#walsh plot
Walsh <- scatter.ls |>
  set_names(names(scatter.ls)) |> 
  enframe(name = "plot",value = "ls")  |> 
  filter(str_detect(plot,"156_Walsh")) |> 
  unnest(ls) |> 
  mutate(x = round(x)) |> 
  select(-group,-pch,-col) |> 
  pivot_wider(values_from = y, names_from = id) |> 
  mutate(bythotrephes = bythotrephes - (Cisco + `yellow perch` + `White bass` + leptodora),
         bythotrephes = if_else(bythotrephes < 0, true = 0, false = bythotrephes),
         group = 1) |> 
  select(plot,group, x, y_variable, x_variable,bythotrephes) |> 
  rename(y = bythotrephes) |> 
  group_by(plot,group) |> 
  nest(.key = "ls")


#obtain vector of plot names for the haubrock papers that use cumulative abundance
haubrock <-  scatter.ls |>
  set_names(names(scatter.ls)) |> 
  enframe(name = "plot",value = "ls") |> 
  mutate(ls = map(ls,as.data.frame)) |> 
  unnest(ls) |> 
  select(-pch,-col) |> 
  group_by(plot,group) |> 
  filter(y_variable == "Cumulative Abundance") |> 
  ungroup() |> 
  select(plot) |> 
  distinct() |> 
  pull(plot)



#obtain vector of plot names that have broken axis
brokenaxis <-  scatter.ls |>
  set_names(names(scatter.ls)) |> 
  enframe(name = "plot",value = "ls") |> 
  mutate(ls = map(ls,as.data.frame)) |> 
  filter(str_detect(plot,"Kauppietal_BiologicalInvasions_2015_Fig3A")|
           str_detect(plot,"Kauppietal_BiologicalInvasions_2015_Fig3B")|
           str_detect(plot,"Morrisetal_JournalofAridEnvironments_2013_Fig5Density-JER")|
           str_detect(plot,"Breedetal_WildlifeResearch_2016_Fig1")) |> 
  select(plot) |> 
  distinct() |> 
  pull(plot)

#bind the two digitized versions (different scales) of the plots together
brokenaxis.corrected <- scatter.ls |>
  set_names(names(scatter.ls)) |> 
  enframe(name = "plot",value = "ls") |> 
  mutate(ls = map(ls,as.data.frame)) |> 
  filter(str_detect(plot,"Kauppietal_BiologicalInvasions_2015_Fig3A")|
           str_detect(plot,"Kauppietal_BiologicalInvasions_2015_Fig3B")|
           str_detect(plot,"Morrisetal_JournalofAridEnvironments_2013_Fig5Density-JER")|
           str_detect(plot,"Breedetal_WildlifeResearch_2016_Fig1")) |> 
  mutate(full.plot = case_when(str_detect(plot,"Kauppietal_BiologicalInvasions_2015_Fig3A")~
                                 "339_Kauppietal_BiologicalInvasions_2015_Fig3A_full",
                               str_detect(plot,"Kauppietal_BiologicalInvasions_2015_Fig3B")~
                                 "340_Kauppietal_BiologicalInvasions_2015_Fig3A_full",
                               str_detect(plot,"Morrisetal_JournalofAridEnvironments_2013_Fig5Density-JER")~
                                 "341_Morrisetal_JournalofAridEnvironments_2013_Fig5Density-JER_full",
                               str_detect(plot,"Breedetal_WildlifeResearch_2016_Fig1")~
                                 "107_Breedetal_WildlifeResearch_2016_Full"),
         df = c(rep("df1", times = 4),rep("df2", times = 4),"df3")) |> 
  select(-plot) |> 
  pivot_wider(names_from=df, values_from = ls) |> 
  mutate(ls = map2(.x = df1, .y = df2, .f = bind_rows),
         ls = map2(.x = ls, .y =df3, .f = bind_rows)) |> 
  select(-df1,-df2,-df3) |> 
  rename(plot = full.plot)

#fix the cecere plot so it is mon-year not mon with year as grouping

Cecere <- scatter.ls |>
  set_names(names(scatter.ls)) |> 
  enframe(name = "plot",value = "ls") |> 
  mutate(ls = map(ls,as.data.frame)) |> 
  filter(str_detect(plot,"Cecere")) |> 
  unnest(cols = ls) |> 
  mutate(month = round(x),
         id = as.double(as.character(id)),
         x = id + ((month-1)/12),
         id = as.factor(NA_character_),
         x_variable = "Mon-Year",
         group = 1) |> 
  group_by(plot) |> 
  nest(.key = "ls")


#fix the pavey plot so it is in mon-year not session id as grouping

Pavey <- scatter.ls |>
  set_names(names(scatter.ls)) |> 
  enframe(name = "plot",value = "ls") |> 
  mutate(ls = map(ls,as.data.frame)) |> 
  filter(str_detect(plot,"Pavey")) |> 
  unnest(cols = ls) |> 
  mutate(x.session = round(x),
         x = case_when(x.session == 1 ~ (2007 + (9/12)),
                       x.session == 2 ~ (2008 + (3/12)),
                       x.session == 3 ~ (2008 + (6/12)),
                       x.session == 4 ~ (2008 + (10/12)),
                       x.session == 5 ~ (2009 + (2/12)),
                       x.session == 6 ~ (2009 + (5/12)),
                       x.session == 7 ~ (2009 + (8/12)),
                       x.session == 8 ~ (2010 + (1/12)),
                       x.session == 9 ~ (2010 + (4/12)),
                       x.session == 10 ~ (2010 + (11/12)),
                       x.session == 11 ~ (2011 + (4/12))),
         x_variable = "Mon-Year") |> 
  select(-x.session)|> 
  group_by(plot) |> 
  nest(.key = "ls")

Pavey

#create a function to convert cumulative abundance to abundance
convert.to.abunance <- function(df) {
  df |> rename(cumabund = y) |> 
    mutate(behind = lag(cumabund),
           y_variable = "N",
           y_variable_old = "Cumulative Abundance",
           behind = if_else(is.na(behind),true = 0,false = behind),
           y = cumabund-behind) |> 
    select(-behind)
}


##create a function to add na and fix year sequences 
#not this only will work with year or time not mon-year
#k

fix.x.aagard <- function (df) {
  temp <- df |> 
    mutate(x.round = round(x),
           x.floor = floor(x),
           x.ceiling = ceiling(x),
           x.seq = 1:length(x.round) + (min(x.round)-1))
  
  temp2 <- tibble(x.temp = full_seq(temp$x.round,1),
                  y = rep(NA_real_, times = length(x.temp)),
                  id = rep(unique(temp$id), times = length(x.temp)),
                  y_variable = rep(unique(temp$y_variable), times = length(x.temp)),
                  x_variable = rep(unique(temp$x_variable), times = length(x.temp)),
                  x = rep(NA_real_, times = length(x.temp)),
                  cumabund = rep(NA_real_, times = length(x.temp)),
                  y_variable_old = rep(NA_character_, times = length(x.temp)),
                  month = rep(NA_integer_, times = length(x.temp)))
  
  
  
  if (nrow(temp)==length(full_seq(temp$x.round,1))|
      nrow(temp)==length(full_seq(temp$x.ceiling,1))|
      nrow(temp)==length(full_seq(temp$x.floor,1))) {
    temp |> 
      mutate(x = x.seq) 
  } else {
    temp2 <- temp2 |> 
      filter(! x.temp %in% temp$x.round) |> 
      rename(x.round = x.temp)
    
    temp |> 
      bind_rows(temp2) |> 
      arrange(x.round) |> 
      mutate(x = x.round) 
  }
}






###Apply all functions and fixes to the scatter plot data

scatter.tib <- scatter.ls |>
  set_names(names(scatter.ls)) |> 
  enframe(name = "plot",value = "ls") |> 
  mutate(ls = map(ls,as.data.frame),
         #convert cumulative abundance to abundance (see haubrock data)
         ls = if_else(plot %in% haubrock,
                      true = map(ls,convert.to.abunance),
                      false = ls)) |> 
  #remove the seperate digitized versions of the broken axis plots
  filter(!plot %in% brokenaxis) |>
  filter(!plot %in% Cecere$plot) |> 
  filter(!str_detect(plot,"Pavey")) |> 
  filter(!str_detect(plot,"Kraemer")) |> 
  #add the full version of the broken axis plots
  bind_rows(brokenaxis.corrected,
            Cecere,
            Pavey) |>
  unnest(ls) |> 
  select(-pch,-col) |> 
  group_by(plot,group) |> 
  nest(.key = "ls")




# read in and comibe the data from tables

table.tib <- read_excel(here("data","BoomBust_DatafromTables.xlsx")) |> 
  group_by(plot,group) |> 
  nest(.key = "ls")


###data wrangling functions to add NAs to nonsample years, to take maximum density for data at the month scale, and 
##to take mean if more than one sample was taken at the same time period,



fill.seq <- function(df,x) {
  
  if (x =="Year"|x=="Time") {
    
    df |> mutate(x = round(x)) |> 
      group_by(id, scale, x) |>
      summarise(y = mean(y)) |> 
      complete(x = full_seq(x,1))
    
  } else if(x=="Mon-Year") {
    
    df |> mutate(x = round(x)) |> 
      group_by(id, scale, x) |>
      summarise(y = max(y)) |> 
      complete(x = full_seq(x,1))
    
    
  }
  
  
}




#combine the reorganized bar graph data to the scatter plots

lit_data_tib <- scatter.tib |> 
  bind_rows(box.tib) |> 
  bind_rows(table.tib) |> 
  unnest(cols = ls) |> 
  separate_wider_delim(y_variable,names = c("measure","scale"),delim = " ",
                       too_few = "align_start", too_many = "merge") |> 
  mutate(y = if_else(y < 0, true = 0, false = y)) |> 
  group_by(plot,group,x_variable,measure) |> 
  nest(.key = "ls") |> 
  mutate(ls.test = if_else(str_detect(plot,"Aagaard"), true = map(.x = ls, .f = fix.x.aagard),
                                                       false = map2(.x = ls,.y = x_variable, .f = fill.seq)))

lit_data_tib <- lit_data_tib |> 
  mutate(measure = if_else(measure == "Annual", true = "Annual Maximum Density", false = measure),
         kraemer_table = lit_data_tib |> 
           filter(str_detect(plot,"353_Kraemeretal_NortheasternNaturalist_2007_Table2")) |> 
           pull(ls)) |> 
  filter(!(str_detect(plot,"353_Kraemeretal_NortheasternNaturalist_2007_Table2"))) |> 
  mutate(ls = if_else(str_detect(plot,"354_Kraemer"), true = map2(.x = ls, .y = kraemer_table, .f = bind_rows),
                      false = ls)) |> 
  select(-kraemer_table)


save(lit_data_tib,file = here("output","literatrure_timeseries.Rdata"))



#function for plotting




plot_timeseries <- function(df) {
  df |> 
    ggplot(aes(x = x, y = y))+
    geom_line()+
    geom_point(size = 2, color = "black", shape = 21, fill = "#666666")+
    theme_bw(base_size = 12) + 
    theme(
      axis.ticks.length = unit(.25, "cm"),
      axis.title.y = element_text(vjust = 2) 
      #plot.margin = unit(c(0,0,0,0), "cm")
    )+
    labs(x = unique(df$x_variable), y = unique(df$measure))
}



lit_data_plots <- lit_data_tib |>
  unnest(cols = ls.test) |> 
  group_by(plot,group) |> 
  nest(.key = "ls") |> 
  mutate(timeseries = map(ls, .f = plot_timeseries)) |> 
  select(-ls)


lit_data_plots |> 
  filter(str_detect(plot,"Breed")) |> 
  #filter(group == "1") |> 
  pull(timeseries)


save(lit_data_plots,file = here("output","literatrure_timeseries_plots.Rdata"))
