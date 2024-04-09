###This script is dedicated to digitizing and extracting the data from the time-series
###obtained from my literature search

#remove everything

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
         x_variable = "Year") |> 
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


####scatter plot nested by group and plot####


#obtain vector of plot names for the haubrock papers that use cumulative abundance
haubrock <-  scatter.ls |>
  set_names(names(scatter.ls)) |> 
  enframe(name = "plot",value = "ls") |> 
  mutate(ls = map(ls,as.data.frame)) |> 
  unnest(ls) |> 
  select(-pch,-col) |> 
  group_by(plot,group) |> 
  filter(y_variable == "Cumulative Abundance") |> 
  select(plot) |> 
  unique()

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


scatter.tib <- scatter.ls |>
  set_names(names(scatter.ls)) |> 
  enframe(name = "plot",value = "ls") |> 
  mutate(ls = map(ls,as.data.frame),
         #convert cumulative abundance to abundance (see haubrock data)
         ls = if_else(plot %in% haubrock$plot,
                      true = map(ls,convert.to.abunance),
                      false = ls)) |> 
  unnest(ls) |> 
  select(-pch,-col) |> 
  group_by(plot,group) |> 
  nest(.key = "ls")


# read in and comibe the data from tables

table.tib <- read_excel(here("data","BoomBust_DatafromTables.xlsx")) |> 
  group_by(plot,group) |> 
  nest(.key = "ls")
#combine the reorganized bar graph data to the scatter plots

lit_data_tib <- scatter.tib |> 
  bind_rows(box.tib) |> 
  bind_rows(table.tib) |> 
  filter(plot %in% haubrock$plot)#|> 
  #unnest(cols = ls) #|> 
  #separate_wider_delim(y_variable,names = c("measure","scale"),delim = " ",
  #                     too_few = "align_start", too_many = "merge")




#fucntion for plotting




plot_timeseries <- function(df) {
  df |> 
    ggplot(aes(x = x, y = y))+
    geom_line()+
    geom_point(size = 2, color = "black", shape = 21, fill = "#666666")+
    theme_bw(base_size = 12) + 
    theme(
      axis.ticks.length = unit(.25, "cm"),
      axis.title.y = element_text(vjust = 2), 
      plot.margin = unit(c(0,0,0,0), "cm")
    )
}



lit_data_plots <- lit_data_tib |> 
  mutate(timeseries = map(ls, .f = plot_timeseries))

lit_data_plots$timeseries
