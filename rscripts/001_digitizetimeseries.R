###This script is dedicated to digitizing and extracting the data from the time-series
###obtained from my literature search

#remove everything

rm(list = ls())

#load library metaDigitise

library(metaDigitise)

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

lit_data_ls <- metaDigitise(dir = "C:/Users/bryzb/OneDrive/Documents/Nate/FIU/Dissertation/boom-bust/TimeSeriesPictures_Literature/TimeSeriesFigures/",
                     summary = FALSE)
#look at the created list
lit_data_ls

#------------------------
####Data wrangling: Prep the list data to get in useable format###
#------------------------

