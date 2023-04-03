##Reading in and processing the Gas exchange data.

###Bridgers has an irga processing Code Somewhere.


#Before Reading in IRGA data, one needs to convert from .xml to csv.
#do this in finder

##Reading in TC data
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# this line finds all those .csv files you just made. Change the regex search string to fit your data storage structure
  allFiles <- Sys.glob("./TC 1 Data/01122023_Cotton_TC1_IRGA/*.csv")
 allFiles_configNEW <- c("01122023_Cotton_TC1_.csv","01122023_Cotton_TC1_1.csv","01122023_Cotton_TC1_IRGA1_.csv",
                         "01122023_Cotton_TC1_IRGA1_1.csv" ,"01122023_Cotton_TC1_IRGA1_2.csv","01132023_Cotton_TC1_.csv",
                         "01132023_Cotton_TC1_IRGA1_.csv","01132023_Cotton_TC1_IRGA1_1.csv","01142023_Cotton_TC1IRGA1_.csv" )
   allFiles_configOLD <- c("01132023_Cotton_TC1_2.csv","01132023_Cotton_TC1_1.csv")


   
   
library(tidyverse)

# Get the functions needed to clean the raw files. Make sure that this points towards the correct source file. If you cloned this from the github, there should be no problems
source("LICOR_6400_Cleaner.R")

  
  # load the clean data into a dataframe and save it
  setwd("/Users/ben/Dropbox/GreenHouseData/TimeCourse")
  datnew <- clean_rawNEW_csvs("TC 1 Data/01122023_Cotton_TC1_IRGA") ### put the file that contains all of your .csv files into this function
  datold <- clean_raw_csvs("./TC 1 Data/01122023_Cotton_TC1_IRGA") ### put the file that contains all of your .csv files into this function
  
  write_csv(dat, "./TC 1 Data/Clean_LICOR_TC1.csv")
  dat <- read_csv("./TC 1 Data//Clean_LICOR_TC1.csv")
  
  
#I dont like Bridgers function
  clean_unremarked <- function(filepath){
    
    
  }