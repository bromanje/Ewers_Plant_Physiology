require(tidyverse)
require(plyr)
## this function gets rid of rows that aren't necessary 
LC<-function(dat){
  #creates a comments column
  dat$comment<-"kitten"
  dat <- dat[dat$HHMMSS != "in",] # deletes column names in the data frame
  dat <- dat[dat$Obs != "Obs",]
  #This is to put comments in comment column####
  for (i in 1:nrow(dat)) {
    if (i>1){
      if (dat[i,1]=="Remark=") {
        dat$comment[i] <- dat[i,2]
      } else {
        dat$comment[i]<- dat$comment[i-1]
      }
    }
  }
  
  #deletes rows where Remarks don't take measurements (Remarks that we didn't type in on the machine)
  todelete <- c()
  for (i in 1:nrow(dat)) {
    if (i>1){
      if (dat[i,1]=="Remark=" & grepl(pattern = "\"", dat[i,2])) {## any row with remark, and a " in it are put into a list
        todelete <- append(todelete,i)
      }
    }
  }
  for (i in 1:nrow(dat)) {
    if (i>1){
      if (grepl(pattern = "_",dat[i,which(names(dat) == "comment")]) & dat[i,1] == "Remark=") {
        dat$comment[i] <- dat[i,2]
      } else {
        dat$comment[i]<- dat$comment[i-1]
      }
    }
  }
  dat <- dat[-todelete,]
  
  ## deletes leading rows that where named "kitten" earlier ^^^^
  dat <- dat[-which(dat$comment=="kitten"),]
  for (i in 1:nrow(dat)) {if(grepl(pattern = ":", dat$comment[i], fixed = TRUE)){ 
    ## these lines us regular expressions to clean up the comments column
    dat$comment[i] <- sub("^.{1}", "", dat$comment[i])
    dat$comment[i] <- sub(".{2}$","", dat$comment[i])
    dat$comment[i] <- sub(".+ ","",dat$comment[i])}}
  dat <- dat[which(dat$Area != ""),] 
  
  #the following code turns all character columns into numeric
  dat[, 4:80] <- sapply(sapply(dat[4:80],as.character),as.numeric)
  return(dat)
}
LCnew<-function(dat){
  #creates a comments column
  dat$comment<-"kitten"
  dat <- dat[dat$HHMMSS != "in",] # deletes column names in the data frame
  dat <- dat[dat$Obs != "Obs",]
  #This is to put comments in comment column####
  for (i in 1:nrow(dat)) {
    if (i>1){
      if (dat[i,1]=="Remark=") {
        dat$comment[i] <- dat[i,2]
      } else {
        dat$comment[i]<- dat$comment[i-1]
      }
    }
  }
  
  #deletes rows where Remarks don't take measurements (Remarks that we didn't type in on the machine)
#  todelete <- c()
#  for (i in 1:nrow(dat)) {
#    if (i>1){
#      if (dat[i,1]=="Remark=" & grepl(pattern = "\"", dat[i,2])) {## any row with remark, and a " in it are put into a list
#        todelete <- append(todelete,i)
#      }
#    }
#  }
#  for (i in 1:nrow(dat)) {
#    if (i>1){
#      if (grepl(pattern = "_",dat[i,which(names(dat) == "comment")]) & dat[i,1] == "Remark=") {
#        dat$comment[i] <- dat[i,2]
#      } else {
#        dat$comment[i]<- dat$comment[i-1]
#      }
#    }
#  }
#  dat <- dat[-todelete,]
  
  ## deletes leading rows that where named "kitten" earlier ^^^^
  dat <- dat[-which(dat$comment=="kitten"),]
  for (i in 1:nrow(dat)) {if(grepl(pattern = ":", dat$comment[i], fixed = TRUE)){ 
    ## these lines us regular expressions to clean up the comments column
    dat$comment[i] <- sub("^.{1}", "", dat$comment[i])
    dat$comment[i] <- sub(".{2}$","", dat$comment[i])
    dat$comment[i] <- sub(".+ ","",dat$comment[i])}}
  dat <- dat[which(dat$Area != ""),] 
  
  #the following code turns all character columns into numeric
  dat[, 6:84] <- sapply(sapply(dat[6:84],as.character),as.numeric)
  return(dat)
}


## this function puts all of the files in one data frame. Put the file path into this function.
clean_rawNEW_csvs <- function(filepath){
  setwd(filepath) ### change this line to be your file path
  
  #gets all files with the given file type should be .csv
  allFiles_configNEW#<-list.files(pattern = "*.csv")
  
  outDF <- data.frame() ### creates a dataframe to store
  
  ## this for-loop combines all the files into one big file
  for (i in 1:length(allFiles_configNEW)){
    #for the current file read it in
    dat<- read.csv(allFiles_configNEW[i], row.names = NULL)
    #dat$filename <- allFiles[i]
    #puts meta data in a column
    meta<- dat[1,1]
   #stores the dat in which this file was created
    dat$meta <- meta 
    
    ## rename columns: in the first column, find the first row that contains
    # the word "Obs." This marks the header row. Set the dataframe names
    # to the names in this row.
    names(dat)<- as.character(unlist(dat[min(which(dat[,1] == "Obs")),])) 
    #stores the dat in which this file was created
    #dat$meta <- meta 
    
    ### some irgas have Mch columns, this removes those
    dat <-dat[,-(which(grepl("Mch",names(dat))))]
    dat <- dat[,-85] #what is Bridger getting at here? ##I belive this is getting rid of date time...
    dat$filename <- allFiles_configNEW[i]
    #binds data frames together
    outDF <- rbind(outDF, dat)
  }
  ### now the dataframe named "d" has a comment column that has all of your remarks in it
  d <- LCnew(outDF)
  
  return(d)
}

## this function puts all of the files in one data frame. Put the file path into this function.
clean_raw_csvs <- function(filepath){
  setwd(filepath) ### change this line to be your file path
  
  #gets all files with the given file type should be .csv
  allFiles_configOLD#<-list.files(pattern = "*.csv")
  
  outDF <- data.frame() ### creates a dataframe to store
  
  ## this for-loop combines all the files into one big file
  for (i in 1:length(allFiles_configOLD)){
    #for the current file read it in
    dat<- read.csv(allFiles_configOLD[i], row.names = NULL)
    #dat$filename <- allFiles[i]
    #puts meta data in a column
    meta<- dat[1,1]
    #stores the dat in which this file was created
    dat$meta <- meta 
    
    ## rename columns: in the first column, find the first row that contains
    # the word "Obs." This marks the header row. Set the dataframe names
    # to the names in this row.
    names(dat)<- as.character(unlist(dat[min(which(dat[,1] == "Obs")),])) 
    
    ### some irgas have Mch columns, this removes those
    dat <-dat[,-(which(grepl("Mch",names(dat))))]
    dat <- dat[,-82]
    dat$filename <- allFiles_configOLD[i]
    #binds data frames together
    outDF <- rbind(outDF, dat)
  }
  
  ### now the dataframe named "d" has a comment column that has all of your remarks in it
  d <- LC(outDF)
  
  return(d)
}

