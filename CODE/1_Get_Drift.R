###############################################################################
#                                                                      Feb 2016
#       Get the Downstream Drift Data
#
#  
###############################################################################
# setwd('C:/Users/mdodrill/Desktop/FB_DOWN/Analysis/DRIFT_DOWNSTREAM')
setwd('P:/BIOLOGICAL/Flyco/NO_DATA_3/Count_GLM')
rm(list=ls(all=TRUE))
library(dplyr)

# load functions stored in R.script
# source("C:/Users/mdodrill/Desktop/FB_DOWN/Analysis/DRIFT_COMMON/FB_Drift_Functions_New_V1.R", chdir = F)
# source("P:/BIOLOGICAL/Flyco/NO_DATA_3/data/FB_Drift_Functions_New_V1.R", chdir = F)
source("P:/BIOLOGICAL/Flyco/NO_DATA_3/data/FB_Drift_Functions_Git_V1.R.R", chdir = F)

#-----------------------------------------------------------------------------#
# see look -- these are the species to do summaries for....
# Right now the functions are only 'wired' for LUM, NZM, GAM, SIM, CHI
sp_num = unique(look[,1])      

# Data to import (these are global and not passed in)
# drift_specimen = 'DRIFT_SPECIMEN_10_23_15.csv'
# drift_sample   = 'DRIFT_SAMPLE_10_23_15.csv'
drift_specimen = 'DRIFT_SPECIMEN_01_21_16.csv'
drift_sample   = 'DRIFT_SAMPLE_01_21_16.csv'

# This is the main function that takes a year (or years) and species of interest as 
# arguments. Creates a summary (acconting for the structure of the data, what you get
# from the functions 'summarize' & 'derive') and associates the sampling info (stuff
# from the DRIFT_SAMPLE table in Access).

data = tbl_df(drift_summary(c(2012,2013,2014,2015), sp_num))

NO_trips = c('GC20120419','GC20120705','GC20120913','GC20130110',
             'GC20130404','GC20130625','GC20130912','GC20140109',
             'GC20140403','GC20140626','GC20140911','GC20150108',
             'GC20150403')

# subset the data for only the NO_trips
no_data = data[which(data$TripID %in% NO_trips),] 
no_data$allsites = ifelse(no_data$DriftRM <= 0, 'I',ifelse(no_data$DriftRM>0 & no_data$DriftRM<17,'badger',
                           ifelse(no_data$DriftRM > 17 & no_data$DriftRM < 22, 'II', 
                                  ifelse(no_data$DriftRM > 23 & no_data$DriftRM < 45, 'III',
                                         ifelse(no_data$DriftRM > 46 & no_data$DriftRM < 62,
                                                'IVa',ifelse(no_data$DriftRM > 65,'runout','IVb'))))))
unique(no_data$TripID) # check to make sure the subset and trips are correct 
#-----------------------------------------------------------------------------#
# filter out any data that you don't want 

data2 = filter(no_data, GearID == 4)

str(data2)


data3 = data_to_counts(data2)
str(data3)

#-----------------------------------------------------------------------------#