###############################################################################
#                                                                      Feb 2016
#   Clean & Organize the Downstream Drift Data -- Prep for Model Fitting
#
#  Notes:
#  * this code was built from "FB_Drift_GLM_V1"
#  * run "1_Get_Drift" first, then this script
#  
###############################################################################


data3b = add_zeros(data3)

# clean out data with negative vol. 
data4 = data3b[which(data3b$Volume_Sampled_M.3 >0),]

# clean out really high values of vol.
data5 = data4[which(data4$Volume_Sampled_M.3 < 100),]

# add new sp. label
data5$SpeciesID2 = as.character(look[match(data5$SpeciesID, look[,1]),6])

# group together some taxa
data5$SpeciesID2 = ifelse(data5$SpeciesID2 == "SIMA", "SIML", data5$SpeciesID2)
data5$SpeciesID2 = ifelse(data5$SpeciesID2 == "SIMP", "SIML", data5$SpeciesID2)

data5$SpeciesID2 = ifelse(data5$SpeciesID2 == "CHIA", "CHIL", data5$SpeciesID2)
data5$SpeciesID2 = ifelse(data5$SpeciesID2 == "CHIP", "CHIL", data5$SpeciesID2)

unique(data5$SpeciesID2)

#-----------------------------------------------------------------------------#
# the loops below will add together the counts for each sample where there are
# multiple rows (observations) of a species (this is introduced above where I
# changed the species names).  Probably a better way to do this....

u.samp = unique(data5$DriftSampleID)

tmp.out = list()

for(i in 1:length(u.samp)){  # each sample
  sub = data5[which(data5$DriftSampleID == u.samp[i]),]
  
  if(any(duplicated(sub$SpeciesID2)) == "TRUE"){
    u.sp = unique(sub$SpeciesID2)
    
    # by setting up an empty data frame, the classes (and col. names) get lost
    # (not really sure why here...). using the first row of sub seems to
    # perserve this, but I don't like doing this.
    # tmp = as.data.frame(matrix(NA, length(u.sp), ncol(sub)))
    tmp = sub[1,]
    
    for(j in 1:length(u.sp)){
      sub2 = sub[which(sub$SpeciesID2 == u.sp[j]),]
      if(dim(sub2)[1] == 1){
        tmp[j,] = sub2[1,]
      } else {
        counts = colSums(sub2[4:24])  # add up the counts for that species
        lt.tmp = sub2[1,]
        lt.tmp[,4:24] = counts
        tmp[j,] = lt.tmp
      }
    }
    tmp.out[[i]] = tmp 
  } else {
    tmp.out[[i]] = sub
  }
}

out = do.call('rbind', tmp.out)
# names(out) = names(data5)

# need to recalculate the count.sum !!!!
data5 = out

#-----------------------------------------------------------------------------#
# remove the depth integrated, this removes ~ 100 samples
#data6 = data5[which(data5$DepthIntegrated == "FALSE"),]
data6<-data5
data6$depth = ifelse(data6$DepthIntegrated=="TRUE","integrated",ifelse((data6$Depth / data6$TotalDepth) < .5,"shallow", "deep"))
# table(data6$depth)
data6$tod = as.factor(ifelse(data6$SolarStamp == "DAY", "day", "twilight"))

###adding in Q
data6$datetime<-as.POSIXct(paste(data6$DriftDate,data6$TimeBegin))
NOq<-read.csv("P:/BIOLOGICAL/Flyco/NO_DATA_3/data/NO_q_apr12_apr15.csv",header=T)
NOq$date<-as.POSIXct(NOq$date,format= "%m/%d/%Y %H:%M")
data6$q<-NOq[findInterval(data6$datetime,NOq$date),2]
tr<-function(x){(x-mean(x))/sd(x)}
data6$qtr<-tr(data6$q)
data6<-subset(data6,substr(data6$allsites,1,1)=='I')
# just the cols. that we want
d1 = dplyr::select(data6, DriftSampleID, SpeciesID2, site, DriftRM, TripID, tod, depth, Volume_Sampled_M.3, Velocity_m.s,q, qtr,  4:24)

# add in the total counts (corrected from above in the loop)
d1$tot.count = rowSums(d1[,which(names(d1) %in% c(".05",seq(1,20)))])

# add some other factors
d1$depth = as.factor(d1$depth)
d1$year = as.factor(substr(d1$TripID, 3, 6))
d1$site = as.factor(d1$site)
d1$season = as.factor(ifelse(substr(d1$TripID, 7, 8) == "01", "winter", 
                             ifelse(substr(d1$TripID, 7, 8) == "04", "spring",
                                    ifelse(substr(d1$TripID, 7, 8) == "09", "fall", "summer"))))
# reset the row names
rownames(d1) = NULL

names(d1) = tolower(names(d1))

names(d1)[c(2,5,8)] = c("taxa", "trip", "vol")

# rm(list=setdiff(ls(), "d1"))
#-----------------------------------------------------------------------------#

