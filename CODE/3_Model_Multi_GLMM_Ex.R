###############################################################################
#                                                                    April 2016
#                             Model Fitting 
#
#  Notes:
#  * this code was built from "3_Model_Working.R" (see other 3_Model... scripts)
#  * run "1_Get_Drift", then "2_Organize", then this script
#  
###############################################################################
library(glmmADMB)
library(glmulti)
# library(mass) # for glm.nb

# clean up the environment 
# rm(list=setdiff(ls(), "d1"))

u.taxa = unique(d1$taxa)

big.out = list()

#timing the model fitting
stt = proc.time()[[3]]

for(i in 1:length(u.taxa)){
  
  # define all these objects to the global env. every iteration of the loop, so
  # that the glmulti can access them (probably a better way to do this...).
  sp.sub = d1[which(d1$taxa == u.taxa[i]),]
  data = sp.sub
  
  tot.count = data$tot.count
  site = data$site
  tod = data$tod
  depth = data$depth
  season = data$season
  year = data$year
  vol = data$vol  
  trip = data$trip
  id = as.factor(paste(data$trip, data$site))
  q = log(data$q)
   
  # Functions used by glmulti to fit the models
  
  # this is a fixed effects version
  # my.glmulti = function(formula, data = data, ...){
  #   glm.nb(as.formula(deparse(formula)), data = data,  ...)
  # } 
  
  # this is the mixed effects version. (you can change what random effect is
  # included by changing the quoted part of the function, ie. (1|trip) or (1|site)).
  
#   my.glmulti.3 = function(formula, data = data, ...){
#     glmmadmb(as.formula(paste(deparse(formula), "+ offset(log(vol)) + (1|id)")), 
#              family = "nbinom", data = data, ...)
#   } 
  
  my.glmulti.3 = function(formula, data = data, ...){
    glmmadmb(as.formula(paste(deparse(formula), "+ offset(log(vol)) + (1|trip)")), 
             family = "nbinom", data = data, ...)
  } 
  
#   here is where you include the different factors for the model fitting. see
#   ?glmulti. setting level = 2 will fit all the interaction terms (probably a
#   bad idea with complex models b/c the model set grows huge).
  
  test = glmulti(tot.count ~ site + tod + depth + season + year + q, fitfunc = my.glmulti.3,
                 level = 1, marginality = TRUE)
  
#   test = glmulti(tot.count ~ season + year, fitfunc = my.glmulti.3,
#                  level = 1, marginality = TRUE)
  
  # add the fitted model object to a list
  big.out[[i]] = test
}

ent = proc.time()[[3]]
(ent-stt)/60

# AIC table
weightable(big.out[[1]])


i<-1
sp.sub = d1[which(d1$taxa == u.taxa[i]),]
data = sp.sub

tot.count = data$tot.count
site = data$site
tod = data$tod
depth = data$depth
season = data$season
year = data$year
vol = data$vol  
trip = data$trip
id = as.factor(paste(data$trip, data$site))
q = log(data$q)
lumb1<-glmmadmb(tot.count ~ 1 + site + depth + offset(log(vol)) + (1|trip),family = "nbinom", data = data)
lumb2<-glmmadmb(tot.count ~ 1 + site + tod + depth + offset(log(vol)) + (1|trip),family = "nbinom", data = data)  
lumb3<-glmmadmb(tot.count ~ 1 + site + depth + q + offset(log(vol)) + (1|trip),family = "nbinom", data = data)
lumb4<-glmmadmb(tot.count ~ 1 + site + tod + depth + q + offset(log(vol)) + (1|trip),family = "nbinom", data = data)
lumb5<-glmmadmb(tot.count ~ 1 + site + depth + season + year + offset(log(vol)) + (1|trip),family = "nbinom", data = data)
lumb6<-glmmadmb(tot.count ~ 1 + site + depth + season + offset(log(vol)) + (1|trip),family = "nbinom", data = data)
lumb7<-glmmadmb(tot.count ~ 1 + site + tod + depth + season + year + offset(log(vol)) + (1|trip),family = "nbinom", data = data)
lumb8<-glmmadmb(tot.count ~ 1 + site + tod + depth + season+ offset(log(vol)) + (1|trip),family = "nbinom", data = data)
lumb9<-glmmadmb(tot.count ~ 1 + site + depth + season + year + q + offset(log(vol)) + (1|trip),family = "nbinom", data = data)
lumb10<-glmmadmb(tot.count ~ 1 + site + depth + season + q + offset(log(vol)) + (1|trip),family = "nbinom", data = data)
lumb11<-glmmadmb(tot.count ~ 1 + site + tod + depth + season + year + q + offset(log(vol)) + (1|trip),family = "nbinom", data = data)
lumb12<-glmmadmb(tot.count ~ 1 + site + tod + depth + season + q + offset(log(vol)) + (1|trip),family = "nbinom", data = data)
lumb13<-glmmadmb(tot.count ~ 1 + site + depth + year + offset(log(vol)) + (1|trip),family = "nbinom", data = data)
lumb14<-glmmadmb(tot.count ~ 1 + site + depth + year + q  + offset(log(vol)) + (1|trip),family = "nbinom", data = data)
lumb15<-glmmadmb(tot.count ~ 1 + site + tod + depth + year  + offset(log(vol)) + (1|trip),family = "nbinom", data = data)
lumb16<-glmmadmb(tot.count ~ 1 + site + tod + depth + year + q + offset(log(vol)) + (1|trip),family = "nbinom", data = data)
lumb17<-glmmadmb(tot.count ~ 1 + site + tod  + offset(log(vol)) + (1|trip),family = "nbinom", data = data)
lumb18<-glmmadmb(tot.count ~ 1 + site + tod + q + offset(log(vol)) + (1|trip),family = "nbinom", data = data)
lumb19<-glmmadmb(tot.count ~ 1 + site + tod + season + year + offset(log(vol)) + (1|trip),family = "nbinom", data = data)
lumb20<-glmmadmb(tot.count ~ 1 + site + offset(log(vol)) + (1|trip),family = "nbinom", data = data)
lumb21<-glmmadmb(tot.count ~ 1 + site + tod + season + offset(log(vol)) + (1|trip),family = "nbinom", data = data)
lumb22<-glmmadmb(tot.count ~ 1 + site + q + offset(log(vol)) + (1|trip),family = "nbinom", data = data)
lumb23<-glmmadmb(tot.count ~ 1 + site + tod + season + year + q + offset(log(vol)) + (1|trip),family = "nbinom", data = data)
lumb24<-glmmadmb(tot.count ~ 1 + site + season + year + offset(log(vol)) + (1|trip),family = "nbinom", data = data)
lumb25<-glmmadmb(tot.count ~ 1 + site + tod + season + q  + offset(log(vol)) + (1|trip),family = "nbinom", data = data)
lumb26<-glmmadmb(tot.count ~ 1 + season  + offset(log(vol)) + (1|trip),family = "nbinom", data = data)
lumb27<-glmmadmb(tot.count ~ 1 + site + season + offset(log(vol)) + (1|trip),family = "nbinom", data = data)
lumb28<-glmmadmb(tot.count ~ 1 + site + season + year + q + offset(log(vol)) + (1|trip),family = "nbinom", data = data)
lumb29<-glmmadmb(tot.count ~ 1 + site + tod + year + offset(log(vol)) + (1|trip),family = "nbinom", data = data)
lumb30<-glmmadmb(tot.count ~ 1 + site + tod + year + q + offset(log(vol)) + (1|trip),family = "nbinom", data = data)
lumb31<-glmmadmb(tot.count ~ 1 + site + season + q + offset(log(vol)) + (1|trip),family = "nbinom", data = data)
lumb32<-glmmadmb(tot.count ~ 1 + site + year + offset(log(vol)) + (1|trip),family = "nbinom", data = data)
lumb33<-glmmadmb(tot.count ~ 1 + site + year + q  + offset(log(vol)) + (1|trip),family = "nbinom", data = data)
lumb34<-glmmadmb(tot.count ~ 1 + depth + q  + offset(log(vol)) + (1|trip),family = "nbinom", data = data)
lumb35<-glmmadmb(tot.count ~ 1 + tod + depth + q + offset(log(vol)) + (1|trip),family = "nbinom", data = data)
lumb36<-glmmadmb(tot.count ~ 1 + depth + season + year + q + offset(log(vol)) + (1|trip),family = "nbinom", data = data)
lumb37<-glmmadmb(tot.count ~ 1 + tod + depth + season + year + q + offset(log(vol)) + (1|trip),family = "nbinom", data = data)
lumb38<-glmmadmb(tot.count ~ 1 + tod + depth + offset(log(vol)) + (1|trip),family = "nbinom", data = data)
lumb39<-glmmadmb(tot.count ~ 1 + depth + offset(log(vol)) + (1|trip),family = "nbinom", data = data)
lumb40<-glmmadmb(tot.count ~ 1 + tod + depth + season + q + offset(log(vol)) + (1|trip),family = "nbinom", data = data)
lumb41<-glmmadmb(tot.count ~ 1 + depth + season + q + offset(log(vol)) + (1|trip),family = "nbinom", data = data)
lumb42<-glmmadmb(tot.count ~ 1 + tod + depth + season + year + offset(log(vol)) + (1|trip),family = "nbinom", data = data)
lumb43<-glmmadmb(tot.count ~ 1 + depth + season + year + offset(log(vol)) + (1|trip),family = "nbinom", data = data)
lumb44<-glmmadmb(tot.count ~ 1 + depth + year + q + offset(log(vol)) + (1|trip),family = "nbinom", data = data)
lumb45<-glmmadmb(tot.count ~ 1 + tod + depth + season  + offset(log(vol)) + (1|trip),family = "nbinom", data = data)
lumb46<-glmmadmb(tot.count ~ 1 + tod + depth + year + q + offset(log(vol)) + (1|trip),family = "nbinom", data = data)
lumb47<-glmmadmb(tot.count ~ 1 + depth + season + offset(log(vol)) + (1|trip),family = "nbinom", data = data)
lumb48<-glmmadmb(tot.count ~ 1 + depth + year + offset(log(vol)) + (1|trip),family = "nbinom", data = data)
lumb49<-glmmadmb(tot.count ~ 1 + tod + depth + year + offset(log(vol)) + (1|trip),family = "nbinom", data = data)
lumb50<-glmmadmb(tot.count ~ 1 + tod + q + offset(log(vol)) + (1|trip),family = "nbinom", data = data)
lumb51<-glmmadmb(tot.count ~ 1 + tod + season + year + q + offset(log(vol)) + (1|trip),family = "nbinom", data = data)
lumb52<-glmmadmb(tot.count ~ 1 + q + offset(log(vol)) + (1|trip),family = "nbinom", data = data)
lumb53<-glmmadmb(tot.count ~ 1 + tod + offset(log(vol)) + (1|trip),family = "nbinom", data = data)
lumb54<-glmmadmb(tot.count ~ 1 + tod + season + q + offset(log(vol)) + (1|trip),family = "nbinom", data = data)
lumb55<-glmmadmb(tot.count ~ 1 + tod + season + year + offset(log(vol)) + (1|trip),family = "nbinom", data = data)
lumb56<-glmmadmb(tot.count ~ 1 + season + year + q + offset(log(vol)) + (1|trip),family = "nbinom", data = data)
lumb57<-glmmadmb(tot.count ~ 1 + tod + season + offset(log(vol)) + (1|trip),family = "nbinom", data = data)
lumb58<-glmmadmb(tot.count ~ 1 + tod + year + q + offset(log(vol)) + (1|trip),family = "nbinom", data = data)
lumb59<-glmmadmb(tot.count ~ 1 + offset(log(vol)) + (1|trip),family = "nbinom", data = data)
lumb60<-glmmadmb(tot.count ~ 1 + season + year + offset(log(vol)) + (1|trip),family = "nbinom", data = data)
lumb61<-glmmadmb(tot.count ~ 1 + season + q + offset(log(vol)) + (1|trip),family = "nbinom", data = data)
lumb62<-glmmadmb(tot.count ~ 1 + year + q + offset(log(vol)) + (1|trip),family = "nbinom", data = data)
lumb63<-glmmadmb(tot.count ~ 1 + tod + year + offset(log(vol)) + (1|trip),family = "nbinom", data = data)
lumb64<-glmmadmb(tot.count ~ 1 + season + offset(log(vol)) + (1|trip),family = "nbinom", data = data)

lumbbic<-c(BIC(lumb1),BIC(lumb2),BIC(lumb3),BIC(lumb4),BIC(lumb5),BIC(lumb6),BIC(lumb7),BIC(lumb8),BIC(lumb9),BIC(lumb10),BIC(lumb11),BIC(lumb12),BIC(lumb13),BIC(lumb14),
           BIC(lumb15),BIC(lumb16),BIC(lumb17),BIC(lumb18),BIC(lumb19),BIC(lumb20),BIC(lumb21),BIC(lumb22),BIC(lumb23),BIC(lumb24),BIC(lumb25),BIC(lumb26),BIC(lumb27),BIC(lumb28),
           BIC(lumb29),BIC(lumb30),BIC(lumb31),BIC(lumb32),BIC(lumb33),BIC(lumb34),BIC(lumb35),BIC(lumb36),BIC(lumb37),BIC(lumb38),BIC(lumb39),BIC(lumb40),BIC(lumb41),BIC(lumb42),
           BIC(lumb43),BIC(lumb44),BIC(lumb45),BIC(lumb46),BIC(lumb47),BIC(lumb48),BIC(lumb49),BIC(lumb50),BIC(lumb51),BIC(lumb52),BIC(lumb53),BIC(lumb54),BIC(lumb55),BIC(lumb56),
           BIC(lumb57),BIC(lumb58),BIC(lumb59),BIC(lumb60),BIC(lumb61),BIC(lumb62),99999,BIC(lumb64))

lumbaic<-c(AIC(lumb1),AIC(lumb2),AIC(lumb3),AIC(lumb4),AIC(lumb5),AIC(lumb6),AIC(lumb7),AIC(lumb8),AIC(lumb9),AIC(lumb10),AIC(lumb11),AIC(lumb12),AIC(lumb13),AIC(lumb14),
           AIC(lumb15),AIC(lumb16),AIC(lumb17),AIC(lumb18),AIC(lumb19),AIC(lumb20),AIC(lumb21),AIC(lumb22),AIC(lumb23),AIC(lumb24),AIC(lumb25),AIC(lumb26),AIC(lumb27),AIC(lumb28),
           AIC(lumb29),AIC(lumb30),AIC(lumb31),AIC(lumb32),AIC(lumb33),AIC(lumb34),AIC(lumb35),AIC(lumb36),AIC(lumb37),AIC(lumb38),AIC(lumb39),AIC(lumb40),AIC(lumb41),AIC(lumb42),
           AIC(lumb43),AIC(lumb44),AIC(lumb45),AIC(lumb46),AIC(lumb47),AIC(lumb48),AIC(lumb49),AIC(lumb50),AIC(lumb51),AIC(lumb52),AIC(lumb53),AIC(lumb54),AIC(lumb55),AIC(lumb56),
           AIC(lumb57),AIC(lumb58),AIC(lumb59),AIC(lumb60),AIC(lumb61),AIC(lumb62),99999,AIC(lumb64))



                               

i<-2
sp.sub = d1[which(d1$taxa == u.taxa[i]),]
data = sp.sub

tot.count = data$tot.count
site = data$site
tod = data$tod
depth = data$depth
season = data$season
year = data$year
vol = data$vol  
trip = data$trip
id = as.factor(paste(data$trip, data$site))
q = log(data$q)
nzms1<-glmmadmb(tot.count ~ 1 + site + depth + offset(log(vol)) + (1|trip),family = "nbinom", data = data)
nzms2<-glmmadmb(tot.count ~ 1 + site + tod + depth + offset(log(vol)) + (1|trip),family = "nbinom", data = data)  
nzms3<-glmmadmb(tot.count ~ 1 + site + depth + q + offset(log(vol)) + (1|trip),family = "nbinom", data = data)
nzms4<-glmmadmb(tot.count ~ 1 + site + tod + depth + q + offset(log(vol)) + (1|trip),family = "nbinom", data = data)
nzms5<-glmmadmb(tot.count ~ 1 + site + depth + season + year + offset(log(vol)) + (1|trip),family = "nbinom", data = data)
nzms6<-glmmadmb(tot.count ~ 1 + site + depth + season + offset(log(vol)) + (1|trip),family = "nbinom", data = data)
nzms7<-glmmadmb(tot.count ~ 1 + site + tod + depth + season + year + offset(log(vol)) + (1|trip),family = "nbinom", data = data)
nzms8<-glmmadmb(tot.count ~ 1 + site + tod + depth + season+ offset(log(vol)) + (1|trip),family = "nbinom", data = data)
nzms9<-glmmadmb(tot.count ~ 1 + site + depth + season + year + q + offset(log(vol)) + (1|trip),family = "nbinom", data = data)
nzms10<-glmmadmb(tot.count ~ 1 + site + depth + season + q + offset(log(vol)) + (1|trip),family = "nbinom", data = data)
nzms11<-glmmadmb(tot.count ~ 1 + site + tod + depth + season + year + q + offset(log(vol)) + (1|trip),family = "nbinom", data = data)
nzms12<-glmmadmb(tot.count ~ 1 + site + tod + depth + season + q + offset(log(vol)) + (1|trip),family = "nbinom", data = data)
nzms13<-glmmadmb(tot.count ~ 1 + site + depth + year + offset(log(vol)) + (1|trip),family = "nbinom", data = data)
nzms14<-glmmadmb(tot.count ~ 1 + site + depth + year + q  + offset(log(vol)) + (1|trip),family = "nbinom", data = data)
nzms15<-glmmadmb(tot.count ~ 1 + site + tod + depth + year  + offset(log(vol)) + (1|trip),family = "nbinom", data = data)
nzms16<-glmmadmb(tot.count ~ 1 + site + tod + depth + year + q + offset(log(vol)) + (1|trip),family = "nbinom", data = data)
nzms17<-glmmadmb(tot.count ~ 1 + site + tod  + offset(log(vol)) + (1|trip),family = "nbinom", data = data)
nzms18<-glmmadmb(tot.count ~ 1 + site + tod + q + offset(log(vol)) + (1|trip),family = "nbinom", data = data)
nzms19<-glmmadmb(tot.count ~ 1 + site + tod + season + year + offset(log(vol)) + (1|trip),family = "nbinom", data = data)
nzms20<-glmmadmb(tot.count ~ 1 + site + offset(log(vol)) + (1|trip),family = "nbinom", data = data)
nzms21<-glmmadmb(tot.count ~ 1 + site + tod + season + offset(log(vol)) + (1|trip),family = "nbinom", data = data)
nzms22<-glmmadmb(tot.count ~ 1 + site + q + offset(log(vol)) + (1|trip),family = "nbinom", data = data)
nzms23<-glmmadmb(tot.count ~ 1 + site + tod + season + year + q + offset(log(vol)) + (1|trip),family = "nbinom", data = data)
nzms24<-glmmadmb(tot.count ~ 1 + site + season + year + offset(log(vol)) + (1|trip),family = "nbinom", data = data)
nzms25<-glmmadmb(tot.count ~ 1 + site + tod + season + q  + offset(log(vol)) + (1|trip),family = "nbinom", data = data)
nzms26<-glmmadmb(tot.count ~ 1 + season  + offset(log(vol)) + (1|trip),family = "nbinom", data = data)
nzms27<-glmmadmb(tot.count ~ 1 + site + season + offset(log(vol)) + (1|trip),family = "nbinom", data = data)
nzms28<-glmmadmb(tot.count ~ 1 + site + season + year + q + offset(log(vol)) + (1|trip),family = "nbinom", data = data)
nzms29<-glmmadmb(tot.count ~ 1 + site + tod + year + offset(log(vol)) + (1|trip),family = "nbinom", data = data)
nzms30<-glmmadmb(tot.count ~ 1 + site + tod + year + q + offset(log(vol)) + (1|trip),family = "nbinom", data = data)
nzms31<-glmmadmb(tot.count ~ 1 + site + season + q + offset(log(vol)) + (1|trip),family = "nbinom", data = data)
nzms32<-glmmadmb(tot.count ~ 1 + site + year + offset(log(vol)) + (1|trip),family = "nbinom", data = data)
nzms33<-glmmadmb(tot.count ~ 1 + site + year + q  + offset(log(vol)) + (1|trip),family = "nbinom", data = data)
nzms34<-glmmadmb(tot.count ~ 1 + depth + q  + offset(log(vol)) + (1|trip),family = "nbinom", data = data)
nzms35<-glmmadmb(tot.count ~ 1 + tod + depth + q + offset(log(vol)) + (1|trip),family = "nbinom", data = data)
nzms36<-glmmadmb(tot.count ~ 1 + depth + season + year + q + offset(log(vol)) + (1|trip),family = "nbinom", data = data)
nzms37<-glmmadmb(tot.count ~ 1 + tod + depth + season + year + q + offset(log(vol)) + (1|trip),family = "nbinom", data = data)
nzms38<-glmmadmb(tot.count ~ 1 + tod + depth + offset(log(vol)) + (1|trip),family = "nbinom", data = data)
nzms39<-glmmadmb(tot.count ~ 1 + depth + offset(log(vol)) + (1|trip),family = "nbinom", data = data)
nzms40<-glmmadmb(tot.count ~ 1 + tod + depth + season + q + offset(log(vol)) + (1|trip),family = "nbinom", data = data)
nzms41<-glmmadmb(tot.count ~ 1 + depth + season + q + offset(log(vol)) + (1|trip),family = "nbinom", data = data)
nzms42<-glmmadmb(tot.count ~ 1 + tod + depth + season + year + offset(log(vol)) + (1|trip),family = "nbinom", data = data)
nzms43<-glmmadmb(tot.count ~ 1 + depth + season + year + offset(log(vol)) + (1|trip),family = "nbinom", data = data)
nzms44<-glmmadmb(tot.count ~ 1 + depth + year + q + offset(log(vol)) + (1|trip),family = "nbinom", data = data)
nzms45<-glmmadmb(tot.count ~ 1 + tod + depth + season  + offset(log(vol)) + (1|trip),family = "nbinom", data = data)
nzms46<-glmmadmb(tot.count ~ 1 + tod + depth + year + q + offset(log(vol)) + (1|trip),family = "nbinom", data = data)
nzms47<-glmmadmb(tot.count ~ 1 + depth + season + offset(log(vol)) + (1|trip),family = "nbinom", data = data)
nzms48<-glmmadmb(tot.count ~ 1 + depth + year + offset(log(vol)) + (1|trip),family = "nbinom", data = data)
nzms49<-glmmadmb(tot.count ~ 1 + tod + depth + year + offset(log(vol)) + (1|trip),family = "nbinom", data = data)
nzms50<-glmmadmb(tot.count ~ 1 + tod + q + offset(log(vol)) + (1|trip),family = "nbinom", data = data)
nzms51<-glmmadmb(tot.count ~ 1 + tod + season + year + q + offset(log(vol)) + (1|trip),family = "nbinom", data = data)
nzms52<-glmmadmb(tot.count ~ 1 + q + offset(log(vol)) + (1|trip),family = "nbinom", data = data)
nzms53<-glmmadmb(tot.count ~ 1 + tod + offset(log(vol)) + (1|trip),family = "nbinom", data = data)
nzms54<-glmmadmb(tot.count ~ 1 + tod + season + q + offset(log(vol)) + (1|trip),family = "nbinom", data = data)
nzms55<-glmmadmb(tot.count ~ 1 + tod + season + year + offset(log(vol)) + (1|trip),family = "nbinom", data = data)
nzms56<-glmmadmb(tot.count ~ 1 + season + year + q + offset(log(vol)) + (1|trip),family = "nbinom", data = data)
nzms57<-glmmadmb(tot.count ~ 1 + tod + season + offset(log(vol)) + (1|trip),family = "nbinom", data = data)
nzms58<-glmmadmb(tot.count ~ 1 + tod + year + q + offset(log(vol)) + (1|trip),family = "nbinom", data = data)
nzms59<-glmmadmb(tot.count ~ 1 + offset(log(vol)) + (1|trip),family = "nbinom", data = data)
nzms60<-glmmadmb(tot.count ~ 1 + season + year + offset(log(vol)) + (1|trip),family = "nbinom", data = data)
nzms61<-glmmadmb(tot.count ~ 1 + season + q + offset(log(vol)) + (1|trip),family = "nbinom", data = data)
nzms62<-glmmadmb(tot.count ~ 1 + year + q + offset(log(vol)) + (1|trip),family = "nbinom", data = data)
nzms63<-glmmadmb(tot.count ~ 1 + tod + year + offset(log(vol)) + (1|trip),family = "nbinom", data = data)
nzms64<-glmmadmb(tot.count ~ 1 + season + offset(log(vol)) + (1|trip),family = "nbinom", data = data)

nzmsbic<-c(BIC(nzms1),BIC(nzms2),BIC(nzms3),BIC(nzms4),BIC(nzms5),BIC(nzms6),BIC(nzms7),BIC(nzms8),BIC(nzms9),BIC(nzms10),BIC(nzms11),BIC(nzms12),BIC(nzms13),BIC(nzms14),
           BIC(nzms15),BIC(nzms16),BIC(nzms17),BIC(nzms18),BIC(nzms19),BIC(nzms20),BIC(nzms21),99999,BIC(nzms23),BIC(nzms24),BIC(nzms25),BIC(nzms26),BIC(nzms27),BIC(nzms28),
           BIC(nzms29),BIC(nzms30),BIC(nzms31),BIC(nzms32),BIC(nzms33),BIC(nzms34),BIC(nzms35),BIC(nzms36),BIC(nzms37),BIC(nzms38),BIC(nzms39),BIC(nzms40),BIC(nzms41),BIC(nzms42),
           BIC(nzms43),BIC(nzms44),BIC(nzms45),BIC(nzms46),BIC(nzms47),BIC(nzms48),BIC(nzms49),BIC(nzms50),BIC(nzms51),BIC(nzms52),BIC(nzms53),BIC(nzms54),BIC(nzms55),BIC(nzms56),
           BIC(nzms57),BIC(nzms58),BIC(nzms59),BIC(nzms60),BIC(nzms61),BIC(nzms62),BIC(nzms63),BIC(nzms64))

nzmsaic<-c(AIC(nzms1),AIC(nzms2),AIC(nzms3),AIC(nzms4),AIC(nzms5),AIC(nzms6),AIC(nzms7),AIC(nzms8),AIC(nzms9),AIC(nzms10),AIC(nzms11),AIC(nzms12),AIC(nzms13),AIC(nzms14),
           AIC(nzms15),AIC(nzms16),AIC(nzms17),AIC(nzms18),AIC(nzms19),AIC(nzms20),AIC(nzms21),99999,AIC(nzms23),AIC(nzms24),AIC(nzms25),AIC(nzms26),AIC(nzms27),AIC(nzms28),
           AIC(nzms29),AIC(nzms30),AIC(nzms31),AIC(nzms32),AIC(nzms33),AIC(nzms34),AIC(nzms35),AIC(nzms36),AIC(nzms37),AIC(nzms38),AIC(nzms39),AIC(nzms40),AIC(nzms41),AIC(nzms42),
           AIC(nzms43),AIC(nzms44),AIC(nzms45),AIC(nzms46),AIC(nzms47),AIC(nzms48),AIC(nzms49),AIC(nzms50),AIC(nzms51),AIC(nzms52),AIC(nzms53),AIC(nzms54),AIC(nzms55),AIC(nzms56),
           AIC(nzms57),AIC(nzms58),AIC(nzms59),AIC(nzms60),AIC(nzms61),AIC(nzms62),BIC(nzms63),AIC(nzms64))



i<-3
sp.sub = d1[which(d1$taxa == u.taxa[i]),]
data = sp.sub

tot.count = data$tot.count
site = data$site
tod = data$tod
depth = data$depth
season = data$season
year = data$year
vol = data$vol  
trip = data$trip
id = as.factor(paste(data$trip, data$site))
q = log(data$q)
gamm1<-glmmadmb(tot.count ~ 1 + site + depth + offset(log(vol)) + (1|trip),family = "nbinom", data = data)
gamm2<-glmmadmb(tot.count ~ 1 + site + tod + depth + offset(log(vol)) + (1|trip),family = "nbinom", data = data)  
gamm3<-glmmadmb(tot.count ~ 1 + site + depth + q + offset(log(vol)) + (1|trip),family = "nbinom", data = data)
gamm4<-glmmadmb(tot.count ~ 1 + site + tod + depth + q + offset(log(vol)) + (1|trip),family = "nbinom", data = data)
gamm5<-glmmadmb(tot.count ~ 1 + site + depth + season + year + offset(log(vol)) + (1|trip),family = "nbinom", data = data)
gamm6<-glmmadmb(tot.count ~ 1 + site + depth + season + offset(log(vol)) + (1|trip),family = "nbinom", data = data)
gamm7<-glmmadmb(tot.count ~ 1 + site + tod + depth + season + year + offset(log(vol)) + (1|trip),family = "nbinom", data = data)
gamm8<-glmmadmb(tot.count ~ 1 + site + tod + depth + season+ offset(log(vol)) + (1|trip),family = "nbinom", data = data)
gamm9<-glmmadmb(tot.count ~ 1 + site + depth + season + year + q + offset(log(vol)) + (1|trip),family = "nbinom", data = data)
gamm10<-glmmadmb(tot.count ~ 1 + site + depth + season + q + offset(log(vol)) + (1|trip),family = "nbinom", data = data)
gamm11<-glmmadmb(tot.count ~ 1 + site + tod + depth + season + year + q + offset(log(vol)) + (1|trip),family = "nbinom", data = data)
gamm12<-glmmadmb(tot.count ~ 1 + site + tod + depth + season + q + offset(log(vol)) + (1|trip),family = "nbinom", data = data)
gamm13<-glmmadmb(tot.count ~ 1 + site + depth + year + offset(log(vol)) + (1|trip),family = "nbinom", data = data)
gamm14<-glmmadmb(tot.count ~ 1 + site + depth + year + q  + offset(log(vol)) + (1|trip),family = "nbinom", data = data)
gamm15<-glmmadmb(tot.count ~ 1 + site + tod + depth + year  + offset(log(vol)) + (1|trip),family = "nbinom", data = data)
gamm16<-glmmadmb(tot.count ~ 1 + site + tod + depth + year + q + offset(log(vol)) + (1|trip),family = "nbinom", data = data)
gamm17<-glmmadmb(tot.count ~ 1 + site + tod  + offset(log(vol)) + (1|trip),family = "nbinom", data = data)
gamm18<-glmmadmb(tot.count ~ 1 + site + tod + q + offset(log(vol)) + (1|trip),family = "nbinom", data = data)
gamm19<-glmmadmb(tot.count ~ 1 + site + tod + season + year + offset(log(vol)) + (1|trip),family = "nbinom", data = data)
gamm20<-glmmadmb(tot.count ~ 1 + site + offset(log(vol)) + (1|trip),family = "nbinom", data = data)
gamm21<-glmmadmb(tot.count ~ 1 + site + tod + season + offset(log(vol)) + (1|trip),family = "nbinom", data = data)
gamm22<-glmmadmb(tot.count ~ 1 + site + q + offset(log(vol)) + (1|trip),family = "nbinom", data = data)
gamm23<-glmmadmb(tot.count ~ 1 + site + tod + season + year + q + offset(log(vol)) + (1|trip),family = "nbinom", data = data)
gamm24<-glmmadmb(tot.count ~ 1 + site + season + year + offset(log(vol)) + (1|trip),family = "nbinom", data = data)
gamm25<-glmmadmb(tot.count ~ 1 + site + tod + season + q  + offset(log(vol)) + (1|trip),family = "nbinom", data = data)
gamm26<-glmmadmb(tot.count ~ 1 + season  + offset(log(vol)) + (1|trip),family = "nbinom", data = data)
gamm27<-glmmadmb(tot.count ~ 1 + site + season + offset(log(vol)) + (1|trip),family = "nbinom", data = data)
gamm28<-glmmadmb(tot.count ~ 1 + site + season + year + q + offset(log(vol)) + (1|trip),family = "nbinom", data = data)
gamm29<-glmmadmb(tot.count ~ 1 + site + tod + year + offset(log(vol)) + (1|trip),family = "nbinom", data = data)
gamm30<-glmmadmb(tot.count ~ 1 + site + tod + year + q + offset(log(vol)) + (1|trip),family = "nbinom", data = data)
gamm31<-glmmadmb(tot.count ~ 1 + site + season + q + offset(log(vol)) + (1|trip),family = "nbinom", data = data)
gamm32<-glmmadmb(tot.count ~ 1 + site + year + offset(log(vol)) + (1|trip),family = "nbinom", data = data)
gamm33<-glmmadmb(tot.count ~ 1 + site + year + q  + offset(log(vol)) + (1|trip),family = "nbinom", data = data)
gamm34<-glmmadmb(tot.count ~ 1 + depth + q  + offset(log(vol)) + (1|trip),family = "nbinom", data = data)
gamm35<-glmmadmb(tot.count ~ 1 + tod + depth + q + offset(log(vol)) + (1|trip),family = "nbinom", data = data)
gamm36<-glmmadmb(tot.count ~ 1 + depth + season + year + q + offset(log(vol)) + (1|trip),family = "nbinom", data = data)
gamm37<-glmmadmb(tot.count ~ 1 + tod + depth + season + year + q + offset(log(vol)) + (1|trip),family = "nbinom", data = data)
gamm38<-glmmadmb(tot.count ~ 1 + tod + depth + offset(log(vol)) + (1|trip),family = "nbinom", data = data)
gamm39<-glmmadmb(tot.count ~ 1 + depth + offset(log(vol)) + (1|trip),family = "nbinom", data = data)
gamm40<-glmmadmb(tot.count ~ 1 + tod + depth + season + q + offset(log(vol)) + (1|trip),family = "nbinom", data = data)
gamm41<-glmmadmb(tot.count ~ 1 + depth + season + q + offset(log(vol)) + (1|trip),family = "nbinom", data = data)
gamm42<-glmmadmb(tot.count ~ 1 + tod + depth + season + year + offset(log(vol)) + (1|trip),family = "nbinom", data = data)
gamm43<-glmmadmb(tot.count ~ 1 + depth + season + year + offset(log(vol)) + (1|trip),family = "nbinom", data = data)
gamm44<-glmmadmb(tot.count ~ 1 + depth + year + q + offset(log(vol)) + (1|trip),family = "nbinom", data = data)
gamm45<-glmmadmb(tot.count ~ 1 + tod + depth + season  + offset(log(vol)) + (1|trip),family = "nbinom", data = data)
gamm46<-glmmadmb(tot.count ~ 1 + tod + depth + year + q + offset(log(vol)) + (1|trip),family = "nbinom", data = data)
gamm47<-glmmadmb(tot.count ~ 1 + depth + season + offset(log(vol)) + (1|trip),family = "nbinom", data = data)
gamm48<-glmmadmb(tot.count ~ 1 + depth + year + offset(log(vol)) + (1|trip),family = "nbinom", data = data)
gamm49<-glmmadmb(tot.count ~ 1 + tod + depth + year + offset(log(vol)) + (1|trip),family = "nbinom", data = data)
gamm50<-glmmadmb(tot.count ~ 1 + tod + q + offset(log(vol)) + (1|trip),family = "nbinom", data = data)
gamm51<-glmmadmb(tot.count ~ 1 + tod + season + year + q + offset(log(vol)) + (1|trip),family = "nbinom", data = data)
gamm52<-glmmadmb(tot.count ~ 1 + q + offset(log(vol)) + (1|trip),family = "nbinom", data = data)
gamm53<-glmmadmb(tot.count ~ 1 + tod + offset(log(vol)) + (1|trip),family = "nbinom", data = data)
gamm54<-glmmadmb(tot.count ~ 1 + tod + season + q + offset(log(vol)) + (1|trip),family = "nbinom", data = data)
gamm55<-glmmadmb(tot.count ~ 1 + tod + season + year + offset(log(vol)) + (1|trip),family = "nbinom", data = data)
gamm56<-glmmadmb(tot.count ~ 1 + season + year + q + offset(log(vol)) + (1|trip),family = "nbinom", data = data)
gamm57<-glmmadmb(tot.count ~ 1 + tod + season + offset(log(vol)) + (1|trip),family = "nbinom", data = data)
gamm58<-glmmadmb(tot.count ~ 1 + tod + year + q + offset(log(vol)) + (1|trip),family = "nbinom", data = data)
gamm59<-glmmadmb(tot.count ~ 1 + offset(log(vol)) + (1|trip),family = "nbinom", data = data)
gamm60<-glmmadmb(tot.count ~ 1 + season + year + offset(log(vol)) + (1|trip),family = "nbinom", data = data)
gamm61<-glmmadmb(tot.count ~ 1 + season + q + offset(log(vol)) + (1|trip),family = "nbinom", data = data)
gamm62<-glmmadmb(tot.count ~ 1 + year + q + offset(log(vol)) + (1|trip),family = "nbinom", data = data)
gamm63<-glmmadmb(tot.count ~ 1 + tod + year + offset(log(vol)) + (1|trip),family = "nbinom", data = data)
gamm64<-glmmadmb(tot.count ~ 1 + season + offset(log(vol)) + (1|trip),family = "nbinom", data = data)

gammbic<-c(BIC(gamm1),BIC(gamm2),BIC(gamm3),BIC(gamm4),BIC(gamm5),BIC(gamm6),BIC(gamm7),BIC(gamm8),BIC(gamm9),BIC(gamm10),BIC(gamm11),BIC(gamm12),BIC(gamm13),BIC(gamm14),
           BIC(gamm15),BIC(gamm16),BIC(gamm17),BIC(gamm18),BIC(gamm19),BIC(gamm20),BIC(gamm21),AIC(gamm22),BIC(gamm23),BIC(gamm24),BIC(gamm25),BIC(gamm26),BIC(gamm27),BIC(gamm28),
           BIC(gamm29),BIC(gamm30),BIC(gamm31),BIC(gamm32),BIC(gamm33),BIC(gamm34),BIC(gamm35),BIC(gamm36),BIC(gamm37),BIC(gamm38),BIC(gamm39),BIC(gamm40),BIC(gamm41),BIC(gamm42),
           BIC(gamm43),BIC(gamm44),BIC(gamm45),BIC(gamm46),BIC(gamm47),BIC(gamm48),BIC(gamm49),BIC(gamm50),BIC(gamm51),BIC(gamm52),BIC(gamm53),BIC(gamm54),BIC(gamm55),BIC(gamm56),
           BIC(gamm57),BIC(gamm58),BIC(gamm59),BIC(gamm60),BIC(gamm61),BIC(gamm62),BIC(gamm63),BIC(gamm64))

gammaic<-c(AIC(gamm1),AIC(gamm2),AIC(gamm3),AIC(gamm4),AIC(gamm5),AIC(gamm6),AIC(gamm7),AIC(gamm8),AIC(gamm9),AIC(gamm10),AIC(gamm11),AIC(gamm12),AIC(gamm13),AIC(gamm14),
           AIC(gamm15),AIC(gamm16),AIC(gamm17),AIC(gamm18),AIC(gamm19),AIC(gamm20),AIC(gamm21),AIC(gamm22),AIC(gamm23),AIC(gamm24),AIC(gamm25),AIC(gamm26),AIC(gamm27),AIC(gamm28),
           AIC(gamm29),AIC(gamm30),AIC(gamm31),AIC(gamm32),AIC(gamm33),AIC(gamm34),AIC(gamm35),AIC(gamm36),AIC(gamm37),AIC(gamm38),AIC(gamm39),AIC(gamm40),AIC(gamm41),AIC(gamm42),
           AIC(gamm43),AIC(gamm44),AIC(gamm45),AIC(gamm46),AIC(gamm47),AIC(gamm48),AIC(gamm49),AIC(gamm50),AIC(gamm51),AIC(gamm52),AIC(gamm53),AIC(gamm54),AIC(gamm55),AIC(gamm56),
           AIC(gamm57),AIC(gamm58),AIC(gamm59),AIC(gamm60),AIC(gamm61),AIC(gamm62),BIC(gamm63),AIC(gamm64))



i<-4
sp.sub = d1[which(d1$taxa == u.taxa[i]),]
data = sp.sub

tot.count = data$tot.count
site = data$site
tod = data$tod
depth = data$depth
season = data$season
year = data$year
vol = data$vol  
trip = data$trip
id = as.factor(paste(data$trip, data$site))
q = log(data$q)
sima1<-glmmadmb(tot.count ~ 1 + site + depth + offset(log(vol)) + (1|trip),family = "nbinom", data = data)
sima2<-glmmadmb(tot.count ~ 1 + site + tod + depth + offset(log(vol)) + (1|trip),family = "nbinom", data = data)  
sima3<-glmmadmb(tot.count ~ 1 + site + depth + q + offset(log(vol)) + (1|trip),family = "nbinom", data = data)
sima4<-glmmadmb(tot.count ~ 1 + site + tod + depth + q + offset(log(vol)) + (1|trip),family = "nbinom", data = data)
sima5<-glmmadmb(tot.count ~ 1 + site + depth + season + year + offset(log(vol)) + (1|trip),family = "nbinom", data = data)
sima6<-glmmadmb(tot.count ~ 1 + site + depth + season + offset(log(vol)) + (1|trip),family = "nbinom", data = data)
sima7<-glmmadmb(tot.count ~ 1 + site + tod + depth + season + year + offset(log(vol)) + (1|trip),family = "nbinom", data = data)
sima8<-glmmadmb(tot.count ~ 1 + site + tod + depth + season+ offset(log(vol)) + (1|trip),family = "nbinom", data = data)
sima9<-glmmadmb(tot.count ~ 1 + site + depth + season + year + q + offset(log(vol)) + (1|trip),family = "nbinom", data = data)
sima10<-glmmadmb(tot.count ~ 1 + site + depth + season + q + offset(log(vol)) + (1|trip),family = "nbinom", data = data)
sima11<-glmmadmb(tot.count ~ 1 + site + tod + depth + season + year + q + offset(log(vol)) + (1|trip),family = "nbinom", data = data)
sima12<-glmmadmb(tot.count ~ 1 + site + tod + depth + season + q + offset(log(vol)) + (1|trip),family = "nbinom", data = data)
sima13<-glmmadmb(tot.count ~ 1 + site + depth + year + offset(log(vol)) + (1|trip),family = "nbinom", data = data)
sima14<-glmmadmb(tot.count ~ 1 + site + depth + year + q  + offset(log(vol)) + (1|trip),family = "nbinom", data = data)
sima15<-glmmadmb(tot.count ~ 1 + site + tod + depth + year  + offset(log(vol)) + (1|trip),family = "nbinom", data = data)
sima16<-glmmadmb(tot.count ~ 1 + site + tod + depth + year + q + offset(log(vol)) + (1|trip),family = "nbinom", data = data)
sima17<-glmmadmb(tot.count ~ 1 + site + tod  + offset(log(vol)) + (1|trip),family = "nbinom", data = data)
sima18<-glmmadmb(tot.count ~ 1 + site + tod + q + offset(log(vol)) + (1|trip),family = "nbinom", data = data)
sima19<-glmmadmb(tot.count ~ 1 + site + tod + season + year + offset(log(vol)) + (1|trip),family = "nbinom", data = data)
sima20<-glmmadmb(tot.count ~ 1 + site + offset(log(vol)) + (1|trip),family = "nbinom", data = data)
sima21<-glmmadmb(tot.count ~ 1 + site + tod + season + offset(log(vol)) + (1|trip),family = "nbinom", data = data)
sima22<-glmmadmb(tot.count ~ 1 + site + q + offset(log(vol)) + (1|trip),family = "nbinom", data = data)
sima23<-glmmadmb(tot.count ~ 1 + site + tod + season + year + q + offset(log(vol)) + (1|trip),family = "nbinom", data = data)
sima24<-glmmadmb(tot.count ~ 1 + site + season + year + offset(log(vol)) + (1|trip),family = "nbinom", data = data)
sima25<-glmmadmb(tot.count ~ 1 + site + tod + season + q  + offset(log(vol)) + (1|trip),family = "nbinom", data = data)
sima26<-glmmadmb(tot.count ~ 1 + season  + offset(log(vol)) + (1|trip),family = "nbinom", data = data)
sima27<-glmmadmb(tot.count ~ 1 + site + season + offset(log(vol)) + (1|trip),family = "nbinom", data = data)
sima28<-glmmadmb(tot.count ~ 1 + site + season + year + q + offset(log(vol)) + (1|trip),family = "nbinom", data = data)
sima29<-glmmadmb(tot.count ~ 1 + site + tod + year + offset(log(vol)) + (1|trip),family = "nbinom", data = data)
sima30<-glmmadmb(tot.count ~ 1 + site + tod + year + q + offset(log(vol)) + (1|trip),family = "nbinom", data = data)
sima31<-glmmadmb(tot.count ~ 1 + site + season + q + offset(log(vol)) + (1|trip),family = "nbinom", data = data)
sima32<-glmmadmb(tot.count ~ 1 + site + year + offset(log(vol)) + (1|trip),family = "nbinom", data = data)
sima33<-glmmadmb(tot.count ~ 1 + site + year + q  + offset(log(vol)) + (1|trip),family = "nbinom", data = data)
sima34<-glmmadmb(tot.count ~ 1 + depth + q  + offset(log(vol)) + (1|trip),family = "nbinom", data = data)
sima35<-glmmadmb(tot.count ~ 1 + tod + depth + q + offset(log(vol)) + (1|trip),family = "nbinom", data = data)
sima36<-glmmadmb(tot.count ~ 1 + depth + season + year + q + offset(log(vol)) + (1|trip),family = "nbinom", data = data)
sima37<-glmmadmb(tot.count ~ 1 + tod + depth + season + year + q + offset(log(vol)) + (1|trip),family = "nbinom", data = data)
sima38<-glmmadmb(tot.count ~ 1 + tod + depth + offset(log(vol)) + (1|trip),family = "nbinom", data = data)
sima39<-glmmadmb(tot.count ~ 1 + depth + offset(log(vol)) + (1|trip),family = "nbinom", data = data)
sima40<-glmmadmb(tot.count ~ 1 + tod + depth + season + q + offset(log(vol)) + (1|trip),family = "nbinom", data = data)
sima41<-glmmadmb(tot.count ~ 1 + depth + season + q + offset(log(vol)) + (1|trip),family = "nbinom", data = data)
sima42<-glmmadmb(tot.count ~ 1 + tod + depth + season + year + offset(log(vol)) + (1|trip),family = "nbinom", data = data)
sima43<-glmmadmb(tot.count ~ 1 + depth + season + year + offset(log(vol)) + (1|trip),family = "nbinom", data = data)
sima44<-glmmadmb(tot.count ~ 1 + depth + year + q + offset(log(vol)) + (1|trip),family = "nbinom", data = data)
sima45<-glmmadmb(tot.count ~ 1 + tod + depth + season  + offset(log(vol)) + (1|trip),family = "nbinom", data = data)
sima46<-glmmadmb(tot.count ~ 1 + tod + depth + year + q + offset(log(vol)) + (1|trip),family = "nbinom", data = data)
sima47<-glmmadmb(tot.count ~ 1 + depth + season + offset(log(vol)) + (1|trip),family = "nbinom", data = data)
sima48<-glmmadmb(tot.count ~ 1 + depth + year + offset(log(vol)) + (1|trip),family = "nbinom", data = data)
sima49<-glmmadmb(tot.count ~ 1 + tod + depth + year + offset(log(vol)) + (1|trip),family = "nbinom", data = data)
sima50<-glmmadmb(tot.count ~ 1 + tod + q + offset(log(vol)) + (1|trip),family = "nbinom", data = data)
sima51<-glmmadmb(tot.count ~ 1 + tod + season + year + q + offset(log(vol)) + (1|trip),family = "nbinom", data = data)
sima52<-glmmadmb(tot.count ~ 1 + q + offset(log(vol)) + (1|trip),family = "nbinom", data = data)
sima53<-glmmadmb(tot.count ~ 1 + tod + offset(log(vol)) + (1|trip),family = "nbinom", data = data)
sima54<-glmmadmb(tot.count ~ 1 + tod + season + q + offset(log(vol)) + (1|trip),family = "nbinom", data = data)
sima55<-glmmadmb(tot.count ~ 1 + tod + season + year + offset(log(vol)) + (1|trip),family = "nbinom", data = data)
sima56<-glmmadmb(tot.count ~ 1 + season + year + q + offset(log(vol)) + (1|trip),family = "nbinom", data = data)
sima57<-glmmadmb(tot.count ~ 1 + tod + season + offset(log(vol)) + (1|trip),family = "nbinom", data = data)
sima58<-glmmadmb(tot.count ~ 1 + tod + year + q + offset(log(vol)) + (1|trip),family = "nbinom", data = data)
sima59<-glmmadmb(tot.count ~ 1 + offset(log(vol)) + (1|trip),family = "nbinom", data = data)
sima60<-glmmadmb(tot.count ~ 1 + season + year + offset(log(vol)) + (1|trip),family = "nbinom", data = data)
sima61<-glmmadmb(tot.count ~ 1 + season + q + offset(log(vol)) + (1|trip),family = "nbinom", data = data)
sima62<-glmmadmb(tot.count ~ 1 + year + q + offset(log(vol)) + (1|trip),family = "nbinom", data = data)
sima63<-glmmadmb(tot.count ~ 1 + tod + year + offset(log(vol)) + (1|trip),family = "nbinom", data = data)
sima64<-glmmadmb(tot.count ~ 1 + season + offset(log(vol)) + (1|trip),family = "nbinom", data = data)
            
simabic<-c(BIC(sima1),BIC(sima2),BIC(sima3),BIC(sima4),BIC(sima5),BIC(sima6),BIC(sima7),BIC(sima8),BIC(sima9),BIC(sima10),BIC(sima11),BIC(sima12),BIC(sima13),BIC(sima14),
           BIC(sima15),BIC(sima16),BIC(sima17),BIC(sima18),BIC(sima19),BIC(sima20),BIC(sima21),AIC(sima22),BIC(sima23),BIC(sima24),BIC(sima25),BIC(sima26),BIC(sima27),BIC(sima28),
           BIC(sima29),BIC(sima30),BIC(sima31),BIC(sima32),BIC(sima33),BIC(sima34),BIC(sima35),BIC(sima36),BIC(sima37),BIC(sima38),BIC(sima39),BIC(sima40),BIC(sima41),BIC(sima42),
           BIC(sima43),BIC(sima44),BIC(sima45),BIC(sima46),BIC(sima47),BIC(sima48),BIC(sima49),BIC(sima50),BIC(sima51),BIC(sima52),BIC(sima53),BIC(sima54),BIC(sima55),BIC(sima56),
           BIC(sima57),BIC(sima58),BIC(sima59),BIC(sima60),BIC(sima61),BIC(sima62),BIC(sima63),BIC(sima64))

simaaic<-c(AIC(sima1),AIC(sima2),AIC(sima3),AIC(sima4),AIC(sima5),AIC(sima6),AIC(sima7),AIC(sima8),AIC(sima9),AIC(sima10),AIC(sima11),AIC(sima12),AIC(sima13),AIC(sima14),
           AIC(sima15),AIC(sima16),AIC(sima17),AIC(sima18),AIC(sima19),AIC(sima20),AIC(sima21),AIC(sima22),AIC(sima23),AIC(sima24),AIC(sima25),AIC(sima26),AIC(sima27),AIC(sima28),
           AIC(sima29),AIC(sima30),AIC(sima31),AIC(sima32),AIC(sima33),AIC(sima34),AIC(sima35),AIC(sima36),AIC(sima37),AIC(sima38),AIC(sima39),AIC(sima40),AIC(sima41),AIC(sima42),
           AIC(sima43),AIC(sima44),AIC(sima45),AIC(sima46),AIC(sima47),AIC(sima48),AIC(sima49),AIC(sima50),AIC(sima51),AIC(sima52),AIC(sima53),AIC(sima54),AIC(sima55),AIC(sima56),
           AIC(sima57),AIC(sima58),AIC(sima59),AIC(sima60),AIC(sima61),AIC(sima62),BIC(sima63),AIC(sima64))


i<-5
sp.sub = d1[which(d1$taxa == u.taxa[i]),]
data = sp.sub

tot.count = data$tot.count
site = data$site
tod = data$tod
depth = data$depth
season = data$season
year = data$year
vol = data$vol  
trip = data$trip
id = as.factor(paste(data$trip, data$site))
q = log(data$q)

chia1<-glmmadmb(tot.count ~ 1 + site + depth + offset(log(vol)) + (1|trip),family = "nbinom", data = data)
chia2<-glmmadmb(tot.count ~ 1 + site + tod + depth + offset(log(vol)) + (1|trip),family = "nbinom", data = data)  
chia3<-glmmadmb(tot.count ~ 1 + site + depth + q + offset(log(vol)) + (1|trip),family = "nbinom", data = data)
chia4<-glmmadmb(tot.count ~ 1 + site + tod + depth + q + offset(log(vol)) + (1|trip),family = "nbinom", data = data)
chia5<-glmmadmb(tot.count ~ 1 + site + depth + season + year + offset(log(vol)) + (1|trip),family = "nbinom", data = data)
chia6<-glmmadmb(tot.count ~ 1 + site + depth + season + offset(log(vol)) + (1|trip),family = "nbinom", data = data)
chia7<-glmmadmb(tot.count ~ 1 + site + tod + depth + season + year + offset(log(vol)) + (1|trip),family = "nbinom", data = data)
chia8<-glmmadmb(tot.count ~ 1 + site + tod + depth + season+ offset(log(vol)) + (1|trip),family = "nbinom", data = data)
chia9<-glmmadmb(tot.count ~ 1 + site + depth + season + year + q + offset(log(vol)) + (1|trip),family = "nbinom", data = data)
chia10<-glmmadmb(tot.count ~ 1 + site + depth + season + q + offset(log(vol)) + (1|trip),family = "nbinom", data = data)
chia11<-glmmadmb(tot.count ~ 1 + site + tod + depth + season + year + q + offset(log(vol)) + (1|trip),family = "nbinom", data = data)
chia12<-glmmadmb(tot.count ~ 1 + site + tod + depth + season + q + offset(log(vol)) + (1|trip),family = "nbinom", data = data)
chia13<-glmmadmb(tot.count ~ 1 + site + depth + year + offset(log(vol)) + (1|trip),family = "nbinom", data = data)
chia14<-glmmadmb(tot.count ~ 1 + site + depth + year + q  + offset(log(vol)) + (1|trip),family = "nbinom", data = data)
chia15<-glmmadmb(tot.count ~ 1 + site + tod + depth + year  + offset(log(vol)) + (1|trip),family = "nbinom", data = data)
chia16<-glmmadmb(tot.count ~ 1 + site + tod + depth + year + q + offset(log(vol)) + (1|trip),family = "nbinom", data = data)
chia17<-glmmadmb(tot.count ~ 1 + site + tod  + offset(log(vol)) + (1|trip),family = "nbinom", data = data)
chia18<-glmmadmb(tot.count ~ 1 + site + tod + q + offset(log(vol)) + (1|trip),family = "nbinom", data = data)
chia19<-glmmadmb(tot.count ~ 1 + site + tod + season + year + offset(log(vol)) + (1|trip),family = "nbinom", data = data)
chia20<-glmmadmb(tot.count ~ 1 + site + offset(log(vol)) + (1|trip),family = "nbinom", data = data)
chia21<-glmmadmb(tot.count ~ 1 + site + tod + season + offset(log(vol)) + (1|trip),family = "nbinom", data = data)
chia22<-glmmadmb(tot.count ~ 1 + site + q + offset(log(vol)) + (1|trip),family = "nbinom", data = data)
chia23<-glmmadmb(tot.count ~ 1 + site + tod + season + year + q + offset(log(vol)) + (1|trip),family = "nbinom", data = data)
chia24<-glmmadmb(tot.count ~ 1 + site + season + year + offset(log(vol)) + (1|trip),family = "nbinom", data = data)
chia25<-glmmadmb(tot.count ~ 1 + site + tod + season + q  + offset(log(vol)) + (1|trip),family = "nbinom", data = data)
chia26<-glmmadmb(tot.count ~ 1 + season  + offset(log(vol)) + (1|trip),family = "nbinom", data = data)
chia27<-glmmadmb(tot.count ~ 1 + site + season + offset(log(vol)) + (1|trip),family = "nbinom", data = data)
chia28<-glmmadmb(tot.count ~ 1 + site + season + year + q + offset(log(vol)) + (1|trip),family = "nbinom", data = data)
chia29<-glmmadmb(tot.count ~ 1 + site + tod + year + offset(log(vol)) + (1|trip),family = "nbinom", data = data)
chia30<-glmmadmb(tot.count ~ 1 + site + tod + year + q + offset(log(vol)) + (1|trip),family = "nbinom", data = data)
chia31<-glmmadmb(tot.count ~ 1 + site + season + q + offset(log(vol)) + (1|trip),family = "nbinom", data = data)
chia32<-glmmadmb(tot.count ~ 1 + site + year + offset(log(vol)) + (1|trip),family = "nbinom", data = data)
chia33<-glmmadmb(tot.count ~ 1 + site + year + q  + offset(log(vol)) + (1|trip),family = "nbinom", data = data)
chia34<-glmmadmb(tot.count ~ 1 + depth + q  + offset(log(vol)) + (1|trip),family = "nbinom", data = data)
chia35<-glmmadmb(tot.count ~ 1 + tod + depth + q + offset(log(vol)) + (1|trip),family = "nbinom", data = data)
chia36<-glmmadmb(tot.count ~ 1 + depth + season + year + q + offset(log(vol)) + (1|trip),family = "nbinom", data = data)
chia37<-glmmadmb(tot.count ~ 1 + tod + depth + season + year + q + offset(log(vol)) + (1|trip),family = "nbinom", data = data)
chia38<-glmmadmb(tot.count ~ 1 + tod + depth + offset(log(vol)) + (1|trip),family = "nbinom", data = data)
chia39<-glmmadmb(tot.count ~ 1 + depth + offset(log(vol)) + (1|trip),family = "nbinom", data = data)
chia40<-glmmadmb(tot.count ~ 1 + tod + depth + season + q + offset(log(vol)) + (1|trip),family = "nbinom", data = data)
chia41<-glmmadmb(tot.count ~ 1 + depth + season + q + offset(log(vol)) + (1|trip),family = "nbinom", data = data)
chia42<-glmmadmb(tot.count ~ 1 + tod + depth + season + year + offset(log(vol)) + (1|trip),family = "nbinom", data = data)
chia43<-glmmadmb(tot.count ~ 1 + depth + season + year + offset(log(vol)) + (1|trip),family = "nbinom", data = data)
chia44<-glmmadmb(tot.count ~ 1 + depth + year + q + offset(log(vol)) + (1|trip),family = "nbinom", data = data)
chia45<-glmmadmb(tot.count ~ 1 + tod + depth + season  + offset(log(vol)) + (1|trip),family = "nbinom", data = data)
chia46<-glmmadmb(tot.count ~ 1 + tod + depth + year + q + offset(log(vol)) + (1|trip),family = "nbinom", data = data)
chia47<-glmmadmb(tot.count ~ 1 + depth + season + offset(log(vol)) + (1|trip),family = "nbinom", data = data)
chia48<-glmmadmb(tot.count ~ 1 + depth + year + offset(log(vol)) + (1|trip),family = "nbinom", data = data)
chia49<-glmmadmb(tot.count ~ 1 + tod + depth + year + offset(log(vol)) + (1|trip),family = "nbinom", data = data)
chia50<-glmmadmb(tot.count ~ 1 + tod + q + offset(log(vol)) + (1|trip),family = "nbinom", data = data)
chia51<-glmmadmb(tot.count ~ 1 + tod + season + year + q + offset(log(vol)) + (1|trip),family = "nbinom", data = data)
chia52<-glmmadmb(tot.count ~ 1 + q + offset(log(vol)) + (1|trip),family = "nbinom", data = data)
chia53<-glmmadmb(tot.count ~ 1 + tod + offset(log(vol)) + (1|trip),family = "nbinom", data = data)
chia54<-glmmadmb(tot.count ~ 1 + tod + season + q + offset(log(vol)) + (1|trip),family = "nbinom", data = data)
chia55<-glmmadmb(tot.count ~ 1 + tod + season + year + offset(log(vol)) + (1|trip),family = "nbinom", data = data)
chia56<-glmmadmb(tot.count ~ 1 + season + year + q + offset(log(vol)) + (1|trip),family = "nbinom", data = data)
chia57<-glmmadmb(tot.count ~ 1 + tod + season + offset(log(vol)) + (1|trip),family = "nbinom", data = data)
chia58<-glmmadmb(tot.count ~ 1 + tod + year + q + offset(log(vol)) + (1|trip),family = "nbinom", data = data)
chia59<-glmmadmb(tot.count ~ 1 + offset(log(vol)) + (1|trip),family = "nbinom", data = data)
chia60<-glmmadmb(tot.count ~ 1 + season + year + offset(log(vol)) + (1|trip),family = "nbinom", data = data)
chia61<-glmmadmb(tot.count ~ 1 + season + q + offset(log(vol)) + (1|trip),family = "nbinom", data = data)
chia62<-glmmadmb(tot.count ~ 1 + year + q + offset(log(vol)) + (1|trip),family = "nbinom", data = data)
chia63<-glmmadmb(tot.count ~ 1 + tod + year + offset(log(vol)) + (1|trip),family = "nbinom", data = data)
chia64<-glmmadmb(tot.count ~ 1 + season + offset(log(vol)) + (1|trip),family = "nbinom", data = data)                        
                   
chiabic<-c(BIC(chia1),BIC(chia2),BIC(chia3),99999,BIC(chia5),BIC(chia6),BIC(chia7),BIC(chia8),BIC(chia9),99999,BIC(chia11),BIC(chia12),BIC(chia13),BIC(chia14),
           99999,99999,BIC(chia17),BIC(chia18),BIC(chia19),BIC(chia20),BIC(chia21),BIC(chia22),BIC(chia23),BIC(chia24),BIC(chia25),BIC(chia26),99999,BIC(chia28),
           BIC(chia29),BIC(chia30),BIC(chia31),BIC(chia32),BIC(chia33),BIC(chia34),BIC(chia35),BIC(chia36),BIC(chia37),BIC(chia38),BIC(chia39),BIC(chia40),BIC(chia41),BIC(chia42),
           BIC(chia43),BIC(chia44),BIC(chia45),99999,99999,BIC(chia48),BIC(chia49),BIC(chia50),BIC(chia51),BIC(chia52),BIC(chia53),BIC(chia54),BIC(chia55),BIC(chia56),
           BIC(chia57),99999,BIC(chia59),BIC(chia60),99999,BIC(chia62),BIC(chia63),BIC(chia64))
                            
chiaaic<-c(AIC(chia1),AIC(chia2),AIC(chia3),99999,AIC(chia5),AIC(chia6),AIC(chia7),AIC(chia8),AIC(chia9),99999,AIC(chia11),AIC(chia12),AIC(chia13),AIC(chia14),
           99999,99999,AIC(chia17),AIC(chia18),AIC(chia19),AIC(chia20),AIC(chia21),AIC(chia22),AIC(chia23),AIC(chia24),AIC(chia25),AIC(chia26),99999,AIC(chia28),
           AIC(chia29),AIC(chia30),AIC(chia31),AIC(chia32),AIC(chia33),AIC(chia34),AIC(chia35),AIC(chia36),AIC(chia37),AIC(chia38),AIC(chia39),AIC(chia40),AIC(chia41),AIC(chia42),
           AIC(chia43),AIC(chia44),AIC(chia45),99999,99999,AIC(chia48),AIC(chia49),AIC(chia50),AIC(chia51),AIC(chia52),AIC(chia53),AIC(chia54),AIC(chia55),AIC(chia56),
           AIC(chia57),99999,AIC(chia59),AIC(chia60),99999,AIC(chia62),AIC(chia63),AIC(chia64))


allbic<-cbind(lumbbic,nzmsbic,gammbic,simabic,chiabic)                                 
round(allbic-matrix(rep(apply(allbic,2,min),each=64),ncol=5),1)     
allaic<-cbind(lumbaic,nzmsaic,gammaic,simaaic,chiaaic)
round(allaic-matrix(rep(apply(allaic,2,min),each=64),ncol=5),1)                                                
                                     
                        
                     
                       
                            
                          
                                           
                           
                              
                                
                              
                                  
                                   



