#library(devtools)
#devtools::install_github("Lauren-Eyler/EconomicClusters")
library(EconomicClusters)

library(haven)
dataset<-read_dta("/Users/laureneeyler/Documents/Projects/GlobalHealth/PASE/Cameroon2018/CM_2018_DHS_02172022_2347_67102/CMHR71DT/CMHR71FL.DTA")

#all cameroon dhs 2018 wealth index (common rural and urban) variables:
#water
#toilet
#cooking fuel
#electricity
#radio
#television
#telephone (non-mobile)
#computer
#refrigerator
#cooker
#gas stove
#air conditioner
#fan
#cd/dvd player
#grain mill
#mixer
#modem/router
#cable
#generator
#solar panel
#water pump
#clock
#watch
#mobile telephone
#bicycle
#motorcycle/scooter
#animal-drawn cart
#car or truck
#boat with motor
#laptop compputer
#tablet computer
#bank account
#account at another financial institution
#floor
#roof
#wall
#house - owns a house
#land - owns land
#memsleep

setting <- rep(NA, nrow(dataset))
setting[which(dataset$hv025==1)] <- "urban"
setting[which(dataset$hv025==2)] <- "rural"
setting<-as.factor(setting)

water <- rep(NA, nrow(dataset))
water[which(dataset$hv201==11)] <-"pipe_dwel"
water[which(dataset$hv201==12)] <-"pipe_yard"
water[which(dataset$hv201==13)] <-"pipe_neighbor"
water[which(dataset$hv201==14)] <-"pipe_public"
water[which(dataset$hv201 %in% c(20, 21, 30, 31, 32))]<-"well"
water[which(dataset$hv201 %in% c(41, 42, 43, 51))] <-"surface"
water[which(dataset$hv201 %in% c(61, 62, 71, 92, 96))] <-"other"
water<-as.factor(water)
  
toilet <- rep(NA, nrow(dataset))
toilet[which(dataset$hv205 %in% c(10, 11, 12, 13, 14, 15))] <-"flush"
toilet[which(dataset$hv205 %in% c(20, 21, 22, 23))] <-"pitlat"
toilet[which(dataset$hv205==31)] <-"none"
toilet[which(dataset$hv205 %in% c(41, 42, 43, 96))] <-"other"
toilet<-as.factor(toilet)

cookfuel <- rep(NA, nrow(dataset))
cookfuel[which(dataset$hv226==1)] <-"electricity"
cookfuel[which(dataset$hv226==2)]<-"LPG"
cookfuel[which(dataset$hv226==3)] <-"natural gas"
cookfuel[which(dataset$hv226==4)] <-"biogas"
cookfuel[which(dataset$hv226==5)] <-"kerosene"
cookfuel[which(dataset$hv226==6)]<-"coal/lignite"
cookfuel[which(dataset$hv226==7)]<-"charcoal"
cookfuel[which(dataset$hv226==8)] <-"wood"
cookfuel[which(dataset$hv226==9)]<-"straw/shrubs/grass"
cookfuel[which(dataset$hv226==10)] <-"crop"
cookfuel[which(dataset$hv226==11)] <-"dung"
cookfuel[which(dataset$hv226==12)] <-"sawdust/woodchips"
cookfuel[which(dataset$hv226==95)] <-"nofoodcookedinhouse"
cookfuel[which(dataset$hv226==96)] <-"other"
cookfuel<-as.factor(cookfuel)

electricity <- rep(NA, nrow(dataset))
electricity[which(dataset$hv206==0)] <- "No"
electricity[which(dataset$hv206==1)] <- "Yes"
electricity <- as.factor(electricity)

radio <- rep(NA, nrow(dataset))
radio[which(dataset$hv207==0)] <- "No"
radio[which(dataset$hv207==1)] <- "Yes"
radio <- as.factor(radio)

tv <- rep(NA, nrow(dataset))
tv[which(dataset$hv208==0)] <- "No"
tv[which(dataset$hv208==1)] <- "Yes"
tv <- as.factor(tv)

landline <- rep(NA, nrow(dataset))
landline[which(dataset$hv221==0)] <- "No"
landline[which(dataset$hv221==1)] <- "Yes"
landline <- as.factor(landline)

computer <- rep(NA, nrow(dataset))
computer[which(dataset$hv243e==0)] <- "No"
computer[which(dataset$hv243e==1)] <- "Yes"
computer <- as.factor(computer)

refrigerator <- rep(NA, nrow(dataset))
refrigerator[which(dataset$hv209==0)] <- "No"
refrigerator[which(dataset$hv209==1)] <- "Yes"
refrigerator <- as.factor(computer)

cooker <- rep(NA, nrow(dataset))
cooker[which(dataset$sh121g==0)] <- "No"
cooker[which(dataset$sh121g==1)] <- "Yes"
cooker <- as.factor(cooker)

gasstove <- rep(NA, nrow(dataset))
gasstove[which(dataset$sh121h==0)] <- "No"
gasstove[which(dataset$sh121h==1)] <- "Yes"
gasstove  <- as.factor(gasstove)

airconditioner <- rep(NA, nrow(dataset))
airconditioner[which(dataset$sh121i==0)] <- "No"
airconditioner[which(dataset$sh121i==1)] <- "Yes"
airconditioner <- as.factor(airconditioner)

fan <- rep(NA, nrow(dataset))
fan[which(dataset$sh121j==0)] <- "No"
fan[which(dataset$sh121j==1)] <- "Yes"
fan <- as.factor(fan)

cd_dvd <- rep(NA, nrow(dataset))
cd_dvd[which(dataset$sh121k==0)] <- "No"
cd_dvd[which(dataset$sh121k==1)] <- "Yes"
cd_dvd <- as.factor(cd_dvd)

grainmill <- rep(NA, nrow(dataset))
grainmill[which(dataset$sh121l==0)] <- "No"
grainmill[which(dataset$sh121l==1)] <- "Yes"
grainmill <- as.factor(grainmill)

mixer <- rep(NA, nrow(dataset))
mixer[which(dataset$sh121m==0)] <- "No"
mixer[which(dataset$sh121m==1)] <- "Yes"
mixer <- as.factor(mixer)

modem_router <- rep(NA, nrow(dataset))
modem_router[which(dataset$sh121n==0)] <- "No"
modem_router[which(dataset$sh121n==1)] <- "Yes"
modem_router <- as.factor(modem_router)

cable <- rep(NA, nrow(dataset))
cable[which(dataset$sh121o==0)] <- "No"
cable[which(dataset$sh121o==1)] <- "Yes"
cable <- as.factor(cable)

generator <- rep(NA, nrow(dataset))
generator[which(dataset$sh121p==0)] <- "No"
generator[which(dataset$sh121p==1)] <- "Yes"
generator <- as.factor(generator)

solar_panel <- rep(NA, nrow(dataset))
solar_panel[which(dataset$sh121q==0)] <- "No"
solar_panel[which(dataset$sh121q==1)] <- "Yes"
solar_panel <- as.factor(solar_panel)

water_pump <- rep(NA, nrow(dataset))
water_pump[which(dataset$sh121r==0)] <- "No"
water_pump[which(dataset$sh121r==1)] <- "Yes"
water_pump <- as.factor(water_pump)

clock <- rep(NA, nrow(dataset))
clock[which(dataset$sh121s==0)] <- "No"
clock[which(dataset$sh121s==1)] <- "Yes"
clock <- as.factor(clock)

watch <- rep(NA, nrow(dataset))
watch[which(dataset$hv243b==0)] <- "No"
watch[which(dataset$hv243b==1)] <- "Yes"
watch <- as.factor(watch)

cell <- rep(NA, nrow(dataset))
cell[which(dataset$hv243a==0)] <- "No"
cell[which(dataset$hv243a==1)] <- "Yes"
cell <- as.factor(cell)


bike <- rep(NA, nrow(dataset))
bike[which(dataset$hv210==0)] <- "No"
bike[which(dataset$hv210==1)] <- "Yes"
bike <- as.factor(bike)

moto_scoot <- rep(NA, nrow(dataset))
moto_scoot[which(dataset$hv211==0)] <- "No"
moto_scoot[which(dataset$hv211==1)] <- "Yes"
moto_scoot <- as.factor(moto_scoot)

animal_cart <- rep(NA, nrow(dataset))
animal_cart[which(dataset$hv243c==0)] <- "No"
animal_cart[which(dataset$hv243c==1)] <- "Yes"
animal_cart <- as.factor(animal_cart)

car_truck <- rep(NA, nrow(dataset))
car_truck[which(dataset$hv212==0)] <- "No"
car_truck[which(dataset$hv212==1)] <- "Yes"
car_truck <- as.factor(car_truck)

motor_boat <- rep(NA, nrow(dataset))
motor_boat[which(dataset$hv243d==0)] <- "No"
motor_boat[which(dataset$hv243d==1)] <- "Yes"
motor_boat <- as.factor(motor_boat )

laptop <- rep(NA, nrow(dataset))
laptop[which(dataset$sh122h==0)] <- "No"
laptop[which(dataset$sh122h==1)] <- "Yes"
laptop <- as.factor(laptop)

tablet <- rep(NA, nrow(dataset))
tablet[which(dataset$sh122i==0)] <- "No"
tablet[which(dataset$sh122i==1)] <- "Yes"
tablet <- as.factor(tablet)

bank_account <- rep(NA, nrow(dataset))
bank_account[which(dataset$hv247==0)] <- "No"
bank_account[which(dataset$hv247==1)] <- "Yes"
bank_account <- as.factor(bank_account)

account_otherfinancial <- rep(NA, nrow(dataset))
account_otherfinancial[which(dataset$sh123a==0)] <- "No"
account_otherfinancial[which(dataset$sh123a==1)] <- "Yes"
account_otherfinancial <- as.factor(account_otherfinancial)

floor <- rep(NA, nrow(dataset))
floor[which(dataset$hv213==11 | dataset$hv213==12)] <-"natural"
floor[which(dataset$hv213==21 | dataset$hv213==22)] <-"rudimentary"
floor[which(dataset$hv213==31 | dataset$hv213==32 | dataset$hv213==33 | dataset$hv213==34 | dataset$hv213==35)] <-"finished"
floor[which(dataset$hv213==96)] <-"other"
floor <- as.factor(floor)

roof <- rep(NA, nrow(dataset))
roof[which(dataset$hv215==11 | dataset$hv215==12 | dataset$hv215==13)] <-"natural"
roof[which(dataset$hv215==21 | dataset$hv215==22 | dataset$hv215==23 | dataset$hv215==24)] <-"rudimentary"
roof[which(dataset$hv215==31 | dataset$hv215==32 | dataset$hv215==33 | dataset$hv215==34 | dataset$hv215==35 | dataset$hv215==36)] <-"finished"
roof[which(dataset$hv215==96)] <-"other"
roof <- as.factor(roof)

wall <- rep(NA, nrow(dataset))
wall[which(dataset$hv214==11 | dataset$hv214==12 | dataset$hv214==13)] <-"natural"
wall[which(dataset$hv214==21 | dataset$hv214==22 | dataset$hv214==23 | dataset$hv214==24 | dataset$hv214==25 | dataset$hv214==26)] <-"rudimentary"
wall[which(dataset$hv214==31 | dataset$hv214==32 | dataset$hv214==33 | dataset$hv214==34 | dataset$hv214==35 | dataset$hv214==36)] <-"finished"
wall[which(dataset$hv214==96)]<-"other"
wall <- as.factor(wall)

#note, in the wealth index file for Cameroon 2018, it says they used a variable LAND. In the steps to constructing the new DHS wealth index document, it says to create this variable from the individual datasets
#unless there is a variable for ownership of agricultural land in the household questionnaire
#so because there is a variable for ownership of agricultural land in the household questionnaire
#and because we used that variable in 2011
#i am going to use the household agland variable
agland <- rep(NA, nrow(dataset))
agland [which(dataset$hv244==0)] <- "No"
agland [which(dataset$hv244==1)] <- "Yes"
agland  <- as.factor(agland )

#house variable 
#making this variable according to the instructions in steps to creating the new dhs wealth index because no house ownership variable at household level in 2018
dat <- subset(dataset, select=c("hv001", "hv002", "hhid"))

individual<-read_dta("/Users/laureneeyler/Documents/Projects/GlobalHealth/PASE/Cameroon2018/CM_2018_DHS_02172022_2347_67102/CMIR71DT/CMIR71FL.DTA")
individual <- subset(individual, select=c("v001", "v002", "v745a"))
individual <- rename(individual, hv001 = v001)
individual <- rename(individual, hv002 = v002)


men<-read_dta("/Users/laureneeyler/Documents/Projects/GlobalHealth/PASE/Cameroon2018/CM_2018_DHS_02172022_2347_67102/CMMR71DT/CMMR71FL.DTA")
men <- subset(men, select=c("mv001", "mv002", "mv745a"))
men <- rename(men, hv001 = mv001)
men <- rename(men, hv002 = mv002)
men <- rename(men, v745a = mv745a)

library(tidyverse)
df_list <- list(dat, individual, men)
merge <- df_list %>% reduce(full_join, by=c("hv001", "hv002")) 
merge$ownshome <- rep(NA, nrow(merge))
merge$ownshome[which(merge$v745a.x %in% c(1,2,3) | merge$v745a.y %in% c(1,2,3))]<-1
merge$ownshome[which(is.na(merge$ownshome)==TRUE & (merge$v745a.x==0 | merge$v745a.y==0))]<-0

library(data.table)
minimerge <- data.frame(merge$hhid, merge$ownshome)

setDT(minimerge)[, merge.hhid]
dat <- dcast(minimerge, merge.hhid ~ 
               rowid(merge.hhid), value.var  = c("merge.ownshome"), sep='')

#new variable HOUSE = 1 if any household member says owns a home alone and/or jointly
HOUSE <- rep(NA, nrow(dataset))
for(i in 1:nrow(dat)){
  if(any(is.na(dat[i,2:ncol(dat)])==FALSE)){
    if(sum(dat[i, 2:ncol(dat)], na.rm = TRUE)>0){
      HOUSE[i] <- 1
    } else {
      if(any(dat[i,2:ncol(dat)]==0)){
        HOUSE[i] <- 0
      }
    }
  }
}

#because I now have the berkeley server, I'm going to see how long it would take to do it with all DHS variables
#and whether that alters if the ones it picks are useful or not...
data_full<-data.frame(setting, water, toilet, cookfuel, electricity, radio, tv, landline, computer, refrigerator, cooker, gasstove, 
                 airconditioner, fan, cd_dvd, grainmill, mixer, modem_router, cable, generator, solar_panel, water_pump, 
                 clock, watch, cell, bike, moto_scoot, animal_cart, car_truck, motor_boat, laptop, tablet, bank_account, 
                 account_otherfinancial, floor, roof, wall, agland, HOUSE)      

assets <- data_full

dejure<-dataset$hv012
defacto<-dataset$hv013
hhwt<-dataset$hv005

library(EconomicClusters)

data_for_EC<-EC_DHSwts(assets, dejure, defacto, hhwt)

EC_time(X=data_for_EC, nvars=4, kmin=5, kmax=10, ncores=6)
#ok it says it is only going to take 5.7 hours on this computer... I am skeptical, but let's give it a shot

col_cat<-seq(1,ncol(data_for_EC),1)
combn<-utils::combn(c(1:ncol(data_for_EC)),4)
col_indx=matrix(c("NULL"), ncol(combn), ncol(data_for_EC), byrow=FALSE)
for (i in 1:ncol(combn)){
  col_indx[i,]<-t(is.element(col_cat, combn[,i]))
  col_indx[i,][col_indx[i,]==FALSE]<-NA
  colnames(col_indx)=colnames(data_for_EC)
}

library(parallelDist)
library(foreach)
library(doParallel)

ASW<-vector()
combi<-list()
parD<-list()
wcKMR<-list()
doParallel::registerDoParallel(cores=2)
start.time<-Sys.time()
ASW<-foreach::foreach (i=1:nrow(col_indx), .combine='c')  %dopar% {
  combi<-data_for_EC[,!is.na(col_indx[i,])]
  combi_mat<-data.matrix(combi)
  parD<-parDist(combi_mat, method =  "hamming") 
  wcKMR<-WeightedCluster::wcKMedRange(parD, kvals=(5), weights=wt)
  ASW[i]<-wcKMR$stats[,5]
  rm(combi)
  rm(parD)
  rm(wcKMR)
  return(ASW[i])
}
end.time<-Sys.time()
total<-end.time-start.time

#### ASW computed through Berkeley server
load("/Users/laureneyler/Documents/CGSS/90DHS_EC/Ethiopia/Ethiopia_ASW.RData")
ASW_max<-max(ASW, na.rm=TRUE)
which(ASW==ASW_max)
#winner is 12
View(col_indx[12,])
#electricity, tv, refrigerator, mitad

combi<-data_for_EC[,!is.na(col_indx[12,])]
combi_mat<-data.matrix(combi)
library(parallelDist)
parD<-parDist(combi_mat, method =  "hamming") 
wcKMR<-WeightedCluster::wcKMedRange(parD, kvals=(5), weights=wt)

Medoids<-c(unique(wcKMR$clustering))
Medoid_df<-cbind(data_for_EC[as.numeric(unlist(Medoids)), !is.na(col_indx[12,])])

library(weights)
dataset$weights<-dataset$hv005/1000000
dataset$cluster<-vector()
for (i in 1:nrow(dataset)){
  if (unlist(wcKMR$clustering)[i]==16514){
    dataset$cluster[i]<-1
  } else if (unlist(wcKMR$clustering)[i]==16625){
    dataset$cluster[i]<-2
  } else if (unlist(wcKMR$clustering)[i]==16645){
    dataset$cluster[i]<-3
  } else if (unlist(wcKMR$clustering)[i]==16097){
    dataset$cluster[i]<-4
  } else if (unlist(wcKMR$clustering)[i]==16495){
    dataset$cluster[i]<-5
  }
}

library(survey)
svydesign<-svydesign(dataset$hv001, weights=dataset$weights,  strata=dataset$hv022, data=dataset)

svytable(~dataset$cluster, design=svydesign)/nrow(dataset)
#         1          2          3          4      5
#0.74323735 0.12083891 0.08229554 0.01962346  0.03400474 

#Addis Ababa
AddisAbaba<-dataset[which(dataset$hv024==10),]
AddisAbaba_wt<-wt[which(dataset$hv024==10)]
AddisAbaba_design<-subset(svydesign, dataset$hv024==10)
svytable(~AddisAbaba$cluster, design=AddisAbaba_design)/sum(unlist(svytable(~AddisAbaba$cluster, design=AddisAbaba_design)))

library(haven)
individual<-read_dta("/Users/laureneyler/Documents/CGSS/DHS_Datasets/Ethiopia_2016/ETIR70DT/ETIR70FL.DTA")

HAZ_av<-matrix(nrow=nrow(dataset), ncol=6)
HAZ_av[,1]<-dataset$hc70_1
HAZ_av[,2]<-dataset$hc70_2
HAZ_av[,3]<-dataset$hc70_3
HAZ_av[,4]<-dataset$hc70_4
HAZ_av[,5]<-dataset$hc70_5
HAZ_av[,6]<-dataset$hc70_6
for (i in 1:nrow(HAZ_av)){
  for (j in 1:6){
    if(is.na(HAZ_av[i,j])==TRUE | HAZ_av[i,j]==9999 | HAZ_av[i,j]==9998 | HAZ_av[i,j]==9996 | HAZ_av[i,j]==9997){
      HAZ_av[i,j]<-NA}
  }
}

dataset$HAZ_av<-NULL
for (i in 1:nrow(dataset)){
  dataset$HAZ_av[i]<-mean(HAZ_av[i,], na.rm=TRUE)
  if(dataset$HAZ_av[i]=="NaN"){
    dataset$HAZ_av[i]<-NA
  }
}

dataset$HAZ_av<-dataset$HAZ_av/100

summary(dataset$HAZ_av)
#9816 NAs, so HAZ available for kids from 6834 households 

dataset$weights<-dataset$hv005/1000000
library(survey)
svydesign_HAZ<-svydesign(dataset$hv001, weights=dataset$weights,  strata=dataset$hv022, data=dataset)
options(survey.lonely.psu="remove")

library(weights)

C1<-subset(dataset, dataset$cluster==1)
C1_design<-subset(svydesign_HAZ, dataset$cluster==1)
C1_HAZsvy<-svymean(C1$HAZ_av, C1_design, na.rm=TRUE)
HAZ_av_mean_C1_wtd<-mean(C1_HAZsvy)
HAZ_sem_C1<-SE(C1_HAZsvy)

C2<-subset(dataset, dataset$cluster==2)
C2_design<-subset(svydesign_HAZ, dataset$cluster==2)
C2_HAZsvy<-svymean(C2$HAZ_av, C2_design, na.rm=TRUE)
HAZ_av_mean_C2_wtd<-mean(C2_HAZsvy)
HAZ_sem_C2<-SE(C2_HAZsvy)

C3<-subset(dataset, dataset$cluster==3)
C3_design<-subset(svydesign_HAZ, dataset$cluster==3)
C3_HAZsvy<-svymean(C3$HAZ_av, C3_design, na.rm=TRUE)
HAZ_av_mean_C3_wtd<-mean(C3_HAZsvy)
HAZ_sem_C3<-SE(C3_HAZsvy)

C4<-subset(dataset, dataset$cluster==4)
C4_design<-subset(svydesign_HAZ, dataset$cluster==4)
C4_HAZsvy<-svymean(C4$HAZ_av, C4_design, na.rm=TRUE)
HAZ_av_mean_C4_wtd<-mean(C4_HAZsvy)
HAZ_sem_C4<-SE(C4_HAZsvy)

C5<-subset(dataset, dataset$cluster==5)
C5_design<-subset(svydesign_HAZ, dataset$cluster==5)
C5_HAZsvy<-svymean(C5$HAZ_av, C5_design, na.rm=TRUE)
HAZ_av_mean_C5_wtd<-mean(C5_HAZsvy)
HAZ_sem_C5<-SE(C5_HAZsvy)

HAZ_av_mean<-rbind(HAZ_av_mean_C1_wtd, HAZ_av_mean_C2_wtd, HAZ_av_mean_C3_wtd, HAZ_av_mean_C4_wtd, HAZ_av_mean_C5_wtd)
HAZ_sem<-rbind(HAZ_sem_C1, HAZ_sem_C2, HAZ_sem_C3, HAZ_sem_C4, HAZ_sem_C5)

HAZ_df<-data.frame(
  x=c(1:5),
  y=HAZ_av_mean,
  sem=HAZ_sem
)

library(Hmisc)
plot(HAZ_av_mean, xlab="Economic Groups", ylab="Average Child Height-for-Age Z-Score", main="Mean Child Height-for-Age Z-Score by Combined Economic Group", ylim=c(-1.8,0))
with(
  data=HAZ_df,
  expr= errbar(x, y, y+sem, y-sem, add=T, pch=1, cap=0.015)
)


merge<-merge(dataset, individual, by.x=c("hv001", "hv002"), by.y=c("v001", "v002"), all.y=FALSE)

merge$hhwt<-merge$hv005/1000000
merge$iwt<-merge$v005/1000000


merge$child_mortality<-NULL
merge$child_mortality<-merge$v206 + merge$v207
for (i in 1:nrow(merge)){
  merge$child_mortality[i]<-merge$child_mortality[i]/merge$v201[i]
}

merge$literacy<-NULL
merge$literacy<-as.numeric(merge$v155)
for (i in 1:nrow(merge)){
  if (is.na(merge$v155[i])==TRUE | merge$v155[i]==3 |merge$v155[i]==4 | merge$v155[i]==9){
    merge$literacy[i]<-NA
  }
}

svydesign_merge<-svydesign(merge$hv021, weights=merge$iwt,  strata=merge$hv022, data=merge, nest=FALSE)



CM1<-subset(merge, merge$cluster==1)
CM1_design<-subset(svydesign_merge, merge$cluster==1)
CM1_litsvy<-svymean(CM1$literacy, CM1_design, na.rm=TRUE)
literacy_mean_CM1_wtd<-mean(CM1_litsvy)
literacy_sem_CM1<-SE(CM1_litsvy)

CM1_cmsvy<-svymean(CM1$child_mortality, CM1_design, na.rm=TRUE)
childmort_mean_CM1_wtd<-mean(CM1_cmsvy)
childmort_sem_CM1<-SE(CM1_cmsvy)

CM2<-subset(merge, merge$cluster==2)
CM2_design<-subset(svydesign_merge, merge$cluster==2)
CM2_litsvy<-svymean(CM2$literacy, CM2_design, na.rm=TRUE)
literacy_mean_CM2_wtd<-mean(CM2_litsvy)
literacy_sem_CM2<-SE(CM2_litsvy)

CM2_cmsvy<-svymean(CM2$child_mortality, CM2_design, na.rm=TRUE)
childmort_mean_CM2_wtd<-mean(CM2_cmsvy)
childmort_sem_CM2<-SE(CM2_cmsvy)

CM3<-subset(merge, merge$cluster==3)
CM3_design<-subset(svydesign_merge, merge$cluster==3)
CM3_litsvy<-svymean(CM3$literacy, CM3_design, na.rm=TRUE)
literacy_mean_CM3_wtd<-mean(CM3_litsvy)
literacy_sem_CM3<-SE(CM3_litsvy)

CM3_cmsvy<-svymean(CM3$child_mortality, CM3_design, na.rm=TRUE)
childmort_mean_CM3_wtd<-mean(CM3_cmsvy)
childmort_sem_CM3<-SE(CM3_cmsvy)

CM4<-subset(merge, merge$cluster==4)
CM4_design<-subset(svydesign_merge, merge$cluster==4)
CM4_litsvy<-svymean(CM4$literacy, CM4_design, na.rm=TRUE)
literacy_mean_CM4_wtd<-mean(CM4_litsvy)
literacy_sem_CM4<-SE(CM4_litsvy)

CM4_cmsvy<-svymean(CM4$child_mortality, CM4_design, na.rm=TRUE)
childmort_mean_CM4_wtd<-mean(CM4_cmsvy)
childmort_sem_CM4<-SE(CM4_cmsvy)

CM5<-subset(merge, merge$cluster==5)
CM5_design<-subset(svydesign_merge, merge$cluster==5)
CM5_litsvy<-svymean(CM5$literacy, CM5_design, na.rm=TRUE)
literacy_mean_CM5_wtd<-mean(CM5_litsvy)
literacy_sem_CM5<-SE(CM5_litsvy)

CM5_cmsvy<-svymean(CM5$child_mortality, CM5_design, na.rm=TRUE)
childmort_mean_CM5_wtd<-mean(CM5_cmsvy)
childmort_sem_CM5<-SE(CM5_cmsvy)


literacy_mean<-rbind(literacy_mean_CM1_wtd, literacy_mean_CM2_wtd, literacy_mean_CM3_wtd, literacy_mean_CM4_wtd, literacy_mean_CM5_wtd)
literacy_sem<-rbind(literacy_sem_CM1, literacy_sem_CM2, literacy_sem_CM3, literacy_sem_CM4, literacy_sem_CM5)

lit_df<-data.frame(
  x=c(1:5),
  y=literacy_mean,
  sem=literacy_sem
)

library(Hmisc)
plot(literacy_mean, xlab="Economic Groups", ylab="Average Women's Literacy Score", main="Mean Women's Literacy Score by Economic Group", ylim=c(0,2.0))
with(
  data=lit_df,
  expr= errbar(x, y, y+sem, y-sem, add=T, pch=1, cap=0.015)
)



childmort_mean<-rbind(childmort_mean_CM1_wtd, childmort_mean_CM2_wtd, childmort_mean_CM3_wtd, childmort_mean_CM4_wtd, childmort_mean_CM5_wtd)
childmort_sem<-rbind(childmort_sem_CM1, childmort_sem_CM2, childmort_sem_CM3, childmort_sem_CM4, childmort_sem_CM5)

cm_df<-data.frame(
  x=c(1:5),
  y=childmort_mean,
  sem=childmort_sem
)

library(Hmisc)
plot(childmort_mean, xlab="Economic Groups", ylab="Average Proportion of Children Deceased", main="Child Mortality by Economic Group", ylim=c(0,0.12))
with(
  data=cm_df,
  expr= errbar(x, y, y+sem, y-sem, add=T, pch=1, cap=0.015)
)

##### Wealth Index #####
WI1<-subset(dataset, dataset$hv270==1)
WI1_design<-subset(svydesign_HAZ, dataset$hv270==1)
WI1_HAZsvy<-svymean(WI1$HAZ_av, WI1_design, na.rm=TRUE)
HAZ_av_mean_WI1_wtd<-mean(WI1_HAZsvy)
HAZ_sem_WI1<-SE(WI1_HAZsvy)

WI2<-subset(dataset, dataset$hv270==2)
WI2_design<-subset(svydesign_HAZ, dataset$hv270==2)
WI2_HAZsvy<-svymean(WI2$HAZ_av, WI2_design, na.rm=TRUE)
HAZ_av_mean_WI2_wtd<-mean(WI2_HAZsvy)
HAZ_sem_WI2<-SE(WI2_HAZsvy)

WI3<-subset(dataset, dataset$hv270==3)
WI3_design<-subset(svydesign_HAZ, dataset$hv270==3)
WI3_HAZsvy<-svymean(WI3$HAZ_av, WI3_design, na.rm=TRUE)
HAZ_av_mean_WI3_wtd<-mean(WI3_HAZsvy)
HAZ_sem_WI3<-SE(WI3_HAZsvy)

WI4<-subset(dataset, dataset$hv270==4)
WI4_design<-subset(svydesign_HAZ, dataset$hv270==4)
WI4_HAZsvy<-svymean(WI4$HAZ_av, WI4_design, na.rm=TRUE)
HAZ_av_mean_WI4_wtd<-mean(WI4_HAZsvy)
HAZ_sem_WI4<-SE(WI4_HAZsvy)

WI5<-subset(dataset, dataset$hv270==5)
WI5_design<-subset(svydesign_HAZ, dataset$hv270==5)
WI5_HAZsvy<-svymean(WI5$HAZ_av, WI5_design, na.rm=TRUE)
HAZ_av_mean_WI5_wtd<-mean(WI5_HAZsvy)
HAZ_sem_WI5<-SE(WI5_HAZsvy)

HAZ_av_mean<-rbind(HAZ_av_mean_WI1_wtd, HAZ_av_mean_WI2_wtd, HAZ_av_mean_WI3_wtd, HAZ_av_mean_WI4_wtd, HAZ_av_mean_WI5_wtd)
HAZ_sem<-rbind(HAZ_sem_WI1, HAZ_sem_WI2, HAZ_sem_WI3, HAZ_sem_WI4, HAZ_sem_WI5)

HAZ_df<-data.frame(
  x=c(1:5),
  y=HAZ_av_mean,
  sem=HAZ_sem
)

library(Hmisc)
plot(HAZ_av_mean, xlab="Economic Groups", ylab="Average Child Height-for-Age Z-Score", main="Mean Child Height-for-Age Z-Score by Combined Economic Group", ylim=c(-1.8,0))
with(
  data=HAZ_df,
  expr= errbar(x, y, y+sem, y-sem, add=T, pch=1, cap=0.015)
)


WIM1<-subset(merge, merge$hv270==1)
WIM1_design<-subset(svydesign_merge, merge$hv270==1)
WIM1_litsvy<-svymean(WIM1$literacy, WIM1_design, na.rm=TRUE)
literacy_mean_WIM1_wtd<-mean(WIM1_litsvy)
literacy_sem_WIM1<-SE(WIM1_litsvy)

WIM1_cmsvy<-svymean(WIM1$child_mortality, WIM1_design, na.rm=TRUE)
childmort_mean_WIM1_wtd<-mean(WIM1_cmsvy)
childmort_sem_WIM1<-SE(WIM1_cmsvy)

WIM2<-subset(merge, merge$hv270==2)
WIM2_design<-subset(svydesign_merge, merge$hv270==2)
WIM2_litsvy<-svymean(WIM2$literacy, WIM2_design, na.rm=TRUE)
literacy_mean_WIM2_wtd<-mean(WIM2_litsvy)
literacy_sem_WIM2<-SE(WIM2_litsvy)

WIM2_cmsvy<-svymean(WIM2$child_mortality, WIM2_design, na.rm=TRUE)
childmort_mean_WIM2_wtd<-mean(WIM2_cmsvy)
childmort_sem_WIM2<-SE(WIM2_cmsvy)

WIM3<-subset(merge, merge$hv270==3)
WIM3_design<-subset(svydesign_merge, merge$hv270==3)
WIM3_litsvy<-svymean(WIM3$literacy, WIM3_design, na.rm=TRUE)
literacy_mean_WIM3_wtd<-mean(WIM3_litsvy)
literacy_sem_WIM3<-SE(WIM3_litsvy)

WIM3_cmsvy<-svymean(WIM3$child_mortality, WIM3_design, na.rm=TRUE)
childmort_mean_WIM3_wtd<-mean(WIM3_cmsvy)
childmort_sem_WIM3<-SE(WIM3_cmsvy)

WIM4<-subset(merge, merge$hv270==4)
WIM4_design<-subset(svydesign_merge, merge$hv270==4)
WIM4_litsvy<-svymean(WIM4$literacy, WIM4_design, na.rm=TRUE)
literacy_mean_WIM4_wtd<-mean(WIM4_litsvy)
literacy_sem_WIM4<-SE(WIM4_litsvy)

WIM4_cmsvy<-svymean(WIM4$child_mortality, WIM4_design, na.rm=TRUE)
childmort_mean_WIM4_wtd<-mean(WIM4_cmsvy)
childmort_sem_WIM4<-SE(WIM4_cmsvy)

WIM5<-subset(merge, merge$hv270==5)
WIM5_design<-subset(svydesign_merge, merge$hv270==5)
WIM5_litsvy<-svymean(WIM5$literacy, WIM5_design, na.rm=TRUE)
literacy_mean_WIM5_wtd<-mean(WIM5_litsvy)
literacy_sem_WIM5<-SE(WIM5_litsvy)

WIM5_cmsvy<-svymean(WIM5$child_mortality, WIM5_design, na.rm=TRUE)
childmort_mean_WIM5_wtd<-mean(WIM5_cmsvy)
childmort_sem_WIM5<-SE(WIM5_cmsvy)


literacy_mean<-rbind(literacy_mean_WIM1_wtd, literacy_mean_WIM2_wtd, literacy_mean_WIM3_wtd, literacy_mean_WIM4_wtd, literacy_mean_WIM5_wtd)
literacy_sem<-rbind(literacy_sem_WIM1, literacy_sem_WIM2, literacy_sem_WIM3, literacy_sem_WIM4, literacy_sem_WIM5)

lit_df<-data.frame(
  x=c(1:5),
  y=literacy_mean,
  sem=literacy_sem
)

library(Hmisc)
plot(literacy_mean, xlab="Economic Groups", ylab="Average Women's Literacy Score", main="Mean Women's Literacy Score by Economic Group", ylim=c(0,2.0))
with(
  data=lit_df,
  expr= errbar(x, y, y+sem, y-sem, add=T, pch=1, cap=0.015)
)



childmort_mean<-rbind(childmort_mean_WIM1_wtd, childmort_mean_WIM2_wtd, childmort_mean_WIM3_wtd, childmort_mean_WIM4_wtd, childmort_mean_WIM5_wtd)
childmort_sem<-rbind(childmort_sem_WIM1, childmort_sem_WIM2, childmort_sem_WIM3, childmort_sem_WIM4, childmort_sem_WIM5)

cm_df<-data.frame(
  x=c(1:5),
  y=childmort_mean,
  sem=childmort_sem
)

library(Hmisc)
plot(childmort_mean, xlab="Economic Groups", ylab="Average Proportion of Children Deceased", main="Child Mortality by Economic Group", ylim=c(0,0.12))
with(
  data=cm_df,
  expr= errbar(x, y, y+sem, y-sem, add=T, pch=1, cap=0.015)
)

View(table(dataset$hv106_01))
dataset$edlevel<-vector()
for (i in 1:nrow(dataset)){
  if (dataset$hv106_01[i]==0){
    dataset$edlevel[i]<-"1"
  } else {
    if (dataset$hv106_01[i]==1){
      dataset$edlevel[i]<-"2"
    } else {
      if (dataset$hv106_01[i]==2){
        dataset$edlevel[i]<-"3"
      } else {
        if (dataset$hv106_01[i]==3){
          dataset$edlevel[i]<-"4"
        } else {
          if (dataset$hv106_01[i]==8){
            dataset$edlevel[i]<-"5"
          }
        }
      }
    }
  }
}

library(descr)
Ed_bycluster<-crosstab(dataset$edlevel, dataset$cluster, weight=wt)
Ed_byWI<-crosstab(dataset$edlevel, dataset$hv270, weight=wt)

#let's see how dividing ethiopia further looks...
test<-cbind(toilet, dataset$hv206, dataset$hv208, dataset$sh121j)
colnames(test)<-c("toilet", "electricity", "TV", "electric mitad")
test_mat<-data.matrix(test)

parD_test<-parDist(test_mat, method =  "hamming") 
wcKMR_test<-WeightedCluster::wcKMedRange(parD_test, kvals=(5), weights=wt)
ASW_test<-wcKMR_test$stats[,5]

Medoids_test<-c(unique(wcKMR_test$clustering))
Medoid_df_test<-rbind(test[as.numeric(unlist(Medoids_test)), ])

library(weights)
dataset$weights<-dataset$hv005/1000000
dataset$cluster_test<-vector()
for (i in 1:nrow(dataset)){
  if (unlist(wcKMR_test$clustering)[i]==15710){
    dataset$cluster_test[i]<-1
  } else if (unlist(wcKMR_test$clustering)[i]==16511){
    dataset$cluster_test[i]<-2
  } else if (unlist(wcKMR_test$clustering)[i]==16650){
    dataset$cluster_test[i]<-3
  } else if (unlist(wcKMR_test$clustering)[i]==15456){
    dataset$cluster_test[i]<-4
  } else if (unlist(wcKMR_test$clustering)[i]==15460){
    dataset$cluster_test[i]<-5
  }
}

library(survey)
svydesign<-svydesign(dataset$hv001, weights=dataset$weights,  strata=dataset$hv022, data=dataset)

svytable(~dataset$cluster_test, design=svydesign)/nrow(dataset)
#1          2          3          4          5 
#0.31879990 0.44332684 0.10222362 0.08229554 0.05335408 
C1_test<-subset(dataset, dataset$cluster_test==1)
C1_test_design<-subset(svydesign_HAZ, dataset$cluster_test==1)
C1_test_HAZsvy<-svymean(C1_test$HAZ_av, C1_test_design, na.rm=TRUE)
HAZ_av_mean_C1_test_wtd<-mean(C1_test_HAZsvy)
HAZ_sem_C1_test<-SE(C1_test_HAZsvy)

C2_test<-subset(dataset, dataset$cluster_test==2)
C2_test_design<-subset(svydesign_HAZ, dataset$cluster_test==2)
C2_test_HAZsvy<-svymean(C2_test$HAZ_av, C2_test_design, na.rm=TRUE)
HAZ_av_mean_C2_test_wtd<-mean(C2_test_HAZsvy)
HAZ_sem_C2_test<-SE(C2_test_HAZsvy)

C3_test<-subset(dataset, dataset$cluster_test==3)
C3_test_design<-subset(svydesign_HAZ, dataset$cluster_test==3)
C3_test_HAZsvy<-svymean(C3_test$HAZ_av, C3_test_design, na.rm=TRUE)
HAZ_av_mean_C3_test_wtd<-mean(C3_test_HAZsvy)
HAZ_sem_C3_test<-SE(C3_test_HAZsvy)

C4_test<-subset(dataset, dataset$cluster_test==4)
C4_test_design<-subset(svydesign_HAZ, dataset$cluster_test==4)
C4_test_HAZsvy<-svymean(C4_test$HAZ_av, C4_test_design, na.rm=TRUE)
HAZ_av_mean_C4_test_wtd<-mean(C4_test_HAZsvy)
HAZ_sem_C4_test<-SE(C4_test_HAZsvy)

C5_test<-subset(dataset, dataset$cluster_test==5)
C5_test_design<-subset(svydesign_HAZ, dataset$cluster_test==5)
C5_test_HAZsvy<-svymean(C5_test$HAZ_av, C5_test_design, na.rm=TRUE)
HAZ_av_mean_C5_test_wtd<-mean(C5_test_HAZsvy)
HAZ_sem_C5_test<-SE(C5_test_HAZsvy)

HAZ_av_mean<-rbind(HAZ_av_mean_C1_test_wtd, HAZ_av_mean_C2_test_wtd, HAZ_av_mean_C3_test_wtd, HAZ_av_mean_C4_test_wtd, HAZ_av_mean_C5_test_wtd)
HAZ_sem<-rbind(HAZ_sem_C1_test, HAZ_sem_C2_test, HAZ_sem_C3_test, HAZ_sem_C4_test, HAZ_sem_C5_test)

HAZ_df<-data.frame(
  x=c(1:5),
  y=HAZ_av_mean,
  sem=HAZ_sem
)

library(Hmisc)
plot(HAZ_av_mean, xlab="Economic Groups", ylab="Average Child Height-for-Age Z-Score", main="Mean Child Height-for-Age Z-Score by Combined Economic Group", ylim=c(-1.8,0))
with(
  data=HAZ_df,
  expr= errbar(x, y, y+sem, y-sem, add=T, pch=1, cap=0.015)
)

merge<-merge(dataset, individual, by.x=c("hv001", "hv002"), by.y=c("v001", "v002"), all.y=FALSE)

merge$hhwt<-merge$hv005/1000000
merge$iwt<-merge$v005/1000000


merge$child_mortality<-NULL
merge$child_mortality<-merge$v206 + merge$v207
for (i in 1:nrow(merge)){
  merge$child_mortality[i]<-merge$child_mortality[i]/merge$v201[i]
}

merge$literacy<-NULL
merge$literacy<-as.numeric(merge$v155)
for (i in 1:nrow(merge)){
  if (is.na(merge$v155[i])==TRUE | merge$v155[i]==3 |merge$v155[i]==4 | merge$v155[i]==9){
    merge$literacy[i]<-NA
  }
}

svydesign_merge<-svydesign(merge$hv021, weights=merge$iwt,  strata=merge$hv022, data=merge, nest=FALSE)



CM1_test<-subset(merge, merge$cluster_test==1)
CM1_test_design<-subset(svydesign_merge, merge$cluster_test==1)
CM1_test_litsvy<-svymean(CM1_test$literacy, CM1_test_design, na.rm=TRUE)
literacy_mean_CM1_test_wtd<-mean(CM1_test_litsvy)
literacy_sem_CM1_test<-SE(CM1_test_litsvy)

CM1_test_cmsvy<-svymean(CM1_test$child_mortality, CM1_test_design, na.rm=TRUE)
childmort_mean_CM1_test_wtd<-mean(CM1_test_cmsvy)
childmort_sem_CM1_test<-SE(CM1_test_cmsvy)

CM2_test<-subset(merge, merge$cluster_test==2)
CM2_test_design<-subset(svydesign_merge, merge$cluster_test==2)
CM2_test_litsvy<-svymean(CM2_test$literacy, CM2_test_design, na.rm=TRUE)
literacy_mean_CM2_test_wtd<-mean(CM2_test_litsvy)
literacy_sem_CM2_test<-SE(CM2_test_litsvy)

CM2_test_cmsvy<-svymean(CM2_test$child_mortality, CM2_test_design, na.rm=TRUE)
childmort_mean_CM2_test_wtd<-mean(CM2_test_cmsvy)
childmort_sem_CM2_test<-SE(CM2_test_cmsvy)

CM3_test<-subset(merge, merge$cluster_test==3)
CM3_test_design<-subset(svydesign_merge, merge$cluster_test==3)
CM3_test_litsvy<-svymean(CM3_test$literacy, CM3_test_design, na.rm=TRUE)
literacy_mean_CM3_test_wtd<-mean(CM3_test_litsvy)
literacy_sem_CM3_test<-SE(CM3_test_litsvy)

CM3_test_cmsvy<-svymean(CM3_test$child_mortality, CM3_test_design, na.rm=TRUE)
childmort_mean_CM3_test_wtd<-mean(CM3_test_cmsvy)
childmort_sem_CM3_test<-SE(CM3_test_cmsvy)

CM4_test<-subset(merge, merge$cluster_test==4)
CM4_test_design<-subset(svydesign_merge, merge$cluster_test==4)
CM4_test_litsvy<-svymean(CM4_test$literacy, CM4_test_design, na.rm=TRUE)
literacy_mean_CM4_test_wtd<-mean(CM4_test_litsvy)
literacy_sem_CM4_test<-SE(CM4_test_litsvy)

CM4_test_cmsvy<-svymean(CM4_test$child_mortality, CM4_test_design, na.rm=TRUE)
childmort_mean_CM4_test_wtd<-mean(CM4_test_cmsvy)
childmort_sem_CM4_test<-SE(CM4_test_cmsvy)

CM5_test<-subset(merge, merge$cluster_test==5)
CM5_test_design<-subset(svydesign_merge, merge$cluster_test==5)
CM5_test_litsvy<-svymean(CM5_test$literacy, CM5_test_design, na.rm=TRUE)
literacy_mean_CM5_test_wtd<-mean(CM5_test_litsvy)
literacy_sem_CM5_test<-SE(CM5_test_litsvy)

CM5_test_cmsvy<-svymean(CM5_test$child_mortality, CM5_test_design, na.rm=TRUE)
childmort_mean_CM5_test_wtd<-mean(CM5_test_cmsvy)
childmort_sem_CM5_test<-SE(CM5_test_cmsvy)


literacy_mean<-rbind(literacy_mean_CM1_test_wtd, literacy_mean_CM2_test_wtd, literacy_mean_CM3_test_wtd, literacy_mean_CM4_test_wtd, literacy_mean_CM5_test_wtd)
literacy_sem<-rbind(literacy_sem_CM1_test, literacy_sem_CM2_test, literacy_sem_CM3_test, literacy_sem_CM4_test, literacy_sem_CM5_test)

lit_df<-data.frame(
  x=c(1:5),
  y=literacy_mean,
  sem=literacy_sem
)

library(Hmisc)
plot(literacy_mean, xlab="Economic Groups", ylab="Average Women's Literacy Score", main="Mean Women's Literacy Score by Economic Group", ylim=c(0,2.0))
with(
  data=lit_df,
  expr= errbar(x, y, y+sem, y-sem, add=T, pch=1, cap=0.015)
)



childmort_mean<-rbind(childmort_mean_CM1_test_wtd, childmort_mean_CM2_test_wtd, childmort_mean_CM3_test_wtd, childmort_mean_CM4_test_wtd, childmort_mean_CM5_test_wtd)
childmort_sem<-rbind(childmort_sem_CM1_test, childmort_sem_CM2_test, childmort_sem_CM3_test, childmort_sem_CM4_test, childmort_sem_CM5_test)

cm_df<-data.frame(
  x=c(1:5),
  y=childmort_mean,
  sem=childmort_sem
)

library(Hmisc)
plot(childmort_mean, xlab="Economic Groups", ylab="Average Proportion of Children Deceased", main="Child Mortality by Economic Group", ylim=c(0,0.12))
with(
  data=cm_df,
  expr= errbar(x, y, y+sem, y-sem, add=T, pch=1, cap=0.015)
)



