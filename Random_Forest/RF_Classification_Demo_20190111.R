library(tidyverse)
library(tree)
library(VSURF)
library(randomForest)
###library(readxl)

# metrics_envi.dat <- read.csv("metrics_envi.csv", header = T, row.names = "Type")
# metrics_list_LEH <- read.table("metrics_list.txt", header = T, stringsAsFactors = F) %>%
#   as.matrix()

cal <- read.csv("C:/Users/lhostert/Documents/GitHub/Stats_Workshop_SEL/Data/Cal_85_env.csv", header = T, row.names = "Site")
envi <- read.csv("C:/Users/lhostert/Documents/GitHub/Stats_Workshop_SEL/Data/Envion_46443_reaches.csv", header = T, row.names = "Reach")
mussel <- read.csv("C:/Users/lhostert/Documents/GitHub/Stats_Workshop_SEL/Data/Mussel_binary_environ.csv", header = T, row.names = "EPA_Code")
cal$G11 <- as.factor(cal$G11)

### mussel = Illinois State Mussel Survey 2010-2017. 917 sites fall into wadable
### Presense/Absense of species data followed by predictive environmental variables
### 
RF1_cal <- randomForest(cal$G11~., data = cal, ntree= 5000, mtry=4, importance= T)
RF1_cal
importance(RF1_cal)

benthic <- randomForest(cal$G11~NAWQA+NRSA+WSA+DRAINAGE+SLOP_PCT+ELEV_AVG+ELEV_MIN+ELEV_MAX+ELEV_RNG+ECO3_39+ECO3_45+PPTAVG_C+PPTAVG_B+T_AVG_C+T_AVG_B+T_MIN_B+T_MAX_B+C_Mnprecp+Mnprecip+WD_JAN+WD_MAR+WD_APR+WD_JUL+WD_AUG+Wetdays+ET+PET_B+FST32F_B+LST32F_B+TOPWET+RF+PERHOR+Wat_Tab+CLAYAVE+SANDAVE+KFACT_UP+NO10_AV+BDAVE+MgO_PCT+RECHARGE+Year+DoY, data = cal, ntree=2000, mtry=5, importance = T)
benthic
importance(benthic)

######START Code from Dr. Yong Cao
# input the three datasets as data framework
# as "cal"= "Cal_85_env.dat", "mussel" ="mussel_binary_env.dat", and "eni"="Envi_reaches.dat")

# check dataset size
dim(cal)
dim(mussel)
dim(envi)

# Exercise-1 multiple-group classification 
#### The difference between the classification and regression classification response variable is categoric.
#### You can either use formula=factor(X) or earlier you can define the class as categoric for the response var
attach(cal)
benthic.rf<-randomForest(formula=factor(G11)~NAWQA+NRSA+WSA+DRAINAGE+SLOP_PCT+ELEV_AVG+ELEV_MIN+ELEV_MAX+ELEV_RNG+ECO3_39+ECO3_45+PPTAVG_C+PPTAVG_B+T_AVG_C+T_AVG_B+T_MIN_B+T_MAX_B+C_Mnprecp+Mnprecip+WD_JAN+WD_MAR+WD_APR+WD_JUL+WD_AUG+Wetdays+ET+PET_B+FST32F_B+LST32F_B+TOPWET+RF+PERHOR+Wat_Tab+CLAYAVE+SANDAVE+KFACT_UP+NO10_AV+BDAVE+MgO_PCT+RECHARGE+Year+DoY, ntree=2000, mtry=5, importance = T)
# Confusion Matrix
benthic.rf
importance(benthic.rf)
detach(cal)

## No varienace explained but class.error rate the % of the time that the model will get the data in the right category
### This is determined from the 1/3 "out of bag" data that is not used in teh bootstrapping()

### Base on the threshold 0.5 The following predict() will give you a probability that the sample 
### belongs to each of the 11 class it is how many times a vote is right. 
### Not an actual p-value but probability interpret it similarly (higher is better in this case)
### greater the value the more frequently you get things in the right class.
### ***UPDATE  this is NOT FOR CLASS it is testing each sample of the original dataset and giving you the 
### probability of the following into each class. But here you do not know the actual class of the sample 
### this is in the original data set. 
##### -> Windmere Lab UK Freshwater Monitoring Station. RIVPACS (...Prediction And Classification Systm) Parallel with IBI. 
predict(benthic.rf, cal, type="prob")

## Exercise-2 binary classification-species occurrence
attach(mussel)
species.rf<-randomForest(formula=factor(Lasmigona_complanata)~CH_bd50+Ch_bd100+Ch_bdG100+Glakes+Missi+Bigriver+Pondup_L+Ponddn_L+Pondup_S+Ponddn_S+Pondupst+Ponddnst+Pond+Pondarea+Damupst+Damdnst+Damup_L+Damdn_L+Order+Downorder+Link+Dlink+Sinuosity+CH_Grad+R_Water+RT_Grass+W_Urban+W_Agri+W_Grass+W_Forest+W_Water+W_Wet+W_Area+W_Perm+W_Slope+W_Darcy+WT_Urban+WT_Agri+WT_Grass+WT_Forest+WT_Alluv+WT_Coars+WT_CMorai+WT_Collu+WT_Dune+WT_Fine+WT_ICont+WT_Lacus+WT_Loess+WT_MMora+WT_OWash+WT_PMuck+WT_Rocky+WT_Sand+WT_Shale+WT_Carb+WT_BR50+WT_BR100+WT_BG100+WT_Area+WT_Slope+WT_Precip+WT_Perm+WT_AMean+WT_JMin+WT_JMean+WT_JMax+WT_GDD+WT_Darcy,ntree=2000,mtry=5,importance=T)
species.rf
## Result -> 24.5% error rate 
importance(species.rf)
detach(mussel)
prediction1 <- predict(species.rf, envi)
prediction2 <- predict(species.rf, envi, type="prob")

write.csv(prediction1, "prediction1.csv")
write.csv(prediction2, "prediction2.csv")

### From this you could do a Bernoulli transformation to the p-value predictor
# Exercise-3 use of RF models to predict species presence-absence (0/1) for new data
attach(envi)
species_binary.pred<-predict(species.rf, envi)
species_binary.pred

# Exercise_4 use of RF models to predict species probability for new data
species_prob.pred<-predict(species.rf, envi, type="prob")
species_prob.pred
