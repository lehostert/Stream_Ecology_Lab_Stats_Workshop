library(tidyverse)
library(tree)
library(VSURF)
library(randomForest)

####### The metrics data set

metrics_envi.dat <- read.csv("metrics_envi.csv", header = T, row.names = "Type")
metrics_list_LEH <- read.table("metrics_list.txt", header = T, stringsAsFactors = F) %>%
  as.matrix()
#### This file must be a matrix in order to use it in the loop
#### try to add data to the RF command in order to get around the "attach" function
for (variable in vector) {
  
}

sink("Metrics_RF_Result_LEH.txt")

for (i in metrics_list_LEH)
  
{
  for (j in 1:8)
  {
    
    Metric<-get(paste(i))
    
    Metric.rf<-randomForest(Metric~NRSA+WSA+DRAINAGE+SLOP_PCT+ELEV_AVG+ELEV_MIN+ELEV_MAX+ELEV_RNG+ECO3_39+ECO3_45+PPTAVG_C+PPTAVG_B+T_AVG_C+T_AVG_B+T_MIN_B+T_MAX_B+C_Mnprecp+Mnprecip+WD_JAN+WD_MAR+WD_APR+WD_JUL+WD_AUG+Wetdays+ET+PET_B+FST32F_B+LST32F_B+TOPWET+RF+PERHOR+Wat_Tab+CLAYAVE+SANDAVE+KFACT_UP+NO10_AV+BDAVE+MgO_PCT+RECHARGE+Year+DoY,ntree=5000,importance=T, mtry=j)
    
    R_value<-Metric.rf$rsq[5000]
    
    A<-c(i,j,R_value)
    
    print (A)
    
  }
}

sink()

list()


for (i in metrics_list_LEH)
{
  print(i)
}

#### Begin text from file Metrics_RF.txt
### Changed "clipboard" to the file name in my local directory. 
#data input
metrics_envi.dat<-read.table("metrics_envi.csv",header=T,row.name="Site_code",sep=",")
attach(metrics_envi.dat)


#metric_list has to be in matrix form.
metrics_list<-read.table("metrics_list.txt",header=T)
metrics_list<-as.matrix(metrics_list)

# record the output into a file (something like dump might do the same thing)
sink("Metrics_RF_Result.txt")

for (i in metrics_list)
  
{
  for (j in 1:8)
  {
    
    Metric<-get(paste(i))
    
    Metric.rf<-randomForest(Metric~NRSA+WSA+DRAINAGE+SLOP_PCT+ELEV_AVG+ELEV_MIN+ELEV_MAX+ELEV_RNG+ECO3_39+ECO3_45+PPTAVG_C+PPTAVG_B+T_AVG_C+T_AVG_B+T_MIN_B+T_MAX_B+C_Mnprecp+Mnprecip+WD_JAN+WD_MAR+WD_APR+WD_JUL+WD_AUG+Wetdays+ET+PET_B+FST32F_B+LST32F_B+TOPWET+RF+PERHOR+Wat_Tab+CLAYAVE+SANDAVE+KFACT_UP+NO10_AV+BDAVE+MgO_PCT+RECHARGE+Year+DoY,ntree=500,importance=T, mtry=j)
    
    R_value<-Metric.rf$rsq[500]
    
    A<-c(i,j,R_value)
    
    print (A)
    
  }
}
## Closes the sink
sink()
## Print to screen
list()

#####################################################################################
#####################################################################################
### Begin text from file R_code_for_plots.txt

# data input
species.dat<-read.table("species_response.csv",header=T,sep=",")

# list all unique species names
Species<-unique(species.dat$Species)

# specify output-file type and name

pdf(file="fish.pdf")

#loop for generating multiple plots for all species

for (j in Species)
{

# set plot arrangement for each page
par(mfrow=c(3,4))

categories=NULL

species<-subset(species.dat,Species==j)

categories <-unique(species$Variable)

for (i in categories)
{
  plot_data <- subset(species, Variable == i)
  plot(plot_data$X, plot_data$Y, main=c(j), xlab=c(i), ylab="log_abundance", type="l")
}
}
dev.off()

### categories = Null is an object that Yong defined to pull out the unique variables from the species.dat see lines 104 & 108. 
### To explain dev.off () to turn off pdf() because this is a command that is considered a device.
### The above will print "null device 1" when finished this is because after you run the dev.off() 
### there are no items to print out/show in R because they have all been saved as .pdfs