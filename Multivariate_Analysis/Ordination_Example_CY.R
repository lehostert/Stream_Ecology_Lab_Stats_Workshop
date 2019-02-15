# Ordination Technique Script from Dr. Yong Cao.
# this program simulates the changes of assemblages from 5 prinstine stream sites along a stress gradient (C)
# based on the estimated tolerance values of individual macroinvertebrate taxa (TV). Each assembalges are consited of 
# 11 super-samples, to which stress is applied.    

# input taxa data and tolerance values
# macro.dat <-read.csv("~/Documents/GitHub/Stats_Workshop_SEL/Multivariate_Analysis/five_assemblages.csv",header=T,row.names="Sample")
macro.dat<-read.table("~/Documents/GitHub/Stats_Workshop_SEL/Multivariate_Analysis/Five_assemblages.txt",sep=",",header=T,row.names="Taxa")
TV.dat <- read.table("~/Documents/GitHub/Stats_Workshop_SEL/Multivariate_Analysis/TV.txt", sep=",",header=T,row.names="Taxa")
attach(macro.dat)
attach(TV.dat)

# simulate stress as described in Cao and Hawkins (2005).

X<-as.matrix(macro.dat)

##Note that this creates a matrix exactly the same size as X but it is an empty matrix. 
##Dimensions of X but all 0 values.
Y<-(X-X)

all<-X

#Set stress level 'c' to a constant
c<-0.2

# j taxa data i from Tolance Value data, X is the original abundance of sample i in sample j
# The purpose is to see how abundace of taxa changes with changing Tolerance Value 
# [TV] calls the TV column from the attached TV.dat data set
for (k in 1:10)
{
  
  for (i in 1:70) 
  {
    
    for (j in 1:55)
    {
      
      Y[i,j]<-as.integer((X[i,j]-(1-TV[i])*c*X[i,j]))
      if(Y[i,j]<0) Y[i,j]<-0
    }
  }
  c<-(c+0.2)
  all<-rbind(all,Y)
}


# combine site super-samples into assemblage

CM<-0
for (m in 1:11){
  CM<-CM+all[,m]
}

GM<-0
for (m in 12:22){
  GM<-GM+all[,m]
}

MK<-0
for (m in 23:33){
  MK<-MK+all[,m]
}

PT<-0
for (m in 34:44){
  PT<-PT+all[,m]
}

TP<-0
for (m in 45:55){
  TP<-TP+all[,m]
}

all_s<-0
all_s<-cbind(CM,GM,MK,PT,TP)

# summarize simulated assemblages changes across 11 stress levels and at five sites 
# in terms of total abundance (TOTAL) and species richness (SR), on which Appendix 1 is based on.
# i controls q value to calc.?? ENS 
# Treat assemblage at each assemblage separately.

total<-matrix(0,11,5)
sr<-matrix(0,11,5)
stress<-seq(0,2,0.2)

for (i in 1:11)
{
  
  for (j in 1:5)
  {
    n1<-(i*70-69)
    n2<-i*70
    total[i,j]<-0
    sr[i,j]<-0
    
    sum<-0
    r<-0
    
    for (k in n1:n2)
    {
      sum<-sum+all_s[k,j]
      if(all_s[k,j]>=1) {r<-r+1}
    }
    total[i,j]<-sum
    sr[i,j]<-r
  }
}

colnames(total)<-c("CM","GM","MK","PT","TP")
colnames(sr)<-c("CM","GM","MK","PT","TP")

TOTAL<-cbind(stress,total)
SR<-cbind (stress,sr)

write.table(TOTAL)
write.table(SR)


# calculate the effective number of species (ENS) for 5 assembalges stressed at 11 elevels based on 11 different q-values
# Figure 1 is created based on the ENS values. The interval of q-value is set to be 0.199999999 instead of 0.2, to avoid  
# q equal to 1, for which ENS not definable (Jost 2007).

ens<-matrix(0,11,5)
B<-0

for (i in 1:11)
{
  q<-i*0.199999999-0.199999999
  
  for (j in 1:5)
  {
    
    for (h in 1:11)
    {
      
      pp<-0
      ens[h,j]<-0
      c<-h*0.2-0.2
      n1<-(h*70-69)
      n2<-h*70
      
      for (k in n1:n2)
      {
        p<-0
        if(all_s[k,j]>0) {p<-all_s[k,j]/total[h,j]
        pp<-pp+p**q}
      }
      
      ens[h,j]<-(pp**(1/(1-q)))
    }
  }
  A<-cbind(stress,round(q,2),round(ens,2))
  B<-rbind(B,A)
}

colnames(B)[2:7]<-c("q-value","CM","GM","MK","PT","TP")

B<-B[-1,]

write.table(B)

# END