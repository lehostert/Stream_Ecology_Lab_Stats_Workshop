##Loop for Adding 1 

for (i in 1:10) {
  
  total<- sum(i+(i-1))
  total2 <- sum +i
} 

total


for (i in 1:10) {
  
total<- sum(i+(i-1))
final_total<- sum(total)
} 
 
final_total
print(total)

######
sum <- 0
for (i in 1:10) 
  {
  sum <- sum+i
} 

print(sum)
######
sum <- 0

for (i in seq(1, 10, 1)) 
{
  sum <- sum+i
} 

print(sum)

######
for (i in 1:10) {
  
factorial<- i*(i+1)
print(factorial)
}

factorial

#  
# for (i in metrics_list_LEH)
#   
# {
#   for (j in 1:8)
#   {
#     
#     Metric<-get(paste(i))
#     
#     Metric.rf<-randomForest(Metric~NRSA+WSA+DRAINAGE+SLOP_PCT+ELEV_AVG+ELEV_MIN+ELEV_MAX+ELEV_RNG+ECO3_39+ECO3_45+PPTAVG_C+PPTAVG_B+T_AVG_C+T_AVG_B+T_MIN_B+T_MAX_B+C_Mnprecp+Mnprecip+WD_JAN+WD_MAR+WD_APR+WD_JUL+WD_AUG+Wetdays+ET+PET_B+FST32F_B+LST32F_B+TOPWET+RF+PERHOR+Wat_Tab+CLAYAVE+SANDAVE+KFACT_UP+NO10_AV+BDAVE+MgO_PCT+RECHARGE+Year+DoY,ntree=5000,importance=T, mtry=j)
#     
#     R_value<-Metric.rf$rsq[5000]
#     
#     A<-c(i,j,R_value)
#     
#     print (A)
#     
#   }
# }