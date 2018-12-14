library(tidyverse)
library(tree)
library(VSURF)
library(randomForest)

ENS <- read.csv("ENS.txt", header = TRUE, row.names = 1, stringsAsFactors = FALSE)
envi <- read.csv("Envi.txt", header = TRUE, row.names = 1, stringsAsFactors = FALSE)

ENS_all <-full_join(ENS,ENVI)
###### Classification tree

### Creates the tree from the first f=placE)
 tree_1 <- tree(ENS$ENS_0~.,data=envi)
   plot(tree_1)
   text(tree(ENS$ENS_0~.,data=envi))
 
### Prune the tree  to remove over fit vpredictor variables.    
   #### First plot the pruned trees then find the ## of terminal branches that reduces the deviance
   #### 
  plot(cv.tree(tree_1, FUN = prune.tree))
  plot(cv.tree(tree_1, FUN = prune.tree(best = 4)))
  
  plot(prune.tree(tree_1,best = 4)) 
text(prune.tree(tree_1,best = 4))

#### The final plots demonstrate that the length of the 
## The advantages of trees is that not all variables are eqaully important accorss the board
## For example DO in a fast moving stream however in slower moving streams it becomes a very important
## So on local levels 

##### Random Forest

RF1 <- randomForest(ENS$ENS_0~., data = envi, ntree= 5000, mtry=4, importance= T)

RF2 <- randomForest(ENS$ENS_0~., data = envi, ntree= 5000, mtry=4, importance= T)

RF3 <- randomForest(ENS$ENS_0~., data = envi, ntree= 5000, mtry=4, importance= T)

### Change ntree to 500 trees and there will be a great difference between the tops variables for the models
## for examples Wt_Area is a greater that 


#PLOT YOUR FORESTS IMPORTANT VARIABLES
varImpPlot(RF1, type = 1)
varImpPlot(RF2, type = 1)
varImpPlot(RF3, type = 1)

## Partial Dependance Plot
### THESE plots each hash mark is indicative of 10% of the data
### for the following plot will tell you some information re. 
### Just modeling Reponse with 1 predictor after removing the variation of the other variables (Anne Cutler papers)
### PD plots will demonstrate the trend of the  relationship of your variable but X axis values are interpretatable, Y axis values are NOT!!!
### You can compare the relative values between PD plots for differnt variables (like the range of Y values) but only realtive between plots and variables not for the actual values 
### PD plots Diagnostic only, not analytic (Linear vs. NonLinear & Positive vs. Negative relationship of variable)
### There is not reason to look at all variables just the important variables. 
### The Y-axis for the top vairable should have a larger range than any of the other variables because it explains more of the variation in the response variables. 
### The second predictive variable should have slightly smaller range but still large than the next precitive variables and so on down the line

partialPlot(RF1, envi, Wt_Area)
partialPlot(RF1, envi, WT_Urban)
partialPlot(RF1, envi, WT_JMin)

PDP_RF1 <- partialPlot(RF1, envi, Wt_Area)

#### VSURF
### Helps you to select predictors either before or after RForest
### 1) Liberal- Kickout variables that do not have important above threshold you set (small number removed)
### 2) Selects 
### 3) Most conservative- trims redundancy (larger number removed)


VSURF(ENS$ENS_0~., data = envi, ntree = 5000, mtry = 4, parallel = TRUE)

###  The following code runs VSURF 

test <- VSURF(envi, ENS$ENS_0)
plot(test)
summary(test)
names(envi)
test$varselect.thres

colnames(envi[60])
