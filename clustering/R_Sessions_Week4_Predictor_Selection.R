##Note: You might have to install some of the following packages!

library(sf) #For working with spatial objectslibrary(ggplot2)
library(ggplot2) #The gold standard for visualizing data and preparing publication-quality figures
library(dplyr) #Another package that helps with data manipulation and cleanup
library(tidyr) #Newer package that helps streamline data manipulation
library(readxl) #This package helps import spreadsheet-formatted data, primarily from Excel
library(cluster) #This package will allow us to generate variable groupings and clusters based on multiple metrics
library(corrplot) #This package will help us evaluate predictors based on correlation
library(fpc) #This package will let us compare among clustering options and results to determine best route to proceed. VERY versatile package
library(pvclust) #This package will let us assess variable relatedness using bootstrapping and p-values
library(factoextra) #Visualize clustering results
library(dendextend) #Visualize and compare between dendrograms


#########################################################
#Let's first bring in our full dataset
#All predictors for Illinois reaches
#########################################################
#Bring in predictor data saved in .csv format
setwd('C:/Users/INHSadmin/Desktop/Lab Group_R Sessions/Week 4 Materials')
il_data<-read.csv('IL_Reaches_All_Info.csv',header=T)

#########################################################
#Bring in spatial data
#Reconcile with our reach-level data
#########################################################
#Let's first get a list of all the shapefiles available in our working directory
spatial_data<-list.files('C:/Users/INHSadmin/Desktop/Lab Group_R Sessions',pattern='.shp',recursive=T,full.names = T)

#Now, let's load our different shapefiles
reaches<-st_read(spatial_data[3])
illinois<-st_read(spatial_data[4])
good_ill<-st_transform(illinois,crs=st_crs(reaches)) #It's easy to specify projections from other spatial objects using the "st_crs" function

#Let's first merge our reach data so all reaches are associated with relevant data
good_reaches<-merge(reaches,il_data,by='COMID')
good_df<-st_drop_geometry(good_reaches)


#########################################################
#Begin processing data
#First, consider removing variables that are of no interest
#########################################################
drops <- c("COMID","X","HUC_4_x","HUC_6","Width","ReachWidth","HUC_4_y","Ct_Pr08","Ct_Pr09","Ct_Tm08",'Ct_Tm09',
          'Ws_Pr08','Ws_Pr09','Ws_Tm08','Ws_Tm09','pG_BMMI')
step1_data<-good_df[,!(names(good_df) %in% drops)] 


#########################################################
#Next, consider combining variables if sensical
#Multiple ways of doing this!
#########################################################
#Combine the following into aggregate variables
Ct_Urb2011<-c('Ct_UrbO','Ct_UrbL','Ct_UrbM','Ct_UrbH')
Ws_Urb2011<-c('Ws_UrbO','Ws_UrbL','Ws_UrbM','Ws_UrbH')
Ct_Til<-c('Ct_PctCl','Ct_PctL','Ct_PcTC')
Ws_Til<-c('Ws_PctCl','Ws_PctL','Ws_PcTC')
Ct_Lake<-c('Ct_PcLC','Ct_PctF')
Ws_Lake<-c('Ws_PcLC','Ws_PctF')
ws_eol<-c('Ws_PcEC','Ws_PcEF')
ct_eol<-c('Ct_PcEC','Ct_PcEF')
Ct_wet<-c('Ct_WdWt','Ct_HbWt')
Ws_wet<-c('Ws_WdWt','Ws_HbWt')
Ct_B_wet<-c('Ct_B_WW','Ct_B_HW')
Ws_B_wet<-c('Ws_B_WW','Ws_B_HW')
Ct_Forest<-c('Ct_Decd','Ct_Conf','Ct_MxFs','Ct_Shrb')
Ws_Forest<-c('Ws_Decd','Ws_Conf','Ws_MxFs','Ws_Shrb')
Ct_B_Forest<-c('Ct_B_Dc','Ct_B_Cn','Ct_B_MF','Ct_B_Sh')
Ws_B_Forest<-c('Ws_B_Dc','Ws_B_Cn','Ws_B_MF','Ws_B_Sh')
Ct_HayGrass<-c('Ct_Hay','Ct_Grs')
Ws_HayGrass<-c('Ws_Hay','Ws_Grs')
Ct_B_HayGrass<-c('Ct_B_Gr','Ct_B_Hy')
Ws_B_HayGrass<-c('Ws_B_Gr','Ws_B_Hy')
Ct_LowLUrp100<-c('Ct_B_UO','Ct_B_UL')
Ws_LowLUrp100<-c('Ws_B_UO','Ws_B_UL')
Ct_HiLUrp100<-c('Ct_B_UM','Ct_B_UH')
Ws_HiLUrp100<-c('Ws_B_UM','Ws_B_UH')

#Combine variables, remove individual variables at end of process
step2_data <- step1_data %>%
  mutate(PctUrb2011Cat=rowSums(step1_data[,Ct_Urb2011], na.rm = TRUE),
         PctUrb2011Ws=rowSums(step1_data[,Ws_Urb2011], na.rm = TRUE),
         PctGlacTilCat=rowSums(step1_data[,Ct_Til], na.rm=TRUE),
         PctGlacTilWs=rowSums(step1_data[,Ws_Til], na.rm=TRUE),
         PctGlacLakeCat=rowSums(step1_data[,Ct_Lake], na.rm=TRUE),
         PctGlacLakeWs=rowSums(step1_data[,Ws_Lake], na.rm=TRUE),
         PctEolWs=rowSums(step1_data[,ws_eol], na.rm=TRUE),
         PctEolCt=rowSums(step1_data[,ct_eol], na.rm=TRUE),
         PctWet2011Ct=rowSums(step1_data[,Ct_wet], na.rm=TRUE),
         PctWet2011Ws=rowSums(step1_data[,Ws_wet], na.rm=TRUE),
         PctWet2011CtRp100= rowSums(step1_data[,Ct_B_wet],na.rm=TRUE),
         PctWet2011WsRp100= rowSums(step1_data[,Ws_B_wet],na.rm=TRUE),
         PctForest2011Cat=rowSums(step1_data[,Ct_Forest], na.rm=TRUE),
         PctForest2011Ws=rowSums(step1_data[,Ws_Forest], na.rm=TRUE),
         PctForest2011CtRp100=rowSums(step1_data[,Ct_B_Forest], na.rm=TRUE),
         PctForest2011WsRp100=rowSums(step1_data[,Ws_B_Forest], na.rm=TRUE),
         PctHayGrassCt=rowSums(step1_data[,Ct_HayGrass], na.rm=TRUE),
         PctHayGrassWs=rowSums(step1_data[,Ws_HayGrass], na.rm=TRUE),
         PctHayGrassCtRp100=rowSums(step1_data[,Ct_B_HayGrass], na.rm=TRUE),
         PctHayGrassWsRp100=rowSums(step1_data[,Ws_B_HayGrass], na.rm=TRUE),
         PctLLUCtRp100=rowSums(step1_data[,Ct_LowLUrp100], na.rm=TRUE),
         PctLLUWsRp100=rowSums(step1_data[,Ws_LowLUrp100], na.rm=TRUE),
         PctHLUWsRp100=rowSums(step1_data[,Ws_HiLUrp100], na.rm=TRUE))%>%
  select(-c(all_of(Ct_Urb2011),all_of(Ws_Urb2011),all_of(Ct_Til),all_of(Ws_Til),all_of(Ct_Lake),all_of(Ws_Lake),all_of(ws_eol),all_of(ct_eol)
            ,all_of(Ct_wet),all_of(Ws_wet),all_of(Ct_B_wet),all_of(Ws_B_wet),all_of(Ct_Forest),all_of(Ws_Forest),all_of(Ct_B_Forest)
            ,all_of(Ws_B_Forest),all_of(Ct_HayGrass),all_of(Ws_HayGrass),all_of(Ct_B_HayGrass),all_of(Ws_B_HayGrass)
            ,all_of(Ct_LowLUrp100),all_of(Ws_LowLUrp100),all_of(Ct_HiLUrp100),Ws_HiLUrp100))


#########################################################
#Next, consider removing variables that contribute little to accounting for variance/deviance or to detecting signal of interest
#Variables with little inherent variance are of particular interest
#########################################################
##Eliminate those variables with <10% unique values##
#Define function Mode, create list of percent modes of each metric/column
Mode <- function(x) { 
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

mode_freq2<-apply(step2_data,2,function(x)length(which(x==Mode(x)))/length(x)) #This function looks at mode relative to variable length

# find the frequency of each mode
logical_ones <- as.vector(mode_freq2 < 0.90) #Identifies variables with less than 90% values that are all the same. 
mode_greater_than_90<- colnames(step2_data)[!logical_ones] #returns list of column names of metrics with greater than 90% similarity.Not much variation to compare.

#Remove metrics with >90% mode frequency
step3_data <- step2_data[,!names(step2_data) %in% mode_greater_than_90] 


#########################################################
#Next, evaluate correlation among variables
#Including highly correlated variables in models can complicate interpretation of each variable's effect on the response
#########################################################
###################################
#Approach 1
#Evaluate correlations among all predictors
#Save as matrix
#Identify, by hand, those that may be redundant
#Remove accordingly
###################################
#Generate matrix of cleaned variable correlations
var_corr_raw=cor(step3_data)

#Write to file
#setwd('C:/Users/INHSadmin/Desktop/Lab Group_R Sessions/Week 4 Materials')
#write.csv(var_corr_final,'Cleaned_Reach_Correlations.csv')

#Visualize
corr_plot<-corrplot(var_corr_raw)

#Examine a subset
var_subset<-cor(step3_data[,1:15])
corr_plot2<-corrplot(var_subset)

###################################
#Approach 2
#Allow data to tell you how many predictor groups to consider
#Partition predictors into groups using k-means partitioning
#Select and remove predictors within groups
###################################
#Consider scaling data to facilitate comparisons
scaled_df <- scale(step3_data)
var_corr_scaled<-cor(scaled_df)

#Generate dissimilarity matrices based on correlation distances
dist_mat_raw<-as.dist(1-abs(var_corr_raw))
dist_mat_scaled<-as.dist(1-abs(var_corr_scaled))

#Cluster
pam_clusters_raw<-pamk(dist_mat_raw)
pam_clusters_scaled<-pamk(dist_mat_scaled)
#Looking at these results suggests that our results are similar regardless of scaling beforehand

#Let's group into 10, consider raw data and data based on distance matrix
pam_dist <- kmeans(dist_mat_raw,10) # 10 cluster solution

#So, let's look at why 10 clusters seems to be best and identify potential redundancy
kmeans_plot<-fviz_cluster(pam_dist,data=dist_mat_raw)


###################################
#Approach 3
#Evaluate correlations among all predictors
#Use correlation values as distance-based metric of predictor similarity/redundancy
#Cluster variables using distance-based, Pearson product-moment correlations and hierarchical clustering methods
#Cut at <0.3, meaning that variables have >0.7 Pearson product moment
###################################
#Generate matrix of cleaned variable correlations
var_corr_final=cor(step3_data)

#Convert correlation matrix into distance-based matrix
new_dist_mat<-as.dist(1 - abs(var_corr_final))

#Use agglomerative clustering
#Huge body of literature here about different types of clustering, metrics, and methods!
#Lots of different functions from multiple packages
?hclust
?agnes
?diana
clusters_average<-agnes(new_dist_mat,diss=TRUE,metric='euclidean',method='average')
clusters_single<-agnes(new_dist_mat,diss=TRUE,metric='euclidean',method='single')
clusters_ward<-agnes(new_dist_mat,diss=TRUE,metric='euclidean',method='ward')

#Visualize
par(mfrow=c(1,2))
plot(clusters_average)
plot(clusters_single)
plot(clusters_ward)

#Just see dendrogram
par(mfrow=c(1,1))
pltree(clusters_average)

#Convert to dendogram objects, allows to save
hc_average<- as.dendrogram(clusters_average)
hc_single<- as.dendrogram(clusters_single)

#Compare between dendrogram options
tanglegram(hc_average,hc_single)


###################################
#Approach 4
#Use multiscale bootstrapping among predictors to hierarchically cluster
#Very time intensive with large datasets, but calculates statistical significance of variable relationships
#I haven't used this before in my own work, but could be a neat way to justify predictor selection
###################################
#Let's subset data to speed up
subset_data<-step3_data[1:500,1:15]

#Bootstrap 100 times, default is 1000
approach4_fit <- pvclust(subset_data, method.hclust="average",method.dist="euclidean",nboot=50)

par(mfrow=c(1,1))
plot(approach4_fit)












