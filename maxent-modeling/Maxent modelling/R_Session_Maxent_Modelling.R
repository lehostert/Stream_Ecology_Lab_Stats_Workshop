##Note: You might have to install some of the following packages!

library(sf) #For working with spatial objectslibrary(ggplot2)
library(ggplot2) #The gold standard for visualizing data and preparing publication-quality figures
library(dplyr) #Another package that helps with data manipulation and cleanup
library(tidyr) #Newer package that helps streamline data manipulation
options(java.parameters = "-Xmx2g" ) #This command will allow you to maximize the amount of memory available for modeling
library(dismo) #This package is what we'll use for our Maxent modeling
library(viridis) #Use color palettes for visually impaired
library(readxl) #This package helps import spreadsheet-formatted data, primarily from Excel
library(spocc) #This package will allow us to search through biodiversity databases to find occurrence records!
library(scrubr) #This package can help clean occurrence records obtained using package spocc
library(maptools) #This package provides some functions that can help plot geographic data
library(rnaturalearth) #This package will help us import world shapefiles
library(rnaturalearthdata) #This package will help us import world shapefiles

#########################################################
#First, a quick introduction to package "spocc"
#Let's look up occurrence records for a species of interest, the smooth softshell turtle (Apalone mutica)
#########################################################
#We can get all locations
all_locations<-occ(query = 'Apalone mutica', from = c('gbif','bison','inat','ecoengine','vertnet'))
all_locations
head(all_locations)

#Convert all locations to data frame
all_location_df<-occ2df(all_locations,what='data')

#Next, let's work on cleaning up those records
#Remove incomplete coordinates, then duplicate coordinates
all_final_locations<-dframe(all_location_df) %>% 
  coord_incomplete() %>%
  dedup()

#Let's convert these locations into sf objects
all_record_sf<-st_as_sf(all_final_locations, coords = c("longitude", "latitude"), crs = 4269)

#Now, let's see where these locations are
#Import entire world shapefile, convert to same projection as records
world <- ne_countries(scale = "medium", returnclass = "sf")
good_world<-st_transform(world,crs=st_crs(all_record_sf))

#Visualize!
plot(st_geometry(good_world))
plot(st_geometry(all_record_sf),add=T,col='red',pch=19)

#Let's look at these records in Illinois
setwd('C:/Users/INHSadmin/Desktop/Lab Group_R Sessions') #This should be changed to your appropriate directory
spatial_data<-list.files('C:/Users/INHSadmin/Desktop/Lab Group_R Sessions',pattern='.shp',recursive=T,full.names = T)
illinois<-st_read(spatial_data[4])
good_ill<-st_transform(illinois,crs=st_crs(good_world)) #It's easy to specify projections from other spatial objects using the "st_crs" function

plot(st_geometry(good_ill))
plot(st_geometry(all_record_sf),add=T,col='red',pch=19)




#########################################################
#########################################################
#OK, let's switch gears slightly
#Imagine if we've already collected occurrence records for a species of interest
#We want to leverage these records to predict its distribution in a region of interest
#But, we don't have a good idea of where it is NOT found
#Ideal situation to use Maxent
#########################################################
#########################################################

#########################################################
#Let's first set our working directory to where our materials have been saved
#########################################################
setwd('C:/Users/INHSadmin/Desktop/Lab Group_R Sessions') #This should be changed to your appropriate directory

#########################################################
#Bring in all Excel data
#########################################################
#There are several ways to import spreadsheet-based data
#The appropriate function depends on the spreadsheet format
reach_info<-read.csv('Illinois_Reach_Info.csv',header=T) 
reach_info<-reach_info[,-1]
mussel_records<-read.csv('Example_Mussel_Records.csv',header=T)
mussel_records<-mussel_records[,-1]

#########################################################
#Bring in spatial data
#########################################################
#Previous packages for working with spatial data (package "sp") forced you to use objects like SpatialPolygons or SpatialLines
#These objects were cumbersome and did not easily lend themselves to data visualization
#Package sf addresses these shortcomings

#Let's first get a list of all the shapefiles available in our working directory
spatial_data<-list.files('C:/Users/INHSadmin/Desktop/Lab Group_R Sessions',pattern='.shp',recursive=T,full.names = T)

#Now, let's load our different shapefiles
reaches<-st_read(spatial_data[3])
illinois<-st_read(spatial_data[4])
good_ill<-st_transform(illinois,crs=st_crs(reaches)) #It's easy to specify projections from other spatial objects using the "st_crs" function

#Let's first merge our reach data so all reaches are associated with relevant data
good_reaches<-merge(reaches,reach_info,by='COMID')

#Generate all reach info template
all_reach_template<-good_reaches

#########################################################
#We now have an idea of the number of IL occurrence records for our species of interest
#We now need to select locations where that species is thought to NOT occur
#Combine these two datasets together to generate a final dataset that will be used for modeling
#########################################################
#Identify the reaches that have occurrence records for each species
#Then partition out remaining reaches that are available to select as "absences"/"pseudoabsences"/"background"
#Clean records so only reaches with complete information used in modeling
spec_name=mussel_records$Species
spec_reaches<-good_reaches[good_reaches$COMID %in% mussel_records$COMID,]
training_reaches<-spec_reaches%>%
  drop_na()

#Select reaches as pseudoabsences or background at random
#This is a very basic method of background selection - there are more sophisticated methods available
`%nin%` = Negate(`%in%`)
potential_reaches<-good_reaches[good_reaches$COMID %nin% training_reaches$COMID,]
background_reaches<-sample_n(potential_reaches,1000)

#Let's visualize everything
ylab <- "Latitude (°N)"
xlab <- "Longitude (°W)"

maxent_info<-ggplot()+
  geom_sf(data=st_geometry(good_ill),lwd=0.75)+
  geom_sf(data=st_geometry(good_reaches),lwd=0.75)+
  geom_sf(data=st_geometry(training_reaches),col='blue',lwd=0.75)+
  geom_sf(data=st_geometry(background_reaches),col='red',lwd=0.75)+
  theme_bw()+
  labs(y=ylab,x=xlab)

#Finally, let's prep our data so we can indicate occurrences "1" and background "0"
training_reaches$Occurrence<-rep(1,nrow(training_reaches))
background_reaches$Occurrence<-rep(0,nrow(background_reaches))

#Split training and background reaches into model building and validating datasets
#Use k-fold partitioning, assign as a new column
training_reaches$kfold<-kfold(training_reaches,k=5)
background_reaches$kfold<-kfold(background_reaches,k=5)

#Designate training/validation presences, training/validation background
training_presences<-training_reaches[training_reaches$kfold!=3,]
validation_presences<-training_reaches[training_reaches$kfold==3,]
training_background<-background_reaches[background_reaches$kfold!=3,]
validation_background<-background_reaches[background_reaches$kfold==3,]

#Combine into our final dataset
final_training_data<-rbind(training_presences,training_background)
final_validation_data<-rbind(validation_presences,validation_background)

#########################################################
#Now that we have our final datasets, let's start with the actual modeling process
#First, prep our training data to be imported into Maxent function
#Then, extract information for each predictor variable
#Lastly, project Maxent model across entire study region
#########################################################
#Write datasets to file!
#Create directory for each species modeled! This is super useful for whatever work you're doing!
spec_name<-mussel_records$SPECIES[1]
species_folder_name=gsub(" ","_",spec_name)
dir.create(species_folder_name)
new_working_directory<-paste(getwd(),species_folder_name,sep='/')
setwd(new_working_directory)

#Start with training data
training_file_name<-paste(species_folder_name,'TRAINING_model_data',sep='_')
#st_write(final_training_data,dsn=training_file_name,layer=training_file_name,driver='ESRI Shapefile')

#Final prep before maxent fitting
final_data_df<-st_drop_geometry(final_training_data[,2:64])

#Conduct Maxent modeling
#Path to save results
use_this<-getwd()
Max_dataset<-maxent(x=final_data_df[,-63],p=final_data_df$Occurrence,path=use_this) #Check the arguments for function "maxent"
results_save<-as.data.frame(Max_dataset@results)
write.csv(results_save,'All_Results.csv')

#Get predictor responses
dir.create("Variable_Response_Info")
write_to_this<-paste(getwd(),"/",'Variable_Response_Info',sep='')
setwd(write_to_this)
variable_names<-colnames(final_data_df)
variable_names<-variable_names[-63]
for (j in 1:length(variable_names)){
  var_interest<-variable_names[j]
  mean_response<-as.data.frame(response(Max_dataset,at=mean,var=var_interest))
  max_response<-as.data.frame(response(Max_dataset,at=max,var=var_interest))
  min_response<-as.data.frame(response(Max_dataset,at=min,var=var_interest))
  median_response<-as.data.frame(response(Max_dataset,at=median,var=var_interest))
  final_response<-cbind(mean_response,max_response[,2],min_response[,2],median_response[,2])
  colnames(final_response)<-c('Pred Value','Mean Prob','Max Prob','Min Prob','Median Prob')
  df_name<-paste(var_interest,'Variable_Response_Values',spec_name,".csv",sep='_')
  write.csv(final_response,df_name)
}

#Predict across all reach data
setwd(new_working_directory)
all_reach_info<-st_drop_geometry(good_reaches)
Max_domain<-predict(Max_dataset,all_reach_info)
big_number<-max(Max_domain)
rescaled_suit<-Max_domain/big_number
all_reach_template$Suit<-rescaled_suit
data_file_name<-paste(spec_name,'PROJECTED_MAXENT_DATA',sep='_')
#st_write(all_reach_template,dsn=data_file_name,layer=data_file_name,driver='ESRI Shapefile')

#Plot the map
figure_name<-paste(spec_name,'_Maxent_pred.png',sep='')
ylab <- "Latitude (°N)"
xlab <- "Longitude (°W)"
legend<- 'Suitability'
proj_maxent_figure<-ggplot()+
  geom_sf(data=st_geometry(good_ill),fill='grey85')+
  geom_sf(data=all_reach_template,aes(color=Suit),lwd=1)+
  scale_color_viridis(option = "D",begin=0,end=1,limits=c(0,1))+
  labs(y=ylab,x=xlab,color=legend)+
  theme_bw()
#ggsave(filename=figure_name,plot=proj_maxent_figure,device='png',path=new_working_directory,width=8,height=8,units='in',dpi=1000)


#########################################################
#Let's evaluate the performance of our Maxent model
#########################################################
#Evaluate our Maxent model
#Need to split our validation data into presences and absences
valid_pres<-final_validation_data[final_validation_data$Occurrence==1,]
valid_abs<-final_validation_data[final_validation_data$Occurrence==0,]

#Now, evaluate Maxent model
model_eval<-evaluate(model=Max_dataset,p=valid_pres,a=valid_abs)
eval_data<-cbind(model_eval@np,model_eval@na,model_eval@auc,model_eval@cor,model_eval@t,model_eval@TPR,model_eval@TNR,model_eval@MCR,model_eval@kappa,model_eval@ODP)
colnames(eval_data)<-c('Number_Presences','Number_Pseudoabsences','Model_Validation_AUC','Corr','Threshold','Model_TruePositiveRate','Model_TrueNegativeRate','Misclass_Rate','Kappa','Overall_Diagnostic_Power')
testing_evaluation<-paste(spec_name,'Model_Validation_Evaluation.csv',sep='')
write.csv(eval_data,testing_evaluation)


#########################################################
#Let's generate binary, presence-absence predictions 
#########################################################
#Impose threshold, add as column
#Write entire file to disk!
model_spec_sens_threshold<-threshold(model_eval,stat='spec_sens')
model_kappa_threshold<-threshold(model_eval,stat='kappa')
all_reach_template$SSThresh<-model_spec_sens_threshold
all_reach_template$KappaThresh<-model_kappa_threshold
all_reach_template$SS<-rep(1,nrow(all_reach_template))
all_reach_template$SS[all_reach_template$Suit>=model_spec_sens_threshold]<-1
all_reach_template$SS[all_reach_template$Suit<model_spec_sens_threshold]<-0
all_reach_template$Kappa<-rep(1,nrow(all_reach_template))
all_reach_template$Kappa[all_reach_template$Suit>=model_kappa_threshold]<-1
all_reach_template$Kappa[all_reach_template$Suit<model_kappa_threshold]<-0
data_file_name<-paste(spec_name,'BINARY_MAXENT_DATA',sep='_')
#st_write(route1_all_reach_template,dsn=data_file_name,layer=data_file_name,driver='ESRI Shapefile')

#Plot the map
figure_name<-paste(spec_name,'_Maxent_BINARY_SpecSens.png',sep='')
ylab <- "Latitude (°N)"
xlab <- "Longitude (°W)"
legend<- 'Presence/Absence_SpecSens_Thresh'
maxent_figure_ss<-ggplot()+
  geom_sf(data=st_geometry(good_ill),fill='grey85')+
  geom_sf(data=all_reach_template,aes(color=SS),lwd=1)+
  scale_color_viridis(option = "D",begin=0,end=1,limits=c(0,1))+
  labs(y=ylab,x=xlab,color=legend)+
  theme_bw()
#ggsave(filename=figure_name,plot=maxent_figure_ss,device='png',path=new_working_directory,width=8,height=8,units='in',dpi=1000)

figure_name<-paste(spec_name,'_Maxent_BINARY_Kappa.png',sep='')
ylab <- "Latitude (°N)"
xlab <- "Longitude (°W)"
legend<- 'Presence/Absence_Kappa_Thresh'
maxent_figure_kappa<-ggplot()+
  geom_sf(data=st_geometry(good_ill),fill='grey85')+
  geom_sf(data=all_reach_template,aes(color=Kappa),lwd=1)+
  scale_color_viridis(option = "D",begin=0,end=1,limits=c(0,1))+
  labs(y=ylab,x=xlab,color=legend)+
  theme_bw()
#ggsave(filename=figure_name,plot=maxent_figure_kappa,device='png',path=new_working_directory,width=8,height=8,units='in',dpi=1000)





