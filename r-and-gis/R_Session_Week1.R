##Note: You might have to install some of the following packages!

library(sf) #For working with spatial objects
library(ggplot2) #The gold standard for visualizing data and preparing publication-quality figures
library(tidyr) #Newer package that helps streamline data manipulation
library(spdep) #This package is very versatile for geospatial analyses and data manipulation
library(readxl) #This package helps import spreadsheet-formatted data, primarily from Excel
library(dplyr) #Another package that helps with data manipulation and cleanup
library(viridis) #Use color palettes for visually impaired
library(plyr) #Another package that helps with data manipulation and cleanup

#########################################################
#Let's first set our working directory to where our materials have been saved
#########################################################
setwd('C:/Users/INHSadmin/Desktop/Lab Group_R Sessions') #This should be changed to your appropriate directory

#########################################################
#Bring in all Excel data
#########################################################
#There are several ways to import spreadsheet-based data
#The appropriate function depends on the spreadsheet format
reach_info<-read.csv('Illinois_Reach_Info.csv',header=T) #read.csv will work for comma-delimited files
huc4_info<-read_excel('HUC_Info.xlsx',sheet=1) #read_excel is for non-.csv files and will let you specify what worksheet to import
huc8_info<-read_excel('HUC_Info.xlsx',sheet=2)

#########################################################
#Bring in spatial data
#########################################################
#Previous packages for working with spatial data (package "sp") forced you to use objects like SpatialPolygons or SpatialLines
#These objects were cumbersome and did not easily lend themselves to data visualization
#Package sf addresses these shortcomings

#Let's first get a list of all the shapefiles available in our working directory
spatial_data<-list.files('C:/Users/INHSadmin/Desktop/Lab Group_R Sessions',pattern='.shp',recursive=T,full.names = T)

#Now, let's load our different shapefiles
huc4<-st_read(spatial_data[1])
huc8<-st_read(spatial_data[2])
reaches<-st_read(spatial_data[3])
illinois<-st_read(spatial_data[4])

#Lastly, let's convert all of these spatial objects into dataframes that can be easily merged together
huc4_df<-st_drop_geometry(huc4)
huc8_df<-st_drop_geometry(huc8)
reaches_df<-st_drop_geometry(reaches)

#########################################################
#First plot of our spatial data
#########################################################
#Let's start with our basic plot
plot(st_geometry(illinois)) #Calling the function "st_geometry" indicates we only want the shape of Illinois, not a feature/attribute
plot(illinois$POPULAT) #Calling specific features plots their data, not the shape

#Let's add together
plot(st_geometry(illinois))
plot(st_geometry(reaches),add=T)
#Huh...something isn't adding up here. Reaches aren't plotting with the state outline. Need to change the state's projection!

good_ill<-st_transform(illinois,crs=st_crs(reaches)) #It's easy to specify projections from other spatial objects using the "st_crs" function

#Let's add together
#Easily customizeable!
plot(st_geometry(good_ill))
plot(st_geometry(reaches),add=T)

#Now, let's prep a publication quality figure
ylab <- "Latitude (°N)"
xlab <- "Longitude (°W)"

study_region<-ggplot()+
  geom_sf(data=st_geometry(good_ill),color='grey60',lwd=0.5)+
  geom_sf(data=st_geometry(reaches),color='grey35',alpha=0.25)+
  theme_bw()

ggsave(filename='StudyRegion.png',plot=study_region,device='png',path='C:/Users/INHSadmin/Desktop/Lab Group_R Sessions',width=8,height=8,units='in',dpi=1000)
#Make sure you change the path argument to whatever directory you want to save your image to. 
#You can also specify image size and resolution. SET THE RESOLUTION HIGH SO ALL FIGURES ARE PUBLICATION QUALITY!

#########################################################
#Compile and merge data
#########################################################
#Let's first merge our reach data so all reaches are associated with relevant data
good_reaches<-merge(reaches,reach_info,by='COMID')

#Now that we have data, we can plot specific features/attributes by specifying column numbers
plot(st_geometry(good_ill))
plot(good_reaches[,3],add=T)

#Prepare to plot again
ylab <- "Latitude (°N)"
xlab <- "Longitude (°W)"
legend <- "Stream Order"

stream_orders<-ggplot()+
  geom_sf(data=st_geometry(good_ill),color='grey60',lwd=0.5)+
  geom_sf(data=good_reaches,aes(color=Order),alpha=0.75)+
  scale_color_gradient(low = "black", high = "chartreuse",limits=c(min(good_reaches$Order),max(good_reaches$Order)))+
  labs(y=ylab,x=xlab,color=legend)

stream_orders1<-ggplot()+
  geom_sf(data=st_geometry(good_ill),color='grey60',lwd=0.5)+
  geom_sf(data=good_reaches,aes(color=Order),alpha=0.75)+
  scale_color_viridis(option = "D",begin=min(good_reaches$Order)/max(good_reaches$Order),end=max(good_reaches$Order)/max(good_reaches$Order))+
  labs(y=ylab,x=xlab,color=legend)


#Next, let's compile our HUC data
#There appears to be an error in our ability to match things up
good_huc4_id<-paste('0',huc4_info$HUC4,sep='')
good_huc8_id<-paste('0',huc8_info$HUC8,sep='')
final_huc4<-huc4_info[,-1]
final_huc4$HUC4<-good_huc4_id
final_huc8<-huc8_info[,-1]
final_huc8$HUC8<-good_huc8_id

#Now merge together
all_huc4<-merge(huc4,final_huc4,by='HUC4')
all_huc8<-merge(huc8,final_huc8,by='HUC8')

#Take a look at the different information included in these shape files
plot(st_geometry(good_ill))
plot(all_huc4[,2],add=T)
#Or, just
plot(all_huc4[,2])
plot(all_huc4[,4])

#########################################################
#OK, let's start looking at ways to manipulate our data
#########################################################
huc4_interest<-all_huc4[2:3,]
huc4_reaches<-st_intersection(good_reaches,huc4_interest)

#Let's plot these to see what we're working with
plot(st_geometry(good_ill))
plot(st_geometry(huc4_interest),add=T)
plot(st_geometry(huc4_reaches),add=T,col='blue')

#Quick way to summarize variables of interest in data
area_interest<-ddply(huc4_reaches,~HUC4,summarise,mean_area=mean(Ws_Area))

#You can also use the following "pipeline" methods of organizing workflow and manipulating data
reach_areas<-huc4_reaches %>%
  select(Ws_Area) %>%
  arrange(Ws_Area) %>%
  slice(1:500) %>%
  plot(add=T,col='green')


#########################################################
#Lastly, let's look at how to write our data to file and keep it as spatial data!
#########################################################
#To write as a .csv file
huc4_reaches_df<-st_drop_geometry(huc4_reaches)
write.csv(huc4_reaches_df,'HUC4_Reaches_Info.csv')

#To write to shapefile
st_write(huc4_reaches,dsn='HUC4_Reaches_Info',layer='HUC4_Reaches_Info',driver='ESRI Shapefile')

st_write(huc4_reaches_df,dsn='HUC4_Reaches_Info',layer='HUC4_Reaches_Info',driver='ESRI Shapefile') #Doesn't work because there is no geometry column!












