library(sf) #For working with spatial objects
library(ggplot2) #The gold standard for visualizing data and preparing publication-quality figures
library(tidyr) #Newer package that helps streamline data manipulation
library(spdep) #This package is very versatile for geospatial analyses and data manipulation
library(readxl) #This package helps import spreadsheet-formatted data, primarily from Excel
library(dplyr) #Another package that helps with data manipulation and cleanup
library(plyr) #Another package that helps with data manipulation and cleanup
library(SSN) #This package will facilitate our spatial regression analyses in stream networks 
library(stars) #This package will let us manipulate spatial objects into the proper format so they are compatible with the SSN package
library(rgrass7) #This packages will let us use GRASS
library(openSTARS) #This package will help us prepare spatial objects into the proper format so they are compatible with the SSN package
library(raster) #Deal with raster data
library(fasterize)

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
good_illinois<-st_transform(illinois,crs=st_crs(reaches))

#Bring in reach predictor information, reconcile
setwd('C:/Users/INHSadmin/Desktop/Lab Group_R Sessions')
reach_info<-read.csv('Illinois_Reach_Info.csv',header=T)
reaches<-merge(reaches,reach_info,by='COMID',all.x=T)

#########################################################
#OK, find target area
#########################################################
huc4_interest<-huc4[2,]
good_huc8<-huc8[7,]

#Visualize
plot(st_geometry(good_illinois))
plot(st_geometry(huc4_interest),add=T)
plot(st_geometry(good_huc8),add=T)

#setwd('C:/Users/INHSadmin/Desktop/Lab Group_R SessionsWeek 6 Materials/Spatial Materials')
#st_write(good_huc8,dsn='HUC8_Interest',layer='HUC8_Interest',driver='ESRI Shapefile')
network_interest<-st_intersection(reaches,good_huc8)
#st_write(network_interest,dsn='Stream_Network_Interest',layer='Stream_Network_Interest',driver='ESRI Shapefile')

plot(st_geometry(network_interest),add=T,col='blue')


#########################################################
#Bring in elevation raster
#########################################################
elevation_data<-list.files('C:/Users/INHSadmin/Desktop/Lab Group_R Sessions',pattern='.tif',recursive=T,full.names = T)
illinois_elevation<-raster(elevation_data[6])

#Transform HUC4 and stream network to same projection
raster_huc4<-st_transform(huc4_interest,crs=crs(illinois_elevation))
raster_huc8<-st_transform(good_huc8,crs=crs(illinois_elevation))
raster_network<-st_transform(network_interest,crs=crs(illinois_elevation))

#Crop elevation raster to appropriate region
cropped_elevation<-crop(illinois_elevation,raster_huc8)
#setwd('C:/Users/INHSadmin/Desktop/Lab Group_R Sessions/Week 6 Materials/Spatial Materials')
#writeRaster(cropped_elevation,filename='HUC8_Elevation',format='GTiff')

#VIsualize everything
plot(cropped_elevation)
plot(st_geometry(raster_huc8),add=T)
plot(st_geometry(raster_network),add=T)


#########################################################
#Let's start selecting our sampling sites
#########################################################
#Need to convert raster network to points so we can sample specific locations
#Select sampling sites
network_points<-st_cast(raster_network,"POINT")

#Now, randomly select sampling sites
sample_reaches<-network_points[network_points$Order<=4,]
final_samples<-sample_n(sample_reaches,25)
pred_reaches<-network_points[network_points$Order>=5,]
final_preds<-sample_n(pred_reaches,25)

final_sample_points<-st_cast(final_samples,"POINT")
final_pred_points<-st_cast(final_preds,'POINT')
#setwd('C:/Users/INHSadmin/Desktop/Lab Group_R Sessions/Week 6 Materials')
#st_write(final_samples,dsn='Sampled_Points',layer='Sampled_Points',driver='ESRI Shapefile')
#st_write(final_preds,dsn='Prediction_Points',layer='Prediction_Points',driver='ESRI Shapefile')

#Add these to the plot
plot(st_geometry(final_sample_points),add=T,col='red',pch=19)
plot(st_geometry(final_pred_points),add=T,col='blue',pch=19)


#########################################################
#Let's first bring in our data for the stream network of interest
#Further chop down our elevation raster so it is directly around the network of interest
#Generate some rasters from our predictor data
#Make sure all data are in same projection to prepare for GRASS
#Examine data to make sure it's ready
#########################################################
#Add a buffer around our stream network
#This will help us chop down the elevation raster
buffer_network<-st_buffer(raster_network,dist=5000,
                          nQuadSegs = 30,
                          endCapStyle = "ROUND",
                          joinStyle = "ROUND",
                          mitreLimit = 1,
                          singleSide = FALSE)
masked_elevation<-mask(cropped_elevation,buffer_network)

#Check that this worked by visualizing
plot(masked_elevation)
plot(st_geometry(raster_network),add=T,col='blue')


#Now, let's create rasters of some network attributes
network_pesticide<-buffer_network[,31]
network_open<-buffer_network[,57]
network_high<-buffer_network[,58]

#Use the masked elevation raster as a template for the spatial scale and resolution
#We just need to specifiy what values to transfer over to this template
#You can use functions (e.g., mean, median, mode) when transfering values
pesticide_raster<-fasterize(network_pesticide,masked_elevation,field="Ct_Ps97")
open_land<-fasterize(network_open,masked_elevation,field="PLLUC10")
high_land<-fasterize(network_high,masked_elevation,field="PHLUC10")

#Make sure these look reasonable
plot(pesticide_raster)
plot(open_land)
plot(high_land)

#Write these predictor rasters to file!
#setwd('C:/Users/INHSadmin/Desktop/Lab Group_R Sessions/Week 6 Materials/GRASS Ready')
#writeRaster(pesticide_raster,filename='Pesticides',format='GTiff')
#writeRaster(open_land,filename='Open_Low_LandUse',format='GTiff')
#writeRaster(high_land,filename='Med_High_LandUse',format='GTiff')
