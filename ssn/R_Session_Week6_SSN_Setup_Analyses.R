#Note: You will likely have to install packages "SSN", "stars", and "openSTARS" for this week's R session
#Note: As of right now, only binomial or Poisson models are supported by SSN

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
library(rgeos)

#########################################################
#Let's first bring in our data for the stream network of interest
#Make sure all data are in same projection to prepare for GRASS
#Examine data to make sure it's ready
#########################################################
#Get data lists
setwd('C:/Users/INHSadmin/Desktop/Lab Group_R Sessions/Week 6 Materials/GRASS Ready')
raster_data<-list.files('C:/Users/INHSadmin/Desktop/Lab Group_R Sessions/Week 6 Materials/GRASS Ready',pattern='.tif',recursive=T,full.names = T)
spatial_data<-list.files('C:/Users/INHSadmin/Desktop/Lab Group_R Sessions/Week 6 Materials/GRASS Ready',pattern='.shp',recursive=T,full.names = T)
reach_data<-list.files('C:/Users/INHSadmin/Desktop/Lab Group_R Sessions/Week 6 Materials/GRASS Ready',pattern='.csv',recursive=T,full.names = T)

#Bring in raster data
huc4_elevation<-raster(raster_data[1])
network_elevation<-raster(raster_data[2])
high_landuse<-raster(raster_data[3])
low_landuse<-raster(raster_data[4])
pesticides<-raster(raster_data[5])

#Bring in HUC4, HUC8, reach, and points 'sf' objects
huc4<-st_read(spatial_data[1])
reaches<-st_read(spatial_data[2])
huc8<-st_read(spatial_data[3])
illinois<-st_read(spatial_data[4])
pred_points<-st_read(spatial_data[5])
network_interest<-st_read(spatial_data[6])
sampled_points<-st_read(spatial_data[7])

#Transform Illiois, network of interest, and sites so they match projections
illinois<-st_transform(illinois,crs=st_crs(reaches))
check_network<-st_transform(network_interest,crs=st_crs(reaches))
check_sample<-st_transform(sampled_points,crs=st_crs(reaches))
check_preds<-st_transform(pred_points,crs=st_crs(reaches))

#Let's see what we're working with
plot(st_geometry(illinois))
plot(st_geometry(huc4),add=T,col='red')
plot(st_geometry(huc8),add=T,col='pink')
plot(st_geometry(reaches),add=T,col='green')
plot(st_geometry(check_network),add=T,col='blue')

#Closer look at sampling and prediction sites
plot(st_geometry(check_network),col='blue')
plot(st_geometry(check_sample),add=T,col='red',pch=19)
plot(st_geometry(check_preds),add=T,col='black',pch=19)

#Check out raster data with our 'sf' objects
plot(huc4_elevation)
plot(network_elevation)
plot(high_landuse)
plot(low_landuse)
plot(pesticides)
plot(st_geometry(check_network),add=T,col='blue')

#Re-project all 'sf' objects
ready_huc4<-st_transform(huc4,crs=crs(network_elevation))
ready_huc8<-st_transform(huc8,crs=crs(network_elevation))
ready_illinois<-st_transform(illinois,crs=crs(network_elevation))
ready_network<-st_transform(network_interest,crs=crs(network_elevation))

#Plot to make sure we're in good shape
plot(st_geometry(ready_huc4))
plot(st_geometry(ready_huc8),add=T)
plot(network_elevation,add=T)
plot(pesticides,add=T)
plot(st_geometry(ready_network),add=T)







#########################################################
#Initialize external GRASS session
#Set up appropriately
#########################################################
initGRASS(gisBase = "C:/Program Files/GRASS GIS 7.6", 
          home = tempdir(),
          location = "test_openSTARS",
          remove_GISRC = T,
          override=TRUE)

############
#Begin setting up GRASS environment by providing cropped DEM raster
############
setup_grass_environment(dem = 'C:/Users/INHSadmin/Desktop/Lab Group_R Sessions/Week 6 Materials/GRASS Ready/Masked_Elevation.tif')
gmeta() #This will check your meta working environment

############
#Import elevation raster, stream network, sampling sites, and prediction sites
#This step is very finicky!
#All must be in the same projection
#All stream and site data, if using 'sf' objects, need to be just the geometry!
#Can also include predictor rasters in this step, as well as later in the process! I have not included predictors here.
############
pred_path<-c(system.file('C:/Users/INHSadmin/Desktop/Lab Group_R Sessions/Week 6 Materials/GRASS Ready/Med_High_LandUse.tif'),
             system.file('C:/Users/INHSadmin/Desktop/Lab Group_R Sessions/Week 6 Materials/GRASS Ready/Open_Low_LandUse.tif'),
             system.file('C:/Users/INHSadmin/Desktop/Lab Group_R Sessions/Week 6 Materials/GRASS Ready/Pesticides.tif'))
ready_data<-import_data(dem = 'C:/Users/INHSadmin/Desktop/Lab Group_R Sessions/Week 6 Materials/GRASS Ready/Masked_Elevation.tif',
            streams="C:/Users/INHSadmin/Desktop/Lab Group_R Sessions/Week 6 Materials/GRASS Ready/Ready_Streams/Ready_Streams.shp",
            sites='C:/Users/INHSadmin/Desktop/Lab Group_R Sessions/Week 6 Materials/GRASS Ready/Sampling_Sites/Sampling_Sites.shp',
            pred_sites='C:/Users/INHSadmin/Desktop/Lab Group_R Sessions/Week 6 Materials/GRASS Ready/Prediction_Sites/Prediction_Sites.shp')

############
#Bring in our input data so we can visualize it
############
use_sp() #Use for "Spatial" objects
#use_sf() #Use if you want to import as 'sf' objects. This seems a bit buggy/glitchy when trying to plot

dem <- readRAST("dem", ignore.stderr = TRUE) #Read in elevation as spatial raster grid
sample_sites <- readVECT("sites_o", ignore.stderr = TRUE) #Also bring in our sampled sites
pred_sites<-readVECT('Prediction_Sites_o',ignore.stderr = TRUE) #Bringin prediction sites, name of file plus "_o" at end
our_network <-readVECT('streams_o',ignore.stderr = TRUE) #This will bring in just our streams of interest!

#Plot everything
plot(dem,col = terrain.colors(20))
plot(sample_sites,add=T,col='red',pch=19,cex = 1.25)
plot(pred_sites,add=T,col='black',pch=19,cex = 1.25)
plot(our_network,add=T,col='green')
#This should look familiar!


############
#Derive streams
#This will 'burn-in' your streams into the elevation raster, associate stream network with topographic variables
#Can be time consuming! This is especially the case if you have a large raster that hasn't been cropped/trimmed to study region
############
derive_streams(burn=0,condition=TRUE,mem=TRUE,clean=TRUE)
all_streams <- readVECT("streams_v", ignore.stderr = TRUE) #This will bring in all streams burnt into DEM!

#Let's see our streams data
head(all_streams@data)

#Plot DEM and streams
#Notice we've picked up more streams, especially smaller order streams!
#Good reason to crop/trim DEM layer before this process!
plot(dem,col = terrain.colors(20))
plot(all_streams,add=T)
plot(our_network,add=T,col='blue')

#Check to make sure all streams are joined/have junctions
check_compl_junctions()
correct_compl_junctions()


############
#Prepare our edges-this can also be time consuming!!!

####Edges now holds the derived network plus attributes needed for the .ssn object
#network identifier (netID)
#reach identifier (rid)
#stream segment length (length)
#distance from the source (sourceDist)
#upstream distance, i.e. distance from the outlet of the network to the start (upstream node) of the stream segment (upDist)
#total catchment area (H2OArea)
#reach contributing area (rcaArea)
############
calc_edges()

#Import data and check it
edges <- readVECT("edges", ignore.stderr = TRUE)
head(edges@data)


############
#Prepare our sampling sites
####Sampling sites now holds the derived network plus attributes needed for the .ssn object

#point identifier (pid)
#location identifier (locID)
#network identifier (netID)
#reach identifier of the edge segment the point lies on (rid)
#upstream distance (upDist), i.e. the distance to the network outlet calculated using r.stream.distance.
#distance ratio, i.e. the ratio of the distance from the outflow of the edge to the point along the edge and the total length of the edge segment (distRatio).
############
calc_sites(pred_sites='Prediction_Sites')

#Import data and check it
sites <- readVECT("sites", ignore.stderr = TRUE)
pred_sites<- readVECT('Prediction_Sites',ignore.stderr = T)
head(sites@data)
head(pred_sites@data)

############
#Select prediction sites
#Does not necessarily have to happen if you have prediction sites chosen
####Following information is included

#point identifier (pid)
#location identifier (locID)
#network identifier (netID)
#reach identifier of the edge segment the point lies on (rid)
#upstream distance (upDist), i.e. the distance to the network outlet calculated using r.stream.distance.
#distance ratio, i.e. the ratio of the distance from the outflow of the edge to the point along the edge and the total length of the edge segment (distRatio).############
############
#calc_prediction_sites(predictions='Prediction_Sites_o')

#Import data and check it
#pred_sites <- readVECT('Prediction_Sites_o', ignore.stderr = TRUE)
#head(pred_sites@data)


############
#Let's visualize everything!
############
dem <- readRAST("dem", ignore.stderr = TRUE)
sites <- readVECT("sites", ignore.stderr = TRUE)
pred_sites <- readVECT('Prediction_Sites', ignore.stderr = TRUE)
edges <- readVECT("edges", ignore.stderr = TRUE)
plot(dem, col = terrain.colors(20))
lines(edges, col = "blue")
points(sites, pch = 21, cex=1.25, bg = "red")
points(pred_sites, pch = 21, cex=1.25, bg = "black")
legend(x = par("usr")[1]*1.002, y = par("usr")[3]*1.01, pt.bg = c("red","blue"), pch = 21, legend = c("(snapped) observation sites","prediction sites"))


############
#Prepare additional predictors and attributes for edges, as well as sampled and prediction sites
############
#The following will run
#calculate slope from DEM as an example attribute from the DEMs
execGRASS("r.slope.aspect", flags = c("overwrite","quiet"),
          parameters = list(
            elevation = "dem",
            slope = "slope"
          ))

#Now, let's try to import our predictor rasters
execGRASS("r.import", flags = c("overwrite","quiet"),
          parameters = list(
            input = 'C:/Users/INHSadmin/Desktop/Lab Group_R Sessions/Week 6 Materials/GRASS Ready/Med_High_LandUse.tif',
            output = 'mhland'))
execGRASS("r.import", flags = c("overwrite","quiet"),
          parameters = list(
            input = 'C:/Users/INHSadmin/Desktop/Lab Group_R Sessions/Week 6 Materials/GRASS Ready/Open_Low_LandUse.tif',
            output = 'olland'))
execGRASS("r.import", flags = c("overwrite","quiet"),
          parameters = list(
            input = 'C:/Users/INHSadmin/Desktop/Lab Group_R Sessions/Week 6 Materials/GRASS Ready/Pesticides.tif',
            output = 'pest'))

high_land <-readRAST('mhland',ignore.stderr = T)
low_land <- readRAST("olland", ignore.stderr = TRUE)
pest_rast<- readRAST('pest',ignore.stderr = T)


#Run for edges first. This works!
calc_attributes_edges(input_raster = c("slope","mhland","olland","pest"), stat_rast = c("max","percent","percent",'max'), attr_name_rast = c("maxSlo","mhland","olland","pest"),round_dig=2)

#Make sure edge table has updated
edges <- readVECT("edges", ignore.stderr = TRUE)
head(edges@data)
#For each attribute, two columns are appended: 
#one giving the attribute for the rca of the edge ("attribute_name_e")
#one for the attribute of the total catchment of the edge ("attribute_name_c").


#Now, calculate these predictors for sites
#Need to run derive_streams(),calc_edges(),calc_sites() or calc_prediction_sites() and calc_attributes_edges() before 
#Two options
?calc_attributes_sites_approx
?calc_attributes_sites_exact

#Let's use the "exact" option because we've snapped these sites to stream
#This can be time consuming, but it will show a progress bar!
calc_attributes_sites_exact(sites_map = "sites",
                            input_raster=c("slope","mhland","olland","pest"),
                            attr_name_rast=c("maxSlo","mhland","olland","pest"),
                            stat_rast=c("max","percent","percent",'max'))
calc_attributes_sites_exact(sites_map = "Prediction_Sites",
                            input_raster=c("slope","mhland","olland","pest"),
                            attr_name_rast=c("maxSlo","mhland","olland","pest"),
                            stat_rast=c("max","percent","percent",'max'))


#Let's make sure our sampled and prediction sites now have these data associated with them!
sites <- readVECT("sites", ignore.stderr = TRUE)
pred_sites <- readVECT('Prediction_Sites', ignore.stderr = TRUE)
head(sites@data)
head(pred_sites@data)


############
#Export entire object and save!
############
ssn_dir <- file.path('C:/Users/INHSadmin/Desktop/Lab Group_R Sessions/Week 6 Materials', 'New_SSN.ssn')
export_ssn(ssn_dir,predictions='Prediction_Sites', delete_directory = TRUE)
list.files(ssn_dir)


############
#Import object and visualize!
############
#Import entire object
ssn_obj <- importSSN('C:/Users/INHSadmin/Desktop/Lab Group_R Sessions/Week 6 Materials/New_SSN.ssn',o.write=T)

#Import prediction sites
pred_points<-importPredpts(target=ssn_obj,predpts='Prediction_Sites', obj.type='ssn')

#Check out what this all entails
names(ssn_obj)
str(ssn_obj@data)
str(ssn_obj@obspoints)
str(ssn_obj@predpoints)

#Initial plot
plot(ssn_obj)


