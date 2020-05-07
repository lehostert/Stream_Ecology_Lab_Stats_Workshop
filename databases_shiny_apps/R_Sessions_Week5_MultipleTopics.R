##Note: You might have to install some of the following packages!

library(sf) #For working with spatial objectslibrary(ggplot2)
library(ggplot2) #The gold standard for visualizing data and preparing publication-quality figures
library(dplyr) #Another package that helps with data manipulation and cleanup
library(tidyr) #Newer package that helps streamline data manipulation
library(readxl) #This package helps import spreadsheet-formatted data, primarily from Excel
library(data.table) #Helps with data management

##################################################################################################################
#Topic 1: Interfacing with databases 
#Focus on using Access
##################################################################################################################
#Load topic-appropriate packages.
##Note: You might have to install some of the following packages!

library(dbplyr) #This package will help interface with databases by translating R code into database-specific variants
library(RODBC) #This package will help interface with Access databases.

#####First and foremost - I have not, for the life of me, been able to get this to work!
#I've probably missed some crucial step in setting up everything necessary
#So...I'll provide some basic code that, provided you have the necessary setup, will get you on your way to interfacing with Access.
#Using the following command should tell you what drivers you have available on your computer
odbcDataSources()

########If you have drivers available, the following may help you
#In order for this to work, you will likely need to switch to a 32-bit version of R Studio
#Go to "Tools" menu
#Go to "Global Options" menu
#Under the "General" tab, choose the newest 32-bit version of R

#Set patch to appropriate directory
db<-file.path("directory path")
driver<-odbcDriverConnect('Driver={Microsoft Access Driver (*.mdb, *.accdb)}') #If you're interested in interfacing with Access
channel<-odbcConnectAccess2007("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=directory to and including file") #More complete line to access specific file

#Additional information can be found at:
#https://db.rstudio.com/
#https://www.r-bloggers.com/getting-access-data-into-r/
#https://stackoverflow.com/questions/13070706/how-to-connect-r-with-access-database-in-64-bit-window
#Many other websites from a Google search


##################################################################################################################
#Topic 2: Writing Shiny apps
##################################################################################################################
#Load topic-appropriate packages.
##Note: You might have to install some of the following packages!
###See bookmark in UofI Box for complete tutorial information!

library(shiny) #Will allow for the development of Shiny apps
library(shinyjs) #Helper package

#Let's see an example of what a Shiny app looks like
runExample("01_hello")

#In short, there are 3 main parts to a Shiny script
#1) A user interface (UI) object. This will allow users to directly manipulate the layout and appearance of the app
#2) A server function. This part will tell the computer how to build and structure your app
#3) A shinyApp function. This part will combine the first 2 parts and create the actual Shiny app.

#Let's see how to structure a UI object
# Define UI for app that draws a histogram ----
ui <- fluidPage(
  # App title ----
  titlePanel("Hello Shiny!"),
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    # Sidebar panel for inputs ----
    sidebarPanel(
      # Input: Slider for the number of bins ----
      sliderInput(inputId = "bins",
                  label = "Number of bins:",
                  min = 1,
                  max = 50,
                  value = 30)
    ),
    # Main panel for displaying outputs ----
    mainPanel(
      # Output: Histogram ----
      plotOutput(outputId = "distPlot")
    )
  )
)

#Let's see how to structure a server function
# Define server logic required to draw a histogram ----
server <- function(input, output) {
  # Histogram of the Old Faithful Geyser Data ----
  # with requested number of bins
  # This expression that generates a histogram is wrapped in a call
  # to renderPlot to indicate that:
  #
  # 1. It is "reactive" and therefore should be automatically
  #    re-executed when inputs (input$bins) change
  # 2. Its output type is a plot
  output$distPlot <- renderPlot({
    x    <- faithful$waiting
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    hist(x, breaks = bins, col = "#75AADB", border = "white",
         xlab = "Waiting time to next eruption (in mins)",
         main = "Histogram of waiting times")
  })
  
}


#Finally, we run both the UI and server function to create the actual Shiny App
shinyApp(ui = ui, server = server)


#########################################################
#Let's look at some other Shiny apps
#########################################################
runExample("01_hello")      # a histogram
runExample("02_text")       # tables and data frames
runExample("03_reactivity") # a reactive expression
runExample("04_mpg")        # global variables
runExample("05_sliders")    # slider bars
runExample("06_tabsets")    # tabbed panels
runExample("07_widgets")    # help text and submit buttons
runExample("08_html")       # Shiny app built from HTML
runExample("09_upload")     # file upload wizard
runExample("10_download")   # file download wizard
runExample("11_timer")      # an automated timer


#########################################################
#Let's see if we can create our own crude Shiny app
#Let's allow for data to be entered in real time and produce updated information
#Might be useful if we're sampling in the field and want to get an idea of our data immediately after sampling
#########################################################

#
#####The following is what I've seen called the "global scope". It contains functions that will be essential to the Shiny App
#
#Let's first specify what information is absolutely essential to be entered (i.e., variables measured in field)
fields<-c("name","stream_id",'location_x','location_y','water_temp','water_flow','species_found','number_recorded')

#Establish that user must fill out these fields
#Otherwise, submission button won't be generated 
fieldsMandatory<-c("name","stream_id",'location_x','location_y','water_temp','water_flow','species_found','number_recorded')

#Use following function to define these pieces of information are essential
labelMandatory <- function(label) {
  tagList(
    label,
    span("*", class = "mandatory_star")
  )
}

#Color mandatory asterisks
appCSS <- ".mandatory_star { color: red; }"

#This will save and update fields each time new input is submitted
fieldsAll<-c("name","stream_id",'location_x','location_y','water_temp','water_flow','species_found','number_recorded')
responsesDir <- file.path("C:/Users/INHSadmin/Desktop/Lab Group_R Sessions/ShinyApp")
epochTime <- function() {
  as.integer(Sys.time())
}

#Define how time will be recorded during entry submission
humanTime <- function() format(Sys.time(), "%Y%m%d-%H%M%OS")

#Save input data to a file
saveData <- function(data) {
  fileName <- sprintf("%s_%s.csv",
                      humanTime(),
                      digest::digest(data))
  
  write.csv(x = data, file = file.path('C:/Users/INHSadmin/Desktop/Lab Group_R Sessions/ShinyApp'), #You would change this directory
            row.names = FALSE, quote = TRUE)
}

#Load all responses into a data.frame
loadData <- function() {
  files <- list.files(file.path(responsesDir), full.names = TRUE)
  data <- lapply(files, read.csv, stringsAsFactors = FALSE) 
  data <- rbindlist(data)
  data
}



#Generate my "trial" UI
trial_ui<-fluidPage(
  DT::dataTableOutput("responsesTable"),
  shinyjs::useShinyjs(),  
  shinyjs::inlineCSS(appCSS),
    column(8,
           titlePanel("Trial App for Entering Stream Data"),
           div(
             id = "form",
             
             textInput("name", labelMandatory("Observer Name")),
             textInput("stream_id", labelMandatory("Stream ID")),
             textInput('location_x',labelMandatory('Site Longitude')),
             textInput('location_y',labelMandatory('Site Latitude')),
             textInput('water_temp',labelMandatory('Water Temperature')),
             textInput('water_flow',labelMandatory('Water Rate of Flow')),
             textInput('species_found',labelMandatory('Species Observed')),
             textInput('number_recorded',labelMandatory('Number of Individuals')),
             actionButton("submit", "Submit", class = "btn-primary"),
             shinyjs::hidden(
               span(id = "submit_msg", "Submitting..."),
               div(id = "error",
                   div(br(), tags$b("Error: "), span(id = "error_msg"))
               )
             )
               ),
           shinyjs::hidden(
             div(
               id = "thankyou_msg",
               h3("Thanks, your response was submitted successfully!"),
               actionButton("submit_another", "Submit another entry?")
             )
            )
           )
          )


#Define my "trial" server
trial_server<-function(input, output, session){
  #Make sure manadatory fields are completed
  observe({
    mandatoryFilled <-
      vapply(fieldsMandatory,
             function(x) {
               !is.null(input[[x]]) && input[[x]] != ""
             },
             logical(1))
    mandatoryFilled <- all(mandatoryFilled)
    shinyjs::toggleState(id = "submit", condition = mandatoryFilled)
  })
  
  #Save data
  formData <- reactive({
    data <- sapply(fieldsAll, function(x) input[[x]])
    data <- c(data, timestamp = epochTime())
    data <- t(data)
    data
  })
  
#Actually save the form  
  saveData <- function(data) {
    fileName <- sprintf("%s_%s.csv",
                        humanTime(),
                        digest::digest(data))
    
    write.csv(x = data, file = file.path(responsesDir, fileName),
              row.names = FALSE, quote = TRUE)
  }
  
#Action to take when submit button is pressed
#Save data
#Reset form
#Show thank you message
  observeEvent(input$submit, {
    saveData(formData())
    shinyjs::reset("form")
    shinyjs::hide("form")
    shinyjs::show("thankyou_msg")
  })
  

#Once one form has been submitted, open another
observeEvent(input$submit_another, {
  shinyjs::show("form")
  shinyjs::hide("thankyou_msg")
}) 
  
#Show the responses in the admin table
output$responsesTable <- DT::renderDataTable(
  loadData(),
  rownames = FALSE,
  options = list(searching = FALSE, lengthChange = FALSE)
  )  
}

shinyApp(trial_ui,trial_server)


##################################################################################################################
#Topic 3: Multivariate analyses looking at ecological differences
##################################################################################################################
#Load topic-appropriate packages.
##Note: You might have to install some of the following packages!

library(reshape2) #This package helps manipulate data
library(FactoMineR) #THis package helps with PCA 
library(alphashape3d) #This package will aid in constructing and visualizing 3D characterizations of groups based on selected variables

#########################################################
#Bring in spatial data
#Reconcile with our mussel occurrence data
#########################################################
#Let's first get a list of all the shapefiles available in our working directory
spatial_data<-list.files('C:/Users/INHSadmin/Desktop/Lab Group_R Sessions',pattern='.shp',recursive=T,full.names = T)
reach_data<-list.files('C:/Users/INHSadmin/Desktop/Lab Group_R Sessions',pattern='.csv',recursive=T,full.names = T)

#Now, let's load our different shapefiles
reaches<-st_read(spatial_data[3])
illinois<-st_read(spatial_data[4])
good_ill<-st_transform(illinois,crs=st_crs(reaches)) #It's easy to specify projections from other spatial objects using the "st_crs" function

#Load reach-level predictor data
reach_info<-read.csv(reach_data[5],header=T)

#Let's first merge our reach data so all reaches are associated with relevant data
good_reaches<-merge(reaches,reach_info,by='COMID')

#Import mussel presence-absence data
setwd('C:/Users/INHSadmin/Desktop/Lab Group_R Sessions/Week 5 Materials')
mussel_data<-read.csv('Species_Presence.csv',header=T)

#Reconcile reach data with mussel occurrence
good_data<-merge(good_reaches,mussel_data,by='COMID')
final_data<-good_data[,-c(2,65)]

#########################################################
#Prep data for comparison
#########################################################
#Split data by species, add species name, partition into presence and absence data, re-combine data
al_data<-st_drop_geometry(final_data[,c(64,2:63)])
ac_data<-st_drop_geometry(final_data[,c(65,2:63)])
et_data<-st_drop_geometry(final_data[,c(66,2:63)])

#Add species name
al_data$Name<-rep('A_ligamentina',nrow(al_data))
colnames(al_data)[1]<-'Occurrence'
ac_data$Name<-rep('A_confragosus',nrow(ac_data))
colnames(ac_data)[1]<-'Occurrence'
et_data$Name<-rep('E_triquetra',nrow(et_data))
colnames(et_data)[1]<-'Occurrence'

#Subset presence data
al_present<-al_data[al_data$Occurrence == 1,]
ac_present<-ac_data[ac_data$Occurrence == 1,]
et_present<-et_data[et_data$Occurrence == 1,]
presence_data<-rbind(al_present,ac_present,et_present)

#Subset presence data
al_absent<-al_data[al_data$Occurrence == 0,]
ac_absent<-ac_data[ac_data$Occurrence == 0,]
et_absent<-et_data[et_data$Occurrence == 0,]
absence_data<-rbind(al_absent,ac_absent,et_absent)

#########################################################
#Compare data between groups
#Approach 1: Principal component analysis
#########################################################
#Run PCA
#Lots of different options for how to run PCA, each function takes different arguments
#princomp
#prcomp
pca_presence<-PCA(presence_data[,-c(1,64)])
pca_absence<-PCA(absence_data[,-c(1,64)])

#Check structure and summary of PCA objects
#QUickly visualize
str(pca_presence)
summary(pca_presence)
plot(pca_presence)

#Prep for plotting in ggplot
#Extract pc scores for first two component and add to our initial dataframe
presence_data$pc1 <- pca_presence$ind$coord[, 1] #Index the first column
presence_data$pc2 <- pca_presence$ind$coord[, 2]  #Index the second column
presence_data$pc3 <- pca_presence$ind$coord[,3] #Index the third column, this will be used for later analyses

absence_data$pc1 <- pca_absence$ind$coord[, 1] #Index the first column
absence_data$pc2 <- pca_absence$ind$coord[, 2]  #Index the second column
absence_data$pc3 <- pca_absence$ind$coord[,3] #Index the third column, this will be used for later analyses

#Also, extract data on variable contribution to each axis
pca_presence_vars <- pca_presence$var$coord %>% data.frame
pca_presence_vars$vars <- rownames(pca_presence_vars)
pca_presence_vars.m <- melt(pca_presence_vars, id.vars = "vars")

pca_absence_vars <- pca_absence$var$coord %>% data.frame
pca_absence_vars$vars <- rownames(pca_absence_vars)
pca_absence_vars.m <- melt(pca_absence_vars, id.vars = "vars")


#Start to visualize results
#The following will help visualize variable loadings (i.e., contributions to principal component axes)
circleFun <- function(center = c(0,0),diameter = 1, npoints = 100){
  r = diameter / 2
  tt <- seq(0,2*pi,length.out = npoints)
  xx <- center[1] + r * cos(tt)
  yy <- center[2] + r * sin(tt)
  return(data.frame(x = xx, y = yy))
}
circ <- circleFun(c(0,0),2,npoints = 500)

#Plot PCA results of PC1 and PC2 to see how groups separate (if at all)
ggplot(data = presence_data, aes(x = pc1, y = pc2,color=Name)) +
  geom_point()+
  stat_ellipse(geom="polygon", aes(fill = Name),alpha = 0.2,show.legend = FALSE,level = 0.95)

ggplot(data = absence_data, aes(x = pc1, y = pc2,color=Name)) +
  geom_point()+
  stat_ellipse(geom="polygon", aes(fill = Name),alpha = 0.2,show.legend = FALSE,level = 0.95)

#########################################################
#Compare data between groups
#Approach 2: Alpha hull shapes
#########################################################
#So, there is substantial overlap between these 3 species in 2-dimensional environmental space
#Do these species separate if we add a third dimension (i.e., PC3)? 
#Let's generate some alphahulls!
#Separate by species
coord_data<-presence_data[,64:67]
al_3d<-coord_data[coord_data$Name=='A_ligamentina',]
ac_3d<-coord_data[coord_data$Name=='A_confragosus',]
et_3d<-coord_data[coord_data$Name=='E_triquetra',]

#Get just coords
al_coords<-as.matrix(al_3d[,2:4])
ac_coords<-as.matrix(ac_3d[,2:4])
et_coords<-as.matrix(et_3d[,2:4])

#Generate alphashapes
al_alpha_p<-ashape3d(al_coords,alpha=5)
ac_alpha_p<-ashape3d(ac_coords,alpha=5)
et_alpha_p<-ashape3d(et_coords,alpha=5)

#Plot them!
plot(ac_alpha_p,col='red',transparency=.5)
plot(et_alpha_p,col='blue',add=T,clear=F)

#####
#DO the same with the absences
######
coord_data<-absence_data[,64:67]
al_3d<-coord_data[coord_data$Name=='A_ligamentina',]
ac_3d<-coord_data[coord_data$Name=='A_confragosus',]
et_3d<-coord_data[coord_data$Name=='E_triquetra',]

#Get just coords
al_coords<-as.matrix(al_3d[,2:4])
ac_coords<-as.matrix(ac_3d[,2:4])
et_coords<-as.matrix(et_3d[,2:4])

#Generate alphashapes
al_alpha_a<-ashape3d(al_coords,alpha=5)
ac_alpha_a<-ashape3d(ac_coords,alpha=5)
et_alpha_a<-ashape3d(et_coords,alpha=5)

#Plot them!
plot(al_alpha_a,col='red',transparency=.5)
plot(et_alpha_a,col='blue',add=T,clear=F)

#Compare presence and absence
plot(al_alpha_p,col='red',transparency=.5)
plot(al_alpha_a,col='blue',add=T,clear=F)
