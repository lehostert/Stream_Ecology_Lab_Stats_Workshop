##Note: You might have to install some of the following packages!
library(tidyverse)
library(sf) #For working with spatial objectslibrary(ggplot2)
library(dismo) #Just for some useful functions like "kfold"
# library(ggplot2) #The gold standard for visualizing data and preparing publication-quality figures
# library(dplyr) #Another package that helps with data manipulation and cleanup
# library(tidyr) #Newer package that helps streamline data manipulation
library(readxl) #This package helps import spreadsheet-formatted data, primarily from Excel
library(fitdistrplus) #This package helps assess distribution fit - very helpful when exploring and fitting statistical models
library(lmtest) #This package will help evaluate model fit and discern between competing models
# library(broom) #Will help clean up and interpret model summaries
library(margins) #This package can help with interpreting model fit and predictor contribution
library(visreg) #This package helps evaluate how response variable probabilities change relative to independent variable values
library(lme4) #This package can help us fit generalized linear mixed models
#Also, take a look at package "pscl" to help with fitting zero-inflated Poisson data

#########################################################
#Let's first take a quick look at some of the utility of the fitdistrplus and lmtest packages
#Quick diagnostic tool to get an idea of what distribution you'll want to use during model fitting
#########################################################
#Generate some random data
random_pois<-rpois(1000,5)
random_normal<-rnorm(1000,50)
random_exp<-rexp(1000,rate=.5)

#Let's visualize probability distribution fit to each of these variables
dist_pois<-descdist(random_pois,discrete=T,boot=1000,obs.col = "darkblue", obs.pch = 16, boot.col = "orange")
dist_norm<-descdist(random_normal,discrete=T,boot=1000,obs.col = "darkblue", obs.pch = 16, boot.col = "orange")
dist_exp<-descdist(random_exp,discrete=F,boot=1000,obs.col = "darkblue", obs.pch = 16, boot.col = "orange")

#Actually fit a distribution to our data
fit_pois<-fitdist(random_pois,'pois')
bad_pois<-fitdist(random_pois,'nbinom')

fit_exp<-fitdist(random_exp,'exp')
bad_exp<-fitdist(random_exp,'norm')

#Get summary of potential fits
summary(fit_exp)
summary(bad_exp)

#Examine diagnostic plots
plot(fit_exp)
#cdfcomp(fit_exp, addlegend=FALSE) #Empirical cumulative distribution against fitted distribution functions
#denscomp(fit_exp, addlegend=FALSE) #Histogram against fitted density functions
#ppcomp(fit_exp, addlegend=FALSE) #Theoretical quantiles against empirical ones
#qqcomp(fit_exp, addlegend=FALSE) #Theoretical probabilities against empirical ones

plot(bad_exp)
#cdfcomp(bad_exp, addlegend=FALSE) #Empirical cumulative distribution against fitted distribution functions
#denscomp(bad_exp, addlegend=FALSE) #Histogram against fitted density functions
#ppcomp(bad_exp, addlegend=FALSE) #Theoretical quantiles against empirical ones
#qqcomp(bad_exp, addlegend=FALSE) #Theoretical probabilities against empirical ones

#Compare candidate fits using goodness of fit testing
compare_fit_exp<-gofstat(list(fit_exp,bad_exp))
compare_fit_pois<-gofstat(list(fit_pois,bad_pois))




#########################################################
#Let's switch gears to working with some actual data concerning mussel occurrence
#Let's set our working directory to where our materials have been saved
#########################################################
# setwd('C:/Users/INHSadmin/Desktop/Lab Group_R Sessions') #This should be changed to your appropriate directory
# Using an R project this will automatically setwd.
#########################################################
#Bring in all Excel data
#########################################################
#There are several ways to import spreadsheet-based data
#The appropriate function depends on the spreadsheet format
reach_info<-read.csv('r-and-gis/Illinois_Reach_Info.csv',header=T) 
reach_info<-reach_info[,-1]
mussel_records<-read.csv('maxent-modeling/Example_Mussel_Records.csv',header=T)
mussel_records<-mussel_records[,-1]

#########################################################
#Bring in spatial data
#Reconcile with our mussel occurrence data
#########################################################

#Let's first get a list of all the shapefiles available in our working directory
spatial_data<-list.files('r-and-gis',pattern='.shp',recursive=T,full.names = T)

#Now, let's load our different shapefiles
reaches<-st_read(spatial_data[3])
illinois<-st_read(spatial_data[4])
good_ill<-st_transform(illinois,crs=st_crs(reaches)) #It's easy to specify projections from other spatial objects using the "st_crs" function

#Let's first merge our reach data so all reaches are associated with relevant data
good_reaches<-merge(reaches,reach_info,by='COMID')

#Prep reach data to be reconciled with species occurrences
reach_presences<-good_reaches[good_reaches$COMID %in% mussel_records$COMID,]
`%nin%` = Negate(`%in%`)
reach_absences<-good_reaches[good_reaches$COMID %nin% reach_presences$COMID,]
reach_presences$Occurrence<-rep(1,nrow(reach_presences))
reach_absences$Occurrence<-rep(0,nrow(reach_absences))
final_occurrence<-rbind(reach_presences,reach_absences)


#########################################################
#Start initial exploration of model fitting. 
#Let's pose a question: "Does catchment and watershed geology affect A. ligamentina occurrence?"
#########################################################
#Subset out data to our relevant predictor variables
#See the Week 3 accompanying material for geology predictor definitions
model_data<-final_occurrence[,c(18:22,42,48,51:55,64)]
model_df<-st_drop_geometry(model_data)

#We can proceed with our predictors as is and we can also scale these predictors 
#Results in predctors that have a mean of zero ("centering") and standard deviation of one ("scaling")
#Scaling can make it easier to compare effect sizes
scaled_df<-as.data.frame(scale(model_df[,1:12], center = TRUE, scale = TRUE))
scaled_df$Occurrence<-model_df[,13]

#Let's take a quick look at our data
glimpse(model_df)
glimpse(scaled_df)

#Finalize model data by removing observations without complete predictor information
#In this case, we should be OK due to past processing

#However, part of our question is whether we can use geology variables to predict A. ligamentina occurrence

#Let's start by fitting a full model with our scaled and unscaled data
par(mfrow=c(2,2)) #This will change plot window setup so all model-fitting plots can be displayed simultaneously
full_model_unscaled <- glm(Occurrence~., data = model_df, family = "binomial")
plot(full_model_unscaled) #Always make sure you plot your models to evaluate their fit!
summary(full_model_unscaled) #Look at predictor estimates, error, P value, and model fit (AIC)

full_model_scaled <- glm(Occurrence~., data = scaled_df, family = "binomial")
plot(full_model_scaled) 
summary(full_model_scaled) 

#Other model components to look at
tidy(full_model_unscaled, exponentiate = TRUE, conf.level = 0.95) #odds ratio
summary(margins(full_model_unscaled)) #Look at predictor marginal effects, describe predictor effect on response variable (i.e., "Occurrence")
par(mfrow=c(1,1))
tidy()
plot(margins(full_model_unscaled)) #Plot marginal effects

#Check out package visreg capabilities by considering variable "Ct_RckD"
visreg(full_model_scaled, "Ct_RckD", scale="response", rug=2, xlab="Catchment depth to bedrock (cm)",ylab="P(Occurrence)")

#Approach 1 - Evaluate model summary, remove insignificant terms from next model
model2_unscaled<-glm(Occurrence~Ct_PctA+Ws_PctC+Ct_RckD+PctGlTC+PctGlLC+PctGlLW+PctElWs,data=model_df,family='binomial')
par(mfrow=c(2,2))
plot(model2_unscaled)
summary(model2_unscaled)
lrtest(full_model_unscaled,model2_unscaled) #Compare fitted models using log-likelihood and Chi-square test 
#to evaluate if one model fits better relative to degrees of freedom

#Approach 2 - Use stepwise selection to fit model. Not the best way due to various issues (e.g.predictor order of removal)
step_back_model_unscaled<-step(full_model_unscaled,direction='backward')
step_both_model_unscaled<-step(full_model_unscaled,direction='both')
par(mfrow=c(2,2))
plot(step_back_model_unscaled)
summary(step_back_model_unscaled)
plot(step_both_model_unscaled)
summary(step_both_model_unscaled)
lrtest(full_model_unscaled,step_back_model_unscaled,step_both_model_unscaled,model2_unscaled)




#########################################################
#Let's switch to and consider mixed models where we can incorporate fixed and random effects
#We'll use a preloaded dataset for this purpose
#Preface - generating mixed models can be difficult, and I have little experience using them in my own work
#Much thought should be spent on research questions and experimental design to a priori identify what fixed and random variables will be measured
#There is also a HUGE amount of literature available on mixed models 
#########################################################
par(mfrow=c(1,1))
#We'll use a dataset that looks at individual Extroverted tendencies 
#We'll attempt to predict Extroversion using fixed effects for the interval scaled predictor Openness to new experiences (open), the
#interval scaled predictor Agreeableness (agree), the interval scaled predictor Social engagement (social), and the
#nominal scaled predictor Class (class); as well as the random (nested) effect of Class within School (school)
lmm_data <- read.table("http://bayes.acs.unt.edu:8083/BayesContent/class/Jon/R_SC/Module9/lmm.data.txt",
                       header=TRUE, sep=",", na.strings="NA", dec=".", strip.white=TRUE)
glimpse(lmm_data)
summary(lmm_data)
#The data contains 1200 cases evenly distributed among 24 nested groups (4 classes within 6 schools).

#Evaluate response variable data to see what distribution to use to fit model
#Normal distribution is easiest, becomes increasingly difficult with other probability distributions
hist(lmm_data$extro)  #Close to normal!
#Looking at our summary of this dataset, it looks like all predictors are on relatively the same scale. Scaling likely not necessary

#Are there any effects of class or school on Extroversion? Let's look at some boxplots
par(mfrow=c(1,2))
boxplot(extro~class,data=lmm_data)
boxplot(extro~school,data=lmm_data)
#Maybe a slight difference among classes?
#Definite difference among schools!

#Let's fit an initial linear model that includes predictors and just "class"
model1_lm<- lm(extro ~ open + agree + social +class, data = lmm_data)
summary(model1_lm)
par(mfrow=c(2,2))
plot(model1_lm)

#Let's fit second linear model looking at Extroversion, allow for differences among classes and schools
model2_lm<- lm(extro ~ open + agree + social+ class+school, data = lmm_data)
summary(model2_lm)
par(mfrow=c(2,2))
plot(model2_lm)

#Compare model fit between our two models
AIC(model1_lm,model2_lm)
lrtest(model1_lm,model2_lm) 
#Could class and school have interactive effect?

model3_lm<- lm(extro ~ open + agree + social+ class*school, data = lmm_data)
summary(model3_lm)
par(mfrow=c(2,2))
plot(model3_lm)
lrtest(model1_lm,model2_lm,model3_lm)
#Evidence of an interactive effect

#####Now, let's generate some mixed models that will allow for different estimates for each class and school!
mixed_model1<- lmer(extro ~ open + agree + social + (1|school), data=lmm_data) #Just school is considered a random effect here
summary(mixed_model1)

#School and class are considered random effects here
mixed_model2<- lmer(extro ~ open + agree + social + (1|school) + (1|class), data=lmm_data) 
summary(mixed_model2)

#School and class are considered random effects here, but class is also nested within school
mixed_model3<-lmer(extro ~ open + agree + social + (1|school/class), data=lmm_data) 
summary(mixed_model3)


