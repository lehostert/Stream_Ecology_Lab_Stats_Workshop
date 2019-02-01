library(tidyverse)
library(vegan)
library(indicspecies)

data(varespec)
data(varechem)

Indic_Species <- read.csv("~/Documents/Github/Stats_Workshop_SEL/Multivariate_Analysis/indicator_species_data.csv", header = T, na = ".", row.names = 1)
perm <- read.csv("~/Documents/Github/Stats_Workshop_SEL/Multivariate_Analysis/PermFactors.csv", header = T, na = ".", row.names = 1)
Abundance <- read.csv("~/Documents/Github/Stats_Workshop_SEL/Multivariate_Analysis/SpeciesAbun.csv", header = T, na = ".", row.names = 1)
IRW_WQ <- read.csv("~/Documents/Github/Stats_Workshop_SEL/Multivariate_Analysis/IRW_WQ.csv", header = T, na = ".", row.names = 1)
IRW_Abundance <- read.csv("~/Documents/Github/Stats_Workshop_SEL/Multivariate_Analysis/IRW_Abundance.csv", header = T, na = ".", row.names = 1)

## CCE
## Correspondance Analysis similar to NMDS but metric not non-metric. Does not need environmental data 
## Unconstrained by environmental data. Arranges sampling sites in the way to best capture spp. composition variation among sites
## Built in similarity index. CHORD index popular in ~70s and 80s. until CCE came out in ~1986
## Constrained ordination. ordingation must be explained by the data that you have. 

##CCA
vare.cca <- cca(varespec, varechem)
vare.cca
plot(vare.cca)

IRW_Abundance <- IRW_Abundance %>% select(-c(BLR))
IRW.cca <- cca(IRW_Abundance, IRW_WQ)
IRW.cca
plot(IRW.cca)

irw_rda_do <- rda(IRW_Abundance~do, IRW_WQ)
plot(irw_rda_do)

##### PerMANOVA using Adonis in vegan

data("dune")
data("dune.env")
adonis(dune~Management, data = dune.env)

IRW_pmanova_4 <- adonis(IRW_Abundance~secchi*do*tss*pH, data = IRW_WQ)
IRW_pmanova_4

perm_pmanova_2 <- adonis(Abundance~Site*Reach*YEAR*TimePeriod, data = perm)
perm_pmanova_2

### proportion explained by each factor SumofSquares/Total*100

perm_pmanova <- adonis(Abundance~Site*Reach*YEAR*RIVER, data = perm)
perm_pmanova

###### Indicator species
## Load species data
data(wetland) 
### Wetland is likely rank abundance of different wetland species.
### Use rank or the 
## Create three clusters using kmeans
## Create 3 separate groups 
wetkm = kmeans(wetland, centers=3)
groupskm = wetkm$cluster
groupskm

## Runs the combination analysis using IndVal.g as statistic
wetpt = multipatt(wetland, wetkm$cluster, control = how(nperm=999)) 

## Lists those species with significant association to one combination
summary(wetpt) 

## Lists those species with significant association to one combination, 
## including indval components.
summary(wetpt, indvalcomp=TRUE) 

## Determine sensitivity of individual species
B=strassoc(wetland, cluster=wetkm$cluster,func="B") 

## Select species with more than 20% of sensitivity for the first group
sel=which(B[,1]>0.2) 

## Run indicator analysis with species combinations for the first group
sc= indicators(X=wetland[,sel], cluster=wetkm$cluster, group=1, verbose=TRUE, 
               At=0.5, Bt=0.2)

#Prints the results
print(sc)

## Plots positive predictive power and sensitivity against the order of 
## combinations
plot(sc, type="A")
plot(sc, type="B")

## Run indicator analysis with species combinations for the first group, 
## but forcing 'Orysp' to be in all combinations
sc2= indicators(X=wetland[,sel], cluster=wetkm$cluster, group=1, verbose=TRUE, 
                At=0.5, Bt=0.2, enableFixed=TRUE)

####
## Compute Dufrene and Legendre's IndVal
strassoc(wetland, wetkm$cluster, func="IndVal.g") 

## STRASSOCCompute point-biserial correlation, with bootstrap 95 percent confidence intervals
wetkm = kmeans(wetland, centers=3)
groupskm = wetkm$cluster
groupskm

wab = multipatt(Indic_Species, Indic_Species$Type, control = how(nperm=999))
summary(wab)

wab_99 = multipatt(Indic_Species, Indic_Species$Type, control = how(nperm=99))
summary(wab_99)

## Changing the number of permutations will reduce the significance however the spp.
## Pattern remains the same for the separation and significance between groups. 
# strassoc(wetland, wetkm$cluster, func="r", nboot =100)

### This is to demonstrate the strength of the relationship between specific spp. and groups 
INdVal_wab <- strassoc(Indic_Species, Indic_Species$Type, func="IndVal.g") 
INdVal_wab
### Also look into InvVal package.


# wabash_spp <- strassoc(Indic_Species, cluster = c(M,S), func = "r", )
