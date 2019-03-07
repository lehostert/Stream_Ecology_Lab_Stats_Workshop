library(tidyverse)
library(vegan)

data(BCI)

fsh <- read.csv("~/Github/Stats_Workshop_SEL/Multivariate_Analysis/five_assemblages_fish.csv", header = T, na = ".", row.names = 1)
LTEF <- read.csv("~/Documents/Github/Stats_Workshop_SEL/Multivariate_Analysis/LTEF_fish_data.csv", header = T, na = ".", row.names = 1)

##NMDS 
data(dune)
# Global NMDS using monoMDS
sol <- metaMDS(dune)
sol
plot(sol, type="t")
## Start from previous best solution
sol <- metaMDS(dune, previous.best = sol)
## Local NMDS and stress 2 of monoMDS
sol2 <- metaMDS(dune, model = "local", stress=2)
sol2
## Use Arrhenius exponent 'z' as a binary dissimilarity measure
sol3 <- metaMDS(dune, distfun = betadiver, distance = "z")
sol3

sol2.3dim <- metaMDS(dune, model = "local", stress=2, k=3)
sol2.3dim

plot(sol, type="t")
plot(sol2, type="t")
plot(sol3, type="t")
plot(sol2.3dim, type="t", choices = c(1,3))

#######################################
## Log transform the data so that any outlying values are standarized.
fsh.log <- log(fsh+1)
# Global NMDS using metaMDS
fish.MDS <- metaMDS(fsh.log)
fish.MDS
plot(fish.MDS, type="t", main = "Fish MDS Bray-Curtis")
## Start from previous best solution
fish.MDS.best <- metaMDS(fsh.log, previous.best = fish.MDS)
fish.MDS.best

fish.MDS.euc <- metaMDS(fsh.log, distance = "euclidean")
fish.MDS
plot(fish.MDS.euc, type="t", main = "Fish MDS Euclidean")
#### The black clouds are clusters are sites that are all in the same streams
## The species names in the center of the plot are speces that are similarly found 
## in all of the streams, more balanced. The species on the edges are close to sites that are
## close to them 

####### PCA
########################################
# fish.dist <- dist(fsh.log, method = "euclidean")
fish.BC <- vegdist(fsh.log, method = "bray")
fish.cluster <- hclust(fish.BC, method = "complete", members = NULL)
fish.cluster.ward <- hclust(fish.BC, method = "ward.D", members = NULL)

plot(fish.cluster, type="t", main = "Fish Cluster BC Complete")
plot(fish.cluster.ward, type="t", main = "Fish Cluster BC Ward")

# fish.BC <- vegdist(fsh, method = "bray")
# fish.CY <- vegdist(fsh, method = "cao")

###########
LTEF_SQ <- sqrt(LTEF)
# fish.dist <- dist(LTEF_SQ, method = "euclidean")
LTEF_BC <- vegdist(LTEF_SQ, method = "bray")
LTEF_cao <- vegdist(LTEF_SQ, method = "cao")
LTEF_MOR <- vegdist(LTEF_SQ, method = "morisita")
fish.cluster.BC <- hclust(LTEF_BC, members = NULL)
fish.cluster.cao<- hclust(LTEF_cao, members = NULL)
fish.cluster.MOR<- hclust(LTEF_MOR, members = NULL)

plot(fish.cluster.BC, type="t", main = "LTEF Cluster BC")
plot(fish.cluster.cao, type="t", main = "LTEF Cluster Cao")
plot(fish.cluster.MOR, type="t", main = "LTEF Cluster MOR")

LTEF.MDS <- metaMDS(LTEF)
LTEF.MDS
plot(LTEF.MDS, type="t", main = "LTEF MDS BC")

LTEF.MDS.cao <- metaMDS(LTEF, distance = "cao")
LTEF.MDS.cao
plot(LTEF.MDS.cao, type="t", main = "LTEF MDS Cao")

LTEF.MDS.cao
LTEF.MDS
##########
########################################
fish.CY <- vegdist(fsh, method = "cao")

evenness  <- function(x) {
  diversity(x)/log(specnumber(x))
}

H <- diversity(fsh, index = "shannon")
J <- evenness(fsh)
Fish_Summary <- as.data.frame(H)
Fish_Summary$Evenness <- J

BC <- vegdist(fsh, method = "bray")
CY <- vegdist(fsh, method = "cao")

Dissimilarity <- as.matrix(BC)