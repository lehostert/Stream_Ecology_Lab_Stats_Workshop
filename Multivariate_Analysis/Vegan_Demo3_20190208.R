library(tidyverse)
library(vegan)

### Ordination Techniques Continued 

fish <- read.csv("~/Documents/Github/Stats_Workshop_SEL/Multivariate_Analysis/five_assemblages.csv", header = T, na = ".", row.names = 1)
fish.evi <- read.delim2("~/Documents/Github/Stats_Workshop_SEL/Multivariate_Analysis/TV.txt", sep = ",", header = T, na = ".", row.names = 1)
LTEF <- read.csv("~/Documents/Github/Stats_Workshop_SEL/Multivariate_Analysis/LTEF_fish_data.csv", header = T, na = ".", row.names = 1)


Indic_Species <- read.csv("~/Documents/Github/Stats_Workshop_SEL/Multivariate_Analysis/indicator_species_data.csv", header = T, na = ".", row.names = 1)

indic_spp.type <- Indic_Species %>% select(Type)
indic_spp.data <- Indic_Species %>% select(-c(Type))
indic_spp.dist2 <- vegdist(indic_spp.data)
indic_spp.anov2 <- with(indic_spp.type, anosim(indic_spp.dist, Type))
summary(indic_spp.anov2)
plot(indic_spp.anov2)


indic_spp.dist <- vegdist(Indic_Species)
indic_spp.ano <- anosim(indic_spp.dist, Indic_Species$Type)
summary(indic_spp.ano)
plot(indic_spp.ano)

##Example 
data(dune)
data(dune.env)
dune.dist <- vegdist(dune)
dune.ano <- with(dune.env, anosim(dune.dist, Management))
summary(dune.ano)
plot(dune.ano)

#### MRPP Multi- Reponse Permutation Procedure
data(dune)
data(dune.env)
dune.mrpp <- with(dune.env, mrpp(dune, Management))
dune.mrpp

indic.mrpp <- with(indic_spp.type, mrpp(indic_spp.data, Type))
indic.mrpp

# Save and change plotting parameters
def.par <- par(no.readonly = TRUE)
layout(matrix(1:2,nr=1))
plot(dune.ord <- metaMDS(dune), type="text", display="sites" )
with(dune.env, ordihull(dune.ord, Management))
with(dune.mrpp, {
  fig.dist <- hist(boot.deltas, xlim=range(c(delta,boot.deltas)),
                   main="Test of Differences Among Groups")
  abline(v=delta);
  text(delta, 2*mean(fig.dist$counts), adj = -0.5,
       expression(bold(delta)), cex=1.5 ) }
)
par(def.par)

## meandist
dune.md <- with(dune.env, meandist(vegdist(dune), Management))
dune.md
summary(dune.md)
plot(dune.md)
plot(dune.md, kind="histogram")