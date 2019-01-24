library(tidyverse)
library(vegan)

data(BCI)

fsh <- read.csv("~/Github/Stats_Workshop_SEL/Multivariate_Analysis/five_assemblages.csv", header = T, na = ".", row.names = 1)
LTEF <- read.csv("~/Github/Stats_Workshop_SEL/Multivariate_Analysis/LTEF_fish_data.csv", header = T, na = ".", row.names = 1)

evenness  <- function(x) {
  diversity(x)/log(specnumber(x))
}

H <- diversity(fsh, index = "shannon")
J <- evenness(fsh)
BC <- vegdist(fsh, method = "bray")
CY <- vegdist(fsh, method = "cao")

Fish_Summary <- as.data.frame(H)
Fish_Summary$Evenness <- J


Dissimilarity <- as.matrix(BC)

# CY <- vegdist(fsh, method = "cao")
# y <- diversity(BCI, index = "shannon")
# yy <- diversity(BCI)
# z <- diversity (fsh, index = "shannon")
# 
# Z <- as.data.frame(z)
# 
# Fish_Summary <- as.data.frame(y)
# Fish_Summary$yy <- y
# 
# J_hat <- evenness(BCI)
# S <- specnumber(BCI)
# 
# yak$J <-(diversity(BCI)/log(S))
# 
# evenness  <- function(x) {
#   diversity(x)/log(specnumber(x))
# }
# 
# J_hat <- evenness(BCI)