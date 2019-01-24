library(tidyverse)
library(vegan)

fsh <- read.csv("~/Github/Stats_Workshop_SEL/Multivariate_Analysis/five_assemblages.csv", header = T, na = ".", row.names = 1)

CY <- vegdist(fsh, method = "cao")
