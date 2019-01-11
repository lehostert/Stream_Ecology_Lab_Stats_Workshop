library(tidyverse)
library(tree)
library(VSURF)
library(randomForest)

# metrics_envi.dat <- read.csv("metrics_envi.csv", header = T, row.names = "Type")
# metrics_list_LEH <- read.table("metrics_list.txt", header = T, stringsAsFactors = F) %>%
#   as.matrix()

cal <- read.csv("Cal_85_env.csv", header = T, row.names = "G5")
env <- read.csv("Envion_46443_reaches.csv", header = T, row.names = "Reach")
mussel <- read.csv("Mussel_binary_environ.csv", header = T, row.names = "EPA_Code")