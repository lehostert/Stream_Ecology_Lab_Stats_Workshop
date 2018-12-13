library(tidyverse)
library(tree)
library(VSURF)
library(randomForest)

ENS <- read.csv("ENS.txt", header = TRUE, stringsAsFactors = FALSE)
Envi <- read.csv("Envi.txt", header = TRUE, stringsAsFactors = FALSE)
