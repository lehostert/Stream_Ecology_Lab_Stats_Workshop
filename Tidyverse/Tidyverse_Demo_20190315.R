library(tidyverse)
library(vegan)

# components of the base tidy verse is readr
# Allows you to do things similar tobase R but makes the syntax consistent

# readxl
# pc file_path <- "~/GitHub/Stream_Ecology_Lab_Stats_Workshop/Tidyverse/Example_Fish_Data.xlsx"
file_path <- "~/Documents/GitHub/Stream_Ecology_Lab_Stats_Workshop/Tidyverse/Example_Fish_Data.xlsx"

#Load xlsx files or xls files directly. Call the sheet # or the Sheet name. 
fish <- readxl::read_excel(file_path, sheet = "Fish" )

#Alternatively you can load the data as a csv. 
fish_base_csv <- read.csv("~/GitHub/Stream_Ecology_Lab_Stats_Workshop/Tidyverse/Example_Fish_Data.csv", header = T, stringsAsFactors = F)

fish_tidy_csv <- readr::read_csv("~/GitHub/Stream_Ecology_Lab_Stats_Workshop/Tidyverse/Example_Fish_Data.csv")

# Introduce teh idea of pipes. They are used in several other programming languages like F# but you can think of them as the statement "then"

tidy_counts_per_species <- fish %>%
  dplyr::filter(Site_Type == "random")%>%
  dplyr::group_by(Fish_Species_Code)%>%
  dplyr::summarise(species_count= n())

counts_per_species <- filter(fish, Site_Type == 'random')
counts_per_species <- sum(counts_per_species, counts_per_species$Fish_Species_Count)
counts_per_species

# <- aggregate(cbind(count = VALUE) ~ MONTH.YEAR, 
#           data = mydf, 
#           FUN = sum(x, ..., na.rm = FALSE))
# 
# fish %>%
#   dplyr::filter(Site_Type == "random")%>%
  
#Create a unique "Site_Id" to name each of your samples.

fish$Compressed_Name <- stringr::str_replace_all(fish$Reach_Name, "[:blank:]", "")
fish$Compressed_Date <- stringr::str_replace_all(fish$Event_Date,"-","")

fish$Site_ID <-paste(str_replace_all(fish$Reach_Name, "[:blank:]", ""), str_replace_all(fish$Event_Date,"-",""), sep = "_")

sparse_fish_data <- fish %>%
  dplyr::filter(Site_Type == 'student_project') %>%
  select(c(Site_ID, Fish_Species_Code, Fish_Species_Count))%>%
  spread(Fish_Species_Code,Fish_Species_Count, fill = 0)
  

# vegan::diversity requires row names for evaluation. 
# This will make Site_ID the row name then remove the collmun Site_ID after row names are assigned
  row.names(sparse_fish_data) <- sparse_fish_data$Site_ID
  sparse_fish_data <- sparse_fish_data %>% select(-c(Site_ID))
  
# Begin metric computation from sparse dataset
  INDIVIDUALS <- rowSums(sparse_fish_data)
  RICHNESS <- rowSums(sparse_fish_data != 0) 
  DIVERSITY <- vegan::diversity(sparse_fish_data, index = "shannon")
  EVENNESS <- vegan::diversity(sparse_fish_data)/log(specnumber(sparse_fish_data))
  
  