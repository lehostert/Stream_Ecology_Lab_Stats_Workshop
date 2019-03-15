library(tidyverse)
library(vegan)

# One component of the base tidyverse is readr
# Allows you to do things similar to base R but makes the syntax consistent

# readxl
# Load xlsx files or xls files directly. Call the sheet # or the "Sheet_name".
file_path <- "~/Documents/GitHub/Stream_Ecology_Lab_Stats_Workshop/Tidyverse/Example_Fish_Data.xlsx"
fish <- readxl::read_excel(file_path, sheet = "Fish" )

#Alternatively you can load the data as a csv 
fish_base_csv <- read.csv("~/Documents/GitHub/Stream_Ecology_Lab_Stats_Workshop/Tidyverse/Example_Fish_Data.csv", header = T, stringsAsFactors = F)
# fish_base_csv$Event_Date <- as.Date(fish_base_csv$Event_Date, "%m/%d/%Y")

fish_tidy_csv <- readr::read_csv("~/Documents/GitHub/Stream_Ecology_Lab_Stats_Workshop/Tidyverse/Example_Fish_Data.csv")

# Pipes
# They are used in several other programming languages like F# but you can think of them as the statement "then"

tidy_counts_per_species <- fish %>%
  dplyr::filter(Site_Type == "random") %>%
  dplyr::group_by(Fish_Species_Code) %>%
  dplyr::summarise(species_count= n())

 counts_per_species <- filter(fish, Site_Type == "random")
# ^To do something similar in R you would have to use aggregate or by 

# Lubridate
# Pull out data components from strings of data and time data
# Calculations from time interval data
fish$Year <- lubridate::year(fish$Event_Date)
fish$Month <- lubridate::month(fish$Event_Date)

# Create "Site_ID" to name each of your samples.
# Individual components for creating a Site_ID
fish$Compressed_Name <- stringr::str_replace_all(fish$Reach_Name, "[:blank:]", "")
fish$Compressed_Date <- stringr::str_replace_all(fish$Event_Date,"-","")

# Create Site_ID in one line
fish$Site_ID <- paste(str_replace_all(fish$Reach_Name, "[:blank:]", ""), str_replace_all(fish$Event_Date,"-",""), sep = "_")

# Create Sparse Species Dataframe
sparse_fish_data <- fish %>%
  dplyr::filter(Site_Type == 'student_project') %>%
  select(c(Site_ID, Fish_Species_Code, Fish_Species_Count))%>%
  spread(Fish_Species_Code,Fish_Species_Count, fill = 0)
  
# Add And Remove row.names
# vegan::diversity requires row names for evaluation. 
# This will make Site_ID the row name then remove the collmun Site_ID after row names are assigned
  row.names(sparse_fish_data) <- sparse_fish_data$Site_ID
  sparse_fish_data <- sparse_fish_data %>% select(-c(Site_ID))
  
# Begin per site metric computation from sparse dataset
  INDIVIDUALS <- rowSums(sparse_fish_data)
  RICHNESS <- rowSums(sparse_fish_data != 0) 
  DIVERSITY <- vegan::diversity(sparse_fish_data, index = "shannon")
  EVENNESS <- vegan::diversity(sparse_fish_data)/log(specnumber(sparse_fish_data))
  
# Joins
traits <- readr::read_csv("~/Documents/GitHub/Stream_Ecology_Lab_Stats_Workshop/Tidyverse/Example_Fish_Traits.csv")
fish_with_traits <- left_join(fish, traits, by = "Fish_Species_Code")

#Create functions using tidy data components
create_site_metric_tibble <- function(counts_and_traits) {
    
    #' Create base for the site metric tibble using functions from package 'vegan'
    #' 
    #' Creates a dataframe with calculated features number of individuals, species richness, shannon diversity index,
    #' and eveness for each site ids in the input tibble
    #' 
    #' @param counts_and_traits A tibble of count data and animal traits
    #' 
    
    sparse_fish_data <- counts_and_traits %>%
      select(c(Fish_Species_Code,Fish_Species_Count,Site_ID)) %>%
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
    
    # Create dataframe from computed metrics and convert row names back to a collumn in the dataframe.
    # Consider making this a tibble directly instead of a dataframe. 
    data.frame(INDIVIDUALS,RICHNESS,DIVERSITY,EVENNESS) %>%
      tibble::rownames_to_column(var = "Site_ID")
    
  }
  