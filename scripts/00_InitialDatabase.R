library(dplyr)
library(readr)

###########################################################################################

###Save raw data collected in folder "Primate_Studies" - link

###Merge CSV's

setwd("") #paste in folder path

#Create list of .csv files in wd
file_list <- list.files(full.names = TRUE, recursive = TRUE)

#Function to force some variables to read in as 'character'. Otherwise these variables read in as both integer and character and produces error.
read_csv_force_character <- function(file_path) {
  read.csv(file_path, colClasses = c(N_groups = "character", Abundanc_1 = "character", Abundance = "character", Locomotion = "character"))
}

#Read all .csv files and bind them into one data frame using function
database <- bind_rows(lapply(file_list, read_csv_force_character))

#Remove all columns not required. Trait information is kept in case it is wanted for future studies with this data
database <- database %>% filter(BASE != "GBIF") #Remove GBIF data as not presence-absence data, only presence
database <- database[,c(1:45)] #Remove buffer columns
database <- database[,-c(1:3, 5, 8, 13, 17:20,24:27)] #Remove other columns not needed.

###Write as csv and use Excel Find and Replace to replace "Â" with "". Have tried countless things in R to fix encoding issue but not working

setwd("") #paste in path to save database
write.csv(database, 'database.csv', row.names = FALSE)

database <- read.csv('database.csv', header = TRUE)

###Add the IUCN red list status for each primate

#First, create new lookup column of IUCN names. These are sometimes different to ITIS names used in database

#Function to handle the replacements
replace_species <- function(species_name) {
  if (species_name == "Aotus_nancymai") {
    return("Aotus nancymaae")
  } else if (species_name == "Lepilemur_grewcocki") {
    return("Lepilemur grewcockorum")
  } else if (species_name == "Microcebus_mittermeieri") {
    return("Microcebus lehilahytsara")
    #species Microcebus mittermeieri no longer recognised by IUCN. Subspecies of Microcebus lehilahytsara
  } else if (species_name == "Saimiri_macrodon") {
    return("Saimiri cassiquiarensis")
    #species Saimiri macrodon no longer recognised by IUCN. Subspecies of Saimiri cassiquiarensis
  } else if (species_name == "Cephalopacus_bancanus") {
    return("Cephalopachus bancanus")
    #extra 'h' in IUCN spelling
  } else {
    return(gsub("_", " ", species_name))
  }
}


#Apply new IUCN name column
database <- database %>%
  mutate(TaxonNameIUCN = sapply(Taxon_ITIS, replace_species)) %>% 
  rename(Occurrence = Ocurrence)

#Download IUCN summary reports from IUCN red list website
#Read in assessments.csv file from IUCN download
red_list <- read.csv('assessments.csv', header=TRUE)

#Lookup IUCN red list status
red_list <- red_list %>% 
  rename(TaxonNameIUCN = scientificName) %>% 
  select(TaxonNameIUCN, redlistCategory)

database <- left_join(database, red_list, by="TaxonNameIUCN")

#Filter out non-forest species. Only two species in database are non-forest as per https://zenodo.org/records/1342459
database <- database %>%
  filter(!Taxon %in%  c("Chlorocebus aethiops", "Galago gallarum"))

#Some species have different Genus for same Taxon. Make these consistent.
database <- database %>%
  mutate(Genus = case_when(
    Taxon_ITIS == "Lagothrix_flavicauda" ~ "Lagothrix", 
    Taxon_ITIS == "Leontocebus_leucogenys" ~ "Leontocebus", ##separated from Saguinus Genus
    Taxon_ITIS == "Sapajus_apella" ~ "Sapajus", ##separated from Cebus Genus
    Taxon_ITIS == "Sapajus_xanthosternos" ~ "Sapajus", ##separated from Cebus Genus
    TRUE ~ Genus
  )) 

#Create new variable for level of Species-per-Study named Dataset
database <- database %>%
  arrange(Taxon_ITIS) %>% 
  mutate(species_number = dense_rank(Taxon_ITIS))

#Identify separate studies for each taxon using alphabetic notation
factors_df <- data.frame()

for (i in 1:max(database$species_number)){
  df <- database %>%
    filter(species_number == i)
  
  factors <- factor(df$Source)
  levels(factors) <- letters[1:length(levels(factors))]
  
  factors_df <- rbind(factors_df, data.frame(Source_grp = factors))
}

database$Source_grp <- factors_df$Source_grp

#Create Dataset variable - Species + alphabetic notation. e.g. Alouatta_caraya _c is the 3rd study of Alouatta caraya
database$Dataset <- paste0(database$Taxon_ITIS,"_",database$Source_grp)

#Remove Species-per-Study (so Dataset) with only Presence or only Absence, studies with 4 or less patches, or duplicated spatial points (same dataset, same sampling year, same coordinates)
min_n_per_study <- 5

database <- database %>%
  #remove all-absence and all-presence
  group_by(Dataset) %>%
  filter(n() >= min_n_per_study) %>%
  filter(sum(Occurrence) > 0) %>% 
  #remove datasets with 4 or less patches
  filter(sum(Occurrence) < n()) %>% 
  ungroup() %>% 
  #remove duplicates
  group_by(X, Y, Sample_yea, Dataset) %>% 
  filter(n() == 1) %>% 
  ungroup() %>% 
  arrange(Sample_yea) #Important for creating rasters in ForestCoverRgee.R script

#Write full database into new csv file
write.csv(database,'Primate_Database_Initial.csv', row.names = FALSE)


