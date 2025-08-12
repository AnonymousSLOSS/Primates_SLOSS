library(dplyr)
library(data.table)
# install.packages("psych")
# install.packages("rlist")
library(psych) #used by functions sourced below
library(rlist) #used by functions sourced below
source("03_1_PatchCombinationFunctions.R")

#setwd

#set seed
set.seed(123)
setwd("...................") #populate

#remove scientific notation
options(scipen=999)

primatepatches <-  read.csv('03_Primate_Database_Patches.csv', header = TRUE) %>% 
  filter(patch_area <= 1000) #main analysis was changed to exclude patches > 1000 ha. 

#Due to change in main analysis, some data cleaning needs to be repeated

min_n_per_study <- 5

primatepatches <- primatepatches %>%
  group_by(Dataset) %>% #Need to reapply filters as some datasets have lost patches and are now 5 or below or may contain all-presence and all-absence 
  filter(n() >= min_n_per_study) %>%
  filter(sum(Occurrence) > 0) %>% 
  filter(sum(Occurrence) < n()) %>% 
  ungroup() %>% 
  arrange(Dataset, patch_area) %>% #Finally arrange by dataset without sampling year and patch area
  mutate(row_id_3 = row_number())

write.csv(primatepatches, "03_Primate_Database_Patches_Reduced.csv", row.names = FALSE)

######

#create patch and dataset_and_patch columns for use in generating patch combinations, where patch is the nth patch in a dataset
primatepatches <- primatepatches %>% 
  group_by(Dataset) %>%
  mutate(patch = paste0("P",row_number()),
         dataset_and_patch = paste(Dataset, patch)) %>% 
  ungroup()

patches <- primatepatches[,c("Dataset", "patch", "dataset_and_patch", "Occurrence",  "patch_area")]
patches_split <- split(patches, patches[,1]) #used in properties_comb function later

#Create data frame of patches for use in PATCH_COMBINATIONS function
patches <- patches[,-4]
patches <- as.data.table(patches)
patches$Dataset <- as.factor(patches$Dataset)
patches$dataset_and_patch <- as.factor(patches$dataset_and_patch)
patch_size <- data.frame(dataset_and_patch = patches$dataset_and_patch, Occurrence = primatepatches$Occurrence, patch_area = patches$patch_area)

# create lists where each dataset is one element of a list
list_patches <- list()
for (i in 1:length(unique(patches$Dataset))){
  list_patches[[i]] <- subset(patches, Dataset == levels(patches$Dataset)[i])}

###Generate patch combinations and their statistics for varying habitat amounts. 
###See functions in below script at 00_functions_modifiedv3.R

list_simulations_twenty <- vector("list",length(list_patches))
for (i in 1:length(list_patches)) {list_simulations_twenty[[i]] <- tryCatch(PATCH_COMBINATIONS(list_patches[[i]], 0.2), error=function(e) print(NA))}

list_simulations_thirty <- vector("list",length(list_patches))
for (i in 1:length(list_patches)) {list_simulations_thirty[[i]] <- tryCatch(PATCH_COMBINATIONS(list_patches[[i]], 0.3), error=function(e) print(NA))}

list_simulations_forty <- vector("list",length(list_patches))
for (i in 1:length(list_patches)) {list_simulations_forty[[i]] <- tryCatch(PATCH_COMBINATIONS(list_patches[[i]], 0.4), error=function(e) print(NA))}

list_simulations_fifty <- vector("list",length(list_patches))
for (i in 1:length(list_patches)) {list_simulations_fifty[[i]] <- tryCatch(PATCH_COMBINATIONS(list_patches[[i]], 0.5), error=function(e) print(NA))}

list_simulations_sixty <- vector("list",length(list_patches))
for (i in 1:length(list_patches)) {list_simulations_sixty[[i]] <- tryCatch(PATCH_COMBINATIONS(list_patches[[i]], 0.6), error=function(e) print(NA))}

list_simulations_seventy <- vector("list",length(list_patches))
for (i in 1:length(list_patches)) {list_simulations_seventy[[i]] <- tryCatch(PATCH_COMBINATIONS(list_patches[[i]], 0.7), error=function(e) print(NA))}

list_simulations_eighty <- vector("list",length(list_patches))
for (i in 1:length(list_patches)) {list_simulations_eighty[[i]] <- tryCatch(PATCH_COMBINATIONS(list_patches[[i]], 0.8), error=function(e) print(NA))}

#Output summary statistics of Patch Combination. See function for info.
tab_comb_twenty <- properties_combination(list_simulations_twenty)
tab_comb_thirty <- properties_combination(list_simulations_thirty)
tab_comb_forty <- properties_combination(list_simulations_forty)
tab_comb_fifty <- properties_combination(list_simulations_fifty)
tab_comb_sixty <- properties_combination(list_simulations_sixty)
tab_comb_seventy <- properties_combination(list_simulations_seventy)
tab_comb_eighty <- properties_combination(list_simulations_eighty)

#Add label for forest amount threshold
tab_comb_twenty$habitat_amount <- rep("twenty_percent", nrow(tab_comb_twenty))
tab_comb_thirty$habitat_amount <- rep("thirty_percent", nrow(tab_comb_thirty))
tab_comb_forty$habitat_amount <- rep("forty_percent", nrow(tab_comb_forty))
tab_comb_fifty$habitat_amount <- rep("fifty_percent", nrow(tab_comb_fifty))
tab_comb_sixty$habitat_amount <- rep("sixty_percent", nrow(tab_comb_sixty))
tab_comb_seventy$habitat_amount <- rep("seventy_percent", nrow(tab_comb_seventy))
tab_comb_eighty$habitat_amount <- rep("eighty_percent", nrow(tab_comb_eighty))

#Occurrence summary of Patch Combinations. See function for info.
occ_twenty <- occurrence_combination(list_simulations_twenty)
occ_thirty <- occurrence_combination(list_simulations_thirty)
occ_forty <- occurrence_combination(list_simulations_forty)
occ_fifty <- occurrence_combination(list_simulations_fifty)
occ_sixty <- occurrence_combination(list_simulations_sixty)
occ_seventy <- occurrence_combination(list_simulations_seventy)
occ_eighty <- occurrence_combination(list_simulations_eighty)

occ_twenty$n <- as.numeric(occ_twenty$n)
occ_thirty$n <- as.numeric(occ_thirty$n)
occ_forty$n <- as.numeric(occ_forty$n)
occ_fifty$n <- as.numeric(occ_fifty$n)
occ_sixty$n <- as.numeric(occ_sixty$n)
occ_seventy$n <- as.numeric(occ_seventy$n)
occ_eighty$n <- as.numeric(occ_eighty$n)


#Combine occurrence and statistics of Patch Combinations into one large data frame with metadata
comb_occ_twenty <- merge(tab_comb_twenty, occ_twenty, by = c("Dataset", "n", "simulation_number", "patch_comb_number"))
comb_occ_twenty <- comb_occ_twenty %>% arrange(Dataset, patch_comb_number)

comb_occ_thirty <- merge(tab_comb_thirty, occ_thirty, by = c("Dataset", "n", "simulation_number", "patch_comb_number"))
comb_occ_thirty <- comb_occ_thirty %>% arrange(Dataset,patch_comb_number)

comb_occ_forty <- merge(tab_comb_forty, occ_forty, by = c("Dataset", "n", "simulation_number", "patch_comb_number"))
comb_occ_forty <- comb_occ_forty %>% arrange(Dataset,patch_comb_number)

comb_occ_fifty <- merge(tab_comb_fifty, occ_fifty, by = c("Dataset", "n", "simulation_number", "patch_comb_number"))
comb_occ_fifty <- comb_occ_fifty %>% arrange(Dataset,patch_comb_number)

comb_occ_sixty <- merge(tab_comb_sixty, occ_sixty, by = c("Dataset", "n", "simulation_number", "patch_comb_number"))
comb_occ_sixty <- comb_occ_sixty %>% arrange(Dataset,patch_comb_number)

comb_occ_seventy <- merge(tab_comb_seventy, occ_seventy, by = c("Dataset", "n", "simulation_number", "patch_comb_number"))
comb_occ_seventy <- comb_occ_seventy %>% arrange(Dataset,patch_comb_number)

comb_occ_eighty <- merge(tab_comb_eighty, occ_eighty, by = c("Dataset", "n", "simulation_number", "patch_comb_number"))
comb_occ_eighty <- comb_occ_eighty %>% arrange(Dataset,patch_comb_number)
                        
#Create full dataframe of patch combinations
patch_comb_stats <- rbind(comb_occ_twenty, comb_occ_thirty,
                           comb_occ_forty, comb_occ_fifty,
                           comb_occ_sixty, comb_occ_seventy,
                           comb_occ_eighty)

#Add back relevent metadata to Patch Combinations dataframe

df_lookup <- primatepatches[,c(4,10:12,14,40:41,44)]      
df_lookup <- df_lookup[!duplicated(df_lookup), ]
patch_comb_stats <- merge(patch_comb_stats, df_lookup, by = "Dataset") 

#Create Occurrence variable representing Occurrence within whole Patch Combination.
patch_comb_stats$Occurrence <- ifelse(patch_comb_stats$Cumulative_Occurrence > 0, 1, 0)

#Log base 10 of mean patch size
patch_comb_stats$mean_patch_area_log <- log10(patch_comb_stats$mean)

write.csv(patch_comb_stats, "04_Primate_Database_PatchCombinations_Reduced.csv", row.names = FALSE)

#save.image("PatchCombinations.RData")




