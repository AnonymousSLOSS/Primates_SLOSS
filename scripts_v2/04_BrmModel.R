#!/usr/bin/env Rscript

#SBATCH --partition=milkun
#SBATCH --time=3-00:00:00
#SBATCH --output "brms.out"
#SBATCH --mem=200G
#SBATCH --job-name=test
#SBATCH --ntasks-per-node=1
#SBATCH --cpus-per-task=1

.libPaths(c("....")) #for use if R package files are located elsewhere than default path
.libPaths()

library(brms)
library(dplyr)
library(parallel)

#set working directory if using local system or cluster environment
if(Sys.info()["sysname"]=="Windows"){
 setwd(".........") #add in path for local windows pc
}else{
  setwd("........") #add in path for cluster environment
}

#Patch combinations data frame
patch_comb_stats_Din <- read.csv('04_Primate_Database_PatchCombinations_Reduced.csv', header = TRUE)

#Create groups of threatened (vulnerable, endangered, critically endangered) and non-threatened (least concern, near threatened)
patch_comb_stats_Din <- patch_comb_stats_Din %>%
  mutate(threatened_status = case_when(
    redlistCategory %in%  c("Critically Endangered", "Endangered", "Vulnerable") ~ "Threatened",
    redlistCategory %in%  c("Least Concern", "Near Threatened") ~ "Non-Threatened",
    redlistCategory %in%  "Data Deficient" ~ "Data Deficient",
    TRUE ~ redlistCategory
  )) 

#Arrange order of habitat area
order <- c("twenty_percent", "thirty_percent", "forty_percent", "fifty_percent", "sixty_percent", "seventy_percent", "eighty_percent")  
order_redlist <- c("Least Concern", "Near Threatened", "Vulnerable", "Endangered", "Critically Endangered")
order_threatened <- c("Non-Threatened", "Threatened")  

#Factorise
patch_comb_stats_Din <- patch_comb_stats_Din %>% 
  filter(redlistCategory != "Data Deficient") %>% 
  mutate(habitat_amount = factor(habitat_amount, levels = order),
         redlistCategory = factor(redlistCategory, levels = order_redlist),
         threatened_status = factor(threatened_status)) %>% 
  arrange(Dataset, habitat_amount)

##################################

#Main model for Mean Patch Size method

brm_occ_Din <-  brm(Occurrence ~ mean_patch_area_log * redlistCategory +
                  (mean_patch_area_log | Dataset) +
                  (mean_patch_area_log | Taxon_ITIS) +  
                  (1 | habitat_amount),
                data = patch_comb_stats_Din,
                family = bernoulli(link = "logit"),
                warmup = 1500,
                iter = 3000,
                seed = 123,
                chains = 3,
                cores = 9,
                control = list(adapt_delta = 0.90, stepsize = 0.1, max_treedepth = 15))

#Alternative model (using threatened vs non-threatened) for Mean Patch Size method. Results in Supplementary

brm_occ_Din_thr <-  brm(Occurrence ~ mean_patch_area_log * threatened_status +
                      (mean_patch_area_log | Dataset) +
                      (mean_patch_area_log | Taxon_ITIS) +  
                      (1 | habitat_amount),
                    data = patch_comb_stats_Din,
                    family = bernoulli(link = "logit"),
                    warmup = 1500,
                    iter = 3000,
                    seed = 123,
                    chains = 3,
                    cores = 9,
                    control = list(adapt_delta = 0.90, stepsize = 0.1, max_treedepth = 15))

save.image("brm_Din_reduced.RData")

#For Supplementary figures, this code needs to be repeated for databases with different canopy cover thresholds and saved under a different .RData name