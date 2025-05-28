## FUNCTION 1
# GENERATES PATCH COMBINATIONS AT EQUAL HABITAT AREA
PATCH_COMBINATIONS <- function(db, percent) {  # db= dataset, percent = habitat amount
  
  set.seed(111)
  n_db <- 100 # 100 attempts to simulate random patch combinations
  
  #total habitat amount 
  target_area <- sum(db$patch_area) * percent
  
  #check how big is the largest patch in a dataset in comparison to the target area
  target_area
  max(db$patch_area)
  
  ##randomly select a n_db sets of patches 
  list_db <- list()
  for (i in 1 : n_db){
    sampled_db <- dplyr::sample_n(db,1) #sample one row
    db2 <- db[db$patch_area != sampled_db$patch_area,] #remove that row from the table
    while( sum(sampled_db$patch_area) < target_area) {
      sampled_db2 <- dplyr::sample_n(db2,1)
      sampled_db <- rbind(sampled_db,sampled_db2) #sample rows until target area, creating a database
      db2 <- db2[db2$patch_area != c(sampled_db2$patch_area),] #remove that row from the table
    }
    list_db[[i]] <-  sampled_db
  }
  #
  
  for (i in 1:n_db) {list_db[i][[1]] <- list_db[i][[1]][order(list_db[i][[1]]$patch_area),]}
  
  list_db <- unique(list_db)
  list_db <- rlist::list.filter(list_db, sum(patch_area) < (target_area + target_area*0.05) ) # a tolerance of 5% of the target area
  
  
  for (i in 1: length(list_db)) {
    list_db[[i]] <- as.data.frame(list_db[[i]]$dataset_and_patch)
    colnames(list_db[[i]]) <- "dataset_and_patch"
  }
  
  print(list_db)
  
}

## FUNCTION 2
# properties of patch combinations

properties_combination <- function(object) {
  LIST_SIM <- object  # No need for an unnecessary loop
  
  # Merge the simulated patch combinations with patch size and Occupancy data
  for (i in seq_along(LIST_SIM)) {
    for (j in seq_along(LIST_SIM[[i]])) {
      if (length(LIST_SIM[[i]]) > 1) {
        LIST_SIM[[i]][[j]] <- merge(LIST_SIM[[i]][[j]], patch_size, by = "dataset_and_patch")
      } else {
        LIST_SIM[[i]] <- NA
      }
    }
  }
  
  # Calculate the average patch areas in each patch combination
  LIST_SIM_area <- LIST_SIM
  
  for (i in seq_along(LIST_SIM_area)) {
    for (j in seq_along(LIST_SIM_area[[i]])) {
      if (any(!is.na(LIST_SIM_area[[i]]))) {
        LIST_SIM_area[[i]][[j]] <- psych::describe(LIST_SIM_area[[i]][[j]][, ncol(LIST_SIM_area[[i]][[j]])])
      }
    }
  }
  
  # Transform lists into tables
  for (i in seq_along(LIST_SIM_area)) {
    if (any(!is.na(LIST_SIM_area[[i]]))) {
      LIST_SIM_area[[i]] <- do.call(rbind.data.frame, LIST_SIM_area[[i]])
    }
  }
  
  # Finalize
  LIST_SIM_combined <- LIST_SIM_area
  
  for (i in seq_along(LIST_SIM_combined)) {
    if (any(!is.na(LIST_SIM_combined[[i]]))) {
      LIST_SIM_combined[[i]]$simulation_number <- rep(i, nrow(LIST_SIM_combined[[i]]))
      LIST_SIM_combined[[i]]$Dataset <- rep(names(patches_split[i]), nrow(LIST_SIM_combined[[i]]))
      LIST_SIM_combined[[i]]$patch_comb_number <- 1:nrow(LIST_SIM_combined[[i]])
    }
  }
  
  # Combine all studies into a table
  LIST_SIM_combined <- do.call(rbind, LIST_SIM_combined)
  LIST_SIM_combined <- na.omit(LIST_SIM_combined)
  LIST_SIM_combined <- LIST_SIM_combined[complete.cases(LIST_SIM_combined[, c("Dataset", "mean")]), ]
  
  return(LIST_SIM_combined)
}


## FUNCTION 3
# CALCULATE OCCUPANCY ACROSS PATCH COMBINATIONS

occurrence_combination <- function(object) {
  LIST_SIM <- object
  
  # Merge the simulated patch combinations with patch size and Occupancy data
  for (i in seq_along(LIST_SIM)) {
    for (j in seq_along(LIST_SIM[[i]])) {
      if (length(LIST_SIM[[i]]) > 1) {
        LIST_SIM[[i]][[j]] <- merge(LIST_SIM[[i]][[j]], patch_size, by = "dataset_and_patch")
      } else {
        LIST_SIM[[i]] <- NA
      }
    }
  }
  
  LIST_SIM_occ <- LIST_SIM
  for (i in seq_along(LIST_SIM_occ)) {
    for (j in seq_along(LIST_SIM_occ[[i]])) {
      if (any(!is.na(LIST_SIM_occ[[i]]))) {
        LIST_SIM_occ[[i]][[j]] <- sum(LIST_SIM_occ[[i]][[j]][,2])
      }
    }
  }
  
  for (i in seq_along(LIST_SIM_occ)) {
      if (any(!is.na(LIST_SIM_occ[[i]]))) {
        LIST_SIM_occ[[i]] <- do.call(rbind.data.frame, LIST_SIM_occ[[i]])
        colnames(LIST_SIM_occ[[i]]) <- "Cumulative_Occurrence"
    }
  }
  
  #####

  LIST_SIM_no <- LIST_SIM
  
  for (i in 1:length(LIST_SIM_no)){
    for (j in 1:length(LIST_SIM_no[[i]])){
        if (any(!is.na(LIST_SIM_no[[i]]))){
          # take the column of each table from column two to column n-1, take their sum, and count how many times their colsum is larger than 0
          LIST_SIM_no[[i]][[j]] <- nrow(LIST_SIM_no[[i]][[j]])
      }
    }
  }
  
  
  for (i in 1:length(LIST_SIM_no)){
      if (any(!is.na(LIST_SIM_no[[i]]))){
        LIST_SIM_no[[i]] <- do.call(rbind.data.frame, LIST_SIM_no[[i]])
        colnames(LIST_SIM_no[[i]]) <- "n"
      }
    }
  
  # Combine all results
  LIST_SIM_combined <- mapply(cbind, LIST_SIM_occ, LIST_SIM_no, SIMPLIFY = FALSE)
  
  for (i in seq_along(LIST_SIM_combined)) {
    if (any(!is.na(LIST_SIM_combined[[i]]))) {
      LIST_SIM_combined[[i]]$simulation_number <- rep(i, nrow(LIST_SIM_combined[[i]]))
      LIST_SIM_combined[[i]]$Dataset <- rep(names(patches_split[i]), nrow(LIST_SIM_combined[[i]]))
      LIST_SIM_combined[[i]]$patch_comb_number <- 1:nrow(LIST_SIM_combined[[i]])
    }
  }
  
  LIST_SIM_combined <- LIST_SIM_combined[sapply(LIST_SIM_combined, is.data.frame)]
  
  # Finalize output
  LIST_SIM_combined <- bind_rows(LIST_SIM_combined)
  # LIST_SIM_combined <- na.omit(LIST_SIM_combined)
  # LIST_SIM_combined <- LIST_SIM_combined[complete.cases(LIST_SIM_combined[, c("Dataset", "mean_patch_area")]), ]
  
  return(LIST_SIM_combined)
}
