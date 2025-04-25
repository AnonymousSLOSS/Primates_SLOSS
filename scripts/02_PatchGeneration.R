#!/usr/bin/env Rscript

#SBATCH --partition=milkun
#SBATCH --time=10:00:00
#SBATCH --output "log.out"
#SBATCH --mem=200G
#SBATCH --job-name=test
#SBATCH --ntasks-per-node=1
#SBATCH --cpus-per-task=1

###Above code for batch running on virtual SLURM CPU

#Any references to path files with vol are for linux virtual CPU (uncomment/comment as required)
.libPaths(c("")) #Folder of R packages for version of R being used remotely
.libPaths()

library(rlang)
library(dplyr)
library(landscapemetrics)
library(sf)
library(fasterize)
library(terra)
library(stringr)
options(scipen = 999) #prevent scientific notation in tables
set.seed(123) #set seed

#set options for terra package in cluster (uncomment/comment as required)
if(Sys.info()["sysname"]!="Windows"){
  terraOptions(memmax = 170, tempdir = "/vol/milkunC/tbeard/Article/temp", memfrac = 0.8)
}

################################################################################################
#########Import bespoke rasters from 01-script and project to UTM Web Mercator.#################
################################################################################################

###WARNING: For unknown reason, some TIF files may save incorrectly
###Duplicate file names appear e.g Forest2001_18-0001, Forest2001_18-0002, Forest2001_18-0003. Should be one file named Forest2001_18
###Remove all these files and access the correct TIF file. This one will be the correct raster for spatial point

###Now import raster files for both 50% and 75% Canopy Cover.

##50% canopy cover first

#Create an empty list to store the raster objects
Binary_list <- list()

#Set the directory where your raster files are located (if function for remote CPU use)
if(Sys.info()["sysname"]=="Windows"){
  directory <- paste0("Z:/tbeard/Article/Raster_Dataset")
}else{
  directory <- paste0("/vol/milkunC/tbeard/Article/Raster_Dataset")
}

directory <- paste0("C:/Users/Milkun2/Documents/Article/Raster_Dataset")

#Get a list of all the raster files in the directory
raster_files <- list.files(directory, pattern = "\\.tif$", recursive = TRUE, full.names = TRUE)

# Extract numeric part of file names
numeric_part <- as.numeric(gsub("[^0-9]", "", raster_files)) #for orgnansing rasters, remove any non-numbers from names

# #Sort raster files based on the numeric part
raster_files <- raster_files[order(numeric_part)]

#Loop through each raster file, read it, and add it to the list
#Note that rasters are in A-Z order of sample year and dataset grouped
for (file in raster_files) {
  Binary_list[[file]] <- rast(file)
}

###Rasters are in Coordinate System WGS84. Needs to be metric to use function for patches later. Convert to Web Mercator

Binary_WM <- list()

#loop reprojection
for (i in 1:length(Binary_list)) {
  crs(Binary_list[[i]]) <- "EPSG:4326"
  projection  <-  project(Binary_list[[i]], "EPSG:3857", method = "near", mask = TRUE, overwrite = TRUE)
  Binary_WM[[i]] <- projection
}

print(Binary_WM)

####################################################################################

##Now, 75% canopy cover

#Create an empty list to store the raster objects
Binary_list_75 <- list()

#Set the directory where your raster files are located

if(Sys.info()["sysname"]=="Windows"){
  directory <- paste0("Z:/tbeard/Article/Raster_Dataset_75")
}else{
  directory <- paste0("/vol/milkunC/tbeard/Article/Raster_Dataset_75")
}

#Get a list of all the raster files in the directory
raster_files <- list.files(directory, pattern = "\\.tif$", recursive = TRUE, full.names = TRUE)

#Extract numeric part of file names
numeric_part <- as.numeric(gsub("[^0-9]", "", raster_files)) #for orgnansing rasters, remove any non-numbers from names

##Sort raster files based on the numeric part
raster_files <- raster_files[order(numeric_part)]

#Loop through each raster file, read it, and add it to the list
#Note that rasters are in A-Z order of sample year and dataset grouped
for (file in raster_files) {
  Binary_list_75[[file]] <- rast(file)
}

##Rasters are in Coordinate System WGS84. Needs to be metric to use function for patches later. Convert to Web Mercator

Binary_WM_75 <- list()

#loop reprojection
for (i in 1:length(Binary_list_75)) {
  crs(Binary_list_75[[i]]) <- "EPSG:4326"
  projection <-  project(Binary_list_75[[i]], "EPSG:3857", method = "near", mask = TRUE, overwrite = TRUE)
  Binary_WM_75[[i]] <- projection
}

print(Binary_WM_75)

##############################################################################
#############Import XY points, project to UTM Web Mercator####################
##############################################################################

###First, import XY data and sort by year and then Dataset as this matches up with how rasters were created

#reset directory
if(Sys.info()["sysname"]=="Windows"){
  setwd("Z:/tbeard/Article")
}else{
  setwd("/vol/milkunC/tbeard/Article")
}

#Read in data and correct Biome values
primateocc <- read.csv("PrimateOccurrencev4.csv", header = TRUE)%>%
  mutate(BIOME = sub("^\\d+", "", BIOME)) #Correct BIOME values

#arrange and introduce row ID
primateocc <- primateocc %>% 
  arrange(Sample_yea, Dataset) %>%  
  mutate(row_id = row_number())


###Create new raster list with 50% and 75% canopy cover rasters combined as per Dinerstein et al

#Lookup for biome type and cancopy cover threshold to use
dinerstein_lookup <- read.csv('dinersteinlookup.csv', header = TRUE)

#Join CC thresholds 
primateocc <- primateocc %>%
  left_join(dinerstein_lookup, by = 'BIOME')

#Some Datasets have different biomes associated with 50% and 75% CC as per Dinerstein. 
#But we want a canopy cover threshold for each Dataset
#So we use the most common threshold within the spatial points of each Dataset
primateocc <- primateocc %>%
  group_by(Dataset) %>%
  mutate(CC_new = as.numeric(names(which.max(table(CC))))) %>%
  ungroup()

write.csv(primateocc, "PrimateOccurrencev4_Din.csv", row.names=FALSE)

################################################################################

###Create new raster list selecting the 50% or 75% raster for each Dataset depending on the threshold assigned to it

#First, create lookup table
CC_lookup <- primateocc %>% select(Sample_yea, Dataset, CC_new) %>% distinct()

#Create raster list with Dinerstein thresholds
Binary_WM_Din <- list()

for (i in 1:length(Binary_WM)){

  if (CC_lookup[i,]$CC_new == 50){
    Binary_WM_Din[[i]] <- Binary_WM[[i]]

  } else {

    Binary_WM_Din[[i]] <- Binary_WM_75[[i]]
  }
}


#clean environment. run if running main model with Dinerstein thresholds
rm(Binary_list, Binary_WM, Binary_list_75, Binary_WM_75)

#################################################################################

#Group database into lists of data frames grouped by Sample year & Dataset
primateocc_grp <- primateocc %>%
  arrange(Sample_yea, Dataset) %>%
  group_by(Sample_yea, Dataset) %>%
  group_split()

#Convert to a terra::vect object
points_terra_grp <- lapply(primateocc_grp, function(df) {
  terra::vect(df, geom = c("X", "Y"), crs = "EPSG:4326")
})

#Project to Web Mercator
points_WM_grp <- lapply(points_terra_grp, function(df) {
  terra::project(df, "EPSG:3857")
})

#Extract new coordinates to data frame
points_df_grp <- lapply(points_WM_grp, function(df) {
  # Extract the coordinates as a matrix
  coordinates <- terra::crds(df)
  
  # Convert to a data frame
  data.frame(
    X = coordinates[, 1],
    Y = coordinates[, 2]
  )
})

print(points_df_grp)

###########################################################################################
####################Extract cell value at XY point and see if located in a patch############
###########################################################################################

###Create 100km2 raster around each sampling point. 
#Outside of this bound, forest is assumed not to influence primate occupancy as not in its home range

buffer_list <- vector("list", length(points_WM_grp))

for (i in 1:length(points_WM_grp)){
  for (j in 1:length(points_WM_grp[[i]])){
    buffer_extent <- ext(points_WM_grp[[i]][j]) + 5000
    buffer_list[[i]][[j]] = crop(Binary_WM_Din[[i]], buffer_extent)
  }
}

#Extract the raster value at each XY coordinate. 1 = FOREST, 0 = NOT FOREST

pixelvalue_list <- list()

#Loop through each point
for (i in 1:length(buffer_list)){
  
  pixelvalue <- data.frame(matrix(ncol = 2, nrow = 0))
  
  for (j in 1:length(buffer_list[[i]])){
    
    #Extract pixel value of point
    value <- terra::extract(buffer_list[[i]][[j]], points_df_grp[[i]][j,])
    colnames(value) <- c("ID", "treecover")
    
    #Append the result to the pixelvalue data frame
    pixelvalue <- rbind(pixelvalue, value) 
  }
  
  pixelvalue_list[[i]] <- pixelvalue %>% select(treecover)
}

#Create list of df's with XY coordinates and binary variable of whether point is located in forest
points_df_grp <- lapply(seq_along(points_df_grp), function(i) {
  cbind(points_df_grp[[i]], pixelvalue_list[[i]])
})

print(points_df_grp)

#######################################################################################
#################Extract Patch Sizes using landscapemetrics package####################
#######################################################################################

#Create function to get patch size
PATCH_SIZE <- function(number, list_points, raster_cover){
  prova <- list_points[number,c(1:2)]
  object <- extract_lsm(raster_cover, y = as.matrix(prova), what = "lsm_p_area", directions = 4)
  object$extract_id <- number
  return(object)
}

#Create list for results
patch_results_list <- list()

#Loop results into list
for (i in 1:length(points_df_grp)){
  
  patch_results_df <- data.frame()
  
  for(j in 1:nrow(points_df_grp[[i]])){
    patch <- PATCH_SIZE(j, list_points = points_df_grp[[i]], raster_cover = buffer_list[[i]][[j]])
    patch_results_df <- rbind(patch_results_df, patch)
  }
  patch_results_list[[i]] <- patch_results_df
}

#Add patch size associated with each XY point to results
patch_df_grp <- lapply(seq_along(patch_results_list), function(i) {
  cbind(patch_results_list[[i]], points_df_grp[[i]]) %>% 
    select(-layer,-level,-class,-extract_id,-X, -Y)
})


#Those located in 0 = NOT FOREST, change patch area values to NA. The values given are areas for the NOT FOREST the XY points are located in
#patch_df_grp <- lapply(patch_df_grp, function(df) {
#df$value[df$treecover == 0] <- NA  # Replace 0 with NA in treecover
#return(df)  # Return the modified data frame
# })

#################################################################################################################################
######################Find Nearest Forest Patch + Extract Distance to Patch + Extract Area of Nearest Patch######################
#################################################################################################################################

####We want to check for forest located within 500m of an XY point if it is not located in forest
####This is to allow for some geographic error

###Extract individual patch areas within each 100km2 raster around each XY point. 
###This is used later as a lookup for patch area values of neighbouring patches for those not located in forest.

ind_patch_area <- vector("list", length(buffer_list))

for(i in 1:length(buffer_list)){
  for(j in 1:length(buffer_list[[i]])){
    lsm_patch <- lsm_p_area(buffer_list[[i]][[j]], directions = 4)  
    ind_patch_area[[i]][[j]] <- lsm_patch
  }
}

###Get information on nearest patches to each XY point

#The nearest neighbour function in landscapemetrics can only calculate neighbours of same class.
#Therefore for our points located in Not Forest cells (cells with value 0), we need to temporarily pretend they are a single cell forest patch (so their cell value is 1).
#This is done by giving a proxy value to the cell where XY point is located

nearest_patch_list <- list()

for (i in 1:length(buffer_list)){
  
  nearest_patch_df <- data.frame(layer = integer(), id = integer(), dist = numeric(), id_neighbour = integer(), stringsAsFactors = FALSE)
  
  for (j in 1:length(buffer_list[[i]])){
    
    #some rasters are entirely NOT FOREST. create NA's for these cases and otherwise find neighbours
    if (global(buffer_list[[i]][[j]], fun = "max", na.rm = TRUE)[[1]] == 0) { 
      
      nearest_patch <- data.frame(layer = NA, id = NA, dist = NA, id_neighbour = NA, stringsAsFactors = FALSE)
      
    } else {
      
      #establish individual forest patches in each raster. returns a list that includes a raster whee each cell value is the patch id
      patches <- get_patches(landscape = buffer_list[[i]][[j]], class = 1, directions = 4, return_raster =TRUE) 
      
      #forest patch raster
      patch_raster <- patches$layer_1$class_1
      
      #Find the cell in patches raster containing occurrence coordinate
      xy_cell <- cellFromXY(patch_raster, as.matrix(points_df_grp[[i]][j,c(1,2)]))
      
      #Convert value of focal cell to random number greater than the maximum patch ID. Here, 10000 chosen. This temporarily makes the cell of forest type. This is necessary to compute nearest neighbour              
      patch_raster[xy_cell] <- 10000
      # row_id_value <- primateocc_grp[[i]][j, "row_id", drop = TRUE]
      
      #Get nearest neighbour. Gives patch ID of nearest neighbour and distance
      nearest_neighbour <- get_nearestneighbour(patch_raster, return_id = TRUE)
      
      nearest_patch <- nearest_neighbour %>% 
        filter(id == 10000) 
      # mutate(row_id = row_id_value)
    }
    
    #Bind the result to the data frame
    nearest_patch_df <- rbind(nearest_patch_df, nearest_patch)
    
  }
  
  nearest_patch_list[[i]] <- nearest_patch_df
}

##################################################################################

###Combine nearest patch and it's area into one row of data

nearest_p_area_list <- list()

for (i in 1:length(buffer_list)){
  
  nearest_p_area_df <- data.frame()
  
  for(j in 1:length(buffer_list[[i]])){
    
    if (global(buffer_list[[i]][[j]], fun = "max", na.rm = TRUE)[[1]] == 0) { #some rasters are entirely NOT FOREST. create NA's for these cases
      
      nearest_patch_area <- data.frame(nearest_patch_list[[i]][j,], value = NA)
      
    } else {
      
      #filter the tibbles of individual patches for the patch that has been derived as nearest neighbour
      nearest_patch_ID <- ind_patch_area[[i]][[j]] %>% 
        filter(id == nearest_patch_list[[i]][j,]$id_neighbour)
      
      #add nearest patch area to nearest patch information
      nearest_patch_area <- data.frame(nearest_patch_list[[i]][j,], value = nearest_patch_ID$value)
    }
    
    #Bind
    nearest_p_area_df <- rbind(nearest_p_area_df, nearest_patch_area)
  }
  
  nearest_p_area_list[[i]] <- nearest_p_area_df
  
}


###########################################################################################################
##############Combine into one large data frame with initial csv and patch information#####################
###########################################################################################################

primatepatches_grp <- lapply(seq_along(primateocc_grp), function(i) {
  data.frame(
    primateocc_grp[[i]], 
    patch_df_grp[[i]], 
    near_p_dist = nearest_p_area_list[[i]]$dist, 
    near_p_area = nearest_p_area_list[[i]]$value
  )
})

primatepatches_grp <- lapply(primatepatches_grp, function(df) {
  df %>%
    mutate(
      near_p_dist = if_else(treecover == 1, 0, near_p_dist),  #Set near_p_dist to 0 where treecover is 1, else keep existing value (including NA)
      near_p_area = if_else(treecover == 1, NA, near_p_area),  #Set near_p_area to NA where treecover is 1, else keep existing value (including NA)
      patch_area = if_else(treecover == 0, near_p_area, value),  #Set patch_area to near_p_area where treecover is 0, else keep the original value
      dataset_row_id = row_number()
    ) %>%
    select(-near_p_area, -value) %>%
    rename(nearpatch_ID = id)
})

#Remove 1) rows of data with NA's in nearest_p_area_df as these rasters are entirely NOT FOREST
#Remove 2) rows of data with near_p_dist < 500 metres as these points considered geographically inaccurate

max_dist_to_patch <- 500

#Filter out both NA's and those with forest >500m away
primatepatches_grp_filt <- lapply(primatepatches_grp, function(df) {
  df %>%                
    filter(near_p_dist <= max_dist_to_patch)
})

#Filter buffer_list of the removed indices
buffer_list_filt <- lapply(seq_along(buffer_list), function(i) {
  # Indices of rows to keep (those not in `removed_rows`)
  retained_indices <- primatepatches_grp_filt[[i]]$dataset_row_id
  
  # Subset buffer_list[[i]] to retain only corresponding rasters
  buffer_list[[i]][retained_indices]
})


#Filter points_WM_grp of the removed indices
points_WM_grp_filt <- lapply(seq_along(points_WM_grp), function(i) {
  #Indices of rows to keep
  retained_indices <- primatepatches_grp_filt[[i]]$dataset_row_id
  
  #Subset points_WM_grp[[i]] to retain only corresponding rasters
  points_WM_grp[[i]][retained_indices]
})

#Filter points_df_grp of the removed indices
points_df_grp_filt <- lapply(seq_along(points_df_grp), function(i) {
  # Indices of rows to keep
  retained_indices <- primatepatches_grp_filt[[i]]$dataset_row_id
  
  #Subset buffer_list[[i]] to retain only corresponding rasters of retained XY points
  points_df_grp[[i]][retained_indices,]
})

#Remove empty dataframes from the lists and adjust indexing
removed_indices <- which(sapply(primatepatches_grp_filt, function(df) nrow(df) == 0))
primatepatches_grp_filt <- primatepatches_grp_filt[-removed_indices]
points_WM_grp_filt <- points_WM_grp_filt[-removed_indices]
points_df_grp_filt <- points_df_grp_filt[-removed_indices]
buffer_list_filt <- buffer_list_filt[-removed_indices]
Binary_WM_filt <- Binary_WM_Din[-removed_indices]

# #clean environment
# filt_objects <- ls(envir = .GlobalEnv) %>%
#   .[!grepl("_filt$", .) & . != "Binary_WM_Din" & . != "primatepatches_grp"]
# rm(list = filt_objects, envir = .GlobalEnv)
# rm(filt_objects)
# 


######################################################################################
###################Identify overlapping patches in Datasets###########################
######################################################################################

####As Hansen maps are 30m resolution, it can have difficulty recognising separate patches when their separation is <30m
####This leaves many points with shared patches. We need to ensure 1 XY point to 1 patch. 
####Remove all but one XY point and associated patches for all cases where patches are shared (overlapping)

###Identify the patches that overlap in datasets

overlap_patches_dataset <- list()

for (i in 1:length(Binary_WM_filt)){

  raster <- Binary_WM_filt[[i]]     
  points <- points_df_grp_filt[[i]]
  nearest_neighbour_df <- data.frame(nearpatchID_dataset = integer(), stringsAsFactors = FALSE)
  
  ##Firstly establish the nearest patch ID of the points that are not located in Forest but within 500m of a patch
  
  #establish individual forest patches in each Dataset raster. returns a list that includes a raster where each cell value is the patch id
  patches <- get_patches(landscape = raster, class = 1, directions = 4, return_raster =TRUE) #patches
  
  #forest patch raster
  patch_raster <- patches$layer_1$class_1
  
  for (j in 1:nrow(points)){
    
    buffer_extent <- ext(points_WM_grp_filt[[i]][j,]) + 5000 #reduces extent of nearest_neighbour function later but still returns patchID at Dataset scale
    patch_raster_cut = crop(patch_raster, buffer_extent)
    
    
    if (points_df_grp_filt[[i]][j, "treecover"] == 1) {
      
      nearest_neighbour <- extract(patch_raster_cut, points_WM_grp_filt[[i]][j,]) %>% 
        select(lyr.1) %>% 
        rename(nearpatchID_dataset = lyr.1)
      
    } else {
      
      #Find the cell in patches raster containing occurrence coordinate
      xy_cell <- cellFromXY(patch_raster_cut, as.matrix(points[j,c(1,2)]))
      
      #Convert value of focal cell to random number greater than the maximum patch ID. Here, 10000 chosen. This temporarily makes the cell of forest type. This is necessary to compute nearest neighbour              
      patch_raster_cut[xy_cell] <- 1000000000
      # row_id_value <- primatepatches_list[[3]][j, "row_id", drop = TRUE]
      
      ##Get nearest neighbouring patches to points located in NOT FOREST. Outputs multiple neighbours if same distance
      nearest_neighbour <- get_nearestneighbour(patch_raster_cut, return_id = TRUE) %>% 
        filter(id == 1000000000 | id_neighbour == 1000000000) %>% 
        mutate(id1 = pmax(id,id_neighbour),
               id2 = pmin(id,id_neighbour)) %>%  
        select(id1,id2) %>%
        distinct()
      
      #Identify which neighbouring patch was used using the output of individual patch areas in main database
      lsm_patch <- lsm_p_area(patch_raster_cut, directions = 4) %>% 
        filter(round(value, 7) == round(primatepatches_grp_filt[[i]][j, "patch_area"], 7))
      
      #filter neighbouring patches by neighbour identified as being used
      nearest_neighbour <- nearest_neighbour %>% 
        filter(id2 %in% unique(lsm_patch$class)) %>% 
        select(id2) %>% 
        rename(nearpatchID_dataset = id2) %>% 
        slice_sample(n=1) #Final step if none of the code above can distinguish the single neighbour that has been sourced by individual patch areas in main database.
      
    }
    
    nearest_neighbour_df <- rbind(nearest_neighbour_df, nearest_neighbour)
    
  }
  
#Check if row counts match before combining
  if (nrow(primatepatches_grp_filt[[i]]) == nrow(nearest_neighbour_df)) {
    overlap_patches_dataset[[i]] <- cbind(primatepatches_grp_filt[[i]] %>%
                                            dplyr::select(Occurrence, row_id, patch_area, treecover, Dataset), nearest_neighbour_df)
  } else {
    
    message(paste0("Problem with dataset ", i))
    
    overlap_patches_dataset[[i]] <- data.frame(
      Occurrence = character(),
      row_id = integer(),
      patch_area = numeric(),
      treecover = integer(),
      Dataset = character(),
      nearpatchID_dataset = integer(),
      stringsAsFactors = FALSE)
  }
}

###Create binary matrices for each Dataset where 1 = overlap and 0 = no overlap

overlap_matrix_dataset <- list()

for (i in 1:length(overlap_patches_dataset)){
  
  if (nrow(overlap_patches_dataset[[i]]) == 0) {
    # If the data frame is empty, create a blank matrix (0x0 matrix)
    overlap_matrix_dataset[[i]] <- matrix(0, nrow = 0, ncol = 0)
  } else { 
   
  nearpatchID <- overlap_patches_dataset[[i]]$nearpatchID_dataset
  
  # Initialize the matrix with zeros
  n <- length(nearpatchID)
  matrix <- matrix(0, nrow = n, ncol = n)
  
  # Fill the matrix with 1 if nearpatchID is equal, otherwise 0
  for (j in 1:n) {
    for (k in 1:n) {
      matrix[j, k] <- ifelse(nearpatchID[j] == nearpatchID[k], 1, 0)
    }
  }
  
  overlap_matrix_dataset[[i]] = matrix
  }
}

###Create matricies within each dataset of overlapping 100km2 Extents.

overlap_matrix_extent <-  list()

#Initialize an empty matrix to store overlap results
for (i in 1:length(overlap_patches_dataset)){
  
  if (nrow(overlap_patches_dataset[[i]]) == 0) {
    #If the data frame is empty, create a blank matrix (0x0 matrix)
    overlap_matrix_extent[[i]] <- matrix(0, nrow = 0, ncol = 0)
  } else { 
    
  rasters <- buffer_list_filt[[i]]
  n <- length(rasters) # Number of SpatRasters
  matrix <- matrix(0, nrow = n, ncol = n)
  
  #Loop through pairs of rasters to check for overlap
  for (j in 1:n) {
    for (k in 1:n) {
      # Check if extents overlap (1 if they do, 0 otherwise)
      matrix[j, k] <- as.integer(!is.null(intersect(ext(rasters[[j]]), ext(rasters[[k]]))))
    }
  }
  overlap_matrix_extent[[i]] <- matrix
  }
}

###Final Overlap Matrices - Element-wise multiplication of matrices. 
###If two patches are overlapping at Dataset level and have overlapping Extents (100km2 bounding box), they are considered Overlapping. Otherwise Not Overlapping.

overlap_matrix_final <- list()  # Initialize the list to store the final matrices

#Loop through both lists and multiply corresponding matrices element-wise
for (i in 1:length(overlap_matrix_dataset)) {
  # Perform element-wise multiplication
  overlap_matrix_final[[i]] <- overlap_matrix_dataset[[i]] * overlap_matrix_extent[[i]]
}

#Name rows and columns according to row_id from filtered dataset
for (i in 1:length(overlap_matrix_final)) {
  
  if (nrow(overlap_patches_dataset[[i]]) == 0) {
    # If the data frame is empty, create a blank matrix (0x0 matrix)
    overlap_matrix_dataset[[i]] <- overlap_matrix_dataset[[i]]
    
  } else { 
  # Get the row_id for the current matrix in the final list
  row_ids <- primatepatches_grp_filt[[i]]$row_id
  
  # Assign row and column names to the matrix in overlap_matrix_final[[i]]
  colnames(overlap_matrix_final[[i]]) <- row_ids
  rownames(overlap_matrix_final[[i]]) <- row_ids
  }
}

#Function to check if a matrix is an identity matrix
is_identity <- function(mat) {
  if (ncol(mat) == 1 && nrow(mat) == 1) {
    return(mat[1, 1] == 1)  # True if the single element is 1
  }
  # Remove row and column names for comparison
  mat_unlabeled <- unname(as.matrix(mat))
  identity_mat <- diag(ncol(mat_unlabeled))
  identical(mat_unlabeled, identity_mat)
}

###Derive patches to be retained. Check patches that overlap i.e. mat[j,k] == 1 and retain only one at random, prioritising presence

set.seed(123)

patches_to_retain <- list()

for (i in 1:length(overlap_matrix_final)){
  
  if (nrow(overlap_matrix_final[[i]]) == 0) {
    #If the data frame is empty, create a blank matrix (0x0 matrix)
    patches_to_retain[[i]] <- data.frame(row_id = integer())
    
  } else { 
    
    mat <- overlap_matrix_final[[i]]
    
    #Initialize variables
    j <- 1
    
    while (!is_identity(mat)) {
      
      old_mat <- mat 
      df <- data.frame(row_id = integer())
      
      for (k in 1:ncol(mat)){
        if (mat[j, k] == 1) {
          row_id <- as.integer(colnames(mat)[k])
        } else {
          row_id <- NULL
        }
        df <- rbind(row_id, df)
      } 
      
      colnames(df) <- "row_id"
      occurrences_lookup <- primatepatches_grp_filt[[i]][, c("row_id", "Occurrence")]
      
      df_join <- df %>% 
        left_join(occurrences_lookup, by = "row_id") %>% 
        { 
          if (any(.$Occurrence == 1)) {
            filter(., Occurrence == 1)
          } else {
            .
          }
        } %>% 
        slice_sample(n = 1)
      
      df_remove <- df %>%
        filter(row_id != df_join$row_id)
      
      mat <- mat[!(rownames(mat) %in% df_remove$row_id), !(colnames(mat) %in% df_remove$row_id), drop = FALSE]
      
      #Adjust j
      if (ncol(old_mat) == ncol(mat)) {
        j <- j + 1
      }
      
      #Reset j to 1 if it exceeds ncol(mat)
      if (j >= ncol(mat)) {
        j <- 1
      }
    }
    patches_to_retain[[i]] <- as.data.frame(as.integer(rownames(mat)))
    colnames(patches_to_retain[[i]]) <- "row_id"
  }}


patches_to_retain_df <- do.call(rbind, patches_to_retain)

primatepatches_ori <- do.call(rbind, primatepatches_grp)
primatepatches_filt <- do.call(rbind, primatepatches_grp_filt)

primatepatches <- primatepatches_filt %>% filter(row_id %in% patches_to_retain_df$row_id)

#Final cleaning
min_n_per_study <- 5

primatepatches <- primatepatches %>%
  group_by(Dataset) %>% #Need to reapply filters as some datasets have lost patches and are now 5 or below or may contain all-presence and all-absence 
  filter(n() >= min_n_per_study) %>%
  filter(sum(Occurrence) > 0) %>% 
  filter(sum(Occurrence) < n()) %>% 
  ungroup() %>% 
  arrange(Dataset, patch_area) %>% #Finally arrange by dataset without sampling year and patch area
  mutate(row_id_2 = row_number()) #new row id

#Filter out Data Deficient species 
primatepatches <- primatepatches %>%  filter(redlistCategory != "Data Deficient")

write.csv(primatepatches, "Primate_Database_Patches.csv", row.names = FALSE)

save.image('Patches.RData')

##################################################################################################

