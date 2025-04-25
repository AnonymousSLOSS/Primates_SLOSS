###Initialise rgee package. Required for 1st time use
###Must be using R version of 4.2.3 or earlier. Rgee not supported after that. 
###Eventually this package may become unusable. It is already temperamental to activate
###Please covert code to be used in ...... Python package if this is the case

remove.packages("reticulate")
install.packages("reticulate")
library(reticulate)
install.packages("rgee")
library(rgee)
install.packages(c("geojsonio","googledrive")) #package dependancies of rgee when generating images
library(geojsonio)
library(googledrive)

#Install Google Cloud SDK https://cloud.google.com/sdk/docs/install and follow instructions in Command box and connect through google account

#Set python path
py_discover_config()

#Set global parameter to google cloud SDK file location. Edit accordingly. Download Google Cloud SDK online
Sys.setenv("EARTHENGINE_GCLOUD" = "C:/Users/Milkun2/AppData/Roaming/Microsoft/Windows/Start Menu/Programs/Google Cloud SDK/google-cloud-sdk/bin/")


#############################################

###Reintialise - required every subsequent use of rgee

library(reticulate)
library(rgee)
library(geojsonio) #package dependancies of rgee when generating images
library(googledrive) #package dependancies of rgee when generating images

#Attempt Reintialise - should fail
ee_Initialize()
reticulate::py_install("earthengine-api", envname = "r-reticulate")
ee_install()

#Restart R!!!
ee_Initialize() #follow steps that pop up

#Re-authenicate
ee$Authenticate(auth_mode='notebook')

#Reinitilise
ee$Initialize(project='mr-beardo-5')  # <-- EDIT THIS FOR YOUR PROJECT. You will need to create project first at https://code.earthengine.google.com/


#The authorization workflow will generate a code, which you should paste in the console where requested.
#e.g Enter verification code: 4/1AeaYSHBwzG8HlbMWPdMO-GxU1QXCNAeFIXht-CBKqOvt5xW5MA_7eKj6kQQ
#Following message should be received: "Successfully saved authorization token."
#You may also be asked other questions with no obvious response. Respond with "1", press Enter, and follow steps


######################################################################################################

#The following code uses a mix of R and Java Script API language to instruct geomatic commands to the Google Earth Engine servers
#It virtually imports a UMD file, specifically Global Forest Cover Change map (Hansen et al., 2013) 
#It then carries out commands to derive GEE Images of forest area for years 2000-2017.
#These virtual Images are then used to extract .tif rasters of dataset spatial extents

library(dplyr)
library(sp)
library(sf)
library(geojsonsf)
library(stars)
library(future)

################################################################################

###Extract tree cover for year 2000 (base year for forest change)

#Source Hansen global forest change map. 
forest_cover <- ee$Image("UMD/hansen/global_forest_change_2023_v1_11")

#Set canopy cover percentage minimum per pixel  
cc <- 50

#Select band for tree cover in year 2000 and apply minimum canopy cover percentage
canopy_cover_50 <- forest_cover$select('treecover2000') %>%
  ee$Image$gte(cc) %>%
  ee$Image$selfMask()

#Visualise in R Viewer if desired
# Map$addLayer(canopy_cover_50, list(palette = c('#96ED89')), 'tree cover: minArea')

#Define and extract a minimum pixel area to be considered forest
pixels <-  6
losspixels <- 6
contArea <- canopy_cover_50$connectedPixelCount() #find connected pixels for whole canopy cover layer
minArea <-  contArea$gte(pixels)$selfMask() #applies rule to mask any connections with less than 6 pixels connected

# Map$addLayer(minArea, list(palette = c('#96ED89')), 'tree cover: minArea')

##################################################################################

###Extract tree cover loss for years 2000 to 2017. Band = lossyear
treeLoss <-  forest_cover$select('lossyear') #values of 'lossyear' range from 1-22 representing years 2001-2022

#Create a list to store all treeLoss layers
treeLoss_list <- list()

#Handle the first iteration separately
treeLoss1 <- treeLoss$eq(1)$selfMask() #equals 1 = year 2001
treeLoss_list[["treeLoss1"]] <- treeLoss1


for (i in 2:17) {
  #Generate cumulative tree loss for each year
  treeLoss_loop <- treeLoss$gte(1)$updateMask(treeLoss$lte(i))
  
  #Append the tree loss layer to the list
  treeLoss_list[[paste0("treeLoss", i)]] <- treeLoss_loop
}

#Map$addLayer(treeLoss_list[["treeLoss1"]], list(palette = c('green')), 'TreeLoss2001')
#Map$addLayer(treeLoss_list[["treeLoss15"]], list(palette = c('green')), 'TreeLoss2015')

##################################################################################

###Create list for tree cover loss given minimum area conditions on Canopy Cover

#For remaining years
treecoverLoss_list <- list()

for (i in 1:17){
  year <- 2000 + i
  #Generate tree loss with minimum conditions for each year range
  treecoverLoss = minArea$rename(paste0('loss', year))$updateMask(treeLoss_list[[i]])
  #Append the tree loss layer to the list
  treecoverLoss_list[[paste0("treecoverLoss", i)]] <- treecoverLoss
}

##################################################################################

###Compute no. of connect pixels for forest loss
contLoss_list <-  list()
  for (i in 1:17){
    contLoss <- treecoverLoss_list[[i]]$connectedPixelCount()
    contLoss_list[[paste0("contLoss", i)]] <- contLoss
  }

##################################################################################

###Retain only forest loss that has a minimum of 6 connected pixels

minLoss_list <- list()
  for (i in 1:17){
    minLoss <- contLoss_list[[i]]$gte(losspixels)$selfMask()
    minLoss_list[[paste0("minLoss", i)]] <- minLoss
  }

##################################################################################

###Use tree loss to generate images of forest not lost

notLoss_list <- list()
  for (i in 1:17){
    year <- 2000 + i
    notLoss <- minLoss_list[[i]]$unmask()$select(paste0('loss', year))$eq(0)$selfMask()
    notLoss_list[[paste0("notLoss", i)]] <- notLoss
  } 

##################################################################################

###Forest cover each year with minimum area and forest loss conditions

ForestCover_list <-  list()

ForestCover_list[["ForestCover2000"]] <- minArea

  for (i in 1:18){
    year <- 2000 + (i-1)
    Forest <- if (i == 1) {minArea} else {minArea$updateMask(notLoss_list[[i-1]])} 
    ForestCover_list[[paste0('ForestCover', year)]] <- Forest
  }

#Map$addLayer(ForestCover_list[["ForestCover2011"]], list(palette = c('green')), 'ForestCover2001')
#Map$addLayer(ForestCover_list[[15]], list(palette = c('green')), 'ForestCover2015')

##################################################################################

###Reapply minimum area requirement to new layer
ForestMinArea_list <-  list()

  for (i in 1:18){
    year = 2000 + (i-1)
    ForestMinArea = ForestCover_list[[i]]$connectedPixelCount()$gte(pixels)$rename(paste0('treecover', year))
    ForestMinArea_list[[paste0('ForestMinArea', year)]] <- ForestMinArea
  }

####################################################################################

###Repeat for 75% canopy cover

#Set canopy cover percentage minimum per pixel  
cc_75 <- 75

#Select band for tree cover in year 2000 and apply minimum canopy cover percentage
canopy_cover_75 <- forest_cover$select('treecover2000') %>%
  ee$Image$gte(cc_75) %>%
  ee$Image$selfMask()

#Visualise in R Viewer if desired
#Map$addLayer(canopy_cover_50, list(palette = c('#96ED89')), 'tree cover: minArea')

#Define and extract a minimum pixel area to be considered forest
pixels <-  6
losspixels <- 6
contArea_75 <- canopy_cover_75$connectedPixelCount() #find connected pixels for whole canopy cover layer
minArea_75 <-  contArea_75$gte(pixels)$selfMask() #applies rule to mask any connections with less than 6 pixels connected

#Map$addLayer(minArea, list(palette = c('#96ED89')), 'tree cover: minArea')

##################################################################################

###Extract tree cover loss for years 2000 to 2017. Band = lossyear
treeLoss <-  forest_cover$select('lossyear') #values from from 1-22 representing years 2001-2017

#Create a list to store all treeLoss layers
treeLoss_list <- list()

#Handle the first iteration separately
treeLoss1 <- treeLoss$eq(1)$selfMask()
treeLoss_list[["treeLoss1"]] <- treeLoss1


for (i in 2:17) {
  #Generate tree loss for each year range
  treeLoss_loop <- treeLoss$gte(1)$updateMask(treeLoss$lte(i))
  
  #Append the tree loss layer to the list
  treeLoss_list[[paste0("treeLoss", i)]] <- treeLoss_loop
}

#Map$addLayer(treeLoss_list[["treeLoss1"]], list(palette = c('green')), 'TreeLoss2001')
#Map$addLayer(treeLoss_list[["treeLoss15"]], list(palette = c('green')), 'TreeLoss2015')

##################################################################################

###Create list for tree cover loss given minimum area conditions on Canopy Cover

#For remaining years
treecoverLoss_list_75 <- list()

for (i in 1:17){
  year <- 2000 + i
  #Generate tree loss with minimum conditions for each year range
  treecoverLoss = minArea_75$rename(paste0('loss', year))$updateMask(treeLoss_list[[i]])
  #Append the tree loss layer to the list
  treecoverLoss_list_75[[paste0("treecoverLoss", i)]] <- treecoverLoss
}

##################################################################################

###Compute no. of connect pixels for forest loss
contLoss_list_75 <-  list()
for (i in 1:17){
  contLoss <- treecoverLoss_list_75[[i]]$connectedPixelCount()
  contLoss_list_75[[paste0("contLoss", i)]] <- contLoss
}

##################################################################################

###Retain only forest loss that has a minimum of 6 connected pixels

minLoss_list_75 <- list()
for (i in 1:17){
  minLoss <- contLoss_list_75[[i]]$gte(losspixels)$selfMask()
  minLoss_list_75[[paste0("minLoss", i)]] <- minLoss
}

##################################################################################

###Use tree loss to generate images of forest not lost

notLoss_list_75 <- list()
for (i in 1:17){
  year <- 2000 + i
  notLoss <- minLoss_list_75[[i]]$unmask()$select(paste0('loss', year))$eq(0)$selfMask()
  notLoss_list_75[[paste0("notLoss", i)]] <- notLoss
} 

##################################################################################

###Forest cover each year with minimum area and forest loss conditions

ForestCover_list_75 <-  list()

ForestCover_list_75[["ForestCover2000"]] <- minArea

for (i in 1:18){
  year <- 2000 + (i-1)
  Forest <- if (i == 1) {minArea_75} else {minArea_75$updateMask(notLoss_list[[i-1]])} 
  ForestCover_list_75[[paste0('ForestCover', year)]] <- Forest
}

##################################################################################

###Reapply minimum area requirement to new layer
ForestMinArea_list_75 <-  list()

for (i in 1:18){
  year = 2000 + (i-1)
  ForestMinArea = ForestCover_list_75[[i]]$connectedPixelCount()$gte(pixels)$rename(paste0('treecover', year))
  ForestMinArea_list_75[[paste0('ForestMinArea', year)]] <- ForestMinArea
}

####################################################################################

###Read in Database for computing spatial extents of Datasets for creation of bespoke forest rasters. Encoding set to latin1 to deal with special symbols 

primateocc <- read.csv('Primate_Database_Initial.csv', header = TRUE, fileEncoding = "latin1")

primateocc <- primateocc %>% arrange(Sample_yea, Dataset) #should already be arranged from 00-script but make sure

#Create spatial extents to extract from forest map per unique combination of Sample Year and Dataset. This saves on computer power needed to download entire global Hansen map
spatial_extent <- primateocc %>%
  group_by(Sample_yea, Dataset) %>%
  summarise(
    xmin = min(X) - 0.2,
    xmax = max(X) + 0.2,
    ymin = min(Y) - 0.2,
    ymax = max(Y) + 0.2,
    .groups = 'drop'
  ) %>%
  rowwise() %>%
  mutate(
    #Create a rectangular polygon using the boundary coordinates
    geometry = st_sfc(st_polygon(list(matrix(
      c(xmin, ymin, 
        xmax, ymin, 
        xmax, ymax, 
        xmin, ymax, 
        xmin, ymin), 
      ncol = 2, byrow = TRUE))))
  ) %>%
  st_as_sf(crs = 4326)  #Specify the coordinate reference system (CRS) as WGS84

#create list of tibbles
spatial_extent_list <- split(spatial_extent, seq(nrow(spatial_extent)))

#Convert spatial extents of Datasets to feature collection 
primate_ee_list <- list()

# Iterate through each sf dataframe
for (i in 1:length(spatial_extent_list)) {
  
  #Extract the geometry from the sf dataframe
  geometry_sf <- spatial_extent_list[[i]]$geometry[[1]]
  
  #Convert sf geometry to an Earth Engine Geometry object
  geometry_ee <- sf_as_ee(geometry_sf)
  
  #Create a FeatureCollection from the geometry
  primate_ee_list[[i]] <- ee$FeatureCollection(geometry_ee)
}

#Map$addLayer(ForestMinArea_list[[2]], list(palette = c('green')), 'Forest')+ 
#Map$addLayer(dataset_feature, list(palette = c('black')), paste0(dataset_chosen)) 
#Map$centerObject(dataset_feature)

################################################################################

###Create sub-folders as containers for raster images

#First for 50% canopy cover 

#Set the base directory for 50% canopy cover
base_dir <- "......................../Raster_Dataset"

#Define the range of years
years <- 1995:2017

#Loop through each year and create a subfolder of rasters for each year
for (year in years) {
  folder_name <- paste0("Raster", year)
  full_path <- file.path(base_dir, folder_name)
  dir.create(full_path, recursive = TRUE, showWarnings = FALSE)
}

#Repeat for 75% canopy cover

#Set the base directory for 75% canopy cover
base_dir <- "C:/......................../Raster_Dataset_75"

#Define the range of years
years <- 1995:2017

#Loop through each year and create a subfolder of rasters for each year
for (year in years) {
  folder_name <- paste0("Raster", year,"_75")
  full_path <- file.path(base_dir, folder_name)
  dir.create(full_path, recursive = TRUE, showWarnings = FALSE)
}


################################################################################

###Generate raster images from Feature Collections

#Need to turn on a link with your Google Drive as this is where .tif files will save to.
ee_Initialize(drive = TRUE) 

#Generate rasters for 50% canopy cover with ee_as_rast function

ee_Initialize(drive = TRUE)

for(i in 1:length(primate_ee_list)){
  
  setwd("C:/........................../Raster_Dataset") #Set directory to global directory of Rasters
  
  bounding_box <- primate_ee_list[[i]]
  bbox <- bounding_box$toList(bounding_box$size())$get(0) #Extract individual polygon from FeatureCollection.
  geometrybbox <- ee$Feature(bbox)$geometry() #Extract geometry of individual polygon
  
  #Get the year based on the index
  year <- spatial_extent$Sample_yea[i]
  
  #Use outer tryCatch to handle any general errors
  tryCatch({
    
    #Nested tryCatch specifically for ee_as_rast
    img <- tryCatch({
      ee_as_rast(
        image = if (year < 2000) {
          ForestMinArea_list[['ForestMinArea2000']]
        } else {
          ForestMinArea_list[[paste0('ForestMinArea', year)]]
        },
        region = geometrybbox,
        via = "drive",
        dsn = paste0("Raster_Dataset_", i),
        container = paste0("Raster", year),
        timePrefix = FALSE,
        maxPixels = 1e13,
        scale = 30,
        add_metadata = TRUE
      )
    }, error = function(e) {
      #Handle errors specific to ee_as_rast function call
      print(paste("Error in ee_as_rast for index:", i, "with year:", year))
      print(e) #Print specific error message from ee_as_rast
      NULL #Return NULL so outer tryCatch can continue
    })
    
    #If img was created successfully, print success message
    if (!is.null(img)) {
      print(paste("Processed raster for index:", i, "with year:", year))
    }
    
  }, error = function(e) {
    # Handle other errors in outer tryCatch
    print(paste("Outer error caught for index:", i, "with year:", year))
    print(e) # Print outer error message
  })
  
  Sys.sleep(1) # Pause between iterations to avoid rate limits or API issues
}

#Generate rasters for 75% canopy cover with ee_as_rast function

ee_Initialize(drive = TRUE)

for(i in 1:length(primate_ee_list)){
  
  setwd("C:/.........................../Raster_Dataset_75") #Set directory to folder for each year. 
  
  bounding_box <- primate_ee_list[[i]]
  bbox <- bounding_box$toList(bounding_box$size())$get(0) #Extract individual polygon from FeatureCollection.
  geometrybbox <- ee$Feature(bbox)$geometry() #Extract geometry of individual polygon
  
  #Get the year based on the index
  year <- spatial_extent$Sample_yea[i]
  
  #Use outer tryCatch to handle any general errors
  tryCatch({
    
    #Nested tryCatch specifically for ee_as_rast
    img <- tryCatch({
      ee_as_rast(
        image = if (year < 2000) {
          ForestMinArea_list_75[['ForestMinArea2000']]
        } else {
          ForestMinArea_list_75[[paste0('ForestMinArea', year)]]
        },
        region = geometrybbox,
        via = "drive",
        dsn = paste0("Raster_Dataset_75_", i),
        container = paste0("Raster",year,"_75"),
        timePrefix = FALSE,
        maxPixels = 1e13,
        scale = 30,
        add_metadata = TRUE
      )
    }, error = function(e) {
      # Handle errors specific to ee_as_rast function call
      print(paste("Error in ee_as_rast for index:", i, "with year:", year))
      print(e) # Print specific error message from ee_as_rast
      NULL # Return NULL so outer tryCatch can continue
    })
    
    # If img was created successfully, print success message
    if (!is.null(img)) {
      print(paste("Processed raster for index:", i, "with year:", year))
    }
    
  }, error = function(e) {
    # Handle other errors in outer tryCatch
    print(paste("Outer error caught for index:", i, "with year:", year))
    print(e) # Print outer error message
  })
  
  Sys.sleep(1) # Pause between iterations to avoid rate limits or API issues
}

###You will now have the .tif raster files in both your Google Drive and (hopefully) in your local files.
###As the package is not maintained, the local saving of files broke for me. But they can be downloaded from Drive.