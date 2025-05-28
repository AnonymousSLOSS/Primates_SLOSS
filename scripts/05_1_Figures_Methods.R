library(ggplot2)
library(ggthemes)
# install.packages("ggeffects")
library(ggeffects)
library(ggpubr)
# install.packages("png")
library("png") 
library("ggplot2") 
# install.packages("patchwork")
library("patchwork") 
library("grid")
# install.packages("extrafont")
library(extrafont)
grDevices::pdfFonts(Arial = grDevices::postscriptFonts()$Arial)

######################################################################
############# Figure 1 generated in ArcGIS ###########################
######################################################################

###Below code used for deriving centroids of each Dataset for Figure 1 - produced in ArcGIS

#Set working directory

primatepatches <-  read.csv('03_Primate_Database_Patches.csv', header = TRUE)

primatepatches <- primatepatches %>%
  group_by(Dataset) %>%
  mutate(
    centroid_x = mean(X, na.rm = TRUE),
    centroid_y = mean(Y, na.rm = TRUE)
  ) %>%
  ungroup()


dataset_centroids <- primatepatches %>%
  select(Dataset, centroid_x, centroid_y) %>% 
  distinct()

write.csv(dataset_centroids, "dataset_centroids.csv", row.names = FALSE)

######################################################################
############# Figure 2 - Generation of patch combinations ############
######################################################################

#Generate data frame for Example Dataset plot

patch_data <- data.frame(
  label = c("H","I","J","D","E","F","G","A","B","C"),
  x = c(0.3, 1.05, 2.8, 0.2, 1.0, 1.75, 3.0, 0.5, 1.35, 2.3),   # +6
  y = c(0.9, 0.7, 0.8, 1.9, 2.0, 2.1, 1.6, 2.8, 2.95, 2.9),      # +2.7
  width = c(1,3,1,1,1,2,1,1,1,2),
  height = c(1,2,1,1,1,1,2,1,1,2)
)

patch_data_raw <- patch_data #used later

patch_data = patch_data %>% 
  mutate(newwidth = width/2,
         newheight = height/2)
  
  
patch_data <- data.frame(patch_data,
                         xmin = patch_data$x,
                         xmax = patch_data$x + patch_data$newwidth,
                         ymin = patch_data$y,
                         ymax = patch_data$y + patch_data$newheight)

#specify the primate image used to represent a presence
path <- "C:/..../monkey2.png"

#Read the png 
img <- readPNG(path, native = TRUE) 


#Signal patches with a presence
presence <- c("E","F","I")
presence_data <- patch_data[patch_data$label %in% presence, ]

#Plot the Example Dataset
dev.new(width = 30, height = 20, noRStudioGD = TRUE)

ggplot()+
  geom_rect(data = patch_data, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), fill = "chartreuse4") +
  annotation_custom(rasterGrob(img), xmin = presence_data[1,]$x + (presence_data[1,]$newwidth / 2) - 0.25, xmax = presence_data[1,]$x + (presence_data[1,]$newwidth / 2) + 0.25, ymin = presence_data[1,]$y + (presence_data[1,]$newheight /2) - 0.25, ymax = presence_data[1,]$y + (presence_data[1,]$newheight /2) + 0.25)+
  annotation_custom(rasterGrob(img), xmin = presence_data[2,]$x + (presence_data[2,]$newwidth / 2) - 0.25, xmax = presence_data[2,]$x + (presence_data[2,]$newwidth / 2) + 0.25, ymin = presence_data[2,]$y + (presence_data[2,]$newheight /2) - 0.25, ymax = presence_data[2,]$y + (presence_data[2,]$newheight /2) + 0.25)+
  annotation_custom(rasterGrob(img), xmin = presence_data[3,]$x + (presence_data[3,]$newwidth / 2) - 0.25, xmax = presence_data[3,]$x + (presence_data[3,]$newwidth / 2) + 0.25, ymin = presence_data[3,]$y + (presence_data[3,]$newheight /2) - 0.25, ymax = presence_data[3,]$y + (presence_data[3,]$newheight /2) + 0.25)+
  geom_text(data = patch_data, aes(label = label, x = x + newwidth / 2, y = y + newheight / 2), color = "white", size = 20) +
  annotate("text", x = 1.35, y = 0.3, label = "Presence of Primate", color = "black", family = "Arial", size = 10, hjust = 0) +
  geom_rect(aes(xmin = 0, xmax = 3.75, ymin = 0, ymax = 4.1), fill = NA, colour = "grey", linewidth = 2) +
  annotation_custom(rasterGrob(img), xmin = 0.75, xmax = 1.25, ymin = 0.1, ymax = 0.6) +
  theme_void() +
  coord_fixed(ratio = 1)

############################################################################

###Create chosen patch combinations from Example Dataset for 30%,50% and 70%.
###Need to scale them down to fit figure

##Scale the points about the centroid

#Calculate the centroid of each patch
centroid_x <- mean(patch_data_raw$x)
centroid_y <- mean(patch_data_raw$y)

#Scale function
scale_point <- function(x, y, centroid_x, centroid_y, scale) {
  new_x <- centroid_x + scale * (x - centroid_x)
  new_y <- centroid_y + scale * (y - centroid_y)
  return(c(new_x, new_y))
}

#Choose scale 
scale <- 0.375

#Apply scale to centroid of each patch
scaled_points <- t(apply(patch_data_raw[, c("x", "y")], 1, function(row) {
  scale_point(row[1], row[2], centroid_x, centroid_y, scale)
}))

#Create a new data frame with the scaled centroids and change width of patches from centroid point
scaled_patch <- patch_data_raw %>% 
  mutate(x = scaled_points[,1], 
         y = scaled_points[,2],
         newwidth = width/6,
         newheight = height/6)

#New extents of each patch
scaled_patch <- data.frame(scaled_patch,
                           xmin = scaled_patch$x,
                           xmax = scaled_patch$x + scaled_patch$newwidth,
                           ymin = scaled_patch$y,
                           ymax = scaled_patch$y + scaled_patch$newheight)

#Create border to scale down
rect <- data.frame(x = c(0, 3.75), y = c(0.5, 4.1))

#Apply scaling
scaled_points_rect <- t(apply(rect[, c("x", "y")], 1, function(row) {
  scale_point(row[1], row[2], centroid_x, centroid_y, scale)
}))

#Data frame of scaled border
scaled_rect <- rect %>% 
  mutate(x = scaled_points_rect[,1], 
         y = scaled_points_rect[,2])

#Select patch combinations representing 30%, 50% and 70%
thirty_pc <- c("A", "E", "C")
thirty_pc2 <- c("B", "D", "H", "J","G")
fifty_pc <- c("I", "C")
fifty_pc2 <- c("A", "E", "C", "B", "D", "F")
seventy_pc <- c("I","C","F","G")
seventy_pc2 <- c("I", "B", "D", "F", "H", "J", "G")

#Apply scalign to chosen patch combinations
thirty_patch <- scaled_patch[scaled_patch$label %in% thirty_pc, ]
thirty_patch2 <- scaled_patch[scaled_patch$label %in% thirty_pc2, ]
fifty_patch <- scaled_patch[scaled_patch$label %in% fifty_pc, ]
fifty_patch2 <- scaled_patch[scaled_patch$label %in% fifty_pc2, ]
seventy_patch <- scaled_patch[scaled_patch$label %in% seventy_pc, ]
seventy_patch2 <- scaled_patch[scaled_patch$label %in% seventy_pc2, ]

#Finalise patch combinations and borders and their position of x and y axis
#x and y values can be adjusted to fit both Example Dataset figure and Patch Combinations figure together

thirty_patch <- thirty_patch %>% 
  mutate(x = x + 3,
         y = y + 1.5,
         xmin = xmin + 3,
         xmax = xmax + 3,
         ymin = ymin + 1.5,
         ymax = ymax + 1.5)

thirty_rect <- scaled_rect %>% 
  mutate(x = x + 3, 
         y = y + 1.5)

thirty_patch2 <- thirty_patch2 %>% 
  mutate(x = x + 3,
         y = y - 0.5,
         xmin = xmin + 3,
         xmax = xmax + 3,
         ymin = ymin - 0.5,
         ymax = ymax - 0.5)

thirty_rect2 <- scaled_rect %>% 
  mutate(x = x + 3, 
         y = y - 0.5)

fifty_patch <- fifty_patch %>% 
  mutate(x = x + 5.25,
         y = y + 1.5,
         xmin = xmin + 5.25,
         xmax = xmax + 5.25,
         ymin = ymin + 1.5,
         ymax = ymax + 1.5)

fifty_rect <- scaled_rect %>% 
  mutate(x = x + 5.25, 
         y = y + 1.5)

fifty_patch2 <- fifty_patch2 %>% 
  mutate(x = x + 5.25,
         y = y - 0.5,
         xmin = xmin + 5.25,
         xmax = xmax + 5.25,
         ymin = ymin - 0.5,
         ymax = ymax - 0.5)

fifty_rect2 <- scaled_rect %>% 
  mutate(x = x + 5.25, 
         y = y - 0.5)

seventy_patch <- seventy_patch %>% 
  mutate(x = x + 7.5,
         y = y + 1.5,
         xmin = xmin + 7.5,
         xmax = xmax + 7.5,
         ymin = ymin + 1.5,
         ymax = ymax + 1.5)

seventy_rect <- scaled_rect %>% 
  mutate(x = x + 7.5, 
         y = y + 1.5)

seventy_patch2 <- seventy_patch2 %>% 
  mutate(x = x + 7.5,
         y = y - 0.5,
         xmin = xmin + 7.5,
         xmax = xmax + 7.5,
         ymin = ymin - 0.5,
         ymax = ymax - 0.5)

seventy_rect2 <- scaled_rect %>% 
  mutate(x = x + 7.5, 
         y = y - 0.5)

#Plot patch combinations of Example Dataset

dev.new()

  ggplot()+
  
  ###Option to plot Example Dataset and patch combinations together
  # geom_rect(data = patch_data, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), fill = "chartreuse4") +
  # annotation_custom(rasterGrob(img), xmin = presence_data[1,]$x + (presence_data[1,]$newwidth / 2) - 0.25, xmax = presence_data[1,]$x + (presence_data[1,]$newwidth / 2) + 0.25, ymin = presence_data[1,]$y + (presence_data[1,]$newheight /2) - 0.25, ymax = presence_data[1,]$y + (presence_data[1,]$newheight /2) + 0.25)+
  # annotation_custom(rasterGrob(img), xmin = presence_data[2,]$x + (presence_data[2,]$newwidth / 2) - 0.25, xmax = presence_data[2,]$x + (presence_data[2,]$newwidth / 2) + 0.25, ymin = presence_data[2,]$y + (presence_data[2,]$newheight /2) - 0.25, ymax = presence_data[2,]$y + (presence_data[2,]$newheight /2) + 0.25)+
  # annotation_custom(rasterGrob(img), xmin = presence_data[3,]$x + (presence_data[3,]$newwidth / 2) - 0.25, xmax = presence_data[3,]$x + (presence_data[3,]$newwidth / 2) + 0.25, ymin = presence_data[3,]$y + (presence_data[3,]$newheight /2) - 0.25, ymax = presence_data[3,]$y + (presence_data[3,]$newheight /2) + 0.25)+
  # geom_text(data = patch_data, aes(label = label, x = x + newwidth / 2, y = y + newheight / 2), color = "white", size = 5) +
  # annotate("text", x = 1.175, y = 0.3, label = "Presence of Primate", color = "black", size = 5, hjust = 0) +
  # geom_rect(aes(xmin = 0, xmax = 3.75, ymin = 0, ymax = 4.1), fill = NA, colour = "grey", linewidth = 1.5) +
  # annotation_custom(rasterGrob(img), xmin = 0.6, xmax = 1.1, ymin = 0.1, ymax = 0.6) +
  # # theme_void() +
  # coord_fixed(ratio = 1) + # Ensure aspect ratio is equal

  geom_rect(data = thirty_patch, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), fill = "chartreuse4", linewidth = 1.5) +
  annotation_custom(rasterGrob(img), xmin = thirty_patch[1,]$x + (thirty_patch[1,]$newwidth / 2) - 0.15, xmax = thirty_patch[1,]$x + (thirty_patch[1,]$newwidth / 2) + 0.15, ymin = thirty_patch[1,]$y + (thirty_patch[1,]$newheight /2) - 0.15, ymax = thirty_patch[1,]$y + (thirty_patch[1,]$newheight /2) + 0.20)+
  geom_text(data = thirty_patch, aes(label = label, x = x + newwidth / 2, y = y + newheight / 2), color = "white", size = 7) +
  geom_rect(aes(xmin = thirty_rect[1,1], xmax = thirty_rect[2,1], ymin = thirty_rect[1,2], ymax = thirty_rect[2,2]), fill = NA, colour = "blue", linewidth = 1.5) +
  
  geom_rect(data = thirty_patch2, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), fill = "chartreuse4", linewidth = 1.5) +
  geom_text(data = thirty_patch2, aes(label = label, x = x + newwidth / 2, y = y + newheight / 2), color = "white", size = 7) +
  geom_rect(aes(xmin = thirty_rect2[1,1], xmax = thirty_rect2[2,1], ymin = thirty_rect2[1,2], ymax = thirty_rect2[2,2]), fill = NA, colour = "red", linewidth = 1.5) +
  
  geom_rect(data = fifty_patch, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), fill = "chartreuse4", linewidth = 1.5) +
  annotation_custom(rasterGrob(img), xmin = fifty_patch[1,]$x + (fifty_patch[1,]$newwidth / 2) - 0.15, xmax = fifty_patch[1,]$x + (fifty_patch[1,]$newwidth / 2) + 0.15, ymin = fifty_patch[1,]$y + (fifty_patch[1,]$newheight /2) - 0.15, ymax = fifty_patch[1,]$y + (fifty_patch[1,]$newheight /2) + 0.20)+
  geom_text(data = fifty_patch, aes(label = label, x = x + newwidth / 2, y = y + newheight / 2), color = "white", size = 7) +
  geom_rect(aes(xmin = fifty_rect[1,1], xmax = fifty_rect[2,1], ymin = fifty_rect[1,2], ymax = fifty_rect[2,2]), fill = NA, colour = "blue", linewidth = 1.5) +

  geom_rect(data = fifty_patch2, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), fill = "chartreuse4", linewidth = 1.5) +
  annotation_custom(rasterGrob(img), xmin = fifty_patch2[2,]$x + (fifty_patch2[2,]$newwidth / 2) - 0.15, xmax = fifty_patch2[2,]$x + (fifty_patch2[2,]$newwidth / 2) + 0.15, ymin = fifty_patch2[2,]$y + (fifty_patch2[2,]$newheight /2) - 0.15, ymax = fifty_patch2[2,]$y + (fifty_patch2[2,]$newheight /2) + 0.20)+
  annotation_custom(rasterGrob(img), xmin = fifty_patch2[3,]$x + (fifty_patch2[3,]$newwidth / 2) - 0.15, xmax = fifty_patch2[3,]$x + (fifty_patch2[3,]$newwidth / 2) + 0.15, ymin = fifty_patch2[3,]$y + (fifty_patch2[3,]$newheight /2) - 0.15, ymax = fifty_patch2[3,]$y + (fifty_patch2[3,]$newheight /2) + 0.20)+
  geom_text(data = fifty_patch2, aes(label = label, x = x + newwidth / 2, y = y + newheight / 2), color = "white", size = 7) +
  geom_rect(aes(xmin = fifty_rect2[1,1], xmax = fifty_rect2[2,1], ymin = fifty_rect2[1,2], ymax = fifty_rect2[2,2]), fill = NA, colour = "blue", linewidth = 1.5) +
  
  geom_rect(data = seventy_patch, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), fill = "chartreuse4", linewidth = 1.5) +
  annotation_custom(rasterGrob(img), xmin = seventy_patch[1,]$x + (seventy_patch[1,]$newwidth / 2) - 0.15, xmax = seventy_patch[1,]$x + (seventy_patch[1,]$newwidth / 2) + 0.15, ymin = seventy_patch[1,]$y + (seventy_patch[1,]$newheight /2) - 0.15, ymax = seventy_patch[1,]$y + (seventy_patch[1,]$newheight /2) + 0.20)+
  annotation_custom(rasterGrob(img), xmin = seventy_patch[2,]$x + (seventy_patch[2,]$newwidth / 2) - 0.15, xmax = seventy_patch[2,]$x + (seventy_patch[2,]$newwidth / 2) + 0.15, ymin = seventy_patch[2,]$y + (seventy_patch[2,]$newheight /2) - 0.15, ymax = seventy_patch[2,]$y + (seventy_patch[2,]$newheight /2) + 0.20)+
  geom_text(data = seventy_patch, aes(label = label, x = x + newwidth / 2, y = y + newheight / 2), color = "white", size = 7) +
  geom_rect(aes(xmin = seventy_rect[1,1], xmax = seventy_rect[2,1], ymin = seventy_rect[1,2], ymax = seventy_rect[2,2]), fill = NA, colour = "blue", linewidth = 1.5) +
  
  geom_rect(data = seventy_patch2, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), fill = "chartreuse4", linewidth = 1.5) +
  # annotation_custom(rasterGrob(img), xmin = seventy_patch2[6,]$x + (seventy_patch2[6,]$newwidth / 2) - 0.15, xmax = seventy_patch2[6,]$x + (seventy_patch2[6,]$newwidth / 2) + 0.15, ymin = seventy_patch2[6,]$y + (seventy_patch2[6,]$newheight /2) - 0.15, ymax = seventy_patch2[6,]$y + (seventy_patch2[6,]$newheight /2) + 0.20)+
  annotation_custom(rasterGrob(img), xmin = seventy_patch2[2,]$x + (seventy_patch2[2,]$newwidth / 2) - 0.15, xmax = seventy_patch2[2,]$x + (seventy_patch2[2,]$newwidth / 2) + 0.15, ymin = seventy_patch2[2,]$y + (seventy_patch2[2,]$newheight /2) - 0.15, ymax = seventy_patch2[2,]$y + (seventy_patch2[2,]$newheight /2) + 0.20)+
  geom_text(data = seventy_patch2, aes(label = label, x = x + newwidth / 2, y = y + newheight / 2), color = "white", size = 7) +
  geom_rect(aes(xmin = seventy_rect2[1,1], xmax = seventy_rect2[2,1], ymin = seventy_rect2[1,2], ymax = seventy_rect2[2,2]), fill = NA, colour = "blue", linewidth = 1.5)+

  theme_void() +
  coord_fixed(ratio = 1)

#####################################################################################

#########################################################################
############### Figure 3 - Predictions and Methods ######################
#########################################################################
  
###Figure 3 a), b), c) - individual 30% forest amount patch combinations for representing MPS change
#Run code for Figure 2 first
  
  #Other example patch combinations for figure3d
  thirty_pc3 <- c("I")
  thirty_pc4 <- c("B", "D", "A", "F","J")
  thirty_pc5 <- c("C", "G")
  thirty_pc6 <- c("B", "D", "G", "H","J")
  thirty_pc7 <- c("B", "C", "D") 
  thirty_pc8 <- c("A", "B", "F", "J","H")
  
  #Other example patch combinations scaled down
  thirty_patch3 <- scaled_patch[scaled_patch$label %in% thirty_pc3, ]
  thirty_patch4 <- scaled_patch[scaled_patch$label %in% thirty_pc4, ]
  thirty_patch5 <- scaled_patch[scaled_patch$label %in% thirty_pc5, ]
  thirty_patch6 <- scaled_patch[scaled_patch$label %in% thirty_pc6, ]
  thirty_patch7 <- scaled_patch[scaled_patch$label %in% thirty_pc7, ]
  thirty_patch8 <- scaled_patch[scaled_patch$label %in% thirty_pc8, ]
  
dev.new()

ggplot()+
  geom_rect(data = thirty_patch2, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), fill = "chartreuse4", linewidth = 1) +
  geom_text(data = thirty_patch2, aes(label = label, x = x + newwidth / 2, y = y + newheight / 2), color = "white", size = 24) +
  geom_rect(aes(xmin = thirty_rect2[1,1], xmax = thirty_rect2[2,1], ymin = thirty_rect2[1,2], ymax = thirty_rect2[2,2]), fill = NA, colour = "grey", linewidth = 2) +
  theme_void() +
  coord_fixed(ratio = 1)

ggplot()+
  geom_rect(data = thirty_patch, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), fill = "chartreuse4", linewidth = 1) +
  annotation_custom(rasterGrob(img), xmin = thirty_patch[1,]$x + (thirty_patch[1,]$newwidth / 2) - 0.15, xmax = thirty_patch[1,]$x + (thirty_patch[1,]$newwidth / 2) + 0.15, ymin = thirty_patch[1,]$y + (thirty_patch[1,]$newheight /2) - 0.15, ymax = thirty_patch[1,]$y + (thirty_patch[1,]$newheight /2) + 0.2)+
  geom_text(data = thirty_patch, aes(label = label, x = x + newwidth / 2, y = y + newheight / 2), color = "white", size = 24) +
  geom_rect(aes(xmin = thirty_rect[1,1], xmax = thirty_rect[2,1], ymin = thirty_rect[1,2], ymax = thirty_rect[2,2]), fill = NA, colour = "grey", linewidth = 2) +
  theme_void() +
  coord_fixed(ratio = 1)

ggplot()+
  geom_rect(data = thirty_patch3, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), fill = "chartreuse4", linewidth = 1) +
  annotation_custom(rasterGrob(img), xmin = thirty_patch3[1,]$x + (thirty_patch3[1,]$newwidth / 2) - 0.15, xmax = thirty_patch3[1,]$x + (thirty_patch3[1,]$newwidth / 2) + 0.15, ymin = thirty_patch3[1,]$y + (thirty_patch3[1,]$newheight /2) - 0.15, ymax = thirty_patch3[1,]$y + (thirty_patch3[1,]$newheight /2) + 0.2)+
  geom_text(data = thirty_patch3, aes(label = label, x = x + newwidth / 2, y = y + newheight / 2), color = "white", size = 24) +
  geom_rect(aes(xmin = scaled_rect[1,1], xmax = scaled_rect[2,1], ymin = scaled_rect[1,2], ymax = scaled_rect[2,2]), fill = NA, colour = "grey", linewidth = 2) +
  theme_void() +
  coord_fixed(ratio = 1)

###Figure 3 d) - individual 30% forest amount patch combinations for representing Direct Comparison
#Run code for Figure 2 first

ggplot()+
  geom_rect(data = thirty_patch2, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), fill = "chartreuse4", linewidth = 1) +
  geom_text(data = thirty_patch2, aes(label = label, x = x + newwidth / 2, y = y + newheight / 2), color = "white", size = 24) +
  geom_rect(aes(xmin = thirty_rect2[1,1], xmax = thirty_rect2[2,1], ymin = thirty_rect2[1,2], ymax = thirty_rect2[2,2]), fill = NA, colour = "red", linewidth = 2) +
  theme_void() +
  coord_fixed(ratio = 1)

ggplot()+
  geom_rect(data = thirty_patch, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), fill = "chartreuse4", linewidth = 1) +
  annotation_custom(rasterGrob(img), xmin = thirty_patch[1,]$x + (thirty_patch[1,]$newwidth / 2) - 0.15, xmax = thirty_patch[1,]$x + (thirty_patch[1,]$newwidth / 2) + 0.15, ymin = thirty_patch[1,]$y + (thirty_patch[1,]$newheight /2) - 0.15, ymax = thirty_patch[1,]$y + (thirty_patch[1,]$newheight /2) + 0.20)+
  geom_text(data = thirty_patch, aes(label = label, x = x + newwidth / 2, y = y + newheight / 2), color = "white", size = 24) +
  geom_rect(aes(xmin = thirty_rect[1,1], xmax = thirty_rect[2,1], ymin = thirty_rect[1,2], ymax = thirty_rect[2,2]), fill = NA, colour = "blue", linewidth = 2) +
  theme_void() +
  coord_fixed(ratio = 1)

ggplot()+
  geom_rect(data = thirty_patch3, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), fill = "chartreuse4", linewidth = 1) +
  annotation_custom(rasterGrob(img), xmin = thirty_patch3[1,]$x + (thirty_patch3[1,]$newwidth / 2) - 0.15, xmax = thirty_patch3[1,]$x + (thirty_patch3[1,]$newwidth / 2) + 0.15, ymin = thirty_patch3[1,]$y + (thirty_patch3[1,]$newheight /2) - 0.15, ymax = thirty_patch3[1,]$y + (thirty_patch3[1,]$newheight /2) + 0.20)+
  geom_text(data = thirty_patch3, aes(label = label, x = x + newwidth / 2, y = y + newheight / 2), color = "white", size = 24) +
  geom_rect(aes(xmin = scaled_rect[1,1], xmax = scaled_rect[2,1], ymin = scaled_rect[1,2], ymax = scaled_rect[2,2]), fill = NA, colour = "blue", linewidth = 2) +
  theme_void() +
  coord_fixed(ratio = 1)

ggplot()+
  geom_rect(data = thirty_patch4, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), fill = "chartreuse4", linewidth = 1) +
  annotation_custom(rasterGrob(img), xmin = thirty_patch4[3,]$x + (thirty_patch4[3,]$newwidth / 2) - 0.15, xmax = thirty_patch4[3,]$x + (thirty_patch4[3,]$newwidth / 2) + 0.15, ymin = thirty_patch4[3,]$y + (thirty_patch4[3,]$newheight /2) - 0.15, ymax = thirty_patch4[3,]$y + (thirty_patch4[3,]$newheight /2) + 0.20)+
  geom_text(data = thirty_patch4, aes(label = label, x = x + newwidth / 2, y = y + newheight / 2), color = "white", size = 24) +
  geom_rect(aes(xmin = scaled_rect[1,1], xmax = scaled_rect[2,1], ymin = scaled_rect[1,2], ymax = scaled_rect[2,2]), fill = NA, colour = "blue", linewidth = 2) +
  theme_void() +
  coord_fixed(ratio = 1)

ggplot()+
  geom_rect(data = thirty_patch5, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), fill = "chartreuse4", linewidth = 1) +
  geom_text(data = thirty_patch5, aes(label = label, x = x + newwidth / 2, y = y + newheight / 2), color = "white", size = 24) +
  geom_rect(aes(xmin = scaled_rect[1,1], xmax = scaled_rect[2,1], ymin = scaled_rect[1,2], ymax = scaled_rect[2,2]), fill = NA, colour = "red", linewidth = 2) +
  theme_void() +
  coord_fixed(ratio = 1)

ggplot()+
  geom_rect(data = thirty_patch6, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), fill = "chartreuse4", linewidth = 1) +
  geom_text(data = thirty_patch6, aes(label = label, x = x + newwidth / 2, y = y + newheight / 2), color = "white", size = 24) +
  geom_rect(aes(xmin = scaled_rect[1,1], xmax = scaled_rect[2,1], ymin = scaled_rect[1,2], ymax = scaled_rect[2,2]), fill = NA, colour = "red", linewidth = 2) +
  theme_void() +
  coord_fixed(ratio = 1)

ggplot()+
  geom_rect(data = thirty_patch7, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), fill = "chartreuse4", linewidth = 1) +
  geom_text(data = thirty_patch7, aes(label = label, x = x + newwidth / 2, y = y + newheight / 2), color = "white", size = 24) +
  geom_rect(aes(xmin = scaled_rect[1,1], xmax = scaled_rect[2,1], ymin = scaled_rect[1,2], ymax = scaled_rect[2,2]), fill = NA, colour = "red", linewidth = 2) +
  theme_void() +
  coord_fixed(ratio = 1)

ggplot()+
  geom_rect(data = thirty_patch8, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), fill = "chartreuse4", linewidth = 1) +
  annotation_custom(rasterGrob(img), xmin = thirty_patch8[3,]$x + (thirty_patch8[3,]$newwidth / 2) - 0.15, xmax = thirty_patch8[3,]$x + (thirty_patch8[3,]$newwidth / 2) + 0.15, ymin = thirty_patch8[3,]$y + (thirty_patch8[3,]$newheight /2) - 0.15, ymax = thirty_patch8[3,]$y + (thirty_patch8[3,]$newheight /2) + 0.2)+
  geom_text(data = thirty_patch8, aes(label = label, x = x + newwidth / 2, y = y + newheight / 2), color = "white", size = 24) +
  geom_rect(aes(xmin = scaled_rect[1,1], xmax = scaled_rect[2,1], ymin = scaled_rect[1,2], ymax = scaled_rect[2,2]), fill = NA, colour = "blue", linewidth = 2) +
  theme_void() +
  coord_fixed(ratio = 1)

###Figure 3b - Global prediction for MPS method

#Create data frame for global prediction line
globalline <- data.frame(
  x= c(0.1,0.65),
  y= c(0.35,0.75), 
  redlist = rep("Global Model", times = 2))

#Plot global prediction
dev.new(width=10, length=10, noRStudioGD = FALSE)

ggplot(data = globalline, aes(x = x, y = y)) +
  geom_line(data = globalline, aes(x=x, y=y), col = "black", linewidth = 4) +
  labs(x = "Mean Patch Size",
       y = "Occupancy")+
  scale_x_continuous(limits = c(0, 0.8)) + # Set x-axis limits
  scale_y_continuous(limits = c(0, 0.9), breaks = c(0,1)) + # Set y-axis limits
  theme_minimal() +
  theme(
    axis.title.x = element_text(size = 38, face = "bold", margin = margin(t = -15), hjust = 0.425), # Increase gap for x-axis title
    axis.title.y = element_text(size = 38, face = "bold", margin = margin(r = -70), hjust = 0.4), # Increase gap for y-axis title
    axis.text.x = element_blank(), # Remove axis text (labels)
    axis.line = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank())+
  guides(col = FALSE)+
  geom_segment(aes(x = 0.04, y = 0, xend = 0.70, yend = 0), linewidth = 1, arrow = arrow(type = "closed", length = unit(0.1, "inches")), color = "black") + # Add x-axis with arrow
  geom_segment(aes(x = 0.04, y = 0, xend = 0.04, yend = 0.85), linewidth = 1, arrow = arrow(type = "closed", length = unit(0.1, "inches")), color = "black") 

###Figure 3c - IUCN prediction for MPS method

#Define colours for IUCN categories (using IUCN colours)
order_redlist <- c("Least Concern", "Near Threatened", "Vulnerable", "Endangered", "C. Endangered") 
redlist_colors <- c("#60C659", "#CCE226", "#F9E814", "#FC7F3F", "#D81E05")

#Create data frame for IUCN prediction lines
prediction_df <- data.frame(
  x = c(rep(0.10, 5),rep(0.65, 5)),
  y = c(c(0.7, 0.575, 0.425, 0.25, 0.05), seq(0.8, 0.6, by = -0.05)),
  redlist = rep(order_redlist2, times = 2))

prediction_df$redlist <- factor(prediction2_df$redlist, levels = order_redlist2)

#Plot IUCN prediction
dev.new(width=10, length=10, noRStudioGD = FALSE)

ggplot(data = prediction2_df, aes(x = x, y = y, col = redlist)) +
  geom_line(linewidth = 4) +
  # geom_line(data = globalline, aes(x=x, y=y, linetype = "Global Trend"), col = "black", linewidth = 4) +
  # geom_text(x = 0.099, y = 0.33, label = "Increased", size = 7, color = "black") + # Add the label
  # geom_text(x = 0.099, y = 0.30, label = "Extinction", size = 7, color = "black") + # Add the label below
  # geom_text(x = 0.095, y = 0.27, label = "Risk In", size = 7, color = "black") + 
  # geom_text(x = 0.095, y = 0.24, label = "Smaller", size = 7, color = "black") +
  # geom_text(x = 0.095, y = 0.21, label = "Patches", size = 7, color = "black") +
  labs(x = "Mean Patch Size",
       y = "Occupancy",
       col = "IUCN Status",
       linetype = NULL) +  # Add legend title for linetype
  scale_color_manual(values = redlist_colors) +
  # scale_linetype_manual(values = c("Global Trend" = "solid")) + 
  scale_x_continuous(limits = c(0, 0.8)) + # Set x-axis limits
  scale_y_continuous(limits = c(0, 0.9), breaks = c(0,1)) + # Set y-axis limits
  theme_minimal() +
  theme(
    axis.title.x = element_text(size = 38, face = "bold", margin = margin(t = -15), hjust = 0.425), # Increase gap for x-axis title
    axis.title.y = element_text(size = 38, face = "bold", margin = margin(r = -70), hjust = 0.4), # Increase gap for y-axis title
    axis.text.x = element_blank(), # Remove axis text (labels)
    axis.line = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    legend.position = c(0.72, 0.27),
    legend.text = element_text(size = 30),
    legend.title = element_text(size = 34, face = "bold"),
    legend.key.height = unit(2.15, "lines"),
    legend.box = "vertical") + # Position legends vertically
  guides(col = guide_legend(order = 1),
         linetype = guide_legend(order = 2)) + # Order legends
  geom_segment(aes(x = 0.04, y = 0, xend = 0.70, yend = 0), linewidth = 1, arrow = arrow(type = "closed", length = unit(0.1, "inches")), color = "black") + # Add x-axis with arrow
  geom_segment(aes(x = 0.04, y = 0, xend = 0.04, yend = 0.85), linewidth = 1, arrow = arrow(type = "closed", length = unit(0.1, "inches")), color = "black") # Add y-axis with arrow


###Figure 3d - show direct comparison outcomes in Direct Comparison method

##Create data frames for different scenarios under the Direct Comparison method

#SL>SS

SL_better <- data.frame(
  label = c("A","E","C","B","D","H","J","G"),
  x = c(0,0,0.5,2.5,2.5,3,3,3.5),
  y = c(0,0.5,0,0,0.5,0,0.5,0),
  width = c(1,1,2,1,1,1,1,1),
  height = c(1,1,2,1,1,1,1,2))

SL_better = SL_better %>% 
  mutate(x = x + 0.5,
         y = y - 2.5,
         newwidth = width/2,
         newheight = height/2)

SL_better <- data.frame(SL_better,
                        xmin = SL_better$x,
                        xmax = SL_better$x + SL_better$newwidth,
                        ymin = SL_better$y,
                        ymax = SL_better$y + SL_better$newheight)

presence_data_2 <- SL_better[SL_better$label %in% presence, ]

#Equal 1

Equal <- data.frame(
  label = c("I","B","D","F","A","J"),
  x = c(0,2.5,3,2.5,3.5,3.5),
  y = c(-1.25,-0.75,-0.75,-1.25,-0.75,-1.25),
  width = c(3,1,1,2,1,1),
  height = c(2,1,1,1,1,1))

Equal = Equal %>% 
  mutate(x = x + 0.5,
         y = y - 2.5,
         newwidth = width/2,
         newheight = height/2)

Equal <- data.frame(Equal,
                    xmin = Equal$x,
                    xmax = Equal$x + Equal$newwidth,
                    ymin = Equal$y,
                    ymax = Equal$y + Equal$newheight)

presence_data_3 <- Equal[Equal$label %in% presence, ]

#Equal 2

Equal2 <- data.frame(
  label = c("G","C","B","D","G","H","J"),
  x = c(0,0.5,2.5,3,3.5,2.5,3),
  y = c(-2.5,-2.5,-2,-2,-2.5,-2.5,-2.5),
  width = c(1,2,1,1,1,1,1),
  height = c(2,2,1,1,2,1,1))

Equal2 = Equal2 %>% 
  mutate(x = x + 0.5,
         y = y - 2.5,
         newwidth = width/2,
         newheight = height/2)


Equal2 <- data.frame(Equal2,
                     xmin = Equal2$x,
                     xmax = Equal2$x + Equal2$newwidth,
                     ymin = Equal2$y,
                     ymax = Equal2$y + Equal2$newheight)

presence_data_4 <- Equal2[Equal2$label %in% presence, ]

#SS>SL

SS_better <- data.frame(
  label = c("B","D","C","A","F","B","H","J"),
  x = c(0,0,0.5,2.5,2.5,3.5,3,3.5),
  y = c(-3.75,-3.25,-3.75,-3.75,-3.25,-3.25,-3.75,-3.75),
  width = c(1,1,2,1,2,1,1,1),
  height = c(1,1,2,1,1,1,1,1))

SS_better = SS_better %>% 
  mutate(x = x + 0.5,
         y = y - 2.5,
         newwidth = width/2,
         newheight = height/2)

SS_better <- data.frame(SS_better,
                        xmin = SS_better$x,
                        xmax = SS_better$x + SS_better$newwidth,
                        ymin = SS_better$y,
                        ymax = SS_better$y + SS_better$newheight)


presence_data_5 <- SS_better[SS_better$label %in% presence, ]

#Plot comparisons for us in (d)
dev.new(width = 30, height = 20, noRStudioGD = TRUE)

ggplot()+
  
  geom_rect(data = SL_better, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), fill = "chartreuse4", color= "white", linewidth = 1) +
  annotation_custom(rasterGrob(img), xmin = presence_data_2$x + (presence_data_2$newwidth / 2) - 0.25, xmax = presence_data_2$x + (presence_data_2$newwidth / 2) + 0.25, ymin = presence_data_2$y + (presence_data_2$newheight /2) - 0.25, ymax = presence_data_2$y + (presence_data_2$newheight /2) + 0.25)+
  geom_text(data = SL_better, aes(label = label, x = x + newwidth / 2, y = y + newheight / 2), color = "white", size = 8) +
  
  geom_rect(data = Equal, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), fill = "chartreuse4", color= "white", linewidth = 1) +
  annotation_custom(rasterGrob(img), xmin = presence_data_3[1,]$x + (presence_data_3[1,]$newwidth / 2) - 0.25, xmax = presence_data_3[1,]$x + (presence_data_3[1,]$newwidth / 2) + 0.25, ymin = presence_data_3[1,]$y + (presence_data_3[1,]$newheight /2) - 0.25, ymax = presence_data_3[1,]$y + (presence_data_3[1,]$newheight /2) + 0.25)+
  annotation_custom(rasterGrob(img), xmin = presence_data_3[2,]$x + (presence_data_3[2,]$newwidth / 2) - 0.25, xmax = presence_data_3[2,]$x + (presence_data_3[2,]$newwidth / 2) + 0.25, ymin = presence_data_3[2,]$y + (presence_data_3[2,]$newheight /2) - 0.25, ymax = presence_data_3[2,]$y + (presence_data_3[2,]$newheight /2) + 0.25)+
  geom_text(data = Equal, aes(label = label, x = x + newwidth / 2, y = y + newheight / 2), color = "white", size = 8) +
  
  geom_rect(data = Equal2, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), fill = "chartreuse4", color= "white", linewidth = 1) +
  geom_text(data = Equal2, aes(label = label, x = x + newwidth / 2, y = y + newheight / 2), color = "white", size = 8) +
  
  geom_rect(data = SS_better, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), fill = "chartreuse4", color= "white", linewidth = 1) +
  annotation_custom(rasterGrob(img), xmin = presence_data_5[1,]$x + (presence_data_5[1,]$newwidth / 2) - 0.25, xmax = presence_data_5[1,]$x + (presence_data_5[1,]$newwidth / 2) + 0.25, ymin = presence_data_5[1,]$y + (presence_data_5[1,]$newheight /2) - 0.25, ymax = presence_data_5[1,]$y + (presence_data_5[1,]$newheight /2) + 0.25)+
  geom_text(data = SS_better, aes(label = label, x = x + newwidth / 2, y = y + newheight / 2), color = "white", size = 8) +
  
  theme_void() +
  coord_fixed(ratio = 1)

##Extra text annotations if desired (but used other software for this)
  #   annotate("text", x = 0.95, y = -1.05, label = "SL", color = "black", size = 8, hjust = 0) + # Adding legend text
  #   annotate("text", x = 3.4, y = -1.05, label = "SS", color = "black", size = 8, hjust = 0) + # Adding legend text
  #   annotate("text", x = 5.05, y = -1.05, label = "Result", color = "black", size = 5, hjust = 0) + # Adding legend text
  # # 
  #   annotate("text", x = 5, y = -2, label = "SL>SS", color = "black", size = 5, hjust = 0) + 
  #   annotate("text", x = 5, y = -3.5, label = "SL=SS", color = "black", size = 5, hjust = 0)+  
  #   annotate("text", x = 5, y = -5, label = "SL=SS", color = "black", size = 5, hjust = 0)+  
  #   annotate("text", x = 5, y = -6.5, label = "SS>SL", color = "black", size = 5, hjust = 0) + 
  
  # annotate("text", x = 2.2, y = -2.1, label = "VS", color = "black", size = 4, hjust = 0) + # Adding legend text
  # annotate("text", x = 2.2, y = -3.51, label = "VS", color = "black", size = 4, hjust = 0)+ # Adding legend text
  # annotate("text", x = 2.2, y = -5.01, label = "VS", color = "black", size = 4, hjust = 0) + # Adding legend text
  # annotate("text", x = 2.2, y = -6.51, label = "VS", color = "black", size = 4, hjust = 0) + # Adding legend text
  
  # annotate("text", x = -0.5, y = 1, label = "i)", color = "black", size = 8, hjust = 0) +
  # annotate("text", x = -0.5, y = -1.5, label = "ii)", color = "black", size = 8, hjust = 0)+  
  # annotate("text", x = -0.5, y = -4, label = "iii)", color = "black", size = 8, hjust = 0)+  
  # annotate("text", x = -0.5, y = -6.5, label = "iv)", color = "black", size = 8, hjust = 0) +
  
  # geom_point(aes(x=-6.2,y=-6), size = 6, shape = 4, color = "red") +  # Adding legend


