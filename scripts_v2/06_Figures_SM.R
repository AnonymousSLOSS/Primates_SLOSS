library(dplyr)
library(ggpubr)
library(grid)
library(ggplot2)
library(ggthemes)
library(viridis)
library(brms)
library(posterior)
library(tidyr)
library(bayesplot)

#load in objects from script 04_BrmModel.R
load("brm_Din_reduced.RData")
load("brm_Din_reduced_50.Rdata") #If this is how output of brm model with 50% canopy cover threshold was saved
load("brm_Din_reduced_75.Rdata") #If this is how output of brm model with 75% canopy cover threshold was saved

patch_comb_stats_50 <- read.csv("04_Primate_Database_PatchCombinations_Reduced_50.csv", header = TRUE)
patch_comb_stats_75 <- read.csv("04_Primate_Database_PatchCombinations_Reduced_75.csv", header = TRUE)
patch_comb_stats_Din <- read.csv("04_Primate_Database_PatchCombinations_Reduced.csv", header = TRUE)

#Arrange order of habitat area for factorisation
order <- c("twenty_percent", "thirty_percent", "forty_percent", "fifty_percent", "sixty_percent", "seventy_percent", "eighty_percent")  
order_redlist <- c("Least Concern", "Near Threatened", "Vulnerable", "Endangered", "Critically Endangered")
order_threatened <- c("Non-Threatened", "Threatened") 


###############################################################
########## Figure S2 - Bayesian Diagnostics ###################
###############################################################

###Figure S2a

summary(brm_occ_Din)

###Figure S2b

yrep <- posterior_predict(brm_occ_Din)
y_obs <- brm_occ_Din$data$Occurrence

ppc_dens_overlay(y = y_obs, yrep = yrep[1:500, ])


###############################################################
########## Figure S3 - Patch Size Distribution ################
###############################################################

#Set working directory

primatepatches_Din <- read.csv('03_Primate_Database_Patches_Reduced.csv', header = TRUE)

primatepatches_Din$patch_area_log <- log10(primatepatches_Din$patch_area)

primatepatches_Din <- primatepatches_Din %>% 
  mutate(redlistCategory = factor(redlistCategory, levels = order_redlist))

patchdist <-  read.csv('04_PatchDistribution_Reduced.csv', header = TRUE)
patchdist$redlistCategory = factor(patchdist$redlistCategory, levels = order_redlist)

#Create histogram for patch size distribution
full_hist_patch <- ggplot(primatepatches_Din, aes(x = patch_area_log)) +
  geom_histogram(fill = "chartreuse4", color = "black") +
  xlab("Patch Size (ha)") + 
  ylab("Number of patches") +
  scale_x_continuous(breaks = c(-2, -1, 0, 1, 2, 3, 4),
                     labels = c("0.01","0.1","1","10","100","1,000","10,000"))+
  theme(axis.title.x = element_text(margin = margin(t = 10)),  # Adjust margin for x-axis label
        axis.title.y = element_text(margin = margin(r = 10)))+  # Adjust margin for y-axis label
  geom_text(data = patchdist[6,], aes(x = -1.8, y = Inf, label = paste("Species =", Species, "\nDatasets =", Datasets, "\nPatches =", Patches)),
            vjust = 1.1, hjust = 0, size = 4, inherit.aes = FALSE, check_overlap = TRUE)+
  theme_few()

#Create histograms for IUCN categories
facet_hist_patch <- ggplot(primatepatches_Din, aes(x = patch_area_log, fill = redlistCategory)) +
  geom_histogram(fill = "chartreuse4", color = "black") +
  facet_wrap(~redlistCategory) +
  xlab("Patch Size (ha)") + 
  ylab("Number of patches") +
  scale_x_continuous(breaks = c(-2, -1, 0, 1, 2, 3, 4),
                     labels = c("0.01","0.1","1","10","100","1,000","10,000")) +
  theme(axis.title.x = element_text(margin = margin(t = 10)),  # Adjust margin for x-axis label
        axis.title.y = element_text(margin = margin(r = 10)))+ # Adjust margin for y-axis label
  geom_text(data = patchdist[c(1:5),], aes(x = -1.8, y = Inf, label = paste("Species =", Species, "\nDatasets =", Datasets, "\nPatches =", Patches)),
            vjust = 1.1, hjust = 0, size = 4, inherit.aes = FALSE, check_overlap = TRUE)+
  theme_few()

#Combine plots
dev.new(width = 10, length  = 20, noRStudioGD = TRUE)
ggarrange(full_hist_patch, facet_hist_patch, ncol = 1, nrow = 2, labels = c("(a)", "(b)"), font.label = list(size = 16), vjust = c(1.5, 0.5), hjust = c(-0.9,-0.9))

##########################################################################
########Figure S4 - Patch Combinations Mean Patch Size Distribution#######
##########################################################################

patch_comb_stats_Din <- read.csv('04_Primate_Database_PatchCombinations_Reduced.csv', header = TRUE)
patch_comb_stats_Din$redlistCategory = factor(patch_comb_stats_Din$redlistCategory, levels = order_redlist)
patch_comb_dist <-  read.csv('04_PatchCombinationsDistribution_Reduced.csv', header = TRUE)
patch_comb_dist$redlistCategory = factor(patch_comb_dist$redlistCategory, levels = order_redlist)

#Create histogram for all patch combinations
full_hist <- ggplot(patch_comb_stats_Din, aes(x = mean_patch_area_log)) +
  geom_histogram(fill = "chartreuse4", color = "black") +
  xlab("Mean Patch Size (ha)") + 
  ylab("Number of patch combinations") +
  scale_x_continuous(breaks = c(-2, -1, 0, 1, 2, 3, 4),
                     labels = c("0.01","0.1","1","10","100","1,000","10,000"))+
  theme(axis.title.x = element_text(margin = margin(t = 10)),  # Adjust margin for x-axis label
        axis.title.y = element_text(margin = margin(r = 10)))+  # Adjust margin for y-axis label
  geom_text(data = patch_comb_dist[6,], aes(x = -1.8, y = Inf, label = paste("Species =", Species, "\nDatasets =", Datasets, "\nCombinations =", Combinations)),
            vjust = 1.1, hjust = 0, size = 4, inherit.aes = FALSE, check_overlap = TRUE)+
  theme_few()

#Create histograms for IUCN categories
facet_hist <- ggplot(patch_comb_stats_Din, aes(x = mean_patch_area_log, fill = redlistCategory)) +
  geom_histogram(fill = "chartreuse4", color = "black", bins = 30) +
  facet_wrap(~redlistCategory) +
  xlab("Mean Patch Size (ha)") + 
  ylab("Number of patch combinations") +
  scale_x_continuous(breaks = c(-2, -1, 0, 1, 2, 3, 4),
                     labels = c("0.01","0.1","1","10","100","1,000","10,000")) +
  theme(axis.title.x = element_text(margin = margin(t = 10)),  # Adjust margin for x-axis label
        axis.title.y = element_text(margin = margin(r = 10)))+ # Adjust margin for y-axis label
  geom_text(data = patch_comb_dist[c(1:5),], aes(x = -1.8, y = Inf, label = paste("Species =", Species, "\nDatasets =", Datasets, "\nCombinations =", Combinations)),
            vjust = 1.1, hjust = 0, size = 4, inherit.aes = FALSE, check_overlap = TRUE)+
  theme_few()

#Combine both plots
dev.new(width = 10, length  = 20, noRStudioGD = TRUE)
ggarrange(full_hist, facet_hist, ncol = 1, nrow = 2, labels = c("(a)", "(b)"), font.label = list(size = 16), vjust = c(1.5, 0.5), hjust = c(-0.9,-0.9))

###############################################################################################
########## Figure S5 - Plots of probability of occupancy as function of MPS ###################
################# (i.e. expected value of posterior distribution) #############################
###############################################################################################

###Figure S5a

#Expected values of the posterior predictive distribution for global reference category (Least Concern)
marginal_effects_global <- conditional_effects(brm_occ_Din, 
                                               effects = "mean_patch_area_log",
                                               prob = 0.5,
                                               method = "posterior_epred")

me_global_e <- as.data.frame(marginal_effects_global$mean_patch_area_log)

#Plot global plot i.e. Least Concern as this is reference category
brm_plot_global_e <- ggplot(me_global_e, aes(x = mean_patch_area_log, y = estimate__)) +
  geom_line(linewidth = 2) +
  geom_ribbon(aes(ymin = lower__, ymax = upper__), alpha = 0.2) +
  # stat_lineribbon(.width = 0.5) +
  scale_fill_brewer(palette = "Greys") +
  theme_few() +
  theme(
    legend.position = "none",
    axis.text.y = element_text(size = 16),
    axis.text.x = element_text(size = 16),
    # axis.title.y = element_text(size = 20, margin = margin(r = 10)),
    axis.title.y = element_blank(),
    # axis.title.x = element_text(size = 20, margin = margin(t = 10))
    axis.title.x = element_blank()
  )+
  scale_x_continuous(breaks = c(-1, 0, 1, 2, 3),
                     labels = c("0.1","1","10","100","1,000"))+
  scale_y_continuous(limits = c(0.4, 1))
# xlab("Mean patch size (ha)") +
# ylab("Probability of occupancy")

###Figure S5b

#Generate global predictions averaged over Least Concern IUCN category
marginal_effects_LC_e <- conditional_effects(brm_occ_Din, 
                                             effects = "mean_patch_area_log", 
                                             conditions = data.frame(redlistCategory = "Least Concern"),
                                             prob = 0.5,
                                             method = "posterior_epred")

#Extract the data for plotting
me_LC_e <- as.data.frame(marginal_effects_LC_e$mean_patch_area_log)

#Generate global predictions averaged over Near Threatened IUCN category
marginal_effects_NT_e <- conditional_effects(brm_occ_Din, 
                                             effects = "mean_patch_area_log", 
                                             conditions = data.frame(redlistCategory = "Near Threatened"),
                                             prob = 0.5,
                                             method = "posterior_epred")

#Extract the data for plotting
me_NT_e <- as.data.frame(marginal_effects_NT_e$mean_patch_area_log)


#Generate global predictions averaged over Vulnerable IUCN category
marginal_effects_V_e <- conditional_effects(brm_occ_Din, 
                                            effects = "mean_patch_area_log", 
                                            conditions = data.frame(redlistCategory = "Vulnerable"),
                                            prob = 0.5,
                                            method = "posterior_epred")


#Extract the data for plotting
me_V_e <- as.data.frame(marginal_effects_V_e$mean_patch_area_log)

#Generate global predictions averaged over Endangered IUCN category
marginal_effects_E_e <- conditional_effects(brm_occ_Din, 
                                            effects = "mean_patch_area_log", 
                                            conditions = data.frame(redlistCategory = "Endangered"),
                                            prob = 0.5,
                                            method = "posterior_epred")

#Extract the data for plotting
me_E_e <- as.data.frame(marginal_effects_E_e$mean_patch_area_log)

#Generate global predictions averaged over C. Endangered IUCN category
marginal_effects_CE_e <- conditional_effects(brm_occ_Din, 
                                             effects = "mean_patch_area_log", 
                                             conditions = data.frame(redlistCategory = "Critically Endangered"),
                                             prob = 0.5,
                                             method = "posterior_epred")

#Extract the data for plotting
me_CE_e <- as.data.frame(marginal_effects_CE_e$mean_patch_area_log)

#Combine into one data frame 
combined_posterior_e <- rbind(me_LC_e, me_NT_e, me_V_e, me_E_e, me_CE_e)

combined_posterior_e <- combined_posterior_e %>% 
  mutate(redlistCategory = factor(redlistCategory, levels = order_redlist))

#Plot
brm_plot_IUCN_e <- 
  ggplot(combined_posterior_e, aes(x = mean_patch_area_log, y = estimate__, fill = redlistCategory, color = redlistCategory)) +
  geom_line(linewidth = 2) +
  geom_ribbon(aes(ymin = lower__, ymax = upper__), color = NA, alpha = 0.05) +
  # stat_lineribbon(.width = 0.5) +  # Adjust alpha for transparency
  scale_color_manual(values = c(
    "Least Concern" = "#60C659",
    "Near Threatened" = "#CCE226",
    "Vulnerable" = "#F9E814",
    "Endangered" = "#FC7F3F",
    "Critically Endangered" = "#D81E05"
  )) +
  scale_fill_manual(values = c(
    "Least Concern" = alpha("#60C659",0.05),
    "Near Threatened" = alpha("#CCE226",0.05),
    "Vulnerable" = alpha("#F9E814",0.05),
    "Endangered" = alpha("#FC7F3F",0.05),
    "Critically Endangered" = alpha("#D81E05",0.05)
  )) +
  theme_few() +
  theme(legend.position = c(0.5,0.5),
        legend.background = element_rect(fill = 'transparent'),  # Black border around legend
        legend.title = element_text(face = "bold", size = 18),
        legend.text = element_text(size = 16),
        axis.text.y = element_text(size = 16),
        axis.text.x = element_text(size = 16),
        # axis.title.y = element_text(size = 20, margin = margin(r = 10)),
        axis.title.y = element_blank(),
        # axis.title.x = element_text(size = 20, margin = margin(t = 10))
        axis.title.x = element_blank()
  )+
  scale_x_continuous(breaks = c(-1, 0, 1, 2, 3),
                     labels = c("0.1","1","10","100","1,000"))+
  # limits = c(0,4.029))+
  scale_y_continuous(limits = c(0.4, 1))+
  # xlab("Mean patch size (ha)") +
  # ylab("Probability of occupancy")+
  labs(
    fill = "IUCN Status",  # Title for the fill legend
    color = "IUCN Status")  # Title for the color legend


#Combine plots to make complete Figure S3
brm_plot_global_n <- brm_plot_global_e + theme(plot.margin = margin(l = 10, r = 10, b = 10, t = 10))
brm_plot_IUCN_n <- brm_plot_IUCN_e + theme(plot.margin = margin(l = 10, r = 10, b = 10, t = 10))

dev.new()
figure <- ggarrange(brm_plot_global_n, brm_plot_IUCN_n, ncol = 2, nrow = 1, labels = c("(a)", "(b)"), hjust = c(-2.0, -2.0), vjust = c(3,3), font.label = list(size = 20))
figure_e <- annotate_figure(figure, left = textGrob("Probability of occupancy", gp = gpar(cex = 1.5, fontfamily = "Arial"), rot = 90, hjust = 0.4), bottom = textGrob("Mean patch size (ha)", gp = gpar(cex = 1.5, fontfamily = "Arial")))
figure_e

#######################################################################################################
##############Figure S6 - Probability of occupancy as function of MPS at individual Level #############
##############                (i.e. incl fixed AND random effects)                         ############
#######################################################################################################

#Compute posterior draws of the expected value of the posterior predictive distribution
predictions <- posterior_epred(brm_occ_Din, newdata = patch_comb_stats_Din)
#Take mean of the predicted probabilities for each proxy value of MPS
predicted_prob <- apply(predictions, 2, mean)
rm(predictions) #remove as very heavy

#Combine Dataset and forest amount variables for plotting
patch_comb_stats_Din$Dataset_habitat <- paste(patch_comb_stats_Din$Dataset,patch_comb_stats_Din$habitat_amount)

#Add predicted probabilites to patch combination data frame
prediction_df <- patch_comb_stats_Din %>%
  mutate(predicted_prob = predicted_prob)

#Plot the predicted probabilities
dev.new()

plot_occ_probabilities <- ggplot(prediction_df, aes(x = mean_patch_area_log, y = predicted_prob, color = Taxon_ITIS, group = Dataset_habitat)) +
  geom_point(size = 0.75, alpha = 0.5)+
  geom_smooth(method = 'glm', method.args = list(family = "binomial"), se = F, linewidth = 0.1, linetype = "solid", alpha = 0.05)+
  # geom_smooth(method = "glm", method.args = list(family = "binomial"), se = FALSE, color = "black", linewidth = 1.5, linetype = "solid")+
  # geom_smooth(method = "loess", se = FALSE, color = "black", linewidth = 1.5, linetype = "solid")+
  xlab("Mean patch size (ha)") + ylab("Probability of Occupancy")+
  theme_few()+
  theme(axis.text.y = element_text(size = 16),
        axis.text.x = element_text(size = 16),
        axis.title.y = element_text(size = 18, margin = margin(r = 10)),
        axis.title.x = element_text(size = 18, margin = margin(t = 10))
  )+
  scale_x_continuous(breaks = c(-1, 0, 1, 2, 3),
                     labels = c("0.1","1","10","100","1,000"))+
  scale_y_continuous(breaks = seq(0, 1, by = 0.2), 
                     limits = c(0, 1.0))+
  theme(legend.position="none")+
  scale_color_viridis(discrete=TRUE, option="turbo")

plot_occ_probabilities

###################################################################################################################
##### Figure S7 - Tests of predictions using the Mean Patch Size method with patches > 1,000 ha retained   ############
#################################################################################################################

### Repeat scripts 04_BrmModel.R and 05_2_Figures_Results.R (Figure 5) using the full database 04_Primate_Database_PatchCombinations.csv


###################################################################################################################
##### Figure S8 - Test of predictions using the MPS Method with different canopy cover thresholds ############
#################################################################################################################

#############50% canopy cover#####################

#Generate global predictions averaged over Least Concern IUCN category
marginal_effects_LC_50 <- conditional_effects(brm_occ_50, 
                                              effects = "mean_patch_area_log", 
                                              conditions = data.frame(redlistCategory = "Least Concern"),
                                              prob = 0.5,
                                              method = "posterior_linpred")


#Extract the data for plotting
me_LC_50 <- as.data.frame(marginal_effects_LC_50$mean_patch_area_log)

#Generate global predictions averaged over Near Threatened IUCN category
marginal_effects_NT_50 <- conditional_effects(brm_occ_50, 
                                              effects = "mean_patch_area_log", 
                                              conditions = data.frame(redlistCategory = "Near Threatened"),
                                              prob = 0.5,
                                              method = "posterior_linpred")

#Extract the data for plotting
me_NT_50 <- as.data.frame(marginal_effects_NT_50$mean_patch_area_log)


#Generate global predictions averaged over Vulnerable IUCN category
marginal_effects_V_50 <- conditional_effects(brm_occ_50, 
                                             effects = "mean_patch_area_log", 
                                             conditions = data.frame(redlistCategory = "Vulnerable"),
                                             prob = 0.5,
                                             method = "posterior_linpred")


#Extract the data for plotting
me_V_50 <- as.data.frame(marginal_effects_V_50$mean_patch_area_log)

#Generate global predictions averaged over Endangered IUCN category
marginal_effects_E_50 <- conditional_effects(brm_occ_50, 
                                             effects = "mean_patch_area_log", 
                                             conditions = data.frame(redlistCategory = "Endangered"),
                                             prob = 0.5,
                                             method = "posterior_linpred")

#Extract the data for plotting
me_E_50 <- as.data.frame(marginal_effects_E_50$mean_patch_area_log)

#Generate global predictions averaged over C. Endangered IUCN category
marginal_effects_CE_50 <- conditional_effects(brm_occ_50, 
                                              effects = "mean_patch_area_log", 
                                              conditions = data.frame(redlistCategory = "Critically Endangered"),
                                              prob = 0.5,
                                              method = "posterior_linpred")

#Extract the data for plotting
me_CE_50 <- as.data.frame(marginal_effects_CE_50$mean_patch_area_log)

#Combine into one data frame
combined_posterior_50 <- rbind(me_LC_50, me_NT_50, me_V_50, me_E_50, me_CE_50)

combined_posterior_50 <- combined_posterior_50 %>% 
  mutate(redlistCategory = factor(redlistCategory, levels = order_redlist))

##########################################################
##################### 75% canopy cover ###################

#Generate global predictions averaged over Least Concern IUCN category
marginal_effects_LC_75 <- conditional_effects(brm_occ_75, 
                                              effects = "mean_patch_area_log", 
                                              conditions = data.frame(redlistCategory = "Least Concern"),
                                              prob = 0.5,
                                              method = "posterior_linpred")


#Extract the data for plotting
me_LC_75 <- as.data.frame(marginal_effects_LC_75$mean_patch_area_log)

#Generate global predictions averaged over Near Threatened IUCN category
marginal_effects_NT_75 <- conditional_effects(brm_occ_75, 
                                              effects = "mean_patch_area_log", 
                                              conditions = data.frame(redlistCategory = "Near Threatened"),
                                              prob = 0.5,
                                              method = "posterior_linpred")

#Extract the data for plotting
me_NT_75 <- as.data.frame(marginal_effects_NT_75$mean_patch_area_log)


#Generate global predictions averaged over Vulnerable IUCN category
marginal_effects_V_75 <- conditional_effects(brm_occ_75, 
                                             effects = "mean_patch_area_log", 
                                             conditions = data.frame(redlistCategory = "Vulnerable"),
                                             prob = 0.5,
                                             method = "posterior_linpred")


#Extract the data for plotting
me_V_75 <- as.data.frame(marginal_effects_V_75$mean_patch_area_log)

#Generate global predictions averaged over Endangered IUCN category
marginal_effects_E_75 <- conditional_effects(brm_occ_75, 
                                             effects = "mean_patch_area_log", 
                                             conditions = data.frame(redlistCategory = "Endangered"),
                                             prob = 0.5,
                                             method = "posterior_linpred")

#Extract the data for plotting
me_E_75 <- as.data.frame(marginal_effects_E_75$mean_patch_area_log)

#Generate global predictions averaged over C. Endangered IUCN category
marginal_effects_CE_75 <- conditional_effects(brm_occ_75, 
                                              effects = "mean_patch_area_log", 
                                              conditions = data.frame(redlistCategory = "Critically Endangered"),
                                              prob = 0.5,
                                              method = "posterior_linpred")

#Extract the data for plotting
me_CE_75 <- as.data.frame(marginal_effects_CE_75$mean_patch_area_log)

#Combine into one data frame
combined_posterior_75 <- rbind(me_LC_75, me_NT_75, me_V_75, me_E_75, me_CE_75)

combined_posterior_75 <- combined_posterior_75 %>% 
  mutate(redlistCategory = factor(redlistCategory, levels = order_redlist))


################################################################
############### Dinerstein canopy cover ########################

#Generate global predictions averaged over Least Concern IUCN category
marginal_effects_LC_Din <- conditional_effects(brm_occ_Din, 
                                               effects = "mean_patch_area_log", 
                                               conditions = data.frame(redlistCategory = "Least Concern"),
                                               prob = 0.5,
                                               method = "posterior_linpred")


#Extract the data for plotting
me_LC_Din <- as.data.frame(marginal_effects_LC_Din$mean_patch_area_log)

#Generate global predictions averaged over Near Threatened IUCN category
marginal_effects_NT_Din <- conditional_effects(brm_occ_Din, 
                                               effects = "mean_patch_area_log", 
                                               conditions = data.frame(redlistCategory = "Near Threatened"),
                                               prob = 0.5,
                                               method = "posterior_linpred")

#Extract the data for plotting
me_NT_Din <- as.data.frame(marginal_effects_NT_Din$mean_patch_area_log)


#Generate global predictions averaged over Vulnerable IUCN category
marginal_effects_V_Din <- conditional_effects(brm_occ_Din, 
                                              effects = "mean_patch_area_log", 
                                              conditions = data.frame(redlistCategory = "Vulnerable"),
                                              prob = 0.5,
                                              method = "posterior_linpred")


#Extract the data for plotting
me_V_Din <- as.data.frame(marginal_effects_V_Din$mean_patch_area_log)

#Generate global predictions averaged over Endangered IUCN category
marginal_effects_E_Din <- conditional_effects(brm_occ_Din, 
                                              effects = "mean_patch_area_log", 
                                              conditions = data.frame(redlistCategory = "Endangered"),
                                              prob = 0.5,
                                              method = "posterior_linpred")

#Extract the data for plotting
me_E_Din <- as.data.frame(marginal_effects_E_Din$mean_patch_area_log)

#Generate global predictions averaged over C. Endangered IUCN category
marginal_effects_CE_Din <- conditional_effects(brm_occ_Din, 
                                               effects = "mean_patch_area_log", 
                                               conditions = data.frame(redlistCategory = "Critically Endangered"),
                                               prob = 0.5,
                                               method = "posterior_linpred")

#Extract the data for plotting
me_CE_Din <- as.data.frame(marginal_effects_CE_Din$mean_patch_area_log)

#Combine into one data frame
combined_posterior_Din <- rbind(me_LC_Din, me_NT_Din, me_V_Din, me_E_Din, me_CE_Din)

combined_posterior_Din <- combined_posterior_Din %>% 
  mutate(redlistCategory = factor(redlistCategory, levels = order_redlist))

##############################################################################
############################# Plots ##########################################


######## IUCN Plots ##########

grDevices::pdfFonts(Arial = grDevices::postscriptFonts()$Arial)

brm_plot_IUCN_50 <- 
  ggplot(combined_posterior_50, aes(x = mean_patch_area_log, y = estimate__, fill = redlistCategory, color = redlistCategory)) +
  geom_line(linewidth = 2) +
  geom_ribbon(aes(ymin = lower__, ymax = upper__), color = NA, alpha = 0.05) +
  # stat_lineribbon(.width = 0.5) +  # Adjust alpha for transparency
  scale_color_manual(values = c(
    "Least Concern" = "#60C659",
    "Near Threatened" = "#CCE226",
    "Vulnerable" = "#F9E814",
    "Endangered" = "#FC7F3F",
    "Critically Endangered" = "#D81E05"
  )) +
  scale_fill_manual(values = c(
    "Least Concern" = alpha("#60C659",0.05),
    "Near Threatened" = alpha("#CCE226",0.05),
    "Vulnerable" = alpha("#F9E814",0.05),
    "Endangered" = alpha("#FC7F3F",0.05),
    "Critically Endangered" = alpha("#D81E05",0.05)
  )) +
  theme_few() +
  theme(legend.position = c(0.7,0.7),
        legend.background = element_rect(fill = 'transparent'),  # Black border around legend
        legend.title = element_text(face = "bold", size = 12),
        legend.text = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.text.x = element_text(size = 10),
        # axis.title.y = element_text(size = 20, margin = margin(r = 10)),
        axis.title.y = element_blank(),
        # axis.title.x = element_text(size = 20, margin = margin(t = 10))
        axis.title.x = element_blank()
  )+
  scale_x_continuous(breaks = c(-2, -1, 0, 1, 2, 3),
                     labels = c("0.01","0.1","1","10","100","1,000"))+
  scale_y_continuous(limits = c(0, 40))+
  # xlab("Mean patch size (ha)") +
  # ylab("Log-odds of occupancy")+
  labs(
    fill = "IUCN Status",  # Title for the fill legend
    color = "IUCN Status")  # Title for the color legend

brm_plot_IUCN_75 <- 
  ggplot(combined_posterior_75, aes(x = mean_patch_area_log, y = estimate__, fill = redlistCategory, color = redlistCategory)) +
  geom_line(linewidth = 2) +
  geom_ribbon(aes(ymin = lower__, ymax = upper__), color = NA, alpha = 0.05) +
  # stat_lineribbon(.width = 0.5) +  # Adjust alpha for transparency
  scale_color_manual(values = c(
    "Least Concern" = "#60C659",
    "Near Threatened" = "#CCE226",
    "Vulnerable" = "#F9E814",
    "Endangered" = "#FC7F3F",
    "Critically Endangered" = "#D81E05"
  )) +
  scale_fill_manual(values = c(
    "Least Concern" = alpha("#60C659",0.05),
    "Near Threatened" = alpha("#CCE226",0.05),
    "Vulnerable" = alpha("#F9E814",0.05),
    "Endangered" = alpha("#FC7F3F",0.05),
    "Critically Endangered" = alpha("#D81E05",0.05)
  )) +
  theme_few() +
  theme(legend.position = c(0.7,0.7),
        legend.background = element_rect(fill = 'transparent'),  # Black border around legend
        legend.title = element_text(face = "bold", size = 12),
        legend.text = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.text.x = element_text(size = 10),
        # axis.title.y = element_text(size = 20, margin = margin(r = 10)),
        axis.title.y = element_blank(),
        # axis.title.x = element_text(size = 20, margin = margin(t = 10))
        axis.title.x = element_blank()
  )+
  scale_x_continuous(breaks = c(-2, -1, 0, 1, 2, 3),
                     labels = c("0.01","0.1","1","10","100","1,000"))+
  scale_y_continuous(limits = c(0, 40))+
  # xlab("Mean patch size (ha)") +
  # ylab("Log-odds of occupancy")+
  labs(
    fill = "IUCN Status",  # Title for the fill legend
    color = "IUCN Status")  # Title for the color legend

brm_plot_IUCN_Din <- 
  ggplot(combined_posterior_Din, aes(x = mean_patch_area_log, y = estimate__, fill = redlistCategory, color = redlistCategory)) +
  geom_line(linewidth = 2) +
  geom_ribbon(aes(ymin = lower__, ymax = upper__), color = NA, alpha = 0.05) +
  # stat_lineribbon(.width = 0.5) +  # Adjust alpha for transparency
  scale_color_manual(values = c(
    "Least Concern" = "#60C659",
    "Near Threatened" = "#CCE226",
    "Vulnerable" = "#F9E814",
    "Endangered" = "#FC7F3F",
    "Critically Endangered" = "#D81E05"
  )) +
  scale_fill_manual(values = c(
    "Least Concern" = alpha("#60C659",0.05),
    "Near Threatened" = alpha("#CCE226",0.05),
    "Vulnerable" = alpha("#F9E814",0.05),
    "Endangered" = alpha("#FC7F3F",0.05),
    "Critically Endangered" = alpha("#D81E05",0.05)
  )) +
  theme_few() +
  theme(legend.position = c(0.7,0.7),
        # legend.background = element_rect(fill = 'transparent'),  # Black border around legend
        legend.background = element_blank(),
        legend.title = element_text(face = "bold", size = 12),
        legend.text = element_text(size = 10),
        legend.margin = margin(0, 0, 0, 0),
        legend.box.margin = margin(0, 0, 0, 0),
        legend.box.spacing = unit(0, "pt"),
        axis.text.y = element_text(size = 10),
        axis.text.x = element_text(size = 10),
        # axis.title.y = element_text(size = 20, margin = margin(r = 10)),
        axis.title.y = element_blank(),
        # axis.title.x = element_text(size = 20, margin = margin(t = 10))
        axis.title.x = element_blank()
  )+
  scale_x_continuous(breaks = c(-2, -1, 0, 1, 2, 3),
                     labels = c("0.01","0.1","1","10","100","1,000"))+
  scale_y_continuous(limits = c(0, 40))+
  # xlab("Mean patch size (ha)") +
  # ylab("Log-odds of occupancy")+
  labs(
    fill = "IUCN Status",  # Title for the fill legend
    color = "IUCN Status")  # Title for the color legend

###################################################################

#### Global Plots ############

#Generate posterior samples for 50% canopy cover (gives the explicit 95% credible interval)
posterior_samples_50 <- posterior_samples(brm_occ_50)
posterior_samples_50 <- posterior_samples_50[,c(1:10)]
# View parameter median, Q2.5 and Q97.5.
posterior_summary_50 <- posterior_summary(brm_occ_50)[c(1:10),]

#Generate posterior samples for 75% canopy cover (gives the explicit 95% credible interval)
posterior_samples_75 <- posterior_samples(brm_occ_75)
posterior_samples_75 <- posterior_samples_75[,c(1:10)]
# View parameter median, Q2.5 and Q97.5.
posterior_summary_75 <- posterior_summary(brm_occ_75)[c(1:10),]

#Generate posterior samples for Dinerstein canopy cover (gives the explicit 95% credible interval)
posterior_samples_Din <- posterior_samples(brm_occ_Din)
posterior_samples_Din <- posterior_samples_Din[,c(1:10)]
# View parameter median, Q2.5 and Q97.5.
posterior_summary_Din <- posterior_summary(brm_occ_Din)[c(1:10),]


#dummy point to be able to plot and see geom_ablines
df_dummy <- data.frame(x = c(0,max(patch_comb_stats_50$mean_patch_area_log)), y = c(max(posterior_samples_50$b_Intercept),0))

plot_occ_50 <- ggplot(data = df_dummy, aes(x = x, y = y)) +
  geom_point(alpha = 0) +
  geom_abline(intercept = posterior_samples_50[1:1500, 1],
              slope     = posterior_samples_50[1:1500, 2],
              linewidth = 1/3, alpha = .05, color = "darkgrey") +
  geom_abline(intercept = mean(posterior_samples_50[, 1]),
              slope     = mean(posterior_samples_50[, 2]),
              linewidth = 2, color ="black", linetype = 1) +
  # xlab("Mean patch size (ha)") +
  # ylab("Log-odds of occupancy")+
  theme_few()+
  scale_x_continuous(breaks = c(-2, -1, 0, 1, 2, 3),
                     labels = c("0.01","0.1","1","10","100","1,000"))+
  theme(
    legend.position = "none",
    axis.text.y = element_text(size = 10),
    axis.text.x = element_text(size = 10),
    # axis.title.y = element_text(size = 20, margin = margin(r = 10)),
    axis.title.y = element_blank(),
    # axis.title.x = element_text(size = 20, margin = margin(t = 10))
    axis.title.x = element_blank()) +
  scale_x_continuous(breaks = c(-2, -1, 0, 1, 2, 3),
                     labels = c("0.01","0.1","1","10","100","1,000"))+
  scale_y_continuous(limits = c(0, 40))+
  scale_color_viridis(discrete=TRUE, option="turbo")+
  annotate("text", x = 1.5, y = 30, label = "β = -3.38 (0.91, -7.30)", 
           size = 5, family = "serif")

plot_occ_75 <- ggplot(data = df_dummy, aes(x = x, y = y)) +
  geom_point(alpha = 0) +
  geom_abline(intercept = posterior_samples_75[1:1500, 1],
              slope     = posterior_samples_75[1:1500, 2],
              linewidth = 1/3, alpha = .05, color = "darkgrey") +
  geom_abline(intercept = mean(posterior_samples_75[, 1]),
              slope     = mean(posterior_samples_75[, 2]),
              linewidth = 2, color ="black", linetype = 1) +
  # xlab("Mean patch size (ha)") +
  # ylab("Log-odds of occupancy")+
  theme_few()+
  scale_x_continuous(breaks = c(-2, -1, 0, 1, 2, 3),
                     labels = c("0.01","0.1","1","10","100","1,000"))+
  theme(
    legend.position = "none",
    axis.text.y = element_text(size = 10),
    axis.text.x = element_text(size = 10),
    # axis.title.y = element_text(size = 20, margin = margin(r = 10)),
    axis.title.y = element_blank(),
    # axis.title.x = element_text(size = 20, margin = margin(t = 10))
    axis.title.x = element_blank()) +
  scale_x_continuous(breaks = c(-2, -1, 0, 1, 2, 3),
                     labels = c("0.01","0.1","1","10","100","1,000"))+
  scale_y_continuous(limits = c(0, 40))+
  scale_color_viridis(discrete=TRUE, option="turbo")+
  annotate("text", x = 1.5, y = 30, label = "β = -3.55 (-0.75, -6.44)", 
           size = 5, family = "serif")

plot_occ_Din <- ggplot(data = df_dummy, aes(x = x, y = y)) +
  geom_point(alpha = 0) +
  geom_abline(intercept = posterior_samples_Din[1:1500, 1],
              slope     = posterior_samples_Din[1:1500, 2],
              linewidth = 1/3, alpha = .05, color = "darkgrey") +
  geom_abline(intercept = mean(posterior_samples_Din[, 1]),
              slope     = mean(posterior_samples_Din[, 2]),
              linewidth = 2, color ="black", linetype = 1) +
  # xlab("Mean patch size (ha)") +
  # ylab("Log-odds of occupancy")+
  theme_few()+
  scale_x_continuous(breaks = c(-2, -1, 0, 1, 2, 3),
                     labels = c("0.01","0.1","1","10","100","1,000"))+
  theme(
    legend.position = "none",
    axis.text.y = element_text(size = 10),
    axis.text.x = element_text(size = 10),
    # axis.title.y = element_text(size = 20, margin = margin(r = 10)),
    axis.title.y = element_blank(),
    # axis.title.x = element_text(size = 20, margin = margin(t = 10))
    axis.title.x = element_blank()) +
  scale_x_continuous(breaks = c(-2, -1, 0, 1, 2, 3),
                     labels = c("0.01","0.1","1","10","100","1,000"))+
  scale_y_continuous(limits = c(0, 40))+
  scale_color_viridis(discrete=TRUE, option="turbo")+
  annotate("text", x = 1.5, y = 30, label = "β = -3.35 (-0.79, -6.04)", 
           size = 5, family = "serif")

################################################################
############ mcmc plots ######################################

#Define axis range and intervals
x_breaks <- seq(-20, 30, by = 10)

#Rename y-axis labels
new_labels <- c("Intercept",
                "MeanPatchSize",
                "NearThreatened",
                "Vulnerable",
                "Endangered",
                "CriticallyEndangered",
                "MeanPatchSize:\nNearThreatened",
                "MeanPatchSize:\nVulnerable",
                "MeanPatchSize:\nEndangered",
                "MeanPatchSize:\nCriticallyEndangered")

##################################################################

#Extract the parameter names
var_names_50 <- variables(brm_occ_50)

#Select the first 10 parameter names
selected_vars_50 <- var_names_50[1:10]

#Plot of parameter estimates with two credible intervals highlighted, 50% and 95%
mcmc_plot_50 <- brms::mcmc_plot(brm_occ_50, 
                                type = "intervals",
                                prob = 0.5,
                                prob_outer = 0.95,
                                variable = selected_vars_50)

mcmc_plot_custom_50 <- mcmc_plot_50 + 
  scale_y_discrete(labels = rev(new_labels), limits = rev) +
  scale_x_continuous(limits = c(-20, 30), breaks = x_breaks) +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 1, color = "black")+
  # labs(x = "Parameter estimates (log-odds scale)")+
  theme(#axis.title.x = element_text(size = 20, margin = margin(t = 10), family = "Arial"), # Increase gap for y-axis title
    axis.text.y = element_text(size = 10, family = "Arial"),
    axis.text.x = element_text(size = 10, family = "Arial"))

#######################################

#Extract the parameter names
var_names_75 <- variables(brm_occ_75)

#Select the first 10 parameter names
selected_vars_75 <- var_names_75[1:10]

#Plot of parameter estimates with two credible intervals highlighted, 50% and 95%
mcmc_plot_75 <- brms::mcmc_plot(brm_occ_75, 
                                type = "intervals",
                                prob = 0.5,
                                prob_outer = 0.95,
                                variable = selected_vars_75)

mcmc_plot_custom_75 <- mcmc_plot_75 + 
  scale_y_discrete(labels = rev(new_labels), limits = rev)+
  scale_x_continuous(limits = c(-20, 30), breaks = x_breaks)+
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 1, color = "black")+
  # labs(x = "Parameter estimates (log-odds scale)")+
  theme(#axis.title.x = element_text(size = 20, margin = margin(t = 10), family = "Arial"), # Increase gap for y-axis title
    axis.text.y = element_text(size = 10, family = "Arial"),
    axis.text.x = element_text(size = 10, family = "Arial"))

##################################

#Extract the parameter names
var_names_Din <- variables(brm_occ_Din)

#Select the first 10 parameter names
selected_vars_Din <- var_names_Din[1:10]

#Plot of parameter estimates with two credible intervals highlighted, 50% and 95%
mcmc_plot_Din <- brms::mcmc_plot(brm_occ_Din, 
                                 type = "intervals",
                                 prob = 0.5,
                                 prob_outer = 0.95,
                                 variable = selected_vars_Din)

mcmc_plot_custom_Din <- mcmc_plot_Din + 
  scale_y_discrete(labels = rev(new_labels), limits = rev)+
  scale_x_continuous(limits = c(-20, 30), breaks = x_breaks)+
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 1, color = "black")+
  # labs(x = "Parameter estimates (log-odds scale)")+
  theme(#axis.title.x = element_text(size = 20, margin = margin(t = 10), family = "Arial"), # Increase gap for y-axis title
    axis.text.y = element_text(size = 10, family = "Arial"),
    axis.text.x = element_text(size = 10, family = "Arial"))


#################################################################

plot_occ_m_50 <- plot_occ_50 + theme(plot.margin = margin(l = 10, r = 10, b = 10, t = 10))
brm_plot_IUCN_m_50 <- brm_plot_IUCN_50 + theme(plot.margin = margin(l = 10, r = 10, b = 10, t = 10))
mcmc_plot_custom_m_50 <- mcmc_plot_custom_50 + theme(plot.margin = margin(l = 10, r = 10, t = 10, b = 10))

plot_occ_m_75 <- plot_occ_75 + theme(plot.margin = margin(l = 10, r = 10, b = 10, t = 10))
brm_plot_IUCN_m_75 <- brm_plot_IUCN_75 + theme(plot.margin = margin(l = 10, r = 10, b = 10, t = 10))
mcmc_plot_custom_m_75 <- mcmc_plot_custom_75 + theme(plot.margin = margin(l = 10, r = 10, t = 10, b = 10))

plot_occ_m_Din <- plot_occ_Din + theme(plot.margin = margin(l = 10, r = 10, b = 10, t = 10))
brm_plot_IUCN_m_Din <- brm_plot_IUCN_Din + theme(plot.margin = margin(l = 10, r = 10, b = 10, t = 10))
mcmc_plot_custom_m_Din <- mcmc_plot_custom_Din + theme(plot.margin = margin(l = 10, r = 10, t = 10, b = 10))

##################################################

figure_mcmc <- ggarrange(mcmc_plot_custom_m_50, mcmc_plot_custom_m_75, mcmc_plot_custom_m_Din, nrow = 1, ncol = 3, labels = "(a)", font.label = list(size = 16))
figure_mcmc_annot <- annotate_figure(figure_mcmc, left = textGrob("Parameter estimates (log-odds)", gp = gpar(cex = 1, fontfamily = "Arial"), rot = 90))
# figure_mcmc_annot

figure_occ <- ggarrange(plot_occ_m_50, plot_occ_m_75, plot_occ_m_Din, nrow = 1, ncol = 3, labels = "(b)", vjust = 0.3, font.label = list(size = 16))
figure_occ_annot <- annotate_figure(figure_occ, left = textGrob("Log-odds of occupancy", gp = gpar(cex = 1, fontfamily = "Arial"), rot = 90))
# figure_occ_annot

figure_IUCN <- ggarrange(brm_plot_IUCN_m_50, brm_plot_IUCN_m_75, brm_plot_IUCN_m_Din, nrow = 1, ncol = 3, labels = "(c)", vjust = 0.3, font.label = list(size = 16))
figure_IUCN_annot <- annotate_figure(figure_IUCN, left = textGrob("Log-odds of occupancy", gp = gpar(cex = 1, fontfamily = "Arial"), rot = 90))
# figure_IUCN_annot

final_figure <- ggarrange(figure_mcmc_annot, figure_occ_annot, figure_IUCN_annot, ncol = 1, nrow = 3, heights = c(4,3,3))

col_titles <- ggarrange(
  textGrob("CC Threshold - 50%", hjust = 0.4, gp = gpar(cex = 1, fontface = "bold")),
  textGrob("CC Threshold - 75%", hjust = 0.4, gp = gpar(cex = 1, fontface = "bold")),
  textGrob("CC Threshold - Dinerstein", hjust = 0.4, gp = gpar(cex = 1, fontface = "bold")),
  nrow = 1, ncol = 3
)

x_axis_titles <- ggarrange(
  textGrob("Mean patch size (ha)", vjust = -0.5, hjust = 0.3, gp = gpar(cex = 1)),
  textGrob("Mean patch size (ha)", vjust = -0.5, hjust = 0.3, gp = gpar(cex = 1)),
  textGrob("Mean patch size (ha)", vjust = -0.5, hjust = 0.3, gp = gpar(cex = 1)),
  nrow = 1, ncol = 3
)

final_figure_with_titles <- ggarrange(
  col_titles,
  final_figure, 
  x_axis_titles,
  ncol = 1,
  heights = c(0.03, 0.94, 0.03) # Adjust height proportions to allocate space for titles
)

dev.new()

final_figure_with_titles

# save.image("FigureSM5.RData")


###################################################################################################################
##### Figure S9 - Test of predictions using the MPS Method with different canopy cover thresholds ############
###################     and grouped into non-threatened and threatened        ###############################
#############################################################################################################

#########################################################
#################### 50% canopy cover ###################

# Generate global predictions averaged over the redlistCategory variable
marginal_effects_50_NonT <- conditional_effects(brm_occ_50_thr, 
                                                effects = "mean_patch_area_log", 
                                                conditions = data.frame(threatened_status = "Non-Threatened"),
                                                prob = 0.5,
                                                method = "posterior_linpred")


me_NonT_50 <- as.data.frame(marginal_effects_50_NonT$mean_patch_area_log)

# Generate global predictions averaged over the threatened status variable
marginal_effects_50_thr <- conditional_effects(brm_occ_50_thr, 
                                               effects = "mean_patch_area_log", 
                                               conditions = data.frame(threatened_status = "Threatened"),
                                               prob = 0.5,
                                               method = "posterior_linpred")

# Extract the data for plotting
me_thr_50 <- as.data.frame(marginal_effects_50_thr$mean_patch_area_log)


combined_posterior_50_thr <- rbind(me_NonT_50, me_thr_50)

combined_posterior_50_thr <- combined_posterior_50_thr %>% 
  mutate(threatened_status = factor(threatened_status, levels = order_threatened))

####################################################
################ 75% canopy cover #################

# Generate global predictions averaged over the redlistCategory variable
marginal_effects_75_NonT <- conditional_effects(brm_occ_75_thr, 
                                                effects = "mean_patch_area_log", 
                                                conditions = data.frame(threatened_status = "Non-Threatened"),
                                                prob = 0.5,
                                                method = "posterior_linpred")


me_NonT_75 <- as.data.frame(marginal_effects_75_NonT$mean_patch_area_log)

# Generate global predictions averaged over the threatened status variable
marginal_effects_75_thr <- conditional_effects(brm_occ_75_thr, 
                                               effects = "mean_patch_area_log", 
                                               conditions = data.frame(threatened_status = "Threatened"),
                                               prob = 0.5,
                                               method = "posterior_linpred")

# Extract the data for plotting
me_thr_75 <- as.data.frame(marginal_effects_75_thr$mean_patch_area_log)

combined_posterior_75_thr <- rbind(me_NonT_75, me_thr_75)

combined_posterior_75_thr <- combined_posterior_75_thr %>% 
  mutate(threatened_status = factor(threatened_status, levels = order_threatened))


#####################################################
################## Dinerstein canopy cover ##########

# Generate global predictions averaged over the redlistCategory variable
marginal_effects_Din_NonT <- conditional_effects(brm_occ_Din_thr, 
                                                 effects = "mean_patch_area_log", 
                                                 conditions = data.frame(threatened_status = "Non-Threatened"),
                                                 prob = 0.5,
                                                 method = "posterior_linpred")


me_NonT_Din <- as.data.frame(marginal_effects_Din_NonT$mean_patch_area_log)

# Generate global predictions averaged over the threatened status variable
marginal_effects_Din_thr <- conditional_effects(brm_occ_Din_thr, 
                                                effects = "mean_patch_area_log", 
                                                conditions = data.frame(threatened_status = "Threatened"),
                                                prob = 0.5,
                                                method = "posterior_linpred")

# Extract the data for plotting
me_thr_Din <- as.data.frame(marginal_effects_Din_thr$mean_patch_area_log)

order_threatened <- c("Non-Threatened", "Threatened")  

combined_posterior_Din_thr <- rbind(me_NonT_Din, me_thr_Din)

combined_posterior_Din_thr <- combined_posterior_Din_thr %>% 
  mutate(threatened_status = factor(threatened_status, levels = order_threatened))


#############################################################
################### Plots ##################################

brm_plot_IUCN_50_thr <- 
  ggplot(combined_posterior_50_thr, aes(x = mean_patch_area_log, y = estimate__, fill = threatened_status, color = threatened_status)) +
  geom_line(linewidth = 2) +
  geom_ribbon(aes(ymin = lower__, ymax = upper__), color = NA, alpha = 0.05) +
  # stat_lineribbon(.width = 0.5) +  # Adjust alpha for transparency
  scale_color_manual(values = c(
    "Non-Threatened" = "#005AB5",
    "Threatened" = "#DC3220"
  )) +
  scale_fill_manual(values = c(
    "Non-Threatened" = alpha("#005AB5",0.05),
    "Threatened" = alpha("#DC3220",0.05)
  )) +
  theme_few() +
  theme(legend.position = c(0.7,0.7),
        legend.background = element_rect(fill = 'transparent'),  # Black border around legend
        legend.title = element_text(face = "bold", size = 12),
        legend.text = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.text.x = element_text(size = 10),
        # axis.title.y = element_text(size = 20, margin = margin(r = 10)),
        axis.title.y = element_blank(),
        # axis.title.x = element_text(size = 20, margin = margin(t = 10))
        axis.title.x = element_blank()
  )+
  scale_x_continuous(breaks = c(-2, -1, 0, 1, 2, 3),
                     labels = c("0.01","0.1","1","10","100","1,000"))+
  scale_y_continuous(limits = c(0, 40))+
  # xlab("Mean patch size (ha)") +
  # ylab("Log-odds of occupancy")+
  labs(
    fill = "IUCN Status",  # Title for the fill legend
    color = "IUCN Status")  # Title for the color legend

brm_plot_IUCN_75_thr <- 
  ggplot(combined_posterior_75_thr, aes(x = mean_patch_area_log, y = estimate__, fill = threatened_status, color = threatened_status)) +
  geom_line(linewidth = 2) +
  geom_ribbon(aes(ymin = lower__, ymax = upper__), color = NA, alpha = 0.05) +
  # stat_lineribbon(.width = 0.5) +  # Adjust alpha for transparency
  scale_color_manual(values = c(
    "Non-Threatened" = "#005AB5",
    "Threatened" = "#DC3220"
  )) +
  scale_fill_manual(values = c(
    "Non-Threatened" = alpha("#005AB5",0.05),
    "Threatened" = alpha("#DC3220",0.05)
  )) +
  theme_few() +
  theme(legend.position = c(0.7,0.7),
        legend.background = element_rect(fill = 'transparent'),  # Black border around legend
        legend.title = element_text(face = "bold", size = 12),
        legend.text = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.text.x = element_text(size = 10),
        # axis.title.y = element_text(size = 20, margin = margin(r = 10)),
        axis.title.y = element_blank(),
        # axis.title.x = element_text(size = 20, margin = margin(t = 10))
        axis.title.x = element_blank()
  )+
  scale_x_continuous(breaks = c(-2, -1, 0, 1, 2, 3),
                     labels = c("0.01","0.1","1","10","100","1,000"))+
  scale_y_continuous(limits = c(0, 40))+
  # xlab("Mean patch size (ha)") +
  # ylab("Log-odds of occupancy")+
  labs(
    fill = "IUCN Status",  # Title for the fill legend
    color = "IUCN Status")  # Title for the color legend

brm_plot_IUCN_Din_thr <- 
  ggplot(combined_posterior_Din_thr, aes(x = mean_patch_area_log, y = estimate__, fill = threatened_status, color = threatened_status)) +
  geom_line(linewidth = 2) +
  geom_ribbon(aes(ymin = lower__, ymax = upper__), color = NA, alpha = 0.05) +
  # stat_lineribbon(.width = 0.5) +  # Adjust alpha for transparency
  scale_color_manual(values = c(
    "Non-Threatened" = "#005AB5",
    "Threatened" = "#DC3220"
  )) +
  scale_fill_manual(values = c(
    "Non-Threatened" = alpha("#005AB5",0.05),
    "Threatened" = alpha("#DC3220",0.05)
  )) +
  theme_few() +
  theme(legend.position = c(0.7,0.7),
        # legend.background = element_rect(fill = 'transparent'),  # Black border around legend
        legend.background = element_blank(),
        legend.title = element_text(face = "bold", size = 12),
        legend.text = element_text(size = 10),
        legend.margin = margin(0, 0, 0, 0),
        legend.box.margin = margin(0, 0, 0, 0),
        legend.box.spacing = unit(0, "pt"),
        axis.text.y = element_text(size = 10),
        axis.text.x = element_text(size = 10),
        # axis.title.y = element_text(size = 20, margin = margin(r = 10)),
        axis.title.y = element_blank(),
        # axis.title.x = element_text(size = 20, margin = margin(t = 10))
        axis.title.x = element_blank()
  )+
  scale_x_continuous(breaks = c(-2, -1, 0, 1, 2, 3),
                     labels = c("0.01","0.1","1","10","100","1,000"))+
  scale_y_continuous(limits = c(0, 40))+
  # xlab("Mean patch size (ha)") +
  # ylab("Log-odds of occupancy")+
  labs(
    fill = "IUCN Status",  # Title for the fill legend
    color = "IUCN Status")  # Title for the color legend

##############################################################

#### Global Plots ############

#Generate posterior samples for 50% canopy cover (gives the explicit 95% credible interval)
posterior_samples_50_thr <- posterior_samples(brm_occ_50_thr)
posterior_samples_50_thr <- posterior_samples_50_thr[,c(1:10)]
# View parameter median, Q2.5 and Q97.5.
posterior_summary_50_thr <- posterior_summary(brm_occ_50_thr)[c(1:10),]

#Generate posterior samples for 75% canopy cover (gives the explicit 95% credible interval)
posterior_samples_75_thr <- posterior_samples(brm_occ_75_thr)
posterior_samples_75_thr <- posterior_samples_75_thr[,c(1:10)]
# View parameter median, Q2.5 and Q97.5.
posterior_summary_75_thr <- posterior_summary(brm_occ_75_thr)[c(1:10),]

#Generate posterior samples for Dinerstein canopy cover (gives the explicit 95% credible interval)
posterior_samples_Din_thr <- posterior_samples(brm_occ_Din_thr)
posterior_samples_Din_thr <- posterior_samples_Din_thr[,c(1:10)]
# View parameter median, Q2.5 and Q97.5.
posterior_summary_Din_thr <- posterior_summary(brm_occ_Din_thr)[c(1:10),]

#dummy point to be able to plot and see geom_ablines
df_dummy <- data.frame(x = c(0,max(patch_comb_stats_50$mean_patch_area_log)), y = c(max(posterior_samples_50_thr$b_Intercept),0))

plot_occ_50_thr <- ggplot(data = df_dummy, aes(x = x, y = y)) +
  geom_point(alpha = 0) +
  geom_abline(intercept = posterior_samples_50_thr[1:1500, 1],
              slope     = posterior_samples_50_thr[1:1500, 2],
              linewidth = 1/3, alpha = .05, color = "darkgrey") +
  geom_abline(intercept = mean(posterior_samples_50_thr[, 1]),
              slope     = mean(posterior_samples_50_thr[, 2]),
              linewidth = 2, color ="black", linetype = 1) +
  # xlab("Mean patch size (ha)") +
  # ylab("Log-odds of occupancy")+
  theme_few()+
  scale_x_continuous(breaks = c(1, 2, 3, 4),
                     labels = c("10","100","1,000","10,000"))+
  theme(
    legend.position = "none",
    axis.text.y = element_text(size = 10),
    axis.text.x = element_text(size = 10),
    # axis.title.y = element_text(size = 20, margin = margin(r = 10)),
    axis.title.y = element_blank(),
    # axis.title.x = element_text(size = 20, margin = margin(t = 10))
    axis.title.x = element_blank()) +
  scale_x_continuous(breaks = c(-2, -1, 0, 1, 2, 3),
                     labels = c("0.01","0.1","1","10","100","1,000"))+
  scale_y_continuous(limits = c(0, 40))+
  scale_color_viridis(discrete=TRUE, option="turbo")+
  annotate("text", x = 1.5, y = 30, label = "β = -4.08 (-0.87, -7.28)", 
           size = 5, family = "serif")

plot_occ_75_thr <- ggplot(data = df_dummy, aes(x = x, y = y)) +
  geom_point(alpha = 0) +
  geom_abline(intercept = posterior_samples_75_thr[1:1500, 1],
              slope     = posterior_samples_75_thr[1:1500, 2],
              linewidth = 1/3, alpha = .05, color = "darkgrey") +
  geom_abline(intercept = mean(posterior_samples_75_thr[, 1]),
              slope     = mean(posterior_samples_75_thr[, 2]),
              linewidth = 2, color ="black", linetype = 1) +
  # xlab("Mean patch size (ha)") +
  # ylab("Log-odds of occupancy")+
  theme_few()+
  scale_x_continuous(breaks = c(-2, -1, 0, 1, 2, 3),
                     labels = c("0.01","0.1","1","10","100","1,000"))+
  theme(
    legend.position = "none",
    axis.text.y = element_text(size = 10),
    axis.text.x = element_text(size = 10),
    # axis.title.y = element_text(size = 20, margin = margin(r = 10)),
    axis.title.y = element_blank(),
    # axis.title.x = element_text(size = 20, margin = margin(t = 10))
    axis.title.x = element_blank()) +
  scale_x_continuous(breaks = c(-2, -1, 0, 1, 2, 3),
                     labels = c("0.01","0.1","1","10","100","1,000"))+
  scale_y_continuous(limits = c(0, 40))+
  scale_color_viridis(discrete=TRUE, option="turbo")+
  annotate("text", x = 1.5, y = 30, label = "β = -2.84 (-0.49, -5.33)", 
           size = 5, family = "serif")

plot_occ_Din_thr <- ggplot(data = df_dummy, aes(x = x, y = y)) +
  geom_point(alpha = 0) +
  geom_abline(intercept = posterior_samples_Din_thr[1:1500, 1],
              slope     = posterior_samples_Din_thr[1:1500, 2],
              linewidth = 1/3, alpha = .05, color = "darkgrey") +
  geom_abline(intercept = mean(posterior_samples_Din_thr[, 1]),
              slope     = mean(posterior_samples_Din_thr[, 2]),
              linewidth = 2, color ="black", linetype = 1) +
  # xlab("Mean patch size (ha)") +
  # ylab("Log-odds of occupancy")+
  theme_few()+
  scale_x_continuous(breaks = c(-2, -1, 0, 1, 2, 3),
                     labels = c("0.01","0.1","1","10","100","1,000"))+
  theme(
    legend.position = "none",
    axis.text.y = element_text(size = 10),
    axis.text.x = element_text(size = 10),
    # axis.title.y = element_text(size = 20, margin = margin(r = 10)),
    axis.title.y = element_blank(),
    # axis.title.x = element_text(size = 20, margin = margin(t = 10))
    axis.title.x = element_blank()) +
  scale_x_continuous(breaks = c(-2, -1, 0, 1, 2, 3),
                     labels = c("0.01","0.1","1","10","100","1,000"))+
  scale_y_continuous(limits = c(0, 40))+
  scale_color_viridis(discrete=TRUE, option="turbo")+
  annotate("text", x = 1.5, y = 30, label = "β = -2.88 (-0.67, -5.25)", 
           size = 5, family = "serif")

################################################################

#Rename y-axis labels
new_labels_thr <- c("Intercept",
                    "MeanPatchSize",
                    "Threatened",
                    "MeanPatchSize:\nThreatened")


#Extract the parameter names
var_names_50_thr <- variables(brm_occ_50_thr)

#Select the first 10 parameter names
selected_vars_50_thr <- var_names_50_thr[1:4]

#Plot of parameter estimates with two credible intervals highlighted, 50% and 95%
mcmc_plot_50_thr <- brms::mcmc_plot(brm_occ_50_thr, 
                                    type = "intervals",
                                    prob = 0.5,
                                    prob_outer = 0.95,
                                    variable = selected_vars_50_thr)

mcmc_plot_custom_50_thr <- mcmc_plot_50_thr + 
  scale_y_discrete(labels = rev(new_labels_thr), limits = rev) +
  scale_x_continuous(limits = c(-20, 30), breaks = x_breaks) +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 1, color = "black")+
  # labs(x = "Parameter estimates (log-odds scale)")+
  theme(#axis.title.x = element_text(size = 20, margin = margin(t = 10), family = "Arial"), # Increase gap for y-axis title
    axis.text.y = element_text(size = 10, family = "Arial"),
    axis.text.x = element_text(size = 10, family = "Arial"))


#################################################################

#Extract the parameter names
var_names_75_thr <- variables(brm_occ_75_thr)

#Select the first 10 parameter names
selected_vars_75_thr <- var_names_75_thr[1:4]

#Plot of parameter estimates with two credible intervals highlighted, 50% and 95%
mcmc_plot_75_thr <- brms::mcmc_plot(brm_occ_75_thr, 
                                    type = "intervals",
                                    prob = 0.5,
                                    prob_outer = 0.95,
                                    variable = selected_vars_75_thr)

mcmc_plot_custom_75_thr <- mcmc_plot_75_thr + 
  scale_y_discrete(labels = rev(new_labels_thr), limits = rev)+
  scale_x_continuous(limits = c(-20, 30), breaks = x_breaks)+
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 1, color = "black")+
  # labs(x = "Parameter estimates (log-odds scale)")+
  theme(#axis.title.x = element_text(size = 20, margin = margin(t = 10), family = "Arial"), # Increase gap for y-axis title
    axis.text.y = element_text(size = 10, family = "Arial"),
    axis.text.x = element_text(size = 10, family = "Arial"))


#Extract the parameter names
var_names_Din_thr <- variables(brm_occ_Din_thr)

#Select the first 10 parameter names
selected_vars_Din_thr <- var_names_Din_thr[1:4]

#Plot of parameter estimates with two credible intervals highlighted, 50% and 95%
mcmc_plot_Din_thr <- brms::mcmc_plot(brm_occ_Din_thr, 
                                     type = "intervals",
                                     prob = 0.5,
                                     prob_outer = 0.95,
                                     variable = selected_vars_Din_thr)

mcmc_plot_custom_Din_thr <- mcmc_plot_Din_thr + 
  scale_y_discrete(labels = rev(new_labels_thr), limits = rev)+
  scale_x_continuous(limits = c(-20, 30), breaks = x_breaks)+
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 1, color = "black")+
  # labs(x = "Parameter estimates (log-odds scale)")+
  theme(#axis.title.x = element_text(size = 20, margin = margin(t = 10), family = "Arial"), # Increase gap for y-axis title
    axis.text.y = element_text(size = 10, family = "Arial"),
    axis.text.x = element_text(size = 10, family = "Arial"))


#################################################################

plot_occ_m_50_thr <- plot_occ_50_thr + theme(plot.margin = margin(l = 10, r = 10, b = 10, t = 10))
brm_plot_IUCN_m_50_thr <- brm_plot_IUCN_50_thr + theme(plot.margin = margin(l = 10, r = 10, b = 10, t = 10))
mcmc_plot_custom_m_50_thr <- mcmc_plot_custom_50_thr + theme(plot.margin = margin(l = 10, r = 10, t = 10, b = 10))

plot_occ_m_75_thr <- plot_occ_75_thr + theme(plot.margin = margin(l = 10, r = 10, b = 10, t = 10))
brm_plot_IUCN_m_75_thr <- brm_plot_IUCN_75_thr + theme(plot.margin = margin(l = 10, r = 10, b = 10, t = 10))
mcmc_plot_custom_m_75_thr <- mcmc_plot_custom_75_thr + theme(plot.margin = margin(l = 10, r = 10, t = 10, b = 10))

plot_occ_m_Din_thr <- plot_occ_Din_thr + theme(plot.margin = margin(l = 10, r = 10, b = 10, t = 10))
brm_plot_IUCN_m_Din_thr <- brm_plot_IUCN_Din_thr + theme(plot.margin = margin(l = 10, r = 10, b = 10, t = 10))
mcmc_plot_custom_m_Din_thr <- mcmc_plot_custom_Din_thr + theme(plot.margin = margin(l = 10, r = 10, t = 10, b = 10))

#############################
#Combine plots into 1 figure

figure_thr_mcmc <- ggarrange(mcmc_plot_custom_m_50_thr, mcmc_plot_custom_m_75_thr, mcmc_plot_custom_m_Din_thr, nrow = 1, ncol = 3, labels = "(a)", font.label = list(size = 16))
figure_thr_mcmc_annot <- annotate_figure(figure_thr_mcmc, left = textGrob("Parameter estimates (log-odds)", gp = gpar(cex = 1, fontfamily = "Arial"), rot = 90))
# figure_thr_mcmc_annot

figure_thr_occ <- ggarrange(plot_occ_m_50_thr, plot_occ_m_75_thr, plot_occ_m_Din_thr, nrow = 1, ncol = 3, labels = "(b)", vjust = 0.3, font.label = list(size = 16))
figure_thr_occ_annot <- annotate_figure(figure_thr_occ, left = textGrob("Log-odds of occupancy", gp = gpar(cex = 1, fontfamily = "Arial"), rot = 90))
# figure_thr_occ_annot

figure_thr_IUCN <- ggarrange(brm_plot_IUCN_m_50_thr, brm_plot_IUCN_m_75_thr, brm_plot_IUCN_m_Din_thr, nrow = 1, ncol = 3, labels = "(c)", vjust = 0.3, font.label = list(size = 16))
figure_thr_IUCN_annot <- annotate_figure(figure_thr_IUCN, left = textGrob("Log-odds of occupancy", gp = gpar(cex = 1, fontfamily = "Arial"), rot = 90))
# figure_thr_IUCN_annot

final_figure_thr <- ggarrange(figure_thr_mcmc_annot, figure_thr_occ_annot, figure_thr_IUCN_annot, ncol = 1, nrow = 3, heights = c(4,3,3))

col_titles <- ggarrange(
  textGrob("CC Threshold - 50%", hjust = 0.4, gp = gpar(cex = 1, fontface = "bold")),
  textGrob("CC Threshold - 75%", hjust = 0.4, gp = gpar(cex = 1, fontface = "bold")),
  textGrob("CC Threshold - Dinerstein", hjust = 0.4, gp = gpar(cex = 1, fontface = "bold")),
  nrow = 1, ncol = 3
)

x_axis_titles <- ggarrange(
  textGrob("Mean patch size (ha)", vjust = -0.5, hjust = 0.3, gp = gpar(cex = 1)),
  textGrob("Mean patch size (ha)", vjust = -0.5, hjust = 0.3, gp = gpar(cex = 1)),
  textGrob("Mean patch size (ha)", vjust = -0.5, hjust = 0.3, gp = gpar(cex = 1)),
  nrow = 1, ncol = 3
)

final_figure_thr_with_titles <- ggarrange(
  col_titles,
  final_figure_thr, 
  x_axis_titles,
  ncol = 1,
  heights = c(0.03, 0.94, 0.03) # Adjust height proportions to allocate space for titles
)

dev.new()

final_figure_thr_with_titles



##############################################################################################
####################Figure S10 - Sensitivty Analysis of DC method #############################
##############################################################################################

###Firstly, repeat code from 05_2_Figures_Results.R - Figure 6 for different thresholds of SLOSS variable 2, 3, 4 and 5. Combined outside of R environment e.g. Powerpoint.

###Change X in object name after running each threshold in 05_2_Figures_Results.R e.g. DC3_Din for SL less than equal to 3 patches.
DCX_Din <- ggplot(long_table, aes(fill = Category, y=Value, x = 1)) + 
  geom_bar(position="fill", stat="identity", width = 0.5)+ 
  geom_text(aes(label = label, y = -0.03), size = 8, color = "black", fontface = "bold") +
  geom_text(aes(label = "", y = -0.07), size = 8, color = "black") +
  scale_fill_manual(values = c("#1f78b4", "#a6cee3", "#b2df8a"),
                    labels = c("Equal_perc" = "SL=SS", "FL_better_perc" = "SL>SS", "SS_better_perc" = "SS>SL"))+
  theme_few()+
  # labs(title = "FL ≤ 5 patches, SS ≥ 6 patches")+
  theme(axis.title.x=element_blank(),
        axis.title.y=element_text(size = 34, margin = margin(r = 20)),
        # axis.title.y=element_blank(),
        axis.text.y=element_text(size = 22, face = "bold"),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        legend.box = element_blank(),
        legend.text = element_text(size = 28, face = "bold", margin = margin(r = 10, unit = "pt")),
        legend.title=element_blank(),
        legend.key.height = unit(1.5, 'cm'),
        # # legend.spacing.y = unit(1.5, 'cm'),
        legend.position = "right")+
  ylab("Result proportions")+
  coord_fixed(ratio = 4 / 2)

###Change X in object name after running each threshold e.g. DC3_IUCN_Din for SL less than equal to 3 patches.
DCX_IUCN_Din <- ggplot(long_table_IUCN, aes(fill=Category, y=Value, x= redlistCategory)) + 
  geom_bar(position="fill", stat="identity")+ 
  geom_text(aes(label = label1, y = -0.03), size = 7, color = "black", fontface = "bold") +
  geom_text(aes(label = label2, y = -0.07), size = 7, color = "black", fontface = "bold") +
  scale_fill_manual(values = c("#1f78b4", "#a6cee3", "#b2df8a"),
                    labels = c("Equal_perc" = "SL=SS", "FL_better_perc" = "SL>SS", "SS_better_perc" = "SS>SL"))+
  theme_few()+
  theme(axis.title.x=element_blank(),
        # axis.title.y=element_text(size = 24, margin = margin(r = 20)),
        axis.title.y = element_blank(),
        axis.text.y=element_text(size = 22, face = "bold"),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        legend.text = element_text(size = 28, face = "bold", margin = margin(r = 10, unit = "pt")),
        legend.title=element_blank(),
        legend.key.height = unit(1.5, 'cm'),
        # legend.spacing.y = unit(1.5, 'cm'),
        legend.position = "right")+
  ylab("Result proportions")

###Plot results for each threshold separately and save

#Add margin to global bar plot
DC2_Din_m <- DC2_Din + theme(plot.margin = margin(r = 20))
DC3_Din_m <- DC3_Din + theme(plot.margin = margin(r = 20))
DC4_Din_m <- DC4_Din + theme(plot.margin = margin(r = 20))
DC5_Din_m <- DC5_Din + theme(plot.margin = margin(r = 20))

#Plot global and IUCN bars with figure notation
DC2_edit <- ggarrange(DC2_Din_m, DC2_IUCN_Din, widths = c(1.5,5), ncol = 2, nrow = 1, labels = c("(a)"), hjust = c(-0.5), font.label = list(size = 28))
DC3_edit <- ggarrange(DC3_Din_m, DC3_IUCN_Din, widths = c(1.5,5), ncol = 2, nrow = 1, labels = c("(b)"), hjust = c(-0.5), font.label = list(size = 28))
DC4_edit <- ggarrange(DC4_Din_m, DC4_IUCN_Din, widths = c(1.5,5), ncol = 2, nrow = 1, labels = c("(c)"), hjust = c(-0.5), font.label = list(size = 28))
DC5_edit <- ggarrange(DC5_Din_m, DC5_IUCN_Din, widths = c(1.5,5), ncol = 2, nrow = 1, labels = c("(d)"), hjust = c(-0.5), font.label = list(size = 28))

dev.new()
DC2_edit
DC3_edit
DC4_edit
DC5_edit

#Plot just the legend
legend_only <- cowplot::get_legend(DC5_Din)
grid.newpage()
grid.draw(legend_only)

#Combine figures outside of R environment

###################################################################################################################
##### Figure S11 - Tests of predictions using the Direct Comparison method with patches > 1,000 ha retained   ############
#################################################################################################################

### Repeat scripts 04_BrmModel.R and 05_2_Figures_Results.R (Figure 6) using the full database 04_Primate_Database_PatchCombinations.csv


###############################################################################################
######## Figure S12 - Probability of occupancy as function of MPS for selected species #########
###############################################################################################

###(a) - selected species that prefer SL

prediction_df_SL <-  prediction_df %>% 
  filter(Taxon_ITIS %in% c("Sapajus_robustus", "Macaca_leonina"))

# Get the colours associatyed to each species in the global plot
species_levels <- unique(prediction_df$Taxon_ITIS)
colour_palette <- viridis(length(species_levels), option = "turbo")
species_colours <- setNames(colour_palette, species_levels)

# Plot for filtered species, retaining the color mapping
plot_occ_probabilities_SL <- ggplot(prediction_df_SL, aes(x = mean_patch_area_log, y = predicted_prob, color = Taxon_ITIS, group = Dataset_habitat)) +
  geom_point(size = 0.75, alpha = 0.5) +
  geom_smooth(method = 'glm', method.args = list(family = "binomial"), se = F, linewidth = 0.1, linetype = "solid", alpha = 0.05) +
  xlab("Mean patch size (ha)") + ylab("Probability of occupancy") +
  theme_few() +
  theme(axis.text.y = element_text(size = 16),
        axis.text.x = element_text(size = 16),
        axis.title.y = element_text(size = 18, margin = margin(r = 10)),
        axis.title.x = element_text(size = 18, margin = margin(t = 10))) +
  scale_x_continuous(breaks = c(-1, 0, 1, 2, 3),
                     labels = c("0.1","1","10","100","1,000"),
                     limits = c(0, 3.0)) +
  scale_y_continuous(breaks = seq(0, 1, by = 0.2), 
                     limits = c(0, 1.0)) +
  theme(legend.position = c(0.25,0.4),
        legend.title = element_text(face = 'bold', size = 13),
        legend.text = element_text(size = 12)) +
  scale_color_manual(values = species_colours) + # Apply the manual color scale
  labs(
    fill = "Species",  # Title for the fill legend
    color = "Species")

plot_occ_probabilities_SL

###(b) - selected species that prefer SS

prediction_df_SS <-  prediction_df %>% 
  filter(Taxon_ITIS %in% c("Callithrix_penicillata", "Macaca_fascicularis", "Microcebus_griseorufus", "Pithecia_chrysocephala"))

#Plot for selected species, retaining the color mapping from Figure S4
plot_occ_probabilities_SS <- ggplot(prediction_df_SS, aes(x = mean_patch_area_log, y = predicted_prob, color = Taxon_ITIS, group = Dataset_habitat)) +
  geom_point(size = 0.75, alpha = 0.5) +
  geom_smooth(method = 'glm', method.args = list(family = "binomial"), se = F, linewidth = 0.1, linetype = "solid", alpha = 0.05) +
  xlab("Mean patch size (ha)") + ylab("Probability of occupancy") +
  theme_few() +
  theme(axis.text.y = element_text(size = 16),
        axis.text.x = element_text(size = 16),
        axis.title.y = element_text(size = 18, margin = margin(r = 10)),
        axis.title.x = element_text(size = 18, margin = margin(t = 10))) +
  scale_x_continuous(breaks = c(-1, 0, 1, 2, 3),
                     labels = c("0.1","1","10","100","1,000"),
                     limits = c(0,3))+
  scale_y_continuous(breaks = seq(0, 1, by = 0.2), 
                     limits = c(0, 1.0)) +
  theme(legend.position = c(0.25,0.4),
        legend.title = element_text(face = 'bold', size = 13),
        legend.text = element_text(size = 12)) +
  scale_color_manual(values = species_colours) + # Apply the manual color scale
  labs(
    fill = "Species",  # Title for the fill legend
    color = "Species")

plot_occ_probabilities_SS

#Plot together
dev.new()
plot_occ_selected <- ggarrange(plot_occ_probabilities_SL, plot_occ_probabilities_SS, ncol = 2, nrow = 1, labels = c("(a)", "(b)"), font.label = list(size = 16), hjust = c(-3.5, -3.5), vjust = c(2,2))
plot_occ_selected



