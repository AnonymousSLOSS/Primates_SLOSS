library(brms)
library(posterior)
library(dplyr)
library(ggpubr)
library(grid)
library(ggplot2)
library(ggthemes)
library(viridis)
library(tidyr)

#set working directory
#load in objects from script 04_BrmModel.R
load("brm_Din_reduced.RData")

patch_comb_stats_Din <- read.csv("04_Primate_Database_PatchCombinations_Reduced.csv", header = TRUE)

primatepatches <- read.csv("03_Primate_Database_Patches_Reduced.csv", header = TRUE)

primatepatches <- primatepatches %>% 
  mutate(patch_area_log = log10(patch_area))

patch_comb_dist <- read.csv("04_PatchCombinationsDistribution_Reduced.csv", header = TRUE)
patch_dist <- read.csv("04_PatchDistribution_Reduced.csv", header = TRUE)

####The following code is for the Dinerstein threshold of canopy cover####
####This is repeated for 50% and 75% canopy cover patch and patch combinations databases for Supplementary####

########################################################################
############Figure 4 - Presence and Absence Histogram###################
########################################################################

full_hist_patch_PA <- ggplot(primatepatches, aes(x = patch_area_log, fill = factor(Occurrence))) +
  geom_histogram(position = "stack", color = "black", linewidth = 0.3) +   # Use position = "stack" to stack bars
  xlab("Patch Size (ha)") + 
  ylab("Number of patches") +
  scale_x_continuous(breaks = c(-2, -1, 0, 1, 2, 3, 4),
                     labels = c("0.01","0.1","1","10","100","1,000","10,000")) +
  scale_fill_manual(values = c("0" = "red", "1" = "blue"),
                    labels = c("Absence", "Presence")) +
  theme_few()+
  theme(legend.title = element_blank(),
        legend.text = element_text(size = 14),
        axis.title.x = element_text(size = 16, margin = margin(t = 10)),  # Adjust margin for x-axis label
        axis.title.y = element_text(size = 16, margin = margin(r = 10)),
        axis.text.x = element_text(size = 16),
        axis.text.y = element_text(size = 16))+ 
  ylim(0, 120) +
  geom_text(data = patch_dist[6,], aes(x = -1.8, y = Inf, label = paste("Species =", Species, "\nDatasets =", Datasets, "\nPatches =", Patches)),
            vjust = 2.25, hjust = 0.1, size = 5.5, inherit.aes = FALSE, check_overlap = TRUE)

full_hist_patch_PA

full_hist_PA <- ggplot(patch_comb_stats_Din, aes(x = mean_patch_area_log, fill = factor(Occurrence))) +
  geom_histogram(position = "stack", color = "black", linewidth = 0.3) +   # Use position = "stack" to stack bars
  xlab("Mean Patch Size (ha)") + 
  ylab("Number of patch combinations") +
  scale_x_continuous(breaks = c(-2, -1, 0, 1, 2, 3, 4),
                     labels = c("0.01","0.1","1","10","100","1,000","10,000")) +
  scale_fill_manual(values = c("0" = "red", "1" = "blue"),
                    labels = c("Absence", "Presence")) +
  theme_few()+
  theme(legend.title = element_blank(),
        legend.text = element_text(size = 14),
        axis.title.x = element_text(size = 16, margin = margin(t = 10)),  # Adjust margin for x-axis label
        axis.title.y = element_text(size = 16, margin = margin(r = 10)),
        axis.text.x = element_text(size = 16),
        axis.text.y = element_text(size = 16))+  # Adjust margin for y-axis label
  ylim(0, 2000) +
  geom_text(data = patch_comb_dist[6,], aes(x = -1.8, y = Inf, label = paste("Species =", Species, "\nDatasets =", Datasets, "\n Combinations =", Combinations)),
            vjust = 2.25, hjust = 0.1, size = 5.5, inherit.aes = FALSE, check_overlap = TRUE)


dev.new()
ggarrange(full_hist_patch_PA, full_hist_PA, ncol = 2, nrow = 1, labels = c("(a)", "(b)"), font.label = list(size = 20), hjust = c(-2.8,-3.0), vjust = c(1.8, 1.8), common.legend = TRUE, legend = "right")

##############################################################################
############# Figure 5 - Test of predictions using the MPS Method ############
##############################################################################

###Figure 5a - MCMC model i.e. plot of parameter estimates

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

mcmc_plot_Din 

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

#Define axis range and intervals
x_breaks <- seq(-20, 30, by = 10)

#Update the plot with new y-axis labels
mcmc_plot_custom_Din <- mcmc_plot_Din + 
  scale_y_discrete(labels = rev(new_labels), limits = rev)+
  scale_x_continuous(limits = c(-20, 30), breaks = x_breaks)+
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 1, color = "black")+
  # labs(x = "Parameter estimates (log-odds scale)")+
  theme(#axis.title.x = element_text(size = 20, margin = margin(t = 10), family = "Arial"), # Increase gap for y-axis title
    axis.text.y = element_text(size = 16, family = "Arial"),
    axis.text.x = element_text(size = 16, family = "Arial"))

###Figure 5b - Global plot of the model's posterior distribution (log-odds occupancy as function of MPS)

#Generate posterior samples (gives the explicit 95% credible interval)
posterior_samples_Din <- posterior_samples(brm_occ_Din)
posterior_samples_Din <- posterior_samples_Din[,c(1:10)]

# View parameter median, Q2.5 and Q97.5.
posterior_summary_Din <- posterior_summary(brm_occ_Din)[c(1:10),]

#dummy point to be able to plot and see geom_ablines. Use point from 50% canopy data to harmonise axes
df_dummy <- data.frame(x = c(0,max(patch_comb_stats_Din$mean_patch_area_log)), y = c(max(posterior_samples_Din$b_Intercept),0))

plot_occ_Din <- ggplot(data = df_dummy, aes(x = x, y = y)) +
  geom_point(alpha = 0) +
  geom_abline(intercept = posterior_samples_Din[1:1500, 1],
              slope     = posterior_samples_Din[1:1500, 2],
              linewidth = 1/3, alpha = .05, color = "darkgrey") +
  geom_abline(intercept = mean(posterior_samples_Din[, 1]),
              slope     = mean(posterior_samples_Din[, 2]),
              linewidth = 2, color ="black", linetype = 1) +
  xlab("Mean patch size (ha)") +
  ylab("Log-odds of occupancy")+
  scale_x_continuous(breaks = c(-2, -1, 0, 1, 2, 3),
                     labels = c("0.01","0.1","1","10","100","1,000"))+
  scale_y_continuous(limits = c(-3, 25))+
  theme_few()+
  theme(
    legend.position = "none",
    axis.text.y = element_text(size = 16),
    axis.text.x = element_text(size = 16),
    # axis.title.y = element_text(size = 20, margin = margin(r = 10)),
    axis.title.y = element_blank(),
    # axis.title.x = element_text(size = 20, margin = margin(t = 10))
    axis.title.x = element_blank()
  ) +
  scale_color_viridis(discrete=TRUE, option="turbo")+
  annotate("text", x = 1.5, y = 20, label = "β = -3.35 (-0.79, -6.04)", 
           size = 8, family = "serif")

plot_occ_Din

###Figure 3c - Log-odds occupancy as function of MPS for each IUCN category (i.e. marginal effects of posterior predictive)

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

#Create plot
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
  theme(legend.position = c(0.75,0.7),
        legend.background = element_rect(fill = 'transparent'),  # Black border around legend
        legend.title = element_text(face = "bold", size = 20),
        legend.text = element_text(size = 18),
        axis.text.y = element_text(size = 16),
        axis.text.x = element_text(size = 16),
        # axis.title.y = element_text(size = 20, margin = margin(r = 10)),
        axis.title.y = element_blank(),
        # axis.title.x = element_text(size = 20, margin = margin(t = 10))
        axis.title.x = element_blank()
        )+
  scale_x_continuous(breaks = c(-2, -1, 0, 1, 2, 3),
                     labels = c("0.01","0.1","1","10","100","1,000"))+
                     # limits = c(0,4))+
  scale_y_continuous(limits = c(-3, 25))+
  xlab("Mean patch size (ha)") +
  ylab("Log-odds of occupancy")+
  labs(
    fill = "IUCN Status",  # Title for the fill legend
    color = "IUCN Status")  # Title for the color legend

brm_plot_IUCN_Din

###Combine into complete Figure 5

#Introduce margins
plot_occ_m_Din <- plot_occ_Din + theme(plot.margin = margin(l = 10, r = 10, t = 10))
brm_plot_IUCN_m_Din <- brm_plot_IUCN_Din + theme(plot.margin = margin(l = 10, r = 10, b = 10, t = 10))
mcmc_plot_custom_m_Din <- mcmc_plot_custom_Din + theme(plot.margin = margin(l = 5, r = 25, t = 10, b = 10))

#Structure plots and annotate
figure_bc_Din <- ggarrange(plot_occ_m_Din, brm_plot_IUCN_m_Din, ncol = 1, nrow = 2, labels = c("(b)", "(c)"), hjust = c(-1.5, -1.5), vjust = c(2,2), font.label = list(size = 20))
# figure_bc_Din

figure_bc_Din <- annotate_figure(figure_bc_Din, left = textGrob("Log-odds of occupancy", gp = gpar(cex = 1.5, fontfamily = "Arial"), rot = 90), bottom = textGrob("Mean patch size (ha)", gp = gpar(cex = 1.5, fontfamily = "Arial")))
figure_a_Din <- annotate_figure(mcmc_plot_custom_m_Din, bottom = textGrob("Parameter estimates (log-odds scale)", gp = gpar(cex = 1.5, fontfamily = "Arial"), hjust=0.32))
# figure_a_Din
final_figure_Din <- ggarrange(figure_a_Din, figure_bc_Din, ncol = 2, nrow = 1, labels = c("(a)"), hjust = -1.5, vjust = 2, font.label = list(size = 20))
dev.new()
final_figure_Din


##############################################################################################
############### Figure 6 - Tests of predictions using the Direct Comparison method ###########
##############################################################################################

###For sensitivity analysis using entire database (patches up to 10,000ha)
# patch_comb_stats_Din <- read.csv("04_Primate_Database_PatchCombinations.csv", header = TRUE)

patch_comb_stats_Din$Dataset_habitat <- paste0(patch_comb_stats_Din$Dataset,"_",patch_comb_stats_Din$habitat_amount)

patch_comb_stats_Din$SLOSS <- ifelse(patch_comb_stats_Din$n <= 3, "SL", "SS") 

patch_comb_stats_Din <- patch_comb_stats_Din %>%
  mutate(threatened_status = case_when(
    redlistCategory %in%  c("Critically Endangered", "Endangered", "Vulnerable") ~ "Threatened",
    redlistCategory %in%  c("Least Concern", "Near Threatened") ~ "Non-Threatened",
    TRUE ~ redlistCategory
  )) 

#########################

#Arrange order of habitat area for factorisation
order <- c("twenty_percent", "thirty_percent", "forty_percent", "fifty_percent", "sixty_percent", "seventy_percent", "eighty_percent")  
order_redlist <- c("Least Concern", "Near Threatened", "Vulnerable", "Endangered", "Critically Endangered")

#Factorise red list status and not-threatened vs threatened status
patch_comb_stats_Din <- patch_comb_stats_Din %>% 
  filter(redlistCategory != "Data Deficient") %>% 
  mutate(habitat_amount = factor(habitat_amount, levels = order),
         redlistCategory = factor(redlistCategory, levels = order_redlist),
         threatened_status = factor(threatened_status)) %>% 
  arrange(Dataset, habitat_amount)

#Retain only datasets that have both SL and SS at the level of habitat area within study
patch_sets_filtered <- patch_comb_stats_Din %>%
  group_by(Dataset, habitat_amount, SLOSS) %>%
  summarise(No_of_sets = n(), .groups = "drop") %>% 
  group_by(Dataset, habitat_amount) %>% 
  filter(all(c("SL", "SS") %in% SLOSS)) %>% 
  distinct() %>% 
  left_join(patch_comb_stats_Din, by = c("Dataset", "habitat_amount", "SLOSS"))

##Compare each individual SL set with each individual SS set for each habitat area per Study.

#First, create separate data frames of SL and SS and add new variable for identifying by habitat amount per Study.

df_SL <- patch_sets_filtered %>%
  filter(SLOSS == "SL") %>%
  select(c("Dataset","habitat_amount","SLOSS","Occurrence", "redlistCategory", "Dataset_habitat"))

df_SS <- patch_sets_filtered %>%
  filter(SLOSS == "SS") %>%
  select(c("Dataset","habitat_amount","SLOSS","Occurrence", "redlistCategory", "Dataset_habitat")) 

#Create function to perform comparison set by set and store results for each ID
compare_and_store <- function(id) {
  df_SL_id <- df_SL[df_SL$Dataset_habitat == id, ]
  df_SS_id <- df_SS[df_SS$Dataset_habitat == id, ]
  df_result <- data.frame(Result = character(0))
  
  for (i in 1:nrow(df_SL_id)) {
    for (j in 1:nrow(df_SS_id)) {
      if (df_SL_id[i, "Occurrence"] > df_SS_id[j, "Occurrence"]) {
        result <- "SL>SS"
      } else if (df_SL_id[i, "Occurrence"] < df_SS_id[j, "Occurrence"]) {
        result <- "SS>SL"
      } else {
        result = "SL=SS"
      }
      df_result <- rbind(df_result, data.frame(Result = result))
    }
  }
  
  return(df_result)
}

#Get unique IDs in order to list results
unique_ids <- unique(df_SL$Dataset_habitat)

#Create a list to store results for each unique ID
list_of_results <- list()

#Loop through each ID, perform comparison, and store results
for (id in unique_ids) {
  list_of_results[[id]] <- compare_and_store(id)
}

full_results <- do.call(rbind, list_of_results)

####Extract the count of each outcome (SL>SS, SS>SL, SL=SS) for each ID and compile into a single data frame

#Initialize an empty data frame to store results
result_df <- data.frame(Dataset_habitat = character(),
                        SS_better = numeric(),
                        SL_better = numeric(),
                        Equal = numeric(),
                        stringsAsFactors = FALSE)

#Iterate over each data frame in the list
for (i in seq_along(list_of_results)) {
  #Extract the ID from the list names
  id <- names(list_of_results)[i]
  
  #Get the results from current ID
  current_df <- list_of_results[[i]]
  
  #Count occurrences of each category in the Result column of the current ID
  SS_better_count <- sum(current_df$Result == "SS>SL")
  SL_better_count <- sum(current_df$Result == "SL>SS")
  Equal_count <- sum(current_df$Result == "SL=SS")
  
  #Create a temporary data frame for this iteration
  temp_df <- data.frame(Dataset_habitat = id,
                        SS_better = SS_better_count,
                        SL_better = SL_better_count,
                        Equal = Equal_count,
                        stringsAsFactors = FALSE)
  
  #Bind the temporary data frame to the compiled data frame
  result_df <- bind_rows(result_df, temp_df)
}

#Add back dataset information to results
lookup_table <- patch_comb_stats_Din %>%
  select(Dataset, Taxon_ITIS, Genus, Family, Dataset_habitat, redlistCategory, threatened_status) %>%
  distinct()

result_df <- left_join(result_df, lookup_table,  by = "Dataset_habitat")

# write.csv(result_df, "result_df_Din.csv", row.names = FALSE)

###############################################################################

####Plot in bar plot at species level

#Aggregate at species level
result_df_agg <- result_df %>% 
  summarise(SS_better = sum(SS_better),
            SL_better = sum(SL_better),
            Equal = sum(Equal))


# write.csv(final_result_agg, paste0("DC", threshold, ".csv"), row.names = FALSE)

#Calculate percentages of SLOSS outcome
result_df_agg_perc <- result_df_agg %>%
  mutate(
    SL_better_perc = SL_better / (SS_better + SL_better + Equal),
    Equal_perc = Equal / (SS_better + SL_better + Equal),
    SS_better_perc = SS_better / (SS_better + SL_better + Equal)
  ) %>% 
  select(-c(SS_better, SL_better, Equal)) 

#Lengthen data frame with Category = SLOSS outcome, Value = Frequency of outcome
long_table <- result_df_agg_perc %>%
  pivot_longer(cols = c(SL_better_perc, Equal_perc, SS_better_perc), names_to = "Category", values_to = "Value")

#Factorise the categories in order desired for plotting
long_table$Category <- factor(long_table$Category, levels = c("SL_better_perc","Equal_perc", "SS_better_perc"))

#Plot
# dev.new()

label <- "Global"

#Plot
DC_Din <- ggplot(long_table, aes(fill = Category, y=Value, x = 1)) +
  geom_bar(position="fill", stat="identity", width = 0.5)+
  geom_text(aes(label = label, y = -0.03), size = 10, color = "black", fontface = "bold") +
  geom_text(aes(label = "", y = -0.07), size = 6.5, color = "black") +
  scale_fill_manual(values = c("#1F449C", "#7CA1CC", "#A8B6CC"),
                    labels = c("Equal_perc" = "SL=SS", "SL_better_perc" = "SL>SS", "SS_better_perc" = "SS>SL"))+
  theme_few()+
  # labs(title = "FL ≤ 5 patches, SS ≥ 6 patches")+
  theme(axis.title.x=element_blank(),
        axis.title.y=element_text(size = 40, margin = margin(r = 20)),
        axis.text.y=element_text(size = 30, face = "bold"),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        legend.text = element_text(size = 28, face = "bold", margin = margin(r = 10, unit = "pt")),
        legend.title=element_blank(),
        legend.key.height = unit(1.5, 'cm'),
        # legend.spacing.y = unit(1.5, 'cm'),
        legend.position = "right")+
  ylab("Result proportions")+
  coord_fixed(ratio = 4 / 2)

###########################################################

#Now aggregate at IUCN red list level

result_df_agg_IUCN <- result_df %>%
  mutate(redlistCategory = factor(redlistCategory, levels = order_redlist)) %>% 
  arrange(redlistCategory) %>% 
  group_by(redlistCategory) %>% 
  reframe(SS_better = sum(SS_better),
          SL_better = sum(SL_better),
          Equal = sum(Equal))

#Calculate percentages of SLOSS outcome
result_df_agg_perc_IUCN <- result_df_agg_IUCN %>%
  mutate(
    SL_better_perc = SL_better / (SS_better + SL_better + Equal),
    Equal_perc = Equal / (SS_better + SL_better + Equal),
    SS_better_perc = SS_better / (SS_better + SL_better + Equal)
  ) %>% 
  select(-c(SS_better, SL_better, Equal)) 

#Lengthen data frame with Category = SLOSS outcome, Value = Frequency of outcome
long_table_IUCN <- result_df_agg_perc_IUCN %>%
  pivot_longer(cols = c(SL_better_perc, Equal_perc, SS_better_perc), names_to = "Category", values_to = "Value")

#Factorise the categories in order desired for plotting
long_table_IUCN$Category <- factor(long_table_IUCN$Category, levels = c("SL_better_perc","Equal_perc", "SS_better_perc"))

#Labels for plot. Have to repeat 3 times to match aesthestics.
label1 <- rep(c("Least", "Near", "Vulnerable", "Endangered", "Critically"), each = 3)
label2 <- rep(c("Concern", "Threatened","","","Endangered"), each = 3)

########

DC_IUCN_Din <- ggplot(long_table_IUCN, aes(fill=Category, y=Value, x= redlistCategory)) + 
  geom_bar(position="fill", stat="identity")+ 
  geom_text(aes(label = label1, y = -0.03), size = 10, color = "black", fontface = "bold") +
  geom_text(aes(label = label2, y = -0.07), size = 10, color = "black", fontface = "bold") +
  scale_fill_manual(values = c("#1F449C", "#7CA1CC", "#A8B6CC"),
                    labels = c("Equal_perc" = "SL=SS", "SL_better_perc" = "SL>SS", "SS_better_perc" = "SS>SL"))+
  theme_few()+
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_text(size = 30, face = "bold"),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        legend.text = element_text(size = 28, face = "bold", margin = margin(r = 10, unit = "pt")),
        legend.title=element_blank(),
        legend.key.height = unit(1.5, 'cm'),
        # legend.spacing.y = unit(1.5, 'cm'),
        legend.position = "right")+
  ylab("Result proportions")

dev.new()

#Used for single plots in Sensitivity analysis (Fig S10) including patches up to 10,000ha. Plots then combined in Powerpoint
# ggarrange(DC_Din, DC_IUCN_Din, widths = c(1.5,5), common.legend = TRUE, legend = "right", ncol = 2, nrow = 1, labels = "(d)", hjust = -0.1, vjust = 1.3, font.label = list(size = 45))

ggarrange(DC_Din, DC_IUCN_Din, widths = c(1.5,5), common.legend = TRUE, legend = "right", ncol = 2, nrow = 1, labels = c("(a)", "(b)"), hjust = c(-3.7, -2.5), vjust = c(1.3, 1.3), font.label = list(size = 30))


