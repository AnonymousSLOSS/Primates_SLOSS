This GitHub repository contains the data and script for the manuscript "Small patches have high conservation value for primates".

The raw data used in this research is an "in preparation" database of primate patch occupancy, created by searching primary and gray literature from 1995-2020 for articles that reported primate species with spatial occurrence and geographic coordinate data, anywhere in the world.

Data:

Primate_Inital_Database.csv - this is database produced after running initial cleaning with 00_InitialDatabase.R script. See variable descriptions below.
assessments.csv - the IUCN red list status of all primate species. Used as a lookup in 00_InitialDatabase.R script
dinerstein_lookup.csv - Dinerstein threshold lookup for canopy cover. i.e. the optimal canopy cover threshold that should be used with Hansen et al 2013 Global Forest Change Maps within each biome.

Primate_Initial_Database.csv 

#variable descriptions

	Source = the study that the primate patch occupancy data was taken from
	X = longitude coordinate in decimal degrees
	Y = latitude coordinate in decimal degrees
	Country = country of study
	Realm = biogeographical realm of study
	Family = family that species belongs to 
	Genus = genus that species belongs to
	Occurrence = presence or absence. presence = 1, absence = 0
	Taxon_ITIS = taxonomic name used by ITIS
	
#variables retained but not used in this research. Of potential use for future research considering the influence of traits
	Sample_met = sampling method 
	Sample_yea = sampling year
	Sampling_e = sampling effort
	Sampling_1 = sampling unit
	BodyMass_k = body mass
	DielActivi = diurnal or nocturnal 
	HomeRange_ = home range size
	Locomotion = terrestrial or arboreal, mixture of both, predominantly one of these
	TrophicGui = species guild e.g. frugivore, folivore etc
	Brain_size = brain size
	Canine_Dim = canine dimensions
	MeanGroupS = mean group size of a troop
	Interbirth = interbirth interval in number of days
	Gestation = pregnancy duration
	WeaningAge = weaning age
	MaxLongevi 
	LitterSz = litter size
	AdultSexRa 
	Social_Sys = social system e.g. polygyny, solitary, pair	
	Mating_Sys = mating system e.g. monogamy

Scripts:

00_InitialDatabase.R - initial collation into one data frame and initial cleaning of the "in preparation" database of primate patch occupancy.

01_ForestRasterGeneration.R - generation of bespoke forest rasters (forest = 1, non-forest = 0) of Hansen et 2013 Global Forest Change maps for each dataset (occupancy data for a particular species from a particular study) that encompass the spatial extent of the dataset.

02_PatchGeneration.R - generation of patch sizes (in hectares) associated with the XY points of occupancy

03_1_PatchCombinationsFunctions.R - bespoke functions that are used in 03_2_PatchCombinationsGeneration.R

03_2_PatchCombinationsGeneration.R - generation of patch combinations i.e. combinations of one or more patches totalling a predefined threshold of cumulative habitat amount. 

04_BrmModel.R - Bayesian model for Mean Patch Size method of analysing occupancy as a function of mean patch size. Lower mean patch size = several small patches and higher mean patch size = few large patches

05_1_Figures_Methods.R - Code for producing elements of Figures 1 - 3, those figures representing the Methods used. These elements are combined outside a coding environment to produce finalised figures.

05_2_Figures_Results.R - Code for producing Figures 4 - 6. 
	i) Figure 4 - produces histograms of distribution of a) patches and b) patch combinations with presence and absence proportions
	ii) Figure 5 - uses output from posterior distribution of Bayesian model to produce a) parameter estimates b) global plot of log odds of occupancy as a function of mean patch size and c) per IUCN category plot of log odds of occupancy as a function of mean patch size
	iii) Figure 6 - derives pairs of SL and SS patch combinations (using SL ≤ 3 patches and SS ≥ 4 patches), directly compares occupancy between these pairs, and then produces bar plots of a) global and b) per IUCN category of the three different outcomes and their overall proportions: SL > SS, SL = SS and SS > SL.

06_Figures_SM.R - Code for producing all supplementary figures.
 
