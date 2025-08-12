The data provided can be used at different stages of the coding process.

00_Primate_Studies.zip - contains the raw data collated from studies of primates should the reader want to run the very initial cleaning

01_Primate_Database_Initial.csv - database of initial XY points and occurrences. SEE THE README.TXT FILE IN MAIN DIRECTORY FOR EXPLANATION OF VARIABLES see the README.txt file in main directory for explanation of variables. 

	This .csv file is output of script 00_InitialDatabase.R and can be used to run code from script 01_ForestRasterGeneration.

03_Primate_Database_Patches.csv - database of patch sizes (in hectares) associated with the XY points of occupancy. 

	This .csv file is output of script 02_PatchGeneration.R and can be used to run code from script 03_2_PatchCombinationsGeneration.R onwards

	#Description of additional variables

04_Primate_Database_PatchCombinations.csv - database of patch combinations i.e. combinations of one or more patches totalling a predefined threshold of cumulative habitat amount.

	This .csv file is output of script 03_2_PatchCombinationsGeneration.R and can be used to run code from script 04_BrmModel.R onwards

06_FigS1_PatchDistribution - table of summary statistics of patches at level of IUCN threatened status. Used in 06_Figures_SM.R script to produce histograms.

06_FigS1_PatchDistribution - table of summary statistics of patch combinations at level of IUCN threatened status. 06_Figures_SM.R script to produce histograms.


Other files:

assessments.csv - used to assign IUCN threatened categories to species in 00_InitialDatabase.R script

dinerstein_lookup.csv - used to assign Dinerstein canopy cover thresholds in 00_InitialDatabase.R script. 
			
		i.e. the optimal canopy cover threshold that should be used with Hansen et al 2013 Global Forest Change Maps within each biome.

monkey2.png - used in methods figures produced in 05_1_Figures_Methods.R script


################################################################################################

Variable Descriptions:

	01_Primate_Database_Initial.csv 


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

	
	03_Primate_Database_Patches.csv

	#Add descriptions here of additional variables produced for this database

	04_Primate_Database_Patch_Combinations.csv

	#Add descriptions here of additional variables produced for this database
