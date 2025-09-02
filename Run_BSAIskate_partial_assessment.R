# BSAI Skate Partial Assessment  ----
# Updated 8/21/2024 by C. Tribuzio
#use this code to work through each step and run all of the other codes

# Setup ----
libs <- c("tidyverse", "janitor", "Hmisc", "RColorBrewer", "gridExtra", "gtable", 
          "grid", "flextable", "officer", "lubridate", "RODBC", "DBI", "gtable", "patchwork")
if(length(libs[which(libs %in% rownames(installed.packages()) == FALSE )]) > 0) {
  install.packages(libs[which(libs %in% rownames(installed.packages()) == FALSE)])}
lapply(libs, library, character.only = TRUE)
'%nin%'<-Negate('%in%') #this is a handy function

# Create Directories ----
SYR <- 2023 #survey year
AYR <- 2024 #assessment year
endyr <- 2023 #for RFX model end year
LAYR <- 2023 #last assessment year

dir.create(paste0(getwd(),"/Data/",AYR), showWarnings = T)
dir.create(paste0(getwd(),"/Documents/",AYR), showWarnings = T)
dir.create(paste0(getwd(),AYR), showWarnings = T)

# Get Data ----
# AKRO CAS
source(paste(getwd(),"/Code/Get_Data/CAS_OBS_catch.R",sep=""))

# check against last years ----

LAYRcatch <- read_csv(paste0(getwd(), "/Data/", LAYR, "/confidential_CAS_skates_", "Nov92023", ".csv")) %>% #normally replace Nov92023 with LAYR
  group_by(YEAR, SPECIES_NAME) %>% 
  summarise(LAYR_catch = sum(WEIGHT_POSTED)) %>% 
  filter(YEAR < LAYR)
AYRcatch <- read_csv(paste0(getwd(), "/Data/", AYR, "/confidential_CAS_skates", AYR, ".csv")) %>% 
  group_by(YEAR, SPECIES_NAME) %>% 
  summarise(AYR_catch = sum(WEIGHT_POSTED)) %>% 
  filter(YEAR < LAYR)
catch_ck <- AYRcatch %>% 
  full_join(LAYRcatch) %>% # use full join to ensure both data sets have the correct number of years/species
  mutate(diff = AYR_catch - LAYR_catch,
         pdiff = diff/LAYR_catch) # should be essentially zeros

# Run skate catch partitioning----
#note that this is run with a bunch of different packages, that mess up above libraries. Need to change this to all dplyr
# close R and restart to after running this
source(paste(getwd(),"/Code/Data_Processing/BSAI_skate_catch_partitioning.R",sep=""))

# check against last years----
LAYRpart <- read_csv(paste0(getwd(), "/Data/", LAYR, "/confidential_CAS_SKpart", LAYR, ".csv")) %>%
  group_by(YEAR, NAMES) %>% 
  summarise(LAYR_catch = sum(CATCH_WEIGHT)) %>% 
  filter(YEAR < LAYR)
AYRpart <- read_csv(paste0(getwd(), "/Data/", AYR, "/confidential_CAS_SKpart", AYR, ".csv")) %>% 
  group_by(YEAR, NAMES) %>% 
  summarise(AYR_catch = sum(CATCH_WEIGHT)) %>% 
  filter(YEAR < LAYR)
part_ck <- AYRpart %>% 
  left_join(LAYRpart) %>% 
  mutate(diff = round(AYR_catch - LAYR_catch, 0),
         pdiff = (diff/LAYR_catch)*100) # should be essentially zeros, but they are not.....GRRRRRR

# Run End of the year catch estimates----
source(paste(getwd(),"/Code/Data_Processing/End_yr_catch.R",sep=""))

# Calculate catch for harvest projections----
# work through the run_AKSK_projections.R file. This isn't automated just yet.

# HP figures----
source(paste(getwd(),"/Code/Figures/BSAIskate_partial_figures.R",sep=""))
