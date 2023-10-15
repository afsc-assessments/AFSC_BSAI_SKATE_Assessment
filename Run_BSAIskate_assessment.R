# BSAI Skate Update Assessment  ----
# Updated Sept 26 2023 by C. Tribuzio
# not completed for 2023 assessment, roadmap for future

# Setup ----
libs <- c("tidyverse", "janitor", "Hmisc", "RColorBrewer", "gridExtra", "gtable", 
          "grid", "flextable", "officer", "lubridate", "RODBC", "DBI", "gtable", 
          "patchwork", 'cowplot', 'rema', 'r4ss')
if(length(libs[which(libs %in% rownames(installed.packages()) == FALSE )]) > 0) {
  install.packages(libs[which(libs %in% rownames(installed.packages()) == FALSE)])}
lapply(libs, library, character.only = TRUE)
'%nin%'<-Negate('%in%') #this is a handy function

# Create Directories ----
SYR <- 2023 #survey year
AYR <- 2023 #assessment year
endyr <- 2023 #for rema model end year
LAYR <- 2020 #last assessment year

dir.create(paste0(getwd(),"/Data/",AYR), showWarnings = T)
dir.create(paste0(getwd(),"/Output/",AYR), showWarnings = T)
dir.create(paste0(getwd(),"/Documents/",AYR), showWarnings = T)

# Get Data ----
# RACE Survey Biomass, this will probably need to be modified next assessment to work for rema
source(paste(getwd(),"/Code/Get_Data/AFSC_RACE_Biomass.R",sep=""))

# add in size/age comp data queries

# Survey length data
#source(paste(getwd(),"/Code/Get Data/Survey_Length_Frequency.R",sep=""))

# AKRO CAS
#source(paste(getwd(),"/Code/Get Data/AKRO_CAS_DATA.R",sep=""))

# Tier 5 rema ----
source(paste(getwd(),"/Code/Tier5/M20_0_rema.R",sep=""))


# Harvest Specs ----
#source(paste(getwd(),"/Code/ABC_OFL/Tier_5_6_Sharks.R",sep=""))

# Assessment Figures ----

# Assessment Tables ----






