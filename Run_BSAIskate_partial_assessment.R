# BSAI Skate Partial Assessment  ----
# Updated 11/1 2022 by C. Tribuzio
#use this code to work through each step and run all of the other codes

# Setup ----
libs <- c("tidyverse", "janitor", "Hmisc", "RColorBrewer", "gridExtra", "gtable", 
          "grid", "flextable", "officer", "lubridate", "RODBC", "DBI", "gtable", "patchwork")
if(length(libs[which(libs %in% rownames(installed.packages()) == FALSE )]) > 0) {
  install.packages(libs[which(libs %in% rownames(installed.packages()) == FALSE)])}
lapply(libs, library, character.only = TRUE)
'%nin%'<-Negate('%in%') #this is a handy function

# Create Directories ----
SYR <- 2022 #survey year
AYR <- 2022 #assessment year
endyr <- 2021 #for RFX model end year
LAYR <- 2020 #last assessment year

dir.create(paste0(getwd(),"/Data/Annual_updates/",AYR), showWarnings = T)
dir.create(paste0(getwd(),"/Output/",AYR), showWarnings = T)
dir.create(paste0(getwd(),"/Documents/",AYR), showWarnings = T)

# Get Data ----
# Catch data code from S Barbeaux
source(paste(getwd(),"/Code/Get_Data/GET_SKATES.R",sep=""))

