# Exploring the center of distribution
# potentially to inform the risk tables
# look at shifts within a survey
# look at expansion/contraction
# probably not for 2026

# Setup ----
libs <- c("tidyverse", "janitor", 'RODBC', 'viridis', 'patchwork')
if(length(libs[which(libs %in% rownames(installed.packages()) == FALSE )]) > 0) {
  install.packages(libs[which(libs %in% rownames(installed.packages()) == FALSE)])}
lapply(libs, library, character.only = TRUE)
'%nin%'<-Negate('%in%') #this is a handy function

AYR <- 2026