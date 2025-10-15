# Query survey and fishery length comp data ----
# Contact: cindy.tribuzio@noaa.gov
# Last Updated: Sept 2023

# Setup ----
libs <- c("tidyverse", "janitor", "RODBC")
if(length(libs[which(libs %in% rownames(installed.packages()) == FALSE )]) > 0) {
  install.packages(libs[which(libs %in% rownames(installed.packages()) == FALSE)])}
lapply(libs, library, character.only = TRUE)
'%nin%'<-Negate('%in%') #this is a handy function

dbname <- "akfin"
db <- read_csv('database.csv')
database_akfin=db %>% filter(database == dbname) %>% select(database) #need to add filter for AKFIN user/pass only
username_akfin=db %>% filter(database == dbname) %>% select(username)
password_akfin=db %>% filter(database == dbname) %>% select(password)

channel_akfin <- odbcConnect(dbname, uid = username_akfin, pwd = password_akfin, believeNRows=FALSE)

AYR <- 2025

#outpath <- paste0("Data/Cleaned/", AYR)
#dir.create(outpath)
dat_path <- here::here(AYR, 'Nov_models', 'AK_skate_Tier3', 'data')
dir.create(dat_path)


# AK skate survey length comp data ----
# EBS shelf survey only
EBSshelf_AKlcomp <- sqlQuery(channel_akfin, query = ("
                SELECT * from gap_products.akfin_sizecomp_v
                WHERE species_code = 471
                and survey_definition_id = 98"))
write_csv(EBSshelf_AKlcomp, here::here(dat_path, paste0('AKskate_GAPsizecomp_', AYR, '.csv'))) 

# AK skate fishery length comp data ----
fishery_AKlcomp2 <- sqlQuery(channel_akfin, query = ("
                SELECT * from NORPAC.debriefed_length_mv
                WHERE (species = 88
                AND nmfs_area BETWEEN 500 AND 543)")) #includes all areas
write_csv(fishery_AKlcomp2, here::here(dat_path, paste0('AKskate_fisherysizecomp_', AYR, ".csv"))) 

          