# Query and clean AFSC RACE survey data ----
# Contact: cindy.tribuzio@noaa.gov
# Last Updated: Sept 2025

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

AYR <- 2026
SYR <- 2026

outpath <- here::here(AYR, 'Nov_models', 'AK_skate_Tier5', 'data')
dir.create(outpath)

# Get data ----

# survey_definition_IDs 52 = AI, 98 = EBS and 78 = slope, and 143 = NBS
# area_ID is the stratum, the '999' is the total for the survey, 01 = EBS, 04 = AI, 05 = slope
GAP_biomass <- sqlQuery(channel_akfin, query = ("
                select * from gap_products.akfin_biomass_v
                where species_code = 471
                and survey_definition_id in (98, 143) 
                and area_id in (99901, 99902)
                                                ")) %>% 
  clean_names()

# Clean up and add CIs and CVs ----
GAP_biomass <- GAP_biomass %>% 
  mutate(cv = sqrt(biomass_var)/biomass_mt,
         se = sqrt(biomass_var),
         bio_ll = biomass_mt - 1.96*se,
         bio_ul = biomass_mt + 1.96*se) %>% 
  replace(is.na(.), 0) %>% 
  rename(survey = survey_definition_id,
         biomass = biomass_mt)

write_csv(GAP_biomass, here::here(outpath, paste0('AK_skate_GAPbiomass_', SYR, '.csv'))) 

