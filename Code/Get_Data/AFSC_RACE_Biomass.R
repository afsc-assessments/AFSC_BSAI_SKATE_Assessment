# Query and clean AFSC RACE survey data ----
# Contact: cindy.tribuzio@noaa.gov
# Last Updated: Sept 2023

# Setup ----
dbname <- "akfin"
db <- read_csv('database.csv')
database_akfin=db %>% filter(database == dbname) %>% select(database) #need to add filter for AKFIN user/pass only
username_akfin=db %>% filter(database == dbname) %>% select(username)
password_akfin=db %>% filter(database == dbname) %>% select(password)

channel_akfin <- odbcConnect(dbname, uid = username_akfin, pwd = password_akfin, believeNRows=FALSE)

#outpath <- paste0("Data/Cleaned/", AYR)
#dir.create(outpath)
rawpath <- paste0("Data/", AYR) 
dir.create(rawpath)

# Get data ----
# skate species code list
specs <- read_csv(paste0(getwd(), "/Data/BSAIskate_species_codes.csv")) %>% 
  rename(species_code = RACE_code)

# Haul data, will need this to put length data into areas? Will not need it for RFX if we go with REMA
#AFSCTWL_HAULGOA <- sqlQuery(channel_akfin, query = ("
#                select    *
#                from      afsc.race_haulaigoa")) %>% 
#  clean_names()
#AFSCTWL_HAULshelf <- sqlQuery(channel_akfin, query = ("
#                select    *
#                from      afsc.race_haul_ebsshelf")) %>% 
#  clean_names()
#AFSCTWL_HAULslope <- sqlQuery(channel_akfin, query = ("
#                select    *
#                from      afsc.race_haul_ebsslope")) %>% 
#  clean_names()

#AFSCTWL_HAUL <- AFSCTWL_HAULGOA %>% bind_rows(AFSCTWL_HAULshelf, AFSCTWL_HAULslope)
#write_csv(AFSCTWL_HAUL, paste0(rawpath, "/RACE_HAUL", SYR, ".csv"))

# AI survey biomass
AFSCTWL_AIBio <- sqlQuery(channel_akfin, query = ("
                select    *
                from      afsc.race_biomasstotalaigoa
                where     species_code between 400 and 500")) %>% 
  clean_names() %>% 
  select(c("survey", 'year', 'species_code', 'haul_count', 'catch_count', 'total_biomass', 'biomass_var')) %>% 
  filter(survey == "AI",
         year >= 1991) %>% #early years are non-standardized
  mutate(regulatory_area_name = "AI") %>% 
  rename(area_biomass = total_biomass)

# EBS Shelf survey biomass
AFSCTWL_shelfBio <- sqlQuery(channel_akfin, query = ("
                select    *
                from      afsc.race_biomass_ebsshelf_plusnw
                where     species_code between 400 and 500")) %>% 
  clean_names() %>% 
  filter(stratum == 999,
         year >= 1982) %>% 
  select(c("survey", 'year', 'species_code', 'haulcount', 'catcount', 'biomass', 'varbio')) %>% 
  mutate(regulatory_area_name = "EBS") %>% 
  rename(biomass_var = varbio,
         area_biomass = biomass,
         haul_count = haulcount,
         catch_count = catcount)

# EBS Slope survey biomass
AFSCTWL_slopeBio <- sqlQuery(channel_akfin, query = ("
                select    *
                from      afsc.race_biomass_ebsslope
                where     species_code between 400 and 500")) %>% 
  clean_names() %>% 
  filter(stratum == 999999,
         year >= 2002) %>% 
  select(c("survey", 'year', 'species_code', 'haul_count', 'catch_count', 'stratum_biomass', 'bio_var')) %>% 
  mutate(regulatory_area_name = "EBS") %>% 
  rename(biomass_var = bio_var,
         area_biomass = stratum_biomass)

# NBS survey biomass
AFSCTWL_NBSBio <- sqlQuery(channel_akfin, query = ("
                select    *
                from      afsc.race_biomass_nbs
                where     species_code between 400 and 500")) %>% #not sure why this query won't work, works in SQLs
  clean_names() %>% 
  filter(stratum == 999999,
         year >= 2002) %>% 
  select(c("survey", 'year', 'species_code', 'haul_count', 'catch_count', 'stratum_biomass', 'bio_var')) %>% 
  mutate(regulatory_area_name = "EBS") %>% 
  rename(biomass_var = bio_var,
         area_biomass = stratum_biomass)

# Clean up and add CIs and CVs ----
AFSCTWL_BIOM <- AFSCTWL_AIBio %>% bind_rows(AFSCTWL_shelfBio, AFSCTWL_slopeBio) %>% 
  mutate(cv = sqrt(biomass_var)/area_biomass,
         se = sqrt(biomass_var),
         bio_ll = area_biomass - 1.96*se,
         bio_ul = area_biomass + 1.96*se) %>% 
  replace(is.na(.), 0) %>% 
  rename(strata = regulatory_area_name,
         biomass = area_biomass)

# data check for new species, should result in zero rows
new_spec <- AFSCTWL_BIOM %>% 
  left_join(specs) %>% 
  group_by(species_code, RACE_name, BSAI_spec, EBS_Tier, AI_Tier, Shelf_Group, Slope_Group, AI_Group) %>% 
  summarise(n_yrs = length(biomass)) %>% 
  filter(is.na(BSAI_spec))
if(nrow(new_spec) > 0) warning("Check for new species")

# final biomass table
AFSCTWL_BIOM <- AFSCTWL_BIOM %>% 
  left_join(specs) %>% 
  filter(BSAI_spec == "Y")

write_csv(AFSCTWL_BIOM, paste0(rawpath, "/RACE_biomass_skates", SYR, ".csv")) 

