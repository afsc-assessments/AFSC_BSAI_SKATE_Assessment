# Tracking survey and fishery catches of egg cases
# potentially to inform the risk tables

# Setup ----
libs <- c("tidyverse", "janitor", "RODBC", 'purrr')
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

outpath <- here::here(AYR, 'Nov_models', 'AK_skate_Tier3', 'mgmt', 'risk_table', 'egg_case')
dir.create(outpath)

# Get data ----

# survey_definition_IDs 52 = AI, 98 = EBS and 78 = slope, and 143 = NBS
# area_ID is the stratum, the '999' is the total for the survey, 01 = EBS, 04 = AI, 05 = slope
GAP_biomass <- sqlQuery(channel_akfin, query = ("
                select * from gap_products.akfin_biomass_v
                where species_code in (474, 473, 402, 436, 421, 484, 476, 411, 478, 441, 456, 446, 461, 401, 481, 486)
                and survey_definition_id in (52, 98, 143)
                and area_id in (99901, 99902, 99904, 99905)")) %>% 
  clean_names()

# Clean up and add CIs and CVs ----
GAP_biomass <- GAP_biomass %>% 
  mutate(cv = sqrt(biomass_var)/biomass_mt,
         se = sqrt(biomass_var),
         bio_ll = biomass_mt - 1.96*se,
         bio_ul = biomass_mt + 1.96*se) %>% 
  replace(is.na(.), 0) %>% 
  dplyr::rename(survey = survey_definition_id,
         biomass = biomass_mt)

write_csv(GAP_biomass, here::here(outpath, paste0('eggcase_GAPbiomass_', AYR, '.csv'))) 

# plot trends----
ggplot(GAP_biomass, aes(x = year, y = biomass))+
  geom_point()+
  geom_line()+
  geom_ribbon(aes(ymin = bio_ll, ymax = bio_ul), fill = 'grey50')+
  scale_colour_viridis_d()+
  facet_grid(common_name~survey_code, scales = 'free')


# 1. Split the data into a list by species
GAP_biomass %>%
  group_split(common_name) %>%
  
  # 2. Loop through each species subset and plot
  walk(function(df) {
    
    # Get the current species name for the filename/title
    species_name <- unique(df$common_name)
    
    p <- ggplot(df, aes(x = year, y = biomass)) +
      geom_point() +
      geom_line() +
      geom_ribbon(aes(ymin = bio_ll, ymax = bio_ul), fill = 'grey50', alpha = 0.3) +
      # Facet only by survey_code now since common_name is isolated
      facet_grid(survey_code~., scales = 'free_y') + 
      labs(title = species_name, x = "Year", y = "Biomass") +
      theme_minimal()
    
    # 3. Save each plot automatically
    ggsave(
      path = outpath,
      filename = paste0("biomass_", gsub(" ", "_", species_name), ".png"), 
      plot = p, 
      width = 8, 
      height = 5
    )
  })


