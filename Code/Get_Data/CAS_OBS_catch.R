#AKRO CAS Data ----
#Updated 8/21/2024 by C. Tribuzio
#This code will pull the data from AKFIN and clean it up for use in the assessment


# Still to do list ----


# Setup ----
datapath<-paste(getwd(),"/Data/",AYR,sep="")

dbname <- "akfin"
db <- read_csv('database.csv')
database_akfin=db %>% filter(database == dbname) %>% select(database) #need to add filter for AKFIN user/pass only
username_akfin=db %>% filter(database == dbname) %>% select(username)
password_akfin=db %>% filter(database == dbname) %>% select(password)

assign(paste0("channel_", dbname), odbcConnect(dbname, uid = username_akfin, pwd = password_akfin, believeNRows=FALSE))

# Pull CAS for all skates in BSAI ----
CASdat <- sqlQuery(channel_akfin, query = ("
                select year, week_end_date, wed, fmp_area,	fmp_subarea, reporting_area_code, agency_gear_code,
                       trip_target_group, trip_target_name, species_group_name, species_name, processor_permit_id,
                       ito_company processor_name, ito_plant processor_plant, ves_akr_adfg, ves_akr_name,	ves_akr_length,
                       retained_or_discarded, weight_posted, gf_harvest_sector, deployment_trip_start_date,	deployment_trip_end_date,
                       deployment_trip_pk, monitoring_status,	sampling_strata_deployment_category, sampling_strata,
                       sampling_strata_name, sampling_strata_selection_rate
                from council.comprehensive_blend_ca
                where year >= 2003
                and (fmp_area = 'BSAI')
                and (species_name = 'skate, Alaskan' or 
                     species_name = 'skate, longnose' or 
                     species_name = 'skate, other' or
                     species_name = 'skate, Aleutian' or
                     species_name = 'skate, Whiteblotched' or
                     species_name = 'skate, big')")) %>% 
  #clean_names() %>% 
  as.data.frame() %>% 
  group_by(YEAR, WEEK_END_DATE, WED, FMP_AREA,FMP_SUBAREA, REPORTING_AREA_CODE, AGENCY_GEAR_CODE,
           TRIP_TARGET_GROUP, TRIP_TARGET_NAME, SPECIES_GROUP_NAME, SPECIES_NAME, PROCESSOR_PERMIT_ID,
           PROCESSOR_NAME, PROCESSOR_PLANT, VES_AKR_ADFG, VES_AKR_NAME, VES_AKR_LENGTH,
           RETAINED_OR_DISCARDED, GF_HARVEST_SECTOR, DEPLOYMENT_TRIP_START_DATE, DEPLOYMENT_TRIP_END_DATE,
           DEPLOYMENT_TRIP_PK, MONITORING_STATUS, SAMPLING_STRATA_DEPLOYMENT_CATEGORY, SAMPLING_STRATA,
           SAMPLING_STRATA_NAME, SAMPLING_STRATA_SELECTION_RATE) %>% 
  summarise(catch_mt = sum(WEIGHT_POSTED)) %>% 
  rename(WEIGHT_POSTED = catch_mt)
  #mutate(trip_target = if_else(trip_target_name == "Pollock - midwater", "Pollock",
  #                             if_else(trip_target_name == "Pollock - bottom", "Pollock",
  #                                     if_else(trip_target_name == "Pacific Cod", "Pacific Cod",
  #                                             if_else(trip_target_name == "Atka Mackerel", "Atka Mackerel",
  #                                                     if_else(trip_target_name == "Other Species", "Other",
  #                                                             if_else(trip_target_name == "Sablefish", "Sablefish",
  #                                                                     if_else(trip_target_name == "Halibut", "Halibut",
  #                                                                             if_else(trip_target_name == "Rockfish", "Rockfish",
  #                                                                                     if_else(is.na(trip_target_name), "Other", "Flatfish")))))))))) %>% 
  #select(-c(species_name))

write_csv(CASdat, paste0(datapath, "/confidential_CAS_skates", AYR, ".csv"))

#AKskt_CAS <- CASdat %>% 
#  group_by(FMP_GEAR, YEAR) %>% 
#  summarise(tot_catch = sum(SUM_WEIGHT_POSTED))

NORPAC_dat <- sqlQuery(channel_akfin, query = ("
SELECT
    norpac.debriefed_haul.haul_date,
    norpac.debriefed_haul.gear_type,
    norpac.debriefed_haul.vessel_type,
    norpac.debriefed_haul.nmfs_area,
    norpac.debriefed_haul.year,
    norpac.debriefed_spcomp.species,
    norpac.debriefed_spcomp.species_name,
    norpac.debriefed_spcomp.extrapolated_weight
FROM
    norpac.debriefed_spcomp
    INNER JOIN norpac.debriefed_haul ON norpac.debriefed_haul.haul_join = norpac.debriefed_spcomp.haul_join
WHERE
    (norpac.debriefed_haul.nmfs_area BETWEEN 500 AND 544
      AND norpac.debriefed_haul.year > 2002
      AND norpac.debriefed_spcomp.species BETWEEN 85 AND 98 )
    OR (norpac.debriefed_spcomp.species BETWEEN 159 AND 168 )"))

write_csv(NORPAC_dat, paste0(datapath, "/confidential_NORPAC_skates", AYR, ".csv"))
