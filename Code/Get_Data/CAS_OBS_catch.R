#AKRO CAS Data ----
#Updated 9/28/2023 by C. Tribuzio
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
                SELECT
    SUM(council.comprehensive_blend_ca.weight_posted) AS sum_weight_posted,
    council.comprehensive_blend_ca.fmp_area,
    council.comprehensive_blend_ca.species_name,
    council.comprehensive_blend_ca.trip_target_name,
    council.comprehensive_blend_ca.week_end_date,
    council.comprehensive_blend_ca.reporting_area_code,
    council.comprehensive_blend_ca.agency_species_code,
    council.comprehensive_blend_ca.species_group_code,
    council.comprehensive_blend_ca.retained_or_discarded,
    council.comprehensive_blend_ca.weight_posted,
    council.comprehensive_blend_ca.agency_gear_code,
    council.comprehensive_blend_ca.trip_target_code,
    council.comprehensive_blend_ca.year,
    council.comprehensive_blend_ca.fmp_subarea,
    council.comprehensive_blend_ca.fmp_gear,
    council.comprehensive_blend_ca.harvest_sector
FROM
    council.comprehensive_blend_ca
WHERE
    council.comprehensive_blend_ca.fmp_area = 'BSAI'
    AND council.comprehensive_blend_ca.species_name LIKE '%skate%'
    AND council.comprehensive_blend_ca.year > 2002
GROUP BY
    council.comprehensive_blend_ca.fmp_area,
    council.comprehensive_blend_ca.species_name,
    council.comprehensive_blend_ca.trip_target_name,
    council.comprehensive_blend_ca.week_end_date,
    council.comprehensive_blend_ca.reporting_area_code,
    council.comprehensive_blend_ca.agency_species_code,
    council.comprehensive_blend_ca.species_group_code,
    council.comprehensive_blend_ca.retained_or_discarded,
    council.comprehensive_blend_ca.weight_posted,
    council.comprehensive_blend_ca.agency_gear_code,
    council.comprehensive_blend_ca.trip_target_code,
    council.comprehensive_blend_ca.year,
    council.comprehensive_blend_ca.fmp_subarea,
    council.comprehensive_blend_ca.fmp_gear,
    council.comprehensive_blend_ca.harvest_sector"))

write_csv(CASdat, paste0(datapath, "/confidential_CAS_skates", AYR, ".csv"))

CASdat <- read_csv("confidential_CAS_skates2023.csv")

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
