# Pulls and summarizes input sample size data
# informs Table 3

# Setup ----
libs <- c("tidyverse", "janitor", "RODBC")
if(length(libs[which(libs %in% rownames(installed.packages()) == FALSE )]) > 0) {
  install.packages(libs[which(libs %in% rownames(installed.packages()) == FALSE)])}
lapply(libs, library, character.only = TRUE)
'%nin%'<-Negate('%in%') #this is a handy function

dat_path <- here::here(AYR, 'Nov_models', 'AK_skate_Tier3', 'data')
dir.create(dat_path)

AYR <- 2025

dbname <- "akfin"
db <- read_csv('database.csv')
database_akfin=db %>% filter(database == dbname) %>% select(database) #need to add filter for AKFIN user/pass only
username_akfin=db %>% filter(database == dbname) %>% select(username)
password_akfin=db %>% filter(database == dbname) %>% select(password)

assign(paste0("channel_", dbname), odbcConnect(dbname, uid = username_akfin, pwd = password_akfin, believeNRows=FALSE))

#fishery input sample size----
fishdat <- sqlQuery(channel_akfin, query = ('
                SELECT norpac.debriefed_length_mv.year,
                       norpac.debriefed_length_mv.fmp_gear,
                       round(sqrt(COUNT(DISTINCT norpac.debriefed_length_mv.haul_join)), 1) AS "Nsamp",
                       COUNT(DISTINCT norpac.debriefed_length_mv.haul_join) AS "Hauls",
                       SUM(norpac.debriefed_length_mv.frequency) AS "Lengths"
                FROM   norpac.debriefed_length_mv
                WHERE  norpac.debriefed_length_mv.species = 88
                AND    norpac.debriefed_length_mv.nmfs_area BETWEEN 500 AND 543
                GROUP BY norpac.debriefed_length_mv.year,
                       norpac.debriefed_length_mv.fmp_gear
                ORDER BY norpac.debriefed_length_mv.year')) %>% 
  clean_names()

write_csv(fishdat, here::here(dat_path, paste0("fisheryISS_", AYR, ".csv")))

# survey input sample size----
gapdat <- sqlQuery(channel_akfin, query = ('
                SELECT
gap_products.akfin_length_v.year,
COUNT(DISTINCT gap_products.akfin_length_v.hauljoin) AS "Hauls",
SUM(gap_products.akfin_length_v.frequency) AS "Lengths"
FROM
gap_products.akfin_length_v
WHERE
gap_products.akfin_length_v.species_code = 471
and gap_products.akfin_length_v.survey_definition_id = 98
GROUP BY
gap_products.akfin_length_v.year
ORDER BY
gap_products.akfin_length_v.year')) %>% 
  clean_names()

write_csv(gapdat, here::here(dat_path, paste0("surveyISS_", AYR, ".csv")))
