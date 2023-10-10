# Query survey and fishery length comp data ----
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


# AK skate survey length comp data ----
EBSshelf_AKlcomp <- sqlQuery(channel_akfin, query = ("
                SELECT * from AFSC.race_sizecmp_ebsshelf_plusnw
                WHERE species_code = 471"))
write_csv(EBSshelf_AKlcomp, paste0(rawpath, "/RACE_Shelflcomp_AKskt", SYR, ".csv")) 

# AK skate fishery length comp data ----
#fishery_AKlcomp <- sqlQuery(channel_akfin, query = ("
#                SELECT * from NORPAC.debriefed_length_mv
#                WHERE species = 88 and fmp_subarea = 'BS'"))
#write_csv(fishery_AKlcomp, paste0(rawpath, "/fisherylcomp_AKskt", SYR, ".csv"))

fishery_AKlcomp2 <- sqlQuery(channel_akfin, query = ("
                SELECT * from NORPAC.debriefed_length_mv
                WHERE (species = 88
                AND nmfs_area BETWEEN 500 AND 543)")) #includes all areas
write_csv(fishery_AKlcomp2, paste0(rawpath, "/fisherylcompall_AKskt", SYR, ".csv")) 

#fishery_AKlcomp3 <- sqlQuery(channel_akfin, query = ("
#                SELECT * from NORPAC.debriefed_age_flat_mv
#                WHERE species_key = 88")) #includes all areas
#write_csv(fishery_AKlcomp2, paste0(rawpath, "/fisherylcompall_AKskt", SYR, ".csv")) 
