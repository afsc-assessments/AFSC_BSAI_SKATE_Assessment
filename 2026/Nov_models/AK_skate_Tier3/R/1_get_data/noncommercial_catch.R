# query code for non commerical catch table

# Setup ----
libs <- c("tidyverse", "janitor", "RODBC")
if(length(libs[which(libs %in% rownames(installed.packages()) == FALSE )]) > 0) {
  install.packages(libs[which(libs %in% rownames(installed.packages()) == FALSE)])}
lapply(libs, library, character.only = TRUE)
'%nin%'<-Negate('%in%') #this is a handy function

dat_path <- here::here(AYR, 'Nov_models', 'AK_skate_Tier3', 'data')
#dir.create(dat_path)

AYR <- 2026

dbname <- "akfin"
db <- read_csv('database.csv')
database_akfin=db %>% filter(database == dbname) %>% select(database) #need to add filter for AKFIN user/pass only
username_akfin=db %>% filter(database == dbname) %>% select(username)
password_akfin=db %>% filter(database == dbname) %>% select(password)

assign(paste0("channel_", dbname), odbcConnect(dbname, uid = username_akfin, pwd = password_akfin, believeNRows=FALSE))




noncommdat <- sqlQuery(channel_akfin, query = ("
               select * 
                from akr.V_noncommercial_fishery_catch
                where species_group_code = 'USKT'
                and fmp_area_code in ('AI', 'BS')")) 
write_csv(noncommdat, here::here(AYR, 'Nov_models', 'full_complex', paste0('noncommercial_catch_', AYR, '.csv')))
