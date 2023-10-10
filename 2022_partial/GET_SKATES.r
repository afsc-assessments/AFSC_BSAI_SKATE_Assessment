
  require(data.table)
  require(reshape2)
  require(rgdal)
  require(dplyr)
  require(lubridate)
  require(swo)
  require(vcdExtra)
  require(misty)


## you will need to source the util functions
 source(paste0(getwd(), "/Code/Get_Data/utils.r"))

##  You will need to connect to the two databases before using this function.
  db <- read_csv('database.csv')
  
  dbname1 <- "akfin"
  database_akfin=db %>% filter(database == dbname1) %>% select(database) #need to add filter for AKFIN user/pass only
  username_akfin=db %>% filter(database == dbname1) %>% select(username)
  password_akfin=db %>% filter(database == dbname1) %>% select(password)
  assign(dbname1, odbcConnect(dbname1, uid = username_akfin, pwd = password_akfin, believeNRows=FALSE))
  
  dbname2 <- "afsc"
  database_afsc=db %>% filter(database == dbname2) %>% select(database) #need to add filter for AKFIN user/pass only
  username_afsc=db %>% filter(database == dbname2) %>% select(username)
  password_afsc=db %>% filter(database == dbname2) %>% select(password)
  assign(dbname2, odbcConnect(dbname2, uid = username_akfin, pwd = password_akfin, believeNRows=FALSE))
  
    
#  	afsc = DBI::dbConnect(odbc::odbc(), "afsc",
#                      UID = afsc_user, PWD = afsc_pwd)
#  	akfin = DBI::dbConnect(odbc::odbc(), "akfin",
#                      UID = akfin_user, PWD = akfin_pwd)
## pulling data using sql files
	cas <- readLines('SQL/BSAI_SKATE_CAS.sql')

	CAS <- sql_run(akfin, cas) %>% 
  	  dplyr::rename_all(toupper)%>% data.table()

	obs <- readLines('SQL/BSAI_SKATE_OBS.sql')

	OBS <- sql_run(afsc, obs) %>% 
  	  dplyr::rename_all(toupper) %>% data.table()

## standardizing names for both data sets
	O_SKATES=c( "BERING SKATE",
				"MUD SKATE","COMMANDER SKATE",
				"SKATE EGG CASE UNIDENTIFIED",      
				"WHITEBROW SKATE","ROUGHTAIL SKATE",            
				"DEEPSEA SKATE",  "OKHOTSK SKATE",              
				"BUTTERFLY SKATE","STARRY SKATE",               
				"SANDPAPER SKATE", "ROUGHSHOULDER SKATE") 

	SPEC_NAMES<-data.table(SPECIES_NAME=c("skate, other","skate, longnose","skate, big","skate, Alaskan","skate, Whiteblotched","skate, Aleutian"),
						NAMES=c("COMBINED SKATE", "LONGNOSE SKATE","BIG SKATE", "ALASKA SKATE","WHITEBLOTCHED SKATE","ALEUTIAN SKATE"))    

	OBS$NAMES <-OBS$SPECIES_NAME
	# OBS[SPECIES_NAME %in% O_SKATES]$NAMES <- "OTHER SKATE"
	CAS <- merge(CAS,SPEC_NAMES,by="SPECIES_NAME")


## CV strata definitions for both data sets
	CV_STRATA <- data.table(CV_STRATUM=c(1,1,2,2,2,3,3,3,3,4,4,6,6,6,7),REPORTING_AREA_CODE=c(521,523,517,518,519,509,512,513,516,508,514,541,542,543,524))
	CV_STRATA_OBS <- data.table(CV_STRATUM=c(1,1,2,2,2,3,3,3,3,4,4,6,6,6,7),NMFS_AREA=c(521,523,517,518,519,509,512,513,516,508,514,541,542,543,524))

	CAS$REPORTING_AREA_CODE<-as.numeric(CAS$REPORTING_AREA_CODE)
	CAS<-merge(CAS,CV_STRATA,by="REPORTING_AREA_CODE")
	OBS<-merge(OBS,CV_STRATA_OBS,by="NMFS_AREA")

## define gears for matching obs and cas
GEARS <- data.table(AGENCY_GEAR_CODE=c('NPT','PTR','NPT','NPT','POT','JIG','HAL'),GEAR_TYPE=c(1,2,3,4,6,7,8))
OBS <- merge(OBS,GEARS,by="GEAR_TYPE")
HARV_SEC<-data.table(VESSEL_TYPE=c(1:6),HARVEST_SECTOR=c('CP','CV','CV','CV','CV','CV'))
OBS<-merge(OBS,HARV_SEC,by='VESSEL_TYPE')

## removeing unidentifed skates from observer data.
OBS<-OBS[!NAMES%in%c('SKATE UNIDENTIFIED','SOFT SNOUT SKATE','STIFF SNOUT SKATE')]


CAS$NMFS_AREA<-CAS$REPORTING_AREA_CODE
CAS$YEAR<-as.numeric(CAS$YEAR)

OBS_CV<-OBS[HARVEST_SECTOR=='CV']
OBS_CP<-OBS[HARVEST_SECTOR=='CP']

CAS_CV<-CAS[HARVEST_SECTOR=='CV']
CAS_CP<-CAS[HARVEST_SECTOR=='CP']

OBS_CP_PROP<-OBS_CP[,list(WEIGHT=sum(EXTRAPOLATED_WEIGHT)),by=c("YEAR","NMFS_AREA","CV_STRATUM","AGENCY_GEAR_CODE","NAMES")]
OBS_CP_TOT<-OBS_CP[,list(TOTAL=sum(EXTRAPOLATED_WEIGHT)),by=c("YEAR","NMFS_AREA","CV_STRATUM","AGENCY_GEAR_CODE")]


OBS_CP_PROP<-merge(OBS_CP_PROP,OBS_CP_TOT)
OBS_CP_PROP$PROP<-OBS_CP_PROP$WEIGHT/OBS_CP_PROP$TOTAL



CAS_CP_AS<-CAS_CP[NAMES!="COMBINED SKATE"][,list(CATCH_WEIGHT=sum(WEIGHT_POSTED)),by=c("YEAR","NMFS_AREA","CV_STRATUM","AGENCY_GEAR_CODE","NAMES")]
CAS_CP_OS<-CAS_CP[NAMES=="COMBINED SKATE"][,list(CATCH_WEIGHT=sum(WEIGHT_POSTED)),by=c("YEAR","NMFS_AREA","CV_STRATUM","AGENCY_GEAR_CODE")]
MERGE_CP_OBS_CAS<-merge(CAS_CP_OS,OBS_CP_PROP,all.x=T,by=c("YEAR","NMFS_AREA","CV_STRATUM","AGENCY_GEAR_CODE"))


## fill those catch from areas without observer data using CV_stratum
OBS_CP_PROP2<-OBS_CP[,list(WEIGHT=sum(EXTRAPOLATED_WEIGHT)),by=c("YEAR","CV_STRATUM","AGENCY_GEAR_CODE","NAMES")]
OBS_CP_TOT2<-OBS_CP[,list(TOTAL=sum(EXTRAPOLATED_WEIGHT)),by=c("YEAR","CV_STRATUM","AGENCY_GEAR_CODE")]
OBS_CP_PROP2<-merge(OBS_CP_PROP2,OBS_CP_TOT2)
OBS_CP_PROP2$PROP<-OBS_CP_PROP2$WEIGHT/OBS_CP_PROP2$TOTAL

EMPTY<-MERGE_CP_OBS_CAS[is.na(PROP)][,1:5]
MERGE_CP_OBS_CAS<-MERGE_CP_OBS_CAS[!is.na(PROP)]
EMPTY<-merge(EMPTY,OBS_CP_PROP2,all.x=T)

OBS_CP_PROP3<-OBS_CP[,list(WEIGHT=sum(EXTRAPOLATED_WEIGHT)),by=c("CV_STRATUM","NAMES")]
OBS_CP_TOT3<-OBS_CP[,list(TOTAL=sum(EXTRAPOLATED_WEIGHT)),by=c("CV_STRATUM")]
OBS_CP_PROP3<-merge(OBS_CP_PROP3,OBS_CP_TOT3)
OBS_CP_PROP3$PROP<-OBS_CP_PROP3$WEIGHT/OBS_CP_PROP3$TOTAL

EMPTY1<-EMPTY[is.na(PROP)][,1:5]
EMPTY<-EMPTY[!is.na(PROP)]
EMPTY1<-merge(EMPTY1,OBS_CP_PROP3,all.x=T,allow.cartesian=T)
EMPTY=rbind(EMPTY,EMPTY1)

CP_CATCH_OS<-rbind(MERGE_CP_OBS_CAS,EMPTY)



CP_CATCH_OS$TCATCH_WEIGHT<-CP_CATCH_OS$PROP*CP_CATCH_OS$CATCH_WEIGHT
CP_CATCH_OS<-CP_CATCH_OS[,c(1:4,6,10)]
names(CP_CATCH_OS)[6]<-"CATCH_WEIGHT"
CP_CATCH<-rbind(CAS_CP_AS,CP_CATCH_OS)

A_CP_CATCH<-CP_CATCH[,list(CATCH=sum(CATCH_WEIGHT)),by=c("NAMES","YEAR")]
A_CP_CATCH<-A_CP_CATCH[order(YEAR,NAMES),]



OBS_CV_PROP<-OBS_CV[,list(WEIGHT=sum(EXTRAPOLATED_WEIGHT)),by=c("YEAR","CV_STRATUM","AGENCY_GEAR_CODE","NAMES")]
OBS_CV_TOT<-OBS_CV[,list(TOTAL=sum(EXTRAPOLATED_WEIGHT)),by=c("YEAR","CV_STRATUM","AGENCY_GEAR_CODE")]


OBS_CV_PROP<-merge(OBS_CV_PROP,OBS_CV_TOT)
OBS_CV_PROP$PROP<-OBS_CV_PROP$WEIGHT/OBS_CV_PROP$TOTAL

CAS_CV_AS<-CAS_CV[NAMES!="COMBINED SKATE"][,list(CATCH_WEIGHT=sum(WEIGHT_POSTED)),by=c("YEAR","CV_STRATUM","AGENCY_GEAR_CODE","NAMES")]
CAS_CV_OS<-CAS_CV[NAMES=="COMBINED SKATE"][,list(CATCH_WEIGHT=sum(WEIGHT_POSTED)),by=c("YEAR","CV_STRATUM","AGENCY_GEAR_CODE")]
MERGE_CV_OBS_CAS<-merge(CAS_CV_OS,OBS_CV_PROP,all.x=T,by=c("YEAR","CV_STRATUM","AGENCY_GEAR_CODE"))

## find catch not paired with observer data
EMPTY<-MERGE_CV_OBS_CAS[is.na(PROP)][,1:4]
MERGE_CV_OBS_CAS<-MERGE_CV_OBS_CAS[!is.na(PROP)]
## fill empty ones from CP data
EMPTY<-merge(EMPTY,OBS_CP_PROP2, all.x=T)

EMPTY1<-EMPTY[is.na(PROP)][,1:4]
EMPTY<-EMPTY[!is.na(PROP)]
EMPTY1<-merge(EMPTY1,OBS_CP_PROP3,all.x=T,allow.cartesian=T)
EMPTY=rbind(EMPTY,EMPTY1)





CV_CATCH_OS<-rbind(MERGE_CV_OBS_CAS,EMPTY)



CV_CATCH_OS$TCATCH_WEIGHT<-CV_CATCH_OS$PROP*CV_CATCH_OS$CATCH_WEIGHT


CV_CATCH_OS<-CV_CATCH_OS[,c(1:3,5,9)]
names(CV_CATCH_OS)[5]<-"CATCH_WEIGHT"
CV_CATCH<-rbind(CAS_CV_AS,CV_CATCH_OS)
CP_CATCH2<- CP_CATCH[,c(1,3:6)]


CATCH<-rbind(CP_CATCH2,CV_CATCH)

A_CATCH<-CATCH[,list(CATCH=sum(CATCH_WEIGHT)),by=c("NAMES","YEAR")]
A_CATCH<-A_CATCH[order(YEAR,NAMES),]

write.csv(A_CATCH,"BSAI_SKATE_CATCH.csv")


