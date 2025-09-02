#based on code from Maia Kapur
# https://github.com/mkapur-noaa/bsai-fhs/blob/main/2023/R/3_runProjections_makeFigsTables.R

# Setup ----
require(dplyr)
require(tidyr)
require(ggplot2)
require(here)
library(readr)
library(janitor)
library(r4ss)

# Catch values used for projections----
#previous year final AK skate catch
LAYRAKscatch <- read_csv(paste0(getwd(), "/Data/", AYR, "/BSAIsk_totcatch_", AYR, ".csv")) %>% 
  filter(OBS_name == "ALASKA SKATE",
         YEAR == LAYR) %>% 
  summarise(AKcatch = sum(CATCH))

# current year total catch of all skate species
AYRcatch <- read_csv(paste0(getwd(), "/Data/", AYR, "/BSAIsk_totcatch_", AYR, ".csv")) %>% 
  group_by(YEAR) %>% 
  summarise(AYR_catch = sum(CATCH)) %>% 
  filter(YEAR == AYR)

# mean of last 5 years of all skate species
m5yr <- read_csv(paste0(getwd(), "/Data/", AYR, "/BSAIsk_totcatch_", AYR, ".csv")) %>% 
  group_by(YEAR) %>% 
  summarise(AYR_catch = sum(CATCH)) %>% 
  filter(YEAR > AYR -5) %>% 
  summarise(meancatch = mean(AYR_catch))

#the above values are hand entered into the harvest projection inputs for now. Will update methods next year.
# last lines of skates_spcat.dat.
# for HPs, update the total catch from LAYR, change AYR catch to estimated total catch for AYR and use 5YRmean for the next year.
# do not change years in HPs because model itself isn't rerun

## run scenario proj in active folder
setwd(paste0(getwd(), "/", AYR, "/Tier_3/prg_AKSK_14_2_HP2024")) 
shell('main')
setwd("C:/Users/cindy.Tribuzio/Work/SAFE/Assessments/AFSC_BSAI_SKATE_Assessment")

## compile tables - see Cole's report.R ----
### The projection model results
## ## Use R to process output into easy file to create the harvest
## ## table in report.xlsx.

rec_table1 <-
  read.table(paste0(getwd(),"/", AYR, '/Tier_3/prg_AKSK_14_2_HP2024/percentdb.out')) %>%
  as.data.frame(stringsAsFactors=FALSE) %>%
  transmute(scenario=as.numeric(V2), year=as.numeric(V3), metric=V4,
            value=as.numeric(V5)) %>%
  filter(year %in% (AYR+1:2) & scenario==1 &
           metric %in% c('SSBMean','SSBFofl', 'SSBFabc', 'SSBF100', 'Fofl', 'Fabc')) %>%
  arrange(year, metric) %>%
  pivot_wider(names_from=year, values_from=value)
rec_table1[3:6,3:4] <- rec_table1[3:6,3:4]

rec_table2 <-
  read.table(paste0(getwd(), "/", AYR, '/Tier_3/prg_AKSK_14_2_HP2024/alt2_proj.out'), header=TRUE) %>%
  filter(Year %in% (AYR+1:2)) %>%
  pivot_longer(cols=c(-Stock, -Year), names_to='metric', values_to='value') %>%
  pivot_wider(names_from=Year, values_from=value)
rec_table1$scenario <- rec_table2$Stock <- NULL
rec_table2[,2:3] <- rec_table2[,2:3]
rec_table <- bind_rows(rec_table1, rec_table2)

## change order to match SAFE format & magnitudes
rec_table <-rec_table[c(11,6,3,4,5,2,1,1,9,8,8),] 

# rec_table[c(1:5,9:11),2:3] <-formatC(rec_table[c(1:5,9:11),2:3] , format="d", big.mark=",") 
write_csv(rec_table, paste0(getwd(), "/", AYR, '/Tier_3/prg_AKSK_14_2_HP2024/rec_table_HP2024.csv'))

