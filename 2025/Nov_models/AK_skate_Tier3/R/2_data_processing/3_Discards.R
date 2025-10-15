# Setup----

libs <- c("tidyverse", "janitor")
if(length(libs[which(libs %in% rownames(installed.packages()) == FALSE )]) > 0) {
  install.packages(libs[which(libs %in% rownames(installed.packages()) == FALSE)])}
lapply(libs, library, character.only = TRUE)
'%nin%'<-Negate('%in%') #this is a handy function

AYR <- 2025

dat_path <- here::here(AYR, 'Nov_models', 'AK_skate_Tier3', 'data')
dir.create(dat_path)

CAS_dat <- read_csv(here::here(dat_path, paste0("confidential_CAS_skates_", AYR, ".csv"))) %>% 
  rename(FMP_GEAR = AGENCY_GEAR_CODE)

# BSAI total discards ----
BSAI_tot <- CAS_dat %>% 
  group_by(Year = YEAR, RETAINED_OR_DISCARDED) %>% 
  summarise(catch = sum(WEIGHT_POSTED)) %>% 
  pivot_wider(names_from = RETAINED_OR_DISCARDED, values_from = catch) %>% 
  mutate(tot_catch = sum(R, D),
         Dprop = D/tot_catch)

# FMP_subarea total discards ----
fsub_tot <- CAS_dat %>% 
  group_by(Year = YEAR, RETAINED_OR_DISCARDED, FMP_SUBAREA) %>% 
  summarise(catch = sum(WEIGHT_POSTED)) %>% 
  pivot_wider(names_from = RETAINED_OR_DISCARDED, values_from = catch) %>% 
  mutate(tot_catch = sum(R, D),
         Dprop = D/tot_catch) %>% 
  select(Year, FMP_SUBAREA, Dprop) %>% 
  pivot_wider(names_from = FMP_SUBAREA, values_from = Dprop)

# gear discards ----
gear_tot <- CAS_dat %>% 
  filter(FMP_GEAR %in% c('HAL', 'TRW')) %>% 
  group_by(Year = YEAR, RETAINED_OR_DISCARDED, FMP_GEAR) %>% 
  summarise(catch = sum(WEIGHT_POSTED)) %>% 
  pivot_wider(names_from = RETAINED_OR_DISCARDED, values_from = catch) %>% 
  mutate(tot_catch = sum(R, D, na.rm = T),
         Dprop = D/tot_catch) %>% 
  select(Year, FMP_GEAR, Dprop) %>% 
  pivot_wider(names_from = FMP_GEAR, values_from = Dprop)

# gear and fmp_sub area discards ----
fsubgear_tot <- CAS_dat %>% 
  filter(FMP_GEAR %in% c('HAL', 'TRW')) %>% 
  group_by(YEAR, RETAINED_OR_DISCARDED, FMP_GEAR, FMP_SUBAREA) %>% 
  summarise(catch = sum(WEIGHT_POSTED)) %>% 
  pivot_wider(names_from = RETAINED_OR_DISCARDED, values_from = catch) %>% 
  mutate(tot_catch = D+R,
         Dprop = D/tot_catch) %>% 
  select(YEAR, FMP_GEAR, FMP_SUBAREA, Dprop)


ggplot(fsubgear_tot, aes(x = YEAR, y = Dprop, shape = FMP_SUBAREA, color = FMP_GEAR))+
  geom_line()+
  facet_grid(.~FMP_SUBAREA)

write_csv(fsubgear_tot, here::here(dat_path, paste0("skate_discards_", AYR, ".csv")))
