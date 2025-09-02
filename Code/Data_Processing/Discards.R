# Setup----

datapath <- paste0(getwd(), "/Data/", AYR)

CAS_dat <- read_csv(paste0(datapath, "/confidential_CAS_skates_Nov8", AYR, ".csv"))

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
  filter(AGENCY_GEAR_CODE %in% c('HAL', 'TRW')) %>% 
  group_by(YEAR, RETAINED_OR_DISCARDED, FMP_GEAR) %>% 
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
  select(YEAR, FMP_GEAR, FMP_SUBAREA, Dprop) %>% 
  pivot_wider(names_from = FMP_GEAR, values_from = Dprop)


ggplot(fsubgear_tot, aes(x = YEAR, y = Dprop, shape = FMP_SUBAREA, color = FMP_GEAR))+
  geom_line()+
  facet_grid(.~FMP_SUBAREA)

write_csv(fsubgear_tot, paste0(datapath, "/skate_discards.csv"))
