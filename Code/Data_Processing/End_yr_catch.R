# estimation of total catch in current year  ----
# Updated Oct 5 2023 by C. Tribuzio

#notes: end of year catch is the mean prop of catch after date of data pull over last 5 years
# too many steps to do this for each species, so look at prop for AK skate by itself, 
# and rest of skates combined. Apply rest of skates combined to each of the species specific skates
# setup----
datapath<-paste(getwd(),"/Data/",AYR,sep="")

codes <- read_csv(paste0(getwd(),"/Data/BSAIskate_species_codes.csv"))

# Get CAS data ----
# also standardizes names
CAS <- read_csv(paste0(getwd(),"/Data/", AYR, "/confidential_CAS_skates2023.csv")) %>%
  clean_names() %>% 
  filter(year >= AYR-6) %>%  #filters 5 previous complete years, plus current year
  mutate(cmonth = month(week_end_date),
         status = if_else(cmonth >= 10, "Y", "N")) #cuts off catch as before or after Oct 1

#All CAS skates AYR catch----
sk_prop <- CAS %>% 
  #filter(agency_species_code == 703) %>% 
  group_by(year, species_name, status, fmp_gear) %>% 
  summarise(pcatch = sum(weight_posted, na.rm = T)) %>% 
  pivot_wider(names_from = status, values_from = pcatch) %>% 
  replace(is.na(.), 0) %>% 
  mutate(totcatch = sum(N, Y),
         eyr_prop = Y/totcatch) %>% 
  group_by(species_name, fmp_gear) %>% 
  summarise(mean5yr = mean(eyr_prop, na.rm = T)) %>% 
  rename(CAS_name = species_name)

#Apply to species specific catch estimates ----
CAS_speccatch <- read_csv(paste0(datapath, "/confidential_CAS_SKpart2023.csv")) %>% 
  select(OBS_name = NAMES, YEAR, fmp_gear = AGENCY_GEAR_CODE, CATCH = CATCH_WEIGHT) %>% 
  mutate(fmp_gear = if_else(fmp_gear == "NPT", "TRW",
                            if_else(fmp_gear == "PTR", "TRW",
                                    if_else(fmp_gear == "POT", "TRW", "HAL")))) %>% 
  group_by(OBS_name, YEAR, fmp_gear) %>% 
  summarise(catch = sum(CATCH)) %>% 
  rename(CATCH = catch)

# Do CAS species first
CAS_specs <- codes %>% 
  filter(!is.na(CAS_name)) %>%
  filter(CAS_name != "skate, other") %>% 
  select(CAS_name, OBS_name)

CAS_prop <- sk_prop %>% 
  filter(CAS_name != "skate, other") %>% 
  left_join(CAS_specs)

CAS_AYRtotcatch <- CAS_speccatch %>%
  filter(OBS_name %in% CAS_specs$OBS_name,
         YEAR == AYR) %>% 
  left_join(CAS_prop) %>% 
  mutate(eyrCatch = if_else(mean5yr == 0, CATCH, CATCH/(1-mean5yr))) %>% 
  select(OBS_name, YEAR, fmp_gear, CATCH = eyrCatch)

# non CAS species, i.e., "other" or "unidentified"
osk_prop <- sk_prop %>% 
  filter(CAS_name == "skate, other",
         fmp_gear != 'POT')

osk_AYRtotcatch <- CAS_speccatch %>%
  filter(OBS_name %nin% CAS_specs$OBS_name,
         YEAR == AYR) %>% 
  left_join(osk_prop) %>% 
  mutate(eyrCatch = CATCH/(1-mean5yr)) %>% 
  select(OBS_name, YEAR, fmp_gear, CATCH = eyrCatch)

# put it all together
BSAI_skate_catch <- CAS_speccatch %>% 
  filter(YEAR != AYR) %>% 
  bind_rows(CAS_AYRtotcatch, osk_AYRtotcatch)

write_csv(BSAI_skate_catch, paste0(datapath, "/BSAIsk_totcatch_", AYR, ".csv"))
