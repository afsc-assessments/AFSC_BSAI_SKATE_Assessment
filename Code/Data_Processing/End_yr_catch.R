# estimation of total catch in current year  ----
# Updated 2023_7_23 by C. Tribuzio

#notes: end of year catch is the mean prop of catch after date of data pull over last 5 years
# CAS species done separately, all other species have the same rate applied

# setup----
datapath<-paste(getwd(),"/Data/",AYR,sep="")

codes <- read_csv(paste0(getwd(),"/Data/BSAIskate_species_codes.csv"))

# Get CAS data ----
# also standardizes names
CAS <- read_csv(paste0(getwd(),"/Data/", AYR, "/confidential_CAS_skates", AYR, ".csv")) %>%
  clean_names() %>% 
  filter(year >= AYR-5) %>%  #filters 5 previous complete years, plus current year
  mutate(cmonth = month(week_end_date),
         status = if_else(cmonth >= 10, "Y", "N")) #cuts off catch as before or after Oct 1

#All CAS skates AYR catch----
sk_prop <- CAS %>% 
  #filter(agency_species_code == 703) %>% 
  mutate(agency_gear_code = if_else(agency_gear_code == "POT", "TRW", agency_gear_code)) %>% 
  mutate(agency_gear_code = if_else(agency_gear_code == "NPT", "TRW",
                                    if_else(agency_gear_code == "PTR", "TRW",
                                            if_else(agency_gear_code == "POT", "TRW", "HAL")))) %>% 
  group_by(year, species_name, status, agency_gear_code) %>% 
  summarise(pcatch = sum(weight_posted, na.rm = T)) %>% 
  pivot_wider(names_from = status, values_from = pcatch) %>% 
  replace(is.na(.), 0) %>% 
  mutate(totcatch = sum(N, Y),
         eyr_prop = Y/totcatch) %>% 
  filter(year < AYR) %>% 
  group_by(species_name, agency_gear_code) %>% 
  summarise(mean5yr = mean(eyr_prop, na.rm = T)) %>% 
  rename(CAS_name = species_name)

#Apply to species specific catch estimates ----
CAS_speccatch <- read_csv(paste0(datapath, "/confidential_CAS_SKpart", AYR, ".csv")) %>% 
  select(OBS_name = NAMES, YEAR, fmp_gear = AGENCY_GEAR_CODE, CATCH = CATCH_WEIGHT) %>% 
  mutate(fmp_gear = if_else(fmp_gear == "NPT", "TRW",
                            if_else(fmp_gear == "PTR", "TRW",
                                    if_else(fmp_gear == "POT", "TRW", "HAL")))) %>% 
  group_by(OBS_name, YEAR, fmp_gear) %>% 
  summarise(catch = sum(CATCH)) %>% 
  rename(CATCH = catch,
         agency_gear_code = fmp_gear)

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
  select(OBS_name, YEAR, agency_gear_code, CATCH = eyrCatch)

# non CAS species, i.e., "other" or "unidentified"
osk_prop <- sk_prop %>% 
  filter(CAS_name == "skate, other")

osk_AYRtotcatch <- CAS_speccatch %>%
  filter(OBS_name %nin% CAS_specs$OBS_name,
         YEAR == AYR) %>% 
  left_join(osk_prop) %>% 
  mutate(eyrCatch = CATCH/(1-mean5yr)) %>% 
  select(OBS_name, YEAR, agency_gear_code, CATCH = eyrCatch)

# combine species all together
all_skate_catch <- CAS_speccatch %>% 
  filter(YEAR != AYR) %>% 
  bind_rows(CAS_AYRtotcatch, osk_AYRtotcatch)

# fix proportioned AYR catch to match estimated total catch for AYR
allAYR <- all_skate_catch %>% 
  filter(YEAR == AYR)

sk_partcatch <- read_csv(paste0(getwd(),"/Data/", AYR, "/confidential_CAS_skpart", AYR, ".csv")) %>% 
  filter(YEAR == AYR) %>% 
  mutate(GEAR = if_else(AGENCY_GEAR_CODE == "HAL", "HAL", "TRW")) %>% 
  group_by(YEAR, GEAR, NAMES) %>% 
  summarise(CATCH = sum(CATCH_WEIGHT)) %>% 
  pivot_wider(names_from = GEAR, values_from = CATCH) %>% 
  mutate(propHAL = HAL/(HAL+TRW)) %>% #not sure what the point of this step was, but doesn't change the outcome at all
  select(YEAR, propHAL, NAMES) %>% 
  rename(OBS_name = NAMES) %>% 
  left_join(allAYR)

test <- sk_partcatch %>% 
  group_by(YEAR) %>% 
  summarise(tcatch = sum(CATCH))


write_csv(all_skate_catch, paste0(datapath, "/BSAIsk_totcatch_", AYR, ".csv"))
