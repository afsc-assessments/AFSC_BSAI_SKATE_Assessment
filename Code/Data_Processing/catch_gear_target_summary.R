# Setup----

datapath <- paste0(getwd(), "/Data/", AYR)

CAS_dat <- read_csv(paste0(datapath, "/confidential_CAS_skates", AYR, ".csv"))

# BSAI total catch by target fishery ----
BSAI_tot <- CAS_dat %>% 
  group_by(Year = YEAR) %>% 
  summarise(tot_catch = sum(WEIGHT_POSTED))

BSAI_targ <- CAS_dat %>% 
  group_by(Year = YEAR, Target = TRIP_TARGET_NAME) %>% 
  summarise(tar_catch = sum(WEIGHT_POSTED)) %>% 
  left_join(BSAI_tot) %>% 
  mutate(prop_catch = tar_catch/tot_catch)

ggplot(BSAI_targ, aes(x = Year, y = tar_catch, fill = Target))+
  geom_bar(position = "fill", stat = "identity")

BSAI_mtarg <- BSAI_targ %>% 
  group_by(Target) %>% 
  summarise(mprop = mean(prop_catch))

BSAI_m5targ <- BSAI_targ %>% 
  filter(Year >= AYR-4) %>% #includes current year because most catch is completed by Oct 1
  group_by(Target) %>% 
  summarise(mprop = mean(prop_catch))

# Fmp_sub area by target fishery ----
fsub_tot <- CAS_dat %>% 
  group_by(Year = YEAR, FMP_SUBAREA) %>% 
  summarise(tot_catch = sum(WEIGHT_POSTED))

fsub_targ <- CAS_dat %>% 
  group_by(Year = YEAR, Target = TRIP_TARGET_NAME, FMP_SUBAREA) %>% 
  summarise(tar_catch = sum(WEIGHT_POSTED)) %>% 
  left_join(fsub_tot) %>% 
  mutate(prop_catch = tar_catch/tot_catch)

ggplot(fsub_targ, aes(x = Year, y = tar_catch, fill = Target))+
  geom_bar(position = "fill", stat = "identity")+
  facet_wrap(FMP_SUBAREA~.)

fsub_mtarg <- fsub_targ %>% 
  group_by(Target, FMP_SUBAREA) %>% 
  summarise(mprop = mean(prop_catch))

fsub_m5targ <- fsub_targ %>% 
  filter(Year >= AYR-4) %>% #includes current year because most catch is completed by Oct 1
  group_by(Target, FMP_SUBAREA) %>% 
  summarise(mprop = mean(prop_catch))

# BSAI total catch by gear ----
BSAI_tot <- CAS_dat %>% 
  group_by(Year = YEAR) %>% 
  summarise(tot_catch = sum(WEIGHT_POSTED))

BSAI_gear <- CAS_dat %>% 
  group_by(Year = YEAR, FMP_GEAR) %>% 
  summarise(gear_catch = sum(WEIGHT_POSTED)) %>% 
  left_join(BSAI_tot) %>% 
  mutate(prop_catch = gear_catch/tot_catch)

ggplot(BSAI_gear, aes(x = Year, y = gear_catch, fill = FMP_GEAR))+
  geom_bar(position = "fill", stat = "identity")

BSAI_mgear <- BSAI_gear %>% 
  group_by(FMP_GEAR) %>% 
  summarise(mprop = mean(prop_catch))

BSAI_m5gear <- BSAI_gear %>% 
  filter(Year >= AYR-4) %>% #includes current year because most catch is completed by Oct 1
  group_by(FMP_GEAR) %>% 
  summarise(mprop = mean(prop_catch))
