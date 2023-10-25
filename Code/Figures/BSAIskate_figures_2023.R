# set up ----
# folder set up
dat_path <- paste0("DATA/", AYR); dir.create(dat_path)
out_path <- paste0("OUTPUT/", AYR, "/Tier5"); dir.create(out_path)

# data ----
CAS_dat <- read_csv(paste0(dat_path, '/confidential_CAS_skates', SYR, ".csv")) %>% 
  mutate(Gear = if_else(FMP_GEAR == 'HAL', 'HAL',
                   if_else(FMP_GEAR == 'JIG', 'HAL', 'TWL')),
         Target = if_else(TRIP_TARGET_NAME == 'Pacific Cod', 'PCod',
                          if_else(TRIP_TARGET_NAME == 'Yellowfin Sole - BSAI', 'YFS', 'Others')))
sk_catch <- read_csv(paste0(dat_path, '/BSAIsk_totcatch_', SYR, ".csv"))

# Catch figures ----
# Total skate catches by fmp sub area
tot_catch <- CAS_dat %>% 
  group_by(Year = YEAR) %>% 
  summarise(Catch = sum(WEIGHT_POSTED))
CAS2 <- CAS_dat %>% 
  group_by(Year = YEAR, FMP_SUBAREA) %>% 
  summarise(Catch = sum(WEIGHT_POSTED)) %>% 
  bind_rows(tot_catch) %>% 
  replace_na(list(FMP_SUBAREA = "Total"))
ggplot(CAS2, aes(x = Year, y = Catch, fill = FMP_SUBAREA, color = FMP_SUBAREA))+
  geom_line()

CAS_gear <- CAS_dat %>% 
  group_by(Year = YEAR, Gear) %>% 
  summarise(Catch = sum(WEIGHT_POSTED)) %>% 
  bind_rows(tot_catch) %>% 
  replace_na(list(Gear = "Total"))
ggplot(CAS_gear, aes(x = Year, y = Catch, fill = Gear, color = Gear))+
  geom_line()

# AK skate and others by target fishery----
tot2 <- CAS_dat %>% 
  group_by(Year = YEAR) %>% 
  summarise(Tot_Catch = sum(WEIGHT_POSTED))
CAS_targ <- CAS_dat %>% 
  group_by(Year = YEAR, Target) %>% 
  summarise(Catch = sum(WEIGHT_POSTED)) %>% 
  left_join(tot2) %>% 
  filter(Year >= 2010) %>% 
  mutate(prop = Catch/Tot_Catch)

ggplot(CAS_targ, aes(x = Year, y = Catch, fill = Target, color = Target))+
  geom_line()

# compare partitioned AK skate catch to CAS AK skate catch----
#don't look, it's not easy to explain, understand
CAS_AKsk <- CAS_dat %>%
  filter(SPECIES_NAME == "skate, Alaskan") %>% 
  group_by(Year = YEAR) %>% 
  summarise(Catch = sum(WEIGHT_POSTED)) %>% 
  mutate(Source = 'CAS')
tot_AK <- sk_catch %>% 
  filter(OBS_name == "ALASKA SKATE") %>% 
  group_by(Year = YEAR) %>% 
  summarise(Catch = sum(CATCH)) %>% 
  mutate(Source = 'Total')
AKcatch <- CAS_AKsk %>% 
  bind_rows(tot_AK) %>% 
  filter(Year >= 2010)
ggplot(AKcatch, aes(x = Year, y = Catch, fill = Source, color = Source))+
  geom_line()

# Survey Biomass ----
RACE_dat <- read_csv(paste0(dat_path, '/RACE_biomass_skates', SYR, ".csv")) %>% 
  filter(year >= 1999)

# need to make combined other biomass with CIs
AKshelf <- RACE_dat %>% 
  filter(survey == "EBS_SHELF" & RACE_name == 'Alaska skate') %>% 
  select(year, survey, biomass, bio_ll, bio_ul) %>% 
  mutate(Species = 'Alaska skate')

Other <- RACE_dat %>% 
  filter(!(survey == "EBS_SHELF" & RACE_name == 'Alaska skate')) %>% 
  group_by(year, survey) %>% 
  summarise(biomass = sum(biomass), biomass_var = sum(biomass_var)) %>% 
  mutate(cv = sqrt(biomass_var)/biomass,
         se = sqrt(biomass_var),
         bio_ll = biomass - 1.96*se,
         bio_ul = biomass + 1.96*se,
         Species = 'Other Skates') %>% 
  select(year, survey, biomass, bio_ll, bio_ul, Species)
sk_biom <- AKshelf %>% 
  bind_rows(Other)

ggplot(sk_biom, aes(x = year, y = biomass, fill = survey, color = survey))+
  geom_point()+
  geom_line()+
  geom_errorbar(aes(ymin = bio_ll, ymax = bio_ul))+
  facet_grid(Species~survey, scales = "free")

# retrospective plots----
setwd(paste0(getwd(), "/Code/Tier3/", AYR, "/Retrospective/M14_2_update/retrospectives"))
Models1 <- SSgetoutput(dirvec = c("retro0", "retro-1", "retro-2", 'retro-3', 'retro-4', 'retro-5', 'retro-6',
                                  'retro-7', 'retro-8', 'retro-9', 'retro-10', 'retro-11', 'retro-12'))
Models1_SS <- SSsummarize(Models1)
endyrvec <-Models1_SS$endyrs + 0:-12

baserun <- retrobio %>% 
  select(M14_2, Yr)

retrobio <- Models1_SS$SpawnBio %>% 
  filter(Yr <= AYR) %>% 
  rename(M14_2 = replist1,
         run2022 = replist2,
         run2021 = replist3,
         run2020 = replist4,
         run2019 = replist5,
         run2018 = replist6,
         run2017 = replist7,
         run2016 = replist8,
         run2015 = replist9,
         run2014 = replist10,
         run2013 = replist11,
         run2012 = replist12,
         run2011 = replist13) %>% 
  select(!Label) %>% 
  pivot_longer(!Yr, names_to = 'Peel', values_to = 'Peel_SSB') %>% 
  left_join(baserun) %>% 
  mutate(Peel_diff = (Peel_SSB-M14_2)/M14_2)

models <- unique(retrobio$Peel)

for (iline in 1:length(endyrvec)) {
  endyr <- endyrvec[iline]
  imodel <- models[iline]
  retrobio[retrobio$Peel == imodel & retrobio$Yr > endyr,]$Peel_SSB <- NA}
for (iline in 1:length(endyrvec)) {
  endyr <- endyrvec[iline]
  imodel <- models[iline]
  retrobio[retrobio$Peel == imodel & retrobio$Yr > endyr,]$Peel_diff <- NA}

ggplot(retrobio, aes(x = Yr, y = Peel_SSB, fill = Peel, color = Peel))+
  geom_line()
ggplot(retrobio, aes(x = Yr, y = Peel_diff, fill = Peel, color = Peel))+
  geom_line()

# projection figure ----
setwd("C:/Users/cindy.Tribuzio/Work/SAFE/Assessments/AFSC_BSAI_SKATE_Assessment")
#note, saved bigfile.out as .csv in excel, need to figure out how to streamline that
projout <- read_csv(paste0(getwd(), "/Code/Tier3/", AYR, "/Projections/prg_AKSK_14_2_2023/bigfile.csv")) %>% 
  select(Alternative, Yr, SSB) %>% 
  group_by(Alternative, Yr) %>% 
  summarise(SSB = mean(SSB))

B40 <- 71370
B35 <- 62449

ggplot(projout, aes(x = Yr, y = SSB, fill = as.factor(Alternative), color = as.factor(Alternative)))+
  geom_line()+
  geom_hline(yintercept = B40, linetype = "dashed")+
  geom_hline(yintercept = B35)
