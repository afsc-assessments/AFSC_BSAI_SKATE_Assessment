# set up ----
# folder set up
dat_path <- paste0("DATA/", AYR); dir.create(dat_path)
#out_path <- paste0("OUTPUT/", AYR, "/Tier5"); dir.create(out_path)

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
plot_totc <- ggplot(CAS2, aes(x = Year, y = Catch, color = FMP_SUBAREA))+
  geom_line(linewidth = 1.5)+
  labs(color = "", x = "", y = "Catch (t)", title = "Catch by FMP Subarea")+
  scale_y_continuous(labels = scales::comma_format(), expand=c(0, 0))+
  theme_bw()+
  theme(legend.position = "top")

CAS_gear <- CAS_dat %>% 
  group_by(Year = YEAR, Gear) %>% 
  summarise(Catch = sum(WEIGHT_POSTED)) %>% 
  bind_rows(tot_catch) %>% 
  replace_na(list(Gear = "Total"))
plot_gear <- ggplot(CAS_gear, aes(x = Year, y = Catch, fill = Gear, color = Gear))+
  geom_line(linewidth = 1.5)+
  labs(color = "Gear", x = "", y = "", title = "Catch by Gear Type")+
  scale_y_continuous(labels = scales::comma_format(), expand=c(0, 0))+
  theme_bw()+
  theme(legend.position = "top")

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

plot_targ <- ggplot(CAS_targ, aes(x = Year, y = Catch, fill = Target, color = Target))+
  geom_line(linewidth = 1.5)+
  labs(color = "", x = "Year", y = "Catch (t)", title = "Catch by Target Fishery")+
  scale_y_continuous(labels = scales::comma_format(), expand=c(0, 0))+
  theme_bw()+
  theme(legend.position = "top")

# species composition in catch ----
ann_c <- sk_catch %>% 
  group_by(YEAR) %>% 
  summarise(totc = sum(CATCH))
sk_mprop <- sk_catch %>% 
  group_by(YEAR, OBS_name) %>% 
  summarise(catch = sum(CATCH)) %>% 
  left_join(ann_c) %>% 
  mutate(prop = catch/totc) %>% 
  group_by(OBS_name) %>% 
  summarise(mprop = mean(prop)) %>% 
  mutate(group = if_else(mprop < 0.03, "Minor", OBS_name)) %>% 
  select(OBS_name, group)
sk_groupcatch <- sk_catch %>% 
  left_join(sk_mprop) %>% 
  group_by(YEAR, group) %>% 
  summarise(catch = sum(CATCH))
plot_specc <- ggplot(sk_groupcatch, aes(x = YEAR, y = catch, fill = group))+
  geom_bar(stat = "identity")+
  scale_fill_brewer(direction = -1)+
  labs(fill = "", x = "Year", y = "", title = "Catch by Species")+
  scale_y_continuous(labels = scales::comma_format(), expand=c(0, 0))+
  theme_bw()+
  theme(legend.position = "top")

plot_catchsumm <- (plot_totc + plot_gear)/(plot_targ + plot_specc)

ggsave(path = paste0(getwd(), "/", AYR, "/Full_complex"),
       "Catch_summary.png",plot=plot_catchsumm,dpi=600,width = 8, height = 8)

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
  geom_line(linewidth = 1.5)+
  labs(color = "Target", x = "Year", y = "Catch (t)")+
  scale_y_continuous(labels = scales::comma_format())+
  theme_bw()

# Survey Biomass ----
RACE_dat <- read_csv(paste0(dat_path, '/RACE_biomass_skates', SYR, ".csv")) %>% 
  filter(year >= 1999)

# need to make combined other biomass with CIs
AKshelf <- RACE_dat %>% 
  filter(survey == 98 & species_code == 471) %>% 
  select(year, survey, biomass, bio_ll, bio_ul) %>% 
  mutate(Species = "Alaska Skate",
         survey = "Shelf")
plot_AKshelf <- ggplot(AKshelf, aes(x = year, y = biomass/1000, fill = survey, color = survey))+
  geom_point()+
  geom_line()+
  geom_errorbar(aes(ymin = bio_ll/1000, ymax = bio_ul/1000))+
  facet_grid(Species~survey, scales = "free")+
  labs(x = "", y = "")+
  scale_y_continuous(labels = scales::comma_format())+
  scale_color_manual(values = "#21908CFF")+
  theme_bw()+
  theme(legend.position = "none")

Other <- RACE_dat %>% 
  filter(!(survey == 98 & species_code == 471))
Other <- Other %>% 
  filter(!(year == 1999 & species_code == 405)) %>% # added this because there is a wonky 405 on 1999 in the new gap products
  group_by(year, survey) %>% 
  summarise(biomass = sum(biomass), biomass_var = sum(biomass_var)) %>% 
  mutate(cv = sqrt(biomass_var)/biomass,
         se = sqrt(biomass_var),
         bio_ll = biomass - 1.96*se,
         bio_ul = biomass + 1.96*se,
         Species = 'Other Skates',
         survey = if_else(survey == 98, "Shelf",
                          if_else(survey == 52, "AI", "Slope"))) %>% 
  select(year, survey, biomass, bio_ll, bio_ul, Species)
sk_biom <- AKshelf %>% 
  bind_rows(Other)

plot_othersurvey <- ggplot(Other, aes(x = year, y = biomass/1000, fill = survey, color = survey))+
  geom_point()+
  geom_line()+
  geom_errorbar(aes(ymin = bio_ll/1000, ymax = bio_ul/1000))+
  facet_grid(Species~survey, scales = "free")+
  labs(x = "Year", y = "")+
  scale_y_continuous(labels = scales::comma_format())+
  scale_color_viridis(discrete = TRUE) +
  theme_bw()+
  theme(legend.position = "none")

plot_survsumm <- plot_AKshelf/plot_othersurvey &
  theme(plot.margin = margin(0.5, 0.5, 0.5, 0))
plot_survsumm <- wrap_elements(plot_survsumm) +
  labs(tag = "Survey Biomass (1,000s t)") +
  theme(
    plot.tag = element_text(size = rel(1), angle = 90),
    plot.tag.position = "left"
  )
 
ggsave(path = paste0(getwd(), "/", AYR, "/Full_complex"),
        "survey_biomass.png",plot=plot_survsumm,dpi=600,width = 6, height = 4)

# model bridging plot ----
datapath <- paste0(getwd(), "/", AYR, "/Tier 3/Model_Runs")

# Current model name
Model_name_old1 <- "M14_2_vold"
Model_name_old2 <- "M14_2_vbridge"
Model_name_fixdat <- "M14_2_fix_old"
Model_name_adddat <- "M14_2_addnew"
Model_name_new <- "M14_2_update"

# read model outputs
setwd(paste0(getwd(), "/", AYR, "/Tier3/Model_Runs"))
bridge_out <- SSgetoutput(dirvec = c("M14_2_vold","M14_2_vbridge", "M14_2_fix_old", 'M14_2_addnew', 'M14_2_update'))
setwd("C:/Users/cindy.Tribuzio/Work/SAFE/Assessments/AFSC_BSAI_SKATE_Assessment")

model_comp <- r4ss::SSsummarize(bridge_out)

brg_SSBUL <- model_comp$SpawnBioUpper %>% 
  filter(Yr <= AYR) %>% 
  mutate(replist2 = if_else(Yr > 2020, NA, replist2),
         replist3 = if_else(Yr > 2020, NA, replist3)) %>% 
  rename(M14.2 = replist1,
         M14.2a = replist2,
         M14.2b = replist3,
         M14.2c = replist4,
         M14.2d = replist5) %>% 
  select(!Label) %>% 
  pivot_longer(!Yr, names_to = 'Model', values_to = 'SSBUL')
brg_SSBLL <- model_comp$SpawnBioLower %>% 
  filter(Yr <= AYR) %>% 
  mutate(replist2 = if_else(Yr > 2020, NA, replist2),
         replist3 = if_else(Yr > 2020, NA, replist3)) %>% 
  rename(M14.2 = replist1,
         M14.2a = replist2,
         M14.2b = replist3,
         M14.2c = replist4,
         M14.2d = replist5) %>% 
  select(!Label) %>% 
  pivot_longer(!Yr, names_to = 'Model', values_to = 'SSBLL')

brg_SSB <- model_comp$SpawnBio %>% 
  filter(Yr <= AYR) %>% 
  mutate(replist2 = if_else(Yr > 2020, NA, replist2),
         replist3 = if_else(Yr > 2020, NA, replist3)) %>% 
  rename(M14.2 = replist1,
         M14.2a = replist2,
         M14.2b = replist3,
         M14.2c = replist4,
         M14.2d = replist5) %>% 
  select(!Label) %>% 
  pivot_longer(!Yr, names_to = 'Model', values_to = 'SSB') %>% 
  left_join(brg_SSBLL) %>% 
  left_join(brg_SSBUL)

bcolors <- viridis(n=5)
plot_bridgebase <- ggplot(brg_SSB[brg_SSB$Model == "M14.2",], 
                          aes(x = Yr, y = SSB/1000, color = Model))+
  geom_ribbon(aes(ymin = SSBLL/1000, ymax = SSBUL/1000), 
              show.legend = F, fill = "grey70", alpha = 0.5, color = NA)+
  geom_line()+
  labs(y = "Spawning Biomass (1,000s t)", x = "Year", color = "")+
  scale_y_continuous(labels = scales::comma_format())+
  scale_color_viridis(discrete = TRUE) +
  coord_cartesian(ylim = c(75, 350), xlim = c(1950, 2025))+
  theme_bw()+
  theme(legend.position = c(0.1, 0.3),
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.spacing.y = unit(0.04, 'cm'))+
  guides(fill = guide_legend(byrow = TRUE))
ggsave(path = paste0(getwd(), "/", AYR, "/Tier3/Output"),
       "Bridge_14_2.png",plot=plot_bridgebase,dpi=600,width = 6, height = 4)

plot_bridgea <- ggplot(brg_SSB[brg_SSB$Model == "M14.2a",], 
                       aes(x = Yr, y = SSB/1000, color = Model))+
  geom_ribbon(aes(ymin = SSBLL/1000, ymax = SSBUL/1000), 
              show.legend = F, fill = "grey70", alpha = 0.5, color = NA)+
  geom_line()+
  labs(y = "Spawning Biomass (1,000s t)", x = "Year", color = "")+
  scale_y_continuous(labels = scales::comma_format())+
  scale_color_manual(values = bcolors[2]) +
  coord_cartesian(ylim = c(75, 350), xlim = c(1950, 2025))+
  theme_bw()+
  theme(legend.position = c(0.1, 0.3),
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.spacing.y = unit(0.04, 'cm'))+
  guides(fill = guide_legend(byrow = TRUE))
ggsave(path = paste0(getwd(), "/", AYR, "/Tier3/Output"),
       "Bridge_14_2a.png",plot=plot_bridgea,dpi=600,width = 6, height = 4)

plot_bridgeb <- ggplot(brg_SSB[brg_SSB$Model == "M14.2b",], aes(x = Yr, y = SSB/1000, color = Model))+
  geom_ribbon(aes(ymin = SSBLL/1000, ymax = SSBUL/1000), 
              show.legend = F, fill = "grey70", alpha = 0.5, color = NA)+
  geom_line()+
  labs(y = "Spawning Biomass (1,000s t)", x = "Year", color = "")+
  scale_y_continuous(labels = scales::comma_format())+
  scale_color_manual(values = bcolors[3]) +
  coord_cartesian(ylim = c(75, 350), xlim = c(1950, 2025))+
  theme_bw()+
  theme(legend.position = c(0.1, 0.3),
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.spacing.y = unit(0.04, 'cm'))+
  guides(fill = guide_legend(byrow = TRUE))
ggsave(path = paste0(getwd(), "/", AYR, "/Tier3/Output"),
       "Bridge_14_2b.png",plot=plot_bridgeb,dpi=600,width = 6, height = 4)

plot_bridgec <- ggplot(brg_SSB[brg_SSB$Model == "M14.2c",], aes(x = Yr, y = SSB/1000, color = Model))+
  geom_ribbon(aes(ymin = SSBLL/1000, ymax = SSBUL/1000), 
              show.legend = F, fill = "grey70", alpha = 0.5, color = NA)+
  geom_line()+
  labs(y = "Spawning Biomass (1,000s t)", x = "Year", color = "")+
  scale_y_continuous(labels = scales::comma_format())+
  scale_color_manual(values = bcolors[4]) +
  coord_cartesian(ylim = c(75, 350), xlim = c(1950, 2025))+
  theme_bw()+
  theme(legend.position = c(0.1, 0.3),
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.spacing.y = unit(0.04, 'cm'))+
  guides(fill = guide_legend(byrow = TRUE))
ggsave(path = paste0(getwd(), "/", AYR, "/Tier3/Output"),
       "Bridge_14_2c.png",plot=plot_bridgec,dpi=600,width = 6, height = 4)

plot_bridged <- ggplot(brg_SSB[brg_SSB$Model == "M14.2d",], aes(x = Yr, y = SSB/1000, color = Model))+
  geom_ribbon(aes(ymin = SSBLL/1000, ymax = SSBUL/1000), 
              show.legend = F, fill = "grey70", alpha = 0.5, color = NA)+
  geom_line()+
  labs(y = "Spawning Biomass (1,000s t)", x = "Year", color = "")+
  scale_y_continuous(labels = scales::comma_format())+
  scale_color_manual(values = bcolors[5]) +
  coord_cartesian(ylim = c(75, 350), xlim = c(1950, 2025))+
  theme_bw()+
  theme(legend.position = c(0.1, 0.3),
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.spacing.y = unit(0.04, 'cm'))+
  guides(fill = guide_legend(byrow = TRUE))
ggsave(path = paste0(getwd(), "/", AYR, "/Tier3/Output"),
       "Bridge_14_2d.png",plot=plot_bridged,dpi=600,width = 6, height = 4)

brg_SSB2 <- brg_SSB %>% 
  mutate(SSB = SSB/1000,
         SSBUL = SSBUL/1000,
         SSBLL = SSBLL/1000)
plot_bridge <- ggplot(as.data.frame(brg_SSB2), aes(x = Yr, y = SSB, color = Model))+
  geom_line()+
  geom_ribbon(aes(x = Yr, ymin = SSBLL, ymax = SSBUL, fill = Model), 
              show.legend = F, fill = "grey70", alpha = 0.1, linetype = 0)+
  labs(y = "Spawning Biomass (1,000s t)", x = "Year", color = "")+
  scale_y_continuous(labels = scales::comma_format())+
  scale_color_viridis(discrete = T) +
  coord_cartesian(ylim = c(75, 350), xlim = c(1950, 2025))+
  theme_bw()+
  theme(legend.position = c(0.1, 0.3),
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.spacing.y = unit(0.04, 'cm'))+
  guides(fill = guide_legend(byrow = TRUE))
ggsave(path = paste0(getwd(), "/", AYR, "/Tier3/Output"),
       "Bridge_all.png",plot=plot_bridge,dpi=600,width = 6, height = 4)

# retrospective plots----
setwd(paste0(getwd(), "/", AYR, "/Tier3/Retrospective/M14_2_update/retrospectives"))
Models1 <- SSgetoutput(dirvec = c("retro0", "retro-1", "retro-2", 'retro-3', 'retro-4', 'retro-5', 'retro-6',
                                  'retro-7', 'retro-8', 'retro-9', 'retro-10', 'retro-11', 'retro-12'))
setwd("C:/Users/cindy.Tribuzio/Work/SAFE/Assessments/AFSC_BSAI_SKATE_Assessment")

Models1_SS <- SSsummarize(Models1)
endyrvec <-Models1_SS$endyrs + 0:-12

#SSB
baserun <- Models1_SS$SpawnBio %>% 
  filter(Yr <= AYR) %>% 
  rename(M14_2d = replist1) %>% 
  select(M14_2d, Yr)

retrobioLL <- Models1_SS$SpawnBioLower %>% 
  filter(Yr <= AYR) %>% 
  rename(M14_2d = replist1,
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
  pivot_longer(!Yr, names_to = 'Peel', values_to = 'Peel_SSBLL')
retrobioUL <- Models1_SS$SpawnBioUpper %>% 
  filter(Yr <= AYR) %>% 
  rename(M14_2d = replist1,
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
  pivot_longer(!Yr, names_to = 'Peel', values_to = 'Peel_SSBUL')

retrobio <- Models1_SS$SpawnBio %>% 
  filter(Yr <= AYR) %>% 
  rename(M14_2d = replist1,
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
  mutate(Peel_diff = (Peel_SSB-M14_2d)/M14_2d) %>% 
  left_join(retrobioLL) %>% 
  left_join(retrobioUL)

models <- unique(retrobio$Peel)

for (iline in 1:length(endyrvec)) {
  endyr <- endyrvec[iline]
  imodel <- models[iline]
  retrobio[retrobio$Peel == imodel & retrobio$Yr > endyr,]$Peel_SSB <- NA}
for (iline in 1:length(endyrvec)) {
  endyr <- endyrvec[iline]
  imodel <- models[iline]
  retrobio[retrobio$Peel == imodel & retrobio$Yr > endyr,]$Peel_diff <- NA}
for (iline in 1:length(endyrvec)) {
  endyr <- endyrvec[iline]
  imodel <- models[iline]
  retrobio[retrobio$Peel == imodel & retrobio$Yr > endyr,]$Peel_SSBLL <- NA}
for (iline in 1:length(endyrvec)) {
  endyr <- endyrvec[iline]
  imodel <- models[iline]
  retrobio[retrobio$Peel == imodel & retrobio$Yr > endyr,]$Peel_SSBUL <- NA}

#recruits
baserun_rec <- Models1_SS$recruits %>% 
  filter(Yr <= AYR) %>% 
  rename(M14_2d = replist1) %>% 
  select(M14_2d, Yr)

retrorecLL <- Models1_SS$recruitsLower %>% 
  filter(Yr <= AYR) %>% 
  rename(M14_2d = replist1,
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
  pivot_longer(!Yr, names_to = 'Peel', values_to = 'Peel_recLL')
retrorecUL <- Models1_SS$recruitsUpper %>% 
  filter(Yr <= AYR) %>% 
  rename(M14_2d = replist1,
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
  pivot_longer(!Yr, names_to = 'Peel', values_to = 'Peel_recUL')

retrorec <- Models1_SS$recruits %>% 
  filter(Yr <= AYR) %>% 
  rename(M14_2d = replist1,
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
  pivot_longer(!Yr, names_to = 'Peel', values_to = 'Peel_rec') %>% 
  left_join(baserun) %>% 
  mutate(Peel_diff = (Peel_rec-M14_2d)/M14_2d) %>% 
  left_join(retrorecLL) %>% 
  left_join(retrorecUL)

models <- unique(retrorec$Peel)

for (iline in 1:length(endyrvec)) {
  endyr <- endyrvec[iline]
  imodel <- models[iline]
  retrorec[retrorec$Peel == imodel & retrorec$Yr > endyr,]$Peel_rec <- NA}
for (iline in 1:length(endyrvec)) {
  endyr <- endyrvec[iline]
  imodel <- models[iline]
  retrorec[retrorec$Peel == imodel & retrorec$Yr > endyr,]$Peel_diff <- NA}
for (iline in 1:length(endyrvec)) {
  endyr <- endyrvec[iline]
  imodel <- models[iline]
  retrorec[retrorec$Peel == imodel & retrorec$Yr > endyr,]$Peel_recLL <- NA}
for (iline in 1:length(endyrvec)) {
  endyr <- endyrvec[iline]
  imodel <- models[iline]
  retrorec[retrorec$Peel == imodel & retrorec$Yr > endyr,]$Peel_recUL <- NA}

#SSB Plots
plot_retrossb <- ggplot(retrobio, aes(x = Yr, y = Peel_SSB/1000, color = Peel))+
  geom_ribbon(aes(x = Yr, ymin = Peel_SSBLL/1000, ymax = Peel_SSBUL/1000, fill = Model), 
              show.legend = F, fill = "grey70", alpha = 0.1, linetype = 0)+
  geom_line()+
  labs(y = "Spawning Biomass (1,000s t)", x = "", color = "")+
  scale_y_continuous(labels = scales::comma_format())+
  scale_color_viridis(discrete = T) +
  #coord_cartesian(ylim = c(75, 350), xlim = c(1950, 2025))+
  theme_bw()+
  theme(legend.position = "none")+
  guides(fill = guide_legend(byrow = TRUE))
plot_retrossb10 <- ggplot(retrobio[retrobio$Yr >= 2013,], aes(x = Yr, y = Peel_SSB/1000, color = Peel))+
  geom_ribbon(aes(x = Yr, ymin = Peel_SSBLL/1000, ymax = Peel_SSBUL/1000, fill = Model), 
              show.legend = F, fill = "grey70", alpha = 0.1, linetype = 0)+
  geom_line()+
  labs(y = "", x = "", color = "")+
  scale_y_continuous(labels = scales::comma_format())+
  scale_x_continuous(breaks =c(2013, 2015, 2017, 2019, 2021, 2023))+
  scale_color_viridis(discrete = T) +
  #coord_cartesian(ylim = c(75, 350), xlim = c(1950, 2025))+
  theme_bw()+
  theme(legend.position = "none")+
  guides(fill = guide_legend(byrow = TRUE))
plot_retrodiff <- ggplot(retrobio, aes(x = Yr, y = Peel_diff, color = Peel))+
  geom_line()+
  labs(y = "SSB Relative Difference", x = "Year", color = "")+
  scale_y_continuous(labels = scales::comma_format())+
  scale_color_viridis(discrete = T) +
  #coord_cartesian(ylim = c(75, 350), xlim = c(1950, 2025))+
  theme_bw()+
  theme(legend.position = "none")+
  guides(fill = guide_legend(byrow = TRUE))
plot_retrodiff10 <- ggplot(retrobio[retrobio$Yr >= 2013,], aes(x = Yr, y = Peel_diff, color = Peel))+
  geom_line()+
  labs(y = "", x = "Year", color = "")+
  scale_y_continuous(labels = scales::comma_format())+
  scale_x_continuous(breaks =c(2013, 2015, 2017, 2019, 2021, 2023))+
  scale_color_viridis(discrete = T) +
  #coord_cartesian(ylim = c(75, 350), xlim = c(1950, 2025))+
  theme_bw()+
  theme(legend.position = "none")+
  guides(fill = guide_legend(byrow = TRUE))
#Recruitment Plots
plot_retrorec <- ggplot(retrorec, aes(x = Yr, y = Peel_rec/1000, color = Peel))+
  geom_ribbon(aes(x = Yr, ymin = Peel_recLL/1000, ymax = Peel_recUL/1000, fill = Model), 
              show.legend = F, fill = "grey70", alpha = 0.1, linetype = 0)+
  geom_line()+
  labs(y = "Recruits (1,000s t)", x = "", color = "")+
  scale_y_continuous(labels = scales::comma_format())+
  scale_color_viridis(discrete = T) +
  #coord_cartesian(ylim = c(75, 350), xlim = c(1950, 2025))+
  theme_bw()+
  theme(legend.position = "none")+
  guides(fill = guide_legend(byrow = TRUE))
plot_retrorec10 <- ggplot(retrorec[retrorec$Yr >= 2013,], aes(x = Yr, y = Peel_rec/1000, color = Peel))+
  geom_ribbon(aes(x = Yr, ymin = Peel_recLL/1000, ymax = Peel_recUL/1000, fill = Model), 
              show.legend = F, fill = "grey70", alpha = 0.1, linetype = 0)+
  geom_line()+
  labs(y = "", x = "", color = "")+
  scale_y_continuous(labels = scales::comma_format())+
  scale_x_continuous(breaks =c(2013, 2015, 2017, 2019, 2021, 2023))+
  scale_color_viridis(discrete = T) +
  #coord_cartesian(ylim = c(75, 350), xlim = c(1950, 2025))+
  theme_bw()+
  theme(legend.position = "none")+
  guides(fill = guide_legend(byrow = TRUE))
plot_retrorecdiff <- ggplot(retrobio, aes(x = Yr, y = Peel_diff, color = Peel))+
  geom_line()+
  labs(y = "Recruitment Relative Difference", x = "Year", color = "")+
  scale_y_continuous(labels = scales::comma_format())+
  scale_color_viridis(discrete = T) +
  #coord_cartesian(ylim = c(75, 350), xlim = c(1950, 2025))+
  theme_bw()+
  theme(legend.background = element_blank(),
        legend.key = element_blank(),
        legend.spacing.y = unit(0.04, 'cm'))+
  guides(fill = guide_legend(byrow = TRUE))

plot_retro <- (plot_retrossb / plot_retrorec / plot_retrodiff) +
  plot_layout(guides = "collect") & theme(legend.position = 'bottom')
plot_retro10 <- ((plot_retrossb + plot_retrossb10) /
                   (plot_retrorec + plot_retrorec10) /
                   (plot_retrodiff + plot_retrodiff10)) +
  plot_layout(guides = "collect") & theme(legend.position = 'bottom')
ggsave(path = paste0(getwd(), "/", AYR, "/Tier3/Output"),
       "Retrospecitve10.png",plot=plot_retro10,dpi=600,width = 6, height = 8)

# projection figure ----
setwd("C:/Users/cindy.Tribuzio/Work/SAFE/Assessments/AFSC_BSAI_SKATE_Assessment")
#note, saved bigfile.out as .csv in excel, need to figure out how to streamline that
projout <- read_csv(paste0(getwd(), "/", AYR, "/Tier3/Projections/prg_AKSK_14_2_2023/bigfile.csv")) %>% 
  select(Alternative, Yr, SSB) %>% 
  group_by(Alternative, Yr) %>% 
  summarise(SSB = mean(SSB))

B40 <- 71370
B35 <- 62449

plot_proj <- ggplot(projout, aes(x = Yr, y = SSB/1000, color = as.factor(Alternative), shape = as.factor(Alternative)))+
  geom_line()+
  geom_point()+
  geom_hline(yintercept = B40/1000, linetype = "dashed")+
  geom_hline(yintercept = B35/1000)+
  labs(y = "Spawning Biomass (1,000s t)", x = "Year", color = "")+
  scale_y_continuous(labels = scales::comma_format())+
  scale_color_viridis(discrete = T) +
  #coord_cartesian(ylim = c(75, 350), xlim = c(1950, 2025))+
  theme_bw()

ggsave(path = paste0(getwd(), "/", AYR, "/Tier3/Output"),
       "Projections.png",plot=plot_proj,dpi=600,width = 6, height = 4)

# Fishing Mortality ----
setwd(paste0(getwd(), "/", AYR, "/Tier3/Model_Runs"))
Models14_2 <- SSgetoutput(dirvec = c("M14_2_update"))
setwd("C:/Users/cindy.Tribuzio/Work/SAFE/Assessments/AFSC_BSAI_SKATE_Assessment")

M14_2d_SS <- SSsummarize(Models14_2)

Fval <- M14_2d_SS$Fvalue %>% 
  select(Yr, Fvalue = replist1)
Fll <- M14_2d_SS$FvalueLower %>% 
  select(Yr, Flower = replist1)
Ful <- M14_2d_SS$FvalueUpper %>% 
  select(Yr, Fupper = replist1)
Fdat <- Fval %>% 
  left_join(Fll) %>% 
  left_join(Ful) %>% 
  filter(Yr <= AYR)

FABC <- 0.08
FOFL <- 0.093
plot_F <- ggplot(Fdat, aes(x = Yr, y = Fvalue))+
  geom_point()+
  geom_hline(yintercept = FABC, color = "red", linetype = "dashed")+
  geom_hline(yintercept = FOFL, color = "red")+
  geom_errorbar(aes(ymin = Flower, ymax = Fupper))+
  labs(x = "Year", y = "F rate")+
  theme_bw()
ggsave(path = paste0(getwd(), "/", AYR, "/Tier3/Output"),
       "Frate.png",plot=plot_F,dpi=600,width = 6, height = 4)

# phase plane plot with two future years----
setwd(paste0(getwd(), "/", AYR, "/Tier3/Model_Runs"))
Models14_2 <- SS_output(dir = c("M14_2_update"))
setwd("C:/Users/cindy.Tribuzio/Work/SAFE/Assessments/AFSC_BSAI_SKATE_Assessment")

#change endyr to 2025
Models14_2$endyr <- 2025
SS_plots(Models14_2, plot = 8)
