# Tier 5 Models

# set up ----
# folder set up
dat_path <- paste0("DATA/", AYR); dir.create(dat_path)
out_path <- paste0("OUTPUT/", AYR, "/Tier5"); dir.create(out_path)

ggplot2::theme_set(cowplot::theme_cowplot(font_size = 13) +
                     cowplot::background_grid() +
                     cowplot::panel_border())

pYEAR <- 2024

# Get Data ----
biomass_dat <- read_csv(paste0(dat_path, '/RACE_biomass_skates', SYR, ".csv"))

#AI data is special case because of introduction of leopard skate in 2010. Suggest to shorten time series 
# for 2025 assessment to skip this confusion
# summed using data from 2000 onward and avg prop adjustment for AK/leopard
akl_dat <- biomass_dat %>% 
  filter(year >= 2010 & year <= 2016,
         survey == "AI",
         species_code %in% c(471, 477)) %>% 
  select(year, AI_Group, biomass)

# Calculate proportions
tot_b <- sum(akl_dat$biomass)

avgL_prop <- akl_dat %>% 
  group_by(year) %>% 
  summarise(tot_bio = sum(biomass)) %>% 
  right_join(akl_dat) %>% 
  mutate(spec_prop = biomass/tot_bio) %>% 
  group_by(AI_Group) %>% 
  summarise(mean_prop = mean(spec_prop)) %>% 
  filter(AI_Group == "Alaska") %>% 
  select(mean_prop)

totL_prop <- akl_dat %>% 
  group_by(AI_Group) %>% 
  summarise(spec_bio = sum(biomass)) %>% 
  mutate(spec_prop = spec_bio/tot_b) %>% 
  filter(AI_Group == "Alaska") %>% 
  select(spec_prop)

# Create new dataframe for avg prop
akl_bio_avg <- biomass_dat %>% 
  filter(year >= 2000 & year <= 2009,
         survey == "AI",
         species_code %in% c(471)) %>% 
  select(strat = AI_Group, year, biomass, biomass_var) %>% 
  mutate(new_biom = biomass * as.numeric(avgL_prop),
         new_var = biomass_var * as.numeric(avgL_prop),
         diff_biom = biomass - new_biom,
         diff_var = biomass_var - new_var)
ak_avg <- biomass_dat %>% 
  filter(year >= 2000 & year <= 2009,
         survey == "AI",
         species_code %in% c(471)) %>% 
  select(strata = AI_Group, year) %>% 
  right_join(akl_bio_avg) %>% 
  select(strata, year, new_biom, new_var) %>% 
  rename(biomass = new_biom,
         var = new_var)
leo_avg <- biomass_dat %>% 
  filter(year >= 2000 & year <= 2009,
         survey == "AI",
         species_code %in% c(477)) %>% 
  select(strata = AI_Group, year) %>% 
  right_join(akl_bio_avg) %>% 
  select(strata, year, diff_biom, diff_var) %>% 
  rename(biomass = diff_biom,
         var = diff_var)

akl_avgbio_var <- leo_avg %>%
  bind_rows(ak_avg) %>% 
  select(strata, year, biomass, biomass_var = var)

AI_dat_2010pres <- biomass_dat %>% 
  filter(survey == "AI",
         year >= 2010)

AI_dat<- biomass_dat %>% 
  filter(survey == "AI",
         year >= 2000 & year <= 2009,
         species_code %nin% c(471, 477)) %>% 
  bind_rows(AI_dat_2010pres) %>% 
  select(strata = AI_Group, year, biomass, biomass_var) %>% 
  bind_rows(akl_avgbio_var) %>% 
  group_by(year) %>% 
  summarise(biomass = sum(biomass),
            var = sum(biomass_var)) %>% 
  ungroup() %>% 
  mutate(cv = sqrt(var)/biomass,
         strata = "AIskates") %>% 
  select(strata, year, biomass, cv)

#EBS shelf Survey, excludes Alaska skate
EBS_dat <- biomass_dat %>% 
  filter(RACE_name != "Alaska skate",
         year >= 1999,
         survey == "EBS_SHELF") %>% 
  group_by(year) %>% 
  summarise(biomass = sum(biomass),
            var = sum(biomass_var)) %>% 
  ungroup() %>% 
  mutate(cv = sqrt(var)/biomass,
         strata = 'EBSskates') %>% 
  select(strata, year, biomass, cv)

#EBS slope survey, nothing special here
Slope_dat <- biomass_dat %>% 
  filter(year >= 1999,
         survey == "EBS_SLOPE") %>% 
  group_by(year) %>% 
  summarise(biomass = sum(biomass),
            var = sum(biomass_var)) %>% 
  ungroup() %>% 
  mutate(cv = sqrt(var)/biomass,
         strata = "Slopeskates") %>% 
  select(strata, year, biomass, cv)

# prep data for rema  ----
EBS_input <- prepare_rema_input(model_name = 'M20_EBSshelf',
                            biomass_dat = EBS_dat,
                            end_year = pYEAR,
                            zeros = list(assumption = "NA"),
                            PE_options = list(pointer_PE_biomass = c(1)))
AI_input <- prepare_rema_input(model_name = 'M20_AI',
                                biomass_dat = AI_dat,
                                end_year = pYEAR,
                                zeros = list(assumption = "NA"),
                                PE_options = list(pointer_PE_biomass = c(1)))
Slope_input <- prepare_rema_input(model_name = 'M20_EBSslope',
                                biomass_dat = Slope_dat,
                                end_year = pYEAR,
                                zeros = list(assumption = "NA"),
                                PE_options = list(pointer_PE_biomass = c(1)))

# fit rema models ----
m20_EBS <- fit_rema(EBS_input)
m20_EBS_out <- tidy_rema(m20_EBS)

m20_AI <- fit_rema(AI_input)
m20_AI_out <- tidy_rema(m20_AI)

m20_Slope <- fit_rema(Slope_input)
m20_Slope_out <- tidy_rema(m20_Slope)

# plot and clean up output ----
m20_EBS_plots <- plot_rema(tidy_rema = m20_EBS_out)
m20_AI_plots <- plot_rema(tidy_rema = m20_AI_out)
m20_Slope_plots <- plot_rema(tidy_rema = m20_Slope_out)

m20_EBS_tot <- tidy_rema(m20_EBS)$biomass_by_strata %>% 
  select(strata, model_name, strata, year, variable, pred, pred_lci, pred_uci, obs, obs_lci, obs_uci) 

m20_AI_tot <- tidy_rema(m20_AI)$biomass_by_strata %>% 
  select(strata, model_name, strata, year, variable, pred, pred_lci, pred_uci, obs, obs_lci, obs_uci) 

m20_Slope_tot <- tidy_rema(m20_Slope)$biomass_by_strata %>% 
  select(strata, model_name, strata, year, variable, pred, pred_lci, pred_uci, obs, obs_lci, obs_uci) 

T5_m20_output <- m20_EBS_tot %>% 
  bind_rows(m20_AI_tot, m20_Slope_tot)

write_csv(T5_m20_output, paste0(out_path, "/Tier5_m20_output.csv"))

# make nice summary graph ----
ggplot(data = T5_m20_output,
       aes(x = year, y = pred,
           col = model_name)) +
  geom_ribbon(aes(ymin = pred_lci, ymax = pred_uci,
                  fill = model_name), col = NA,
              alpha = 0.25, show.legend = F) +
  geom_line(show.legend = F) +
  facet_wrap(~model_name, ncol = 1, scales = "free") +
  geom_point(aes(x = year, y = obs), col = "black") +
  geom_errorbar(aes(x = year, ymin = obs_lci, ymax = obs_uci), col = "black") +
  scale_y_continuous(labels = scales::comma, expand = c(0.01, 0), limits = c(0, NA)) +
  labs(x = "", y = 'Biomass (t)',
       fill = NULL, colour = NULL, shape = NULL) +
  ggplot2::scale_fill_viridis_d(direction = 1) +
  ggplot2::scale_colour_viridis_d(direction = 1)

# Tier 5 harvest recommendations ----
SurvBiom <- T5_m20_output %>% 
  group_by(year) %>% 
  summarise(rbiom = sum(pred),
            OFL = rbiom * 0.1,
            ABC = OFL * 0.75) %>% 
  filter(year == 2023)

