# Tier 5 Alaska Skate model alternative

# Setup ----
libs <- c("tidyverse", "janitor", "Hmisc", "RColorBrewer", "gridExtra", "gtable", 
          "grid", "flextable", "officer", "lubridate", "DBI", "gtable", 
          "patchwork", 'cowplot', 'rema', 'viridis')
if(length(libs[which(libs %in% rownames(installed.packages()) == FALSE )]) > 0) {
  install.packages(libs[which(libs %in% rownames(installed.packages()) == FALSE)])}
lapply(libs, library, character.only = TRUE)
'%nin%'<-Negate('%in%') #this is a handy function

# set up ----
# folder set up
dat_path <- here::here(AYR, 'Nov_models', 'AK_skate_Tier5', 'data')
out_path <- here::here(AYR, 'Nov_models', 'AK_skate_Tier5', 'mgmt')

ggplot2::theme_set(cowplot::theme_cowplot(font_size = 13) +
                     cowplot::background_grid() +
                     cowplot::panel_border())

pYEAR <- 2025
SYR <- 2025

# Get Data ----
biomass_dat <- read_csv(here::here(dat_path, paste0('AK_skate_GAPbiomass_', SYR, '.csv')))

# REMA for EBS Shelf----
#EBS shelf Survey for Alaska skate
EBS_dat <- biomass_dat %>% 
  filter(common_name == "Alaska skate",
         year >= 1999,
         survey == 98) %>% 
  mutate(strata = 'EBS') 

# prep data for rema  ----
EBS_input <- prepare_rema_input(model_name = 'AKskate_EBS',
                            biomass_dat = EBS_dat,
                            end_year = pYEAR,
                            zeros = list(assumption = "NA"),
                            PE_options = list(pointer_PE_biomass = c(1)))

# fit rema models ----
m_AKskt <- fit_rema(EBS_input)
m_AKskt_out <- tidy_rema(m_AKskt)

# plot and clean up output ----
#note: need to save plots individually, not part of output code
m_AKskt_plots <- plot_rema(tidy_rema = m_AKskt_out)

m_AKskt_tot <- tidy_rema(m_AKskt)$biomass_by_strata %>% 
  select(strata, model_name, strata, year, variable, pred, pred_lci, pred_uci, obs, obs_cv, obs_lci, obs_uci) 

write_csv(m_AKskt_tot, here::here(AYR, 'Nov_models', 'AK_skate_Tier5', 'mgmt', 'Tier5_AKskt_EBSoutput.csv'))

# Tier 5 harvest recommendations ----
T5recs_EBS <- m_AKskt_tot %>% 
  group_by(year) %>% 
  summarise(rbiom = round(pred, 0),
            OFL = round(rbiom * 0.13, 0),
            ABC = round(OFL * 0.75, 0)) %>% 
  filter(year == AYR) %>% 
  mutate(FOFL = 0.13,
         FABC = 0.75 * FOFL,
         model_name = "Tier 5 EBS")

# REMA for NBS----
#NBS shelf Survey for Alaska skate
NBS_dat <- biomass_dat %>% 
  filter(common_name == "Alaska skate",
         year >= 1999,
         survey == 143) %>% 
  mutate(strata = 'NBS') 

# prep data for rema  ----
NBS_input <- prepare_rema_input(model_name = 'AKskate_NBS',
                                biomass_dat = NBS_dat,
                                end_year = pYEAR,
                                zeros = list(assumption = "NA"),
                                PE_options = list(pointer_PE_biomass = c(1)))

# fit rema models ----
m_AKsktNBS <- fit_rema(NBS_input)
m_AKsktNBS_out <- tidy_rema(m_AKsktNBS)

# plot and clean up output ----
#note: need to save plots individually, not part of output code
m_AKsktNBS_plots <- plot_rema(tidy_rema = m_AKsktNBS_out)

m_AKsktNBS_tot <- tidy_rema(m_AKsktNBS)$biomass_by_strata %>% 
  select(strata, model_name, strata, year, variable, pred, pred_lci, pred_uci, obs, obs_cv, obs_lci, obs_uci) 

write_csv(m_AKsktNBS_tot, here::here(AYR, 'Nov_models', 'AK_skate_Tier5', 'mgmt', 'Tier5_AKskt_NBSoutput.csv'))

# Tier 5 NBS harvest recommendations ----
T5recs_NBS <- m_AKsktNBS_tot %>% 
  group_by(year) %>% 
  summarise(rbiom = round(pred, 0),
            OFL = round(rbiom * 0.13, 0),
            ABC = round(OFL * 0.75, 0)) %>% 
  filter(year == AYR) %>% 
  mutate(FOFL = 0.13,
         FABC = 0.75 * FOFL,
         model_name = "Tier 5 NBS")

# combined REMA----
all_dat <- EBS_dat %>% 
  bind_rows(NBS_dat)

all_input <- prepare_rema_input(model_name = 'AKskate_all',
                                biomass_dat = all_dat,
                                end_year = pYEAR,
                                zeros = list(assumption = "NA"),
                                PE_options = list(pointer_PE_biomass = c(1, 1)))
m_AKsktall <- fit_rema(all_input)
m_AKsktall_out <- tidy_rema(m_AKsktall)
m_AKsktall_plots <- plot_rema(tidy_rema = m_AKsktall_out)
m_AKsktall_tot <- tidy_rema(m_AKsktall)$biomass_by_strata %>% 
  select(strata, model_name, strata, year, variable, pred, pred_lci, pred_uci, obs, obs_cv, obs_lci, obs_uci) 

write_csv(m_AKsktall_tot, here::here(AYR, 'Nov_models', 'AK_skate_Tier5', 'mgmt', 'Tier5_AKskt_alloutput.csv'))

T5recs_all <- m_AKsktall_tot %>% 
  group_by(year) %>% 
  summarise(rbiom = round(sum(pred), 0),
            OFL = round(rbiom * 0.13, 0),
            ABC = round(OFL * 0.75, 0)) %>% 
  filter(year == AYR) %>% 
  mutate(FOFL = 0.13,
         FABC = 0.75 * FOFL,
         model_name = "Tier 5 combined")

# all T5 options----
T5recs_combined <- T5recs_EBS %>% 
  bind_rows(T5recs_NBS) %>% 
  bind_rows(T5recs_all)
write_csv(T5recs_combined, here::here(AYR, 'Nov_models', 'AK_skate_Tier5', 'mgmt', 'Tier5_AKskt_options.csv'))


# compare to Tier 3 models----
T3_out <- read_csv(paste0(dat_path, '/Tier3_AKskt_survfits.csv')) %>% 
  clean_names() %>% 
  filter(yr >= 1999) %>% 
  select(yr, obs, exp, model) %>% 
  rename(year = yr,
         pred = exp,
         model_name = model)

T5_out <- m_AKskt_tot %>% 
  select(year, obs, pred, pred_lci, pred_uci, obs_lci, obs_uci) %>% 
  mutate(model_name = 'rema')

model_comp <- T3_out %>% 
  bind_rows(T5_out)

ggplot(data = model_comp,
       aes(x = year, y = pred,
           col = model_name)) +
  geom_ribbon(aes(ymin = pred_lci, ymax = pred_uci,
                  fill = model_name), col = NA,
              alpha = 0.25) +
  geom_line() +
  geom_point(aes(x = year, y = obs, col = model_name, shape = model_name)) +
  geom_errorbar(aes(x = year, ymin = obs_lci, ymax = obs_uci, col = model_name)) +
  scale_y_continuous(labels = scales::comma, expand = c(0.01, 0), limits = c(0, NA)) +
  labs(x = 'Year', y = 'Biomass',
       fill = NULL, colour = NULL, shape = NULL)
  #ggplot2::scale_fill_viridis_d(direction = 1) +
  #ggplot2::scale_colour_viridis_d(direction = 1)
