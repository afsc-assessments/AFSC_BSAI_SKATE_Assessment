# Tier 5 alternative Models

# set up ----
# run this in case of updates
# install.packages("devtools")
#devtools::install_github("afsc-assessments/rema", dependencies = TRUE, build_vignettes = TRUE)

libs <- c("tidyverse", "janitor", "Hmisc", "RColorBrewer", "gridExtra", "gtable", 
          "grid", "flextable", "officer", "lubridate", "DBI", "gtable", 
          "patchwork", 'cowplot', 'rema', 'viridis')
if(length(libs[which(libs %in% rownames(installed.packages()) == FALSE )]) > 0) {
  install.packages(libs[which(libs %in% rownames(installed.packages()) == FALSE)])}
lapply(libs, library, character.only = TRUE)
'%nin%'<-Negate('%in%') #this is a handy function

# folder set up
AYR <- 2026 #assessment year
pYEAR <- 2026 #projection year, or how many years forward should this go
SYR <- 2026 #recent survey year

dat_path <- here::here(AYR, 'Nov_models', 'Oskate_Tier5', 'data'); dir.create(dat_path)
out_path <- here::here(AYR, 'Nov_models', 'Oskate_Tier5', 'rsch','M23_0a_shortened'); dir.create(out_path)

ggplot2::theme_set(cowplot::theme_cowplot(font_size = 13) +
                     cowplot::background_grid() +
                     cowplot::panel_border())



# Get Data ----
sk_group <- read_csv(here::here(dat_path, 'BSAIskate_species_codes.csv')) %>% 
  rename(species_code = RACE_code) %>% 
  select(species_code, Shelf_Group, Slope_Group, AI_Group) %>% 
  clean_names()

eggcodes <- c(474, 473, 402, 436, 421, 484, 476, 411, 478, 441, 456, 446, 461, 401, 481, 486)

biomass_dat <- read_csv(here::here(dat_path, paste0('Oskate_GAPbiomass_', AYR, '.csv'))) %>% 
  left_join(sk_group) %>% 
  filter(species_code %nin% eggcodes)

#Prep AI data----
#AI data is special case because of introduction of leopard skate in 2010. Model 23_0a only uses data from 2010 onward in AI
AI_dat<- biomass_dat %>% 
  filter(survey == 52,
         year >= 2010) %>% 
  select(strata = ai_group, year, biomass, biomass_var) %>% 
  group_by(year) %>% 
  summarise(biomass = sum(biomass),
            var = sum(biomass_var)) %>% 
  ungroup() %>% 
  mutate(cv = sqrt(var)/biomass,
         strata = "AIskates") %>% 
  select(strata, year, biomass, cv)

#Prep EBS shelf----
# excludes Alaska skate
EBS_dat <- biomass_dat %>% 
  filter(common_name != "Alaska skate",
         year >= 1999,
         survey == 98) %>% 
  group_by(year) %>% 
  summarise(biomass = sum(biomass),
            var = sum(biomass_var)) %>% 
  ungroup() %>% 
  mutate(cv = sqrt(var)/biomass,
         strata = 'EBSskates') %>% 
  select(strata, year, biomass, cv)

#Prep EBS slope----
#nothing special here
Slope_dat <- biomass_dat %>% 
  filter(year >= 1999,
         survey == 78) %>% 
  group_by(year) %>% 
  summarise(biomass = sum(biomass),
            var = sum(biomass_var)) %>% 
  ungroup() %>% 
  mutate(cv = sqrt(var)/biomass,
         strata = "Slopeskates") %>% 
  select(strata, year, biomass, cv)

# prep data for rema  ----
EBS_input <- prepare_rema_input(model_name = 'M23_EBSshelf',
                            biomass_dat = EBS_dat,
                            end_year = pYEAR,
                            zeros = list(assumption = "NA"),
                            PE_options = list(pointer_PE_biomass = c(1)))
AI_input <- prepare_rema_input(model_name = 'M23_AI',
                                biomass_dat = AI_dat,
                                end_year = pYEAR,
                                zeros = list(assumption = "NA"),
                                PE_options = list(pointer_PE_biomass = c(1)))
Slope_input <- prepare_rema_input(model_name = 'M23_EBSslope',
                                biomass_dat = Slope_dat,
                                end_year = pYEAR,
                                zeros = list(assumption = "NA"),
                                PE_options = list(pointer_PE_biomass = c(1)))

# fit rema models ----
m23_EBS <- fit_rema(EBS_input)
m23_EBS_out <- tidy_rema(m23_EBS)

m23_AI <- fit_rema(AI_input)
m23_AI_out <- tidy_rema(m23_AI)

m23_Slope <- fit_rema(Slope_input)
m23_Slope_out <- tidy_rema(m23_Slope)

# plot and clean up output ----
m23_EBS_plots <- plot_rema(tidy_rema = m23_EBS_out)
m23_AI_plots <- plot_rema(tidy_rema = m23_AI_out)
m23_Slope_plots <- plot_rema(tidy_rema = m23_Slope_out)

m23_EBS_tot <- tidy_rema(m23_EBS)$biomass_by_strata %>% 
  select(strata, model_name, strata, year, variable, pred, pred_lci, pred_uci, obs, obs_cv, obs_lci, obs_uci) 

m23_AI_tot <- tidy_rema(m23_AI)$biomass_by_strata %>% 
  select(strata, model_name, strata, year, variable, pred, pred_lci, pred_uci, obs, obs_cv, obs_lci, obs_uci) 

m23_Slope_tot <- tidy_rema(m23_Slope)$biomass_by_strata %>% 
  select(strata, model_name, strata, year, variable, pred, pred_lci, pred_uci, obs, obs_cv, obs_lci, obs_uci) 

T5_m23_output <- m23_EBS_tot %>% 
  bind_rows(m23_AI_tot, m23_Slope_tot)

write_csv(T5_m23_output, paste0(out_path, "/Tier5_m23_output.csv"))

# make nice summary graph ----
T5_m23_output <- read_csv(paste0(out_path, "/Tier5_m23_output.csv"))

plot_M23 <- ggplot(data = T5_m23_output,
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

ggsave(path = out_path,
       "M23_biomass.png",plot=plot_M23,dpi=600,width = 6, height = 8)

# Tier 5 harvest recommendations ----
SurvBiom <- T5_m23_output %>% 
  group_by(year) %>% 
  summarise(rbiom = sum(pred),
            OFL = rbiom * 0.1,
            ABC = OFL * 0.75) %>% 
  filter(year == AYR) %>% 
  mutate(FOFL = 0.1,
         FABC = 0.75 * FOFL,
         model_name = "Tier 5 Other Skates")
write_csv(SurvBiom, here::here(out_path, 'Tier5_Otherskates.csv'))

# species specific rema for appendices ----
AIspec_dat<- biomass_dat %>% 
  filter(survey == 52,
         year >= 2010) %>% 
  select(strata = ai_group, year, biomass, biomass_var) %>% 
  filter(!is.na(strata)) %>% 
  group_by(year, strata) %>% 
  summarise(biomass = sum(biomass),
            var = sum(biomass_var)) %>% 
  ungroup() %>% 
  mutate(cv = sqrt(var)/biomass) %>% 
  select(strata, year, biomass, cv) 

EBSspec_dat <- biomass_dat %>% 
  filter(common_name != "Alaska skate",
         year >= 1999,
         survey == 98) %>% 
  group_by(year, shelf_group) %>% 
  summarise(biomass = sum(biomass),
            var = sum(biomass_var)) %>% 
  ungroup() %>% 
  mutate(cv = sqrt(var)/biomass) %>% 
  select(strata = shelf_group, year, biomass, cv) %>% 
  filter(!is.na(strata))

Slopespec_dat <- biomass_dat %>% 
  filter(year >= 1999,
         survey == 78) %>%
  group_by(year, slope_group) %>% 
  summarise(biomass = sum(biomass),
            var = sum(biomass_var)) %>% 
  ungroup() %>% 
  mutate(cv = sqrt(var)/biomass) %>% 
  select(strata = slope_group, year, biomass, cv) %>% 
  filter(!is.na(strata))

# prep data for rema  ----
EBSspec_input <- prepare_rema_input(model_name = 'M23_EBSshelf_spec',
                                biomass_dat = EBSspec_dat,
                                end_year = pYEAR,
                                zeros = list(assumption = "NA"),
                                PE_options = list(pointer_PE_biomass = c(1, 1, 1, 1)))
AIspec_input <- prepare_rema_input(model_name = 'M23_AIspec',
                               biomass_dat = AIspec_dat,
                               end_year = pYEAR,
                               zeros = list(assumption = "NA"),
                               PE_options = list(pointer_PE_biomass = c(1, 1, 1, 1, 1, 1, 1)))
Slopespec_input <- prepare_rema_input(model_name = 'M23_EBSslope_spec',
                                  biomass_dat = Slopespec_dat,
                                  end_year = pYEAR,
                                  zeros = list(assumption = "NA"),
                                  PE_options = list(pointer_PE_biomass = c(1, 1, 1, 1, 1, 1, 1, 1, 1)))

# fit rema models ----
m23_EBSspec <- fit_rema(EBSspec_input)
m23_EBSspec_out <- tidy_rema(m23_EBSspec)

m23_AIspec <- fit_rema(AIspec_input)
m23_AIspec_out <- tidy_rema(m23_AIspec)

m23_Slopespec <- fit_rema(Slopespec_input)
m23_Slopespec_out <- tidy_rema(m23_Slopespec)

# plot and clean up output ----
m23_EBSspec_plots <- plot_rema(tidy_rema = m23_EBSspec_out)
m23_AIspec_plots <- plot_rema(tidy_rema = m23_AIspec_out)
m23_Slopespec_plots <- plot_rema(tidy_rema = m23_Slopespec_out)

m23_EBSspec_tot <- tidy_rema(m23_EBSspec)$biomass_by_strata %>% 
  select(strata, model_name, strata, year, variable, pred, pred_lci, pred_uci, obs, obs_cv, obs_lci, obs_uci) 

m23_AIspec_tot <- tidy_rema(m23_AIspec)$biomass_by_strata %>% 
  select(strata, model_name, strata, year, variable, pred, pred_lci, pred_uci, obs, obs_cv, obs_lci, obs_uci) 

m23_Slopespec_tot <- tidy_rema(m23_Slopespec)$biomass_by_strata %>% 
  select(strata, model_name, strata, year, variable, pred, pred_lci, pred_uci, obs, obs_cv, obs_lci, obs_uci) 

Species_m23_output <- m23_EBSspec_tot %>% 
  bind_rows(m23_AIspec_tot, m23_Slopespec_tot)

write_csv(Species_m23_output, paste0(out_path, "/Appendix_SpeciesSpecific_rema_output.csv"))

# makes species specific summary plots----
# make nice summary graph ----
Species_m23_output <- read_csv(paste0(out_path, "/Appendix_SpeciesSpecific_rema_output.csv")) %>% 
  mutate(Survey = if_else(model_name == 'M23_AIspec', "AI",
                          if_else(model_name == 'M23_EBSshelf_spec', "Shelf", "Slope")))

AIspec_plot <- ggplot(data = Species_m23_output[Species_m23_output$Survey == 'AI',],
       aes(x = year, y = pred,
           col = model_name)) +
  geom_ribbon(aes(ymin = pred_lci, ymax = pred_uci,
                  fill = model_name), col = NA,
              alpha = 0.25, show.legend = F) +
  geom_line(show.legend = F) +
  facet_grid(strata~Survey, scales = "free") +
  geom_point(aes(x = year, y = obs), col = "black") +
  geom_errorbar(aes(x = year, ymin = obs_lci, ymax = obs_uci), col = "black") +
  scale_y_continuous(labels = scales::comma, expand = c(0.01, 0), limits = c(0, NA)) +
  labs(x = "", y = 'Biomass (t)',
       fill = NULL, colour = NULL, shape = NULL) +
  ggplot2::scale_fill_viridis_d(direction = 1) +
  ggplot2::scale_colour_viridis_d(direction = 1)

ggsave(path = out_path, "Appendix_AIrema.png",plot=AIspec_plot,dpi=600,width = 6, height = 8)

bcolors <- viridis(n=3)
Shelfspec_plot <- ggplot(data = Species_m23_output[Species_m23_output$Survey == 'Shelf',],
                      aes(x = year, y = pred,
                          col = model_name)) +
  geom_ribbon(aes(ymin = pred_lci, ymax = pred_uci,
                  fill = model_name), col = NA,
              fill = bcolors[2], alpha = 0.25, show.legend = F) +
  geom_line(show.legend = F, col = bcolors[2]) +
  facet_grid(strata~Survey, scales = "free") +
  geom_point(aes(x = year, y = obs), col = "black") +
  geom_errorbar(aes(x = year, ymin = obs_lci, ymax = obs_uci), col = "black") +
  scale_y_continuous(labels = scales::comma, expand = c(0.01, 0), limits = c(0, NA)) +
  labs(x = "", y = 'Biomass (t)',
       fill = NULL, colour = NULL, shape = NULL)

ggsave(path = out_path, "Appendix_EBSshelfrema.png",plot=Shelfspec_plot,dpi=600,width = 6, height = 8)

Slopespec_plot <- ggplot(data = Species_m23_output[Species_m23_output$Survey == 'Slope',],
                         aes(x = year, y = pred,
                             col = model_name)) +
  geom_ribbon(aes(ymin = pred_lci, ymax = pred_uci,
                  fill = model_name), col = NA,
              fill = bcolors[3], alpha = 0.25, show.legend = F) +
  geom_line(show.legend = F, col = bcolors[3]) +
  facet_grid(strata~Survey, scales = "free") +
  geom_point(aes(x = year, y = obs), col = "black") +
  geom_errorbar(aes(x = year, ymin = obs_lci, ymax = obs_uci), col = "black") +
  scale_y_continuous(labels = scales::comma, expand = c(0.01, 0), limits = c(0, NA)) +
  labs(x = "", y = 'Biomass (t)',
       fill = NULL, colour = NULL, shape = NULL)

ggsave(path = out_path, "Appendix_EBSsloperema.png",plot=Slopespec_plot,dpi=600,width = 6, height = 8)

