#BETWEEN model retrospectives
# Attempt at mining historic files
# 2014 model 14_2: C:\Users\cindy.tribuzio\Work\SAFE\Assessments\AFSC_BSAI_SKATE_Assessment\Archived_Ormseth\Dropbox\BSAI skates\2014 BSAI skates\oct14_pref_model
# 2016 model 14_2: C:\Users\cindy.tribuzio\Work\SAFE\Assessments\AFSC_BSAI_SKATE_Assessment\Archived_Ormseth\Dropbox\BSAI skates\2016 BSAI skates\model_14_2_Oct_2016
# 2018 model 14_2: C:\Users\cindy.tribuzio\Work\SAFE\Assessments\AFSC_BSAI_SKATE_Assessment\Archived_Ormseth\Dropbox\BSAI skates\AK skate SS\AKSK_model_14_2_Oct2018
# 2020 model 14_2: C:\Users\cindy.tribuzio\Work\SAFE\Assessments\AFSC_BSAI_SKATE_Assessment\Archived_Ormseth\Dropbox\BSAI skates\AK skate SS\AKSK_model_14_2_OCT2020
# 2023 model 14_2d: C:\Users\cindy.tribuzio\Work\SAFE\Assessments\AFSC_BSAI_SKATE_Assessment\2023\Tier3\Model_Runs\M14_2d_fixedcatch
# 2026 author preferred model

# setup ----
libs <- c("r4ss", "here", "tidyverse", 'viridis', 'patchwork')
if(length(libs[which(libs %in% rownames(installed.packages()) == FALSE )]) > 0) {
  install.packages(libs[which(libs %in% rownames(installed.packages()) == FALSE)])}
lapply(libs, library, character.only = TRUE)
'%nin%'<-Negate('%in%') #this is a handy function

# Default with no version downloads the latest release
# r4ss::get_ss3_exe()

AYR <- 2026

# collect data----
# this will collect the SSB (w uncertainty) for previously accepted model runs
# it is cumulative, so the only update should be the most recent model
# should not have to run this section

# set up in 2026
# M14_2_2014_path <- 'C:/Users/cindy.tribuzio/Work/SAFE/Assessments/AFSC_BSAI_SKATE_Assessment/Archived_Ormseth/Dropbox/BSAI skates/2014 BSAI skates/oct14_pref_model'
# M14_2_2014_out <- SS_output(M14_2_2014_path, printstats = FALSE, verbose = FALSE)
# M14_2_2014_ssb_df <- M14_2_2014_out$derived_quants[grep("SSB_", M14_2_2014_out$derived_quants$Label), ] |>
#  select(Label, Value, StdDev) |>
#  mutate(lowerCI = Value - 1.96 * StdDev,
#         upperCI = Value + 1.96 * StdDev,
#         ModelYear = '14_2_2014') |>
#  remove_rownames() |>
#  filter(Label %nin% c('SSB_Virgin', 'SSB_Initial', 'SSB_unfished', 'B_MSY/SSB_unfished', 'SSB_SPR', 'SSB_MSY', 'SSB_Btgt')) |>
#  mutate(year = as.numeric(str_remove(Label, "SSB_"))) |>
#  filter(year <= 2014) |> 
#  select(!Label) |>
#  rename(SSB = Value)
# 
# M14_2_2016_path <- 'C:/Users/cindy.tribuzio/Work/SAFE/Assessments/AFSC_BSAI_SKATE_Assessment/Archived_Ormseth/Dropbox/BSAI skates/2016 BSAI skates/model_14_2_Oct_2016'
# M14_2_2016_out <- SS_output(M14_2_2016_path, printstats = FALSE, verbose = FALSE)
# M14_2_2016_ssb_df <- M14_2_2016_out$derived_quants[grep("SSB_", M14_2_2016_out$derived_quants$Label), ] |>
#  select(Label, Value, StdDev) |>
#  mutate(lowerCI = Value - 1.96 * StdDev,
#         upperCI = Value + 1.96 * StdDev,
#         ModelYear = '14_2_2016')|>
#   remove_rownames() |>
#   filter(Label %nin% c('SSB_Virgin', 'SSB_Initial', 'SSB_unfished', 'B_MSY/SSB_unfished', 'SSB_SPR', 'SSB_MSY', 'SSB_Btgt')) |>
#   mutate(year = as.numeric(str_remove(Label, "SSB_"))) |>
#   filter(year <= 2016) |> 
#   select(!Label) |>
#   rename(SSB = Value)
# 
# M14_2_2018_path <- 'C:/Users/cindy.tribuzio/Work/SAFE/Assessments/AFSC_BSAI_SKATE_Assessment/Archived_Ormseth/Dropbox/BSAI skates/AK skate SS/AKSK_model_14_2_Oct2018'
# M14_2_2018_out <- SS_output(M14_2_2018_path, printstats = FALSE, verbose = FALSE)
# M14_2_2018_ssb_df <- M14_2_2018_out$derived_quants[grep("SSB_", M14_2_2018_out$derived_quants$Label), ] |>
#  select(Label, Value, StdDev) |>
#  mutate(lowerCI = Value - 1.96 * StdDev,
#         upperCI = Value + 1.96 * StdDev,
#         ModelYear = '14_2_2018')|>
#   remove_rownames() |>
#   filter(Label %nin% c('SSB_Virgin', 'SSB_Initial', 'SSB_unfished', 'B_MSY/SSB_unfished', 'SSB_SPR', 'SSB_MSY', 'SSB_Btgt')) |>
#   mutate(year = as.numeric(str_remove(Label, "SSB_"))) |>
#   filter(year <= 2018) |> 
#   select(!Label) |>
#   rename(SSB = Value)
# 
# M14_2_2020_path <- 'C:/Users/cindy.tribuzio/Work/SAFE/Assessments/AFSC_BSAI_SKATE_Assessment/Archived_Ormseth/Dropbox/BSAI skates/AK skate SS/AKSK_model_14_2_OCT2020'
# M14_2_2020_out <- SS_output(M14_2_2020_path, printstats = FALSE, verbose = FALSE)
# M14_2_2020_ssb_df <- M14_2_2020_out$derived_quants[grep("SSB_", M14_2_2020_out$derived_quants$Label), ] |>
#  select(Label, Value, StdDev) |>
#  mutate(lowerCI = Value - 1.96 * StdDev,
#         upperCI = Value + 1.96 * StdDev,
#         ModelYear = '14_2_2020')|>
#   remove_rownames() |>
#   filter(Label %nin% c('SSB_Virgin', 'SSB_Initial', 'SSB_unfished', 'B_MSY/SSB_unfished', 'SSB_SPR', 'SSB_MSY', 'SSB_Btgt')) |>
#   mutate(year = as.numeric(str_remove(Label, "SSB_"))) |>
#   filter(year <= 2020) |> 
#   select(!Label) |>
#   rename(SSB = Value)
# 
# M14_2d_2023_path <- 'C:/Users/cindy.tribuzio/Work/SAFE/Assessments/AFSC_BSAI_SKATE_Assessment/2023/Tier3/Model_Runs/M14_2d_fixedcatch'
# M14_2d_2023_out <- SS_output(M14_2d_2023_path, printstats = FALSE, verbose = FALSE)
# M14_2d_2023_ssb_df <- M14_2d_2023_out$derived_quants[grep("SSB_", M14_2d_2023_out$derived_quants$Label), ] |>
#  select(Label, Value, StdDev) |>
#  mutate(lowerCI = Value - 1.96 * StdDev,
#         upperCI = Value + 1.96 * StdDev,
#         ModelYear = '14_2d_2023')|>
#   remove_rownames() |>
#   filter(Label %nin% c('SSB_Virgin', 'SSB_Initial', 'SSB_unfished', 'B_MSY/SSB_unfished', 'SSB_SPR', 'SSB_MSY', 'SSB_Btgt')) |>
#   mutate(year = as.numeric(str_remove(Label, "SSB_"))) |>
#   filter(year <= 2023) |> 
#   select(!Label) |>
#   rename(SSB = Value)
# 
# combined_biomass <- M14_2_2014_ssb_df |>
#  bind_rows(M14_2_2016_ssb_df,
#            M14_2_2018_ssb_df,
#            M14_2_2020_ssb_df,
#            M14_2d_2023_ssb_df)

#write_csv(combined_biomass, here::here(AYR, 'Nov_models', 'AK_skate_Tier3', 'mgmt', 'retro_between', paste0(AYR, '_SSB_historic.csv')))

# Add current year----
#read in the last year file
prev_ssb <- read_csv(here::here(AYR, 'Nov_models', 'AK_skate_Tier3', 'mgmt', 'retro_between', paste0(AYR, '_SSB_historic.csv'))) 

# get ssb from current recommended model
# this will need to be updated each year, unless folder heirarchy changes
recM <- '25_3'
recM_path <- here::here(AYR, 'Nov_models', 'AK_skate_Tier3', 'mgmt', paste0('M', recM))
recM_out <- SS_output(recM_path, printstats = FALSE, verbose = FALSE)
recM_ssb_df <- recM_out$derived_quants[grep("SSB_", recM_out$derived_quants$Label), ] |> 
  select(Label, Value, StdDev) |> 
  mutate(lowerCI = Value - 1.96 * StdDev,
         upperCI = Value + 1.96 * StdDev,
         ModelYear = paste0(recM, '_', AYR)) |> 
  remove_rownames() |> 
  filter(Label %nin% c('SSB_Virgin', 'SSB_Initial', 'SSB_unfished', 'B_MSY/SSB_unfished', 'SSB_SPR', 'SSB_MSY', 'SSB_Btgt')) |> 
  mutate(year = as.numeric(str_remove(Label, "SSB_"))) |> 
  filter(year <= AYR) |> 
  select(!Label) |> 
  rename(SSB = Value)

combined_biomass <- prev_ssb |> 
  bind_rows(recM_ssb_df)
write_csv(combined_biomass, here::here(AYR, 'Nov_models', 'AK_skate_Tier3', 'mgmt', 'retro_between', paste0(AYR, '_SSB_historic.csv')))

# Plot ----
plot_bweenssb <- ggplot(combined_biomass, aes(x = year, y = SSB/1000, color = ModelYear))+
  geom_ribbon(aes(x = year, ymin = lowerCI/1000, ymax = upperCI/1000, fill = ModelYear), 
              show.legend = F, alpha = 0.3, linetype = 0)+
  geom_line()+
  labs(y = "Spawning Biomass (1,000s t)", x = "Year")+
  #scale_y_continuous(labels = scales::comma_format())+
  scale_color_viridis(discrete = T) +
  #coord_cartesian(ylim = c(75, 350), xlim = c(1950, 2025))+
  theme_bw()+
  #theme(legend.position = "none")+
  guides(fill = guide_legend(byrow = TRUE))

ggsave(path = here::here(AYR, 'Nov_models', 'AK_skate_Tier3', 'mgmt', 'retro_between'),
       paste0(AYR, "between_retro_plot.png") ,plot=plot_bweenssb, dpi=600, width = 6, height = 4)

