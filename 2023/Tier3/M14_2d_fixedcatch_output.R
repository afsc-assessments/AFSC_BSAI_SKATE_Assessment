datapath <- paste0(getwd(), "/", AYR, "/Tier3/Model_Runs")

# Current model name
#Model_name_old1 <- "M14_2_vold"
#Model_name_old2 <- "M14_2_vbridge"
#Model_name_fixdat <- "M14_2_fix_old"
#Model_name_adddat <- "M14_2_addnew"
#Model_name_new <- "M14_2_update"

# compare bridging models----
# read model outputs
setwd(datapath)
bridge_out <- SSgetoutput(dirvec = c("M14_2_update","M14_2d_fixedcatch"))
setwd("C:/Users/cindy.Tribuzio/Work/SAFE/Assessments/AFSC_BSAI_SKATE_Assessment")

model_comp <- r4ss::SSsummarize(bridge_out)

r4ss::SSplotComparisons(model_comp,
                        print = TRUE,
                        plotdir = here::here(datapath) )


#final model output----
M14_2_out <- SSgetoutput(dir = paste0(getwd(), "/", AYR, "/Tier3/Model_Runs/M14_2d_fixedcatch"), verbose = TRUE)

M14_2_SS <- SSsummarize(M14_2_out)

# plots the results
SS_plots(M14_2_out)

# pull out parameters for summary table ----
llhoods <- M14_2_SS$likelihoods
SSB <- M14_2_SS$SpawnBio %>% 
  bind_cols(M14_2_SS$SpawnBioLower$replist1, M14_2_SS$SpawnBioUpper$replist1, M14_2_SS$SpawnBioSD$replist1) %>% 
  select(year = Yr, SSB = replist1, SSBLL = ...4, SSBUL = ...5, SSBSD = ...6) %>% 
  mutate(CV = SSBSD/SSB)
write_csv(SSB, paste0(getwd(), "/", AYR, "/Tier3/Output/M14_2dSSB_summary.csv"))

TotBiom <- M14_2_out$replist1$timeseries %>% 
  select(year = Yr, Bio_all)
write_csv(TotBiom, paste0(getwd(), "/", AYR, "/Tier3/Output/M14_2dBioall_summary.csv"))

recruit <- M14_2_SS$recruits %>% 
  bind_cols(M14_2_SS$recruitsLower$replist1, M14_2_SS$recruitsUpper$replist1, M14_2_SS$recruitsSD$replist1) %>% 
  select(year = Yr, rec = replist1, recLL = ...4, recUL = ...5, recSD = ...6) %>% 
  mutate(CV = recSD/rec)
write_csv(recruit, paste0(getwd(), "/", AYR, "/Tier3/Output/M14_2drecruit_summary.csv"))

pars <- M14_2_SS$pars %>% 
  bind_cols(M14_2_SS$parsSD$replist1) %>% 
  select(Parameter = Label, value = replist1, parSD = ...5)

# input N and effN pulled directly from Report.SSO file, could use
#M14_2_out$Length_comp_fit_summary

# some other summary information ----
F_exploit <- M14_2_out$replist1$exploitation %>% 
  mutate(Longline = round(LGL, 3),
         Trawl = round(TWL, 3),
         Total_F = round(annual_F, 3)) %>% 
  select(Yr, Longline, Trawl, Total_F)
write_csv(F_exploit, paste0(getwd(), "/", AYR, "/Tier3/Output/M14_2dexploitation_summary.csv"))

NAA <- M14_2_out$replist1$natage %>% 
  filter(`Beg/Mid` == "B") %>% 
  select(!c(Era, Area, Bio_Pattern, Sex, BirthSeas, Settlement, Platoon, Morph, Seas, Time, 'Beg/Mid')) %>% 
  pivot_longer(!Yr, names_to = "Age", values_to = "Numbers") %>% 
  mutate(Numbers = round(Numbers, 0)) %>% 
  pivot_wider(names_from = Age, values_from = Numbers)
write_csv(NAA, paste0(getwd(), "/", AYR, "/Tier3/Output/M14_2dNAA_summary.csv"))

Lcomp <- M14_2_out$replist1$len_comp_fit_table

# Survey fit summaries ----
rmse <- M14_2_out[['replist1']]['index_variance_tuning_check'][['index_variance_tuning_check']]$RMSE

surv_corr <- rcorr(M14_2_SS[["indices"]]$Obs, M14_2_SS[["indices"]]$Exp)

mod_dat <- M14_2_out[["replist1"]][["timeseries"]] %>% 
  select(Yr, Bio_all)

#surv_dat <- read_csv(paste0(getwd(), "/Data/", AYR, "/RACE_biomass_skates", AYR, ".csv"))
surv_limits <- M14_2_SS[["indices"]] %>% 
  select(Yr, Obs, CV = SE, Exp) %>% 
  mutate(obs_ul = Obs + 1.96*(CV*Obs),
         obs_ll = Obs - 1.96*(CV*Obs)) %>% 
  mutate(inrange = if_else(Exp <= obs_ul & Exp >= obs_ll, 1, 0))

prop_cov <- surv_limits %>% 
  group_by(inrange) %>% 
  summarise(n = n())

23/40
  
