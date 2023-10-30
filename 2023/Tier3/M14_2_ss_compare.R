datapath <- paste0(getwd(), "/", AYR, "/Tier 3/Model_Runs")

# Current model name
Model_name_old1 <- "M14_2_vold"
Model_name_old2 <- "M14_2_vbridge"
Model_name_fixdat <- "M14_2_fix_old"
Model_name_adddat <- "M14_2_addnew"
Model_name_new <- "M14_2_update"

# compare bridging models----
# read model outputs
setwd(datapath)
bridge_out <- SSgetoutput(dirvec = c("M14_2_vold","M14_2_vbridge", "M14_2_fix_old", 'M14_2_addnew', 'M14_2_update'))
setwd("C:/Users/cindy.Tribuzio/Work/SAFE/Assessments/AFSC_BSAI_SKATE_Assessment")

model_comp <- r4ss::SSsummarize(bridge_out)

r4ss::SSplotComparisons(model_comp,
                        print = TRUE,
                        plotdir = here::here(datapath) )


#final model output----
M14_2_out <- SSgetoutput( dir = paste0(getwd(), "/Code/Tier3/", AYR, "/Model_Runs/M14_2_update"), verbose = TRUE)

M14_2_SS <- SSsummarize(M14_2_out)

# plots the results
SS_plots(M14_2_out)

# pull out parameters for summary table ----
llhoods <- M14_2_SS$likelihoods
SSB <- M14_2_SS$SpawnBio %>% 
  bind_cols(M14_2_SS$SpawnBioLower$replist1, M14_2_SS$SpawnBioUpper$replist1, M14_2_SS$SpawnBioSD$replist1) %>% 
  select(year = Yr, SSB = replist1, SSBLL = ...4, SSBUL = ...5, SSBSD = ...6) %>% 
  mutate(CV = SSBSD/SSB)

recruit <- M14_2_SS$recruits %>% 
  bind_cols(M14_2_SS$recruitsLower$replist1, M14_2_SS$recruitsUpper$replist1, M14_2_SS$recruitsSD$replist1) %>% 
  select(year = Yr, rec = replist1, recLL = ...4, recUL = ...5, recSD = ...6) %>% 
  mutate(CV = recSD/rec)
write_csv(recruit, paste0(getwd(), "/Output/", AYR, "/Tier3/M14_2recruit_summary.csv"))

pars <- M14_2_SS$pars %>% 
  bind_cols(M14_2_SS$parsSD$replist1) %>% 
  select(Parameter = Label, value = replist1, parSD = ...5)





# input N and effN pulled directly from Report.SSO file, could use
#M14_2_out$Length_comp_fit_summary

# RMSE
# from Olav's files for retros, can't find direct evidence of what he did for model fit
#> x<-matrix(ncol=12,nrow= S_N)
#> for(i in 1:12){
#  +     x[1:( S_N -i),i]<-(log(SAB[1:( S_N -i),(14-i)])-log(SAB[1:( S_N -i),14]))^2
#  +     }
#> 
#  > RMSE=sqrt(sum(x[!is.na(x)])/length(x[!is.na(x)]))
#> RMSE
#[1] 0.1799513

#test Olav's way with old model data
M14_2_OO <- SSgetoutput( dir = paste0(getwd(), "/Code/Tier3/", AYR, "/Model_Runs/M14_2_vold"), verbose = TRUE)

M14_2_OOSS <- SSsummarize(M14_2_OO)

bio_all_OO <- M14_2_OO$replist1$timeseries %>% 
  select(Yr, Bio_all)
surv_est_OO <- M14_2_OOSS$indices %>% 
  select(Yr, Vuln_bio) %>% 
  filter(Yr >= 1999)
RMSE_OO <- surv_est_OO %>% 
  left_join(bio_all_OO) %>% 
  mutate(sqresid = (log(Vuln_bio)-log(Bio_all))^2) %>% 
  select(sqresid)
RMSE_OO <- sqrt(mean(RMSE_OO$sqresid))
#still doesn't match exactly what was in last assessment, but closer
corcoef_OO <- surv_est_OO %>% 
  left_join(bio_all_OO) 
corcoef_OO <- cor(corcoef_OO$Vuln_bio, corcoef_OO$Bio_all)


# attempt to recreate Olav's methods with updated model data
bio_all <- M14_2_out$replist1$timeseries %>% 
  select(Yr, Bio_all)
surv_est <- M14_2_SS$indices %>% 
  select(Yr, Vuln_bio)
RMSE <- surv_est %>% 
  left_join(bio_all) %>% 
  mutate(sqresid = (log(Vuln_bio)-log(Bio_all))^2) %>% 
  select(sqresid)
RMSE <- sqrt(mean(RMSE$sqresid))

bio_out <- SSB %>% 
  select(Yr = year, SSB, CV) %>% 
  left_join(bio_all)
write_csv(bio_out, paste0(getwd(), "/Output/", AYR, "/Tier3/M14_2biomass_summary.csv"))

# Correlation Coeff
corcoef <- surv_est %>%
  left_join(bio_all) 
corcoef <- cor(corcoef$Vuln_bio, corcoef$Bio_all)

# some other summary information ----
F_exploit <- M14_2_out$replist1$exploitation %>% 
  mutate(Longline = round(LGL, 3),
         Trawl = round(TWL, 3),
         Total_F = round(annual_F, 3)) %>% 
  select(Yr, Longline, Trawl, Total_F)
write_csv(F_exploit, paste0(getwd(), "/Output/", AYR, "/Tier3/M14_2exploitation_summary.csv"))



