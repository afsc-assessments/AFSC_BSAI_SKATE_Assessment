# Bridging Model 14_2d to the proposed model for 2025
# developed by C Tribuzio October 2026

#TODO
# confirm 25_2a
# run 25_3 and 25_3a
# run both with high M

# setup ----
libs <- c("r4ss", "here", "tidyverse")
if(length(libs[which(libs %in% rownames(installed.packages()) == FALSE )]) > 0) {
  install.packages(libs[which(libs %in% rownames(installed.packages()) == FALSE)])}
lapply(libs, library, character.only = TRUE)
'%nin%'<-Negate('%in%') #this is a handy function

# Default with no version downloads the latest release
# r4ss::get_ss3_exe()

AYR <- 2026

exe_loc <- here::here(paste0(AYR, '/Nov_models/AK_skate_Tier3/ss.exe'))

##########
# Model 14_2d1_fixedslx----
# fixed all selectivities to see if that changed outcome
# note: did this because when I fixed the selectivies in 25_2 to be the same as 14_d1, fit was different
M14_2d1_fs_path <- here::here(AYR, 'Nov_models', 'AK_skate_Tier3', 'rsch', 'M14_2d1_fixedslx')

r4ss::run(dir = M14_2d1_fs_path, skipfinished = FALSE, exe = exe_loc)

M14_2d1_fs_out <- SS_output(M14_2d1_fs_path, printstats = FALSE, verbose = FALSE)

# plots the results
SS_plots(M14_2d1_fs_out)

# Model 14_2d1_cvbounds----
# widened the bounds on the CV for young/old ages
M14_2d1_cv_path <- here::here(AYR, 'Nov_models', 'AK_skate_Tier3', 'rsch', 'M14_2d1_cvbounds')

r4ss::run(dir = M14_2d1_cv_path, skipfinished = FALSE, exe = exe_loc)

M14_2d1_cv_out <- SS_output(M14_2d1_cv_path, printstats = FALSE, verbose = FALSE)

# plots the results
SS_plots(M14_2d1_cv_out)

# Model 14_2d1_fixedslxgrowth----
# fixed all selectivities like 14_2d1
# fixing growth and CV on age like 25_2
M14_2d1_fsg_path <- here::here(AYR, 'Nov_models', 'AK_skate_Tier3', 'rsch', 'M14_2d1_fixedslxgrowth')

r4ss::run(dir = M14_2d1_fsg_path, skipfinished = FALSE, exe = exe_loc)

M14_2d1_fsg_out <- SS_output(M14_2d1_fsg_path, printstats = FALSE, verbose = FALSE)

# plots the results
SS_plots(M14_2d1_fsg_out)

# Model 14_2d1_fixedslx_cvgr----
# fixed all selectivities like 14_2d1
# fixed CV on age like 14_2d1
# fixed growth parameters from outside the model
M14_2d1_fscv_path <- here::here(AYR, 'Nov_models', 'AK_skate_Tier3', 'rsch', 'M14_2d1_fixedslx_cvgr')

r4ss::run(dir = M14_2d1_fscv_path, skipfinished = FALSE, exe = exe_loc)

M14_2d1_fscv_out <- SS_output(M14_2d1_fscv_path, printstats = FALSE, verbose = FALSE)

# plots the results
SS_plots(M14_2d1_fscv_out)


##########
# Model 25_2a----
# growth from 14_2d1_fixedslx_cvgr
# selectivities from 25_2
M25_2a_path <- here::here(AYR, 'Nov_models', 'AK_skate_Tier3', 'rsch', 'M25_2a')

r4ss::run(dir = M25_2a_path, skipfinished = FALSE, exe = exe_loc)

M25_2a_out <- SS_output(M25_2a_path, printstats = FALSE, verbose = FALSE)

# plots the results
SS_plots(M25_2a_out)

##########
# Model 25_2----
# original 25_2 from Sept 2025
# Q = 0.836 (or something close to that)
M25_2_path <- here::here(AYR, 'Nov_models', 'AK_skate_Tier3', 'rsch', 'M25_2')

r4ss::run(dir = M25_2_path, skipfinished = FALSE, exe = exe_loc)

M25_2_out <- SS_output(M25_2_path, printstats = FALSE, verbose = FALSE)

# plots the results
SS_plots(M25_2_out)

##########
# Model 25_3a----
# growth from 14_2d1_fixedslx_cvgr
# selectivities from 25_2
M25_3a_path <- here::here(AYR, 'Nov_models', 'AK_skate_Tier3', 'rsch', 'M25_3a')

r4ss::run(dir = M25_3a_path, skipfinished = FALSE, exe = exe_loc)

M25_3a_out <- SS_output(M25_3a_path, printstats = FALSE, verbose = FALSE)

# plots the results
SS_plots(M25_3a_out)

##########
# Model 25_3----
# original from Sept 2025
# Q = 1
M25_3_path <- here::here(AYR, 'Nov_models', 'AK_skate_Tier3', 'rsch', 'M25_3')

r4ss::run(dir = M25_3_path, skipfinished = FALSE, exe = exe_loc)

M25_3_out <- SS_output(M25_3_path, printstats = FALSE, verbose = FALSE)

# plots the results
SS_plots(M25_3_out)

# Model 25_2M----
# only changed NatM ~0.2 to see if that fixed anything
M25_2aM_path <- here::here(AYR, 'Nov_models', 'AK_skate_Tier3', 'rsch', 'M25_2a_M')

r4ss::run(dir = M25_2aM_path, skipfinished = FALSE, exe = exe_loc)

M25_2aM_out <- SS_output(M25_2aM_path, printstats = FALSE, verbose = FALSE)

# plots the results
SS_plots(M25_2aM_out)

# Model 25_3M----
# only changed NatM ~0.2 to see if that fixed anything
M25_3aM_path <- here::here(AYR, 'Nov_models', 'AK_skate_Tier3', 'rsch', 'M25_3a_M')

r4ss::run(dir = M25_3aM_path, skipfinished = FALSE, exe = exe_loc)

M25_3aM_out <- SS_output(M25_3aM_path, printstats = FALSE, verbose = FALSE)

# plots the results
SS_plots(M25_3aM_out)




# Compare models ----
# bridge
datapath <- here::here(AYR, 'Nov_models', 'AK_skate_Tier3', 'rsch')
setwd(datapath)
bridge_out <- SSgetoutput(dirvec = c("M14_2d1",
                                     "M14_2d1_fixedslx",
                                     "M14_2d1_cvbounds",
                                     #"M14_2d1_fixedslxgrowth",
                                     "M14_2d1_fixedslx_cvgr",
                                     "M25_3a",
                                     "M25_3"))
setwd("C:/Users/cindy.Tribuzio/Work/SAFE/Assessments/BSAI_skates")
model_comp <- SSsummarize(bridge_out)
plotpath <- here::here(AYR, 'Nov_models', 'AK_skate_Tier3', 'rsch', 'model_comparison', 'bridge')
if (!dir.exists(plotpath)) dir.create(plotpath, recursive = TRUE)
SSplotComparisons(model_comp,
                  print = TRUE,
                  plotdir = here::here(plotpath),
                  legendlabels = c("M14_2d1",
                                   "M14_2d1_fixed_slx",
                                   "M14_2d1_cvbounds",
                                   #"M14_2d1_25_2growth",
                                   "M14_2d1_25_3gr_14_2CV",
                                   "M25_3a",
                                   "M25_3"
                                   ))

#catchability
setwd(datapath)
bridge_out <- SSgetoutput(dirvec = c("M25_2",
                                     "M25_2a",
                                     "M25_3a",
                                     "M25_3"))
setwd("C:/Users/cindy.Tribuzio/Work/SAFE/Assessments/BSAI_skates")
model_comp <- SSsummarize(bridge_out)
plotpath <- here::here(AYR, 'Nov_models', 'AK_skate_Tier3', 'rsch', 'model_comparison', 'catchability')
if (!dir.exists(plotpath)) dir.create(plotpath, recursive = TRUE)
SSplotComparisons(model_comp,
                  print = TRUE,
                  plotdir = here::here(plotpath),
                  legendlabels = c("M25_2",
                                   "M25_2a",
                                   "M25_3a",
                                   "M25_3"))

#mortality
setwd(datapath)
bridge_out <- SSgetoutput(dirvec = c("M25_2a_M",
                                     "M25_2a",
                                     "M25_3a",
                                     "M25_3a_M"))
setwd("C:/Users/cindy.Tribuzio/Work/SAFE/Assessments/BSAI_skates")
model_comp <- SSsummarize(bridge_out)
plotpath <- here::here(AYR, 'Nov_models', 'AK_skate_Tier3', 'rsch', 'model_comparison', 'mortality')
if (!dir.exists(plotpath)) dir.create(plotpath, recursive = TRUE)
SSplotComparisons(model_comp,
                  print = TRUE,
                  plotdir = here::here(plotpath),
                  legendlabels = c("M25_2a_M",
                                   "M25_2a",
                                   "M25_3a",
                                   "M25_3a_M"))

