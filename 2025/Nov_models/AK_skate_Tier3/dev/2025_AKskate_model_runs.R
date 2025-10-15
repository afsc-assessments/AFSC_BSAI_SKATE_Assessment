# Bridging Model 14_2d to the proposed model for 2025
# developed by C Tribuzio October 2025

# setup ----
libs <- c("r4ss", "here", "tidyverse")
if(length(libs[which(libs %in% rownames(installed.packages()) == FALSE )]) > 0) {
  install.packages(libs[which(libs %in% rownames(installed.packages()) == FALSE)])}
lapply(libs, library, character.only = TRUE)
'%nin%'<-Negate('%in%') #this is a handy function

# Default with no version downloads the latest release
# r4ss::get_ss3_exe()

AYR <- 2025

exe_loc <- here::here(paste0(AYR, '/Nov_models/AK_skate_Tier3/ss.exe'))

##########
#base model (14_2d) brought over without re-running
#version update ran in M14_2d_ss3version

# Model 14_2d----
# Previously accepted model, with updated data
exe_loc_old <- here::here(paste0(AYR, '/Nov_models/AK_skate_Tier3/mgmt/M14_2d/ss_win.exe'))
M14_2d_path <- here::here(AYR, 'Nov_models', 'AK_skate_Tier3', 'mgmt', 'M14_2d')
r4ss::run(dir = M14_2d_path, skipfinished = FALSE, exe = exe_loc_old)

M14_2d_out <- SS_output(M14_2d_path, printstats = FALSE, verbose = FALSE)

# plots the results
SS_plots(M14_2d_out)

# Model 14_2d1----
# base model with version update and recruitment ramp corrections
M14_2d1_path <- here::here(AYR, 'Nov_models', 'AK_skate_Tier3', 'mgmt', 'M14_2d1')

r4ss::run(dir = M14_2d1_path, skipfinished = FALSE, exe = exe_loc)

M14_2d1_out <- SS_output(M14_2d1_path, printstats = FALSE, verbose = FALSE)

# plots the results
SS_plots(M14_2d1_out)

# Model 25_2----
# fixed starting point for slx, and widened some parameters, fixed ending pt for survey slx
M25_2_path <- here::here(AYR, 'Nov_models', 'AK_skate_Tier3', 'mgmt', 'M25_2')

r4ss::run(dir = M25_2_path, skipfinished = FALSE, exe = exe_loc)

M25_2_out <- SS_output(M25_2_path, printstats = FALSE, verbose = FALSE)

# plots the results
SS_plots(M25_2_out)

# Compare bridging models ----
datapath <- here::here(AYR, 'Nov_models', 'AK_skate_Tier3', 'mgmt')
setwd(datapath)
bridge_out <- SSgetoutput(dirvec = c("M14_2d", 
                                     "M14_2d1",
                                     "M25_2"))
setwd("C:/Users/cindy.Tribuzio/Work/SAFE/Assessments/BSAI_skates")
model_comp <- SSsummarize(bridge_out)
plotpath <- here::here(AYR, 'Nov_models', 'AK_skate_Tier3', 'mgmt', 'model_comparison')
SSplotComparisons(model_comp,
                  print = TRUE,
                  plotdir = here::here(plotpath),
                  legendlabels = c('M14_2d', 
                                   'M14_2d1',
                                   'M25_2'
                                   ))