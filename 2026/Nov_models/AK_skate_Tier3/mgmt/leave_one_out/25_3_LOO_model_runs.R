# leave one out for model 25_3
# developed by C Tribuzio October 2026

#TODO
# Automate the .dat file changes
# for now, done the hard way
# fix the control files so selectivity doesn't estimate when no data

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
# Model 25_3 (base)----
# model was already run and copied to this folder
M25_3_path <- here::here(AYR, 'Nov_models', 'AK_skate_Tier3', 'mgmt', 'leave_one_out', 'M25_3')

M25_3_out <- SS_output(M25_3_path, printstats = FALSE, verbose = FALSE)

##########
# Model 25_3_noagecomp----
# removes the age comp data
M25_3ac_path <- here::here(AYR, 'Nov_models', 'AK_skate_Tier3', 'mgmt', 'leave_one_out', 'M25_3_noagecomp')

r4ss::run(dir = M25_3ac_path, skipfinished = FALSE, exe = exe_loc)

M25_3ac_out <- SS_output(M25_3ac_path, printstats = FALSE, verbose = FALSE)

# plots the results
SS_plots(M25_3ac_out)

##########
# Model 25_3_notwllength----
# removes trawl fishery length comp data
M25_3twl_path <- here::here(AYR, 'Nov_models', 'AK_skate_Tier3', 'mgmt', 'leave_one_out', 'M25_3_notwllength')

r4ss::run(dir = M25_3twl_path, skipfinished = FALSE, exe = exe_loc)

M25_3twl_out <- SS_output(M25_3twl_path, printstats = FALSE, verbose = FALSE)

# plots the results
SS_plots(M25_3twl_out)

##########
# Model 25_3_nolgllength----
# removes longline fishery length comp data
M25_3lgl_path <- here::here(AYR, 'Nov_models', 'AK_skate_Tier3', 'mgmt', 'leave_one_out', 'M25_3_nolgllength')

r4ss::run(dir = M25_3lgl_path, skipfinished = FALSE, exe = exe_loc)

M25_3lgl_out <- SS_output(M25_3lgl_path, printstats = FALSE, verbose = FALSE)

# plots the results
SS_plots(M25_3lgl_out)

##########
# Model 25_3_nofisherylength----
# removes all fishery length comp data
M25_3fl_path <- here::here(AYR, 'Nov_models', 'AK_skate_Tier3', 'mgmt', 'leave_one_out', 'M25_3_nofisherylength')

r4ss::run(dir = M25_3fl_path, skipfinished = FALSE, exe = exe_loc)

M25_3fl_out <- SS_output(M25_3fl_path, printstats = FALSE, verbose = FALSE)

# plots the results
SS_plots(M25_3fl_out)

##########
# Model 25_3_nosurvlength----
# removes all survey length comp data
M25_3sl_path <- here::here(AYR, 'Nov_models', 'AK_skate_Tier3', 'mgmt', 'leave_one_out', 'M25_3_nosurvlength')

r4ss::run(dir = M25_3sl_path, skipfinished = FALSE, exe = exe_loc)

M25_3sl_out <- SS_output(M25_3sl_path, printstats = FALSE, verbose = FALSE)

# plots the results
SS_plots(M25_3sl_out)

##########
# Model 25_3_nolength----
# removes all survey length comp data
M25_3nl_path <- here::here(AYR, 'Nov_models', 'AK_skate_Tier3', 'mgmt', 'leave_one_out', 'M25_3_nolength')

r4ss::run(dir = M25_3nl_path, skipfinished = FALSE, exe = exe_loc)

M25_3nl_out <- SS_output(M25_3nl_path, printstats = FALSE, verbose = FALSE)

# plots the results
SS_plots(M25_3nl_out)

##########
# Model 25_3_nosurvindex----
# removes all survey length comp data
M25_3si_path <- here::here(AYR, 'Nov_models', 'AK_skate_Tier3', 'mgmt', 'leave_one_out', 'M25_3_nosurvindex')

r4ss::run(dir = M25_3si_path, skipfinished = FALSE, exe = exe_loc)

M25_3si_out <- SS_output(M25_3si_path, printstats = FALSE, verbose = FALSE)

# plots the results
SS_plots(M25_3si_out)

##########
# Model 25_3_survonly----
# no fishery lengths no age comps, trying to see if it will fit the survey index better
M25_3so_path <- here::here(AYR, 'Nov_models', 'AK_skate_Tier3', 'mgmt', 'leave_one_out', 'M25_3_survonly')

r4ss::run(dir = M25_3so_path, skipfinished = FALSE, exe = exe_loc)

M25_3so_out <- SS_output(M25_3so_path, printstats = FALSE, verbose = FALSE)

# plots the results
SS_plots(M25_3so_out)

##########
# Model 25_3_survageonly----
# no fishery lengths no age comps, trying to see if it will fit the survey index better
M25_3sa_path <- here::here(AYR, 'Nov_models', 'AK_skate_Tier3', 'mgmt', 'leave_one_out', 'M25_3_survageonly')

r4ss::run(dir = M25_3sa_path, skipfinished = FALSE, exe = exe_loc)

M25_3sa_out <- SS_output(M25_3sa_path, printstats = FALSE, verbose = FALSE)

# plots the results
SS_plots(M25_3sa_out)


# Compare models ----
datapath <- here::here(AYR, 'Nov_models', 'AK_skate_Tier3', 'mgmt', 'leave_one_out')
setwd(datapath)
bridge_out <- SSgetoutput(dirvec = c("M25_3",
                                     "M25_3_noagecomp",
                                     "M25_3_notwllength",
                                     "M25_3_nolgllength",
                                     "M25_3_nofisherylength",
                                     "M25_3_nosurvlength",
                                     "M25_3_nolength"))
setwd("C:/Users/cindy.Tribuzio/Work/SAFE/Assessments/BSAI_skates")
model_comp <- SSsummarize(bridge_out)
plotpath <- here::here(AYR, 'Nov_models', 'AK_skate_Tier3', 'mgmt', 'leave_one_out', 'model_comparison')
if (!dir.exists(plotpath)) dir.create(plotpath, recursive = TRUE)
SSplotComparisons(model_comp,
                  print = TRUE,
                  plotdir = here::here(plotpath),
                  legendlabels = c("M25_3",
                                   "no age comp",
                                   "no twl length",
                                   "no lgl length",
                                   "no fishery length",
                                   "no survey length",
                                   "no lengths"
                                   ))

# adding data back in
datapath <- here::here(AYR, 'Nov_models', 'AK_skate_Tier3', 'mgmt', 'leave_one_out')
setwd(datapath)
bridge_out <- SSgetoutput(dirvec = c("M25_3",
                                     "M25_3_survonly",
                                     "M25_3_survageonly"))
setwd("C:/Users/cindy.Tribuzio/Work/SAFE/Assessments/BSAI_skates")
model_comp <- SSsummarize(bridge_out)
plotpath <- here::here(AYR, 'Nov_models', 'AK_skate_Tier3', 'mgmt', 'leave_one_out', 'model_comparison', 'surveydat')
if (!dir.exists(plotpath)) dir.create(plotpath, recursive = TRUE)
SSplotComparisons(model_comp,
                  print = TRUE,
                  plotdir = here::here(plotpath),
                  legendlabels = c("M25_3",
                                   "survey index",
                                   "survey index/ages"
                  ))

