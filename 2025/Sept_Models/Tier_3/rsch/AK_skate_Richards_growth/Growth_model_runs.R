# Compares different approaches to fixing growth in SS3
# developed by C Tribuzio July 2025

# setup ----
libs <- c("r4ss", "here", "tidyverse")
if(length(libs[which(libs %in% rownames(installed.packages()) == FALSE )]) > 0) {
  install.packages(libs[which(libs %in% rownames(installed.packages()) == FALSE)])}
lapply(libs, library, character.only = TRUE)
'%nin%'<-Negate('%in%') #this is a handy function

# Default with no version downloads the latest release
# r4ss::get_ss3_exe()

exe_loc <- here::here('2025/2025_Sept_models/AK_skate_Tier3/ss.exe')

# Arun1 fixed growth parameters based on model estimated with Amax = 20----
Arun1_mode_path <- here::here('2025/2025_Sept_models/AK_skate_Tier3/AK_skate_Richards_growth/agerun1_fixed_params_Amx20')
run(dir = Arun1_mode_path, skipfinished = FALSE, exe = exe_loc)

mArun1 <- SS_output(Arun1_mode_path, printstats = FALSE, verbose = FALSE)
mArun1_out <- SS_output(dir = Arun1_mode_path, verbose = TRUE)

#mArun1_SS <- SSsummarize(mArun1_out)

# plots the results
SS_plots(mArun1_out)

# Arun2 fixed growth parameters based on model estimated with Amax = 26----
Arun2_mode_path <- here::here('2025/2025_Sept_models/AK_skate_Tier3/AK_skate_Richards_growth/agerun2_fixed_params_Amx26')
run(dir = Arun2_mode_path, skipfinished = FALSE, exe = exe_loc)

mArun2 <- SS_output(Arun2_mode_path, printstats = FALSE, verbose = FALSE)
mArun2_out <- SS_output(dir = Arun2_mode_path, verbose = TRUE)

#mArun1_SS <- SSsummarize(mArun1_out)

# plots the results
SS_plots(mArun2_out)


#overall comparison of model runs----
datapath <- paste0(getwd(), "/2025/2025_Sept_models/AK_skate_Tier3/AK_skate_Richards_growth")
setwd(datapath)
bridge_out <- SSgetoutput(dirvec = c("base_M14_2d_fixedcatch", 
                                     "agerun1_fixed_params_Amx20", 
                                     "agerun2_fixed_params_Amx26"))
setwd("C:/Users/cindy.Tribuzio/Work/SAFE/Assessments/BSAI_skates")

model_comp <- SSsummarize(bridge_out)

SSplotComparisons(model_comp,
                  print = TRUE,
                  plotdir = here::here(datapath),
                  legendlabels = c('base', 'Amax20', 'Amax26'))




# failed models----
# Arun3 same as Arun2 with expanded age bins, not changing ageing error or size at age ----
Arun3_mode_path <- here::here('2025/2025_Sept_models/AK_skate_Tier3/AK_skate_Richards_growth/agerun3_Amx26_bins')
run(dir = Arun3_mode_path, skipfinished = FALSE, exe = exe_loc)

mArun3 <- SS_output(Arun3_mode_path, printstats = FALSE, verbose = FALSE)
mArun3_out <- SS_output(dir = Arun3_mode_path, verbose = TRUE)

#mArun1_SS <- SSsummarize(mArun1_out)

# plots the results
SS_plots(mArun3_out)

# Arun4 same as Arun2 with mean size at age turned off----
Arun4_mode_path <- here::here('2025/2025_Sept_models/AK_skate_Tier3/AK_skate_Richards_growth/agerun4_Amx26_SAAoff')
run(dir = Arun4_mode_path, skipfinished = FALSE, exe = exe_loc)

mArun4 <- SS_output(Arun4_mode_path, printstats = FALSE, verbose = FALSE)
mArun4_out <- SS_output(dir = Arun4_mode_path, verbose = TRUE)

#mArun1_SS <- SSsummarize(mArun1_out)

# plots the results
SS_plots(mArun4_out)
