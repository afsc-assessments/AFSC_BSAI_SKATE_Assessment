# Model 14_2d1 to the proposed model for 2026
# developed by C Tribuzio October 2026

#TODO
# update LOO
# run M sensitivies and growth option, not for doc, but for presentation

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
M25_3_path <- here::here(AYR, 'Nov_models', 'AK_skate_Tier3', 'mgmt', 'M25_3')
profM_path <- here::here(AYR, 'Nov_models', 'AK_skate_Tier3', 'mgmt', 'likelihood_profiles', 'NatM')
profq_path <- here::here(AYR, 'Nov_models', 'AK_skate_Tier3', 'mgmt', 'likelihood_profiles', 'q_catch')
profR0_path <- here::here(AYR, 'Nov_models', 'AK_skate_Tier3', 'mgmt', 'likelihood_profiles', 'LNR0')

#copy_SS_inputs(dir.old = M25_3_path, dir.new = profM_path, copy_exe = TRUE, overwrite = TRUE, dir.exe = exe_loc)
#copy_SS_inputs(dir.old = M25_3_path, dir.new = profq_path, copy_exe = TRUE)
#copy_SS_inputs(dir.old = M25_3_path, dir.new = profR0_path, copy_exe = TRUE)




# Read control file to find exact parameter text strings
#ctl <- SS_readctl("control.ss_new", use_datlist = TRUE, datlist = "data_echo.ss_new")
#head(ctl$MG_parms) # Look here for M and q labels
#head(ctl$SR_parms) # Look here for ln(R0) labels


##########
# M profile----
setwd(profM_path)
profile(
  dir = profM_path,
  masterctlfile = "control.ss_new",
  newctlfile = "control_modified.ss",  # Change starter.ss to match this
  string = "NatM_uniform_Fem_GP_1",        # Find this label inside ctl$MG_parms
  profilevec = seq(0.07, 0.20, by = 0.01), 
  exe = "ss",                        # Name of the binary executable in the directory
  extras = "-nox"                      # Speeds up runtimes by hiding text outputs
)

profM  <- SSgetoutput(dirvec = profM_path, keyvec = 1:14) 
profM_summary <- SSsummarize(profM)

# Generate diagnostic curves
SSplotProfile(
  summaryoutput = profM_summary,
  profile.string = "NatM", 
  profile.label = "Natural Mortality (M)",
  print = TRUE, 
  plotdir = profM_path
)

##########
# ln(R0) profile----
profile(
  dir = profR0_path,
  masterctlfile = "control.ss_new",
  newctlfile = "control_modified.ss",
  string = "SR_LN(R0)",
  profilevec = seq(9.0, 11.2, by = 0.2), 
  exe = "ss",
  extras = "-nox"
)

profR0  <- SSgetoutput(dirvec = profR0_path, keyvec = 1:12) 
profR0_summary <- SSsummarize(profR0)

# Generate diagnostic curves
SSplotProfile(
  summaryoutput = profR0_summary,
  profile.string = "R0", 
  profile.label = "SR_LN(R0)",
  print = TRUE, 
  plotdir = profR0_path
)

##########
# q profile----
profile(
  dir = profq_path,
  masterctlfile = "control.ss_new",
  newctlfile = "control_modified.ss",
  string = "LnQ_base_SURV(3)",
  profilevec = seq(0, 1, by = 0.1), 
  exe = "ss",
  extras = "-nox"
)

profq  <- SSgetoutput(dirvec = profq_path, keyvec = 1:11) 
profq_summary <- SSsummarize(profq)

# Generate diagnostic curves
SSplotProfile(
  summaryoutput = profq_summary,
  profile.string = "LnQ", 
  profile.label = "Catchability (Lnq)",
  print = TRUE, 
  plotdir = profq_path
)
