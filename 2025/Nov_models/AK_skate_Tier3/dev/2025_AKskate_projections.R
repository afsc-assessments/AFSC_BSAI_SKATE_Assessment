# Script to run the AK skate projection scenarios
# 2025 by C Tribuzio
# something in this is creating issues, not running the projections properly.
#need help with sleuthing it out

# Step 1: create PROJ folder
# Step 2: copy over final converged model files
# Step 3: In the starter.ss file you should change it to read from the converged .par file 
#          1 # 0=use init values in control file; 1=use ss.par
# Step 4: Make sure forecast file is correct

# Setup ----
# load required packages and code
libs <- c("data.table",
          'tidyverse',
          "magrittr", 
          "r4ss",
          'foreach')

if(length(libs[which(libs %in% rownames(installed.packages()) == FALSE )]) > 0) {
  install.packages(libs[which(libs %in% rownames(installed.packages()) == FALSE)])}

lapply(libs, library, character.only = TRUE)

# Current assessment year
AYR <- 2025

exe_loc <- here::here(AYR, "Nov_models", "AK_skate_Tier3", 'ss.exe')



# load functions
# from: https://github.com/afsc-assessments/AK_Scenarios_For_SS
source(here::here(AYR, "Nov_models", "AK_skate_Tier3","R", 'AK_Scenarios_For_SS', "Running_scenarios_Update.r"))

# Run management scenarios----
# Model 14_2d is so old, it can't use consistent proj methods, only archaic method from previous author

# updated model that doesn't converge
M14_2d1_projrun <- Do_AK_TIER_3_Scenarios(DIR = here::here(AYR, 'Nov_models', 'AK_skate_Tier3', 'mgmt', 'M14_2d1', 'PROJ'), 
                                         CYR = AYR, 
                                         SYR = 1977,  
                                         SEXES = 1, 
                                         FLEETS = c(1:3), 
                                         Scenario2 = 2, 
                                         S2_F = 0.67, #mean proportion catch of ABC 
                                         s4_F = 0.5, 
                                         do_fig = TRUE, 
                                         do_mark=FALSE, 
                                         URL='https://www.npfmc.org/wp-content/PDFdocuments/SAFE/2024/BSAIskate.pdf')
M14_2d1_projrun$FIGS

# save the plots separately
output_dir <- "proj_plots"
if (!dir.exists(output_dir)) {
  dir.create(output_dir)
}

plot_name <- names(M14_2d1_projrun$FIGS[[1]])
# Iterate through the list and save each plot
for (i in seq_along(M14_2d1_projrun$FIGS[[1]])) {
  file_path <- file.path(output_dir, paste0("spbiom", plot_name[i], ".png"))
  ggsave(filename = file_path, plot = M14_2d1_projrun$FIGS[[1]][[i]], 
         width = 8, height = 6, units = "in", dpi = 300)
}

for (i in seq_along(M14_2d1_projrun$FIGS[[2]])) {
  file_path <- file.path(output_dir, paste0("catch", plot_name[i], ".png"))
  ggsave(filename = file_path, plot = M14_2d1_projrun$FIGS[[2]][[i]], 
         width = 8, height = 6, units = "in", dpi = 300)
}

# save the tables separately
output_dir <- "proj_tables"
if (!dir.exists(output_dir)) {
  dir.create(output_dir)
}
#output tables
two_yr <- M14_2d1_projrun$Two_year
write_csv(two_yr, here::here(getwd(), output_dir, 'proj_2yr.csv'))
scen_catch <- M14_2d1_projrun$CATCH
write_csv(scen_catch, here::here(getwd(), output_dir, 'proj_catch.csv'))
scen_biom <- M14_2d1_projrun$SSB
write_csv(scen_biom, here::here(getwd(), output_dir, 'proj_ssb.csv'))







# Model 25_2
M25_2_projrun <- Do_AK_TIER_3_Scenarios(DIR = here::here(AYR, 'Nov_models', 'AK_skate_Tier3', 'mgmt', 'M25_2', 'PROJ'), 
                                          CYR = AYR, 
                                          SYR = 1977,  
                                          SEXES = 1, 
                                          FLEETS = c(1:3), 
                                          Scenario2 = 2, 
                                          S2_F = 0.67, #mean proportion catch of ABC 
                                          s4_F = 0.5, 
                                          do_fig = TRUE, 
                                          do_mark=FALSE, 
                                          URL='https://www.npfmc.org/wp-content/PDFdocuments/SAFE/2024/BSAIskate.pdf')
M25_2_projrun$FIGS

# save the plots separately
output_dir <- "proj_plots"
if (!dir.exists(output_dir)) {
  dir.create(output_dir)
}

plot_name <- names(M25_2_projrun$FIGS[[1]])
# Iterate through the list and save each plot
for (i in seq_along(M25_2_projrun$FIGS[[1]])) {
  file_path <- file.path(output_dir, paste0("spbiom", plot_name[i], ".png"))
  ggsave(filename = file_path, plot = M25_2_projrun$FIGS[[1]][[i]], 
         width = 8, height = 6, units = "in", dpi = 300)
}

for (i in seq_along(M25_2_projrun$FIGS[[2]])) {
  file_path <- file.path(output_dir, paste0("catch", plot_name[i], ".png"))
  ggsave(filename = file_path, plot = M25_2_projrun$FIGS[[2]][[i]], 
         width = 8, height = 6, units = "in", dpi = 300)
}

# save the tables separately
output_dir <- "proj_tables"
if (!dir.exists(output_dir)) {
  dir.create(output_dir)
}
#output tables
two_yr <- M25_2_projrun$Two_year
write_csv(two_yr, here::here(getwd(), output_dir, 'proj_2yr.csv'))
scen_catch <- M25_2_projrun$CATCH
write_csv(scen_catch, here::here(getwd(), output_dir, 'proj_catch.csv'))
scen_biom <- M25_2_projrun$SSB
write_csv(scen_biom, here::here(getwd(), output_dir, 'proj_ssb.csv'))
