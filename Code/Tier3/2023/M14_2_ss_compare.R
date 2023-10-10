datapath <- paste0(getwd(), "/Code/Tier3/", AYR, "/Model_Runs")

# Current model name
Model_name_old1 <- "M14_2_vold"
Model_name_old2 <- "M14_2_vbridge"
Model_name_fixdat <- "M14_2_fix_old"
Model_name_adddat <- "M14_2_addnew"
Model_name_new <- "M14_2_update"

#######################################################################################
######## Model comparisons

# read model outputs
setwd(datapath)
bridge_out <- SSgetoutput(dirvec = c("M14_2_vold","M14_2_vbridge", "M14_2_fix_old", 'M14_2_addnew', 'M14_2_update'))
setwd("C:/Users/cindy.Tribuzio/Work/SAFE/Assessments/AFSC_BSAI_SKATE_Assessment")

model_comp <- r4ss::SSsummarize(bridge_out)

r4ss::SSplotComparisons(model_comp,
                        print = TRUE,
                        plotdir = here::here(datapath) )

