datapath <- paste0(getwd(), "/Code/Tier3/", AYR, "/Model_Runs")

# Current model name
Model_name_old1 <- "M14_2_vold"
Model_name_old2 <- "M14_2_vbridge"
Model_name_new <- "M14_2_update"

#######################################################################################
######## Model comparisons

# read model outputs
model_dir_old1 <- here::here(datapath, Model_name_old1)
model_run_old1 <- r4ss::SS_output(dir = model_dir_old1,
                                 verbose = TRUE,
                                 printstats = TRUE)

model_dir_old2 <- here::here(datapath, Model_name_old2)
model_run_old2 <- r4ss::SS_output(dir = model_dir_old2, #does not work
                                 verbose = TRUE,
                                 printstats = TRUE)

model_dir_new <- here::here(datapath, Model_name_new)
model_run_new <- r4ss::SS_output(dir = model_dir_new, #does not work
                                  verbose = TRUE,
                                  printstats = TRUE)

model_comp <- r4ss::SSsummarize(list(model_run_old1, model_run_old2, model_run_new))

r4ss::SSplotComparisons(model_comp,
                        print = TRUE,
                        plotdir = here::here(datapath) )

