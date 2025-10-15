# function to run retrospecitve analysis
#adapted from P Hulson: https://github.com/pete-hulson/goa_pcod/blob/master/2024/R/mdl_anlys_fcns.R#L18

# setup ----
libs <- c("r4ss", "here", "tidyverse")
if(length(libs[which(libs %in% rownames(installed.packages()) == FALSE )]) > 0) {
  install.packages(libs[which(libs %in% rownames(installed.packages()) == FALSE)])}
lapply(libs, library, character.only = TRUE)
'%nin%'<-Negate('%in%') #this is a handy function

# Default with no version downloads the latest release
# r4ss::get_ss3_exe()

AYR <- 2025

# retro function----
#' function to run retrospective analysis
#' developed by p hulson
#' 
#' @param new_year current assessment year (default = NULL)
#' @param full_run boolean, whether to run full analysis or test (default = NULL)
#' @param base_mdl base model for current assessment (default = NULL)
#' @param rec_mdl recommended model for current assessment (default = NULL)
#' 
run_retro <- function(new_year = NULL,
                      full_run = NULL,
                      base_mdl = NULL,
                      rec_mdl = NULL){
  
  # define how many retro years you want to go back
  if(isTRUE(full_run)){
    ret_yr <- 10
  } else{ret_yr <- 1}
  
  ## run retro in parallel
  # list models
  mdls = c(base_mdl, rec_mdl)
  # set up index
  indx = seq(1, length(mdls))
  
  # Get the number of available cores
  num_cores <- parallel::detectCores()
  if(num_cores > length(mdls)) num_cores = length(mdls)
  # Set the number of cores to be used for parallel computing
  doParallel::registerDoParallel(cores = num_cores)
  
  foreach::foreach(i = indx) %dopar% {
    
    r4ss::retro(dir = here::here(new_year, "mgmt", mdls[i]),
                years = 0:-ret_yr)
    
  }
  
  # Stop parallel computing
  doParallel::stopImplicitCluster()
  
  # load the retrospective models
  retro_base <- r4ss::SSgetoutput(dirvec = here::here(new_year, "mgmt", base_mdl, "retrospectives", paste("retro", 0:-ret_yr, sep = "")),
                                  verbose = FALSE)
  retro_rec <- r4ss::SSgetoutput(dirvec = here::here(new_year, "mgmt", rec_mdl, "retrospectives", paste("retro", 0:-ret_yr, sep = "")),
                                 verbose = FALSE)
  # summarize the model results
  retrosumm_rec <- r4ss::SSsummarize(retro_rec,
                                     verbose = FALSE)
  retrosumm_base <- r4ss::SSsummarize(retro_base,
                                      verbose = FALSE)
  
  retro_res <- list(retrosumm_rec = retrosumm_rec, retrosumm_base = retrosumm_base)
  
  # save results
  if(isTRUE(full_run)){
    if (!dir.exists(here::here(new_year, "output", "retro"))) {
      dir.create(here::here(new_year, "output", "retro"), recursive = TRUE)
    }
    save(retro_res, file = here::here(new_year, "output", "retro", "retro_res.RData"))
  }
  
}

# plotting retro function----
plot_retro <- function(rec_mdl_res = NULL,
                       retro_name = NULL,
                       new_year = NULL){
  
  # get plot data
  # get recommended model ssb
  rec_mdl_res$derived_quants %>% 
    tidytable::select(Label, Value, StdDev) %>% 
    tidytable::slice(grep("SSB", Label, perl = TRUE)) %>% 
    tidytable::filter(!(Label %in% c("SSB_Virgin", "SSB_Initial", "SSB_unfished", "SSB_Btgt", "SSB_SPR", "SSB_MSY", "B_MSY/SSB_unfished"))) %>% 
    tidytable::mutate(Year = as.numeric(substr(Label, start = nchar(Label) - 3, stop = nchar(Label)))) %>% 
    tidytable::filter(Year <= new_year) %>% 
    tidytable::select(year = Year, Value, sd = StdDev) %>% 
    tidytable::mutate(Value = Value / 2, 
                      sd = sd / 2,
                      uci = Value + 1.96 * sd,
                      lci = Value - 1.96 * sd) %>% 
    tidytable::rename(!!paste0("a", new_year) := Value) -> rec_ssb
  
  # load retrospective results
  load(here::here(new_year, 'Nov_models', 'AK_skate_Tier3', 'mgmt', "retro", "retro_res.RData"))
  
  # get retro runs (data and model)
  retro_res[[retro_name]]$SpawnBio %>% 
    tidytable::filter(Yr %in% seq(1977, new_year)) %>% 
    tidytable::mutate(replist2 = case_when(Yr >= new_year ~ NA,
                                           .default = replist2),
                      replist3 = case_when(Yr >= new_year - 1 ~ NA,
                                           .default = replist3),
                      replist4 = case_when(Yr >= new_year - 2 ~ NA,
                                           .default = replist4),
                      replist5 = case_when(Yr >= new_year - 3 ~ NA,
                                           .default = replist5),
                      replist6 = case_when(Yr >= new_year - 4 ~ NA,
                                           .default = replist6),
                      replist7 = case_when(Yr >= new_year - 5 ~ NA,
                                           .default = replist7),
                      replist8 = case_when(Yr >= new_year - 6 ~ NA,
                                           .default = replist8),
                      replist9 = case_when(Yr >= new_year - 7 ~ NA,
                                           .default = replist9),
                      replist10 = case_when(Yr >= new_year - 8 ~ NA,
                                            .default = replist10),
                      replist11 = case_when(Yr >= new_year - 9 ~ NA,
                                            .default = replist11)) %>% 
    tidytable::pivot_longer(cols = c(paste0("replist", seq(1, 11)))) %>% 
    tidytable::mutate(peel = case_when(name == "replist1" ~ "0 years",
                                       name == "replist2" ~ "-1 years",
                                       name == "replist3" ~ "-2 years",
                                       name == "replist4" ~ "-3 years",
                                       name == "replist5" ~ "-4 years",
                                       name == "replist6" ~ "-5 years",
                                       name == "replist7" ~ "-6 years",
                                       name == "replist8" ~ "-7 years",
                                       name == "replist9" ~ "-8 years",
                                       name == "replist10" ~ "-9 years",
                                       name == "replist11" ~ "-10 years"),
                      type = "Data retrospective (10 years)") %>% 
    tidytable::select(type, peel, year = Yr, ssb = value) %>% 
    tidytable::drop_na() %>% 
    tidytable::mutate(ssb = ssb / 2) %>% 
    #tidytable::bind_rows(vroom::vroom(here::here(AYR, 'data', 'historical', 'hist_mdls.csv'), 
    #                                  progress = FALSE, 
    #                                  show_col_types = FALSE) %>% 
    #                       tidytable::right_join(rec_ssb %>% 
    #                                               tidytable::select(year, paste0("a", new_year))) %>% 
    #                       # write out appended historical models
    #                       vroom::vroom_write(., here::here(new_year, "output", "retro", 'hist_mdls.csv'), delim = ",") %>% 
    #                       tidytable::pivot_longer(cols = paste0("a", seq(2003, new_year))) %>% 
    #                       tidytable::mutate(peel = paste(as.numeric(substr(name, 2, 5)) - new_year, "years"),
    #                                         type = "Model retrospective (to 2003 assessment)") %>% 
    #                       tidytable::select(type, peel, year, ssb = value) %>% 
    #                       tidytable::drop_na()) %>% 
    tidytable::mutate(peel = factor(peel, levels = paste(seq(0, 2003 - new_year), "years"))) %>% 
    tidytable::left_join(rec_ssb %>% 
                           tidytable::select(year, uci, lci)) -> retro_dat
  
  # plot
  retro_plot <- ggplot(data = retro_dat, 
                       aes(x = year, y = ssb, color = peel)) +
    geom_ribbon(aes(ymax = uci, ymin = lci), alpha = 0.5, color = NA) +
    geom_line(linewidth = 0.777) +
    facet_wrap(~ type, ncol = 1, scale = "free_y") +
    scico::scale_color_scico_d(palette = 'roma') +
    labs(x = "Year", y = "Spawning biomass (t)", color = "Retrospective peel") +
    theme_bw(base_size = 14) +
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          axis.text.x = element_text(vjust = 0.5, angle = 90),
          legend.position = "none")
  # save
  ggsave(filename = paste0(retro_name, "_.png"),
         path = here::here(new_year, 'Nov_models', 'AK_skate_Tier3', 'mgmt', "retro"),
         width = 6,
         height = 4,
         units = "in")
  
}

# run retrospectives----
ret_yr <- 10

# M14_2d
r4ss::retro(dir = here::here(AYR, 'Nov_models', 'AK_skate_Tier3', "mgmt", 'M14_2d'),
            years = 0:-ret_yr,
            exe = 'ss_win')

# M14_2d1
r4ss::retro(dir = here::here(AYR, 'Nov_models', 'AK_skate_Tier3', "mgmt", 'M14_2d1'),
            years = 0:-ret_yr,
            exe = 'ss')

# M25_2
r4ss::retro(dir = here::here(AYR, 'Nov_models', 'AK_skate_Tier3', "mgmt", 'M25_2'),
            years = 0:-ret_yr,
            exe = 'ss')

# summarize the results----
# load the retrospective models
retro_m14_2d <- r4ss::SSgetoutput(dirvec = here::here(AYR, 'Nov_models', 'AK_skate_Tier3', "mgmt", 'M14_2d', 
                                                    'retrospectives', paste("retro", 0:-ret_yr, sep = "")),
                                verbose = FALSE)
retro_m14_2d1 <- r4ss::SSgetoutput(dirvec = here::here(AYR, 'Nov_models', 'AK_skate_Tier3', "mgmt", 'M14_2d1', 
                                                   'retrospectives', paste("retro", 0:-ret_yr, sep = "")),
                               verbose = FALSE)
retro_m25_2 <- r4ss::SSgetoutput(dirvec = here::here(AYR, 'Nov_models', 'AK_skate_Tier3', "mgmt", 'M25_2', 
                                                       'retrospectives', paste("retro", 0:-ret_yr, sep = "")),
                                   verbose = FALSE)
# summarize the model results (for plotting)
retrosumm_m14_2d <- r4ss::SSsummarize(retro_m14_2d,
                                   verbose = FALSE)
retrosumm_m14_2d1 <- r4ss::SSsummarize(retro_m14_2d1,
                                    verbose = FALSE)
retrosumm_m25_2 <- r4ss::SSsummarize(retro_m25_2,
                                       verbose = FALSE)

retro_res <- list(retrosumm_m14_2d = retrosumm_m14_2d, retrosumm_m14_2d1 = retrosumm_m14_2d1, retrosumm_m25_2 = retrosumm_m25_2)

# save results
  if (!dir.exists(here::here(AYR, 'Nov_models', 'AK_skate_Tier3', 'mgmt', "retro"))) {
    dir.create(here::here(AYR, 'Nov_models', 'AK_skate_Tier3', 'mgmt', "retro"), recursive = TRUE)
  }
  save(retro_res, file = here::here(AYR, 'Nov_models', 'AK_skate_Tier3', 'mgmt', "retro", "retro_res.RData"))





#plot retrospectives----
#M14_2d
M14_2d_path <- here::here(AYR, 'Nov_models', 'AK_skate_Tier3', 'mgmt', 'M14_2d')
M14_2d_out <- SS_output(M14_2d_path, printstats = FALSE, verbose = FALSE)

plot_retro(rec_mdl_res = M14_2d_out, 
           retro_name = 'retrosumm_m14_2d', 
           new_year = AYR)

#M14_2d1
M14_2d1_path <- here::here(AYR, 'Nov_models', 'AK_skate_Tier3', 'mgmt', 'M14_2d1')
M14_2d1_out <- SS_output(M14_2d1_path, printstats = FALSE, verbose = FALSE)

plot_retro(rec_mdl_res = M14_2d1_out, 
           retro_name = 'retrosumm_m14_2d1', 
           new_year = AYR)

#M25_2
M25_2_path <- here::here(AYR, 'Nov_models', 'AK_skate_Tier3', 'mgmt', 'M25_2')
M25_2_out <- SS_output(M25_2_path, printstats = FALSE, verbose = FALSE)

plot_retro(rec_mdl_res = M25_2_out, 
           retro_name = 'retrosumm_m25_2', 
           new_year = AYR)
