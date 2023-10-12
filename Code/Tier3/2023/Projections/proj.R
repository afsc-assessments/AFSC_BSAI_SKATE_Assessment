#' Understanding proj (or not)
#'
#' workflow:
#'   1. setup folders
#'   2. populate folders with "built in files"
#'   3. populate folders with assessment info
#'   4. run model
#'       a. max F output
#'       b. author F output
#'   5. compile and retrieve results
#'
#' @param year model year
#' @param last_full_assess year of the last full assessment, default: NULL
#' @param alt prefix folder structure e.g. "mgmt" - default = NULL folder structure: 'year\alt'
#' @param folder folder that model is in, folder structure: 'year\alt\folder'
#' @param model_name name of the assessment model e.g., "m18.2" not the projection model, this is hopefully the same name as the folder...
#' @param species e.g., "dusky"
#' @param region e.g., "goa"
#' @param rec_age recruitment age, default: 2
#' @param off_yr is this a full assessment or off year, default: FALSE
#' @param run_name  - default: "Standard"
#' @param tac_abc -  set TAC equal to ABC (1 means true, otherwise false)",
#' @param srr - Stock-recruitment type (1=Ricker, 2=Bholt)
#' @param rec_proj projection rec form (default: 1 = use observed mean and std, option 2 = use estimated SRR and estimated sigma R)
#' @param srr_cond SR-Conditioning (0 means no, 1 means use Fmsy == F35%?, 2 means Fmsy == F35% and Bmsy=B35%  condition (affects SRR fits)
#' @param srr_prior a prior that mean historical recruitment is similar to expected recruitment at half mean SSB and double mean SSB 0 means don't use, otherwise specify CV
#' @param nyrs_proj Number of projection years default: 14
#' @param nsims Number of simulations default: 1000
#' @param admb_home location of ADMB if different than c:/admb
#' @param n_species number of species being evaluated: default: 1
#' @param abc_mult abc multiplier, default: 1
#' @param pop_scalar population scalar, default: 1000
#' @param tac_categories  default 1
#' @param tac_models default 1

#' @export

proj_ak <- function(year, last_full_assess=NULL, alt=NULL, folder, species, region, rec_age = 2,
                    off_yr = FALSE, run_name = "Standard",
                    tac_abc = 1, srr = 1, rec_proj = 1, srr_cond = 0, srr_prior = 0,
                    nyrs_proj = 14, nsims = 1000, n_species = 1, abc_mult = 1, pop_scalar = 1000,
                    tac_categories = 1, tac_models = 1, admb_home = NULL) {



  # messages
  if(isTRUE(off_yr) & is.null(last_full_assess)) {
    stop("for an 'off year' you need to provide the year of the last full assessment. \ne.g., last_full_assess = 2020")
  }
  # globals
  if(isTRUE(off_yr)){
    yr = last_full_assess:year
  } else {
    yr = year
  }

  if(!is.null(alt)) {
    folder = paste0(alt, "/", folder)
  }

  if(is.null(admb_home)){
    R2admb::setup_admb()
  } else {
    R2admb::setup_admb(admb_home)
  }

  # 1. setup folders ----
  if(dir.exists(here::here(year, folder, "proj", "model", "data")) == FALSE){
    dir.create(here::here(year, folder, "proj", "model", "data"), recursive = TRUE)
  }
  if(dir.exists(here::here(year, folder, "proj", "apportionment")) == FALSE){
    dir.create(here::here(year, folder, "proj", "apportionment"))
  }
  if(dir.exists(here::here(year, folder, "proj", "author_f")) == FALSE){
    dir.create(here::here(year, folder, "proj", "author_f"))
  }
  if(dir.exists(here::here(year, folder, "proj", "max_f")) == FALSE){
    dir.create(here::here(year, folder, "proj", "max_f"))
  }
  if(dir.exists(here::here(year, folder, "processed")) == FALSE){
    dir.create(here::here(year, folder, "processed"), recursive = TRUE)
  }

  # 2 populate folders i.e. copy files
  if(isTRUE(off_yr)) {
    file.copy(here::here(year, "base", "proj.dat"),
              here::here(year, folder, "proj", "model", "data", paste0(species, ".dat")))
  } else {
  file.copy(here::here(year, folder, "proj.dat"),
            here::here(year, folder, "proj", "model", "data", paste0(species, ".dat")))
  }
  file.copy(system.file("tpl", c("main.tpl", "main.exe"), package = "afscassess"),
            here::here(year, folder, "proj", "model"))
  file.copy(system.file("dat", "tacpar.dat", package = "afscassess"),
            here::here(year, folder, "proj", "model"))

  # get data
  catch = vroom::vroom(here::here(year, "data", "output", "fsh_catch.csv")) # currently setup for 1 fleet...
  yld_rat = vroom::vroom(here::here(year, "data", "output", "yld_rat.csv"))
  data_pull = read.table(here::here(year, "data", "raw", "data_called.txt"))[2,1]

  # 3a create the setup.dat file
  dat = c(paste("# projection run for", region, "-", species, "in", year),
          "# Run name",
          run_name,
          "# numer of alt models",
          "7",
          "# alts - alternate models",
          as.character(seq_len(7)),
          "# tac_abc - Flag to set TAC equal to ABC (1 means true, otherwise false)",
          as.character(tac_abc),
          "# srr - Stock-recruitment type (1=Ricker, 2=Bholt)",
          as.character(srr),
          "# rec_proj - projection rec form (default: 1 = use observed mean and std, option 2 = use estimated SRR and estimated sigma R)",
          as.character(rec_proj),
          "# srr_cond - SR-Conditioning (0 means no, 1 means use Fmsy == F35%?, 2 means Fmsy == F35% and Bmsy=B35%  condition (affects SRR fits)",
          as.character(srr_cond),
          "# srr_prior - Condition that there is a prior that mean historical recruitment is similar to expected recruitment at half mean SSB and double mean SSB 0 means don't use, otherwise specify CV",
          as.character(srr_prior),
          "# write_big - Flag to write big file (of all simulations rather than a summary, 0 means don't do it, otherwise do it)",
          "1",
          "# nyrs_proj - Number of projection years",
          "14",
          "# nsims - Number of simulations",
          as.character(nsims),
          "# beg_yr_label - Begin Year",
          if(isFALSE(off_yr)){
            as.character(year)
          } else {
            as.character(last_full_assess)
          }
  )

  write.table(dat, file = here::here(year, folder, "proj", "model", "setup.dat"),
              quote=FALSE, row.names=FALSE, col.names=FALSE)

  # create .dat files for max f projections
  dat <- c("#_Number_of_years with specified catch",
           if(isFALSE(off_yr)) 1 else length(yr),
           "# Number of species",
           as.character(n_species),
           "# dat file for each species",
           paste0("data/", species, ".dat"),
           "# ABC multiplier",
           as.character(abc_mult),
           "# Population scalar",
           as.character(pop_scalar),
           "# Number of TAC model categories",
           as.character(tac_categories),
           "# TAC model indices (for aggregating)",
           as.character(tac_models),
           if(isTRUE(off_yr)){
             offs = NA
             if(length(yr) - 1 == 1) {
               offs = paste(paste(yr[1], dplyr::filter(catch, year %in% yr[1])  %>%
                                    dplyr::pull(catch), sep = "\t"),
                            "# Finalized previous year catch")
             } else {
               for(i in 1:(length(yr) - 1)) {
                 offs[i] = paste(paste(yr[i], dplyr::filter(catch, year %in% yr[i])  %>%
                                         dplyr::pull(catch), sep = "\t"),
                                 "# Finalized previous year catch")
               }
             }
             offs
           },
           paste(paste(tail(yr,1), yld_rat$proj_catch, sep ="\t"),
                 "# Catch for", year,
                 "Estimated from catch thru", data_pull)

  )

  write.table(dat, file = here::here(year, folder, "proj", "model", "data", paste0(species, "_max_spcat.dat")),
              quote=FALSE, row.names=FALSE, col.names=FALSE)
  write.table(dat, file = here::here(year, folder, "proj", "model", "spp_catch.dat"),
              quote=FALSE, row.names=FALSE, col.names=FALSE)

  # run model
  setwd(here::here(year, folder, "proj", "model"))
  R2admb::run_admb("main", verbose = TRUE)

  # move files to results folder
  file.copy(here::here(year, folder, "proj", "model",
                       c("bigfile.out", "F_profile.out", "means.out", "percentdb.out", "percentiles.out")),
            here::here(year, folder, "proj", "max_f"), overwrite = TRUE)

  # cleanup
  file.remove(here::here(year, folder, "proj", "model",
                         c("bigfile.out", "F_profile.out", "means.out", "percentdb.out", "percentiles.out")))

  # get mean values
  read.delim(here::here(year, folder, "proj", "max_f", "bigfile.out"), sep="", header=T) %>%
    tidytable::rename(Year=Yr, Alt = Alternative, Stock = Spp, Total_Biom = Tot_biom) %>%
    tidytable::summarise(tidytable::across(c(ABC, OFL, Catch, SSB, F, Total_Biom), mean),
                         .by = c(Alt, Stock, Year)) %>%
    vroom::vroom_write(here::here(year, folder, "proj", "max_f", "bigsum.csv"), ",")

  # author F
  # get catch for next three years under max f scenario
  maxf <- readLines(here::here(here::here(year, folder, "proj", "max_f", "percentiles.out")))

  c_1 = as.numeric(stringr::str_split(maxf[grep(year+1, maxf)[1]], " ")[[1]][8]) * 1000
  c_2 = as.numeric(stringr::str_split(maxf[grep(year+2, maxf)[1]], " ")[[1]][8]) * 1000

  # .dat file with future catch set at yield ratio * max catch
  dat <- c("#_Number_of_years with specified catch",
           if(isFALSE(off_yr)) 3 else length(yr) + 2,
           "# Number of species",
           1,
           "# data files for each species",
           paste0("data/", species, ".dat"),
           "# ABC Multipliers",
           1,
           "# Population scalars",
           1000,
           "# Number of TAC model categories",
           1,
           "# TAC model indices (for aggregating)",
           1,
           "# Catch in each future year",
           if(isTRUE(off_yr)) {
             offs = NA
             for(i in 1:(length(yr) - 1)) {
               offs[i] = paste(paste(yr[i], dplyr::filter(catch, year %in% yr[i])  %>%
                                       dplyr::pull(catch), sep = "\t"),
                               "# Finalized previous year catch")
             }
             offs
           },
           c(paste0(paste(year, yld_rat$proj_catch, sep = "\t"),
                    " # Estimated from catch thru ",
                    as.Date(data_pull)," with expansion factor = ", round(yld_rat$ratio, digits = 4)),
             paste0(paste(year + 1, round(c_1 * yld_rat$yld), sep = "\t"),
                    " # Estimated as Max F scenario catch * yieldratio of ", round(yld_rat$yld, digits = 3)),
             paste0(paste(year + 2, round(c_2 * yld_rat$yld), sep = "\t"),
                    " # Estimated as Max F scenario catch * yieldratio of ", round(yld_rat$yld, digits = 3)))
  )


  write.table(dat, file = here::here(year, folder, "proj", "model", "spp_catch.dat"),
              quote = FALSE, row.names = FALSE, col.names = FALSE)
  write.table(dat, file = here::here(year, folder, "proj", "model", "data", paste0(species, "_spcat.dat")),
              quote = FALSE, row.names = FALSE, col.names = FALSE)
  # run model
  R2admb::run_admb("main", verbose = TRUE) # you get a warning message here but you can ignore it
  setwd(here::here())

  # move files to results folder
  file.copy(here::here(year, folder, "proj", "model",
                       c("bigfile.out", "F_profile.out", "means.out", "percentdb.out", "percentiles.out")),
            here::here(year, folder, "proj", "author_f"), overwrite = TRUE)

  # cleanup
  file.remove(here::here(year, folder, "proj", "model",
                         c("bigfile.out", "F_profile.out", "means.out", "percentdb.out", "percentiles.out")))

  # get mean values
  read.delim(here::here(year, folder, "proj", "author_f", "bigfile.out"), sep="", header=T) %>%
    tidytable::rename(Year=Yr, Alt = Alternative, Stock = Spp, Total_Biom = Tot_biom) %>%
    tidytable::summarise(tidytable::across(c(ABC, OFL, Catch, SSB, F, Total_Biom), mean),
                         .by = c(Alt, Stock, Year)) %>%
    vroom::vroom_write(here::here(year, folder, "proj", "author_f", "bigsum.csv"), ",")

  # results
  # utility function for compiling results
  mscen_tbl <- function(maxf, authf, start, end, catch = FALSE){
    if(catch == FALSE){
      st = grep(start, maxf)
      ed = grep(end, maxf)
    } else{
      st = grep(start, maxf)[which(grep(start, maxf)%%2 == 1)]
      ed = grep(end, maxf)
    }

    # maxf scenario
    data.frame(stringr::str_split_fixed(maxf[(st[1] + 2):(ed[1] - 2)], " ", n = 14)) %>%
      tidytable::select(year = X1, maxf = X7) %>%
      # author f scenario
      tidytable::left_join(data.frame(stringr::str_split_fixed(authf[(st[1] + 2):(ed[1] - 2)], " ", n = 14)) %>%
                             tidytable::select(year = X1, authf = X7)) %>%
      # half maxf scenario
      tidytable::left_join(data.frame(stringr::str_split_fixed(maxf[(st[3] + 2):(ed[3] - 2)], " ", n = 14)) %>%
                             tidytable::select(year = X1, half_maxf = X7)) %>%
      # 5-year avg f scenario
      tidytable::left_join(data.frame(stringr::str_split_fixed(maxf[(st[4] + 2):(ed[4] - 2)], " ", n = 14)) %>%
                             tidytable::select(year = X1, avg5f = X7)) %>%
      # no fishing scenario
      tidytable::left_join(data.frame(stringr::str_split_fixed(maxf[(st[5] + 2):(ed[5] - 2)], " ", n = 14)) %>%
                             tidytable::select(year = X1, nof = X7)) %>%
      # overfishing scenario
      tidytable::left_join(data.frame(stringr::str_split_fixed(maxf[(st[6] + 2):(ed[6] - 2)], " ", n = 14)) %>%
                             tidytable::select(year = X1, overf = X7)) %>%
      # approaching overfishing scenario
      tidytable::left_join(data.frame(stringr::str_split_fixed(maxf[(st[7] + 2):(ed[7] - 2)], " ", n = 14)) %>%
                             tidytable::select(year = X1, appoverf = X7)) %>%
      tidytable::mutate(tidytable::across(tidytable::everything(), as.numeric))

  }

  # get results data
  authf <- readLines(here::here(year, folder, "proj", "author_f", "percentiles.out"))
  auth_bs = vroom::vroom(here::here(year, folder, "proj", "author_f", "bigsum.csv")) %>%
    tidytable::rename_with(tolower)
  nat_mort = readLines(here::here(year, folder, "proj", "model", "data", paste0(species, ".dat")))
  nat_mort = round(as.numeric(stringr::str_split(nat_mort[grep("Natural_Mortality", nat_mort):
                                                            grep("Natural_Mortality", nat_mort)+1], " ")[[1]][2]), digits = 3)

  # spawning biomass
  mscen_tbl(maxf, authf, start = "Spawning_Biomass", end = "Fishing_mortality") %>%
    tidytable::mutate(maxf = 1000 * maxf,
                      authf = 1000 * authf,
                      half_maxf = 1000 * half_maxf,
                      avg5f = 1000 * avg5f,
                      nof = 1000 * nof,
                      overf = 1000 * overf,
                      appoverf = 1000 * appoverf) %>%
  vroom::vroom_write(here::here(year, folder, "processed", "mscen_ssb.csv"), delim = ",")

  # fishing mortality
  mscen_tbl(maxf, authf, start = "Fishing_mortality", end = "Total_Biomass") %>%
    tidytable::mutate(maxf = round(maxf, digits = 3),
                      authf = round(authf, digits = 3),
                      half_maxf = round(half_maxf, digits = 3),
                      avg5f = round(avg5f, digits = 3),
                      nof = round(nof, digits = 3),
                      overf = round(overf, digits = 3),
                      appoverf = round(appoverf, digits = 3)) %>%
  vroom::vroom_write(here::here(year, folder, "processed", "mscen_f.csv"), delim = ",")

  # yield
  mscen_tbl(maxf, authf, start = "Catch", end = "Spawning_Biomass", catch = TRUE) %>%
    tidytable::mutate(maxf = 1000 * maxf,
                      authf = 1000 * authf,
                      half_maxf = 1000 * half_maxf,
                      avg5f = 1000 * avg5f,
                      nof = 1000 * nof,
                      overf = 1000 * overf,
                      appoverf = 1000 * appoverf)  %>%
  vroom::vroom_write(here::here(year, folder, "processed", "mscen_yld.csv"), delim = ",")

  # get executive summary table

# projected total biomass
data.frame(stringr::str_split_fixed(authf[(grep("Total_Biomass", authf)[1] + 2):
                                            (grep("Catch", authf)[3] - 2)], " ", n = 14)) %>%
  tidytable::select(year = X1, tb_proj = X7) %>%
  # tidytable::filter(year %in% c(max(yr) + 1, max(yr) + 2)) %>%
  #projected spawning biomass
  tidytable::left_join(
    data.frame(stringr::str_split_fixed(authf[(grep("Spawning_Biomass", authf)[1] + 2):
                                                (grep("Fishing_mortality", authf)[1] - 2)], " ", n = 14)) %>%
      tidytable::select(year = X1, ssb_proj = X7)) %>%
  # B0, 40, 35
  tidytable::left_join(data.frame(stringr::str_split_fixed(authf[(grep("SB0", authf) + 1)], " ", n = 6)) %>%
                         tidytable::select(sb0 = X1, sb40 = X2, sb35 = X3) %>%
                         tidytable::slice(rep(1:n(), each = 2)) %>%
                         tidytable::mutate(year = as.character(c(max(yr)+1, max(yr+2))))) %>%
  tidytable::mutate(tidytable::across(tidytable::everything(), as.numeric),
                    tidytable::across(c(tidytable::where(is.numeric), -year), ~round(.x * 1000))) %>%
  tidytable::drop_na() %>%
  # f_ofl and f_abc
  tidytable::left_join(data.frame(stringr::str_split_fixed(authf[(grep("Fishing_mortality", authf)[1]+2):
                                                                   (grep("Fishing_mortality", authf)[1]+14)], " ", n = 14)) %>%
                         tidytable::mutate(tidytable::across(tidytable::everything(), as.numeric)) %>%
                         tidytable::select(year = X1, f_ofl = X5)) %>%
  tidytable::left_join(data.frame(stringr::str_split_fixed(authf[(grep("Fishing_mortality", authf)[1]+2):
                                                                   (grep("Fishing_mortality", authf)[1]+14)], " ", n = 14)) %>%
                         tidytable::mutate(tidytable::across(tidytable::everything(), as.numeric)) %>%
                         tidytable::select(year = X1, maxf_abc = X4)) %>%
  tidytable::mutate(f_ofl = round(ifelse(ssb_proj < sb40, (ssb_proj / sb40 - 0.05) / 0.95 * f_ofl, f_ofl), 3),
                    maxf_abc = round(ifelse(ssb_proj < sb40, (ssb_proj / sb40 - 0.05) / 0.95 * maxf_abc, maxf_abc), 3),
                    f_abc = maxf_abc,
                    tier = tidytable::case_when(ssb_proj > sb40 ~ "3a",
                                                ssb_proj < sb40 ~ "3b")) %>%
  # ofl and abc
  tidytable::left_join(auth_bs %>%
                         tidytable::filter(alt == 1) %>%
                         tidytable::select(year, ofl, abc) %>%
                         tidytable::filter(year %in% c(max(yr) + 1, max(yr) + 2)) %>%
                         tidytable::mutate(ofl = round(ofl * 1000),
                                           maxabc = round(abc * 1000),
                                           abc = maxabc)) %>%
  # natural mortality
  tidytable::left_join(data.frame(nat_mort) %>%
                         tidytable::slice(rep(1:n(), each = 2)) %>%
                         tidytable::rename(m = nat_mort) %>%
                         tidytable::mutate(year = c(max(yr) + 1, max(yr) + 2),
                                           blank = NA,
                                           Status = c(max(yr), max(yr + 1)),
                                           Overfishing = NA,
                                           Overfished = NA,
                                           `Approaching overfished` = NA)) %>%
  tidytable::select(`M (natural mortality)` = m,
                    Tier = tier,
                    tb_proj,
                    `Projected female spawning biomass (t)` = ssb_proj,
                    `B100%` = sb0,
                    `B40%` = sb40,
                    `B35%` = sb35,
                    FOFL = f_ofl,
                    maxFABC = maxf_abc,
                    FABC = f_abc,
                    `OFL (t)` = ofl,
                    `maxABC (t)` = maxabc,
                    `ABC (t)` = maxabc,
                    blank,
                    Status,
                    Overfishing,
                    Overfished,
                    `Approaching overfished`) %>%
  t() %>%
  as.data.frame() %>%
  tibble::rownames_to_column("item")  %>%
  tidytable::rename(y3 = V1, y4 = V2) %>%
  tidytable::mutate(item = ifelse(item == "tb_proj",
                                  paste0("Projected total (age ", rec_age,"+) biomass (t)"),
                                  item)) -> exec_summ


# if old exec table exists join it to this one
if(file.exists(here::here(year, "base", "processed", "exec_summ.csv"))) {
  vroom::vroom(here::here(year, "base", "processed", "exec_summ.csv")) %>%
    tidytable::select(item, y1 = y3, y2 = y4) %>%
    tidytable::left_join(exec_summ) -> exec_summ
}

vroom::vroom_write(exec_summ, here::here(year, folder, "processed", "exec_summ.csv"), ",")
}
