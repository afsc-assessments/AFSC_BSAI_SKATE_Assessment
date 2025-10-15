#' ALASKA PROJECTION SCENARIOS FOR STOCK SYNTHESIS 3 FOR TIER 3 MODELS WITH PARALLEL COMPUTING
#' Version JAN 11, 2024
#' Created by Steve Barbeaux E-mail: steve.barbeaux@noaa.gov  Phone: (206) 729-0871 3
#' Redeveloped without markdown option by P Hulson
#' 
#' @param dir is the model directory (default = NULL)
#' @param cyr is the model current year (default = NULL)
#' @param syr is the start year for the model (default = 1977)
#' @param sexes is the number of sexes in model (default = 1) 
#' @param fleets the fleet number in SS for your fisheries (default = c(1:3) for GOA, would be 1 for EBS/AI)
#' @param Scenario2 indicates whether you wish to have a different catch for scenario 2 (1 = FmaxABC,2 = F as s2_F, 3 = specified catch from a 
#'            formatted csv saved in the root directory named 'Scenario2_catch.csv', must have an entry for each year, season, and fleet for the years 
#'            that differ from Fmaxabc with columns "Year,Seas,Fleet,Catch_or_F" (default = 1)
#' @param s2_F F for scenario 2 (default = 0.4)
#' @param s4_F is the F for scenario 4, defaults to 0.75, should be 0.65 for some species (check your requirements)
#' @param do_fig whether to plot figures (default = TRUE)
#'
run_ss3_mgmnt_scen <- function(dir = NULL,
                               cyr = NULL,
                               syr = 1977,
                               sexes = 1,
                               fleets = c(1:3), 
                               Scenario2 = 1, 
                               s2_F = 0.4,
                               s4_F = 0.75,
                               do_fig = TRUE) {

  # Check if the directory for management scenrios exists, if it doesn't, create it
  if (!dir.exists(here::here(dir, "mscen"))) {
    dir.create(here::here(dir, "mscen"), recursive = TRUE)
  }
  
  # get model executable name
  exe_name <- ss3_exename(dir)

	# set up and run scenario 1 ----
	
	# set up scenario 1 model folder
	R.utils::copyDirectory(dir, here::here(dir, "mscen", "scenario_1"), recursive = FALSE)
	# read forecast file and set scenario 1 params
	scenario_1 <- r4ss::SS_readforecast(file = here::here(dir, "forecast.ss"),
	                                    verbose = FALSE)
	scenario_1$Btarget   <- 0.4
	scenario_1$SPRtarget <- 0.4
	scenario_1$Flimitfraction <- 1.0
	r4ss::SS_writeforecast(scenario_1, 
	                       dir = here::here(dir, "mscen", "scenario_1"), 
	                       file = "forecast.ss", 
	                       writeAll = TRUE, 
	                       overwrite = TRUE,
	                       verbose = FALSE)
	# get number of forecast years (for tables later)
	fcasty <- scenario_1$Nforecastyrs
	# set starter to start at params
	mdl_starter <- r4ss::SS_readstarter(file = here::here(dir, "mscen", "scenario_1", "starter.ss"),
	                                    verbose = FALSE)
	mdl_starter$init_values_src = 1
	r4ss::SS_writestarter(mdl_starter, 
	                      dir = here::here(dir, "mscen", "scenario_1"),
	                      overwrite = TRUE,
	                      verbose = FALSE)
	# get results from report file for base model (for tables later)
	rep1 <- r4ss::SS_output(dir = dir,
	                        verbose = FALSE,
	                        printstats = FALSE)
	# run scenario 1 first to get the forecast parameters for scenarios 6 and 7
	r4ss::run(dir = here::here(dir, "mscen", "scenario_1"),
	          exe = exe_name,
	          skipfinished = FALSE, 
	          verbose = FALSE,
	          show_in_console = FALSE)

	# run models in parallel ---- 
	# Define the list of remaining scenarios
	scenarios <- c("scenario_2", "scenario_3", "scenario_4", "scenario_5", "scenario_6", "scenario_7", "scenario_8")
		
	# Get the number of available cores
	num_cores <- parallel::detectCores()
	if(num_cores > length(scenarios)) num_cores = length(scenarios)
	
	# Set the number of cores to be used for parallel computing
	doParallel::registerDoParallel(cores = num_cores)

	# Run scenarios
	foreach::foreach(scenario = scenarios) %dopar% {

	  # read forecast file
	  scenario_C <- r4ss::SS_readforecast(file = here::here(dir, "mscen", "scenario_1", "forecast.ss"),
	                                      verbose = FALSE)
	  
	  # Create a directory for the scenario
	  dir.create(here::here(dir, "mscen", scenario), recursive = TRUE, showWarnings = FALSE)
	  R.utils::copyDirectory(dir, here::here(dir, "mscen", scenario), recursive = FALSE)
	  
	  # Modify the scenario attributes based on the scenario number
	  if (scenario == 'scenario_2') {
	    # scenario 2 options
	    if(Scenario2 == 2){
	      # set F at specified s2_F value
	      Fyear <- paste0("F_", cyr + 1)
	      years <- seq(cyr + 1, cyr + 15, 1)
	      f2 <- s2_F * data.table::data.table(rep1$derived_quants)[Label == Fyear]$Value
	      newF <- data.table::data.table(Year = years, Seas = 1, Fleet = fleets, F = f2)
	      scenario_C$Forecast <- 4
	      scenario_C$BforconstantF <- 0.001  ## cluge to get rid of control rule of ave. F
	      scenario_C$BfornoF <- 0.0001      ## cluge to get rid of control rule scaling of ave. F
	      scenario_C$InputBasis <- 99
	      scenario_C$ForeCatch <- newF
	    }
	    if(Scenario2 == 3){
	      # read in specified future catch
	      scenario_C$ForeCatch <- read.csv("Scenario2_catch.csv", header = TRUE)
	    }
	  } else if (scenario == 'scenario_3') {
	    # scenario 3
	    scenario_C$Forecast <- 4
	    scenario_C$BforconstantF <- 0.001  ## cluge to get rid of control rule of ave. F
	    scenario_C$BfornoF <- 0.0001      ## cluge to get rid of control rule scaling of ave. F
	    scenario_C$Fcast_years[c(3,4)] <- c(cyr - 5, cyr - 1)
	  } else if (scenario == 'scenario_4') {
	    # scenario 4
	    scenario_C$Flimitfraction <- s4_F
	  } else if (scenario == 'scenario_5') {
	    # scenario 5
	    catch <- expand.grid(Year = c((cyr + 1):(cyr + fcasty)), Seas = 1, Fleet = fleets, Catch_or_F = 0)
	    names(catch) <- names(scenario_C$ForeCatch)
	    scenario_C$ForeCatch <- rbind(scenario_C$ForeCatch,catch)
	  } else if (scenario == 'scenario_6') {
	    # scenario 6
	    scenario_C$Btarget <- 0.35
	    scenario_C$SPRtarget <- 0.35
	    scenario_C$Flimitfraction <- 1.0
	  } else if (scenario == 'scenario_7') {
	    # scenario 7
	    scenario_C$Btarget <- 0.35
	    scenario_C$SPRtarget <- 0.35
	    scenario_C$Flimitfraction <- 1.0
	    x <- r4ss::SS_output(dir = here::here(dir, "mscen", "scenario_1"))
	    scenario_C$ForeCatch <- r4ss::SS_ForeCatch(x, yrs = cyr:(cyr + 2))
	  } else if (scenario == 'scenario_8') {
	    # scenario 8
	    scenario_C$Btarget   <- 0.35
	    scenario_C$SPRtarget <- 0.35
	    scenario_C$Flimitfraction <- 1.0
	    x <- r4ss::SS_output(dir = here::here(dir, "mscen", "scenario_1"))
	    scenario_C$ForeCatch <- r4ss::SS_ForeCatch(x, yrs = cyr:(cyr + 1))
	  }
	  # Write the modified scenario to the scenario directory
	  r4ss::SS_writeforecast(scenario_C, 
	                         dir = here::here(dir, "mscen", scenario), 
	                         file = "forecast.ss", 
	                         writeAll = TRUE, 
	                         overwrite = TRUE,
	                         verbose = FALSE)
	  # set starter to start at params
	  mdl_starter <- r4ss::SS_readstarter(file = here::here(dir, "mscen", scenario, "starter.ss"),
	                                      verbose = FALSE)
	  mdl_starter$init_values_src = 1
	  r4ss::SS_writestarter(mdl_starter, 
	                        dir = here::here(dir, "mscen", scenario),
	                        overwrite = TRUE,
	                        verbose = FALSE)
	  
	  # Run the scenario
	  r4ss::run(dir = here::here(dir, "mscen", scenario),
	            exe = exe_name,
	            skipfinished = FALSE, 
	            verbose = FALSE,
	            show_in_console = FALSE)
	}
	
	# Stop parallel computing
	doParallel::stopImplicitCluster()

	# Get the output from each scenario ----
	
	# determine number of sexes to divide ssb by
	if(sexes == 1) {sex = 2} else sex = 1
	
	# get model outputs
	scenarios <- c("scenario_1", scenarios)
	mods1 <- r4ss::SSgetoutput(dirvec = here::here(dir, "mscen", scenarios),
	                           verbose = FALSE)

	# kluge to deal with column name change between versions
	kluge1 <- data.table(mods1[[1]]$sprseries)$F_report[1]
 
	# Calculate the year index for the summary statistics
	eyr <- cyr + fcasty
	yr1 <- eyr - syr + 3

	# Calculate summary statistics for each scenario
	summ <- lapply(seq_along(mods1), 
	               function(i) {
	                 mod <- mods1[[i]]
	                 Yrs <- syr:eyr
	                 TOT <- data.table(mod$timeseries)[Yr %in% Yrs]$Bio_all
	                 SUMM <- data.table(mod$timeseries)[Yr %in% Yrs]$Bio_smry
	                 SSB <- data.table(mod$timeseries)[Yr %in% Yrs]$SpawnBio / sex
	                 std <- data.table(mod$stdtable)[name %like% "SSB"][3:yr1, ]$std / sex
	                 if(!is.null(kluge1)){
	                   F <- data.table(mod$sprseries)[Yr %in% Yrs]$F_report
	                 } else { 
	                   F <- data.table(mod$sprseries)[Yr %in% Yrs]$F_std
	                 }
	                 Catch <- data.table(mod$sprseries)[Yr %in% Yrs]$Enc_Catch
	                 SSB_unfished <- data.table(mod$derived_quants)[Label == "SSB_unfished"]$Value / sex
	                 model <- scenarios[i]
	                 data.table(Yr = Yrs, TOT = TOT, SUMM = SUMM, SSB = SSB, std = std, F = F, Catch = Catch, SSB_unfished = SSB_unfished, model = model)
	               })

	# Calculate catch projections for each scenario
	Pcatch <- lapply(seq_along(mods1), 
	                 function(i) {
	                   mod <- mods1[[i]]
	                   Yrs <- (cyr + 1):eyr
	                   Catch <- data.table(mod$sprseries)[Yr %in% Yrs]$Enc_Catch
	                   Catch_std <- data.table(mod$stdtable)[name %like% "ForeCatch_"]$std
	                   model <- scenarios[i]
	                   data.table(Yr = Yrs, Catch = Catch, Catch_std = Catch_std, model = model)
	                 })

	# Calculate 2-year projections for catch and F
	SB100 <- summ[[1]][Yr == cyr + 1]$SSB_unfished
	SB40 <- data.table(mods1[[1]]$derived_quants)[Label == "SSB_SPR"]$Value / sex
	SB35 <- data.table(mods1[[8]]$derived_quants)[Label == "SSB_SPR"]$Value / sex
	F40_1 <- summ[[1]][Yr == cyr + 1]$F
	F35_1 <- summ[[6]][Yr == cyr + 1]$F
	catchABC_1 <- Pcatch[[1]][Yr == cyr + 1]$Catch
	catchOFL_1 <- Pcatch[[6]][Yr == cyr + 1]$Catch
	F40_2 <- summ[[1]][Yr == cyr + 2]$F
	F35_2 <- summ[[8]][Yr == cyr + 2]$F
	catchABC_2 <- Pcatch[[1]][Yr == cyr + 2]$Catch
	catchOFL_2 <- Pcatch[[8]][Yr == cyr + 2]$Catch
	SSB_1 <- summ[[1]][Yr == cyr + 1]$SSB
	SSB_2 <- summ[[1]][Yr == cyr + 2]$SSB
	
	Two_Year <- data.table(Yr = c((cyr + 1):(cyr + 2)),
	                       SSB = c(SSB_1, SSB_2),
	                       SSB_PER = c(SSB_1 / SB100, SSB_2 / SB100),
	                       SB100 = rep(SB100, 2),
	                       SB40 = rep(SB40, 2),
	                       SB35 = rep(SB35, 2),
	                       F40 = c(F40_1, F40_2),
	                       F35 = c(F35_1, F35_2),
	                       C_ABC = c(catchABC_1, catchABC_2),
	                       C_OFL = c(catchOFL_1, catchOFL_2))
	
	# Combine summary statistics and catch projections into tables
	summ   <- do.call(rbind, summ)
	Pcatch <- do.call(rbind, Pcatch)
	summ   <- summ[model != "scenario_8"]
	Pcatch <- Pcatch[model != "scenario_8"]
	
	# Create output list with SSB, CATCH, and Two_year tables
	output <- list(SSB = summ,
	               CATCH = Pcatch,
	               Two_year = Two_Year)

	# Create scenario tables for the document
	BC <- list(Catch = dcast(output$SSB[Yr >= cyr], Yr ~ model, value.var = "Catch"),
	           F = dcast(output$SSB[Yr >= cyr], Yr ~ model, value.var = "F"),
	           SSB = dcast(output$SSB[Yr >= cyr], Yr ~ model, value.var = "SSB"))
	output$Tables <- BC
	
	# plot figures (if desired) ----
	if(do_fig){
	  
	  SSB_unfished <- data.table(mods1[[1]]$derived_quants)[Label == "SSB_unfished"]$Value / sex
	  
	  # Calculate upper and lower confidence intervals for SSB
	  summ2 <- data.table(Yr = c(syr:eyr),
	                      TOT = 0,
	                      SUMM = 0,
	                      SSB = SB40,
	                      std = 0,
	                      F = 0,
	                      Catch = 0,
	                      SSB_unfished = SSB_unfished,
	                      model = "SSB40%")
	  
	  summ2 <- rbind(summ2,
	                 data.table(Yr = c(syr:eyr),
	                            TOT = 0,
	                            SUMM = 0,
	                            SSB = SB35,
	                            std = 0,
	                            F = 0,
	                            Catch = 0,
	                            SSB_unfished = SSB_unfished,
	                            model = "SSB35%"),
	                 data.table(Yr = c(syr:eyr),
	                            TOT = 0,
	                            SUMM = 0,
	                            SSB = SSB_unfished * 0.2,
	                            std = 0,
	                            F = 0,
	                            Catch = 0,
	                            SSB_unfished = SSB_unfished,
	                            model = "SSB20%"),
	                 summ)
	  
	  summ2$model <- factor(summ2$model, levels = unique(summ2$model))
	  summ2$UCI <- summ2$SSB + 1.96 * summ2$std
	  summ2$LCI <- summ2$SSB - 1.96 * summ2$std
	  summ2[LCI < 0]$LCI <- 0
	  
	  # Calculate upper and lower confidence intervals for catch projections
	  Pcatch2 <- data.table(Yr = c(cyr + 1:eyr),
	                        Catch = Pcatch[model == "scenario_1" & Yr == eyr]$Catch,
	                        Catch_std = 0,
	                        model = "Catch Fmaxabc")
	  
	  Pcatch2 <- rbind(Pcatch2,
	                   data.table(Yr = c(cyr + 1:eyr),
	                              Catch = Pcatch[model == "scenario_6" & Yr == eyr]$Catch,
	                              Catch_std = 0,
	                              model = "Catch Fofl"),
	                   Pcatch)
	  
	  Pcatch2$model <- factor(Pcatch2$model, levels = unique(Pcatch2$model))
	  Pcatch2$UCI <- Pcatch2$Catch + 1.96 * Pcatch2$Catch_std
	  Pcatch2$LCI <- Pcatch2$Catch - 1.96 * Pcatch2$Catch_std
	  Pcatch2[LCI < 0]$LCI <- 0
	  
	  ## SSB_Figures ----
	  # Define the scenarios to plot
	  SSB_reference <- summ2[!model %like% 'scenario_']
	  # Create a list to store the plots
	  Figs_SSB <- list()
	  
	  Figs_SSB[['ALL']] <- ggplot(summ2[model %in% unique(summ2$model)[1:10]], aes(x = Yr, y = SSB, color = model, linetype = model)) +
	    geom_line(size = 1.2) +
	    lims(y = c(0, max(summ2$UCI)), x = c(cyr - 1, eyr)) +
	    theme_bw(base_size = 12) +
	    labs(x = "Year", y = "Spawning biomass (t)", title = "Projections") +
	    scale_color_manual(values = c("darkgreen", "orange", "red", 2:6, 8, 9), name = "Scenarios") +
	    scale_linetype_manual(values = c(rep(1, 3), 2:8), name = "Scenarios") +
	    theme(legend.position = "bottom", 
	          legend.title = element_text(face = "bold"), 
	          legend.text = element_text(size = 10), 
	          plot.title = element_text(face = "bold", size = 14),
	          axis.title = element_text(face = "bold", size = 12), 
	          axis.text = element_text(size = 10))
	  
	  # Iterate over each individual scenario and create the plot
	  scenarios_P <- unique(summ$model)
	  scenarios_P2 <- as.character(unique(summ2$model)[1:3])
	  
	  pcol <- c("darkgreen", "orange", "red", 2:6, 8, 9)
	  
	  for(i in 1:length(scenarios_P)){
	    scenarios_P3 <- c(scenarios_P2, scenarios_P[i])
	    plot_data <- summ2[model %in% scenarios_P3]
	    
	    plot <- ggplot(plot_data, aes(x = Yr, y = SSB, linewidth = model, fill = model, color = model, linetype = model)) +
	      geom_line() +
	      lims(y = c(0, max(summ2$UCI)), x = c(cyr - 1, eyr)) +
	      geom_ribbon(aes(ymin = LCI, ymax = UCI), alpha = 0.2, color = NA) +
	      theme_bw(base_size = 12) +
	      labs(x = "Year", y = "Spawning biomass (t)", title = "Projections") +
	      scale_linetype_manual(values = c(rep(1, 3), (i + 1)), name = "Scenarios") +
	      scale_color_manual(values = c(pcol[1:3], pcol[i + 3]), name = "Scenarios") +
	      scale_fill_manual(values = c(pcol[1:3], pcol[i + 3]), name = "Scenarios") +
	      scale_linewidth_manual(values = c(rep(1.5, 3), 1.2), name = "Scenarios") +
	      theme(legend.position = "bottom", 
	            legend.title = element_text(face = "bold"), 
	            legend.text = element_text(size = 10), 
	            plot.title = element_text(face = "bold", size = 14),
	            axis.title = element_text(face = "bold", size = 12),
	            axis.text = element_text(size = 10))
	    
	    Figs_SSB[[scenarios_P[i]]] <- plot
	  }
	  ## Catch Figures ----
	  # Define the scenarios to plot
	  scenarios <- unique(Pcatch2$model)[1:9]
	  
	  # Create a list to store the plots
	  Figs_Catch <- list()
	  
	  Figs_Catch[['ALL']]<- ggplot(Pcatch2[model %in% unique(Pcatch2$model)[1:9]], aes(x = Yr, y = Catch, linewidth = model, color = model, linetype = model)) +
	    geom_line() + 
	    lims(y = c(0, max(Pcatch2$UCI)), x = c((cyr + 1), eyr)) +
	    theme_bw(base_size = 12) +
	    labs(x = "Year", y = "Catch (t)", title = "Projections") +
	    scale_linetype_manual(values = c(rep(1, 2), 2:8), name = "Scenarios") +
	    scale_color_manual(values = c("darkgreen", "orange", 2:6, 8, 9),name = "Scenarios") +
	    scale_linewidth_manual(values = c(rep(1.5, 2), rep(1, 7)), name = "Scenarios") +
	    theme(legend.position = "bottom", 
	          legend.title = element_text(face = "bold"), 
	          legend.text = element_text(size = 10),
	          plot.title = element_text(face = "bold", size = 14),
	          axis.title = element_text(face = "bold", size = 12),
	          axis.text = element_text(size = 10))
	  
	  # Create the remaining plots
	  scenarios_P2 <- as.character(unique(Pcatch2$model)[1:2])
	  
	  for(i in 1:length(scenarios_P)){
	    scenarios_P3 <- c(scenarios_P2, scenarios_P[i])
	    plot_data <- Pcatch2[model %in% scenarios_P3]
	    
	    plot <- ggplot(plot_data, aes(x = Yr, y = Catch, linewidth = model, fill = model, color = model, linetype = model)) +
	      geom_line() + 
	      lims(y = c(0, max(Pcatch2$UCI)), x = c((cyr + 1), eyr)) +
	      geom_ribbon(aes(ymin = LCI, ymax = UCI), alpha = 0.2, color = NA) +
	      theme_bw(base_size = 12) +
	      labs(x = "Year", y = "Catch (t)", title = "Projections") +
	      scale_linetype_manual(values = c(rep(1, 2), (i + 1)), name = "Scenarios") +
	      scale_color_manual(values = c(pcol[1:2], pcol[i + 3]), name = "Scenarios") +
	      scale_fill_manual(values = c(pcol[1:2], pcol[i + 3]), name = "Scenarios") +
	      scale_linewidth_manual(values = c(rep(1.5, 2), 1), name = "Scenarios") +
	      labs(y = "Catch (t)", x = "Year", title = "Projections") +
	      theme(legend.position = "bottom",
	            legend.title = element_text(face = "bold"), 
	            legend.text = element_text(size = 10),
	            plot.title = element_text(face = "bold", size = 14),
	            axis.title = element_text(face = "bold", size = 12), 
	            axis.text = element_text(size = 10))
	    
	    Figs_Catch[[scenarios_P[i]]] <- plot
	  }
	  
	  output$FIGS <- list(Figs_SSB, Figs_Catch)
	}
	
	return(output)
}
