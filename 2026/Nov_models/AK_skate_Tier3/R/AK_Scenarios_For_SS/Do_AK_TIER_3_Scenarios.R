
## ALASKA PROJECTION SCENARIOS FOR STOCK SYNTHESIS 3 FOR TIER 3 MODELS WITH PARALLEL COMPUTING
## Version Oct 22, 2026
## Created by Steve Barbeaux E-mail: steve.barbeaux@noaa.gov  Phone: (206) 729-0871 
## 
##
## In the starter.ss file you should change it to read from the converged .par file 
##   1 # 0=use init values in control file; 1=use ss.par
## 
## Assumes you already have the forecast parameters already specified appropriately in the forecast.ss for scenario 1, 
## Make sure there is no catch or F already specified in the forecast file.
## You will also need to make a seperate folder named PROJ and copy your final model run into the folder, 
## this folder will be the folder you point to using DIR
##
#' @DIR is the model directory (see above)
#' @CYR is the model current year, 
#' @SYR is the start year for the model, 
#' @SEXES is the number of sexes in model, 
#' @fleets= the fleet number in SS for your fisheries,
#' @Scenario2 indicates whether you wish to have a different catch for scenario 2 (1= FmaxABC,2= F as S2_F, 3 = specified catch from a 
##            formatted csv saved in the root directory named 'Scenario2_catch.csv', must have an entry for each year, season, and fleet for the years 
##            that differ from Fmaxabc with columns "Year,Seas,Fleet,Catch_or_F"
#' @s4_F is the F for scenario 4, defaults to 0.75, should be 0.65 for some species check your requirments
#' @Fyr_ts is the scenario for which you want to base the 5-year projection table.
#' @do_fig whether to plot figures
#' @init_dir is the director of the AK_SCENARIOS_FOR_SS files.
##
#DIR = "Model_26.1_750/PROJ"; CYR = 2026; SYR = 1977;  SEXES = 1; FLEETS = c(1,2); Scenario2 = 1; S2_F = 0.4; s4_F = 0.75; Fyr_ts=2 ;do_fig = TRUE;init_dir="C:/Users/steve.barbeaux/Work/GitHub/AK_Scenarios_For_SS"


Do_AK_TIER_3_Scenarios <- function(DIR = "Model_24.1.25/PROJ", CYR = 2025, SYR = 1977,  SEXES = 1, FLEETS = 1, Scenario2 = 1, 
				   S2_F = 0.4, s4_F = 0.75, Fyr_ts=2, do_fig = TRUE, init_dir="C:/Users/steve.barbeaux/Work/GitHub/AK_Scenarios_For_SS") {

# Check if the specified directory exists
  	if (!dir.exists(DIR)) {
    	stop("Error: The specified directory does not exist.")
  	}

# Get the operating system information
	# Get the operating system information
	os_info <- Sys.info()

# Check the operating system
	supported_os <- c("Windows", "Darwin")
	if (os_info["sysname"] %in% supported_os) {
  # Code to execute if the operating system is supported
  	print(paste(os_info["sysname"], "operating system detected and supported."))
	} else {
  # Code to execute if the operating system is unknown or unsupported
  	print("Unknown or unsupported operating system.")
  	stop()
	}
			

 ## Specify the libraries to load
	libraries <- c("r4ss", "data.table","dplyr","flextable", "ggplot2", "R.utils", "parallel", "doParallel", "foreach","pdftools")

# Loop through the libraries and load them if they don't exist
	for (lib in libraries) {
  		if (!require(lib, character.only = TRUE)) {
    		install.packages(lib)  # Install the library if it is not already installed
    		library(lib, character.only = TRUE)  # Load the library
  		}
	}


  parent_dir<-getwd()
	.DIR <- DIR
    #setwd(DIR) ## folder with converged modelinit
   scenario_1 <- SS_readforecast(file = file.path(DIR,"forecast.ss"))
   
   rep1<-SS_output(dir=DIR)
#   # Define the list of scenarios

 	copyDirectory(DIR,file.path(DIR,"scenario_1"),recursive=FALSE)
 	scenario_1$Btarget   <- 0.4
 	scenario_1$SPRtarget <- 0.4
 	scenario_1$Flimitfraction <- 1.0
 	SS_writeforecast(scenario_1, dir = file.path(DIR,"scenario_1"), file = "forecast.ss", writeAll = TRUE, overwrite = TRUE)
    
	FCASTY<-scenario_1$Nforecastyrs

## find the SS executable
	os_info <- Sys.info()
	
	executables <- list(
	  		"Windows" = c("ss.exe", "ss3.exe", "ss3_win.exe"),
	  		"Darwin" = c("ss_osx")
		)

	setwd(file.path(DIR,"scenario_1"))

# Check the operating system
		if (os_info["sysname"] %in% names(executables)) {
  # Get the list of executables for the detected operating system
  			os_executables <- executables[[os_info["sysname"]]]
  
  # Check if any of the executables exist
  			existing_executables <- os_executables[file.exists(os_executables)]
}
   

## have to run scenario 1 first to get the forecast parameters for scenarios 6 and 7
	  
  			if (length(existing_executables) > 0) {
    # Run the first existing executable
    			r4ss::run(exe = existing_executables[1], skipfinished = FALSE, verbose = FALSE)
  			} else {
    			print("No executable found for the operating system.")
    			stop()
  			}
		  

#  r4ss::run(exe = "ss", skipfinished = FALSE, verbose = TRUE)

## setting up parallel computing    
	# Get the number of available cores
	
	setwd(parent_dir)

	num_cores <- detectCores()
	if(num_cores>11)num_cores=11
	
# Set the number of cores to be used for parallel computing

	registerDoParallel(cores = num_cores)

# Define the list of scenarios
	scenarios <- c("scenario_2", "scenario_3", "scenario_4", "scenario_5", "scenario_6", "scenario_7", "scenario_8", "scenario_9", "scenario_10", "scenario_11")

# Define 5-year scenario
scenario_F <- paste0("scenario_",Fyr_ts)

# Create a function to run a scenario
	run_scenario <- function(scenario) {
		library(r4ss)
		library(R.utils)
		
		dir1 <- get(".DIR", envir = parent.frame())
  		
		scenario_C <- SS_readforecast(file = file.path(dir1,"scenario_1","forecast.ss"))

  # Create a directory for the scenario
  		dir_name <- file.path(dir1, scenario)
  		dir.create(dir_name, recursive = TRUE, showWarnings = FALSE)
  		copyDirectory(dir1,dir_name,recursive=FALSE)
  
  # Modify the scenario attributes based on the scenario number
  		if (scenario == 'scenario_2') {
	
    		if(Scenario2==2){

    			   Fyear<-paste0("F_",CYR+1)
    			   years<-seq(CYR+1,CYR+15,1)


    			   f2<-S2_F*data.table::data.table(rep1$derived_quants)[Label==Fyear]$Value
    			   newF<-data.table::data.table(Year=years,Seas=1,Fleet=1,F=f2)

    				scenario_C$Forecast<-4
    				scenario_C$BforconstantF<-0.001  ## cluge to get rid of control rule of ave. F
    				scenario_C$BfornoF<-0.0001      ## cluge to get rid of control rule scaling of ave. F
            
            scenario_C$InputBasis<-99
            scenario_C$ForeCatch<-newF

    		}
			if(Scenario2==3){
				scenario_C$ForeCatch <- read.csv("Scenario2_catch.csv",header=T)
    		}
  		} else if (scenario == 'scenario_3') {
    		scenario_C$Forecast<-4
    		scenario_C$BforconstantF<-0.001  ## cluge to get rid of control rule of ave. F
    		scenario_C$BfornoF<-0.0001      ## cluge to get rid of control rule scaling of ave. F
			scenario_C$Fcast_years [c(3,4)]<-c(CYR-5, CYR-1)
  		} else if (scenario == 'scenario_4') {
  		#scenario_C$Forecast <- 5
			scenario_C$Flimitfraction <- s4_F
			#scenario_C$SPRtarget <- s4_F

  		} else if (scenario == 'scenario_5') {
			catch <- expand.grid(Year=c((CYR+1):(CYR+FCASTY)),Seas=1,Fleet=FLEETS,Catch_or_F=0)
			names(catch)<-names(scenario_C$ForeCatch)
			scenario_C$ForeCatch <- rbind(scenario_C$ForeCatch,catch)
  		} else if (scenario == 'scenario_6') {
  			scenario_C$Btarget   <- 0.35
			scenario_C$SPRtarget <- 0.35
			scenario_C$Flimitfraction <- 1.0
  		} else if (scenario == 'scenario_7') {
			scenario_C$Btarget   <- 0.35
			scenario_C$SPRtarget <- 0.35
			scenario_C$Flimitfraction <- 1.0
    		x<-SS_output(dir=file.path(dir1,"scenario_1"))
			scenario_C$ForeCatch<-SS_ForeCatch(x,yrs=CYR:(CYR+2))
  		} else if (scenario == 'scenario_8') {
    		scenario_C$Btarget   <- 0.35
			scenario_C$SPRtarget <- 0.35
			scenario_C$Flimitfraction <- 1.0
			x<-SS_output(dir=file.path(dir1,"scenario_1"))
			scenario_C$ForeCatch<-SS_ForeCatch(x,yrs=CYR:(CYR+1))
			} else if (scenario == 'scenario_9') {   ## OFL for 2029
    		scenario_C$Btarget   <- 0.35
			scenario_C$SPRtarget <- 0.35
			scenario_C$Flimitfraction <- 1.0
			x<-SS_output(dir=file.path(dir1,scenario_F))
			scenario_C$ForeCatch<-SS_ForeCatch(x,yrs=CYR:(CYR+2))
			} else if (scenario == 'scenario_10') {  ## OFL for 2030
    		scenario_C$Btarget   <- 0.35
			scenario_C$SPRtarget <- 0.35
			scenario_C$Flimitfraction <- 1.0
			x<-SS_output(dir=file.path(dir1,scenario_F))
			scenario_C$ForeCatch<-SS_ForeCatch(x,yrs=CYR:(CYR+3))
			} else if (scenario == 'scenario_11') {  ## OFL for 2031
    		scenario_C$Btarget   <- 0.35
			scenario_C$SPRtarget <- 0.35
			scenario_C$Flimitfraction <- 1.0
			x<-SS_output(dir=file.path(dir1,scenario_F))
			scenario_C$ForeCatch<-SS_ForeCatch(x,yrs=CYR:(CYR+4))
			} 

  # Write the modified scenario to the scenario directory
  		SS_writeforecast(scenario_C, dir = dir_name, file = "forecast.ss", writeAll = TRUE, overwrite = TRUE)
  
  # Set the working directory to the scenario directory
  setwd(dir_name)
    # Run the scenario
	os_info <- Sys.info()

	executables <- list(
  		"Windows" = c("ss.exe", "ss3.exe", "ss3_win.exe"),
  		"Darwin" = c("ss_osx")
 )

# Check the operating system
		if (os_info["sysname"] %in% names(executables)) {
  # Get the list of executables for the detected operating system
  			os_executables <- executables[[os_info["sysname"]]]
  
  # Check if any of the executables exist
  			existing_executables <- os_executables[file.exists(os_executables)]
  
  			if (length(existing_executables) > 0) {
    # Run the first existing executable
    			r4ss::run(exe = existing_executables[1], skipfinished = FALSE, verbose = FALSE)
  			} else {
    			print("No executable found for the operating system.")
    			stop()
  			}
  		}
  	}



# Run scenarios in parallel
	foreach(scenario = scenarios, .export = '.DIR') %dopar% {
  		run_scenario(scenario)
	}

# Stop parallel computing
	stopImplicitCluster()

## Get the output from each scenario
	if(SEXES==1) { sex=2 } else sex=1
	
	
	setwd(DIR)
	scenarios <- c("scenario_1", "scenario_2", "scenario_3", "scenario_4", "scenario_5", "scenario_6", "scenario_7", "scenario_8", "scenario_9", "scenario_10", "scenario_11")
	mods1<-SSgetoutput(dirvec=scenarios[1:11])

	kluge1<-data.table(mods1[[1]]$sprseries)$F_report[1]  ## kluge to deal with column name change between versions
 

# Calculate the year index for the summary statistics
	EYR<- CYR+FCASTY
	yr1<- EYR-SYR+3

    
   # Calculate summary statistics for each scenario
	summ <- lapply(seq_along(mods1), function(i) {
		mod <- mods1[[i]]
  		Yrs <- SYR:EYR
  		TOT <- data.table(mod$timeseries)[Yr %in% Yrs]$Bio_all
  		SUMM <- data.table(mod$timeseries)[Yr %in% Yrs]$Bio_smry
  		SSB <- data.table(mod$timeseries)[Yr %in% Yrs]$SpawnBio / sex
  		std <- data.table(mod$stdtable)[name %like% "SSB"][3:yr1, ]$std / sex
  		
  		F <- data.table(mod$annual_time_series)[year %in% Yrs]$F_std
  		
       
  		#Catch <- data.table(mod$sprseries)[Yr %in% Yrs]$Enc_Catch

  		C1<-data.table(mod$annual_time_series)[year %in% SYR:CYR]$dead_catch_B_an
  		C2<-data.table(mod$derived_quants)[Label %like% "ForeCatch_"]$Value
  		Catch<-c(C1,C2)
  		
   		SSB_unfished <- data.table(mod$derived_quants)[Label == "SSB_unfished"]$Value / sex
  		model <- scenarios[i]
  
  		data.table(Yr = Yrs, TOT = TOT, SUMM = SUMM, SSB = SSB, std = std, F = F, Catch = Catch, SSB_unfished = SSB_unfished, model = model)
	})

# Calculate catch projections for each scenario
	Pcatch <- lapply(seq_along(mods1), function(i) {
		mod <- mods1[[i]]
  		Yrs <- (CYR + 1):EYR


  		#Catch <- data.table(mod$sprseries)[Yr %in% Yrs]$Enc_Catch
  		Catch<-data.table(mod$derived_quants)[Label %like% "ForeCatch_"]$Value
  		Catch_std <- data.table(mod$stdtable)[name %like% "ForeCatch_"]$std
  		model <- scenarios[i]
  
		data.table(Yr = Yrs, Catch = Catch, Catch_std = Catch_std, model = model)
	})

# Calculate 5-year projections for catch and F
	SB100 <- summ[[1]][Yr == CYR + 1]$SSB_unfished
	SB40 <- data.table(mods1[[1]]$derived_quants)[Label == "SSB_SPR"]$Value / sex
	SB35 <- data.table(mods1[[8]]$derived_quants)[Label == "SSB_SPR"]$Value / sex
	
	F40_1 <- summ[[Fyr_ts]][Yr == CYR + 1]$F
	F35_1 <- summ[[6]][Yr == CYR + 1]$F
	
	catchABC_1 <- Pcatch[[Fyr_ts]][Yr == CYR + 1]$Catch
	catchOFL_1 <- Pcatch[[6]][Yr == CYR + 1]$Catch
	
	F40_2 <- summ[[Fyr_ts]][Yr == CYR + 2]$F
	F40_3 <- summ[[Fyr_ts]][Yr == CYR + 3]$F
	F40_4 <- summ[[Fyr_ts]][Yr == CYR + 4]$F
	F40_5 <- summ[[Fyr_ts]][Yr == CYR + 5]$F
	
	F35_2 <- summ[[8]][Yr == CYR + 2]$F
	F35_3 <- summ[[9]][Yr == CYR + 3]$F
	F35_4 <- summ[[10]][Yr == CYR + 4]$F
	F35_5 <- summ[[11]][Yr == CYR + 5]$F
	
	catchABC_2 <- Pcatch[[Fyr_ts]][Yr == CYR + 2]$Catch
	catchABC_3 <- Pcatch[[Fyr_ts]][Yr == CYR + 3]$Catch
	catchABC_4 <- Pcatch[[Fyr_ts]][Yr == CYR + 4]$Catch
	catchABC_5 <- Pcatch[[Fyr_ts]][Yr == CYR + 5]$Catch

	catchOFL_2 <- Pcatch[[8]][Yr == CYR + 2]$Catch
	catchOFL_3 <- Pcatch[[9]][Yr == CYR + 3]$Catch
	catchOFL_4 <- Pcatch[[10]][Yr == CYR + 4]$Catch
	catchOFL_5 <- Pcatch[[11]][Yr == CYR + 5]$Catch

	SSB_1 <- summ[[Fyr_ts]][Yr == CYR + 1]$SSB
	SSB_2 <- summ[[Fyr_ts]][Yr == CYR + 2]$SSB
	SSB_3 <- summ[[Fyr_ts]][Yr == CYR + 3]$SSB
	SSB_4 <- summ[[Fyr_ts]][Yr == CYR + 4]$SSB
	SSB_5 <- summ[[Fyr_ts]][Yr == CYR + 5]$SSB

	Five_Year <- data.table(
  		Yr = c((CYR + 1):(CYR + 5)),
  		SSB = c(SSB_1, SSB_2, SSB_3, SSB_4, SSB_5),
  		SSB_PER = c(SSB_1 / SB100, SSB_2 / SB100, SSB_3 / SB100, SSB_4 / SB100, SSB_5 / SB100),
  		SB100 = rep(SB100, 5),
  		SB40 = rep(SB40, 5),
  		SB35 = rep(SB35, 5),
  		F40 = c(F40_1, F40_2, F40_3, F40_4, F40_5),
  		F35 = c(F35_1, F35_2, F35_3, F35_4, F35_5),
  		C_ABC = c(catchABC_1, catchABC_2, catchABC_3, catchABC_4, catchABC_5),
  		C_OFL = c(catchOFL_1, catchOFL_2, catchOFL_3, catchOFL_4, catchOFL_5)
	)
# Combine summary statistics and catch projections into tables
	summ   <- do.call(rbind, summ)
	Pcatch <- do.call(rbind, Pcatch)

	summ   <- summ[model!="scenario_8"]
	Pcatch <- Pcatch[model!="scenario_8"]
	
# Create output list with SSB, CATCH, and Two_year tables
	output <- list(
  		SSB = summ,
  		CATCH = Pcatch,
  		Five_year = Five_Year
	)


pscenarios<-scenarios[1:7]
# Create scenario tables for the document
	BC <- list(
  		Catch = dcast(output$CATCH[Yr >= CYR & model%in%pscenarios], Yr ~ model, value.var = "Catch"),
  		F = dcast(output$SSB[Yr >= CYR & model%in%pscenarios], Yr ~ model, value.var = "F"),
  		SSB = dcast(output$SSB[Yr >= CYR & model%in%pscenarios], Yr ~ model, value.var = "SSB")
	)

	output$Tables <- BC
		
	
## if figures are to be made do the following	
	if(do_fig){

	SSB_unfished<-data.table(mods1[[1]]$derived_quants)[Label=="SSB_unfished"]$Value/sex

# Calculate upper and lower confidence intervals for SSB
		summ2 <- data.table(
  			Yr = c(SYR:EYR),
  			TOT = 0,
  			SUMM = 0,
  			SSB = SB40,
  			std = 0,
  			F = 0,
  			Catch = 0,
  			SSB_unfished = SSB_unfished,
  			model = "SSB40%"
		)

		summ2 <- rbind(
  			summ2,
  			data.table(
    			Yr = c(SYR:EYR),
    			TOT = 0,
    			SUMM = 0,
    			SSB = SB35,
    			std = 0,
    			F = 0,
    			Catch = 0,
    			SSB_unfished = SSB_unfished,
    			model = "SSB35%"
  			),
  			data.table(
    			Yr = c(SYR:EYR),
    			TOT = 0,
    			SUMM = 0,
    			SSB = SSB_unfished * 0.2,
    			std = 0,
    			F = 0,
    			Catch = 0,
    			SSB_unfished = SSB_unfished,
    			model = "SSB20%"
  			),
  			summ
		)

		summ2$model <- factor(summ2$model, levels = unique(summ2$model))
		summ2$UCI <- summ2$SSB + 1.96 * summ2$std
		summ2$LCI <- summ2$SSB - 1.96 * summ2$std
		summ2[LCI < 0]$LCI <- 0

# Calculate upper and lower confidence intervals for catch projections
		Pcatch2 <- data.table(
  			Yr = c(CYR + 1:EYR),
  			Catch = Pcatch[model == "scenario_1" & Yr == EYR]$Catch,
  			Catch_std = 0,
  			model = "Catch Fmaxabc"
		)

		Pcatch2 <- rbind(
  			Pcatch2,
  			data.table(
    			Yr = c(CYR + 1:EYR),
    			Catch = Pcatch[model == "scenario_6" & Yr == EYR]$Catch,
    			Catch_std = 0,
    			model = "Catch Fofl"
  			),
  			Pcatch
		)

		Pcatch2$model <- factor(Pcatch2$model, levels = unique(Pcatch2$model))
		Pcatch2$UCI <- Pcatch2$Catch + 1.96 * Pcatch2$Catch_std
		Pcatch2$LCI <- Pcatch2$Catch - 1.96 * Pcatch2$Catch_std
		Pcatch2[LCI < 0]$LCI <- 0

## SSB_Figures
# Define the scenarios to plot
		#scenarios_P <- unique(summ2$model)[1:10]
        SSB_reference <- summ2[!model%like%'scenario_']
# Create a list to store the plots
		Figs_SSB <- list()
	
	Figs_SSB[['ALL']] <- ggplot(summ2[model%in%unique(summ2$model)[1:10]], aes(x = Yr, y = SSB, color = model, linetype = model)) +
  		geom_line(size = 1.2)+lims(y=c(0,max(summ2$UCI)),x=c(CYR-1,EYR))+
  		theme_bw(base_size = 12) +
  		labs(x = "Year", y = "Spawning biomass (t)", title = "Projections") +
  		scale_color_manual(values=c("dark green","orange","red",2:6,8,9),name="Scenarios")+
  		scale_linetype_manual(values=c(rep(1,3),2:8),name="Scenarios")+
  		theme(legend.position = "bottom", legend.title = element_text(face = "bold"), 
        	legend.text = element_text(size = 10), plot.title = element_text(face = "bold", size = 14),
        	axis.title = element_text(face = "bold", size = 12), axis.text = element_text(size = 10))

# Iterate over each individual scenario and create the plot
 		scenarios_P<-unique(summ$model)
 		scenarios_P2<-as.character(unique(summ2$model)[1:3])
		
		pcol<-c("dark green","orange","red",2:6,8,9)

		for (i in 1:length(scenarios_P)){
			scenarios_P3<-c(scenarios_P2,scenarios_P[i])
  			plot_data <- summ2[model %in% scenarios_P3]
			
			plot <- ggplot(plot_data, aes(x = Yr, y = SSB, linewidth = model, fill = model, color = model, linetype = model)) +
  				geom_line() +lims(y=c(0,max(summ2$UCI)),x=c(CYR-1,EYR))+
  				geom_ribbon(aes(ymin = LCI, ymax = UCI), alpha = 0.2, color = NA) +
  				theme_bw(base_size = 12) +
  				labs(x = "Year", y = "Spawning biomass (t)", title = "Projections") +
  				scale_linetype_manual(values=c(rep(1,3),(i+1)),name="Scenarios")+
        		scale_color_manual(values=c(pcol[1:3],pcol[i+3]),name="Scenarios")+
				scale_fill_manual(values=c(pcol[1:3],pcol[i+3]),name="Scenarios") +
  				scale_linewidth_manual(values = c(rep(1.5, 3), 1.2), name = "Scenarios") +
  				theme(legend.position = "bottom", legend.title = element_text(face = "bold"), 
        			legend.text = element_text(size = 10), plot.title = element_text(face = "bold", size = 14),
        			axis.title = element_text(face = "bold", size = 12), axis.text = element_text(size = 10))

	
    			Figs_SSB[[scenarios_P[i]]] <- plot
		}
## Catch Figures
# Define the scenarios to plot
		scenarios <- unique(Pcatch2$model)[1:9]

# Create a list to store the plots
		Figs_Catch <- list()

		Figs_Catch[['ALL']]<- ggplot(Pcatch2[model %in% unique(Pcatch2$model)[1:9]], aes(x = Yr, y = Catch, linewidth = model, color = model, linetype = model)) +
  			geom_line() + lims(y=c(0,max(Pcatch2$UCI)),x=c((CYR+1),EYR))+
  			theme_bw(base_size = 12) +
  			labs(x = "Year", y = "Catch (t)", title = "Projections") +
  			scale_linetype_manual(values=c(rep(1,2),2:8),name="Scenarios")+
    		scale_color_manual(values=c("dark green","orange",2:6,8,9),name="Scenarios")+
    		scale_linewidth_manual(values=c(rep(1.5,2),rep(1,7)),name="Scenarios")+
  			theme(legend.position = "bottom", legend.title = element_text(face = "bold"), 
        		legend.text = element_text(size = 10), plot.title = element_text(face = "bold", size = 14),
        		axis.title = element_text(face = "bold", size = 12), axis.text = element_text(size = 10))


# Create the remaining plots
scenarios_P2<-as.character(unique(Pcatch2$model)[1:2])

for (i in 1:length(scenarios_P)){
			scenarios_P3<-c(scenarios_P2,scenarios_P[i])
  			plot_data <- Pcatch2[model %in% scenarios_P3]
			
		plot <- ggplot(plot_data, aes(x = Yr, y = Catch, linewidth = model, fill = model, color = model, linetype = model)) +
  			geom_line() + lims(y=c(0,max(Pcatch2$UCI)),x=c((CYR+1),EYR))+
			geom_ribbon(aes(ymin=LCI, ymax=UCI), alpha=0.2,color=NA)+
  			theme_bw(base_size = 12) +
  			labs(x = "Year", y = "Catch (t)", title = "Projections") +
  			scale_linetype_manual(values=c(rep(1,2),(i+1)),name="Scenarios")+
        	scale_color_manual(values=c(pcol[1:2],pcol[i+3]),name="Scenarios")+
			scale_fill_manual(values=c(pcol[1:2],pcol[i+3]),name="Scenarios")+
			scale_linewidth_manual(values=c(rep(1.5,2),1),name="Scenarios")+
  			labs(y = "Catch (t)", x = "Year", title = "Projections") +
  			theme(legend.position = "bottom", legend.title = element_text(face = "bold"), 
        		legend.text = element_text(size = 10), plot.title = element_text(face = "bold", size = 14),
        		axis.title = element_text(face = "bold", size = 12), axis.text = element_text(size = 10))

	
    			Figs_Catch[[scenarios_P[i]]] <- plot
		}

	output$FIGS <- list(Figs_SSB, Figs_Catch)
    }
 
## create markdown tables for assessment

	return(output)
}




## test26.1=Do_AK_TIER_3_Scenarios(DIR = "Model_26.1_750/PROJ", CYR = 2026, SYR = 1977,  SEXES = 1, FLEETS = c(1:2), Scenario2 = 1, S2_F = 0.4, s4_F = 0.75, Fyr_ts=2,do_fig = TRUE) 
