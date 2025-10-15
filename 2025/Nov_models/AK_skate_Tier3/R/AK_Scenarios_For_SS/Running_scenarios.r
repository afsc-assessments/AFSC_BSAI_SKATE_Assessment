
## ALASKA PROJECTION SCENARIOS FOR STOCK SYNTHESIS 3 
## Version October 7, 2021
## Created by Steve Barbeaux E-mail: steve.barbeaux@noaa.gov  Phone: (206) 729-0871 
## 
##
## In the starter.ss file you should change it to read from the converged .par file 
##   1 # 0=use init values in control file; 1=use ss.par
## 
## Assumes you already have the forecast parameters already specified appropriately in the forecast.ss for scenario 1, 
## Make sure there is no catch or F already specified in the forecast file.
##
## DIR is the model directory
## CYR is the model current year, SYR is the start year for the model, SEXES is the number of sexes in model, fleets= the fleet number in SS for your fisheries,
## Scenario2 indicates whether you wish to have a different catch for scenario 2 (1= FmaxABC,2= F as S2_F, 3 = specified catch from a 
## formatted csv saved in the root directory named 'Scenario2_catch.csv', must have an entry for each year, season, and fleet for the years 
## that differ from Fmaxabc
## with columns "Year,Seas,Fleet,Catch_or_F"
## do_fig whether to plot figures
##
##


Do_AK_Scenarios<-function(DIR="C:/WORKING_FOLDER/EBS_PCOD/2022_ASSESSMENT/NOVEMBER_MODELS/GRANT_MODELS/Model19_12A/PROJ",CYR=2022,SYR=1977,FCASTY=12,SEXES=1,FLEETS=1,Scenario2=1,S2_F=0.4,S4_F=0.75,do_fig=TRUE){

	require(r4ss)
	require(data.table)
	require(ggplot2)
	require(R.utils)

	setwd(DIR) ## folder with converged model

	scenario_1 <- SS_readforecast(file = "forecast.ss")

	copyDirectory(getwd(),paste0(getwd(),"/scenario_1"),recursive=FALSE)
	scenario_1$Btarget   <- 0.4
	scenario_1$SPRtarget <- 0.4
	scenario_1$Flimitfraction <- 1.0
	SS_writeforecast(scenario_1, dir = paste0(getwd(),"/scenario_1"), file = "forecast.ss", writeAll = TRUE, overwrite = TRUE)

    
	scenario_2 <- scenario_1
	copyDirectory(getwd(),paste0(getwd(),"/scenario_2"),recursive=FALSE)
	
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
		scenario_2$ForeCatch <- read.csv("Scenario2_catch.csv",header=T)
    }
	SS_writeforecast(scenario_2, dir = paste0(getwd(),"/scenario_2"), file = "forecast.ss", writeAll = TRUE, overwrite = TRUE)

## Average f for previous 5 years  cyear=year(Sys.Date()) 
	scenario_3<-scenario_1
	copyDirectory(getwd(),paste0(getwd(),"/scenario_3"),recursive=FALSE)
	        scenario_C$Forecast<-4
    		scenario_C$BforconstantF<-0.001  ## cluge to get rid of control rule of ave. F
    		scenario_C$BfornoF<-0.0001      ## cluge to get rid of control rule scaling of ave. F
			scenario_C$Fcast_years [c(3,4)]<-c(CYR-5, CYR-1)

	SS_writeforecast(scenario_3, dir = paste0(getwd(),"/scenario_3"), file = "forecast.ss", writeAll = TRUE, overwrite = TRUE)

#F75%
	scenario_4<-scenario_1
	copyDirectory(getwd(),paste0(getwd(),"/scenario_4"),recursive=FALSE)
	scenario_C$Flimitfraction <- s4_F

	SS_writeforecast(scenario_4, dir = paste0(getwd(),"/scenario_4"), file = "forecast.ss", writeAll = TRUE, overwrite = TRUE)

#F=0
	scenario_5<-scenario_1
	copyDirectory(getwd(),paste0(getwd(),"/scenario_5"),recursive=FALSE)
## must enter a 0 in for all fisheries
	catch <- expand.grid(Year=c((CYR+1):(CYR+FCASTY)),Seas=1,Fleet=FLEETS,Catch_or_F=0)
	names(catch)<-names(scenario_5$ForeCatch)
	scenario_5$ForeCatch <- rbind(scenario_5$ForeCatch,catch)
	SS_writeforecast(scenario_5, dir = paste0(getwd(),"/scenario_5"), file = "forecast.ss", writeAll = TRUE, overwrite = TRUE)


## Fofl = F35% for all years
	scenario_6<-scenario_1
	copyDirectory(getwd(),paste0(getwd(),"/scenario_6"),recursive=FALSE)
	scenario_6$Btarget   <- 0.35
	scenario_6$SPRtarget <- 0.35
	#scenario_4$BforconstantF <- 0.4
	scenario_6$Flimitfraction <- 1.0
	SS_writeforecast(scenario_6, dir = paste0(getwd(),"/scenario_6"), file = "forecast.ss", writeAll = TRUE, overwrite = TRUE)

## F40%=Fabc for 20&21 and Fofl for all further years

	scenario_7<-scenario_6
	copyDirectory(getwd(),paste0(getwd(),"/scenario_7"),recursive=FALSE)
	x<-SS_output(dir=paste0(getwd(),"/scenario_1"))
	scenario_7$ForeCatch<-SS_ForeCatch(x,yrs=CYR:(CYR+2))
	SS_writeforecast(scenario_7, dir = paste0(getwd(),"/scenario_7"), file = "forecast.ss", writeAll = TRUE, overwrite = TRUE)

## for calculating OFL values for Yr+2 F40% for 20 & Fofl for all further years
    scenario_8<-scenario_6
	copyDirectory(getwd(),paste0(getwd(),"/scenario_8"),recursive=FALSE)
	x<-SS_output(dir=paste0(getwd(),"/scenario_1"))
	scenario_8$ForeCatch<-SS_ForeCatch(x,yrs=CYR:(CYR+1))
	SS_writeforecast(scenario_8, dir = paste0(getwd(),"/scenario_8"), file = "forecast.ss", writeAll = TRUE, overwrite = TRUE)




## run all the scenarios
	scen<-c("scenario_1","scenario_2","scenario_3","scenario_4","scenario_5","scenario_6","scenario_7","scenario_8")

	for(i in 1:8){
		setwd(paste0(DIR,"/",scen[i]))
    	system("ss")
  	}

## For running on a mac computer you will need to turn off the previous code starting on line 107 and instead use this:
##	
##	scen<-c("scenario_1","scenario_2","scenario_3","scenario_4","scenario_5","scenario_6","scenario_7","scenario_8")
## 
##      Exe_path <- "/path/Executables/SS_V3_30_21/ss_osx"
##
## for(i in 1:8){
##    file.copy(Exe_path,file.path(Model_path,paste0(scen[i])))
##    setwd(paste0(DIR,"/",scen[i]))
##    system("./ss_osx")
## }
	
	
	
	
if(SEXES==1) sex=2
    if(SEXES>1) sex=1
   	setwd(DIR)
	mods1<-SSgetoutput(dirvec=scen[1:8])
	
	
	summ<-vector("list",length=7)
	Pcatch<-vector("list",length=7)
	EYR<- CYR+FCASTY
	yr1<- EYR-SYR+3

	kluge1<-data.table(mods1[[1]]$sprseries)$F_report[1]  ## kluge to deal with column name change between versions

    F1<-vector("list",length=8)
    
    for(i in 1:8){
    	if(!is.null(kluge1)){ F1[[i]]=data.table(mods1[[i]]$sprseries)[Yr%in%c(SYR:EYR)]$F_report
    		} else {
    		F1[[i]]=data.table(mods1[[i]]$sprseries)[Yr%in%c(SYR:EYR)]$F_std}
    } 

	
	for(i in 1:7){
		summ[[i]]<-data.table(Yr=SYR:EYR,
			TOT=data.table(mods1[[i]]$timeseries)[Yr%in%c(SYR:EYR)]$Bio_all,
			SUMM=data.table(mods1[[i]]$timeseries)[Yr%in%c(SYR:EYR)]$Bio_smry,
			SSB=data.table(mods1[[i]]$timeseries)[Yr%in%c(SYR:EYR)]$SpawnBio/sex,
			std=data.table(mods1[[i]]$stdtable)[name%like%"SSB"][3:yr1,]$std/sex,
			F=F1[[i]],
			Catch=data.table(mods1[[i]]$sprseries)[Yr%in%c(SYR:EYR)]$Enc_Catch,
			SSB_unfished=data.table(mods1[[i]]$derived_quants)[Label=="SSB_unfished"]$Value/sex,
			model=scen[i])
	    Pcatch[[i]]<-data.table(Yr=(CYR+1):EYR,
	    	Catch=data.table(mods1[[i]]$sprseries)[Yr%in%c((CYR+1):EYR)]$Enc_Catch,
	    	Catch_std=data.table(mods1[[i]]$stdtable)[name%like%"ForeCatch_"]$std[2:FCASTY+1], 
	    	model=scen[i])
	
	}

    summ8<-data.table(Yr=SYR:EYR,
    	TOT=data.table(mods1[[8]]$timeseries)[Yr%in%c(SYR:EYR)]$Bio_all,
    	SUMM=data.table(mods1[[8]]$timeseries)[Yr%in%c(SYR:EYR)]$Bio_smry,
    	SSB=data.table(mods1[[8]]$timeseries)[Yr%in%c(SYR:EYR)]$SpawnBio/sex,
    	std=data.table(mods1[[8]]$stdtable)[name%like%"SSB"][3:yr1,]$std/sex,
    	F=F1[[8]],
    	Catch=data.table(mods1[[8]]$sprseries)[Yr%in%c(SYR:EYR)]$Enc_Catch,
    	SSB_unfished=data.table(mods1[[8]]$derived_quants)[Label=="SSB_unfished"]$Value/sex,
    	model=scen[8])
	Pcatch8<-data.table(Yr=(CYR+1):EYR,
		Catch=data.table(mods1[[8]]$sprseries)[Yr%in%c((CYR+1):EYR)]$Enc_Catch,
		Catch_std=data.table(mods1[[8]]$stdtable)[name%like%"ForeCatch_"]$std[1:FCASTY], 
		model=scen[8])
	## Calculate 2 year projections for catch and F
	SB100=summ[[1]][Yr==CYR+1]$SSB_unfished
	SB40=data.table(mods1[[1]]$derived_quants)[Label=="SSB_SPR"]$Value/sex
	SB35=data.table(mods1[[8]]$derived_quants)[Label=="SSB_SPR"]$Value/sex
	
    F40_1=summ[[1]][Yr==CYR+1]$F
    F35_1=summ[[6]][Yr==CYR+1]$F
    catchABC_1=Pcatch[[1]][Yr==CYR+1]$Catch
    catchOFL_1=Pcatch[[6]][Yr==CYR+1]$Catch
    
    F40_2=summ[[1]][Yr==CYR+2]$F
    F35_2=summ8[Yr==CYR+2]$F
    catchABC_2=Pcatch[[1]][Yr==CYR+2]$Catch
    catchOFL_2=Pcatch8[Yr==CYR+2]$Catch
    SSB_1<-summ[[1]][Yr==CYR+1]$SSB
    SSB_2<-summ[[1]][Yr==CYR+2]$SSB

    Two_Year=data.table(Yr=c((CYR+1):(CYR+2)),SSB=c(SSB_1,SSB_2),SSB_PER=c(SSB_1/SB100,SSB_2/SB100),SB100=c(SB100,SB100),SB40=c(SB40,SB40),SB35=c(SB35,SB35),F40=c(F40_1,F40_2),F35=c(F35_1,F35_2),C_ABC=c(catchABC_1,catchABC_2),C_OFL=c(catchOFL_1,catchOFL_2))

    ## rbind vectors into tables
	summ=do.call(rbind,summ)
	Pcatch=do.call(rbind,Pcatch)

	output=list(SSB=summ,CATCH=Pcatch,Two_year=Two_Year)
    
    ## create scenario tables for document
  	BC=vector("list")
  	BC$Catch<-dcast(output$SSB[Yr>=CYR],Yr~model,value.var="Catch") 
  	BC$F<-dcast(output$SSB[Yr>=CYR],Yr~model,value.var="F")
  	BC$SSB<-dcast(output$SSB[Yr>=CYR],Yr~model,value.var="SSB")

  	output$Tables<-BC

	if(do_fig){
		
		x<-SS_output(getwd())
		SSB_unfished<-data.table(x$derived_quants)[Label=="SSB_unfished"]$Value/sex
		
		y<-data.table(Yr=c(SYR:EYR),TOT=0,SUMM=0,SSB=SB40,std=0,F=0,Catch=0,SSB_unfished=SSB_unfished,model="SSB40%")
		y1<-data.table(Yr=c(SYR:EYR),TOT=0,SUMM=0,SSB=SB35,std=0,F=0,Catch=0,SSB_unfished=SSB_unfished,model="SSB35%")
		y2<-data.table(Yr=c(SYR:EYR),TOT=0,SUMM=0,SSB=SSB_unfished*0.2,std=0,F=0,Catch=0,SSB_unfished=SSB_unfished,model="SSB20%")
		summ2<-rbind(y,y1,y2,summ)

		summ2$model<-factor(summ2$model,levels=unique(summ2$model))
		summ2$UCI<-summ2$SSB+1.96*summ2$std
		summ2$LCI<-summ2$SSB-1.96*summ2$std
		summ2[LCI<0]$LCI=0

		
		y<-data.table(Yr=c(CYR+1:EYR),Catch=Pcatch[model=="scenario_1" & Yr==EYR]$Catch,Catch_std=0,model="Catch Fmaxabc")
		y1<-data.table(Yr=c(CYR+1:EYR),Catch=Pcatch[model=="scenario_6" & Yr==EYR]$Catch,Catch_std=0,model="Catch Fofl")
		Pcatch2<-rbind(y,y1,Pcatch)

        Pcatch2$model<-factor(Pcatch2$model,levels=unique(Pcatch2$model))
		Pcatch2$UCI<-Pcatch2$Catch+1.96*Pcatch2$Catch_std
		Pcatch2$LCI<-Pcatch2$Catch-1.96*Pcatch2$Catch_std
		Pcatch2[LCI<0]$LCI=0

##SSB_Figures
		SS_ALL<-ggplot(summ2[model%in%unique(summ2$model)[1:10]],aes(x=Yr,y=SSB,size=model,color=model,linetype=model))+
		geom_line()+theme_bw(base_size=16)+lims(y=c(0,max(summ2$UCI)),x=c(CYR-1,EYR))+
		scale_linetype_manual(values=c(rep(1,3),2:8),name="Scenarios")+
        scale_color_manual(values=c("dark green","orange","red",2:6,8,9),name="Scenarios")+
        scale_size_manual(values=c(rep(1.5,3),rep(1,7)),name="Scenarios")+labs(y="Spawning biomass (t)",x="Year",title="Projections")
		
        SS_1<-ggplot(summ2[model%in%unique(summ2$model)[1:4]],aes(x=Yr,y=SSB,size=model,color=model,linetype=model,fill=model))+
		geom_line()+theme_bw(base_size=16)+lims(y=c(0,max(summ2$UCI)),x=c(CYR-1,EYR))+
		geom_ribbon(aes(ymin=LCI, ymax=UCI,linetype=model), alpha=0.2,color="black",size=0.2)+
		scale_linetype_manual(values=c(rep(1,3),2),name="Scenarios")+
		scale_fill_manual(values=c("dark green","orange","red",2),name="Scenarios")+
        scale_color_manual(values=c("dark green","orange","red",2),name="Scenarios")+
        scale_size_manual(values=c(rep(1.5,3),rep(1,7)),name="Scenarios")+labs(y="Spawning biomass (t)",x="Year",title="Projections MaxFABC")

        SS_2<-ggplot(summ2[model%in%unique(summ2$model)[c(1:3,5)]],aes(x=Yr,y=SSB,size=model,color=model,linetype=model,fill=model))+
		geom_line()+theme_bw(base_size=16)+lims(y=c(0,max(summ2$UCI)),x=c(CYR-1,EYR))+
		geom_ribbon(aes(ymin=LCI, ymax=UCI,linetype=model), alpha=0.2,color="black",size=0.2)+
		scale_linetype_manual(values=c(rep(1,3),3),name="Scenarios")+
		scale_fill_manual(values=c("dark green","orange","red",3),name="Scenarios")+
        scale_color_manual(values=c("dark green","orange","red",3),name="Scenarios")+
        scale_size_manual(values=c(rep(1.5,3),rep(1,7)),name="Scenarios")+labs(y="Spawning biomass (t)",x="Year",title="Projections Scenario_2")
 
        SS_3<-ggplot(summ2[model%in%unique(summ2$model)[c(1:3,6)]],aes(x=Yr,y=SSB,size=model,color=model,linetype=model,fill=model))+
		geom_line()+theme_bw(base_size=16)+lims(y=c(0,max(summ2$UCI)),x=c(CYR-1,EYR))+
		geom_ribbon(aes(ymin=LCI, ymax=UCI,linetype=model), alpha=0.2,color="black",size=0.2)+
		scale_linetype_manual(values=c(rep(1,3),4),name="Scenarios")+
		scale_fill_manual(values=c("dark green","orange","red",4),name="Scenarios")+
        scale_color_manual(values=c("dark green","orange","red",4),name="Scenarios")+
        scale_size_manual(values=c(rep(1.5,3),rep(1,7)),name="Scenarios")+labs(y="Spawning biomass (t)",x="Year",title="Projections Scenario 3 - Average F")
 
   		SS_4<-ggplot(summ2[model%in%unique(summ2$model)[c(1:3,7)]],aes(x=Yr,y=SSB,size=model,color=model,linetype=model,fill=model))+
		geom_line()+theme_bw(base_size=16)+lims(y=c(0,max(summ2$UCI)),x=c(CYR-1,EYR))+
		geom_ribbon(aes(ymin=LCI, ymax=UCI,linetype=model), alpha=0.2,color="black",size=0.2)+
		scale_linetype_manual(values=c(rep(1,3),5),name="Scenarios")+
		scale_fill_manual(values=c("dark green","orange","red",5),name="Scenarios")+
        scale_color_manual(values=c("dark green","orange","red",5),name="Scenarios")+
        scale_size_manual(values=c(rep(1.5,3),rep(1,7)),name="Scenarios")+labs(y="Spawning biomass (t)",x="Year",title="Projections Scenario 4 - F75%")
        
        SS_5<-ggplot(summ2[model%in%unique(summ2$model)[c(1:3,8)]],aes(x=Yr,y=SSB,size=model,color=model,linetype=model,fill=model))+
		geom_line()+theme_bw(base_size=16)+lims(y=c(0,max(summ2$UCI)),x=c(CYR-1,EYR))+
		geom_ribbon(aes(ymin=LCI, ymax=UCI,linetype=model), alpha=0.2,color="black",size=0.2)+
		scale_linetype_manual(values=c(rep(1,3),6),name="Scenarios")+
		scale_fill_manual(values=c("dark green","orange","red",6),name="Scenarios")+
        scale_color_manual(values=c("dark green","orange","red",6),name="Scenarios")+
        scale_size_manual(values=c(rep(1.5,3),rep(1,7)),name="Scenarios")+labs(y="Spawning biomass (t)",x="Year",title="Projections Scenario 5 - No catch")   
                    
        SS_6<-ggplot(summ2[model%in%unique(summ2$model)[c(1:3,9,10)]],aes(x=Yr,y=SSB,size=model,color=model,linetype=model,fill=model))+
		geom_line()+theme_bw(base_size=16)+lims(y=c(0,max(summ2$UCI)),x=c(CYR-1,EYR))+
		geom_ribbon(aes(ymin=LCI, ymax=UCI,linetype=model), alpha=0.2,color="black",size=0.2)+
		scale_linetype_manual(values=c(rep(1,3),2:8),name="Scenarios")+
		scale_fill_manual(values=c("dark green","orange","red",8,9),name="Scenarios")+
        scale_color_manual(values=c("dark green","orange","red",8,9),name="Scenarios")+
        scale_size_manual(values=c(rep(1.5,3),rep(1,7)),name="Scenarios")+labs(y="Spawning biomass (t)",x="Year",title="Projections Scenarios 6 and 7")
        
        Figs_SSB<-list(SS_ALL,SS_1,SS_2,SS_3,SS_4,SS_5,SS_6)
   ## Catch Figures
       C_ALL<-ggplot(Pcatch2[model%in%unique(Pcatch2$model)[1:9]],aes(x=Yr,y=Catch,size=model,color=model,linetype=model))+
		geom_line()+theme_bw(base_size=16)+lims(y=c(0,max(Pcatch2$UCI)),x=c(CYR+1,EYR))+
		scale_linetype_manual(values=c(rep(1,2),2:8),name="Scenarios")+
        scale_color_manual(values=c("dark green","orange",2:6,8,9),name="Scenarios")+
        scale_size_manual(values=c(rep(1.5,2),rep(1,7)),name="Scenarios")+labs(y="Catch (t)",x="Year",title="Projections")

        C_1<-ggplot(Pcatch2[model%in%unique(Pcatch2$model)[1:3]],aes(x=Yr,y=Catch,size=model,color=model,linetype=model,fill=model))+
		geom_line()+theme_bw(base_size=16)+lims(y=c(0,max(Pcatch2$UCI)),x=c(CYR+1,EYR))+
		geom_ribbon(aes(ymin=LCI, ymax=UCI,linetype=model), alpha=0.2,color="black",size=0.2)+
		scale_linetype_manual(values=c(rep(1,2),2),name="Scenarios")+
		scale_fill_manual(values=c("dark green","orange",2),name="Scenarios")+
        scale_color_manual(values=c("dark green","orange",2),name="Scenarios")+
        scale_size_manual(values=c(rep(1.5,2),rep(1,7)),name="Scenarios")+labs(y="Catch (t)",x="Year",title="Projections MaxFABC")

		C_2<-ggplot(Pcatch2[model%in%unique(Pcatch2$model)[c(1:2,4)]],aes(x=Yr,y=Catch,size=model,color=model,linetype=model,fill=model))+
		geom_line()+theme_bw(base_size=16)+lims(y=c(0,max(Pcatch2$UCI)),x=c(CYR+1,EYR))+
		geom_ribbon(aes(ymin=LCI, ymax=UCI,linetype=model), alpha=0.2,color="black",size=0.2)+
		scale_linetype_manual(values=c(rep(1,2),3),name="Scenarios")+
		scale_fill_manual(values=c("dark green","orange",3),name="Scenarios")+
        scale_color_manual(values=c("dark green","orange",3),name="Scenarios")+
        scale_size_manual(values=c(rep(1.5,2),rep(1,7)),name="Scenarios")+labs(y="Catch (t)",x="Year",title="Projections Scenario 2")

        C_3<-ggplot(Pcatch2[model%in%unique(Pcatch2$model)[c(1:2,5)]],aes(x=Yr,y=Catch,size=model,color=model,linetype=model,fill=model))+
		geom_line()+theme_bw(base_size=16)+lims(y=c(0,max(Pcatch2$UCI)*1.25),x=c(CYR+1,EYR))+
		geom_ribbon(aes(ymin=LCI, ymax=UCI,linetype=model), alpha=0.2,color="black",size=0.2)+
		scale_linetype_manual(values=c(rep(1,2),5),name="Scenarios")+
		scale_fill_manual(values=c("dark green","orange",5),name="Scenarios")+
        scale_color_manual(values=c("dark green","orange",5),name="Scenarios")+
        scale_size_manual(values=c(rep(1.5,2),rep(1,7)),name="Scenarios")+labs(y="Catch (t)",x="Year",title="Projections Scenario 3 - Average F")
    
  		C_4<-ggplot(Pcatch2[model%in%unique(Pcatch2$model)[c(1:2,6)]],aes(x=Yr,y=Catch,size=model,color=model,linetype=model,fill=model))+
		geom_line()+theme_bw(base_size=16)+lims(y=c(0,max(Pcatch2$UCI)),x=c(CYR+1,EYR))+
		geom_ribbon(aes(ymin=LCI, ymax=UCI,linetype=model), alpha=0.2,color="black",size=0.2)+
		scale_linetype_manual(values=c(rep(1,2),4),name="Scenarios")+
		scale_fill_manual(values=c("dark green","orange",4),name="Scenarios")+
        scale_color_manual(values=c("dark green","orange",4),name="Scenarios")+
        scale_size_manual(values=c(rep(1.5,2),rep(1,7)),name="Scenarios")+labs(y="Catch (t)",x="Year",title="Projections Scenario 4 - F75%")

        C_6<-ggplot(Pcatch2[model%in%unique(Pcatch2$model)[c(1:2,8,9)]],aes(x=Yr,y=Catch,size=model,color=model,linetype=model,fill=model))+
		geom_line()+theme_bw(base_size=16)+lims(y=c(0,max(Pcatch2$UCI)),x=c(CYR+1,EYR))+
		geom_ribbon(aes(ymin=LCI, ymax=UCI,linetype=model), alpha=0.2,color="black",size=0.2)+
		scale_linetype_manual(values=c(rep(1,2),8,9),name="Scenarios")+
		scale_fill_manual(values=c("dark green","orange",8,9),name="Scenarios")+
        scale_color_manual(values=c("dark green","orange",8,9),name="Scenarios")+
        scale_size_manual(values=c(rep(1.5,2),rep(1,7)),name="Scenarios")+labs(y="Catch (t)",x="Year",title="Projections Scenarios 6 and 7")

        Figs_Catch<-list(C_ALL,C_1,C_2,C_3,C_4,C_6)
        output$FIGS=list(Figs_SSB,Figs_Catch)
        }
 
	return(output)
}


#profiles_M23.1.0.d<-Do_AK_Scenarios(DIR="C:/WORKING_FOLDER/EBS_PCOD_work_folder/2023_ASSESSMENT/NOVEMBER_MODELS/2023_MODELS/Model_23.1.0.d2/PROJ",CYR=2023,SYR=1977,SEXES=1,FLEETS=c(1),Scenario2=1,S2_F=0.4,do_fig=TRUE)

