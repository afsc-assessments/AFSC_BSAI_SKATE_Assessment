GET_SARA_ENSEMBLE<-function(dir=getwd(),models=mods,nam_mod=mods_nam,WEIGHT=WT,email="'steve.barbeaux@noaa.gov'",Enter_Data=FALSE,STOCK="EBS PCOD",STOCK_NAME="'Pacific cod'",REGION="'EBS'",TIER="'3b'",UPDATE="'benchmark'",COMPLEX="'NA'",CID=5,AID=5,BID=3,SCID=5,EL=2,MFR=1000,F_LIMIT=0.324, F_MSY_PROJ=0.2, SURV_DESC="'EBS Slope Bottom trawl survey'",NOTES="'2022 New Series Ensemble.'",stryr=1977,SS3_proj=TRUE)
{
  require(r4ss)
  require(data.table)
  require(stringr)
  setwd(dir)
  ##data<-SS_output(dir=dir,forecast=F,covar=T)
  data<-SSgetoutput(dirvec=mods)
  input<-SS_readdat(paste0(models[1],"/data_echo.ss_new"),version="3.30")
  starter=SS_readstarter(paste0(models[1],"/starter.ss_new"))

  endyr=data[[1]]$endyr

 if(length(models)>1){ENS<-"'YES'"}else{ENS<-"'NA'"}  ## ensemble or not
 funit<-starter$F_report_units

 if(funit==0){print("Your F units are not specified in the start file, you must rerun model and estimate F");exit()}
 if(funit==1)F_rep="'exploitation(Bio)'" 
 if(funit==2)F_rep="'exploitation(Num)'"
 if(funit==3)F_rep="'sum(Apical_F's)'"
 if(funit==4)F_rep="'true F for range of ages'"
 if(funit==5)F_rep="'unweighted avg. F for range of ages'"

fbasis<-starter$F_report_basis
 if(fbasis==0) F_BAS="'raw_annual_F'"
  if(fbasis==1) F_BAS="'F/Fspr'" 
  if(fbasis==2) F_BAS="'F/Fmsy'" 
  if(fbasis==3) F_BAS="'F/Fbtgt; where F means annual_F'"
  if(fbasis>3){ print("Your Fbasis is incorrect you must rerun model with appropriate basis specified in the start file");exit()}

F_LIMIT_BASIS<-paste0("'F from ",endyr-1," asmt corresponding to specified ",endyr-2," OFL'") #Should endyr-2 be endyr??? 

for(i in 1:length(models)){
  names(data[[i]]$timeseries)<-str_replace_all(names(data[[i]]$timeseries),"\\(",".")
  names(data[[i]]$timeseries)<-str_replace_all(names(data[[i]]$timeseries),"\\)",".")
  names(data[[i]]$timeseries)<-str_replace_all(names(data[[i]]$timeseries),":",".")
  names(data[[i]]$endgrowth)<-str_replace_all(names(data[[i]]$endgrowth),":",".")
  names(data[[i]]$sprseries)<-str_replace_all(names(data[[i]]$sprseries),"=",".")
  names(data[[i]]$sprseries)<-str_replace_all(names(data[[i]]$sprseries),"-",".")
}



  fish_name<-as.character(data[[1]]$definitions[,9])[1:(data[[1]]$nfishfleets)]
  #survey_name<-as.character(data[[1]]$definitions[2,])[(data[[1]]$nfishfleets+2):ncol(data[[1]]$definitions)] #NOT GIVING SURVEY NAMES
  #MDB modification
  survey_name<-as.character(data[[1]]$definitions[,9])[(data[[1]]$nfishfleets+1):(data[[1]]$nfishfleets+length(SURV_DESC))]
  nfish<-nrow(data.table(input$fleetinfo)[type==1])
  NSEX<-input$Nsexes
    
  if(Enter_Data){

    STOCK  <-readline("Enter STOCK - identifier key for joining SARA data, see lookup table for key code:" )
    STOCK_NAME <-readline("Enter Stock Name: " )
    REGION <- readline("Enter Region (AI AK BOG BSAI EBS GOA SEO WCWYK): ")
    TIER <- readline("Enter Tier TIER (1a 1b 2a 2b 3a 3b 4 5 6): " )
    NOTES<-readline("Enter any notes on the assessment: ")
    MFR<- readline("Enter multiplier for recruitment data, N at age, and survey number (1,1000,1000000): ")
    CID<-readline("CATCH_INPUT_DATA - Categorical level of catch input data used in the assessment model accepted for use in management, as described in Appendix A of the 2018 Next Gen SAIP document, see lookup table (options: 0 - None, 1 - Major gaps preclude use, 2 - Major gaps in some sectors(s), 3 - Minor gaps across sectors, 4 - Minor gaps in some sector(s), 5 - Near complete knowledge):")
    AID<-readline("ABUNDANCE_INPUT_DATA - Categorical level of abundance input data used in the assessment model accepted for use in management, as described in Appendix A of the 2018 Next Gen SAIP document, see lookup table (options: 0 - None, 1 - Uncertain or expert opinion, 2 - Standardized fishery-dependent, 3 - Limited fishery-independent, 4 - Comprehensive fishery-independent, 5 - Absolute abundance):")
    BID<-readline("BIOLOGICAL_INPUT_DATA - Categorical level of biological or life history input data used in the assessment model accepted for use in management, as described in Appendix A of the 2018 Next Gen SAIP document, see lookup table (options: 0 - None, 1 - Proxy-based, 2 - Empirical and proxy-based, 3 - Mostly empirical estimates, 4 - Track changes over time, 5 - Comprehensive over time and space):")
    SCID<-readline("SIZEAGE_COMP_INPUT_DATA - Categorical level of size or age composition input data used in the assessment model accepted for use in management, as described in Appendix A of the 2018 Next Gen SAIP document, see lookup table (options: 0 - None, 1 - Major gaps preclude use, 2 - Suport data-limited only, 3 - Gaps, bus supports age-structured assessment, 4 - Support fishery composition, 5 - Very complete):")
    EL<- readline("ECOSYSTEM_LINKAGE - Categorical level of ecosystem linkage used in the assessment model accepted for use in management, as described in Appendix A of the 2018 Next Gen SAIP document, see lookup table (options: 0 - None, 1 - Informative or used to process input data, 2 - Random variation, not mechanistic, 3 - Direct linkages(s), 4 - Linkage(s) informed by process studies, 5 - Fully coupled):")
    F_LIMIT<-readline("F_LIMIT - Fishing mortality rate or total catch threshold, above which the stock is considered to be overfishing. NOTE: For Tiers 1-3 during a full or operational assessment this is the reverse engineered F from this year's accepted model that would have produced a catch for last year equal to last year’s OFL. For Tiers 1-3 during a partial assessment this is FMSY. For Tiers 4-5, single stock assessment, this is the estimate of M. For Tiers 4-5 stock complex assessments or Tier 6, this is sum of the OFL in tons for the stocks in the complex.):")
    SURV_DESC<-readline("The names of the surveys used in this assessment seperated by comas:")
}



if(TIER %like% "1"){ F_MSY_BASIS <- "'Direct estmate'"} else{F_MSY_BASIS <- "'F35% as proxy'"}


F_MSY<-vector('list',length=length(models))
minb<-vector('list',length=length(models))
maxb<-vector('list',length=length(models))
ssb_unfished<-vector('list',length=length(models))
spwnbio<-vector('list',length=length(models))
bio_sum<-vector('list',length=length(models))
bio_tot<-vector('list',length=length(models))
F_ap<-vector('list',length=length(models))
recr<-vector('list',length=length(models))

for(i in 1:length(models)){
 minb[[i]]<-(data[[i]]$derived_quants$Value[data[[i]]$derived_quants$Label==paste("SSB_",data[[i]]$endyr,sep="")]-(1.96*data[[i]]$derived_quants$StdDev[data[[i]]$derived_quants$Label==paste("SSB_",data[[i]]$endyr,sep="")]))*WEIGHT[i]
 maxb[[i]]<- (data[[i]]$derived_quants$Value[data[[i]]$derived_quants$Label==paste("SSB_",data[[i]]$endyr,sep="")]+(1.96*data[[i]]$derived_quants$StdDev[data[[i]]$derived_quants$Label==paste("SSB_",data[[i]]$endyr,sep="")]))*WEIGHT[i]
 ssb_unfished[[i]]<-(data[[i]]$derived_quants$Value[data[[i]]$derived_quants$Label=="SSB_unfished"]/2)*WEIGHT[i]
 recr[[i]]<-(data[[i]]$recruit$pred_recr[data[[i]]$recruit$era!="Forecast"&data[[i]]$recruit$Yr>=stryr & data[[i]]$recruit$Yr<=data[[i]]$endyr])*WEIGHT[i]
 spwnbio[[i]]<-(data[[i]]$recruit$SpawnBio[data[[i]]$recruit$era!="Forecast"&data[[i]]$recruit$Yr>=stryr & data[[i]]$recruit$Yr<=data[[i]]$endyr])*WEIGHT[i]
 bio_sum[[i]]<-(data[[i]]$timeseries$Bio_smry[data[[i]]$timeseries$Era=="TIME"&data[[i]]$timeseries$Yr>=stryr & data[[i]]$timeseries$Yr<=data[[i]]$endyr])*WEIGHT[i]
 bio_tot[[i]]<-(data[[i]]$timeseries$Bio_all[data[[i]]$timeseries$Era=="TIME"&data[[i]]$timeseries$Yr>=stryr & data[[i]]$timeseries$Yr<=data[[i]]$endyr])*WEIGHT[i]

 F_ap[[i]]<-(data[[i]]$sprseries$sum_Apical_F[data[[i]]$sprseries$Yr>=stryr & data[[i]]$sprseries$Yr<=data[[i]]$endyr])*WEIGHT[i] 
 #This estimate if you use SS3 for projections
 if(SS3_proj==TRUE)
 {
 	F_MSY[[i]]<- (data[[i]]$derived_quants$Value[data[[i]]$derived_quants$Label=="annF_Btgt"])*WEIGHT[i]
 }
  if(SS3_proj==FALSE)
 {
 	F_MSY[[i]]<- (F_MSY_PROJ[i])*WEIGHT[i]
 }
}

minb<-do.call(rbind,minb)
maxb<-do.call(rbind,maxb)
ssb_unfished<-do.call(rbind,ssb_unfished)
spwnbio<-do.call(rbind,spwnbio)
bio_sum<-do.call(rbind,bio_sum)
bio_tot<-do.call(rbind,bio_tot)
F_ap<-do.call(rbind,F_ap)
recr<-do.call(rbind,recr)
F_MSY<-do.call(rbind,F_MSY)

minb<-sum(minb)
maxb<-sum(maxb)
ssb_unfished<-sum(ssb_unfished)
recr<-colSums(recr)
F_ap<-colSums(F_ap)
bio_sum<-colSums(bio_sum)
bio_tot<-colSums(bio_tot)

if(NSEX==1){spwnbio<-colSums(spwnbio/2)}else{ spwnbio<-colSums(spwnbio)}

F_MSY<-sum(F_MSY)

  prolog=paste0(
    "#Stock Assessment Results Archive (SARA) file for stocks managed under the North Pacific Fisheries Management Council\n",
    "#This form is only required for Tier 1-6 stocks in a full year, or Tier 1-3 stocks in a partial year \n",
    "#There are four required sections to update: Assessment Summary, Fishing Mortality Estimates, Biomass Estimates, and Time Series Estimates with an optional fifth Survey Estimates section \n",
    "#Please fill in the text (surrounded by quotes) or data as values in the line after each field marked with a # and capitalized name (e.g., #STOCK, the next line should be ATF) \n",
    "#Note that several of the fields below may be static or only change occasionally, please see the SARA_HQ_Lookup Google sheet for more details \n",
    "#ASSESSMENT_SUMMARY ------------------------------------------------------------------------------------------------- \n",
    "#STOCK - identifier key for joining SARA data, see lookup table for key code \n",
    paste("",STOCK,"",sep="'"),
    "\n#STOCK_NAME - Entity name of stock for Species Information System (SIS), see lookup table, if stock does not exist in list, contact Kalei Shotwell or Jim Ianelli \n",
    STOCK_NAME,
    "\n#REGION - General area of assessment for stock, go to lookup table \n",
    REGION,
    "\n# ASSESS_YEAR - year assessment is presented to the SSC \n",
    endyr, 
    "\n#ASMT_MONTH - Month when assessment completes its final technical review by NPFMC (options: Jan, Feb, Mar, Apr, May, Jun, Jul, Aug, Sep, Oct, Nov, Dec) \n",
    "'Dec'",
    "\n#TIER - Assessment tier for the stock (options: 1a, 1b, 2a, 2b, 3a, 3b, 4, 5, 6; NOTE: no stock assessment should be conducted for Tiers 4, 5, or 6, on off-cycle years)  \n",
    TIER, 
    "\n#NUM_SEXES - Number of sexes if 1 sex=ALL elseif 2 sex=(FEMALE MALE)\n",
    NSEX,
    "\n#NUM_FIHSERIES - Number of fisheries \n",
    nfish,
    "\n#REC_MULT - Multiplier for recruitment N at age and survey number (options: 1, 1000, 1000000)\n",
    MFR,
    "\n#RECAGE - Recruitment age used by model or size \n",
    data[[1]]$recruitment_dist$recruit_dist$Age,
    "\n#COMPLEX - Is this a stock complex? (options: Yes, NA) \n", 
    COMPLEX,
    "\n#LAST_DATA_YEAR - Last year of input data included in the assessment\n",
    endyr,
    "\n#ASMT_MODEL_CATEGORY - Category of stock assessment model accepted for use in management, as described in Appendix A of the 2018 Next Gen SAIP document, see lookup table (options: 1 - Data-Limited, 2 - Index-Based, 3 - Aggregate Biomass Dynamics, 4 - Virtual Population Dynamics, 5 - Statistical Catch-at-length, 6 - Statistical Catch-at-Age) \n",
    "'6 - Statistical Catch-at-Age'",
    "\n#ASMT_MODEL - Data model accespted by the scientific review process and used to complete the stock assessment, see lookup table (options dependent on ASMT_MODEL_CATEGORY choice: if select 1, then options = Catch Only, DB-SRA: Depletion-Based Stock Reduction Analysis, DCAC: Depletion Corrected Average Catch, Mean Length, or write-in; if select 2, \n",
    "#then options = AIM: An Index Method, Catch Only, Index Method, Survey Abudance; if select 3, then options = ASPIC: A Stock Production Model Incorporating Covariates, BSP: Bayesian Surplus Production Model, BSP-PIFSC; PIFSC Bayesian Surplus Production Model, CSA(Collie Sissenwine): Catch-Survey Analysis, KLAMZ, Schaefer, or write-in; if select 4, \n", 
    "#then options = ADAPT: Adaptive Framework VPA, VPA-2BOX, Dual Zone Virtual Population Analysis, VPA Shrimp: Shrimp Virtual Population Analysis, or write-in; if select 5, then options = A-SCALA: Age-Structured Statistical Catch-at-Length, BAM-Length: Beaufort Statistical Catch-at-Length Model, CASA: Catch-at-Size Analysis, Custom SCAL: Custom Statistical \n",
    "#Catch-at-Length Model, GMACS: General Model for Alaska Crab Stocks, Multifan-CL, SCALE: Statistical Catch-At-LEngth, SS: Stock Synthesis, or write-in; if select 6, then options = AMAK: Assessment Model for Alaska, ASAP: Age-Structured Assessment Program, ASMP: Age-Structured Production Model, BAM: Beaufort Assessment Model, CASAL: C++ Algorithmic Stock \n",
    "#Assessment Library, Coleraine, Custom SCAA: Custom Statistical Catch-at-Age Model, Multifan-CL, PSC Chinook Model, SS: Stock Synthesis) \n",
    "'SS: Stock Synthesis'",
    "\n#MODEL_VERSION - Version of the assessment model accepted by the scientific review process and used to complete the stock assessment \n",
    paste("",paste(nam_mod,collapse=", "),"",sep="'"),
    "\n#ENSEMBLE - Does the final assessment configuration utilize multimodel inferencing, either in the form of some type of model averaging, or ensemble modeling methods? (options: Yes, NA) Please provide additional details in the comments section. \n", 
    ENS,
    "\n#LEAD_LAB - NMFS Laboratory or outside agency with lead responsibility for the stock assessment \n",
    "'AFSC'",
    "\n#POC_EMAIL - Full name of person to contact with questions regarding the assessment (must be in email format) \n",
    email,
    "\n#REVIEW_RESULT - Result of the assessment review after final technical review by NPFMC (options: NA, Not Reviewed, Accept prevoius approach remand new attmept, Full acceptance, Partial acceptance - Fishing mortality estimates, Partial acceptance - Biomass estimates, Partial acceptance - Status determinations only, Reject - Data insufficient for assessment, Reject - Results too uncertain to be considered adequate, Remand) \n",
    "'Full acceptance'",
    "\n#CATCH_INPUT_DATA - Categorical level of catch input data used in the assessment model accepted for use in management, as described in Appendix A of the 2018 Next Gen SAIP document, see lookup table (options: 0 - None, 1 - Major gaps preclude use, 2 - Major gaps in some sectors(s), 3 - Minor gaps across sectors, 4 - Minor gaps in some sector(s), 5 - Near complete knowledge) \n",
     CID,
    "\n#ABUNDANCE_INPUT_DATA - Categorical level of abundance input data used in the assessment model accepted for use in management, as described in Appendix A of the 2018 Next Gen SAIP document, see lookup table (options: 0 - None, 1 - Uncertain or expert opinion, 2 - Standardized fishery-dependent, 3 - Limited fishery-independent, 4 - Comprehensive fishery-independent, 5 - Absolute abundance) \n",
     AID,
    "\n#BIOLOGICAL_INPUT_DATA - Categorical level of biological or life history input data used in the assessment model accepted for use in management, as described in Appendix A of the 2018 Next Gen SAIP document, see lookup table (options: 0 - None, 1 - Proxy-based, 2 - Empirical and proxy-based, 3 - Mostly empirical estimates, 4 - Track changes over time, 5 - Comprehensive over time and space) \n",
     BID,
     "\n#SIZEAGE_COMP_INPUT_DATA - Categorical level of size or age composition input data used in the assessment model accepted for use in management, as described in Appendix A of the 2018 Next Gen SAIP document, see lookup table (options: 0 - None, 1 - Major gaps preclude use, 2 - Suport data-limited only, 3 - Gaps, bus supports age-structured assessment, 4 - Support fishery composition, 5 - Very complete) \n",
     SCID,
     "\n#ECOSYSTEM_LINKAGE - Categorical level of ecosystem linkage used in the assessment model accepted for use in management, as described in Appendix A of the 2018 Next Gen SAIP document, see lookup table (options: 0 - None, 1 - Informative or used to process input data, 2 - Random variation, not mechanistic, 3 - Direct linkages(s), 4 - Linkage(s) informed by process studies, 5 - Fully coupled \n",
      EL,
      "\n#FISHING_MORTALITY_ESTIMATES ----------------------------------------------------------------------------------------\n",
      "\n#F_YEAR - Year of fishing mortality rates (NOTE: report the most recent full year of fishing, should be current year-1, regardless of ASMT_TYPE) \n",  
      endyr-1,
      "\n#F_BASIS - Basis for fishing mortality estimates, F rate or total catch estimates, see lookup table (options: Max F @ Age, F for Fully-Selected Fish, Catch / Biomass, Catch / Exploitable Biomass, Catch, Fishing Intensity, True F, or write-in) \n",
      F_BAS,
      "\n#F_UNIT - The unit of the F related data fields, see lookup table (options: Apical F, Fully-selected F, Exploitation Rate, Relative F, Metric Tons, 1 - SPR, F = Z - M, or write-in) \n",
      F_rep,
       "\n#BEST_F_ESTIMATE - Best available point estimate of fishing mortality rate or total catch used to determine the rate of fishing. This field is used when a point estimate is available. NOTE: for Tiers 1-3, this is the estimate of total fishing mortality from the most recent full year of fishing; for Tiers 4-5 this is exploitation rate (catch divided by total abundance from the random effects model) for the most recent full year of fishing; for Tier 6 this is total catch. \n", 
      F_ap[length(F_ap)-1],
      "\n#F_LIMIT - Fishing mortality rate or total catch threshold, above which the stock is considered to be overfishing. NOTE: For Tiers 1-3 during a full or operational assessment this is the reverse engineered F from this year's accepted model that would have produced a catch for last year equal to last year’s OFL. For Tiers 1-3 during a partial assessment this is FMSY. For Tiers 4-5, single stock assessment, this is the estimate of M. For Tiers 4-5 stock complex assessments or Tier 6, this is sum of the OFL in tons for the stocks in the complex. \n",
      F_LIMIT,
      "\n#F_LIMIT_BASIS - Basis for the Flimit estimate, see lookup table. F rate (F35%, etc.) or total catch estimate (options: Fmsy, Frebuild, M, F30% as proxy, F35% as proxy, F40% as proxy, or write-in) \n",
      F_LIMIT_BASIS,
      "\n#F_MSY - Fishing mortality rate that, on average, would produce the maximum sustainable yield from a stock that has a size of BMSY(e.g. F35%, or FOFL). NOTE: For Tiers 1b, 2b, or 3b this should be the unadjusted estimate of FOFL. \n",
     F_MSY,
     "\n#F_MSY_BASIS - Basis for the Fmsy estimate, see lookup table (options: F40% as proxy, F35% as proxy, F30% as proxy, Direct estmate, F = M, or write-in) \n",
     F_MSY_BASIS,
     "\n#BIOMASS_ESTIMATES -------------------------------------------------------------------------------------------------- \n",
     "#B_YEAR - Year of biomass estimate (NOTE: if ASMT_TYPE = Operational, then B_YEAR = ASMT_YEAR, otherwise this is the year of the last operational or full assessment). \n",
    endyr,
    "\n#B_BASIS - Basis for the biomass estimate, see lookup table (options: Spawning Stock Biomass, Total Stock Biomass, Survey-Estimated Biomass, Escapement, Stock Reproductive Output, Survey Index, Total Stock Abundance, or write-in) \n",
    "'Mature Female Biomass'",
    "\n#B_UNIT - The unit of the B related data fields, see lookup table (options: Metric Tons, Thousand Metric Tons, Number of eggs, kg/tow, Number of Fish, or write-in) \n",
    "'Metric Tons'",
    "\n#BEST_B_ESTIMATE - Best available point estimate of stock size in terms of biomass from the stock assessment model (NOTE: for Tiers 1-3 this is the spawning biomass estimate in B_YEAR, for Tiers 4-5 single stock assessments this is the random effects biomass estimate in B_YEAR, for Tiers 4-5 complex stock assessments this is the combined biomass estimate from the random effects model for the stock complex in B_YEAR, NA for Tier 6) \n",
    spwnbio[length(spwnbio)],
    "\n#LOWER_B_ESTIMATE - Minimum plausible value of B as estimated by the model given the specified confidence interval for spawning biomass in B_YEAR (Tiers 1-3 only) \n",
    round(minb),
    "\n#UPPER_B_ESTIMATE - Maximum plausible value of B as estimated by the model given the specified confidence interval for spawning biomass in B_YEAR (Tiers 1-3 only) \n",
    round(maxb),
    "\n#ESTIMATE_METHOD - Specify statistical approach used to determine confidence intervals if provide (options: Asymptotic, Credible, Bootstrapped, Tiers 1-3 only) \n",
    "'Asymtotic'",
    "\n#INTERVAL_SIZE - Specify size of confidence interval (options: 50 to 99, Tiers 1-3 only) \n",
    95,
    "\n#B_MSY - Stock size that, on average, would produce MSY when it is fished at a level that is equal to F_MSY (Tiers 1-3 only) \n",
    round(ssb_unfished*0.35),
    "\n#B_MSY_BASIS - Basis for B_MSY estimate, see lookup table (options: Direct estimate, S_MSY escapement, Average Survey CPUE, B40%, B35%, B30%, Tiers 1-3 only) \n",
    "'B35%'")

  write(noquote(prolog),paste(getwd(),"/",STOCK,".dat",sep=""),ncolumns=45, append=F)


  data1<-paste0(
    "#TIME_SERIES_ESTIMATES ---------------------------------------------------------------------------------------------- \n",
    "#FISHERYYEAR -list years used in model \n",
    paste(data[[1]]$recruit$Yr[data[[1]]$recruit$era!="Forecast"&data[[1]]$recruit$Yr>=stryr&data[[1]]$recruit$Yr<=data[[1]]$endyr],collapse=" ") ," \n",
    "#AGE - list of ages used in the model where applicable \n",
    paste(data[[1]]$agebins,collapse=" ") ," \n",
    "#RECRUITMENT - Number of recruits by year (Tiers 1-3 only) \n",
    paste(recr,collapse=" ") ," \n",
    "#SPAWNBIOMASS - Spawning biomass by year in metric tons (Tiers 1-3 only) \n",
    paste(spwnbio,collapse=" ") ," \n",
   "#TOTALBIOMASS - Total biomass by year in metric tons (NA for Tier 6) \n",
    paste(bio_tot,collapse=" ") ," \n",
   "#TOTFSHRYMORT - Fishing mortality rate by year (Tiers 1-3 only) \n",
   paste(F_ap,collapse=" ")," \n",
   "#TOTALCATCH - Total catch by year in metric tons \n",
   paste(subset(data[[1]]$timeseries,data[[1]]$timeseries$Yr>=stryr&data[[1]]$timeseries$Yr<=data[[1]]$endyr)$dead.B.._1,collapse=" "))

  write(noquote(data1),paste(getwd(),"/",STOCK,".dat",sep=""),ncolumns=100, append=T)

  write("#SURVEYDESC",paste(getwd(),"/",STOCK,".dat",sep=""),ncolumns=500,append=T)

  write(SURV_DESC,paste(getwd(),"/",STOCK,".dat",sep=""),append=T,ncolumns=100)

 # write("#SURVEYMULT",paste(getwd(),"/",STOCK,".dat",sep=""),append=T,ncolumns=100)
 # write(noquote( paste(paste(input$CPUEinfo$Units[sort(unique(input$CPUE$index[input$CPUE$index>0]))],collapse=" "),paste(" # survey units multipliers"))),paste(getwd(),"/",STOCK,".dat",sep=""),append=T,ncolumns=500)
  write("#STOCKNOTES",paste(getwd(),"/",STOCK,".dat",sep=""),append=T,ncolumns=50)
  write(NOTES,paste(getwd(),"/",STOCK,".dat",sep=""),append=T,ncolumns=100)

   surveys<-sort(unique(input$CPUE$index[input$CPUE$index>0]))
   for( j in 1:max(input$CPUE$seas)){
    for (i in surveys){

       x<-input$CPUE$year[input$CPUE$index==i&input$CPUE$seas==j]
       y<-input$CPUE$obs[input$CPUE$index==i&input$CPUE$seas==j]
       if(length(x)>0){
       write(paste("#",input$fleetnames[i]," - Season ",j,sep=""),paste(getwd(),"/",STOCK,".dat",sep=""),ncolumns=500, append=T)
       write(x,paste(getwd(),"/",STOCK,".dat",sep=""),ncolumns=500, append=T)
       write(y,paste(getwd(),"/",STOCK,".dat",sep=""),ncolumns=500, append=T)
       remove(x)
       remove(y)
       }}}

  }





 #WT=c(0.2842,0.3158,0.2316,0.1684) 
 WT=c(1.00)
 mods<-c("3_2withSlopeMnSAA")
 mods_nam<-c("Model 16.4c")

    ## STOCK - identifier key for joining SARA data, see lookup table for key code:
    ## STOCK_NAME  -  Enter Stock Name:  (e.g. "'EBS Pacific cod'"")
    ## REGION  - Enter Region (AI AK BOG BSAI EBS GOA SEO WCWYK): 
    ## TIER - Enter Tier TIER (1a 1b 2a 2b 3a 3b 4 5 6): 
    ## NOTES - readline("Enter any notes on the assessment: 
    ## MFR - Enter multiplier for recruitment data, N at age, and survey number (1,1000,1000000): 
    ## CID - CATCH_INPUT_DATA - Categorical level of catch input data used in the assessment model accepted for use in management, as described in Appendix A of the 2018 Next Gen SAIP document, see lookup table (options: 0 - None, 1 - Major gaps preclude use, 2 - Major gaps in some sectors(s), 3 - Minor gaps across sectors, 4 - Minor gaps in some sector(s), 5 - Near complete knowledge)
    ## AID - ABUNDANCE_INPUT_DATA - Categorical level of abundance input data used in the assessment model accepted for use in management, as described in Appendix A of the 2018 Next Gen SAIP document, see lookup table (options: 0 - None, 1 - Uncertain or expert opinion, 2 - Standardized fishery-dependent, 3 - Limited fishery-independent, 4 - Comprehensive fishery-independent, 5 - Absolute abundance):
    ## BID - BIOLOGICAL_INPUT_DATA - Categorical level of biological or life history input data used in the assessment model accepted for use in management, as described in Appendix A of the 2018 Next Gen SAIP document, see lookup table (options: 0 - None, 1 - Proxy-based, 2 - Empirical and proxy-based, 3 - Mostly empirical estimates, 4 - Track changes over time, 5 - Comprehensive over time and space):
    ## SCID - SIZEAGE_COMP_INPUT_DATA - Categorical level of size or age composition input data used in the assessment model accepted for use in management, as described in Appendix A of the 2018 Next Gen SAIP document, see lookup table (options: 0 - None, 1 - Major gaps preclude use, 2 - Suport data-limited only, 3 - Gaps, bus supports age-structured assessment, 4 - Support fishery composition, 5 - Very complete):
    ## EL - ECOSYSTEM LINKAGE - Categorical level of ecosystem linkage used in the assessment model accepted for use in management, as described in Appendix A of the 2018 Next Gen SAIP document, see lookup table (options: 0 - None, 1 - Informative or used to process input data, 2 - Random variation, not mechanistic, 3 - Direct linkages(s), 4 - Linkage(s) informed by process studies, 5 - Fully coupled):
    ## F_LIMIT - Fishing mortality rate or total catch threshold, above which the stock is considered to be overfishing. NOTE: For Tiers 1-3 during a full or operational assessment this is the reverse engineered F from this year's accepted model that would have produced a catch for last year equal to last year’s OFL. For Tiers 1-3 during a partial assessment this is FMSY. For Tiers 4-5, single stock assessment, this is the estimate of M. For Tiers 4-5 stock complex assessments or Tier 6, this is sum of the OFL in tons for the stocks in the complex.):
    ## SURV_DESC - The names of the surveys used in this assessment seperated by comas:
    ## see example below...


GET_SARA_ENSEMBLE(dir=getwd(),models=mods,nam_mod=mods_nam,WEIGHT=WT,
	email="'meaghan.bryan@noaa.gov'",Enter_Data=FALSE,STOCK="BSAI GREENLAND TURBOT",STOCK_NAME="'Greenland turbot'",
	REGION="'BSAI'",TIER="'3a'",UPDATE="'benchmark'",COMPLEX="'NA'",CID=5,AID=3,BID=3,SCID=3,EL=2,MFR=1000,F_LIMIT=0.284, F_MSY_PROJ=0.20,
	SURV_DESC=c('EBS Shelf Bottom trawl survey','EBS Slope Bottom trawl survey','AFSC longline survey'),
	NOTES="'2022 accepted model.'",stryr=1945,SS3_proj=FALSE)
