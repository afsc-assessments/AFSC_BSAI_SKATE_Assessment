  #write updated catch estimates to species.dat file that is required to run projections
  write_proj_spcat<-function(dir="foo",data_file="Model_Proj_spcat.dat",data=Models[[1]],ct_yrs=2, catch_vector_use = catchvec)
  {
	  write_file=paste0(dir,"/",data_file)
	  
	  write(noquote("#_SETUP_FILE_FOR_THE_BERING_SEA/AI_FISHERIES"),file=write_file)

	  write(noquote("#_Number_of_years	with	specified	catch	(if	begin-yr	=	2005,	and	this	number	is	3,	then	subsequent	values	represent	catches	in	2005,	06,	and	7	(to	    evaluate	alts	for	2008"),file=write_file,append=TRUE)										
	  write(ct_yrs,file=write_file,append=TRUE)																																					
	  write(noquote("#	Number	of	runs"),file=write_file,append=TRUE)																																		
	  write(1,file=write_file,append=TRUE)																																					

	  write(noquote("#	OY	Minimum"),file=write_file,append=TRUE)																																			
	  write(paste(01343.248,	noquote("#	Note	that	this	is	for	age-structured	species	1330.148"),sep=" "),file=write_file,append=TRUE)																				

	  write(noquote("#	OY	Maximum"),file=write_file,append=TRUE)																																			
	  write(paste(1943.248,	noquote("#	Note	that	this	is	for	age-structured	species	1930.148"),sep=" "),file=write_file,append=TRUE)

	  write(noquote("#	data	files	for	each	species"),file=write_file,append=TRUE)																															
	  write(noquote("#	Pollock	Pacific	cod	sablefish	Yellowfin	Greenland	turbot	arrowtooth	flounder	Rock	sole	Flathead	sole	AK	Plaice	Pacific	ocean	perch	Nrthrn	RF	Atka	mackerel"),file=write_file,append=TRUE)												
	  write(noquote("#	1	2	3	4	5	6	7	8	9	10	11	12"),file=write_file,append=TRUE)																									

	  write(noquote("#	data	files	for	each	species"),file=write_file,append=TRUE)
	  write(noquote("Model_Proj.dat"),file=write_file,append=TRUE)																				

	  write(noquote("#	ABC	Multipliers	"),file=write_file,append=TRUE)																																					
	  write(1,file=write_file,append=TRUE)																																					

	  write(noquote("#	Population	scalars"),file=write_file,append=TRUE)																																			
	  write(1,file=write_file,append=TRUE)																																					

	  write(noquote("#	New	Alt	4	Fabc	SPRs	(Rockfish	=	0.75,	other	0.6),	Steller	sea	lion	prey	species	between	F40	and	F60	(to	meet	OY	Min)"),file=write_file,append=TRUE)										
	  write(0.75,file=write_file,append=TRUE)	

	  write(noquote("#	Number	of	TAC	model	categories"),file=write_file,append=TRUE)																																					
	  write(1,file=write_file,append=TRUE)																															

	  write(noquote("#	TAC	model	indices	(for	aggregating)"),file=write_file,append=TRUE)																																			
	  write(1,file=write_file,append=TRUE)																															

	  write(noquote("#	Catch	in	each	future	year"),file=write_file,append=TRUE)
	  	LY=data$Retro_year
	  
	  #Set catch to last year's catch if this year's catch is 0
	  write(paste(LY, sum(data$catch$Obs[data$catch$Yr==(LY)])),file=write_file,append=TRUE)
	  if(ct_yrs>1)
	  {
	  	for(i in 2:ct_yrs)
	  	{
	  	  write(paste(LY+(i-1), catch_vector_use$catches[catch_vector_use$year==(LY+(i-1))]),file=write_file,append=TRUE)
	  	}
	  }
	  
	  file.copy(write_file, file.path(dir),overwrite=TRUE)
  }
  