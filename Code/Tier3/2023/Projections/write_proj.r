write_proj<-function(dir="foo", 
                     data_file="Model1_Proj.dat",
                     data=Models[[1]],
                     NSEX=2,NAGE=30,Nfishery=2,fleets=1:2,rec_age=3, max_age=36,FY=1977,rec_FY=1977,rec_LY_decrement=2,
spawn_month=1, Fratios=0.5)
{
	#Writing data file required by proj

	LY=data$Retro_year # last year in the report file is the last year of full model;
							 # need this calc to get end year for the retros

	Y5<-LY-5	## start year to calculate 5-year mean apical F

	rec_LY=LY-rec_LY_decrement ## last year of recruitment used to get mean rec

	write_file=paste0(dir,"/",data_file)

	T1<-noquote(paste(data_file))
	  write(T1,paste(write_file),ncolumns =  1 )

	T1<-noquote(" 0 # SSL Species???")
	  write(T1,paste(write_file),ncolumns = 1,append=T)

	T1<-noquote(" 0 # Constant Buffer Dorn")
	  write(T1,paste(write_file),append = T)

	T1<-noquote(paste(Nfishery, "# Number of fisheries", sep=" "))
	  write(T1,paste(write_file),append = T)

	T1<-noquote(paste(NSEX, "# Number of Sexes",sep=" "))
	  write(T1,paste(write_file),append = T)

	T1<-noquote(paste(mean(data$sprseries$sum_Apical_F[data$sprseries$Yr>Y5 & data$sprseries$Yr<=LY]),"# 5 year average F"))
	  write(T1,paste(write_file),append = T)

	T1<-noquote("1 # Author f")
	  write(T1,paste(write_file),append = T)

	T1<-noquote("0.4 # SPR ABC")
	  write(T1,paste(write_file),append = T)

	T1<-noquote("0.35 # SPR MSY")
	  write(T1,paste(write_file),append = T)

	T1<-noquote(paste(spawn_month, "# Spawning month",sep=" "))
	  write(T1,paste(write_file),append = T)

	T1<-noquote(paste(NAGE,"# number of ages",sep=" "))
	  write(T1,paste(write_file),append = T)

	T1<-noquote(paste(Fratios,"# Fratio",sep=" "))
	  write(T1,paste(write_file),append = T)

	if(NSEX<2)
	{
		print("In NSEX <2")
		T1<-noquote("# natural mortality")
	  		write(T1,paste(write_file),append = T)
	  		print("Writing natural mortality")
	  		if(rec_age==0)
			{
				print("In rec_age==0")
				write(as.numeric(c(subset(data$M_at_age,data$M_at_age$Yr==(LY-1)& data$M_at_age$Sex==NSEX)[,as.character(seq(rec_age,(max_age-1),1))],
				subset(data$M_at_age,data$M_at_age$Yr==(LY-1)&data$M_at_age$Sex==NSEX)[,as.character((max_age-1))])),
				paste(write_file),append = T,ncolumns =  45)
			}
			if(rec_age>0){
				print("In rec_age>0")
				write(as.numeric(subset(data$M_at_age,data$M_at_age$Yr==(LY-1)&data$M_at_age$Sex==NSEX)[,as.character(seq((rec_age-1),(max_age-1),1))]),
				paste(write_file),append = T,ncolumns =  45)
			}

		T1<-noquote("# Maturity females")
		  write(T1,paste(write_file),append = T)
		  print("Writing maturity")
		  if(spp=="GT")write(rep(1,NAGE),paste(write_file),append = T,ncolumns = 45)
	  	  if(spp!="GT") write(round(as.numeric(subset(data$endgrowth,data$endgrowth$Sex==NSEX & data$endgrowth$int_Age %in% rec_age:max_age)[,"Age_Mat"]),4),paste(write_file),append = T,ncolumns =  45)

		T1<-noquote("# wt spawn females")
	 	  write(T1,paste(write_file),append = T)
	 	  print("Writing wt spawn females")
	  	  if(spp=="GT")write(round(as.numeric(subset(data$endgrowth,data$endgrowth$Sex==NSEX & data$endgrowth$int_Age %in% rec_age:max_age)[,"Mat*Fecund"]),4),paste(write_file),append = T,ncolumns =  45)
	  	  if(spp!="GT")write(round(as.numeric(subset(data$endgrowth,data$endgrowth$Sex==NSEX & data$endgrowth$int_Age %in% rec_age:max_age)[,"Wt_Beg"]),4),paste(write_file),append = T,ncolumns =  45)

		T1<-noquote("# WtAge females by fishery")
	  	  write(T1,paste(write_file),append = T)
		  print("Writing WtAge by sex and fishery")
		  for (i in 1:length(fleets))
		  {
		  	write(round(as.numeric(subset(data$ageselex,data$ageselex$Fleet==fleets[i] & data$ageselex$Yr==LY & data$ageselex$Factor=="bodywt")[NSEX,as.character(seq(rec_age,max_age,1))]),4),paste(write_file),append = T,ncolumns =  45)

		  }

		T1<-noquote("# Selectivity by fishery")
		  write(T1,paste(write_file),append = T)
		  print("Writing Selectivity by sex and fishery")
		  for (i in 1:length(fleets))
		  {
		  	write(round(as.numeric(subset(data$ageselex,data$ageselex$Fleet==fleets[i] & data$ageselex$Yr==LY & data$ageselex$Factor=="Asel2")[NSEX,as.character(seq(rec_age,max_age,1))]),4),paste(write_file),append = T,ncolumns =  45)
		  }

		T1<-noquote("# Numbers at age females males")
		  write(T1,paste(write_file),append = T)
		  print("Writing Numbers at age by sex")
	          write(as.numeric(subset(data$natage,data$natage[,"Beg/Mid"]=="B"&data$natage$Yr==LY)[NSEX,as.character(seq(rec_age,max_age,1))]),paste(write_file),append = T,ncolumns =  45)

		Rec_1<-as.numeric(data$natage[,as.character(rec_age)][data$natage$Yr<=rec_LY & data$natage$Yr>=rec_FY & data$natage$Sex==(NSEX) & data$natage[,"Beg/Mid"]=="B"])
		N_rec<-length(Rec_1)
		T1<-noquote("# No Recruitments")
	  	  write(T1,paste(write_file),append = T)
		  print("Writing number of recruit observations")
	          write(N_rec,paste(write_file),append = T,ncolumns =  45)

		T1<-noquote("# Recruitment")
	 	  write(T1,paste(write_file),append = T)
		  print("Writing recruits")
	  	  write(round(Rec_1,1),paste(write_file),append = T,ncolumns =  45)

	}

	if(NSEX>1)
	{
		T1<-noquote("# natural mortality")
	  		write(T1,paste(write_file),append = T)
	  		print("Writing natural mortality")
		for(i in 1:NSEX)
		{
	  		if(rec_age==0)
			{
				write(as.numeric(c(subset(data$M_at_age,data$M_at_age$Yr==(LY-1)&data$M_at_age$Sex==NSEX)[,as.character(seq(rec_age,(max_age-1),1))],
				subset(data$M_at_age,data$M_at_age$Yr==(LY-1)&data$M_at_age$Sex==NSEX)[,as.character((max_age-1))])),
				paste(write_file),append = T,ncolumns =  45)
			}
			if(rec_age>0){
				write(as.numeric(subset(data$M_at_age,data$M_at_age$Yr==(LY-1)&data$M_at_age$Sex==NSEX)[,as.character(seq((rec_age-1),(max_age-1),1))]),
				paste(write_file),append = T,ncolumns =  45)
			}
		}

		T1<-noquote("# Maturity females")
			write(T1,paste(write_file),append = T)
			print("Writing female maturity")
		  	if(spp=="GT")write(rep(1,NAGE),paste(write_file),append = T,ncolumns = 45)
	  	  	if(spp!="GT") write(round(as.numeric(subset(data$endgrowth,data$endgrowth$Sex==1 & data$endgrowth$int_Age %in% rec_age:max_age)[,"Age_Mat"]),4),paste(write_file),append = T,ncolumns =  45)

		T1<-noquote("# Maturity males")
			write(T1,paste(write_file),append = T)
			print("Writing male maturity")
		  	if(spp=="GT")write(rep(1,NAGE),paste(write_file),append = T,ncolumns = 45)
	  	  	if(spp!="GT") write(round(as.numeric(subset(data$endgrowth,data$endgrowth$Sex==2 & data$endgrowth$int_Age %in% rec_age:max_age)[,"Age_Mat"]),4),paste(write_file),append = T,ncolumns =  45)

		T1<-noquote("# wt spawn females")
	 	  write(T1,paste(write_file),append = T)
	 	  print("Writing wt spawn females")
	  	  if(spp=="GT")write(round(as.numeric(subset(data$endgrowth,data$endgrowth$Sex==1 & data$endgrowth$int_Age %in% rec_age:max_age)[,"Mat*Fecund"]),4),paste(write_file),append = T,ncolumns =  45)
	  	  if(spp!="GT")write(round(as.numeric(subset(data$endgrowth,data$endgrowth$Sex==1 & data$endgrowth$int_Age %in% rec_age:max_age)[,"Wt_Beg"]),4),paste(write_file),append = T,ncolumns =  45)

		T1<-noquote("# WtAge by sex and fishery")
		  write(T1,paste(write_file),append = T)
		  print("Writing wtAge by sex and fishery")
		  for(j in 1:NSEX)
		  {
		  	for (i in 1:length(fleets))
		  	{
		  		write(round(as.numeric(subset(data$ageselex,data$ageselex$Fleet==fleets[i] & data$ageselex$Yr==LY & data$ageselex$Factor=="bodywt")[j,as.character(seq(rec_age,max_age,1))]),4),paste(write_file),append = T,ncolumns =  45)
		  	}
		  }

		T1<-noquote("# Selectivity by sex and fishery")
		  write(T1,paste(write_file),append = T)
		  print("Writing selectivity by sex and fishery")
		  for(j in 1:NSEX)
		  {
		  	for (i in 1:length(fleets))
		  	{
		  		write(round(as.numeric(subset(data$ageselex,data$ageselex$Fleet==fleets[i] & data$ageselex$Yr==LY & data$ageselex$Factor=="Asel2")[j,as.character(seq(rec_age,max_age,1))]),4),paste(write_file),append = T,ncolumns =  45)
		  	}
		  }

		T1<-noquote("# Numbers at age females males")
		  write(T1,paste(write_file),append = T)
		  print("Writing Numbers at age females males")
		  for(j in 1:NSEX)
		  {
	          	write(as.numeric(subset(data$natage,data$natage[,"Beg/Mid"]=="B"&data$natage$Yr==LY)[j,as.character(seq(rec_age,max_age,1))]),paste(write_file),append = T,ncolumns =  45)
		  }

		print("Getting number of recruits")
		Rec_1<-as.numeric(data$natage[,as.character(rec_age)][data$natage$Yr<=rec_LY & data$natage$Yr>=rec_FY & data$natage$Sex==(NSEX-1) & data$natage[,"Beg/Mid"]=="B"]
			+data$natage[,as.character(rec_age)][data$natage$Yr<=rec_LY & data$natage$Yr>=rec_FY & data$natage$Sex==NSEX & data$natage[,"Beg/Mid"]=="B"])
		print("Getting number of recruit obs")
		N_rec<-length(Rec_1)
		T1<-noquote("# No Recruitments")
	  	  write(T1,paste(write_file),append = T)
	  	  print("Writing number of recruit obs")
	          write(N_rec,paste(write_file),append = T,ncolumns =  45)

		T1<-noquote("# Recruitment")
	 	  write(T1,paste(write_file),append = T)
	 	  print("Writing number of recruits")
	  	  write(round(Rec_1,1),paste(write_file),append = T,ncolumns =  45)


	}

	T1<-noquote(paste("# SSB ", FY,"-",LY,sep=""))
	  write(T1,paste(write_file),append = T)
	  print("Writing SSB")
	  if(spp!="EBS_Pcod") write(as.numeric(data$sprseries$SSB[data$sprseries$Yr<=LY&data$sprseries$Yr>=FY]),paste(write_file),append = T,ncolumns =  45)
	  if(spp=="EBS_Pcod") write(as.numeric(data$sprseries$SSB[data$sprseries$Yr<=LY&data$sprseries$Yr>=FY])/2,paste(write_file),append = T,ncolumns =  45)
	file.copy(write_file, file.path(dir),overwrite=TRUE)

}


