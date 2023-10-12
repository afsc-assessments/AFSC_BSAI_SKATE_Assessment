 setup<-function(dir="foo", data_file="setup.dat",data=Models[[1]],nsims=100,nproj=20)
 {
	 LY=data$Retro_year	
	 
	 write_file=paste0(dir,"/",data_file)
	 #write set-up.dat required to run projection
	  write(paste(noquote("std"), noquote("#No idea what this is"),sep=" "),file=write_file)
	  write(paste(7, noquote("#Number of alternatives???"),sep=" "),file=write_file,append=TRUE)
	  write(paste(1, noquote("#Vector of alt numbers"),sep=" "),file=write_file,append=TRUE)
	  write(2,file=write_file,append=TRUE)
	  write(3,file=write_file,append=TRUE)
	  write(4,file=write_file,append=TRUE)
	  write(5,file=write_file,append=TRUE)
	  write(6,file=write_file,append=TRUE)
	  write(7,file=write_file,append=TRUE)
	  write(paste(1,noquote("#TAC_ABC"),sep=" "),file=write_file,append=TRUE)
	  write(paste(1,noquote("#SRType"),sep=" "),file=write_file,append=TRUE)
	  write(paste(1,noquote("#Rec_Gen"),sep=" "),file=write_file,append=TRUE)
	  write(paste(1,noquote("#Fmsy_F35"),sep=" "),file=write_file,append=TRUE)
	  write(paste(0,noquote("#Rec_Cond"),sep=" "),file=write_file,append=TRUE)
	  write(paste(1,noquote("#Write_Big"),sep=" "),file=write_file,append=TRUE)
	  write(paste(nproj,noquote("#NProj"),sep=" "),file=write_file,append=TRUE)
	  write(paste(nsims,noquote("#Nsims"),sep=" "),file=write_file,append=TRUE)
	  write(paste(LY,noquote("#Styr"),sep=" "),file=write_file,append=TRUE)

	  #create copy in data folder
	  file.copy(write_file, file.path(dir),overwrite=TRUE)
  }