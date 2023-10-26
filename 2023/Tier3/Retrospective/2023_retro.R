library(r4ss)
library(readr)

setwd("C:/testing_ground/AKsk_retro_2023")
# Run retrospective analysis----
# Note: this was run in a separate folder following guidance from package instructions, copied to .Rproj directory
M14_2_ret <-retro(dir = paste0(getwd(), "/M14_2_update"),
                      oldsubdir = "",
                      newsubdir = "retrospectives",
                      subdirstart = "retro",
                      years = 0:-13,
                      verbose = TRUE,
                      exe = "ss_win.exe")


# Mohns Rho calc ----
setwd(paste0(getwd(), "/M14_2_update/retrospectives"))
Models1 <- SSgetoutput(dirvec = c("retro0", "retro-1", "retro-2", 'retro-3', 'retro-4', 'retro-5', 'retro-6',
                                  'retro-7', 'retro-8', 'retro-9', 'retro-10', 'retro-11', 'retro-12'))
Models1_SS <- SSsummarize(ret_out)

MR_out <- SSmohnsrho(Models1_SS)
MR_out <- as.data.frame(unlist(MR_out))
MR_out$Metric <- rownames(MR_out)
write_csv(MR_out, paste0(getwd(), "/Mohns_Rho_M14_2.csv"))

# Plot retrospective----
#endyrvec <- Models1_SS[["endyrs"]]+ 0:-10
#SSplotComparisons(ret_summ,
#                  endyrvec = endyrvec,
#                  print = TRUE,
#                  plotdir = here::here(getwd()),
#                  subplots = c(1, 9),
#                  legend = F)

#recreation of previous authors method ----
endyrvec <-Models1_SS$endyrs + 0:-12
SSplotComparisons(Models1_SS, endyrvec=endyrvec, legendlabels=paste("Data",0:-12,"years"))
SSplotComparisons(Models1_SS, endyrvec=endyrvec, legendlabels=paste("Data",0:-11,"years"))

# Plot retrospective----
endyrvec <- ret_summ[["endyrs"]]+ 0:-10
SSplotComparisons(ret_summ,
                  endyrvec = endyrvec,
                  print = TRUE,
                  plotdir = here::here(getwd()),
                  subplots = c(1, 9),
                  legend = F)

model1<-SS_output(paste(getwd(),"/retro0",sep=""), forecast=FALSE)
YEARS<-model1$timeseries$Yr

ssb<-data.frame(YEAR=YEARS[3:length(YEARS)],value=model1$derived_quants[3:length(YEARS),2],sd=model1$derived_quants[3:length(YEARS),3])
rec<-data.frame(YEAR=YEARS[3:length(YEARS)],value=model1$timeseries$Recruit_0[3:length(YEARS)],sd=subset(model1$derived_quants,strtrim(model1$derived_quants[,1],4)=="Recr")[3:length(YEARS),3])
N_SSB<-nrow(ssb)
sab<-matrix(ncol=15,nrow=N_SSB)
sab[,14]<-ssb$value
sab[,15]<-ssb$sd
sab[,1]<-ssb$YEAR

j<-c(13:2)
for(i in 1:12){
  sab[1:(N_SSB-i),j[i]] <- Models1[[(i+1)]]$timeseries$SpawnBio[Models1[[(i+1)]]$timeseries$Yr>=min(YEARS+2) &
                                                                  Models1[[(i+1)]]$timeseries$Yr<=(max(YEARS)-i)]
}

rac <-matrix(ncol=15,nrow=N_SSB)
rac[,15]<-rec$sd
rac[,14]<-rec$value
rac[,1] <-rec$YEAR
for(i in 1:12){
  rac[1:(N_SSB-i),j[i]]<- Models1[[(i+1)]]$timeseries$Recruit_0[Models1[[(i+1)]]$timeseries$Yr>=min(YEARS+2)& Models1[[(i+1)]]$timeseries$Yr<=(max(YEARS)-i)]
}

SAB=data.frame(sab)
names(SAB)<-c("Year",paste("M_",seq(12,0,-1)),"M_0SD")
SAB<-subset(SAB,SAB$Year>=1977)

RAC=data.frame(rac)
names(RAC)<-c("Year",paste("M_",seq(12,0,-1)),"M_0SD")
RAC<-subset(RAC,RAC$Year>=1977)
RAC
S_N<-nrow(SAB)

#Rho calcs ----
x<-array(dim=12)
for(i in 1:12){
  x[i]<-(SAB[(S_N-i),(14-i)]-SAB[(S_N-i),14])/SAB[(S_N-i),14]
}

rho=sum(x)/12

x<-matrix(ncol=12,nrow= S_N)
for(i in 1:12){
  x[1:( S_N -i),i]<-(SAB[1:( S_N -i),(14-i)]-SAB[1:( S_N -i),14])/SAB[1:( S_N -i),14]
}
y<-array(dim=12)
for( i in 1:12){
  y[i]<-sum(x[,i][!is.na(x[,i])]/length(x[,i][!is.na(x[,i])]))
}
WH_rho<-sum(y)/12
WH_rho

x<-matrix(ncol=12,nrow= S_N)
for(i in 1:12){
  x[1:( S_N -i),i]<-(log(SAB[1:( S_N -i),(14-i)])-log(SAB[1:( S_N -i),14]))^2
}
RMSE=sqrt(sum(x[!is.na(x)])/length(x[!is.na(x)]))
RMSE

S_N<-nrow(RAC)
x<-array(dim=12)
for(i in 1:12){
  x[i]<-(RAC[(S_N-i),(14-i)]-RAC[(S_N-i),14])/RAC[(S_N-i),14]
}
R_rho=sum(x)/12
R_rho
x<-matrix(ncol=12,nrow= S_N)
for(i in 1:12){
  x[1:( S_N -i),i]<-(RAC[1:( S_N -i),(14-i)]-RAC[1:( S_N -i),14])/RAC[1:( S_N -i),14]
}
y<-array(dim=12)
for( i in 1:12){
  y[i]<-sum(x[,i][!is.na(x[,i])]/length(x[,i][!is.na(x[,i])]))
}
R_WH_rho <- sum(y)/12
R_WH_rho
x<-matrix(ncol=12,nrow= S_N)
for(i in 1:12){
  x[1:( S_N -i),i]<-(log(RAC[1:( S_N -i),(14-i)])-log(RAC[1:( S_N -i),14]))^2
}
R_RMSE=sqrt(sum(x[!is.na(x)])/length(x[!is.na(x)]))
R_RMSE


