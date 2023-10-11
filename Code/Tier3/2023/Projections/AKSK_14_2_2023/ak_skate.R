#--Projections---------
library(tidyverse)
pdt <- read_csv("spm_detail.csv" ) %>% mutate(Alternative=as.factor(Alt))
names(pdt)
glimpse(pdt)
#[1] "Alternative" "SpNo"        "Spp"         "Yr"          "ABC"        
# [6] "OFL"         "Catch"       "SSB"         "F"           "Tot_biom"   
#[11] "SPR_Implied" "Ntot"        "SexRatio"    "FABC"        "FOFL"       
#[16] "B0"          "B40"         "B35"        
library(ggthemes)
library(scales)
head(pdt)
nsims=1000
sim=rep(rep(1:15,nsims),7)
length(sim)
dim(pdt)
pdt$Sim=sim
tt <- (sample(nsims, size=30))
mtmp <- pdt %>% filter(Sim %in% tt) %>% transmute(Yr,sim=as.factor(Sim),SSB,Catch,Alternative) 
mtmp
#p1 <- p1 + facet_grid(Area~.,scales="free") + geom_line(data=mtmp,aes(group=sim),size=.2,col=SSB) 

p1 <- pdt %>% filter(Alt==2) %>% mutate(Year=Yr) %>% ggplot(aes(x=Year,y=Catch),fill="salmon") + 
        expand_limits(y = 0) + scale_y_continuous(labels = comma) +
        ggplot2::stat_summary(fun.min = function(x) quantile(x, 0.05), fun.max = function(x) quantile(x, 0.95), geom = "ribbon", alpha = 0.25, colour = NA) +
        ggplot2::stat_summary(fun = function(x) quantile(x, 0.5), geom = "line", lwd = 1) +
        ggplot2::stat_summary(fun = function(x) quantile(x, 0.5), geom = "point", size = 1) +
        theme_few()+ labs(x="Year",y='Catch (t)')    #+ geom_line(data=pdt,aes(x=Yr,y=B40))
       p1 <- p1 + geom_line(data=mtmp,aes(x=Yr,y=Catch),size=.2,col="grey40") 
p1
p1 <- pdt %>% filter(Alt==2) %>% mutate(Year=Yr) %>% ggplot(aes(x=Year,y=SSB,fill=Alternative)) + 
        expand_limits(y = 0) + 
        scale_y_continuous(labels = comma) +
        ggplot2::stat_summary(fun.min = function(x) quantile(x, 0.05), fun.max = function(x) quantile(x, 0.95), geom = "ribbon", alpha = 0.25, colour = NA) +
        ggplot2::stat_summary(fun = function(x) quantile(x, 0.5), geom = "line", lwd = 1) +
        ggplot2::stat_summary(fun = function(x) quantile(x, 0.5), geom = "point", size = 1) +
        theme_few()+ labs(x="Year",y='SSB (t)')    + geom_line(data=pdt,aes(x=Yr,y=B40))
p1

pt <- pdt[Yr>2019,.(Catch=mean(Catch),ABC=mean(ABC),OFL=mean(OFL),SSB=median(SSB) ,lb=quantile(SSB,.2) ,ub=quantile(SSB,.8) ),.(Yr,Alternative)]
pt
ggplot(pt,aes(x=Yr,y=SSB,fill=Alternative)) + geom_line() + mytheme + ylim(c(0,400)) + geom_ribbon(aes(ymin=lb,ymax=ub,fill=Alternative),alpha=0.25) + labs(y="Spawning biomass (kt)",x="Year") + scale_x_continuous(breaks=seq(2015,2032,2)) 
c1 <- ggplot(pt,aes(x=Yr,y=Catch,color=Alternative,size=1.)) + geom_line(size=1.5) + mytheme + labs(y="Catch (kt)",x="Year") + scale_x_continuous(breaks=seq(2015,2032,2)) 
c1 <- c1 + geom_line(aes(x=Yr,y=ABC),size=1)
#c1 <- c1 + geom_line(data=pt[as.numeric(Alternative)==2,.(Yr,ABC)],aes(x=Yr,y=ABC))
c1
pt[as.numeric(Alternative)==2,.(Yr,ABC)]
pt <- pdt[Yr>2016,.(Catch=mean(Catch),ABC=mean(ABC),OFL=mean(OFL)),.(Yr,Alternative)] 
pt
ggplot(pt,aes(x=Yr,y=OFL,color=Alternative)) + geom_line() + mytheme
pdt
pdx <-rbind(pdt2,pdt)
setkey(pdx,Yr,Alternative)
pt <- pdx[.(Yr>2016,(Alternative)==1),.(Catch=mean(Catch),ABC=mean(ABC),OFL=mean(OFL),SSB=median(SSB)  ),.(Yr,config)]
pt <- pdx[Yr>2016&Alternative==1,.(Catch=mean(Catch),ABC=mean(ABC),OFL=mean(OFL),SSB=median(SSB) ,lb=quantile(Catch,.1) ,ub=quantile(Catch,.9)  ),.(Yr,config)]
pt

  # write some sims out for easy use later...
  bfs <- pdt %>% filter(Sim<=30)
  head(bfs)
  write.csv(bfs,"data/proj.csv")
 # head(bfs)
  bfss <- bfs %>% filter(Alternative==2) %>% transmute(Alternative,Yr,Catch,SSB,Sim=as.factor(Sim)) 
  pf <- data.frame(read.table("mod16b/proj/percentdb.out",header=F) )
  names(pf) <- c("stock","Alt","Yr","variable","value") 
  thisyr=2020
  dev.off()
  p1 <- pf %>% filter(substr(variable,1,1)=="C",variable!="CStdn",Alt==2) %>% select(Yr,variable,value) %>% spread(variable,value) ;p1#%>%
    ggplot(p1,aes(x=Yr,y=CMean),width=1.2) + geom_ribbon(aes(ymax=CUCI,ymin=CLCI),fill="goldenrod",alpha=.5) + theme_few() + geom_line() +
    scale_x_continuous(breaks=seq(thisyr,thisyr+14,2))  +  xlab("Year") + ylim(0,200000) + ylab("Tier 3 ABC (kt)") + geom_point() + 
    geom_line(aes(y=Cabc)) + geom_line(aes(y=Cofl),linetype="dashed") + geom_line(data=bfss,aes(x=Yr,y=Catch,col=as.factor(Sim)))+ guides(size=FALSE,fill=FALSE,alpha=FALSE,col=FALSE) 
  p2 <- pf %>% filter(substr(variable,1,1)=="S",variable!="SSBStdn",Alt==2) %>% select(Yr,variable,value) %>% spread(variable,value) %>%
    ggplot(aes(x=Yr,y=SSBMean),width=1.2) + geom_ribbon(aes(ymax=SSBUCI,ymin=SSBLCI),fill="coral",alpha=.5) + theme_few() + geom_line() +
    scale_x_continuous(breaks=seq(thisyr,thisyr+14,2))  +  xlab("Year") + ylim(0,5000) + ylab("Tier 3 Spawning biomass (kt)") + geom_point() + 
    geom_line(aes(y=SSBFabc)) + geom_line(aes(y=SSBFofl),linetype="dashed")+ geom_line(data=bfss,aes(x=Yr,y=SSB,col=as.factor(Sim)))+ guides(size=FALSE,fill=FALSE,alpha=FALSE,col=FALSE) 
  #t3 <- grid.arrange(p1, p2, nrow=2)
    library(patchwork)
    p1
  t3 <- p1/ p2
  print(t3)
  ggsave("figs/tier3_proj.pdf",plot=t3,width=5.4,height=7,units="in")
}

ggplot(pt,aes(x=Yr,y=Catch,color=Alternative)) + geom_line() + geom_point() + mytheme + geom_ribbon(aes(ymin=lb,ymax=ub,fill=config),alpha=0.25) + labs(x="Year")+ scale_x_continuous(breaks=seq(2015,2032,2))  + ylim(c(0,130))

#system("./amak -mceval")
#sdf <- read.table("estM/mceval_srv.dat")

sdf <- tibble(read.table("dbl_log/mceval_srv.dat"),mod="3par")
sdf <- rbind(sdf,tibble(read.table("mod16b/mceval_srv.dat"),mod="Non-parametric"))
sdf <- rbind(sdf,tibble(read.table("estM/mceval_srv.dat"),mod="3par, est. M"))
names(sdf)<- c("Year","obs","mean","sim","draw","var","mod")
head(sdf)
# How I read Oleary et al 2018
sdf %>% group_by(Year,mod) %>%
      summarise(mean_sim=mean(sim),var_sim=var(sim),
        ppl=mean(-log(
        exp(log(obs/mean_sim))^2 / (2*var_sim))  /
        obs*sqrt(var_sim) )) %>% ungroup() %>% group_by(mod) %>%summarize(sum(ppl))
# How I read Hooten and Hobbs:
sdf %>% group_by(Year,mod) %>%
        summarise(mean_sim=mean(log(sim)),var_sim=var(log(sim)),
        ppl=sum( (log(obs)-mean_sim)^2 + var_sim) ) %>%
        ungroup() %>% group_by(mod) %>%summarize(sum(ppl))

sdf2 <- sdf %>% filter(draw==1)
ggplot(sdf2,aes(x=Year,y=obs)) + 
   geom_point(data=sdf,aes(x=jitter(Year),y=sim), size=.5,alpha=.2,color="grey") + 
   geom_point(data=sdf,aes(x=jitter(Year,.5),y=mean), size=.5,alpha=.2,color="yellow") + 
   scale_y_continuous(breaks=seq(0,1.5e6,by=5e5),limits=c(0,1.5e6))  +
   scale_x_continuous(breaks=seq(1990,2020,by=2))  +
   theme_few() + ylab("Survey estimates") +
    geom_line(size=2,color="salmon") + geom_point(size=4,color="red",shape=3) +
    facet_grid(mod~.)
    ggsave("estM.png")
head(sdf)

sdf <- read.table("estM/mceval_M.dat") 
names(sdf)<- c("Year","M","x")
tail(sdf)
sdf <- sdf %>%filter(Year==2020) %>% mutate(source="Posterior")
tmp <- tibble(Year=2020,M=rlnorm(10000,log(0.3),0.1),x=0.3,source="Prior") 
sdf <- rbind(sdf,tmp)
#%>%filter(Year==2020) %>% mutate(source="Posterior")
dim(sdf)
dim(tmp)
ggplot(sdf,aes(x=M,fill=source)) + geom_density(alpha=.2) + theme_few() + geom_vline(xintercept=mean(sdf$M)) +
       geom_vline(xintercept=0.3, color = "grey",size=1,linetype=2)


sdf <- read.table("estM/mceval.dat",skip=0)
head(sdf)
getwd()
sdf <- read.table("mceval_sr.dat")
head(sdf)
names(sdf)<- c("Source","Stock","Recruits")
dim(sdf)
sdf2<-sdf %>% sample_n(100000)
ggplot(sdf2,aes(x=Stock,y=Recruits,color=Source)) + 
   theme_few() + geom_point(size=4,alpha=.1) 
   head(sdf)
   ylab("Survey estimates") +
   scale_y_continuous(breaks=seq(0,12e6,by=4e5))  +
   scale_x_continuous(breaks=seq(1990,2020,by=2))  +

