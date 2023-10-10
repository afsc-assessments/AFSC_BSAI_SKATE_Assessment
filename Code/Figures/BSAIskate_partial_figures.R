# Shark Assessment Maine Document Figures ----
# Updated 10/24/2022 by C. Tribuzio

# Setup ----
datadir<-paste(getwd(),"/Data/Annual_updates/",AYR,sep="")
cleandatdir<-paste(getwd(),"/Data/Cleaned/",AYR,sep="")
figdir<-paste(getwd(),"/Output/",AYR,"/Figures/",sep="")
dir.create(figdir, showWarnings = T)
outdir <- paste0(getwd(),"/Output/",AYR)

# ggplot themes
theme_doc<- function(base_size = 12, base_family = "Helvetica") { #this function sets the theme for the whole figure
  theme_bw(base_size = base_size, base_family = base_family) %+replace% #also note that this creates a bunch of font warnings that are not a real problem, I just haven't dealt with it yet
    theme(
      plot.title=element_text(size=20,colour='black',hjust = 0.5),
      plot.background=element_blank(),
      panel.grid.minor=element_blank(),
      panel.grid.major=element_line(color="grey90"),
      panel.border = element_blank(),
      panel.background = element_blank(),
      axis.line.x = element_line(colour='black'),
      axis.line.y = element_line(colour='black'),
      axis.text=element_text(size=12,colour='black'),
      axis.ticks=element_line(colour='black'),
      axis.title.y=element_text(colour='black',angle=90),
      axis.title.x=element_text(colour='black'),
      legend.background=element_blank(),
      legend.text=element_text(colour='black',size=12),
      legend.title=element_text(colour='black',size=12),
      strip.background=element_blank(),
      strip.text=element_text(size=20,colour='black')
    )
}

# 18.1 Survey Biomass ----
RACEdat <- read_csv(paste0(getwd(),"/Data/Annual_updates/",AYR,"/RACE_shelfNW_skate_biom.csv",sep="")) %>% 
  clean_names()

allspec <- ggplot(RACEdat, aes(x = year, y = stratum_biomass/1000, color = species_common_name, fill = species_common_name))+
  geom_bar(stat = "identity")+
  scale_fill_brewer(palette = "Set1", direction = -1, name = "Species")+
  scale_color_brewer(palette = "Set1", direction = -1, name = "Species")+
  scale_y_continuous(expand = c(0,0))+
  labs(x = "", y = "")+
  theme_doc()+
  theme (legend.position = "none")

sansAK <- RACEdat %>% 
  filter(species_common_name != "Alaska skate")
noak <- ggplot(sansAK, aes(x = year, y = stratum_biomass/1000, color = species_common_name, fill = species_common_name))+
  geom_bar(stat = "identity")+
  scale_fill_brewer(palette = "Set1", direction = -1, name = "Species")+
  scale_color_brewer(palette = "Set1", direction = -1, name = "Species")+
  scale_y_continuous(expand = c(0,0))+
  labs(x = "", y = "")+
  theme_doc()+
  theme (legend.position = "none")

biomleg <-ggplot(RACEdat, aes(x = year, y = stratum_biomass/1000, color = species_common_name, fill = species_common_name))+
  geom_bar(stat = "identity")+
  scale_fill_brewer(palette = "Set1", direction = -1, name = "Species")+
  scale_color_brewer(palette = "Set1", direction = -1, name = "Species")+
  scale_y_continuous(expand = c(0,0))+
  labs(x = "", y = "")+
  theme_doc()
legend_doc = gtable_filter(ggplot_gtable(ggplot_build(biomleg)), "guide-box") 
grid.draw(legend_doc)

biom_fig <- grid.arrange(arrangeGrob(allspec, noak, nrow = 2,
                                       bottom = textGrob("Year", vjust = -0.5,gp=gpar(col="black", fontsize=12)),
                                       left = textGrob("Biomass (1000 t)", rot = 90, vjust = 1 ,gp=gpar(col="black", fontsize=12))), 
                           legend_doc, widths=unit.c(unit(1, "npc") - legend_doc$width, legend_doc$width), 
                           nrow=1)
ggsave(path = figdir, "18.1_biomass.png",plot=biom_fig,dpi=600,width = 10, height = 11)


# 18.2 harvets ratios----
specdat <- read_csv(paste0(getwd(),"/Data/harvest_ratios.csv",sep="")) %>% 
  clean_names() %>% 
  mutate(catchABC = catch/abc,
         catchbiom = catch/biomass)
ratio_mean <- specdat %>% 
  group_by(group) %>% 
  summarise(LTmean = mean(catchbiom))
curr_mean <- specdat %>% 
  filter(year == AYR)

cABC <- ggplot(specdat, aes(x = year, y = catchABC, color = group))+
  geom_point(size = 4)+
  geom_line()+
  scale_fill_brewer(palette = "Dark2", name = "Group")+
  scale_color_brewer(palette = "Dark2", name = "Group")+
  labs(x = "", y = "Catch:ABC")+
  scale_x_continuous(breaks=seq(2011,max(specdat$year),2))+
  theme_doc()+
  theme (legend.position = "none")

cbiom <- ggplot(specdat, aes(x = year, y = catchbiom, color = group))+
  geom_point(size = 4)+
  geom_line()+
  scale_fill_brewer(palette = "Dark2", name = "Group")+
  scale_color_brewer(palette = "Dark2", name = "Group")+
  geom_hline(aes(yintercept = 0.0389), color = "#1B9E77", linetype = "dashed")+
  geom_hline(aes(yintercept = 0.0373), color = "#D95F02", linetype = "dashed")+
  geom_hline(aes(yintercept = 0.0481), color = "#7570B3", linetype = "dashed")+
  labs(x = "", y = "Catch:Biomass")+
  scale_x_continuous(breaks=seq(2011,max(specdat$year),2))+
  theme_doc()+
  theme (legend.position = "none")

ratioleg <- ggplot(specdat, aes(x = year, y = catchbiom, color = group))+
  geom_point()+
  geom_line()+
  scale_fill_brewer(palette = "Dark2", name = "Group")+
  scale_color_brewer(palette = "Dark2", name = "Group")+
  geom_hline(aes(yintercept = 0.0389), color = "#1B9E77", linetype = "dashed")+
  geom_hline(aes(yintercept = 0.0373), color = "#D95F02", linetype = "dashed")+
  geom_hline(aes(yintercept = 0.0481), color = "#7570B3", linetype = "dashed")+
  labs(x = "Year", y = "Catch:Biomass")+
  scale_x_continuous(breaks=seq(2011,max(specdat$year),2))+
  theme_doc()
legend_doc = gtable_filter(ggplot_gtable(ggplot_build(ratioleg)), "guide-box") 
grid.draw(legend_doc)

ratio_fig <- grid.arrange(arrangeGrob(cABC, cbiom, nrow = 2,
                                     bottom = textGrob("Year", vjust = -0.5,gp=gpar(col="black", fontsize=12))),
                         legend_doc, widths=unit.c(unit(1, "npc") - legend_doc$width, legend_doc$width), 
                         nrow=1)
ggsave(path = figdir, "18.2_ratios.png",plot=ratio_fig,dpi=600,width = 10, height = 11)

cBiom_fig <- grid.arrange(arrangeGrob(cbiom, nrow = 1,
                                      bottom = textGrob("Year", vjust = -0.5,gp=gpar(col="black", fontsize=12))),
                          legend_doc, widths=unit.c(unit(1, "npc") - legend_doc$width, legend_doc$width), 
                          nrow=1)
ggsave(path = figdir, "CBiomass_ratios.png",plot=cBiom_fig,dpi=600,width = 6, height = 5)
