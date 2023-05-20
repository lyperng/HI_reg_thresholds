####### Dec 2020####
###code by Lansing Perng

rm(list=ls())

#for gam
PKG <- c("gratia", "mgcv", "tidyr", "dplyr","plyr",'stringr', "ggplot2", 'scales','RColorBrewer')
for (p in PKG) {
  if(!require(p,character.only = TRUE)) {  
    install.packages(p)
    require(p,character.only = TRUE)}
}

#scales controls number of decimal places on plot axis labels



###############################################################################
######################## FOSS REEF/PELAGIC/DEEP data ##########################
###############################################################################

###################### clean and deflate ######################
#FOSS<-read.csv("data/foss_landings and revenue_comm_raw.csv")
FOSS<-read.csv("data/foss_landings_2020.csv")

##clean
FOSS$Dollars <-  as.numeric(gsub(",","",FOSS$Dollars)) #Replace comma with nothing--aka delete commas
FOSS$Pounds <-  as.numeric(gsub(",","",FOSS$Pounds)) #Replace comma with nothing--aka delete commas
FOSS<-FOSS[,c('Year','NMFS.Name','Pounds','Dollars')]
write.csv(FOSS,'data/foss_landings and revenue_cleaned.csv', row.names = F)

## deflate
base_year = 2020 # base year for GDP deflator - Maximum = 2020, Minimum = 1947  - max(GDPDEF_quarterly$Year)
GDPDEF_annual<-read.csv("scripts/GDPDEF_annual.csv") #deflator values calculated from 'Annual_GDP_Deflator_Code.R'
REVENUEFILE<-read.csv("data/foss_landings and revenue_cleaned.csv") %>% 
  mutate(nominal_revenue = Dollars) #renaming revenue nominal revenue to keep track
REVENUEFILE <- as_tibble(merge(REVENUEFILE, GDPDEF_annual, by="Year", all.x=TRUE))
REVENUEFILE[["Dollars"]] <- REVENUEFILE[["nominal_revenue"]]*
  unique(GDPDEF_annual$GDPDEF[GDPDEF_annual$Year==base_year])/REVENUEFILE$GDPDEF #deflating
REVENUEFILE$GDPDEF <- NULL
write.csv(REVENUEFILE,'data/foss_landings and revenue_deflated.csv', row.names = F) 

############### split FOSS data by species type ###########
FOSS<-read.csv('data/foss_landings and revenue_deflated.csv') 
FOSS<-FOSS[,-5]

##entered 5 habitat types on excel sheet: Pelagic, Reef, Deep, Freshwater, Offshore
#and saved as csv (species_habitat.csv) --read in
# may redo in R so code is all inclusive
spec.hab<-read.csv('data/HCC cleaned/species_habitat.csv') 
names(FOSS)<-c('Year', 'Species', 'Pounds', 'Dollars')
FOSS.spec<-join(FOSS,spec.hab, type = 'full')

#check NAs
FOSS.others<-FOSS.spec[which(is.na(FOSS.spec$Habitat) == 'TRUE'),] 
unique(FOSS.others$Species) #only jawed verts--too general to be placed into habitats
FOSS.spec[which(is.na(FOSS.spec$Habitat) == 'TRUE'),'Habitat'] <- 'Jawed Vertebrates'
FOSS.spec<-FOSS.spec[FOSS.spec$Habitat != 'Freshwater',] #everything except freshwater (lake herring)
FOSS.spec$Habitat<-factor(FOSS.spec$Habitat, 
                          levels = c('Pelagic','Reef', 'Deep', 'Fishpond', 'Jawed Vertebrates', 'NWHI'))

#label them jawed vertebrates for final stacked plot
write.csv(FOSS.spec, 'data/HCC cleaned/FOSS species and groups for plotting.csv', row.names = F)

############## first do simple catch by species group plot ##############
#read in final FOSS catch df with species and habitat groups assigned
FOSSgrouped<-read.csv('data/HCC cleaned/FOSS species and groups for plotting.csv')
groups2<-c('Pelagic','Reef', 'Deep', 'Jawed Vertebrates', 'Lobsters (NWHI)'  )

FOSSgrouped$Habitat<-factor(FOSSgrouped$Habitat, 
                               levels = c('Pelagic','Reef', 'Deep', 'Fishpond', 'Jawed Vertebrates', 'NWHI'))
FOSSgrouped$Species<-as.factor(FOSSgrouped$Species)

FOSS.ann<-aggregate(Pounds~Species+Year, data= FOSSgrouped, FUN = sum)
str(FOSS.ann)
FOSS.agg<-aggregate(.~Species, data= FOSS.ann, FUN = sum)
top<-top_n(FOSS.agg, 25, Pounds) 
top<-top[,c('Species','Pounds')]
names(top)<-c('Species','Pounds.tot')
write.csv(top,'data/HCC cleaned/FOSS Catch_top 25.csv', row.names = F)

#join top 25 species to catch
FOSS.ann0<-join(FOSS.ann,top, by = 'Species', type = 'inner') # 'inner' keeps only Species that are only in top 25 catch

#join habitats to annual catch by species
spec.hab<-FOSSgrouped[,c('Species','Habitat')]
#spec.hab<-spec.hab[,c('Species', 'Habitat')]
FOSS.ann0<-join(FOSS.ann0,spec.hab, type ='inner')

FOSS.ann0$Species<-gsub("SHARKS, CHONDRICHTHYES (CLASS) **","SHARKS",FOSS.ann0$Species, fixed = T) #relabel in legend
FOSS.ann0$Species<-gsub("SNAPPERS, LUTJANIDAE (FAMILY) **","SNAPPER FAMILY",FOSS.ann0$Species, fixed = T) #relabel in legend
FOSS.ann0$Species<-gsub("LOBSTER, CARIBBEAN SPINY","SPINY LOBSTER",FOSS.ann0$Species, fixed = T) #relabel in legend
FOSS.ann0$Species<-gsub("JOBFISH, GREEN /UKU","GREEN JOBFISH",FOSS.ann0$Species, fixed = T) #relabel in legend
FOSS.ann0$Species<-gsub("**","",FOSS.ann0$Species, fixed = T) #relabel in legend

FOSS.ann0$Species<-sub("(\\w+),\\s(\\w+)","\\2 \\1", FOSS.ann0$Species) #switches order of names with comma eg. TUNA, BIGEYE->BIGEYE TUNA

FOSS.ann0<-arrange(FOSS.ann0, desc(Pounds.tot)) #arrange in descending order of total catch
unique(FOSS.ann0$Species) #make sure they are in the right order
FOSS.ann0$Species<-factor(FOSS.ann0$Species, levels = unique(FOSS.ann0$Species)) #convert from chr to factor
#set levels as rearranged by catch
FOSS.ann0$Pounds<-FOSS.ann0$Pounds/1000000

#same colorblind friendly palette used in FOSS plots so colors match
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

#make colors diff for presentation slides
cbbPalette1 <- c("#000000", "#661100", "#332288", "#DDCC77", "#117733", "#999933", "#88CCEE", "#AA4499")

ggplot(FOSS.ann0, aes(Year, Pounds)) +
  facet_wrap(~Species,scales = "free_y",ncol=5) +
  geom_line(aes(color = Habitat))+
  scale_color_manual(labels = groups2,
               #      values = cbbPalette[c(3:5,7,8)]) +
                      values = cbbPalette1[c(3:5,7,8)]) +
  
  labs(x="Year", y="Landings (Millions of Pounds)")+
  theme_bw() +
  theme(axis.title.x = element_blank(), # vjust adjusts vertical space between plot & axis title
        axis.title.y = element_text(size = 21),
        axis.text = element_text(size = 11.5),
        strip.text = element_text(size = 12),
        legend.position = 'bottom',
        legend.text=element_text(size=17),
        legend.title=element_blank(),
        panel.grid.major = element_blank(), #delete major grid lines
        panel.grid.minor = element_blank() #delete minor grid lines
  )

#ggsave('figures/FOSS compiled species groups/FOSS top species Catch.png', 
 #      width =  12, height = 9, units = 'in', #w & h in inches
  #     dpi = 300, bg = 'transparent')


ggsave(paste('figs for slides/top species Catch.png',sep=''), 
       width =  12, height = 9, units = 'in', #w & h in inches
       dpi = 300, bg = 'transparent')

##############################################################################

#double the df and put 'All Species' in habitat column for copy
FOSS.all<-FOSS.spec
FOSS.all$Habitat<-'All Species'

FOSS.spec<-rbind(FOSS.all,FOSS.spec)
write.csv(FOSS.spec, paste('data/HCC cleaned/FOSS species and groups.csv', sep =''), row.names = F)

#code later will sum everything into an annual all species value, so no need to do it now
#will also have to fix up the all species code that reads in from 'FOSS with Millions'

########### split groups and add diversity ##############
FOSS.spec<-read.csv(paste('data/HCC cleaned/FOSS species and groups.csv', sep =''))
groups<-(unique(FOSS.spec$Habitat))

for(i in 1:length(unique(FOSS.spec$Habitat))) {
  FOSS.group<-FOSS.spec[FOSS.spec$Habitat==groups[i],] #subset just reef species and drop species column
  TOT_FOSS <- aggregate(Pounds~Year, data=FOSS.group, FUN=sum)
  TOT_REV <- aggregate(Dollars~Year, data=FOSS.group, FUN=sum)
  
  names(TOT_FOSS) <- c('Year','Pounds.tot') #rename total pounds
  names(TOT_REV) <- c('Year','Dollars.tot') #rename total pounds
  
  FOSS.group <- merge(FOSS.group,TOT_FOSS, by=c('Year')) #add total column onto original df to get proportions
  FOSS.group <- merge(FOSS.group,TOT_REV, by=c('Year')) #add total column onto original df to get proportions
  
  #calculate shannon index: SUM[-p*ln(p)]
  FOSS.group$P_Catch <- -((FOSS.group$Pounds/FOSS.group$Pounds.tot)*
                           log(FOSS.group$Pounds/FOSS.group$Pounds.tot)) #-p*ln(p)
  FOSS_Div <- aggregate(P_Catch~Year, data=FOSS.group, FUN=sum) #Sum P_Catch by year to get annual diversity values
  
  FOSS.group$P_Rev <- -((FOSS.group$Dollars/FOSS.group$Dollars.tot)*
                            log(FOSS.group$Dollars/FOSS.group$Dollars.tot)) #-p*ln(p)
  FOSS_RevDiv <- aggregate(P_Rev~Year, data=FOSS.group, FUN=sum) #Sum P_Catch by year to get annual diversity values
  
  #effective number of species
  FOSS_Div$CommDiv <- exp(FOSS_Div$P_Catch) #effective num of species as calculated by Shannon Diversity Index
  FOSS_Div <- subset(FOSS_Div, select=c('Year','CommDiv'))
  
  FOSS_RevDiv$RevDiv <- exp(FOSS_RevDiv$P_Rev) #effective num of species as calculated by Shannon Diversity Index
  FOSS_RevDiv <- subset(FOSS_RevDiv, select=c('Year','RevDiv'))
  
  FOSS.group<-join_all(list(TOT_FOSS, TOT_REV, FOSS_Div,FOSS_RevDiv), by = "Year", type = 'full') #join dfs together for annual values of all FOSS vars
  FOSS.group$Catch.Millions<-FOSS.group$Pounds.tot/1000000
  FOSS.group$Revenue.Millions<-FOSS.group$Dollars/1000000
  FOSS.fin<-FOSS.group[,c('Year', 'Catch.Millions','Revenue.Millions','CommDiv','RevDiv')]
  
  write.csv(FOSS.fin, paste('data/HCC cleaned/FOSS ',groups[i],' fish Pounds and Diversity.csv', sep =''), row.names = F)
}

################# GAMFIT TNT FUNCTION FOR EACH GROUP AND STACK DFS #############
####stack dfs into one with species separated and a group for all species
#first load in all species to stack dfs for ind groups onto
FOSS<-read.csv('data/HCC cleaned/FOSS All Species fish Pounds and Diversity.csv')
FOSS.spec<-read.csv(paste('data/HCC cleaned/FOSS species and groups.csv', sep =''))

#vectors used
FOSSvar<-colnames(FOSS)
groups<-(unique(FOSS.spec$Habitat))

#set sp just for lobster revenue--default plot doesn't match data 

for(k in 2:5) { # 4 FOSS variables are columns 2:5
  #created 2 empty dfs to bind others to#
  FOSS0 <- data.frame(matrix(ncol = 5, nrow = 0)) #one for data points
  colnames(FOSS0) <- colnames(FOSS)
  
  df<- data.frame(matrix(ncol = 8, nrow = 0)) #one for gamfits
  colnames(df)<-c('Species.Type',"Year","fit", "se.fit", "inc.trend", "dec.trend", "threshold", "se.threshold" )
  
  #run rest of species groups in a loop and rbind each to df-->overwrites with compiled df as the loop runs
  for(i in 1:length(groups)) {
    df0<-read.csv(paste('data/HCC cleaned/FOSS ',groups[i],' fish Pounds and Diversity.csv', sep =''))
    df0$Species.Type<-groups[i]
    if(nrow(unique(df0[k]))>1) { #need to put if there are multiple unique values in target column
      #because CommDiv of Jawed Verts and NWHI is only one because theres only 1 specified species--no GAM model can be calculated
      # so this if() calculates GAMs if current df is NOT CommDiv or JV or NWHI
      gammod<-gam(unlist(df0[k]) ~ s(Year), sp=0.007,
                  data = df0, method = 'REML')

      df.new<-gam_tnt(df0, 500,gammod,df0$Year,'Year',df0$Species.Type, 'Species.Type')
      df<-rbind(df.new,df)
    }
    
    if(nrow(unique(df0[k]))==1) { #if is IS CommDiv of JV or NWHI, just save raw div values, which are 1 every year
      
      df.new<-df0[,c('Species.Type','Year','CommDiv')]
      names(df.new)<-c('Species.Type', 'Year', 'fit') #there will be no se, trend, or threshold
      df<-bind_rows(df.new,df)
    }
    
    FOSS0<-rbind(df0,FOSS0)
  }

  write.csv(df, paste('data/HCC cleaned/FOSS species compiled ',FOSSvar[k],'_gamfit.csv', sep =''), row.names = F)
  write.csv(FOSS0, 'data/HCC cleaned/FOSS species compiled data points_raw.csv', row.names = F)  
  }

############## PLOT ALL SPECIES GROUPS TOGETHER FOR EACH FOSS VAR ###############
#vectors used
groups1<-c('All Species','Pelagic','Reef', 'Deep','Fishpond', 'Jawed Vertebrates', 'NWHI'  )
groups2<-c('All Species','Pelagic','Reef', 'Deep','Fishpond', 'Jawed Vertebrates', 'Lobsters (NWHI)'  )
#relabel lobsters in plot
FOSSlabs<-c('1','Pounds (Millions)', 'Millions of Dollars (2020 value)', 'Effective Number of Species','Revenue Diversity\n (Effective Shannon Diversity)')
FOSStitle<-c('1','Commercial Landings', 'Commercial Revenue', 'Catch Diversity','Revenue Diversity')

#read in raw data--includes all vars so read in outside of loop
FOSS0<-read.csv('data/HCC cleaned/FOSS species compiled data points_raw.csv') #for plotting original data points
FOSSvar<-colnames(FOSS0)

str(FOSS0)
FOSS0$Species.Type<-factor(FOSS0$Species.Type, levels = groups1)

#for zoomed in plot
FOSS0cut<-FOSS0[FOSS0$Species.Type == 'Reef' |
                FOSS0$Species.Type == "Deep"|
                FOSS0$Species.Type == "Fishpond"|
                FOSS0$Species.Type == "NWHI",]
for(k in 2:3){ #k is FOSS vars
  df1<-read.csv(paste('data/HCC cleaned/FOSS species compiled ',FOSSvar[k],'_gamfit.csv', sep =''))
  #df1<-df1[df1$Species.Type != 'Freshwater',] #remove freshwater group from final plot
  
  # set Species.Type as factor and reorder according to how i want it to be plotted
  df1$Species.Type<-factor(df1$Species.Type, levels = groups1)
  str(df1)
  
  # colorblind friendly palette with black:
  cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", '#737373')
  #value 4 and 7 are green and red used for increasing and decreasing trends

  #make colors diff for presentation slides
  cbbPalette1 <- c("#000000", "#661100", "#332288", "#DDCC77", "#117733", "#999933", "#88CCEE", "#AA4499")
  
  ############### plot gam ##################
  ggplot(df1, #switch between df1 or df1cut for zoomed plot
         aes(x = Year, y = fit)) +
    theme(  plot.title=element_text(hjust=0.5, size = 27),
            panel.grid.major = element_blank(), #delete major grid lines
            panel.grid.minor = element_blank(), #delete minor grid lines
            axis.text=element_text(size=22, color = 'black'), #adjust font size of axis tick labels
            axis.title=element_text(size=26,face="plain"), #adjust size of axis titles
            #     axis.title.x = element_text(margin = margin(t = 12, r = 0, b = 0, l = 0)), #increases distance between x axis label and plot
            panel.background = element_rect(fill = "transparent",colour = 'black', size = 1),
            plot.background = element_rect(fill = "transparent",colour = NA),
            plot.margin = unit(c(0.3,2,0.3,0.3), "cm"),
            legend.position = 'right', # c(0.37,0.57),
            legend.background = element_rect(fill="transparent", color = 'black'),
            legend.key = element_rect(fill = "transparent", colour = NA),
            legend.key.size = unit(1, 'cm'),
            legend.spacing.y = unit(0.2,'cm'),
            legend.text=element_text(size=17),
            legend.title=element_blank(),
            legend.margin = margin(3,12,9,10)) +
    guides(fill = guide_legend(byrow = T)) + #need this line for legend.spacing.y to work
    scale_x_continuous(breaks=seq(1980,2020,5), #so that a tick appears every year--default ticks only where breaks are assigned
                       labels = c("1980",'',"1990",'',"2000",'',"2010",'','2020')) +
    scale_y_continuous(limits = c(min(0,round_any(min(df1$fit-2*df1$se.fit),1, f = floor)), round_any(max(df1$fit+2*df1$se.fit),1, f = ceiling))) + #0 to max point (points go further than CI band)
    geom_ribbon(aes(ymin = fit-(2*se.fit), ymax = fit+(2*se.fit), fill = Species.Type), alpha = 0.3, color = 'gray45') + #95CI = (2*SE) for gam function
    scale_fill_manual(labels = groups2, #use groups2 for all legend labels
                      values = cbbPalette[c(9,3:6,7,8)]) + #color combined CI, individual Species.Types are gray
    {if("threshold" %in% colnames(df1))
      geom_ribbon(aes(ymin = threshold-(2*se.threshold), ymax = threshold+(2*se.threshold), group = Species.Type), 
                  alpha = 0.6, fill = 'gray40', color = 'black')} + #recolor se band for threshold Species.Types
    geom_line(size = 0.7, aes(color = Species.Type)) + #full gam function
    scale_color_manual(labels = groups2,
                       values = cbbPalette[c(9,3:6,7,8)]) +
    {if("inc.trend" %in% colnames(df1)) 
      geom_line(aes(x=Year, y = inc.trend, group = Species.Type),size = 1.8, color = "#009E73", alpha = 0.6)}  +  #increasing trends
    { if("dec.trend" %in% colnames(df1))
      geom_line(aes(x=Year, y = dec.trend, group = Species.Type),size = 1.8, color = "#D55E00", alpha = 0.6) } +
    labs(y = FOSSlabs[k], x = "Year", title = paste(FOSStitle[k],sep='')) +
    geom_point(data= FOSS0, #switch between FOSS0 or FOSS0cut for zoomed plot
               aes(x = Year, y = unlist(FOSS0[,k]), 
                   color = Species.Type, shape = Species.Type), fill = NA, size = 2.5) + # data points
    scale_shape_manual(labels = groups2,
                       values = c(4,2,1,0,5,6,8))
  
  ggsave(paste('figures/FOSS compiled species groups/',FOSSvar[k],' GAM_ggplot.png',sep=''), 
         width =  9.2, height = 6, units = 'in', #w & h in inches
          dpi = 300, bg = 'transparent')
  
#  ggsave(paste('figs for slides/',FOSSvar[k],' GAM_ggplot.png',sep=''), 
 #        width =  9.2, height = 6, units = 'in', #w & h in inches
  #       dpi = 300, bg = 'transparent')
  
  ########################### ZOOMED PLOT #############################
  df1cut<-df1[df1$Species.Type == 'Reef' |
                df1$Species.Type == "Deep"|
                df1$Species.Type == "Fishpond"|
                df1$Species.Type == "NWHI",]
  
  # colorblind friendly palette with black:
  cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
  #value 4 and 7 are green and red used for increasing and decreasing trends
  
  #make colors diff for presentation slides
  cbbPalette1 <- c("#000000", "#661100", "#332288", "#DDCC77", "#117733", "#999933", "#88CCEE", "#AA4499")
  
  zoom <-c(1,8,10,3,3)
  zoombreak<-c(1,1,5,1,1)
  ggplot(df1cut, #switch between df1 or df1cut for zoomed plot
         aes(x = Year, y = fit)) +
    theme(  plot.title=element_text(hjust=0.5, size = 27),
            panel.grid.major = element_blank(), #delete major grid lines
            panel.grid.minor = element_blank(), #delete minor grid lines
            axis.text=element_text(size=22, color = 'black'), #adjust font size of axis tick labels
            axis.title=element_text(size=26,face="plain"), #adjust size of axis titles
            #     axis.title.x = element_text(margin = margin(t = 12, r = 0, b = 0, l = 0)), #increases distance between x axis label and plot
            panel.background = element_rect(fill = "transparent",colour = 'black', size = 1),
            plot.background = element_rect(fill = "transparent",colour = NA),
            plot.margin = unit(c(0.3,2,0.3,0.3), "cm"),
            legend.position = 'right', # c(0.37,0.57),
            legend.background = element_rect(fill="transparent", color = 'black'),
            legend.key = element_rect(fill = "transparent", colour = NA),
            legend.key.size = unit(1, 'cm'),
            legend.spacing.y = unit(0.2,'cm'),
            legend.text=element_text(size=17),
            legend.title=element_blank(),
            legend.margin = margin(3,12,9,10)) +
    guides(fill = guide_legend(byrow = T)) + #need this line for legend.spacing.y to work
    scale_x_continuous(breaks=seq(1980,2020,5), #so that a tick appears every year--default ticks only where breaks are assigned
                       labels = c("1980",'',"1990",'',"2000",'',"2010",'','2020')) +
    scale_y_continuous(breaks=seq(0,zoom[k],zoombreak[k]))+
    geom_ribbon(aes(ymin = fit-(2*se.fit), ymax = fit+(2*se.fit), fill = Species.Type), alpha = 0.3, color = 'gray45') + #95CI = (2*SE) for gam function
    scale_fill_manual(labels = groups1[4:6], #groups are labeled incorrectly, but legend will be deleted 
                      #in manuscript plot anyway--left it this way so legend will take up same width
                      values = cbbPalette[c(4,5,6,8)]) + #color combined CI, individual Species.Types are gray
    {if("threshold" %in% colnames(df1))
      geom_ribbon(aes(ymin = threshold-(2*se.threshold), ymax = threshold+(2*se.threshold), group = Species.Type), 
                  alpha = 0.6, fill = 'gray40', color = 'black')} + #recolor se band for threshold Species.Types
    geom_line(size = 0.7, aes(color = Species.Type)) + #full gam function
    scale_color_manual(labels = groups1[4:6],
                       values = cbbPalette[c(4,5,6,8)]) +
    {if("inc.trend" %in% colnames(df1)) 
      geom_line(aes(x=Year, y = inc.trend, group = Species.Type),size = 1.8, color = "#009E73", alpha = 0.6)}  +  #increasing trends
    { if("dec.trend" %in% colnames(df1))
      geom_line(aes(x=Year, y = dec.trend, group = Species.Type),size = 1.8, color = "#D55E00", alpha = 0.6) } +
    labs(y = FOSSlabs[k], x = "Year", title = paste(FOSStitle[k],sep='')) +
    geom_point(data= FOSS0cut, #switch between FOSS0 or FOSS0cut for zoomed plot
               aes(x = Year, y = unlist(FOSS0cut[,k]), 
                   color = Species.Type, shape = Species.Type), fill = NA, size = 2.5) + # data points
    scale_shape_manual(labels = groups1[4:6],
                       values = c(1,0,5,8))
  
  ggsave(paste('figures/FOSS compiled species groups/',FOSSvar[k],' GAM_zoom.png',sep=''), 
         width =  9.2, height = 3.7, units = 'in', #w & h in inches
         dpi = 300, bg = 'transparent')

#  ggsave(paste('figs for slides/',FOSSvar[k],' GAM_zoom.png',sep=''), 
 #        width =  9.2, height = 3.7, units = 'in', #w & h in inches
  #       dpi = 300, bg = 'transparent')
  
}


################################## EMPLOYMENT #################################
##### BLS
############# COMPILED GAM PLOT WITH INDIVIDUAL SECTORS PLUS COMBINED ##########
BLSvar<-c('1','Establishments', 'Employment','Wages') # 
BLSlabs<-c('1','Number of Marine-Related Establishments','Total Employment (Thousands of Individuals)', 'Total Wages (Millions of Dollars - 2020 Value)') 
BLSleg<-c("Fishing", "Seafood Markets", 'Seafood Processing','Seafood Wholesale' ,'All Sectors')#colors
# colorblind friendly palette with black:
#cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# colorblind friendly 
mixed<-c('#bf812d', '#dfc27d', '#c7eae5','#35978f', '#737373')
mixedHI<-c('#bf812d', '#dfc27d','#35978f', '#737373') #just for hawaii which is missing seafood packaging (color 3)
zoom<-c(1,1,0.5,30)

#adjust label sizes and whatnot for manuscript 
  for(j in 2:4){ #j represents the column number in the BLS df (skip 1), BLSvar (for writing files),and BLSlabs (y axis labels)
    
    df1<-read.csv(paste('data/HI/HI BLS ',BLSvar[j],' combined gamfit.csv', sep=''))
    df1 <- df1[,colSums(is.na(df1))<nrow(df1)] #if any trend or threshold columns are all NA, delete
    
    BLS0<-read.csv(paste('data/HI/HI BLS ',BLSvar[j],' combined_points_raw.csv', sep=''))
    
    #to calculate max y-axis for zoomed plot
    df1.z<-df1[df1$Sector == 'Fishing' |
                 df1$Sector == 'Seaf Markets' |
                 df1$Sector == 'Seaf Packaging' |
                 df1$Sector == 'Seaf Wholesale',]
    df1.z$Sector<-as.character(df1.z$Sector)
    
    #reorder levels of 'Sector' so that combined (All Sectors) layers on last and is at the bottom of the legend
    df1$Sector <- factor(df1$Sector, levels = c(unique(df1.z$Sector),'Combined'))
    BLS0$Sector <- factor(BLS0$Sector, levels = c(unique(df1.z$Sector),'Combined'))
    
    BLSlegHI<-c(unique(df1.z$Sector),'Combined')
    
    
    ##### plot gam ######
    # BLS employment and wages will have weird legends because they are missing seaf packaging
    
    ggplot(df1,
           aes(x = Year, y = fit)) +
      theme(  plot.title=element_text(hjust=0.5, size = 30),
              panel.grid.major = element_blank(), #delete major grid lines
              panel.grid.minor = element_blank(), #delete minor grid lines
              axis.text=element_text(size=22, color = 'black'), #adjust font size of axis tick labels
              axis.title=element_text(size=26,face="plain"), #adjust size of axis titles
              #     axis.title.x = element_text(margin = margin(t = 12, r = 0, b = 0, l = 0)), #increases distance between x axis label and plot
              panel.background = element_rect(fill = "transparent",colour = 'black', size = 1),
              plot.background = element_rect(fill = "transparent",colour = NA),
              plot.margin = unit(c(0.3,2,0.3,0.3), "cm"),
              legend.position = 'right', # c(0.37,0.57),
              legend.background = element_rect(fill="transparent", color = 'black'),
              legend.key = element_rect(fill = "transparent", colour = NA),
              legend.key.size = unit(1, 'cm'),
              legend.spacing.y = unit(0.2,'cm'),
              legend.text=element_text(size=17),
              legend.title=element_blank(),
              legend.margin = margin(3,12,9,10)) +
      guides(fill = guide_legend(byrow = T)) + #need this line for legend.spacing.y to work
      scale_x_continuous(breaks=seq(2000,2020,1), labels = c("2000",rep("",4),"2005",rep("",4),'2010',rep("",4),'2015',rep("",4), '2020')) +
      scale_y_continuous(limits = c(min(0,round_any(min(df1$fit-(2*df1$se.fit)), 1, f = floor)), round_any(max(df1$fit+(2*df1$se.fit)), 1, f = ceiling)), position = 'left')+
      #    scale_y_continuous(limits = c(0,zoom[j])) +
      #      scale_y_continuous(limits = c(round_any(min(df1.z$fit-(2*df1.z$se.fit)), 10, f = floor), round_any(max(df1.z$fit+(2*df1.z$se.fit)), 10, f = ceiling)), position = 'left')+
      geom_ribbon(aes(ymin = fit-(2*se.fit), ymax = fit+(2*se.fit), fill = Sector), alpha = 0.7) + #95CI = (2*SE) for gam function
      {if(BLSvar[j] == 'Establishments') #palette with 5 colors if not HI or HI Establishments
        scale_fill_manual(labels = BLSleg,
                          values = mixed)} + #color combined CI, individual sectors are gray
      {if(BLSvar[j] != 'Establishments') #palette minus 3rd color (-seafood packaging)
        scale_fill_manual(labels = BLSlegHI,
                          values = mixedHI)} + 
      {if("threshold" %in% colnames(df1))
        geom_ribbon(aes(ymin = threshold-(2*se.threshold), ymax = threshold+(2*se.threshold), group = Sector), 
                    alpha = 0.8, fill = 'gray40', color = 'black')}  + #recolor se band for threshold regions
      geom_line(size = 0.7, aes(color = Sector), alpha = 1) + #full gam function
      {if(BLSvar[j] == 'Establishments') #palette minus 3rd color (-seafood packaging)
          scale_color_manual(labels = BLSleg, 
                             values = c(mixed[1:4],'black'))} +
      {if(BLSvar[j] != 'Establishments') #palette minus 3rd color (-seafood packaging)
        scale_color_manual(labels = BLSlegHI, 
                           values = c(mixedHI[1:3],'black'))} +
      geom_point(data = BLS0,aes(x = Year, y = BLS0[,2], shape = Sector, fill = Sector), size = 4.8) + # data points
      {if(BLSvar[j] == 'Establishments') #shapes minus 3rd  (-seafood packaging)
        scale_shape_manual(labels = BLSleg, 
                           values = c(21:25))} +
      {if(BLSvar[j] != 'Establishments') #shapes minus 3rd  (-seafood packaging)
        scale_shape_manual(labels = BLSleg, 
                           values = c(21,22,24,25))} +
      {if("inc.trend" %in% colnames(df1)) 
        geom_line(aes(x=Year, y = inc.trend, group = Sector),size = 1.3, color = "#009E73", alpha = 0.5)}   +  #increasing trends
      { if("dec.trend" %in% colnames(df1))
        geom_line(aes(x=Year, y = dec.trend, group = Sector),size = 1.3, color = "#D55E00", alpha = 0.5) }  +
      labs(y = BLSlabs[j], x = "Year", title = "Hawai‘i")   
    
    ggsave(paste('figures/Employment/BLS ',BLSvar[j],' GAM_ggplot.png',sep=''), 
           width =  11, height = 10, units = 'in', #w & h in inches
           dpi = 300, bg = 'transparent')

    #   ggsave(paste('figures/Employment/BLS ',BLSvar[j],' GAM_ggplot_zoom.png',sep=''), 
    #         width =  10.5, height = 4.8, units = 'in', #w & h in inches
    #        dpi = 300, bg = 'transparent')
  }

    
############################### NES ########################################
##prior calculations in 'Gratia cross reigional analyses.R
############COMPILED GAM PLOT WITH INDIVIDUAL SECTORS PLUS COMBINED #############
NES1var<-c('1','Establishments', 'Receipts') #for manuscript, probably better to focus on receipts
NES1labs<-c('1','Number of Self-Employed Individuals','Non-Employer Receipts\n (Millions of Dollars - 2020 Value)')
NESleg<-c( "Fishing","Seafood Markets", "Seafood Processing", 'All Sectors')

## rescale HI employment--was scaled to thousands to compare across regions
est<-read.csv(paste('data/HI/HI NES combined Establishments gamfit.csv', sep=''))
NES0est<-read.csv(paste('data/HI/HI NES combined Establishments_data points_raw.csv', sep=''))
est[,c(3:8)]<-est[,c(3:8)]/1
NES0est$Establishments<-NES0est$Establishments/1
write.csv(est, paste('data/HI/HI NES combined Establishments_scaled gamfit.csv', sep=''),row.names = F)
write.csv(NES0est, paste('data/HI/HI NES combined Establishments_scaled_data points_raw.csv', sep=''),row.names = F)

# colorblind friendly
mixed<-c('#bf812d', '#dfc27d', '#c7eae5', '#737373')

  for(j in 2:3){ #j represents the column number in the NES df (skip 1), the variable name(for writing files),and labs (y axis labels)
    #with predicted values from prior calculation
    df1<-read.csv(paste('data/HI/HI NES combined ', NES1var[j],' gamfit.csv', sep=''))
    NES0<-read.csv(paste('data/HI/HI NES combined ', NES1var[j],'_data points_raw.csv', sep=''))
    
    #reorder factor so that 'All Species' layers on top of 'Without Menhadens'
    #All Species is plotted first by default (alphabetically)
    df1$Sector <- factor(df1$Sector, levels = c("Fishing", "Seafood Markets", 'Seafood Packaging', 'Combined'))
    NES0$Sector <- factor(NES0$Sector, levels = c("Fishing", "Seafood Markets", 'Seafood Packaging', 'Combined'))
    
    #to calculate max y-axis for zoomed plot
    df1.z<-df1[df1$Sector == 'Seafood Markets' |
                 df1$Sector == 'Seafood Packaging',]
    
    ggplot(df1,
           aes(x = Year, y = fit)) +
      theme(  plot.title=element_text(hjust=0.5, size = 30),
              panel.grid.major = element_blank(), #delete major grid lines
              panel.grid.minor = element_blank(), #delete minor grid lines
              axis.text=element_text(size=21, color = 'black'), #adjust font size of axis tick labels
              axis.title=element_text(size=25,face="plain"), #adjust size of axis titles
              axis.title.x = element_text(margin = margin(t = 12, r = 0, b = 0, l = 0)), #increases distance between x axis label and plot
              panel.background = element_rect(fill = "transparent",colour = 'black', size = 1),
              plot.background = element_rect(fill = "transparent",colour = NA),
              plot.margin = unit(c(4,2,3.6,0.3), "cm"),
              legend.position = 'right', # c(0.37,0.57),
              legend.background = element_rect(fill="transparent", color = 'black'),
              legend.key = element_rect(fill = "transparent", colour = NA),
              legend.key.size = unit(1, 'cm'),
              legend.spacing.y = unit(0.2,'cm'),
              legend.text=element_text(size=17),
              legend.title=element_blank(),
              legend.margin = margin(3,12,9,10)) +
      guides(fill = guide_legend(byrow = T))+
  #    scale_x_continuous(breaks=seq(1995,2020,1), labels = c("1995",rep("",4),"2000",rep("",4),"2005",rep("",4),'2010',rep("",4),'2015',rep("",4), '2020')) +
      scale_y_continuous(limits = c(min(0,round_any(min(df1$fit-(2*df1$se.fit)), 1, f = floor)), round_any(max(df1$fit+(2*df1$se.fit)), 0.1, f = ceiling)))+
  #    scale_y_continuous(limits = c(round_any(min(df1.z$fit-(2*df1.z$se.fit)),0.01, f = floor), round_any(max(df1.z$fit+(2*df1.z$se.fit)), 0.01, f = ceiling)))+
      geom_ribbon(aes(ymin = fit-(2*se.fit), ymax = fit+(2*se.fit), fill = Sector), alpha = 0.7) + #95CI = (2*SE) for gam function
      scale_fill_manual(labels = NESleg,
                        values = scales::alpha(mixed, c(0.7,0.7,0.7,0.4))) + #color combined CI, individual sectors are gray
      {if("threshold" %in% colnames(df1))
        geom_ribbon(aes(ymin = threshold-(2*se.threshold), ymax = threshold+(2*se.threshold), group = Sector), 
                    alpha = 0.8, fill = 'gray40', color = 'black')} + #recolor se band for threshold regions
      geom_line(size = 0.6, aes(color = Sector), alpha = 1) + #full gam function
      scale_color_manual(labels = NESleg, 
                         values = c(mixed[1:3],'black')) +
      geom_point(data = NES0,aes(x = Year, y = unlist(NES0[2]), shape = Sector, fill = NES0$Sector),color = 'gray20', size = 4.8)  + # data points
      scale_shape_manual(labels = NESleg, 
                         values = c(21:23,25)) +
      {if("inc.trend" %in% colnames(df1)) 
        geom_line(aes(x=Year, y = inc.trend, group = Sector),size = 2, color = "#009E73", alpha = 0.6)}  +  #increasing trends
      { if("dec.trend" %in% colnames(df1))
        geom_line(aes(x=Year, y = dec.trend, group = Sector),size = 2, color = "#D55E00", alpha = 0.6) } +
      labs(y = NES1labs[j], x = "Year", title = "Hawai‘i")   
    
    ggsave(paste('figures/Employment/NES ',NES1var[j],' GAM.png',sep=''), 
           width =  11.3, height = 9.5, units = 'in', #w & h in inches
           dpi = 300, bg = 'transparent')
    
#    ggsave(paste('figures/Employment/NES ',NES1var[j],' GAM_zoom.png',sep=''), 
 #         width =  11, height = 6.5, units = 'in', #w & h in inches
  #        dpi = 300, bg = 'transparent')
  }

###############################################################################
##################################### NCRMP ###################################
###############################################################################

#######################load and organize data cleaned from raw########
#raw is HCC cleaned/NCRMP_socio_secondarydata_Hawaii_ERDDAP.csv
NCRMP<-read.csv('data/NCRMP HI region specific soc indicators.csv')
tour<-NCRMP[,c('Year','Tourism_Emp','Tourism_GDP')]
colnames(tour)[c(2,3)]<-c('Count','Dollars')
tour$Category<-'Tourism'

visit<-NCRMP[,c('Year','Tourism_Arrivals','Visitor_Spending')]
colnames(visit)[c(2,3)]<-c('Count','Dollars')
visit$Category<-'Visitors'

livres<-NCRMP[,c('Year','LivRes_Emp','LivRes_GDP')]
colnames(livres)[c(2,3)]<-c('Count','Dollars')
livres$Category<-'Living Resources'

NCRMP0<-rbind(tour,visit, livres)
NCRMP0<-NCRMP0[NCRMP0$Year>2004,] #2001 - 2004 are NAs
NCRMP0<-NCRMP0[NCRMP0$Year<2017,] #2017 also NA

## deflate
base_year = 2020 # base year for GDP deflator - Maximum = 2020, Minimum = 1947  - max(GDPDEF_quarterly$Year)
GDPDEF_annual<-read.csv("scripts/GDPDEF_annual.csv") #deflator values calculated from 'Annual_GDP_Deflator_Code.R'
REVENUEFILE<-NCRMP0 %>% 
  mutate(nominal_revenue = Dollars) #renaming revenue nominal revenue to keep track
REVENUEFILE <- as_tibble(merge(REVENUEFILE, GDPDEF_annual, by="Year", all.x=TRUE))
REVENUEFILE[["Dollars"]] <- REVENUEFILE[["nominal_revenue"]]*
  unique(GDPDEF_annual$GDPDEF[GDPDEF_annual$Year==base_year])/REVENUEFILE$GDPDEF #deflating
REVENUEFILE$GDPDEF <- NULL
write.csv(REVENUEFILE, 'data/NCRMP_long_deflated.csv', row.names = F)

################# GAMFIT TNT FUNCTION FOR EACH CATEGORY AND STACK DFS #############
#read in points df
NCRMP0<-read.csv('data/NCRMP_long_deflated.csv')
NCRMP0$Individuals.Thousands<-NCRMP0$Count/1000
NCRMP0$Dollars.Millions<-NCRMP0$Dollars/1000000
write.csv(NCRMP0,'data/NCRMP_long_deflated_scaled.csv', row.names = F)
#vectors used
NCRMPvar<-c('Individuals.Thousands','Dollars.Millions')
groups<-(unique(NCRMP0$Category))

for(k in 1:length(NCRMPvar)) { # NCRMP scaled variables are columns 6:7
  #created empty df to bind others to
  df<- data.frame(matrix(ncol = 8, nrow = 0)) #one for gamfits
  colnames(df)<-c('Category',"Year","fit", "se.fit", "inc.trend", "dec.trend", "threshold", "se.threshold" )
  
  #run groups in a loop and rbind each to df-->overwrites with compiled df as the loop runs
  for(i in 1:length(groups)) {
    df0<-NCRMP0[NCRMP0$Category==groups[i],]
      gammod<-gam(unlist(df0[k+5]) ~ s(Year), 
                  data = df0, method = 'REML')
      
      df.new<-gam_tnt(df0, 500,gammod,df0$Year,'Year',df0$Category, 'Category')
      df<-rbind(df.new,df)
  }
  write.csv(df, paste('data/NCRMP ',NCRMPvar[k],'_gamfit.csv', sep =''), row.names = F)
}

############## PLOT ALL SOC CATEGORIES IN FACETTED ###############
## read in points data first to use to write grouping vector
NCRMP0<-read.csv('data/NCRMP_long_deflated_scaled.csv')
#vectors used
groups<-c('Visitor Input','Tourism\n Employment','Living Resources\n Employment')
NCRMPvar<-c('Individuals.Thousands','Dollars.Millions')

NCRMP0$Category<-factor(NCRMP0$Category, levels=c('Visitors','Tourism','Living Resources'), labels = groups)
str(NCRMP0)

NCRMPlabs<-c('Thousands of Individuals','Millions of Dollars')
#NCRMPtitle<-c('1','Commercial Landings', 'Commercial Revenue', 'Catch Diversity')

for(k in 1:length(NCRMPvar)) { # NCRMP scaled variables are columns 6:7
  df1<-read.csv(paste('data/NCRMP ',NCRMPvar[k],'_gamfit.csv', sep =''))
  str(df1)
  
  # set Category as factor and reorder according to how i want it to be plotted
  df1$Category<-factor(df1$Category, levels=c('Visitors','Tourism','Living Resources'), labels = groups)
  
  #using RColorBrewer manual selection
  indpal<-c('#b3cde3','#decbe4','#fddaec')
  
  ############### plot gam ##################
  
  ggplot(df1,
         aes(x = Year, y =  fit)) +
    facet_wrap(~Category, scales = 'free') +
    theme(  panel.grid.major = element_blank(), #delete major grid lines
            panel.grid.minor = element_blank(), #delete minor grid lines
            axis.text=element_text(size=14, color = 'black'), #adjust font size of axis tick labels
            axis.title=element_text(size=17,face="plain"), #adjust size of axis titles
            #     axis.title.x = element_text(margin = margin(t = 12, r = 0, b = 0, l = 0)), #increases distance between x axis label and plot
            strip.background =element_rect(fill="gray94", colour = 'black', size =1), #change panel label color
            strip.text = element_text(size = 19),
            panel.background = element_rect(fill = "transparent",colour = 'black', size = 1),
            plot.background = element_rect(fill = "transparent",colour = NA),
            plot.margin = unit(c(0.3,2,0.3,0.3), "cm"),
            legend.position = 'none') +
    scale_x_continuous(breaks=seq(2005,2016,1), #so that a tick appears every year--default ticks only where breaks are assigned
                       labels = c("2005",rep('',4),"2010",rep('',4),"2015",'')) +
    geom_ribbon(aes(ymin = fit-(2*se.fit), ymax = fit+(2*se.fit),fill=Category), alpha = 0.7, color = 'gray45') + #95CI = (2*SE) for gam function
    scale_fill_manual(labels = groups,
                      values = indpal) + #color combined CI, individual Categorys are gray
    {if("threshold" %in% colnames(df1))
      geom_ribbon(aes(ymin = threshold-(2*se.threshold), ymax = threshold+(2*se.threshold)), 
                  alpha = 0.6, fill = 'gray40', color = 'black')} + #recolor se band for threshold Categorys
    geom_line(size = 0.7) + #full gam function
    geom_point(data = NCRMP0,aes(x = Year, y = unlist(NCRMP0[,k+5])),color='gray30', pch =21, fill = NA, size = 2) + # data points
    scale_color_manual(labels = groups,
                       values = indpal) +
    {if("inc.trend" %in% colnames(df1)) 
      geom_line(aes(x=Year, y = inc.trend),size = 1.2, color = "#009E73", alpha = 0.6)}  +  #increasing trends
    { if("dec.trend" %in% colnames(df1))
      geom_line(aes(x=Year, y = dec.trend),size = 1.2, color = "#D55E00", alpha = 0.6) } +
    labs(y = NCRMPlabs[k], x = "Year",sep='')   
  
  ggsave(paste('figures/NCRMP grouped/',NCRMPvar[k],' GAM_ggplot.png',sep=''), 
         width =  12, height = 3.7, units = 'in', #w & h in inches
         dpi = 300, bg = 'transparent') 
}

###############################################################################
##################################### ECO DATA ###################################
###############################################################################
#tried a few diff relationships in both datasets, so far no patterns observed

#################### IVOR FG Data for previous Atlantis run ###################
FG<-read.csv('data/old data/FG_2010_2016.csv')
FG$ANALYSIS_YEAR<-as.numeric(FG$ANALYSIS_YEAR)
FG$OBS_YEAR<-as.numeric(FG$OBS_YEAR)

#FG$Herbivores<-rowSums(FG[,c('Browsers','Grazers','Scrapers')])
#FG$Fleshy<-rowSums(FG[,c('Macro','Turf')])
FG$Coralline<-rowSums(FG[,c('HARD_CORAL','CCA')])
#FG$CFRatio<-FG$Coralline/FG$Fleshy

#explore data with ggplot below
ggplot(FG, aes(OBS_YEAR, HARD_CORAL)) +
  facet_wrap(~ISLAND) +
#  geom_line() +
  geom_boxplot(aes(group = OBS_YEAR)) #+
 # geom_point() 

######################## dryad ########################
regimes<-read.csv('data/Hawaii_regimes.csv')
regimes$Herbivores<-rowSums(regimes[,c('Browsers','Grazers','Scrapers')])
regimes$Fleshy<-rowSums(regimes[,c('Macro','Turf')])
regimes$Coralline<-rowSums(regimes[,c('Coral','CCA')])
regimes$CFRatio<-regimes$Coralline/regimes$Fleshy

#explore data with ggplot below
ggplot(regimes, aes(Turf, Herbivores)) +
  facet_wrap(~Island) +
  geom_point(aes(color = Exposure)) 


#########################################################################
for(k in 1:length(CSVIvar)) { # CSVI scaled variables are columns 6:7
  df1<-read.csv(paste('data/CSVI ',CSVIvar[k],'_gamfit.csv', sep =''))
  str(df1)
  
  # set County as factor and reorder according to how i want it to be plotted
  df1$County<-factor(df1$County)
  df1 <- df1[,colSums(is.na(df1))<nrow(df1)] #if any trend or threshold columns are all NA, delete
ggplot(df1,
       aes(x = Year, y =  fit)) +
  facet_wrap(~County, nrow = 1) +
  geom_line(data = CSVI0, aes(x = Year, y = unlist(CSVI0[,k+3]),color=Community, linetype=Community), size = 0.5)+  # data points
  scale_linetype_manual(values = c(rep(1:10,5)))
}


############################### MRIP #####################################
########################### load and clean data ##########################
# assign habitat and species groups
catch<-read.csv('data/HI/HI MRIP Catch_SE.csv')
trips<-read.csv('data/HI/HI MRIP Trips_SE.csv')
MRIP.spec<-read.csv('data for DEA/MRIP Rec Data/MRIP HI Catch annual.csv')
MRIP.spec[MRIP.spec$Species == 'GLASSEYE',2]<-'GLASSEYE SNAPPER'
MRIP.spec[MRIP.spec$Species == 'SCALLOPED HAMMERHEAD',2]<-'SCALLOPED HAMMERHEAD SHARK'
write.csv(MRIP.spec,'data/HCC cleaned/MRIP Catch_by species.csv', row.names = F)

MRIP.list<-data.frame(unique(MRIP.spec$Species)) #maybe go through and split species groups like FOSS--but only if there's time
colnames(MRIP.list)<-c('Species')

#pull out the important group names so it is easier to assign habitats in excel
MRIP.list$spec<-gsub(" FAMILY","",MRIP.list$Species) #drop the word FAMILY
MRIP.list$spec.last<-word(MRIP.list$spec,-1) #pull out last word only

write.csv(MRIP.list,paste('data/HCC cleaned/MRIP species list.csv', sep =''), row.names = F)

#assign habitat groups (reef, pelagic, etc) directly in excel
#also reassigned some species groups--most are fine using the last word of the species name
#some changed (etc ALBACORE assigned to TUNA group)
#read in species-habitat list
spec.hab<-read.csv('data/HCC cleaned/MRIP species_habitat.csv') 
MRIPgrouped<-join(MRIP.spec, spec.hab, by = 'Species', type = 'full')

#check NAs
MRIP.others<-MRIPgrouped[which(is.na(MRIPgrouped$Habitat) == 'TRUE'),] #no NAs! every species is accounted for
write.csv(MRIPgrouped,'data/HCC cleaned/MRIP Catch_grouped.csv', row.names = F)

############## simple catch by species time series plot ##############
#read in final mrip catch df with species and habitat groups assigned
MRIPgrouped<-read.csv('data/HCC cleaned/MRIP Catch_grouped.csv')
MRIPgrouped$Habitat<-as.factor(MRIPgrouped$Habitat)
MRIPgrouped$Group<-as.factor(MRIPgrouped$Group)

MRIP.ann<-aggregate(RCatch~Group+Year, data= MRIPgrouped, FUN = sum)
str(MRIP.ann)
MRIP.agg<-aggregate(.~Group, data= MRIP.ann, FUN = sum)
top<-top_n(MRIP.agg, 25, RCatch) 
top<-top[,c('Group','RCatch')]
names(top)<-c('Group','RCatch.tot')
write.csv(top,'data/HCC cleaned/MRIP Catch_top 25.csv')

MRIP.ann0<-join(MRIP.ann,top, by = 'Group', type = 'inner') #keep only Group that are only in top 25 catch
MRIP.ann0<-arrange(MRIP.ann0, desc(RCatch.tot)) #arrange in descending order of total catch
unique(MRIP.ann0$Group) #make sure they are in the right order
MRIP.ann0$Group<-factor(MRIP.ann0$Group, levels = unique(MRIP.ann0$Group)) #convert from chr to factor
#set levels as rearranged by catch
MRIP.ann0$RCatch<-MRIP.ann0$RCatch/1000

#join habitats to annual catch by species
spec.hab<-read.csv('data/HCC cleaned/MRIP species_habitat.csv')
spec.hab<-spec.hab[,c('Group', 'Habitat')]
MRIP.ann0<-join(MRIP.ann0,spec.hab, type ='inner')

#same colorblind friendly palette used in FOSS plots so colors match
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

ggplot(MRIP.ann0, aes(Year, RCatch)) +
  facet_wrap(~Group,scales = "free_y",ncol=5) +
  geom_line(aes(color = Habitat))+
scale_color_manual(values = cbbPalette[c(8,5,6,3,4)]) +
  labs(x="Year", y="Recreational Catch (Thousands of Fish)")+
  theme_bw() +
  theme(axis.title.x = element_blank(), # vjust adjusts vertical space between plot & axis title
           axis.title.y = element_text(size = 21),
        axis.text = element_text(size = 11.5),
           strip.text = element_text(size = 12.5),
           legend.position = 'bottom',
           legend.text=element_text(size=17),
           legend.title=element_blank(),
           panel.grid.major = element_blank(), #delete major grid lines
           panel.grid.minor = element_blank() #delete minor grid lines
  )

ggsave('figures/MRIP/MRIP top species Catch.png', 
       width =  12, height = 9, units = 'in', #w & h in inches
       dpi = 300, bg = 'transparent')

############# add 'all species' group and calculate diversity per group ##############

#double the df and put 'All Species' in habitat column for copy
MRIP.all<-MRIPgrouped
MRIP.all$Habitat<-'All Species'

MRIPgrouped<-rbind(MRIP.all,MRIPgrouped)
write.csv(MRIPgrouped, paste('data/HCC cleaned/MRIP species and groups.csv', sep =''), row.names = F)

#code later will sum everything into an annual all species value, so no need to do it now
#will also have to fix up the all species code that reads in from 'MRIP with Millions'

groups<-(unique(MRIPgrouped$Habitat))

for(i in 1:length(unique(MRIPgrouped$Habitat))) {
  MRIP.group<-MRIPgrouped[MRIPgrouped$Habitat==groups[i],] #subset just reef species and drop species column
  TOT_MRIP <- aggregate(RCatch~Year, data=MRIP.group, FUN=sum)
  
  names(TOT_MRIP) <- c('Year','RCatch.tot') #rename total Catch
  MRIP.group <- merge(MRIP.group,TOT_MRIP, by=c('Year')) #add total column onto original df to get proportions
  
  #calculate shannon index: SUM[-p*ln(p)]
  MRIP.group$P_Catch <- -((MRIP.group$RCatch/MRIP.group$RCatch.tot)*
                            log(MRIP.group$RCatch/MRIP.group$RCatch.tot)) #-p*ln(p)
  MRIP_Div <- aggregate(P_Catch~Year, data=MRIP.group, FUN=sum) #Sum P_Catch by year to get annual diversity values
  
  #effective number of species
  MRIP_Div$CommDiv <- exp(MRIP_Div$P_Catch) #effective num of species as calculated by Shannon Diversity Index
  MRIP_Div <- subset(MRIP_Div, select=c('Year','CommDiv'))
  
  MRIP.group<-join_all(list(TOT_MRIP, MRIP_Div), by = "Year", type = 'full') #join dfs together for annual values of all MRIP vars
  MRIP.group$Catch.Millions<-MRIP.group$RCatch.tot/1000000
  MRIP.fin<-MRIP.group[,c('Year', 'Catch.Millions','CommDiv')]
  
  write.csv(MRIP.fin, paste('data/HCC cleaned/MRIP ',groups[i],' fish RCatch and Diversity.csv', sep =''), row.names = F)
}

################# GAMFIT TNT FUNCTION FOR EACH GROUP AND STACK DFS #############
####stack dfs into one with species separated and a group for all species
#first load in all species to stack dfs for ind groups onto
MRIP<-read.csv('data/HCC cleaned/MRIP All Species fish RCatch and Diversity.csv')
MRIP.spec<-read.csv(paste('data/HCC cleaned/MRIP species and groups.csv', sep =''))

#vectors used
MRIPvar<-colnames(MRIP)
groups<-(unique(MRIP.spec$Habitat))

for(k in 2:3) { # 2 MRIP variables are columns 3:4
  #created 2 empty dfs to bind others to#
  MRIP0 <- data.frame(matrix(ncol = 4, nrow = 0)) #one for data points
  colnames(MRIP0) <- colnames(MRIP)
  
  df<- data.frame(matrix(ncol = 8, nrow = 0)) #one for gamfits
  colnames(df)<-c('Species.Type',"Year","fit", "se.fit", "inc.trend", "dec.trend", "threshold", "se.threshold" )
  
  #run rest of species groups in a loop and rbind each to df-->overwrites with compiled df as the loop runs
  for(i in 1:length(groups)) {
    df0<-read.csv(paste('data/HCC cleaned/MRIP ',groups[i],' fish RCatch and Diversity.csv', sep =''))
    df0$Species.Type<-groups[i]
    if(nrow(unique(df0[k]))>1) { #need to put if there are multiple unique values in target column
      #because CommDiv of Jawed Verts and NWHI is only one because theres only 1 specified species--no GAM model can be calculated
      # so this if() calculates GAMs if current df is NOT CommDiv or JV or NWHI
      gammod<-gam(unlist(df0[k]) ~ s(Year), 
                  data = df0, method = 'REML')
      
      df.new<-gam_tnt(df0, 500,gammod,df0$Year,'Year',df0$Species.Type, 'Species.Type')
      df<-rbind(df.new,df)
    }
    
    if(nrow(unique(df0[k]))==1) { #if is IS CommDiv of JV or NWHI, just save raw div values, which are 1 every year
      
      df.new<-df0[,c('Species.Type','Year','CommDiv')]
      names(df.new)<-c('Species.Type', 'Year', 'fit') #there will be no se, trend, or threshold
      df<-bind_rows(df.new,df)
    }
    
    MRIP0<-rbind(df0,MRIP0)
  }
  
  write.csv(df, paste('data/HCC cleaned/MRIP species compiled ',MRIPvar[k],'_gamfit.csv', sep =''), row.names = F)
  write.csv(MRIP0, 'data/HCC cleaned/MRIP species compiled data points_raw.csv', row.names = F)  
}

############## PLOT ALL SPECIES GROUPS TOGETHER FOR EACH MRIP VAR ###############

groups1<-c("All Species", "Pelagic", "Reef","Deep", "Fishpond","Coastal")
MRIPlabs<-c('1','Recreational Landings (Millions of Fish)', 'Diversity (Effective Number of Species)')
MRIPtitle<-c('1', 'Recreational Landings', 'Catch Diversity')

#read in raw data--includes all vars so read in outside of loop
MRIP0<-read.csv('data/HCC cleaned/MRIP species compiled data points_raw.csv') #for plotting original data points
MRIPvar<-colnames(MRIP0)


str(MRIP0)
MRIP0$Species.Type<-factor(MRIP0$Species.Type, levels = groups1)

for(k in 2:3){ #k is MRIP vars
  df1<-read.csv(paste('data/HCC cleaned/MRIP species compiled ',MRIPvar[k],'_gamfit.csv', sep =''))
  df1 <- df1[,colSums(is.na(df1))<nrow(df1)] #if any trend or threshold columns are all NA, delete
  str(df1)
  
  # set Species.Type as factor and reorder according to how i want it to be plotted
  df1$Species.Type<-factor(df1$Species.Type, levels = groups1)
  # colorblind friendly palette with black and gray(for ribbons):
  cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", '#737373')
  #value 4 and 7 are green and red used for increasing and decreasing trends
  
  ############### plot gam ##################
  ggplot(df1, #switch between df1 or df1cut for zoomed plot
         aes(x = Year, y = fit)) +
    theme(  plot.title=element_text(hjust=0.5, size = 27),
            panel.grid.major = element_blank(), #delete major grid lines
            panel.grid.minor = element_blank(), #delete minor grid lines
            axis.text=element_text(size=22, color = 'black'), #adjust font size of axis tick labels
            axis.title=element_text(size=26,face="plain"), #adjust size of axis titles
            #     axis.title.x = element_text(margin = margin(t = 12, r = 0, b = 0, l = 0)), #increases distance between x axis label and plot
            panel.background = element_rect(fill = "transparent",colour = 'black', size = 1),
            plot.background = element_rect(fill = "transparent",colour = NA),
            plot.margin = unit(c(1.32,2,1.32,0.3), "cm"),
            legend.position = 'right', # c(0.37,0.57),
            legend.background = element_rect(fill="transparent", color = 'black'),
            legend.key = element_rect(fill = "transparent", colour = NA),
            legend.key.size = unit(1, 'cm'),
            legend.spacing.y = unit(0.2,'cm'),
            legend.text=element_text(size=17),
            legend.title=element_blank(),
            legend.margin = margin(3,12,9,10)) +
    guides(fill = guide_legend(byrow = T
                               #, ncol =2 #just put in for the 2 column legend
                               )) + #need this line for legend.spacing.y to work
    scale_x_continuous(breaks=seq(2000,2020,5)) + #so that a tick appears every year--default ticks only where breaks are assigned
    scale_y_continuous(limits = c(min(0,round_any(min(df1$fit-2*df1$se.fit),1, f = floor)), round_any(max(df1$fit+2*df1$se.fit),1, f = ceiling))) + #0 to max point (points go further than CI band)
    geom_ribbon(aes(ymin = fit-(2*se.fit), ymax = fit+(2*se.fit), fill = Species.Type), alpha = 0.3, color = 'gray45') + #95CI = (2*SE) for gam function
    scale_fill_manual(labels = groups1,
                      values = cbbPalette[c(9,3,4,5,6,2)]) + #color combined CI, individual Species.Types are gray
    {if("threshold" %in% colnames(df1))
      geom_ribbon(aes(ymin = threshold-(2*se.threshold), ymax = threshold+(2*se.threshold), group = Species.Type), 
                  alpha = 0.6, fill = 'gray40', color = 'black')} + #recolor se band for threshold Species.Types
    geom_line(size = 0.7, aes(color = Species.Type)) + #full gam function
    scale_color_manual(labels = groups1,
                       values = cbbPalette[c(9,3,4,5,6,2)]) +
    {if("inc.trend" %in% colnames(df1)) 
      geom_line(aes(x=Year, y = inc.trend, group = Species.Type),size = 1.8, color = "#009E73", alpha = 0.6)}  +  #increasing trends
    { if("dec.trend" %in% colnames(df1))
      geom_line(aes(x=Year, y = dec.trend, group = Species.Type),size = 1.8, color = "#D55E00", alpha = 0.6) } +
    labs(y = MRIPlabs[k], x = "Year", title = paste(MRIPtitle[k],sep='')) +
    geom_point(data= MRIP0, #switch between MRIP0 or MRIP0cut for zoomed plot
               aes(x = Year, y = unlist(MRIP0[,k]), 
                   color = Species.Type, shape = Species.Type), fill = NA, size = 2.7) + # data points
    scale_shape_manual(labels = groups1,
                       values = c(4,2,1,0,5,18))
  
  ggsave(paste('figures/MRIP/',MRIPvar[k],' GAM_ggplot.png',sep=''), 
         width =  9.2, height = 6.6, units = 'in', #w & h in inches
         dpi = 300, bg = 'transparent')
  
  ########################### ZOOMED PLOT #############################
  df1cut<-df1[df1$Species.Type == 'Pelagic' |
                df1$Species.Type == "Coastal"|
                df1$Species.Type == 'Deep' |
                df1$Species.Type == "Fishpond",]
  MRIP0cut<-MRIP0[MRIP0$Species.Type == 'Pelagic' |
                    MRIP0$Species.Type == "Coastal"|
                    MRIP0$Species.Type == 'Deep' |
                    MRIP0$Species.Type == "Fishpond",]
  
  # colorblind friendly palette with black:
  cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7",'#737373')
  #value 4 and 7 are green and red used for increasing and decreasing trends
  zoom <-c(1,8,10,3)
  zoombreak<-c(1,0.25,1)
  ggplot(df1cut, #switch between df1 or df1cut for zoomed plot
         aes(x = Year, y = fit)) +
    theme(  panel.grid.major = element_blank(), #delete major grid lines
            panel.grid.minor = element_blank(), #delete minor grid lines
            axis.text=element_text(size=22, color = 'black'), #adjust font size of axis tick labels
            axis.title=element_text(size=26,face="plain"), #adjust size of axis titles
            #     axis.title.x = element_text(margin = margin(t = 12, r = 0, b = 0, l = 0)), #increases distance between x axis label and plot
            panel.background = element_rect(fill = "transparent",colour = 'black', size = 1),
            plot.background = element_rect(fill = "transparent",colour = NA),
            plot.margin = unit(c(0.3,2,0.3,0.3), "cm"),
            legend.position = 'right', # c(0.37,0.57),
            legend.background = element_rect(fill="transparent", color = 'black'),
            legend.key = element_rect(fill = "transparent", colour = NA),
            legend.key.size = unit(1, 'cm'),
            legend.spacing.y = unit(0.2,'cm'),
            legend.text=element_text(size=17),
            legend.title=element_blank(),
            legend.margin = margin(3,12,9,10)) +
    guides(fill = guide_legend(byrow = T)) + #need this line for legend.spacing.y to work
    scale_x_continuous(breaks=seq(2000,2020,5)) + #so that a tick appears every year--default ticks only where breaks are assigned
    scale_y_continuous(breaks=seq(0,round_any(max(df1cut$fit+2*df1cut$se.fit),1, f = ceiling),zoombreak[k]))+
    geom_ribbon(aes(ymin = fit-(2*se.fit), ymax = fit+(2*se.fit), fill = Species.Type), alpha = 0.3, color = 'gray45') + #95CI = (2*SE) for gam function
    scale_fill_manual(labels = groups1[c(2,4:6)],
                      values = cbbPalette[c(3,5,6,2)]) + #color combined CI, individual Species.Types are gray
    {if("threshold" %in% colnames(df1))
      geom_ribbon(aes(ymin = threshold-(2*se.threshold), ymax = threshold+(2*se.threshold), group = Species.Type), 
                  alpha = 0.6, fill = 'gray40', color = 'black')} + #recolor se band for threshold Species.Types
    geom_line(size = 0.7, aes(color = Species.Type)) + #full gam function
    scale_color_manual(labels = groups1[c(2,4:6)],
                       values = cbbPalette[c(3,5,6,2)]) +
    {if("inc.trend" %in% colnames(df1)) 
      geom_line(aes(x=Year, y = inc.trend, group = Species.Type),size = 1.8, color = "#009E73", alpha = 0.6)}  +  #increasing trends
    { if("dec.trend" %in% colnames(df1))
      geom_line(aes(x=Year, y = dec.trend, group = Species.Type),size = 1.8, color = "#D55E00", alpha = 0.6) } +
    labs(y = MRIPlabs[k], x = "Year") +
    geom_point(data= MRIP0cut, #switch between MRIP0 or MRIP0cut for zoomed plot
               aes(x = Year, y = unlist(MRIP0cut[,k]), 
                   color = Species.Type, shape = Species.Type), fill = NA, size = 2.5) + # data points
    scale_shape_manual(labels = groups1[c(2,4:6)],
                       values = c(2,0,5,18))
  
  ggsave(paste('figures/MRIP/',MRIPvar[k],' GAM_zoom.png',sep=''), 
         width =  9.2, height = 4.2, units = 'in', #w & h in inches
         dpi = 300, bg = 'transparent')
}

############## SEPARATE MRIP PLOT FOR TRIPS--NO SPECIES DISAGG DATA ###############

  df1<-read.csv(paste('data/MRIP MC sims/ MC Trips.Thousands all regions summary gamfit_MRIP.csv', sep=''))
  MRIP0<-read.csv(paste('data/MRIP MC sims/MRIP Trips.Thousands_all regions_raw.csv', sep=''))
  df1 <- df1[,colSums(is.na(df1))<nrow(df1)] #if any trend or threshold columns are all NA, delete
  df1<-df1[df1$Region == "Hawai'i",]
  MRIP0<-MRIP0[MRIP0$Region == "Hawai'i",]
  MRIP0<-MRIP0[MRIP0$Year < 2021,]
  
  df1[,c(3:length(colnames(df1)))]<-df1[,c(3:length(colnames(df1)))]/1000 #divide values by 1000 so final units are in millions
  MRIP0[,3]<-MRIP0[,3]/1000

  ############### plot gam ##################
  
  ggplot(df1,
         aes(x = Year, y = fit)) +
    theme(  plot.title=element_text(hjust=0.5, size = 27),
            panel.grid.major = element_blank(), #delete major grid lines
            panel.grid.minor = element_blank(), #delete minor grid lines
            axis.text=element_text(size=22, color = 'black'), #adjust font size of axis tick labels
            axis.title=element_text(size=26,face="plain"), #adjust size of axis titles
            axis.title.y = element_text(vjust = 3.5), #increases distance between x axis label and plot
            panel.background = element_rect(fill = "transparent",colour = 'black', size = 1),
            plot.background = element_rect(fill = "transparent",colour = NA),
            plot.margin = unit(c(0.3,7.4,0.3,0.7), "cm")) +
    scale_x_continuous(breaks=seq(2000,2020,5)) + #so that a tick appears every year--default ticks only where breaks are assigned
    scale_y_continuous(limits = c(min(0,round_any(min(df1$fit-2*df1$se.fit),1, f = floor)), round_any(max(df1$fit+2*df1$se.fit),1, f = ceiling))) + #0 to max point (points go further than CI band)
    geom_ribbon(aes(ymin = fit-(2*se.fit), ymax = fit+(2*se.fit)),fill = '#737373', alpha = 0.3, color = 'gray45') + #95CI = (2*SE) for gam function
    {if("threshold" %in% colnames(df1))
      geom_ribbon(aes(ymin = threshold-(2*se.threshold), ymax = threshold+(2*se.threshold), group = Region), 
                  alpha = 0.7, fill = 'gray40', color = 'black')} + #recolor se band for threshold regions
    geom_line(size = 1,color = '#737373') + #full gam function
    geom_point(data = MRIP0,aes(x = Year, y = unlist(MRIP0[3])), color = '#737373', pch = 4, size = 2.7, alpha = 1, fill = NA) + # data points
    {if("inc.trend" %in% colnames(df1)) 
      geom_line(aes(x=Year, y = inc.trend, group = Region),size = 2, color = "#009E73", alpha = 0.6)}  +  #increasing trends
    { if("dec.trend" %in% colnames(df1))
      geom_line(aes(x=Year, y = dec.trend, group = Region),size = 2, color = "#D55E00", alpha = 0.6) } +
    labs(y = 'Recreational Trips (Millions)', x = "Year", title = 'Recreational Effort')   
  
  ggsave(paste('figures/MRIP/Trips GAM_ggplot.png',sep=''), 
         width =  9.2, height = 5.8, units = 'in', #w & h in inches
         dpi = 300, bg = 'transparent')


  
