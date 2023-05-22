#########################################################################
##########################OUTCOME RANKING: DEA###########################
#########################################################################

##########################################################################
#R Program to compute F-P Index using Toy data from Chris's Book
#Text: "Productivity and Efficiency Analysis", by Chris O'Donnell
#Also calculate Quantity Indices frm page 83, "New Directions: Efficiency and 
#Productivity" by Rolf Fare and Shawna Grosskopf
#Author: John Walden, NMFS, NEFSC
#Edited for 1) HI CSVI (spatial) Data and 2) time series DEA by Lansing Perng, NOAA PIFSC, UHM
#February 27, 2021
#############################################################################
#First Clear any previous data stored in memory, and require lpSolveAPI, Rglpk,
#lpSolveApI and readr
##############################################################################
rm(list=ls())
PKG <- c("lpSolveAPI", "readr", "Rglpk", "Benchmarking","ggplot2", "reshape2", 'psych',
         "data.table", "plyr", "dplyr","Compind", "tidyr","doBy","stringr", "Rmisc", "RColorBrewer")

for (p in PKG) {
  
  if(!require(p,character.only = TRUE)) {  
    install.packages(p)
    require(p,character.only = TRUE)}
}

#################################################################################

#####################################################################################
######################CSVI DATA split by fishing communities#########################
#####################################################################################
############################## load and clean data ##############################
#this is where I was working with the HI soc data (original data from HDWG google drive)
HIsoc0<-read.csv('data/HI_Soc_TS.csv')
HIsoc<-HIsoc0[,c('Year','Community','County','PopDensity',"PCTWaterCover",
                 "Pounds1000", "Value1000", "Dealers1000", "CommPermits1000", "RecFshTrps1000")]

## deflate value
base_year = 2020 # base year for GDP deflator - Maximum = 2020, Minimum = 1947  - max(GDPDEF_quarterly$Year)
GDPDEF_annual<-read.csv("scripts/GDPDEF_annual.csv") #deflator values calculated from 'Annual_GDP_Deflator_Code.R'
REVENUEFILE<-HIsoc %>% 
  mutate(nominal_revenue = Value1000) #renaming revenue nominal revenue to keep track
REVENUEFILE <- as_tibble(merge(REVENUEFILE, GDPDEF_annual, by="Year", all.x=TRUE))
REVENUEFILE[["Dollars1000"]] <- REVENUEFILE[["nominal_revenue"]]*
  unique(GDPDEF_annual$GDPDEF[GDPDEF_annual$Year==base_year])/REVENUEFILE$GDPDEF #deflating
REVENUEFILE$GDPDEF <- NULL
write.csv(REVENUEFILE,'data/HI_Soc_TS_deflated.csv', row.names = F) 
##? for now not sure if this dataset needs deflation
# i looked into the jepson et al. reference for the csvi data collection methods
# source does not mention deflation

HIsoc0<-read.csv('data/HI_Soc_TS_deflated.csv')

#fix diacriticals
HIsoc0$County<-gsub("Hawaii County","Hawai‘i County",HIsoc0$County) #fix the okina
HIsoc0$County<-gsub("Kauai County","Kaua‘i County",HIsoc0$County) #fix the okina
diacrit<-read.csv('data/diacriticals.csv', encoding = 'UTF-8')

HIsoc0<-join(HIsoc0,diacrit)
HIsoc0$Community<-HIsoc0$Diacritical

HIsoc<-HIsoc0[,c('Year','Community','County','PopDensity',"PCTWaterCover",
                 "Pounds1000", "Dollars1000", "Dealers1000", "CommPermits1000", "RecFshTrps1000")]

#rescale so raw ind plots have similar scales
#wouldn't be necessary for the DEA calculation because we normalized to mean for DEA calcs anyway
HIsoc[,c('PopDensity.Thousands',"Pounds.Thousands1000", "Dollars.Thousands1000")]<-HIsoc[,c('PopDensity',"Pounds1000", "Dollars1000")]/1000

####output soc
soc<-HIsoc[,c('Year','Community','County',
              "Pounds1000", "Dollars1000", "Dealers1000", "CommPermits1000", "RecFshTrps1000")]
soc<-na.omit(soc)
soc$Community<-as.factor(soc$Community)

##### visualize data summarized by Community and by County #####
#convert to long format to plot all dependent vars as facets
df_long<-pivot_longer(HIsoc,c(5,8:13), 'Indicator','value')
unique(df_long$Indicator) #check if all inds are there

#df_long<-df_long[df_long$County== 'Maui County',] #just to check spreckelsville
#quick plot by community
ggplot(df_long, aes(Year, value)) +
  facet_wrap(~Indicator,scales = "free_y",ncol=5) +
  geom_line(aes(group = Community, color = Community)) #plots lines for each community color coded by county

df_summary<-df_long %>%
  group_by(Year,County,Indicator) %>%
  dplyr::summarise(val.mean = mean(value, na.rm = T), val.se = sd(value, na.rm = T)/sqrt(n()))

df_summary<-df_summary[df_summary$County!='Kalawao County',]

write.csv(df_summary, file="outputs/CSVI over time by County_se_long.csv", row.names = F)

###################### plot by County with mean & se ###################### 
pd <- position_dodge(0.2) # move them .02 to the left and right

countypal4<-c('#e78ac3','#8da0cb','#66c2a5','#fc8d62')
#reorder by counties in geographic order using dplyr
counordered <- c("Kaua‘i County", "Honolulu County", "Maui County", 'Hawai‘i County')

df_summary<-read.csv("outputs/CSVI over time by County_se_long.csv")
df_summary$Indicator <- factor(df_summary$Indicator, 
                               levels = c("Pounds.Thousands1000", "Dollars.Thousands1000", "Dealers1000", "CommPermits1000", "RecFshTrps1000",
                                          'PopDensity.Thousands',"PCTWaterCover"), 
                               labels = c("Pounds\n (Thousands)", "Dollars\n (Thousands)", "Dealers", 
                                          "Commercial\n Permits", "Recreational\n Trips",
                                         'Population\n per sq km',"Percent\n Water Cover"))
df_summary$County<-factor(df_summary$County, levels = counordered)

ggplot(df_summary, aes(Year, val.mean)) +
  facet_wrap(~Indicator,scales = "free_y",ncol=5,
             strip.position = 'left') +
 # geom_errorbar(aes(ymin=val.mean-val.se, ymax=val.mean+val.se,color = County), width=.1, position=pd) +
  #commented out variation for visual clarity
  geom_line(aes(color = County), size = 1) + #plots lines for each community color coded by county
  scale_color_manual(labels=counordered,
                     values =countypal4) +

  geom_point(aes(color = County, shape = County), size = 2.5) +
  scale_shape_manual(labels=counordered,
                     values =c(21:24)) +  
  scale_x_continuous(breaks=seq(2010,2018,4)) + #so that a tick appears every year--default ticks only where breaks are assigned
  theme_bw() +
  theme(plot.title = element_text(size = 17, hjust =0.5),
        axis.title.x = element_text(size = 16), # vjust adjusts vertical space between plot & axis title
        axis.title.y = element_blank(),
        axis.text = element_text(size = 10),
        strip.text = element_text(size = 14, vjust = -0.14),
        legend.text=element_text(size=17),
        legend.title=element_blank(),
        legend.key.size = unit(1, 'cm'),
        legend.spacing.y = unit(0.6,'cm'),
        legend.position = c(0.71,0.24),
        legend.background = element_rect(fill="transparent"),
        legend.margin = margin(3,12,9,10),
        panel.grid.major = element_blank(), #delete major grid lines
        panel.grid.minor = element_blank(), #delete minor grid lines
        panel.spacing.x = unit(-0.2, "lines"),
        #    panel.spacing.y = unit(2, "lines"),
        strip.background = element_blank(),
        strip.placement = "outside") +
  guides(color=guide_legend(nrow=2),byrow=T) + #so the legend is ordered by rows not columns
  ggtitle('Indicator Values per Thousand Population')
#ggsave(paste('figures/DEA/Indicators over time by County_se.png',sep=''), 
 #      width =  12, height = 3.3, units = 'in', #w & h in inches
  #     dpi = 300, bg = 'transparent')

#plot with no error
ggsave(paste('figures/DEA/Indicators over time by County_means.png',sep=''), 
       width =  11.5, height = 3.7, units = 'in', #w & h in inches
       dpi = 300, bg = 'transparent')

########################## normalize outputs and inputs for DEA ##########################

#divide each value by column mean (which is mean of each type of landing across all years and regions)
soc$Pounds1000=soc$Pounds1000/mean(soc$Pounds1000)
soc$Dollars1000=soc$Dollars1000/mean(soc$Dollars1000)
soc$Dealers1000=soc$Dealers1000/mean(soc$Dealers1000)
soc$CommPermits1000=soc$CommPermits1000/mean(soc$CommPermits1000)
soc$RecFshTrps1000=soc$RecFshTrps1000/mean(soc$RecFshTrps1000)

#####inputs

#pop density
pop<-HIsoc[,c(1,2,4)]
pop<-pop[!is.na(pop$PopDensity),]
pop_region<-aggregate(PopDensity~Community, data = pop, FUN = mean) #average indicator values for each comm over all years
colnames(pop_region)<-c('Community','PopRegAvg')
pop1<-join(pop,pop_region,by="Community",type="inner")
pop1$Popdens.norm=pop1$PopDensity/pop1$PopRegAvg #Popdens.norm is SXI (from formula) divided by regional avg SXI
pop2<-as.data.frame(pop1[,c("Year","Community","Popdens.norm")])
pop2<-droplevels(pop2)
#final pop density index as Popdens.norm in df pop2

#same for PCT water cover
pwc<-HIsoc[,c(1,2,5)]
pwc<-pwc[!is.na(pwc$PCTWaterCover),]
#  pwc$Community<-as.factor(pwc$Community)
#  pwc$PCTWaterCover<-as.numeric(pwc$PCTWaterCover)
pwc_region<-aggregate(PCTWaterCover~Community, data = pwc, FUN = mean) #average indicator values by Year
colnames(pwc_region)<-c('Community','pwcRegAvg')
pwc1<-join(pwc,pwc_region,by="Community",type="inner")
pwc1$PWC.norm=pwc1$PCTWaterCover/pwc1$pwcRegAvg #PWC.norm is SXI (from formula) divided by regional avg SXI
pwc2<-as.data.frame(pwc1[,c("Year","Community","PWC.norm")])
pwc2<-droplevels(pwc2)

inputs<-join(pop2, pwc2, by=c("Year","Community"), type="inner")

final<-join(inputs,soc, by=c("Year","Community"), type="inner")
write_csv(final, 'data/HI_CSVI_final1.csv')

####################################################################################
###############################DEA by Community####################################
####################################################################################
#make input and ouput dfs
final<- read_csv('data/HI_CSVI_final1.csv')

Y=final[,c("Pounds1000","Dollars1000",
           "Dealers1000","CommPermits1000", "RecFshTrps1000")]
(M=ncol(Y))
(J=nrow(Y))
#X<-as.matrix(pprmean[,"pprnorm"])
XE<-final[,c("PWC.norm", "Popdens.norm")]
(NXE=ncol(XE))
YX=cbind(Y,XE)
#End of DATA Step
###################### Define A Matrix for GLPK and LPSolve ######################
A=matrix(0,M+NXE+1,J+1)
#Next, Transform YX matrix and copy to A. 
A[1:(M+NXE),1:J]=t(YX)
A[(M+NXE+1),1:J]=1.0

############################ calculate F-P model ################################
#This next code segment calculates the F-P model from equation #8 in 
#Fare et. al. 2015 Marine Policy Using GLPK

###First Program is for Numerator of Output Quantity Index
#We use GLPK package for LP model
# numerator represents maximal outputs for each observation (per community per time period)

obj=c(rep(0,J),1)
(dir=c(rep('>=',M),rep('==',NXE), '<='))
max<-TRUE
objvals1=0
status1=0
for(j in 1:J){
  A[1:M,J+1]=-A[1:M,j]                               #last column of A Matrix set equal to 
  #observation j output
  rhs=c(rep(0,M),as.matrix(XE[1,]),1)                #rhs is being set to 0 for outputs, 
  #obs #1 from environmental data XE
  
  sol<- Rglpk_solve_LP(obj, A, dir, rhs, max=max)    #Define model and use RGLPK to solve
  if(j%%100==0|j==J)  print(paste('on dmu',j,'of',J))#shows when 100 observations solved
  objvals1[j]=round(1/sol$optimum,4)                 #objective function value 
  #returned by sol$optimum
  status1[j]=sol$status                              #For Error Checking. 
  #If Model solves, all values should equal zero
}
#### Denominator of Output Quantity Index - based on optimization using one reference firm
# denominator represents reference observation (obs 1 is Hilo 2010)

#Define A Matrix for GLPK. 
A=matrix(0,M+NXE+1,J+1)
#Next, Transform YX matrix and copy to A. 
A[1:(M+NXE),1:J]=t(YX)
A[(M+NXE+1),1:J]=1.0
obj=c(rep(0,J),1)
(dir=c(rep('>=',M),rep('==',NXE), '<='))
max<-TRUE
status2=0
objvals2=0
for(j in 1:J){
  A[1:M,J+1]=as.matrix(-Y[1,])
  rhs=c(rep(0,M),as.matrix(XE[1,]),1) 
  sol<- Rglpk_solve_LP(obj, A, dir, rhs, max=max)
  if(j%%100==0|j==J)  print(paste('on dmu',j,'of',J))
  objvals2[j]=round(1/sol$optimum,5)
  status2[j]=sol$status
}

EI=round(objvals1/objvals2, 5) # social output index is obs/ref

#### Environmental Input Index. 
##First program is for the Numerator
#represents minimal env inputs needed to produce a certain level of output

#Define A Matrix for GLPK. 
A=matrix(0,M+NXE+1,J+1)
#Next, Transform YX matrix and copy to A. 
A[1:(M+NXE),1:J]=t(YX)
A[(M+NXE+1),1:J]=1.0

obj=c(rep(0,J),1)
(dir=c(rep('==',M),rep('<=',NXE), '<='))
max<-FALSE                                           #FALSE means minimization Model
status3=0
objvals3=0
for(j in 1:J){
  A[(M+1):(M+NXE),J+1]=-A[(M+1):(M+NXE),j]            #last column of A matrix set equal 
  #to Environmental variables 
  rhs=c(as.matrix(Y[1,]),rep(0,NXE),1)                #Constraint structure is different 
  #than model #1
  
  sol<- Rglpk_solve_LP(obj, A, dir, rhs, max=FALSE)
  if(j%%100==0|j==J)  print(paste('on dmu',j,'of',J))
  objvals3[j]=round(1/sol$optimum,4)
  status3[j]=sol$status
}
### denominator of environmental quality variable
# reference observation (obs 1 is Hilo 2010)
#Define A Matrix for GLPK and LPSolve. 
A=matrix(0,M+NXE+1,J+1)
#Next, Transform YX matrix and copy to A. 
A[1:(M+NXE),1:J]=t(YX)
A[(M+NXE+1),1:J]=1.0

obj=c(rep(0,J),1)
(dir=c(rep('==',M),rep('<=',NXE), '<='))
max<-FALSE
status4=0
objvals4=0
for(j in 1:J){
  A[(M+1):(M+NXE),J+1]=as.matrix(-XE[1,])
  rhs=c(as.matrix(Y[1,]),rep(0,NXE),1) 
  sol<- Rglpk_solve_LP(obj, A, dir, rhs, max=max)
  if(j%%100==0|j==J)  print(paste('on dmu',j,'of',J))
  objvals4[j]=round(1/sol$optimum,4)
  status4[j]=sol$status
}
ZI=objvals3/objvals4

EQI=EI/ZI          #Final Index Number

resfinal<-as.data.table(cbind(final[,c(1,2,5)],ZI,EI,EQI))
colnames(resfinal)[1]<-"Year"
write_csv(resfinal, 'data/DEA_indices.csv')

#####################################################################################
######################################Figures########################################
#####################################################################################
# using package 'psych'

#reorganize data so plots are arranged the way i want
### reorder df so plots are arranged correctly (by community within county)
resfinal<-read_csv('data/DEA_indices.csv')

#get county means
temp.resfinal<-resfinal[,-2]
temp.resfinal0<-aggregate(.~Year+County, data = temp.resfinal, FUN = mean)

#reorder by counties in geographic order using dplyr
x <- c("Kaua‘i County", "Honolulu County", "Maui County", 'Hawai‘i County')

#reorders counties
resfinal<-resfinal %>%
  mutate(County =  factor(County, levels = x)) %>%
  arrange(County) 

#create community vector to reorder communities
#since this is what actually will appear on the x axis
#necessary to get the correct order
comm.order<-unique(resfinal$Community)

#reorders communities by the order developed from reordering counties
resfinal<-resfinal %>%
  mutate(Community =  factor(Community, levels = comm.order)) %>%
  arrange(Community) 
# cannot save reordered df as csv because levels info is lost
# when reading in, defaults to alphabetical order when plotting

#####some hyp testing to compare values by county
kruskal.test(ZI~County,data=resfinal) #(input)
kruskal.test(EI~County,data=resfinal) #** (output)
kruskal.test(EQI~County,data=resfinal)#** (input-output)

########################################################################
##################### BOXPLOTS BY COUNTY/COMMUNITY #####################
########################################################################
countypal4<-c('#e78ac3','#8da0cb','#66c2a5','#fc8d62')
##################################EQI###################################
## by community
temp2<-ggplot(resfinal, aes(x=Community, y=EQI)) +
  theme(  plot.title=element_text(hjust=0.5, size = 17),
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
          axis.text.y = element_text(size = 11),
          axis.title.x = element_text(margin = margin(-7,0,0,0)),
          axis.title = element_text(size = 13),
          panel.grid.major = element_blank(), #delete major grid lines
          panel.grid.minor = element_blank(), #delete minor grid lines
          panel.background = element_rect(fill = "transparent",colour = 'black', size = 1),
          plot.background = element_rect(fill = "transparent",colour = NA),
          legend.position = 'top', legend.margin = margin(-4,0,-6,0),
          legend.title=element_blank(),
          legend.background = element_rect(fill="transparent"),
          legend.text = element_text(size = 11),
          legend.key.size = unit(0.7, 'cm'),
          legend.key = element_rect(fill = "transparent", colour = NA)) +
  geom_boxplot(aes(color=County, fill = County), lwd = 1, outlier.alpha = 1, alpha =0.3)+
  scale_color_manual(values =countypal4)+
  scale_fill_manual(values =countypal4)+
  scale_x_discrete(expand = c(0.005, 1)) +
  labs(x="Fishing Community", y="Social-Ecological Index" , title="Social-Ecological Productivity by Fishing Community")

temp2

ggsave('figures/DEA/EQI by Community.png', 
       width =  9, height = 5.5, units = 'in', #w & h in inches
       dpi = 300, bg = 'transparent')

#######################REPLOT WITH COUNTY MEDIANS########################

##############plot all data split by county
temp2.1<-ggplot(resfinal, aes(x=County, y=EQI)) +
  theme(  plot.title=element_text(hjust=0.5, size = 17),
          axis.text = element_text(size = 11),
          axis.title = element_text(size = 13),
          panel.grid.major = element_blank(), #delete major grid lines
          panel.grid.minor = element_blank(), #delete minor grid lines
          panel.background = element_rect(fill = "transparent",colour = 'black', size = 1),
          plot.background = element_rect(fill = "transparent",colour = NA),
          legend.position = 'top', legend.margin = margin(-4,0,-6,0),
          legend.title=element_blank(),
          legend.text = element_text(size = 11),
          legend.background = element_rect(fill="transparent"),
          legend.key.size = unit(0.7, 'cm'),
          legend.key = element_rect(fill = "transparent", colour = NA))  + 
  geom_boxplot(aes(color=County, fill = County), alpha = 0.3, lwd = 1, outlier.alpha = 1,
               width = 0.62)+
  scale_color_manual(values =countypal4)+
  scale_fill_manual(values =countypal4)+
  labs(x="County", y="Social-Ecological Index " , title="Social-Ecological Productivity by County")

temp2.1

ggsave('figures/DEA/EQI by County.png', 
       width =  9, height = 4.8, units = 'in', #w & h in inches
       dpi = 300, bg = 'transparent')

###############################PLOT EI##################################
##by community
temp2qi<-ggplot(resfinal, aes(x=Community, y=EI))+
  theme( plot.title=element_text(hjust=0.5, size = 17),
         axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
         axis.text.y = element_text(size = 11),
         axis.title.x = element_text(margin = margin(-7,0,0,0)),
         axis.title = element_text(size = 13),
         panel.grid.major = element_blank(), #delete major grid lines
         panel.grid.minor = element_blank(), #delete minor grid lines
         panel.background = element_rect(fill = "transparent",colour = 'black', size = 1),
         plot.background = element_rect(fill = "transparent",colour = NA),
         legend.position = 'top', legend.margin = margin(3,0,-7,0),
         legend.title=element_blank(),
         legend.background = element_rect(fill="transparent"),
         legend.text = element_text(size = 11),
         legend.key.size = unit(0.7, 'cm'),
         legend.key = element_rect(fill = "transparent", colour = NA)) +
  geom_boxplot(aes(color=County, fill = County), alpha =0.3, lwd = 1, outlier.alpha = 1)+
  scale_color_manual(values =countypal4)+
  scale_fill_manual(values =countypal4)+
  scale_x_discrete(expand = c(0.03, 1)) +
  labs(x="Fishing Community", y="Social Output Index" , title="Social Performance by Fishing Community")

temp2qi

ggsave('figures/DEA/EI by Community.png', 
       width =  9, height = 5.5, units = 'in', #w & h in inches
       dpi = 300, bg = 'transparent')

#######################REPLOT WITH COUNTY MEDIANS########################
temp2qi1<-ggplot(resfinal, aes(x=County, y=EI)) +
  theme(   plot.title=element_text(hjust=0.5, size = 17),
           axis.text = element_text(size = 11),
           axis.title = element_text(size = 13),
           panel.grid.major = element_blank(), #delete major grid lines
           panel.grid.minor = element_blank(), #delete minor grid lines
           panel.background = element_rect(fill = "transparent",colour = 'black', size = 1),
           plot.background = element_rect(fill = "transparent",colour = NA),
           legend.position = 'top', legend.margin = margin(3,0,-7,0),
           legend.title=element_blank(),
           legend.text = element_text(size = 11),
           legend.background = element_rect(fill="transparent"),
           legend.key.size = unit(0.7, 'cm'),
           legend.key = element_rect(fill = "transparent", colour = NA))  + 
  geom_boxplot(aes(color=County, fill = County), alpha = 0.3, lwd = 1, outlier.alpha = 1,
               width = 0.62)+
  scale_color_manual(values =countypal4)+
  scale_fill_manual(values =countypal4)+
  labs(x="County", y="Social Output Index" , title="Social Performance by County")

temp2qi1

ggsave('figures/DEA/EI by County.png', 
       width =  9, height = 5.5, units = 'in', #w & h in inches
       dpi = 300, bg = 'transparent')

###############################PLOT ZI##################################
##by community
temp2zi<-ggplot(resfinal, aes(x=Community, y=ZI, fill = Community))+
  theme(  plot.title=element_text(hjust=0.5, size = 17),
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
          axis.text.y = element_text(size = 11),
          axis.title.x = element_text(margin = margin(-7,0,0,0)),
          axis.title = element_text(size = 13),
          panel.grid.major = element_blank(), #delete major grid lines
          panel.grid.minor = element_blank(), #delete minor grid lines
          panel.background = element_rect(fill = "transparent",colour = 'black', size = 1),
          plot.background = element_rect(fill = "transparent",colour = NA),
          legend.position = 'top', legend.margin = margin(3,0,-7,0),
          legend.title=element_blank(),
          legend.background = element_rect(fill="transparent"),
          legend.text = element_text(size = 11),
          legend.key.size = unit(0.7, 'cm'),
          legend.key = element_rect(fill = "transparent", colour = NA)) +
  geom_boxplot(aes(color=County, fill =County),alpha =0.3, lwd = 1, outlier.alpha = 1)+
  scale_color_manual(values =countypal4)+
  scale_fill_manual(values =countypal4)+
  scale_x_discrete(expand = c(0.03, 1)) +
  labs(x="Fishing Community", y="Environmental Input Index" , title="Environmental Input by Fishing Community")

temp2zi
ggsave('figures/DEA/ZI by Community.png', 
       width =  9, height = 5.5, units = 'in', #w & h in inches
       dpi = 300, bg = 'transparent')

#######################REPLOT WITH COUNTY MEDIANS########################
temp2zi1<-ggplot(resfinal, aes(x=County, y=ZI)) +
  theme( plot.title=element_text(hjust=0.5, size = 17),
         axis.text = element_text(size = 11),
         axis.title = element_text(size = 13),
         panel.grid.major = element_blank(), #delete major grid lines
         panel.grid.minor = element_blank(), #delete minor grid lines
         panel.background = element_rect(fill = "transparent",colour = 'black', size = 1),
         plot.background = element_rect(fill = "transparent",colour = NA),
         legend.position = 'top', legend.margin = margin(3,0,-7,0),
         legend.title=element_blank(),
         legend.text = element_text(size = 11),
         legend.background = element_rect(fill="transparent"),
         legend.key.size = unit(0.7, 'cm'),
         legend.key = element_rect(fill = "transparent", colour = NA))  + 
  geom_boxplot(aes(color=County, fill = County), alpha = 0.3, lwd = 1, outlier.alpha = 1,
               width = 0.62)+
  scale_color_manual(values =countypal4)+
  scale_fill_manual(values =countypal4)+
  labs(x="County", y="Environmental Input Index" , title="Environmental Input by County")

temp2zi1

ggsave('figures/DEA/ZI by County.png', 
       width =  9, height = 5.5, units = 'in', #w & h in inches
       dpi = 300, bg = 'transparent')

########################################################################
########################PLOT INDICES OVER TIME##########################
########################################################################
############# palettes used #########
## create a palette for all communities with themed colors per county
#Kauai: 9; Honolulu:7;  Maui 13; Hawaii: 12
mypalette<-c(brewer.pal(9,"RdPu"), 
             brewer.pal(7,"Blues"),
             brewer.pal(9,"Greens"),brewer.pal(4,"Greens"),
             brewer.pal(9,"Oranges"),brewer.pal(3,"Oranges"))
countypal<- c(rep('#e78ac3',9), rep('#8da0cb',9),rep('#66c2a5',9), rep('#fc8d62',9))
#must specify color for each line segment since i can only specify one color scheme for scale_color_manual

##################################EI####################################

##############by community
ei1<-ggplot(resfinal, aes(x=Year, y=EI))+
  facet_wrap(~Community)+
  labs(x="Year", y="Social Output Index", title="Social Performance by Fishing Community 2010-2018")+
  geom_line(aes(color = County)) + #plots lines for each community color coded by county
  scale_x_continuous(breaks = seq(2012,2018,3)) +
  scale_color_manual(values =countypal4)+
  theme_bw() +
  theme(plot.title = element_text(size = 17,hjust=0.5), # vjust adjusts vertical space between plot & axis title
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 10),
        #  axis.text.x = element_text(angle = 45),
        strip.text = element_text(size = 10),
        legend.position = 'bottom', legend.margin = margin(-1,0,-5,0),
        legend.text=element_text(size=12),
        legend.title=element_blank(),
        panel.grid.major = element_blank(), #delete major grid lines
        panel.grid.minor = element_blank() #delete minor grid lines'
  )

ei1
ggsave('figures/DEA/EI Over Time by Community.png', 
       width =  9, height = 7, units = 'in', #w & h in inches
       dpi = 300, bg = 'transparent')

###################by county
#using package 'Rmisc' 
res_countySUM <- summarySE(resfinal, measurevar="EI", groupvars=c("Year","County"))
res_countySUM

ei2<-ggplot(resfinal, aes(x=Year, y=EI))+
  geom_line( aes(group = Community, color = Community))+
  scale_color_manual(values = mypalette) +
  geom_line(data = res_countySUM, size =2, alpha = 0.7, aes(color = County), col = countypal) +
  geom_point(data = res_countySUM, pch =1) +
  geom_errorbar(data = res_countySUM,aes(ymin=EI-se, ymax=EI+se), width=.1, alpha = 0.5) +
  facet_wrap(~County)+
  labs(x="Year", y="Social Output Index")+
  theme(plot.title=element_text(hjust=0.5, size = 21),
        axis.text = element_text(size = 14),
        axis.title.x = element_text(size = 17, vjust = -0.7), # vjust adjusts vertical space between plot & axis title
        axis.title.y = element_text(size = 17, vjust = 3),
        strip.background =element_rect(fill="gray94", colour = 'black', size =1), #change panel label color
        strip.text = element_text(colour = 'black', size = 18),
        panel.grid.major = element_blank(), #delete major grid lines
        panel.grid.minor = element_blank(), #delete minor grid lines
        panel.background = element_rect(fill = "transparent",colour = 'black', size = 1),
        panel.spacing = unit(0.7, "lines"),
        plot.background = element_rect(fill = "transparent",colour = NA),
        plot.margin = unit(c(0,0,0.5,0.5), "cm"),
        legend.position = 'none'
  )

ei2
ggsave('figures/DEA/EI Over Time by County.png', 
       width =  11.5, height = 7, units = 'in', #w & h in inches
       dpi = 300, bg = 'transparent')

##################################ZI####################################
zi1<-ggplot(resfinal, aes(x=Year, y=ZI))+
  facet_wrap(~Community)+
  labs(x="Year", y="Environmental Input Index", title="Environmental Index by Fishing Community 2010-2018")+
  geom_line(aes(color = County)) + #plots lines for each community color coded by county
  scale_x_continuous(breaks = seq(2012,2018,3)) +
  scale_color_manual(values =countypal4)+
  theme_bw() +
  theme(plot.title = element_text(size = 17,hjust=0.5), # vjust adjusts vertical space between plot & axis title
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 10),
        #  axis.text.x = element_text(angle = 45),
        strip.text = element_text(size = 10),
        legend.position = 'bottom', legend.margin = margin(-1,0,-5,0),
        legend.text=element_text(size=12),
        legend.title=element_blank(),
        panel.grid.major = element_blank(), #delete major grid lines
        panel.grid.minor = element_blank() #delete minor grid lines'
  )

zi1
ggsave('figures/DEA/ZI Over Time by Community.png', 
       width =  9, height = 7, units = 'in', #w & h in inches
       dpi = 300, bg = 'transparent')

#################by county
#using package 'Rmisc' 
res_countySUM <- summarySE(resfinal, measurevar="ZI", groupvars=c("Year","County"))
res_countySUM

zi2<-ggplot(resfinal, aes(x=Year, y=ZI))+
  geom_line( aes(group = Community, color = Community))+
  scale_color_manual(values = mypalette) +
  geom_line(data = res_countySUM, size =2, alpha = 0.7, aes(color = County), col = countypal) +
  geom_point(data = res_countySUM, pch =1) +
  geom_errorbar(data = res_countySUM,aes(ymin=ZI-se, ymax=ZI+se), width=.1, alpha = 0.5) +
  facet_wrap(~County)+
  labs(x="Year", y="Environmental Input Index")+
  theme(plot.title=element_text(hjust=0.5, size = 21),
        axis.text = element_text(size = 14),
        axis.title.x = element_text(size = 17, vjust = -0.7), # vjust adjusts vertical space between plot & axis title
        axis.title.y = element_text(size = 17, vjust = 3),
        strip.background =element_rect(fill="gray94", colour = 'black', size =1), #change panel label color
        strip.text = element_text(colour = 'black', size = 18),
        panel.grid.major = element_blank(), #delete major grid lines
        panel.grid.minor = element_blank(), #delete minor grid lines
        panel.background = element_rect(fill = "transparent",colour = 'black', size = 1),
        panel.spacing = unit(0.7, "lines"),
        plot.background = element_rect(fill = "transparent",colour = NA),
        plot.margin = unit(c(0,0,0.5,0.5), "cm"),
        legend.position = 'none'
  )

zi2
ggsave('figures/DEA/ZI Over Time by County.png', 
       width =  9, height = 7, units = 'in', #w & h in inches
       dpi = 300, bg = 'transparent')

##################################EQI###################################
eqi1<-ggplot(resfinal, aes(x=Year, y=EQI))+
  facet_wrap(~Community)+
  labs(x="Year", y="Social-Ecological Index",title="Social-Ecological Productivity by Fishing Community 2010-2018" )+
  geom_line(aes(color = County), size = 0.9) + #plots lines for each community color coded by county
  scale_x_continuous(breaks = seq(2012,2018,3)) +
  scale_color_manual(values =countypal4)+
  theme_bw() +
  theme(plot.title = element_text(size = 17,hjust=0.5), # vjust adjusts vertical space between plot & axis title
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 10),
        #  axis.text.x = element_text(angle = 45),
        strip.text = element_text(size = 9),
        legend.position = 'top', legend.margin = margin(-1,0,-5,0),
        legend.text=element_text(size=13),
        legend.title=element_blank(),
        panel.grid.major = element_blank(), #delete major grid lines
        panel.grid.minor = element_blank() #delete minor grid lines'
  )

eqi1
ggsave('figures/DEA/EQI Over Time by Community.png', 
       width =  10, height = 8, units = 'in', #w & h in inches
       dpi = 300, bg = 'transparent')

##################by county
#using package 'Rmisc' 
res_countySUM <- summarySE(resfinal, measurevar="EQI", groupvars=c("Year","County"))
res_countySUM

eqi2<-ggplot(resfinal, aes(x=Year, y=EQI))+
  geom_line( aes(group = Community, color = Community))+
  scale_color_manual(values = mypalette) +
  geom_line(data = res_countySUM, size =2, alpha = 0.7, aes(color = County), col = countypal) +
  geom_point(data = res_countySUM, pch =1) +
  geom_errorbar(data = res_countySUM,aes(ymin=EQI-se, ymax=EQI+se), width=.1, alpha = 0.5) +
  facet_wrap(~County)+
  labs(x="Year", y="Social-Ecological Productivity Index")+
  theme(plot.title=element_text(hjust=0.5, size = 21),
        axis.text = element_text(size = 14),
        axis.title.x = element_text(size = 17, vjust = -0.7), # vjust adjusts vertical space between plot & axis title
        axis.title.y = element_text(size = 17, vjust = 3),
        strip.background =element_rect(fill="gray94", colour = 'black', size =1), #change panel label color
        strip.text = element_text(colour = 'black', size = 18),
        panel.grid.major = element_blank(), #delete major grid lines
        panel.grid.minor = element_blank(), #delete minor grid lines
        panel.background = element_rect(fill = "transparent",colour = 'black', size = 1),
        panel.spacing = unit(0.7, "lines"),
        plot.background = element_rect(fill = "transparent",colour = NA),
        plot.margin = unit(c(0,0,0.5,0.5), "cm"),
        legend.position = 'none'
  )

eqi2
ggsave('figures/DEA/EQI Over Time by County.png', 
       width =  9, height = 7, units = 'in', #w & h in inches
       dpi = 300, bg = 'transparent')

#################################################################################

#################################################################################
############################# DEA CALCS for statewide dfs #######################
#################################################################################

#using HI indicator cleaned data from 'FP_cross_region_norm_edited.R' (line 81)
df0<-read.csv('data/HI DEA df_clean.csv') 
df0$Fishing.Seaf.Emp<-df0$BLS.Emp.Thousands + df0$NES.Emp.Thousands
df<-df0[,c("Year",'chlorA','Comm.Catch.Mil',"CommRev.Mil",'RevDiv',"Catch.Millions",'Trips.Millions',"Fishing.Seaf.Emp", 'NES.Emp.Thousands',"MPOP")]

#load ncrmp data used in ncrmp gams
NCRMP<-read_csv('data/NCRMP_long_deflated_scaled.csv')
NCRMP0<-NCRMP[,c('Year', 'Category','Individuals.Thousands','Dollars.Millions')]
NCRMPwide<-NCRMP0 %>% 
  gather(variable, value, -(Year:Category)) %>% #first turn the two value columns to long format
  unite(temp, Category, variable) %>% #then join the two variable columns into a 'temp' column
  spread(temp, value) #finally spread values acoss the 'temp' variable encompassing:
#the 3 groups (tourism, visitors, living resources) and value type (individuals or dollars)

#keep tourism GDP and visitor spending--matches with bls/nes emp and vis spending is akin to catch revenue
#drop liv res because it might overlap with fishing and/or seafood emp
NCRMP<-NCRMPwide[,c('Year',"Tourism_Individuals.Thousands","Visitors_Dollars.Millions")]
final0<-join(df,NCRMP, type='inner')
#final0$Tourism_Individuals.Thousands<- final0$Tourism_Individuals.Thousands/final0$MPOP #normalize tourism emp to pop
final0<-final0[,-which(names(final0) %in% c("MPOP"))] #drop MPOP

#add some region specific env data from dryad
dry<-read.csv('data/dryad annual averages.csv')

#normalize for easy exploratory plots
#divide each value by column mean
for(i in 2:14) {
  dry[i]=dry[i]/mean(dry[,i]) #mean() function requires specification of exact column number
}

dry$Ratio<-dry$Coral/dry$Algae*100

#explore env data
plot(Herbivores~Year, data=dry, ylim = c(0,2),xlim = c(2005,2016) )
points(dry$Year, dry$Calcifiers, col = 'blue') #
points(dry$Year, dry$SecConsumers, col = 'red')
points(dry$Year, dry$Ratio, col = 'green') #more info than just calcifiers

points(final0$Year, final0$chlorA*10, col = 'purple') #barely changes
#drop chlorA since cross reg paper determined it wasn't a good predictor for hawaii
#final0<-final0[,-2] #drop chlorA since cross reg paper determined it wasn't a good predictor for hawaii

env<-dry[,c('Year','Herbivores','Ratio','SecConsumers')]
final<-na.omit(join(env,final0, type = 'inner'))

#normalize to mean
for(i in 2:14) {
  #final[i]=final[i]-round_any(min(final[,i]),10, f = floor) #subtract min rounded down to nearest 10
  final[i]=final[i]/mean(final[,i]) #then normalize to mean
}

write_csv(final, 'data/DEA final_inds set.csv')

#explore soc data
plot(CommRev.Mil~Year, data=final, ylim = c(0,2))
points(final$Year, final$RevDiv, col = 'blue') #
points(final$Year, final$Catch.Millions, col = 'red')
#points(final$Year, final$Trips.Millions, col = 'purple')
#points(final$Year, final$Fishing.Seaf.Emp, col = 'green') 
points(final$Year, final$NES.Emp.Thousands, col = 'darkseagreen') #might want to edit emp data to just keep fishing
#but for now, nes employment is essentially all fishing
points(final$Year, final$Visitors_Dollars.Millions, col = 'brown') 

#####################################################################################
#set up input and output dfs and reference points
#assign social outputs
Y<-final[,c("CommRev.Mil", "RevDiv", "Catch.Millions","NES.Emp.Thousands","Tourism_Individuals.Thousands")] 

#env inputs
X<-as.matrix(final[,c('Herbivores','Ratio','chlorA','SecConsumers')])
YMED<-as.matrix(Y[1,]) #just using first value
XMED<-X[1,] #just using first value
#############################################################################################
YP<-as.matrix(Y)      
XE<-X
########################################################################################
J=nrow(YP)
(M=ncol(YP))
(NXE=ncol(XE))
#Define A Matrix for GLPK and LPSolve.
YX=cbind(YP,XE)
A=matrix(0,M+NXE+1,J+1)
#Next, Transform YX matrix and copy to A. 
A[1:(M+NXE),1:J]=t(YX)
A[(M+NXE+1),1:J]=1.0
######################################################################
#First Program is for Numerator of Output Quantity Index
#We use GLPK package for LP model
#########################################################################
obj=c(rep(0,J),1)
(dir=c(rep('>=',M),rep('<=',NXE), '<='))
max<-TRUE
objvals1=0
status1=0
for(j in 1:J){
  A[1:M,J+1]=-A[1:M,j]                               #last column of A Matrix set equal to 
  #observation j output
  rhs=c(rep(0,M),XMED,1)                #rhs is being set to 0 for outputs, 
  #Median Value of environmental input 
  sol<- Rglpk_solve_LP(obj, A, dir, rhs, max=max)    #Define model and use RGLPK to solve
  if(j%%100==0|j==J)  print(paste('on dmu',j,'of',J))#shows when 100 observations solved
  objvals1[j]=round(1/sol$optimum,4)                 #objective function value 
  #returned by sol$optimum
  status1[j]=sol$status                              #For Error Checking. 
  #If Model solves, all values of status1 should equal zero
}
#####################################################################################
#FP #2 Reference observation is 2005
#Denominator of Output Quantity Index - based on optimization using one reference firm
#Just using observation #1 for this example
######################################################################################
#Define A Matrix for GLPK. 
A=matrix(0,M+NXE+1,J+1)
#Next, Transform YX matrix and copy to A. 
A[1:(M+NXE),1:J]=t(YX)
A[(M+NXE+1),1:J]=1.0
obj=c(rep(0,J),1)
(dir=c(rep('>=',M),rep('<=',NXE), '<='))
max<-TRUE
status2=0
objvals2=0
for(j in 1:J){
  A[1:M,J+1]=-YMED        #Median Y value for outputs
  rhs=c(rep(0,M),XMED,1)  #Median X value for inputs
  sol<- Rglpk_solve_LP(obj, A, dir, rhs, max=max)
  if(j%%100==0|j==J)  print(paste('on dmu',j,'of',J))
  objvals2[j]=round(1/sol$optimum,5)
  status2[j]=sol$status
}

QI=round(objvals1/objvals2, 5)  #Output Index
########################################################################################
##################################################################################
##Continue here with next two LP programs. These are for the Environmental Quantity
#Index. First program is for the Numerator
##################################################################################
#Define A Matrix for GLPK. 
A=matrix(0,M+NXE+1,J+1)
#Next, Transform YX matrix and copy to A. 
A[1:(M+NXE),1:J]=t(YX)
A[(M+NXE+1),1:J]=1.0

obj=c(rep(0,J),1)
(dir=c(rep('>=',M),rep('<=',NXE), '<='))
max<-FALSE                                           #FALSE means minimization Model
status3=0
objvals3=0
for(j in 1:J){
  A[(M+1):(M+NXE),J+1]=-A[(M+1):(M+NXE),j]            #last column of A matrix set equal 
  #to Environmental variables 
  rhs=c(as.matrix(YMED),rep(0,NXE),1)                 #Constraint structure is different 
  #than model #1
  
  sol<- Rglpk_solve_LP(obj, A, dir, rhs, max=max)
  if(j%%100==0|j==J)  print(paste('on dmu',j,'of',J))
  objvals3[j]=round(1/sol$optimum,4)
  status3[j]=sol$status
}
###################################################################################
#Final LP program for denominator of environmental quality variable
###################################################################################
#Define A Matrix for GLPK and LPSolve. 
A=matrix(0,M+NXE+1,J+1)
#Next, Transform YX matrix and copy to A. 
A[1:(M+NXE),1:J]=t(YX)
A[(M+NXE+1),1:J]=1.0

obj=c(rep(0,J),1)
(dir=c(rep('>=',M),rep('<=',NXE), '<='))
max<-FALSE
status4=0
objvals4=0
for(j in 1:J){
  A[(M+1):(M+NXE),J+1]=-XMED
  rhs=c(YMED,rep(0,NXE),1) 
  sol<- Rglpk_solve_LP(obj, A, dir, rhs, max=max)
  if(j%%100==0|j==J)  print(paste('on dmu',j,'of',J))
  objvals4[j]=round(1/sol$optimum,4)
  status4[j]=sol$status
}
ZI=objvals3/objvals4

EQI=QI/ZI          #Final Index Number

#save as different resfinal df depending which variables were included
resfinal<-as.data.table(cbind(final[1],QI,ZI,EQI))

write_csv(resfinal, 'data/DEA all dfs_resfinal.csv')

###################################################################################
################## DEA Scores Over Time (FACETTED LINEPLOTS) #################
###################################################################################
#expand to long so we can facet the index type
resfinal<-read.csv('data/DEA all dfs_resfinal.csv')
resfinal<-gather(resfinal,'Index','Value',2:4)

#gather normalized indicators to long form to plot indiv line plots
final<-read.csv('data/DEA final_inds set.csv')
soc<-final[,c('Year',"CommRev.Mil", "RevDiv", "Catch.Millions","NES.Emp.Thousands","Tourism_Individuals.Thousands")] 
colnames(soc)<-c('Year','Commercial Revenue','Revenue Diversity','Recreational Catch','Fishing Employment','Tourism Employment')
soc<-gather(soc, 'Indicator','Value',2:6)
soc$Index<-'Social\n Output'

env<-final[,c('Year', "Herbivores", "Ratio",'chlorA','SecConsumers')] 
colnames(env)<-c('Year', "Herbivores", "Coral-Fleshy Algae Ratio",'Chlorophyll a', 'Secondary Consumers')
env<-gather(env, 'Indicator','Value',2:5)
env$Index<-'Ecological\n Input'

inds<-rbind(soc,env)
#using RColorBrewer
set3<-brewer.pal(9,'Set3')
deapal<-c('black','peru',set3[c(1,3:9)]) #skipped 2 which was yellow--too hard to see
#indpal<-c(blues4[c(4,5,6)],'black')

resfinal$Index<-factor(resfinal$Index, levels=c('ZI','QI','EQI'), labels = c('Ecological\n Input','Social\n Output','Social-Ecological\n Productivity'))
resfinal$Indicator<-'Composite'
resfinal<-resfinal[,c('Year','Indicator','Value','Index')]
resfinal<-rbind(resfinal,inds)
resfinal$Indicator<-factor(resfinal$Indicator, levels=c('Composite','Secondary Consumers',"Herbivores",'Coral-Fleshy Algae Ratio','Chlorophyll a',
                                                        'Commercial Revenue','Revenue Diversity','Fishing Employment',
                                                        'Recreational Catch',"Tourism Employment"))

##productivitye
temp4<-ggplot(resfinal, aes(x=Year, y=Value))+
  facet_wrap(~Index) +
  geom_line(aes(color=Indicator,linetype=Indicator), size = 0.9)+
  scale_color_manual(values=deapal)+
  scale_linetype_manual(values = c('solid',rep('longdash',9)))+
  labs(x="Year", y="Index Score"
       #, title = "Productivity Index Over Time"
  )+
  theme(plot.title=element_text(hjust=0.5, size = 21),
        axis.text = element_text(size = 17),
        #    axis.text.x = element_blank(),
        #    axis.title.x = element_text(size = 17, vjust = -1), # vjust adjusts vertical space between plot & axis title
        #     axis.title.x = element_blank(), # vjust adjusts vertical space between plot & axis title
        axis.title = element_text(size = 21),
        strip.background =element_rect(fill="gray94", colour = 'black', size =1), #change panel label color
        strip.text = element_text(size = 22),
        panel.grid.major = element_blank(), #delete major grid lines
        panel.grid.minor = element_blank(), #delete minor grid lines
        panel.background = element_rect(fill = "transparent",colour = 'black', size = 1),
        #panel.spacing = unit(0.7, "lines"),
        plot.background = element_rect(fill = "transparent",colour = NA),
        plot.margin = unit(c(0.1,1,0.3,0.3), "cm"),
        legend.position = 'top',
        legend.background = element_rect(fill="transparent"),
        legend.key = element_rect(fill = "transparent", colour = NA),
        legend.key.size = unit(1, 'cm'),
        legend.spacing.x = unit(0.1, 'cm'),
        legend.text=element_text(size=12),
        legend.title=element_blank(),
        legend.margin = margin(b = -10)
  ) +
  guides(color=guide_legend(nrow=2, byrow=TRUE, legend.box.just = "center")) + #so the legend is ordered by rows not columns
  #4 columns so that legend can be separated by env then soc, will bring extra soc key to side in ppt
  scale_x_continuous(breaks=seq(2005,2015,5)
                     #, labels = c('2000',rep('',4),'2005',rep('',4),'2010',rep('',4),'2015',rep('',2))
  )

temp4
#ggsave('figures/DEA/Facetted Indices Over Time.png', 
 #      width =  11, height = 4.3, units = 'in', #w & h in inches
  #     dpi = 300, bg = 'transparent')

ggsave('figures/DEA/Facetted Indices Over Time_seccons.png', 
       width =  12, height = 4.4, units = 'in', #w & h in inches
       dpi = 300, bg = 'transparent')
#########################################################################################
