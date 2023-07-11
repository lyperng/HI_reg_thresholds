###############################################################################################
#Program to create cross-regional F-P index
# John Walden
#John.Walden@Noaa.gov
##April 6, 2022
####################################################################
#First, clear memory and load needed packages
rm(list=ls())
PKG <- c("lpSolveAPI", "Rglpk", "readr", "Benchmarking","ecodata","data.table",
         "plyr","dplyr", "tidyr","doBy","stringr","ggplot2", "rstudioapi")

for (p in PKG) {
  if(!require(p,character.only = TRUE)) {  
    install.packages(p)
    require(p,character.only = TRUE)}
}
#Load and clean data
#setwd("/Users/lansingperng/Desktop/Research/NOAA/HDWG/HDWG Analyses")

########### READ IN DFS FROM ALL SOURCES AND COMBINE INTO OUTPUT DF ##############
source<-c('FOSS','MRIP','NES','BLS')
#empty df to bind others to
df <- data.frame(matrix(ncol = 2, nrow = 0)) #one for gamfits
colnames(df) <- c('Year','Region')

df0 <- df #one with raw points

for(j in 1:length(source)){ #j represents the column number in the NES df (skip 1), NESvar
  
  df0.new<-read.csv(paste('cross regional data/DEA/', source[j],' combined_data points_raw.csv', sep=''))
  df.new<-read.csv(paste('cross regional data/DEA/', source[j],' combined_gamfit.csv', sep=''))
  
  df0<-join(df0,df0.new, type = 'full')
  df<-join(df,df.new, type = 'full')
  
}

write.csv(df0,'cross regional data/DEA/DEA input_all sources_points.csv', row.names = F)
write.csv(df, 'cross regional data/DEA/DEA input_all sources_gamfit.csv', row.names = F)

############## normalize to population

### with gamfit df
df<-read.csv('cross regional data/DEA/DEA input_all sources_gamfit.csv')
df$Year.fit<-df$Year #save the decimal years 
#might add back if i want finer time scale later

df$Year<-round(df$Year, digits = 0) #round Year to nearest integer than aggregate by year, since there is only one pop value per year
df.sum<-aggregate(.~Year + Region, data=df,FUN =mean, na.rm=TRUE, na.action=na.pass) #aggregate all values to one year
#na.rm = T removes nas when applying function (allows summarized mean not to be NA when one value in the year is NA)
#na.pass then does not delete an entire row when the row has NA(s)--allows df to keep longer series for comm data
# but creates NaN-->replace with NA
df.sum <- df.sum %>% mutate_all(~ifelse(is.nan(.), NA, .)) #using dplyr

### with points df
df0<-read.csv('cross regional data/DEA/DEA input_all sources_points.csv')

#vectors
regnames<-unique(df0$Region)
#ready to join annual pop

pop<-read.csv('cross regional data/DEA/Coastal_Population_EIWG.csv')
pop<-pop[,-c(1)] #delete rownames column
pop$MPOP<-pop$Population/1000000

#rewrite some region names to match df0 and df00
pop$Region<-gsub('California.Current','California Current', pop$Region)
pop$Region<-gsub('Gulf.of.Mexico','Gulf of Mexico', pop$Region)
pop$Region<-gsub('Hawaii',"Hawai'i", pop$Region)

df0.pop<-join(df0,pop,type='full') # keep all years, will cut based on which inds are used in DEA index
df0.pop[,c(8:13)]<-df0.pop[,c(8:13)]/df0.pop$MPOP #standardize all employment data plus rec trips per million pop

#setwd(dirname(getActiveDocumentContext()$path))
#load("data/bodi_data_06012021.RData")
#final$Trips.Percap=(final$Trips.Thousands*1000)/final$MPOP
chlor<-read.csv("data/chlorophyll a.csv")
chlor<-chlor[,-c(3,4)]
colnames(chlor)<-c('Year','California Current','Alaska','Gulf of Mexico', "Hawai'i", 'Northeast', 'Southeast')

#save df for just HI region for later region specific analyses
chlorHI<-chlor[,c('Year',"Hawai'i")]
colnames(chlorHI)[2]<-"chlorA"
finalHI<-df0.pop[df0.pop$Region==	"Hawai'i",]  #cut HI
dfHI<-join(finalHI,chlorHI,by="Year",type="inner")
write_csv(dfHI, 'data/HI DEA df_clean.csv') 
# soc inds, chlorA, and pop--rec trips + emp inds are all normalized to pop

###########################################################################################
#final<-as.data.table(final)
#cut chosen outputs
final<-df0.pop[,c('Year','Region','Comm.Catch.Mil','CommRev.Mil','Catch.Millions','Trips.Millions','NES.Emp.Thousands','BLS.Emp.Thousands')]
final<-na.omit(final) #no NAs can be in DEA

mfinal<-final[,-c(1)] #delete year column so everything can be summed to mean by Region
mfinal<-aggregate(.~Region, data=mfinal, FUN = mean) #means by region of only the time frame included in DEA
colnames(mfinal)[2:7]<-c("mccatch","mrev","mrcatch","mrtrips","mNemp","mBemp")

final<-join(final,mfinal,by="Region",type="inner")

final$nc_catch=final$Comm.Catch.Mil/final$mccatch #millions of pounds (commercial)
final$n_rev=final$CommRev.Mil/final$mrev #deflated revenue
final$nr_catch=final$Catch.Millions/final$mrcatch #(recreational)
final$nr_trips=final$Trips.Millions/final$mrtrips #angler trips per million pop
final$n_Nemp=final$NES.Emp.Thousands/final$mNemp
final$n_Bemp=final$BLS.Emp.Thousands/final$mBemp

######################################################################################
#Add environmental variables
###################################################################################
e2=as.data.frame(chlor)
#e2<-subset(e, Year>2004)
E<-as.data.table(melt(e2,id.vars="Year", measure.vars=c('California Current','Alaska','Gulf of Mexico', "Hawai'i", 'Northeast', 'Southeast')))
colnames(E)[2:3]<-c("Region", "ChlorA")
ENORM<-E[,.(mean(ChlorA)),
                  keyby=.(Region)]


colnames(ENORM)[2]<-"mean_chlorA"
E<-join(E,ENORM,by="Region",type="inner")
E$ChlorA_N=E$ChlorA/E$mean_chlorA
final<-join(final,E,by=c("Year","Region"),type="inner")
write_csv(final, 'cross regional data/DEA/DEA all vars_norm.csv')

#####################################################################################
#set up Median Values for Reference
#####################################################################################
#only assign one of these to Y per index calculation
Y<-final[,c("nc_catch","n_rev","nr_catch","nr_trips","n_Nemp","n_Bemp")] 
#Y<-final[,c("nc_catch","n_rev")] #just comm
#Y<-final[,c("nr_catch","nr_trips")]  #just rec
#Y<-final[,c("n_Nemp","n_Bemp")] # just emp

X<-as.matrix(final[,c("ChlorA_N")])
YMED<-as.matrix(Y[1,])
XMED<-X[1,]
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
#FP #2 Reference observation is Alaska in 2005
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
resfinal0<-as.data.table(cbind(final[,1:2],objvals1,objvals2,objvals3,objvals4,QI,ZI,EQI))
#resfinal1<-as.data.table(cbind(final[,1:2],QI,ZI,EQI)) #comm
#resfinal2<-as.data.table(cbind(final[,1:2],QI,ZI,EQI)) #rec
#resfinal3<-as.data.table(cbind(final[,1:2],QI,ZI,EQI)) #emp

resfinal0$IND<-'Combined'
#resfinal1$IND<-'Commercial Fishing'
#resfinal2$IND<-'Recreational Fishing'
#resfinal3$IND<-'Employment'

write_csv(resfinal0, 'cross regional data/DEA/DEA final indices.csv')

#resfinal<-rbind(resfinal0,resfinal1,resfinal2,resfinal3) #bind all together so they can be plotted in same plot
resfinal<-resfinal0 #revert to original DEA analysis without splitting out 'sub' sectors
###################################################################################
#################### Comparison of Regional Averages (BOXPLOTS) ###################
###################################################################################
resfinal0<-read_csv('cross regional data/DEA/DEA final indices.csv')
resfinal0$Region <- factor(resfinal0$Region, levels = c('Alaska', "Hawai'i", 'California Current', 'Gulf of Mexico', "Northeast", "Southeast"))

resfinalHI<-resfinal0[resfinal0$Region=="Hawai'i",]

# colorblind friendly palette with black:
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

##Social Output Index
temp1<-ggplot(resfinal0, aes(x=Region, y=QI))+
  geom_boxplot(aes(color=Region, fill = Region),show.legend = F, alpha = 0.5, lwd = 1, outlier.alpha = 1)+
  # ggtitle("Figure 1. Median Values of Output Index\n by region")+
  scale_x_discrete(labels=c("California Current" = "California\n Current", 
                            "Gulf of Mexico" = "Gulf of\n Mexico")) +
  scale_color_manual(values = cbbPalette[c(7,8,6,5,2,3)]) +
  scale_fill_manual(values = cbbPalette[c(7,8,6,5,2,3)]) +
  labs(x="Region", y="Social Output Index" , title="Social Index by Region")+
  theme(plot.title=element_text(hjust=0.5, size = 17),
        panel.grid.major = element_blank(), #delete major grid lines
        panel.grid.minor = element_blank(), #delete minor grid lines
        panel.background = element_rect(fill = "transparent",colour = 'black', size = 1),
        plot.background = element_rect(fill = "transparent",colour = NA),
        legend.background = element_rect(fill="transparent"), #whole legend box
        legend.key = element_rect(fill = "transparent", colour = NA), #area behind key icons
        legend.title.align=0.5,
        axis.text.x = element_text( vjust = 0.5),
        axis.text = element_text(size = 20),
        axis.title.x = element_text(margin = margin(4,0,0,0)),
        axis.title = element_text(size = 25)
  )

temp1

ggsave('figures/cross regional/BODI/QI by Region.png', 
       width =  10, height = 6.5, units = 'in', #w & h in inches
       dpi = 300, bg = 'transparent')


####Environmental input index
temp2<-ggplot(resfinal0, aes(x=Region, y=ZI))+
  geom_boxplot(aes(color=Region, fill = Region),show.legend = F, alpha = 0.5, lwd = 1, outlier.alpha = 1)+
  #  ggtitle("Figure 2. Median Values of Environmental Quantity Index\n by Region")+
  scale_x_discrete(labels=c("California Current" = "California\n Current", 
                            "Gulf of Mexico" = "Gulf of\n Mexico")) +
  scale_color_manual(values = cbbPalette[c(7,8,6,5,2,3)]) +
  scale_fill_manual(values = cbbPalette[c(7,8,6,5,2,3)]) +
  labs(x="Region", y="Ecological Input Index" , title="Ecological Index by Region")+
  theme(plot.title=element_text(hjust=0.5, size = 17),
        panel.grid.major = element_blank(), #delete major grid lines
        panel.grid.minor = element_blank(), #delete minor grid lines
        panel.background = element_rect(fill = "transparent",colour = 'black', size = 1),
        plot.background = element_rect(fill = "transparent",colour = NA),
        legend.background = element_rect(fill="transparent"), #whole legend box
        legend.key = element_rect(fill = "transparent", colour = NA), #area behind key icons
        legend.title.align=0.5,
        axis.text.x = element_text( vjust = 0.5),
        axis.text = element_text(size = 20),
        axis.title.x = element_text(margin = margin(4,0,0,0)),
        axis.title = element_text(size = 25)
  )

temp2

ggsave('figures/cross regional/BODI/ZI by Region.png', 
       width =  10, height = 6.5, units = 'in', #w & h in inches
       dpi = 300, bg = 'transparent')


##productivity (input-output) index
temp3<-ggplot(resfinal0, aes(x=Region, y=EQI))+
  geom_boxplot(aes(color=Region, fill = Region),show.legend = F, alpha = 0.5, lwd = 0.7, outlier.alpha = 1)+
#  ggtitle("Figure 3. Median Values of Output-Environmental Quantity Index\n  by Region")+
  scale_x_discrete(labels=c("California Current" = "California\n Current", 
                            "Gulf of Mexico" = "Gulf of\n Mexico")) +
  scale_color_manual(values = cbbPalette[c(7,8,6,5,2,3)]) +
  scale_fill_manual(values = cbbPalette[c(7,8,6,5,2,3)]) +
  labs(#x="Region", 
    y="Social-Ecological (Productivity) Index" # , title="Social-Ecological Index by Region"
       )+
  theme( #plot.title=element_text(hjust=0.5, size = 17),
        panel.grid.major = element_blank(), #delete major grid lines
        panel.grid.minor = element_blank(), #delete minor grid lines
        panel.background = element_rect(fill = "transparent",colour = 'black', size = 1),
        plot.background = element_rect(fill = "transparent",colour = NA),
        plot.margin = unit(c(1.2,0.3,0.3,0.05), "cm"),
        legend.background = element_rect(fill="transparent"), #whole legend box
        legend.key = element_rect(fill = "transparent", colour = NA), #area behind key icons
        legend.title.align=0.5,
        axis.text.x = element_text( vjust = 0.5),
        axis.text = element_text(size = 15),
        axis.title.x = element_blank(),
        axis.title = element_text(size = 18)
  )
temp3

ggsave('figures/cross regional/BODI/EQI by Region.png', 
       width =  10, height = 4.75, units = 'in', #w & h in inches
       dpi = 300, bg = 'transparent')

###################################################################################
################## Regional Scores Over Time (FACETTED LINEPLOTS) #################
###################################################################################
resfinal$IND<-factor(resfinal$IND, levels =c( 'Commercial Fishing', 'Recreational Fishing', 'Employment','Combined'))
resfinal$Region <- factor(resfinal$Region, levels = c('Alaska', "Hawai'i", 'California Current', 'Gulf of Mexico', "Northeast", "Southeast"))

#using RColorBrewer
#library(RColorBrewer)
#blues4<-brewer.pal(6,'Blues')
#indpal<-c(blues4[c(4,5,6)],'black')

##productivity
temp4<-ggplot(resfinal, aes(x=Year, y=EQI))+
  geom_line(#aes(color = IND,linetype = IND)
    )+
 # scale_color_manual(values =indpal) +
 # scale_linetype_manual(values = c('longdash','longdash','longdash','solid')) +
  facet_wrap(~Region, nrow = 1) + #, 
           #  labeller = labeller(Region = c("AK" = "Alaska", "CA" = "California Current", 
                 #                           "GOM" = "Gulf of Mexico","HI" = "Hawai'i", 
                  #                          "NE" = "Northeast", "SE" = "Southeast")))+
  labs(x="Year", y="Productivity Index"
      #, title = "Productivity Index Over Time"
       )+
  theme(plot.title=element_text(hjust=0.5, size = 21),
        axis.text.y = element_text(size = 16),
        axis.text.x = element_blank(),
 #       axis.title.x = element_text(size = 17, vjust = -1), # vjust adjusts vertical space between plot & axis title
        axis.title.x = element_blank(), # vjust adjusts vertical space between plot & axis title
        axis.title.y = element_text(size = 21, hjust = 1),
        strip.background =element_rect(fill="gray94", colour = 'black', size =1), #change panel label color
        strip.text = element_text(size = 22),
        panel.grid.major = element_blank(), #delete major grid lines
        panel.grid.minor = element_blank(), #delete minor grid lines
        panel.background = element_rect(fill = "transparent",colour = 'black', size = 1),
        #panel.spacing = unit(0.7, "lines"),
        plot.background = element_rect(fill = "transparent",colour = NA),
        plot.margin = unit(c(0.1,0.3,0.3,0.05), "cm"),
        legend.position = 'top',
        legend.background = element_rect(fill="transparent"),
        legend.key = element_rect(fill = "transparent", colour = NA),
        legend.key.size = unit(1.3, 'cm'),
        legend.spacing.x = unit(1, 'cm'),
        legend.text=element_text(size=21),
        legend.title=element_blank(),
        legend.margin = margin(b = 0)
 ) +
  scale_x_continuous(breaks=seq(2000,2017,1), labels = c('2000',rep('',4),'2005',rep('',4),'2010',rep('',4),'2015',rep('',2)))

temp4
ggsave('figures/cross regional/BODI/EQI by Region and Year_split.png', 
       width =  18, height = 3.2, units = 'in', #w & h in inches
       dpi = 300, bg = 'transparent')

##environmental input
temp5<-ggplot(resfinal, aes(x=Year, y=ZI))+
  geom_line()+
  facet_wrap(~Region, nrow = 1, 
             labeller = labeller(Region = c("AK" = "Alaska", "CA" = "California Current", 
                                            "GOM" = "Gulf of Mexico","HI" = "Hawai'i", 
                                            "NE" = "Northeast", "SE" = "Southeast")))+
  labs(x="Year", y="Environmental Index"
       #,title = "Environmental Index Over Time"
       )+
  theme(plot.title=element_text(hjust=0.5, size = 21),
        axis.text.x = element_text(size = 16),
        axis.text.y = element_text(size = 16,hjust = -1.3),
        axis.title.x = element_text(size = 21, vjust = 0.2), # vjust adjusts vertical space between plot & axis title
        axis.title.y = element_text(size = 21, hjust = 0.5),
    #    strip.background =element_rect(fill="gray94", colour = 'black', size =1), #change panel label color
    #    strip.text = element_text(size = 18),
        strip.background = element_blank(), #make facet title strip blank because productivity index row will be on top
        strip.text.x = element_blank(),
        panel.grid.major = element_blank(), #delete major grid lines
        panel.grid.minor = element_blank(), #delete minor grid lines
        panel.background = element_rect(fill = "transparent",colour = 'black', size = 1),
        #panel.spacing = unit(0.7, "lines"),
        plot.background = element_rect(fill = "transparent",colour = NA),
    plot.margin = unit(c(0.1,0.3,0.3,
                         
                         0.05), "cm")) +
     #   plot.margin = unit(c(0.4,0.3,0.4,0.35), "cm")) +
  scale_x_continuous(breaks=seq(2000,2017,1), labels = c('2000',rep('',4),'2005',rep('',4),'2010',rep('',4),'2015',rep('',2)))


temp5
ggsave('figures/cross regional/BODI/ZI by Region and Year.png', 
       width =  18, height = 3.45, units = 'in', #w & h in inches
       dpi = 300, bg = 'transparent')

##social output        
temp6<-ggplot(resfinal, aes(x=Year, y=QI))+
  geom_line(#aes(color = IND,linetype =IND), show.legend = F
    )+
 # scale_color_manual(values =indpal) +
 # scale_linetype_manual(values = c('longdash','longdash','longdash','solid')) +
  facet_wrap(~Region, nrow = 1, 
             labeller = labeller(Region = c("AK" = "Alaska", "CA" = "California Current", 
                                            "GOM" = "Gulf of Mexico","HI" = "Hawai'i", 
                                            "NE" = "Northeast", "SE" = "Southeast")))+
  labs(x="Year", y="Social Index"
       #,title = "Environmental Index Over Time"
  )+
  theme(plot.title=element_text(hjust=0.5, size = 21),
        axis.text.y = element_text(size = 16),
        axis.text.x = element_blank(),
        #       axis.title.x = element_text(size = 17, vjust = -1), # vjust adjusts vertical space between plot & axis title
        axis.title.x = element_blank(), # vjust adjusts vertical space between plot & axis title
        axis.title.y = element_text(size = 21, hjust = 0.6),
        #    strip.background =element_rect(fill="gray94", colour = 'black', size =1), #change panel label color
        #    strip.text = element_text(size = 18),
        strip.background = element_blank(), #make facet title strip blank because productivity index row will be on top
        strip.text.x = element_blank(),
        panel.grid.major = element_blank(), #delete major grid lines
        panel.grid.minor = element_blank(), #delete minor grid lines
        panel.background = element_rect(fill = "transparent",colour = 'black', size = 1),
        #panel.spacing = unit(0.7, "lines"),
        plot.background = element_rect(fill = "transparent",colour = NA),
        plot.margin = unit(c(0.15,0.3,0.3,0.05), "cm"),
      
  )+
  scale_x_continuous(breaks=seq(2000,2017,1), labels = c('2000',rep('',4),'2005',rep('',4),'2010',rep('',4),'2015',rep('',2)))


temp6
ggsave('figures/cross regional/BODI/QI by Region and Year_split.png', 
       width =  18, height = 2.8, units = 'in', #w & h in inches
       dpi = 300, bg = 'transparent')
#########################################################################################
