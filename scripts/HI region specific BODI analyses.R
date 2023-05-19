##########################################################################
#R Program to compute F-P Index using Toy data from Chris's Book
#Text: "Productivity and Efficiency Analysis", by Chris O'Donnell
#Also calculate Quantity Indices frm page 83, "New Directions: Efficiency and 
#Productivity" by Rolf Fare and Shawna Grosskopf
#Author: John Walden, NMFS, NEFSC
#Edited for HI CSVI Data by Lansing Perng, NOAA PIFSC, UHMM
#February 27, 2021
#############################################################################
#First Clear any previous data stored in memory, and require lpSolveAPI, Rglpk,
#lpSolveApI and readr
##############################################################################
rm(list=ls())
PKG <- c("lpSolveAPI", "readr", "Rglpk", "Benchmarking","ggplot2", "reshape2", 
         "data.table", "plyr", "dplyr","Compind", "tidyr","doBy","stringr", "Rmisc", "RColorBrewer")

for (p in PKG) {
  
if(!require(p,character.only = TRUE)) {  
    install.packages(p)
    require(p,character.only = TRUE)}
}

#################################################################################

#####################################################################################
####################################NCRMP Data#######################################
#####################################################################################

NCRMP<-read.csv('data/NCRMP HI region specific soc indicators.csv') #cleaned from raw in 'HI region specific inds analyses.R'
#NCRMP.cut<-na.omit(NCRMP[,c(1,2,6,12,14,17,22,25,26)]) #includes 7 summary inds--might be too many for one BODI score?

#data set needs inputs to create BODI
#chl a
chla<-read.csv('data/chlorophyll a.csv')
chla<-chla[,c(1,7)]
colnames(chla)<-c("Year",'Chlorophyll.a')
chl.NCRMP<-join(chla,NCRMP,by = 'Year', type = 'inner')

#dryad
dry<-read.csv('data/dryad annual averages.csv')
dry$Ratio<-dry$Calcifiers/dry$Algae
dry.cut<-dry[,c(1,12,6,13,14)]
env.NCRMP<-na.omit(join(dry.cut,chl.NCRMP,by = 'Year', type = 'inner'))

#divide each value by column mean (which is mean of each type of landing across all years and regions)
for(i in 2:31) {
  env.NCRMP[i]=env.NCRMP[i]/mean(env.NCRMP[,i]) #mean() function requires specification of exact column number
}

data2<-env.NCRMP #assign new name

#each group needs its own section of code, but rerunning per group for time's sake
Y=data2[,c(20,22,26:28)] #Tourism
#Y=data2[,c('LivRes_Emp','LivRes_GDP',"Commercial_Licenses_Revenue","Commercial_Landings","Commercial_Revenue")]
#Y=data2[,c("Recreational_Landings","PCT_Beach_Days_W.out_Not" )]

(M=ncol(Y))
(J=nrow(Y))
#X<-as.matrix(pprmean[,"pprnorm"])
XE<-data2[,c(2:7)]
(NXE=ncol(XE))
YX=cbind(Y,XE)
###################################################################
#End of DATA Step
####################################################################
#Define A Matrix for GLPK and LPSolve. 
A=matrix(0,M+NXE+1,J+1)
#Next, Transform YX matrix and copy to A. 
A[1:(M+NXE),1:J]=t(YX)
A[(M+NXE+1),1:J]=1.0
######################################################################
#This next code segment calculates the F-P model from equation #8 in 
#Fare et. al. 2015 Marine Policy Using GLPK
#First Program is for Numerator of Output Quantity Index
#We use GLPK package for LP model
#########################################################################

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
######################################################################################
#Denominator of Output Quantity Index - based on optimization using one reference firm
#Just using observation #1 for this example
######################################################################################
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

EI=round(objvals1/objvals2, 5)

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
###################################################################################
#Final LP program for denominator of environmental quality variable
###################################################################################
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

###3 options--match with 3 options starting at line 57
resfinal.tour<-as.data.table(cbind(data2[,1],ZI,EI,EQI))
colnames(resfinal.tour)[1]<-"Year"
resfinal.tour$Sector<-'Tourism'

#resfinal.fish<-as.data.table(cbind(data2[,1],ZI,EI,EQI))
#colnames(resfinal.fish)[1]<-"Year"
#resfinal.fish$Sector<-'Fisheries & Living Resources'

#resfinal.rec<-as.data.table(cbind(data2[,1],ZI,EI,EQI))
#colnames(resfinal.rec)[1]<-"Year"
#resfinal.rec$Sector<-'Recreation'

#####################################################################################
######################################Figures########################################
#####################################################################################
library(psych)

########################################################################
####################PLOT GROUPED INDICES OVER TIME######################
########################################################################
#stack all 3 lines together using facet_wrap
resfinal_grouped <- rbind(resfinal.tour,resfinal.fish,resfinal.rec)
#ended up using extra geom-line arguments instead because facet_wrap shows 3 panels

##################################EI####################################
#soc outputs only
ei_lines<-ggplot(resfinal_grouped, aes(x=Year, y=EI, group = Sector, color = Sector))+
  geom_line(lwd = 1)+
  scale_color_manual(values=c('mediumseagreen','orchid3','deepskyblue3')) + #,
 #                    labels=c("Environmental Inputs", "Social Outputs", "Input-Output Index"))+
#  scale_linetype_manual(values=c('dashed', 'dashed','solid')+ #,
 #                       labels=c("Environmental Inputs", "Social Outputs", "Input-Output Index"))+
  # geom_line(data=resfinal,aes(x=Year, y=EQI), color = 'black')+
  #geom_line(data=resfinal,aes(x=Year, y=ZI),color = 'firebrick', lty = 'dashed')+
  labs(x="Year", y="Performance Index", title="Performance Indexes by Sector")+
  scale_x_continuous(breaks=seq(2006, 2016, 2))+
  theme(text = element_text(size=24),
        plot.title=element_text(hjust=0.5),
        axis.text = element_text(angle=0, hjust=1,size=21),
        axis.title = element_text(size = 25),
        legend.title=element_blank(), 
        legend.position = c(0.25,0.87),
       # legend.position = c(-1,1.5), #temporary for making 
        legend.key = element_rect(fill = "transparent", colour = NA),
        legend.margin = margin(0,14,13,15),
        legend.background = element_rect(fill="transparent",colour = 'black', size = 0.5),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "transparent",colour = 'black', size = 1),
        plot.background = element_rect(fill = "transparent",colour = NA))

ei_lines
#ggsave('figures/HI region specific/BODI/NCRMP Grouped 3.png', 
 #      width =  10.5, height = 7, units = 'in', #w & h in inches
  #     dpi = 300, bg = 'transparent')

ggsave('figures/HI region specific/BODI/NCRMP Grouped Indexes Over Time.png', 
       width =  10.5, height = 7, units = 'in', #w & h in inches
       dpi = 300, bg = 'transparent')




#################################################################################

#####################################################################################
######################CSVI DATA split by fishing communities#########################
#####################################################################################

#?
#this is where I was working with the HI soc data (original data from HDWG google drive)
HIsoc0<-read.csv('data/HI_Soc_TS.csv')
HIsoc<-HIsoc0[,c(1,3,4,8:15)]

####output soc
soc<-HIsoc[,c(1,2,6:11)]
soc<-na.omit(soc)
soc$Community<-as.factor(soc$Community)

#summarize by counties to explore county indicator means over time
coun<-HIsoc[c(1:44),c(2,3)]
temp.soc<-join(soc, coun, by = 'Community', type = 'inner')
temp.soc<-temp.soc[,-2]
temp.soc<-temp.soc[,c(1,8,2:7)]
temp.soc0<-aggregate(.~Year+County, data = temp.soc, FUN = mean) 

# explore the data summarized by County
ggplot(temp.soc0, aes(x = Year, y = RecFshTrps1000, colour = County)) +
  geom_line()

write.csv(temp.soc0, file="outputs/HI region specific BODI/CSVI over time by County_raw.csv", row.names = F)

#divide each value by column mean (which is mean of each type of landing across all years and regions)
soc$BoatLaunches1000=soc$BoatLaunches1000/mean(soc$BoatLaunches1000)
soc$Pounds1000=soc$Pounds1000/mean(soc$Pounds1000)
soc$Value1000=soc$Value1000/mean(soc$Value1000)
soc$Dealers1000=soc$Dealers1000/mean(soc$Dealers1000)
soc$CommPermits1000=soc$CommPermits1000/mean(soc$CommPermits1000)
soc$RecFshTrps1000=soc$RecFshTrps1000/mean(soc$RecFshTrps1000)

#not sure if applicable, since each column is diff category--but scaled the same so left code here in case
#soc$QY=((soc$BoatLaunches1000^(1/6))*(soc$Pounds1000^(1/6))*(soc$Value1000^(1/6))*
 #         (soc$Dealers1000^(1/6))*(soc$CommPermits1000^(1/6))*(soc$RecFshTrps1000^(1/6)))
#not great, because 0 columns make the entire QY value 0

#####inputs

#pop density
pop<-HIsoc[,c(1,2,4)]
pop<-pop[!is.na(pop$PopDensity),]
pop$Community<-as.factor(pop$Community)
pop$PopDensity<-as.numeric(pop$PopDensity)
pop_region<-aggregate(PopDensity~Community, data = pop, FUN = mean) #average indicator values for each comm over all years
colnames(pop_region)<-c('Community','PopRegAvg')
pop1<-join(pop,pop_region,by="Community",type="inner")
pop1$MSXI=pop1$PopDensity/pop1$PopRegAvg #MSXI is SXI (from formula) divided by regional avg SXI
pop2<-as.data.frame(pop1[,c("Year","Community","MSXI")])
pop2<-droplevels(pop2)
#final pop density index as MSXI in df pop2

#same for PCT water cover
pwc<-HIsoc[,c(1,2,5)]
pwc<-pwc[!is.na(pwc$PCTWaterCover),]
pwc$Community<-as.factor(pwc$Community)
pwc$PCTWaterCover<-as.numeric(pwc$PCTWaterCover)
pwc_region<-aggregate(PCTWaterCover~Community, data = pwc, FUN = mean) #average indicator values by Year
colnames(pwc_region)<-c('Community','pwcRegAvg')
pwc1<-join(pwc,pwc_region,by="Community",type="inner")
pwc1$PXI=pwc1$PCTWaterCover/pwc1$pwcRegAvg #PXI is SXI (from formula) divided by regional avg SXI
pwc2<-as.data.frame(pwc1[,c("Year","Community","PXI")])
pwc2<-droplevels(pwc2)

inputs<-join(pop2, pwc2, by=c("Year","Community"), type="inner")

######################################################################################
#################################BODI by Community####################################
######################################################################################
final<-join(inputs,soc, by=c("Year","Community"), type="inner")

coun<-HIsoc[c(1:44),c(2,3)]

counties<-as.data.frame(as.matrix(5,2))
counties[(1:5),1]<-as.numeric(c(1,2,3,4,5))
counties[(1:5),2] <-unique(coun$County)
colnames(counties)[1]<-"County.Num"
colnames(counties)[2]<-"County"

coun<-join(coun, counties, by = 'County', type = 'inner')
temp.final<-join(final, coun, by = 'Community', type = 'inner')

temp.final<-temp.final[,-c(2,12)]
temp.final0<-aggregate(.~Year+County, data = temp.final, FUN = mean) 
write.csv(temp.final0, file="outputs/HI region specific BODI/CSVI over time by County.csv", row.names = F)

Y=final[,c( #"BoatLaunches1000",  #removed boat launches because it turns out these are infrastructure numbers
           "Pounds1000","Value1000",
           "Dealers1000","CommPermits1000", "RecFshTrps1000")]
(M=ncol(Y))
(J=nrow(Y))
#X<-as.matrix(pprmean[,"pprnorm"])
XE<-final[,c("PXI", "MSXI")]
(NXE=ncol(XE))
YX=cbind(Y,XE)
###################################################################
#End of DATA Step
####################################################################
#Define A Matrix for GLPK and LPSolve. 
A=matrix(0,M+NXE+1,J+1)
#Next, Transform YX matrix and copy to A. 
A[1:(M+NXE),1:J]=t(YX)
A[(M+NXE+1),1:J]=1.0
######################################################################
#This next code segment calculates the F-P model from equation #8 in 
#Fare et. al. 2015 Marine Policy Using GLPK
#First Program is for Numerator of Output Quantity Index
#We use GLPK package for LP model
#########################################################################

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
######################################################################################
#Denominator of Output Quantity Index - based on optimization using one reference firm
#Just using observation #1 for this example
######################################################################################
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

EI=round(objvals1/objvals2, 5)
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
###################################################################################
#Final LP program for denominator of environmental quality variable
###################################################################################
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

resfinal<-as.data.table(cbind(final[,1:2],ZI,EI,EQI))
colnames(resfinal)[1]<-"Year"

#####################################################################################
######################################Figures########################################
#####################################################################################
library(psych)

#####reorganize data by counties
coun<-HIsoc[c(1:44),c(2,3)]

counties<-as.data.frame(as.matrix(5,2))
counties[(1:5),1]<-as.numeric(c(1,2,3,4,5))
counties[(1:5),2] <-unique(coun$County)
colnames(counties)[1]<-"County.Num"
colnames(counties)[2]<-"County"

coun<-join(coun, counties, by = 'County', type = 'inner')
resfinal<-join(resfinal, coun, by = 'Community', type = 'inner')

#exploratory to see values
temp.resfinal<-resfinal[,-c(2,7)]
temp.resfinal0<-aggregate(.~Year+County, data = temp.resfinal, FUN = mean)

#reorder by counties in geographic order using dplyr
# create a vector with letters in the desired order
x <- c("Kauai County", "Honolulu County", "Maui County", 'Hawaii County')

#reorders counties
resfinal<-resfinal %>%
  mutate(County =  factor(County, levels = x)) %>%
  arrange(County) 

#create community vector to reorder communities
#since this is what actually will appear on the x axis
comm.order<-unique(resfinal$Community)

#reorders communities by the order developed from reordering counties
resfinal<-resfinal %>%
  mutate(Community =  factor(Community, levels = comm.order)) %>%
  arrange(Community) 


write.csv(resfinal, file="community_res.csv", row.names = F)


#####some hyp testing to compare values by county
kruskal.test(ZI~County,data=resfinal) #(input)
kruskal.test(EI~County,data=resfinal) #** (output)
kruskal.test(EQI~County,data=resfinal)#** (input-output)


############################PLOTS BY INDICES#############################

########################################################################
##################################EQI###################################
########################################################################
temp2<-ggplot(resfinal, aes(x=Community, y=EQI, fill = Community)) +
  theme(  panel.grid.major = element_blank(), #delete major grid lines
          panel.grid.minor = element_blank(), #delete minor grid lines
          panel.background = element_rect(fill = "transparent",colour = 'black', size = 1),
          plot.background = element_rect(fill = "transparent",colour = NA),
          legend.position = 'top', legend.margin = margin(3,0,-7,0),
          legend.background = element_rect(fill="transparent"),
          legend.key.size = unit(0.7, 'cm'),
          legend.key = element_rect(fill = "transparent", colour = NA),
          legend.title.align=0.5) +
  geom_boxplot(aes(color=County), fill = 'white', lwd = 1, outlier.alpha = 1)+
  scale_x_discrete(expand = c(0.03, 1)) +
  labs(x="Fishing Community", y="Productivity Index" , title="Social Productivity Index by Fishing Community")+
  theme(plot.title=element_text(hjust=0.5, size = 17),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        axis.title.x = element_text(margin = margin(-7,0,0,0)),
        axis.title = element_text(size = 13))
temp2

ggsave('figures/HI region specific/BODI/EQI by Community.png', 
       width =  8.47, height = 5, units = 'in', #w & h in inches
       dpi = 300, bg = 'transparent')

#######################REPLOT WITH COUNTY MEDIANS########################

##############plot all data split by county
temp2.1<-ggplot(resfinal, aes(x=County, y=EQI)) +
  theme(  panel.grid.major = element_blank(), #delete major grid lines
          panel.grid.minor = element_blank(), #delete minor grid lines
          panel.background = element_rect(fill = "transparent",colour = 'black', size = 1),
          plot.background = element_rect(fill = "transparent",colour = NA),
          legend.position = 'top', legend.margin = margin(3,0,-7,0),
          legend.background = element_rect(fill="transparent"),
          legend.key.size = unit(0.7, 'cm'),
          legend.key = element_rect(fill = "transparent", colour = NA))  + #no legend
  geom_boxplot(aes(color=County, fill = County), alpha = 0.3, lwd = 1, outlier.alpha = 1,
               width = 0.62)+
  labs(x="County", y="Productivity Index" , title="Social Productivity Index by County")+
  theme(plot.title=element_text(hjust=0.5))

temp2.1

ggsave('figures/HI region specific/BODI/EQI by County.png', 
       width =  9, height = 5, units = 'in', #w & h in inches
       dpi = 300, bg = 'transparent')

##############plot geometric means by county
#####newer code for geometric means of indices from John Walden

#res_county<-as.data.table(resfinal[,.(geometric.mean(EI), geometric.mean(ZI), 
 #                                     geometric.mean(EQI)),
  #                                 keyby= .(Year,County)])
#colnames(res_county)[3]<-"EI"
#colnames(res_county)[4]<-"ZI"
#colnames(res_county)[5]<-"EQI"

#write.csv(res_county, file="county_res.csv", row.names = F)

#temp2.2<-ggplot(res_county, aes(x=County, y=EQI)) +
#  theme(  panel.grid.major = element_blank(), #delete major grid lines
 #         panel.grid.minor = element_blank(), #delete minor grid lines
  #        panel.background = element_rect(fill = "transparent",colour = 'black', size = 1),
   #       plot.background = element_rect(fill = "transparent",colour = NA),
    #      legend.position = "none")  + #no legend
#  geom_boxplot(aes(color=County, fill = County), alpha = 0.3, lwd = 1, outlier.alpha = 1)+
#  labs(x="County", y="EQI" , title="Median Values of EQI by County")+
#  theme(plot.title=element_text(hjust=0.5))

#temp2.2

#ggsave('figures/HI region specific/BODI/EQI by County GMeans.png', 
  #     width =  9, height = 5, units = 'in', #w & h in inches
   #    dpi = 300, bg = 'transparent')

########################################################################
###############################PLOT EI##################################
########################################################################
temp2qi<-ggplot(resfinal, aes(x=Community, y=EI, fill = Community))+
  theme(  panel.grid.major = element_blank(), #delete major grid lines
          panel.grid.minor = element_blank(), #delete minor grid lines
          panel.background = element_rect(fill = "transparent",colour = 'black', size = 1),
          plot.background = element_rect(fill = "transparent",colour = NA),
          legend.position = 'top', legend.margin = margin(3,0,-7,0),
          legend.background = element_rect(fill="transparent"),
          legend.key.size = unit(0.7, 'cm'),
          legend.key = element_rect(fill = "transparent", colour = NA),
          legend.title.align=0.5) +
  geom_boxplot(aes(color=County), fill = 'white', lwd = 1, outlier.alpha = 1)+
  scale_x_discrete(expand = c(0.03, 1)) +
  labs(x="Fishing Community", y="Social Performance Index" , title="Social Index by Fishing Community")+
  theme(plot.title=element_text(hjust=0.5, size = 17),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        axis.title.x = element_text(margin = margin(-7,0,0,0)),
        axis.title = element_text(size = 13))


temp2qi

#ggsave('figures/HI region specific/BODI/EI by Community recolored.png', 
 #      width =  8, height = 5.5, units = 'in', #w & h in inches
  #     dpi = 300, bg = 'transparent')

ggsave('figures/HI region specific/BODI/EI by Community.png', 
       width =  8, height = 5.5, units = 'in', #w & h in inches
       dpi = 300, bg = 'transparent')

#######################REPLOT WITH COUNTY MEDIANS########################
temp2qi1<-ggplot(resfinal, aes(x=County, y=EI)) +
  theme(  panel.grid.major = element_blank(), #delete major grid lines
          panel.grid.minor = element_blank(), #delete minor grid lines
          panel.background = element_rect(fill = "transparent",colour = 'black', size = 1),
          plot.background = element_rect(fill = "transparent",colour = NA),
          legend.position = 'top', legend.margin = margin(3,0,-7,0),
          legend.background = element_rect(fill="transparent"),
          legend.key.size = unit(0.7, 'cm'),
          legend.key = element_rect(fill = "transparent", colour = NA))  + #no legend
  geom_boxplot(aes(color=County, fill = County), alpha = 0.3, lwd = 1, outlier.alpha = 1,
               width = 0.62)+
  labs(x="County", y="Social Performance Index" , title="Social Index by County")+
  theme(plot.title=element_text(hjust=0.5))

temp2qi1

ggsave('figures/HI region specific/BODI/EI by County.png', 
       width =  9, height = 5, units = 'in', #w & h in inches
       dpi = 300, bg = 'transparent')

#################GMeans
#temp2qi2<-ggplot(res_county, aes(x=County, y=EI)) +
 # theme(  panel.grid.major = element_blank(), #delete major grid lines
 #         panel.grid.minor = element_blank(), #delete minor grid lines
 #         panel.background = element_rect(fill = "transparent",colour = 'black', size = 1),
 #          plot.background = element_rect(fill = "transparent",colour = NA),
 #         legend.position = "none", #no legend
#          plot.title = element_text(size = 21),
#          axis.text = element_text(size = 15.5),
#          axis.title.x = element_text(size = 18, vjust = -1), # vjust adjusts vertical space between plot & axis title
#          axis.title.y = element_text(size = 18, vjust = 4),
#          plot.margin = unit(c(0,0,0.5,0.5), "cm"))  + 
#  geom_boxplot(aes(color=County, fill = County), alpha = 0.3, lwd = 1, outlier.alpha = 1)+
#  labs(x="County", y="Output Quantity Index" , title="Median Values of EI by County")+
#  theme(plot.title=element_text(hjust=0.5))

# temp2qi2

# ggsave('figures/HI region specific/BODI/EI by County GMeans.png', 
  #     width =  11, height = 6, units = 'in', #w & h in inches
   #    dpi = 300, bg = 'transparent')
########################################################################
###############################PLOT ZI##################################
########################################################################

temp2zi<-ggplot(resfinal, aes(x=Community, y=ZI, fill = Community))+
  theme(  panel.grid.major = element_blank(), #delete major grid lines
          panel.grid.minor = element_blank(), #delete minor grid lines
          panel.background = element_rect(fill = "transparent",colour = 'black', size = 1),
          plot.background = element_rect(fill = "transparent",colour = NA),
          legend.position = 'top', legend.margin = margin(3,0,-7,0),
          legend.background = element_rect(fill="transparent"),
          legend.key.size = unit(0.7, 'cm'),
          legend.key = element_rect(fill = "transparent", colour = NA),
          legend.title.align=0.5) +
  geom_boxplot(aes(color=County), fill = 'white', lwd = 1, outlier.alpha = 1)+
  scale_x_discrete(expand = c(0.03, 1)) +
  labs(x="Fishing Community", y="Environmental Index" , title="Environmental Index by Fishing Community")+
  theme(plot.title=element_text(hjust=0.5, size = 17),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        axis.title.x = element_text(margin = margin(-7,0,0,0)),
        axis.title = element_text(size = 13))

temp2zi
ggsave('figures/HI region specific/BODI/ZI by Community.png', 
       width =  8.47, height = 5, units = 'in', #w & h in inches
       dpi = 300, bg = 'transparent')

#######################REPLOT WITH COUNTY MEDIANS########################
temp2zi1<-ggplot(resfinal, aes(x=County, y=ZI)) +
  theme(  panel.grid.major = element_blank(), #delete major grid lines
          panel.grid.minor = element_blank(), #delete minor grid lines
          panel.background = element_rect(fill = "transparent",colour = 'black', size = 1),
          plot.background = element_rect(fill = "transparent",colour = NA),
          legend.position = 'top', legend.margin = margin(3,0,-7,0),
          legend.background = element_rect(fill="transparent"),
          legend.key.size = unit(0.7, 'cm'),
          legend.key = element_rect(fill = "transparent", colour = NA))  + #no legend
  geom_boxplot(aes(color=County, fill = County), alpha = 0.3, lwd = 1, outlier.alpha = 1,
               width = 0.62)+
  labs(x="County", y="Environmental Index" , title="Environmental Index by County")+
  theme(plot.title=element_text(hjust=0.5))

temp2zi1

ggsave('figures/HI region specific/BODI/ZI by County.png', 
       width =  9, height = 5, units = 'in', #w & h in inches
       dpi = 300, bg = 'transparent')

###############GMeans
#temp2zi2<-ggplot(res_county, aes(x=County, y=ZI)) +
#  theme(  panel.grid.major = element_blank(), #delete major grid lines
 #         panel.grid.minor = element_blank(), #delete minor grid lines
  #        panel.background = element_rect(fill = "transparent",colour = 'black', size = 1),
   #       plot.background = element_rect(fill = "transparent",colour = NA),
    #      legend.position = "none")  + #no legend
#  geom_boxplot(aes(color=County))+
#  labs(x="County", y="ZI" , title="Median Values of ZI by County")+
#  theme(plot.title=element_text(hjust=0.5))

#temp2zi2

#ggsave('figures/HI region specific/BODI/ZI by County GMeans.png', 
 #      width =  9, height = 5, units = 'in', #w & h in inches
  #     dpi = 300, bg = 'transparent')

########################################################################
########################PLOT INDICES OVER TIME##########################
########################################################################

##################################EI####################################

##############by community
ei1<-ggplot(resfinal, aes(x=Year, y=EI))+
  geom_line()+
  facet_wrap(~Community)+
  labs(x="Year", y="EI", title="Output Quantity Index 2010-2018")+
  theme(plot.title=element_text(hjust=0.5))

ei1
ggsave('figures/HI region specific/BODI/EI Over Time by Community.png', 
       width =  9, height = 7, units = 'in', #w & h in inches
       dpi = 300, bg = 'transparent')

###################by county

#using package 'Rmisc' 
res_countySUM <- summarySE(resfinal, measurevar="EI", groupvars=c("Year","County"))
res_countySUM

## create a palette for all communities with themed colors per county
#Kauai: 9; Honolulu:7;  Maui 13; Hawaii: 12
mypalette<-c(brewer.pal(9,"Purples"), 
             brewer.pal(7,"Blues"),
             brewer.pal(9,"Greens"),brewer.pal(3,"Greens"),
             brewer.pal(9,"Oranges"),brewer.pal(4,"Oranges"))
countypal<- c(c('purple','purple','purple','purple','purple','purple','purple','purple','purple', 
                'blue','blue','blue','blue','blue','blue','blue','blue','blue',
                'green','green','green','green','green','green','green','green','green', 
                'orange','orange','orange','orange','orange','orange','orange','orange','orange'))
#must specify color for each line segment

ei2<-ggplot(resfinal, aes(x=Year, y=EI))+
  geom_line( aes(group = Community, color = Community))+
  scale_color_manual(values = mypalette) +
  geom_line(data = res_countySUM, size =2, alpha = 0.5, aes(color = County), col = countypal) +
  geom_point(data = res_countySUM, pch =1) +
  geom_errorbar(data = res_countySUM,aes(ymin=EI-se, ymax=EI+se), width=.1, alpha = 0.5) +
  facet_wrap(~County)+
  labs(x="Year", y="Composite Index Scores", title="DEA Scores Over Time")+
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
ggsave('figures/HI region specific/BODI/EI Over Time by County.png', 
       width =  11.5, height = 7, units = 'in', #w & h in inches
       dpi = 300, bg = 'transparent')

##################################ZI####################################
zi1<-ggplot(resfinal, aes(x=Year, y=ZI))+
  geom_line()+
  facet_wrap(~Community)+
  labs(x="Year", y="ZI", title="Environmental Index by Community 2010-2018")+
  theme(plot.title=element_text(hjust=0.5),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

zi1
ggsave('figures/HI region specific/BODI/ZI Over Time by Community.png', 
       width =  9, height = 7, units = 'in', #w & h in inches
       dpi = 300, bg = 'transparent')

#################by county
zi2<-ggplot(res_county, aes(x=Year, y=ZI))+
  geom_line()+
  facet_wrap(~County)+
  labs(x="Year", y="ZI", title="Environmental Index by County 2010-2018")+
  theme(plot.title=element_text(hjust=0.5),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

zi2
ggsave('figures/HI region specific/BODI/ZI Over Time by County.png', 
       width =  9, height = 7, units = 'in', #w & h in inches
       dpi = 300, bg = 'transparent')

##################################EQI###################################
eqi1<-ggplot(resfinal, aes(x=Year, y=EQI))+
  geom_line()+
  facet_wrap(~Community)+
  labs(x="Year", y="EQI",title="Environmental Output Quantity Index by Community 2010-2018" )+
  theme(plot.title=element_text(hjust=0.5),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

eqi1
ggsave('figures/HI region specific/BODI/EQI Over Time by Community.png', 
       width =  9, height = 7, units = 'in', #w & h in inches
       dpi = 300, bg = 'transparent')

##################by county
eqi2<-ggplot(res_county, aes(x=Year, y=EQI))+
  geom_line()+
  facet_wrap(~County)+
  labs(x="Year", y="EQI",title="Environmental Output Quantity Index by County 2010-2018" )+
  theme(plot.title=element_text(hjust=0.5),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

eqi2
ggsave('figures/HI region specific/BODI/EQI Over Time by County.png', 
       width =  9, height = 7, units = 'in', #w & h in inches
       dpi = 300, bg = 'transparent')

#################################################################################



