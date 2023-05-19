##########################################################################
#R Program to compute F-P Index using Toy data from Chris's Book
#Text: "Productivity and Efficiency Analysis", by Chris O'Donnell
#Also calculate Quantity Indices frm page 83, "New Directions: Efficiency and 
#Productivity" by Rolf Fare and Shawna Grosskopf
#Author: John Walden, NMFS, NEFSC
#February 24, 2021
#############################################################################
#First Clear any previous data stored in memory, and require lpSolveAPI, Rglpk,
#lpSolveApI and readr
##############################################################################
rm(list=ls())
PKG <- c("lpSolveAPI", "readr", "Rglpk", "Benchmarking","ggplot2", "reshape2", 
         "data.table", "plyr", "dplyr")
for (p in PKG) {
  if(!require(p,character.only = TRUE)) {  
    install.packages(p)
    require(p,character.only = TRUE)}
}
#################################################################################
###Function Needed for later calculation
###############################################################################
plank1<-function(df1,region){
  zoo<-subset(df1, EPU==region)
  zoo$PXI=(zoo$X1^0.25)*(zoo$X2^0.25)*(zoo$X3^0.25)*(zoo$X4^0.25)
  zoo$PXI=zoo$PXI/mean(zoo$PXI)
  
  return(zoo)
  
} 
##############################################################################
#Pull output data
comland<-ecodata::comdat     #commercial landings
########################################################################################
#Commercial Landings Only
comland2<-subset(comland, Units=="metric tons" & Time >=1964)
comland2<-as.data.table(comland2[,c("Time","Var","EPU","Value")])

spgroup<-as.data.frame(as.matrix(6,2))
spgroup[(1:6),1]<-c("Q1","Q2","Q3","Q4","Q5","Q6")
spgroup[(1:6),2] <-c("Planktivore Landings", "Other Landings", "Piscivore Landings",
                     "Benthivore Landings", "Apex Predator Landings", "Benthos Landings")
colnames(spgroup)[1]<-"Q"
colnames(spgroup)[2]<-"Var"

comland2<-merge(comland2,spgroup, by="Var", all.x=TRUE)
comland2<-comland2[!is.na(comland2$Q),]

comland2<-as.data.table(comland2[,c(2,5,3,4)])
land_avg<-as.data.table(comland2[,.(mean(Value)),
                             keyby= .(Q)])
####################################################################################
#Long to Wide Format
comland3<-dcast(comland2, Time+EPU~Q, value.var="Value")
comland3[is.na(comland3)]<-0
#####################################################################################
comland4<-comland3
comland4<-subset(comland4, Time>=1979)
comland4$Q1=comland4$Q1/mean(comland4$Q1)
comland4$Q2=comland4$Q2/mean(comland4$Q2)
comland4$Q3=comland4$Q3/mean(comland4$Q3)
comland4$Q4=comland4$Q4/mean(comland4$Q4)
comland4$Q5=comland4$Q5/mean(comland4$Q5)
comland4$Q6=comland4$Q6/mean(comland4$Q6)
comland4<-subset(comland4,EPU!="OTHER")
comland4<-subset(comland4,EPU!="SS")
comland4$QY=((comland4$Q1^(1/6))*(comland4$Q2^(1/6))*(comland4$Q3^(1/6))*
               (comland4$Q4^(1/6))*(comland4$Q5^(1/6))*(comland4$Q6^(1/6)))
####################################################################################
#Pull Zooplankton Data
zoo<-ecodata::zoo_strat_abun
zoogroup<-as.data.frame(as.matrix(4,2))
zoogroup[(1:4),1]<-c("X1","X2","X3","X4")
zoogroup[(1:4),2] <-c("SmallCalanoida","LargeCalanoida","Euphausiacea","Cnidaria")
################################################################################
colnames(zoogroup)[1]<-"X"
colnames(zoogroup)[2]<-"Var"

zoo2<-merge(zoo, zoogroup, by="Var", all.x=TRUE)
zoo2<-as.data.table(zoo2[,c(2,4:6)])
zoo2<-subset(zoo2, Time>=1979)
###############################################################################
#Long to Wide Format
###############################################################################
zoo3<-dcast(zoo2, Time+EPU~X, value.var="Value")
##############################################################################
##Construct EPU dataframe with function plank1
##############################################################################
zooGB<-plank1(zoo3,"GB")
zooGOM<-plank1(zoo3,"GOM")
zooMAB<-plank1(zoo3,"MAB")

zoo4<-rbind(zooGB,zooGOM,zooMAB)
zoo4<-zoo4[,c("Time","EPU","PXI")]
zoomean<-mean(zoo4$PXI)
##############################################################################
#Survey Indices
#First, Pull Survey Data
##############################################################################
surv<-ecodata::nefsc_survey
surv<-surv[!is.na(surv$Value),]
surv<-subset(surv, EPU != "SS")

group=matrix(NA,12,2)
group[,1]=sort(unique(surv$Var), decreasing=FALSE)
spgroup=(c("X1","X1","X2","X2","X3","X3","X4","X4","X5","X5","X6","X6"))
group[,2]<-spgroup
group<-as.data.frame(group)
colnames(group)[1]<-"Var"
colnames(group)[2]<-"X"

surv2<-join(surv,group, by="Var",type="inner")
surv2<-as.data.table(subset(surv2, spgroup != "X4"))

surv_av<-as.data.table(surv2[,.(sum(Value)),
                             keyby= .(Time,EPU,X)])

surv3<-dcast(surv_av, Time+EPU~X, value.var="V1")
surv3[is.na(surv3)]<-0
surv3$X1<-NULL
surv3[surv3==0]<-1
surv3<-as.data.table(subset(surv3, Time>=1979))

surv3$SXI=(surv3$X2^0.2)*(surv3$X3^0.2)*(surv3$X4^0.2)*(surv3$X5^0.2)*(surv3$X6^0.2)

surv_region<-as.data.table(surv3[,.(mean(SXI)),
                                keyby=.(EPU)])

surv3<-join(surv3,surv_region,by="EPU",type="inner")
surv3$MSXI=surv3$SXI/surv3$V1
surv3<-as.data.frame(surv3[,c("Time","EPU","MSXI")])
surv3<-droplevels(surv3)
###########################################################################
inputs<-join(surv3, zoo4, by=c("Time","EPU"), type="inner")
######################################################################################
final<-join(inputs,comland4, by=c("Time","EPU"), type="inner")

Y=final[,c("Q1","Q2","Q3","Q4","Q5","Q6")]
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
Figures
#####################################################################################
library(psych)
res_yr<-as.data.table(resfinal[,.(geometric.mean(EI)),
                             keyby= .(Year,EPU)])
colnames(res_yr)[3]<-"EI"

temp2<-ggplot(resfinal, aes(x=EPU, y=EQI))+
  geom_boxplot(aes(color=EPU))+
  labs(x="EPU", y="EQI" , title="Median Values of EQI by Region")+
  theme(plot.title=element_text(hjust=0.5))

temp2

temp2qi<-ggplot(resfinal, aes(x=EPU, y=EI))+
  geom_boxplot(aes(color=EPU))+
  labs(x="EPU", y="EI" , title="Median Values of EI by Region")+
  theme(plot.title=element_text(hjust=0.5))

temp2qi

temp2zi<-ggplot(resfinal, aes(x=EPU, y=ZI))+
  geom_boxplot(aes(color=EPU))+
  labs(x="EPU", y="ZI" , title="Median Values of ZI by Region")+
  theme(plot.title=element_text(hjust=0.5))

temp2zi



eqi1<-ggplot(resfinal, aes(x=Year, y=EI))+
  geom_line()+
  facet_wrap(~EPU)+
  labs(x="Year", y="EI", title="Output Quantity Index 1979-2019")+
  theme(plot.title=element_text(hjust=0.5))

eqi1

temp3<-ggplot(resfinal, aes(x=Year, y=ZI))+
  geom_line()+
  facet_wrap(~EPU)+
  labs(x="Year", y="QI", title="Environmental Index by EPU 1979-2019")+
  theme(plot.title=element_text(hjust=0.5))

temp3


temp4<-ggplot(resfinal, aes(x=Year, y=EQI))+
  geom_line()+
  facet_wrap(~EPU)+
  labs(x="Year", y="EQI",title="Environmental Output Quantity Index by EPU 1979-2019" )+
  theme(plot.title=element_text(hjust=0.5))

temp4


#gridExtra::grid.arrange(temp2, bottom="Figure 1.1 Median Values of EQI by Region")
#################################################################################



