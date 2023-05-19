##########################################################################
#Split FOSS data by species habitat groups
#calculate species catch diversity

#Author: Lansing Perng
#last updated: 6/29/21
##########################################################################
#this is original diversity code modified for HI calculations
#cross regional FOSS commercial diversity calculations in 'BODI National Data Cleaning.R'

rm(list=ls())

library(tidyr)
library(plyr)
library(dplyr)

# rec catch div analysis

FOSS<-read.csv('data/foss_landings and revenue_deflated.csv')
FOSS<-FOSS0[,-5]

#FOSS$Dollars <-  as.numeric(gsub(",","",FOSS$Dollars)) #Replace comma with nothing--aka delete commas
#FOSS$Pounds <-  as.numeric(gsub(",","",FOSS$Pounds)) #Replace comma with nothing--aka delete commas

#######new section for splitting reef fish and pelagics##########
#create new df with all uniqe species and assign 'Habitat' column with NAs

#species<-unique(FOSS0$NMFS.Name)
#species

#spec.hab<-data.frame('Species' = species, 'Habitat' = NA)

#write.csv(spec.hab, 'data/HCC cleaned/species habitat list.csv', row.names = F)


###not sure why below code didnt work--only assigns whatever value is after the last else*
#spec.hab$Habitat<-if(spec.hab$Species == 'BLACK MARLIN' || #|| is 'OR', && is 'AND'
#                     spec.hab$Species == 'DOLPHINFISH ' || 
#                           spec.hab$Species == 'ESCOLAR ' || 
#                           spec.hab$Species == 'MARLIN, BLUE ' ||
#                           spec.hab$Species == 'OILFISH ' ||
#                           spec.hab$Species == 'OPAH ' ||
#                           spec.hab$Species == 'SAILFISH ' ||
#                           spec.hab$Species == 'SPEARFISHES **' ||
#                           spec.hab$Species == 'STRIPED MARLIN' ||
#                           spec.hab$Species == 'SWORDFISH ' ||
#                           spec.hab$Species == 'TUNA, ALBACORE ' ||
#                           spec.hab$Species == 'TUNA, BIGEYE ' ||
#                           spec.hab$Species == 'TUNA, KAWAKAWA ' ||
#                           spec.hab$Species == 'TUNA, SKIPJACK ' ||
#                           spec.hab$Species == 'TUNA, YELLOWFIN ' ||
#                           spec.hab$Species == 'TUNAS **' ||
#                           spec.hab$Species == 'WAHOO ' ||
#                           spec.hab$Species == 'SHARKS, MAKO **' ||
#                           spec.hab$Species == 'SHARKS, THRESHER **	' ||
#                           spec.hab$Species == 'ALFONSINO ' ||
#                           spec.hab$Species == 'SUNFISH, OCEAN ' ||
#                           spec.hab$Species == 'BILLFISHES **	' ||
#                           spec.hab$Species == 'MACKEREL, FRIGATE	' ||
#                           spec.hab$Species == 'SHARKS, HAMMERHEAD **' ||
#                     spec.hab$Species == 'FLYINGFISHES **' ||
#                     spec.hab$Species == 'MACKEREL, CHUB ' ||
#                     spec.hab$Species == 'JACK, ALMACO ' ||
#                     spec.hab$Species == 'SHARK, WHITETIP, OCEANIC ' ||
#                     spec.hab$Species == 'MACKEREL, SNAKE ' || 
#                     spec.hab$Species == 'SQUID, JUMBO ' || 
#                     spec.hab$Species == 'TUNA, BLUEFIN PACIFIC ' || 
#                     spec.hab$Species == 'TUNA, LITTLE TUNNY ') {'Pelagic'
#  } else if(spec.hab$Species == 'HERRING, LAKE OR CISCO	' ||
#          spec.hab$Species == 'TILAPIAS **') {'Freshwater'
#    } else {'Reef'}

##entered 5 habitat types on excel sheet: Pelagic, Reef, Deep, Freshwater, Offshore
#and saved as csv (species_habitat.csv) --read in
spec.hab<-read.csv('data/HCC cleaned/species_habitat.csv')
names(FOSS)<-c('Year', 'Species', 'Pounds', 'Dollars')
FOSS.spec<-join(FOSS,spec.hab, by = 'Species', type = 'inner')

#######diversity calculation with just reef species############
FOSS.reef<-FOSS.spec[FOSS.spec$Habitat=='Reef',] #subset just reef species

TOT_FOSS.r <- aggregate(Pounds~Year, data=FOSS.reef, FUN=sum)
names(TOT_FOSS.r) <- c('Year','Tot_Comm')
FOSS.reef <- merge(FOSS.reef,TOT_FOSS.r, by=c('Year'))

#calculate shannon index: SUM[-p*ln(p)]
FOSS.reef$P_Catch <- -((FOSS.reef$Pounds/FOSS.reef$Tot_Comm)*
                    log(FOSS.reef$Pounds/FOSS.reef$Tot_Comm)) #-p*ln(p)
FOSSr_Div <- aggregate(P_Catch~Year, data=FOSS.reef, FUN=sum) #Sum previous line
FOSSr_Div$CommDiv <- exp(FOSSr_Div$P_Catch)
FOSSr_Div <- subset(FOSSr_Div, select=c('Year','CommDiv'))

FOSS_TOT.r<-aggregate(Tot_Comm~Year, data=FOSS.reef, FUN=mean) #mean because every row already has the total for Shannon calculation
FOSSr_Rev<-aggregate(Dollars~Year, data=FOSS.reef, FUN=sum)

FOSS.reef<-join(FOSSr_Rev, FOSS_TOT.r, by = "Year", type = 'inner')
FOSS.reef<-join(FOSS.reef, FOSSr_Div, by = "Year", type = 'inner')

write.csv(FOSS.reef, 'data/HCC cleaned/FOSS reef fish Pounds and Diversity.csv', row.names = F)

##scale values to millions
FOSS.reef$Revenue.Millions<-round(FOSS.reef$Dollars/1000000, digits = 3)
FOSS.reef$Catch.Millions<-round(FOSS.reef$Tot_Comm/1000000, digits = 3)
FOSS1<-FOSS.reef[,-c(2,3)] #delete by pounds and by dollars since we have millions columns

write.csv(FOSS1, 'data/HCC cleaned/FOSS reef fish with Millions.csv', row.names = F)


#######diversity calculation with just pelagic species############
FOSS.p<-na.omit(FOSS.spec[FOSS.spec$Habitat=='Pelagic',]) #subset just pelagic species

TOT_FOSS.p <- aggregate(Pounds~Year, data=FOSS.p, FUN=sum)
names(TOT_FOSS.p) <- c('Year','Tot_Comm')
FOSS.p <- merge(FOSS.p,TOT_FOSS.p, by=c('Year'))

#calculate shannon index: SUM[-p/ln(p)]
FOSS.p$P_Catch <- -(FOSS.p$Pounds/FOSS.p$Tot_Comm*
                         log(FOSS.p$Pounds/FOSS.p$Tot_Comm)) #-p/ln(p)
FOSSp_Div <- aggregate(P_Catch~Year, data=FOSS.p, FUN=sum) #Sum previous line
FOSSp_Div$CommDiv <- exp(FOSSp_Div$P_Catch)
FOSSp_Div <- subset(FOSSp_Div, select=c('Year','CommDiv'))

FOSS_TOT.p<-aggregate(Tot_Comm~Year, data=FOSS.p, FUN=mean) #mean because every row already has the total for Shannon calculation
FOSSp_Rev<-aggregate(Dollars~Year, data=FOSS.p, FUN=sum)

FOSS.p<-join(FOSSp_Rev, FOSS_TOT.p, by = "Year", type = 'inner')
FOSS.p<-join(FOSS.p, FOSSp_Div, by = "Year", type = 'inner')

write.csv(FOSS.p, 'data/HCC cleaned/FOSS pelagic fish Pounds and Diversity.csv', row.names = F)

##scale values to millions
FOSS.p$Revenue.Millions<-round(FOSS.p$Dollars/1000000, digits = 3)
FOSS.p$Catch.Millions<-round(FOSS.p$Tot_Comm/1000000, digits = 3)
FOSSp<-FOSS.p[,-c(2,3)] #delete by pounds and by dollars since we have millions columns

write.csv(FOSSp, 'data/HCC cleaned/FOSS pelagic fish with Millions.csv', row.names = F)

#######diversity calculation with just deep species############
FOSS.d<-na.omit(FOSS.spec[FOSS.spec$Habitat=='Deep',]) #subset just deep species

TOT_FOSS.d <- aggregate(Pounds~Year, data=FOSS.d, FUN=sum)
names(TOT_FOSS.d) <- c('Year','Tot_Comm')
FOSS.d <- merge(FOSS.d,TOT_FOSS.d, by=c('Year'))

#calculate shannon index: SUM[-p/ln(p)]
FOSS.d$P_Catch <- -(FOSS.d$Pounds/FOSS.d$Tot_Comm*
                      log(FOSS.d$Pounds/FOSS.d$Tot_Comm)) #-p/ln(p)
FOSSd_Div <- aggregate(P_Catch~Year, data=FOSS.d, FUN=sum) #Sum previous line
FOSSd_Div$CommDiv <- exp(FOSSd_Div$P_Catch)
FOSSd_Div <- subset(FOSSd_Div, select=c('Year','CommDiv'))

FOSS_TOT.d<-aggregate(Tot_Comm~Year, data=FOSS.d, FUN=mean) #mean because every row already has the total for Shannon calculation
FOSSd_Rev<-aggregate(Dollars~Year, data=FOSS.d, FUN=sum)

FOSS.d<-join(FOSSd_Rev, FOSS_TOT.d, by = "Year", type = 'inner')
FOSS.d<-join(FOSS.d, FOSSd_Div, by = "Year", type = 'inner')

write.csv(FOSS.d, 'data/HCC cleaned/FOSS deep fish Pounds and Diversity.csv', row.names = F)

##scale values to millions
FOSS.d$Revenue.Millions<-round(FOSS.d$Dollars/1000000, digits = 3)
FOSS.d$Catch.Millions<-round(FOSS.d$Tot_Comm/1000000, digits = 3)
FOSSd<-FOSS.d[,-c(2,3)] #delete by pounds and by dollars since we have millions columns

write.csv(FOSSd, 'data/HCC cleaned/FOSS deep fish with Millions.csv', row.names = F)

###########diversity calculate with all species combined########
##################################################################
TOT_FOSS <- aggregate(Pounds~Year, data=FOSS0, FUN=sum)
names(TOT_FOSS) <- c('Year','Tot_Comm')
FOSS <- merge(FOSS0,TOT_FOSS, by=c('Year'))

#calculate shannon index: SUM[-p/ln(p)]
FOSS$P_Catch <- -(FOSS$Pounds/FOSS$Tot_Comm*
                         log(FOSS$Pounds/FOSS$Tot_Comm)) #-p/ln(p)
FOSS_Div <- aggregate(P_Catch~Year, data=FOSS, FUN=sum) #Sum previous line
FOSS_Div$CommDiv <- exp(FOSS_Div$P_Catch)
FOSS_Div <- subset(FOSS_Div, select=c('Year','CommDiv'))

FOSS_TOT<-aggregate(Tot_Comm~Year, data=FOSS, FUN=mean) #mean because every row already has the total for Shannon calculation
FOSS_Rev<-aggregate(Dollars~Year, data=FOSS, FUN=sum)

FOSS<-join(FOSS_Rev, FOSS_TOT, by = "Year", type = 'inner')
FOSS<-join(FOSS, FOSS_Div, by = "Year", type = 'inner')
write.csv(FOSS, 'data/HCC cleaned/FOSS Totals and Diversity.csv', row.names = F)

##scale values to millions
FOSS$Revenue.Millions<-round(FOSS$Dollars/1000000, digits = 3)
FOSS$Catch.Millions<-round(FOSS$Tot_Comm/1000000, digits = 3)
FOSS<-FOSS[,-c(2,3)] #delete by pounds and by dollars since we have millions columns

write.csv(FOSS, 'data/HCC cleaned/FOSS with Millions.csv', row.names = F)


#FOSS$Region[FOSS$Region=="MID-ATLANTIC"] <- 'MA' #if we add smaller than island-scale regions 
#FOSS$Region[FOSS$Region=="NORTH ATLANTIC"] <- 'NE' #will need to add in "Region" column wherever grouping by Year currently appears

#FOSS$Units <- 'Effective Shannon'
#FOSS$Var <- 'Commercial Diversity of Catch' 

##Species include: 
specieslist<-unique(FOSS0$NMFS.Name)
specieslist


#FOSS$Source <- 'FOSS Commercial Fishing Data' # https://foss.nmfs.noaa.gov/apexfoss/f?p=215:200:84434366965::NO:::





################BRIEF EXPLORATION##############
plot(Dollars~Year, data = FOSS)
plot(Tot_Comm~Year, data = FOSS)
plot(CommDiv~Year, data = FOSS)





