########Clean up national datasets for IEA Regions######3
######October 2020
######Code by Lansing Perng

rm(list=ls())

####load libraries
library(tidyr)
library(plyr)
library(readxl)
library(openxlsx)

#libraries for GDP deflator
library(data.table)
library(tidyr)
library(dplyr)



##########
setwd("/Users/lansingperng/Desktop/Research/NOAA/HDWG/HDWG Analyses/cross regional data for DEA/")
#set to whatever folder the 5 data folders are saved in
#the google drive code wasn't authorizing for me for some reason
#but this should be an easy way to keep how we read files consistent


###############################################################
###################FOSS Commercial Fishing Data################
#commercial landings, commercial catch diversity, commercial revenue 
#1981-2019

FOSS0<-read.csv('FOSS commerical fishery data/foss_landings.csv')
FOSS<-FOSS0[,-c(6,7)] #cut off 'Collection' and 'Confidentiality Columns
unique(FOSS$State)

FOSS$Dollars <-  as.numeric(gsub(",","",FOSS$Dollars)) #Replace comma with nothing--aka delete commas
FOSS$Pounds <-  as.numeric(gsub(",","",FOSS$Pounds)) #Replace comma with nothing--aka delete commas

##### split regions ########
##ALASKA
FOSS_AK<-FOSS[FOSS$State=="ALASKA",] #subset only Alaska
write.csv(FOSS_AK, 'FOSS commerical fishery data/species disaggregated/FOSS AK.csv', row.names = F)

##CALIFORNIA CURRENT
FOSS_CA<-FOSS[FOSS$State=="CALIFORNIA" | 
                FOSS$State == "OREGON" | 
                FOSS$State == "WASHINGTON",]

write.csv(FOSS_CA, 'FOSS commercial fishery data/species disaggregated/FOSS CA.csv', row.names = F)


##GULF OF MEXICO
FOSS_GOM<-FOSS[FOSS$State=="ALABAMA" | 
                FOSS$State == "FLORIDA-WEST" |  #for FOSS FL was split by E & W, but not split for employment data
                FOSS$State == "LOUISIANA" |
                FOSS$State == "MISSISSIPPI" | 
                FOSS$State == "TEXAS",]
write.csv(FOSS_GOM, 'FOSS commercial fishery data/species disaggregated/FOSS GOM.csv', row.names = F)

##NORTHEAST
FOSS_NE<-FOSS[FOSS$State=="CONNECTICUT" | 
                 FOSS$State == "DELAWARE" |  #for FOSS FL was split by E & W, but not split for employment data
                 FOSS$State == "MAINE" |
                 FOSS$State == "MARYLAND" | 
                 FOSS$State == "MASSACHUSETTS" |
                 FOSS$State == "NEW HAMPSHIRE" |
                 FOSS$State == "NEW JERSEY" | 
                 FOSS$State == "NEW YORK" |
                 FOSS$State == "NORTH CAROLINA" | 
                 FOSS$State == "RHODE ISLAND" |
                 FOSS$State == "VIRGINIA",]
write.csv(FOSS_NE, 'FOSS commercial fishery data/species disaggregated/FOSS NE.csv', row.names = F)

##SOUTHEAST
FOSS_SE<-FOSS[FOSS$State=="FLORIDA-EAST" | 
                FOSS$State == "GEORGIA" | 
                FOSS$State == "SOUTH CAROLINA",]

write.csv(FOSS_SE, 'FOSS commercial fishery data/species disaggregated/FOSS SE.csv', row.names = F)

##HI Data already cleaned separately for HCC
##moved file to same folder as other regions and renamed

#add state column to HI data to match the others-->then analyze all 6 regions together
HI0<-read.csv('FOSS commercial fishery data/species disaggregated/FOSS HI0.csv')
HI0$State<-'HAWAII'
HI<-HI0[,c(1,5,2,3,4)] 
write.csv(HI, 'FOSS commercial fishery data/species disaggregated/FOSS HI00.csv', row.names = F)
#changed 2019 manually to match updated data, so save this as HI00 to not overwrite
############################DEFLATE DATA####################################
#would be better to deflate raw file, but already split into regional files
#can do quick for loop
Region<- c('AK', 'CA', 'GOM', 'NE', 'SE', 'HI')

base_year = 2019 # base year for GDP deflator - Maximum = 2020, Minimum = 1947  - max(GDPDEF_quarterly$Year)
# base_year: the base year for adjusting nominal values to real values
# pull from online text file at https://fred.stlouisfed.org/data/GDPDEF.txt instead of a flat file
temp <- tempfile()
temp.connect <- url("https://fred.stlouisfed.org/data/GDPDEF.txt")
temp <- data.table(read.delim(temp.connect, fill=FALSE, stringsAsFactors=FALSE, skip = 15))
temp <- temp %>% separate(col= "DATE..........VALUE", into=c("DATE", "GDPDEF"), sep="  ", convert=TRUE)
temp$DATE <- as.Date(temp$DATE)
temp$GDPDEF <- as.double(temp$GDPDEF)
GDPDEF_quarterly <- as_tibble(temp %>%
                                mutate(mon = as.integer(format(DATE,"%m"))) %>%
                                mutate(day = as.integer(format(DATE,"%d"))) %>%
                                mutate(Year = as.integer(format(DATE,"%Y"))) %>%
                                dplyr::select(GDPDEF, Year, mon) %>%  #  reduce to relevant columns
                                mutate(Quarter = "") %>% # Create quarters column
                                mutate(Quarter = ifelse(mon %in% 1:3, "Q1", Quarter)) %>%
                                mutate(Quarter = ifelse(mon %in% 4:6, "Q2", Quarter)) %>%
                                mutate(Quarter = ifelse(mon %in% 7:9, "Q3", Quarter)) %>%
                                mutate(Quarter = ifelse(mon %in% 10:12 , "Q4", Quarter)) %>%
                                dplyr::select(GDPDEF, Year, Quarter))
#I double checked, and annual deflator posted on the the St. Louis Fed is just
#the arithmetic average of the quarterly deflator by year, so collapsing to the average
GDPDEF_annual <- GDPDEF_quarterly %>% select(GDPDEF,Year) %>%
  group_by(Year) %>%
  mutate(GDPDEF=mean(GDPDEF)) %>% 
  distinct(.keep_all = TRUE) 
#Joining to the file you are needing to deflate--cross regional FOSS data (pulling from regional files separated from original)
for(i in 1:length(Region)) {
  REVENUEFILE<-read.csv(paste('FOSS commercial fishery data/species disaggregated/FOSS ', Region[i], '.csv', sep = '')) %>% 
    
    mutate(nominal_revenue = Dollars) #renaming revenue nominal revenue to keep track
  REVENUEFILE <- as_tibble(merge(REVENUEFILE, GDPDEF_annual, by="Year", all.x=TRUE))
  REVENUEFILE[["Dollars"]] <- REVENUEFILE[["nominal_revenue"]]*
    unique(GDPDEF_annual$GDPDEF[GDPDEF_annual$Year==base_year])/REVENUEFILE$GDPDEF 
    #deflating 'nominal_revenue' and replacing values in 'Dollars' column
  
  REVENUEFILE$GDPDEF <- NULL #delete deflator multiplier column
  write.csv(REVENUEFILE,paste('FOSS commercial fishery data/species disaggregated/deflated/FOSS ', Region[i], ' deflated.csv', sep = ''), row.names = F) 
}

###################### SUM FOSS ANNUAL SPECIES DISAG DATA TO SEE SPECIES_SPECIFIC TRENDS #########
Region<- c('AK', 'CA', 'GOM', 'NE', 'SE', 'HI')

for(i in 1:length(Region)) {
FOSS00<-read.csv(paste('FOSS commercial fishery data/species disaggregated/deflated/FOSS ', Region[i], ' deflated.csv',sep=''))
FOSS00<-FOSS00[,c("Year", "NMFS.Name" ,"Pounds" ,"Dollars" ,"nominal_revenue")] #keep everythign except state
#have to say which to keep instead of deleting statebecause some regions only have one state and no specific state column
FOSSannual<-aggregate(.~Year + NMFS.Name, data=FOSS00, sum) 
write.csv(FOSSannual,file =paste('FOSS commercial fishery data/species disaggregated/deflated/FOSS ', Region[i], ' deflated_no states.csv',sep=''), row.names = F)
}

library(dplyr)
library(ggplot2)
library(ggthemes)

for(i in 1:length(Region)) {
  FOSS.ann<-read.csv(paste('FOSS commercial fishery data/species disaggregated/deflated/FOSS ', Region[i], ' deflated_no states.csv',sep=''))
  str(FOSS.ann)
  FOSS.ann$NMFS.Name<-factor(FOSS.ann$NMFS.Name) #convert from chr to factor
#  FOSS.ann00<-FOSS.ann[FOSS.ann$Year > 2000,]
#  FOSS.ann00<-FOSS.ann00[FOSS.ann$Year < 1983,]
#  FOSS.ann00<-FOSS.ann00[FOSS.ann$NMFS.Name!= 'MENHADENS **',]
  FOSS.agg<-aggregate(.~NMFS.Name, data= FOSS.ann, FUN = sum)
  top.lbs<-top_n(FOSS.agg, 15, Pounds) 
  top.lbs<-top.lbs[,c('NMFS.Name','Pounds','Dollars')]
  names(top.lbs)<-c('NMFS.Name','Pounds.tot','Dollars.tot')
  top.lbs$TOP<-'POUNDS'
  top.rev<-top_n(FOSS.agg, 15, Dollars) 
  top.rev<-top.rev[,c('NMFS.Name','Pounds','Dollars')]
  names(top.rev)<-c('NMFS.Name','Pounds.tot','Dollars.tot')
  top.rev$TOP<-'REVENUE'
  top<-rbind(top.lbs, top.rev)
  write.csv(top,paste('FOSS commercial fishery data/species disaggregated/deflated/summarize species by region/', Region[i], ' top pounds and revenue.csv',sep=''))
  
  FOSS.ann0<-join(FOSS.ann,top, by = 'NMFS.Name', type = 'inner') #keep only species that are either in top 15 pounds or rev
  unique(FOSS.ann0$NMFS.Name) #could be more than 15
  FOSS.ann0$NMFS.Name<-factor(FOSS.ann0$NMFS.Name) #convert from chr to factor
  
  ggplot(FOSS.ann0, aes(Year, Pounds)) +
    geom_line()+
    facet_wrap(~NMFS.Name) +
    theme_bw()

  ggsave(paste('FOSS commercial fishery data/species disaggregated/deflated/summarize species by region/', Region[i], ' top species pounds.png',sep=''), 
         width =  12, height = 10, units = 'in', #w & h in inches
         dpi = 300, bg = 'transparent')
  
  ggplot(FOSS.ann0, aes(Year, Dollars)) +
    geom_line()+
    facet_wrap(~NMFS.Name)+
    theme_bw()
  
  ggsave(paste('FOSS commercial fishery data/species disaggregated/deflated/summarize species by region/', Region[i], ' top species dollars.png',sep=''), 
         width =  12, height = 10, units = 'in', #w & h in inches
         dpi = 300, bg = 'transparent')
  
}
######################################################################
############ calculate commercial catch shannon diversity ############
#for loop for all 6 regions

Region<- c('AK', 'CA', 'GOM', 'NE', 'SE', 'HI')

for(i in 1:6) {
  FOSS0<-read.csv(paste('FOSS commercial fishery data/species disaggregated/deflated/FOSS ', Region[i], ' deflated.csv', sep = ''))
  FOSS0 <- subset (FOSS0, select = -State) #delete 'State' column for aggregating values by year

  TOT_FOSS <- aggregate(Pounds~Year, data=FOSS0, FUN=sum)
  names(TOT_FOSS) <- c('Year','Tot_Comm') #rename because total pounds will be merged with original dataframe
  FOSS <- merge(FOSS0,TOT_FOSS, by=c('Year'))
  
  #calculate shannon index: SUM[-p*ln(p)]
  FOSS$P_Catch <- -(FOSS$Pounds/FOSS$Tot_Comm*
                      log(FOSS$Pounds/FOSS$Tot_Comm)) #-p*ln(p)
  
  FOSS_Div <- aggregate(P_Catch~Year, data=FOSS, FUN=sum) #Sum previous line
  FOSS_Div$CommDiv <- exp(FOSS_Div$P_Catch)
  FOSS_Div <- subset(FOSS_Div, select=c('Year','CommDiv'))
  
  #repeat div calc for revenue
  TOT_FOSSr <- aggregate(Dollars~Year, data=FOSS0, FUN=sum)
  names(TOT_FOSSr) <- c('Year','Tot_Rev') #rename because total Dollars will be merged with original dataframe
  FOSS <- merge(FOSS,TOT_FOSSr, by=c('Year'))
  
  #calculate shannon index: SUM[-p*ln(p)]
  FOSS$P_Rev <- -(FOSS$Dollars/FOSS$Tot_Rev*
                      log(FOSS$Dollars/FOSS$Tot_Rev)) #-p*ln(p)
  
  FOSS_Divr <- aggregate(P_Rev~Year, data=FOSS, FUN=sum) #Sum previous line
  FOSS_Divr$RevDiv <- exp(FOSS_Divr$P_Rev)
  FOSS_Divr <- subset(FOSS_Divr, select=c('Year','RevDiv'))
  
  #join all vectors/dfs
  FOSS<-join(TOT_FOSSr, FOSS_Divr, by = "Year", type = 'inner')
  FOSS<-join(FOSS, TOT_FOSS, by = "Year", type = 'inner')
  FOSS<-join(FOSS, FOSS_Div, by = "Year", type = 'inner')
  names(FOSS) <- c('Year','Dollars', 'RevDiv','Tot_Comm', 'CommDiv')
  
  write.csv(FOSS, paste('FOSS commercial fishery data/species aggregated with diversity/FOSS', Region[i] ,'with catch and rev Diversity_deflated.csv', sep = ' '), row.names = F)
  
  #############PULLING OUT MENHADENS TO SEE THE DIFFERENCE####################
  #multiple names: 'MENHADEN, ATLANTIC ' & 'MENHADENS **'
  #FOSS.MH<-subset(FOSS0, (NMFS.Name %in% c('MENHADEN, ATLANTIC ', 'MENHADENS **'))) #subsetting rows where NMFS.Name includes menhadens
  #write.csv(FOSS.MH, paste('FOSS commercial fishery data/species disaggregated/FOSS ', Region[i] ,' Menhadens Only_deflated.csv', sep = ''), row.names = F)
  #AK, CA, and HI have no menhadens
  
  FOSS.no.MH<-subset(FOSS0, !(NMFS.Name %in% c('MENHADEN, ATLANTIC ', 'MENHADENS **')))
  #subsetting everything EXCEPT menhaden rows
  
  write.csv(FOSS.no.MH, paste('FOSS commercial fishery data/species disaggregated/FOSS ', Region[i] ,' No Menhadens_deflated.csv', sep = ''), row.names = F)
  
  #sum pounds per year and add total annual values to each species disaggregated row to get proportions
  TOT_FOSS <- aggregate(Pounds~Year, data=FOSS.no.MH, FUN=sum)
  names(TOT_FOSS) <- c('Year','Tot_Comm') #rename because total pounds will be merged with original dataframe
  FOSS <- merge(FOSS.no.MH,TOT_FOSS, by=c('Year'))
  
  #calculate shannon index: SUM[-p*ln(p)]
  FOSS$P_Catch <- -(FOSS$Pounds/FOSS$Tot_Comm*
                      log(FOSS$Pounds/FOSS$Tot_Comm)) #-p*ln(p)
  FOSS_Div <- aggregate(P_Catch~Year, data=FOSS, FUN=sum) #Sum previous line
  FOSS_Div$CommDiv <- exp(FOSS_Div$P_Catch)
  FOSS_Div <- subset(FOSS_Div, select=c('Year','CommDiv'))
  
  #repeat div calc for revenue
  TOT_FOSSr <- aggregate(Dollars~Year, data=FOSS.no.MH, FUN=sum)
  names(TOT_FOSSr) <- c('Year','Tot_Rev') #rename because total Dollars will be merged with original dataframe
  FOSS <- merge(FOSS,TOT_FOSSr, by=c('Year'))
  
  #calculate shannon index: SUM[-p*ln(p)]
  FOSS$P_Rev <- -(FOSS$Dollars/FOSS$Tot_Rev*
                    log(FOSS$Dollars/FOSS$Tot_Rev)) #-p*ln(p)
  
  FOSS_Divr <- aggregate(P_Rev~Year, data=FOSS, FUN=sum) #Sum previous line
  FOSS_Divr$RevDiv <- exp(FOSS_Divr$P_Rev)
  FOSS_Divr <- subset(FOSS_Divr, select=c('Year','RevDiv'))
  
  #join all vectors/dfs
  FOSS<-join(TOT_FOSSr, FOSS_Divr, by = "Year", type = 'inner')
  FOSS<-join(FOSS, TOT_FOSS, by = "Year", type = 'inner')
  FOSS<-join(FOSS, FOSS_Div, by = "Year", type = 'inner')
  names(FOSS) <- c('Year','Dollars', 'RevDiv','Tot_Comm', 'CommDiv') #rename Dollars to keep consistency with code already written in 'Gratia cross regiona analyses.R'
  
  write.csv(FOSS, paste('FOSS commercial fishery data/species aggregated with diversity/FOSS', Region[i] ,'with catch and rev Diversity no MH_deflated.csv', sep = ' '), row.names = F)
  
  
  #write csv of cleaned up data before rescaling
  #write into main folder but manually separate into subfolders after
}

#############PULLING OUT MENHADENS TO SEE THE DIFFERENCE####################


###############################################################################

#rescale so values are comparable to other indicators for BODI calculation
#another for loop for the 6 regions

for(i in 1:6) {
  FOSS<-read.csv(paste('FOSS commercial fishery data/species aggregated with diversity/FOSS', Region[i] ,'with Diversity no MH_deflated.csv', sep = ' '))
  
  FOSS$Revenue.Millions<-round(FOSS$Dollars/1000000, digits = 3)
  FOSS$Catch.Millions<-round(FOSS$Tot_Comm/1000000, digits = 3)
  FOSS1<-FOSS[,-c(2,3)] #delete by pounds and by dollars since we have millions columns
  
  write.csv(FOSS1, paste('FOSS commercial fishery data/with diversity scaled/FOSS', Region[i] ,'with Diversity no MH scaled_deflated.csv', sep = ' '), row.names = F)
  
}

for(i in 1:6) {
  FOSS<-read.csv(paste('FOSS commercial fishery data/species aggregated with diversity/FOSS', Region[i] ,'with Diversity_deflated.csv', sep = ' '))
  
  FOSS$Revenue.Millions<-round(FOSS$Dollars/1000000, digits = 3)
  FOSS$Catch.Millions<-round(FOSS$Tot_Comm/1000000, digits = 3)
  FOSS1<-FOSS[,-c(2,3)] #delete by pounds and by dollars since we have millions columns
  
  write.csv(FOSS1, paste('FOSS commercial fishery data/with diversity scaled/FOSS', Region[i] ,'with Diversity scaled_deflated.csv', sep = ' '), row.names = F)
  
}


################################################################################
##MRIP Recreational Fishing Data
#2003-2020

#########REVISED to include 2021 values and PSEs for Monte Carlo sim##################
setwd("/Users/lansingperng/Desktop/Research/NOAA/HDWG/HDWG Analyses/")
#recland0<-read.csv('cross regional data/MRIP_Catch_2021.csv', skip = 26) #skip first 26 lines which are metadata

#test new data download (4/6/22) from MRIP National Series--same source as ecowatch--because plot didnt match up with ecowatch
recland0<-read.csv('cross regional data/mrip_national_series.csv', skip = 26) #skip first 26 lines which are metadata
recland<-recland0[,-c(1,4)] #delete estimate status
names(recland)<- c('Year', 'Region', 'Harvest' ,'PSE.Harvest', 'Released', 'PSE.Released')

#change columns from character to numeric
recland$Released<-as.numeric(recland0$Released)
recland$PSE.Harvest<-as.numeric(recland0$PSE.Harvest)
recland$PSE.Released<-as.numeric(recland0$PSE.Released)

recland[is.na(recland)]<-0 #replace NAs with 0

#calculate SD with provided PSE
recland$SE.Harvest<-((recland$PSE.Harvest/100)*recland$Harvest)
recland$SE.Released<-((recland$PSE.Released/100)*recland$Released)
#recland$RCatch<-recland$Harvest+recland$Released # original-- try with only harvest
#recland$SE<-recland$SE.Harvest+recland$SE.Released
recland$RCatch<-recland$Harvest
recland$SE<-recland$SE.Harvest

recland.cut<-recland[,c(1,2,9,10)]
write.csv(recland.cut, paste('cross regional data/MRIP Catch All Regions_SE.csv', sep = ' '), row.names = F)

#########split by regions
unique(recland.cut$Region)
#Caribbean was excluded from analysis

##ALASKA
rec_AK<- recland.cut[recland.cut$Region=="ALASKA",]
write.csv(rec_AK, 'cross regional data/AK/AK MRIP Catch_SE.csv', row.names = F)

##CALIFORNIA CURRENT
rec_CC<- recland.cut[recland.cut$Region=="SOUTHERN CALIFORNIA" | 
                       recland.cut$Region == "NORTHERN CALIFORNIA" | 
                       recland.cut$Region == "PACIFIC NORTHWEST",]
rec_CC$Region<-'CALIFORNIA CURRENT'
rec_CC <- aggregate(.~Year+Region, data=rec_CC, FUN=sum)
write.csv(rec_CC, 'cross regional data/CC/CC MRIP Catch_SE.csv', row.names = F)

##GULF OF MEXICO
rec_GOM<- recland.cut[recland.cut$Region=="GULF OF MEXICO" |
                        recland.cut$Region=="TEXAS",]
rec_GOM$Region<-'GOM'
rec_GOM <- aggregate(.~Year+Region, data=rec_GOM, FUN=sum)

write.csv(rec_GOM, 'cross regional data/GOM/GOM MRIP Catch_SE.csv', row.names = F)

##NORTHEAST
rec_NE<- recland.cut[recland.cut$Region=="NORTH ATLANTIC" |
                       recland.cut$Region=="MID-ATLANTIC" ,]
rec_NE$Region<-'NE'
rec_NE <- aggregate(.~Year+Region, data=rec_NE, FUN=sum)

write.csv(rec_NE, 'cross regional data/NE/NE MRIP Catch_SE.csv', row.names = F)

##SOUTHEAST
rec_SE<- recland.cut[recland.cut$Region=="SOUTH ATLANTIC" ,]
write.csv(rec_SE, 'cross regional data/SE/SE MRIP Catch_SE.csv', row.names = F)

##HAWAII
rec_HI<-recland.cut[recland.cut$Region=="HAWAIIAN ISLANDS",]
write.csv(rec_HI, 'cross regional data/HI/HI MRIP Catch_SE.csv', row.names = F)

######################################################

#angler trips (rec effort)

angtrips0<-read.csv('cross regional data//MRIP_Angler_Trips_2021.csv', skip = 26)
angtrips<-angtrips0[,c(2:5)]

#change columns from character to numeric
angtrips$PSE<-as.numeric(angtrips$PSE)

angtrips[is.na(angtrips)]<-0 #replace NAs with 0

#set upper and lower limits of 95CI with provided PSE
angtrips$SE.Trips<-((angtrips$PSE/100)*angtrips$Angler.Trips)

#angtrips$Trips.lowerCI<-angtrips$Angler.Trips-(1.96*((angtrips$PSE/100)*angtrips$Angler.Trips))
#angtrips$Trips.Range<-angtrips$Trips.upperCI-angtrips$Trips.lowerCI #exploratory

angtrips<-angtrips[,-c(4)]

write.csv(angtrips, paste('cross regional data/MRIP Angler Trips All Regions_SE.csv', sep = ' '), row.names = F)

#########split by regions
unique(angtrips$Region)
#South Atlantic and Caribbean excluded from analysis

##ALASKA
trips_AK<- angtrips[angtrips$Region=="ALASKA",]
write.csv(trips_AK, 'cross regional data/AK/AK MRIP Trips_SE.csv', row.names = F)

##CALIFORNIA CURRENT
trips_CC<- angtrips[angtrips$Region=="SOUTHERN CALIFORNIA" | 
                      angtrips$Region == "NORTHERN CALIFORNIA" | 
                      angtrips$Region == "PACIFIC NORTHWEST",]
trips_CC$Region<-'CALIFORNIA CURRENT'
trips_CC <- aggregate(.~Year+Region, data=trips_CC, FUN=sum)

write.csv(trips_CC, 'cross regional data/CC/CC MRIP Trips_SE.csv', row.names = F)

##GULF OF MEXICO
trips_GOM<- angtrips[angtrips$Region=="GULF OF MEXICO" |
                       angtrips$Region=="TEXAS",]
trips_GOM$Region<-'GOM'
trips_GOM <- aggregate(.~Year+Region, data=trips_GOM, FUN=sum)

write.csv(trips_GOM, 'cross regional data/GOM/GOM MRIP Trips_SE.csv', row.names = F)

##NORTHEAST
trips_NE<- angtrips[angtrips$Region=="NORTH ATLANTIC" |
                      angtrips$Region=="MID-ATLANTIC" ,]
trips_NE$Region<-'NE'
trips_NE <- aggregate(.~Year+Region, data=trips_NE, FUN=sum)

write.csv(trips_NE, 'cross regional data/NE/NE MRIP Trips_SE.csv', row.names = F)

##SOUTHEAST
trips_SE<- angtrips[angtrips$Region=="SOUTH ATLANTIC" ,]
write.csv(trips_SE, 'cross regional data/SE/SE MRIP Trips_SE.csv', row.names = F)

#HAWAII
trips_HI<-angtrips[angtrips$Region=="HAWAIIAN ISLANDS",] #subset only the rows where Region column is exactly "HAWAIIAN ISLANDS"
write.csv(trips_HI, 'cross regional data/HI/HI MRIP Trips_SE.csv', row.names = F)

#no need to join into one data frame because it will be done during the simulation in 'Gratia cross regional analysis.R'

###################### SUM MRIP ANNUAL SPECIES DISAG DATA TO SEE SPECIES-SPECIFIC TRENDS #########
Region<- c('AK', 'CA', 'GOM', 'NE', 'SE', 'HI')

for(i in 1:length(Region)) {
  MRIP00<-read.csv(paste('MRIP Rec Data/MRIP ', Region[i], ' Catch.csv',sep=''))
  MRIP00<-MRIP00[,c("Year",'Species', "RCatch")] 
  MRIPannual<-aggregate(.~Year + Species, data=MRIP00, sum) 
  write.csv(MRIPannual,file =paste('MRIP Rec Data/MRIP ', Region[i], ' Catch annual.csv',sep=''), row.names = F)
}

library(dplyr)
library(ggplot2)
library(ggthemes)

for(i in 1:length(Region)) {
  MRIP.ann<-read.csv(paste('MRIP Rec Data/MRIP ', Region[i], ' Catch annual.csv',sep=''))
  str(MRIP.ann)
  MRIP.ann$Species<-factor(MRIP.ann$Species) #convert from chr to factor
  #  MRIP.ann00<-MRIP.ann[MRIP.ann$Year > 2000,]
  #  MRIP.ann00<-MRIP.ann00[MRIP.ann$Year < 1983,]
  #  MRIP.ann00<-MRIP.ann00[MRIP.ann$Species!= 'MENHADENS **',]
  MRIP.agg<-aggregate(.~Species, data= MRIP.ann, FUN = sum)
  top<-top_n(MRIP.agg, 25, RCatch) 
  top<-top[,c('Species','RCatch')]
  names(top)<-c('Species','RCatch.tot')

  write.csv(top,paste('MRIP Rec Data/MRIP ', Region[i], ' Catch_top.csv',sep=''))
  
  MRIP.ann0<-join(MRIP.ann,top, by = 'Species', type = 'inner') #keep only species that are either in top 15 RCatch or rev
  unique(MRIP.ann0$Species)
  MRIP.ann0$Species<-factor(MRIP.ann0$Species) #convert from chr to factor
  
  ggplot(MRIP.ann0, aes(Year, RCatch)) +
    geom_line()+
    facet_wrap(~Species) +
    theme_bw()
  
  ggsave(paste('MRIP Rec Data/summarize species by region/', Region[i], ' top species RCatch.png',sep=''), 
         width =  12, height = 10, units = 'in', #w & h in inches
         dpi = 300, bg = 'transparent')
  
}


################################################################################
#original data cleaning without error
#recreational landings (harvest + released)
#recland0<-read.csv('MRIP Rec Data/mrip_national_series.csv', skip = 28) #skip first 28 lines which are metadata
#recland0$Released..B2. <-as.numeric(recland0$Released..B2.) #convert values from character to numeric

#recland0$Released..B2.[is.na(recland0$Released..B2.)] = 0 #replace NAs in Released column with 0s
#recland0$RCatch<- rowSums(recland0[,c(5,7)]) #sum columns 5 and 7 (Harvest + Released) to get total rec catch

recland0<-read.csv('data/MRIP_National_Harvest.csv') #diff from original raw downloaded file
recland0$RCatch<- rowSums(recland0[,c(5,7)]) #sum columns 5 and 7 (Harvest + Released) to get total rec catch
recland<-recland0[,c(2:4,9)]
names(recland)<- c('Year', 'Region', 'Species' , 'RCatch')
write.csv(recland, paste('MRIP Rec Data/MRIP Catch cleaned All Regions.csv', sep = ' '), row.names = F)

#########split by regions
unique(recland$Region)
#South Atlantic and Caribbean were not included

##ALASKA
rec_AK<- recland[recland$Region=="ALASKA",]
write.csv(rec_AK, 'MRIP Rec Data/MRIP AK Catch.csv', row.names = F)

##CALIFORNIA CURRENT
rec_CA<- recland[recland$Region=="SOUTHERN CALIFORNIA" | 
                   recland$Region == "NORTHERN CALIFORNIA" | 
                   recland$Region == "PACIFIC NORTHWEST",]
write.csv(rec_CA, 'MRIP Rec Data/MRIP CA Catch.csv', row.names = F)

##GULF OF MEXICO
rec_GOM<- recland[recland$Region=="GULF OF MEXICO" |
                    recland$Region=="TEXAS",]
write.csv(rec_GOM, 'MRIP Rec Data/MRIP GOM Catch.csv', row.names = F)

##NORTHEAST
rec_NE<- recland[recland$Region=="NORTH ATLANTIC" |
                    recland$Region=="MID-ATLANTIC" ,]
write.csv(rec_NE, 'MRIP Rec Data/MRIP NE Catch.csv', row.names = F)

##SOUTHEAST
rec_SE<- recland[recland$Region=="SOUTH ATLANTIC" ,]
write.csv(rec_SE, 'MRIP Rec Data/MRIP SE Catch.csv', row.names = F)

#rec_HI<-recland[recland$Region=="HAWAIIAN ISLANDS",]
#already have cleaned HI rec data in HI folder


#####join rec catch & trips into one dataframe for each region
MRIPcatch_data<- c('MRIP Rec Data/MRIP AK Catch.csv', 'MRIP Rec Data/MRIP CA Catch.csv', 
                     'MRIP Rec Data/MRIP GOM Catch.csv', 'MRIP Rec Data/MRIP NE Catch.csv',
                   'MRIP Rec Data/MRIP SE Catch.csv')

MRIPtrips_data<- c('MRIP Rec Data/MRIP AK Trips.csv', 'MRIP Rec Data/MRIP CA Trips.csv', 
                   'MRIP Rec Data/MRIP GOM Trips.csv', 'MRIP Rec Data/MRIP NE Trips.csv',
                   'MRIP Rec Data/MRIP SE Trips.csv')

for(i in 1:5) {
  rec<-read.csv(paste(MRIPcatch_data[i]))
  trips<-read.csv(paste(MRIPtrips_data[i]))
  
  #first need to aggregate all species
  rec<-rec[,-c(2,3)]
  rec <- aggregate(RCatch~Year, data=rec, FUN=sum)
  
  trips<-trips[,-2]
  trips <- aggregate(Angler.Trips~Year, data=trips, FUN=sum)
  
  recr<-join(trips,rec, by = "Year", type = 'inner')
  
  recr$Trips.Thousands<-recr$Angler.Trips/1000
  recr$RCatch.Thousands<-recr$RCatch/1000
  #leave separate from FOSS for now because FOSS is longer term--better for GAMs
  
  Region<- c('AK', 'CA', 'GOM', 'NE', 'SE')
  write.csv(recr,paste('MRIP Rec Data/MRIP', Region[i] ,'Annual Averages scaled.csv', sep = ' '), row.names = F)

}

#########################################################################################
###### BLS (bureau of labor statistics)
#2001-2019

#write industry vector to call in the for loops
industries<-c('Fishing', 'Seaf Markets', 'Seaf Packaging', 'Seaf Wholesale')

###################################ALASKA REGION###################################
######
#attempt to impute fishing employment and wages values from fishing subindustries 
#(finfish and shellfish have more data) and the two total up to the AK fishing totals that are provided
#if this works, will need to optimize for less interaction because I have to see the file list to pick which to use currently

  AKfiles<- list.files(path = "Bureau of Labor Statistics/Alaska", pattern = paste("AK",industries[i],"*"), full.names = T)
  #list all AK files of i industry--all 3 inds; loop will cycle through all 4 industries in the order listed in 'industries' vector
  
  #one df for employment, one for wages--with fin, shell, and totals to get sums and run regressions
  ##EMPLOYMENT
  #first finfish
  emp.fin<-read_excel(AKfiles[3], skip = 13)
  emp0<-emp.fin[,c('Year','Annual')] #make working df to add other values to
  names(emp0)<-c('Year','Finfish')
  
  #add shellfish
  emp.shell<-read_excel(AKfiles[5], skip = 13)
  emp0$Shellfish<-emp.shell$Annual
  
  #add total fishing for comparison
  emp.fsh<-read_excel(AKfiles[1], skip = 13)
  emp0$Fishing.raw<-c(emp.fsh$Annual,NA) #had to add the NA for 2020 because old fishing data only went up to 2019
  #checked new data, 2020 is NA anyway so this was quicker
  
  #calculate fin + shell totals
  emp0$Fishing.tot<-rowSums(emp0[ , c(2:3)], na.rm=F)
  
  plot(Fishing.tot~Finfish, data = emp0, cex = 2) #explore data
  fin.lm<-lm(Fishing.tot~Finfish, data = emp0) #linear regression
  summary(fin.lm) #very high R-squared
  pred<-predict(fin.lm, newdata = emp0[,c('Finfish')], se.fit = T) #get predicted values and SEs
  points(emp0$Finfish,pred$fit, col = 'red')
  #see how predicted data look overlaid on real observations
  
  #First put Fishing.raw totals that we have
  for (k in 1:length(emp0$Fishing.tot)) {
    emp0$Fishing.tot[k]<-if(is.na(emp0$Fishing.raw[k])) { emp0$Fishing.tot[k] #if Fishing raw is na, leave Fishing tot as is
    } else{emp0$Fishing.raw[k] #if not na, replace with Fishing raw value
    }
  } 
  
#save original data in separate column so we can go back and mark predicted data after calculating gamfit
  emp0$Fishing.raw<-emp0$Fishing.tot
  
  #Then add column for pred.se while Fishing.tot still has NAs so that it only fills SE values where we use predictions
  #-->might use in MC sim later
  for (k in 1:length(emp0$Fishing.tot)) {
    emp0$pred.se[k]<-if(is.na(emp0$Fishing.tot[k])) {pred$se.fit[k] #if Fishing tot is na, add se value
    } else{NA} #if Fishing.tot not NA, then put NA for se column
    }
  
  #Then add predicted values
  for (k in 1:length(emp0$Fishing.tot)) {
    emp0$Fishing.tot[k]<-if(is.na(emp0$Fishing.tot[k])) {pred$fit[k] #if Fishing tot is na, use predicted value
    } else{emp0$Fishing.tot[k] #if not na, leave as is
    }
  } 
  emp<-emp0[,c('Year','Fishing.tot','pred.se')]
  names(emp)<-c('Year','Employment','Employment.se')
  
  ##WAGES
  #first finfish
  wag.fin<-read_excel(AKfiles[4], skip = 13)
  wag0<-wag.fin[,c('Year','Annual')] #make working df to add other values to
  names(wag0)<-c('Year','Finfish')
  
  #add shellfish
  wag.shell<-read_excel(AKfiles[6], skip = 13)
  wag0$Shellfish<-wag.shell$Annual
  
  #add total fishing for comparison
  wag.fsh<-read_excel(AKfiles[7], skip = 13)
  wag0$Fishing.raw<-c(wag.fsh$Annual,NA) #had to add the NA for 2020 because old fishing data only went up to 2019
  #checked new data, 2020 is NA anyway so this was quicker
  
  #calculate fin + shell totals
  wag0$Fishing.tot<-rowSums(wag0[ , c(2:3)], na.rm=F)
  
  plot(Fishing.tot~Finfish, data = wag0, cex = 2) #explore data
  fin.lm<-lm(Fishing.tot~Finfish, data = wag0) #linear regression
  summary(fin.lm) #very high R-squared
  pred<-predict(fin.lm, newdata = wag0[,c('Finfish')], se.fit = T) #get predicted values and SEs
  points(wag0$Finfish,pred$fit, col = 'red')
  #see how predicted data look overlaid on real observations
  
  #First put Fishing.raw totals that we have
  for (k in 1:length(wag0$Fishing.tot)) {
    wag0$Fishing.tot[k]<-if(is.na(wag0$Fishing.raw[k])) { wag0$Fishing.tot[k] #if Fishing raw is na, leave Fishing tot as is
    } else{wag0$Fishing.raw[k] #if not na, replace with Fishing raw value
    }
  } 
  
  #Then add column for pred.se while Fishing.tot still has NAs so that it only fills SE values where we use predictions
  #-->might use in MC sim later
  for (k in 1:length(wag0$Fishing.tot)) {
    wag0$pred.se[k]<-if(is.na(wag0$Fishing.tot[k])) {pred$se.fit[k] #if Fishing tot is na, add se value
    } else{NA} #if Fishing.tot not NA, then put NA for se column
  }
  
  #Then add predicted values
  for (k in 1:length(wag0$Fishing.tot)) {
    wag0$Fishing.tot[k]<-if(is.na(wag0$Fishing.tot[k])) {pred$fit[k] #if Fishing tot is na, use predicted value
    } else{wag0$Fishing.tot[k] #if not na, leave as is
    }
  } 
  wag<-wag0[,c('Year','Fishing.tot','pred.se')]
  names(wag)<-c('Year','Wages.Thousands','Wages.se')

  #join emp and wag dfs
  AKFsh<-join(emp,wag,by = 'Year', type = 'full')
  
  ###### MAKE FINAL DF BY ADDING IN ESTABLISHMENTS
  #establishments
  est0<-read_excel(AKfiles[2], skip = 13)
  est<-est0[,c('Year','Annual')]
  names(est)<-c('Year','Establishments')
  
  #join imputed emp and wag to establishments
  AKFsh<-join(est,AKFsh,by = 'Year', type = 'full')
  AKFsh$State<-'AK'
  AKFsh$Industry<-'Fishing'
 # write.csv(AKFsh,paste('cross regional data/',folders[i],'/',files[i],' BLS ',BLSinds[k],' Totals.csv', sep=''),row.names = F)
  write.csv(AKFsh,paste('Bureau of Labor Statistics/Alaska/cleaned/AK BLS',industries[i],".csv"), row.names = F)
  

#####


#there are 12 files in the AK folder: 4 industries X 3 indicators

#since, AK is the only state in AK IEA region, no need to make vector for region

for (i in 1:length(industries)) {
  AKfiles<- list.files(path = "Bureau of Labor Statistics/Alaska", pattern = paste("AK",industries[i],"*"), full.names = T)
  #list all AK files of i industry--all 3 inds; loop will cycle through all 4 industries in the order listed in 'industries' vector
  
  #employment first (alphabetical order)
  emp<-read_excel(AKfiles[1], skip = 13)
  emp<-emp[,c(1,14)]
  names(emp)<-c('Year','Employment')
  
  #establishments
  est<-read_excel(AKfiles[2], skip = 13)
  est<-est[,c(1,6)]
  names(est)<-c('Year','Establishments')
  
  #wages
  wag<-read_excel(AKfiles[3], skip = 13)
  wag<-wag[,c(1,6)]
  names(wag)<-c('Year','Wages.Thousands')
  
  #join all three variables
  df0<-join(emp,est, by = 'Year', type = 'inner')
  df<-join(df0,wag, by = 'Year', type = 'inner')
  
  df$Industry<-paste(industries[i]) #make 'Industry' column
  df$State<-"AK" #make 'State' column
  df<-df[,c(1,6,5,2,3,4)]
  df$Employment.Tot<-df$Employment

  df$Wages.Tot.Millions<-(df$Wages.Thousands)/1000 

  write.csv(df,paste('Bureau of Labor Statistics/Alaska/cleaned/AK BLS',industries[i],".csv"), row.names = F)
  
}

##################################HAWAII REGION###################################
#there are 12 files in the HI folder: 4 industries X 3 indicators

#since, HI is the only state in PACIFIC ISLANDS IEA region, no need to make vector for region

for (i in 1:length(industries)) {
  HIfiles<- list.files(path = "Bureau of Labor Statistics/Pacific Islands", pattern = paste("HI",industries[i],"*"), full.names = T)
  #list all AK files of i industry--all 3 inds; loop will cycle through all 4 industries in the order listed in 'industries' vector
  
  #employment first (alphabetical order)
  emp<-read_excel(HIfiles[1], skip = 13)
  emp<-emp[,c(1,14)]
  names(emp)<-c('Year','Employment')
  
  #establishments
  est<-read_excel(HIfiles[2], skip = 13)
  est<-est[,c(1,6)]
  names(est)<-c('Year','Establishments')
  
  #wages
  wag<-read_excel(HIfiles[3], skip = 13)
  wag<-wag[,c(1,6)]
  names(wag)<-c('Year','Wages.Thousands')
  
  #join all three variables
  df0<-join(emp,est, by = 'Year', type = 'inner')
  df<-join(df0,wag, by = 'Year', type = 'inner')
  
  df$Industry<-paste(industries[i]) #make 'Industry' column
  df$State<-"HI" #make 'State' column
  df<-df[,c(1,6,5,2,3,4)]
  df$Employment.Tot<-df$Employment 
  #total employment is Employment (per establishment) * Establishments
  
  df$Wages.Tot.Millions<-(df$Wages.Thousands)/1000 
  #total wages (in millions--scaled to be comparable to other values) is Wages (per establishment)* Establishments
  
  write.csv(df,paste('Bureau of Labor Statistics/Pacific Islands/cleaned/HI BLS',industries[i],".csv"), row.names = F)
  
}
##########################################################################################
###################################CALIFORNIA CURRENT###################################

#there are 36 files in the AK folder:3 states X 4 industries X 3 indicators
CCfiles<- list.files(path = "Bureau of Labor Statistics/California Current/", pattern = "*.xlsx", full.names = T)

#state vector--nested for loop
CCstates<-c('CA', 'OR', 'WA')

for (j in 1:length(CCstates)) {
  for (i in 1:length(industries)) {
    CCfiles<- list.files(path = "Bureau of Labor Statistics/California Current/", pattern = paste(CCstates[j],industries[i],"*"), full.names = T)
    #list all AK files of i industry--all 3 inds; loop will cycle through all 4 industries in the order listed in 'industries' vector
    
    #employment first (alphabetical order)
    emp<-read_excel(CCfiles[1], skip = 13)
    emp<-emp[,c(1,14)]
    names(emp)<-c('Year','Employment')
    
    #establishments
    est<-read_excel(CCfiles[2], skip = 13)
    est<-est[,c(1,6)]
    names(est)<-c('Year','Establishments')
    
    #wages
    wag<-read_excel(CCfiles[3], skip = 13)
    wag<-wag[,c(1,6)]
    names(wag)<-c('Year','Wages.Thousands')
    
    #join all three variables
    df0<-join(emp,est, by = 'Year', type = 'inner')
    df<-join(df0,wag, by = 'Year', type = 'inner')
    
    df$Industry<-paste(industries[i]) #make 'Industry' column
    df$State<-paste(CCstates[j]) #make 'State' column
    df<-df[,c(1,6,5,2,3,4)]
    df$Employment.Tot<-df$Employment 
    #total employment is Employment (per establishment) * Establishments
    
    df$Wages.Tot.Millions<-(df$Wages.Thousands)/1000 
    #total wages (in millions--scaled to be comparable to other values) is Wages (per establishment)* Establishments
    
    write.csv(df,paste('Bureau of Labor Statistics/California Current/cleaned/',CCstates[j],'BLS',industries[i],".csv"), row.names = F)
    
  }
}

#join 3 states to make one file for each industry in the region

for (i in 1:length(industries)) {
  CC<- list.files(path = "Bureau of Labor Statistics/California Current/cleaned", pattern = paste("*",industries[i],".csv"), full.names = T)
  #list all AK files of i industry--all 3 inds; loop will cycle through all 4 industries in the order listed in 'industries' vector
  
  #CA first (alphabetical order)
  CA<-read.csv(CC[1])
  
  #establishments
  OR<-read.csv(CC[2])
  
  #wages
  WA<-read.csv(CC[3])
  
  #join all three variable
  CCdata<-rbind(CA,OR,WA)

  write.csv(CCdata,paste('Bureau of Labor Statistics/California Current/cleaned/Cali Current',industries[i],".csv"), row.names = F)
  
}
##########################################################################################
################################NORTHEAST###########################################

#2 folders, split by subregions Mid Atlantic and New England
#so we can append the subregion to all files in that folder
#then join later with subregion as a column/factor

#####MID ATLANTIC
#loop won't work for delaware because seaf packaging only has Establishments data, and not the other 2 variables
#do delaware separately

##DELAWARE##

DEpkg<-read_excel('Bureau of Labor Statistics/Northeast/Mid Atlantic/DE Seaf Packaging Establishments.xlsx', skip = 13)
DEpkg<-DEpkg[,c(1,6)]
names(DEpkg)<-c('Year','Establishments')

#make NA columns for the missing data so dfs can be joined later
DEpkg$Employment<-NA
DEpkg$Wages.Thousands<-NA
DEpkg$Employment.Tot<-NA
DEpkg$Wages.Tot.Millions<-NA

DEpkg$Industry<-"Seaf Packaging" #make 'Industry' column
DEpkg$State<-"DE" #make 'State' column
DEpkg$Subregion<-"Mid Atlantic"
DEpkg<-DEpkg[,c(1,9,8,7,3,2,4,5,6)]

write.csv(DEpkg,'Bureau of Labor Statistics/Northeast/Mid Atlantic/cleaned/ DE BLS Seaf Packaging .csv', row.names = F)
#the paste() fxn inserts spaces--i inserted a space before DE and after Packaging to make the alphabetical order consistent


#use loop for other 3 industries
DEindustries<-c('Fishing', 'Seaf Markets', 'Seaf Wholesale')

for (i in 1:length(DEindustries)) {
  DEfiles<- list.files(path = "Bureau of Labor Statistics/Northeast/Mid Atlantic", pattern = paste("DE",DEindustries[i],"*"), full.names = T)
  #list all AK files of i industry--all 3 inds; loop will cycle through all 4 industries in the order listed in 'industries' vector
  
  #employment first (alphabetical order)
  emp<-read_excel(DEfiles[1], skip = 13)
  emp<-emp[,c("Year","Annual")]
  names(emp)<-c('Year','Employment')
  
  #establishments
  est<-read_excel(DEfiles[2], skip = 13)
  est<-est[,c(1,6)]
  names(est)<-c('Year','Establishments')
  
  #wages
  wag<-read_excel(DEfiles[3], skip = 13)
  wag<-wag[,c(1,6)]
  names(wag)<-c('Year','Wages.Thousands')
  
  #join all three variables
  df0<-join(emp,est, by = 'Year', type = 'inner')
  df<-join(df0,wag, by = 'Year', type = 'inner')
  
  df$Industry<-paste(DEindustries[i]) #make 'Industry' column
  df$State<-"DE" #make 'State' column
  df$Subregion<-'Mid Atlantic'
  df<-df[,c(1,7,6,5,2,3,4)]
  df$Employment.Tot<-df$Employment 
  #total employment is Employment (per establishment) * Establishments
  
  df$Wages.Tot.Millions<-(df$Wages.Thousands)/1000 
  #total wages (in millions--scaled to be comparable to other values) is Wages (per establishment)* Establishments
  
  write.csv(df,paste('Bureau of Labor Statistics/Northeast/Mid Atlantic/cleaned/ DE BLS',DEindustries[i],".csv"), row.names = F)
  
}
################
#rest of Mid Atl states
#there are 70 files in the AK folder
#-DE is 60:5 states X 4 industries X 3 indicators
MAfiles<- list.files(path = "Bureau of Labor Statistics/Northeast/Mid Atlantic", pattern = "*.xlsx", full.names = T)

#state vector--nested for loop
MAstates<-c('MD', 'NC', 'NJ', 'NY', 'VA')

for (j in 1:length(MAstates)) {
  for (i in 1:length(industries)) {
    MAfiles<- list.files(path = "Bureau of Labor Statistics/Northeast/Mid Atlantic", pattern = paste(MAstates[j],industries[i],"*"), full.names = T)
    #list all AK files of i industry--all 3 inds; loop will cycle through all 4 industries in the order listed in 'industries' vector
    
    #employment first (alphabetical order)
    emp<-read_excel(MAfiles[1], skip = 13)
    emp<-emp[,c(1,14)]
    names(emp)<-c('Year','Employment')
    
    #establishments
    est<-read_excel(MAfiles[2], skip = 13)
    est<-est[,c(1,6)]
    names(est)<-c('Year','Establishments')
    
    #wages
    wag<-read_excel(MAfiles[3], skip = 13)
    wag<-wag[,c(1,6)]
    names(wag)<-c('Year','Wages.Thousands')
    
    #join all three variables
    df0<-join(emp,est, by = 'Year', type = 'inner')
    df<-join(df0,wag, by = 'Year', type = 'inner')
    
    df$Industry<-paste(industries[i]) #make 'Industry' column
    df$State<-paste(MAstates[j]) #make 'State' column
    df$Subregion<-'Mid Atlantic'
    df<-df[,c(1,7,6,5,2,3,4)]
    df$Employment.Tot<-df$Employment 
    #total employment is Employment (per establishment) * Establishments
    
    df$Wages.Tot.Millions<-(df$Wages.Thousands)/1000 
    #total wages (in millions--scaled to be comparable to other values) is Wages (per establishment)* Establishments
    
    write.csv(df,paste('Bureau of Labor Statistics/Northeast/Mid Atlantic/cleaned/',MAstates[j],'BLS',industries[i],".csv"), row.names = F)
    
  }
}

#join 6 states to make one file for each industry in the region

for (i in 1:length(industries)) {
  MA<- list.files(path = "Bureau of Labor Statistics/Northeast/Mid Atlantic/cleaned", pattern = paste("*",industries[i],".csv"), full.names = T)
  #list all AK files of i industry--all 3 inds; loop will cycle through all 4 industries in the order listed in 'industries' vector
  
  #DE first (alphabetical order)
  DE<-read.csv(MA[1])
  MD<-read.csv(MA[2])
  NC<-read.csv(MA[3])
  NJ<-read.csv(MA[4])
  NY<-read.csv(MA[5])
  VA<-read.csv(MA[6])
  
  #join all three variable
  MAdata<-rbind(DE,MD,NC,NJ,NY,VA)
  
  write.csv(MAdata,paste('Bureau of Labor Statistics/Northeast/Mid Atlantic/cleaned/Mid Atlantic',industries[i],".csv"), row.names = F)
  
}
##########################################################################################
####NEW ENGLAND####
#there are 60 files in the NE folder: 5 states X 4 industries X 3 indicators
NEfiles<- list.files(path = "Bureau of Labor Statistics/Northeast/New England/", pattern = "*.xlsx", full.names = T)

#state vector--nested for loop
NEstates<-c('CT', 'MA', 'ME', 'NH', 'RI')

for (j in 1:length(NEstates)) {
  for (i in 1:length(industries)) {
    NEfiles<- list.files(path = "Bureau of Labor Statistics/Northeast/New England/", pattern = paste(NEstates[j],industries[i],"*"), full.names = T)
    #list all AK files of i industry--all 3 inds; loop will cycle through all 4 industries in the order listed in 'industries' vector
    
    #employment first (alphabetical order)
    emp<-read_excel(NEfiles[1], skip = 13)
    emp<-emp[,c(1,14)]
    names(emp)<-c('Year','Employment')
    
    #establishments
    est<-read_excel(NEfiles[2], skip = 13)
    est<-est[,c(1,6)]
    names(est)<-c('Year','Establishments')
    
    #wages
    wag<-read_excel(NEfiles[3], skip = 13)
    wag<-wag[,c(1,6)]
    names(wag)<-c('Year','Wages.Thousands')
    
    #join all three variables
    df0<-join(emp,est, by = 'Year', type = 'inner')
    df<-join(df0,wag, by = 'Year', type = 'inner')
    
    df$Industry<-paste(industries[i]) #make 'Industry' column
    df$State<-paste(NEstates[j]) #make 'State' column
    df$Subregion<-'New England'
    df<-df[,c(1,7,6,5,2,3,4)]
    df$Employment.Tot<-df$Employment 
    #total employment is Employment (per establishment) * Establishments
    
    df$Wages.Tot.Millions<-(df$Wages.Thousands)/1000 
    #total wages (in millions--scaled to be comparable to other values) is Wages (per establishment)* Establishments
    
    write.csv(df,paste('Bureau of Labor Statistics/Northeast/New England/cleaned/',NEstates[j],'BLS',industries[i],".csv"), row.names = F)
    
  }
}

#join 5 states to make one file for each industry in the region

for (i in 1:length(industries)) {
  NE<- list.files(path = "Bureau of Labor Statistics/Northeast/New England/cleaned", pattern = paste("*",industries[i],".csv"), full.names = T)
  #list all AK files of i industry--all 3 inds; loop will cycle through all 4 industries in the order listed in 'industries' vector
  
  #CT first (alphabetical order)
  CT<-read.csv(NE[1])
  MA<-read.csv(NE[2])
  ME<-read.csv(NE[3])
  NH<-read.csv(NE[4])
  RI<-read.csv(NE[5])
  
  #join all three variable
  NEdata<-rbind(CT,MA,ME,NH,RI)
  
  write.csv(NEdata,paste('Bureau of Labor Statistics/Northeast/New England/cleaned/New England',industries[i],".csv"), row.names = F)
  
}
##########################################################################################
#################################SOUTHEAST######################################
#FL split into East (Southeast Region) and West (Gulf of Mexico Region)
#downloaded FL BLS date by individual coastal county
#first, combine all E FL counties to get totals for East FL

###EAST FLORIDA
EFLfiles<- list.files(path = "Bureau of Labor Statistics/Southeast/Atlantic", pattern = "*.xlsx", full.names = T)
#there are 109 files in the EFL folder: many counties did not have seaf packaging data
#establishments also tended to have more data than employment or wages
#will combine the files by industry and indicator instead of by county since not all counties have all 3 indicators

for (j in 1:length(inds)) {
  for (i in 1:length(industries)) {
    EFLfiles<- list.files(path = "Bureau of Labor Statistics/Southeast/Atlantic", 
                          pattern = paste("*",industries[i]," ",inds[j],".xlsx", sep = ''), #sep ='' means no spaces will be inserted--had to manually insert one between industry and ind with " "
                          full.names = T)
    #list all EFL files of i industry--within all 3 inds; 
    #nested loop will cycle through all 4 industries in the order listed in 'industries' vector
    #and then repeat for each ind
    
    #this 3rd loop reads in the files one industry and one indicator at a time,
    #gets rid of the extra rows and columns and renames the indicator column
    #and writes them as cleaned csv files
    for (k in 1:length(EFLfiles)) {
      efl<-read_excel(EFLfiles[k], skip = 13)
      efl<-efl[,c('Year', 'Annual')]
      names(efl)<-c('Year',paste(inds[j]))
      write.csv(efl, paste('Bureau of Labor Statistics/Southeast/Atlantic/cleaned/EFL ',industries[i],' ',inds[j],' County ',k,'.csv', sep=''), row.names = F)
    }
    
    #then we read in the cleaned files to sum them together for one Totals file for each indicator within an industry
    EFLfiles1<- list.files(path = "Bureau of Labor Statistics/Southeast/Atlantic/cleaned", 
                          pattern = paste('*EFL ',industries[i],' ',inds[j],'*', sep=''), #sep ='' means no spaces will be inserted--had to manually insert one between industry and ind with " "
                          full.names = T)
    
    #join all counties in one df
    df0 <- ldply(EFLfiles1, read.csv) #reads in all files and stacks them into one df
    df0<-df0[,c('Year',paste(inds[j]))] #just keep Year & Ind column--sometimes not a problem; sometimes an 'X' column with row labels appears that we want to get rid of
    df0[is.na(df0)] <- 0 #aggregate() function deletes NAs--which may omit some years if all are NA
    df<-aggregate(.~Year, data = df0, FUN = sum) #sum indicator values by Year
    df[df==0] <- NA #return the artificial 0s to NAs
    write.csv(df, paste('Bureau of Labor Statistics/Southeast/Atlantic/cleaned/EFL ',industries[i],' Totals ',inds[j],'.csv', sep=''), row.names = F)
    #we write each cleaned Totals file
  }
}


for (j in 1:length(inds)) {
  for (i in 1:length(industries)) {
    #last, we put the Totals per indicator files together so we get a complete file per industry with all 3 indicators
    EFLindTotals<- list.files(path = "Bureau of Labor Statistics/Southeast/Atlantic/cleaned", 
                           pattern = paste('*',industries[i],' Totals*', sep=''), #sep ='' means no spaces will be inserted--had to manually insert one between industry and ind with " "
                           full.names = T)

    #employment first (alphabetical order)
    emp<-read.csv(EFLindTotals[1])
    emp<-emp[,c('Year', 'Employment')]
    
    #establishments
    est<-read.csv(EFLindTotals[2])
    est<-est[,c('Year', 'Establishments')]
    
    #wages
    wag<-read.csv(EFLindTotals[3])
    wag<-wag[,c('Year', 'Wages')]
    names(wag)<-c('Year','Wages.Thousands')
    
    #join all three variables
    tot0<-join(emp,est, by = 'Year', type = 'inner')
    tot<-join(tot0,wag, by = 'Year', type = 'inner')
    
    
    tot$Industry<-paste(industries[i]) #make 'Industry' column
    tot$State<-"EFL" #make 'State' column
    tot<-tot[,c(1,6,5,2,3,4)]
    tot$Employment.Tot<-tot$Employment*tot$Establishments 
    #total employment is Employment (per establishment) * Establishments
    
    tot$Wages.Tot.Millions<-(tot$Wages.Thousands*tot$Establishments)/1000 
    #total wages (in millions--scaled to be comparable to other values) is Wages (per establishment)* Establishments
    
    write.csv(tot,paste('Bureau of Labor Statistics/Southeast/cleaned/ EFL BLS',industries[i],".csv"), row.names = F)
    #write with space in front of EFL for consistency with other file names
  }
}

#########first clean rest of the states
SEfiles<- list.files(path = "Bureau of Labor Statistics/Southeast", pattern = "*.xlsx", full.names = T)
#there are 24 files in the SE folder:2 states X 4 industries X 3 indicators
#plus EFL--already has 4 files (one per industry) in the 'Southeast/cleaned folder

#state vector--nested for loop
SEstates<-c('GA','SC')

for (j in 1:length(SEstates)) {
  for (i in 1:length(industries)) {
    SEfiles<- list.files(path = "Bureau of Labor Statistics/Southeast", pattern = paste(SEstates[j],industries[i],"*"), full.names = T)
    #list all AK files of i industry--all 3 inds; loop will cycle through all 4 industries in the order listed in 'industries' vector
    
    #employment first (alphabetical order)
    emp<-read_excel(SEfiles[1], skip = 13)
    emp<-emp[,c(1,14)]
    names(emp)<-c('Year','Employment')
    
    #establishments
    est<-read_excel(SEfiles[2], skip = 13)
    est<-est[,c(1,6)]
    names(est)<-c('Year','Establishments')
    
    #wages
    wag<-read_excel(SEfiles[3], skip = 13)
    wag<-wag[,c(1,6)]
    names(wag)<-c('Year','Wages.Thousands')
    
    #join all three variables
    df0<-join(emp,est, by = 'Year', type = 'inner')
    df<-join(df0,wag, by = 'Year', type = 'inner')
    
    df$Industry<-paste(industries[i]) #make 'Industry' column
    df$State<-paste(SEstates[j]) #make 'State' column
    df<-df[,c(1,6,5,2,3,4)]
    df$Employment.Tot<-df$Employment 
    #total employment is Employment (per establishment) * Establishments
    
    df$Wages.Tot.Millions<-(df$Wages.Thousands)/1000 
    #total wages (in millions--scaled to be comparable to other values) is Wages (per establishment)* Establishments
    
    write.csv(df,paste('Bureau of Labor Statistics/Southeast/cleaned/',SEstates[j],'BLS',industries[i],".csv"), row.names = F)
    
  }
}

#join 3 states to make one file for each industry in the region

for (i in 1:length(industries)) {
  SE<- list.files(path = "Bureau of Labor Statistics/Southeast/cleaned", pattern = paste("*",industries[i],".csv"), full.names = T)
  #list all SE files of i industry--all 3 inds; loop will cycle through all 4 industries in the order listed in 'industries' vector
  
  #CA first (alphabetical order)
  EFL<-read.csv(SE[1])
  
  #establishments
  GA<-read.csv(SE[2])
  
  #wages
  SC<-read.csv(SE[3])
  
  #join all three variable
  SEdata<-rbind(EFL,GA,SC)
  
  write.csv(SEdata,paste('Bureau of Labor Statistics/Southeast/cleaned/Southeast',industries[i],".csv"), row.names = F)
  
}
##########################################################################################
##############################GULF OF MEXICO###########################################
#FL split into East (Southeast Region) and West (Gulf of Mexico Region)
#downloaded FL BLS date by individual coastal county
#first, combine all W FL counties to get totals for West FL

###WEST FLORIDA
WFLfiles<- list.files(path = "Bureau of Labor Statistics/Gulf of Mexico/Gulf", pattern = "*.xlsx", full.names = T)
#there are 194 files in the WFL folder: many counties did not have seaf packaging data
#establishments also tended to have more data than employment or wages
#will combine the files by industry and indicator instead of by county since not all counties have all 3 indicators

for (j in 1:length(inds)) {
  for (i in 1:length(industries)) {
    WFLfiles<- list.files(path = "Bureau of Labor Statistics/Gulf of Mexico/Gulf", 
                          pattern = paste("*",industries[i]," ",inds[j],".xlsx", sep = ''), #sep ='' means no spaces will be inserted--had to manually insert one between industry and ind with " "
                          full.names = T)
    #list all WFL files of i industry--within all 3 inds; 
    #nested loop will cycle through all 4 industries in the order listed in 'industries' vector
    #and then repeat for each ind
    
    #this 3rd loop reads in the files one industry and one indicator at a time,
    #gets rid of the extra rows and columns and renames the indicator column
    #and writes them as cleaned csv files
    for (k in 1:length(WFLfiles)) {
      wfl<-read_excel(WFLfiles[k], skip = 13)
      wfl<-wfl[,c('Year', 'Annual')]
      names(wfl)<-c('Year',paste(inds[j]))
      write.csv(wfl, paste('Bureau of Labor Statistics/Gulf of Mexico/Gulf/cleaned/WFL ',industries[i],' ',inds[j],' County ',k,'.csv', sep=''), row.names = F)
    }
    
    #then we read in the cleaned files to sum them together for one Totals file for each indicator within an industry
    WFLfiles1<- list.files(path = "Bureau of Labor Statistics/Gulf of Mexico/Gulf/cleaned", 
                           pattern = paste('*WFL ',industries[i],' ',inds[j],'*', sep=''), #sep ='' means no spaces will be inserted--had to manually insert one between industry and ind with " "
                           full.names = T)
    
    #join all counties in one df
    df0 <- ldply(WFLfiles1, read.csv) #reads in all files and stacks them into one df
    df0<-df0[,c('Year',paste(inds[j]))] #just keep Year & Ind column--sometimes not a problem; sometimes an 'X' column with row labels appears that we want to get rid of
    df0[is.na(df0)] <- 0 #aggregate() function deletes NAs--which may omit some years if all are NA
    df<-aggregate(.~Year, data = df0, FUN = sum) #sum indicator values by Year
    df[df==0] <- NA #return the artificial 0s to NAs
    write.csv(df, paste('Bureau of Labor Statistics/Gulf of Mexico/Gulf/cleaned/WFL ',industries[i],' Totals ',inds[j],'.csv', sep=''), row.names = F)
    #we write each cleaned Totals file
    
  }
}


for (j in 1:length(inds)) {
  for (i in 1:length(industries)) {
    #last, we put the Totals per indicator files together so we get a complete file per industry with all 3 indicators
    WFLindTotals<- list.files(path = "Bureau of Labor Statistics/Gulf of Mexico/Gulf/cleaned", 
                              pattern = paste('*',industries[i],' Totals*', sep=''), #sep ='' means no spaces will be inserted--had to manually insert one between industry and ind with " "
                              full.names = T)
    
    #employment first (alphabetical order)
    emp<-read.csv(WFLindTotals[1])
    emp<-emp[,c('Year', 'Employment')]
    
    #establishments
    est<-read.csv(WFLindTotals[2])
    est<-est[,c('Year', 'Establishments')]
    
    #wages
    wag<-read.csv(WFLindTotals[3])
    wag<-wag[,c('Year', 'Wages')]
    names(wag)<-c('Year','Wages.Thousands')
    
    #join all three variables
    tot0<-join(emp,est, by = 'Year', type = 'inner')
    tot<-join(tot0,wag, by = 'Year', type = 'inner')
    
    
    tot$Industry<-paste(industries[i]) #make 'Industry' column
    tot$State<-"WFL" #make 'State' column
    tot<-tot[,c(1,6,5,2,3,4)]
    tot$Employment.Tot<-tot$Employment*tot$Establishments 
    #total employment is Employment (per establishment) * Establishments
    
    tot$Wages.Tot.Millions<-(tot$Wages.Thousands*tot$Establishments)/1000 
    #total wages (in millions--scaled to be comparable to other values) is Wages (per establishment)* Establishments
    
    write.csv(tot,paste('Bureau of Labor Statistics/Gulf of Mexico/cleaned/ WFL BLS',industries[i],".csv"), row.names = F)
    #write with space in front of WFL for consistency with other file names
  }
}

#########first clean rest of the states
GOMfiles<- list.files(path = "Bureau of Labor Statistics/Gulf of Mexico", pattern = "*.xlsx", full.names = T)
#there are 48 files in the GOM folder:4 states X 4 industries X 3 indicators
#plus WFL--already has 4 files (one per industry) in the 'Gulf of Mexico/cleaned folder

#state vector--nested for loop
GOMstates<-c('AL','LA','MS','TX')

for (j in 1:length(GOMstates)) {
  for (i in 1:length(industries)) {
    GOMfiles<- list.files(path = "Bureau of Labor Statistics/Gulf of Mexico", pattern = paste(GOMstates[j],industries[i],"*"), full.names = T)
    #list all GOM files of i industry--all 3 inds; loop will cycle through all 4 industries in the order listed in 'industries' vector
    
    #employment first (alphabetical order)
    emp<-read_excel(GOMfiles[1], skip = 13)
    emp<-emp[,c(1,14)]
    names(emp)<-c('Year','Employment')
    
    #establishments
    est<-read_excel(GOMfiles[2], skip = 13)
    est<-est[,c(1,6)]
    names(est)<-c('Year','Establishments')
    
    #wages
    wag<-read_excel(GOMfiles[3], skip = 13)
    wag<-wag[,c(1,6)]
    names(wag)<-c('Year','Wages.Thousands')
    
    #join all three variables
    df0<-join(emp,est, by = 'Year', type = 'inner')
    df<-join(df0,wag, by = 'Year', type = 'inner')
    
    df$Industry<-paste(industries[i]) #make 'Industry' column
    df$State<-paste(GOMstates[j]) #make 'State' column
    df<-df[,c(1,6,5,2,3,4)]
    df$Employment.Tot<-df$Employment 
    #total employment is Employment (per establishment) * Establishments
    
    df$Wages.Tot.Millions<-(df$Wages.Thousands)/1000 
    #total wages (in millions--scaled to be comparable to other values) is Wages (per establishment)* Establishments
    
    write.csv(df,paste('Bureau of Labor Statistics/Gulf of Mexico/cleaned/',GOMstates[j],'BLS',industries[i],".csv"), row.names = F)
    
  }
}

#join 3 states to make one file for each industry in the region

for (i in 1:length(industries)) {
  GOM<- list.files(path = "Bureau of Labor Statistics/Gulf of Mexico/cleaned", pattern = paste("*",industries[i],".csv"), full.names = T)
  #list all GOM files of i industry--all 3 inds; loop will cycle through all 4 industries in the order listed in 'industries' vector
  
  #CA first (alphabetical order)
  AL<-read.csv(GOM[1])
  LA<-read.csv(GOM[2])
  MS<-read.csv(GOM[3])
  TX<-read.csv(GOM[4])
  WFL<-read.csv(GOM[5])
  
  #join all three variable
  GOMdata<-rbind(AL,LA,MS,TX,WFL)
  
  write.csv(GOMdata,paste('Bureau of Labor Statistics/Gulf of Mexico/cleaned/Gulf of Mexico',industries[i],".csv"), row.names = F)
  
}



##########################################################################################

#NES (non-employer statistics)
#1997-2018

NESfiles<- list.files(path = "Non Employer Statistics", pattern = "*.xlsx", full.names = T)
#list all NES files: 22 files for 22 yrs

states<-c('AL','AK','CA','CT','DE','FL', 'GA',
          'HI','LA','ME','MD','MA','MS',
          'NH','NJ','NY','NC','OR',
          'RI','SC','TX','VA','WA')
statecodes<-c(1,2,6,9,10,12,13,15,22,23,24,25,28,33,34,36,37,41,44,45,48,51,53)
#same length as states-->use j

#create years vector to add 'Year' column
yrs<-c(1997,1998,1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,
       2011,2012,2013,2014,2015,2016,2017,2018) #same length as NESfiles-->use i

for( i in 1:length(NESfiles)) {
  NES0<-read_excel(NESfiles[i])
  NES0<-NES0[,c('ST', 'NAICS', 'ESTAB', 'RCPTOT')]
  NES0[NES0=='D']<-NA #D is value for unreported-->replace with NA for consistency
  names(NES0)<-c('State', 'Industry', 'Establishments','Total.Receipts')
  
  for (j in 1:length(states)) {
      substate<-NES0[NES0$State==statecodes[j],]  #subset out target state
      
      ##subset & separate by 4 industry codes
      Fsh<-substate[substate$Industry=="1141",]  #Fishing
      Fsh$Year<-yrs[i]
      Fsh$State<-as.character(states[j])
      Fsh$Industry<-as.character('Fishing')
      Fsh<-Fsh[,c(5,1,2,3,4)]
      Fsh<-Fsh[1,] #just keep first row: some years had subtotals--first row is total
      write.csv(Fsh,paste('Non Employer Statistics/',states[j],' Fishing ',yrs[i],'.csv',sep =''), row.names = F)
      
      SP<-substate[substate$Industry == "3117",]  #Seafood Packaging
      SP$Year<-yrs[i]
      SP$State<-as.character(states[j])
      SP$Industry<-as.character('Seaf Packaging')
      SP<-SP[,c(5,1,2,3,4)]
      SP<-SP[1,] #just keep first row: some years had subtotals--first row is total
      write.csv(SP,paste('Non Employer Statistics/',states[j],' Seaf Packaging ',yrs[i],'.csv',sep =''), row.names = F)
      
      #SW<-substate[substate$Industry == "42446",]  #Seafood Wholesalers--no Seaf Wholesale in NES
      SM<-substate[substate$Industry == "44522",] #Seafood Markets
      SM$Year<-yrs[i]
      SM$State<-as.character(states[j])
      SM$Industry<-as.character('Seaf Markets')
      SM<-SM[,c(5,1,2,3,4)]
      SM<-SM[1,] #just keep first row: some years had subtotals--first row is total
      write.csv(SM,paste('Non Employer Statistics/',states[j],' Seaf Markets ',yrs[i],'.csv',sep =''), row.names = F)
      
  }
}

NESindus<-c('Fishing', 'Seaf Markets', 'Seaf Packaging')

for (j in 1:length(states)) {
  for(l in 1:length(NESindus)) {
    statefiles<- list.files(path = "Non Employer Statistics", pattern = paste(states[j],' ',NESindus[l],"*",sep =''), full.names = T)
    #list all NES files: 22 files for 22 yrs
    df <- ldply(statefiles, read.csv) #reads in all files and stacks them into one df
    df[df==0]<-NA #RCPTOT and ESTAB listen as 0 when unreported-->replace with NA for consistency
    write.csv(df,paste('Non Employer Statistics/cleaned/',states[j],' NES ',NESindus[l],'.csv',sep =''), row.names = F)
  }
}







