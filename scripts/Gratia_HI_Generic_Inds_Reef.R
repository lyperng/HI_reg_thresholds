####### Sept 2020####
###code by Lansing Perng
##edit GAM code to pull out reef-associated fish only
#more directly comparable to dryad data

rm(list=ls())

library(gratia)
library(mgcv)
library(tidyr)
library(plyr)
library(ggplot2)

##############################load and clean data##############################
######################ecological data from dryad

#1993-2016
dryad<-read.csv('data/Hawaii_regime_timeseries_.csv')

###README
#1- repID_ts - a unique identifier for each of 1279 replicates
#2- Browsers - browser biomass (grams per meter squared)
#3- Grazers - grazer biomass (grams per meter squared)
#4- Scrapers - scraper biomass (grams per meter squared)
#5- Predators - predator biomass (grams per meter squared)
#6- SecConsumers - secondary consumer biomass (grams per meter squared)
#7- Coral - coral cover (%)
#8- Macro - macroalgal cover (%)
#9- Other - other benthic cover (%)
#10- Turf - turf algal cover (%)
#11- CCA - coralline algal cover (%)

dryad<- dryad[,2:12] #delete rep ID and lat/long because for this--we just need annual state averages
dryad$Herbivores<-rowSums(dryad[,2:4])
dryad$Calcifiers<-rowSums(dryad[,c(7,11)])
dryad$Algae<-rowSums(dryad[,c(8,10)])
dryad2<-aggregate(. ~ Year, data=dryad, mean, na.rm=TRUE) #aggregate by mean because all values are proportions, not counts
write.csv(dryad2,'data/HCC Cleaned/dryad annual averages.csv', row.names = F)

#dryad2<-dryad2[,c(1,5,7,8)]
dryad2<-read.csv('data/HCC cleaned/dryad annual averages.csv')

#############################################
#commercial landings, commercial catch diversity, commercial revenue 
#1981-2019
#split in 'Shannon_Diversity_Calculation.R'
FOSS<-read.csv('data/HCC cleaned/FOSS reef fish Pounds and Diversity.csv') 

FOSS$Revenue.Millions<-round(FOSS$Dollars/1000000, digits = 3)
FOSS$Catch.Millions<-round(FOSS$Tot_Comm/1000000, digits = 3)
FOSS1<-FOSS[,-c(2,3)] #delete by pounds and by dollars since we have millions columns

write.csv(FOSS1, 'data/HCC cleaned/FOSS reef fish with Millions.csv', row.names = F)

FOSS.r_dryad<-join(dryad2, FOSS1, by = 'Year', type = 'inner')
#only 18 of data years left-- see if it's enough for GAM analyses
#if not, may have to creatively pad the dryad data

write.csv(FOSS.r_dryad, 'data/HCC cleaned/FOSS reef dryad.csv', row.names = F)

########same for pelagics
FOSS<-read.csv('data/HCC cleaned/FOSS pelagic fish Pounds and Diversity.csv') 

FOSS$Revenue.Millions<-round(FOSS$Dollars/1000000, digits = 3)
FOSS$Catch.Millions<-round(FOSS$Tot_Comm/1000000, digits = 3)
FOSS1<-FOSS[,-c(2,3)] #delete by pounds and by dollars since we have millions columns

write.csv(FOSS1, 'data/HCC cleaned/FOSS pelagic fish with Millions.csv', row.names = F)

########same for deep
FOSS<-read.csv('data/HCC cleaned/FOSS deep fish Pounds and Diversity.csv') 

FOSS$Revenue.Millions<-round(FOSS$Dollars/1000000, digits = 3)
FOSS$Catch.Millions<-round(FOSS$Tot_Comm/1000000, digits = 3)
FOSS1<-FOSS[,-c(2,3)] #delete by pounds and by dollars since we have millions columns

write.csv(FOSS1, 'data/HCC cleaned/FOSS deep fish with Millions.csv', row.names = F)


#############################################

#recreational landings (harvest + released)
#2003-2020
##KEEP this if inclusive 
#recland0<-read.csv('data/MRIP Rec Harvest Totals.csv')
#rec_HI<-recland0[recland0$Region=="HAWAIIAN ISLANDS",] #subset only the rows where Region column is exactly "HAWAIIAN ISLANDS"
#recland<-rec_HI[,c(2,5,7)]
#recland<-recland[-18,]
#names(recland)<- c('Year', "Harvest", "Released")
#recland$Harvest <-as.numeric(gsub(",","",recland$Harvest)) #Replace comma with nothing--aka delete commas
#recland$Released <-  as.numeric(gsub(",","",recland$Released)) #Replace comma with nothing--aka delete commas
#recland$RCatch<- rowSums(recland[,2:3]) #sum columns 2 and 3 to get total rec landings
#leave separate from FOSS for now because FOSS is longer term--better for GAMs

#2003-2020
recland0<-read.csv('data/MRIP_National_Harvest.csv')
rec_HI<-recland0[recland0$Region=="HAWAIIAN ISLANDS",] #subset only the rows where Region column is exactly "HAWAIIAN ISLANDS"
recland<-rec_HI[,c(2,4,5,7)]
#recland<-recland[-18,]
names(recland)<- c('Year','Species', "Harvest", "Released")
recland$Harvest <-as.numeric(gsub(",","",recland$Harvest)) #Replace comma with nothing--aka delete commas
recland$Released <-  as.numeric(gsub(",","",recland$Released)) #Replace comma with nothing--aka delete commas
recland$RCatch<- rowSums(recland[,3:4]) #sum columns 2 and 3 to get total rec landings
#leave separate from FOSS for now because FOSS is longer term--better for GAMs


species<-unique(recland$Species)
species

MRIP.spec.hab<-data.frame('Species' = species, 'Habitat' = NA)

#write.csv(MRIP.spec.hab, 'data/HCC cleaned/MRIP species habitat.csv', row.names = F)
#filled in habitat: reef, pelagic, or freshwater, in excel

MRIP.spec.hab<-read.csv('data/HCC cleaned/MRIP species habitat list.csv')
MRIP.spec.hab<-MRIP.spec.hab[,c(1,3)]

#add habitat to rec data by joining the species habitat list and the rec data
recland1<-join(recland, MRIP.spec.hab, by = "Species", type = 'inner')

########subset reef fish only
reef.recland<-recland1[recland1$Habitat == 'Reef',]
reef.recland<-reef.recland[,c(1,5)]
reef.recland<-aggregate(. ~ Year, data=reef.recland, sum, na.rm=TRUE) 
#aggregate by sum to get total annual recreation catch of reef fishes

########subset pelagic fish only
pel.recland<-recland1[recland1$Habitat == 'Pelagic',]
pel.recland<-pel.recland[,c(1,5)]
pel.recland<-aggregate(. ~ Year, data=pel.recland, sum, na.rm=TRUE) 
#aggregate by sum to get total annual recreation catch of pelagic fishes



#############################################

#angler trips (rec effort)
#2003-2020
angtrips<-read.csv('data/MRIP Angler Trips.csv')
ang_HI<-angtrips[angtrips$Region=="HAWAIIAN ISLANDS",] #subset only the rows where Region column is exactly "HAWAIIAN ISLANDS"
angtrips<-ang_HI[,c(2,4)]
angtrips$Angler.Trips <-as.numeric(gsub(",","",angtrips$Angler.Trips)) #Replace comma with nothing--aka delete commas

###species all inclusive
recr<-join(angtrips,recland, by = "Year", type = 'inner')
recr$Trips.Millions<-recr$Angler.Trips/1000000
recr$RCatch.Millions<-recr$RCatch/1000000
#leave separate from FOSS for now because FOSS is longer term--better for GAMs
write.csv(recr,'data/HCC Cleaned/Recreational Catch and Trips.csv', row.names = F)

##only reef fishes
rec.r<-join(angtrips,reef.recland, by = "Year", type = 'inner')
rec.r$Trips.Millions<-rec.r$Angler.Trips/1000000
rec.r$RCatch.Millions<-rec.r$RCatch/1000000
#leave separate from FOSS for now because FOSS is longer term--better for GAMs
write.csv(rec.r,'data/HCC Cleaned/Reef Recreational Catch and Trips.csv', row.names = F)

##only pelagic fishes
rec.p<-join(angtrips,pel.recland, by = "Year", type = 'inner')
rec.p$Trips.Millions<-rec.p$Angler.Trips/1000000
rec.p$RCatch.Millions<-rec.p$RCatch/1000000
#leave separate from FOSS for now because FOSS is longer term--better for GAMs
write.csv(rec.p,'data/HCC Cleaned/Pelagic Recreational Catch and Trips.csv', row.names = F)

#species inslusive
rec_dryad<-join(dryad2, recr, by = 'Year', type = 'inner')
#only 14 of data years left-- see if it's enough for GAM analyses
#if not, may have to creatively pad the dryad data

write.csv(rec_dryad, 'data/HCC cleaned/Recreation dryad.csv', row.names = F)

#reef fishes & dryad
rec.r_dryad<-join(dryad2, rec.r, by = 'Year', type = 'inner')
#only 14 of data years left-- see if it's enough for GAM analyses
#if not, may have to creatively pad the dryad data

write.csv(rec.r_dryad, 'data/HCC cleaned/Reef Recreation dryad.csv', row.names = F)


 ###brief exploration of recreation
rec_mod<-lm(RCatch~Angler.Trips ,data = rec.r)
summary(rec_mod)
plot(RCatch~Angler.Trips ,data = rec.r)
plot(RCatch ~ Year, data = rec.r)
plot(Angler.Trips ~ Year, data = rec.r)
#############################################

#non-employer statistics
#1997-2018
NES0<-read.csv('data/NES_HI.csv')

#############data exploration to see if any initial patterns emerge###################
NES_Fishing<-NES0[NES0$Industry=="Fishing",] #subset only the rows where Industry column is exactly "Fishing"
NES_Markets<-NES0[NES0$Industry=="Seafood Markets",] #subset only the rows where Industry column is exactly "Fishing"

plot(NES0$Receipts..Thousands.of.Dollars.~NES0$Year) #industry lines are distinct
plot(NES_Fishing$Receipts..Thousands.of.Dollars.~NES_Fishing$Year) #somewhat pattern--nonlinear
plot(NES_Markets$Receipts..Thousands.of.Dollars.~NES_Markets$Year) #pretty linear

plot(NES0$Establishments~NES0$Year) #industry lines are distinct
plot(NES_Fishing$Establishments~NES_Fishing$Year) #somewhat pattern--nonlinear
plot(NES_Markets$Establishments~NES_Markets$Year) #pretty linear

####################################################################################

NES<-NES0[,-2]
NES<-aggregate(.~Year, data = NES, FUN = sum) #join by Year (summed across the 2 industries)
names(NES)[3]<- 'Receipts.Thousands'
NES$Receipts.Millions<-NES$Receipts.Thousands/1000

plot(NES$Receipts.Thousands~NES$Year) #driven way more by fishing than markets
plot(NES$Establishments~NES$Year) #driven way more by fishing than markets

write.csv(NES,'data/HCC Cleaned/NES Establishments and Receipts.csv', row.names = F)

NES_dryad<-join(dryad2, NES, by = 'Year', type = 'inner')
#only 17 of data years left-- see if it's enough for GAM analyses
#if not, may have to creatively pad the dryad data

write.csv(NES_dryad, 'data/HCC cleaned/NES dryad.csv', row.names = F)

###### BLS bureau of labor statistics
#2001-2019
BLS<-read.csv('data/HI_BLS.csv')
names(BLS)<- c("Year",  "Industry", "EmploymentPerEstablishment", "WagesPerEstablishment.Thousands", "Annual.Pay", "Establishments")
BLS$Total.Employment <- BLS$EmploymentPerEstablishment*BLS$Establishments
BLS$Total.Wages.Millions <- BLS$WagesPerEstablishment.Thousands*BLS$Establishments/1000

#############data exploration to see if any initial patterns emerge###################
BLS_Fishing<-BLS[BLS$Industry=="Fishing",] #subset only the rows where Industry column is exactly "Fishing"
BLS_Markets<-BLS[BLS$Industry=="Seafood Markets",] #subset only the rows where Industry column is exactly "Fishing"
BLS_Whole<-BLS[BLS$Industry=="Seafood Wholesalers",] #subset only the rows where Industry column is exactly "Fishing"

BLS_Markets1<-BLS_Markets[,-2]
write.csv(BLS_Markets1, 'data/HCC cleaned/BLS Markets.csv', row.names = F)

BLS_Whole1<-BLS_Whole[,-2]
write.csv(BLS_Whole1, 'data/HCC cleaned/BLS Wholesalers.csv', row.names = F)

plot(BLS$Annual.Pay~BLS$Year) #industry lines are distinct
plot(BLS_Fishing$Annual.Pay~BLS_Fishing$Year) #somewhat pattern--nonlinear
plot(BLS_Markets$Annual.Pay~BLS_Markets$Year) #pretty linear
plot(BLS_Whole$Annual.Pay~BLS_Whole$Year) #nonlinear pattern or linear--with 1st 3 pts outliers

plot(BLS$Establishments~BLS$Year) #industry lines are distinct
plot(BLS_Fishing$Establishments~BLS_Fishing$Year) #nonlinear pattern--decrease
plot(BLS_Markets$Establishments~BLS_Markets$Year) #linear pattern--decrease
plot(BLS_Whole$Establishments~BLS_Whole$Year) #not much of a trend--1st 3 look like outliers

plot(BLS$Total.Employment~BLS$Year) #no clear patter--industry lines mixed up--much clearer pattern when separated
plot(BLS_Fishing$Total.Employment~BLS_Fishing$Year) #nonlinear pattern--decrease, possible outlier for 2001
plot(BLS_Markets$Total.Employment~BLS_Markets$Year) #nonlinear pattern--decrease
plot(BLS_Whole$Total.Employment~BLS_Whole$Year) #increasing trend if first 3 are outliers

plot(BLS$Total.Wages.Millions~BLS$Year) #industry lines distinct
plot(BLS_Fishing$Total.Wages.Millions~BLS_Fishing$Year) #decrease, possible outlier for 2001
plot(BLS_Markets$Total.Employment~BLS_Markets$Year) #nonlinear pattern--decrease
plot(BLS_Whole$Total.Wages.Millions~BLS_Whole$Year) #increase, 1st 3 yrs diff

#distinct patterns across industries--maybe best not to pool
#wholesalers--possible threshold after first 3 yrs?

#########################################################################

BLS1<-BLS[,-c(3,4)] #subtract per establishment employment and wages

#fishing industry has lots of NAs--see if we can fill them in to eventually join numbers with other industries
BLS_Fishing<-BLS1[BLS1$Industry=="Fishing",] #subset only the rows where Industry column is exactly "Fishing"

#there are fewer NAs in 'Establishments' --> 
#get relationship between Employment/Wages and Establishments
#then can fill in based on Establishment values provided
#then only 2 NAs left
fmod1<-lm(Total.Employment~Establishments, data = BLS_Fishing)
summary(fmod1) #very high adjusted R (0.9393)--should give reliable estimates of Employment based on Establishments
plot(Total.Employment~Establishments, data = BLS_Fishing)

new<-data.frame(Establishments = BLS_Fishing$Establishments)
Predict<-predict(fmod1, new, se.fit = TRUE) #predict Employment values based on number of Establishments
BLS_Fishing$Predict.Employment<-Predict$fit
BLS_Fishing$Total.Employment[c(4,7,8,15)]<-BLS_Fishing$Predict.Employment[c(4,7,8,15)] #fill in just the NAs with predicted values
BLS_Fishing$Total.Employment<-round(BLS_Fishing$Total.Employment, digits = 0) #round to nearest integer
BLS_Fishing<-BLS_Fishing[,-7] #subtract column 7 (Predict column)--need to do this so can bind with main dataset
plot(Total.Employment~Establishments, data = BLS_Fishing) #3 new points look good on the plot


#try to predict Wages based on employment
#fmod2<-lm(Total.Wages.Millions~Total.Employment, data = BLS_Fishing)
#summary(fmod2) #R^2 not very good (Adjusted = 0.5742)--not good enough for predictions
#plot(Total.Wages.Millions~Total.Employment, data = BLS_Fishing)

#try to predict Wages based on establishments
#fmod3<-lm(Total.Wages.Millions~Establishments, data = BLS_Fishing)
#summary(fmod3) #R^2 not very good (Adjusted = 0.4561)--don't deal with Wages for now
#plot(Total.Wages.Millions~Establishments, data = BLS_Fishing)

#try to predict Annual Pay based on establishments
#fmod4<-lm(Annual.Pay~Establishments, data = BLS_Fishing)
#summary(fmod4) #R^2 not very good (Adjusted = 0.4956)--not good enough
#plot(Annual.Pay~Establishments, data = BLS_Fishing)

#try to predict Annual Pay based on employment
#fmod5<-lm(Annual.Pay~Total.Employment, data = BLS_Fishing)
#summary(fmod5) #R^2 not very good (Adjusted = 0.408)--not good enough
#plot(Annual.Pay~Total.Employment, data = BLS_Fishing)

#only able to fill in a few NAs for employment based on establishments

BLS_Fishing1<-BLS_Fishing[,-2]
write.csv(BLS_Fishing1, 'data/HCC cleaned/BLS Fishing.csv', row.names = F)

#############

BLS_Seafood<-BLS1[BLS1$Industry!='Fishing',] #subset only the rows where Industry column is not "Fishing"
BLS_Seafood_agg<-BLS_Seafood[,-2]
BLS_Seafood_agg<-aggregate(.~Year, data = BLS_Seafood_agg, FUN = sum) #join by Year (summed across the 2 industries)
BLS_Seafood_agg$Annual.Pay<-BLS_Seafood_agg$Annual.Pay/length(unique(BLS_Seafood$Industry)) #annual pay was summed 
#because the aggregate() code can only apply one function accross all variables
#dividing that sum by # of industries will give average Annual Pay
plot(BLS_Seafood_agg$Annual.Pay~BLS_Seafood_agg$Year) #increase, 1st 3 yrs diff
plot(BLS_Seafood_agg$Establishments~BLS_Seafood_agg$Year) #decrease, 1st 3 yrs diff
plot(BLS_Seafood_agg$Total.Employment~BLS_Seafood_agg$Year) #drop, then slight increase
plot(BLS_Seafood_agg$Total.Wages.Millions~BLS_Seafood_agg$Year) #increase, 1st 3 yrs diff

write.csv(BLS_Seafood_agg, 'data/HCC cleaned/BLS Seafood Agg.csv', row.names = F)
BLS_Seafood_agg<- read.csv('data/HCC cleaned/BLS Seafood Agg.csv')

seafood_dryad<-join(BLS_Seafood_agg, dryad2, by = 'Year', type = 'inner')
#only 15 of data years left-- see if it's enough for GAM analyses
#if not, may have to creatively pad the dryad data
write.csv(seafood_dryad, 'data/HCC cleaned/Seafood BLS dryad.csv', row.names = F)

BLS2<-rbind(BLS_Seafood, BLS_Fishing)
write.csv(BLS2, 'data/HCC cleaned/BLS disaggregated by industry.csv', row.names = F)

FishBLS_dryad<-join(dryad2, BLS_Fishing, by = 'Year', type = 'inner')
##need one dataset per industry because can't have multiple values per year when joining with dryad
#only 14 of data years left-- see if it's enough for GAM analyses
####only for establishments and employment (includes predicted data)--other variables have even fewer points due to NAs
#if not, may have to creatively pad the dryad data

write.csv(FishBLS_dryad, 'data/HCC cleaned/Fishing BLS dryad.csv', row.names = F)

MktBLS_dryad<-join(dryad2, BLS_Markets, by = 'Year', type = 'inner')
#only 15 of data years left-- see if it's enough for GAM analyses
#if not, may have to creatively pad the dryad data

write.csv(MktBLS_dryad, 'data/HCC cleaned/Markets BLS dryad.csv', row.names = F)

WhBLS_dryad<-join(dryad2, BLS_Whole, by = 'Year', type = 'inner')
#only 15 of data years left-- see if it's enough for GAM analyses
#if not, may have to creatively pad the dryad data

write.csv(WhBLS_dryad, 'data/HCC cleaned/Wholesale BLS dryad.csv', row.names = F)

#BLS3<-BLS2[,-c(3,6)] #subtract AnnualPay & Total Wages column because too many NAs for Fishing
#leave for now--we can use the data from seafood industries and skip fishing since industries have distinct trends anyway
#BLS4<-BLS3[,-2] #subtract industry column to aggregate Establishments and Total.Employment across industries
#BLS0<-aggregate(.~Year, data = BLS2, FUN = sum) #join by Year (summed across the 2 industries)

##################################################################################################

##gratia code from Scott with MRIP, NES, BLS, and FOSS Data

###############################################################################
###############################FOSS data by YEAR###############################
###############################################################################

FOSS<-read.csv('data/HCC cleaned/FOSS reef fish with Millions.csv') 

commcatch<-gam(Catch.Millions ~ s(Year), 
               data = FOSS, method = 'REML', correlation=corAR1(form=~Year))

FOSS<-FOSS[-39,] # can delete because it was a prelim estimate

##cut off years before 1988--numbers look off--but also they don't
#overlap the bodi analyses time range anyway
FOSS<-FOSS[-c(1:7),]

commcatch1<-gam(Catch.Millions ~ s(Year, k = 9), sp = 0.001,
               data = FOSS, method = 'REML', correlation=corAR1(form=~Year))

commcatch<-gam(Catch.Millions ~ s(Year, k = 7), sp = 0.0008, 
                data = FOSS, method = 'REML')

AIC(commcatch,commcatch1) #same--no need to add Year fir corAR1

commcatch$sp
gam.check(commcatch)

png('figures/HCC/Commercial Reef Fish Catch Assumptions.png', width =  1500, height = 1000, #w & h in pixels
    res = 200) #sets high resolution--default is 150
appraise(commcatch) #much better! #better assumptions than commcatch1
dev.off()




###FOR LOOP for the 3 variables in FOSS in other script
#should work but for some reason only worked when manually setting i = 2 to i = 4
#and sometimes the 2nd FD plot (Comm Rev) sometimes is replaced by an assumption plot
#may be computer glitches--maybe will work on restart
#for now--plots achieve manually are fine


####################PLOT GAM##############

png('figures/HCC/Commercial Catch Reef GAM.png', width =  2400, height = 1700, #w & h in pixels
    res = 300) #sets high resolution--default is 150

par(family = 'Helvetica',
    bg = NA, #empty background--easier to place on ppt
    mar = c(5.6, 5.7, 4, 2.4) #sets margins (bottom, left, top, right)--default values c(5.1,4.1,4.1,2.1)
)
plot(commcatch, residuals = T, pch = 1, #plot points with empty cirles
     seWithMean = TRUE, shade = T, shade.col = 'lightblue', #adds model uncertainty and shades it light blue
     shift = coef(commcatch)[1],  #adds model intercept by shifting y axis by first coef
     lwd = 1.4,
     main = "Commercial Catch in Hawai'i",
     font.main = 1, #makes the title regular (default is bold)
     cex.main = 1.6, #expansion factor for title
     ylab = 'Landings (Millions of Pounds)',
     cex.lab = 1.45, #expansion factor--multiply size of x and y axis labels by 1.7
     cex.axis = 1.3 #expansion factor for numbered axis labels
)
dev.off()

#####first derivatives

#super simple but why no error?
#ftCR<-derivatives(commrev, order = 1, interval = "simultaneous")
#plot(ftCR$derivative~ftCR$data,  )

## parameters for testing
UNCONDITIONAL <- FALSE # unconditional or conditional on estimating smooth params?
N <- 10000             # number of posterior draws
n <- 500               # number of newdata values
EPS <- 1e-07           # finite difference

## where are we going to predict at?
newd <- with(FOSS, data.frame(Year = seq(min(Year), max(Year), 
                                         length = n)))

fd <- fderiv(commcatch, newdata = newd, eps = EPS, unconditional = UNCONDITIONAL)
str(fd, max = 1)

set.seed(42)     # set the seed to make this repeatable 
sint <- confint(fd, type = "simultaneous", nsim = N)
head(sint)

#plot first derivs
theme_set(theme_bw())
ggplot(cbind(sint, Year = newd$Year),
       aes(x = Year, y = est)) +
  theme(  panel.grid.major = element_blank(), #delete major grid lines
          panel.grid.minor = element_blank(), #delete minor grid lines
          axis.text=element_text(size=17, color = 'black'), #adjust font size of axis tick labels
          axis.title=element_text(size=19.5,face="plain"), #adjust size of axis titles
          axis.title.x = element_text(margin = margin(t = 12, r = 0, b = 0, l = 0)), #increases distance between x axis label and plot
          panel.background = element_rect(fill = "transparent",colour = 'black', size = 1),
          plot.background = element_rect(fill = "transparent",colour = NA)) +
  scale_x_continuous(breaks=seq(1980,2020,10)) +
  geom_hline(aes(yintercept = 0), size =0.2 ) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +
  geom_line() +
  #  scale_x_reverse() +
  labs(y = "s'(X)", x = "Year")

ggsave('figures/HCC/Commercial Catch Reef FD.png', 
       width =  8.47, height = 5, units = 'in', #w & h in inches
       dpi = 300, bg = 'transparent')


#plot 2nd derivatives

## second derivatives of all smooths using central finite differences with simultaneous CI
sdCR<-derivatives(commcatch, newdata = newd, eps = EPS, unconditional = UNCONDITIONAL, order = 2, interval = "simultaneous")

as.data.frame(sdCR)

theme_set(theme_bw())
ggplot(sdCR,
       aes(x = data, y = derivative)) +
  theme(  panel.grid.major = element_blank(), #delete major grid lines
          panel.grid.minor = element_blank(), #delete minor grid lines
          axis.text=element_text(size=17, color = 'black'), #adjust font size of axis tick labels
          axis.title=element_text(size=19.5,face="plain"), #adjust size of axis titles
          axis.title.x = element_text(margin = margin(t = 12, r = 0, b = 0, l = 0)), #increases distance between x axis label and plot
          panel.background = element_rect(fill = "transparent",colour = 'black', size = 1),
          plot.background = element_rect(fill = "transparent",colour = NA)) +
  scale_x_continuous(breaks=seq(1980,2020,10)) +
  geom_hline(aes(yintercept = 0), size = 0.2) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +
  geom_line() +
  labs(y = 's"(X)', x = "Year")

ggsave('figures/HCC/Commercial Catch Reef SD.png', 
       width =  8.47, height = 5, units = 'in', #w & h in inches
       dpi = 300, bg = 'transparent')

#graph is super wiggly for some reason, also the y axis seems way too big??


###############################################################################
##############################GAMS: INDS BY DRYAD##############################
###############################################################################

####write nested for loop: loop through all datasets

datasets<-c('data/HCC cleaned/FOSS dryad.csv', 'data/HCC cleaned/Reef Recreation dryad.csv', 
            'data/HCC cleaned/NES dryad.csv', 'data/HCC cleaned/Markets BLS dryad.csv',
            'data/HCC cleaned/Wholesale BLS dryad.csv', 'data/HCC cleaned/Fishing BLS dryad.csv')


FOSS0<-read.csv(paste(datasets[1]))
rec0<-read.csv(paste(datasets[2]))
NES0<-read.csv(paste(datasets[3]))
markets0<-read.csv(paste(datasets[4]))
whole0<-read.csv(paste(datasets[5]))
fishing0<-read.csv(paste(datasets[6]))

##actually the variables for each dataset are different, so can't loop different datasets

###write separate loop for each data set
###then loop through predictors

###then look through variables per predictor

####################################################################################
################################FOSS BY DRYAD#######################################
####################################################################################
## ? drivers & responses vectors not working for loop because they read as text (in quotes)
##   not as column titles
##come back to this later--using S. Large GAM code for now

#  drivers<-c('Predators', "Herbivores", 'Calcifiers', 'Algae')
#  responses<-c('CommDiv', 'Revenue.Millions', 'Catch.Millions')

##Predators has one super high value
FOSS0$Predators[which(FOSS0$Predators > 50)]<-NA #replace the super high Predators value with NA

###maybe try loop later--for now do one by one
############################comm revenue ~ multiple predictors######################

revmod<-gam(Revenue.Millions ~ s(Herbivores, k = 5) + s(Predators, k =5) + s(Calcifiers, k = 5), 
            data = FOSS0, method = 'REML', correlation=corAR1(form=~Year))
#no collinearity is an assumption--maybe can't use related predictors? e.g. predators & herbivores

gam.check(revmod) #see p value for k and model convergence
appraise(revmod) #hist of resids not good--but just exploratory 


# Check overall concurvity
concurvity(rev_calc, full = TRUE) #all low--good
#full = TRUE, reports overall concurvity for each smooth
#full = F would show pairwise concurvity for each predictor against each other predictor
#worst, obs, est are good to look at for diff cases.
#but you should always at least look at worst case, and if value is high (e.g. >0.8), inspect model more carefully


####################PLOT GAMS##############

pdf('figures/HCC/Revenue by Herbs Preds Calcifs.pdf', width =  10, height = 7, #w & h in inches
    bg = 'transparent') 

par(family = 'Helvetica',
    bg = NA, #empty background--easier to place on ppt
    mar = c(5.6, 5.7, 4, 2.4) #sets margins (bottom, left, top, right)--default values c(5.1,4.1,4.1,2.1)
)
plot(revmod, residuals = T, pch = 1, #plot points with empty cirles
     seWithMean = TRUE, shade = T, shade.col = 'lightblue', #adds model uncertainty and shades it light blue
     shift = coef(revmod)[1],  #adds model intercept by shifting y axis by first coef
     lwd = 1.4,
     main = 'Revenue from Commercial Fishing in Hawaii',
     font.main = 1, #makes the title regular (default is bold)
     cex.main = 1.6, #expansion factor for title
     ylab = 'Millions of Dollars',
     cex.lab = 1.45, #expansion factor--multiply size of x and y axis labels by 1.7
     cex.axis = 1.3 #expansion factor for numbered axis labels
)
dev.off()
######all relationships linear######

#############################CommDiv############################

divmod<-gam(log(CommDiv) ~ s(Herbivores, k = 5) + s(Predators, k =5) + s(Calcifiers, k = 5), 
            data = FOSS0, method = 'REML', correlation=corAR1(form=~Year))
#no collinearity is an assumption--maybe can't use related predictors? e.g. predators & herbivores

gam.check(divmod) #see p value for k and model convergence
appraise(divmod) #nope--resids 2 peaks (a second peak at v high revenue in recent yrs)


# Check overall concurvity
concurvity(rev_calc, full = TRUE) #all low--good
#full = TRUE, reports overall concurvity for each smooth
#full = F would show pairwise concurvity for each predictor against each other predictor
#worst, obs, est are good to look at for diff cases.
#but you should always at least look at worst case, and if value is high (e.g. >0.8), inspect model more carefully


####################PLOT GAMS##############

pdf('figures/HCC/Diversity by Herbs Preds Calcifs.pdf', width =  10, height = 7, #w & h in inches
    bg = 'transparent') 

par(family = 'Helvetica',
    bg = NA, #empty background--easier to place on ppt
    mar = c(5.6, 5.7, 4, 2.4) #sets margins (bottom, left, top, right)--default values c(5.1,4.1,4.1,2.1)
)
plot(divmod, residuals = T, pch = 1, #plot points with empty cirles
     seWithMean = TRUE, shade = T, shade.col = 'lightblue', #adds model uncertainty and shades it light blue
     shift = coef(revmod)[1],  #adds model intercept by shifting y axis by first coef
     lwd = 1.4,
     main = "Diversity of Commercial Catch in Hawai'i ",
     font.main = 1, #makes the title regular (default is bold)
     cex.main = 1.6, #expansion factor for title
     ylab = 'Effective Shannon Diversity',
     cex.lab = 1.45, #expansion factor--multiply size of x and y axis labels by 1.7
     cex.axis = 1.3 #expansion factor for numbered axis labels
)
dev.off()

######all relationships linear######

#############################Comm Catch############################

catchmod<-gam(Catch.Millions ~ s(Herbivores, k = 7) + s(Predators, k =7), 
              data = FOSS0, method = 'REML', correlation=corAR1(form=~Year))
#no collinearity is an assumption--maybe can't use related predictors? e.g. predators & herbivores

gam.check(catchmod) #see p value for k and model convergence
appraise(catchmod) #nope--resids 2 peaks (a second peak at v high revenue in recent yrs)


# Check overall concurvity (if predictors are related)
concurvity(catchmod, full = TRUE) #worst is > 0.8--herbs and preds may be too related
#full = TRUE, reports overall concurvity for each smooth
#full = F would show pairwise concurvity for each predictor against each other predictor
#worst, obs, est are good to look at for diff cases.
#but you should always at least look at worst case, and if value is high (e.g. >0.8), inspect model more carefully


####################PLOT GAMS##############

pdf('figures/HCC/Commerical Catch by Herbs Preds.pdf', width =  10, height = 7, #w & h in inches
    bg = 'transparent') 

par(family = 'Helvetica',
    bg = NA, #empty background--easier to place on ppt
    mar = c(5.6, 5.7, 4, 2.4) #sets margins (bottom, left, top, right)--default values c(5.1,4.1,4.1,2.1)
)
plot(catchmod, residuals = T, pch = 1, #plot points with empty cirles
     seWithMean = TRUE, shade = T, shade.col = 'lightblue', #adds model uncertainty and shades it light blue
     shift = coef(revmod)[1],  #adds model intercept by shifting y axis by first coef
     lwd = 1.4,
     main = "Commercial Catch in Hawai'i ",
     font.main = 1, #makes the title regular (default is bold)
     cex.main = 1.6, #expansion factor for title
     ylab = 'Millions of Pounds',
     cex.lab = 1.45, #expansion factor--multiply size of x and y axis labels by 1.7
     cex.axis = 1.3 #expansion factor for numbered axis labels
)
dev.off()

######all relationships linear######

####################################################################################
##################################DATA: MRIP########################################
####################################################################################


#############################Angler Trips##############################

trips<-gam(Trips.Millions ~ s(Predators, k =7), sp =0.1, 
           data = rec0, method = 'REML', correlation=corAR1(form=~Year))
#no collinearity is an assumption--maybe can't use related predictors? e.g. predators & herbivores

trips1<-gam(Trips.Millions ~ s(Algae, k =7), sp =0.1,
            data = rec0, method = 'REML', correlation=corAR1(form=~Year))

trips$sp

AIC(trips, trips1) #AIC with and without AR1 are exact same because Year is already the predictor
#only affects if Year is used as AR1 factor and env variable used as predictor

gam.check(trips) #see p value for k and model convergence
appraise(trips) #nope--hist of resids not good--just exploratory
appraise(trips1) #nope--hist of resids not good--just exploratory


# Check overall concurvity (if predictors are related)
concurvity(trips, full = TRUE) #worst is > 0.8--herbs and preds may be too related
#full = TRUE, reports overall concurvity for each smooth
#full = F would show pairwise concurvity for each predictor against each other predictor
#worst, obs, est are good to look at for diff cases.
#but you should always at least look at worst case, and if value is high (e.g. >0.8), inspect model more carefully


####################PLOT GAMS##############

pdf('figures/HCC/Angler Trips by Algae Preds.pdf', width =  10, height = 7, #w & h in inches
    bg = 'transparent') 

par(family = 'Helvetica',
    bg = NA, #empty background--easier to place on ppt
    mar = c(5.6, 5.7, 4, 2.4) #sets margins (bottom, left, top, right)--default values c(5.1,4.1,4.1,2.1)
)
plot(trips, residuals = T, pch = 1, #plot points with empty cirles
     seWithMean = TRUE, shade = T, shade.col = 'lightblue', #adds model uncertainty and shades it light blue
     shift = coef(revmod)[1],  #adds model intercept by shifting y axis by first coef
     lwd = 1.4,
     main = "Number of Recreational Angler Trips in Hawai'i ",
     font.main = 1, #makes the title regular (default is bold)
     cex.main = 1.6, #expansion factor for title
     ylab = 'Millions of Trips',
     cex.lab = 1.45, #expansion factor--multiply size of x and y axis labels by 1.7
     cex.axis = 1.3 #expansion factor for numbered axis labels
)
dev.off()

######all relationships linear######


#########################BREAK FROM SOC-ECO###################################
#since no nonlinear relationships-->just do inds by year

##############################################################################
##############################MRIP by YEAR####################################
##############################################################################

#####################rec catch

#KEEP if inclusive
#rec.Y<-read.csv('data/HCC cleaned/Recreational Catch and Trips.csv')

#KEEP if reef only
#rec.Y<-read.csv('data/HCC cleaned/Reef Recreational Catch and Trips.csv')

#KEEP if pelagics only
rec.Y<-read.csv('data/HCC cleaned/Pelagic Recreational Catch and Trips.csv')

rec.Y<-rec.Y[-18,] #delete year 2020--prelim only

rcatch<-gam(RCatch.Millions ~ s(Year, k =7), sp = 0.25,
            data = rec.Y, method = 'REML', correlation=corAR1(form=~Year))


appraise(rcatch) #meh
#looks like high outlier
rcatch$sp

gam.check(rcatch)

#rec.Y$RCatch.Millions[which(rec.Y$RCatch.Millions > 10)]<-NA #replace the three extra high RCatch value with NA

#rcatch<-gam(RCatch.Millions ~ s(Year), 
#            data = rec.Y, method = 'REML', correlation=corAR1(form=~Year))

##inclusive
#png('figures/HCC/Rec Catch Assumptions.png', width =  1500, height = 1000, #w & h in pixels
 #   res = 200) #sets high resolution--default is 150

##reef only
#png('figures/HCC/Reef Rec Catch Assumptions.png', width =  1500, height = 1000, #w & h in pixels
 #   res = 200) #sets high resolution--default is 150

##pelagic only
png('figures/HCC/Pelagic Rec Catch Assumptions.png', width =  1500, height = 1000, #w & h in pixels
    res = 200) #sets high resolution--default is 150 

appraise(rcatch) #little better! --hist resids meh
dev.off()

####################PLOT GAM##############
#inclusive
#png('figures/HCC/Rec Catch over Time.png', width =  2400, height = 1700, #w & h in pixels
 #   res = 300) #sets high resolution--default is 150

##reef only
#png('figures/HCC/Reef Rec Catch over Time.png', width =  2400, height = 1700, #w & h in pixels
 #   res = 300) #sets high resolution--default is 150

##pelagic only
png('figures/HCC/Pelagic Rec Catch over Time.png', width =  2400, height = 1700, #w & h in pixels
    res = 300) #sets high resolution--default is 150

par(family = 'Helvetica',
    bg = NA, #empty background--easier to place on ppt
    mar = c(5.6, 5.7, 4, 2.4) #sets margins (bottom, left, top, right)--default values c(5.1,4.1,4.1,2.1)
)
plot(rcatch, residuals = T, pch = 1, #plot points with empty cirles
     seWithMean = TRUE, shade = T, shade.col = 'lightblue', #adds model uncertainty and shades it light blue
     shift = coef(rcatch)[1],  #adds model intercept by shifting y axis by first coef
     lwd = 1.4,
     main = "Recreational Catch in Hawai'i Over Time",
     font.main = 1, #makes the title regular (default is bold)
     cex.main = 1.6, #expansion factor for title
     ylab = 'Millions of Pounds',
     cex.lab = 1.45, #expansion factor--multiply size of x and y axis labels by 1.7
     cex.axis = 1.3 #expansion factor for numbered axis labels
)
dev.off()

#####first derivatives

## parameters for testing
UNCONDITIONAL <- FALSE # unconditional or conditional on estimating smooth params?
N <- 10000             # number of posterior draws
n <- 500               # number of newdata values
EPS <- 1e-07           # finite difference

## where are we going to predict at?
newd <- with(rec.Y, data.frame(Year = seq(min(Year), max(Year), 
                                          length = n)))

fd <- fderiv(rcatch, newdata = newd, eps = EPS, unconditional = UNCONDITIONAL)
str(fd, max = 1)

set.seed(42)     # set the seed to make this repeatable 
sint <- confint(fd, type = "simultaneous", nsim = N)
head(sint)

#plot first derivs

theme_set(theme_bw())
ggplot(cbind(sint, Year = newd$Year),
       aes(x = Year, y = est)) +
  theme(  panel.grid.major = element_blank(), #delete major grid lines
          panel.grid.minor = element_blank(), #delete minor grid lines
          axis.text=element_text(size=17, color = 'black'), #adjust font size of axis tick labels
          axis.title=element_text(size=19.5,face="plain"), #adjust size of axis titles
          axis.title.x = element_text(margin = margin(t = 12, r = 0, b = 0, l = 0)), #increases distance between x axis label and plot
          panel.background = element_rect(fill = "transparent",colour = 'black', size = 1),
          plot.background = element_rect(fill = "transparent",colour = NA)) +
  scale_x_continuous(breaks=seq(2000,2020,5)) +
  geom_hline(aes(yintercept = 0), size =0.2 ) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +
  geom_line() +
  #  scale_x_reverse() +
  labs(y = "s'(X)", x = "Year")

##inclusive
#ggsave('figures/HCC/Rec Catch FD.png', 
 #      width =  8.47, height = 5, units = 'in', #w & h in inches
  #     dpi = 300, bg = 'transparent')

##reef
#ggsave('figures/HCC/Reef Rec Catch FD.png', 
 #      width =  8.47, height = 5, units = 'in', #w & h in inches
  #     dpi = 300, bg = 'transparent')

#pelagic
ggsave('figures/HCC/Pelagic Rec Catch FD.png', 
       width =  8.47, height = 5, units = 'in', #w & h in inches
       dpi = 300, bg = 'transparent')

#plot 2nd derivatives

## second derivatives of all smooths using central finite differences with simultaneous CI
sdCR<-derivatives(rcatch, newdata = newd, eps = EPS, unconditional = UNCONDITIONAL, 
                  order = 2, interval = "simultaneous")

as.data.frame(sdCR)

theme_set(theme_bw())
ggplot(sdCR,
       aes(x = data, y = derivative)) +
  theme(  panel.grid.major = element_blank(), #delete major grid lines
          panel.grid.minor = element_blank(), #delete minor grid lines
          axis.text=element_text(size=17, color = 'black'), #adjust font size of axis tick labels
          axis.title=element_text(size=19.5,face="plain"), #adjust size of axis titles
          axis.title.x = element_text(margin = margin(t = 12, r = 0, b = 0, l = 0)), #increases distance between x axis label and plot
          panel.background = element_rect(fill = "transparent",colour = 'black', size = 1),
          plot.background = element_rect(fill = "transparent",colour = NA)) +
  scale_x_continuous(breaks=seq(2000,2020,5)) +
  geom_hline(aes(yintercept = 0), size = 0.2) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +
  geom_line() +
  labs(y = 's"(X)', x = "Year")
#graph is super wiggly for some reason, also the y axis seems way too big??

#inclusive
#ggsave('figures/HCC/Rec Catch SD.png', 
 #      width =  8.47, height = 5, units = 'in', #w & h in inches
  #     dpi = 300, bg = 'transparent')

#reef
#ggsave('figures/HCC/Reef Rec Catch SD.png', 
 #      width =  8.47, height = 5, units = 'in', #w & h in inches
  #     dpi = 300, bg = 'transparent')

#pelagic
ggsave('figures/HCC/Pelagic Rec Catch SD.png', 
       width =  8.47, height = 5, units = 'in', #w & h in inches
       dpi = 300, bg = 'transparent')


########################rec trips
rtrips<-gam(Trips.Millions ~ s(Year, k =12), sp = 0.008, 
            data = rec.Y, method = 'REML', correlation=corAR1(form=~Year))

rtrips$sp
gam.check(rtrips)

png('figures/HCC/Angler Trips Assumptions.png', width =  1500, height = 1000, #w & h in pixels
    res = 200) #sets high resolution--default is 150
appraise(rtrips) #looks pretty decent
dev.off()

####################PLOT GAM##############

png('figures/HCC/Angler Trips over Time.png', width =  2400, height = 1700, #w & h in pixels
    res = 300) #sets high resolution--default is 150

par(family = 'Helvetica',
    bg = NA, #empty background--easier to place on ppt
    mar = c(5.6, 5.7, 4, 2.4) #sets margins (bottom, left, top, right)--default values c(5.1,4.1,4.1,2.1)
)
plot(rtrips, residuals = T, pch = 1, #plot points with empty cirles
     seWithMean = TRUE, shade = T, shade.col = 'lightblue', #adds model uncertainty and shades it light blue
     shift = coef(rtrips)[1],  #adds model intercept by shifting y axis by first coef
     lwd = 1.4,
     main = "Recreational Angler Trips in Hawai'i Over Time",
     font.main = 1, #makes the title regular (default is bold)
     cex.main = 1.6, #expansion factor for title
     ylab = 'Millions of Trips',
     cex.lab = 1.45, #expansion factor--multiply size of x and y axis labels by 1.7
     cex.axis = 1.3 #expansion factor for numbered axis labels
)
dev.off()

#############looks mostly linear but will test derivatives

#####first derivatives

## parameters for testing
UNCONDITIONAL <- FALSE # unconditional or conditional on estimating smooth params?
N <- 10000             # number of posterior draws
n <- 500               # number of newdata values
EPS <- 1e-07           # finite difference

## where are we going to predict at?
newd <- with(rec.Y, data.frame(Year = seq(min(Year), max(Year), 
                                          length = n)))

fd <- fderiv(rtrips, newdata = newd, eps = EPS, unconditional = UNCONDITIONAL)
str(fd, max = 1)

set.seed(42)     # set the seed to make this repeatable 
sint <- confint(fd, type = "simultaneous", nsim = N)
head(sint)

#plot first derivs

theme_set(theme_bw())
ggplot(cbind(sint, Year = newd$Year),
       aes(x = Year, y = est)) +
  theme(  panel.grid.major = element_blank(), #delete major grid lines
          panel.grid.minor = element_blank(), #delete minor grid lines
          axis.text=element_text(size=17, color = 'black'), #adjust font size of axis tick labels
          axis.title=element_text(size=19.5,face="plain"), #adjust size of axis titles
          axis.title.x = element_text(margin = margin(t = 12, r = 0, b = 0, l = 0)), #increases distance between x axis label and plot
          panel.background = element_rect(fill = "transparent",colour = 'black', size = 1),
          plot.background = element_rect(fill = "transparent",colour = NA)) +
  scale_x_continuous(breaks=seq(2000,2020,5)) +
  geom_hline(aes(yintercept = 0), size =0.2 ) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +
  geom_line() +
  #  scale_x_reverse() +
  labs(y = "s'(X)", x = "Year")

ggsave('figures/HCC/Angler Trips FD.png', 
       width =  8.47, height = 5, units = 'in', #w & h in inches
       dpi = 300, bg = 'transparent')

#plot 2nd derivatives

## second derivatives of all smooths using central finite differences with simultaneous CI
sdCR<-derivatives(rtrips, newdata = newd, eps = EPS, unconditional = UNCONDITIONAL, 
                  order = 2, interval = "simultaneous")

as.data.frame(sdCR)

theme_set(theme_bw())
ggplot(sdCR,
       aes(x = data, y = derivative)) +
  theme(  panel.grid.major = element_blank(), #delete major grid lines
          panel.grid.minor = element_blank(), #delete minor grid lines
          axis.text=element_text(size=17, color = 'black'), #adjust font size of axis tick labels
          axis.title=element_text(size=19.5,face="plain"), #adjust size of axis titles
          axis.title.x = element_text(margin = margin(t = 12, r = 0, b = 0, l = 0)), #increases distance between x axis label and plot
          panel.background = element_rect(fill = "transparent",colour = 'black', size = 1),
          plot.background = element_rect(fill = "transparent",colour = NA)) +
  scale_x_continuous(breaks=seq(2000,2020,5)) +
  geom_hline(aes(yintercept = 0), size = 0.2) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +
  geom_line() +
  labs(y = 's"(X)', x = "Year")
#graph is super wiggly for some reason, also the y axis seems way too big??
ggsave('figures/HCC/Angler Trips SD.png', 
       width =  8.47, height = 5, units = 'in', #w & h in inches
       dpi = 300, bg = 'transparent')


##############################################################################
###############################NES by YEAR####################################
##############################################################################

NES.Y<-read.csv('data/HCC cleaned/NES Establishments and Receipts.csv')

#####################Establishments

estab<-gam(Establishments ~ s(Year, k =14), sp = 0.001,
           data = NES.Y, method = 'REML', correlation=corAR1(form=~Year))
appraise(estab)

estab$sp
gam.check(estab)

png('figures/HCC/NES Establishment Assumptions.png', width =  1500, height = 1000, #w & h in pixels
    res = 200) #sets high resolution--default is 150
appraise(estab) #looks pretty good--hist of resids a little iffy
dev.off()


####################PLOT GAM##############

png('figures/HCC/NES Establishments Over Time.png', width =  2400, height = 1700, #w & h in pixels
    res = 300) #sets high resolution--default is 150

par(family = 'Helvetica',
    bg = NA, #empty background--easier to place on ppt
    mar = c(5.6, 5.7, 5, 2.4) #sets margins (bottom, left, top, right)--default values c(5.1,4.1,4.1,2.1)
)
plot(estab, residuals = T, pch = 1, #plot points with empty cirles
     seWithMean = TRUE, shade = T, shade.col = 'lightblue', #adds model uncertainty and shades it light blue
     shift = coef(estab)[1],  #adds model intercept by shifting y axis by first coef
     lwd = 1.4,
     main = c("Self Employed Individuals in Seafood Markets" ,"and Fishing in Hawai'i"),
     font.main = 1, #makes the title regular (default is bold)
     cex.main = 1.6, #expansion factor for title
     ylab = 'Number of Individuals',
     cex.lab = 1.45, #expansion factor--multiply size of x and y axis labels by 1.7
     cex.axis = 1.3 #expansion factor for numbered axis labels
)
dev.off()

#####first derivatives

## parameters for testing
UNCONDITIONAL <- FALSE # unconditional or conditional on estimating smooth params?
N <- 10000             # number of posterior draws
n <- 500               # number of newdata values
EPS <- 1e-07           # finite difference

## where are we going to predict at?
newd <- with(NES.Y, data.frame(Year = seq(min(Year), max(Year), 
                                          length = n)))

fd <- fderiv(estab, newdata = newd, eps = EPS, unconditional = UNCONDITIONAL)
str(fd, max = 1)

set.seed(42)     # set the seed to make this repeatable 
sint <- confint(fd, type = "simultaneous", nsim = N)
head(sint)

#plot first derivs

theme_set(theme_bw())
ggplot(cbind(sint, Year = newd$Year),
       aes(x = Year, y = est)) +
  theme(  panel.grid.major = element_blank(), #delete major grid lines
          panel.grid.minor = element_blank(), #delete minor grid lines
          axis.text=element_text(size=17, color = 'black'), #adjust font size of axis tick labels
          axis.title=element_text(size=19.5,face="plain"), #adjust size of axis titles
          axis.title.x = element_text(margin = margin(t = 12, r = 0, b = 0, l = 0)), #increases distance between x axis label and plot
          panel.background = element_rect(fill = "transparent",colour = 'black', size = 1),
          plot.background = element_rect(fill = "transparent",colour = NA)) +
  scale_x_continuous(breaks=seq(1995,2020,5)) +
  geom_hline(aes(yintercept = 0), size =0.2 ) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +
  geom_line() +
  #  scale_x_reverse() +
  labs(y = "s'(X)", x = "Year")

ggsave('figures/HCC/NES Establishments FD.png', 
       width =  8.47, height = 5, units = 'in', #w & h in inches
       dpi = 300, bg = 'transparent')

#plot 2nd derivatives

## second derivatives of all smooths using central finite differences with simultaneous CI
sdCR<-derivatives(estab, newdata = newd, eps = EPS, unconditional = UNCONDITIONAL, 
                  order = 2, interval = "simultaneous")

as.data.frame(sdCR)

theme_set(theme_bw())
ggplot(sdCR,
       aes(x = data, y = derivative)) +
  theme(  panel.grid.major = element_blank(), #delete major grid lines
          panel.grid.minor = element_blank(), #delete minor grid lines
          axis.text=element_text(size=17, color = 'black'), #adjust font size of axis tick labels
          axis.title=element_text(size=19.5,face="plain"), #adjust size of axis titles
          axis.title.x = element_text(margin = margin(t = 12, r = 0, b = 0, l = 0)), #increases distance between x axis label and plot
          panel.background = element_rect(fill = "transparent",colour = 'black', size = 1),
          plot.background = element_rect(fill = "transparent",colour = NA)) +
  scale_x_continuous(breaks=seq(1995,2020,5)) +
  geom_hline(aes(yintercept = 0), size = 0.2) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +
  geom_line() +
  labs(y = 's"(X)', x = "Year")
#graph is super wiggly for some reason, also the y axis seems way too big??
ggsave('figures/HCC/NES Establishments SD.png', 
       width =  8.47, height = 5, units = 'in', #w & h in inches
       dpi = 300, bg = 'transparent')

#####################Receipts

rct<-gam(Receipts.Millions ~ s(Year, k=15), #adjusting k helped resids hist
         sp = 0.008,  
         data = NES.Y, method = 'REML', correlation=corAR1(form=~Year))
rct$sp
gam.check(rct)

png('figures/HCC/NES Receipts Assumptions.png', width =  1500, height = 1000, #w & h in pixels
    res = 200) #sets high resolution--default is 150
appraise(rct) #looks pretty good
dev.off()

####################PLOT GAM##############

png('figures/HCC/NES Receipts Over Time.png', width =  2400, height = 1700, #w & h in pixels
    res = 300) #sets high resolution--default is 150

par(family = 'Helvetica',
    bg = NA, #empty background--easier to place on ppt
    mar = c(5.6, 5.7, 4, 2.4) #sets margins (bottom, left, top, right)--default values c(5.1,4.1,4.1,2.1)
)
plot(rct, residuals = T, pch = 1, #plot points with empty cirles
     seWithMean = TRUE, shade = T, shade.col = 'lightblue', #adds model uncertainty and shades it light blue
     shift = coef(rct)[1],  #adds model intercept by shifting y axis by first coef
     lwd = 1.4,
     main = "Non Employer Total Receipts in Hawai'i",
     font.main = 1, #makes the title regular (default is bold)
     cex.main = 1.6, #expansion factor for title
     ylab = 'Millions of Dollars',
     cex.lab = 1.45, #expansion factor--multiply size of x and y axis labels by 1.7
     cex.axis = 1.3 #expansion factor for numbered axis labels
)
dev.off()

#####first derivatives

## parameters for testing
UNCONDITIONAL <- FALSE # unconditional or conditional on estimating smooth params?
N <- 10000             # number of posterior draws
n <- 500               # number of newdata values
EPS <- 1e-07           # finite difference

## where are we going to predict at?
newd <- with(NES.Y, data.frame(Year = seq(min(Year), max(Year), 
                                          length = n)))

fd <- fderiv(rct, newdata = newd, eps = EPS, unconditional = UNCONDITIONAL)
str(fd, max = 1)

set.seed(42)     # set the seed to make this repeatable 
sint <- confint(fd, type = "simultaneous", nsim = N)
head(sint)

#plot first derivs

theme_set(theme_bw())
ggplot(cbind(sint, Year = newd$Year),
       aes(x = Year, y = est)) +
  theme(  panel.grid.major = element_blank(), #delete major grid lines
          panel.grid.minor = element_blank(), #delete minor grid lines
          axis.text=element_text(size=17, color = 'black'), #adjust font size of axis tick labels
          axis.title=element_text(size=19.5,face="plain"), #adjust size of axis titles
          axis.title.x = element_text(margin = margin(t = 12, r = 0, b = 0, l = 0)), #increases distance between x axis label and plot
          panel.background = element_rect(fill = "transparent",colour = 'black', size = 1),
          plot.background = element_rect(fill = "transparent",colour = NA)) +
  scale_x_continuous(breaks=seq(1995,2020,5)) +
  geom_hline(aes(yintercept = 0), size =0.2 ) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +
  geom_line() +
  #  scale_x_reverse() +
  labs(y = "s'(X)", x = "Year")

ggsave('figures/HCC/NES Receipts FD.png', 
       width =  8.47, height = 5, units = 'in', #w & h in inches
       dpi = 300, bg = 'transparent')


#plot 2nd derivatives

## second derivatives of all smooths using central finite differences with simultaneous CI
sdCR<-derivatives(rct, newdata = newd, eps = EPS, unconditional = UNCONDITIONAL, 
                  order = 2, interval = "simultaneous")

as.data.frame(sdCR)

theme_set(theme_bw())
ggplot(sdCR,
       aes(x = data, y = derivative)) +
  theme(  panel.grid.major = element_blank(), #delete major grid lines
          panel.grid.minor = element_blank(), #delete minor grid lines
          axis.text=element_text(size=17, color = 'black'), #adjust font size of axis tick labels
          axis.title=element_text(size=19.5,face="plain"), #adjust size of axis titles
          axis.title.x = element_text(margin = margin(t = 12, r = 0, b = 0, l = 0)), #increases distance between x axis label and plot
          panel.background = element_rect(fill = "transparent",colour = 'black', size = 1),
          plot.background = element_rect(fill = "transparent",colour = NA)) +
  scale_x_continuous(breaks=seq(1995,2020,5)) +
  geom_hline(aes(yintercept = 0), size = 0.2) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +
  geom_line() +
  labs(y = 's"(X)', x = "Year")
#graph is super wiggly for some reason, also the y axis seems way too big??
ggsave('figures/HCC/NES Receipts SD.png', 
       width =  8.47, height = 5, units = 'in', #w & h in inches
       dpi = 300, bg = 'transparent')

##############################################################################
###############################BLS by YEAR####################################
##############################################################################

#test full mod with Year as smoothing factor and Industry as fixed factor
BLS.Y<-read.csv('data/HCC cleaned/BLS disaggregated by industry.csv')

blsmod<-gam(Establishments ~ s(Year) + Industry, 
            data = BLS.Y, method = 'REML', correlation=corAR1(form=~Year))

gam.check(blsmod)
appraise(blsmod) #qq and hist pretty good, hov and obs v. fitted values have two clear clusters
plot(blsmod, residuals = T, pch = 1, #plot points with empty cirles
     seWithMean = TRUE, shade = T, shade.col = 'lightblue', #adds model uncertainty and shades it light blue
     shift = coef(blsmod)[1],
     ylab = 'Number of Establishments')
#linear relationship


##############################SEAFOOD#############################

#test mod with Seafood Wholesalers and Seafood Markets combined
seaf<-read.csv('data/HCC cleaned/BLS Seafood Agg.csv')

variables<- c('1', 'Annual Pay', 'Establishments', 'Employment', 'Total Wages')
ylabels<- c('1', 'Dollars', 'Number of Establishments', 'Number of Individuals', 'Millions of Dollars')
basis<-c(0,6,8,10,6)

##first derivatives
## parameters for testing
UNCONDITIONAL <- FALSE # unconditional or conditional on estimating smooth params?
N <- 10000             # number of posterior draws
n <- 500               # number of newdata values
EPS <- 1e-07           # finite difference

## where are we going to predict at?
newd <- with(seaf, data.frame(Year = seq(min(Year), max(Year), 
                                         length = n)))

i=5
for (i in 2:5) {
  sfmod<-gam(seaf[,i] ~ s(Year, k = basis[i]), 
             data = seaf, method = 'REML', correlation=corAR1(form=~Year))
  
  gam.check(sfmod)
  
  png(paste('figures/HCC/BLS Seafood', variables[i] ,'Assumptions.png', sep = ' '), 
      width =  1500, height = 1000, #w & h in pixels
      res = 200) #sets high resolution--default is 150
  appraise(sfmod) #looks pretty good--hist of resids a little iffy
  dev.off()
  
  ####################PLOT GAM##############
  
  png(paste('figures/HCC/BLS Seafood', variables[i] ,'Over Time.png', sep = ' '), width =  2400, height = 1700, #w & h in pixels
      res = 300) #sets high resolution--default is 150
  
  par(family = 'Helvetica',
      bg = NA, #empty background--easier to place on ppt
      mar = c(5.6, 5.7, 4, 2.4) #sets margins (bottom, left, top, right)--default values c(5.1,4.1,4.1,2.1)
  )
  plot(sfmod, residuals = T, pch = 1, #plot points with empty cirles
       seWithMean = TRUE, shade = T, shade.col = 'lightblue', #adds model uncertainty and shades it light blue
       shift = coef(sfmod)[1],  #adds model intercept by shifting y axis by first coef
       lwd = 1.4,
       main = paste("Seafood Industry", variables[i] ,"in Hawai'i", sep = " "),
       font.main = 1, #makes the title regular (default is bold)
       cex.main = 1.6, #expansion factor for title
       ylab = paste(ylabels[i]),
       cex.lab = 1.45, #expansion factor--multiply size of x and y axis labels by 1.7
       cex.axis = 1.3 #expansion factor for numbered axis labels
  )
  dev.off()
  
  #####first derivatives
  
  fd <- fderiv(sfmod, newdata = newd, eps = EPS, unconditional = UNCONDITIONAL)
  str(fd, max = 1)
  
  set.seed(42)     # set the seed to make this repeatable 
  sint <- confint(fd, type = "simultaneous", nsim = N)
  head(sint)
  
  #plot first derivs
  
  theme_set(theme_bw())
  ggplot(cbind(sint, Year = newd$Year),
         aes(x = Year, y = est)) +
    theme(  panel.grid.major = element_blank(), #delete major grid lines
            panel.grid.minor = element_blank(), #delete minor grid lines
            axis.text=element_text(size=17, color = 'black'), #adjust font size of axis tick labels
            axis.title=element_text(size=19.5,face="plain"), #adjust size of axis titles
            axis.title.x = element_text(margin = margin(t = 12, r = 0, b = 0, l = 0)), #increases distance between x axis label and plot
            panel.background = element_rect(fill = "transparent",colour = 'black', size = 1),
            plot.background = element_rect(fill = "transparent",colour = NA)) +
    scale_x_continuous(breaks=seq(2000,2020,5)) +
    geom_hline(aes(yintercept = 0), size =0.2 ) +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +
    geom_line() +
    #  scale_x_reverse() +
    labs(y = "s'(X)", x = "Year")
  
  ggsave(paste('figures/HCC/BLS Seafood', variables[i] ,'FD.png', sep = ' '), 
         width =  8.47, height = 5, units = 'in', #w & h in inches
         dpi = 300, bg = 'transparent')
  
  #plot 2nd derivatives
  
  ## second derivatives of all smooths using central finite differences with simultaneous CI
  sdCR<-derivatives(sfmod, newdata = newd, eps = EPS, unconditional = UNCONDITIONAL, 
                    order = 2, interval = "simultaneous")
  
  as.data.frame(sdCR)
  
  theme_set(theme_bw())
  ggplot(sdCR,
         aes(x = data, y = derivative)) +
    theme(  panel.grid.major = element_blank(), #delete major grid lines
            panel.grid.minor = element_blank(), #delete minor grid lines
            axis.text=element_text(size=17, color = 'black'), #adjust font size of axis tick labels
            axis.title=element_text(size=19.5,face="plain"), #adjust size of axis titles
            axis.title.x = element_text(margin = margin(t = 12, r = 0, b = 0, l = 0)), #increases distance between x axis label and plot
            panel.background = element_rect(fill = "transparent",colour = 'black', size = 1),
            plot.background = element_rect(fill = "transparent",colour = NA)) +
    scale_x_continuous(breaks=seq(2000,2020,5)) +
    geom_hline(aes(yintercept = 0), size = 0.2) +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +
    geom_line() +
    labs(y = 's"(X)', x = "Year")
  #graph is super wiggly for some reason, also the y axis seems way too big??
  ggsave(paste('figures/HCC/BLS Seafood', variables[i] ,'SD.png', sep = ' '), 
         width =  8.47, height = 5, units = 'in', #w & h in inches
         dpi = 300, bg = 'transparent')
  
}

###for loop doesn't export the png's for some reason--
### have to manually sub i for each variable and source code through the loop


############################FISHING###############################

#test mod with Seafood Wholesalers and Seafood Markets combined
fsh<-read.csv('data/HCC cleaned/BLS Fishing.csv')

variables<- c('1', 'Annual Pay', 'Establishments', 'Employment', 'Total Wages')
ylabels<- c('1', 'Dollars', 'Number of Establishments', 'Number of Individuals', 'Millions of Dollars')


##first derivatives
## parameters for testing
UNCONDITIONAL <- FALSE # unconditional or conditional on estimating smooth params?
N <- 10000             # number of posterior draws
n <- 500               # number of newdata values
EPS <- 1e-07           # finite difference

## where are we going to predict at?
newd <- with(fsh, data.frame(Year = seq(min(Year), max(Year), 
                                        length = n)))

##################for loop for fishing industry only########
basis<-c(0,5,10,11,6) #allows me to set diff number of basis fxns for each variable
sm<- c(0,0.01,0.00265,0.001,0.006)

for (i in 2:5) {
  fshmod<-gam(fsh[,i] ~ s(Year, k = basis[i]), sp = sm[i],
              data = fsh, method = 'REML', correlation=corAR1(form=~Year))
  fshmod$sp
  
  gam.check(fshmod)
  
  png(paste('figures/HCC/BLS Fishing', variables[i] ,'Assumptions.png', sep = ' '), 
      width =  1500, height = 1000, #w & h in pixels
      res = 200) #sets high resolution--default is 150
  appraise(fshmod) #looks pretty good--hist of resids a little iffy
  dev.off()
  
  ####################PLOT GAM##############
  
  png(paste('figures/HCC/BLS Fishing', variables[i] ,'Over Time.png', sep = ' '), width =  2400, height = 1700, #w & h in pixels
      res = 300) #sets high resolution--default is 150
  
  par(family = 'Helvetica',
      bg = NA, #empty background--easier to place on ppt
      mar = c(5.6, 5.7, 4, 2.4) #sets margins (bottom, left, top, right)--default values c(5.1,4.1,4.1,2.1)
  )
  plot(fshmod, residuals = T, pch = 1, #plot points with empty cirles
       seWithMean = TRUE, shade = T, shade.col = 'lightblue', #adds model uncertainty and shades it light blue
       shift = coef(fshmod)[1],  #adds model intercept by shifting y axis by first coef
       lwd = 1.4,
       main = paste("Fishing Industry", variables[i] ,"in Hawai'i", sep = " "),
       font.main = 1, #makes the title regular (default is bold)
       cex.main = 1.6, #expansion factor for title
       ylab = paste(ylabels[i]),
       cex.lab = 1.45, #expansion factor--multiply size of x and y axis labels by 1.7
       cex.axis = 1.3 #expansion factor for numbered axis labels
  )
  dev.off()
  
  #####first derivatives
  
  fd <- fderiv(fshmod, newdata = newd, eps = EPS, unconditional = UNCONDITIONAL)
  str(fd, max = 1)
  
  set.seed(42)     # set the seed to make this repeatable 
  sint <- confint(fd, type = "simultaneous", nsim = N)
  head(sint)
  
  #plot first derivs
  png(paste('figures/HCC/BLS Fishing', variables[i] ,'FD.png', sep = ' '), width =  2400, height = 1700, #w & h in pixels
      res = 300) #sets high resolution--default is 150
  
  par(family = 'Helvetica',
      bg = NA, #empty background--easier to place on ppt
      mar = c(5.6, 5.7, 4, 2.4) #sets margins (bottom, left, top, right)--default values c(5.1,4.1,4.1,2.1)
  )
  theme_set(theme_bw())
  ggplot(cbind(sint, Year = newd$Year),
         aes(x = Year, y = est)) +
    theme(  panel.grid.major = element_blank(), #delete major grid lines
            panel.grid.minor = element_blank(), #delete minor grid lines
            axis.text=element_text(size=17, color = 'black'), #adjust font size of axis tick labels
            axis.title=element_text(size=19.5,face="plain"), #adjust size of axis titles
            axis.title.x = element_text(margin = margin(t = 12, r = 0, b = 0, l = 0)), #increases distance between x axis label and plot
            panel.background = element_rect(fill = "transparent",colour = 'black', size = 1),
            plot.background = element_rect(fill = "transparent",colour = NA)) +
    scale_x_continuous(breaks=seq(2000,2020,5)) +
    geom_hline(aes(yintercept = 0), size =0.2 ) +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +
    geom_line() +
    #  scale_x_reverse() +
    labs(y = "s'(X)", x = "Year")
  
  dev.off()
  
  #plot 2nd derivatives
  
  ## second derivatives of all smooths using central finite differences with simultaneous CI
  sdCR<-derivatives(fshmod, newdata = newd, eps = EPS, unconditional = UNCONDITIONAL, 
                    order = 2, interval = "simultaneous")
  
  as.data.frame(sdCR)
  
  png(paste('figures/HCC/BLS Fishing', variables[i] ,'SD.png', sep = ' '), width =  2400, height = 1700, #w & h in pixels
      res = 300) #sets high resolution--default is 150
  
  par(family = 'Helvetica',
      bg = NA, #empty background--easier to place on ppt
      mar = c(5.6, 5.7, 4, 2.4) #sets margins (bottom, left, top, right)--default values c(5.1,4.1,4.1,2.1)
  )
  theme_set(theme_bw())
  ggplot(sdCR,
         aes(x = data, y = derivative)) +
    theme(  panel.grid.major = element_blank(), #delete major grid lines
            panel.grid.minor = element_blank(), #delete minor grid lines
            axis.text=element_text(size=17, color = 'black'), #adjust font size of axis tick labels
            axis.title=element_text(size=19.5,face="plain"), #adjust size of axis titles
            axis.title.x = element_text(margin = margin(t = 12, r = 0, b = 0, l = 0)), #increases distance between x axis label and plot
            panel.background = element_rect(fill = "transparent",colour = 'black', size = 1),
            plot.background = element_rect(fill = "transparent",colour = NA)) +
    scale_x_continuous(breaks=seq(2000,2020,5)) +
    geom_hline(aes(yintercept = 0), size = 0.2) +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +
    geom_line() +
    labs(y = 's"(X)', x = "Year")
  #graph is super wiggly for some reason, also the y axis seems way too big??
  dev.off()
  
}

