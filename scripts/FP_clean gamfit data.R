#reorganize gamfit data into dfs for DEA index calculations

####################################################################
#First, clear memory and load needed packages
rm(list=ls())
PKG <- c("plyr","dplyr")

for (p in PKG) {
  if(!require(p,character.only = TRUE)) {  
    install.packages(p)
    require(p,character.only = TRUE)}
}

#Load and clean data
regnames<-c('Alaska','California Current','Gulf of Mexico','Northeast','Southeast',"Hawai'i")

######### FOSS
FOSSvar<-c('Year','Commercial Catch', 'Commercial Revenue', 'Catch Diversity', 'Revenue Diversity')
FOSS1var<-c('Year','CommCatch.Mil', 'CommRev.Mil', 'CommDiv', 'RevDiv')

#combine all vars into 1 df
#created empty df to bind others to
df <- data.frame(matrix(ncol = 2, nrow = 0))
colnames(df) <- c('Year','Region')

for(j in 2:5){
  
  df.new<-read.csv(paste('cross regional data/DEA/FOSS ', FOSSvar[j],' all regions_gamfit.csv', sep=''))
  df.new<-df.new[,c('Year','Region', 'fit')]
  names(df.new)<-c('Year','Region', FOSS1var[j])
  
  df.new$Year<-round(df.new$Year,1)
  df.new<-aggregate(.~Year+Region,df.new,FUN = mean)
  df<-join(df,df.new, type = 'full')
}
write.csv(df,paste('cross regional data/DEA/FOSS combined_gamfit.csv', sep=''), row.names = F)

######### MRIP
MRIPvar<- c('Catch.Thousands', 'Trips.Thousands')
MRIP1var<-c('Catch.Millions', 'Trips.Millions')

df <- data.frame(matrix(ncol = 2, nrow = 0))
colnames(df) <- c('Year','Region')
df0 <- df #second identical df for points

#combine vars into 1 df
for(k in 1:length(MRIPvar)){ #k is Catch Thousands or Trips.Thousands
  df.new<-read.csv(paste('cross regional data/MRIP MC sims/ MC ',MRIPvar[k],' all regions summary gamfit_MRIP.csv', sep=''))
  df.new<-df.new[,c('Year','Region', 'fit')]
  df.new[,3]<-df.new[,3]/1000 #divide values by 1000 so final units are in millions
  names(df.new)<-c('Year','Region', MRIP1var[k])
  df.new$Year<-round(df.new$Year,1)
  df.new<-aggregate(.~Year+Region,df.new,FUN = mean)
  df<-join(df,df.new, type = 'full')
  
  df0.new<-read.csv(paste('cross regional data/MRIP MC sims/MRIP ',MRIPvar[k],'_all regions_raw.csv', sep=''))
  df0.new<-df0.new[,c(1:3)]
  df0.new[,3]<-df0.new[,3]/1000
  names(df0.new)<-c('Year','Region', MRIP1var[k])
  df0<-join(df0,df0.new, type = 'full')
  
}
write.csv(df,paste('cross regional data/DEA/MRIP combined_gamfit.csv', sep=''), row.names = F)
write.csv(df0,paste('cross regional data/DEA/MRIP combined_data points_raw.csv', sep=''), row.names = F)

##### NES
folders<-c('AK','CC','GOM','NE','SE','HI')
files<-c('AK','Cali Current','Gulf of Mexico','NE','Southeast','HI') 
NES1var<-c('1','Establishments', 'Receipts') #for manuscript, probably better to focus on receipts
NESvar<-c('1','NES.Emp.Thousands', 'NES.Receipts.Mil') #rename columns so they're diff from BLS when the dfs are bound together

## combined all regions into 1 df
for(j in 2:3){ #j represents the column number in the NES df (skip 1), NESvar
  #create empty df to bind rest to for gamfit
  df <- data.frame(matrix(ncol = 3, nrow = 0))
  x <- c("Year", NESvar[j], "Region")
  colnames(df) <- x
  
  #create empty df to bind rest to for points
  df0 <- data.frame(matrix(ncol = 3, nrow = 0))
  colnames(df0) <- x
  for(i in 1:length(folders)) { #i represents the folder name and the region name in the file
    
    df1<-read.csv(paste('cross regional data/',folders[i],'/',files[i],' NES combined ', NES1var[j],' gamfit.csv', sep=''))
    NES0<-read.csv(paste('cross regional data/',folders[i],'/',files[i],' NES combined ', NES1var[j],'_data points_raw.csv', sep=''))
    df1<-df1[df1$Sector == 'Combined',]
    df1<-df1[,c('Year', 'fit')]
    names(df1)<-c('Year', NESvar[j])
    df1$Region<- regnames[i]
    df<-rbind(df,df1)
    
    NES0<-NES0[NES0$Sector == 'Combined',]
    df00<-NES0[,c('Year',NES1var[j])]
    names(df00)<-c('Year', NESvar[j])
    df00$Region<- regnames[i]
    df0<-rbind(df0,df00)
    
  }
  write.csv(df, paste('cross regional data/DEA/NES combined ', NES1var[j],' gamfit.csv', sep=''), row.names = F)
  write.csv(df0, paste('cross regional data/DEA/NES combined ', NES1var[j],'_data points_raw.csv', sep=''), row.names = F)
  
}

### combine all NES vars into 1 df
#empty df to bind others to
df <- data.frame(matrix(ncol = 2, nrow = 0))
colnames(df) <- c('Year','Region')

df0 <- df

for(j in 2:3){
  NES0<-read.csv(paste('cross regional data/DEA/NES combined ', NES1var[j],'_data points_raw.csv', sep=''))
  df0<-join(df0,NES0, type = 'full')
  
  NES<-read.csv(paste('cross regional data/DEA/NES combined ', NES1var[j],' gamfit.csv', sep=''))
  NES$Year<-round(NES$Year,1)
  NES<-aggregate(.~Year+Region,NES,FUN = mean)
  
  df<-join(df,NES, type = 'full')
}
write.csv(df0,paste('cross regional data/DEA/NES combined_data points_raw.csv', sep=''), row.names = F)
write.csv(df,paste('cross regional data/DEA/NES combined_gamfit.csv', sep=''), row.names = F)


#######BLS 
folders<-c('AK','CC','GOM','NE','SE','HI')
files<-c('AK','Cali Current','Gulf of Mexico','NE','Southeast','HI') 
regnames<-c('Alaska','California Current','Gulf of Mexico','Northeast','Southeast',"Hawai'i")
BLS1var<-c('1','Establishments', 'Employment','Wages') # name of files 
BLSvar<-c('1','Establishments', 'BLS.Emp.Thousands','BLS.Wages.Millions') # rename columns

# combine all regions into 1 df
for(j in 2:4){ #j represents the column number in the NES df (skip 1), NESvar
  #create empty df to bind rest to for gamfit
  df <- data.frame(matrix(ncol = 3, nrow = 0))
  x <- c("Year", BLSvar[j], "Region")
  colnames(df) <- x
  
  #create empty df to bind rest to for points
  df0 <- data.frame(matrix(ncol = 3, nrow = 0))
  colnames(df0) <- x
  
  for(i in 1:length(folders)) { #i represents the folder name and the region name in the file
    
    df1<-read.csv(paste('cross regional data/',folders[i],'/',files[i],' BLS ',BLS1var[j],' combined gamfit.csv', sep=''))
    BLS0<-read.csv(paste('cross regional data/',folders[i],'/',files[i],' BLS ',BLS1var[j],' combined_points_raw.csv', sep=''))
    
    df1<-df1[df1$Sector == 'Combined',]
    df1<-df1[,c('Year', 'fit')]
    names(df1)<-c('Year', BLSvar[j])
    df1$Region<- regnames[i]
    df<-rbind(df,df1)
    
    BLS0<-BLS0[BLS0$Sector == 'Combined',]
    df00<-BLS0[,c('Year',BLS1var[j])]
    names(df00)<-c('Year', BLSvar[j])
    df00$Region<- regnames[i]
    df0<-rbind(df0,df00)
    
  }
  write.csv(df, paste('cross regional data/DEA/BLS combined ', BLS1var[j],' gamfit.csv', sep=''), row.names = F)
  write.csv(df0, paste('cross regional data/DEA/BLS combined ', BLS1var[j],'_data points_raw.csv', sep=''), row.names = F)
  
}

### combine all BLS vars into 1 df
#empty df to bind others to
df <- data.frame(matrix(ncol = 2, nrow = 0))
colnames(df) <- c('Year','Region')

df0 <- df
for(j in 2:4){ #j represents the column number in the NES df (skip 1), NESvar
  
  BLS0.new<-read.csv(paste('cross regional data/DEA/BLS combined ', BLS1var[j],'_data points_raw.csv', sep=''))
  BLS.new<-read.csv(paste('cross regional data/DEA/BLS combined ', BLS1var[j],' gamfit.csv', sep=''))
  BLS.new$Year<-round(BLS.new$Year,1)
  BLS.new<-aggregate(.~Year+Region,BLS.new,FUN = mean)
  
  df0<-join(df0,BLS0.new, type = 'full')
  df<-join(df,BLS.new, type = 'full')
  
}

write.csv(df0,paste('cross regional data/DEA/BLS combined_data points_raw.csv', sep=''), row.names = F)
write.csv(df,paste('cross regional data/DEA/BLS combined_gamfit.csv', sep=''), row.names = F)
