library(data.table)
library(tidyr)
library(dplyr)



base_year = 2020 # base year for GDP deflator - Maximum = 2020, Minimum = 1947  - max(GDPDEF_quarterly$Year)
# base_yethe base year for adjusting nominal values to real values
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
write.csv(GDPDEF_annual, file = 'scripts/GDPDEF_annual.csv', row.names = F)

#Joining to the file you are needing to deflate
setwd("/Users/lansingperng/Desktop/Research/NOAA/HDWG/HDWG Analyses")

#(1) cross regional FOSS data (pulling from cleaned file with diversity already calculated)
folders<-c('AK','CC','GOM','NE','SE','HI')
files<-c('AK','Cali Current','Gulf of Mexico','NE','Southeast','HI') #file names, some abbreviated because these are the names the original data files were written under
for(i in 1:length(folders)) { #i represents the folder name and the region name in the file
  REVENUEFILE<-read.csv(paste('cross regional data/',folders[i],'/',files[i],' FOSS.csv', sep='')) %>% 
    
    mutate(nominal_revenue = Revenue.Millions) #renaming revenue nominal revenue to keep track
  REVENUEFILE <- as_tibble(merge(REVENUEFILE, GDPDEF_annual, by="Year", all.x=TRUE))
  REVENUEFILE[["Revenue.Millions"]] <- REVENUEFILE[["nominal_revenue"]]*
    unique(GDPDEF_annual$GDPDEF[GDPDEF_annual$Year==base_year])/REVENUEFILE$GDPDEF #deflating
  REVENUEFILE$GDPDEF <- NULL
write.csv(REVENUEFILE,paste('cross regional data/',folders[i],'/',files[i],' FOSS.csv', sep=''), row.names = F) 
}

#(2) HI data- using slightly cleaned up file from cross regional data
FOSS<-read.csv("data/foss_landings and revenue_comm_raw.csv")
FOSS$Dollars <-  as.numeric(gsub(",","",FOSS$Dollars)) #Replace comma with nothing--aka delete commas
FOSS$Pounds <-  as.numeric(gsub(",","",FOSS$Pounds)) #Replace comma with nothing--aka delete commas
FOSS<-FOSS[,c(1,3:5)]
write.csv(FOSS,'data/foss_landings and revenue_cleaned.csv', row.names = F)

  REVENUEFILE<-read.csv("data/foss_landings and revenue_cleaned.csv") %>% 
    mutate(nominal_revenue = Dollars) #renaming revenue nominal revenue to keep track
  REVENUEFILE <- as_tibble(merge(REVENUEFILE, GDPDEF_annual, by="Year", all.x=TRUE))
  REVENUEFILE[["Dollars"]] <- REVENUEFILE[["nominal_revenue"]]*
    unique(GDPDEF_annual$GDPDEF[GDPDEF_annual$Year==base_year])/REVENUEFILE$GDPDEF #deflating
  REVENUEFILE$GDPDEF <- NULL
  write.csv(REVENUEFILE,'data/foss_landings and revenue_deflated.csv', row.names = F) 
  
  
  ############ APPLY DEFLATOR TO BLS WAGES #################
  folders<-c('AK','CC','GOM','NE','SE','HI') #i
  files<-c('AK','Cali Current','Gulf of Mexico','NE','Southeast','HI') #i
  BLSinds<-c('Fishing', 'Seaf Markets', 'Seaf Packaging', 'Seaf Wholesale') #k
  BLSvar<-c('1','Establishments', 'Employment','Wages') # j
  
  GDPDEF_annual<-read.csv('scripts/GDPDEF_annual.csv') 
  base_year = 2020 # base year for GDP deflator - Maximum = 2020, Minimum = 1947  - max(GDPDEF_quarterly$Year)
  #deflated to base year 2020 because it's the newest data year

  for(i in 1:length(folders)) { #i represents the folder name and the region name in the file
    for(k in 1:length(BLSinds)) { # which sector
      REVENUEFILE<-read.csv(paste('cross regional data/',folders[i],'/',files[i],' BLS ',BLSinds[k],' Totals.csv', sep='')) %>% 
        
        mutate(nominal_revenue = Wages.Thousands) #renaming revenue nominal revenue to keep track
      REVENUEFILE <- as_tibble(merge(REVENUEFILE, GDPDEF_annual, by="Year", all.x=TRUE))
      REVENUEFILE[["Wages.Thousands"]] <- REVENUEFILE[["nominal_revenue"]]*
        unique(GDPDEF_annual$GDPDEF[GDPDEF_annual$Year==base_year])/REVENUEFILE$GDPDEF #deflating
      
      if("Wages.se" %in% colnames(REVENUEFILE)) { #also deflate wages.se if it exists--should only be in AK
        REVENUEFILE$nominal_se <- REVENUEFILE$Wages.se #renaming revenue nominal revenue to keep track
        REVENUEFILE[["Wages.se"]] <- REVENUEFILE[["nominal_se"]]*
          unique(GDPDEF_annual$GDPDEF[GDPDEF_annual$Year==base_year])/REVENUEFILE$GDPDEF #deflating
      }
      REVENUEFILE$GDPDEF <- NULL
      write.csv(REVENUEFILE,paste('cross regional data/',folders[i],'/',files[i],' BLS ',BLSinds[k],' Totals_deflated.csv', sep=''), row.names = F)
      
    }
  }
  
  ############ APPLY DEFLATOR TO NES RECEIPTS #################
  folders<-c('AK','CC','GOM','NE','SE','HI') #i
  files<-c('AK','Cali Current','Gulf of Mexico','NE','Southeast','HI') #i
  NESinds<-c('Fishing', 'Seaf Markets', 'Seaf Packaging')
  
  GDPDEF_annual<-read.csv('scripts/GDPDEF_annual.csv') 
  base_year = 2020 # base year for GDP deflator - Maximum = 2020, Minimum = 1947  - max(GDPDEF_quarterly$Year)
  #deflated to base year 2020 because it's the newest data year
  
  for(i in 1:length(folders)) { #i represents the folder name and the region name in the file
    for(k in 1:length(NESinds)) { # which sector
      REVENUEFILE<-read.csv(paste('cross regional data/',folders[i],'/',files[i],' NES ',NESinds[k],' Totals.csv', sep='')) %>% 
        mutate(nominal_revenue = Total.Receipts) #renaming revenue nominal revenue to keep track
      REVENUEFILE <- as_tibble(merge(REVENUEFILE, GDPDEF_annual, by="Year", all.x=TRUE))
      REVENUEFILE[["Total.Receipts"]] <- REVENUEFILE[["nominal_revenue"]]* 
        unique(GDPDEF_annual$GDPDEF[GDPDEF_annual$Year==base_year])/REVENUEFILE$GDPDEF #deflating
      # 'Total.Receipts' set equal to nominal revenue*(base year(2020) deflator/target year deflator)
      REVENUEFILE$GDPDEF <- NULL
      write.csv(REVENUEFILE,paste('cross regional data/',folders[i],'/',files[i],' NES ',NESinds[k],' Totals_deflated.csv', sep=''), row.names = F)
      
    }
  }
  
  
  