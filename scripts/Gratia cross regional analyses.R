####### Dec 2020####
###code by Lansing Perng

rm(list=ls())

PKG <- c("gratia", "mgcv", "tidyr", "dplyr","plyr", "ggplot2")
for (p in PKG) {
  if(!require(p,character.only = TRUE)) {  
    install.packages(p)
    require(p,character.only = TRUE)}
}

## DONT FORGET TO RUN GAM FUNCTION 

##gratia code from Scott with MRIP, NES, BLS, FOSS, overfished stocks, chlorophyll a Data
#project already has wd in the HDWG analyses folder, but we want to read everything
#straight from the 'cross regional data' folder
setwd("/Users/lansingperng/Desktop/Research/NOAA/HDWG/HDWG Analyses")

###############################################################################
########################Separate overfished by Region##########################
###############################################################################
overf0<-read.csv(file = 'cross regional data/overfished stocks.csv')
overf1<-gather(overf0, "year", 'Stocks', 2:21)

#extract certain characters from Description column to create new columns
#extract the 4th to last character to the last character into 'Year' column
overf1$Year<-substr(overf1$year, nchar(overf1$year)-4+1, nchar(overf1$year))
overf<-overf1[,c(4,1,3)]
colnames(overf)<-c('Year', 'Region', 'Stocks')
unique(overf$Region)

overregnames<-c('NE/NE', 'SE/SE','GOM/GOM', 'CC/CC', 'HI/HI', 'AK/AK')
overreglabs<-c('NE','SE', 'GoMex', 'CalCu', 'PI', 'AK/Arctic')


#for loop separating overfished data by region and writing files
for(i in 1:length(overregnames)) {
  over<-overf[overf$Region == overreglabs[i],]
  write.csv(over, paste('cross regional data/', overregnames[i],' overfished.csv', sep = ''), row.names = F)
}


###############################################################################
############################Separate chl a by Region###########################
###############################################################################
chla<-read.csv(file = 'cross regional data/chlorophyll a.csv')
chla<-chla[,c(1,2,5:9)]
colnames(chla)<-c('Year', 'CC', 'AK', 'GOM','HI','NE','SE')

chlaregnames<-c('1', 'CC/CC', 'AK/AK','GOM/GOM', 'HI/HI', 'NE/NE', 'SE/SE')

#for loop separating overfished data by region and writing files
for(i in 2:length(colnames(chla))) {
  chla1<-chla[,c(1,i)]
  colnames(chla1)<-c('Year', 'Chl.a')
  write.csv(chla1, paste('cross regional data/', chlaregnames[i],' chlorophyll a.csv', sep = ''), row.names = F)
}

###############################################################################
############################Aggregate NES to Region############################
###############################################################################

#AK and HI are already regional

Regionslist<-c('CC','GOM','NE/MA','NE/NE','SE')
Regionnames<-c('Cali Current','Gulf of Mexico','Mid Atlantic','New England','Southeast')
NESinds<- c('Fishing', 'Seaf Markets', 'Seaf Packaging')

##write for loop for rest of regions to aggregate in same format as BLS data

for(i in 1:length(Regionslist)) {
  for(j in 1:length(NESinds)) {
      NESregfiles<- list.files(path = paste('cross regional data/' ,Regionslist[i],'', sep = ''), 
                               pattern = paste('*NES ',NESinds[j],'.csv', sep=''), #sep ='' means no spaces will be inserted--had to manually insert one between industry and ind with " "
                               full.names = T)
      df0 <- ldply(NESregfiles, read.csv) #reads in all files and stacks them into one df
      write.csv(df0, paste('cross regional data/' ,Regionslist[i],'/',Regionnames[i],' NES ',NESinds[j]," Totals.csv", sep = ''), row.names = F)
      
  }

}

####join Mid Atlantic and New England NES together for full regional analysis##

#add Subregion column to the MA Aand NE subregions of Northeast and rewrite files with same name
#it's okay if this part gets rerun--the code is set up so that the output file is always the same
NEsubregions<-c('Mid Atlantic', 'New England')

for(i in 1:length(NEsubregions)) {
  for(j in 1:length(NESinds)) {
    NEfiles<- list.files(path = 'cross regional data/NE', 
                             pattern = paste('*NES ',NESinds[j],'*', sep=''), #sep ='' means no spaces will be inserted--had to manually insert one between industry and ind with " "
                             full.names = T)
    
    df<-read.csv(NEfiles[i])
    df$Subregion<-NEsubregions[i]
    df<-df[,c('Year','Subregion','State','Industry','Establishments','Total.Receipts')]
    write.csv(df, NEfiles[i], row.names = F)
  }
}

empstat<-c('NES','BLS')

for(i in 1:length(empstat)) {
  for(j in 1:length(NESinds)) {
    NEfiles<- list.files(path = 'cross regional data/NE', 
                         pattern = paste('*',empstat[i],' ',NESinds[j],'*', sep=''), #sep ='' means no spaces will be inserted--had to manually insert one between industry and ind with " "
                         full.names = T)
    df0 <- ldply(NEfiles, read.csv) #reads in all files and stacks them into one df
    write.csv(df0, paste('cross regional data/NE/NE ',empstat[i],' ',NESinds[j],' Totals.csv', sep = ''), row.names = F)
  }
}


##do BLS Seaf Wholesale separately because there was no Wholesale for NES
NEfiles<- list.files(path = 'cross regional data/NE', 
                     pattern = '*BLS Seaf Wholesale*', #sep ='' means no spaces will be inserted--had to manually insert one between industry and ind with " "
                     full.names = T)
df0 <- ldply(NEfiles, read.csv) #reads in all files and stacks them into one df
write.csv(df0, 'cross regional data/NE/NE BLS Seaf Wholesale Totals.csv', row.names = F)

#############################################in ##################################
#########################overfished data by YEAR###############################
###############################################################################
reglabs<-c('AK','CC','GOM','NE','SE','HI')
regnames<-c('Alaska','California Current','Gulf of Mexico','Northeast','Southeast',"Hawai'i")
reg.k<-c(6,6,8,5,5,4)
reg.sp<-c(.1,0.01,0.001,0.001,0.001,0.0208)


for(i in 1:length(reglabs)) { #i represents the folder name and the region name in the file
  overf<-read.csv(paste('cross regional data/',reglabs[i],'/',reglabs[i],' overfished.csv', sep='')) 
  
    #exploratory plot to see if there are obvious weird values
    plot(Stocks~Year, data = overf) 
    #looks pretty good
    
    gammod<-gam(Stocks ~ s(Year, k = reg.k[i]), sp = reg.sp[i], 
                data = overf, method = 'REML')
    
    print(gammod$sp)
    gam.check(gammod)
    
    pdf(paste('figures/cross regional/',reglabs[i],' Overfished Assumptions.pdf',sep=''), width =  7, height = 5.5 #w & h in inches
      )
    par(mfrow = c(2, 2)) #show all 4 plots on 1 page
    gam.check(gammod) #much better!
    #not sure why but the appraise() plot doesnt come out in the for loop  
    #but the gam.check pdf does
    dev.off()
}


for(i in 1:length(reglabs)) { #i represents the folder name and the region name in the file
  overf<-read.csv(paste('cross regional data/',reglabs[i],'/',reglabs[i],' overfished.csv', sep='')) 
    
  gammod<-gam(Stocks ~ s(Year, k = reg.k[i]), sp = reg.sp[i], 
              data = overf, method = 'REML')
    ####################PLOT GAM##############
    
    png(paste('figures/cross regional/',reglabs[i],' Overfished GAM.png',sep=''), width =  2400, height = 1700, #w & h in pixels
        res = 300) #sets high resolution--default is 150
    
    par(family = 'Helvetica',
        bg = NA, #empty background--easier to place on ppt
        mar = c(5.6, 5.7, 4, 2.4) #sets margins (bottom, left, top, right)--default values c(5.1,4.1,4.1,2.1)
    )
    plot(gammod, residuals = T, pch = 1, #plot points with empty cirles
         seWithMean = TRUE, shade = T, shade.col = 'lightblue', #adds model uncertainty and shades it light blue
         shift = coef(gammod)[1],  #adds model intercept by shifting y axis by first coef
         lwd = 1.4,
         main = paste("Overfished Stocks in ",regnames[i],sep=''),
         font.main = 1, #makes the title regular (default is bold)
         cex.main = 1.6, #expansion factor for title
         ylab = 'Number of Stocks',
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
    newd <- with(overf, data.frame(Year = seq(min(Year), max(Year), 
                                             length = n)))
    
    fd<-derivatives(gammod, newdata = newd, eps = EPS, unconditional = UNCONDITIONAL, n_sim = N, order = 1, interval = "simultaneous")
    as.data.frame(fd)
    
    #plot first derivs
    theme_set(theme_bw())
    ggplot(fd,
           aes(x = data, y = derivative)) +
      theme(  panel.grid.major = element_blank(), #delete major grid lines
              panel.grid.minor = element_blank(), #delete minor grid lines
              axis.text=element_text(size=17, color = 'black'), #adjust font size of axis tick labels
              axis.title=element_text(size=19.5,face="plain"), #adjust size of axis titles
              axis.title.x = element_text(margin = margin(t = 12, r = 0, b = 0, l = 0)), #increases distance between x axis label and plot
              panel.background = element_rect(fill = "transparent",colour = 'black', size = 1),
              plot.background = element_rect(fill = "transparent",colour = NA)) +
      scale_x_continuous(breaks=seq(min(overf$Year),max(overf$Year),5)) +
      geom_hline(aes(yintercept = 0), size =0.2 ) +
      geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +
      geom_line() +
      #  scale_x_reverse() +
      labs(y = "s'(X)", x = "Year") + 
      geom_segment(data=fd, mapping=aes(x=data, y=0, xend=data,
                                        yend=(ifelse(lower>0, lower, ifelse(upper<0, upper, 0)))), # arrow=arrow(),
                   size=0.5, color="indianred3")
    
    ggsave(paste('figures/cross regional/',reglabs[i],' Overfished FD.png', sep=''), 
           width =  8.47, height = 5, units = 'in', #w & h in inches
           dpi = 300, bg = 'transparent')
    
    
    #plot 2nd derivatives
    
    ## second derivatives of all smooths using central finite differences with simultaneous CI
    sdCR<-derivatives(gammod, newdata = newd, eps = EPS, unconditional = UNCONDITIONAL, order = 2, interval = "simultaneous")
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
      scale_x_continuous(breaks=seq(min(overf$Year),max(overf$Year),5)) +
      geom_hline(aes(yintercept = 0), size = 0.2) +
      geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +
      geom_line() +
      labs(y = 's"(X)', x = "Year") + 
      geom_segment(data=sdCR, mapping=aes(x=data, y=0, xend=data,
                                          yend=(ifelse(lower>0, lower, ifelse(upper<0, upper, 0)))), # arrow=arrow(),
                   size=0.5, color="indianred3")
    
    ggsave(paste('figures/cross regional/',reglabs[i],' Overfished SD.png', sep=''), 
           width =  8.47, height = 5, units = 'in', #w & h in inches
           dpi = 300, bg = 'transparent')
  }
#not sure why but the appraise() plot doesnt come out in the for loop

###############################################################################
#######################chlorophyll a data by YEAR##############################
###############################################################################
reglabs<-c('AK','CC','GOM','NE','SE','HI')
regnames<-c('Alaska','California Current','Gulf of Mexico','Northeast','Southeast',"Hawai'i")
reg.k<-c(6,9,7,6,6,4)
reg.sp<-c(.01,0.0001,0.01,0.005,0.1,0.1)

for(i in 1:length(reglabs)) { #i represents the folder name and the region name in the file
  chla<-read.csv(paste('cross regional data/',reglabs[i],'/',reglabs[i],' chlorophyll a.csv', sep='')) 
  
  #exploratory plot to see if there are obvious weird values
  plot(Chl.a~Year, data = chla) 
  #looks pretty good
  
  gammod<-gam(Chl.a ~ s(Year, k = reg.k[i]), sp = reg.sp[i], 
              data = chla, method = 'REML')
  
  print(gammod$sp)
  
  gam.check(gammod)
  
  pdf(paste('figures/cross regional/',reglabs[i],' Chlorophyll a Assumptions.pdf',sep=''), width =  7, height = 5.5 #w & h in inches
  )
  par(mfrow = c(2, 2))
  gam.check(gammod) #much better!
  #not sure why but the appraise() plot doesnt come out in the for loop  
  #but the gam.check pdf does
  dev.off()
  
####################PLOT GAM##############
  
  png(paste('figures/cross regional/',reglabs[i],' chlorophyll a GAM.png',sep=''), width =  2400, height = 1700, #w & h in pixels
      res = 300) #sets high resolution--default is 150
  
  par(family = 'Helvetica',
      bg = NA, #empty background--easier to place on ppt
      mar = c(5.6, 5.7, 4, 2.4) #sets margins (bottom, left, top, right)--default values c(5.1,4.1,4.1,2.1)
  )
  plot(gammod, residuals = T, pch = 1, #plot points with empty cirles
       seWithMean = TRUE, shade = T, shade.col = 'lightblue', #adds model uncertainty and shades it light blue
       shift = coef(gammod)[1],  #adds model intercept by shifting y axis by first coef
       lwd = 1.4,
       main = paste("Chlorophyll a in ",regnames[i],sep=''),
       font.main = 1, #makes the title regular (default is bold)
       cex.main = 1.6, #expansion factor for title
       ylab = 'Annual Average',
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
  newd <- with(chla, data.frame(Year = seq(min(Year), max(Year), 
                                            length = n)))
  
  fd<-derivatives(gammod, newdata = newd, eps = EPS, unconditional = UNCONDITIONAL, n_sim = N, order = 1, interval = "simultaneous")
  as.data.frame(fd)
  
  #plot first derivs
  theme_set(theme_bw())
  ggplot(fd,
         aes(x = data, y = derivative)) +
    theme(  panel.grid.major = element_blank(), #delete major grid lines
            panel.grid.minor = element_blank(), #delete minor grid lines
            axis.text=element_text(size=17, color = 'black'), #adjust font size of axis tick labels
            axis.title=element_text(size=19.5,face="plain"), #adjust size of axis titles
            axis.title.x = element_text(margin = margin(t = 12, r = 0, b = 0, l = 0)), #increases distance between x axis label and plot
            panel.background = element_rect(fill = "transparent",colour = 'black', size = 1),
            plot.background = element_rect(fill = "transparent",colour = NA)) +
    scale_x_continuous(breaks=seq(min(chla$Year),max(chla$Year),4)) +
    geom_hline(aes(yintercept = 0), size =0.2 ) +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +
    geom_line() +
    #  scale_x_reverse() +
    labs(y = "s'(X)", x = "Year") + 
    geom_segment(data=fd, mapping=aes(x=data, y=0, xend=data,
                                      yend=(ifelse(lower>0, lower, ifelse(upper<0, upper, 0)))), # arrow=arrow(),
                 size=0.5, color="indianred3")
  
  ggsave(paste('figures/cross regional/',reglabs[i],' chlorophyll a FD.png', sep=''), 
         width =  8.47, height = 5, units = 'in', #w & h in inches
         dpi = 300, bg = 'transparent')
  
  
  #plot 2nd derivatives
  
  ## second derivatives of all smooths using central finite differences with simultaneous CI
  sdCR<-derivatives(gammod, newdata = newd, eps = EPS, unconditional = UNCONDITIONAL, order = 2, interval = "simultaneous")
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
    scale_x_continuous(breaks=seq(min(chla$Year),max(chla$Year),4)) +
    geom_hline(aes(yintercept = 0), size = 0.2) +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +
    geom_line() +
    labs(y = 's"(X)', x = "Year") + 
    geom_segment(data=sdCR, mapping=aes(x=data, y=0, xend=data,
                                        yend=(ifelse(lower>0, lower, ifelse(upper<0, upper, 0)))), # arrow=arrow(),
                 size=0.5, color="indianred3")
  
  ggsave(paste('figures/cross regional/',reglabs[i],' chlorophyll a SD.png', sep=''), 
         width =  8.47, height = 5, units = 'in', #w & h in inches
         dpi = 300, bg = 'transparent')
}

###############################################################################
###############################FOSS data by YEAR###############################
###############################################################################

################################full data################################
###cleaned in BODI National Data Cleaning.R

folders<-c('AK','CA','GOM','NE','SE','HI') #CA in the 'cross regional data for DEA' folder, but CC elsewhere
folders.saveas<-c('AK','CC','GOM','NE','SE','HI') #fix CA/CC discrepancy for saving df1 csvs line 707
files<-c('AK','Cali Current','Gulf of Mexico','NE','Southeast','HI') #file names, some abbreviated because these are the names the original data files were written under
regnames<-c('Alaska','California Current','Gulf of Mexico','Northeast','Southeast',"Hawai'i")
FOSSvar<-c('Year','Commercial Catch', 'Commercial Revenue', 'Catch Diversity', 'Revenue Diversity')
FOSSlabs<-c('Year','Landings (Millions of Pounds)','Revenue (Millions of Dollars)','Effective Number of Species', 'Revenue Diversity\n (Effective Shannon Diversity)')
#reg.k<-c(8,6,7,7,4,6) #can only set k per region based on the nested for loop

#loop for assumptions
for(i in 1:length(folders)) { #i represents the folder name and the region name in the file
  FOSS<-read.csv(paste('cross regional data for DEA/FOSS commercial fishery data/species aggregated with diversity/FOSS ',folders[i],' with catch and rev Diversity_deflated.csv', sep='')) 
  FOSS$Revenue.Millions<- FOSS$Dollars/1000000
  FOSS$Catch.Millions<- FOSS$Tot_Comm/1000000
  FOSS<-FOSS[,c('Year','Catch.Millions', 'Revenue.Millions', 'CommDiv', 'RevDiv')]
  FOSS$MH<-'All Species'
  

  for(j in 2:5){ #j represents the column number in the FOSS df (skip 1), the variable name(for writing files),and FOSSlabs (y axis labels)
    #exploratory plot to see if there are obvious weird values
    plot(FOSS[,j]~Year, data = FOSS) 
    #looks pretty good
    
    gammod<-gam(FOSS[,j] ~ s(Year), 
                data = FOSS, method = 'REML')
 #   gammod<-gam(FOSS[,j] ~ s(Year,k = (length(FOSS[,1])-1)),
  #              data = FOSS, method = 'REML')
    
    gammod$sp
  #  gam.check(gammod)
    print(summary(gammod))
    
  #  pdf(paste('figures/cross regional/',files[i],' ',FOSSvar[j],' Assumptions.pdf',sep=''), width =  7, height = 5.5 #w & h in inches
  # )
   # par(mfrow = c(2, 2))
    #gam.check(gammod) #much better!
    #not sure why but the appraise() plot doesnt come out in the for loop  
    #but the gam.check pdf does
  #  dev.off()
  }
}
#not sure why but the appraise() plot doesnt come out in the for loop

##### ORIGINAL GAM AND DERIVATIVE PLOTS ##############
############# also includes gamfit calculations
for(i in 1:length(folders)) { #i represents the folder name and the region name in the file
  FOSS<-read.csv(paste('cross regional data for DEA/FOSS commercial fishery data/species aggregated with diversity/FOSS ',folders[i],' with catch and rev Diversity_deflated.csv', sep='')) 
  FOSS$Revenue.Millions<- FOSS$Dollars/1000000
  FOSS$Catch.Millions<- FOSS$Tot_Comm/1000000
  FOSS<-FOSS[,c('Year','Catch.Millions', 'Revenue.Millions', 'CommDiv','RevDiv')]
  FOSS$MH<-'All Species'
  
  for(j in 2:5){ #j represents the column number in the FOSS df (skip 1), the variable name(for writing files),and FOSSlabs (y axis labels)
   gammod<-gam(FOSS[,j] ~ s(Year),   
               data = FOSS, method = 'REML')
#    gammod<-gam(FOSS[,j] ~ s(Year,k = (length(FOSS[,1])-1)),
 #               data = FOSS, method = 'REML')
    
    
    ####################PLOT GAM##############
    
 #   png(paste('figures/cross regional/',files[i],' ',FOSSvar[j],' GAM.png',sep=''), width =  2400, height = 1700, #w & h in pixels
  #      res = 300) #sets high resolution--default is 150
   
   png(paste('figs for slides/FOSS',files[i],' ',FOSSvar[j],' GAM.png',sep=''), width =  2400, height = 1700, #w & h in pixels
       res = 300) #sets high resolution--default is 150
    
    par(family = 'Helvetica',
        bg = NA, #empty background--easier to place on ppt
        mar = c(5.6, 5.7, 4, 2.4) #sets margins (bottom, left, top, right)--default values c(5.1,4.1,4.1,2.1)
    )
    plot(gammod, residuals = T, pch = 1, #plot points with empty cirles
         seWithMean = TRUE, shade = T, shade.col = 'lightblue', #adds model uncertainty and shades it light blue
         shift = coef(gammod)[1],  #adds model intercept by shifting y axis by first coef
         lwd = 1.4,
         main = paste(FOSSvar[j]," in ",regnames[i],sep=''),
         font.main = 1, #makes the title regular (default is bold)
         cex.main = 1.6, #expansion factor for title
         ylab = FOSSlabs[j],
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
    EPS <- 1e-05           # finite difference
    
    ## where are we going to predict at?
    newd <- with(FOSS, data.frame(Year = seq(min(Year), max(Year), 
                                             length = n)))
    
    fd<-derivatives(gammod, newdata = newd, eps = EPS, unconditional = UNCONDITIONAL, n_sim = N, order = 1, interval = "simultaneous")
    as.data.frame(fd)
    
    #plot first derivs
    theme_set(theme_bw())
    ggplot(fd,
           aes(x = data, y = derivative)) +
      theme(  panel.grid.major = element_blank(), #delete major grid lines
              panel.grid.minor = element_blank(), #delete minor grid lines
              axis.text=element_text(size=17, color = 'black'), #adjust font size of axis tick labels
              axis.title=element_text(size=19.5,face="plain"), #adjust size of axis titles
              axis.title.x = element_text(margin = margin(t = 12, r = 0, b = 0, l = 0)), #increases distance between x axis label and plot
              panel.background = element_rect(fill = "transparent",colour = 'black', size = 1),
              plot.background = element_rect(fill = "transparent",colour = NA)) +
      scale_x_continuous(breaks=seq(min(FOSS$Year),2020,10)) +
      geom_hline(aes(yintercept = 0), size =0.2 ) +
      geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +
      geom_line() +
      #  scale_x_reverse() +
      labs(y = "s'(X)", x = "Year") + 
      geom_segment(data=fd, mapping=aes(x=data, y=0, xend=data,
                                        yend=(ifelse(lower>0, lower, ifelse(upper<0, upper, 0)))), # arrow=arrow(),
                   size=0.5, color="indianred3")
    
#    ggsave(paste('figures/cross regional/',files[i],' ',FOSSvar[j],' FD.png', sep=''), 
 #          width =  8.47, height = 5, units = 'in', #w & h in inches
  #         dpi = 300, bg = 'transparent')
    
    ggsave(paste('figs for slides/FOSS ',files[i],' ',FOSSvar[j],' FD.png', sep=''), 
           width =  8.47, height = 5, units = 'in', #w & h in inches
           dpi = 300, bg = 'transparent')
    
    
    #plot 2nd derivatives
    
    ## second derivatives of all smooths using central finite differences with simultaneous CI
    sdCR<-derivatives(gammod, newdata = newd, eps = EPS, unconditional = UNCONDITIONAL, order = 2, interval = "simultaneous")
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
      scale_x_continuous(breaks=seq(min(FOSS$Year),2020,10)) +
      geom_hline(aes(yintercept = 0), size = 0.2) +
      geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +
      geom_line() +
      labs(y = 's"(X)', x = "Year") + 
      geom_segment(data=sdCR, mapping=aes(x=data, y=0, xend=data,
                                          yend=(ifelse(lower>0, lower, ifelse(upper<0, upper, 0)))), # arrow=arrow(),
                   size=0.5, color="indianred3")
    
#    ggsave(paste('figures/cross regional/',files[i],' ',FOSSvar[j],' SD.png', sep=''), 
 #          width =  8.47, height = 5, units = 'in', #w & h in inches
  #         dpi = 300, bg = 'transparent')
    
#    ggsave(paste('figs for slides/FOSS ',files[i],' ',FOSSvar[j],' SD.png', sep=''), 
 #          width =  8.47, height = 5, units = 'in', #w & h in inches
  #         dpi = 300, bg = 'transparent')
    
    ggsave(paste('figs for slides/eps testing/FOSS ',files[i],' ',FOSSvar[j],' SD.png', sep=''), 
           width =  8.47, height = 5, units = 'in', #w & h in inches
           dpi = 300, bg = 'transparent')
    
    ##############gam fit with thresholds and trends marked #################
    #based on derivative calculations, so those calcs needed to be done first
    
    #calculates fit & se
    pred <- predict.gam(gammod, newdata = newd, type = "response", se.fit = T)
    
    #convert data to df for ggplot
    df1<-data.frame('All Species',newd$Year,pred$fit, pred$se.fit)
    colnames(df1) <- c('MH','Year', 'fit', 'se.fit')
    
    #add column to plot only values that represent significant trends calculated by fd
    df1$inc.trend<-ifelse(fd$lower>0, df1$fit, NA) #increasing trends only
    df1$dec.trend<-ifelse(fd$upper<0, df1$fit, NA) #decreasing trends only
    
    #add column to plot only values that occur within threshold region
    df1$threshold<-ifelse(sdCR$lower>0, df1$fit, ifelse(sdCR$upper<0, df1$fit, NA))
    df1$se.threshold<-ifelse(sdCR$lower>0, df1$se.fit, ifelse(sdCR$upper<0, df1$se.fit, NA))
    
    #change column 
    
    write.csv(df1, file=paste('cross regional data/',folders.saveas[i],'/',files[i],' FOSS ' ,FOSSvar[j],' gamfit.csv', sep=''), row.names = F)
    
     }
}
##but this whole loop works!!


##############################################################################
#################RE RUN WITH NO MENHADENS TO COMPARE DIFFERENCE###############
##MH separated out in BODI National Data Cleaning.R
folders<-c('AK','CA','GOM','NE','SE','HI') #CA in the 'cross regional data for DEA' folder, but CC elsewhere
folders.saveas<-c('AK','CC','GOM','NE','SE','HI') #fix CA/CC discrepancy for saving df1 csvs line 707
files<-c('AK','Cali Current','Gulf of Mexico','NE','Southeast','HI') #file names, some abbreviated because these are the names the original data files were written under
regnames<-c('Alaska','California Current','Gulf of Mexico','Northeast','Southeast',"Hawai'i")
FOSSvar<-c('Year','Commercial Catch', 'Commercial Revenue', 'Catch Diversity', 'Revenue Diversity')
FOSSlabs<-c('Year','Landings (Millions of Pounds)','Revenue (Millions of Dollars)','Effective Number of Species', 'Revenue Diversity\n (Effective Shannon Diversity)')
#reg.k<-c(8,6,7,7,4,6) #can only set k per region based on the nested for loop


####assumptions
for(i in 1:length(folders)) { #i represents the folder name and the region name in the file
  FOSS<-read.csv(paste('cross regional data for DEA/FOSS commercial fishery data/species aggregated with diversity/FOSS ',folders[i],' with catch and rev Diversity no MH_deflated.csv', sep='')) 
  FOSS$Revenue.Millions<- FOSS$Dollars/1000000
  FOSS$Catch.Millions<- FOSS$Tot_Comm/1000000
  FOSS<-FOSS[,c('Year','Catch.Millions', 'Revenue.Millions', 'CommDiv', 'RevDiv')]
  FOSS$MH<-'Without Menhadens'
  
  for(j in 2:5){ #j represents the column number in the FOSS df (skip 1), the variable name(for writing files),and FOSSlabs (y axis labels)
    
#    gammod<-gam(FOSS[,j] ~ s(Year,k = (length(FOSS[,1])-1)), 
 #               data = FOSS, method = 'REML')
    gammod<-gam(FOSS[,j] ~ s(Year),
                data = FOSS, method = 'REML')
    
    gammod$sp
    gam.check(gammod)
    summary(gammod)
    k.check(gammod)

 #   write.csv(dfsum, file=paste('cross regional data/',folders.saveas[i],'/',files[i],' FOSS ' ,FOSSvar[j],' no MH_modsum.csv', sep=''))
    
    pdf(paste('figures/cross regional/',files[i],' ',FOSSvar[j],' No MH Assumptions.pdf',sep=''), width =  7, height = 5.5 #w & h in inches
    )
    par(mfrow = c(2, 2))
    gam.check(gammod) #much better!
    #not sure why but the appraise() plot doesnt come out in the for loop  
    #but the gam.check pdf does
    dev.off()
  }
}
#not sure why but the appraise() plot doesnt come out in the for loop

########## original plots (one var per plot) and gamfit #########
for(i in 1:length(folders)) { #i represents the folder name and the region name in the file
  FOSS<-read.csv(paste('cross regional data for DEA/FOSS commercial fishery data/species aggregated with diversity/FOSS ',folders[i],' with catch and rev Diversity no MH_deflated.csv', sep='')) 
  FOSS$Revenue.Millions<- FOSS$Dollars/1000000
  FOSS$Catch.Millions<- FOSS$Tot_Comm/1000000
  FOSS<-FOSS[,c('Year','Catch.Millions', 'Revenue.Millions', 'CommDiv', 'RevDiv')]
  FOSS$MH<-'Without Menhadens'
  
    for(j in 2:5){ #j represents the column number in the FOSS df (skip 1), the variable name(for writing files),and FOSSlabs (y axis labels)
  #  gammod<-gam(FOSS[,j] ~ s(Year,k = (length(FOSS[,1])-1)),
   #             data = FOSS, method = 'REML')
    gammod<-gam(FOSS[,j] ~ s(Year),
                data = FOSS, method = 'REML')
    
    ####################PLOT GAM##############
    
    png(paste('figures/cross regional/',files[i],' ',FOSSvar[j],' No MH GAM.png',sep=''), width =  2400, height = 1700, #w & h in pixels
        res = 300) #sets high resolution--default is 150
    
    par(family = 'Helvetica',
        bg = NA, #empty background--easier to place on ppt
        mar = c(5.6, 5.7, 4, 2.4) #sets margins (bottom, left, top, right)--default values c(5.1,4.1,4.1,2.1)
    )
    plot(gammod, residuals = T, pch = 1, #plot points with empty cirles
         seWithMean = TRUE, shade = T, shade.col = 'lightblue', #adds model uncertainty and shades it light blue
         shift = coef(gammod)[1],  #adds model intercept by shifting y axis by first coef
         lwd = 1.4,
         main = paste(FOSSvar[j]," in ",regnames[i], ' (No Menhadens)',sep=''),
         font.main = 1, #makes the title regular (default is bold)
         cex.main = 1.6, #expansion factor for title
         ylab = FOSSlabs[j],
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
    
    fd<-derivatives(gammod, newdata = newd, eps = EPS, unconditional = UNCONDITIONAL, n_sim = N, order = 1, interval = "simultaneous")
    as.data.frame(fd)
    
    #plot first derivs
    theme_set(theme_bw())
    ggplot(fd,
           aes(x = data, y = derivative)) +
      theme(  panel.grid.major = element_blank(), #delete major grid lines
              panel.grid.minor = element_blank(), #delete minor grid lines
              axis.text=element_text(size=17, color = 'black'), #adjust font size of axis tick labels
              axis.title=element_text(size=19.5,face="plain"), #adjust size of axis titles
              axis.title.x = element_text(margin = margin(t = 12, r = 0, b = 0, l = 0)), #increases distance between x axis label and plot
              panel.background = element_rect(fill = "transparent",colour = 'black', size = 1),
              plot.background = element_rect(fill = "transparent",colour = NA)) +
      scale_x_continuous(breaks=seq(min(FOSS$Year),2020,10)) +
      geom_hline(aes(yintercept = 0), size =0.2 ) +
      geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +
      geom_line() +
      #  scale_x_reverse() +
      labs(y = "s'(X)", x = "Year") + 
      geom_segment(data=fd, mapping=aes(x=data, y=0, xend=data,
                                        yend=(ifelse(lower>0, lower, ifelse(upper<0, upper, 0)))), # arrow=arrow(),
                   size=0.5, color="indianred3")
    
    ggsave(paste('figures/cross regional/',files[i],' ',FOSSvar[j],' No MH FD.png', sep=''), 
           width =  8.47, height = 5, units = 'in', #w & h in inches
           dpi = 300, bg = 'transparent')
    
    
    #plot 2nd derivatives
    
    ## second derivatives of all smooths using central finite differences with simultaneous CI
    sdCR<-derivatives(gammod, newdata = newd, eps = EPS, unconditional = UNCONDITIONAL, order = 2, interval = "simultaneous")
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
      scale_x_continuous(breaks=seq(min(FOSS$Year),2020,10)) +
      geom_hline(aes(yintercept = 0), size = 0.2) +
      geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +
      geom_line() +
      labs(y = 's"(X)', x = "Year") + 
      geom_segment(data=sdCR, mapping=aes(x=data, y=0, xend=data,
                                          yend=(ifelse(lower>0, lower, ifelse(upper<0, upper, 0)))), # arrow=arrow(),
                   size=0.5, color="indianred3")
    
    ggsave(paste('figures/cross regional/',files[i],' ',FOSSvar[j],' No MH SD.png', sep=''), 
           width =  8.47, height = 5, units = 'in', #w & h in inches
           dpi = 300, bg = 'transparent')
    
    ##############gam fit with thresholds and trends marked #################
    #based on derivative calculations, so those calcs needed to be done first
    
    #calculates fit & se
    pred <- predict.gam(gammod, newdata = newd, type = "response", se.fit = T)
    
    #convert data to df for ggplot
    df1<-data.frame('Without Menhadens',newd$Year,pred$fit, pred$se.fit)
    colnames(df1) <- c('MH','Year', 'fit', 'se.fit')
    
    #add column to plot only values that represent significant trends calculated by fd
    df1$inc.trend<-ifelse(fd$lower>0, df1$fit, NA) #increasing trends only
    df1$dec.trend<-ifelse(fd$upper<0, df1$fit, NA) #decreasing trends only
    
    #add column to plot only values that occur within threshold region
    df1$threshold<-ifelse(sdCR$lower>0, df1$fit, ifelse(sdCR$upper<0, df1$fit, NA))
    df1$se.threshold<-ifelse(sdCR$lower>0, df1$se.fit, ifelse(sdCR$upper<0, df1$se.fit, NA))
    
    write.csv(df1, file=paste('cross regional data/',folders.saveas[i],'/',files[i],' FOSS ' ,FOSSvar[j],' no MH gamfit.csv', sep=''), row.names = F)
    
  }
}

###############COMBINE TO MAKE PLOTS THAT SHOW BOTH WITH AND WITHOUT MH###############
folders<-c('AK','CA','GOM','NE','SE','HI') #CA in the 'cross regional data for DEA' folder, but CC elsewhere
folders.saveas<-c('AK','CC','GOM','NE','SE','HI') #fixed CA/CC discrepancy in 'cross regional data' folder
files<-c('AK','Cali Current','Gulf of Mexico','NE','Southeast','HI') #file names, some abbreviated because these are the names the original data files were written under
regnames<-c('Alaska','California Current','Gulf of Mexico','Northeast','Southeast',"Hawai'i")
FOSSvar<-c('Year','Commercial Catch', 'Commercial Revenue', 'Catch Diversity', 'Revenue Diversity')
FOSSlabs<-c('Year','Landings (Millions of Pounds)','Revenue (Millions of Dollars)','Effective Number of Species', 'Revenue Diversity\n (Effective Shannon Diversity)')

for(i in 1:length(folders)) { #i represents the folder name and the region name in the file
  #read in original data (not gamfit) for plotting points
  FOSS0<-read.csv(paste('cross regional data for DEA/FOSS commercial fishery data/species aggregated with diversity/FOSS ',folders[i],' with catch and rev Diversity_deflated.csv', sep='')) 
  FOSS0$Revenue.Millions<- FOSS0$Dollars/1000000
  FOSS0$Catch.Millions<- FOSS0$Tot_Comm/1000000
  FOSS0<-FOSS0[,c('Year','Catch.Millions', 'Revenue.Millions', 'CommDiv', 'RevDiv')]
  FOSS0$MH<-'All Species'
  
  write.csv(FOSS0,paste('cross regional data/FOSS combined regions/',folders.saveas[i],' FOSS data points_raw.csv', sep=''), row.names = F) 
  
  FOSS0.n<-read.csv(paste('cross regional data for DEA/FOSS commercial fishery data/species aggregated with diversity/FOSS ',folders[i],' with catch and rev Diversity no MH_deflated.csv', sep='')) 
  FOSS0.n$Revenue.Millions<- FOSS0.n$Dollars/1000000
  FOSS0.n$Catch.Millions<- FOSS0.n$Tot_Comm/1000000
  FOSS0.n<-FOSS0.n[,c('Year','Catch.Millions', 'Revenue.Millions', 'CommDiv', 'RevDiv')]
  FOSS0.n$MH<-'Without Menhadens'
  
  FOSS0<-rbind(FOSS0,FOSS0.n) #final df for points
  
  for(j in 2:5){ #j represents the variable name (FOSSvar),and FOSSlabs (y axis labels)
    #gamfit dfs saved in 'cross regional data' folder, call 'CC' for cali current (folders.saveas)
    FOSS.noMH<-read.csv(paste('cross regional data/',folders.saveas[i],'/',files[i],' FOSS ' ,FOSSvar[j],' no MH gamfit.csv', sep='')) 
    FOSS<-read.csv(paste('cross regional data/',folders.saveas[i],'/',files[i],' FOSS ' ,FOSSvar[j],' gamfit.csv', sep='')) 
    
    df1<-rbind(FOSS.noMH, FOSS) #stacks dfs
    df1 <- df1[,colSums(is.na(df1))<nrow(df1)] #if any trend or threshold columns are all NA, delete
    
    #reorder MH factor so that 'All Species' layers on top of 'Without Menhadens'
    #All Species is plotted first by default (alphabetically)
    df1$MH <- factor(df1$MH, levels = c("Without Menhadens", "All Species"))

    #GAM PLOT WITH ALL SPECIES AND NO MH
    # colorblind friendly palette with black:
    cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
    
    ##plot gam
    ggplot(df1,
           aes(x = Year, y = fit)) +
      theme(  plot.title=element_text(hjust=0.5, size = 24),
              panel.grid.major = element_blank(), #delete major grid lines
              panel.grid.minor = element_blank(), #delete minor grid lines
              axis.text=element_text(size=16, color = 'black'), #adjust font size of axis tick labels
              axis.title=element_text(size=20,face="plain"), #adjust size of axis titles
              axis.title.x = element_text(margin = margin(t = 12, r = 0, b = 0, l = 0)), #increases distance between x axis label and plot
              panel.background = element_rect(fill = "transparent",colour = 'black', size = 1),
              plot.background = element_rect(fill = "transparent",colour = NA),
              plot.margin = unit(c(0.3,2,0.3,0.3), "cm"),
              legend.position = 'right', # c(0.37,0.57),
              legend.background = element_rect(fill="transparent", color = 'black'),
              legend.key = element_rect(fill = "transparent", colour = NA),
              legend.key.size = unit(1, 'cm'),
              legend.spacing.y = unit(0.2, 'cm'),
              legend.text=element_text(size=17),
              legend.title=element_blank(),
              legend.margin = margin(2,12,8,10)) +
      guides(fill=guide_legend(byrow = T)) +
      scale_x_continuous(breaks=seq(round_any(min(df1$Year),10, f = floor),round_any(max(df1$Year),10, f = ceiling),10)) +
  #    scale_y_continuous(limits = c(0, round_any(max(df1$fit+(2*df1$se.fit)),round_any(max(df1$fit+(2*df1$se.fit))/5,50,f =ceiling) , f = ceiling)), position = 'left')+
      scale_y_continuous(limits = c(0, round_any(max(FOSS0[j]),5,f = ceiling)), position = 'left')+
      geom_ribbon(aes(ymin = fit-(2*se.fit), ymax = fit+(2*se.fit), fill = MH)) + #95CI = (2*SE) for gam function
      scale_fill_manual(labels = c("Without Menhadens", "All Species"),
                        values = scales::alpha(c('grey80','gray68'),c(0.8,0.8))) + #color combined CI, individual sectors are gray
      {if("threshold" %in% colnames(df1))
        geom_ribbon(aes(ymin = threshold-(2*se.threshold), ymax = threshold+(2*se.threshold), group = MH), 
                    alpha = 0.8, fill = 'grey45')} + #recolor se band for threshold regions
      geom_line(size = 1, aes(group = MH, color = MH), alpha = 1) + #full gam function
      scale_color_manual(labels = c("Without Menhadens", "All Species"), 
                         values = scales::alpha(c('gray60','dodgerblue4'),c(0.5,1))) +
      {if("threshold" %in% colnames(df1)) 
        geom_line(aes(x=Year, y = threshold, group = MH), size = 1.1)} + #plot threshold regions
      {if("inc.trend" %in% colnames(df1)) 
        geom_line(aes(x=Year, y = inc.trend, group = MH),size = 2.1, color = "#009E73", alpha = 0.6)}  +  #increasing trends
      { if("dec.trend" %in% colnames(df1))
        geom_line(aes(x=Year, y = dec.trend, group = MH),size = 2.1, color = "#D55E00", alpha = 0.6) } +
      geom_point(data = FOSS0,aes(x = Year, y = unlist(FOSS0[j]), color = MH), pch =1, size = 1.2,alpha =1) + # data points
      labs(y = paste(FOSSlabs[j]), x = "Year", title = paste(regnames[i]))  
    
    ggsave(paste('figures/cross regional/MH plots/',files[i],' FOSS ' ,FOSSvar[j],' Combined GAM_ggplot.png',sep=''), 
           width =  10, height = 6, units = 'in', #w & h in inches
           dpi = 300, bg = 'transparent')
  }
}

###################### PLOT ALL REGIONS COMBINED--ALL SPECIES ONLY ##################
folders<-c('AK','CA','GOM','NE','SE','HI') #CA in the 'cross regional data for DEA' folder, but CC elsewhere
folders.saveas<-c('AK','CC','GOM','NE','SE','HI') #fixed CA/CC discrepancy in 'cross regional data' folder
files<-c('AK','Cali Current','Gulf of Mexico','NE','Southeast','HI') #file names, some abbreviated because these are the names the original data files were written under
regnames<-c('Alaska','California Current','Gulf of Mexico','Northeast','Southeast',"Hawai'i")
FOSSvar<-c('Year','Commercial Catch', 'Commercial Revenue', 'Catch Diversity', 'Revenue Diversity')
FOSSlabs<-c('Year','Landings (Millions of Pounds)','Revenue (Millions of Dollars)','Effective Number of Species', 'Revenue Diversity\n (Effective Shannon Diversity)')

#code already in MH splitting section but temporarily re-run here to avoid re-running the whole MH loop
for(i in 1:length(folders)) { #i represents the folder name and the region name in the file
  #read in original data (not gamfit) for plotting points
  FOSS0<-read.csv(paste('cross regional data for DEA/FOSS commercial fishery data/species aggregated with diversity/FOSS ',folders[i],' with catch and rev Diversity_deflated.csv', sep='')) 
  FOSS0$Revenue.Millions<- FOSS0$Dollars/1000000
  FOSS0$Catch.Millions<- FOSS0$Tot_Comm/1000000
  FOSS0<-FOSS0[,c('Year','Catch.Millions', 'Revenue.Millions','CommDiv', 'RevDiv')]
  
  write.csv(FOSS0,paste('cross regional data/FOSS combined regions/',folders.saveas[i],' FOSS data points_raw.csv', sep=''), row.names = F) 
}

###compile all regions 
#first read in region 1 for base df to compile onto (AK)

FOSS0<-read.csv(paste('cross regional data/FOSS combined regions/',folders.saveas[1],' FOSS data points_raw.csv', sep='')) 
FOSS0$Region<-regnames[1]

for(i in 2:length(folders)) { #i represents the folder name and the region name in the file
  df0<-read.csv(paste('cross regional data/FOSS combined regions/',folders.saveas[i],' FOSS data points_raw.csv', sep='')) 
  df0$Region<-regnames[i]
  FOSS0<-bind_rows(FOSS0,df0) #use dplyr::bind_rows because rbind can't handle the non-matching columns
  #bind rows just fills in NAs for a column that doesn't exist in one of this original dfs
}
write.csv(FOSS0,'cross regional data/FOSS combined regions/FOSS all regions data points_raw.csv',row.names = F) 


  for(j in 2:5){ #j represents the variable name (FOSSvar),and FOSSlabs (y axis labels)
    FOSS<-read.csv(paste('cross regional data/',folders.saveas[1],'/',files[1],' FOSS ' ,FOSSvar[j],' gamfit.csv', sep='')) 
    FOSS$Region<-regnames[1]
    for(i in 2:length(folders)) { #i represents the folder name and the region name in the file
  df<-read.csv(paste('cross regional data/',folders.saveas[i],'/',files[i],' FOSS ' ,FOSSvar[j],' gamfit.csv', sep='')) 
  df$Region<-regnames[i]
  FOSS<-bind_rows(FOSS,df)
  
    }
    write.csv(FOSS,paste('cross regional data/FOSS combined regions/FOSS ',FOSSvar[j],' all regions_gamfit.csv',sep =''),row.names = F) 
}

################ PLOT ONE PER VAR WITH ALL REGIONS COMBINED ########################

FOSSvar<-c('Year','Commercial Catch', 'Commercial Revenue', 'Catch Diversity', 'Revenue Diversity')
FOSSlabs<-c('Year','Landings (Millions of Pounds)','Revenue (Millions of Dollars)','Effective Number of Species', 'Revenue Diversity\n (Effective Shannon Diversity)')
FOSStitle<-c('1','Commercial Fishing Landings', 'Commercial Fishing Revenue', 'Catch Diversity', 'Revenue Diversity')

FOSS0<-read.csv('cross regional data/FOSS combined regions/FOSS all regions data points_raw.csv') 
str(FOSS0) #'Region' is character and not factor

# set Region as factor and reorder according to how i want it to be plotted
# order is highest to lowest, as well as geographically clockwise 
FOSS0$Region <- factor(FOSS0$Region, levels = c("Northeast", "Southeast", 'Gulf of Mexico', 'California Current', "Hawai'i", 'Alaska'))

for(j in 2:5) { #j is FOSS vars, skip 'Year' column
  df1<-read.csv(paste('cross regional data/FOSS combined regions/FOSS ',FOSSvar[j],' all regions_gamfit.csv',sep ='')) 
  FOSSvar[j]
  #leave 'Region' and 'Year' alone
  str(df1) 
  
  df1$Region <- factor(df1$Region, levels = c("Northeast", "Southeast", 'Gulf of Mexico', 'California Current', "Hawai'i", 'Alaska'))
  
  # colorblind friendly palette with black:
  cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
  
  ############### plot gam ##################
  
  ggplot(df1,
         aes(x = Year, y = fit)) +
    theme(  plot.title=element_text(hjust=0.5, size = 16),
            panel.grid.major = element_blank(), #delete major grid lines
            panel.grid.minor = element_blank(), #delete minor grid lines
            axis.text=element_text(size=24, color = 'black'), #adjust font size of axis tick labels
            axis.title=element_text(size=27,face="plain"), #adjust size of axis titles
            axis.title.x = element_text(margin = margin(t = 12, r = 0, b = 0, l = 0)), #increases distance between x axis label and plot
            panel.background = element_rect(fill = "transparent",colour = 'black', size = 1),
            plot.background = element_rect(fill = "transparent",colour = NA),
            plot.margin = unit(c(0.3,2,0.3,0.3), "cm"),
            legend.position = 'right', # c(0.37,0.57),
            legend.background = element_rect(fill="transparent", color = 'black'),
            legend.key = element_rect(fill = "transparent", color = NA),
            legend.key.size = unit(1, 'cm'),
            legend.spacing.y = unit(0.2, 'cm'),
            legend.text=element_text(size=17),
            legend.title=element_blank(),
            legend.margin = margin(5,13,10,11)) +
    guides(fill = guide_legend(byrow = T)) + # needed for legend.spacing to work
    scale_x_continuous(breaks=seq(1950,2020,5), #so that a tick appears every 5 years--default ticks only where breaks are assigned
                       labels = c("1950",'',"1960",'',"1970",'',"1980",'',"1990",'',"2000",'','2010','','2020')) +
    scale_y_continuous(limits = c(0, round_any(max(max(FOSS0[,j]),max(df1$fit+2*df1$se.fit)),10, f = ceiling)), #0 to max point (points go further than CI band)
                       position = 'left')+
 #   scale_y_continuous(limits = c(min(FOSS0[,j]),max(FOSS0[,j]))) + # to just get the plot for a specific region
    geom_ribbon(aes(ymin = fit-(2*se.fit), ymax = fit+(2*se.fit), fill = Region), alpha = 0.3, color = 'gray60') + #95CI = (2*SE) for gam function
    scale_fill_manual(values = cbbPalette[c(2,3,5,6,8,7)]) + #color combined CI, individual Regions are gray
    {if("threshold" %in% colnames(df1))
      geom_ribbon(aes(ymin = threshold-(2*se.threshold), ymax = threshold+(2*se.threshold), group = Region), 
                  alpha = 0.6, fill = 'gray40', color = 'black')} + #recolor se band for threshold regions
    geom_line(size = 0.8, aes(color = Region)) + #full gam function
    scale_color_manual(values = cbbPalette[c(2,3,5,6,8,7)]) +
    geom_point(data = FOSS0,aes(x = Year, y = unlist(FOSS0[,j]), color = Region, shape = Region), size = 2.7, alpha = 1, fill = NA) + # data points
    scale_shape_manual(values = c(2,4,1,0,5,6)) +
    {if("inc.trend" %in% colnames(df1)) 
      geom_line(aes(x=Year, y = inc.trend, group = Region),size = 1.8, color = "#009E73", alpha = 0.6)}  +  #increasing trends
    { if("dec.trend" %in% colnames(df1))
      geom_line(aes(x=Year, y = dec.trend, group = Region),size = 1.8, color = "#D55E00", alpha = 0.6) } +
    labs(y = paste(FOSSlabs[j]), x = "Year"
         #, title = paste(FOSStitle[j],' in All IEA Regions',sep='') #Get rid of title for manuscript
         )   
  
#  ggsave(paste('figures/cross regional/FOSS all regions combined/FOSS ' ,FOSSvar[j],' all regions GAM_ggplot.png',sep=''), 
 #        width =  11.5, height = 7.5, units = 'in', #w & h in inches
  #       dpi = 300, bg = 'transparent')
  
  ggsave(paste('figs for slides/FOSS ' ,FOSSvar[j],' all regions GAM_ggplot_wide.png',sep=''), 
         width =  16.2, height = 7.5, units = 'in', #w & h in inches
         dpi = 300, bg = 'transparent')
  
#    ggsave(paste('figures/cross regional/FOSS all regions combined/FOSS ' ,FOSSvar[j],' HI and SE GAM_ggplot.png',sep=''), 
 #         width =  11.5, height = 5, units = 'in', #w & h in inches
  #       dpi = 300, bg = 'transparent')
}

################ REPLOT WITH JUST SE AND HI FOR ZOOM IN #####################
FOSSvar<-c('Year','Commercial Catch', 'Commercial Revenue', 'Catch Diversity', 'Revenue Diversity')
FOSSlabs<-c('Year','Landings (Millions of Pounds)','Revenue (Millions of Dollars)','Effective Number of Species', 'Revenue Diversity\n (Effective Shannon Diversity)')
FOSStitle<-c('1','Commercial Fishing Landings', 'Commercial Fishing Revenue', 'Catch Diversity', 'Revenue Diversity')

FOSS0<-read.csv('cross regional data/FOSS combined regions/FOSS all regions data points_raw.csv') 
#FOSScut<-FOSS0[FOSS0$Region == 'Southeast' |
 #                 FOSS0$Region == "Hawai'i",]
FOSScutSE<-FOSS0[FOSS0$Region == 'Southeast' |
                 FOSS0$Region == "Hawai'i",]

#southeast only for slides
FOSScut<-FOSS0[FOSS0$Region == 'Southeast',]

#HI only for slides
#FOSScut<-FOSS0[FOSS0$Region == "Hawai'i",]
#AK only for slides
#FOSScut<-FOSS0[FOSS0$Region == "Alaska",]
#NE, SE, GOM for comm v. rec slide
#FOSScut<-FOSS0[FOSS0$Region == 'Northeast' |
 #                FOSS0$Region == 'Southeast' |
  #               FOSS0$Region == "Gulf of Mexico",]

str(FOSScut) #'Region' is character and not factor

# set Region as factor and reorder according to how i want it to be plotted
# order is highest to lowest, as well as geographically clockwise 
#FOSScut$Region <- factor(FOSScut$Region, levels = c( "Southeast", "Hawai'i"))

for(j in 2:3) { #j is FOSS vars, skip 'Year' column
  df1<-read.csv(paste('cross regional data/FOSS combined regions/FOSS ',FOSSvar[j],' all regions_gamfit.csv',sep ='')) 
#  df1cut<-df1[df1$Region == 'Southeast' |
 #               df1$Region == "Hawai'i",]
#  df1cut<-df1[df1$Region == 'Northeast' |
 #               df1$Region == 'Southeast' |
  #              df1$Region == "Gulf of Mexico",]  
  df1cut<-df1[df1$Region == 'Southeast',]
#  df1cut<-df1[df1$Region == "Hawai'i",]
#  df1cut<-df1[df1$Region == "Alaska",]

  FOSSvar[j]
  #leave 'Region' and 'Year' alone
  str(df1cut) 
  
#  df1cut$Region <- factor(df1cut$Region, levels = c( "Southeast", "Hawai'i"))

  # colorblind friendly palette with black:
  cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

  ############### plot gam ##################
  
  ggplot(df1cut,
         aes(x = Year, y = fit)) +
    theme(  plot.title=element_text(hjust=0.5, size = 16),
            panel.grid.major = element_blank(), #delete major grid lines
            panel.grid.minor = element_blank(), #delete minor grid lines
            axis.text=element_text(size=24, color = 'black'), #adjust font size of axis tick labels
            axis.title=element_text(size=27,face="plain"), #adjust size of axis titles
            axis.title.x = element_text(margin = margin(t = 12, r = 0, b = 0, l = 0)), #increases distance between x axis label and plot
            panel.background = element_rect(fill = "transparent",colour = 'black', size = 1),
            plot.background = element_rect(fill = "transparent",colour = NA),
            plot.margin = unit(c(0.3,2,0.3,0.3), "cm"),
            legend.position = 'right', # c(0.37,0.57),
            legend.background = element_rect(fill="transparent", color = 'black'),
            legend.key = element_rect(fill = "transparent", color = NA),
            legend.key.size = unit(1, 'cm'),
            legend.spacing.y = unit(0.2, 'cm'),
            legend.text=element_text(size=17),
            legend.title=element_blank(),
            legend.margin = margin(5,13,10,11)) +
    guides(fill = guide_legend(byrow = T)) + # needed for legend.spacing to work
    scale_x_continuous(limits= c(1950,2019),breaks=seq(1950,2020,5), #so that a tick appears every 5 years--default ticks only where breaks are assigned
                       labels = c("1950",'',"1960",'',"1970",'',"1980",'',"1990",'',"2000",'','2010','','2020')) +
    scale_y_continuous(limits = c(0, round_any(max(FOSScutSE[,j]),10,f = ceiling)), #0 to max point (points go further than CI band)
                       position = 'left')+ #for lower (zoom) panel
#    scale_y_continuous(limits = c(0, round_any(max(max(FOSScut[,j]),max(df1$fit+2*df1$se.fit)),10, f = ceiling)), #0 to max point (points go further than CI band)
 #                      position = 'left')+ #for upper (full) panel
    geom_ribbon(aes(ymin = fit-(2*se.fit), ymax = fit+(2*se.fit), fill = Region), alpha = 0.3, color = 'gray60') + #95CI = (2*SE) for gam function
#    scale_fill_manual(values = c(cbbPalette[c(8,3)])) + #SE & HI
#    scale_fill_manual(values = c(cbbPalette[c(5,2,3)])) + #NE, SE, GOM for plot to compare to rec
#     scale_fill_manual(values = c(cbbPalette[c(8)])) + #HI
     scale_fill_manual(values = c(cbbPalette[c(3)])) + #SE
#    scale_fill_manual(values = c(cbbPalette[c(7)])) + #AK
    {if("threshold" %in% colnames(df1cut))
      geom_ribbon(aes(ymin = threshold-(2*se.threshold), ymax = threshold+(2*se.threshold), group = Region), 
                   alpha = 0.6, fill = 'gray40', color = 'black')} + #recolor se band for threshold regions
    geom_line(size = 0.8, aes(color = Region)) + #full gam function
#    scale_color_manual(values = c(cbbPalette[c(8,3)])) + #SE & HI
#    scale_color_manual(values = c(cbbPalette[c(5,2,3)])) + #NE, SE, GOM for plot to compare to rec
#     scale_color_manual(values = c(cbbPalette[c(8)])) + #HI
     scale_color_manual(values = c(cbbPalette[c(3)])) + #SE
#    scale_color_manual(values = c(cbbPalette[c(7)])) + #AK
    geom_point(data = FOSScut,aes(x = Year, y = unlist(FOSScut[,j]), color = Region, shape = Region), size = 2.7, alpha = 1, fill = NA) + # data points
#    scale_shape_manual(labels = c('Gulf of Mexico', 'California Current'), #mislabeled but need longest reg name CC in legend to keep plot sizes consistent when making multi-panel plots
 #                      values = c(4,5)) + #SE & HI
#    scale_shape_manual(labels = c('Gulf of Mexico', 'Northeast', 'California Current'),
 #                      values = c(1,2,4)) + #NE, SE, GOM for plot to compare to rec
#     scale_shape_manual(labels = c('California Current'),
 #                       values = c(5)) + #HI
     scale_shape_manual(labels = c('California Current'),
                       values = c(4)) + #SE
#    scale_shape_manual(labels = c('California Current'), #put longest label so that output plot area width is equal
 #                      values = c(6)) + #AK
    {if("inc.trend" %in% colnames(df1cut)) 
      geom_line(aes(x=Year, y = inc.trend, group = Region),size = 1.8, color = "#009E73", alpha = 0.6)}  +  #increasing trends
    { if("dec.trend" %in% colnames(df1cut))
      geom_line(aes(x=Year, y = dec.trend, group = Region),size = 1.8, color = "#D55E00", alpha = 0.6) } +
    labs(y = FOSSlabs[j], x = "Year")   
  
#  ggsave(paste('figures/cross regional/FOSS all regions combined/FOSS ' ,FOSSvar[j],' HI and SE GAM_ggplot.png',sep=''), 
 #        width =  11.5, height = 4.8, units = 'in', #w & h in inches
  #       dpi = 300, bg = 'transparent')
  
#  ggsave(paste('figs for slides/FOSS ' ,FOSSvar[j],' HI and SE GAM_ggplot_wide.png',sep=''), 
 #        width =  16.2, height = 4.8, units = 'in', #w & h in inches
  #       dpi = 300, bg = 'transparent')
  
#  ggsave(paste('figs for slides/FOSS ' ,FOSSvar[j],' HI GAM_ggplot.png',sep=''), 
 #        width =  11.5, height = 4.8, units = 'in', #w & h in inches
  #       dpi = 300, bg = 'transparent')
#  ggsave(paste('figs for slides/FOSS ' ,FOSSvar[j],' AK GAM_ggplot.png',sep=''), 
 #        width =  11.5, height = 7.5, units = 'in', #w & h in inches
  #       dpi = 300, bg = 'transparent')
#  ggsave(paste('figs for slides/FOSS ' ,FOSSvar[j],' rec compare GAM_ggplot_wide.png',sep=''), 
 #         width =  16.2, height = 7.5, units = 'in', #w & h in inches
  #       dpi = 300, bg = 'transparent')
  ggsave(paste('figs for slides/FOSS ' ,FOSSvar[j],' SE GAM_ggplot_wide.png',sep=''), 
         width =  16.2, height = 4.8, units = 'in', #w & h in inches
         dpi = 300, bg = 'transparent')
}

##############################################################################
##############################MRIP by YEAR####################################
##############################################################################

#MRIP data cleaned in 'BODI National Data Cleaning.R' for the cross-regional BODI analysis
#revised to include PSE for monte carlo simulations here

folders<-c('AK','CC','GOM','NE','SE','HI')
regnames<-c('Alaska','California Current','Gulf of Mexico','Northeast','Southeast',"Hawai'i")

MRIPvar<-c('1', 'Recreational Catch','Recreational Angler Trips')
MRIPlabs<-c('1','Recreational Landings (Thousands of Pounds)', 'Recreational Fishing Effort (Thousands of Trips)')
#reg.k<-c(0,8,4,6,4,9) #can only set k per region based on the nested for loop
#reg.sp<-c(0.005,0.001,0.006,0.008,0.002,0.002) #can only set sp per region based on the nested for loop

#################fill in missing SEs for MC sims###############
############################################################### 

# for loops to fill in missing SEs for both Trips & Catch for all regions
# do alaska separately because the MRIP data had no SEs for AK
# filled in AK SEs conservatively using SEs of all other regions combined
#skip to 995 to run MC sims--reads in files with filled in SEs written by this section
#skip to 1023 to plot MC sims
########################### ALASKA #############################
##Alaska has no SEs--> use conservative estimates from SE prediction in all regions combined
#create one dataframe each for Catch and Trips with all regions (except AK) combined
dfTrips<-data.frame(matrix(ncol = 4, nrow = 0))
dfCatch<-data.frame(matrix(ncol = 4, nrow = 0))
for(i in 2:length(folders)) { #i represents the folder name and the region name in the file
  MRIP.Trips<-read.csv(paste('cross regional data/',folders[i],'/',folders[i],' MRIP Trips_SE.csv', sep='')) 
  colnames(dfTrips)<-colnames(MRIP.Trips)
  dfTrips<-rbind(dfTrips, MRIP.Trips)
  
  MRIP.Catch<-read.csv(paste('cross regional data/',folders[i],'/',folders[i],' MRIP Catch_SE.csv', sep='')) 
  colnames(dfCatch)<-colnames(MRIP.Catch)
  dfCatch<-rbind(dfCatch, MRIP.Catch)

}

#read in AK Trips data
MRIP.Trips.AK<-read.csv(paste('cross regional data/AK/AK MRIP Trips_SE.csv', sep='')) 

dfTrips[dfTrips$SE.Trips == 0,4]<-NA #replace SEs that = 0 with NA->exclude from linear model
#because 0s would skew the regression model

plot(SE.Trips~Angler.Trips, data = dfTrips
   # ,  xlim = c(0,8500000), ylim = c(0,500000) #zoomed in to see only lower trip values since AK trips are very low compared to rest of the regions
     ) #explore data

dfTrips<-dfTrips[which(dfTrips[,3] < 9000000),] 
#include just enough points from the lower trips region to predict AK values
#since. PSE skew upwards as trip numbers get higher

trips.lm<-lm(SE.Trips~Angler.Trips, data = dfTrips) #linear regression
summary(trips.lm)

MRIP.Trips.AK$SE.Trips<-predict(trips.lm, newdata = MRIP.Trips.AK)
points(MRIP.Trips.AK$Angler.Trips, MRIP.Trips.AK$SE.Trips, col = 'red')
#see how predicted data look overlaid on real observations
#predictions for AK are very low compared to rest of regions
#but SE estimates are still conservative, as they are higher than the real observations at the same trip numbers
View(MRIP.Trips.AK) 

write.csv(MRIP.Trips.AK, paste('cross regional data/AK/AK MRIP Trips_SE_pred.csv', sep=''), row.names = F)

######same for Catch
MRIP.Catch.AK<-read.csv(paste('cross regional data/AK/AK MRIP Catch_SE.csv', sep='')) 

dfCatch[dfCatch$SE == 0,4]<-NA #replace SEs that = 0 with NA->exclude from linear model
#because 0s would skew the regression model

plot(SE~RCatch, data = dfCatch
  #    ,  xlim = c(0,60000000), ylim = c(0,5000000) #zoomed in to see only lower trip values since AK Catch are very low compared to rest of the regions
) #explore data

dfCatch1<-dfCatch[which(dfCatch[,3] <50000000),] 
#include just enough points from the lower Catch region to predict AK values
#since. PSE skew upwards as trip numbers get higher
plot(SE~RCatch, data = dfCatch1)

Catch.lm<-lm(SE~RCatch, data = dfCatch1) #linear regression
summary(Catch.lm)

MRIP.Catch.AK$SE<-predict(Catch.lm, newdata = MRIP.Catch.AK)
points(MRIP.Catch.AK$RCatch, MRIP.Catch.AK$SE, col = 'red')
#see how predicted data look overlaid on real observations
#predictions for AK are very low compared to rest of regions
#but SE estimates are still conservative, as they are higher than the real observations at the same trip numbers
View(MRIP.Catch.AK) 

write.csv(MRIP.Catch.AK, paste('cross regional data/AK/AK MRIP Catch_SE_pred.csv', sep=''), row.names = F)

####################### REST OF THE REGIONS ######################
for(i in 2:length(folders)) { #i represents the folder name and the region name in the file
  MRIP.Catch<-read.csv(paste('cross regional data/',folders[i],'/',folders[i],' MRIP Catch_SE.csv', sep='')) 
  MRIP.Trips<-read.csv(paste('cross regional data/',folders[i],'/',folders[i],' MRIP Trips_SE.csv', sep='')) 

  MRIP.Trips[MRIP.Trips$SE.Trips == 0,4]<-NA #replace SEs that = 0 with NA->exclude from linear model
  #because 0s would skew the regression model
  
  plot(SE.Trips~Angler.Trips, data = MRIP.Trips, cex = 2) #explore data
  text(SE.Trips~Angler.Trips, labels=rownames(MRIP.Trips),data=MRIP.Trips, cex=0.9, font=2)
  #labeling points shows which points are low SE estimates-->drop in predict model for conservative SEs
  dat<-MRIP.Trips[-c(1,2,4),]
  
  trips.lm<-lm(SE.Trips~Angler.Trips, data = dat) #linear regression
  summary(trips.lm)
  
  MRIP.Trips$pred<-predict(trips.lm, newdata = MRIP.Trips)
  points(MRIP.Trips$Angler.Trips, MRIP.Trips$pred, col = 'red')
  #see how predicted data look overlaid on real observations
  View(MRIP.Trips)  #some SE values are negative-->
  
  #replace negative values with minimum SE values
  for (k in 1:length(MRIP.Trips$Angler.Trips)) {
    MRIP.Trips$pred[k]<-if(MRIP.Trips$pred[k]<0) {min(MRIP.Trips$SE.Trips, na.rm = T)
    } else{MRIP.Trips$pred[k]
    }
  } #output for each k for loop is MRIP.Trips dataframe with predicted values
  
  points(MRIP.Trips$Angler.Trips, MRIP.Trips$pred,cex =2, col = 'blue')
  #predicted data with min(SE) replacing predicted points with neg values
  #some new positive values appear at low trip numbers
  
  #replace SE NAs with predicted SE values
  for (k in 1:length(MRIP.Trips$Angler.Trips)) {
    MRIP.Trips$SE.Trips[k]<-if(is.na(MRIP.Trips$SE.Trips[k])) {MRIP.Trips$pred[k]
    } else{MRIP.Trips$SE.Trips[k]
    }
  } #output for each k for loop is MRIP.Trips dataframe with SE's that had NA values
      #replaced with predicted SEs
  
  #drop 'pred' column
  MRIP.Trips<-MRIP.Trips[,-5]
  
  write.csv(MRIP.Trips, paste('cross regional data/',folders[i],'/',folders[i],' MRIP Trips_SE_pred.csv', sep=''), row.names = F)

  #####repeat predict SE values for Catch#####
  MRIP.Catch[MRIP.Catch$SE == 0,4]<-NA #replace SEs that = 0 with NA->exclude from linear model
 
  plot(SE~RCatch, data = MRIP.Catch, cex = 2) #explore data
  text(SE~RCatch, labels=rownames(MRIP.Catch),data=MRIP.Catch, cex=0.9, font=2)
  #labeling points shows which points are low SE estimates-->drop in predict model for conservative SEs
  dat<-MRIP.Catch[-c(1,2,4),]
  
  catch.lm<-lm(SE~RCatch, data = dat) #linear regression
  summary(catch.lm)
  
  MRIP.Catch$pred<-predict(catch.lm, newdata = MRIP.Catch)
  points(MRIP.Catch$RCatch, MRIP.Catch$pred, col = 'red')
  #see how predicted data look overlaid on real observations
  View(MRIP.Catch)  #some SE values may be negative or NA-->
  
  #replace negative values with minimum SE values
  for (k in 1:length(MRIP.Catch$RCatch)) {
    MRIP.Catch$pred[k]<-if(MRIP.Catch$pred[k]<0) {min(MRIP.Catch$SE, na.rm = T)
    } else{MRIP.Catch$pred[k]
    }
  } #output for each k for loop is MRIP.Catch dataframe with predicted values
  
  points(MRIP.Catch$RCatch, MRIP.Catch$pred,cex =2, col = 'blue')
  #predicted data with min(SE) replacing predicted points with neg values
  #some new positive values appear at low trip numbers
  
  #replace SE NAs with predicted SE values
  for (k in 1:length(MRIP.Catch$RCatch)) {
    MRIP.Catch$SE[k]<-if(is.na(MRIP.Catch$SE[k])) {MRIP.Catch$pred[k]
    } else{MRIP.Catch$SE[k]
    }
  } #output for each k for loop is MRIP.Catch dataframe with SE's that had NA values
  #replaced with predicted SEs
  #drop 'pred' column
  MRIP.Catch<-MRIP.Catch[,-5]
  write.csv(MRIP.Catch, paste('cross regional data/',folders[i],'/',folders[i],' MRIP Catch_SE_pred.csv', sep=''), row.names = F)  #save csv's with predicted SEs for easier file call if necessary
}

#NECatch.test<-read.csv('cross regional data/NE/NE MRIP Catch_SE_pred.csv')
#plot(RCatch~Year, data = NECatch.test)
#compared to ecowatch and matches up
 
############################################################### 
############## GET MC SIM DFs FOR ALL REGIONS #################
for(i in 1:length(folders)) { #i represents the folder name and the region name in the file
  MRIP.Catch<-read.csv(paste('cross regional data/',folders[i],'/',folders[i],' MRIP Catch_SE_pred.csv', sep='')) 
  MRIP.Trips<-read.csv(paste('cross regional data/',folders[i],'/',folders[i],' MRIP Trips_SE_pred.csv', sep='')) 
  
  MRIP.Catch<-MRIP.Catch[MRIP.Catch$Year<2021,] #delete 2021 prelim numbers
  MRIP.Trips<-MRIP.Trips[MRIP.Trips$Year<2021,] #delete 2021 prelim numbers
  
  #create data frame to hold simulation draws  
  #################### 

  Years<-unique(MRIP.Catch$Year)
  Year<-rep(Years, each=1000)
  Sim.num<-rep(1:1000,times=length(Years))
  Catch<-numeric(length = length(Year))
  Trips<-numeric(length = length(Year))
  MRIP.sim<-data.frame(Year,Sim.num,Catch,Trips)
  MRIP.sim$Sim.num<-as.factor(MRIP.sim$Sim.num)
  
  for (j in 1:length(Years)){
    MRIP.sim[MRIP.sim$Year == Years[j],3]<- rnorm(1000, mean=MRIP.Catch$RCatch[j], MRIP.Catch$SE[j])
    MRIP.sim[MRIP.sim$Year == Years[j],4]<- rnorm(1000, MRIP.Trips$Angler.Trips[j], MRIP.Trips$SE.Trips[j])
   
    #MRIP.sim[MRIP.sim$Year == Years[j],2]<- runif(1000, MRIP.Catch$RCatch.lowerCI[j], MRIP.Catch$RCatch.upperCI[j])
    #MRIP.sim[MRIP.sim$Year == Years[j],3]<- runif(1000, MRIP.Trips$Trips.lowerCI[j], MRIP.Trips$Trips.upperCI[j])
  }
  write.csv(MRIP.sim, paste('cross regional data/',folders[i],'/',folders[i],' MRIP Monte Carlo Sim.csv', sep=''), row.names = F)
}
############################################################
#####for loop for getting gamfit dfs with all sims#########
############################################################
#vectors used
folders<-c('AK','CC','GOM','NE','SE','HI')
regnames<-c('Alaska','California Current','Gulf of Mexico','Northeast','Southeast',"Hawai'i")

##########WRITE FUNCTION FOR CREATING GAMFIT/TRENDS/THRESHOLDS DF########
#function that converts raw data to gamfit with trends & thresholds marked
#creates gamfit output named df1

gam_tnt<-function(df, newd.num, gammod, cont.pred,cont.name, cat.pred, cat.name ) {
  #df: input df
  #newd.num: how many values within the continuous predictor range to predict on?--have been using 500
  #the gam() object
  #cont.pred: continuous predictor--Year in this case
  #cont.name: Name the continuous predictor in output df-- 'Year'
  #cat.pred: categorical predictor--Sim.num in this case
  #cat.name: name the cat predictor--'Sim.num'
  
  #what values to predict on?
  newd<- with(df, data.frame(Year = seq(min(cont.pred), max(cont.pred), 
                                        length = newd.num)))
  
  #calculates fit & se
  pred <- predict.gam(gammod, newdata = newd, type = "response", se.fit = T)
  
  #convert data to df for ggplot
  df1<-data.frame(cat.pred[1],newd[,1],pred$fit, pred$se.fit)
  colnames(df1) <- c(cat.name,cont.name, 'fit', 'se.fit')
  
  #   plot(df1$Year,df1$fit) #exploratory
  
  ################################FD CALCULATIONS###############################
  #add columns to plot only values that represent significant trends calculated by fd
  ## parameters for testing
  UNCONDITIONAL <- FALSE # unconditional or conditional on estimating smooth params?
  N <- 10000             # number of posterior draws
  EPS <- 1e-07           # finite difference
  
  #newd calculated earlier for predictive gam
  fd<-derivatives(gammod, newdata = newd, eps = EPS, unconditional = UNCONDITIONAL, n_sim = N, order = 1, interval = "simultaneous")
  as.data.frame(fd)
  
  df1$inc.trend<-ifelse(fd$lower>0, df1$fit, NA) #increasing trends only
  df1$dec.trend<-ifelse(fd$upper<0, df1$fit, NA) #decreasing trends only
  
  ################################SD CALCULATIONS###############################
  ###add column to plot only values that occur within threshold region
  # second derivatives of all smooths using central finite differences with simultaneous CI
  sdCR<-derivatives(gammod, newdata = newd, eps = EPS, unconditional = UNCONDITIONAL, order = 2, interval = "simultaneous")
  as.data.frame(sdCR)
  
  df1$threshold<-ifelse(sdCR$lower>0, df1$fit, ifelse(sdCR$upper<0, df1$fit, NA))
  df1$se.threshold<-ifelse(sdCR$lower>0, df1$se.fit, ifelse(sdCR$upper<0, df1$se.fit, NA))
  return(df1)
  
}

############### FOR LOOP WITH GAM FUNCTION FOR ALL REGIONS ###############
#loop--read in MC sim df for each region
for(i in 2:2) { #i represents the folder name and the region name in the file
  #AK PSE's are all predicted off of other regions' PSE's
  MRIP<-read.csv(paste('cross regional data/',folders[i],'/',folders[i],' MRIP Monte Carlo Sim.csv', sep='')) 
  simnum<-unique(MRIP$Sim.num)
  MRIP$Catch.Thousands<-MRIP$Catch/1000
  MRIP$Trips.Thousands<-MRIP$Trips/1000
  
  #calculate CPUE and delete CC outliers
  MRIP$CPUE<-MRIP$Catch/MRIP$Trips
  upr<-boxplot.stats(MRIP$CPUE)$stats[5]
  outs<-which(MRIP$CPUE>upr)
  MRIP<-MRIP[-c(outs),]
  
  MRIP<-MRIP[,c(1,2,5,6,7)]
  MRIP$Sim.num<-as.factor(MRIP$Sim.num)
  str(MRIP) #check that sim.num is factor with 1000 levels
  
  ###calculate predictive gam for EACH MC sim (of 1000) with fd & sd values for marking on plot
  #subset each sim & loop to rbind together 

  ## ?? come back to this--did a gamfit for CPUE of sims just to see if i see anything
  #but from data exploration, doesn't look like any consistent trend so might remove later
  for(k in 3:5) { #catch and trips columns 
    ################### get gamfit for first sim ################
    #first do sim 1 outside of j loop so that there is a df to bind new df1 to
    df0<-MRIP[MRIP$Sim.num == MRIP$Sim.num[1],] #subsetting only rows where sim.num is j=1 
    #j should represent the actual sim.sum, but use first occurence [1] to make sure there is no overlap in the j loop later
    
  # do one sim outside of loop first so there is an original df for the new loop dfs to rbind to
  gammod<-gam(unlist(df0[k]) ~ s(Year), 
              data = df0, method = 'REML')
  
  #run gam_tnt function I wrote to convert raw sim df (sim 1) to gamfit & trends & thresholds df
  df<-gam_tnt(df0, 500,gammod,df0$Year,'Year',df0$Sim.num, 'Sim.num')
  # 'df' will be the actively compiling dataframe for the j loop
  
  ##### also start blank df to hold all deviance and p values for summarized gam stats for the MRIP sims
  Simnum<-seq(1:1000)
  Dev<-numeric(length = length(Simnum))
  pval<-numeric(length = length(Simnum))
  sim.devs<-data.frame(Simnum,Dev,pval)

  #run rest of sims in a loop and rbind each to df-->overwrites with compiled df as the loop runs


  for(j in 1:1000) {
    df0<-MRIP[MRIP$Sim.num == MRIP$Sim.num[j],] #subsetting only rows where sim.num = j
    #raw data for simulation j
    
    ################### get gamfit for simulation j ################
    gammod<-gam(unlist(df0[k]) ~ s(Year),  #gammod() needs to be outside of function
                data = df0, method = 'REML')
    #summary(gammod)
    #str(summary(gammod))
    sim.devs[j,2]<-summary(gammod)$dev.expl #pull deviance of current sim to sim.devs df
    sim.devs[j,3]<-summary(gammod)$s.table[1,4] #pull p value of current sim to sim.devs df
    
    #calculate predictive gam for each MC sim with fd & sd values for marking on plot with gam_tnt function
    df.new<-gam_tnt(df0, 500,gammod,df0$Year,'Year',df0$Sim.num, 'Sim.num')
    
    ##bind new sim gamfit df to compiled sim df
    df<-rbind(df.new,df) #overwrite original df so that when loop re reuns next sim, it binds to the actively compiling df
    
    if (j %% 50 == 0) print(paste('on sim',j,'of 1000')) #every multiple of 50, shows what sim.num the loop is on 
    #since this run takes a long time, it's helpful to know what progress has been made
    
  }
  #outside of simulation loop
  
  ### summary stats of devs and p's for MRIP simulation GAMs
  se <- function(x) sd(x)/sqrt(length(x)) #function to calculate se of devs and p's
  
  #deviances
  range(sim.devs[,2])
  mean(sim.devs[,2])
  se(sim.devs[,2])
  hist(sim.devs[,2], breaks = 30) #explore devs and p's
  
  #p values
  range(sim.devs[,3])
  mean(sim.devs[,3])
  se(sim.devs[,3])
  hist(sim.devs[,3], breaks = 30) #explore devs and p's
  
  #didn't save the sim.devs df to csv because i ran one region at a time anyway (since sims are computationally heavy)
  #so i just manually put the means and se's of deviances and p values in a separate excel sheet
  
  df <- df[,colSums(is.na(df))<nrow(df)] #if any trend or threshold columns are all NA, delete
  write.csv(df, file=paste('cross regional data/MRIP MC sims/',folders[i],' MRIP ',colnames(MRIP[k]),' Monte Carlo Sim gamfit.csv', sep=''), row.names = F)
  
  } 

}

################# EXPLORE MC GAM SIM DFS FOR TRENDS AND THRESHOLDS ###########

#maybe get vals for summary gam plot using, means, min(mean-se) and max(mean+se)
#mark summary plot using years that had trends/thresh >95% of the time?

#vectors used
folders<-c('AK','CC','GOM','NE','SE','HI')
regnames<-c('Alaska','California Current','Gulf of Mexico','Northeast','Southeast',"Hawai'i")
MRIPvar<- c('Catch.Thousands', 'Trips.Thousands', 'CPUE')


##############FIRST SUMMARIZE ALL SIMS INTO ONE GAMFIT ####################
#one per region per var (catch/trips)
for(i in 1:length(folders)) { #i represents the folder name and the region name in the file

  for(k in 1:length(MRIPvar)){
  MRIP<-read.csv(paste('cross regional data/MRIP MC sims/',folders[i],' MRIP ',MRIPvar[k],' Monte Carlo Sim gamfit.csv', sep='')) 
  
  MRIP$Region<-regnames[i]
  MRIP.cut<-MRIP[,c('Region','Year','fit','se.fit')] #cut just year, fit & se, to get fit summary vals over all 1000 sims, will do trends/thresholds in loop below
  
  ########test to visualize and compare to final sim summary ####
#  test<-MRIP[c(10001:20000),] #can check 20 sims at a time so the plot is not completely swamped
 # test$Sim.num<-as.factor(test$Sim.num)
  
#  ggplot(test,
 #        aes(x = Year, y = fit)) +
 #   theme(  plot.title=element_text(hjust=0.5, size = 16),
  #          panel.grid.major = element_blank(), #delete major grid lines
  #          panel.grid.minor = element_blank(), #delete minor grid lines
  #          axis.text=element_text(size=13.8, color = 'black'), #adjust font size of axis tick labels
  #          axis.title=element_text(size=15.2,face="plain"), #adjust size of axis titles
  #         axis.title.x = element_text(margin = margin(t = 12, r = 0, b = 0, l = 0)), #increases distance between x axis label and plot
  #          panel.background = element_rect(fill = "transparent",colour = 'black', size = 1),
  #         plot.background = element_rect(fill = "transparent",colour = NA),
  #         plot.margin = unit(c(0.3,2,0.3,0.3), "cm"),
  #          legend.position = 'right', # c(0.37,0.57),
  #          legend.background = element_rect(fill="transparent", color = 'black'),
  #          legend.key = element_rect(fill = "transparent", colour = NA),
  #          legend.key.size = unit(0.7, 'cm'),
  #         legend.text=element_text(size=10),
  #          legend.title=element_blank(),
  #          legend.margin = margin(0,12,6,10)) +
  #  scale_x_continuous(breaks=seq(1980,2021,1), #so that a tick appears every year--default ticks only where breaks are assigned
   #                    labels = c("1980",rep("",4),"1985",rep("",4),"1990",rep("",4),"1995",rep("",4),"2000",rep("",4),"2005",rep("",4),'2010',rep("",4),'2015',rep("",4), '2020', '')) +
  #  scale_y_continuous(limits = c(round_any(min(test$fit-2*test$se.fit),10, f = floor), round_any(max(test$fit+2*test$se.fit),10, f = ceiling)), #0 to max point (points go further than CI band)
  #                     position = 'left')+
  #  geom_ribbon(aes(ymin = fit-(2*se.fit), ymax = fit+(2*se.fit), fill = Sim.num), alpha = 0.4, color = 'white') + #95CI = (2*SE) for gam function
  #  scale_fill_manual(values = c(cbbPalette[c(1:9)],cbbPalette[c(1:9)],cbbPalette[c(1,2)])) + #color combined CI, individual Sim.nums are gray
 #   {if("threshold" %in% colnames(test))
  #    geom_ribbon(aes(ymin = threshold-(2*se.threshold), ymax = threshold+(2*se.threshold), group = Sim.num), 
   #               alpha = 1, fill = 'gray40')} + #recolor se band for threshold Sim.nums
  #  geom_line(size = 0.8, aes(color = Sim.num)) + #full gam function
  #  scale_color_manual(values = c(cbbPalette[c(1:9)],cbbPalette[c(1:9)],cbbPalette[c(1,2)])) +
  #  {if("inc.trend" %in% colnames(test)) 
   #   geom_line(aes(x=Year, y = inc.trend, group = Sim.num),size = 1.5, color = "#009E73", alpha = 0.5)}  +  #increasing trends
  #  { if("dec.trend" %in% colnames(test))
   #   geom_line(aes(x=Year, y = dec.trend, group = Sim.num),size = 1.5, color = "#D55E00", alpha = 0.5) } +
  #  labs(y = MRIPlabs[k], x = "Year", title = paste(MRIPtitle[k],' in All IEA Regions',sep=''))   
  
#  ggsave(paste('figures/cross regional/MRIP Monte Carlo/TEST_MC Sims ',MRIPvar[k],'_GAM_ggplot.png',sep=''), 
 #        width =  9.2, height = 8, units = 'in', #w & h in inches
  #       dpi = 300, bg = 'transparent')

  ########## end test ########
  
  df0<-aggregate(.~Region+Year, data=MRIP.cut, FUN = mean)  #get mean of all variables (notated by '.') by Year + Region
  #only one Region, but include as first factor to keep in aggregated df
  
  #get mean values of trends/thresholds (rowMeans) only where they are detected in >-95% of the sims
  for (l in 5:7) { #once per var (1) inc, (2) dec, (3) thresh (se.thresh is added in later onto thresh)
    sub<-MRIP[,c(1,2,l)] #subset var of interest

    wide<-spread(sub,Sim.num,colnames(sub[3])) #spread by Sim.num
      # The arguments to spread():
      # - data: Data object
      # - key: Name of column containing the new column names
      # - value: Name of column containing values
    wide$NA.num<-rowSums(is.na(wide))
    
    NA.accept<-350 #can change level of acceptance
    wide$val<-ifelse(wide$NA.num<=NA.accept, #if NAs at most 350 (majority of sims detect this trend/threshold--65%)
                     rowMeans(wide[,c(2:1001)],na.rm = T), #then put mean of var of interest
                     NA) #otherwise put NA
    
    df1<-wide[,c(1,1003)] #save var df
    colnames(df1)<-c('Year',colnames(sub[3]))
    
    df0<-join(df0,df1,by = 'Year', type = 'full') 
    df0[,l]<-ifelse(!is.na(df0[,l]),df0[,3],NA)
    #full means also join rows that dont have matching Year key--for purposes of error checking
    #if previous steps were correct, then inner would do the same thing (inner only joins rows with matching Year key)

  }
  # after this loop, still need to add in se.threshold
  if("threshold" %in% colnames(df0)) { #only need to add se if threshold exists
    df0$se.threshold<-ifelse(!is.na(df0$threshold), df0[,4], NA)  
    }
  
  #save complete file outside of trends/thresh (l) loop--one per var (catch/trips) per region
  write.csv(df0,file=paste('cross regional data/MRIP MC sims/',folders[i],' MC summary gamfit ',MRIPvar[k],'_MRIP.csv', sep=''), row.names = F)
  }
}
########## COMPILE ONE DF PER VAR (CATCH/TRIPS) FOR ALL REGIONS ##########
for(k in 1:length(MRIPvar)){ #k is Catch Thousands or Trips.Thousands
  df<-read.csv(paste('cross regional data/MRIP MC sims/',folders[1],' MC summary gamfit ',MRIPvar[k],'_MRIP.csv', sep='')) 
  MRIPvar[k]
  #read in region 1 (AK) to create base df to bind new dfs from i loop to
  
  for(i in 2:length(folders)) { #i represents the folder name and the region name in the file
    #go through all regions for one var first to compile into one df per var
    
    df.new<-read.csv(paste('cross regional data/MRIP MC sims/',folders[i],' MC summary gamfit ',MRIPvar[k],'_MRIP.csv', sep='')) 
    df<-bind_rows(df,df.new) #use dplyr::bind_rows because rbind can't handle the non-matching columns
    #bind rows just fills in NAs for a column that doesn't exist in one of this original dfs
  }
  write.csv(df,file=paste('cross regional data/MRIP MC sims/ MC ',MRIPvar[k],' all regions summary gamfit_MRIP.csv', sep=''), row.names = F)
}

##### also compile original data for plotting points
#first get make dfs for reg1 as base for looped dfs to bind to
folders<-c('AK','CC','GOM','NE','SE','HI')
MRIPvar<- c('Catch.Thousands', 'Trips.Thousands','CPUE')
MRIPvar0<-c('Catch','Trips','CPUE')
regnames<-c('Alaska','California Current','Gulf of Mexico','Northeast','Southeast',"Hawai'i")


#add CPUE to raw MRIP dfs
for(i in 1:length(folders)) { #i represents the folder name and the region name in the file
  MRIP.Catch<-read.csv(paste('cross regional data/',folders[i],'/',folders[i],' MRIP Catch_SE.csv', sep='')) 
  MRIP.Trips<-read.csv(paste('cross regional data/',folders[i],'/',folders[i],' MRIP Trips_SE.csv', sep=''))
  MRIP.CPUE<-join(MRIP.Catch,MRIP.Trips, by = 'Year', type = 'full')
  MRIP.CPUE$CPUE<-MRIP.CPUE$RCatch/MRIP.CPUE$Angler.Trips
  MRIP.CPUE<-MRIP.CPUE[,c('Year','Region','CPUE')]
  MRIP.CPUE$Region<-regnames[i]
  write.csv(MRIP.CPUE, file = paste('cross regional data/',folders[i],'/',folders[i],' MRIP CPUE_SE.csv', sep=''), row.names = F)
}

for(k in 1:length(MRIPvar)){ #k is Catch Thousands or Trips.Thousands or CPUE
MRIP0<-read.csv(paste('cross regional data/',folders[1],'/',folders[1],' MRIP ',MRIPvar0[k],'_SE.csv', sep='')) 
MRIP0$Region <- regnames[1] #replace with regnames because original data format was all caps--'ALASKA'

for(i in 2:length(folders)) { #i represents the folder name and the region name in the file
  MRIP<-read.csv(paste('cross regional data/',folders[i],'/',folders[i],' MRIP ',MRIPvar0[k],'_SE.csv', sep='')) 
  MRIP$Region <- regnames[i] 
  
  MRIP0<-rbind(MRIP0,MRIP) 
}
if(mean(MRIP0[,3])> 30) { #workaround so that it doesnt try to divide CPUE by 1000
  MRIP0[,3]<-MRIP0[,3]/1000 #convert to thousands to match sim data
}
outs<-boxplot.stats(MRIP0[,3])$out
upr<-boxplot.stats(MRIP0[,3])$stats[5]
outs<-which(MRIP0[,3]>upr)
MRIP0<-MRIP0[-c(outs),]

write.csv(MRIP0, file= paste('cross regional data/MRIP MC sims/MRIP ',MRIPvar[k],'_all regions_raw.csv', sep=''), row.names = F)
}
############## PLOT ALL REGIONS TOGETHER FOR EACH CATCH/TRIPS ###############
MRIPvar<- c('Catch.Thousands', 'Trips.Thousands', 'CPUE')
MRIPvar1<-c('Catch.Millions', 'Trips.Millions','CPUE')
MRIPlabs<-c('Catch (Millions of Fish)', 'Angler Trips (Millions)','Catch per Unit Effort (Fish per Trip')
MRIPtitle<-c('Recreational Landings', 'Recreational Effort','Catch per Unit Effort')
zoom<-c(40,15)
cut<-c(50,20)

for(k in 1:length(MRIPvar)){ #k is Catch Thousands or Trips.Thousands
  df1<-read.csv(paste('cross regional data/MRIP MC sims/ MC ',MRIPvar[k],' all regions summary gamfit_MRIP.csv', sep=''))
  MRIP0<-read.csv(paste('cross regional data/MRIP MC sims/MRIP ',MRIPvar[k],'_all regions_raw.csv', sep=''))
  MRIPvar[k]
  
  df1 <- df1[,colSums(is.na(df1))<nrow(df1)] #if any trend or threshold columns are all NA, delete
  
  if(mean(MRIP0[,3])>30) {#don't divide CPUE
    df1[,c(3:length(colnames(df1)))]<-df1[,c(3:length(colnames(df1)))]/1000 #divide values by 1000 so final units are in millions
    MRIP0[,3]<-MRIP0[,3]/1000
    #leave 'Region' and 'Year' alone
  }

  str(df1) #'Region' is character and not factor
  str(MRIP0)
  
  # set Region as factor and reorder according to how i want it to be plotted
  # order is highest to lowest, as well as geographically clockwise 
  df1$Region <- factor(df1$Region, levels = c("Northeast", "Southeast", 'Gulf of Mexico', 'California Current', "Hawai'i", 'Alaska'))
  MRIP0$Region <- factor(MRIP0$Region, levels = c("Northeast", "Southeast", 'Gulf of Mexico', 'California Current', "Hawai'i", 'Alaska'))
  
    # colorblind friendly palette with black:
    cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
    
    ############### plot gam ##################
    
    ggplot(df1,
           aes(x = Year, y = fit)) +
      theme(  plot.title=element_text(hjust=0.5, size = 28.5),
              panel.grid.major = element_blank(), #delete major grid lines
              panel.grid.minor = element_blank(), #delete minor grid lines
              axis.text=element_text(size=22, color = 'black'), #adjust font size of axis tick labels
              axis.title=element_text(size=27,face="plain"), #adjust size of axis titles
              axis.title.x = element_text(margin = margin(t = 12, r = 0, b = 0, l = 0)), #increases distance between x axis label and plot
              panel.background = element_rect(fill = "transparent",colour = 'black', size = 1),
              plot.background = element_rect(fill = "transparent",colour = NA),
              plot.margin = unit(c(0.3,2,0.3,0.3), "cm"),
              legend.position = 'right', # c(0.37,0.57),
              legend.background = element_rect(fill="transparent", color = 'black'),
              legend.key = element_rect(fill = "transparent", colour = NA),
              legend.key.size = unit(1, 'cm'),
              legend.text=element_text(size=17),
              legend.title=element_blank(),
              legend.spacing.y = unit(0.2, 'cm'),
              legend.margin = margin(5,13,10,11)) +
      guides(fill = guide_legend(byrow =  T)) + # needed for legend.spacing to work
      scale_x_continuous(breaks=seq(1980,2021,5), #so that a tick appears every year--default ticks only where breaks are assigned
                         labels = c("1980",'',"1990",'',"2000",'','2010','', '2020')) +
  #    scale_y_continuous(limits = c(0, round_any(max(MRIP0[,3]),5, f = ceiling))) + #0 to max point (points go further than CI band)
   #   scale_y_continuous(limits = c(0,zoom[k])) + #zoomed bottom
      scale_y_continuous(limits = c(cut[k],round_any(max(MRIP0[,3]),5, f = ceiling))) + #cut top
      geom_ribbon(aes(ymin = fit-(2*se.fit), ymax = fit+(2*se.fit), fill = Region), alpha = 0.3, color = 'gray45') + #95CI = (2*SE) for gam function
      scale_fill_manual(values = cbbPalette[c(2,3,5,6,8,7)]) + #color combined CI, individual Regions are gray
      {if("threshold" %in% colnames(df1))
        geom_ribbon(aes(ymin = threshold-(2*se.threshold), ymax = threshold+(2*se.threshold), group = Region), 
                    alpha = 0.7, fill = 'gray40', color = 'black')} + #recolor se band for threshold regions
      geom_line(size = 1, aes(color = Region)) + #full gam function
      geom_point(data = MRIP0,aes(x = Year, y = unlist(MRIP0[3]), color = Region, shape = Region), size = 2.7, alpha = 1, fill = NA) + # data points
      scale_color_manual(values = cbbPalette[c(2,3,5,6,8,7)]) +
      scale_shape_manual(values = c(2,4,1,0,5,6,8)) +
      {if("inc.trend" %in% colnames(df1)) 
        geom_line(aes(x=Year, y = inc.trend, group = Region),size = 2, color = "#009E73", alpha = 0.6)}  +  #increasing trends
      { if("dec.trend" %in% colnames(df1))
        geom_line(aes(x=Year, y = dec.trend, group = Region),size = 2, color = "#D55E00", alpha = 0.6) } +
      labs(y = MRIPlabs[k], x = "Year", title = paste(MRIPtitle[k]))   
    
    
#    ggsave(paste('figures/cross regional/MRIP Monte Carlo/MC Sims ',MRIPvar1[k],'_zoom GAM_ggplot.png',sep=''), 
 #          width =  11.5, height = 5.6, units = 'in', #w & h in inches
  #         dpi = 300, bg = 'transparent')
    
    ggsave(paste('figures/cross regional/MRIP Monte Carlo/MC Sims ',MRIPvar1[k],'_cut GAM_ggplot.png',sep=''), 
           width =  11.5, height = 7.5, units = 'in', #w & h in inches
           dpi = 300, bg = 'transparent')
    
#    ggsave(paste('figures/cross regional/MRIP Monte Carlo/MC Sims ',MRIPvar1[k],'-all regions GAM_ggplot.png',sep=''), 
 #          width =  11.5, height = 8, units = 'in', #w & h in inches
  #         dpi = 300, bg = 'transparent')

}

##########################################################################
##############################################################################
###############################NES by YEAR####################################
##############################################################################
folders<-c('AK','CC','GOM','NE','SE','HI')
files<-c('AK','Cali Current','Gulf of Mexico','NE','Southeast','HI') 
regnames<-c('Alaska','California Current','Gulf of Mexico','Northeast','Southeast',"Hawai'i")
NES1var<-c('1','Establishments', 'Receipts') #for manuscript, probably better to focus on receipts
NES1labs<-c('1','Number of Establishments','Receipts (Millions of Dollars)')
NESinds<-c('Fishing', 'Seaf Markets', 'Seaf Packaging')
NESmain<-c('Fishing', 'Seafood Markets', 'Seafood Packaging')

#reg.k<-c(4,7,9,4,8,4) #can only set k per region based on the nested for loop
#reg.sp<-c(0.01,0.006,0.02,0.1,0.1,0.05) #can only set sp per region based on the nested for loop

##### also start blank df to hold all deviance and p values for summarized gam stats for the MRIP sims
Region<-c(rep('AK',each =6),rep('CC',each =6),rep('GOM',each =6),rep('NE',each =6),rep('SE',each =6),rep('HI',each =6))
Sector<-character(length = length(Region))
Ind<-character(length = length(Region))
Dev<-numeric(length = length(Region))
pval<-numeric(length = length(Region))
gam.stats<-data.frame(Region,Sector,Ind,Dev,pval)

for(i in 1:length(folders)) { #i represents the folder name and the region name in the file
  for(k in 1:length(NESinds)) {
  NES0<-read.csv(paste('cross regional data/',folders[i],'/',files[i],' NES ',NESinds[k],' Totals_deflated.csv', sep='')) 
  NES0$Receipts.Millions<-NES0$Total.Receipts/1000
#  NES0$Establishments.Thousands<-NES0$Establishments/1000
  
  NES00<-NES0[,c('Year','Establishments','Receipts.Millions')]
  NES1 <- aggregate(.~Year, data=NES00, FUN=sum)
  NES1$Sector<-NESmain[k]

  for(j in 2:3){ #j represents the column number in the NES df (skip 1), NES1var
    #exploratory plot to see if there are obvious weird values
 #   plot(NES1[,j]~Year, data = NES1) 
    #looks pretty good
    
    gammod<-gam(NES1[,j] ~ s(Year),
                data = NES1, method = 'REML')
    gam.stats[(i*6)-6+(k*2)-2+(j-1),2]<-NESinds[k]
    gam.stats[(i*6)-6+(k*2)-2+(j-1),3]<-NES1var[j]
    
    gam.stats[(i*6)-6+(k*2)-2+(j-1),4]<-summary(gammod)$dev.expl #pull deviance of current sim to sim.devs df
    gam.stats[(i*6)-6+(k*2)-2+(j-1),5]<-summary(gammod)$s.table[1,4] #pull p value of current sim to sim.devs df
    
    #run gam_tnt function I wrote to convert raw data df BLS00 to gamfit & trends & thresholds df
  #  df1<-gam_tnt(NES1, 500,gammod,NES1$Year,'Year',NES1$Sector, 'Sector')

    write.csv(df1, file=paste('cross regional data/',folders[i],'/',files[i],' NES ',NESinds[k],' ',NES1var[j],' gamfit.csv', sep=''), row.names = F)
    
  }
  }
}

write.csv(gam.stats, file='outputs/gam stats/NES.csv', row.names = F)


###########################################################################
###### NES GET GAMFIT OF 'COMBINED' SECTOR AND JOIN TO OTHERS #############
###########################################################################
folders<-c('AK','CC','GOM','NE','SE','HI')
files<-c('AK','Cali Current','Gulf of Mexico','NE','Southeast','HI') 
regnames<-c('Alaska','California Current','Gulf of Mexico','Northeast','Southeast',"Hawai'i")
NES1var<-c('1','Establishments', 'Receipts') #for manuscript, probably better to focus on receipts
NES1labs<-c('1','Number of Establishments','Receipts (Millions of Dollars)')
NESinds<-c('Fishing', 'Seaf Markets', 'Seaf Packaging')
NESabbr<-c('Fsh','SM','SP')
#reg.k<-c(4,7,9,4,8,4) #can only set k per region based on the nested for loop
#reg.sp<-c(0.01,0.006,0.02,0.1,0.1,0.05) #can only set sp per region based on the nested for loop

#### summarize 'all sectors' value and get gamfit
for(i in 1:length(folders)) { #i represents the folder name and the region name in the file
  for(j in 2:3){ #j represents the column number in the NES df (skip 1), and NES1var
  #with predicted values from prior calculation

  #read in gamfit dfs for individual sectors  
  NES.F<-read.csv(paste('cross regional data/',folders[i],'/',files[i],' NES ',NESinds[1],' ',NES1var[j],' gamfit.csv', sep=''))
  NES.SM<-read.csv(paste('cross regional data/',folders[i],'/',files[i],' NES ',NESinds[2],' ',NES1var[j],' gamfit.csv', sep=''))
  NES.SP<-read.csv(paste('cross regional data/',folders[i],'/',files[i],' NES ',NESinds[3],' ',NES1var[j],' gamfit.csv', sep=''))

  NES.com<- bind_rows(NES.F,NES.SM,NES.SP) #combine 3 individual sectors, still need to add a 'combined' sector
  NES.com<-NES.com[,c( "Sector", "Year",  "fit", "se.fit",  "inc.trend","dec.trend", "threshold", "se.threshold")]
  #reorder columns for consistency
#  breakmin<-max((NES.com$fit+NES.com$se.fit))
  
  #read in raw datasets to sum for combined establishment numbers
  
  NES.F0<-read.csv(paste('cross regional data/',folders[i],'/',files[i],' NES ',NESinds[1],' Totals_deflated.csv', sep=''))
  NES.F0$Receipts.Millions<-NES.F0$Total.Receipts/1000
#  NES.F0$Establishments.Thousands<-NES.F0$Establishments/1000
  NES.F0<-NES.F0[,c('Year','Establishments','Receipts.Millions')]  
  NES.F0<-NES.F0[,c(1,j)]
  NES.F0<- aggregate(unlist(NES.F0[2])~Year, data=NES.F0, FUN=sum)
  names(NES.F0)<-c('Year','Fishing')
  NES.F00<-NES.F0
  names(NES.F00)<-c('Year',NES1var[j])
  NES.F00$Sector<- 'Fishing'

  
  NES.SM0<-read.csv(paste('cross regional data/',folders[i],'/',files[i],' NES ',NESinds[2],' Totals_deflated.csv', sep=''))
  NES.SM0$Receipts.Millions<-NES.SM0$Total.Receipts/1000
#  NES.SM0$Establishments.Thousands<-NES.SM0$Establishments/1000
  NES.SM0<-NES.SM0[,c('Year','Establishments','Receipts.Millions')]  
  NES.SM0<-NES.SM0[,c(1,j)]
  NES.SM0<- aggregate(unlist(NES.SM0[2])~Year, data=NES.SM0, FUN=sum)
  names(NES.SM0)<-c('Year','SM')
  NES.SM00<-NES.SM0
  names(NES.SM00)<-c('Year',NES1var[j])
  NES.SM00$Sector<- 'Seafood Markets'
  
  NES.SP0<-read.csv(paste('cross regional data/',folders[i],'/',files[i],' NES ',NESinds[3],' Totals_deflated.csv', sep=''))
  NES.SP0$Receipts.Millions<-NES.SP0$Total.Receipts/1000
#  NES.SP0$Establishments.Thousands<-NES.SP0$Establishments/1000
  NES.SP0<-NES.SP0[,c('Year','Establishments','Receipts.Millions')]  
  NES.SP0<-NES.SP0[,c(1,j)]
  NES.SP0<- aggregate(unlist(NES.SP0[2])~Year, data=NES.SP0, FUN=sum)
  names(NES.SP0)<-c('Year','SP')
  NES.SP00<-NES.SP0
  names(NES.SP00)<-c('Year',NES1var[j])
  NES.SP00$Sector<- 'Seafood Packaging'
  
  #join 3 sector datasets wide for calculating row sums
  NES.com000<-join(NES.F0,NES.SM0, by = 'Year', type = 'full')
  NES.com00<-join(NES.com000,NES.SP0, by = 'Year', type = 'full')
  #use join() instead of simple cbind so that the Year column isnt repeated
  
  #calculate rowSums to get combined establishment values
  NES.com00$Combined<-rowSums(NES.com00[ , c(2:4)], na.rm=F) #na.rm = F to make the combined sum NA if any of the sectors are NA
  NES.com00<-NES.com00[,c('Year','Combined')]
  names(NES.com00)<-c('Year',NES1var[j])
  NES.com00$Sector<- 'Combined'
  NES.com0<-na.omit(NES.com00) #drop rows where any sectors had NAs--if dropped in rowSums, would get incomplete totals
  
  #long df with raw data from all sectors plus combined numbers
  NES0<-rbind(NES.F00, NES.SM00,NES.SP00,NES.com0) #stacks dfs
  write.csv(NES0, file=paste('cross regional data/',folders[i],'/',files[i],' NES combined ', NES1var[j],'_data points_raw.csv', sep=''), row.names = F)
  
  ################### get gamfit for 'combined' sector ################
  #gammod with just 'combined' values
  gammod<-gam(unlist(NES.com0[2]) ~ s(Year), 
              data = NES.com0, method = 'REML')
  
  #run gam_tnt function I wrote to convert raw data df BLS00 to gamfit & trends & thresholds df
  df1<-gam_tnt(NES.com0, 500,gammod,NES.com0$Year,'Year',NES.com0$Sector, 'Sector')
 
  
  ##bind all-sectors gamfit data with individual sectors
  df1<-rbind(df1,NES.com)
  df1$Sector<-as.factor(df1$Sector)
  
  df1 <- df1[,colSums(is.na(df1))<nrow(df1)] #if any trend or threshold columns are all NA, delete
  
  write.csv(df1, file=paste('cross regional data/',folders[i],'/',files[i],' NES combined ', NES1var[j],' gamfit.csv', sep=''), row.names = F)
  }
}
   
############COMPILED GAM PLOT WITH INDIVIDUAL SECTORS PLUS COMBINED #############
folders<-c('AK','CC','GOM','NE','SE','HI')
files<-c('AK','Cali Current','Gulf of Mexico','NE','Southeast','HI') 
NES1var<-c('1','Establishments', 'Receipts') #for manuscript, probably better to focus on receipts
NES1labs<-c('1','Number of Self-Employed Individuals','Non-Employer Receipts (Millions of Dollars - 2020 Value)')
regnames<-c('Alaska','California Current','Gulf of Mexico','Northeast','Southeast',"Hawai'i")
NESleg<-c( "Fishing","Seafood Markets", "Seafood Processing", 'All Sectors')
  # colorblind friendly palette with black:
  #cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# colorblind friendly blue gradient
mixed<-c('#bf812d', '#dfc27d', '#c7eae5', '#737373')

 for(i in 1:length(folders)) { #i represents the folder name and the region name in the file
    for(j in 2:3){ #j represents the column number in the NES df (skip 1), the variable name(for writing files),and labs (y axis labels)
      #with predicted values from prior calculation
  df1<-read.csv(paste('cross regional data/',folders[i],'/',files[i],' NES combined ', NES1var[j],' gamfit.csv', sep=''))
  NES0<-read.csv(paste('cross regional data/',folders[i],'/',files[i],' NES combined ', NES1var[j],'_data points_raw.csv', sep=''))
  
  #reorder MH factor so that 'All Species' layers on top of 'Without Menhadens'
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
    scale_x_continuous(breaks=seq(1995,2020,1), labels = c("1995",rep("",4),"2000",rep("",4),"2005",rep("",4),'2010',rep("",4),'2015',rep("",4), '2020')) +
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
    geom_point(data = NES0,aes(x = Year, y = unlist(NES0[2]), shape = Sector, fill = NES0$Sector),color = 'gray20', size = 2.7)  + # data points
    scale_shape_manual(labels = NESleg, 
                       values = c(21:23,25)) +
    {if("inc.trend" %in% colnames(df1)) 
      geom_line(aes(x=Year, y = inc.trend, group = Sector),size = 2, color = "#009E73", alpha = 0.6)}  +  #increasing trends
    { if("dec.trend" %in% colnames(df1))
      geom_line(aes(x=Year, y = dec.trend, group = Sector),size = 2, color = "#D55E00", alpha = 0.6) } +
    labs(y = NES1labs[j], x = "Year", title = paste(regnames[i]))   
  
  ggsave(paste('figures/cross regional/NES plots/',files[i],' NES ',NES1var[j],' Combined GAM_ggplot.png',sep=''), 
         width =  10.5, height = 9.5, units = 'in', #w & h in inches
         dpi = 300, bg = 'transparent')
  
#  ggsave(paste('figures/cross regional/NES plots/',files[i],' NES ',NES1var[j],' Combined GAM_zoom.png',sep=''), 
 #        width =  10.5, height = 6.5, units = 'in', #w & h in inches
  #       dpi = 300, bg = 'transparent')
  }
}

##############################################################################
###############################BLS by YEAR####################################
##############################################################################
#ALL VECTORS USED IN BLS SECTION
folders<-c('AK','CC','GOM','NE','SE','HI')
files<-c('AK','Cali Current','Gulf of Mexico','NE','Southeast','HI') 
regnames<-c('Alaska','California Current','Gulf of Mexico','Northeast','Southeast',"Hawai'i")
BLSvar<-c('1','Establishments', 'Employment','Wages') # 
BLSlabs<-c('1','Number of Establishments','Number of Individuals', 'Total Wages (Thousands of Dollars)') # ,'Number of Individuals', 'Total Wages (Thousands of Dollars)'
BLSinds<-c('Fishing', 'Seaf Markets', 'Seaf Packaging', 'Seaf Wholesale')
BLSmain<-c('Fishing', 'Seafood Markets', 'Seafood Packaging', 'Seafood Wholesale')
BLSabbr<-c('Fsh','SM','SP','SW')

#Employment is probably better, but only 3 yr values for HI

############# GET GAMFIT DF WITH TRENDS AND THRESHOLDS MARKED PER SECTOR #############
###################### ALSO MAKE RAW POINTS DF PER SECTOR #############################
folders<-c('AK','CC','GOM','NE','SE','HI') #i
files<-c('AK','Cali Current','Gulf of Mexico','NE','Southeast','HI') #i
BLSinds<-c('Fishing', 'Seaf Markets', 'Seaf Packaging', 'Seaf Wholesale') #k
BLSvar<-c('1','Establishments', 'Employment','Wages') # j


##### also start blank df to hold all deviance and p values for summarized gam stats for the MRIP sims
Region<-c(rep('AK',each =12),rep('CC',each =12),rep('GOM',each =12),rep('NE',each =12),rep('SE',each =12),rep('HI',each =12))
Sector<-character(length = length(Region))
Ind<-character(length = length(Region))
Dev<-numeric(length = length(Region))
pval<-numeric(length = length(Region))
gam.stats<-data.frame(Region,Sector,Ind,Dev,pval)

for(i in 1:length(folders)) { #i represents the folder name and the region name in the file
  for(k in 1:length(BLSinds)) { # which sector
    BLS0<-read.csv(paste('cross regional data/',folders[i],'/',files[i],' BLS ',BLSinds[k],' Totals_deflated.csv', sep='')) 
    BLS0$Employment.Thousands<-BLS0$Employment/1000 #might add in later if larger industry totals are too high
    BLS0$Wages.Millions<-BLS0$Wages.Thousands/1000
    BLS00<-BLS0[,c('Year','Establishments','Employment.Thousands','Wages.Millions')] 
    
    for(j in 2:4){ #j represents the column number in the BLS df (skip 1), BLSvar (for writing files),and BLSlabs (y axis labels)
      #exploratory plot to see if there are obvious weird values
      BLS<- aggregate(BLS00[,j]~Year, data=BLS00, FUN=sum) #aggregate var over all subregions/states
      BLS$Sector<-BLSinds[k]
      names(BLS)<-c('Year',BLSvar[j],'Sector')
      
      write.csv(BLS, file = paste('cross regional data/',folders[i],'/',files[i],' BLS ',BLSinds[k],' ',BLSvar[j],' raw points.csv', sep=''),row.names = F)
      
      plot(BLS[,2]~Year, data = BLS) 
      #looks pretty good
      
      #wrap the gam mod and gamfit function in tryCatch with an error handling function 
      #so that loop continues even after data that is too sparse to produce GAM
      #in those cases, the output df is raw data instead so i can see why the error occurred
     df1<- tryCatch({
        #model
        gammod<-gam(BLS[,2] ~ s(Year), 
                    data = BLS, method = 'REML')
        #run gam_tnt function I wrote to convert raw data df BLS00 to gamfit & trends & thresholds df
      #  df1<-gam_tnt(BLS, 500,gammod,BLS$Year,'Year',BLS$Sector, 'Sector')
      }, error=function(e){data.frame(BLS$Year,BLS[,2])})
     
     gam.stats[(i*12)-12+(k*3)-3+(j-1),2]<-BLSinds[k]
     gam.stats[(i*12)-12+(k*3)-3+(j-1),3]<-BLSvar[j]
     
     gam.stats[(i*12)-12+(k*3)-3+(j-1),4]<-summary(gammod)$dev.expl #pull deviance of current sim to sim.devs df
     gam.stats[(i*12)-12+(k*3)-3+(j-1),5]<-summary(gammod)$s.table[1,4] #pull p value of current sim to sim.devs df
     

      write.csv(df1, file=paste('cross regional data/',folders[i],'/',files[i],' BLS ',BLSinds[k],' ',BLSvar[j],' gamfit.csv', sep=''), row.names = F)
      
    }
  }
}

write.csv(gam.stats, file='outputs/gam stats/BLS.csv', row.names = F)


###########################################################################
############ BLS GAMFIT AND POINTS DF WITH COMBINED SECTORS ###############
###########################################################################
folders<-c('AK','CC','GOM','NE','SE','HI')
files<-c('AK','Cali Current','Gulf of Mexico','NE','Southeast','HI') 
BLSinds<-c('Fishing', 'Seaf Markets', 'Seaf Packaging', 'Seaf Wholesale')
BLSvar<-c('1','Establishments', 'Employment','Wages')

#combine gamfits per sector into one-->still need to get gamfit for 'combined' sector and bind to this
for(i in 1:length(folders)) { #i represents the folder name and the region name in the file
  for(j in 2:4){ #j represents the column number in the BLS df (skip 1), BLSvar (for writing files)
    
    #first read in gamfit of first sector to bind rest to
    BLS.com<-read.csv(paste('cross regional data/',folders[i],'/',files[i],' BLS ',BLSinds[1],' ',BLSvar[j],' gamfit.csv', sep=''))
    
    for(k in 2:4) { #bind other 3 sectors to first
      BLS.new<-read.csv(paste('cross regional data/',folders[i],'/',files[i],' BLS ',BLSinds[k],' ',BLSvar[j],' gamfit.csv', sep=''))
      BLS.com<-if("fit" %in% colnames(BLS.com)) { 
        if( "fit" %in% colnames(BLS.new)) {
          rbind(BLS.com,BLS.new)
        } else { BLS.com }
        } else { BLS.new }
    }        

    BLS.com<-BLS.com[,c( "Sector", "Year",  "fit", "se.fit",  "inc.trend","dec.trend", "threshold", "se.threshold")]
    #reorder columns for consistency
    breakmin<-max((BLS.com$fit+BLS.com$se.fit))
    
    
    #compile raw data in both wide (BLS.W) to calculate RowSums 
    #and long formats (BLS.com0) for final df--will still need to tack on combined values
    #first get one then use actively compiling loop
    BLS.com0<-read.csv(paste('cross regional data/',folders[i],'/',files[i],' BLS ',BLSinds[1],' ',BLSvar[j],' raw points.csv', sep=''))
    BLS.W<-BLS.com0[,c(1:2)]
    names(BLS.W)<-c('Year',BLSinds[1])
    
    for(k in 2:4) {
      #compile raw data points in long format
      BLS.new0<-read.csv(paste('cross regional data/',folders[i],'/',files[i],' BLS ',BLSinds[k],' ',BLSvar[j],' raw points.csv', sep=''))
      BLS.com0<-if(nrow(BLS.com0)>6) { #only include sector if not too many NAs
        if(nrow(BLS.new0)>6) {
          rbind(BLS.com0,BLS.new0)
        } else { BLS.com0 }
      } else { BLS.new0 }
      
      #compile in wide
      BLS.newW<-BLS.new0[,c(1:2)]
      names(BLS.newW)<-c('Year',BLSinds[k])
      BLS.W<-if(nrow(BLS.W)>6) { 
        if(nrow(BLS.newW)>6) {
          join(BLS.W,BLS.newW, by ='Year', type = 'inner')
        } else { BLS.W }
      } else { BLS.newW }
    }     
    
    #calculate rowSums to get combined var values
    BLS.W$Combined<-rowSums(BLS.W[ , c(2:length(colnames(BLS.W)))], na.rm=F) #na.rm = F to make the combined sum NA if any of the sectors are NA
    BLS.W<-BLS.W[,c('Year','Combined')]
    names(BLS.W)<-c('Year',BLSvar[j])
    BLS.W$Sector<- 'Combined'
    BLS.W<-na.omit(BLS.W)
    
    #long df with raw establishments data from all sectors plus combined numbers
    BLS0<-rbind(BLS.W,BLS.com0) #stacks dfs
    #to try multi-factorial gam
    
    write.csv(BLS0, file=paste('cross regional data/',folders[i],'/',files[i],' BLS ',BLSvar[j],' combined_points_raw.csv', sep=''), row.names = F)
    
    #gammod with just 'combined' values
    gammod<-gam(BLS.W[,2] ~ s(Year), 
                data = BLS.W, method = 'REML')
    
    dfcom<-gam_tnt(BLS.W, 500,gammod,BLS.W$Year,'Year',BLS.W$Sector, 'Sector')
    df1<-rbind(BLS.com,dfcom)

    write.csv(df1, file=paste('cross regional data/',folders[i],'/',files[i],' BLS ',BLSvar[j],' combined gamfit.csv', sep=''), row.names = F)
  }
}
############# COMPILED GAM PLOT WITH INDIVIDUAL SECTORS PLUS COMBINED ##########
folders<-c('AK','CC','GOM','NE','SE','HI')
files<-c('AK','Cali Current','Gulf of Mexico','NE','Southeast','HI') 
regnames<-c('Alaska','California Current','Gulf of Mexico','Northeast','Southeast',"Hawai'i")
BLSvar<-c('1','Establishments', 'Employment','Wages') # 
BLSlabs<-c('1','Number of Marine-Related Establishments','Total Employment (Thousands of Individuals)', 'Total Wages (Millions of Dollars - 2020 Value)') 
BLSleg<-c("Fishing", "Seafood Markets", 'Seafood Processing','Seafood Wholesale' ,'All Sectors')
#colors

#plot widths (SE plot wider due to low magnitude y axis labels, and HI wider due to missing processing in legend)
pwid<- c()

# colorblind friendly palette with black:
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# colorblind friendly 
mixed<-c('#bf812d', '#dfc27d', '#c7eae5','#35978f', '#737373')
mixedHI<-c('#bf812d', '#dfc27d','#35978f', '#737373') #just for hawaii which is missing seafood packaging (color 3)
zoom<-c(1,1,0.5,30)

#adjust label sizes and whatnot for manuscript 
for(i in 1:length(folders)) { #i represents the folder name and the region name in the file
  for(j in 3:3){ #j represents the column number in the BLS df (skip 1), BLSvar (for writing files),and BLSlabs (y axis labels)
    
    df1<-read.csv(paste('cross regional data/',folders[i],'/',files[i],' BLS ',BLSvar[j],' combined gamfit.csv', sep=''))
    df1 <- df1[,colSums(is.na(df1))<nrow(df1)] #if any trend or threshold columns are all NA, delete
    
    BLS0<-read.csv(paste('cross regional data/',folders[i],'/',files[i],' BLS ',BLSvar[j],' combined_points_raw.csv', sep=''))
    
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
    

        ##plot gam
    ggplot(df1,
           aes(x = Year, y = fit)) +
      theme(  plot.title=element_text(hjust=0.5, size = 30),
              panel.grid.major = element_blank(), #delete major grid lines
              panel.grid.minor = element_blank(), #delete minor grid lines
              axis.text=element_text(size=28, color = 'black'), #adjust font size of axis tick labels
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
      
      # the following code adjusts palette within the loop
      # HI doesn't have seafood processing for employment or wages
      # need palette needs to skip processing for HI, apply diff palette with 4 colors
      {if(folders[i]!='HI' | (folders[i]=='HI' && BLSvar[j] == 'Establishments')) #palette with 5 colors if not HI or HI Establishments
      scale_fill_manual(labels = BLSleg,
                        values = mixed)} + #color combined CI, individual sectors are gray
      {if(folders[i]=='HI'&& BLSvar[j] != 'Establishments') #palette minus 3rd color (-seafood packaging)
        scale_fill_manual(labels = BLSlegHI,
                          values = mixedHI)}  + 
      {if("threshold" %in% colnames(df1))
        geom_ribbon(aes(ymin = threshold-(2*se.threshold), ymax = threshold+(2*se.threshold), group = Sector), 
                    alpha = 0.8, fill = 'gray40', color = 'black')} + #recolor se band for threshold regions
      geom_line(size = 0.7, aes(color = Sector), alpha = 1) + #full gam function
      {if(folders[i]!='HI' | (folders[i]=='HI' && BLSvar[j] == 'Establishments'))
      scale_color_manual(labels = BLSleg, 
                         values = c(mixed[1:4],'black'))} +
      {if(folders[i]=='HI'&& BLSvar[j] != 'Establishments') #palette minus 3rd color (-seafood packaging)
        scale_color_manual(labels = BLSlegHI, 
                           values = c(mixedHI[1:3],'black'))} +
      geom_point(data = BLS0,aes(x = Year, y = BLS0[,2], shape = Sector, fill = Sector), size = 2.7) + # data points
      {if(folders[i]!='HI' | (folders[i]=='HI' && BLSvar[j] == 'Establishments'))
      scale_shape_manual(labels = BLSleg, 
                         values = c(21:25))} +
      {if(folders[i]=='HI'&& BLSvar[j] != 'Establishments') #shapes minus 3rd  (-seafood packaging)
        scale_shape_manual(labels = BLSlegHI, 
                           values = c(21,22,24,25))} +
      {if("inc.trend" %in% colnames(df1)) 
        geom_line(aes(x=Year, y = inc.trend, group = Sector),size = 1.3, color = "#009E73", alpha = 0.5)}   +  #increasing trends
      { if("dec.trend" %in% colnames(df1))
        geom_line(aes(x=Year, y = dec.trend, group = Sector),size = 1.3, color = "#D55E00", alpha = 0.5) }  +
      labs(y = BLSlabs[j], x = "Year", title = paste(regnames[i]))   
    
    ggsave(paste('figs for slides/',files[i],' BLS ',BLSvar[j],' Combined GAM_ggplot.png',sep=''), 
           width =  11.3, height = 10, units = 'in', #w & h in inches
           dpi = 300, bg = 'transparent')
    
#    ggsave(paste('figures/cross regional/BLS plots/',files[i],' BLS ',BLSvar[j],' Combined GAM_ggplot.png',sep=''), 
 #          width =  10.5, height = 10, units = 'in', #w & h in inches
  #         dpi = 300, bg = 'transparent')
    
 #   ggsave(paste('figures/cross regional/BLS plots/',files[i],' BLS Combined GAM_ggplot_zoom.png',sep=''), 
  #         width =  10.5, height = 4.8, units = 'in', #w & h in inches
   #        dpi = 300, bg = 'transparent')
#       ggsave(paste('figures/cross regional/BLS plots/',files[i],' BLS ',BLSvar[j],' Combined GAM_ggplot_zoom_AK.png',sep=''), 
 #            width =  10.5, height = 4.8, units = 'in', #w & h in inches
  #          dpi = 300, bg = 'transparent')
  }   
}
    ##?? whole other plot just to make a manual legend
    #might have more efficient method
    ##plot for manual threshold/trends legend
    
    df1.0<-df1[df1$Sector == 'Fishing' |
                 df1$Sector == 'Seaf Markets' |
                 df1$Sector == 'Seaf Packaging',]
    
    df1.0$Sector <- factor(df1.0$Sector, levels = c("Fishing", "Seaf Markets", 'Seaf Packaging'))    
    
    ggplot(df1.0,
           aes(x = Year, y = fit)) +
      theme(  plot.title=element_text(hjust=0.5, size = 16),
              panel.grid.major = element_blank(), #delete major grid lines
              panel.grid.minor = element_blank(), #delete minor grid lines
              axis.text=element_text(size=13.8, color = 'black'), #adjust font size of axis tick labels
              axis.title=element_text(size=15.2,face="plain"), #adjust size of axis titles
              axis.title.x = element_text(margin = margin(t = 12, r = 0, b = 0, l = 0)), #increases distance between x axis label and plot
              panel.background = element_rect(fill = "transparent",colour = 'black', size = 1),
              plot.background = element_rect(fill = "transparent",colour = NA),
              plot.margin = unit(c(0.3,2,0.3,0.3), "cm"),
              legend.position = 'right', # c(0.37,0.57),
              legend.background = element_rect(fill="transparent", color = 'black'),
              legend.key = element_rect(fill = "transparent", colour = NA),
              legend.key.size = unit(1, 'cm'),
              legend.spacing.y = unit(0.2, 'cm'),
              legend.text=element_text(size=17),
              legend.title=element_blank(),
              legend.margin = margin(3,12,10,10)) +
      guides(fill = guide_legend(byrow = T)) +
      geom_ribbon(aes(ymin = fit-(2*se.fit), ymax = fit+(2*se.fit), fill = Sector)) + #95CI = (2*SE) for gam function
      scale_fill_manual(labels = c("Threshold","Increasing Trend", "Decreasing Trend"),
                        values = scales::alpha(c( 'gray40', 'transparent','transparent'),c(0.8,0,0))) + #color combined CI, individual sectors are gray
      geom_line(aes(color = Sector, size = Sector)) + #full gam function
      scale_size_manual(labels = c("Threshold","Increasing Trend", "Decreasing Trend"),
                        values = c(1,2,2)) +
      scale_color_manual(labels = c("Threshold","Increasing Trend", "Decreasing Trend"), 
                         values = scales::alpha(c('black', '#009E73','#D55E00'),c(1,0.5,0.5))) 

    ggsave('figures/cross regional/BLS plots/BLS threshold legend.png', 
           width =  9.2, height = 8, units = 'in', #w & h in inches
           dpi = 300, bg = 'transparent')

    
###########################################################################
################################END FOR NOW################################
###########################################################################
