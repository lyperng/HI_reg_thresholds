####### Dec 2020####
###code by Lansing Perng



#-----------------------------------------------------------------------------#
###########################FOSS REEF/PELAGIC/DEEP data#########################
#-------------------------------#ends line 595#-------------------------------#

###############################################################################
######################FOSS REEF/PELAGIC/DEEP data by YEAR######################
###############################################################################
#split in 'Shannon_Diversity_Calculation.R'
#optimized diversity and group split in 'HI_Thresholds_Analyses_2022.R'
#split species type into reef, pelagic, and deep; some species didn't fit into either
#and were classified as freshwater, or offshore
#LOBSTERS: banded spiny & slipper (reef), caribbean spiny (offshore)
#(1) deflated FOSS data in 'Annual_GDP_Delator_Code.R'
#(2) script for diversity calculations & scaling to millions in 'Shannon_Diversity_Calculation.R' 

#load cleaned FOSS
FOSS0<-read.csv('data/HCC cleaned/FOSS with Millions.csv')

FOSSreef<-read.csv('data/HCC cleaned/FOSS reef fish with Millions.csv')
colnames(FOSSreef)<-c('Year','CommDiv.r','Revenue.Millions.r','Catch.Millions.r')
FOSSpel<-read.csv('data/HCC cleaned/FOSS pelagic fish with Millions.csv')
colnames(FOSSpel)<-c('Year','CommDiv.p','Revenue.Millions.p','Catch.Millions.p')
FOSSdeep<-read.csv('data/HCC cleaned/FOSS deep fish with Millions.csv')
colnames(FOSSdeep)<-c('Year','CommDiv.d','Revenue.Millions.d','Catch.Millions.d')

FOSS1<-join(FOSSreef,FOSSpel,by = 'Year', type = 'inner')
FOSS2<-join(FOSS1,FOSSdeep,by = 'Year', type = 'inner')
FOSS<-join(FOSS2,FOSS0,by = 'Year', type = 'inner')

var<-colnames(FOSS)

titles<-c('Year','Reef Commercial Diversity', 'Reef Revenue',
          'Reef Commercial Catch','Pelagic Commercial Diversity', 'Pelagic Revenue',
          'Pelagic Commercial Catch','Deep Commercial Diversity', 'Deep Revenue',
          'Deep Commercial Catch','Total Commercial Diversity', 'Total Revenue',
          'Total Commercial Catch')

labs<-c('Year','Effective Shannon Diversity', 'Millions of Dollars',
        'Millions of Pounds','Effective Shannon Diversity', 'Millions of Dollars',
        'Millions of Pounds','Effective Shannon Diversity', 'Millions of Dollars',
        'Millions of Pounds','Effective Shannon Diversity', 'Millions of Dollars',
        'Millions of Pounds')

for(j in 2:length(var)){ #j represents the column number in the FOSS df (skip 1) & the y label for each plot
  #exploratory plot to see if there are obvious weird values
  plot(FOSS[,j]~Year, data = FOSS) 
  #looks pretty good
  
  gammod<-gam(FOSS[,j] ~ s(Year), 
              data = FOSS, method = 'REML')
  
  gammod$sp
  
  pdf(paste('figures/HI region specific/FOSS split/',titles[j],' Assumptions.pdf',sep=''), width =  7, height = 5.5, #w & h in inches
  ) #sets high resolution--default is 150
  gam.check(gammod) #much better!
  dev.off()
  
}

#not sure why but the appraise() plot doesnt come out in the for loop

for(j in 2:length(var)){ #j represents the column number in the FOSS df (skip 1) & the y label for each plot
  
  gammod<-gam(FOSS[,j] ~ s(Year), 
              data = FOSS, method = 'REML')
  
  ####################PLOT GAM##############
  
  png(paste('figures/HI region specific/FOSS split/',titles[j],' GAM.png',sep=''), width =  2400, height = 1700, #w & h in pixels
      res = 300) #sets high resolution--default is 150
  
  par(family = 'Helvetica',
      bg = NA, #empty background--easier to place on ppt
      mar = c(5.6, 5.7, 4, 2.4) #sets margins (bottom, left, top, right)--default values c(5.1,4.1,4.1,2.1)
  )
  plot(gammod, residuals = T, pch = 1, #plot points with empty circles
       seWithMean = TRUE, shade = T, shade.col = 'lightblue', #adds model uncertainty and shades it light blue
       shift = coef(gammod)[1],  #adds model intercept by shifting y axis by first coef
       lwd = 1.4,
       main = paste(titles[j]," in Hawai'i",sep=''),
       font.main = 1, #makes the title regular (default is bold)
       cex.main = 1.6, #expansion factor for title
       ylab = labs[j],
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
  
  fd<-derivatives(gammod, newdata = newd, eps = EPS, unconditional = UNCONDITIONAL, order = 1, interval = "simultaneous")
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
    scale_x_continuous(breaks=seq(min(FOSS$Year),max(FOSS$Year),10)) +
    geom_hline(aes(yintercept = 0), size =0.2 ) +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +
    geom_line() +
    #  scale_x_reverse() +
    labs(y = "s'(X)", x = "Year") +  
    geom_segment(data=fd, mapping=aes(x=data, y=0, xend=data,
                                      yend=(ifelse(lower>0, lower, ifelse(upper<0, upper, 0)))), # arrow=arrow(),
                 size=0.5, color="indianred3")
  
  ggsave(paste('figures/HI region specific/FOSS split/',titles[j],' FD.png',sep=''), 
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
    scale_x_continuous(breaks=seq(min(FOSS$Year),max(FOSS$Year),10)) +
    geom_hline(aes(yintercept = 0), size = 0.2) +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +
    geom_line() +
    labs(y = 's"(X)', x = "Year") + 
    geom_segment(data=sdCR, mapping=aes(x=data, y=0, xend=data,
                                        yend=(ifelse(lower>0, lower, ifelse(upper<0, upper, 0)))), # arrow=arrow(),
                 size=0.5, color="indianred3")
  
  ggsave(paste('figures/HI region specific/FOSS split/',titles[j],' SD.png',sep=''), 
         width =  8.47, height = 5, units = 'in', #w & h in inches
         dpi = 300, bg = 'transparent')
 
}

###############################################################################
####################### FOSS data (reef only) by dryad ########################
###############################################################################

#join with dryad (1993-2016)
dry<-read.csv('data/dryad annual averages.csv')
dry$Ratio<-dry$Calcifiers/dry$Algae

reefdry<-join(dry,FOSSreef,by = 'Year', type = 'inner') #even fewer time points
#cols 2:15 are dryad driver with varying levels of ecosystem description (from specific herb groups to a califier:algae ratio)
#cols 16-18 are FOSS variables (responses)

var<-colnames(reefdry) #vector of column names for labeling plots

#explore relationships before deciding which to pursue in GAMs

pdf(paste('figures/HI region specific/dryad as drivers/exploratory with FOSS reef.pdf',sep=''), width =  9, height = 6, #w & h in inches
) #sets high resolution--default is 150
#plotting 3x14 = 42 exploratory plots

par(mfrow=c(3, 2),     # 3x2 layout
    oma = c(1, 1, 0, 0), # two rows of text at the outer left and bottom margin
    mar = c(3, 3, 0, 0), # space for one row of text at ticks and to separate plots
    mgp = c(2, 1, 0),    # axis label at 2 rows distance, tick labels at 1 row
    xpd = NA) #lay out plots in 3 rows, 2 columns (6 per page)

for(i in 2:15){ #i represents the column number in the reefdry df (skip 1) for dryad drivers & the y label for each plot
  #exploratory plot to see if there are obvious weird values
  for (j in 16:18){ #j 
    
    plot(reefdry[,j]~reefdry[,i], data = reefdry,
         ylab = colnames(reefdry[j]),
         xlab = colnames(reefdry[i])) 
    
    #looks pretty good
    
  }
}

dev.off()
######no clear patterns or trends so did not go on to do GAM


###############################################################################
############### FOSS data (including reef/pel/deep) by chl.a ##################
###############################################################################
#split species type into reef, pelagic, and some species didn't fit into either
#and were classified as freshwater, deep, or offshore
#LOBSTERS: banded spiny & slipper (reef), caribbean spiny (offshore)
#original script for splitting & div calculations in 'Shannon_Diversity_Calculation.R' 
#script for scaling to millions in 'Gratia_HI_Generic_Inds_Reef.R'

chla<-read.csv('data/chlorophyll a.csv')
chla<-chla[,c(1,7)]
colnames(chla)<-c("Year",'Chlorophyll.a')
chl.FOSS<-join(chla,FOSS,by = 'Year', type = 'inner')

#vectors to be used in for loop

titles0<-c('1','2','Reef Commercial Diversity', 'Reef Revenue',
           'Reef Commercial Catch','Pelagic Commercial Diversity', 'Pelagic Revenue',
           'Pelagic Commercial Catch','Deep Commercial Diversity', 'Deep Revenue',
           'Deep Commercial Catch','Total Commercial Diversity', 'Total Revenue',
           'Total Commercial Catch')

labs0<-c('1','2','Effective Shannon Diversity', 'Millions of Dollars',
         'Millions of Pounds','Effective Shannon Diversity', 'Millions of Dollars',
         'Millions of Pounds','Effective Shannon Diversity', 'Millions of Dollars',
         'Millions of Pounds','Effective Shannon Diversity', 'Millions of Dollars',
         'Millions of Pounds')

##########################GAMs################
for(j in 3:14){ #j represents the column number in the chl.FOSS df & the y label for each plot
  
  SEmod<-gam(chl.FOSS[,j]~ s(Chlorophyll.a),
             data = chl.FOSS, method = 'REML', correlation=corAR1(form=~Year))
  
  ####################PLOT GAM##############
  
  png(paste('figures/HI region specific/FOSSclim/chl a/',titles0[j],'by chl a GAM.png',sep=''), width =  2400, height = 1700, #w & h in pixels
      res = 300) #sets high resolution--default is 150
  
  par(family = 'Helvetica',
      bg = NA, #empty background--easier to place on ppt
      mar = c(5.6, 5.7, 4, 2.4) #sets margins (bottom, left, top, right)--default values c(5.1,4.1,4.1,2.1)
  )
  plot(SEmod, residuals = T, pch = 1, #plot points with empty cirles
       seWithMean = TRUE, shade = T, shade.col = 'lightblue', #adds model uncertainty and shades it light blue
       shift = coef(SEmod)[1],  #adds model intercept by shifting y axis by first coef
       lwd = 1.4,
       main = paste(titles0[j]," in Hawai'i",sep=''),
       font.main = 1, #makes the title regular (default is bold)
       cex.main = 1.6, #expansion factor for title
       ylab = labs0[j],
       xlab = 'Chlorophyll a',
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
  newd <- with(chl.FOSS, data.frame(Chlorophyll.a = seq(min(Chlorophyll.a), max(Chlorophyll.a), 
                                                        length = n)))
  
  fd <- derivatives(SEmod, newdata = newd, eps = EPS, unconditional = UNCONDITIONAL,
                    n_sim = N, order = 1, interval = 'simultaneous')
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
    scale_x_continuous(breaks=seq(min(chl.FOSS$Chlorophyll.a),max(chl.FOSS$Chlorophyll.a),5), 
                       labels = scales::number_format(accuracy = 5)) +
    geom_hline(aes(yintercept = 0), size =0.2 ) +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +
    geom_line() +
    #  scale_x_reverse() +
    labs(y = "s'(X)", x = "Chlorophyll a")
  
  ggsave(paste('figures/HI region specific/FOSSclim/chl a/',titles0[j],' by chl a FD.png',sep=''), 
         width =  8.47, height = 5, units = 'in', #w & h in inches
         dpi = 300, bg = 'transparent')
  
  
  #plot 2nd derivatives
  
  ## second derivatives of all smooths using central finite differences with simultaneous CI
  sdCR<-derivatives(SEmod, newdata = newd, eps = EPS, unconditional = UNCONDITIONAL, order = 2, interval = "simultaneous")
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
    scale_x_continuous(breaks=seq(min(chl.FOSS$Chlorophyll.a),max(chl.FOSS$Chlorophyll.a),5), 
                       labels = scales::number_format(accuracy = 5)) +
    geom_hline(aes(yintercept = 0), size = 0.2) +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +
    geom_line() +
    labs(y = 's"(X)', x = "Chlorophyll a") + 
    geom_segment(data=sdCR, mapping=aes(x=data, y=0, xend=data,
                                        yend=(ifelse(lower>0, lower, ifelse(upper<0, upper, 0)))), # arrow=arrow(),
                 size=0.5, color="indianred3")
  
  ggsave(paste('figures/HI region specific/FOSSclim/chl a/',titles0[j],' by chl a SD.png',sep=''), 
         width =  8.47, height = 5, units = 'in', #w & h in inches
         dpi = 300, bg = 'transparent')
  
  #graph is super wiggly for some reason, also the y axis seems way too big??
}

###############################################################################
############################FOSS data by PDO/ENSO#############################
###############################################################################
#load cleaned FOSS
FOSS<-read.csv('data/HCC cleaned/FOSS Totals and Diversity.csv')

#join climate with HIspec
FOSSclim<-join(LTclim,FOSS,by = 'Year', type = 'inner') #even fewer time points

#vectors to be used in for loop

restitles2<-c('1','2','3','4','Commercial Revenue', 'Commercial Fishing Landings',
              'Commerical Catch Diversity')

reslabs2<-c('1','2','3','4','Revenue (Dollars)','Landings (Pounds)',
            'Catch Diversity (Shannon Index)')

###############################DRIVER: PDO##################################
for(j in 5:7){ #responses
  #exploratory plot to see if there are obvious weird values
  plot(FOSSclim[,j]~PDO, data = FOSSclim) 
  #looks pretty good
  
  SEmod<-gam(FOSSclim[,j]~ s(PDO),  # not sure why drivers can't be looped as FOSSclim[,i] but keep getting error: 
             #Error in variable.summary(gp$pf, dl, nrow(mf)) : 
             #'list' object cannot be coerced to type 'double'
             data = FOSSclim, method = 'REML', correlation=corAR1(form=~Year))
  
  SEmod$sp
  
  pdf(paste('figures/HI region specific/FOSSclim/',restitles2[j],' by PDO Assumptions.pdf',sep=''), width =  7, height = 5.5, #w & h in inches
  ) #sets high resolution--default is 150
  gam.check(SEmod) #much better!
  dev.off()
}


for(j in 5:7){ #j represents the column number in the FOSSclim df & the y label for each plot
  
  SEmod<-gam(FOSSclim[,j]~ s(PDO),
             data = FOSSclim, method = 'REML', correlation=corAR1(form=~Year))
  
  ####################PLOT GAM##############
  
  png(paste('figures/HI region specific/FOSSclim/',restitles2[j],'by PDO GAM.png',sep=''), width =  2400, height = 1700, #w & h in pixels
      res = 300) #sets high resolution--default is 150
  
  par(family = 'Helvetica',
      bg = NA, #empty background--easier to place on ppt
      mar = c(5.6, 5.7, 4, 2.4) #sets margins (bottom, left, top, right)--default values c(5.1,4.1,4.1,2.1)
  )
  plot(SEmod, residuals = T, pch = 1, #plot points with empty cirles
       seWithMean = TRUE, shade = T, shade.col = 'lightblue', #adds model uncertainty and shades it light blue
       shift = coef(SEmod)[1],  #adds model intercept by shifting y axis by first coef
       lwd = 1.4,
       main = paste(restitles2[j]," in Hawai'i",sep=''),
       font.main = 1, #makes the title regular (default is bold)
       cex.main = 1.6, #expansion factor for title
       ylab = reslabs2[j],
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
  newd <- with(FOSSclim, data.frame(PDO = seq(min(PDO), max(PDO), 
                                              length = n)))
  
  fd <- derivatives(SEmod, newdata = newd, eps = EPS, unconditional = UNCONDITIONAL,
                    n_sim = N, order = 1, interval = 'simultaneous')
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
    scale_x_continuous(breaks=seq(min(FOSSclim$PDO),max(FOSSclim$PDO),10), 
                       labels = scales::number_format(accuracy = 5)) +
    geom_hline(aes(yintercept = 0), size =0.2 ) +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +
    geom_line() +
    #  scale_x_reverse() +
    labs(y = "s'(X)", x = "Pacific Decadal Oscillation")
  
  ggsave(paste('figures/HI region specific/FOSSclim/',restitles2[j],' by PDO FD.png',sep=''), 
         width =  8.47, height = 5, units = 'in', #w & h in inches
         dpi = 300, bg = 'transparent')
  
  
  #plot 2nd derivatives
  
  ## second derivatives of all smooths using central finite differences with simultaneous CI
  sdCR<-derivatives(SEmod, newdata = newd, eps = EPS, unconditional = UNCONDITIONAL, order = 2, interval = "simultaneous")
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
    scale_x_continuous(breaks=seq(min(FOSSclim$PDO),max(FOSSclim$PDO),10), 
                       labels = scales::number_format(accuracy = 5)) +
    geom_hline(aes(yintercept = 0), size = 0.2) +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +
    geom_line() +
    labs(y = 's"(X)', x = "Pacific Decadal Oscillation") + 
    geom_segment(data=sdCR, mapping=aes(x=data, y=0, xend=data,
                                        yend=(ifelse(lower>0, lower, ifelse(upper<0, upper, 0)))), # arrow=arrow(),
                 size=0.5, color="indianred3")
  
  ggsave(paste('figures/HI region specific/FOSSclim/',restitles2[j],' SD.png',sep=''), 
         width =  8.47, height = 5, units = 'in', #w & h in inches
         dpi = 300, bg = 'transparent')
  
  #graph is super wiggly for some reason, also the y axis seems way too big??
}

##############################DRIVER:ENSO SST#################################
for(j in 5:7){ #responses
  #exploratory plot to see if there are obvious weird values
  plot(FOSSclim[,j]~Nino3.4, data = FOSSclim) 
  #looks pretty good
  
  SEmod<-gam(FOSSclim[,j]~ s(Nino3.4),  # not sure why drivers can't be looped as FOSSclim[,i] but keep getting error: 
             #Error in variable.summary(gp$pf, dl, nrow(mf)) : 
             #'list' object cannot be coerced to type 'double'
             data = FOSSclim, method = 'REML', correlation=corAR1(form=~Year))
  
  SEmod$sp
  
  pdf(paste('figures/HI region specific/FOSSclim/',restitles2[j],' by ENSO Assumptions.pdf',sep=''), width =  7, height = 5.5, #w & h in inches
  ) #sets high resolution--default is 150
  gam.check(SEmod) #much better!
  dev.off()
}


for(j in 5:7){ #j represents the column number in the FOSSclim df & the y label for each plot
  
  SEmod<-gam(FOSSclim[,j]~ s(Nino3.4),
             data = FOSSclim, method = 'REML', correlation=corAR1(form=~Year))
  
  ####################PLOT GAM##############
  
  png(paste('figures/HI region specific/FOSSclim/',restitles2[j],'by ENSO GAM.png',sep=''), width =  2400, height = 1700, #w & h in pixels
      res = 300) #sets high resolution--default is 150
  
  par(family = 'Helvetica',
      bg = NA, #empty background--easier to place on ppt
      mar = c(5.6, 5.7, 4, 2.4) #sets margins (bottom, left, top, right)--default values c(5.1,4.1,4.1,2.1)
  )
  plot(SEmod, residuals = T, pch = 1, #plot points with empty cirles
       seWithMean = TRUE, shade = T, shade.col = 'lightblue', #adds model uncertainty and shades it light blue
       shift = coef(SEmod)[1],  #adds model intercept by shifting y axis by first coef
       lwd = 1.4,
       main = paste(restitles2[j]," in Hawai'i",sep=''),
       font.main = 1, #makes the title regular (default is bold)
       cex.main = 1.6, #expansion factor for title
       ylab = reslabs2[j],
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
  newd <- with(FOSSclim, data.frame(Nino3.4 = seq(min(Nino3.4), max(Nino3.4), 
                                                  length = n)))
  
  fd <- derivatives(SEmod, newdata = newd, eps = EPS, unconditional = UNCONDITIONAL,
                    n_sim = N, order = 1, interval = 'simultaneous')
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
    scale_x_continuous(breaks=seq(min(FOSSclim$Nino3.4),max(FOSSclim$Nino3.4),10), 
                       labels = scales::number_format(accuracy = 5)) +
    geom_hline(aes(yintercept = 0), size =0.2 ) +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +
    geom_line() +
    #  scale_x_reverse() +
    labs(y = "s'(X)", x = "Pacific Decadal Oscillation")
  
  ggsave(paste('figures/HI region specific/FOSSclim/',restitles2[j],' by ENSO FD.png',sep=''), 
         width =  8.47, height = 5, units = 'in', #w & h in inches
         dpi = 300, bg = 'transparent')
  
  
  #plot 2nd derivatives
  
  ## second derivatives of all smooths using central finite differences with simultaneous CI
  sdCR<-derivatives(SEmod, newdata = newd, eps = EPS, unconditional = UNCONDITIONAL, order = 2, interval = "simultaneous")
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
    scale_x_continuous(breaks=seq(min(FOSSclim$Nino3.4),max(FOSSclim$Nino3.4),10), 
                       labels = scales::number_format(accuracy = 5)) +
    geom_hline(aes(yintercept = 0), size = 0.2) +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +
    geom_line() +
    labs(y = 's"(X)', x = "Pacific Decadal Oscillation") + 
    geom_segment(data=sdCR, mapping=aes(x=data, y=0, xend=data,
                                        yend=(ifelse(lower>0, lower, ifelse(upper<0, upper, 0)))), # arrow=arrow(),
                 size=0.5, color="indianred3")
  
  ggsave(paste('figures/HI region specific/FOSSclim/',restitles2[j],' SD.png',sep=''), 
         width =  8.47, height = 5, units = 'in', #w & h in inches
         dpi = 300, bg = 'transparent')
  
  #graph is super wiggly for some reason, also the y axis seems way too big??
}

###############################################################################
###############################clean NCRMP data################################
###############################################################################
##ncrmp data are already deflated
ncrmp0<-read.csv('data/HCC cleaned/NCRMP_socio_secondarydata_Hawaii_ERDDAP.csv')
ncrmp1<-ncrmp0[,c('Variable','Hawaii.data','category_text')] #keep these columns

desc<-read.csv('data/HCC cleaned/NCRMP_socio_secondarydata_dictionary_variables.csv')
colnames(desc)<-c('Variable','Description') #rename columns so we can join by 'Variable'

ncrmp<-join(ncrmp1,desc,'Variable','inner') #join descriptions to ncrmp dataset
ncrmp$Hawaii.data <-  as.numeric(gsub("[$,%]","", ncrmp$Hawaii.data)) #Replace '$',',',and '%' with nothing--aka delete commas
#also converts columns whose value is '.' to NA, which is accurate

#extract certain characters from Description column to create new columns
#extract the 4th to last character to the last character into 'Year' column
ncrmp$Year<-substr(ncrmp$Description, nchar(ncrmp$Description)-4+1, nchar(ncrmp$Description))

#extract the 1st to 7th to last characters (which is every thing except ' - 2xxx')
ncrmp$Category<-substr(ncrmp$Description,1, nchar(ncrmp$Description)-7)

#subset the inds we want
HIspec<-ncrmp[ncrmp$Category=="total commercial fishery landings" | #subset only rows in certain categories
                ncrmp$Category=="total commercial fishery landings for reef and bottom fish species in addition to any other shellfish and marine life that depend on a coral reef or rocky hard bottom ecosystem" |
                ncrmp$Category=="total commercial fishery revenue" |
                ncrmp$Category=="total commercial fishery revenue for reef and bottom fish species in addition to any other shellfish and marine life that depend on a coral reef or rocky hard bottom ecosystem" |
                ncrmp$Category=="total recreational fishery landings" |
                ncrmp$Category=="total recreational fishery landings for reef and bottom fish species in addition to any other shellfish and marine life that depend on a coral reef or rocky hard bottom ecosystem" |
                ncrmp$Category=="total GDP produced by the living resources industry" |
                ncrmp$Category=="total employment within living resources industry" | 
                ncrmp$Category=="total establishments within living resources industry" |
                ncrmp$Category=="Proportion of total GDP produced by living resources industry"  |
                ncrmp$Category=="Proportion of total employees employed in living resources" |
                ncrmp$Category=="Proportion of total estblishments dedicated to living resources" |
                ncrmp$Category=="Commerical fishing license revenue" |
                ncrmp$Category=="Total number of tourism arrivals" |
                ncrmp$Category=="total GDP produced by the tourism industry" |
                ncrmp$Category=="total employment within tourism industry" |
                ncrmp$Category=="total establishments within tourism industry"  |
                ncrmp$Category=="Proportion of total GDP produced by tourism industry"|
                ncrmp$Category=="Proportion of total employees employed in tourism" |
                ncrmp$Category=="Proportion of total estblishments dedicated to tourism" |
                ncrmp$Category=="Total visitor spending" |
                ncrmp$Category=="Hotel occupancy rate"  |
                ncrmp$Category=="Percent of beach days affected by notification actions at monitored beaches" |
                ncrmp$Category=="Total toxic releases" ,]

#subset toxic releases per coast separately because the initial 'Description' did not include year
HIspec_coast<-ncrmp[ncrmp$Category=="Toxic releases per mile of co",]
HIspec_coast$Year<-substr(HIspec_coast$Variable, nchar(HIspec_coast$Variable)-4+1, nchar(HIspec_coast$Variable))
HIspec_coast$Category<-HIspec_coast$Description

# combine 2 dfs by row
HIspec <- rbind(HIspec, HIspec_coast)

#clean up columns (reorder and delete unnecessary ones)
HIspec<-HIspec[,c('Year','Category','Hawaii.data')]
unique(HIspec$Category) #25 indicators

#long to wide
HIspec<-spread(HIspec, Category,Hawaii.data) #a lot of NAs for years where only some data sets have values--it's fine for GAMs

#rename columns
colnames(HIspec)<-c('Year','Commercial_Licenses_Revenue','Hotel_Occupancy', 'Notification_Actions_PCT_Beach_Days',
                    'PCT_LivRes_Emp', 'PCT_Tourism_Emp', 'PCT_LivRes_Est', 'PCT_Tourism_Est',
                    'PCT_LivRes_GDP', 'PCT_Tourism_GDP','Commercial_Landings','Commercial_Landings_Reef',
                    'Commercial_Revenue', 'Commercial_Revenue_Reef','LivRes_Emp', 'Tourism_Emp',
                    'LivRes_Est','Tourism_Est','LivRes_GDP','Tourism_GDP','Tourism_Arrivals',
                    'Recreational_Landings','Recreational_Landings_Reef',
                    'Toxic_Releases_TOT','Visitor_Spending','Toxic_Releases_per_Coastline')
HIspec$PCT_Beach_Days_W.out_Not<-100-HIspec$Notification_Actions_PCT_Beach_Days #convert to a desirable variable (beach days withOUT notification actions instead)
HIspec<-HIspec[,-4] #subtract var beach days WITH notif

#reorder columns (grouped by drivers, ocean-related econ, tourism, recreation)
HIspec<-HIspec[,c(1,23,25,14,16,18,4,6,8,2,10:13,15,17,19,5,7,9,20,24,3,21,22,26)]

write.csv(HIspec, 'data/NCRMP HI region specific soc indicators.csv', row.names = F)

###############################################################################
##############################NCRMP data by YEAR###############################
###############################################################################

HIspec<-read.csv('data/NCRMP HI region specific soc indicators.csv')

var<-colnames(HIspec)
titles<-c('Year','Toxic Releases','Toxic Releases per Coastline',
          'Living Resources Employment','Living Resources Establishments', 'Living Resources GDP',
          'Proportion of Employment in LR','Proportion of Establishments in LR','Proportion of GDP from LR',
          'Commercial Licenses Revenue','Commerical Fishing Landings','Reef-Related Commercial Landings', 
          'Commercial Fishing Revenue','Reef-Related Commerical Fishing Revenue','Tourism Employment',
          'Tourism Establishments','Tourism GDP', 'Proportion of Employment in Tourism',
          'Proportion of Establishments in Tourism','Proportion of GDP from Tourism','Tourism Arrivals',
          'Total Visitor Spending', 'Hotel Occupancy','Recreational Fishing Landings',
          'Reef-Related Recreational Landings','Beach Days Without Notification Actions')

labs<-c('Year','Total Releases (Pounds)','Total Releases per Coastline (Permits per Mile)',
        'Number of Employees','Number of Establishments','Gross Domestic Product (Dollars)',
        'Percent','Percent','Percent',
        'Revenue (Dollars)','Total Landings (Pounds)','Landings (Pounds)',
        'Revenue (Dollars)','Revenue (Dollars)','Number of Employees',
        'Number of Establishments','Gross Domestic Product (Dollars)','Percent',
        'Percent','Percent','Number of People',
        'Visitor Spending (Dollars)','Percent Occupied','Total Landings (Pounds)',
        'Landings (Pounds)', 'Percent')

for(j in 2:length(colnames(HIspec))){ #j represents the column number in the HIspec df (skip 1) & the y label for each plot
  #exploratory plot to see if there are obvious weird values
  plot(HIspec[,j]~Year, data = HIspec) 
  #looks pretty good
  
  gammod<-gam(HIspec[,j] ~ s(Year), 
              data = HIspec, method = 'REML')
  
  gammod$sp
  
  pdf(paste('figures/HI region specific/',var[j],' Assumptions.pdf',sep=''), width =  7, height = 5.5, #w & h in inches
  ) #sets high resolution--default is 150
  gam.check(gammod) #much better!
  dev.off()
  
}

#not sure why but the appraise() plot doesnt come out in the for loop

for(j in 2:length(colnames(HIspec))){ #j represents the column number in the HIspec df (skip 1) & the y label for each plot
  
  gammod<-gam(HIspec[,j] ~ s(Year), 
              data = HIspec, method = 'REML')
  
  ####################PLOT GAM##############
  
  png(paste('figures/HI region specific/',var[j],' GAM.png',sep=''), width =  2400, height = 1700, #w & h in pixels
      res = 300) #sets high resolution--default is 150
  
  par(family = 'Helvetica',
      bg = NA, #empty background--easier to place on ppt
      mar = c(5.6, 5.7, 4, 2.4) #sets margins (bottom, left, top, right)--default values c(5.1,4.1,4.1,2.1)
  )
  plot(gammod, residuals = T, pch = 1, #plot points with empty cirles
       seWithMean = TRUE, shade = T, shade.col = 'lightblue', #adds model uncertainty and shades it light blue
       shift = coef(gammod)[1],  #adds model intercept by shifting y axis by first coef
       lwd = 1.4,
       main = paste(titles[j]," in Hawai'i",sep=''),
       font.main = 1, #makes the title regular (default is bold)
       cex.main = 1.6, #expansion factor for title
       ylab = labs[j],
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
  newd <- with(HIspec, data.frame(Year = seq(min(Year), max(Year), 
                                             length = n)))
  
  fd<-derivatives(gammod, newdata = newd, eps = EPS, unconditional = UNCONDITIONAL, order = 1, interval = "simultaneous")
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
    scale_x_continuous(breaks=seq(min(HIspec$Year),max(HIspec$Year),2)) +
    geom_hline(aes(yintercept = 0), size =0.2 ) +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +
    geom_line() +
    #  scale_x_reverse() +
    labs(y = "s'(X)", x = "Year") + 
    geom_segment(data=fd, mapping=aes(x=data, y=0, xend=data,
                                      yend=(ifelse(lower>0, lower, ifelse(upper<0, upper, 0)))), # arrow=arrow(),
                 size=0.5, color="indianred3")
  
  ggsave(paste('figures/HI region specific/',var[j],' FD.png',sep=''), 
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
    scale_x_continuous(breaks=seq(min(HIspec$Year),max(HIspec$Year),2)) +
    geom_hline(aes(yintercept = 0), size = 0.2) +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +
    geom_line() +
    labs(y = 's"(X)', x = "Year") + 
    geom_segment(data=sdCR, mapping=aes(x=data, y=0, xend=data,
                                        yend=(ifelse(lower>0, lower, ifelse(upper<0, upper, 0)))), # arrow=arrow(),
                 size=0.5, color="indianred3")
  
  ggsave(paste('figures/HI region specific/',var[j],' SD.png',sep=''), 
         width =  8.47, height = 5, units = 'in', #w & h in inches
         dpi = 300, bg = 'transparent')
  
  #graph is super wiggly for some reason, also the y axis seems way too big??
}

###############################################################################
############################### CSVI ATTEMPTED GAMS ###########################
###############################################################################
##produces plots for soc inds split by counties with individual lines per community 
#but no county trends
#######################load and organize data cleaned from raw########
CSVI<-read.csv('data/HI_Soc_TS.csv')
CSVI<-CSVI[,c('Year','Community','County','Pounds1000', #cut just the vars i want to do gams on
              'Value1000','Dealers1000','CommPermits1000','RecFshTrps1000')]

CSVI[,c('ThousandPounds','ThousandVal')]<-CSVI[,c(4,5)]/1000 #scale large values
CSVI<-CSVI[,-c(4,5)] #delete unscaled
write.csv(CSVI,'data/HI_Soc_TS_scaled.csv',row.names = F)

################# GAMFIT TNT FUNCTION FOR EACH CATEGORY AND STACK DFS #############
# gamfit code only supports one categorical var
# comm nested within county, can loop comms to get gamfits
# then join counties back in by=Commununity

#read in points df
CSVI0<-read.csv('data/HI_Soc_TS_scaled.csv')

#vectors used
CSVIvar<-unique(colnames(CSVI0[,c(4:8)]))
groups<-c("Hawai‘i County","Honolulu County", "Kaua‘i County", "Maui County") #drop Kalawao--barely any data

for(j in 1:length(CSVI0$Year)) {
  CSVI0$Year[j]<-rnorm(1,CSVI0$Year[j],0.2) #gives a little flexibility in years 
  #so that gam registers it as more than 9 predictor vals
}

for(k in 1:length(CSVIvar)) { # CSVI scaled variables are columns 6:7
  #created empty df to bind others to
  df<- data.frame(matrix(ncol = 8, nrow = 0)) #one for gamfits
  colnames(df)<-c('County',"Year","fit", "se.fit", "inc.trend", "dec.trend", "threshold", "se.threshold" )
  
  #run groups in a loop and rbind each to df-->overwrites with compiled df as the loop runs
  for(i in 1:length(groups)) {
    df0<-CSVI0[CSVI0$County==groups[i],]
    gammod<-gam(unlist(df0[k+3]) ~ s(Year), 
                data = df0, method = 'REML')
    
    df.new<-gam_tnt(df0, 500,gammod,df0$Year,'Year',df0$County, 'County')
    df<-rbind(df.new,df)
  }
  write.csv(df, paste('data/CSVI ',CSVIvar[k],'_gamfit.csv', sep =''), row.names = F)
}

######################################################################
############## PLOT ALL SOC CATEGORIES IN FACETTED ###############
## read in points data first to use to write grouping vector
CSVI0<-read.csv('data/HI_Soc_TS_scaled.csv')
str(CSVI0)
CSVI0<-CSVI0[CSVI0$County=="Hawai‘i County"|
               CSVI0$County=="Honolulu County"|
               CSVI0$County=="Kaua‘i County"|
               CSVI0$County=="Maui County",]
CSVI0$County<-as.factor()

#vectors used
CSVIvar<-unique(colnames(CSVI0[,c(4:8)]))
groups<-c("Hawai‘i County","Honolulu County", "Kaua‘i County", "Maui County") #drop Kalawao--barely any data

CSVIlabs<-c("Dealers", "Commercial Permits", "Recreational Angler Trips",
            "Commercial Landings (Thousands of Pounds)", "Value Landed (Thousands of Dollars)")
CSVItitle<-c('Dealers per 1000 Capita', 'Permits per 1000 Capita', 'Trips per 1000 Capita', 'Landings per 1000 Capita','Value per 1000 Capita')

for(k in 1:length(CSVIvar)) { # CSVI scaled variables are columns 6:7
  df1<-read.csv(paste('data/CSVI ',CSVIvar[k],'_gamfit.csv', sep =''))
  str(df1)
  
  # set County as factor and reorder according to how i want it to be plotted
  df1$County<-factor(df1$County)
  df1 <- df1[,colSums(is.na(df1))<nrow(df1)] #if any trend or threshold columns are all NA, delete
  
  #using RColorBrewer manual selection
  indpal<-c('#b3cde3','#decbe4','#fddaec','#fed9a6')
  
  ############### plot gam ##################
  #gams have no trends--just linear
  ggplot(df1,
         aes(x = Year, y =  fit)) +
    facet_wrap(~County, nrow = 1) +
    scale_linetype_manual(values = c(rep(1:10,5))) + 
    theme(title = element_text(size=24, color = 'black'), #adjust font size of title
          panel.grid.major = element_blank(), #delete major grid lines
          panel.grid.minor = element_blank(), #delete minor grid lines
          axis.text=element_text(size=14 , color = 'black'), #adjust font size of axis tick labels
          axis.title=element_text(size=17,face="plain"), #adjust size of axis titles
          #     axis.title.x = element_text(margin = margin(t = 12, r = 0, b = 0, l = 0)), #increases distance between x axis label and plot
          strip.background =element_rect(fill="gray94", colour = 'black', size =1), #change panel label color
          strip.text = element_text(size = 19),
          panel.background = element_rect(fill = "transparent",colour = 'black', size = 1),
          plot.background = element_rect(fill = "transparent",colour = NA),
          plot.margin = unit(c(0.3,2,0.3,0.3), "cm"),
          legend.position = 'none', # c(0.37,0.57),
          #       legend.background = element_rect(fill="transparent", color = 'black'),
          #      legend.key = element_rect(fill = "transparent", colour = NA),
          #     legend.key.size = unit(1, 'cm'),
          #    legend.spacing.y = unit(0.2,'cm'),
          #        legend.text=element_text(size=17),
          #       legend.title=element_blank(),
          #      legend.margin = margin(3,12,9,10)
    ) +
    #  guides(fill = guide_legend(byrow = T)) + #need this line for legend.spacing.y to work
    #  scale_x_continuous(breaks=seq(2010,2018,1), #so that a tick appears every year--default ticks only where breaks are assigned
    #                    labels = c("2005",rep('',4),"2010",rep('',4),"2015",'')) +
    scale_y_continuous(limits = c(min(0,round_any(min(CSVI0[,k+3],na.rm=T),1, f = floor)), round_any(max(CSVI0[,k+3],na.rm=T),1, f = ceiling))) + #0 to max point (points go further than CI band)
    geom_ribbon(aes(ymin = fit-(2*se.fit), ymax = fit+(2*se.fit),fill=County), alpha = 0.7, color = 'gray45') + #95CI = (2*SE) for gam function
    scale_fill_manual(labels = levels(df1$County),
                      values = indpal) + #color combined CI, individual Countys are gray
    {if("threshold" %in% colnames(df1))
      geom_ribbon(aes(ymin = threshold-(2*se.threshold), ymax = threshold+(2*se.threshold)), 
                  alpha = 0.6, fill = 'gray40', cdolor = 'black')} + #recolor se band for threshold Countys
    geom_line(size = 0.7) + #full gam function
    #   geom_point(data = CSVI0, aes(x = Year, y = unlist(CSVI0[,k+3])),color='gray30', pch =21, fill = NA, size = 2) + # data points
    geom_line(data = CSVI0, aes(x = Year, y = unlist(CSVI0[,k+3]),color=Community, linetype=Community), size = 0.5)+  # data points
    #  scale_color_manual(labels = levels(df1$County),
    #                    values = indpal) +
    {if("inc.trend" %in% colnames(df1)) 
      geom_line(aes(x=Year, y = inc.trend),size = 1.2, color = "#009E73", alpha = 0.6)} +  #increasing trends
    { if("dec.trend" %in% colnames(df1))
      geom_line(aes(x=Year, y = dec.trend),size = 1.2, color = "#D55E00", alpha = 0.6) } +
    labs(y = CSVIlabs[k], x = "Year",title = 'Per Thousand Capita Values')   
  
  ggsave(paste('figures/HI region specific/CSVI grouped/',CSVIvar[k],' GAM_ggplot.png',sep=''), 
         width =  12, height = 3.5, units = 'in', #w & h in inches
         dpi = 300, bg = 'transparent') 
  
}

#-----------------------------------------------------------------------------#
###########################Dryad (eco) data by year############################
#-----------------------------------------------------------------------------#
dry<-read.csv('data/dryad annual averages.csv')
dry$Ratio<-dry$Calcifiers/dry$Algae
var<-colnames(dry)

titles<-c('Year','Biomass of Herbivorous Fish (Browsers)', 'Biomass of Herbivorous Fish (Grazers)',
          'Biomass of Herbivorous Fish (Scrapers)','Biomass of Predators', 'Biomass of Secondary Consumers',
          'Coral Cover', 'Macroalgal Cover', 'Other Benthic Cover', 'Turf Algal Cover',
          'Crustose Coralline Algal Cover', 'Biomass of Herbivorous Fishes',
          'Calcifier Cover', 'Algal Cover','Ratio of Calcifier to Algal Cover')

labs<-c('Year',expression(paste('Biomass (g'%.%'m'^'-2',')')), expression(paste('Biomass (g'%.%'m'^'-2',')')),
        expression(paste('Biomass (g'%.%'m'^'-2',')')),expression(paste('Biomass (g'%.%'m'^'-2',')')), 
        expression(paste('Biomass (g'%.%'m'^'-2',')')),
        'Percent Cover','Percent Cover', 'Percent Cover',
        'Percent Cover','Percent Cover', expression(paste('Biomass (g'%.%'m'^'-2',')')),
        'Percent Cover', 'Percent Cover ', 'Cover Ratio')

for(j in 2:length(var)){ #j represents the column number in the dry df (skip 1) & the y label for each plot
  #exploratory plot to see if there are obvious weird values
  plot(dry[,j]~Year, data = dry) 
  #looks pretty good
  
  gammod<-gam(dry[,j] ~ s(Year), 
              data = dry, method = 'REML')
  
  gammod$sp
  
  pdf(paste('figures/HI region specific/dryad/',var[j],' Assumptions.pdf',sep=''), width =  7, height = 5.5, #w & h in inches
  ) #sets high resolution--default is 150
  gam.check(gammod) #much better!
  dev.off()
  
}

#not sure why but the appraise() plot doesnt come out in the for loop

for(j in 2:length(var)){ #j represents the column number in the dry df (skip 1) & the y label for each plot
  
  gammod<-gam(dry[,j] ~ s(Year), 
              data = dry, method = 'REML')
  
  ####################PLOT GAM##############
  
  png(paste('figures/HI region specific/dryad/',var[j],' GAM.png',sep=''), width =  2400, height = 1700, #w & h in pixels
      res = 300) #sets high resolution--default is 150
  
  par(family = 'Helvetica',
      bg = NA, #empty background--easier to place on ppt
      mar = c(5.6, 5.7, 4, 2.4) #sets margins (bottom, left, top, right)--default values c(5.1,4.1,4.1,2.1)
  )
  plot(gammod, residuals = T, pch = 1, #plot points with empty circles
       seWithMean = TRUE, shade = T, shade.col = 'lightblue', #adds model uncertainty and shades it light blue
       shift = coef(gammod)[1],  #adds model intercept by shifting y axis by first coef
       lwd = 1.4,
       main = paste(titles[j],sep=''),
       font.main = 1, #makes the title regular (default is bold)
       cex.main = 1.6, #expansion factor for title
       ylab = labs[j],
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
  newd <- with(dry, data.frame(Year = seq(min(Year), max(Year), 
                                           length = n)))
  
  fd<-derivatives(gammod, newdata = newd, eps = EPS, unconditional = UNCONDITIONAL, order = 1, interval = "simultaneous")
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
    scale_x_continuous(breaks=seq(1995,2015,5)) +
    geom_hline(aes(yintercept = 0), size =0.2 ) +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +
    geom_line() +
    #  scale_x_reverse() +
    labs(y = "s'(X)", x = "Year") +  
    geom_segment(data=fd, mapping=aes(x=data, y=0, xend=data,
                                      yend=(ifelse(lower>0, lower, ifelse(upper<0, upper, 0)))), # arrow=arrow(),
                 size=0.5, color="indianred3")
  
  ggsave(paste('figures/HI region specific/dryad/',var[j],' FD.png',sep=''), 
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
    scale_x_continuous(breaks=seq(1995,2015,5)) +
    geom_hline(aes(yintercept = 0), size = 0.2) +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +
    geom_line() +
    labs(y = 's"(X)', x = "Year") + 
    geom_segment(data=sdCR, mapping=aes(x=data, y=0, xend=data,
                                        yend=(ifelse(lower>0, lower, ifelse(upper<0, upper, 0)))), # arrow=arrow(),
                 size=0.5, color="indianred3")
  
  ggsave(paste('figures/HI region specific/dryad/',var[j],' SD.png',sep=''), 
         width =  8.47, height = 5, units = 'in', #w & h in inches
         dpi = 300, bg = 'transparent')
  
}



#-----------------------------------------------------------------------------#
################################soc data by eco################################
#-----------------------------------------------------------------------------#


###############################################################################
##############################NCRMP data by DRYAD##############################
###############################################################################
#load cleaned NCRMP data (2000-2017 but lots of NAs for 2000-2004)
HIspec<-read.csv('data/NCRMP HI region specific soc indicators.csv')

#join with dryad (1993-2016)
dry<-read.csv('data/dryad annual averages.csv')
soceco0<-join(dry,HIspec,by = 'Year', type = 'inner') #even fewer time points

#create new dataframe with only the columns needed
soceco<-soceco0[,c('Year','Herbivores','Predators', 'Calcifiers', 'Algae',
                  "Commercial_Licenses_Revenue","Notification_Actions_PCT_Beach_Days",
                  "Commerical_Landings","Commercial_Revenue","Tourism_Employment",
                  "Living_Resources_GDP","Tourism_GDP","Recreational_Landings",
                  "Toxic_Releases_TOT","Visitor_Spending","Toxic_Releases_per_Coastline")]

#vectors to be used in for loop
dri<-c('1','Herbivores','Predators', 'Calcifiers', 'Algae')

restitles<-c('1','2','3','4','5','Commercial Licenses Revenue', 'Beach Notification Actions',
          'Commerical Fishing Landings','Commercial Fishing Revenue', 'Tourism Industry Employment',
          'Living Resources GDP','Tourism GDP','Recreational Fishing Landings',
          'Toxic Releases','Total Visitor Spending','Toxic Releases per Coastline')

reslabs<-c('1','2','3','4','5','Revenue (Dollars)','Notification Actions (% Beach Days)',
        'Total Landings (Pounds)','Total Revenue (Dollars)',
        'Employment (Number of Individuals)','Gross Domestic Product (Dollars)',
        'Gross Domestic Product (Dollars)', 'Total Landings (Pounds)', 
        'Total Releases (Pounds)','Visitor Spending (Dollars)',
        'Total Releases per Coastline (Permits per Mile)')

###############################DRIVER: HERBIVORES##################################
  for(j in 6:16){ #responses
    #exploratory plot to see if there are obvious weird values
    plot(soceco[,j]~Herbivores, data = soceco) 
    #looks pretty good
    
    SEmod<-gam(soceco[,j]~ s(Herbivores),  # not sure why drivers can't be looped as soceco[,i] but keep getting error: 
               #Error in variable.summary(gp$pf, dl, nrow(mf)) : 
               #'list' object cannot be coerced to type 'double'
               data = soceco, method = 'REML', correlation=corAR1(form=~Year))
     
    SEmod$sp
    
    pdf(paste('figures/HI region specific/soceco/',restitles[j],' by Herbivores Assumptions.pdf',sep=''), width =  7, height = 5.5, #w & h in inches
    ) #sets high resolution--default is 150
    gam.check(SEmod) #much better!
    dev.off()
  }


for(j in 6:16){ #j represents the column number in the soceco df & the y label for each plot
  
  SEmod<-gam(soceco[,j]~ s(Herbivores),
             data = soceco, method = 'REML', correlation=corAR1(form=~Year))
  
  ####################PLOT GAM##############
  
  png(paste('figures/HI region specific/soceco/',restitles[j],'by Herbivores GAM.png',sep=''), width =  2400, height = 1700, #w & h in pixels
      res = 300) #sets high resolution--default is 150
  
  par(family = 'Helvetica',
      bg = NA, #empty background--easier to place on ppt
      mar = c(5.6, 5.7, 4, 2.4) #sets margins (bottom, left, top, right)--default values c(5.1,4.1,4.1,2.1)
  )
  plot(SEmod, residuals = T, pch = 1, #plot points with empty cirles
       seWithMean = TRUE, shade = T, shade.col = 'lightblue', #adds model uncertainty and shades it light blue
       shift = coef(SEmod)[1],  #adds model intercept by shifting y axis by first coef
       lwd = 1.4,
       main = paste(restitles[j]," in Hawai'i",sep=''),
       font.main = 1, #makes the title regular (default is bold)
       cex.main = 1.6, #expansion factor for title
       ylab = reslabs[j],
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
  newd <- with(soceco, data.frame(Herbivores = seq(min(Herbivores), max(Herbivores), 
                                             length = n)))
  
  fd <- derivatives(SEmod, newdata = newd, eps = EPS, unconditional = UNCONDITIONAL,
                    n_sim = N, order = 1, interval = 'simultaneous')
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
    scale_x_continuous(breaks=seq(min(soceco$Herbivores),max(soceco$Herbivores),10), 
                       labels = scales::number_format(accuracy = 5)) +
    geom_hline(aes(yintercept = 0), size =0.2 ) +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +
    geom_line() +
    #  scale_x_reverse() +
    labs(y = "s'(X)", x = expression(paste("Herbivore Biomass (g•m"^"-2",")")))
  
  ggsave(paste('figures/HI region specific/soceco/',restitles[j],' by Herbivores FD.png',sep=''), 
         width =  8.47, height = 5, units = 'in', #w & h in inches
         dpi = 300, bg = 'transparent')
  
  
  #plot 2nd derivatives
  
  ## second derivatives of all smooths using central finite differences with simultaneous CI
  sdCR<-derivatives(SEmod, newdata = newd, eps = EPS, unconditional = UNCONDITIONAL, order = 2, interval = "simultaneous")
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
    scale_x_continuous(breaks=seq(min(soceco$Herbivores),max(soceco$Herbivores),10), 
                       labels = scales::number_format(accuracy = 5)) +
    geom_hline(aes(yintercept = 0), size = 0.2) +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +
    geom_line() +
    labs(y = 's"(X)', x = expression(paste("Herbivore Biomass (g•m"^"-2",")")))+ 
    geom_segment(data=sdCR, mapping=aes(x=data, y=0, xend=data,
                                        yend=(ifelse(lower>0, lower, ifelse(upper<0, upper, 0)))), # arrow=arrow(),
                 size=0.5, color="indianred3")
  
  ggsave(paste('figures/HI region specific/soceco/',restitles[j],' SD.png',sep=''), 
         width =  8.47, height = 5, units = 'in', #w & h in inches
         dpi = 300, bg = 'transparent')
  
  #graph is super wiggly for some reason, also the y axis seems way too big??
}

################################DRIVER: PREDATORS##################################
#Predators has an outlier--replace with NA
soceco$Predators[which(soceco$Predators>15)]<-NA

for(j in 6:16){ #responses
  #exploratory plot to see if there are obvious weird values
  plot(soceco[,j]~Predators, data = soceco) 
  #looks pretty good
  
  SEmod<-gam(soceco[,j]~ s(Predators),  # not sure why drivers can't be looped as soceco[,i] but keep getting error: 
             #Error in variable.summary(gp$pf, dl, nrow(mf)) : 
             #'list' object cannot be coerced to type 'double'
             data = soceco, method = 'REML', correlation=corAR1(form=~Year))
  
  SEmod$sp
  
  pdf(paste('figures/HI region specific/soceco/',restitles[j],' by Predators Assumptions.pdf',sep=''), width =  7, height = 5.5, #w & h in inches
  ) #sets high resolution--default is 150
  gam.check(SEmod) #much better!
  dev.off()
}


for(j in 6:16){ #j represents the column number in the soceco df & the y label for each plot
  
  SEmod<-gam(soceco[,j]~ s(Predators),
             data = soceco, method = 'REML', correlation=corAR1(form=~Year))
  
  ####################PLOT GAM##############
  
  png(paste('figures/HI region specific/soceco/',restitles[j],'by Predators GAM.png',sep=''), width =  2400, height = 1700, #w & h in pixels
      res = 300) #sets high resolution--default is 150
  
  par(family = 'Helvetica',
      bg = NA, #empty background--easier to place on ppt
      mar = c(5.6, 5.7, 4, 2.4) #sets margins (bottom, left, top, right)--default values c(5.1,4.1,4.1,2.1)
  )
  plot(SEmod, residuals = T, pch = 1, #plot points with empty cirles
       seWithMean = TRUE, shade = T, shade.col = 'lightblue', #adds model uncertainty and shades it light blue
       shift = coef(SEmod)[1],  #adds model intercept by shifting y axis by first coef
       lwd = 1.4,
       main = paste(restitles[j]," in Hawai'i",sep=''),
       font.main = 1, #makes the title regular (default is bold)
       cex.main = 1.6, #expansion factor for title
       ylab = reslabs[j],
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
  newd <- with(soceco, data.frame(Predators = seq(min(na.omit(Predators)), max(na.omit(Predators)), 
                                                   length = n)))
  
  fd <- derivatives(SEmod, newdata = newd, eps = EPS, unconditional = UNCONDITIONAL,
                    n_sim = N, order = 1, interval = 'simultaneous')
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
    scale_x_continuous(breaks=seq(min(na.omit(soceco$Predators)),max(na.omit(soceco$Predators)),10), 
                       labels = scales::number_format(accuracy = 5)) +
    geom_hline(aes(yintercept = 0), size =0.2 ) +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +
    geom_line() +
    #  scale_x_reverse() +
    labs(y = "s'(X)", x = expression(paste("Predator Biomass (g•m"^"-2",")")))
  
  ggsave(paste('figures/HI region specific/soceco/',restitles[j],' by Predators FD.png',sep=''), 
         width =  8.47, height = 5, units = 'in', #w & h in inches
         dpi = 300, bg = 'transparent')
  
  
  #plot 2nd derivatives
  
  ## second derivatives of all smooths using central finite differences with simultaneous CI
  sdCR<-derivatives(SEmod, newdata = newd, eps = EPS, unconditional = UNCONDITIONAL, order = 2, interval = "simultaneous")
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
    scale_x_continuous(breaks=seq(min(na.omit(soceco$Predators)),max(na.omit(soceco$Predators)),10), 
                       labels = scales::number_format(accuracy = 5)) +
    geom_hline(aes(yintercept = 0), size = 0.2) +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +
    geom_line() +
    labs(y = 's"(X)', x = expression(paste("Predator Biomass (g•m"^"-2",")")))+ 
    geom_segment(data=sdCR, mapping=aes(x=data, y=0, xend=data,
                                        yend=(ifelse(lower>0, lower, ifelse(upper<0, upper, 0)))), # arrow=arrow(),
                 size=0.5, color="indianred3")
  
  ggsave(paste('figures/HI region specific/soceco/',restitles[j],' SD.png',sep=''), 
         width =  8.47, height = 5, units = 'in', #w & h in inches
         dpi = 300, bg = 'transparent')
  
  #graph is super wiggly for some reason, also the y axis seems way too big??
}

###############################DRIVER: CALCIFIERS################################## 
for(j in 6:16){ #responses
#exploratory plot to see if there are obvious weird values
plot(soceco[,j]~Calcifiers, data = soceco) 
#looks pretty good

SEmod<-gam(soceco[,j]~ s(Calcifiers),  # not sure why drivers can't be looped as soceco[,i] but keep getting error: 
           #Error in variable.summary(gp$pf, dl, nrow(mf)) : 
           #'list' object cannot be coerced to type 'double'
           data = soceco, method = 'REML', correlation=corAR1(form=~Year))

SEmod$sp

pdf(paste('figures/HI region specific/soceco/',restitles[j],' by Calcifiers Assumptions.pdf',sep=''), width =  7, height = 5.5, #w & h in inches
) #sets high resolution--default is 150
gam.check(SEmod) #much better!
dev.off()
}


for(j in 6:16){ #j represents the column number in the soceco df & the y label for each plot
  
  SEmod<-gam(soceco[,j]~ s(Calcifiers),
             data = soceco, method = 'REML', correlation=corAR1(form=~Year))
  
  ####################PLOT GAM##############
  
  png(paste('figures/HI region specific/soceco/',restitles[j],'by Calcifiers GAM.png',sep=''), width =  2400, height = 1700, #w & h in pixels
      res = 300) #sets high resolution--default is 150
  
  par(family = 'Helvetica',
      bg = NA, #empty background--easier to place on ppt
      mar = c(5.6, 5.7, 4, 2.4) #sets margins (bottom, left, top, right)--default values c(5.1,4.1,4.1,2.1)
  )
  plot(SEmod, residuals = T, pch = 1, #plot points with empty cirles
       seWithMean = TRUE, shade = T, shade.col = 'lightblue', #adds model uncertainty and shades it light blue
       shift = coef(SEmod)[1],  #adds model intercept by shifting y axis by first coef
       lwd = 1.4,
       main = paste(restitles[j]," in Hawai'i",sep=''),
       font.main = 1, #makes the title regular (default is bold)
       cex.main = 1.6, #expansion factor for title
       ylab = reslabs[j],
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
  newd <- with(soceco, data.frame(Calcifiers = seq(min(Calcifiers), max(Calcifiers), 
                                                   length = n)))
  
  fd <- derivatives(SEmod, newdata = newd, eps = EPS, unconditional = UNCONDITIONAL,
                    n_sim = N, order = 1, interval = 'simultaneous')
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
    scale_x_continuous(breaks=seq(min(soceco$Calcifiers),max(soceco$Calcifiers),10), 
                       labels = scales::number_format(accuracy = 5)) +
    geom_hline(aes(yintercept = 0), size =0.2 ) +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +
    geom_line() +
    #  scale_x_reverse() +
    labs(y = "s'(X)", x = expression(paste("Calcifier Biomass (g•m"^"-2",")")))
  
  ggsave(paste('figures/HI region specific/soceco/',restitles[j],' by Calcifiers FD.png',sep=''), 
         width =  8.47, height = 5, units = 'in', #w & h in inches
         dpi = 300, bg = 'transparent')
  
  
  #plot 2nd derivatives
  
  ## second derivatives of all smooths using central finite differences with simultaneous CI
  sdCR<-derivatives(SEmod, newdata = newd, eps = EPS, unconditional = UNCONDITIONAL, order = 2, interval = "simultaneous")
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
    scale_x_continuous(breaks=seq(min(soceco$Calcifiers),max(soceco$Calcifiers),10), 
                       labels = scales::number_format(accuracy = 5)) +
    geom_hline(aes(yintercept = 0), size = 0.2) +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +
    geom_line() +
    labs(y = 's"(X)', x = expression(paste("Calcifier Biomass (g•m"^"-2",")"))) + 
    geom_segment(data=sdCR, mapping=aes(x=data, y=0, xend=data,
                                        yend=(ifelse(lower>0, lower, ifelse(upper<0, upper, 0)))), # arrow=arrow(),
                 size=0.5, color="indianred3")
  
  ggsave(paste('figures/HI region specific/soceco/',restitles[j],' SD.png',sep=''), 
         width =  8.47, height = 5, units = 'in', #w & h in inches
         dpi = 300, bg = 'transparent')
  
  #graph is super wiggly for some reason, also the y axis seems way too big??
}

###############################DRIVER: ALGAE##################################
for(j in 6:16){ #responses
  #exploratory plot to see if there are obvious weird values
  plot(soceco[,j]~Algae, data = soceco) 
  #looks pretty good
  
  SEmod<-gam(soceco[,j]~ s(Algae),  # not sure why drivers can't be looped as soceco[,i] but keep getting error: 
             #Error in variable.summary(gp$pf, dl, nrow(mf)) : 
             #'list' object cannot be coerced to type 'double'
             data = soceco, method = 'REML', correlation=corAR1(form=~Year))
  
  SEmod$sp
  
  pdf(paste('figures/HI region specific/soceco/',restitles[j],' by Algae Assumptions.pdf',sep=''), width =  7, height = 5.5, #w & h in inches
  ) #sets high resolution--default is 150
  gam.check(SEmod) #much better!
  dev.off()
}


for(j in 6:16){ #j represents the column number in the soceco df & the y label for each plot
  
  SEmod<-gam(soceco[,j]~ s(Algae),
             data = soceco, method = 'REML', correlation=corAR1(form=~Year))
  
  ####################PLOT GAM##############
  
  png(paste('figures/HI region specific/soceco/',restitles[j],'by Algae GAM.png',sep=''), width =  2400, height = 1700, #w & h in pixels
      res = 300) #sets high resolution--default is 150
  
  par(family = 'Helvetica',
      bg = NA, #empty background--easier to place on ppt
      mar = c(5.6, 5.7, 4, 2.4) #sets margins (bottom, left, top, right)--default values c(5.1,4.1,4.1,2.1)
  )
  plot(SEmod, residuals = T, pch = 1, #plot points with empty cirles
       seWithMean = TRUE, shade = T, shade.col = 'lightblue', #adds model uncertainty and shades it light blue
       shift = coef(SEmod)[1],  #adds model intercept by shifting y axis by first coef
       lwd = 1.4,
       main = paste(restitles[j]," in Hawai'i",sep=''),
       font.main = 1, #makes the title regular (default is bold)
       cex.main = 1.6, #expansion factor for title
       ylab = reslabs[j],
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
  newd <- with(soceco, data.frame(Algae = seq(min(Algae), max(Algae), 
                                                   length = n)))
  
  fd <- derivatives(SEmod, newdata = newd, eps = EPS, unconditional = UNCONDITIONAL,
                    n_sim = N, order = 1, interval = 'simultaneous')
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
    scale_x_continuous(breaks=seq(min(soceco$Algae),max(soceco$Algae),10), 
                       labels = scales::number_format(accuracy = 5)) +
    geom_hline(aes(yintercept = 0), size =0.2 ) +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +
    geom_line() +
    #  scale_x_reverse() +
    labs(y = "s'(X)", x = expression(paste("Algae Biomass (g•m"^"-2",")")))
  
  ggsave(paste('figures/HI region specific/soceco/',restitles[j],' by Algae FD.png',sep=''), 
         width =  8.47, height = 5, units = 'in', #w & h in inches
         dpi = 300, bg = 'transparent')
  
  
  #plot 2nd derivatives
  
  ## second derivatives of all smooths using central finite differences with simultaneous CI
  sdCR<-derivatives(SEmod, newdata = newd, eps = EPS, unconditional = UNCONDITIONAL, order = 2, interval = "simultaneous")
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
    scale_x_continuous(breaks=seq(min(soceco$Algae),max(soceco$Algae),10), 
                       labels = scales::number_format(accuracy = 5)) +
    geom_hline(aes(yintercept = 0), size = 0.2) +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +
    geom_line() +
    labs(y = 's"(X)', x = expression(paste("Algae Biomass (g•m"^"-2",")"))) + 
    geom_segment(data=sdCR, mapping=aes(x=data, y=0, xend=data,
                                        yend=(ifelse(lower>0, lower, ifelse(upper<0, upper, 0)))), # arrow=arrow(),
                 size=0.5, color="indianred3")
  
  ggsave(paste('figures/HI region specific/soceco/',restitles[j],' SD.png',sep=''), 
         width =  8.47, height = 5, units = 'in', #w & h in inches
         dpi = 300, bg = 'transparent')
  
  #graph is super wiggly for some reason, also the y axis seems way too big??
  
}


###############################################################################
################################chl.a by year##################################
###############################################################################

chla<-read.csv('data/chlorophyll a.csv')
chla<-chla[,c(1,7)]
colnames(chla)<-c("Year",'Chlorophyll.a')

########check assumptions
  plot(chla$Chlorophyll.a~chla$Year, data = chla) 
  #looks pretty good
  
chlmod<-gam(chla$Chlorophyll.a~s(chla$Year, k = 9), sp = 0.001,
             data = chla, method = 'REML')
  
  chlmod$sp
  
  pdf('figures/HI region specific/eco and clim/chl a by year Assumptions.pdf', width =  7, height = 5.5 #w & h in inches
  )
  gam.check(chlmod) #much better!
  dev.off()

  
  ####################PLOT GAM##############
  
  png('figures/HI region specific/eco and clim/chl a GAM.png', width =  2400, height = 1700, #w & h in pixels
      res = 300) #sets high resolution--default is 150
  
  par(family = 'Helvetica',
      bg = NA, #empty background--easier to place on ppt
      mar = c(5.6, 5.7, 4, 2.4) #sets margins (bottom, left, top, right)--default values c(5.1,4.1,4.1,2.1)
  )
  plot(chlmod, residuals = T, pch = 1, #plot points with empty cirles
       seWithMean = TRUE, shade = T, shade.col = 'lightblue', #adds model uncertainty and shades it light blue
       shift = coef(chlmod)[1],  #adds model intercept by shifting y axis by first coef
       lwd = 1.4,
       main = "Chlorophyll a in Hawai'i",
       font.main = 1, #makes the title regular (default is bold)
       cex.main = 1.6, #expansion factor for title
       ylab = 'Chlorophyll a',
       xlab = 'Year',
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
  
  ##?????????? -format same as other fd's, but this one keeps getting error:
  #Error in model.frame.default(ff, data = newdata, na.action = na.act) : 
  #invalid type (list) for variable 'chla'
  #In addition: Warning message:
    #In predict.gam(model, newdata, type = "lpmatrix") :
    #not all required variables have been supplied in  newdata!
  
  newd <- with(chla, data.frame(Year = seq(min(Year), max(Year), 
                                             length = n)))
  
  fd<-derivatives(chlmod, newdata = newd, eps = EPS, unconditional = UNCONDITIONAL, order = 1, interval = "simultaneous")
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
    scale_x_continuous(breaks=seq(min(chl.soc$Chlorophyll.a),max(chl.soc$Chlorophyll.a),5), 
                       labels = scales::number_format(accuracy = 5)) +
    geom_hline(aes(yintercept = 0), size =0.2 ) +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +
    geom_line() +
    #  scale_x_reverse() +
    labs(y = "s'(X)", x = "Chlorophyll a")
  
  ggsave(paste('figures/HI region specific/soclim/',restitles0[j],' by chl a FD.png',sep=''), 
         width =  8.47, height = 5, units = 'in', #w & h in inches
         dpi = 300, bg = 'transparent')
  
  
  #plot 2nd derivatives
  
  ## second derivatives of all smooths using central finite differences with simultaneous CI
  sdCR<-derivatives(chlmod, newdata = newd, eps = EPS, unconditional = UNCONDITIONAL, order = 2, interval = "simultaneous")
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
    scale_x_continuous(breaks=seq(min(chl.soc$Chlorophyll.a),max(chl.soc$Chlorophyll.a),5), 
                       labels = scales::number_format(accuracy = 5)) +
    geom_hline(aes(yintercept = 0), size = 0.2) +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +
    geom_line() +
    labs(y = 's"(X)', x = "Chlorophyll a") + 
    geom_segment(data=sdCR, mapping=aes(x=data, y=0, xend=data,
                                        yend=(ifelse(lower>0, lower, ifelse(upper<0, upper, 0)))), # arrow=arrow(),
                 size=0.5, color="indianred3")
  
  ggsave(paste('figures/HI region specific/soclim/',restitles0[j],' by chl a SD.png',sep=''), 
         width =  8.47, height = 5, units = 'in', #w & h in inches
         dpi = 300, bg = 'transparent')



###############################################################################
####################NCRMP data by chl.a/overfished stocks######################
###############################################################################
#load cleaned NCRMP data (2000-2017 but lots of NAs for 2000-2004)
HIspec<-read.csv('data/NCRMP HI region specific soc indicators.csv')

chla<-read.csv('data/chlorophyll a.csv')
chla<-chla[,c(1,7)]
colnames(chla)<-c("Year",'Chlorophyll.a')
chl.soc<-join(chla,HIspec,by = 'Year', type = 'inner')

#vectors to be used in for loop
restitles0<-c('1','2','Commercial Licenses Revenue', 'Beach Notification Actions',
              'Commerical Fishing Landings','Commercial Fishing Revenue', 'Tourism Industry Employment',
              'Living Resources GDP','Tourism GDP','Recreational Fishing Landings',
              'Toxic Releases','Total Visitor Spending','Toxic Releases per Coastline')

reslabs0<-c('1','2','Revenue (Dollars)','Notification Actions (% Beach Days)',
            'Total Landings (Pounds)','Total Revenue (Dollars)',
            'Employment (Number of Individuals)','Gross Domestic Product (Dollars)',
            'Gross Domestic Product (Dollars)', 'Total Landings (Pounds)', 
            'Total Releases (Pounds)','Visitor Spending (Dollars)',
            'Total Releases per Coastline (Permits per Mile)')

##########################GAMs################
for(j in 3:13){ #j represents the column number in the chl.soc df & the y label for each plot
  
  SEmod<-gam(chl.soc[,j]~ s(Chlorophyll.a),
             data = chl.soc, method = 'REML', correlation=corAR1(form=~Year))
  
  ####################PLOT GAM##############
  
  png(paste('figures/HI region specific/soclim/',restitles0[j],'by chl a GAM.png',sep=''), width =  2400, height = 1700, #w & h in pixels
      res = 300) #sets high resolution--default is 150
  
  par(family = 'Helvetica',
      bg = NA, #empty background--easier to place on ppt
      mar = c(5.6, 5.7, 4, 2.4) #sets margins (bottom, left, top, right)--default values c(5.1,4.1,4.1,2.1)
  )
  plot(SEmod, residuals = T, pch = 1, #plot points with empty cirles
       seWithMean = TRUE, shade = T, shade.col = 'lightblue', #adds model uncertainty and shades it light blue
       shift = coef(SEmod)[1],  #adds model intercept by shifting y axis by first coef
       lwd = 1.4,
       main = paste(restitles0[j]," in Hawai'i",sep=''),
       font.main = 1, #makes the title regular (default is bold)
       cex.main = 1.6, #expansion factor for title
       ylab = reslabs0[j],
       xlab = 'Chlorophyll a',
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
  newd <- with(chl.soc, data.frame(Chlorophyll.a = seq(min(Chlorophyll.a), max(Chlorophyll.a), 
                                            length = n)))
  
  fd <- derivatives(SEmod, newdata = newd, eps = EPS, unconditional = UNCONDITIONAL,
                    n_sim = N, order = 1, interval = 'simultaneous')
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
    scale_x_continuous(breaks=seq(min(chl.soc$Chlorophyll.a),max(chl.soc$Chlorophyll.a),5), 
                       labels = scales::number_format(accuracy = 5)) +
    geom_hline(aes(yintercept = 0), size =0.2 ) +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +
    geom_line() +
    #  scale_x_reverse() +
    labs(y = "s'(X)", x = "Chlorophyll a")
  
  ggsave(paste('figures/HI region specific/soclim/',restitles0[j],' by chl a FD.png',sep=''), 
         width =  8.47, height = 5, units = 'in', #w & h in inches
         dpi = 300, bg = 'transparent')
  
  
  #plot 2nd derivatives
  
  ## second derivatives of all smooths using central finite differences with simultaneous CI
  sdCR<-derivatives(SEmod, newdata = newd, eps = EPS, unconditional = UNCONDITIONAL, order = 2, interval = "simultaneous")
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
    scale_x_continuous(breaks=seq(min(chl.soc$Chlorophyll.a),max(chl.soc$Chlorophyll.a),5), 
                       labels = scales::number_format(accuracy = 5)) +
    geom_hline(aes(yintercept = 0), size = 0.2) +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +
    geom_line() +
    labs(y = 's"(X)', x = "Chlorophyll a") + 
    geom_segment(data=sdCR, mapping=aes(x=data, y=0, xend=data,
                                        yend=(ifelse(lower>0, lower, ifelse(upper<0, upper, 0)))), # arrow=arrow(),
                 size=0.5, color="indianred3")
  
  ggsave(paste('figures/HI region specific/soclim/',restitles0[j],' by chl a SD.png',sep=''), 
         width =  8.47, height = 5, units = 'in', #w & h in inches
         dpi = 300, bg = 'transparent')
  
  #graph is super wiggly for some reason, also the y axis seems way too big??
  }

###########################################################################
##########overfished stocks (CANT DO BECAUSE RANGE 1-3 provides too few degrees of freedom)
###########################################################################
#skip to line 879

overfish<-read.csv('data/HI overfished.csv')
overfish<-overfish[,c(1,3)]
colnames(overfish)<-c("Year",'Stocks')
overf.soc<-join(overfish,HIspec,by = 'Year', type = 'inner')

#vectors to be used in for loop
restitles0<-c('1','2','Commercial Licenses Revenue', 'Beach Notification Actions',
              'Commerical Fishing Landings','Commercial Fishing Revenue', 'Tourism Industry Employment',
              'Living Resources GDP','Tourism GDP','Recreational Fishing Landings',
              'Toxic Releases','Total Visitor Spending','Toxic Releases per Coastline')

reslabs0<-c('1','2','Revenue (Dollars)','Notification Actions (% Beach Days)',
            'Total Landings (Pounds)','Total Revenue (Dollars)',
            'Employment (Number of Individuals)','Gross Domestic Product (Dollars)',
            'Gross Domestic Product (Dollars)', 'Total Landings (Pounds)', 
            'Total Releases (Pounds)','Visitor Spending (Dollars)',
            'Total Releases per Coastline (Permits per Mile)')

##########################GAMs################
for(j in 3:13){ #j represents the column number in the overf.soc df & the y label for each plot
  
  SEmod<-gam(overf.soc[,j]~ s(Stocks),
             data = overf.soc, method = 'REML', correlation=corAR1(form=~Year))
  
  ####################PLOT GAM##############
  
  png(paste('figures/HI region specific/soclim/',restitles0[j],'by overfished stocks GAM.png',sep=''), width =  2400, height = 1700, #w & h in pixels
      res = 300) #sets high resolution--default is 150
  
  par(family = 'Helvetica',
      bg = NA, #empty background--easier to place on ppt
      mar = c(5.6, 5.7, 4, 2.4) #sets margins (bottom, left, top, right)--default values c(5.1,4.1,4.1,2.1)
  )
  plot(SEmod, residuals = T, pch = 1, #plot points with empty cirles
       seWithMean = TRUE, shade = T, shade.col = 'lightblue', #adds model uncertainty and shades it light blue
       shift = coef(SEmod)[1],  #adds model intercept by shifting y axis by first coef
       lwd = 1.4,
       main = paste(restitles0[j]," in Hawai'i",sep=''),
       font.main = 1, #makes the title regular (default is bold)
       cex.main = 1.6, #expansion factor for title
       ylab = reslabs0[j],
       xlab = 'Number of Overfished Stocks',
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
  newd <- with(overf.soc, data.frame(Stocks = seq(min(Stocks), max(Stocks), 
                                                       length = n)))
  
  fd <- derivatives(SEmod, newdata = newd, eps = EPS, unconditional = UNCONDITIONAL,
                    n_sim = N, order = 1, interval = 'simultaneous')
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
    scale_x_continuous(breaks=seq(min(overf.soc$Stocks),max(overf.soc$Stocks),1), 
                       labels = scales::number_format(accuracy = 5)) +
    geom_hline(aes(yintercept = 0), size =0.2 ) +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +
    geom_line() +
    #  scale_x_reverse() +
    labs(y = "s'(X)", x = "Number of Overfished Stocks")
  
  ggsave(paste('figures/HI region specific/soclim/',restitles0[j],' by overfished stocks FD.png',sep=''), 
         width =  8.47, height = 5, units = 'in', #w & h in inches
         dpi = 300, bg = 'transparent')
  
  
  #plot 2nd derivatives
  
  ## second derivatives of all smooths using central finite differences with simultaneous CI
  sdCR<-derivatives(SEmod, newdata = newd, eps = EPS, unconditional = UNCONDITIONAL, order = 2, interval = "simultaneous")
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
    scale_x_continuous(breaks=seq(min(overf.soc$Stocks),max(overf.soc$Stocks),1), 
                       labels = scales::number_format(accuracy = 5)) +
    geom_hline(aes(yintercept = 0), size = 0.2) +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +
    geom_line() +
    labs(y = 's"(X)', x = "Number of Overfished Stocks") + 
    geom_segment(data=sdCR, mapping=aes(x=data, y=0, xend=data,
                                        yend=(ifelse(lower>0, lower, ifelse(upper<0, upper, 0)))), # arrow=arrow(),
                 size=0.5, color="indianred3")
  
  ggsave(paste('figures/HI region specific/soclim/',restitles0[j],' by overfished stocks SD.png',sep=''), 
         width =  8.47, height = 5, units = 'in', #w & h in inches
         dpi = 300, bg = 'transparent')
  
  #graph is super wiggly for some reason, also the y axis seems way too big??
}


###############################################################################
############################NCRMP data by PDO/ENSO#############################
###############################################################################
#load cleaned NCRMP data (2000-2017 but lots of NAs for 2000-2004)
HIspec<-read.csv('data/NCRMP HI region specific soc indicators.csv')

#ENSO data
enso0<-read.csv('data/ElNinoTimeSeries.csv')
enso1<-enso0[,c(1,9,10)]
enso<-aggregate(.~Year, data = enso1, FUN = mean) #average indicator values by Year
colnames(enso)<-c('Year','Nino3.4', 'Anomaly')

#PDO data
pdo0<-read.csv('data/PDOTimeSeries.csv')
#Date column has a 6 character string representing year (xxxx) and month (xx)
#extract the 1st to 4th character into new 'Year' column
pdo0$Year<-substr(pdo0$Date,1, 4) # new column loses month info, but we are going to get annual averages anyway
pdo1<-pdo0[,c(3,2)]
pdo<-aggregate(.~Year, data = pdo1, FUN = mean) #average indicator values by Year
colnames(pdo)<-c('Year','PDO')

#join PDO and ENSO
LTclim<-join(pdo,enso,by = 'Year', type = 'inner') #even fewer time points

#join climate with HIspec
soclim<-join(LTclim,HIspec,by = 'Year', type = 'inner') #even fewer time points

#vectors to be used in for loop

restitles1<-c('1','2','3','4','Commercial Licenses Revenue', 'Beach Notification Actions',
             'Commerical Fishing Landings','Commercial Fishing Revenue', 'Tourism Industry Employment',
             'Living Resources GDP','Tourism GDP','Recreational Fishing Landings',
             'Toxic Releases','Total Visitor Spending','Toxic Releases per Coastline')

reslabs1<-c('1','2','3','4','Revenue (Dollars)','Notification Actions (% Beach Days)',
           'Total Landings (Pounds)','Total Revenue (Dollars)',
           'Employment (Number of Individuals)','Gross Domestic Product (Dollars)',
           'Gross Domestic Product (Dollars)', 'Total Landings (Pounds)', 
           'Total Releases (Pounds)','Visitor Spending (Dollars)',
           'Total Releases per Coastline (Permits per Mile)')

###############################DRIVER: PDO##################################
for(j in 5:15){ #responses
  #exploratory plot to see if there are obvious weird values
  plot(soclim[,j]~PDO, data = soclim) 
  #looks pretty good
  
  SEmod<-gam(soclim[,j]~ s(PDO),  # not sure why drivers can't be looped as soclim[,i] but keep getting error: 
             #Error in variable.summary(gp$pf, dl, nrow(mf)) : 
             #'list' object cannot be coerced to type 'double'
             data = soclim, method = 'REML', correlation=corAR1(form=~Year))
  
  SEmod$sp
  
  pdf(paste('figures/HI region specific/soclim/',restitles1[j],' by PDO Assumptions.pdf',sep=''), width =  7, height = 5.5, #w & h in inches
  ) #sets high resolution--default is 150
  gam.check(SEmod) #much better!
  dev.off()
}


for(j in 6:16){ #j represents the column number in the soclim df & the y label for each plot
  
  SEmod<-gam(soclim[,j]~ s(PDO),
             data = soclim, method = 'REML', correlation=corAR1(form=~Year))
  
  ####################PLOT GAM##############
  
  png(paste('figures/HI region specific/soclim/',restitles1[j],'by PDO GAM.png',sep=''), width =  2400, height = 1700, #w & h in pixels
      res = 300) #sets high resolution--default is 150
  
  par(family = 'Helvetica',
      bg = NA, #empty background--easier to place on ppt
      mar = c(5.6, 5.7, 4, 2.4) #sets margins (bottom, left, top, right)--default values c(5.1,4.1,4.1,2.1)
  )
  plot(SEmod, residuals = T, pch = 1, #plot points with empty cirles
       seWithMean = TRUE, shade = T, shade.col = 'lightblue', #adds model uncertainty and shades it light blue
       shift = coef(SEmod)[1],  #adds model intercept by shifting y axis by first coef
       lwd = 1.4,
       main = paste(restitles1[j]," in Hawai'i",sep=''),
       font.main = 1, #makes the title regular (default is bold)
       cex.main = 1.6, #expansion factor for title
       ylab = reslabs1[j],
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
  newd <- with(soclim, data.frame(PDO = seq(min(PDO), max(PDO), 
                                                   length = n)))
  
  fd <- derivatives(SEmod, newdata = newd, eps = EPS, unconditional = UNCONDITIONAL,
                    n_sim = N, order = 1, interval = 'simultaneous')
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
    scale_x_continuous(breaks=seq(min(soclim$PDO),max(soclim$PDO),10), 
                       labels = scales::number_format(accuracy = 5)) +
    geom_hline(aes(yintercept = 0), size =0.2 ) +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +
    geom_line() +
    #  scale_x_reverse() +
    labs(y = "s'(X)", x = "Pacific Decadal Oscillation")
  
  ggsave(paste('figures/HI region specific/soclim/',restitles1[j],' by PDO FD.png',sep=''), 
         width =  8.47, height = 5, units = 'in', #w & h in inches
         dpi = 300, bg = 'transparent')
  
  
  #plot 2nd derivatives
  
  ## second derivatives of all smooths using central finite differences with simultaneous CI
  sdCR<-derivatives(SEmod, newdata = newd, eps = EPS, unconditional = UNCONDITIONAL, order = 2, interval = "simultaneous")
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
    scale_x_continuous(breaks=seq(min(soclim$PDO),max(soclim$PDO),10), 
                       labels = scales::number_format(accuracy = 5)) +
    geom_hline(aes(yintercept = 0), size = 0.2) +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +
    geom_line() +
    labs(y = 's"(X)', x = "Pacific Decadal Oscillation") + 
    geom_segment(data=sdCR, mapping=aes(x=data, y=0, xend=data,
                                        yend=(ifelse(lower>0, lower, ifelse(upper<0, upper, 0)))), # arrow=arrow(),
                 size=0.5, color="indianred3")
  
  ggsave(paste('figures/HI region specific/soclim/',restitles1[j],' by PDO SD.png',sep=''), 
         width =  8.47, height = 5, units = 'in', #w & h in inches
         dpi = 300, bg = 'transparent')
  
  #graph is super wiggly for some reason, also the y axis seems way too big??
}

###############################DRIVER: ENSO SST################################
for(j in 5:15){ #responses
  #exploratory plot to see if there are obvious weird values
  plot(soclim[,j]~Nino3.4, data = soclim) 
  #looks pretty good
  
  SEmod<-gam(soclim[,j]~ s(Nino3.4),  # not sure why drivers can't be looped as soclim[,i] but keep getting error: 
             #Error in variable.summary(gp$pf, dl, nrow(mf)) : 
             #'list' object cannot be coerced to type 'double'
             data = soclim, method = 'REML', correlation=corAR1(form=~Year))
  
  SEmod$sp
  
  pdf(paste('figures/HI region specific/soclim/',restitles1[j],' by Nino Assumptions.pdf',sep=''), width =  7, height = 5.5, #w & h in inches
  ) #sets high resolution--default is 150
  gam.check(SEmod) #much better!
  dev.off()
}


for(j in 6:16){ #j represents the column number in the soclim df & the y label for each plot
  
  SEmod<-gam(soclim[,j]~ s(Nino3.4),
             data = soclim, method = 'REML', correlation=corAR1(form=~Year))
  
  ####################PLOT GAM##############
  
  png(paste('figures/HI region specific/soclim/',restitles1[j],'by Nino GAM.png',sep=''), width =  2400, height = 1700, #w & h in pixels
      res = 300) #sets high resolution--default is 150
  
  par(family = 'Helvetica',
      bg = NA, #empty background--easier to place on ppt
      mar = c(5.6, 5.7, 4, 2.4) #sets margins (bottom, left, top, right)--default values c(5.1,4.1,4.1,2.1)
  )
  plot(SEmod, residuals = T, pch = 1, #plot points with empty cirles
       seWithMean = TRUE, shade = T, shade.col = 'lightblue', #adds model uncertainty and shades it light blue
       shift = coef(SEmod)[1],  #adds model intercept by shifting y axis by first coef
       lwd = 1.4,
       main = paste(restitles1[j]," in Hawai'i",sep=''),
       font.main = 1, #makes the title regular (default is bold)
       cex.main = 1.6, #expansion factor for title
       ylab = reslabs1[j],
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
  newd <- with(soclim, data.frame(Nino3.4 = seq(min(Nino3.4), max(Nino3.4), 
                                            length = n)))
  
  fd <- derivatives(SEmod, newdata = newd, eps = EPS, unconditional = UNCONDITIONAL,
                    n_sim = N, order = 1, interval = 'simultaneous')
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
    scale_x_continuous(breaks=seq(min(soclim$Nino3.4),max(soclim$Nino3.4),10), 
                       labels = scales::number_format(accuracy = 5)) +
    geom_hline(aes(yintercept = 0), size =0.2 ) +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +
    geom_line() +
    #  scale_x_reverse() +
    labs(y = "s'(X)", x = "Pacific Decadal Oscillation")
  
  ggsave(paste('figures/HI region specific/soclim/',restitles1[j],' by Nino FD.png',sep=''), 
         width =  8.47, height = 5, units = 'in', #w & h in inches
         dpi = 300, bg = 'transparent')
  
  
  #plot 2nd derivatives
  
  ## second derivatives of all smooths using central finite differences with simultaneous CI
  sdCR<-derivatives(SEmod, newdata = newd, eps = EPS, unconditional = UNCONDITIONAL, order = 2, interval = "simultaneous")
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
    scale_x_continuous(breaks=seq(min(soclim$Nino3.4),max(soclim$Nino3.4),10), 
                       labels = scales::number_format(accuracy = 5)) +
    geom_hline(aes(yintercept = 0), size = 0.2) +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +
    geom_line() +
    labs(y = 's"(X)', x = "Pacific Decadal Oscillation") + 
    geom_segment(data=sdCR, mapping=aes(x=data, y=0, xend=data,
                                        yend=(ifelse(lower>0, lower, ifelse(upper<0, upper, 0)))), # arrow=arrow(),
                 size=0.5, color="indianred3")
  
  ggsave(paste('figures/HI region specific/soclim/',restitles1[j],' SD.png',sep=''), 
         width =  8.47, height = 5, units = 'in', #w & h in inches
         dpi = 300, bg = 'transparent')
  
  #graph is super wiggly for some reason, also the y axis seems way too big??
}


###############################################################################
###############################clean NCRMP data################################
###############################################################################
HIspec<-read.csv('data/NCRMP HI region specific soc indicators.csv')

#cut time series to eliminate NAs
#and cut redundant inds
HIbodi0<-HIspec[c(6:17),]
HIbodi<-HIbodi0[,c(1,2,5:6,9,11:12)] #keep 6 inds for constructing BODI
which(is.na(HIbodi)) #are there any NAs left?

#rescale because data must be on similar scales for BODI
#toxic releases scale is 1000s--get everything on 1000s
HIbodi$Licenses_Rev_Hundreds<-HIbodi$Commercial_Licenses_Revenue/100
HIbodi$Commercial_Revenue_TenThousands<-HIbodi$Commercial_Revenue/10000
HIbodi$Tourism_Emp_Tens<-HIbodi$Tourism_Employment/10
HIbodi$Rec_Landings_Thousands<-HIbodi$Recreational_Landings/1000
HIbodi$Visitor_Spending_TenMillions<-HIbodi$Visitor_Spending/10000000

HIbodi<-HIbodi[,c(1,7:12)]


#Create outputs

####################################################################################
#Y=outputs[,-1]  #Outputs are everything except Year --resulted in too many 1s
#maybe theres too many output variables

#######first 
Y=HIbodi[,c(3,5,6,7)]  #Outputs are licenses rev, tourism emp, rec land, vis spending
YP=as.matrix(Y)      #Put outputs in Matrix
CI=ci_bod(YP)        #Calculates BOD Value for an Output Oriented model using ci_bod function from package Compind
w=CI$ci_bod_weights  #Weights are put into w
qobj=CI$ci_bod_est   #qobj is objective function value
QI1=qobj/qobj[12]    #QI1 is an output index with observation 13 (2016) as the base

#######second
Y.1=outputs[,c(2,5,7,12)]  #comm rev,r trips, NES Receipts, Seafood Wages
YP=as.matrix(Y.1)      #Put outputs in Matrix
CI=ci_bod(YP)        #Calculates BOD Value for an Output Oriented model using ci_bod function from package Compind
w=CI$ci_bod_weights  #Weights are put into w
qobj=CI$ci_bod_est   #qobj is objective function value
QI2=qobj/qobj[13]    #QI1 is an output index with observation 13 (2016) as the base

#######third
Y.2=outputs[,c(2,5,8,12)] #comm rev,r trips, NES Establishments, Seafood Wages
YP=as.matrix(Y.2)      #Put outputs in Matrix
CI=ci_bod(YP)        #Calculates BOD Value for an Output Oriented model using ci_bod function from package Compind
w=CI$ci_bod_weights  #Weights are put into w
qobj=CI$ci_bod_est   #qobj is objective function value
QI3=qobj/qobj[13]    #QI1 is an output index with observation 13 (2016) as the base


##########################################################################################
# Now, create input index
##########################################################################################
HIeco<-read.csv('data/HCC cleaned/dryad annual averages.csv')
#1993, 1999:2000, 2002:2016
#if compared to outputs, will cut off another 2 years (2017-2018)
#we want to maximize 1-Algae PC instead of Algae PC
#change Algae into 1-% Cover because Algae is a 'bad' input
HIeco$Non.Algae<-100-HIeco$Algae

HIeco$Benthic.Total<-HIeco$Coral + HIeco$Macro +HIeco$Other + HIeco$Turf +HIeco$CCA


inputs<-as.data.table(HIeco[,c(1,12,5,6,13)]) #included Calcifiers (13) instead of Non.Algae (15) or Algae
inputs<-inputs[c(7:18),] #cut years to match NCRMP years

##########################################################################
#BOD input Index
#This uses the Benchmarking package and an input oriented DEA model with constant returns to scale
#########################################################################
X<-inputs[,-1]      #Defines Input Matrix
X<-as.matrix(X)
(J=nrow(X))          # J is the number of observations
Y2=matrix(1,J,1)     #Y2 is a matrix of one's which are the outputs for the DEA model
crsdea <- dea(X,Y2,RTS="crs",ORIENTATION="in")   #This runs the DEA Model
#Warning message:
#In if (class(X) == "data.frame") { :
#    the condition has length > 1 and only the first element will be used

xobj=1/crsdea$eff                                #This is the inverse of the objective function value
XI=xobj/xobj[10]                                 #This creates the BOD input index with observation 18 (2014) as base
##################################################################
#DUAL input with Benchmarking
#To get the weights for the BOD input index, you need to run the dual DEA input oriented model
#The objective function values will be exactly the same (perhaps with some rounding differences)
#Running the dual model will give you the weights 
#You could simply run the dual to get both the weights and objective function values
#I'm including bothjust for your reference
##################################################################
inpdual<-dea.dual(X,Y,RTS="crs",ORIENTATION="out")

w3=inpdual$u
eff3=inpdual$eff
XI2=eff3/eff3[10]
########################################################################
#My suspicion when looking at the results is that there is a dimensionality problem,
#meaning there are too many inputs. We have 6 inputs and 1 output and only 29 years
#of data. That may be why there are so many "1's" as objective function values
########################################################################
#Productivity Index
#######################################################################
out.bodi<-QI1
in.bodi<-XI2

PROD=out.bodi/in.bodi
PROD1=out.bodi1/in.bodi
PROD2=out.bodi2/in.bodi


years<-inputs[,1] # list only the years that overlap both output and input
#subtract rows 1-5 (1993, 1999, 2000, 2002, 2003) on input df, and pull values from column 1 (Years)
final<-cbind(years,out.bodi)
final<-cbind(final,in.bodi,PROD)

final1<-cbind(years,out.bodi1)
final1<-cbind(final1,in.bodi,PROD1)

final2<-cbind(years,out.bodi2)
final2<-cbind(final2,in.bodi,PROD2)

###################################################################

#plot outputs over all years
years.out<-HIbodi[,1] # list only the years for output range

######this one has a good pattern and fewest 1's: 
#Comm Revenue, Comm and Rec Catch, Seafood Employment 

######cut 2004-2006 for direct comparison###
##inclusive
#png('figures/HCC/BODI Outputs3 cut.png', width =  2400, height = 1700, #w & h in pixels
#   res = 300) #sets high resolution--default is 150

png('figures/HI region specific/BODI Outputs.png', width =  2400, height = 1700, #w & h in pixels
    res = 300) #sets high resolution--default is 150

par(family = 'Helvetica',
    bg = NA, #empty background--easier to place on ppt
    mar = c(5.6, 5.7, 4, 2.4) #sets margins (bottom, left, top, right)--default values c(5.1,4.1,4.1,2.1)
)
plot(out.bodi~Year,data = final ,
     xlab = 'Years',
     ylab = 'Benefit of the Doubt Index',
     main = "Achievement of Social Objectives in Hawai'i",
     col = 'white',
     pch = 21, #filled circles that can have diff fill and outline
     cex=2, #expansion factor for size of pts
     bg="mediumseagreen", #fill color of pts
     lwd = 1.4,
     font.main = 1, #makes the title regular (default is bold)
     cex.main = 1.6, #expansion factor for title
     cex.lab = 1.45, #expansion factor--multiply size of x and y axis labels by 1.7
     cex.axis = 1.3 #expansion factor for numbered axis labels
)

dev.off()

#####################################


png('figures/HCC/BODI Outputs.png', width =  2400, height = 1700, #w & h in pixels
    res = 300) #sets high resolution--default is 150

par(family = 'Helvetica',
    bg = NA, #empty background--easier to place on ppt
    mar = c(5.6, 5.7, 4, 2.4) #sets margins (bottom, left, top, right)--default values c(5.1,4.1,4.1,2.1)
)
plot(QI1~years.out,
     xlab = 'Years',
     ylab = 'Benefit of the Doubt Index',
     main = "Achievement of Social Objectives in Hawai'i",
     col = 'white',
     pch = 21, #filled circles that can have diff fill and outline
     cex=2, #expansion factor for size of pts
     bg="mediumseagreen", #fill color of pts
     lwd = 1.4,
     font.main = 1, #makes the title regular (default is bold)
     cex.main = 1.6, #expansion factor for title
     cex.lab = 1.45, #expansion factor--multiply size of x and y axis labels by 1.7
     cex.axis = 1.3 #expansion factor for numbered axis labels
)

dev.off()



###plot inputs over all years
years.in<-inputs[,1] # list only the years for input range
input.bodi<-cbind(years.in,XI2) #for some reason, above line coerces years.in into df
#cannot simply write plot(XI2~years.in) because the two are of diff classes
#cbind puts both columns into same df, can then be plotted

##########cut 2004-2016 inputs for direct comparison###
png('figures/HI region specific/BODI Inputs.png', width =  2400, height = 1700, #w & h in pixels
    res = 300) #sets high resolution--default is 150

par(family = 'Helvetica',
    bg = NA, #empty background--easier to place on ppt
    mar = c(5.6, 5.7, 4, 2.4) #sets margins (bottom, left, top, right)--default values c(5.1,4.1,4.1,2.1)
)
plot(in.bodi~Year, data = final,
     xlab = 'Years',
     ylab = 'Benefit of the Doubt Index',
     main = "Ecological Drivers",
     col = 'white',
     pch = 21, #filled circles that can have diff fill and outline
     cex=2, #expansion factor for size of pts
     bg="coral2", #fill color of pts
     lwd = 1.4,
     font.main = 1, #makes the title regular (default is bold)
     cex.main = 1.6, #expansion factor for title
     cex.lab = 1.45, #expansion factor--multiply size of x and y axis labels by 1.7
     cex.axis = 1.3 #expansion factor for numbered axis labels
)
dev.off()
###################################


####plot productivity over overlapping years

png('figures/HI region specific/BODI Productivity.png', width =  2400, height = 1700, #w & h in pixels
    res = 300) #sets high resolution--default is 150

par(family = 'Helvetica',
    bg = NA, #empty background--easier to place on ppt
    mar = c(5.6, 5.7, 4, 2.4) #sets margins (bottom, left, top, right)--default values c(5.1,4.1,4.1,2.1)
)
plot(PROD~Year, data = final,
     xlab = 'Years',
     ylab = 'Productivity Index',
     main = "Social-Ecological Productivity in Hawai'i",
     col = 'white',
     pch = 21, #filled circles that can have diff fill and outline
     cex=2, #expansion factor for size of pts
     bg="steelblue3", #fill color of pts
     lwd = 1.4,
     font.main = 1, #makes the title regular (default is bold)
     cex.main = 1.6, #expansion factor for title
     cex.lab = 1.45, #expansion factor--multiply size of x and y axis labels by 1.7
     cex.axis = 1.3 #expansion factor for numbered axis labels
)
dev.off ()


