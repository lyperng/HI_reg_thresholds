##########WRITE FUNCTION FOR CREATING GAMFIT/TRENDS/THRESHOLDS DF########
#function that converts raw data to gamfit with trends & thresholds marked
#creates gamfit output named df1

gam_tnt<-function(df, newd.num, gammod, cont.pred,cont.name, cat.pred, cat.name ) {
  #df: input df
  #newd.num: how many values within the continuous predictor range to predict on?--have been using 500
  #the gam() object
  #cont.pred: continuous predictor--Year in this case
  #cont.name: Name the continuous predictor in output df-- 'Year'
  #cat.pred: categorical predictor level (this is just for labeling the categorical var in the output)
  #cat.name: name the cat predictor
  
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
  
  #the following two lines are for linear relationships where, by default, the entire function is highlighted as a thresholds
  #these lines delete the threshold columns when this is the case
  ifelse(sum(is.na(df1$se.threshold))==0,df1$se.threshold<-NA, df1$se.threshold<-df1$se.threshold)
  ifelse(sum(is.na(df1$threshold))==0,df1$threshold<-NA, df1$threshold<-df1$threshold)
  
  return(df1)
  
}

