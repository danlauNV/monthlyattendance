
fn.calc.eoy <- function(df.yr) {
  # if not approximation, would need to use popn <- with(df.yr[ month==last_month_with_data,], Present + Absent)
  # below predict has more values than just the predictions (also contains the in-sample predicts)
  df1 <- df.yr[df.yr$yr==2015, ]
  avg_rate <- mean(df1[df1$month <= (10- MONTHSTOFORECAST),]$rate, na.rm= TRUE) * (10-MONTHSTOFORECAST)/10 + MONTHSTOFORECAST/10*mean(df1[df1$month > (10- MONTHSTOFORECAST),]$predict.y, na.rm=TRUE)
  eoy.calctd.halfinterval <- fn.calc.predict.halfinterval(df1$projectionUpper, df1$projectionLower) 
  EOYlower <- avg_rate - eoy.calctd.halfinterval / (0.2)^(1/MONTHSTOFORECAST -1) * (MONTHSTOFORECAST)/10  # ought to t-lookup 0.2 expr
  EOYupper <- avg_rate + eoy.calctd.halfinterval / (0.2)^(1/MONTHSTOFORECAST -1) * (MONTHSTOFORECAST)/10  # ought to t-lookup 0.2 expr
  list(EOYforecast=avg_rate,EOYlower=EOYlower, EOYupper=EOYupper )
}


# do for one school
fn.eoy <- function(df) {
  # does not output CSD nor rankPres
  EOYforecast <- fn.calc.eoy( df[df$yr==2015 ,])[1] 
  EOYlower <- fn.calc.eoy( df[df$yr==2015 ,])[2]
  EOYupper <- fn.calc.eoy( df[df$yr==2015 ,])[3]
  #attach(fn.calc.eoy(df[df$yr==2015 ,])) # df with last year. from df with one school
  df.temp<- data.frame(yr=2015, month= 11, SchoolDBN = unique(df$SchoolDBN),  EOYforecast=EOYforecast,EOYlower=EOYlower, EOYupper=EOYupper ) # yet to add bench
  df.eoy.for.one.school <- rbind.fill(df, df.temp)
  return(df.eoy.for.one.school)
}


fn.calc.predict.halfinterval <- function(var.of.df.projectionUpper, var.of.df.projectionLower) {
  # only to be used in EOY calc. assumes Holt winters
  vec.diffs <- var.of.df.projectionUpper- var.of.df.projectionLower
  eoy.calctd.halfinterval <- mean(vec.diffs, na.rm=TRUE)
  return(eoy.calctd.halfinterval)
}

# fullresults.hw WHICH IS REALLY JUST EOY (not full results. that is a hangover from the stata xtabond route)

#fullresults[, c("month","yr","SchoolDBN", "EOYforecast", "EOYlower","EOYupper")]
fn.create.df.for.eoy.calc <- function(ts.rate, df.with.preds){
  
  df.with.preds <- rename(df.with.preds, predict.y = projection)
  df <- bind_rows(df.with.preds, ts.rate)
  return(df)
}
## DEPENDENCY ALERT on toTableau.R
## Create ts.send.april which has all forecasts in df
## ############################# NEED TO RUN BELOW BEFORE from.stata.R############################
ts.send.april <- bind_rows(li.dfs.prediction) 
fullresults.hw <-  fn.create.df.for.eoy.calc(ts.rate, ts.send.april)


#################################Add eoy to a df called full results#################################
# output df is called fullresults because it used to use the full stata observation output to calculate
# there is a bug that makes hte EOYlower and EOYupper flipped. I can't find the bug
# yet to add EOYbench=benchmarks[benchmarks$SchoolDBN]
fullresults <- fullresults.hw %>% group_by(SchoolDBN) %>%  do(fn.eoy(.)) 



#################################create df for QA#################################
fn.create.df.for.QA <- function(ts.rate, df.with.preds, fullresults){
  
  df.with.preds <- rename(df.with.preds, rate = projection)
  df <- bind_rows(df.with.preds, ts.rate[ts.rate$yr < 2015 | ts.rate$month <= (10- MONTHSTOFORECAST),])
  fullresults$rate <- fullresults$EOYforecast
  fullresults[!is.na(fullresults$EOYforecast),]$yr <- 2015
  df <- bind_rows(df, fullresults[fullresults$month ==11,])
  return(df)
}
## DEPENDENCY ALERT on toTableau.R
fullresults.QA <-  fn.create.df.for.QA(ts.rate, ts.send.april, fullresults)
fullresults.QA$yrmonth <- (fullresults.QA$yr-2010)*11 + fullresults.QA$month
sampleDBN <- sample(fullresults.QA$SchoolDBN, 1)
View(fullresults.QA[fullresults.QA$SchoolDBN == sampleDBN & !is.na(fullresults.QA$rate), c("yrmonth", "rate")])
plot(fullresults.QA[fullresults.QA$SchoolDBN == sampleDBN, ]$yrmonth
     , fullresults.QA[fullresults.QA$SchoolDBN == sampleDBN, ]$rate)