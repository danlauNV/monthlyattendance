

# xtabond2 residuals prediction interval calculation

#######

fromstata <- read.csv("J:/myRrepo/attendanceJP/fromstata.csv", na.strings = "" , stringsAsFactors = TRUE)

#fromstata <- data.frame(school = c(23004, 23005, 23006), residuals = c(1,2,3))
  #artificial

fn.calc.eoy.stata <- function(df.yr) {
  # if not approximation, would need to use popn <- with(df.yr[ month==last_month_with_data,], Present + Absent)
  # below predict has more values than just the predictions (also contains the in-sample predicts)
  df1 <- df.yr[df.yr$yr==2015, ]
  avg_rate <- mean(df1[df1$month <= (10- MONTHSTOFORECAST),]$rate, na.rm= TRUE) * (10-MONTHSTOFORECAST)/10 + MONTHSTOFORECAST/10*mean(df1[df1$month > (10- MONTHSTOFORECAST),]$predict.y, na.rm=TRUE)
  EOYlower <- avg_rate - fn.calctd.halfinterval / (0.2)^(1/MONTHSTOFORECAST -1) * (MONTHSTOFORECAST)/10  # ought to t-lookup 0.2 expr
  EOYupper <- avg_rate + fn.calctd.halfinterval / (0.2)^(1/MONTHSTOFORECAST -1) * (MONTHSTOFORECAST)/10  # ought to t-lookup 0.2 expr
  list(EOYforecast=avg_rate,EOYlower=EOYlower, EOYupper=EOYupper )
}




fn.calc.predict.halfinterval.stata <- function() {
  t <- 1.29 # close enough
  n <- length(!is.na(fullresults$residuals))
  rse <- sqrt(sum(sapply(fullresults$residuals, function(x) x^2),na.rm = TRUE) / (n-2))
  be_rooted_inside_stdp <- 1 + 1/n # this should be function of x_i and sxx_inverse and be entered for each row
  stderr_predict <- t*rse*sqrt(be_rooted_inside_stdp)
  return(stderr_predict)
}
  
# fromstata$CSD <- fromstata$school %/% 1000
# fromstata$rankPres <- fromstata$school %% 1000
# fromstata$CSD <- sprintf("%02d",fromstata$CSD)
# fullresults <- merge(fromstata, ref.school[, c("CSD", "SchoolDBN", "rankPres")], by = c( "CSD", "rankPres")) %>%
#                 filter(!is.na(SchoolDBN)) #### TODO  include all schools
# 
# fn.calctd.halfinterval <- fn.calc.predict.halfinterval() # only bc x's won't seem to matter

# before using fn.eoy need to bring in predictions from ts.lagged into the fullresults table
# fullresults <- merge(fullresults, 
#                      ts.lagged[!is.na(ts.lagged$SchoolDBN) & ts.lagged$yr == 2015 & ts.lagged$month > 10 - MONTHSTOFORECAST, 
#                                c("SchoolDBN", "month" , "yr", "predict")] , by =c("SchoolDBN", "month" , "yr"), all.x = TRUE)



#################################Add eoy to a df called full results#################################
# output df is called fullresults because it used to use the full stata observation output to calculate
# yet to add EOYbench=benchmarks[benchmarks$SchoolDBN]
fullresults <- fullresults.hw %>% group_by(SchoolDBN) %>%  do(fn.eoy(.)) 

#length(fullresults[!is.na(fullresults$EOYforecast) & fullresults$month==11 , ]$yr)
# missing EOYforecasts for 20 schools [1] 173


# 
# ts.projection <- rbind.fill(ts.projection, fullresults[fullresults$month==11, c("SchoolDBN", "CSD", "rankPres", "month", "yr", "EOYforecast","EOYlower", "EOYupper" )])
# ts.projection <- ts.projection %>% group_by(SchoolDBN)     %>%
#   mutate( projectionlower = projection - fn.calctd.halfinterval, 
#           projectionupper = projection + fn.calctd.halfinterval)



####################


#y^ ? t_n/2 s_y  sqrt(1 + 1/n + (xi - xbar)^2 / [(n-1)s_x^2]  )

# OMG matrix inversion - leave out for now and we  underestimate the interval
# fullresults[,]
# sxx_inverse <- fullresults[,]
# xbar <- sapply( fullresults, function(x) mean(x, na.rm = TRUE))

# attach(fn.eoy())
# attach( list(EOYbench=NA, EOYforecast=NA, EOYlower=NA, EOYupper=NA)) # a list of 3 lists, ie EOYforecast is a list

