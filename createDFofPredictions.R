# from matrix of predictions
fn.preds.from.mt.todf <- function(mt, str_school, monthstoforecast, year){
  df = data.frame(month = seq(10-monthstoforecast+1, 10),yr = rep.int(year, monthstoforecast),SchoolDBN = str_school, 
                  projection = mt[, "fit"], projectionLower =  mt[, "upr"],projectionUpper =  mt[, "lwr"])
  return(df)
}

# grid expand on all possible month year school combos of forecast
# possible schools include any with a predict
#SchoolDBN <- unique(ts.lagged[!is.na(ts.lagged$predict), "SchoolDBN"] )
#holt
SchoolDBN <- schools.to.hw
month <- seq(10- MONTHSTOFORECAST +1, 11)
yr <- 2015
ts.to.tableau <- expand.grid(month = month, yr = yr, SchoolDBN= SchoolDBN)


# merge the projections and forecasts by the grid-fields

# ts.sendmarch <- ts.lagged[!is.na(ts.lagged$SchoolDBN) & !is.na(ts.lagged$month) & ts.lagged$month > (10 - MONTHSTOFORECAST), ]
# # ts.sendmarch$date <- fn.format.date(ts.sendmarch$yr, ts.sendmarch$month)
# ts.sendmarch$projection <- ts.sendmarch$predict
# ts.sendmarch$projectionLower <- ts.sendmarch$predict - fn.calctd.halfinterval
# # MAR chg this to use forecast half for the appropriate entry Will need to make a function
# # #takes df and list, and list schools, spits out forecast
# ts.sendmarch$projectionUpper <- ts.sendmarch$predict + fn.calctd.halfinterval

li.dfs.prediction <- Map(fn.preds.from.mt.todf, li.mts.forecast, schools.to.hw, MONTHSTOFORECAST, 2015)
############################# NEED TO RUN BELOW BEFORE creating EOY ############################
ts.send.april <- bind_rows(li.dfs.prediction)