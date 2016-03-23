# format to warehouse
fn.format.date <- function(yr, month) {
  ifelse (month ==11, return(as.Date(paste(yr, month-4, "01", sep="-"))) , 
    ifelse ( month <= 4, return(as.Date(paste(yr, month+8, "15", sep="-"))) , 
      ifelse (month > 4, return(as.Date(paste(yr+1, month-4, "15", sep="-"))) )))
}

# grid expand on all possible month year school combos of forecast
# possible schools include any with a predict
SchoolDBN <- unique(ts.lagged[!is.na(ts.lagged$predict), "SchoolDBN"] )
month <- seq(10- MONTHSTOFORECAST +1, 11)
yr <- 2015
ts.to.tableau <- expand.grid(month = month, yr = yr, SchoolDBN= SchoolDBN)


# merge the projections and forecasts by the grid-fields

ts.sendmarch <- ts.lagged[!is.na(ts.lagged$SchoolDBN) & !is.na(ts.lagged$month) & ts.lagged$month > (10 - MONTHSTOFORECAST), ]
# ts.sendmarch$date <- fn.format.date(ts.sendmarch$yr, ts.sendmarch$month)
ts.sendmarch$projection <- ts.sendmarch$predict
ts.sendmarch$projectionLower <- ts.sendmarch$predict - fn.calctd.halfinterval
ts.sendmarch$projectionUpper <- ts.sendmarch$predict + fn.calctd.halfinterval

ts.to.tableau <- merge(ts.to.tableau, ts.sendmarch[, c("month","yr","SchoolDBN", "projection", "projectionLower","projectionUpper")], all.x=TRUE, by = c("month", "yr", "SchoolDBN"))
ts.to.tableau <- merge(ts.to.tableau, fullresults[, c("month","yr","SchoolDBN", "EOYforecast", "EOYlower","EOYupper")], all.x=TRUE, by= c("month", "yr", "SchoolDBN"))

ts.to.tableau$date <- fn.format.date(ts.to.tableau$yr, ts.to.tableau$month)
ts.to.tableau <- ts.to.tableau[, !names(ts.to.tableau) %in% c("month", "year")]

write.csv( ts.to.tableau, "c:/users/dlau/documents/R/attendancecsv/monthlyattdforecast.csv")


#QA check
View(ts.to.tableau[ts.to.tableau$SchoolDBN=="19K158", ])
View(ts.lagged[ts.lagged$SchoolDBN=="19K158" & ts.lagged$yr==2015, ])
View(ts.lagged[ts.lagged$SchoolDBN=="11X370" & ts.lagged$yr==2015, ])
View(ts.to.tableau[ts.to.tableau$SchoolDBN=="11X370", ])

  
)