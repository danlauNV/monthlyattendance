# format to warehouse
fn.format.date <- function(yr, month) {
  ifelse (month ==11, return(as.Date(paste(yr, month-4, "01", sep="-"))) , 
    ifelse ( month <= 4, return(as.Date(paste(yr, month+8, "15", sep="-"))) , 
      ifelse (month > 4, return(as.Date(paste(yr+1, month-4, "15", sep="-"))) )))
}


# grid used to be here 

############################# merge ############################
ts.to.tableau <- merge(ts.to.tableau, ts.send.april[, c("month","yr","SchoolDBN", "projection", "projectionLower","projectionUpper")], all.x=TRUE, by = c("month", "yr", "SchoolDBN"))
ts.to.tableau <- merge(ts.to.tableau, fullresults[, c("month","yr","SchoolDBN", "EOYforecast", "EOYlower","EOYupper")], all.x=TRUE, by= c("month", "yr", "SchoolDBN"))

ts.to.tableau$date <- fn.format.date(ts.to.tableau$yr, ts.to.tableau$month)
ts.to.tableau <- ts.to.tableau[, !names(ts.to.tableau) %in% c("month", "year")]
####################### DUE TO BUG ####################### 
ts.to.tableau <- rename(ts.to.tableau, EOYlower = EOYupper, EOYupper= EOYlower)  #OMG SO HORRIBLE. even begs for a reordering of columns


####################### write csv ####################### 
write.csv( ts.to.tableau, "c:/users/dlau/documents/R/attendancecsv/monthlyattdforecast_apr.csv")
# 	yr	SchoolDBN	 projection	projectionLower	projectionUpper	EOYforecast	EOYlower	EOYupper	date=eg.6/1/2015


#QA check
View(ts.to.tableau[ts.to.tableau$SchoolDBN=="19K158", ])
View(ts.lagged[ts.lagged$SchoolDBN=="19K158" & ts.lagged$yr==2015, ])
View(ts.lagged[ts.lagged$SchoolDBN=="11X370" & ts.lagged$yr==2015, ])
View(ts.to.tableau[ts.to.tableau$SchoolDBN=="11X370", ])



source("~/R/SetWorkspace.R")
pusher2000("manual DL", ts.to.tableau, "prod_reference", "refschool_attdmonthlyforecast", type= "create")

 
)