# assumes xtabond2 gives  coefficients as num[1:] depending on the number of factors in each
# in a vector coeffts
# requires last CSD, rankPres combn to be as full as can be (see while(is.na(ts.lagged[length(ts.lagged[,1]),]$rate)) )

install.packages("DataCombine")
install.packages("dynlm")



rm(p2 , m, coeffts, mod,df.m )
# names(ts.lagged) <- c("year",names(ts.lagged)[2:length(names(ts.lagged))] )
names(ts.lagged) <- c( names(ts.lagged)[1:length(names(ts.lagged))-1], "lagrate") 
# Calculate predicted probabilities manually
coefftformula <- "~ lagrate  + year + factor(month) + factor(CSD) + factor(rankPres) " # + *&^%year^2
first_month_predicting <- 10 + 1 - MONTHSTOFORECAST
looping_month_predicting <- first_month_predicting # initialise



################### SKIP ################### 

p2 <- NULL
ts.lagged$predict <-  as.numeric(NA) #as.numeric(ts.lagged$predict)
while(looping_month_predicting <= 5) {
  # alternatively , while the last line is na is.na(ts.lagged[length(ts.lagged[,1]),]$rate
  # keep predicting
  ts.lagged <- fn.predict.one.month(looping_month_predicting)
  looping_month_predicting = looping_month_predicting + 1 
}

### DONT SEE WHY P2 IS RESET TO NULL, AND WHY ERROR IN REPLACE(TS.LAGGED) EVEN AFTER GOING THRU THE FIRST TIME

# below TODO return ts.lagged and p2 thru attach(fn.predict.one.month) ... dangerous
fn.predict.one.month <- function(looping_month_predicting) {
  # and before predicting, put the prior predicted value in this month's lag if not already there
  if(any(is.na(ts.lagged[ts.lagged$yr== yrmax & ts.lagged$month== looping_month_predicting,]$lagrate  ))){
    vec_obsns_to_fill_lag <- ts.lagged$yr== yrmax & ts.lagged$month== looping_month_predicting
    vec_obsns_to_fill_lag <- na.fill(vec_obsns_to_fill_lag, c("extend", FALSE)) 
    if (!is.null(p2)) {
      ts.lagged$lagrate <- try(replace(ts.lagged$lagrate, vec_obsns_to_fill_lag, p2)) # filling with prior predictions p2
    }
  }
  vec_obsns_to_predict <- with(ts.lagged, yr==yrmax & month== looping_month_predicting)
  vec_obsns_to_predict <- na.fill(vec_obsns_to_predict, c("extend", FALSE)) 
  # "extend" means keep the boundary non-NA value unchanged
  # m to be predicted
  m<- NULL
  df_obsns_to_predict <-  ts.lagged[vec_obsns_to_predict,]
  m <- model.matrix(~ lagrate + year + yr + factor(month)  + factor(rankPres) , ts.lagged )[,]
  df.m <- data.frame(m)
  
  m <- as.matrix(df.m[df.m$yr==yrmax & fn.indexsubset.df.var(df.m, "factor.month.", looping_month_predicting, "== 1"),])
  
  
  # one prediction per school for current month in p2 . Names and order matters
  # run fake regression just to get the names of coeffts
  mod <- lm(rate ~ lagrate  + year + yr + factor(month) + factor(rankPres) ,  ts.lagged)
  coeffts <- coef(mod)
  coeffts[["factor(month)10"]] <- 1/100  # TODO
  if (any(is.na(coeffts))) {browser()} #break
  assign("p2", coeffts %*% t(m)) # assign the global p2
  
  ts.lagged$predict <- replace(ts.lagged$predict, vec_obsns_to_predict, p2)
  # TODO check the order on p2 vs order on predict
  #alternatively ? setDT(ts.lagged)[yr==yrmax & month== month_predicting, predict := p2[1,] ]
  return(ts.lagged)
}


# write.csv(, na= "", )
######################################################################################

library("dynlm")
library(data.table)
library("DataCombine")
library("readxl")
library("stringr")
source("fncomparison.R")



fn.is.which.school <- function(df.m.rows) {
  # assumes has factor names which contain the school
  cols.have.school <- str_detect(names(df.m.rows), "SchoolDBN")
  df.m.rows <- cbind(rownumber_df.m = rownames(df.m.rows), df.m.rows)
  df.m.rows <- df.m.rows[, c(TRUE, cols.have.school)]
  #   df.m.rows <- split(df.m.rows, seq(nrow(df.m.rows)))
  #   whch <- which(df.m.rows[1,])
  #   df.m.rows$schoolcode <- names() 
  df <- melt(df.m.rows)
  df <- df %>% filter(value==1)
  df$SchoolDBN <- substr(df$variable, 18, 25)
  return(df)
}  

fn.slide <- function(df) {
  # takes df assumed to have one school (CSD rankPres) and puts into 
  # slides the second-to-last column and puts it in the last
  df[,length(df)] <- c(NA,  (df[1:length(df[[1]])-1, length(df)-1] )[[1]]   ) # df[all but last row, 2nd to last column] [[1]]
  return(df)
}

coeffts <- read_excel("J:/attendance_timeseries/coeffts2.xlsx")
coeffts <- coeffts[1,2:length(coeffts)]
coeffts <- as.matrix(coeffts)
coeffts <- coeffts
coeffts_attr <- attributes(coeffts)


rm(p2 , m,  mod,df.m )

yrmax <- max(ts.rate$yr, na.rm=TRUE) #duplicated from uploadAttendance
ts.lagged <- ts.rate[!is.na(ts.rate$SchoolDBN) & !is.na(ts.rate$month),] %>% arrange(CSD, rankPres, year)
ts.lagged$yearsq <- (ts.lagged$year)^2
# doesn't work with larger file ts.lagged <-  slide(ts.rate, Var = "rate", GroupVar = c("CSD","rankPres"),slideBy = -1)
# put rate in last col
col_idx <- grep("rate", names(ts.lagged))
ts.lagged <- ts.lagged[, c((1:ncol(ts.lagged))[-col_idx], col_idx)]

# instead of ts.lagged$L.rate <- NA  # ts.lagged$L.rate <- as.numeric(ts.lagged$L.rate )
ts.lagged$L.rate <- as.numeric(NA)
ts.lagged <- ts.lagged %>% group_by(CSD,rankPres) %>% do(fn.slide(.)) 

ts.lagged$L2.rate <- as.numeric(NA)
ts.lagged <- ts.lagged %>% group_by(CSD,rankPres) %>% do(fn.slide(.)) 
ts.lagged$L3.rate <- as.numeric(NA)
ts.lagged <- ts.lagged %>% group_by(CSD,rankPres) %>% do(fn.slide(.)) 
ts.lagged$L4.rate <- as.numeric(NA)
ts.lagged <- ts.lagged %>% group_by(CSD,rankPres) %>% do(fn.slide(.)) 


# end 4 slides


ts.lagged$rankPres<- as.numeric(ts.lagged$rankPres)
# Calculate predicted probabilities manually
#coefftformula <- "~ lagrate  + year + factor(month) + factor(CSD) + factor(rankPres) " # + *&^%year^2


first_month_predicting <- 10 + 1 - MONTHSTOFORECAST
looping_month_predicting <- first_month_predicting # initialise

ts.projection <- ts.lagged[ts.lagged$yr==2015 & ts.lagged$month > (10 - MONTHSTOFORECAST),]
ts.projection$predict <-  as.numeric(NA) #as.numeric(ts.lagged$predict)


###################### below is for getting rownumber_df.m into ts.lagged. will then join on rownumber_df.m, 
#                       joining a second predict column in ts.lagged, and then consolidate the two columns
m<- NULL
m <- model.matrix(~ L.rate + L2.rate + L3.rate + L4.rate  +  factor(month) + factor(yr) + year + yearsq  + rankPres , ts.lagged )[,]
m <- cbind(m[,-1],m[,1]) # in order to get the intercept  to last col to line up with t from stata 
m <- m[,-17] # in order to get ride of the yrr value that stata did not use, and is thus not in t
#df_obsns_to_predict <-  ts.lagged[vec_obsns_to_predict,]
df.m      <- data.frame(m) 
df.m <- cbind(rownumber_df.m = rownames(df.m), df.m)


m.rows.being.predicted <- model.matrix(~ L.rate  + factor(SchoolDBN) + month + yr + year + yearsq  + rankPres , ts.lagged )[,]
df.m.rows <- data.frame(m.rows.being.predicted)

df.m.rows <- fn.is.which.school(df.m.rows)
df.m <- merge(df.m, df.m.rows[,c("rownumber_df.m", "SchoolDBN")], by="rownumber_df.m")

df2015 <- melt(df.m[df.m$factor.yr.2015==1, c(1,3:11, length(df.m))], id.vars=c("SchoolDBN", "rownumber_df.m"))  %>%  
  filter(value==1) %>%
  mutate(month = as.integer(substr(.$variable, 14, 18)))
#df2015$month <-  # extract month number

ts.lagged$rownumber_df.m <- NULL
ts.lagged2015 <- merge(ts.lagged[ts.lagged$yr==2015, ], df2015[, c("SchoolDBN", "rownumber_df.m", "month")], all.x = TRUE)
ts.lagged <- rbind.fill(ts.lagged2015, ts.lagged[ts.lagged$yr != 2015, ])

#######################
# don't need to transform the mul p2 <- plogis(p2)
p2 <- NULL


while(looping_month_predicting <= 10) {
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  if(any(is.na(ts.lagged[ts.lagged$yr== yrmax & ts.lagged$month== looping_month_predicting,]$L.rate  ))){
    vec_obsns_to_fill_lag <- ts.lagged$yr== yrmax & ts.lagged$month== looping_month_predicting
    vec_obsns_to_fill_lag <- na.fill(vec_obsns_to_fill_lag, c("extend", FALSE)) 
    if (!is.null(p2)) {
      ts.lagged$L.rate <- try(replace(ts.lagged$L.rate, vec_obsns_to_fill_lag, p2)) # filling with prior predictions p2 # below fill L2 -L4
      if ( looping_month_predicting + 1 <= 10) {
        vec_obsns_to_fill_lag <- ts.lagged$yr== yrmax & ts.lagged$month== looping_month_predicting+1
        vec_obsns_to_fill_lag <- na.fill(vec_obsns_to_fill_lag, c("extend", FALSE)) 
        ts.lagged$L2.rate <- try(replace(ts.lagged$L2.rate, vec_obsns_to_fill_lag, p2)) 
        if( looping_month_predicting + 2 <= 10) {
          vec_obsns_to_fill_lag <- ts.lagged$yr== yrmax & ts.lagged$month== looping_month_predicting+2
          vec_obsns_to_fill_lag <- na.fill(vec_obsns_to_fill_lag, c("extend", FALSE)) 
          ts.lagged$L3.rate <- try(replace(ts.lagged$L3.rate, vec_obsns_to_fill_lag, p2)) 
          if( looping_month_predicting + 3 <= 10) {
            vec_obsns_to_fill_lag <- ts.lagged$yr== yrmax & ts.lagged$month== looping_month_predicting+3
            vec_obsns_to_fill_lag <- na.fill(vec_obsns_to_fill_lag, c("extend", FALSE)) 
            ts.lagged$L4.rate <- try(replace(ts.lagged$L4.rate, vec_obsns_to_fill_lag, p2)) 
            
          }
        }
      }
    }
  }
  #vec_obsns_to_predict <- with(ts.lagged, yr==yrmax & month== looping_month_predicting)
  #vec_obsns_to_predict <- na.fill(vec_obsns_to_predict, c("extend", FALSE)) 
    # "extend" means keep the boundary non-NA value unchanged
    # m to be predicted
  m<- NULL
  m <- model.matrix(~ L.rate + L2.rate + L3.rate + L4.rate  + factor(month) + factor(yr) + year + yearsq  + rankPres , ts.lagged )[,]
  m <- cbind(m[,-1],m[,1]) # in order to get the intercept  to last col to line up with t from stata 
  m <- m[,-17] # in order to get ride of the yrr value that stata did not use, and is thus not in t   TO DO check this number
  #df_obsns_to_predict <-  ts.lagged[vec_obsns_to_predict,]
  df.m      <- data.frame(m)
  df.m <- cbind(rownumber_df.m = rownames(df.m), df.m)
  

  m.rows.being.predicted <- model.matrix(~ L.rate + L2.rate + L3.rate + L4.rate  + factor(SchoolDBN) + month + yr + year + yearsq  + rankPres , ts.lagged )[,]
  df.m.rows <- data.frame(m.rows.being.predicted)
  
  df.m.rows <- fn.is.which.school(df.m.rows)
  df.m <- merge(df.m, df.m.rows[,c("rownumber_df.m", "SchoolDBN")], by="rownumber_df.m")
  # below drops column SchoolDBN and first column TODO
  m <- as.matrix(df.m[fn.indexsubset.df.var(df.m, "factor.yr.", yrmax, "== 1") & fn.indexsubset.df.var(df.m, "factor.month.", looping_month_predicting, "== 1"), -c(1,length(df.m))])
  
  #df.m.rows <- df.m.rows[df.m.rows$yr ==yrmax  & df.m.rows$month == looping_month_predicting,]
  
  vec_obsns_to_predict <- df.m$factor.yr.2015==1 & fn.indexsubset.df.var(df.m, "factor.month.", looping_month_predicting, "== 1")
  vec_obsns_to_predict <- na.fill(vec_obsns_to_predict, c("extend", FALSE)) 
  # one prediction per school for current month in p2 . Names and order matters
  # run fake regression just to get the names of coeffts
  if (any(is.na(coeffts))) {browser()} #break
  p2 <- coeffts %*% t(m) # assign the global p2   # in case  coeffts not conform to m # p2 <- coeffts[1:length(m[1,])] %*% t(m)
  
  
  df.m$predict <- as.numeric(NA) #

  df.m$predict <-  replace(df.m$predict, vec_obsns_to_predict, p2)
  
  
  #######
#   
#   # drop rows that I will merge 
#   ts.projection <- merge(ts.projection[complete.cases(ts.projection[,c("SchoolDBN", "yr" , "month", "predict" ) ]), ] , 
#                          df.m[ fn.indexsubset.df.var(df.m, "factor.yr.", yrmax, "== 1") & fn.indexsubset.df.var(df.m, "factor.month.", looping_month_predicting, "== 1"), c("SchoolDBN", "predict"), by= "SchoolDBN"]
#   
#                          
                         
                         
                         
                         
                         
                         
                         
                         
                         
                         
                         
                         
  ts.laggedpred <- merge(ts.lagged[ts.lagged$yr==2015 & ts.lagged$month== looping_month_predicting, !names(ts.lagged) %in% c("predict") ],
                         df.m[fn.indexsubset.df.var(df.m, "factor.month.", looping_month_predicting, "== 1" ) & df.m$factor.yr.2015==1, c("rownumber_df.m", "predict", "SchoolDBN")], by="SchoolDBN")
  ts.lagged <- merge(ts.laggedpred[, c("SchoolDBN", "month", "yr", "predict")]  , ts.lagged , all.y=TRUE, by=c("SchoolDBN", "month", "yr"))
  ts.laggedpred$predict <- NULL                            
                              
# ts.laggedasdf <- merge(ts.lagged , df.m[, c("rownumber_df.m", "predict")] , by="rownumber_df.m", all.x=TRUE)
  # keep only one predict
  #browser()
  if ("predict.x" %in% names(ts.lagged)){
    ts.lagged$predict <- ifelse (!is.na(ts.lagged$predict.x) , ts.lagged$predict.x , ts.lagged$predict.y )
    ts.lagged$predict.x <- NULL
    ts.lagged$predict.y <- NULL
  }
  
  looping_month_predicting = looping_month_predicting + 1 
}

