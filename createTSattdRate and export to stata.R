library("plyr")
library("dplyr")
library("readxl")
data_dir <- "C://Users//Dlau//Documents//R//attendancecsv//"
data_dir2 <- "C:/Users/Dlau/Documents/R/attendancecsv/"
setwd("J:/myRrepo/attendanceJP")

install.packages("intervals")
install.packages("plm")
install.packages("sma")
install.packages("lattice")
install.packages("foreign")
install.packages("gridExtra")


library(devtools)
install_github("IQSS/YourCast")
 
# # load package w/o installing
# load_all("c:/Users/Dlau/Downloads/unloadedpackages")

#install.packages("C:/Users/Dlau/Downloads/YourCast.zip", repos = NULL, type = "source")
library("YourCast")
library("intervals")




#################################3
MONTHSTOFORECAST <- 4
ARTIFICIALYEAR <- 100 #arificial year minus 1, so actually will start at 101. Change if we do daily forecasts
###############

attdbyschoolraw<- read_excel("j:/attendance_timeseries/5 year look by month_toFeb.xlsx", sheet = 2)
attdbyschool <- attdbyschoolraw[,1:16]
names(attdbyschool) <- c(names(attdbyschool)[1],"SchoolDBN",names(attdbyschool)[3:6],"typerolledup", names(attdbyschool)[8:12], "Present", "Absent", "cumP", "cumA")
attdbyschoolraw <- NULL



for(variable in names(attdbyschool)[2:10] ){
  attdbyschool[[variable]] <- as.factor(attdbyschool[[variable]])
}

# for(variable in names(attdbyschool)[c(3,4,6,7,8,9,10)] ){
#   print(variable)
#   print(table(attdbyschool[,variable], useNA="always"))
# }
# for(variable in names(attdbyschool)) factor(variable)
#pusher2000DL(script = "JP_attd", data= attdbyschool, schema = "attendance", table = "nyc_2010",  type = "create" )

attdbyschool$yr[attdbyschool$yr== "2010-11"]<- "2010"
attdbyschool$yr[attdbyschool$yr== "2011-12"]<- "2011"
attdbyschool$yr[attdbyschool$yr== "2012-13"]<- "2012"
attdbyschool$yr[attdbyschool$yr== "2013-14"]<- "2013"
attdbyschool$yr[attdbyschool$yr== "2014-15"]<- "2014"
attdbyschool$yr[attdbyschool$yr== "2015-16"]<- "2015"
attdbyschool$yr <- as.numeric(attdbyschool$yr)
attdbyschool$rate <- attdbyschool$Present / (attdbyschool$Present + attdbyschool$Absent )
attdbyschool$ytd <- attdbyschool$cumP / (attdbyschool$cumA + attdbyschool$cumP )
attdbyschool$pop <- mean(attdbyschool$Present, na.rm=TRUE) + mean(attdbyschool$Absent, na.rm = TRUE)  


#yourcast
# year monhth to numeric digits
attdbyschool$time <- (attdbyschool$yr -2010 )*10 + attdbyschool$month




ref.twoway <- with(attdbyschool, table(SchoolDBN, CSD))
ref.twoway <- melt(ref.twoway, value.name = "obs")
ref.twoway <- ref.twoway[ref.twoway$obs>0,]


sum.attd <- attdbyschool %>% group_by(SchoolDBN, yr, month) %>% select(Present)  %>% summarise(obs=n())
sum.attd[sum.attd$obs>1,]  
# 75X811 2014 3

# setdiff expected to be from dplyr
deleting.obsns <- 

sum.attd <- attdbyschool %>% group_by(SchoolDBN, CSD) %>% select(Present)  %>% summarise(obs=n())
sum.attd[sum.attd$obs>1,] # chg this value from 1 to say 5 if i want to enforce minimum number of obsns
sum.attd <- attdbyschool %>% group_by(SchoolDBN) %>% select(CSD)  %>% summarise(obs=n_distinct(CSD))
sum.attd[sum.attd$obs>1,]
sum.attd <- attdbyschool %>% group_by(CSD) %>% select(boro)  %>% summarise(obs=n_distinct(boro))
sum.attd[sum.attd$obs>1,]
# CSD   obs
# 1     75     5
# 2     79     3
sum.attd <- attdbyschool %>% group_by(SchoolDBN) %>% select(SST)  %>% summarise(obs=n_distinct(SST))
sum.attd[sum.attd$obs>1,]

# these schools not in analysis set
with(attdbyschool[attdbyschool$month ==4,], plot(CSD, ytd))
# transfer
with(attdbyschool[attdbyschool$month ==4 & attdbyschool$ytd < 0.7,],plot(CSD, typerolledup))

table(attdbyschool[attdbyschool$month >3 & attdbyschool$ytd < 0.6,]$yr, attdbyschool[attdbyschool$month >3 & attdbyschool$ytd < 0.6,]$type)
table(attdbyschool[attdbyschool$month >3 & attdbyschool$ytd > 0.6,]$SST, attdbyschool[attdbyschool$month >3 & attdbyschool$ytd > 0.6,]$CSD)




ref.dbn <- fn.into.unique(attdbyschool, "SchoolDBN", sdf)
# supporting function
# break into unique schools based on var(s)
fn.test.groupby <- function(df, g_var, age_var){
  x <- df %>% group_by(eval(g_var)) %>% unique("month")
  #%>% select(eval(age_var)) %>% s
  return(x)
}

###########################################################################

delete <- fn.test.groupby(attdbyschool[1:50,], "CSD", "SchoolDBN")

delete <- attdbyschool[1:50,] %>% group_by(CSD) %>% select(month) %>% distinct(month) 
fn.list.pairsvalues <- function(listvalues){
  listvalues <- sort(listvalues)
  intvls <- cbind(listvalues[1:(length(listvalues)-1)], listvalues[2:length(listvalues)])
  return(intvls)
}

fn.list.pairsvalues(c(2,5,6))

sort(delete2)
delete <- c(1,2,3)
delete2 <- c(2,3,4)
delete <- cbind(delete, delete2)

fn.into.unique <- function( df, g_var, age_var){
  ref.dbn <- data.frame(table(df[, g_var]))
  names(ref.dbn) <- c(g_var, "freq")
  ref.dbn$g_code <- sprintf("attd%04d", ref.dbn$code)
  #ref.dbn[dbn, g_var, age_var, g_code, age_code]
  return(ref.dbn)
}
  names(ref.dbn) <- c("dbn", "freq")
  ref.dbn$code <- seq(1,length(ref.dbn$dbn))
  ref.dbn$g_code <- sprintf("attd%04d", ref.dbn$code)
  
  vec1 <- df[, c("SchoolDBN","CSD", "pop")] %>% group_by(SchoolDBN) %>% select(CSD, pop) 
  vecranges <- fn.group.second.coordinate(vec1)
  ref.csd <- data.frame(table(attdbyschool$CSD))
  names(ref.csd) <- c("csd", "freq")
  ref.csd$code <- seq(1,length(ref.csd$csd))
  ref.csd$filecode <- sprintf("%04d",ref.csd$code)
  ref.dbn <- merge(ref.dbn, ref.csd, by.x = g_var )
  return(ref.dbn)
}

fn.group.second.coordinate <- function(vec){
  # first elem of vec is geog, 2nd elem is age. it is as long as there are dbn's
  # intermediate table is of size (geogxnumranges) long (longer than num dbns)  with  cols geog, range, count of either 0 or 1
  # returns vector of the ranges wihtin which the second coordinates should be grouped, ie a list explaining denoting the partition  
  # size is (geogxranges) long (longer than num dbns), x 3 wide :  
  # first elem is index of age-group, 2nd elem is lower bound of agegroup, , 3rd is  upper bound of agegroup
  # requires Intervals
  ## algorithm
  # 1 within each first coord (geog) construct intervals within which age group boundaries must exist. name them
  
  # 1.1 for each g_var a matrix of intervals
  # append all intervals (removing the group)
  # make intervals out of them
  
  # ?? attdbyschool[] %>% group_by()
  to <- Intervals(
    matrix(
      c(
        2, 8,
        3, 4,
        5, 10
      ),
      ncol = 2, byrow = TRUE
    ),
    closed = c( TRUE, FALSE )
  # 2.cluster the intervals : returning A list whose components are the clusters. 
  # 3. find the component wiht the most interval-members. find the midpoint. log as a boundary.
  # For each interval in that one component's list, 
  # remove it ( the interval index) from all other components
  # 4. repeat 3 utnil no more clusters of 2 
  # find midpoints of all remaining intervals and log them
  # order and give an index to them, and create the intervals in longvec
  return(longvec)
}


# write.csv for all files
d_ply(attdbyschool[1:50,], .(SchoolDBN), 
      function(x) {write.csv2(x, 
                         file=paste0("delete", unique(x$SchoolDBN),".csv",sep=""))
      })





#####################################################################################################

# population 5-yr
ref.school <- attdbyschool %>% 
                filter(SST == "New Visions" | !is.na(Focus  )) %>%  
                # filter(typerolledup != "tran" && typerolledup != "YABC") %>%  
                group_by(CSD,SchoolDBN ) %>% 
                summarise(avgPres = mean(Present, na.rm=TRUE))

ref.school <-   ref.school %>%  group_by(CSD) %>% mutate(rankPres = rank(avgPres))

#############
ts.rate <- merge(attdbyschool, ref.school[,1:length(ref.school)], by = c("SchoolDBN", "CSD"))  #### TODO  include all schools
ts.rate$year_artif= ts.rate$month + (10*(ts.rate$yr - 2010)) + ARTIFICIALYEAR # plus 1 because yourprep in df mode won't allow 1as first rowname
ts.rate$year = ts.rate$year_artif
ts.rate$yearsq = (ts.rate$year)^2

# merge to fill out all observations OR only forecast those with 54 observations
# MONTHSTOFORECAST

create.df.allmonths <- function(df.with.schools){
  df.a <- unique(df.with.schools[, c("SchoolDBN", "CSD", "rankPres")])
  numschools <- length(df.a$SchoolDBN)
  df.a <- df.a[rep(seq_len(nrow(df.a)), 60 - MONTHSTOFORECAST), ] # replicate  max_observations times
  df.a <- arrange(.data = df.a, SchoolDBN ) # this may not be active but it may not matter anyway TO TEST
  df.a$year <- rep(seq(ARTIFICIALYEAR +1, ARTIFICIALYEAR+60 - MONTHSTOFORECAST), numschools)
  return(df.a)
}
df.all.schools.months <- create.df.allmonths(ts.rate)


ts.rate <- merge(df.all.schools.months, ts.rate, all.x = TRUE ) #by = all

# fn.addrows 
  # assumes forecast to the end of the year
  ## could make this look at the last row and increment the forecast months automatcly
fn.addrows <- function(df, numrows){
  # expects the cols in d_ply below
  # expects to be used with only one CSD rankPres (so in ddply)
  SchoolDBN <- rep(unique(df$SchoolDBN), times= numrows)
  CSD <- rep(unique(df$CSD), times= numrows)
  rankPres <- rep(unique(df$rankPres), times= numrows)
  month <- seq((10-numrows +1),10)
  year <- seq((ARTIFICIALYEAR+60- MONTHSTOFORECAST +1), ARTIFICIALYEAR+60)
  rows_to_add <- data.frame(SchoolDBN, CSD, rankPres, month, year)
  df <- rbind.fill(df, rows_to_add)
  return(df)
}



ts.rate <- ts.rate %>%
  select(year,rate, SchoolDBN ,  CSD, rankPres, month, yr) %>%  # TODO # add absent present
  group_by(CSD, rankPres) %>%
  do(fn.addrows(., MONTHSTOFORECAST)) # fn.add.rows has assumptions

yrmax <- max(ts.rate$yr, na.rm=TRUE)
ts.rate[is.na(ts.rate$yr), ]$yr <- yrmax
ts.rate$CSD <- sprintf("%02d",ts.rate$CSD)
ts.rate <- ts.rate[!duplicated(ts.rate),]
write.csv(ts.rate, file="tostata_toFeb.csv", na="", row.names = FALSE         , quote= FALSE)





##################### ALL BELOW IS NOT USED ############################







# formatting oddities of yourcast  ## TODO remove <100 and the subsetting #[ts.rate$rankPres < 100,]
ts.rate <- ts.rate[, ] %>% arrange(CSD, rankPres, year) # bec the format can only handle double digits
ts.rate$CSD <- sprintf("%04d",ts.rate$CSD)
#ts.rate$rankPres <- sprintf("%03d",ts.rate$rankPres) # for yourprep needs to be %02d



ts.rate <- ts.rate[!duplicated(ts.rate),]

# write.csv here

ts.rate <- ts.rate[(ts.rate$CSD=="0001" | ts.rate$CSD=="0002" )& ts.rate$rankPres < "10"  ,]
# print filesj
d_ply(ts.rate, .(CSD, rankPres), 
      function(x) {write.table(x, 
                              file=paste0(data_dir2, "pres", unique(x$CSD),unique(x$rankPres), ".csv",sep=" "),row.names = FALSE,quote=FALSE)
      })

fn.assign.df <- function(df){
  
  #datalist <- append(datalist, paste0("pres", unique(df$CSD), unique(df$rankPres)))
  
  #assign(paste0("pres", unique(df$CSD), unique(df$rankPres)), df, pos = 1)
  rownames(df) <- df$year #  yourprep in df mode requires rownames
  assign(paste0( unique(df$CSD), unique(df$rankPres)), df, pos = 1)
  #nm = paste0( unique(df$CSD), unique(df$rankPres))
  #nm = paste0("pres", unique(df$CSD), unique(df$rankPres)), df, pos = 1
  #datalist[[nm]] <- df
  #assign("datalist", datalist, pos=1 )
}
delete <- ts.rate %>% group_by(CSD, rankPres) %>% summarise(length(unique(year)))
delete <- NULL
ts.rate %>% group_by(CSD, rankPres) %>% do(fn.assign.df(.))

datalist <- Filter(function(x) is(x, "data.frame") , mget(ls()))
names(datalist)


datalist[["deleting.obsns"]] <- NULL
datalist[["database"]] <- NULL
datalist[["attdbyschool"]] <- NULL
datalist[["ref.dbn"]] <- NULL
datalist[["ref.school"]] <- NULL
datalist[["ref.twoway"]] <- NULL
datalist[["sum.attd"]] <- NULL
datalist[["ts.rate"]] <- NULL
datalist[["df1"]] <- NULL
datalist[["df.all.units.months"]] <- NULL
datalist[["df.allmonths"]] <- NULL


 #rm(list= names(Filter(function(x) is(x, "data.frame"), mget(ls()))))


# d_ply(ts.rate[1:200,], .(CSD, rankPres), 
#       function(x) {fn.assign.df(x)
#       })

# YOURPREP
dta <- yourprep(datalist = datalist, tag="pres", year.var =TRUE, sample.frame =c(ARTIFICIALYEAR+1,ARTIFICIALYEAR+60- MONTHSTOFORECAST,ARTIFICIALYEAR+60- MONTHSTOFORECAST+1,ARTIFICIALYEAR+60), verbose =TRUE)

dta <- yourprep(dpath=data_dir2, tag="pres", year.var=TRUE, sample.frame=c(ARTIFICIALYEAR+1,ARTIFICIALYEAR+60- MONTHSTOFORECAST,ARTIFICIALYEAR+60- MONTHSTOFORECAST+1,ARTIFICIALYEAR+60), verbose=TRUE)
ylc <- yourcast(formula= log(rate) ~ month, dataobj=dta, model="map")

ts.rate[ts.rate$yr==2015 & ts.rate$month ==4,],table(CSD, poprank))

, 

names(attdbyschool)
# can't get below dplyr to work. Used plyr instead
attdbyschool[1:50,] %>% group_by(SchoolDBN) %>% select(SchoolDBN, Present) %>% (function(x) write.csv(x,file = paste0("deleteab",x[1,]$SchoolDBN,".csv") ))


schools.notanalyzing

# create
# viz and clean
## remove schools with few obsns ?
# filter
# viz TS attribs
# format
attdbyschool$year_artif <- attdbyschool$month + (10*(attdbyschool$yr - 2010))
attdbyschool$csd_artif <- 

attd.to.prep <- cbind(dbn, indx_historical_attd, SSTcode,)
# indx is two-digit
# create SST ref code table



# TS



# archive
attdbyschool %>% filter(cyl == 4) %>% write.csv(.,file = "~/Desktop/piping.csv")
d_ply(df, .(theday), 
      function(x) {write(paste(">", x$var1,"_", x$var2, "\n", x$theday, sep=""), 
                         file=paste(unique(x$theday),".fasta",sep=""))
      })
