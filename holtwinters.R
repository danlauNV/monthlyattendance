T=12
schools.NV <- unique(attdbyschool[attdbyschool$SST == "New Visions" | !is.na(attdbyschool$Focus) , ]$SchoolDBN)
schools.recent.months <- attdbyschool %>% group_by(SchoolDBN) %>% 
  summarise(hasallrecentmonths = (sum(yr==2015, na.rm=TRUE)==6)) %>%
  filter(hasallrecentmonths==TRUE) %>% select(SchoolDBN)


schools.without.last.year <- attdbyschool %>% group_by(SchoolDBN) %>% 
  summarise(hasallrecentmonths = (sum(yr==2014 & !is.na(rate), na.rm=TRUE)<10)) %>%
  filter(hasallrecentmonths==TRUE) %>% select(SchoolDBN)

schools.we.lose <- schools.recent.months[schools.recent.months$SchoolDBN %in% (setdiff(schools.recent.months$SchoolDBN, schools.without.last.year$SchoolDBN)),]$SchoolDBN
table(attdbyschool[which(attdbyschool$SchoolDBN %in% schools.we.lose), ]$SST)


schools.w.last.year <- unique(attdbyschool[schools.recent.months$SchoolDBN %in% (setdiff(schools.recent.months$SchoolDBN, schools.without.last.year$SchoolDBN)), ]$SchoolDBN)
length(schools.w.last.year)

schools.NV.w.last.year <- intersect(schools.w.last.year, schools.NV)


five.schools <- levels(attdbyschool$SchoolDBN)[1:5]
library(stats)
# schools.w.last.year is list of schools we are making HW preds for


fn.forecast.school <- function(str_school){
  one.school <- attdbyschool[which(attdbyschool$SchoolDBN==str_school),]$rate
  demand <- ts(one.school, start = c(2000, 1), frequency = 10)
  
  hw <- HoltWinters(demand)
 
  forecast <- predict(hw, n.ahead = 4, prediction.interval = T, level = 0.8)
  return(forecast)
}
# five.plots <- lapply(five.schools, fn.forecast.school)
# one.school <- attdbyschool[which(attdbyschool$SchoolDBN=="01M015"),]$rate
# demand <- ts(one.school, start = c(2000, 1), frequency = 10)
# plot(demand)
# hw <- HoltWinters(demand)
# plot(hw)
# forecast <- predict(hw, n.ahead = 4, prediction.interval = T, level = 0.8)
# p <- plot(hw, forecast)
# str(forecast)
# forecast[,"fit"]
schools.to.hw <- setdiff(schools.NV.w.last.year , schools.NV.w.last.year[112])
li.mts.forecast <- lapply(schools.to.hw, fn.forecast.school)

##################### FINDING THE ONE school that spits out error ################
lapply(schools.NV.w.last.year[31:40], fn.plot.one ) 
fn.plot.one <- function(str_school){
  plot(attdbyschool[attdbyschool$SchoolDBN ==str_school,]$rate)
}
for(i in c(1:11)){
  print(i)
  fn.forecast.school(schools.NV.w.last.year[110:120][i])
}
lapply(schools.NV.w.last.year[112], fn.plot.one ) # don't understand why this doesn't work





attdbyschool[which(attdbyschool$SchoolDBN %in% schools.NV.w.last.year[110:120]),]$rate
