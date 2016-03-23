T=12
schools.recent.months <- attdbyschool %>% group_by(SchoolDBN) %>% 
  summarise(hasallrecentmonths = (sum(yr==2015, na.rm=TRUE)==6)) %>%
  filter(hasallrecentmonths==TRUE) %>% select(SchoolDBN)


schools.without.last.year <- attdbyschool %>% group_by(SchoolDBN) %>% 
  summarise(hasallrecentmonths = (sum(yr==2014 & month >6, na.rm=TRUE)==4)) %>%
  filter(hasallrecentmonths==TRUE) %>% select(SchoolDBN)

schools.we.lose <- schools.recent.months[schools.recent.months$SchoolDBN %in% (setdiff(schools.recent.months$SchoolDBN, schools.without.last.year$SchoolDBN)),]$SchoolDBN
table(attdbyschool[which(attdbyschool$SchoolDBN %in% schools.we.lose), ]$SST)


schools.w.last.year <- unique(attdbyschool[schools.recent.months$SchoolDBN %in% (setdiff(schools.recent.months$SchoolDBN, schools.without.last.year$SchoolDBN)), ]$SchoolDBN)
length(schools.w.last.year)

)
five.schools <- levels(attdbyschool$SchoolDBN)[1:5]
library(stats)
# schools.w.last.year is list of schools we are making HW preds for


fn.forecast.school <- function(str_school){
  one.school <- attdbyschool[which(attdbyschool$SchoolDBN==str_school),]$rate
  demand <- ts(one.school, start = c(2000, 1), frequency = 10)
  
  hw <- HoltWinters(demand)
 
  forecast <- predict(hw, n.ahead = 4, prediction.interval = T, level = 0.8)
  plot1 <- plot(hw, forecast)
  return(plot1)
}
five.plots <- lapply(five.schools, fn.forecast.school)
one.school <- attdbyschool[which(attdbyschool$SchoolDBN=="01M015"),]$rate
demand <- ts(one.school, start = c(2000, 1), frequency = 10)
plot(demand)
hw <- HoltWinters(demand)
plot(hw)
forecast <- predict(hw, n.ahead = 4, prediction.interval = T, level = 0.8)
p <- plot(hw, forecast)
str(forecast)
