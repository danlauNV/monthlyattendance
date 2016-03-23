# This script uploaded the prod_reference.refschool_geocode table, without checks nor cleaning. There are missing DBNs
# code is duplicated from a section of Erik's latlon.R 
# depends on : pusher2000
# Dependencies : JP analytic report Mar 2016
# DL 2016/2/17
library(ggmap)

schools <- as.data.table(warehouseRefPull("refschool_lcgms"))

schools <- schools[,.(dbn,address,city,stateCode,zip)]
schools[,fulladdress:=paste(address,city,stateCode,zip,sep=",")]
geos <- geocode(schools$fulladdress)
schools <- cbind(schools,as.data.table(geos))

#pusher2000 from warehouseFunctions.R Feb 17 2016
pusher2000("upload geodata NYC schools DL", data = schools, schema = "prod_reference", table = "refschool_geocode", type = "create")
