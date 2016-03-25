source("createTSattdRate and export to stata.R")


if (1==1) {
  source("holtwinters.R") # creates FC with confidence intvls
  source("createDFofPredictions.R")
  source("calculateEOYforecast.R")

  
  } else {
    source("xtabond.R")
    source("calcEOYbasedonStata.R")
}



source("exporttoTableau.R")
