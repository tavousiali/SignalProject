library(plumber)
setwd("C:\\Users\\a.tavoosi\\Documents\\R Script\\SignalProject")
r <- plumb("Test.R")
r$run(port=8000)
