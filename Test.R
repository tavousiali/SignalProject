#install.packages("plumber")

library(plumber)
detach("package:NoavaranSymbols", unload=TRUE)
library("NoavaranSymbols", lib.loc="C:/Program Files/R/R-3.5.2/library")
library(NoavaranIndicators)
library(TTR)
# plumber.R

#* @filter cors
cors <- function(res) {
  res$setHeader("Access-Control-Allow-Origin", "*")
  plumber::forward()
}

#* Echo back the input
#* @param msg The message to echo
#* @get /MaCross
function(){

  stockVector = c()

  for(i in 1:nrow(Noavaran.Companies)) {
    #i = 1
    symbolName = Noavaran.Companies[i,2]
    stringSymbolName = paste("Noavaran.Symbols.", symbolName, sep = "")

    thisSymbolDataframe = tryCatch({
      get(stringSymbolName)
    }, error = function(e) {

    })

    if (!is.null(thisSymbolDataframe)) {
      smaLow = Noavaran.Indicator.SMA(thisSymbolDataframe, 9)
      smaMid = Noavaran.Indicator.SMA(thisSymbolDataframe, 21)

      if (!is.null(smaLow) && !is.null(smaMid)) {
        thisSymbolDataframe = cbind(thisSymbolDataframe, smaLow = smaLow, smaMid = smaMid)
      } else  {
        thisSymbolDataframe = cbind(thisSymbolDataframe, smaLow = NA, smaMid = NA)
      }

      assign(stringSymbolName, thisSymbolDataframe)
      #print(stringSymbolName)

      smaLowToday = thisSymbolDataframe[nrow(thisSymbolDataframe),]$smaLow
      smaLowYesterday = thisSymbolDataframe[nrow(thisSymbolDataframe) - 1,]$smaLow
      smaMidToday = thisSymbolDataframe[nrow(thisSymbolDataframe),]$smaMid
      smaMidYesterday = thisSymbolDataframe[nrow(thisSymbolDataframe) - 1,]$smaMid

      if (!is.na(smaLowToday) && !is.na(smaLowYesterday) && !is.na(smaMidToday) && !is.na(smaMidYesterday)) {
        if (smaLowToday-smaMidToday > 0 && smaLowYesterday - smaMidYesterday <= 0) {

          comId = Noavaran.Companies[i,1]
          stockVector = c(stockVector, comId)
          #print(comId)
          #print(stockVector)
        }
      }
    }
  }

  stockDF = data.frame(Com_ID = stockVector)

  #View(stockDF)
  return(stockDF)
}

