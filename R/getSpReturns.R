# getSpReturns = function(inds, startDate = NULL, endDate = NULL){
#   require(xts)
#   require(PerformanceAnalytics)
#   if(is.null(startDate))
#       startDate = start(inds)
#   if(is.null(endDate))
#       endDate = end(inds)
#   sp = inds[,"SPTR"]
#   sp = na.omit(sp)
#   sp=CalculateReturns(sp)
#   return(na.omit(sp[paste(startDate,endDate,sep = "/"),]))
# }

getSpIndex = function(){
  # require(xts)
  # require(PerformanceAnalytics)
  # require(quantmod)
  quantmod::getSymbols('^GSPC', from = '1950-01-03')
  sp = quantmod::Ad(SP500TR)
  names(sp)[1] = "SPTR"
  return(sp)
}

getSpReturns = function() {
  sp = getSpIndex()
  Return.calculate(sp)
}
