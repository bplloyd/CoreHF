rollVsBM = function(Ra, Rb, width = 12, FUN = "ActivePremium"){
  f = match.fun(FUN)
  Ra = na.omit(Ra)
  Rb = Rb[index(Ra)]
  if(indexClass(Ra) != "Date"){
    indexClass(Ra) = "Date"
    indexClass(Rb) = "Date"
  }
  roll = apply.rolling(Ra, 
                       width = width, 
                       FUN = function(Ra)f(Ra, Rb))
  
  
  
}