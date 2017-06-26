autocorrFactor = function(R, h=12, lags = 1){
  R = na.omit(R)
  Q = acf(R, plot = FALSE)$acf[2]
  sqrt(h + 2*(Q / (1-Q)^2)*((h - 1)*(1 - Q) - Q*(1 - Q^(h-1))))
}

volACFAdjusted = function(R, h=12, lags=1){
  R=na.omit(R)
  sf = autocorrFactor(R, h=h, lags=lags)
  return(sd(R)*sf)
}

sharpeACFAdjusted = function(R, h=12, lags=1, Rf=0){
  return.annualized(R-Rf, h) / volACFAdjusted(R, h, lags)
}