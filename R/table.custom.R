table.custom = function(Ra, Rb) {
  
  Ra = na.omit(Ra)
  Rb=Rb[index(Ra)]
  
  # tab.dates = sapply(cbind(Ra, Rb), function(f)as.character(c(start(na.omit(f)), end(na.omit(f)))))
  row.names(tab.dates) = c("From", "To")
  
  tab.ret = PerformanceAnalytics::table.AnnualizedReturns(cbind(Ra, Rb))
  tab.dist = PerformanceAnalytics::table.Distributions(cbind(Ra, Rb))
  tab.dsr = PerformanceAnalytics::table.DownsideRisk(cbind(Ra, Rb))
  tab.stat = PerformanceAnalytics::table.Stats(cbind(Ra, Rb))
  tab.var = PerformanceAnalytics::table.Variability(cbind(Ra, Rb))
 
  tab.capm = table.CAPM2(Ra, Rb, scale = 12)
  tab.capm = cbind(tab.capm, rep(NA, nrow(tab.capm)))
  colnames(tab.capm) = c(colnames(Ra),colnames(Rb))
  
  if(indexClass(Ra) != "Date"){
    indexClass(Ra) = "Date"
    indexClass(Rb) = "Date"
  } 
  tab.sr = PerformanceAnalytics::table.SpecificRisk(Ra, Rb)
  tab.sr = cbind(tab.sr, rep(NA, nrow(tab.sr)))
  colnames(tab.sr) = c(colnames(Ra),colnames(Rb))
  
 
  
  rbind(  tab.ret
        #tab.dist[c(1, 2, 4),, drop=FALSE]), 
        , tab.stat[6, , drop=FALSE]
        , tab.var[2, , drop=FALSE]
        , tab.stat[c(5, 15:16, 9, 3),, drop=FALSE]
        , tab.dsr[7:9, , drop=FALSE]
        , tab.capm
        , tab.sr)

}