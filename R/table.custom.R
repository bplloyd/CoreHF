table.custom = function(Ra, Rb) {
  
  Ra = na.omit(Ra)
  Rb=Rb[index(Ra)]
  
  # tab.dates = sapply(cbind(Ra, Rb), function(f)as.character(c(start(na.omit(f)), end(na.omit(f)))))
  # row.names(tab.dates) = c("From", "To")
  
  tab.ret = PerformanceAnalytics::table.AnnualizedReturns(cbind(Ra, Rb), scale = 12)
  tab.dist = PerformanceAnalytics::table.Distributions(cbind(Ra, Rb), scale = 12)
  tab.dsr = PerformanceAnalytics::table.DownsideRisk(cbind(Ra, Rb), scale = 12)
  tab.stat = PerformanceAnalytics::table.Stats(cbind(Ra, Rb))
  tab.var = PerformanceAnalytics::table.Variability(cbind(Ra, Rb), scale = 12)
  # tab.ir = PerformanceAnalytics::table.InformationRatio(Ra, Rb, scale = 12)
  
  per_lengths = c(1,3,12,36, 60)
  tabs.op =  lapply(Ra, function(R) table.ProbOutPerformance(R, Rb[index(R)], period_lengths = per_lengths))
  tab.op = tabs.op[[1]][, 5, drop =FALSE]
  if(ncol(Ra) > 1){
    for(j in 2:ncol(Ra)){
      tab.op = cbind(tab.op, tabs.op[[j]][, 5, drop =FALSE])
    }
  }
  tab.op = cbind(tab.op, rep(NA, nrow(tab.op)))
  colnames(tab.op) = c(names(Ra), names(Rb))
  row.names(tab.op) = paste0("Prob Outperform ", names(Rb), " ", per_lengths, "m")
  
  
  
  tab.capm = table.CAPM2(Ra, Rb, scale = 12)
  tab.capm = cbind(tab.capm, rep(NA, nrow(tab.capm)))
  
  tab.capm_bull = table.CAPM2(Ra[Rb > 0], Rb[Rb > 0], scale = 12)
  tab.capm_bull = cbind(tab.capm_bull, rep(NA, nrow(tab.capm_bull)))
  
  tab.capm_bear = table.CAPM2(Ra[Rb < 0], Rb[Rb < 0], scale = 12)
  tab.capm_bear  = cbind(tab.capm_bear, rep(NA, nrow(tab.capm_bear)))
  
  colnames(tab.capm) = colnames(tab.capm_bear) = colnames(tab.capm_bull) = c(colnames(Ra),colnames(Rb))
  row.names(tab.capm_bear) = paste0(row.names(tab.capm_bear), " (bear)")
  row.names(tab.capm_bull) = paste0(row.names(tab.capm_bull), " (bull)")
  
  tab.rw_bull_bear = rbind(tab.capm_bull["Correlation (bull)",]^2, tab.capm_bear["Correlation (bear)",]^2)
  row.names(tab.rw_bull_bear)[1] = "Systematic Risk Weight (bull)"
  row.names(tab.rw_bull_bear)[2] = "Systematic Risk Weight (bear)"
  
  if(indexClass(Ra) != "Date"){
    indexClass(Ra) = "Date"
    indexClass(Rb) = "Date"
  } 
  tab.sr = PerformanceAnalytics::table.SpecificRisk(Ra, Rb)
  tab.sr = cbind(tab.sr, rep(NA, nrow(tab.sr)))
  colnames(tab.sr) = c(colnames(Ra),colnames(Rb))
  tab.sr = rbind(tab.sr, `Systematic Risk Weight` = tab.sr[2,]^2/tab.sr[3,]^2)
  
 
  
  rbind(  tab.ret
        #tab.dist[c(1, 2, 4),, drop=FALSE]), 
        , tab.stat[6, , drop=FALSE]
        , tab.var[2, , drop=FALSE]
        , tab.stat[c(5, 15:16, 9, 3),, drop=FALSE]
        , tab.dsr[7:9, , drop=FALSE]
        , tab.capm
        , tab.capm_bull
        , tab.capm_bear
        , tab.sr
        , tab.rw_bull_bear
        , tab.op)

}