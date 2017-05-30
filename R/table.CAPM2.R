table.CAPM2 = function (Ra, Rb, scale = NA, Rf = 0, digits = 4) 
{
  Ra = PerformanceAnalytics::checkData(Ra)
  Rb = PerformanceAnalytics::checkData(Rb)
  if (!is.null(dim(Rf))) 
    Rf = PerformanceAnalytics::checkData(Rf)
  columns.a = ncol(Ra)
  columns.b = ncol(Rb)
  columnnames.a = colnames(Ra)
  columnnames.b = colnames(Rb)
  Ra.excess = PerformanceAnalytics::Return.excess(Ra, Rf)
  Rb.excess = PerformanceAnalytics::Return.excess(Rb, Rf)
  if (is.na(scale)) {
    freq = periodicity(Ra)
    switch(freq$scale, minute = {
      stop("Data periodicity too high")
    }, hourly = {
      stop("Data periodicity too high")
    }, daily = {
      scale = 252
    }, weekly = {
      scale = 52
    }, monthly = {
      scale = 12
    }, quarterly = {
      scale = 4
    }, yearly = {
      scale = 1
    })
  }
  for (column.a in 1:columns.a) {
    for (column.b in 1:columns.b) {
      merged.assets = merge(Ra.excess[, column.a, drop = FALSE], 
                            Rb.excess[, column.b, drop = FALSE])
      merged.assets = as.data.frame(na.omit(merged.assets))
      model.lm = lm(merged.assets[, 1] ~ merged.assets[, 
                                                       2])
      alpha = coef(model.lm)[[1]]
      beta = coef(model.lm)[[2]]
      if(any(Rb[, column.b] > 0)){
        CAPMbull = PerformanceAnalytics::CAPM.beta.bull(Ra[, column.a], Rb[, column.b], 
                                  Rf)
      } else {
        CAPMbull = NA
      }
      if(any(Rb[, column.b] < 0)){
        CAPMbear = PerformanceAnalytics::CAPM.beta.bear(Ra[, column.a], Rb[, column.b], 
                                  Rf)
      } else {
        CAPMbear = NA
      }
      
      htest = cor.test(merged.assets[, 1], merged.assets[, 
                                                         2])
      active.premium = PerformanceAnalytics::ActivePremium(Ra = Ra[, column.a], 
                                     Rb = Rb[, column.b], scale = scale)
      tracking.error = PerformanceAnalytics::TrackingError(Ra[, column.a], Rb[, 
                                                        column.b], scale = scale)
      treynor.ratio = PerformanceAnalytics::TreynorRatio(Ra = Ra[, column.a], 
                                   Rb = Rb[, column.b], Rf = Rf, scale = scale)
      z = c(alpha, beta, CAPMbull, CAPMbear, summary(model.lm)$r.squared, 
            ((1 + alpha)^scale - 1), htest$estimate, htest$p.value, 
            tracking.error, active.premium, active.premium/tracking.error, 
            treynor.ratio)
      znames = c("Alpha", "Beta", "Beta+", "Beta-", "R-squared", 
                 "Annualized Alpha", "Correlation", "Correlation p-value", 
                 "Tracking Error", "Active Premium", "Information Ratio", 
                 "Treynor Ratio")
      if (column.a == 1 & column.b == 1) {
        result.df = data.frame(Value = z, row.names = znames)
        colnames(result.df) = paste(columnnames.a[column.a], 
                                    columnnames.b[column.b], sep = " to ")
      }
      else {
        nextcolumn = data.frame(Value = z, row.names = znames)
        colnames(nextcolumn) = paste(columnnames.a[column.a], 
                                     columnnames.b[column.b], sep = " to ")
        result.df = cbind(result.df, nextcolumn)
      }
    }
  }
  result.df = base::round(result.df, digits)
  result.df
}
