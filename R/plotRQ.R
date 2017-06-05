plotRQ = function(Ra, Rb, tau = seq(0.05, 0.95, 0.05), ...){
  Ra = na.omit(Ra)
  Rb = Rb[index(Ra),]
  lhs = names(Ra)
  rhs = names(Rb)
  if(ncol(Rb) > 1){
    for(j in 2:ncol(Rb)){
      rhs = paste(rhs, names(Rb)[j], sep = " + ")
    }
  }
  form = as.formula(paste(lhs, rhs, sep = " ~ "))
  R.rq = quantreg::rq(formula = form,
                      data = cbind(Ra, Rb),
                      tau = tau)
  
  # layout(matrix(c(1, 2)), heights = c(1, 1), widths = 1)
  # par(mar = c(1, 4, 4, 2))
  plot(summary(R.rq), ...)
  # par(mar = c(1, 4, 0, 2))
  # plotRQ.scatter(Ra, Rb, tau)
}

plotRQ.scatter = function(Ra, Rb, tau = seq(0.05, 0.95, 0.05), ...){
  Ra = na.omit(Ra)
  Rb = Rb[index(Ra),]
  data = as.data.frame(cbind(Ra, Rb))
  lhs = names(Ra)
  rhs = names(Rb)
  if(ncol(Rb) > 1){
    for(j in 2:ncol(Rb)){
      rhs = paste(rhs, names(Rb)[j], sep = " + ")
    }
  }
  form = as.formula(paste(lhs, rhs, sep = " ~ "))
  # R.rq = quantreg::rq(formula = form,
  #                     data = cbind(Ra, Rb),
  #                     tau = tau)
  # plot(Ra, Rb)
  plot(data[, c(2,1)], ...)
  # abline(quantreg::rq(log10(Ra) ~ log10(Rb), tau = 0.5), col = "blue")
  # abline(quantreg::rq(data = data, tau = 0.5), col = "blue", lwd = 3)
  abline(quantreg::rq(formula = form, data = data, tau = 0.5), col = "blue", lwd = 3)
  abline(lm(formula = form, data = data), lty = 3, col = "red", lwd = 3)
  # abline(lm(log10(Ra) ~ log10(Rb)), lty = 3, col = "red")
  for(i in 1:length(tau)){
    abline(reg= quantreg::rq(formula = form, data = data, tau=tau[i]), col="gray")
  }
  
  
}
# plot(income,foodexp,log="xy",xlab="Household Income", ylab="Food Expenditure") > taus <- c(.05,.1,.25,.75,.90,.95) > abline(rq(log10(foodexp)~log10(income),tau=.5),col="blue") > abline(lm(log10(foodexp)~log10(income)),lty = 3,col="red") > for( i in 1:length(taus)){ + +
#     abline(rq(log10(foodexp)~log10(income),tau=taus[i]),col="gray")
# }