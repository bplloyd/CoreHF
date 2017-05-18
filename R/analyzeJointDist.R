#' @include portCalcs.R
#' @include categorizeMktQuantiles.R

analyzeJointDist = function(jointDist, assets, ports, mkt, calcs = c("return.annualized", "std.annualized", "sharpe.annualized", "mean", "min", "max"), sorts = c("return.annualized"=TRUE, "std.annualized"=FALSE,  "sharpe" = TRUE), probs = seq(0, 1, 0.1), ...){
    quants = categorizeMktQuantiles(jointDist, mkt, probs)
    R.quants = lapply(1:(length(probs)-1), function(q) jointDist[quants == q, assets])
    names(R.quants) = 1:(length(probs)-1)

    # R.quants.calcs = analyzeQuantilePorts(R.quants,
    #                       ports = ports, calcs = calcs
    #                       )

    system.time({R.quants.calcs = analyzeQuantilePorts(R.quants, ports = ports, calcs=calcs, par = F)})

    names(R.quants.calcs) = 1:(length(probs)-1)
    system.time({R.full.calcs = analyzePorts(R = jointDist[, assets], ports = ports, calcs = calcs, par = T)})
    R.quants.calcs = append(list(FULL = R.full.calcs), R.quants.calcs)
    R.quants.calcs = lapply(R.quants.calcs, function(q) cbind(q, sharpe=q[, "return.annualized"]/q[, "std.annualized"]))

    R.quants.sorted = lapply(names(sorts),
                             function(s) lapply(R.quants.calcs, function(q.data) q.data[order(q.data[, s],
                                                                                              decreasing = sorts[s]),]))
    names(R.quants.sorted) = names(sorts)

    return(R.quants.sorted)

}


# R.quants.calcs = analyzeJointDist(jointDist, assets = colnames(ports), ports = ports, mkts = mkts,
#                                   calcs = c("return.annualized", "std.annualized", "sharpe.annualized", "mean", "sd", "beta.sim", "max", "min"))
#
# names(R.quants.calcs)[2:11] = 1:10
# sorts = c("return.annualized", "std.annualized", "sharpe.annualized", "max.drawdown")
# R.quants.sorted = lapply(sorts,
#        function(s) lapply(R.quants.calcs, function(q.data) q.data[order(q.data[, s],
#                                                                         decreasing =  switch(s, 'return.annualized' = TRUE,
#                                                                                              'std.annualized' = FALSE,
#                                                                                              'sharpe.annualized' = TRUE)),]))
# names(R.quants.sorted) = sorts
#
# R.quants.tops_10 = lapply(R.quants.sorted,
#        function(s) t(sapply(s, function(q.data) colMeans(q.data[1:10,, drop=FALSE]))))
#
# for(i in 1:length(R.quants.tops)){R.quants.tops[[i]] = as.data.frame(R.quants.tops[[i]], row.names = row.names(R.quants.tops[[i]]))}
# for(i in 1:length(R.quants.tops)){
#   XLConnect::writeWorksheetToFile(file = "QuantilePorts.xlsx", data = R.quants.tops[[i]], sheet = names(R.quants.tops)[i])
# }
#
# # for(q in 1:10){
#   betas = rep(NA_real_, nrow(ports))
#   # mkt.calcs = analyzePort(R = as.matrix(jointDist[quants[,1] == q, "Mkt.RF"]), weights = 1, calcs = calcs)
#   mkt.calcs = analyzePort(R = as.matrix(jointDist[, "Mkt.RF"]), weights = 1, calcs = calcs)
#   mkt.ret_ann = rep(mkt.calcs["return.annualized"], nrow(ports))
#   mkt.ret_m = rep(mkt.calcs["mean"], nrow(ports))
#
#   mkt.vol_ann = rep(mkt.calcs["std.annualized"], nrow(ports))
#   mkt.vol_m = rep(mkt.calcs["sd"], nrow(ports))
#
#
#
#
#   for(r in 1:nrow(ports)){
#     # betas[r] = lm((R.quants[[1]][[q]] %*% ports[r,]) ~ jointDist[quants[,1] == q, "Mkt.RF"])$coefficients[2]
#     betas[r] = lm((jointDist[, assets] %*% ports[r,]) ~ jointDist[, "Mkt.RF"])$coefficients[2]
#   }
#
#   #m2 = R.quants.calcs[[1+q]][, "sharpe.annualized"] * mkt.vol_ann
#   m2 = R.quants.calcs$FULL[, "sharpe.annualized"] * mkt.vol_ann
#   # names(m2) = NULL
#   # names(betas) = NULL
#   # names(mkt.ret_ann) = NULL
#   # names(mkt.ret_m) = NULL
#   # names(mkt.vol_ann)=
#   #cbind(R.quants.calcs[[1+q]], betas, m2, mkt.ret_m, mkt.vol_m, mkt.ret_ann, mkt.vol_ann) -> R.quants.calcs[[1+q]]
#   cbind(R.quants.calcs$FULL, betas, m2, mkt.ret_m, mkt.vol_m, mkt.ret_ann, mkt.vol_ann) -> R.quants.calcs$FULL
#   #row.names(R.quants.calcs[[1+q]]) = NULL
#   row.names(R.quants.calcs$FULL) = NULL
# # }
#
#
#
# q = R.quants$Mkt.RF_QUANTILES[[1]]
# r=1
