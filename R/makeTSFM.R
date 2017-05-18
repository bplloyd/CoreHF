makeTSFM = function(mod_list, data, insigAlpha = F){
  mgrs = names(mod_list)
  coefs = lapply(mod_list, function(m)m$asset.fit[[1]]$coefficients)
  alpha = sapply(coefs, function(v)v[1])
  resid.sd = sapply(mod_list, function(m)m$resid.sd)
  resids = lapply(mod_list, function(m)m$asset.fit[[1]]$residuals)
  resids_xts = xts::xts(resids[[1]], order.by = zoo::as.yearmon(names(resids[[1]])))
  for(j in 2:length(resids)){
    resids_xts = cbind(resids_xts, xts::xts(resids[[j]], order.by = zoo::as.yearmon(names(resids[[j]]))))
  }
  names(resids_xts) = names(resids)

  names(alpha) = names(resid.sd) = mgrs
  factor_names = unique(unlist(lapply(coefs, function(v)names(v)[2:length(v)])))
  beta = matrix(data = 0, nrow = length(mod_list), ncol = length(factor_names))
  colnames(beta) = factor_names
  row.names(beta) = mgrs
  for(n in mgrs){
    mgr_beta = coefs[[n]][2:length(coefs[[n]])]
    beta[n, names(mgr_beta)] = mgr_beta
  }
  beta = as.data.frame(beta)
  alpha = as.data.frame(alpha)
  names(alpha) = "(Intercept)"
  r2 = sapply(mod_list, function(m)m$r2)
  names(r2) = names(mod_list)
  res = list(asset.fit = lapply(mod_list, function(m)m$asset.fit[[1]]),
             alpha = alpha,
             beta = beta,
             r2 = r2,
             resid.sd = resid.sd,
             call = NULL,
             data = data,
             asset.names = names(mod_list),
             factor.names = factor_names,
             mkt.name = NULL,
             fit.method =  mod_list[[1]]$fit.method,
             variable.selection = mod_list[[1]]$variable.selection)
  class(res) = "tsfm"

  if(!insigAlpha){
    mods_sum = summary(res)
    alpha_sig = sapply(mods_sum$sum.list,
                       function(s) if(s$coefficients[1,3] >= 2.5){TRUE} else {FALSE})
    res$alpha[!alpha_sig, ] = 0
  }
  return(res)
}
