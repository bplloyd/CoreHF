makeResids = function(mod_list){
  if(class(mod_list)[1] == "list"){
    resids = lapply(mod_list, function(m)m$asset.fit[[1]]$residuals)
  } else if(class(mod_list)[1] == "tsfm"){
    resids = lapply(mod_list$asset.fit, function(m)m$residuals)
  }
  resids_xts = xts::xts(resids[[1]], order.by = zoo::as.yearmon(names(resids[[1]])))
  for(j in 2:length(resids)){
    resids_xts = cbind(resids_xts,  xts::xts(resids[[j]], order.by = zoo::as.yearmon(names(resids[[j]]))))
  }
  names(resids_xts) = names(resids)
  resids_xts
}
