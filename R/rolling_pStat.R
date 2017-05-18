rolling_pStat = function(mod_roll){
  funds = unique(unlist(lapply(mod_roll, function(m)names(m$asset.fit))))
  factors = lapply(funds, 
                   function(n) unique(unlist(lapply(names(mod_roll), 
                                                    function(d)if(n %in% names(mod_roll[[d]]$asset.fit)){names(mod_roll[[d]]$asset.fit[[n]]$coefficients)}))))
  names(factors) = funds
  pStats = lapply(factors, function(f)matrix(data = NA_real_, nrow = length(mod_roll), ncol = length(f), dimnames = list(names(mod_roll), f)))
  for(n in funds){
    for(d in names(mod_roll)){
      coefs = summary(mod_roll[[d]]$asset.fit[[n]])$coefficients
      pStats[[n]][d, row.names(coefs)] = coefs[, 4]
    }
    pStats[[n]] = xts::xts(pStats[[n]], order.by = zoo::as.yearmon(names(mod_roll)))
  }
  if(length(funds) > 1){
    return(pStats)
  } else {
    return(pStats[[1]])
  }
  
  
}