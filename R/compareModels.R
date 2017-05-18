#' @include extractSingleBetaroll.R
#' @include accuracy_OOS.R

compareModels = function(mod_rolls, data, burnIn=12){
  mgr.name =  mod_rolls[[1]][[length(mod_rolls[[1]])]]$asset.names[1]
  mod_betas = lapply(mod_rolls, function(m)extractSingleBetaroll(Fm.roll = m,
                                                            name = mgr.name))
  mod_betas = lapply(mod_betas, function(b)xts::xts(b, order.by = zoo::as.yearmon(row.names(b))))

  mod_accs = lapply(mod_betas,
                    function(b)roll_accuracy_OOS(b, mgr.data = data[, mgr.name],
                                                 factor.data = data[, names(data) %in% names(b)], burnIn = burnIn))
  res = lapply(names(mod_accs[[1]]),
         function(n){
           n_xts = mod_accs[[1]][, n]
           for(i in 2:length(mod_accs)){
             n_xts = cbind(n_xts, mod_accs[[i]][, n])
           }
           return(n_xts)
         })
  names(res) = names(mod_accs[[1]])
  res
}
