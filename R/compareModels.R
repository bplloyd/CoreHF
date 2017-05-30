#' @include extractSingleBetaroll.R
#' @include accuracy_OOS.R

compareModels = function(mod_rolls, data, burnIn=12){
  mgr.name =  mod_rolls[[1]][[length(mod_rolls[[1]])]]$asset.names[1]
  mod_betas = lapply(mod_rolls, function(m)extractSingleBetaroll(Fm.roll = m,
                                                            name = mgr.name))
  mod_betas = lapply(mod_betas, function(b)xts::xts(b, order.by = zoo::as.yearmon(row.names(b))))
  
  beta_start = max(as.yearmon(sapply(mod_betas, function(b)as.character(start(na.omit(b))))))
  mod_betas = lapply(mod_betas, function(b)b[index(b) >= beta_start, ])
  
  mod_accs = lapply(mod_betas,
                    function(beta_roll)try(roll_accuracy_OOS(beta_roll, mgr.data = data[index(data) >= beta_start, mgr.name],
                                                 factor.data = data[index(data) >= beta_start, names(data) %in% names(beta_roll)], burnIn = burnIn)))
  res = lapply(names(mod_accs[[1]]),
         function(n){
           n_xts = mod_accs[[1]][, n]
           names(n_xts) = paste0(n, "_", names(mod_accs)[1])
           for(i in 2:length(mod_accs)){
             n_xts = cbind(n_xts, mod_accs[[i]][, n])
             names(n_xts)[i] = paste0(n, "_", names(mod_accs)[i])
           }
           return(n_xts)
         })
  names(res) = names(mod_accs[[1]])
  list(accuracy_measures = res, betas = mod_betas)
}
