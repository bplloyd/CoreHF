#' @include createSlices.R

rollTsfm = function(asset.names, factor.names, data, fit.method = "LS", variable.selection = "none", window = 12, fixedWindow = T, on=NULL, par = TRUE, control = NULL){
  data = data[which(rowSums(is.na(data[, asset.names, drop=F])) < length(asset.names)), , drop=F]
  slices = createSlices(data, initialWindow = window, fixedWindow = fixedWindow, on = on)
  
  if(par) {
    ncore = parallel::detectCores()
    cl = parallel::makeCluster(ncore)
    env = new.env()
    
    assign(x = "asset.names", value = asset.names, envir = env)
    assign(x = "factor.names", value = factor.names, envir = env)
    assign(x = "data", value = data, envir = env)
    assign(x = "slices", value = slices, envir = env)
    assign(x = "fit.method", value = fit.method, envir = env)
    assign(x = "variable.selection", value = variable.selection, envir = env)
    assign(x = "control", value = control, envir = env)
    
    parallel::clusterExport(cl = cl, 
                            varlist = c("asset.names", "factor.names", "data", "slices", "fit.method", "variable.selection", "control"),
                            envir = env)
    
    parallel::clusterEvalQ(cl = cl, 
                            expr = {library(xts);})
    
    if(!is.null(control)){
      res = parallel::parLapply(cl = cl, 
                                X = slices, 
                                fun = function(x)return(try(factorAnalytics::fitTsfm(asset.names = asset.names[which(colSums(is.na(data[x, asset.names, drop = F]))==0)],
                                                                                     factor.names = factor.names,
                                                                                     data = data[x, , drop=F],
                                                                                     fit.method = fit.method,
                                                                                     variable.selection = variable.selection,
                                                                                     control = control)
                                )
                                )
      )
      
      
    } else {
      res = parallel::parLapply(cl = cl, 
                                X = slices, 
                                fun = function(x)return(try(factorAnalytics::fitTsfm(asset.names = asset.names[which(colSums(is.na(data[x, asset.names, drop = F]))==0)],
                                                                                     factor.names = factor.names,
                                                                                     data = data[x, , drop=F],
                                                                                     fit.method = fit.method,
                                                                                     variable.selection = variable.selection)
                                )
                                )
      )
    }
    
    
    parallel::stopCluster(cl)
    return(res)
  } else {
    if(!is.null(control)){
      return(lapply(slices,
                    FUN = function(x)return(try(factorAnalytics::fitTsfm(asset.names = asset.names[which(colSums(is.na(data[x, asset.names, drop = F]))==0)],
                                                                     factor.names = factor.names,
                                                                     data = data[x, , drop=F],
                                                                     fit.method = fit.method,
                                                                     variable.selection = variable.selection,
                                                                     control = control)
                    )
                    )
      ))
    } else {
      return(lapply(slices,
                    FUN = function(x)return(try(factorAnalytics::fitTsfm(asset.names = asset.names[which(colSums(is.na(data[x, asset.names, drop = F]))==0)],
                                                                         factor.names = factor.names,
                                                                         data = data[x, , drop=F],
                                                                         fit.method = fit.method,
                                                                         variable.selection = variable.selection)
                    )
                    )
      ))
      
    }
  }
}
