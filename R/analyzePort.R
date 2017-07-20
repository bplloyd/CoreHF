#' @include portCalcs.R


analyzePort = function(R, weights = rep(1/ncol(R), ncol(R)), calcs = c("return.annualized", "std.annualized"), ...){
  R.port = return.port(R, weights)
  sapply(calcs, function(f)do.call(f, list(R.port, ...)))
}

analyzePorts = function(R, ports, calcs = c("return.annualized", "std.annualized"), par = T, ...){
  if(par){
    ncore = parallel::detectCores()
    cl = parallel::makeCluster(ncore)
    cl.env = new.env()
    for(f in calcs){
      assign(x = f, value = match.fun(f), envir = cl.env)
    }
    assign(x = "R", value = R, envir = cl.env)
    assign(x = "ports", value = ports, envir = cl.env)
    assign(x = "calcs", value = calcs, envir = cl.env)
    assign(x = "analyzePort", value = analyzePort, cl.env)
    assign(x = "return.port", value = return.port, cl.env)
    varlist = c("R", "ports", "calcs", "analyzePort", "return.port")
    for(f in calcs){
      varlist = c(varlist, f)
    }
    parallel::clusterExport(cl, varlist = varlist, envir = cl.env)

    res = t(parallel::parSapply(cl = cl,
                                X = 1:nrow(ports),
                                FUN = function(r)analyzePort(R=R, weights=ports[r, ], calcs=calcs)))
    parallel::stopCluster(cl)
    rm(cl.env)
  } else {
    res = t(sapply(1:nrow(ports),
            FUN = function(r)analyzePort(R=R, weights = ports[r, ], calcs = calcs)))
  }
  cbind(ports, res)
}

sortAnalysis = function(assets, analysis, funcs = colnames(analysis[, -which(colnames(analysis) %in% assets)])){
  sorted = lapply(funcs,
         function(f) analysis[order(analysis[, f], decreasing = TRUE),])
  names(sorted) = funcs       
  sorted
}

