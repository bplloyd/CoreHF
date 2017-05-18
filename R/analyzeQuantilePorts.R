analyzeQuantilePorts = function(R.quants, ports, calcs = c("return.annualized", "std.annualized", "sharpe.annualized"), par=TRUE, ...){
  if(par){
    ncore = parallel::detectCores()
    cl = parallel::makeCluster(ncore)
    cl.env = new.env()
    for(f in calcs){
      assign(x = f, value = match.fun(f), envir = cl.env)
    }
    # assign(x = "R", value = R, envir = cl.env)
    assign(x = "R.quants", value = R.quants, envir = cl.env)
    assign(x = "ports", value = ports, envir = cl.env)
    assign(x = "analyzePorts", value = analyzePorts, cl.env)
    assign(x = "analyzePort", value = analyzePort, cl.env)
    assign(x = "return.port", value = return.port, cl.env)
    assign(x = "calcs", value = calcs, cl.env)
    varlist = c("R.quants", "ports",  "analyzePorts", "analyzePort", "return.port", "calcs")

    for(f in calcs){
      varlist = c(varlist, f)
    }

    parallel::clusterExport(cl, varlist = varlist, envir = cl.env)

    R.quant.calcs = parallel::parLapply(cl = cl,
                                        X = R.quants,
                                        function(R.q)analyzePorts(R = R.q,
                                                                  ports = ports,
                                                                  calcs = calcs,
                                                                  par = FALSE))
    rm(cl.env)
    parallel::stopCluster(cl)
  } else {
    R.quant.calcs = lapply(R.quants,
                           function(R.q)analyzePorts(R = R.q, ports = ports, calcs = calcs, par = TRUE))
  }


  R.quant.calcs
}
