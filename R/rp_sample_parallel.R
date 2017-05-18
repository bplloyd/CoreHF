rp_sample_parallel = function (portfolio, permutations = NULL, max_permutations = NULL)
{
  #library(parallel)
  #portfolio
  if(is.null(max_permutations))
    max_permutations = 200

  if(is.null(permutations))
    permutations = 100000

  ncore = parallel::detectCores()
  cl = parallel::makeCluster(ncore)

  env = new.env()
  assign(x = "portfolio", value = portfolio, envir = env)
  assign(x = "permutations", value = permutations, envir = env)
  assign(x = "max_permutations", value = max_permutations, envir = env)

  parallel::clusterEvalQ(cl=cl, {
                        library(PortfolioAnalytics);
                        NULL;
                      })


  callRP = function(portfolio, max_permutations){return(PortfolioAnalytics::randomize_portfolio(portfolio, max_permutations))}

  parallel::clusterExport(cl=cl,
                          varlist = c("portfolio", "max_permutations", "permutations", "callRP"),
                          envir = env
  )

  seed = portfolio$assets

  result <- matrix(nrow = permutations, ncol = length(seed))
  result[1, ] <- seed
  ew = rep(1/length(seed), length(seed))
  result[2, ] <- ew

  result[3:permutations, ] <- t(parallel::parSapply(cl=cl,
                                                    X=3:permutations,
                                                FUN = function(j){return(callRP(portfolio = portfolio,
                                                                          max_permutations = max_permutations))
                                          }
                                        ))



  parallel::stopCluster(cl)
  colnames(result) <- names(seed)
  return(unique(result))
}
