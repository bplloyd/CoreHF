#' @include rp_sample_parallel.R

randomPortfolios = function(assets,
                            maxWeights,
                            minWeights,
                            max_permutations = 200,
                            permutations = 100000)

{


  # library(PortfolioAnalytics)
  # library(data.table)

  portfolio = PortfolioAnalytics::portfolio.spec(assets = assets)
  portfolio = PortfolioAnalytics::add.constraint(portfolio = portfolio, type = "weight_sum", min_sum = 0.98, max_sum = 1.02)
  portfolio = PortfolioAnalytics::add.constraint(portfolio = portfolio, type = "long_only")
  portfolio = PortfolioAnalytics::add.constraint(portfolio = portfolio, type = "box", min = minWeights, max = maxWeights)

  rp_sample_parallel(portfolio = portfolio, permutations = permutations, max_permutations = max_permutations)


  # filePath = "bigMemFiles//"
  # port.fileName = "randPorts"
  # dist.fileName = "jointDist"
  #
  # i=0
  # while(file.exists(paste0(filePath, port.fileName, i, ".bin")) & file.exists(paste0(filePath, dist.fileName, i, ".bin")))
  # {
  #   i = i+1
  # }
  #
  # ports.bm = as.big.matrix(ports, backingfile = paste0(port.fileName, i, ".bin"), descriptorfile = paste0(port.fileName, i, ".desc"), backingpath = filePath)
  # #dist.bm = as.big.matrix(as.matrix(jointDist$subDist), backingfile = paste0(dist.fileName, i, ".bin"), descriptorfile = paste0(dist.fileName, i, ".desc"), backingpath = filePath)
  # dist.bm = as.big.matrix(jointDistribution, backingfile = paste0(dist.fileName, i, ".bin"), descriptorfile = paste0(dist.fileName, i, ".desc"), backingpath = filePath)
  #
  # ports.obj = randPorts_obj(dist.bm@address, ports.bm@address, p = p, objFunc = objectiveFunc)
  # ports = cbind(ports.obj, ports)
  # colnames(ports)[1] = objectiveFunc
  # ports = ports[order(ports[,1]),]
  # return(as.data.table(ports))
}
