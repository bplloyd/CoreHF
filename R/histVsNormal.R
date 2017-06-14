histVsNormal = function(R, scale = F, main = paste0("Histogram of ", colnames(R), " Returns"), ...)
{
  R=na.omit(R)
  if(scale)
    R=scale(R)
  
  hist(R, main = main,  col = "lightblue", freq =  F, ...)
  lines(density(R), lwd= 2)
  curve(dnorm(x, mean = mean(R), sd = sd(R)), add = T, col = "red", lty = 2, lwd = 2)
  rug(R)
  legend(x = "topleft",
          # x = rep(min(R, na.rm = TRUE), 2),
         y = c(30, 20),
          legend = c(paste0(names(R), " Distribution"), "Normal Distribution"),
         col = c("black", "red"),
         lty = c(1, 2))
}

# testN = function(n)
# {
#   m_n = apply.rolling(R=sp2, width = n, FUN = mean)
#   sd_n = apply.rolling(R=sp2, width = n, FUN = sd)
#   histVsNormal((sp2 - m_n)/sd_n, scale = T)
# }