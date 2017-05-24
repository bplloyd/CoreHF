rebalanceProRata = function(weights_cur, new, names) {
  weights_new = c(weights_cur * (1 - sum(new)), new)
  names(weights_new) = c(names(weights_cur), names)
  weights_new
}