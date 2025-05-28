p_wrapper = function(rprey, alpha, eff, pmort, K, currpop, days, func) {
  parms = list(rprey=rprey, alpha=alpha, eff=eff, pmort=pmort, K=K)
  result = ode(y=currpop, times=days, func=func, parms=parms)
  colnames(result) = c("time","prey","pred")
  metrics = compute_metrics(as.data.frame(result))
  return(metrics)
}
