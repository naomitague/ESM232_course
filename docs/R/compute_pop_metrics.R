
#'  Extract metrics of interest from a population time series
#' @param result data frame with time eries of population (P) and time (time)
#' @return maximum population and time that this population doubles
#'
compute_pop_metrics = function(result) {
  maxpop = max(result$P)
  idx = min(which(result$P > result$P[1]*2))
  dyear = result$time[idx]
  return(list(maxpop=maxpop, dyear=dyear))}
