
#'  Logistic population growth derivative with harvesting
#' @param time time since start
#' @param P population
#' @param parms - as list with three values, r, K, harv
#' @param r intrinsic growth rate 
#' @param K carrying capacity
#' @param h harvest rate
#' @return derivative of population with time 

dharvest= function(Time, P, parms) {
	
	dP = parms$r * P * (1- P/parms$K) - parms$harv*P
	return(list(dP))
}
