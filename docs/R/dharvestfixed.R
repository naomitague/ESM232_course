
#'  Logistic population growth derivative with harvesting
#' @param time time since start
#' @param P population
#' @param parms - as list with three values, r, K, harv
#' @param r intrinsic growth rate 
#' @param K carrying capacity
#' @param h harvest rate
#' @param mincarbon minimum carbon to allow harvest
#' @return derivative of population with time 

dharvestfixed= function(Time, P, parms) {
	
	dP = parms$r * P * (1- P/parms$K) - parms$harv
	if (P+dP < parms$mincarbon) 
	  dP = parms$r*P*(1-P/parms$K)
	
	return(list(dP))
}
