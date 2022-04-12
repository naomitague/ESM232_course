
#'  Forest growth model
#' @param time time since start
#' @param C forest carbon
#' @param parms - as list with three values, r, K, closer
#' @param r intrinsic growth rate 
#' @param K carrying capacity (kgC)
#' @param g linear growth rate after closer
#' @param closer canopy closer (kg C)
#' @return derivative of population with time 

dforestgrowth= function(Time, C, parms) {
	
  dC = ifelse(C < parms$closer, parms$r*C, parms$g)
  dC = ifelse(C >= parms$K, 0, dC)
	
	return(list(dC))
}
