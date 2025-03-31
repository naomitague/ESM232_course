#'  Logistic population growth derivative with harvesting
#' @param time time since start
#' @param biomass biomass
#' @param parms - as list with three values, r, K, harv
#' @param r intrinsic growth rate
#' @param K carrying capacity
#' @param h harvest rate
#' @return derivative of population with time

dharvest <- function(Time, bio, parms) {
  dbio <- parms$r * bio * (1 - bio / parms$K) - parms$harv * bio
  return(list(dbio))
}
