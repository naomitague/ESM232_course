#'  Logistic population growth derivative with harvesting
#' @param time time since start
#' @param P population
#' @param parms - as list with three values, r, K, harv
#' @param r intrinsic growth rate
#' @param K carrying capacity
#' @param h harvest rate
#' @param mincarbon minimum carbon to allow harvest
#' @return derivative of population with time

dharvestvary <- function(Time, biomass, parms) {
  # reduce harvest rate as you get below minimum carbon

  # actual growth rate
  modified_r = parms$r - abs((parms$precip[Time]-parms$precip_opt))/parms$precip_opt*parms$r

  modified_r = max(0, modified_r)

  expected_biomass = biomass-parms$harv
  # modified harvest if harvest will get us close to min carbon
  safe_biomass = parms$mincarbon*(1+parms$safety_margin)
  modified_harvest = ifelse((expected_biomass < parms$mincarbon), 0,
                            ifelse((expected_biomass < safe_biomass),
                                   min(parms$harv, (expected_biomass-parms$mincarbon)/2),
                                   parms$harv))

      db <- modified_r * biomass * (1 - biomass / parms$K) - modified_harvest


  return(list(db))
}
